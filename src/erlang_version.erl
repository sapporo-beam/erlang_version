-module('erlang_version').

%% API exports
-export([full/0,
         major/0,
         minor/0,
         patch/0,
         parse/1]).

%%====================================================================
%% API functions
%%====================================================================
full() ->
    % See also
    % http://www.erlang.org/doc/system_principles/versions.html
    % "Retrieving Current OTP Version"
    Filepath = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
    {ok, Binary} = file:read_file(Filepath),
    % Strip <<"\n">>
    binary:part(Binary, 0, byte_size(Binary) - 1).

major() ->
    {Major, _, _} = parse(full()),
    Major.

minor() ->
    {_, Minor, _} = parse(full()),
    Minor.

patch() ->
    {_, _, Patch} = parse(full()),
    Patch.

parse(Bin) ->
    case do_parse(Bin) of
        [Major, Minor]        -> {Major, Minor, <<"">>};
        [Major, Minor, Patch] -> {Major, Minor, Patch}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
do_parse([Major, Rest]) ->
    [Major | binary:split(Rest, <<".">>)];
do_parse(Bin) ->
    do_parse(binary:split(Bin, <<".">>)).

%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

another_version_detamination() ->
    Pathnames = lists:reverse(string:tokens(code:root_dir(), "/")),
    [Match|_] = lists:filtermap(fun(Pathname) ->
                                        case re:run(Pathname, "^\(\\d+\)\.\(\\d+\)\.?\(.*\)$") of
                                            nomatch ->
                                                false;
                                            {match, [_, {MajorStart, MajorLength}, {MinorStart, MinorLength}, {RestStart, RestLength}]} ->
                                                Major = string:substr(Pathname, MajorStart + 1, MajorLength),
                                                Minor = string:substr(Pathname, MinorStart + 1, MinorLength),
                                                Rest = string:substr(Pathname, RestStart + 1, RestLength),
                                                {true, {Major, Minor, Rest}}
                                        end
                                end, Pathnames),
    Match.

major_version_test() ->
    {Major, _, _} = another_version_detamination(),
    ?assertEqual(list_to_binary(Major), major()).

minor_version_test() ->
    {_, Minor, _} = another_version_detamination(),
    ?assertEqual(list_to_binary(Minor), minor()).

patch_version_test() ->
    {_, _, Patch} = another_version_detamination(),
    ?assertEqual(list_to_binary(Patch), patch()).

parse_18_1_test() ->
    ?assertEqual({<<"18">>, <<"1">>, <<"">>},
                 parse(<<"18.1">>)).

parse_18_1_1_test() ->
    ?assertEqual({<<"18">>, <<"1">>, <<"1">>},
                 parse(<<"18.1.1">>)).

parse_18_1_0_1_test() ->
    ?assertEqual({<<"18">>, <<"1">>, <<"0.1">>},
                 parse(<<"18.1.0.1">>)).

version_to_binary_test() ->
    {Major, Minor, _} = another_version_detamination(),
    Full = string:join([Major, Minor], "."),
    ?assertEqual(list_to_binary(Full), full()).
-endif.
