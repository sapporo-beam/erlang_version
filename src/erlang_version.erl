%% @doc Retrieve Erlang/OTP version like `18.1'.
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

%% @doc Return full version of Erlang/OTP.
-spec full() -> binary().
full() ->
    % See also
    % http://www.erlang.org/doc/system_principles/versions.html
    % "Retrieving Current OTP Version"
    Filepath = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
    {ok, Binary} = file:read_file(Filepath),
    % Strip <<"\n">>
    binary:part(Binary, 0, byte_size(Binary) - 1).

%% @doc Return major version of Erlang/OTP.
-spec major() -> binary().
major() ->
    {Major, _, _} = parse(full()),
    Major.

%% @doc Return minor version of Erlang/OTP.
-spec minor() -> binary().
minor() ->
    {_, Minor, _} = parse(full()),
    Minor.

%% @doc Return patch version of Erlang/OTP.
%%
%% When patch version isn't declered, this function returns empty binary `<<>>'.
%%
%% When patch version includes many periods, this function returns binary which includes many periods like `<<"0.1">>'.
-spec patch() -> binary().
patch() ->
    {_, _, Patch} = parse(full()),
    Patch.

%% @doc Parse binary as Erlang/OTP version.
%%
%% When patch version isn't declered, third element of the tuple is empty binary `<<"">>'. following like:
%%
%% ```
%% parse(<<"18.1">>) # => {<<"18">>, <<"1">>, <<>>}
%% '''
%%
%% When patch version includes many periods, third element of the tuple just returns it. following like:
%%
%% ```
%% parse(<<"18.1.0.1">>) # => {<<"18">>, <<"1">>, <<"0.1">>}
%% '''
-spec parse(binary()) -> {binary(), binary(), binary()}.
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

full_test() ->
    Full = case another_version_detamination() of
               {Major, Minor, ""}   -> string:join([Major, Minor], ".");
               {Major, Minor, Rest} -> string:join([Major, Minor, Rest], ".")
           end,
    ?assertEqual(list_to_binary(Full), full()).

major_test() ->
    {Major, _, _} = another_version_detamination(),
    ?assertEqual(list_to_binary(Major), major()).

minor_test() ->
    {_, Minor, _} = another_version_detamination(),
    ?assertEqual(list_to_binary(Minor), minor()).

patch_test() ->
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
-endif.
