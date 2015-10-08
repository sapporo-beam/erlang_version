-module('erlang_version').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([version_to_binary/0,
         major_version/0,
         minor_version/0,
         patch_version/0]).

%%====================================================================
%% API functions
%%====================================================================
version_to_binary() ->
    % See also
    % http://www.erlang.org/doc/system_principles/versions.html
    % "Retrieving Current OTP Version"
    Filepath = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
    {ok, Binary} = file:read_file(Filepath),
    % Strip <<"\n">>
    binary:part(Binary, 0, byte_size(Binary) - 1).

major_version() ->
    [Major, _ | _] = binary:split(version_to_binary(), <<".">>),
    Major.

minor_version() ->
    [_, Minor | _] = binary:split(version_to_binary(), <<".">>),
    Minor.

patch_version() ->
    [_, _ | Patch] = binary:split(version_to_binary(), <<".">>),
    case Patch of
        [] -> <<"">>;
        true -> Patch
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
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
    ?assertEqual(list_to_binary(Major), major_version()).

minor_version_test() ->
    {_, Minor, _} = another_version_detamination(),
    ?assertEqual(list_to_binary(Minor), minor_version()).

patch_version_test() ->
    {_, _, Patch} = another_version_detamination(),
    ?assertEqual(list_to_binary(Patch), patch_version()).

version_to_binary_test() ->
    {Major, Minor, _} = another_version_detamination(),
    Full = string:join([Major, Minor], "."),
    ?assertEqual(list_to_binary(Full), version_to_binary()).
-endif.
