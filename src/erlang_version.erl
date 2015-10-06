-module('erlang_version').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([version_to_binary/0,
         major_version/0,
         minor_version/0]).

%%====================================================================
%% API functions
%%====================================================================
version_to_binary() ->
    Major = major_version(),
    Minor = minor_version(),
    <<Major/binary, <<".">>/binary, Minor/binary>>.

major_version() ->
    list_to_binary(erlang:system_info(otp_release)).

minor_version() ->
    [_, MinorPart] = string:tokens(erlang:system_info(version), "."),
    list_to_binary(MinorPart).

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

version_to_binary_test() ->
    {Major, Minor, _} = another_version_detamination(),
    Full = string:join([Major, Minor], "."),
    ?assertEqual(list_to_binary(Full), version_to_binary()).
-endif.
