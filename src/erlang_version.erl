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
version_to_binary() -> "18.1".

major_version() ->
    erlang:system_info(otp_release).

minor_version() ->
    [_, MinorPart] = string:tokens(erlang:system_info(version), "."),
    MinorPart.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
major_version_test() ->
    ?assertEqual("18", major_version()).

minor_version_test() ->
    ?assertEqual("1", minor_version()).

version_to_binary_test() ->
    ?assertEqual("18.1", version_to_binary()).
-endif.
