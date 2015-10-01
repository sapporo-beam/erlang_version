-module('erlang_version').

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([version_to_binary/0,
         major_version/0]).

%%====================================================================
%% API functions
%%====================================================================
version_to_binary() -> "18.1".

major_version() ->
    erlang:system_info(otp_release).

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================
-ifdef(TEST).
major_version_test() ->
    ?assertEqual("18", major_version()).
version_to_binary_test() ->
    ?assertEqual("18.1", version_to_binary()).
-endif.
