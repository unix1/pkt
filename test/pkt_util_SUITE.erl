-module(pkt_util_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    b64_encode/1,
    b64_decode/1,
    list_pos/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        b64_encode,
        b64_decode,
        list_pos
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(pkt),
    Config.

end_per_suite(_) ->
    ok = application:stop(pkt).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

b64_encode(_) ->
    "0" = pkt_b64:encode(0),
    "A" = pkt_b64:encode(10),
    "10" = pkt_b64:encode(64),
    "3-" = pkt_b64:encode(254),
    "80" = pkt_b64:encode(512).

b64_decode(_) ->
    0 = pkt_b64:decode("0"),
    10 = pkt_b64:decode("A"),
    64 = pkt_b64:decode("10"),
    254 = pkt_b64:decode("3-"),
    512 = pkt_b64:decode("80"),
    8 = pkt_b64:decode("0008").

list_pos(_) ->
    false = pkt_util:list_pos(5, [1, 2, 3, 4], 0),
    0 = pkt_util:list_pos(5, [5, 4, 3, 2, 1], 0),
    1 = pkt_util:list_pos(5, [5, 4, 3, 2, 1], 1),
    4 = pkt_util:list_pos(5, [1, 2, 3, 4, 5, 6, 7], 0),
    5 = pkt_util:list_pos(5, [1, 2, 3, 4, 5, 6, 7], 1).
