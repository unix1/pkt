-module(pkt_storage_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    server_init/1,
    server_bucket_exists/1,
    server_get_put/1,
    server_list/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        server_init,
        server_bucket_exists,
        server_get_put,
        server_list
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(pkt),
    Config.

end_per_suite(_) ->
    ok = application:stop(pkt).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = pkt_storage_server:empty(uri),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify uri bucket is pre-created
    {error, already_exists, _} = pkt_storage_server:create(uri).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = pkt_storage_server:get(pkt_testing, 33333),
    ok = pkt_storage_server:create(pkt_testing),
    [] = pkt_storage_server:get(pkt_testing, 33333).

server_get_put(_) ->
    [] = pkt_storage_server:get(uri, 12345),
    ok = pkt_storage_server:put(uri, 12345, [{<<"some-hash">>, <<"http://foo/bar">>}]),
    [{12345, [{<<"some-hash">>, <<"http://foo/bar">>}]}] = pkt_storage_server:get(uri, 12345).

server_list(_) ->
    [] = pkt_storage_server:list(uri),
    ok = pkt_storage_server:put(uri, 54321, [{<<"hash-other">>, <<"ftp://why">>}]),
    ok = pkt_storage_server:put(uri, 12345, [{<<"hash-some2">>, <<"fish://server:path/to/file">>}]),
    [
        {12345, [{<<"hash-some2">>, <<"fish://server:path/to/file">>}]},
        {54321, [{<<"hash-other">>, <<"ftp://why">>}]}
    ] = lists:sort(pkt_storage_server:list(uri)).
