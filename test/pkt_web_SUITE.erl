-module(pkt_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    create/1,
    request_invalid_resource/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        create,
        request_invalid_resource
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(pkt),
    ok = application:start(inets),
    Config.

end_per_suite(_) ->
    ok = application:stop(inets),
    ok = application:stop(pkt).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

get_host() ->
    "http://localhost:8080".

get_content_type() ->
    "application/x-www-form-urlencoded".

get_options() ->
    [{body_format, binary}].

get_http_options() ->
    [{autoredirect, false}].

%%====================================================================
%% Tests
%%====================================================================

request_invalid_resource(_) ->
    Host = get_host(),
    Headers = [],
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Host ++ "/foobar", Headers}, [], []),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Host ++ "/foo/bar", Headers}, [], []),
    {ok, {{_, 404, _}, _, _}} = httpc:request(get, {Host ++ "/foo/bar/baz", Headers}, [], []).

create(_) ->
    Host = get_host(),
    Headers = [],
    ContentType = get_content_type(),
    HttpOpts = get_http_options(),
    Opts = get_options(),
    Uri = "http://foo/bar",
    {ok, {{_, 303, _}, ResponseHeaders, _Body}} = httpc:request(
        post,
        {Host, Headers, ContentType, "uri=" ++ Uri},
        HttpOpts,
        Opts),
    {value, {"location", Location}} = lists:keysearch("location", 1, ResponseHeaders),
    "/" ++ IdStr = Location,
    Id = pkt_b64:decode(IdStr),
    Hash = crypto:hash(sha256, Uri),
    UriBin = list_to_binary(Uri),
    [{Id, {Hash, UriBin}}] = pkt_storage_server:get(uri, Id).
