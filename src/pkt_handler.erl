%%%-------------------------------------------------------------------
%% @doc `pkt_handler' module
%%
%% Cowboy handler for / resource.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_handler).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([create_uri/2]).
-export([to_html/2]).

%%====================================================================
%% Callbacks
%%====================================================================

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_uri}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, to_html}
    ], Req, State}.

to_html(Req, State) ->
    Body = [<<"<html>
<head>
    <meta charset=\"utf-8\">
    <title>URL shortener</title>
</head>
<body>
    <h2>Hello User</h2>
    <p>Shorten all kinds of URLs here.</p>
</body>
</html>">>],
    {Body, Req, State}.

create_uri(Req, State) ->
    {ok, [{<<"uri">>, Uri}], Req2} = cowboy_req:read_urlencoded_body(Req),
    MaxSize = 7,
    AlphabetSize = 64,
    Hash = crypto:hash(sha256, Uri),
    Id = rand:uniform(trunc(math:pow(AlphabetSize, MaxSize))),
    IdB64 = list_to_binary(pkt_b64:encode(Id)),
    io:format("~nID is ~p for URL ~p with the hash of ~p", [IdB64, Uri, Hash]),
    case cowboy_req:method(Req2) of
        <<"POST">> ->
            ok = pkt_storage_server:put(uri, Id, {Hash, Uri}),
            {{true, <<$/, IdB64/binary>>}, Req2, State};
        _ ->
            {true, Req2, State}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
