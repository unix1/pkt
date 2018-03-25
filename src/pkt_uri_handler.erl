%%%-------------------------------------------------------------------
%% @doc `pkt_uri_handler' module
%%
%% Cowboy handler for /[:uri_id] resource.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_uri_handler).

-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([to_html/2]).

%%====================================================================
%% Callbacks
%%====================================================================

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, to_html}
    ], Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(uri_id, Req) of
        undefined ->
            {false, Req, index};
        Id ->
          {false, Req, Id}
    end.

to_html(Req, State) ->
    Body = [<<"<html>
<head>
    <meta charset=\"utf-8\">
    <title>URL shortener</title>
</head>
<body>
    <h2>Hello User</h2>
    <p>Short URL: </p>
</body>
</html>">>],
    {Body, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================
