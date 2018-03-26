%%%-------------------------------------------------------------------
%% @doc `pkt_app' module
%%
%% This starts the pkt application.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_app).

-behaviour(application).

%% Supervision
-export([start/2, stop/1]).

%%====================================================================
%% Supervision
%%====================================================================

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", pkt_handler, []},
            {"/[:uri_id]", pkt_uri_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    pkt_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
