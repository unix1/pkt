%%%-------------------------------------------------------------------
%% @doc `pkt_sup' module
%%
%% This is a top level `one_for_one' pkt supervisor started by the pkt
%% application.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 10, 10}, children()}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get children specs
%% @private
%%
%% A convenience function to return all children specs.
%% @end
-spec children() -> [supervisor:child_spec()].
children() ->
    [].
