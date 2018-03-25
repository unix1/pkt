%%%-------------------------------------------------------------------
%% @doc `pkt_util' module
%%
%% Convenience functions
%% @end
%%%-------------------------------------------------------------------

-module(pkt_util).

-export([list_pos/3]).

%% @doc Returns the position of the value in the list or `false' if not found
%%
%% The `Base' argument specifies base non-negative integer for counting. This
%% is commonly 0 or 1.
%% @end
-spec list_pos(V :: term(), List :: list(), Base :: non_neg_integer()) ->
    non_neg_integer() |
    false.
list_pos(_, [], _Acc) -> false;
list_pos(V, [V|_Rest], Acc) -> Acc;
list_pos(V, [_|Rest], Acc) -> list_pos(V, Rest, Acc + 1).
