%%%-------------------------------------------------------------------
%% @doc `pkt_util' module
%%
%% Convenience functions
%% @end
%%%-------------------------------------------------------------------

-module(pkt_util).

% API
-export([list_pos/3]).
-export([html_encode/1]).

%%====================================================================
%% API
%%====================================================================

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

%% @doc HTML encode a string or a binary
%%
%% @end
-spec html_encode(Text :: binary()|list()) -> binary()|list().
html_encode(Text) when is_binary(Text) ->
    TextList = binary_to_list(Text),
    list_to_binary(html_encode(TextList));
html_encode(Text) when is_list(Text) ->
    lists:flatten(lists:map(fun quote/1, Text)).

%%====================================================================
%% Internal functions
%%====================================================================

quote($<) -> "&lt;";
quote($>) -> "&gt;";
quote($&) -> "&amp;";
quote($") -> "&quot;";
quote(C) -> C.
