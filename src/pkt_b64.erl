%%%-------------------------------------------------------------------
%% @doc `pkt_b64' module
%%
%% Converts a decimal integer into its string base 64 representation. Provides
%% both encode and decode functions.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_b64).

-export([encode/1]).
-export([decode/1]).

-define(ALPHABET, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_").
-define(LENGTH, 64). % ALPHABET length

%% @doc Encode a non-negative integer into base 64 string
%%
%% @end
-spec encode(Int :: non_neg_integer()) -> string().
encode(0) -> "0";
encode(Int) when is_integer(Int), Int > 0 ->
    encode(Int, []).

encode(0, Acc) ->
    Acc;
encode(Int, Acc) ->
    Mod = Int rem ?LENGTH,
    NewInt = Int div ?LENGTH,
    encode(NewInt, [lists:nth(Mod + 1, ?ALPHABET)|Acc]).

%% @doc Decode a base 64 string into a decimal integer
%%
%% @end
decode(Str) ->
    decode(Str, 0).

decode("", Int) ->
    Int;
decode([Chr|Rest], Int) ->
    Pos = pkt_util:list_pos(Chr, ?ALPHABET, 0),
    decode(Rest, Int * ?LENGTH + Pos).
