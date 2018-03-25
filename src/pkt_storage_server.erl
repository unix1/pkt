%%%-------------------------------------------------------------------
%% @doc `pkt_storage_server' module
%%
%% This is a simple in-memory implementation of an storage server
%% for pkt.
%% @end
%%%-------------------------------------------------------------------

-module(pkt_storage_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/1]).
-export([empty/1]).
-export([get/2]).
-export([list/1]).
-export([put/3]).

-type state() :: map().
-type key() :: non_neg_integer().

%%===================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    % Pre-create uri bucket
    ok = create_bucket(uri),
    {ok, #{}}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a bucket
%%
%% @end
-spec create(Bucket :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {create, Bucket}).

%% @doc Empty a bucket
%%
%% @end
-spec empty(Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {empty, Bucket}).

%% @doc Get an item from the bucket by its key
%%
%% @end
-spec get(Bucket :: atom(), Key :: key()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
get(Bucket, Key) when is_atom(Bucket), is_integer(Key), Key >= 0 ->
    gen_server:call(?MODULE, {get, Bucket, Key}).

%% @doc List all items in a bucket
%%
%% @end
-spec list(Bucket :: atom()) -> [tuple()].
list(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {list, Bucket}).

%% @doc Put an item key value pair in an existing bucket
%%
%% @end
-spec put(Bucket :: atom(), Key :: key(), Value :: any()) -> ok.
put(Bucket, Key, Value) when is_atom(Bucket), is_integer(Key), Key >= 0 ->
    ok = gen_server:call(?MODULE, {put, Bucket, Key, Value}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({create, Bucket}, _From, State) ->
    {reply, create_bucket(Bucket), State};
handle_call({empty, Bucket}, _From, State) ->
    {reply, empty_bucket(Bucket), State};
handle_call({get, Bucket, Key}, _From, State) ->
    {reply, lookup_key(Key, Bucket), State};
handle_call({list, Bucket}, _From, State) ->
    {reply, list_items(Bucket), State};
handle_call({put, Bucket, Key, Value}, _From, State) ->
    {reply, put_item(Key, Value, Bucket), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check whether bucket already exists
%% @private
%%
%% @end
-spec bucket_exists(Bucket :: atom()) -> boolean().
bucket_exists(Bucket) when is_atom(Bucket) ->
    case ets:info(Bucket) of
        undefined -> false;
        _ -> true
    end.

%% @doc Create a bucket
%% @private
%%
%% If bucket with the same name already exists an error will be returned.
%% @end
-spec create_bucket(Bucket :: atom()) ->
    ok |
    {error, already_exists, string()}.
create_bucket(Bucket) when is_atom(Bucket) ->
    case bucket_exists(Bucket) of
        false ->
            Bucket = ets:new(Bucket, [set, named_table]),
            ok;
        _ ->
            {error, already_exists, "ETS table " ++ atom_to_list(Bucket) ++ " already exists."}
    end.

%% @doc Empty a bucket
%% @private
%%
%% If no such bucket exists, error will be returned.
%% @end
-spec empty_bucket(Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty_bucket(Bucket) when is_atom(Bucket) ->
    case bucket_exists(Bucket) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            true = ets:delete_all_objects(Bucket),
            ok
    end.

%% @doc List all items in a bucket
%% @private
%%
%% Returns all items stored in the bucket.
%% @end
-spec list_items(Bucket :: atom()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
list_items(Bucket) when is_atom(Bucket) ->
    case bucket_exists(Bucket) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            ets:tab2list(Bucket)
    end.

%% @doc Look up a game
%% @private
%%
%% Search for a game by its name in the map.
%% @end
-spec lookup_key(Key :: key(), Bucket :: atom()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
lookup_key(Key, Bucket) when is_atom(Bucket), is_integer(Key), Key >= 0 ->
    case bucket_exists(Bucket) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            ets:lookup(Bucket, Key)
    end.

%% @doc Put a key value pair in bucket
%% @private
%%
%% @end
-spec put_item(Key :: key(), Value :: any(), Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
put_item(Key, Value, Bucket) when is_atom(Bucket), is_integer(Key), Key >= 0 ->
    case bucket_exists(Bucket) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            true = ets:insert(Bucket, {Key, Value}),
            ok
    end.
