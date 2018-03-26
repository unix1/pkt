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

-define(HOST, "http://localhost:8080/").

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
        IdBin ->
            IdStr = binary_to_list(IdBin),
            Id = pkt_b64:decode(IdStr),
            case pkt_storage_server:get(uri, Id) of
                [] ->
                    {false, Req, Id};
                [{Id, {_Hash, _UriBin}}] ->
                    {true, Req, Id}
            end
    end.

to_html(Req, State) ->
    #{r := R} = cowboy_req:match_qs([{r, [], <<"1">>}], Req),
    io:format("~nr is: ~p", [R]),
    Id = cowboy_req:binding(uri_id, Req),
    #{hash := _Hash, uri := Uri} = get_resource(Id),
    UriEncoded = pkt_util:html_encode(Uri),
    UriShortEncoded = pkt_util:html_encode(list_to_binary(?HOST ++ Id)),
    MetaRedirect = case R of
        <<"1">> ->
            [<<"<meta http-equiv=\"refresh\" content=\"0; url=">>, UriEncoded, <<"\" />">>];
        _ ->
            <<>>
    end,
    Body = [<<"<html>
<head>
    <meta charset=\"utf-8\">
    ">>, MetaRedirect, <<"
    <title>URL shortener</title>
</head>
<body>
    <h2>Hello User</h2>
    <p>Short URL: <a href=\"">>, UriShortEncoded, <<"\">">>, UriShortEncoded, <<"</a></p>
    <p>Destination URL: <a href=\"">>, UriEncoded, <<"\">">>, UriEncoded, <<"</a></p>
</body>
</html>">>],
    {Body, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_resource(IdBin :: binary())
    -> #{id => non_neg_integer(), hash => binary(), uri => binary()}.
get_resource(IdBin) ->
    IdStr = binary_to_list(IdBin),
    Id = pkt_b64:decode(IdStr),
    % Assumption is record exists at this point (no deletes)
    [{Id, {Hash, Uri}}|_] = pkt_storage_server:get(uri, Id),
    #{
        id => Id,
        hash => Hash,
        uri => Uri
    }.
