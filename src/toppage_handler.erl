-module(toppage_handler).

-export([init/3, rest_init/2, terminate/3]).
-export([allowed_methods/2, 
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         expires/2,
         resource_exists/2,
         note_html/2,
         create_note/2]).




%%% ============================================================================
%%$                    handler required call-backs
%%% ============================================================================


init(_Type, _Req, []) ->

    put(node, 'nook@127.0.0.1'),
    {upgrade, protocol, cowboy_rest}.

terminate(_Reason, _Req, _State) ->
	ok.

rest_init(Req, _Opts) ->
    {ok, Req, 'nook@127.0.0.1'}.


%%% ============================================================================
%%$                    RESTful call-backs : GET and POST
%%% ============================================================================


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.


% For POST, PUT, PATCH content-types the resource type accepted and which call-
% back to use. 
content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_note}],
         Req, State}.


% For GET, HEAD, POST, PUT, PATCH, DELETE, the resource types provided and the
% call-back to use when a representation of the resource needs to be returned 
% used for GET and HEAD. 
content_types_provided(Req, State) ->
	{[
          {{<<"text">>, <<"plain">>, []}, note_text},
          {{<<"text">>, <<"html">>, []}, note_html}
         ], Req, State}.


% Delete the resource. This happens immediately, do delete_completed is not 
% explicitly needed because its default is true.
delete_resource(Req, State) -> 
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Id, Req2} ->
            case rpc:call(get(node), nook, destory, [for_nook(Id)]) of
                ok ->
                    {true, Req2, State};
                _ ->
                    {false, Req2, State}
                end
    end.

    
% Date the resource expires.
expires(Req, State) ->
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->
            {undefined, Req2, State};
        {Id, Req2} ->
            case rpc:call(get(node), nook, expiration, [for_nook(Id)]) of
                {ok, DateTime} ->
                    Expiration = list_to_binary(ec_date:format("D, j M Y G:i:s", DateTime) ++ " GMT"),
                    {Expiration, Req2, State};
                {error, missing_note} ->
                    {undefined, Req2, State}
            end
    end.

% For GET, HEAD, POST, PUT, PATCH, DELETE: Return wheather the resource exists.
resource_exists(Req, _State) -> 
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->          
            % no note_id on the uri
            {true, Req2, index};
        {NoteId, Req2} ->     
            case valid_id(NoteId) of
                true -> {true, Req2, NoteId};
                false -> {false, Req2, NoteId}
            end
    end.

    

% Result of a Post: if succesful should take the user to /noteid
create_note(Req, State) ->          
    {ok, Params, Req2} = cowboy_req:body_qs(Req),
    Note = params(<<"note">>, Params), 
    TTL = params(<<"TTL">>, Params),
    Gets = params(<<"Gets">>, Params),
    Id = for_cowboy(rpc:call(get(node), nook, new, [Note, TTL, Gets])),
    case cowboy_req:method(Req2) of
        {<<"POST">>, Req3} ->
            {{true, <<$/, Id/binary>>}, Req3, State};
        {_, Req3} ->
            {true, Req3, State}
	end.


% Result of a GET or HEAD: Return an HTML representation of the note.
note_html(Req, index) ->
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
    {read_file("index.html"), Req2, index};

note_html(Req, Id) ->
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
    {format_html(Id), Req2, Id}.


format_html(Id) ->
    NookId = for_nook(Id),
    {ok, #{contents := Contents}} = rpc:call(get(node), nook, get, [NookId]),
    rpc:call(get(node), nook, decriment, [NookId]),

    <<"<!DOCTYPE html><html>",
      "<head><title>note</title></head>",
      "<body><pre><code>", Contents/binary, "</code></pre></body></html>\n">>.


valid_id(NoteId) ->
    Id = for_nook(NoteId),
    case rpc:call(get(node), nook, exists, [Id]) of
        true ->  
            true;
        _ ->     
            false
    end.
        

read_file(Name) ->
	{ok, Binary} = file:read_file(full_path(Name)),
	Binary.

full_path(Name) ->
	filename:join([code:priv_dir(nook_fe), Name]).


for_nook(<<_/binary>> = Id) ->
    binary_to_list(Id);
for_nook([_|_] = Id) ->
    Id.

for_cowboy([_|_] = Id) ->
    list_to_binary(Id);
for_cowboy(<<_/binary>> = Id) ->
    Id.


params(<<"note">> = Key, PropList) ->
    proplists:get_value(Key, PropList);

params(Key, PropList) ->
    params_(proplists:get_value(Key, PropList)).


params_(<<"infinite">>) ->
    infinite;
params_(N) -> 
    binary_to_integer(N).

    
