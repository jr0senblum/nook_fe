-module(toppage_handler).

-export([init/3, rest_init/2, terminate/3]).
-export([allowed_methods/2, 
         content_types_accepted/2,
         content_types_provided/2,
         delete_resource/2,
         expires/2,
         resource_exists/2,
         note_html/2,         note_css/2,
         create_note/2]).




%%% ============================================================================
%%$                    handler required call-backs
%%% ============================================================================


init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.


terminate(_Reason, _Req, _State) ->
	ok.


rest_init(Req, _Opts) ->
    {ok, Node} = application:get_env(nook_fe, node),
    {ok, Req, #{node => Node}}.




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
          {{<<"text">>, <<"css">>, []}, note_css},
          {{<<"text">>, <<"html">>, []}, note_html}
         ], Req, State}.


% Delete the resource. This happens immediately, so delete_completed is not 
% explicitly needed because its default is true.
delete_resource(Req, #{node := Node} = State) -> 
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Id, Req2} ->
            case rpc:call(Node, nook, destory, [for_nook(Id)]) of
                ok ->
                    {true, Req2, State};
                _ ->
                    {false, Req2, State}
                end
    end.

    
% Date the resource expires.
expires(Req, #{node := Node} = State) ->
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->
            {undefined, Req2, State};
        {Id, Req2} ->
            case rpc:call(Node, nook, expiration, [for_nook(Id)]) of
                {ok, DateTime} ->
                    Expiration = list_to_binary(ec_date:format("D, j M Y G:i:s", DateTime) ++ " GMT"),
                    {Expiration, Req2, State};
                {error, missing_note} ->
                    {undefined, Req2, State}
            end
    end.


% For GET, HEAD, POST, PUT, PATCH, DELETE: Return whether the resource exists.
resource_exists(Req, #{node := Node} = State) -> 
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->          
            % no note_id on the uri
            {true, Req2, maps:put(resource, index, State)};
        {NoteId, Req2} ->     
            case valid_id(Node, NoteId) of
                true -> 
                    case cowboy_req:binding(new, Req2) of
                        {<<"new">>, Req3} -> 
                            % getting a newly created note 
                            {true, Req3, maps:put(resource, new, State)};
                        {undefined, Req3} ->
                            % getting a previously created note 
                            {true, Req3, maps:put(resource, NoteId, State)}
                    end;
                false ->
                    {false, Req2, State}
            end
    end.

    
% Result of a Post: if succesful should take the user to /note_id
create_note(Req, #{node := Node} = State) ->          
    Headers = [{<<"cache-control">>, <<"no-store">>}],

    {ok, Params, Req2} = cowboy_req:body_qs(Req),
    case validate([<<"note">>,<<"TTL">>,<<"Gets">>], Params) of
        error -> 
            {ok, _Req3} = cowboy_req:reply(400, Headers, index(error), Req2),
            {false, Req2, State};
        Results ->
            [Note, TTL, Gets] = Results,
            case rpc:call(Node, nook, new, [Note, TTL, Gets]) of
                {error, badarg} ->
                    {ok, _Req3} = cowboy_req:reply(400, Headers, index(error), Req2),
                    {false, Req2, State};
                Id -> 
                    Id2 = for_cowboy(Id),
                    case cowboy_req:method(Req2) of
                        {<<"POST">>, Req3} ->
                            {{true, <<$/, "new/", Id2/binary>>}, Req3, maps:put(resource, new, State)};
                        {_, Req3} ->
                            {true, Req3, State}
                    end
            end
    end.


% Result of a GET to the css directory (text/css content type)
note_css(Req, State) ->
    case cowboy_req:binding(file, Req) of
        {undefined, Req2} -> 
            {index(no_error), Req2, State};
        {File, Req2} ->
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/css">>, Req2),
            {read_file("css/" ++ binary_to_list(File)), Req3, State}
    end.


% Result of a GET or HEAD: Return an HTML representation of the note.
note_html(Req, #{resource := index} = State) ->
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
    {index(no_error), Req2, State};

note_html(Req, #{resource := new} = State) ->
    {NoteId, Req2} = cowboy_req:binding(note_id, Req),
    {HostUrl, Req3} = cowboy_req:host_url(Req2),
    Req4 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req3),
    {format_new_html(NoteId, HostUrl), Req4, State};


note_html(Req, #{resource := NoteId, node := Node} = State) ->
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
    {HostUrl, Req3} = cowboy_req:host_url(Req2),
    {format_html(NoteId, Node, HostUrl), Req3, State}.

format_new_html(NoteId, HostUrl) ->
    Id = for_cowboy(NoteId),
    Subject = read_file("recepit.html"),
    Targets = ["ID_BINARY", "HOST_BINARY"],
    Values = [<<Id/binary>>, <<HostUrl/binary>>],
    substitute(Subject, Targets, Values).


format_html(NoteId, Node, HostUrl) ->
    NookId = for_nook(NoteId),
    
    Message = case rpc:call(Node, nook, get, [NookId]) of
                  {ok, #{contents := Contents}} ->
                      rpc:call(Node, nook, decriment, [NookId]),
                      Contents;
                  {error, missing_note} ->
                      <<"Missing note">>;
                  {error, {storage_error, E}} ->
                      E
              end,
    Subject = read_file("note.html"),
    Targets = ["NOTE_BINARY","ID_BINARY","HOST_BINARY"],
    Values = [<<Message/binary>>, <<NoteId/binary>>, <<HostUrl/binary>>],
    substitute(Subject, Targets, Values). 





valid_id(Node, NoteId) ->
    Id = for_nook(NoteId),
    case rpc:call(Node, nook, exists, [Id]) of
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


validate(Keys, Proplist) ->
    try
        [params(Key, Proplist) || Key <- Keys]
    catch
        _:_ ->
            error
    end.


params(<<"note">> = Key, PropList) ->
    proplists:get_value(Key, PropList);

params(Key, PropList) ->
    params_(proplists:get_value(Key, PropList)).


params_(<<"infinite">>) ->
    infinite;
params_(N) -> 
    binary_to_integer(N).

    
index(error) ->
    Subject = read_file("index.html"),
    Value = <<"Ensure TTL and Gets are positive integers and that at most one is infinite.">>,
    substitute(Subject, ["ERROR"], [Value]);

index(_) ->
    Subject = read_file("index.html"),
    substitute(Subject, ["ERROR"], [<<"">>]).
    

substitute(Subject, [], []) ->
    Subject;

substitute(Subject, [T|Ts], [V|Vs]) ->
    Result = re:replace(Subject, T, V, [{return, list}, global]),
    substitute(Result, Ts, Vs).
