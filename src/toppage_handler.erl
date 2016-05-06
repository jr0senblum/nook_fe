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
%%%                    Nook interface
%%% ============================================================================


% Encapsulate the rpc call to cut down on code redundancy. Id needs to be a
% string and there may be 0 to n arguments to the nook call.
%
nook(Node, Method, Id) ->
    rpc:call(Node, nook, Method, [fix_id(Id)]).


nook(Node, Method, No_Id, Args) when is_atom(No_Id) ->
    rpc:call(Node, nook, Method, Args);

nook(Node, Method, Id, Args) ->
    rpc:call(Node, nook, Method, [fix_id(Id) | Args]).


fix_id(Id) when is_binary(Id) -> binary_to_list(Id);
fix_id(Id) -> Id.
     


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
% call-back used when a representation of the resource needs to be returned 
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
            case nook(Node, destory, Id) of
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
            case nook(Node, expiration, Id) of
                {ok, DateTime} ->
                    Expiration = exp_string(DateTime),
                    {Expiration, Req2, State};
                {error, missing_note} ->
                    {undefined, Req2, State}
            end
    end.


% For GET, HEAD, POST, PUT, PATCH, DELETE: Return whether the resource exists.
resource_exists(Req, #{node := Node} = State) -> 
    case cowboy_req:binding(note_id, Req) of
        {undefined, Req2} ->          
            % no note_id on the uri,
            {true, Req2, maps:put(resource, index, State)};
        {NoteId, Req2} ->     
            case note_exists(Node, NoteId) of
                true -> 
                    case cowboy_req:binding(new, Req2) of
                        {<<"new">>, Req3} -> 
                            % GETting a newly created note 
                            {true, Req3, maps:put(resource, new, State)};
                        {undefined, Req3} ->
                            % getting a previously created note 
                            {true, Req3, maps:put(resource, NoteId, State)}
                    end;
                false ->
                    {false, Req2, State}
            end
    end.

    
% Result of a POST: if succesful should take the user to /note_id
create_note(Req, #{node := Node} = State) ->          
    Headers = [{<<"cache-control">>, <<"no-store">>}],

    {ok, Params, Req2} = cowboy_req:body_qs(Req),
    case validate([<<"note">>,<<"TTL">>,<<"Gets">>], Params) of
        error -> 
            {ok, _Req3} = cowboy_req:reply(400, Headers, index(error), Req2),
            {false, Req2, State};
        [Note, TTL, Gets] ->
            case nook(Node, new, no_id, [Note, TTL, Gets]) of
                {error, badarg} ->
                    {ok, _Req3} = cowboy_req:reply(400, Headers, index(error), Req2),
                    {false, Req2, State};
                Id -> 
                    Id2 = to_binary(Id),
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
    Id = to_binary(NoteId),
    Subject = read_file("recepit.html"),
    Targets = ["ID_BINARY", "HOST_BINARY"],
    Values = [<<Id/binary>>, <<HostUrl/binary>>],
    substitute(Subject, Targets, Values).


format_html(NoteId, Node, HostUrl) ->
    Subject = read_file("note.html"),
    Targets = ["NOTE_BINARY","ID_BINARY","HOST_BINARY","GETS","EXP"],

    Values = case nook(Node, get, NoteId) of
                 {ok, #{contents := Message, gets := Gets, created := C, ttl := TTL}} ->
                     nook(Node, decriment, NoteId),
                     Exp = get_exp(C, TTL),
                     RExp = <<"Expiration: ", Exp/binary>>,
                     BGets = case is_integer(Gets) of true -> integer_to_binary(Gets); false -> <<"infinite">> end,
                     RGets = <<"Gets: ", BGets/binary>>,
                     [<<Message/binary>>, <<NoteId/binary>>, <<HostUrl/binary>>, RGets, RExp];
                 {error, missing_note} ->
                     [<<"MISSING NOTE">>, <<NoteId/binary>>, <<HostUrl/binary>>,<<"">>,<<"">>];
                 {error, {storage_error, E}} ->
                     [list_to_binary(E), <<NoteId/binary>>, <<HostUrl/binary>>,<<"">>,<<"">>]
             end,
    substitute(Subject, Targets, Values).



note_exists(Node, NoteId) ->
    case nook(Node, exists, NoteId) of
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


% nook wants the ID to be a string, not a binary.
to_list(<<_/binary>> = Id) ->
    binary_to_list(Id);

to_list([_|_] = Id) ->
    Id.


to_binary([_|_] = Id) ->
    list_to_binary(Id);

to_binary(<<_/binary>> = Id) ->
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
    Bin = proplists:get_value(Key, PropList),
    params_(string:to_lower(binary_to_list(Bin))).


params_("infinite") ->
    infinite;
params_(N) -> 
    list_to_integer(N).

    
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


exp_string(DateTime) ->
    list_to_binary(ec_date:format("D, j M Y G:i:s", DateTime) ++ " GMT").



get_exp(_Created, infinite) ->
    <<"never">>;

get_exp(Created, TTL) ->
    exp_string(calendar:gregorian_seconds_to_datetime(Created + TTL)).
