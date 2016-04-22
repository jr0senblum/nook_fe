%%%-------------------------------------------------------------------
%% @doc nook_fe public API
%% @end
%%%-------------------------------------------------------------------

-module(nook_fe_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    Dispatch = 
        cowboy_router:compile([
                               {'_', [ 
                                       {"/[:note_id]", toppage_handler, []},
                                       {"/:new/:note_id", toppage_handler, []}
                                     ]}
                              ]),

    cowboy:start_http(http, 100, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    nook_fe_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
