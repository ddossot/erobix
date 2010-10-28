%%%
%%% @doc Object index gen_server.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_index_server).
-author('David Dossot <david@dossot.net>').

-behaviour(gen_server).
-include("erobix.hrl").
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%% Server functions
init([]) ->
    ?log_info("started", []),
    {ok, #state{}}.
  
handle_call(_Request, _From, State) ->
    ?unexpected_call(handle_call, [_Request, _From]),
    {reply, {error, {no_matching_handle_call_clause, _Request}}, State}.

handle_cast(_Msg, State) ->
    ?unexpected_call(handle_cast, [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    ?unexpected_call(handle_info, [_Info]),
    {noreply, State}.
  
terminate(normal, _State) ->
    ?log_info("stopped normally", []),
    ok;
  
terminate(shutdown, _State) ->
    ?log_info("shutdown", []),
    ok;
  
terminate(Reason, _State) ->
    ?unexpected_call(terminate, [Reason]),
    ok.
  
code_change(_OldVsn, State, _Extra) ->
    ?unexpected_call(code_change, [_OldVsn, _Extra]),
    {ok, State}.

