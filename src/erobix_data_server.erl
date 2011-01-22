%%%
%%% @doc Object data server.
%%%      Babysits data for writable extents of a particular object. Unique accross a cluster. 
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_data_server).
-author('David Dossot <david@dossot.net>').

-behaviour(gen_server).
-include("erobix.hrl").

-export([start_link/2, get_values/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_name, store, object_def, extents_data_dict}).

start_link(ServerName, StoragePath = {storage_path, RawStoragePath})
  when is_atom(ServerName), is_list(RawStoragePath) ->
  
  gen_server:start_link({global, ServerName}, ?MODULE, [ServerName, StoragePath], []).

get_values(StoragePath = {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  
  _DataServer = get_data_server(StoragePath), 
  
  % FIXME implement a gen_call
  [].
  
%% Server functions
init([ServerName, StoragePath]) ->
  Store = erobix:get_store(),
  ObjectXml = Store:get_object_def(StoragePath),
  Object = erobix_lib:parse_object_xml(ObjectXml),
  {writable_extents, RawWritableExtents} = erobix_lib:get_writable_extents(Object),
  
  ExtentsData =
    lists:foldl(
      fun(RawWritableExtent, Dict) ->
        WritableExtent = {extent, RawWritableExtent},
        
        case Store:get_latest_value(StoragePath, WritableExtent) of
          {error, _} ->
            dict:store(WritableExtent,
                       {calendar:universal_time(), {value, erobix_lib:get_object_value(Object, WritableExtent, "")}},
                       Dict);
                       
          TimeStampUtcAndValue ->
            dict:store(WritableExtent,
                       TimeStampUtcAndValue,
                       Dict)
        end
      end,
      dict:new(),
      RawWritableExtents),
  
  ?log_info("started ~p with ~p writable extents", [ServerName, dict:size(ExtentsData)]),
  {ok, #state{server_name=ServerName, store=Store, object_def=Object, extents_data_dict=ExtentsData}}.

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

%% Private functions
get_server_name({storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  list_to_atom("data_server://" ++ RawStoragePath).
  
get_data_server(StoragePath = {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  ServerName = get_server_name(StoragePath),
  
  case global:whereis_name(ServerName) of
    undefined ->
      supervisor:start_child(erobix_data_sup, [ServerName, StoragePath]),
      % in case another process registered the server in the meantime, try another global lookup
      global:whereis_name(ServerName);
      
    Pid ->
      Pid
  end.

% TODO add unit tests
