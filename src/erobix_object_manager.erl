%%%
%%% @doc Object object manager gen_server.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_object_manager).
-author('David Dossot <david@dossot.net>').

-behaviour(gen_server).
-include("erobix.hrl").
-define(SERVER, ?MODULE).

-export([serve/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

serve(Req, StoragePath = {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  Method = Req:get(method),
  
  case Method of
    'GET' when RawStoragePath =:= "objects" ->
      list_all_objects(Req);

    'GET' when RawStoragePath =/= "" ->
      get_object(Req, StoragePath);

    _ ->
      {error, bad_request}
  end.

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

%% Private functions
list_all_objects(Req) ->
  % FIXME turn this into a gen_server function
  % trim "objects/" out of storage path
  ObjectRefs =
    [{ref, [{href, string:substr(RawStoragePath, 9)} | erobix_lib:get_object_names(Object)], []}
     || {{storage_path, RawStoragePath}, Object} <- erobix_store:get_all_objects()],
     
  Url = erobix_lib:get_url(Req),
  erobix_lib:build_xml_response(Url,
                                list,
                                [{displayName, "Object List"}, {'of', "obix:ref"}],
                                ObjectRefs).

get_object(Req, StoragePath) ->
  case erobix_store:get_object(StoragePath) of
    Object = {object, _} ->
      erobix_lib:render_object_xml({url, erobix_lib:get_url(Req)}, Object);
      
    {Object = {object, _}, Extent = {extent, _}} ->
      erobix_lib:render_object_xml({url, erobix_lib:get_url(Req)}, Object, Extent);
      
    Error ->
      Error
  end.

