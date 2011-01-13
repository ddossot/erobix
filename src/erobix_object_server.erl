%%%
%%% @doc Object server.
%%%      Serves Obix objects as XML entities. 
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_object_server).
-author('David Dossot <david@dossot.net>').

-behaviour(gen_server).
-include("erobix.hrl").
-define(SERVER, ?MODULE).

-export([start_link/0, serve/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {objects_and_refs_dict}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

serve(Req, {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  Method = Req:get(method),
  Url = erobix_lib:get_url(Req),
  
  case Method of
    'GET' when RawStoragePath =:= "objects" ->
      gen_server:call(?SERVER, {list_all_objects, Url});

    'GET' when RawStoragePath =/= "" ->
      gen_server:call(?SERVER,
                      {get_object,
                       Url,
                       {storage_path, erobix_lib:ensure_trailing_slash(RawStoragePath)}
                      });

    _ ->
      {error, bad_request}
  end.

%% Server functions
init([]) ->
  Store = erobix:get_store(),
  % FIXME load object definitions from the file system
  AllObjectDefs = Store:get_all_object_defs(),
  ObjectsAndRefsDict = parse_object_defs(AllObjectDefs),
  ?log_info("started with ~p object definitions", [length(AllObjectDefs)]),
  {ok, #state{objects_and_refs_dict=ObjectsAndRefsDict}}.

handle_call({get_object, Url, StoragePath}, _From, State = #state{objects_and_refs_dict=ObjectsAndRefsDict}) ->
  % FIXME get and merge values in object
  % FIXME fork a process to build the response
  Response =
    case get_object(StoragePath, ObjectsAndRefsDict) of
      Object = {object, _} ->
        erobix_lib:render_object_xml(Url, Object);
        
      {Object = {object, _}, Extent = {extent, _}} ->
        erobix_lib:render_object_xml(Url, Object, Extent);
        
      Error = {error, _} ->
        Error;
      
      Other ->
        ?log_error("Unexpected get_object result: ~1024p", [Other]),
        {error, server_error}
    end,
    
  {reply, Response, State};

handle_call({list_all_objects, Url}, _From, State = #state{objects_and_refs_dict=ObjectsAndRefsDict}) ->
  ObjectRefs =
    [{ref, [{href, rel_url(RawStoragePath)} | erobix_lib:get_object_names(Object)], []}
     || {{storage_path, RawStoragePath}, Object} <- only_objects(ObjectsAndRefsDict)],
     
  Response =
    erobix_lib:build_xml_response(Url,
                                  list,
                                  [{displayName, "Object List"}, {'of', "obix:ref"}],
                                  ObjectRefs),
  {reply, Response, State};
  
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
parse_object_defs(AllObjectDefs) ->
  parse_object_defs(AllObjectDefs, dict:new()).

parse_object_defs([], Objects) ->
  Objects;
parse_object_defs([{StoragePath = {storage_path, RawStoragePath}, ObjectXml}|Rest], Objects) ->
  Object = erobix_lib:parse_object_xml(ObjectXml),
  {extents, RawExtents} = erobix_lib:get_all_extents(Object),
  
  NewObjects =
    lists:foldl(
      fun(RawExtent, Acc) ->
        dict:store({storage_path, RawStoragePath ++ RawExtent},
                   {storage_path_ref, RawExtent, RawStoragePath},
                   Acc)
      end,
      dict:store(StoragePath, Object, Objects),
      RawExtents),
  
  parse_object_defs(Rest, NewObjects).

only_objects(ObjectsAndRefsDict) ->
  only_objects(dict:to_list(ObjectsAndRefsDict), []).
only_objects([], Result) ->
  Result;
only_objects([SP_O = {_, {object, _}}|Rest], Result) ->
  only_objects(Rest, [SP_O | Result]);
only_objects([_|Rest], Result) ->
  only_objects(Rest, Result).

get_object(StoragePath, ObjectsAndRefsDict) ->
  case dict:find(StoragePath, ObjectsAndRefsDict) of
    error ->
      {error, not_found};
    
    {ok, {storage_path_ref, RawExtent, RawStoragePath}} ->
      {get_object({storage_path, RawStoragePath}, ObjectsAndRefsDict), {extent, RawExtent}};
    
    {ok, Object} ->
      Object
  end.

rel_url(RawStoragePath) ->
  % trim "objects/" out of storage path
  string:substr(RawStoragePath, 9).

% TODO add unit tests
