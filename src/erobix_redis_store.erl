%%%
%%% @doc Redis backed storage.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_redis_store).
-author('David Dossot <david@dossot.net>').

-include_lib("xmerl/include/xmerl.hrl").
-include("erobix.hrl").

-export([start/0, stop/0,
         store_object_def/3, get_object_def/1, get_all_object_defs/0,
         get_object_value/2]).

-define(OBJECT_DEFS, <<"object_defs">>).
-define(OBJECT_DATA, <<"object_data">>).

start() ->
  application:start(erldis),
  ?log_info("started, Redis info: ~1024p", [erldis:info(erldis_client())]).

stop() ->
  application:stop(erldis).

store_object_def({url, RawServerUrl},
                 StoragePath = {storage_path, RawStoragePath},
                 ObjectXml = {xml, RawObjectXml})
  when is_list(RawServerUrl), is_list(RawStoragePath), is_list(RawObjectXml) ->

  RawServerUrlS = erobix_lib:ensure_trailing_slash(RawServerUrl),
  RawStoragePathS = erobix_lib:ensure_trailing_slash(RawStoragePath),
  
  {xml, RawNormalizedObjectXml} =
    erobix_lib:normalize_object_xml({url, RawServerUrlS ++ RawStoragePathS}, ObjectXml),
  
  erldis:hset(erldis_client(),
              ?OBJECT_DEFS,
              get_object_def_path(StoragePath),
              list_to_binary(RawNormalizedObjectXml)),
              
  Object = erobix_lib:parse_object_xml(RawNormalizedObjectXml),
  {writable_extents, RawWritableExtents} = erobix_lib:get_writable_extents(Object),
  % FIXME store object values
  ok.

% TODO delete_object_def, dropping data history and watches 

get_object_def(StoragePath = {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  case erldis:hget(erldis_client(),
                   ?OBJECT_DEFS,
                   get_object_def_path(StoragePath)) of
    nil ->
      {error, not_found};

    RawObjectBin ->
      {xml, binary_to_list(RawObjectBin)}
  end.
  
get_all_object_defs() ->
  get_all_object_defs(erldis:hgetall(erldis_client(), ?OBJECT_DEFS), []).
  
get_all_object_defs([], Acc) ->
  lists:reverse(Acc);

get_all_object_defs([{RawStoragePathBin, RawObjectBin}|Rest], Acc) ->
  get_all_object_defs(
    Rest,
    [{{storage_path, binary_to_list(RawStoragePathBin)}, {xml, binary_to_list(RawObjectBin)}} | Acc]).

store_object_value(StoragePath = {storage_path, RawStoragePath}, Extent = {extent, RawExtent}, {value, RawValue})
  when is_list(RawStoragePath), is_list(RawExtent), is_list(RawValue) ->
  
  erldis:hset(erldis_client(),
              ?OBJECT_DATA,
              get_object_value_path(StoragePath, Extent),
              list_to_binary(RawValue)).
    
get_object_value(StoragePath = {storage_path, RawStoragePath}, Extent = {extent, RawExtent})
  when is_list(RawStoragePath), is_list(RawExtent) ->
  
  case erldis:hget(erldis_client(),
                   ?OBJECT_DATA,
                   get_object_value_path(StoragePath, Extent)) of
    nil ->
      {error, not_found};

    RawDataBin ->
      {value, binary_to_list(RawDataBin)}
  end.
    
%% Private functions
erldis_client() ->
  erldis_sup:client().
  
get_object_def_path({storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  list_to_binary(erobix_lib:ensure_trailing_slash(RawStoragePath)).

get_object_value_path({storage_path, RawStoragePath}, {extent, RawExtent})
  when is_list(RawStoragePath), is_list(RawExtent) ->
  
  list_to_binary(erobix_lib:ensure_trailing_slash(RawStoragePath) ++ RawExtent).

