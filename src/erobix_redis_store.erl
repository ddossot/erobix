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
         store_value/4, get_latest_value/2]).

-define(OBJECT_DEFS, <<"erobix:object_defs">>).
-define(OBJECT_DATA, <<"erobix:object_data">>).

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
  ok.

% TODO delete_object_def, dropping data history? watches alarms... 

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

store_value(StoragePath = {storage_path, RawStoragePath}, Extent = {extent, RawExtent},
            TimeStampUtc = {timestamp_utc, {_Date, _Time}}, Value = {value, RawValue})
  when is_list(RawStoragePath), is_list(RawExtent), is_list(RawValue) ->
  
  erldis:lpush(erldis_client(),
               get_object_value_path(StoragePath, Extent),
               term_to_binary({TimeStampUtc, Value})).
    
get_latest_value(StoragePath = {storage_path, RawStoragePath}, Extent = {extent, RawExtent})
  when is_list(RawStoragePath), is_list(RawExtent) ->
  
  case erldis:lrange(erldis_client(),
                     get_object_value_path(StoragePath, Extent),
                     0,
                     0) of
                     
    [RawDataBin] ->
      binary_to_term(RawDataBin);
      
    _ ->
      {error, not_found}
  end.
    
%% Private functions
erldis_client() ->
  erldis_sup:client().
  
get_object_def_path({storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  list_to_binary(erobix_lib:ensure_trailing_slash(RawStoragePath)).

get_object_value_path({storage_path, RawStoragePath}, {extent, RawExtent})
  when is_list(RawStoragePath), is_list(RawExtent) ->
  
  SubPath = list_to_binary(erobix_lib:ensure_trailing_slash(RawStoragePath) ++ RawExtent),
  <<?OBJECT_DATA/binary, ":", SubPath/binary>>.

