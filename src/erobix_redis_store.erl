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
         store_object_def/3, get_object_def/1, get_all_object_defs/0]).

-define(OBJECT_DEFS, <<"object_defs">>).

start() ->
  application:start(erldis),
  ?log_info("started, Redis info: ~1024p", [erldis:info(erldis_client())]).

stop() ->
  application:stop(erldis).

store_object_def({url, RawServerUrl},
                 {storage_path, RawStoragePath},
                 ObjectXml = {xml, RawObjectXml})
  when is_list(RawServerUrl), is_list(RawStoragePath), is_list(RawObjectXml) ->

  RawServerUrlS = erobix_lib:ensure_trailing_slash(RawServerUrl),
  RawStoragePathS = erobix_lib:ensure_trailing_slash(RawStoragePath),
  
  {xml, RawNormalizedObjectXml} =
    erobix_lib:normalize_object_xml({url, RawServerUrlS ++ RawStoragePathS}, ObjectXml),
  
  erldis:hset(erldis_client(),
              ?OBJECT_DEFS,
              list_to_binary(RawStoragePathS),
              list_to_binary(RawNormalizedObjectXml)),
  ok.

% TODO delete_object_def, dropping history and watches 

get_object_def({storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  RawStoragePathBin = list_to_binary(erobix_lib:ensure_trailing_slash(RawStoragePath)),
  
  case erldis:hget(erldis_client(), ?OBJECT_DEFS, RawStoragePathBin) of
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

%% Private functions
erldis_client() ->
  erldis_sup:client().

