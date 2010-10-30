%%%
%%% @doc Storage methods.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_store).
-author('David Dossot <david@dossot.net>').

-include_lib("xmerl/include/xmerl.hrl").
-include("erobix.hrl").

-export([store_object/3, delete_object/1, get_object/1]).

-define(OBJECT_INDEX, <<"object_index">>).
-define(OBJECT_LOCATIONS, <<"object_locations">>).

store_object(StoragePath = {storage_path, RawStoragePath},
             Object = {object, RawObject},
             Extents = {extents, RawExtents})
  when is_list(RawStoragePath), is_list(RawExtents), is_record(RawObject, xmlElement) ->
  
  % drop the object first, as this function is about recreating an object not mutating its state
  delete_object(StoragePath),
  
  RawStoragePathBin = list_to_binary(RawStoragePath),
  C = erldis_client(),
  
  % store the object itself at its main storage path location
  erldis:hset(C, ?OBJECT_INDEX, RawStoragePathBin, term_to_binary(Object)),
  
  % map all extents StoragePath
  lists:foreach(
    fun(RawExtent) ->
      RawExtentBin = list_to_binary(RawExtent),
      erldis:hset(C,
                  ?OBJECT_INDEX,
                  <<RawStoragePathBin/binary, RawExtentBin/binary>>,
                  term_to_binary({{storage_path_ref, RawStoragePath}, {extent, RawExtent}}))
    end,
    RawExtents),
  
  % add the object location to the global set (so objects can be enumerated)
  erldis:hset(C, ?OBJECT_LOCATIONS, RawStoragePathBin, term_to_binary(Extents)),
    
  ok.

delete_object({storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  % TODO drop point history and watches 
  RawStoragePathBin = list_to_binary(RawStoragePath),
  C = erldis_client(),
  
  erldis:hdel(C, ?OBJECT_INDEX, RawStoragePathBin),
  
  case erldis:hget(C, ?OBJECT_LOCATIONS, RawStoragePathBin) of
    nil ->
      noop;
      
    SerializedExtents ->
      {extents, RawExtents} = binary_to_term(SerializedExtents),
      
      lists:foreach(
        fun(RawExtent) ->
          RawExtentBin = list_to_binary(RawExtent),
          erldis:hdel(C, ?OBJECT_INDEX, <<RawStoragePathBin/binary, RawExtentBin/binary>>)
        end,
        RawExtents),
      
      erldis:hdel(C, ?OBJECT_LOCATIONS, RawStoragePathBin)
  end,
  ok.

get_object(StoragePath = {storage_path, RawStoragePath}) when is_list(RawStoragePath) ->
  get_object(StoragePath, erldis_client()).
  
get_object({storage_path, RawStoragePath}, C) when is_list(RawStoragePath) ->
  RawStoragePathBin = list_to_binary(RawStoragePath),

  case erldis:hget(C, ?OBJECT_INDEX, RawStoragePathBin) of
    nil ->
      {error, not_found};

    SerializedData ->
      get_object(binary_to_term(SerializedData), C)
  end;

get_object(Object = {object, _}, _) ->
  Object;

get_object({{storage_path_ref, RawStoragePath}, Extent = {extent, RawExtent}}, C)
  when is_list(RawStoragePath), is_list(RawExtent) ->
  
  {get_object({storage_path, RawStoragePath}, C), Extent};

get_object(_, _) ->
  {error, invalid_storage_layout}.

%% TODO update_object (get values only)

%% Private functions
erldis_client() ->
  erldis_sup:client().

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-endif.
