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

-export([store_object/3]).

-define(OBJECT_LOCATIONS_SET, <<"object_locations">>).
-define(OBJECT_INDEX, <<"object_index">>).

store_object(StoragePath = {storage_path, RawStoragePath},
             Object = {object, RawObject},
             Extents = {extents, RawExtents})
  when is_list(RawStoragePath), is_list(RawExtents), is_record(RawObject, xmlElement) ->
  
  % FIXME should only be allowed to server admins
  % TODO if exists, drop first
  
  RawStoragePathBin = list_to_binary(RawStoragePath),
  
  erldis_exec(
    fun(C) ->
      % store the object itself at its main storage path location
      erldis:hset(C, ?OBJECT_INDEX, RawStoragePathBin, term_to_binary(Object)),
      
      % map all extents StoragePath
      lists:all(
        fun(RawExtent) ->
          RawExtentBin = list_to_binary(RawStoragePath ++ "/" ++ RawExtent),
          erldis:hset(C, ?OBJECT_INDEX, RawExtentBin, term_to_binary({storage_path_ref, RawStoragePath}))
        end,
      RawExtents),
      
      % add the object location to the global set (so objects can be enumerated)
      erldis:sadd(C, ?OBJECT_LOCATIONS_SET, term_to_binary({StoragePath, Extents}))
    end),
    
  ok.

%% TODO delete_object (drop history and watches too)
%% TODO update_object (get values only)

%% Private functions
erldis_exec(StorageFun) when is_function(StorageFun, 1) ->
  erldis:exec(erldis_sup:client(), StorageFun).

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-endif.
