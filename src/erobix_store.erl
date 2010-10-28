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

-export([store_object/2]).

store_object(StoragePath, Object) when is_list(StoragePath), is_record(Object, xmlElement) ->
  % FIXME ensure StoragePath is allowed (here or at router level, with ACLs)
  % TODO consider using a global client
  erldis:exec(erldis_sup:client(),
              fun(C) ->
                erldis:set(C, StoragePath, Object),
                % TODO map all extent of sub paths to StoragePath
                erldis:hset(C, <<"object_index">>, StoragePath, StoragePath)
              end),
  
  % FIXME ensure no href attribute on root element
  ok.
  
%% Private functions

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-endif.
