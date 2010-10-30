%%%
%%% @doc Web request router for erobix.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_router).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([handle/2]).

handle(Req, _DocRoot) ->
  Path = Req:get(path),
  
  case string:tokens(Path, "/") of
    [?OBIX_BASE_PATH|Rest] ->
      handle_obix(Req, Rest);
    
    _ ->
      {error, bad_request}
  end.

%% Handling of /obix/*
handle_obix(Req, PathElements) ->
  % TODO accept only text/xml for PUT/POST
  % TODO support PUT for writing any object with an href and writable=true
  % TODO support POST for invoking any op object

  case PathElements of
    [] ->
      erobix_lobby:serve(Req);
      
    ["about" | _] ->
      % FIXME about should not be cached
      % FIXME extract / refactor this, which should be handled by the index server
      case Req:get(method) of
        'GET' ->
          StoragePath = {storage_path, string:join(PathElements, "/") ++ "/"},
          
          case erobix_store:get_object(StoragePath) of
            Object = {object, _} ->
              erobix_lib:render_object_xml({url, erobix_lib:get_url(Req)}, Object);
              
            {Object = {object, _}, Extent = {extent, _}} ->
              erobix_lib:render_object_xml({url, erobix_lib:get_url(Req)}, Object, Extent);
              
            Error ->
              Error
          end;
          
        _ ->
          {error, bad_request}
      end;
      
    _ ->
      {error, not_found}
  end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
