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
      
    ["about" | RawExtent] ->
      erobix_about:serve(Req, {extent, string:join(RawExtent, "/")});

    ["objects" | _] ->
      StoragePath = {storage_path, string:join(PathElements, "/")},
      erobix_object_server:serve(Req, StoragePath);
      
    _ ->
      {error, not_found}
  end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
