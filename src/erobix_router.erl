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

% Handling of /obix/*
handle_obix(Req, PathElements) ->
  % TODO accept only text/xml for put/post
  case PathElements of
    [] ->
      erobix_lobby:serve(Req);
      
    ["about"] ->
      erobix_about:serve(Req);
    
    _ ->
      {error, not_found}
  end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
