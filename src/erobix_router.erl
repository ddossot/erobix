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
  % TODO enforce text/xml for put/post
  case PathElements of
    [] ->
      erobix_lobby:serve(Req);
    
    _ ->
      {error, bad_request}
  end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
