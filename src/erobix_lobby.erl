%%%
%%% @doc Lobby service.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_lobby).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([serve/1]).

serve(Req) ->
  Method = Req:get(method),
  
  case Method of
    'GET' ->
      {data, {obj,attributes(), children()}};

    _ ->
      {error, bad_request}
  end.
  
%% Private function
attributes() ->
  [{is, "obix:Lobby"}].
  
children() ->
  % FIXME implement
  [erobix_about:as_ref()].

