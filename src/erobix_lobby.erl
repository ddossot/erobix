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
      ?log_info("Req: ~1024p", [erobix_lib:base_url(Req)]),
      {data, {obj, [{href, erobix_lib:base_url(Req) ++ "/"}], []}};

    _ ->
      {error, bad_request}
  end.

