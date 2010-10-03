%%%
%%% @doc About service.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_about).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([serve/1, as_ref/0]).

serve(Req) ->
  Method = Req:get(method),
  
  case Method of
    'GET' ->
      {data, {obj, attributes(), children()}};

    _ ->
      {error, bad_request}
  end.

as_ref() ->
  % TODO centralize refs and use in router
  % TODO compute href based on current url
  {ref, [{name, "about"}, {href, "about/"} | attributes()], children()}.
  
%% Private function
attributes() ->
  [{is, "obix:About"}, {display, "Obix About"}].
  
children() ->
  % TODO implement
  [].

