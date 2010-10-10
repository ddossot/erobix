%%%
%%% @doc Integration tests for erobix.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_int_tests).
-author('David Dossot <david@dossot.net>').

-export([start/0]).

-define(OBIX_SERVER_URL, "http://localhost:8000/obix").

%% Runs all integration tests on localhost:8000
start() ->
  application:start(ibrowse),
  unauthenticated_tests(),
  application:stop(ibrowse),
  init:stop(),
  ok.

unauthenticated_tests() ->
  get_lobby(),
  ok.
  
get_lobby() ->
  LobbyHref = ?OBIX_SERVER_URL ++ "/",
  Result = ibrowse:send_req(LobbyHref, [], get),
  LobbyDom = ensure_valid_obix_response(Result),
  
  ensure_href(LobbyHref, LobbyDom),
  ensure_obj_of_type("obix:Lobby", LobbyDom),
  
  % TODO test lobby specific child elements
  io:format("~n~1024p~n", [LobbyDom]),
  
  io:format("(Unauth) Lobby: ok~n"),
  ok.

ensure_valid_obix_response({_, _, Headers, Body}) ->
  % TODO use etap
  "text/xml" = proplists:get_value("Content-Type", Headers),
  
  % validate against schema
  BaseDir = erobix_deps:get_base_dir(?MODULE),
  {ok, Model} = erlsom:compile_xsd_file(BaseDir ++ "/priv/xsd/obix.xsd"),
  {ok, _, _} = erlsom:scan(Body, Model),
  
  % return a simple form DOM
  {ok, Dom, _} = erlsom:simple_form(Body),
  Dom.

ensure_href(ExpectedHref, {_, Attributes, _}) ->
  % TODO use etap
  ExpectedHref = proplists:get_value("href", Attributes),
  ok.  
  
ensure_obj_of_type(ExpectedType, {"{http://obix.org/ns/schema/1.0}obj", Attributes, _}) ->
  % TODO use etap
  ExpectedType = proplists:get_value("is", Attributes),
  ok.

