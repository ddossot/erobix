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
  
  etap:plan(unknown),
  
  unauthenticated_tests(),
  
  etap:end_tests(),
  
  init:stop(),
  ok.

unauthenticated_tests() ->
  etap:msg("Unauthenticated Tests"),
  get_lobby(),
  get_about(),
  ok.

get_lobby() ->
  etap:msg("Lobby Tests"),
  LobbyHref = ?OBIX_SERVER_URL ++ "/",
  Result = ibrowse:send_req(LobbyHref, [], get),
  LobbyDom = ensure_valid_obix_response(Result),
  
  ensure_href(LobbyHref, LobbyDom),
  ensure_obj_of_type("obj", "obix:Lobby", LobbyDom),
  
  {_, _, [AboutRef]} = LobbyDom,
  ensure_href("about/", AboutRef),
  ensure_obj_of_type("ref", "obix:About", AboutRef),
  ok.

get_about() ->
  etap:msg("About Tests"),
  AboutHref = ?OBIX_SERVER_URL ++ "/about/",
  Result = ibrowse:send_req(AboutHref, [], get),
  AboutDom = ensure_valid_obix_response(Result),
  
  ensure_href(AboutHref, AboutDom),
  ensure_obj_of_type("obj", "obix:About", AboutDom),
  
  {_, _, [ObixVersionStr|_]} = AboutDom,
  ensure_href("obixVersion/", ObixVersionStr),
  ensure_obj_of_type("str", ObixVersionStr),
  ok.

ensure_valid_obix_response({_, _, Headers, Body}) ->
  etap:is(proplists:get_value("Content-Type", Headers),
          "text/xml",
          "Obix response has 'text/xml' content type"),
  
  % validate against schema
  BaseDir = erobix_deps:get_base_dir(?MODULE),
  {ok, Model} = erlsom:compile_xsd_file(BaseDir ++ "/priv/xsd/obix.xsd"),
  {ok, _, _} = erlsom:scan(Body, Model),
  
  % return a simple form DOM
  {ok, Dom, _} = erlsom:simple_form(Body),
  Dom.

ensure_href(ExpectedHref, {ActualElement, Attributes, _}) ->
  etap:is(proplists:get_value("href", Attributes),
          ExpectedHref,
          "@href is correct on " ++ ActualElement),
  ok.  

ensure_obj_of_type(ExpectedElement, Actual) ->
  ensure_obj_of_type(ExpectedElement, undefined, Actual),
  ok.

ensure_obj_of_type(ExpectedElement,
                   ExpectedType,
                   {ActualElement = "{http://obix.org/ns/schema/1.0}" ++ ExpectedElement, Attributes, _}) ->
                   
  etap:is(proplists:get_value("is", Attributes),
          ExpectedType,
          "@is is correct on " ++ ActualElement),
  ok;
ensure_obj_of_type(ExpectedElement, _, Actual) ->
  etap:is(ExpectedElement, Actual, "correct xml element"),
  ok.

