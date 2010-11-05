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
-define(OBIX_NS, "{http://obix.org/ns/schema/1.0}").

%% Runs all integration tests on localhost:8000
start() ->
  application:start(ibrowse),
  etap:plan(unknown),
  
  unauthenticated_tests(),
  
  etap:end_tests(),
  init:stop(),
  ok.

unauthenticated_tests() ->
  etap:msg("\nUnauthenticated Tests --------------------"),
  bad_uri_err(),
  get_lobby(),
  get_about(),
  ok.

bad_uri_err() ->
  etap:msg("\nBad Uri Err Tests"),
  bad_uri_err(?OBIX_SERVER_URL ++ "/_bad_uri_/"),
  ok.
  
bad_uri_err(ErrUrl) ->
  err(ErrUrl, "obix:BadUriErr", 'get'),
  ok.
  
bad_method_err(ErrUrl) ->
  err(ErrUrl, "obix:UnsupportedErr", delete),
  ok.
  
err(ErrUrl, ErrCode, HttpMethod) ->
  Result = ibrowse:send_req(ErrUrl, [], HttpMethod),
  ErrUrlDom = ensure_valid_obix_response(Result),
  ensure_href(ErrUrl, ErrUrlDom),
  ensure_obj_of_type("err", ErrCode, ErrUrlDom),
  ok.
  
get_lobby() ->
  etap:msg("\nLobby Tests"),
  LobbyUrl = ?OBIX_SERVER_URL ++ "/",
  Result = ibrowse:send_req(LobbyUrl, [], get),
  LobbyDom = ensure_valid_obix_response(Result),
  ensure_href(LobbyUrl, LobbyDom),
  ensure_obj_of_type("obj", "obix:Lobby", LobbyDom),
  
  {_, _, [AboutRef, ObjectsRef]} = LobbyDom,
  
  ensure_href("about/", AboutRef),
  ensure_obj_of_type("ref", "obix:About", AboutRef),
  
  ensure_href("objects/", ObjectsRef),
  ensure_obj_of_type("ref", ObjectsRef),
  
  bad_method_err(LobbyUrl),
  ok.

get_about() ->
  etap:msg("\nAbout Tests"),
  AboutUrl = ?OBIX_SERVER_URL ++ "/about/",
  Result = ibrowse:send_req(AboutUrl, [], get),
  AboutDom = ensure_valid_obix_response(Result),
  ensure_href(AboutUrl, AboutDom),
  ensure_obj_of_type("obj", "obix:About", AboutDom),
  
  {_, _, [ObixVersionStr|_]} = AboutDom,
  ensure_href("obixVersion/", ObixVersionStr),
  ensure_obj_of_type("str", ObixVersionStr),
  ensure_value("1.0", ObixVersionStr),
  
  ObixVersionUrl = AboutUrl ++ "obixVersion/",
  Result2 = ibrowse:send_req(ObixVersionUrl, [], get),
  ObixVersionDom = ensure_valid_obix_response(Result2),
  ensure_href(ObixVersionUrl, ObixVersionDom),
  ensure_obj_of_type("str", ObixVersionDom),
  ensure_value("1.0", ObixVersionDom),
  
  bad_method_err(AboutUrl),
  bad_uri_err(AboutUrl ++ "bad_extent/"),
  ok.

% TODO test /objects then get one object and its extents
  
ensure_valid_obix_response({_, _, Headers, Body}) ->
  etap:is(proplists:get_value("Content-Type", Headers),
          "text/xml",
          "response has 'text/xml' content type"),
  
  % validate against schema
  BaseDir = erobix_deps:get_base_dir(?MODULE),
  {ok, Model} = erlsom:compile_xsd_file(BaseDir ++ "/priv/xsd/obix.xsd"),
  {ok, _, _} = erlsom:scan(Body, Model),
  
  % export a simple form DOM
  {ok, Dom, _} = erlsom:simple_form(Body),
  
  % ensure root element in obix namespace
  {RootElement, _, _} = Dom,
  etap:is(1, string:str(RootElement, ?OBIX_NS), "correct namespace"),
  Dom.

ensure_obj_of_type(ExpectedElement, Actual) ->
  ensure_obj_of_type(ExpectedElement, undefined, Actual),
  ok.

ensure_obj_of_type(ExpectedElement,
                   ExpectedType,
                   {ActualElement = ?OBIX_NS ++ ExpectedElement, Attributes, _}) ->
                   
  etap:is(proplists:get_value("is", Attributes),
          ExpectedType,
          "@is is correct on " ++ ActualElement),
  ok;
ensure_obj_of_type(ExpectedElement, _, Actual) ->
  etap:is(ExpectedElement, Actual, "correct xml element"),
  ok.

ensure_href(ExpectedHref, {ActualElement, Attributes, _}) ->
  etap:is(proplists:get_value("href", Attributes),
          ExpectedHref,
          "@href is correct on " ++ ActualElement),
  ok.  

ensure_value(ExpectedValue, {ActualElement, Attributes, _}) ->
  etap:is(proplists:get_value("val", Attributes),
          ExpectedValue,
          "@val is correct on " ++ ActualElement),
  ok.

