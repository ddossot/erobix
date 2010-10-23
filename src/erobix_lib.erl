%%%
%%% @doc Utility methods.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_lib).
-author('David Dossot <david@dossot.net>').

-include_lib("xmerl/include/xmerl.hrl").
-include("erobix.hrl").

-export([build_xml_response/2]).

build_xml_response(Req, Data) when is_tuple(Data) ->
  do_build_xml_response(get_url(Req), Data).
  
%% Private functions
get_url(Req) ->
  atom_to_list(Req:get(scheme)) ++ "://" ++ Req:get_header_value("host") ++ Req:get(path).

normalize_url(RequestUrl, UrlToNormalize) when is_list(RequestUrl), is_list(UrlToNormalize) ->
  SlashedRequestUrl = ensure_trailing_slash(RequestUrl),
  ValidatedUrl = validate_url(UrlToNormalize),
  
  RelativizedUrl = 
    case string:str(ValidatedUrl, SlashedRequestUrl) of
      0 ->
        ValidatedUrl;
      Index ->
        string:substr(ValidatedUrl, Index + string:len(SlashedRequestUrl))
    end,
    
  ensure_trailing_slash(RelativizedUrl).
  
validate_url(Url = [$/|_]) ->
  throw({unsupported_uri, Url});
validate_url(Url) when is_list(Url) ->
  case string:left(Url, 2) of
    ".." ->
      throw({unsupported_uri, Url});
    "./" ->
      string:substr(Url, 3);
    _ ->
      Url
  end.

normalize_xml(RequestUrl, Xml) when is_list(RequestUrl), is_list(Xml) ->
  % TODO implement
  ok.
  
do_build_xml_response(Url, {ElementName, Attributes, Children}) when is_list(Url) ->

  ResponseData = {ElementName,
                  [
                   {href, ensure_trailing_slash(Url)},
                   {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"},
                   {'xsi:schemaLocation', "http://obix.org/ns/schema/1.0"},
                   {xmlns, "http://obix.org/ns/schema/1.0"}
                   |Attributes
                  ],
                  Children},
                  
  xmerl:export_simple([ResponseData], xmerl_xml).

ensure_trailing_slash(Url) when is_list(Url) ->
  lists:reverse(ensure_leading_slash(lists:reverse(Url))).

ensure_leading_slash(Url = [$/|_]) ->
  Url;
ensure_leading_slash(Url) when is_list(Url) ->
  [$/|Url].

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

normalize_url_test() ->
  ?assertEqual("http://other/path/", normalize_url("http://foo/bar", "http://other/path")),
  ?assertEqual("baz/", normalize_url("http://foo/bar", "http://foo/bar/baz")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "http://foo/bar/baz")),
  ?assertEqual("baz/", normalize_url("http://foo/bar", "http://foo/bar/baz/")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "http://foo/bar/baz/")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "baz")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "baz/")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "./baz/")),
  ?assertThrow({unsupported_uri, "../baz/"}, normalize_url("http://foo/bar/", "../baz/")),
  ?assertThrow({unsupported_uri, "/baz"}, normalize_url("http://foo/bar/", "/baz")),
  ok.

ensure_trailing_slash_test() ->
  ?assertEqual("/", ensure_trailing_slash("")),
  ?assertEqual("/", ensure_trailing_slash("/")),
  ?assertEqual("a/", ensure_trailing_slash("a")),
  ?assertEqual("/a/", ensure_trailing_slash("/a")),
  ?assertEqual("/a/", ensure_trailing_slash("/a/")),
  ok.

ensure_leading_slash_test() ->
  ?assertEqual("/", ensure_leading_slash("")),
  ?assertEqual("/", ensure_leading_slash("/")),
  ?assertEqual("/a", ensure_leading_slash("a")),
  ?assertEqual("/a", ensure_leading_slash("/a")),
  ?assertEqual("/a/", ensure_leading_slash("/a/")),
  ok.

do_build_xml_response_test() ->
  ?assertEqual("<?xml version=\"1.0\"?><obj href=\"fake://url/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>",
               lists:flatten(do_build_xml_response("fake://url", {obj, [], []}))),
  ?assertEqual("<?xml version=\"1.0\"?><obj href=\"fake://url/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>",
               lists:flatten(do_build_xml_response("fake://url/", {obj, [], []}))),
  ok.

-endif.
