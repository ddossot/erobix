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

-export([build_xml_response/2, normalize_xml/2]).

-define(URI_SCHEME_SUFFIX, "://").

build_xml_response(Req, Data) when is_tuple(Data) ->
  do_build_xml_response(get_url(Req), Data).
  
%% Private functions
get_url(Req) ->
  atom_to_list(Req:get(scheme)) ++ ?URI_SCHEME_SUFFIX ++ Req:get_header_value("host") ++ Req:get(path).
 
normalize_xml(RequestUrl, Xml) when is_list(RequestUrl), is_list(Xml) ->
  % TODO support inheritance flattening (6.6.1)
  {Doc, _} = xmerl_scan:string(Xml),
  Normalized = normalize_xml(RequestUrl, Doc),
  export_xml(Normalized);
normalize_xml(RequestUrl,
              Element = #xmlElement{name = Name,
                                    namespace = #xmlNamespace{default = ?OBIX_NAMESPACE},
                                    attributes = Attributes,
                                    content = Content})
  when is_list(RequestUrl), Name =/= ref ->
  
  Element#xmlElement{attributes = [normalize_attribute(RequestUrl, A) || A <- Attributes],
                     content = [normalize_xml(RequestUrl, C) || C <- Content]};
normalize_xml(RequestUrl,
              Element = #xmlElement{content = Content})
  when is_list(RequestUrl) ->
  Element#xmlElement{content = [normalize_xml(RequestUrl, C) || C <- Content]};
normalize_xml(RequestUrl, Other) when is_list(RequestUrl) ->
  Other.

normalize_attribute(RequestUrl,
                    Attribute = #xmlAttribute{name = href,
                                              value = Value})
  when is_list(RequestUrl) ->
  
  Attribute#xmlAttribute{value = normalize_url(RequestUrl, Value)};
normalize_attribute(_, Attribute) ->
  Attribute.
  
normalize_url(RequestUrl, UrlToNormalize)
  when is_list(RequestUrl), is_list(UrlToNormalize) ->
  
  normalize_url(ensure_trailing_slash(RequestUrl),
                ensure_trailing_slash(UrlToNormalize),
                uri_type(UrlToNormalize)).
  
normalize_url(RequestUrl, UrlToNormalize, globally_absolute) ->
  case string:str(UrlToNormalize, RequestUrl) of
    0 ->
      UrlToNormalize;
    Index ->
      string:substr(UrlToNormalize, Index + string:len(RequestUrl))
  end;

normalize_url(RequestUrl, UrlToNormalize, server_absolute) ->
  % keep only scheme and host port
  {Scheme, Netloc, _, Query, Fragment} = mochiweb_util:urlsplit(RequestUrl),
  ResultUrl = mochiweb_util:urlunsplit({Scheme, Netloc,  UrlToNormalize, Query, Fragment}),
  % renormalize the newly formed global URL
  normalize_url(RequestUrl, ResultUrl, globally_absolute);

normalize_url(RequestUrl, UrlToNormalize, backup) ->
  {Scheme, Netloc, Path, Query, Fragment} = mochiweb_util:urlsplit(RequestUrl),
  
  BackedUpPath =
    case mochiweb_util:safe_relative_path(string:strip(Path, left, $/) ++ UrlToNormalize) of
      undefined ->
        throw_bad_uri(UrlToNormalize);
      Other ->
        Other
    end,
    
  ResultUrl = mochiweb_util:urlunsplit({Scheme, Netloc, "/" ++ BackedUpPath, Query, Fragment}),
  
  % renormalize the newly formed global URL
  normalize_url(RequestUrl, ResultUrl, globally_absolute);

normalize_url(_, UrlToNormalize, relative) ->
  UrlToNormalize;

normalize_url(_, UrlToNormalize, _) ->
  throw_bad_uri(UrlToNormalize).

throw_bad_uri(Uri) ->
  throw({unsupported_uri, Uri}).
  
uri_type(Uri) when is_list(Uri) ->
  % TODO support fragment identifier
  case string:str(Uri, ?URI_SCHEME_SUFFIX) of
    0 ->
      case string:left(Uri, 2) of
        [$/|_] -> server_absolute;
        ".."   -> backup;
        "./"   -> unsupported;
        _      -> relative
      end;
      
    _ ->
      globally_absolute
  end.

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
                  
  export_xml(ResponseData).

export_xml(Document) ->
  xmerl:export_simple([Document], xmerl_xml).
  
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
  ?assertEqual("http://foo/baz/", normalize_url("http://foo/bar/", "../baz/")),
  ?assertEqual("http://foo/baz/", normalize_url("http://foo/bar/", "/baz")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "/bar/baz")),
  ?assertEqual("baz/", normalize_url("http://foo/bar/", "/bar/baz/")),
  ?assertThrow({unsupported_uri, "./baz/"}, normalize_url("http://foo/bar/", "./baz/")),
  ?assertThrow({unsupported_uri, "../../baz/"}, normalize_url("http://foo/bar/", "../../baz/")),
  ok.
  
normalize_xml_test() ->
  ?assertEqual("<?xml version=\"1.0\"?><obj href=\"bar/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>",
               lists:flatten(normalize_xml("http://data/foo", "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"))),
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
