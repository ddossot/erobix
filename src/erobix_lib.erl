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

-define(EXTENT_ATTRIBUTE_NAME, '_extent').

-export([build_xml_response/2, parse_object_xml/2]).

build_xml_response(Req, Data) when is_tuple(Data) ->
  do_build_xml_response(get_url(Req), Data).
  
%% Private functions
get_url(Req) ->
  mochiweb_util:urlunsplit({atom_to_list(Req:get(scheme)), Req:get_header_value("host"), Req:get(path), "", ""}).

%% FIXME render_object_xml (filter _extent out, xpath to get subpart, add top href)

parse_object_xml(RequestUrl, ObjectXml) when is_list(RequestUrl), is_list(ObjectXml) ->
  {ObjectDoc, _} = xmerl_scan:string(ObjectXml),
  NormalizedObjectDoc = normalize_object_xml(RequestUrl, ObjectDoc),
  
  Extents = [Value || #xmlAttribute{value = Value} <- find_all_extent_attributes(NormalizedObjectDoc)],
  io:format("~n~1024p~n", [Extents]),
  
  % FIXME return record, not Xml + Extents
  lists:flatten(export_xml(NormalizedObjectDoc)).
  
find_all_extent_attributes(NormalizedObjectDoc) ->
  xmerl_xpath:string("//node()/@" ++ atom_to_list(?EXTENT_ATTRIBUTE_NAME), NormalizedObjectDoc).

normalize_object_xml(RequestUrl, Doc) when is_list(RequestUrl) ->
  NoRootRefDoc = remove_root_href(Doc), 
  % TODO support inheritance flattening (6.6.1)
  normalize_object_xml(RequestUrl, "", NoRootRefDoc).


remove_root_href(Element = #xmlElement{attributes = Attributes}) ->
  FilteredAttributes =
    lists:filter(fun(#xmlAttribute{name = Name}) -> Name =/= href end,
                 Attributes),
    
  Element#xmlElement{attributes = FilteredAttributes}.


normalize_object_xml(RequestUrl,
                     Extent,
                     Element = #xmlElement{name = Name,
                                           namespace = #xmlNamespace{default = ?OBIX_NAMESPACE},
                                           attributes = Attributes,
                                           content = Content})
  when is_list(RequestUrl), is_list(Extent), Name =/= ref ->
  
  {NormalizedHrefs, NormalizedAttributes} =
    lists:unzip([normalize_attribute(RequestUrl ++ Extent, A) || A <- Attributes]),
  
  case lists:filter(fun(E) -> E =/= undefined end, NormalizedHrefs) of
    [] ->
        Element#xmlElement{attributes = NormalizedAttributes,
                           content = [normalize_object_xml(RequestUrl, Extent, C) || C <- Content]};
                   
    [NormalizedHref|_] ->
        NewExtent = Extent ++ NormalizedHref,
        Element#xmlElement{attributes = [#xmlAttribute{name=?EXTENT_ATTRIBUTE_NAME, value=NewExtent} | NormalizedAttributes],
                           content = [normalize_object_xml(RequestUrl, NewExtent, C) || C <- Content]}
  end;
                     
normalize_object_xml(RequestUrl,
                     Extent,
                     Element = #xmlElement{content = Content})
  when is_list(RequestUrl), is_list(Extent) ->
  
  Element#xmlElement{content = [normalize_object_xml(RequestUrl, Extent, C) || C <- Content]};
  
normalize_object_xml(_, _, Other) ->
  Other.


normalize_attribute(RequestUrl,
                    Attribute = #xmlAttribute{name = href,
                                              value = Value})
  when is_list(RequestUrl) ->
  
  NormalizedHref = normalize_object_href(RequestUrl, Value),
  {NormalizedHref, Attribute#xmlAttribute{value = NormalizedHref}};

normalize_attribute(_, Attribute) ->
  {undefined, Attribute}.

%% @doc Ensure the object href complies to Erobix strict rules.
%%      This is not applicable for ref elements, for which href is free.
normalize_object_href(RequestUrl, UrlToNormalize)
  when is_list(RequestUrl), is_list(UrlToNormalize) ->
  
  normalize_object_href(ensure_trailing_slash(RequestUrl),
                ensure_trailing_slash(UrlToNormalize),
                uri_type(UrlToNormalize)).
  
normalize_object_href(RequestUrl, UrlToNormalize, global_absolute) ->
  case string:str(UrlToNormalize, RequestUrl) of
    0 ->
      throw_bad_uri(UrlToNormalize);
    Index ->
      string:substr(UrlToNormalize, Index + string:len(RequestUrl))
  end;

normalize_object_href(RequestUrl, UrlToNormalize, server_absolute) ->
  % keep only scheme and host port
  {Scheme, Netloc, _, Query, Fragment} = mochiweb_util:urlsplit(RequestUrl),
  ResultUrl = mochiweb_util:urlunsplit({Scheme, Netloc,  UrlToNormalize, Query, Fragment}),
  % renormalize the newly formed global URL
  normalize_object_href(RequestUrl, ResultUrl, global_absolute);

normalize_object_href(RequestUrl, UrlToNormalize, current_dir_relative) ->
  normalize_object_href(RequestUrl, string:substr(UrlToNormalize, 3), relative);

normalize_object_href(_, UrlToNormalize, relative) ->
  UrlToNormalize;

normalize_object_href(_, UrlToNormalize, _) ->
  throw_bad_uri(UrlToNormalize).

throw_bad_uri(Uri) ->
  throw({unsupported_uri, Uri}).
  
uri_type(Uri) when is_list(Uri) ->
  % TODO deal with fragment
  {Scheme, _Netloc, Path, _Query, _Fragment} = mochiweb_util:urlsplit(Uri),
  
  case Scheme of
    [] ->
      case string:left(Path, 2) of
        [$/|_] -> server_absolute;
        ".."   -> backup;
        "./"   -> current_dir_relative;
        _      -> relative
      end;
      
    _ ->
      global_absolute
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

uri_type_test() ->
  ?assertEqual(global_absolute, uri_type("http://server/obix/a")),
  ?assertEqual(server_absolute, uri_type("/a")),
  ?assertEqual(backup, uri_type("../a")),
  ?assertEqual(current_dir_relative, uri_type("./a")),
  ?assertEqual(relative, uri_type("a")),
  ok.

normalize_object_href_test() ->
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "http://server/obix/foo/baz")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo/", "http://server/obix/foo/baz")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "http://server/obix/foo/baz/")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo/", "http://server/obix/foo/baz/")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "baz")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "baz/")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "/obix/foo/baz")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo", "/obix/foo/baz/")),
  ?assertEqual("baz/", normalize_object_href("http://server/obix/foo/", "./baz/")),
  ?assertEqual("baz/bar/", normalize_object_href("http://server/obix/foo/", "./baz/bar")),
  
  ?assertThrow({unsupported_uri, "http://server/baz/"}, normalize_object_href("http://server/obix/foo", "/baz")),
  ?assertThrow({unsupported_uri, "http://other/path/"}, normalize_object_href("http://server/obix/foo", "http://other/path")),
  ?assertThrow({unsupported_uri, "../baz/"}, normalize_object_href("http://server/obix/foo/", "../baz/")),
  ?assertThrow({unsupported_uri, "../../baz/"}, normalize_object_href("http://server/obix/foo/", "../../baz/")),
  ok.
  
parse_object_xml_test() ->
  ?assertEqual("<?xml version=\"1.0\"?><obj xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>",
               parse_object_xml("http://data/foo", "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>")),
  
  ?assertEqual("<?xml version=\"1.0\"?><obj displayName=\"HomeControlCenter 1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><str _extent=\"type/\" name=\"type\" displayName=\"Device Type\" href=\"type/\" val=\"HomeControlCenter:1\"/></obj>",
               parse_object_xml("http://testbed.tml.hut.fi/obix/tg-at-tuas/1/", "<?xml version='1.0' encoding='UTF-8'?><obj href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/' displayName='HomeControlCenter 1' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><str name='type' displayName='Device Type' href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/type/' val='HomeControlCenter:1'></str></obj>")),
               
  ?assertEqual("<?xml version=\"1.0\"?><obj name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum _extent=\"enum/\" name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list _extent=\"enum/range/\" href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum></obj>",
               parse_object_xml("http://testbed.tml.hut.fi/obix/test/TestDevice/", "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum></obj>")),               
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
