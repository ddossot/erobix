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

% optional: {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}, {'xsi:schemaLocation', ?OBIX_NAMESPACE},
-define(SCHEMA_ATTRIBUTES, [{xmlns, ?OBIX_NAMESPACE}]).

-define(HREF_ATTRIBUTE_NAME, href).
-define(EXTENT_ATTRIBUTE_NAME, '_extent').

-export([get_url/1, ensure_trailing_slash/1, build_xml_response/4, export_xml/1,
         build_object_xml/3, normalize_object_xml/2, parse_object_xml/1,
         get_all_extents/1, get_writable_extents/1, get_object_names/1, get_object_value/3,
         render_object_xml/2, render_object_xml/3,
         xml_zulu_timestamp/0, xml_zulu_boottime/0]).

get_url(Req) ->
  {url, ensure_trailing_slash(mochiweb_util:urlunsplit({atom_to_list(Req:get(scheme)),
                                                       Req:get_header_value("host"),
                                                       Req:get(path),
                                                       "",
                                                       ""}))}.

ensure_trailing_slash(Url) when is_list(Url) ->
  lists:reverse(ensure_leading_slash(lists:reverse(Url))).

export_xml(Document) ->
  {xml, lists:flatten(xmerl:export_simple([Document], xmerl_xml))}.

build_xml_response({url, RawRequestUrl}, ElementName, Attributes, Children)
  when is_list(RawRequestUrl), is_atom(ElementName), is_list(Attributes), is_list(Children) ->
  
  build_object_xml(ElementName, [ {?HREF_ATTRIBUTE_NAME, ensure_trailing_slash(RawRequestUrl)} | Attributes], Children);
  
build_xml_response(Req, ElementName, Attributes, Children)
  when is_atom(ElementName), is_list(Attributes), is_list(Children) ->
  
  build_xml_response(get_url(Req), ElementName, Attributes, Children).

build_object_xml(ElementName, Attributes, Children)
  when is_atom(ElementName), is_list(Attributes), is_list(Children) ->

  ResponseData = {ElementName, ?SCHEMA_ATTRIBUTES ++ Attributes, Children},
  export_xml(ResponseData).

normalize_object_xml({url, RawRequestUrl}, {xml, RawObjectXml})
  when is_list(RawRequestUrl), is_list(RawObjectXml) ->
  
  {ObjectDoc, _} = xmerl_scan:string(RawObjectXml),
  NormalizedObjectDoc = normalize_object_doc(RawRequestUrl, ObjectDoc),
  export_xml(NormalizedObjectDoc).

% @doc Object XML definition must have been normalized before calling this!
parse_object_xml({xml, RawObjectXml}) when is_list(RawObjectXml) ->
  {ObjectDoc, _} = xmerl_scan:string(RawObjectXml),
  {object, ObjectDoc}.

get_all_extents({object, RawObject}) when is_record(RawObject, xmlElement) ->
  AllExtents = [Value || #xmlAttribute{value = Value} <- find_all_extent_attributes(RawObject)],
  {extents, AllExtents}.

get_writable_extents({object, RawObject}) when is_record(RawObject, xmlElement) ->
  WritableExtents = [Value || #xmlAttribute{value = Value} <- find_all_writable_extent_attributes(RawObject)],
  {writable_extents, WritableExtents}.  
  
get_object_names({object, RawObject}) when is_record(RawObject, xmlElement) ->
  NameAttributes = xmerl_xpath:string("/node()/@name | /node()/@displayName", RawObject),
  [{Name, Value} || #xmlAttribute{name=Name, value=Value} <- NameAttributes].

get_object_value({object, RawObject}, Extent = {extent, RawExtent}, DefaultValue)
  when is_record(RawObject, xmlElement), is_list(RawExtent) ->
  
  case xmerl_xpath:string(select_extent_xpath(Extent) ++ "/@val", RawObject) of
    [#xmlAttribute{value=Value}] ->
      Value;
      
    [] ->
      DefaultValue
  end.

render_object_xml({url, RawRequestUrl}, {object, RawObject})
  when is_list(RawRequestUrl), is_record(RawObject, xmlElement) ->
  
  FilteredObject =
    #xmlElement{attributes = Attributes} =
      remove_extents(remove_root_href(RawObject)),
  
  EnrichedAttributes =
    add_schema_attributes_if_needed([#xmlAttribute{name=?HREF_ATTRIBUTE_NAME, value=RawRequestUrl} | Attributes]),
      
  HrefedObject =
    FilteredObject#xmlElement{attributes = EnrichedAttributes},
  
  export_xml(HrefedObject).

render_object_xml({url, RawRequestUrl}, {object, RawObject}, {extent, ""})
  when is_list(RawRequestUrl), is_record(RawObject, xmlElement) ->
  
  render_object_xml({url, RawRequestUrl}, {object, RawObject});

render_object_xml({url, RawRequestUrl}, {object, RawObject}, Extent = {extent, RawExtent})
  when is_list(RawRequestUrl), is_record(RawObject, xmlElement), is_list(RawExtent) ->
 
  case xmerl_xpath:string(select_extent_xpath(Extent), RawObject) of
    [ChildObject | _] ->
      render_object_xml({url, RawRequestUrl}, {object, ChildObject});
      
    _ ->
      {error, not_found}
  end.

xml_zulu_timestamp() ->
  xml_zulu_format(erlang:universaltime()).

xml_zulu_boottime() ->
  {UpTimeMillisec, _} = erlang:statistics(wall_clock),
  BootTimeGSec = calendar:datetime_to_gregorian_seconds(erlang:universaltime()) - (UpTimeMillisec div 1000),
  xml_zulu_format(calendar:gregorian_seconds_to_datetime(BootTimeGSec)).

%%
%% Private functions
%%
  
find_all_extent_attributes(NormalizedObjectDoc) ->
  xmerl_xpath:string("//node()/@" ++ atom_to_list(?EXTENT_ATTRIBUTE_NAME), NormalizedObjectDoc).

find_all_writable_extent_attributes(NormalizedObjectDoc) ->
  xmerl_xpath:string("//node()[@writable='true' or @is='obix:WritablePoint']/@" ++ atom_to_list(?EXTENT_ATTRIBUTE_NAME), NormalizedObjectDoc).
  
add_schema_attributes_if_needed(Attributes) when is_list(Attributes) ->
  Finder =
    fun(#xmlAttribute{name = Name, value = Value}) ->
      Name =:= xmlns andalso Value =:= ?OBIX_NAMESPACE_STRING
    end,
    
  case lists:any(Finder, Attributes) of
    true ->
      Attributes;
    false ->
      ?SCHEMA_ATTRIBUTES ++ Attributes
  end.

normalize_object_doc(RequestUrl, Doc)
  when is_list(RequestUrl), is_record(Doc, xmlElement) ->
  
  NoRootRefDoc = remove_root_href(Doc), 
  normalize_object_doc(RequestUrl, "", NoRootRefDoc).

normalize_object_doc(RequestUrl,
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
                           content = [normalize_object_doc(RequestUrl, Extent, C) || C <- Content]};
                   
    [NormalizedHref|_] ->
        NewExtent = Extent ++ NormalizedHref,
        Element#xmlElement{attributes = [#xmlAttribute{name=?EXTENT_ATTRIBUTE_NAME, value=NewExtent} | NormalizedAttributes],
                           content = [normalize_object_doc(RequestUrl, NewExtent, C) || C <- Content]}
  end;
                     
normalize_object_doc(RequestUrl,
                     Extent,
                     Element = #xmlElement{content = Content})
  when is_list(RequestUrl), is_list(Extent) ->
  
  Element#xmlElement{content = [normalize_object_doc(RequestUrl, Extent, C) || C <- Content]};
  
normalize_object_doc(_, _, Other) ->
  Other.


normalize_attribute(RequestUrl,
                    Attribute = #xmlAttribute{name = ?HREF_ATTRIBUTE_NAME,
                                              value = Value})
  when is_list(RequestUrl) ->
  
  NormalizedHref = normalize_object_href(RequestUrl, Value),
  {NormalizedHref, Attribute#xmlAttribute{value = NormalizedHref}};

normalize_attribute(_, Attribute) ->
  {undefined, Attribute}.

remove_extents(Element = #xmlElement{content = Content}) ->
  FilteredElement = remove_attribute(Element, ?EXTENT_ATTRIBUTE_NAME),
  FilteredElement#xmlElement{content = [remove_extents(C) || C <- Content]};

remove_extents(Other) ->
  Other.
  
remove_root_href(Element) when is_record(Element, xmlElement) ->
  remove_attribute(Element, ?HREF_ATTRIBUTE_NAME).


remove_attribute(Element = #xmlElement{attributes = Attributes}, AttributeName)
  when is_atom(AttributeName) ->
  
  FilteredAttributes =
    lists:filter(fun(#xmlAttribute{name = Name}) -> Name =/= AttributeName end,
                 Attributes),
    
  Element#xmlElement{attributes = FilteredAttributes}.
  
select_extent_xpath({extent, RawExtent}) when is_list(RawExtent) ->
  "//node()[@" ++ atom_to_list(?EXTENT_ATTRIBUTE_NAME) ++ "='" ++ ensure_trailing_slash(RawExtent) ++ "']".
                       
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
  
ensure_leading_slash(Url = [$/|_]) ->
  Url;
ensure_leading_slash(Url) when is_list(Url) ->
  [$/|Url].

xml_zulu_format({{Year,Month,Day},{Hour,Min,Sec}}) ->
  io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
      [Year, Month, Day, Hour, Min, Sec]).

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

normalize_object_xml_test() ->
  NormalizedObjectXml1 =
    normalize_object_xml({url, "http://data/foo"},
                         {xml, "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"}), 
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"},
               NormalizedObjectXml1),

  NormalizedObjectXml2 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/' displayName='HomeControlCenter 1' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><str name='type' displayName='Device Type' href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/type/' val='HomeControlCenter:1'></str></obj>"}),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj displayName=\"HomeControlCenter 1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><str _extent=\"type/\" name=\"type\" displayName=\"Device Type\" href=\"type/\" val=\"HomeControlCenter:1\"/></obj>"},
               NormalizedObjectXml2),
               
  NormalizedObjectXml3 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum></obj>"}),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum _extent=\"enum/\" name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list _extent=\"enum/range/\" href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum></obj>"},
               NormalizedObjectXml3),
  ok.
  
parse_object_xml_test() ->
  NormalizedObjectXml1 =
    normalize_object_xml({url, "http://data/foo"},
                         {xml, "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"}), 
  {object, RawObject1} = parse_object_xml(NormalizedObjectXml1), 
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"},
               export_xml(RawObject1)),

  NormalizedObjectXml2 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/' displayName='HomeControlCenter 1' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><str name='type' displayName='Device Type' href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/type/' val='HomeControlCenter:1'></str></obj>"}),
  {object, RawObject2} = parse_object_xml(NormalizedObjectXml2),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj displayName=\"HomeControlCenter 1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><str _extent=\"type/\" name=\"type\" displayName=\"Device Type\" href=\"type/\" val=\"HomeControlCenter:1\"/></obj>"},
               export_xml(RawObject2)),
               
  NormalizedObjectXml3 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum><bool name='running' href='./running' is='obix:WritablePoint' val='true'/></obj>"}),
  {object, RawObject3} = parse_object_xml(NormalizedObjectXml3),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum _extent=\"enum/\" name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list _extent=\"enum/range/\" href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum><bool _extent=\"running/\" name=\"running\" href=\"running/\" is=\"obix:WritablePoint\" val=\"true\"/></obj>"},
               export_xml(RawObject3)),
  ok.
  
get_all_extents_test() ->
  NormalizedObjectXml1 =
    normalize_object_xml({url, "http://data/foo"},
                         {xml, "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"}), 
  Object1 = parse_object_xml(NormalizedObjectXml1), 
  ?assertEqual({extents, []}, get_all_extents(Object1)),

  NormalizedObjectXml2 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/' displayName='HomeControlCenter 1' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><str name='type' displayName='Device Type' href='http://testbed.tml.hut.fi/obix/tg-at-tuas/1/type/' val='HomeControlCenter:1'></str></obj>"}),
  Object2 = parse_object_xml(NormalizedObjectXml2),
  ?assertEqual({extents, ["type/"]}, get_all_extents(Object2)),
               
  NormalizedObjectXml3 =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum><bool name='running' href='./running' is='obix:WritablePoint' val='true'/></obj>"}),
  Object3 = parse_object_xml(NormalizedObjectXml3),
  ?assertEqual({extents, ["enum/", "running/", "enum/range/"]}, get_all_extents(Object3)),
  ok.
  
get_writable_extents_test() ->
  Object1 = parse_object_xml({xml, "<?xml version=\"1.0\"?><obj name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum _extent=\"enum/\" name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list _extent=\"enum/range/\" href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum><bool _extent=\"running/\" name=\"running\" href=\"running/\" is=\"obix:WritablePoint\" val=\"true\"/></obj>"}),
  ?assertEqual({writable_extents, ["enum/", "running/"]}, get_writable_extents(Object1)),
  
  Object2 = parse_object_xml({xml, "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"}),
  ?assertEqual({writable_extents, []}, get_writable_extents(Object2)),
  ok.
  
get_object_names_test() ->
  Object1= parse_object_xml({xml, "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum></obj>"}),
  ObjectNames = get_object_names(Object1),
  ?assertEqual("TestDevice", proplists:get_value(name, ObjectNames)),
  ?assertEqual("Device for tests", proplists:get_value(displayName, ObjectNames)),
  
  Object2 = parse_object_xml({xml, "<?xml version=\"1.0\"?><obj href=\"/foo/bar\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"/>"}),
  ?assertEqual([], get_object_names(Object2)),
  ok.

get_object_value_test() ->
  NormalizedObject =
    normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"},
                         {xml, "<?xml version='1.0' encoding='UTF-8'?><obj name='TestDevice' href='http://testbed.tml.hut.fi/obix/test/TestDevice/' displayName='Device for tests' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xsi:schemaLocation='http://obix.org/ns/schema/1.0' xmlns='http://obix.org/ns/schema/1.0'><enum name='conditionMode' href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/' displayName='Air Condition Mode' val='homeDay' writable='true'><list href='http://testbed.tml.hut.fi/obix/test/TestDevice/enum/range/' is='obix:Range'><obj name='homeDay' displayName='At home: Day mode'></obj></list></enum><bool name='running' href='./running' is='obix:WritablePoint' val='true'/></obj>"}),
  Object = parse_object_xml(NormalizedObject),
  ?assertEqual("homeDay", get_object_value(Object, {extent, "enum/"}, "")),
  ?assertEqual("true", get_object_value(Object, {extent, "running/"}, "")),
  ?assertEqual("", get_object_value(Object, {extent, "foo/"}, "")),
  ok.
  
render_object_xml_test() ->
  ObjectXml1 = {xml, "<?xml version=\"1.0\"?><obj href=\"http://testbed.tml.hut.fi/obix/tg-at-tuas/1/\" name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum></obj>"},
  Object1 = parse_object_xml(normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"}, ObjectXml1)),
  ?assertEqual(ObjectXml1, render_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/"}, Object1)),
  ?assertEqual(ObjectXml1, render_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/"}, Object1, {extent, ""})),
  
  ObjectXml2 = {xml, "<?xml version=\"1.0\"?><obj href=\"http://testbed.tml.hut.fi/obix/tg-at-tuas/1/\" name=\"TestDevice\" displayName=\"Device for tests\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://obix.org/ns/schema/1.0\" xmlns=\"http://obix.org/ns/schema/1.0\"><enum name=\"conditionMode\" href=\"enum/\" displayName=\"Air Condition Mode\" val=\"homeDay\" writable=\"true\"><list href=\"range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list></enum></obj>"},
  Object2 = parse_object_xml(normalize_object_xml({url, "http://testbed.tml.hut.fi/obix/test/TestDevice/"}, ObjectXml2)),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><list xmlns=\"http://obix.org/ns/schema/1.0\" href=\"http://testbed.tml.hut.fi/obix/tg-at-tuas/1/enum/range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list>"},
               render_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/enum/range/"}, Object2, {extent, "enum/range/"})),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><list xmlns=\"http://obix.org/ns/schema/1.0\" href=\"http://testbed.tml.hut.fi/obix/tg-at-tuas/1/enum/range/\" is=\"obix:Range\"><obj name=\"homeDay\" displayName=\"At home: Day mode\"/></list>"},
               render_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/enum/range/"}, Object2, {extent, "enum/range"})),
  ?assertEqual({error, not_found},
               render_object_xml({url, "http://testbed.tml.hut.fi/obix/tg-at-tuas/1/enum/range/"}, Object2, {extent, "missing_extent"})),
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

build_object_xml_test() ->
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj xmlns=\"http://obix.org/ns/schema/1.0\"/>"},
               build_object_xml(obj, [], [])),
  ?assertEqual({xml, "<?xml version=\"1.0\"?><obj xmlns=\"http://obix.org/ns/schema/1.0\"/>"},
               build_object_xml(obj, [], [])),
  ok.

xml_zulu_timestamp_test() ->
  ?assertMatch([_|_], xml_zulu_timestamp()),
  ok.
  
xml_zulu_boottime_test() ->
  ?assertMatch([_|_], xml_zulu_boottime()),
  ok.

-endif.
