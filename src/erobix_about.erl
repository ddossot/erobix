%%%
%%% @doc About service.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_about).
-author('David Dossot <david@dossot.net>').

-include_lib("xmerl/include/xmerl.hrl").
-include("erobix.hrl").

-export([initialize/0]).

initialize() ->
  % refresh the cached "about" object only if we're first in cluster
  case erlang:nodes() of
    [] ->
      AboutXml = generate(),
      {AboutObject, AboutExtents} = erobix_lib:parse_object_xml({url, "http://amulet:8888/obix/about"}, {xml, AboutXml}),
      erobix_store:store_object({storage_path, "about/"}, AboutObject, AboutExtents);
    _ ->
      noop
  end,
  ok.

generate() ->
  erobix_lib:build_object_xml(obj, attributes(), children()).

%% Private function
attributes() ->
  [{is, "obix:About"}, {display, "Obix About"}].
  
children() ->
  {ok, Version} = application:get_key(erobix, vsn),
  
  [
   {str, [{name, "obixVersion"}, {val, "1.0"}, {href, "obixVersion/"}, {displayName, "oBIX Version"}], []},
   {str, [{name, "serverName"}, {val, "erOBIX Server"}, {href, "serverName/"}, {displayName, "Server Name"}], []},
   {str, [{name, "vendorName"}, {val, "Dossot Software Consulting Inc."}, {href, "vendorName/"}, {displayName, "Vendor Name"}], []},
   {str, [{name, "productName"}, {val, "erOBIX"}, {href, "productName/"}, {displayName, "Product Name"}], []},
   {str, [{name, "productVersion"}, {val, Version}, {href, "productVersion/"}, {displayName, "Product Version"}], []},
   {uri, [{name, "vendorUrl"}, {val, "http://consulting.dossot.net"}, {href, "vendorUrl/"}, {displayName, "Vendor Url"}], []},
   {uri, [{name, "productUrl"}, {val, "http://github.net/ddossot/erobix"}, {href, "productUrl/"}, {displayName, "Product Url"}], []},
   {abstime, [{name, "serverTime"}, {null, "true"}, {href, "serverTime/"}, {displayName, "Server Time"}], []},
   {abstime, [{name, "serverBootTime"}, {val, erobix_lib:xml_zulu_timestamp()}, {href, "serverBootTime/"}, {displayName, "Server Boot Time"}], []}
  ].


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

generate_test() ->
  AboutXml = generate(),
  {{AboutObjectType, _}, {extents, RawAboutExtents}} = erobix_lib:parse_object_xml({url, "http://amulet:8888/obix/about"}, {xml, AboutXml}),
  ?assertEqual(object, AboutObjectType),
  ?assertEqual(9, length(RawAboutExtents)),
  ok.

-endif.
