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

-export([serve/2]).

serve(Req, Extent) ->
  Method = Req:get(method),
  
  case Method of
    'GET' ->
      render(Req, Extent);

    _ ->
      {error, bad_request}
  end.

%% Private functions
render(Req, Extent) ->
  AboutXml = generate(),
  Url = erobix_lib:get_url(Req),
  AboutObject = erobix_lib:parse_object_xml(AboutXml),
  erobix_lib:render_object_xml(Url, AboutObject, Extent).

generate() ->  
  erobix_lib:build_object_xml(obj, attributes(), children()).

attributes() ->
  [{is, "obix:About"}, {display, "Obix About"}].
  
children() ->
  {ok, Version} = application:get_key(erobix, vsn),
  
  [
   {str,     [{'_extent', "obixVersion/"}, {name, "obixVersion"}, {val, "1.0"}, {href, "obixVersion/"}, {displayName, "oBIX Version"}], []},
   {str,     [{'_extent', "serverName/"}, {name, "serverName"}, {val, "erOBIX Server"}, {href, "serverName/"}, {displayName, "Server Name"}], []},
   {str,     [{'_extent', "vendorName/"}, {name, "vendorName"}, {val, "Dossot Software Consulting Inc."}, {href, "vendorName/"}, {displayName, "Vendor Name"}], []},
   {str,     [{'_extent', "productName/"}, {name, "productName"}, {val, "erOBIX"}, {href, "productName/"}, {displayName, "Product Name"}], []},
   {str,     [{'_extent', "productVersion/"}, {name, "productVersion"}, {val, Version}, {href, "productVersion/"}, {displayName, "Product Version"}], []},
   {uri,     [{'_extent', "vendorUrl/"}, {name, "vendorUrl"}, {val, "http://consulting.dossot.net"}, {href, "vendorUrl/"}, {displayName, "Vendor Url"}], []},
   {uri,     [{'_extent', "productUrl/"}, {name, "productUrl"}, {val, "http://github.net/ddossot/erobix"}, {href, "productUrl/"}, {displayName, "Product Url"}], []},
   {abstime, [{'_extent', "serverTime/"}, {name, "serverTime"}, {val, erobix_lib:xml_zulu_timestamp()}, {href, "serverTime/"}, {displayName, "Server Time"}], []},
   {abstime, [{'_extent', "serverBootTime/"}, {name, "serverBootTime"}, {val, erobix_lib:xml_zulu_boottime()}, {href, "serverBootTime/"}, {displayName, "Server Boot Time"}], []}
  ].


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

generate_test() ->
  AboutXml = generate(),
  AboutObject = {AboutObjectType, _} = erobix_lib:parse_object_xml(AboutXml),
  ?assertEqual(object, AboutObjectType),
  {extents, RawAboutExtents} = erobix_lib:get_all_extents(AboutObject),
  ?assertEqual(9, length(RawAboutExtents)),
  ok.

-endif.
