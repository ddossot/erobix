%%%
%%% @doc Web server for erobix.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_web).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  
  Loop = fun (Req) ->
    ?MODULE:loop(Req, DocRoot)
  end,

  Result = mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]),

  {ok, Version} = application:get_key(erobix, vsn),
  ?log_info("eroBIX Server v~s started w/options: ~1024p", [Version, Options]),
  Result.

stop() ->
  ?log_info("eroBIX Server is going down...", []),
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  try erobix_router:handle(Req, DocRoot) of
    {xml, XmlData} ->
      respond(Req, XmlData);
    
    {error, bad_request} ->
      ?log_info("Bad Request: ~1024p", [Req]),
      respond_error(Req, "UnsupportedErr");
    
    {error, not_found} ->
      ?log_info("Not Found: ~1024p", [Req]),
      respond_error(Req, "BadUriErr");
      
    {error, forbidden} ->
      ?log_info("Forbidden: ~1024p", [Req]),
      respond_error(Req, "PermissionErr");
    
    {error, Cause} ->
      ?log_info("~1024p: ~1024p", [Cause, Req]),
      respond_error(Req, "ServerErr");
      
    Other ->
      ?log_info("Unknown response type: ~1024p", [Other]),
      respond_error(Req, "ServerErr")
    
  catch
    Type:Reason ->
      ?log_error_with_stacktrace(Type, Reason,
                                 "processing request: ~4096p, Body: ~4096p",
                                 [Req, erlang:get(mochiweb_request_body)]),
      respond_error(Req, "ServerErr")
  end.

%% Private functions
get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

respond(Req, XmlData) ->
  Req:respond({200, [{"Content-Type", ?OBIX_MIME_TYPE}], XmlData}).

respond_error(Req, ErrorCode) ->
  Url = {url, RawUrl} = erobix_lib:get_url(Req),
  {xml, XmlData} = erobix_lib:build_xml_response(Url,
                                                 err,
                                                 [{is, "obix:" ++ ErrorCode},
                                                  {displayName, ErrorCode},
                                                  {display, ErrorCode ++ " for: " ++ RawUrl}],
                                                 []),
  respond(Req, XmlData).
  
%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
% TODO add unit tests
-endif.
