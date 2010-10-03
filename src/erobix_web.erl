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
    log4erl:conf("conf/log4erl.cfg"),
    
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
        % TODO redis cache w/expiry
        {data, Data} ->
            XmlData = xmerl:export_simple([Data], xmerl_xml),
            Req:respond({200, [{"Content-Type", ?OBIX_MIME_TYPE}], XmlData});
        
        {error, bad_request} ->
            % FIXME return 200 and obix error like
            % <err href="http://testbed.tml.hut.fi/obix/" displayName="Write Error" display="Unable to read request input." xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://obix.org/ns/schema/1.0" xmlns="http://obix.org/ns/schema/1.0"></err>
            Req:respond({400, [], "Bad Request"});
        
        {error, not_authorized} ->
            % FIXME return 200 and obix error
            Req:respond({401, [{"WWW-Authenticate", "Basic realm=\"eroBIX Server\""}], "Unauthorized"});
        
        {error, forbidden} ->
            % FIXME return 200 and obix error
            Req:respond({403, [], "Forbidden"});
        
        {error, not_found} ->
            % FIXME return 200 and obix error
            Req:not_found()
    catch
        Type:Reason ->
            ?log_error_with_stacktrace(Type, Reason,
                                       "processing request: ~4096p, Body: ~4096p",
                                       [Req, erlang:get(mochiweb_request_body)]),
            Req:respond({500, [], <<"Server error">>})
    end.

%% Private functions
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
