%%%
%%% @doc Main module.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
-module(erobix).
-author('David Dossot <david@dossot.net>').

-export([start/0, stop/0, quit/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the erobix server.
start() ->
    erobix_deps:ensure(),
    ensure_started(crypto),
    ensure_started(ssl),
    ensure_started(log4erl),
    application:start(erldis),
    application:start(erobix).

%% @spec stop() -> ok
%% @doc Stop the erobix server.
stop() ->
    Res = application:stop(erobix),
    application:stop(erldis),
    application:stop(log4erl),
    application:stop(ssl),
    application:stop(crypto),
    Res.

%% @spec quit() -> ok
%% @doc Stop and quit the erobix server.
quit() ->
    stop(),
    init:stop().

