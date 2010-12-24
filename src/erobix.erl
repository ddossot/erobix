%%%
%%% @doc Main module.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%
-module(erobix).
-author('David Dossot <david@dossot.net>').

-export([start/0, stop/0, quit/0,
         get_store/0]).

%% @spec start() -> ok
%% @doc Start the erobix server.
start() ->
  erobix_deps:ensure(),
  ensure_started(crypto),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(log4erl),
  log4erl:conf("conf/log4erl.cfg"),
  application:load(erobix),
  Store = get_store(),
  Store:start(),
  application:start(erobix).

%% @spec stop() -> ok
%% @doc Stop the erobix server.
stop() ->
  Res = application:stop(erobix),
  Store = get_store(),
  Store:stop(),
  application:stop(log4erl),
  application:stop(ssl),
  application:stop(crypto),
  Res.

%% @spec quit() -> ok
%% @doc Stop and quit the erobix server.
quit() ->
  stop(),
  init:stop().

%% @spec get_store() -> atom()
%% @doc Get the store implementation.
get_store() ->
  {ok, Store} = application:get_env(erobix, store),
  Store.
  
%% Private functions
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

