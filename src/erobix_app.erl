%%%
%%% @doc Callbacks for the erobix application.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_app).
-author('David Dossot <david@dossot.net>').

-behaviour(application).
-export([start/2, stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erobix.
start(_Type, _StartArgs) ->
  erobix_deps:ensure(),
  erobix_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erobix.
stop(_State) ->
  ok.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
