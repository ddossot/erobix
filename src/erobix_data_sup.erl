%%%
%%% @doc Supervisor for the data servers of the erobix application.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_data_sup).
-author('David Dossot <david@dossot.net>').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
  
%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  DataServer = {data_server,
       {erobix_data_server,start_link,[]}, 
        permanent, 5000, worker, []}, 
  {ok, {{simple_one_for_one, 10, 120}, [DataServer]}}.

