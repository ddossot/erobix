%%%
%%% @doc Utility methods.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_lib).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([base_url/1]).

base_url(Req) ->
  atom_to_list(Req:get(scheme)) ++ "://" ++ Req:get_header_value("host") ++ "/" ++ ?OBIX_BASE_PATH.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
