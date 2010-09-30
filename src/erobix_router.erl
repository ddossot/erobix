%%%
%%% @doc Web request router for erobix.
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-module(erobix_router).
-author('David Dossot <david@dossot.net>').

-include("erobix.hrl").

-export([handle/2]).

handle(Req, _DocRoot) ->
    Path = Req:get(path),

    case string:tokens(Path, "/") of
        ["obix"|_] ->
            handle_obix(Req);
        
        _ ->
            {error, bad_request}
    end.

% Handling of /obix/*
handle_obix(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    {data, <<"text/plain">>, <<"lobby!">>}.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
