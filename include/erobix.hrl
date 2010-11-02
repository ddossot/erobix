%%%
%%% @doc eroBIX includes
%%% @author David Dossot <david@dossot.net>
%%%
%%% See LICENSE for license information.
%%% Copyright (c) 2010 David Dossot
%%%

-define(OBIX_NAMESPACE, 'http://obix.org/ns/schema/1.0').
-define(OBIX_NAMESPACE_STRING, "http://obix.org/ns/schema/1.0").
-define(OBIX_BASE_PATH, "obix").
-define(OBIX_MIME_TYPE, <<"text/xml">>).

%% Log Definitions
-define(fmt(Fmt, Arguments),
        lists:flatten(io_lib:format(Fmt, Arguments))).

-define(log(Level, Message, Arguments),
        apply(log4erl, Level, [?fmt("~p ~p:~p " ++ Message, [self(), ?MODULE, ?LINE|Arguments])])).

-define(log_debug(Message, Arguments),
        ?log(debug, Message, Arguments)).

-define(log_info(Message, Arguments),
        ?log(info, Message, Arguments)).

-define(log_warn(Message, Arguments),
        ?log(warn, Message, Arguments)).

-define(log_error(Message, Arguments),
        ?log(error, Message, Arguments)).

-define(log_error_with_stacktrace(Type, Reason, Message, Arguments),
        ?log_error_with_given_stacktrace(Type, Reason, Message, Arguments, erlang:get_stacktrace())).

-define(log_error_with_given_stacktrace(Type, Reason, Message, Arguments, Stacktrace),
        ?log_error("~p:~1024p when " ++ Message, [Type, Reason|Arguments]),
        ?log_error("~p:~1024p stacktrace: ~1024p~n", [Type, Reason, Stacktrace])).

-define(unexpected_call(Name, Args),
        ?log_warn("unexpected ~p(~1024p)", [Name, Args])).

