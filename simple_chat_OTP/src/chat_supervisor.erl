%% @author ekdbmrx
%% @doc Supervisor behaviour to start individual worker 
%%		processes


-module(chat_supervisor).
-behaviour(supervisor).

%% API 
-export([start/0]).

%% Callback
-export([init/1]).


-define(SHUTDOWNTIME, 5000).
-define(SERVER, chat_server).
-define(MSG_HANDLER, message_handler).
-define(LOG_HANDLER, log_udp_server).

-define(MAXRESTART, 60).
-define(MAXTIME, 3600). %% in seconds
%% ====================================================================
%% API functions
%% ====================================================================
start() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% Callback functions
%% ====================================================================

init([]) ->
	%% {Name, StartFunction, 
	%% RestartType, ShutdownTime, ProcessType, Modules}
	ChatServer = {?SERVER, {?SERVER, start, []},
				   permanent, ?SHUTDOWNTIME, worker, dynamic},
	
	MsgHandler = {?MSG_HANDLER, {?MSG_HANDLER, start, []},
				   permanent, ?SHUTDOWNTIME, worker, dynamic},
	
	LogHandler = {?LOG_HANDLER, {?LOG_HANDLER, start, []},
				   permanent, ?SHUTDOWNTIME, worker, dynamic},
	%% {RestartType, MaxRestart, MaxTime}
	RestartTuple = {one_for_all, ?MAXRESTART, ?MAXTIME},
	
	SupervisorSpecs = {RestartTuple, [LogHandler, ChatServer, MsgHandler]},
	{ok, SupervisorSpecs}.
	
