%% @author ekdbmrx
%% @doc Supervisor behaviour to start individual worker 
%%		processes


-module(chat_supervisor).
-behaviour(supervisor).

%% API 
-export([start/1]).

%% Callback
-export([init/1]).


-define(SHUTDOWNTIME, 5000).
-define(SERVER, chat_server).
-define(MSG_HANDLER, message_handler).
-define(MAXRESTART, 60).
-define(MAXTIME, 3600). %% in seconds
%% ====================================================================
%% API functions
%% ====================================================================
start(SrvName) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [SrvName]).


%% ====================================================================
%% Callback functions
%% ====================================================================

init([SrvName]) ->
	%% {Name, StartFunction, 
	%% RestartType, ShutdownTime, ProcessType, Modules}
	Chat_server = {?SERVER, {?SERVER, start, [SrvName]},
				   permanent, ?SHUTDOWNTIME, worker, dynamic},
	
	Broadcast_proc = {?MSG_HANDLER, {?MSG_HANDLER, start, []},
				   permanent, ?SHUTDOWNTIME, worker, dynamic},
	
	%% {RestartType, MaxRestart, MaxTime}
	RestartTuple = {one_for_all, ?MAXRESTART, ?MAXTIME},
	
	SupervisorSpecs = {RestartTuple, [Chat_server, Broadcast_proc]},
	{ok, SupervisorSpecs}.
	
