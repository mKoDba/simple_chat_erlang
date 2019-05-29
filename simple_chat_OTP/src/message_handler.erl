%% @author ekdbmrx
%% @doc @todo Add description to message_handler.


-module(message_handler).
-behaviour(gen_server).

-export([start/0, broadcast_msg/3]).
-export([init/1,
		 handle_cast/2,
		 handle_call/3]).
%% ====================================================================
%% API functions
%% ====================================================================

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Callback functions
%% ====================================================================

init([]) ->
	process_flag(trap_exit,true),
	{ok, []}.

handle_cast(_,S) ->
	{ok, S}.

handle_call(_,_,S) ->
	{ok, S}.
	
%% get all pids (keys) from dict and broadcast message to each client subscribed to chat server
broadcast_msg(From, Msg, Clients) ->
	{ok, AppName} = application:get_env(app_name),
	KeysList = orddict:fetch_keys(Clients),
	log_udp_server:log({broadcast, From, Msg, AppName, trace, chat_server:format_time()}),
	lists:foreach(fun(Client) -> chat_client:receive_msg(Client, From, Msg) end, KeysList).

