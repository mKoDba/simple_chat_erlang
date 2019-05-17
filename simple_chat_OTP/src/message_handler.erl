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
	KeysList = orddict:fetch_keys(Clients),
	lists:foreach(fun(Client) -> chat_client:receive_msg(Client, From, Msg) end, KeysList).

%% za tcp/udp verziju:
%%
%%broadcast_msg(_From, Msg, Clients) ->
%%	Sockets = orddict:fetch_keys(Clients),
%%	lists:foreach(fun(Socket) -> gen_tcp:send(Socket, Msg) end, Sockets).

