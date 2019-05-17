%% @author ekdbmrx
%% @doc @todo Add description to chat_server.

-module(chat_server).

-behaviour(gen_server).

-export([start/1,
		  stop/1,
		  subscribe_client/3,
		  broadcast_msg/3]).

-export([init/1, 
		 handle_cast/2, 
		 handle_call/3,
		 handle_info/2,
		 terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================


start(ServerName) ->
	gen_server:start_link({local, ServerName}, ?MODULE, [], []).

stop(ServerName) ->
	gen_server:cast(ServerName, stop).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% connect chat client process with chat server
%% Client = Name of the client
%% ClientPid = Pid of the Client
subscribe_client(ServerName, Client, ClientPid) ->
	gen_server:call(ServerName, {subscribe, Client, ClientPid}).

%% broadcast message sent from client 'Client' to each connected client
broadcast_msg(ServerName, Client, Msg) ->
	gen_server:cast(ServerName, {broadcast, Client, Msg}).


%% ====================================================================
%% Callback functions
%% ====================================================================

%% initializing state
init([]) -> 
	erlang:process_flag(trap_exit, true),
	Clients = orddict:new(),
	{ok, Clients}.

%% link client with server and add client to the dict(key(PID)-value(NAME))
handle_call({subscribe, Client, ClientPid}, _From, Clients) ->
	erlang:link(ClientPid),
	io:format("~p with ~p subscribed to the server! ~n", [Client, ClientPid]),
	NewClientList = orddict:store(ClientPid, Client, Clients),
	{reply, ok, NewClientList}.

%% for each client -> send message sent from 'From' client
handle_cast({broadcast, From, Msg}, Clients) ->
	message_handler:broadcast_msg(From, Msg, Clients),
	{noreply, Clients};

%% disconnect clients if server is ordered to stop
handle_cast(stop, Clients) ->
	{stop, shutdown, Clients}.

%% handling exit signals from clients
handle_info({'EXIT', Client, _Reason}, Clients) ->
	io:format("~p unsubscribed from chat server!~n", [Client]),
	{noreply, orddict:erase(Client, Clients)};
handle_info(Msg, Clients) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Clients}.

terminate(_Reason, _State) -> 
	io:format("shutting down chat server...~n"),
	exit(normal).

