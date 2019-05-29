%% @author ekdbmrx
%% @doc @todo Add description to chat_server.

-module(chat_server).

-behaviour(gen_server).

-export([start/0,
		 stop/0,
		 subscribe_client/2,
		 broadcast_msg/2,
		 format_time/0]).

-export([init/1, 
		 handle_cast/2, 
		 handle_call/3,
		 handle_info/2,
		 terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% connect chat client process with chat server
%% Client = Name of the client
%% ClientPid = Pid of the Client
subscribe_client(Client, ClientPid) ->
	gen_server:call(?MODULE, {subscribe, Client, ClientPid}).

%% broadcast message sent from client 'Client' to each connected client
broadcast_msg(Client, Msg) ->
	gen_server:cast(?MODULE, {broadcast, Client, Msg}).

format_time() ->
	{Y, M, D} = date(),
    {Hour, Min, Sec} = time(),
    io_lib:format('~2w. ~w. ~4w -- ~2..0b:~2..0b:~2..0b', [D, M, Y, Hour, Min, Sec]).
%% ====================================================================
%% Callback functions
%% ====================================================================

%% initializing state
init([]) -> 
	erlang:process_flag(trap_exit, true),
	{ok, AppName} = application:get_env(app_name),
	Clients = orddict:new(),
	io:format("Server started!~n"),
	log_udp_server:log({server_start, AppName, info, format_time()}),
	{ok, Clients}.

%% link client with server and add client to the dict(key(PID)-value(NAME))
handle_call({subscribe, Client, ClientPid}, _From, Clients) ->
	erlang:link(ClientPid),
	io:format("~s ~n", [format_time()]),
	{ok, AppName} = application:get_env(app_name),
	log_udp_server:log({subscribe, Client, ClientPid, AppName, info, format_time()}),
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
handle_info({'EXIT', ClientPid, _Reason}, Clients) ->
	{ok, Client} = orddict:find(ClientPid, Clients),
	{ok, AppName} = application:get_env(app_name),
	log_udp_server:log({unsubscribe, Client, ClientPid, AppName, info, format_time()}),
	io:format("~p unsubscribed from chat server!~n", [Client]),
	{noreply, orddict:erase(Client, Clients)};

handle_info(Msg, Clients) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Clients}.

terminate(Reason, _Clients) -> 
	{ok, AppName2} = application:get_env(app_name),
	log_udp_server:log({server_crash, AppName2, error, format_time(), Reason}),
	io:format("shutting down chat server...~n"),
	ok.