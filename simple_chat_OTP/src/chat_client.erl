%% @author ekdbmrx
%% run chat_supervisor:start(ServerName)
%% run chat_client:subscribe(ClientName, Servername)
%% repeat subscribe for more users

%% to broadcast message from ClientName
%% run chat_client:send(FromClientName, MessageToBroadcast)
%% to unsubscribe client run chat_client:unsubscribe(ClientName)

-module(chat_client).
-behaviour(gen_server).

-export([subscribe/2, 
		 unsubscribe/1,
		 receive_msg/3, 
		 send/2]).

-export([init/1, 
		 handle_cast/2,
		 handle_call/3,
		 handle_info/2,
		 terminate/2]).

-record(state, {client, clientpid, server}).
%% ====================================================================
%% API functions
%% ====================================================================

subscribe(Client, ServerName) ->
	gen_server:start_link({local, Client}, ?MODULE, [Client, ServerName], []).

receive_msg(Client, From, Msg) ->
     gen_server:cast(Client, {receive_msg, From, Msg}).

send(Client, Msg) ->
	gen_server:cast(Client, {send, Msg}).

unsubscribe(Client) ->
	gen_server:cast(Client, stop).

%% ====================================================================
%% Callback functions
%% ====================================================================
init([Client, ServerName]) ->
	%% handle_info/2 connects to chat_server
	self()!subscribe,
	{ok, #state{client=Client, clientpid=self(), server=ServerName}}.

%% send message to server module, which broadcasts it to everyone
handle_cast({send, Msg}, #state{server=ServerName, client=Client} = S) ->
	chat_server:broadcast_msg(ServerName, Client, Msg),
	{noreply, S};
%% print received message
handle_cast({receive_msg, From, Msg}, State) ->
	io:format("[~p client]: ~p says: ~p ~n", [State#state.client, From, Msg]),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call(_, _From, Clients) ->
	{reply, ok, Clients}.

handle_info(subscribe, State) ->
    chat_server:subscribe_client(State#state.server, State#state.client, State#state.clientpid),
    {noreply, State};
handle_info(Msg, Clients) ->
	io:format("Unexpected message: ~p~n",[Msg]),
	{noreply, Clients}.

terminate(normal, _State) ->
	exit(normal).



