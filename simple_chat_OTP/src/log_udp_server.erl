%% @author Mario
%% @doc @todo Add description to log_udp_server.


-module(log_udp_server).
-behaviour(gen_server).

-export([start/0, stop/0, log/1, get_logs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-record(state, {port, socket, file}).

%% ====================================================================
%% API functions
%% ====================================================================

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:cast(?MODULE, stop).

log(Message) ->
    gen_server:call(?MODULE, Message).

get_logs() ->
    {ok, Data} = file:read_file('disk_log.log'),
	io:format("~s ~n", [Data]).

%% ====================================================================
%% Internal functions
%% ====================================================================

init([]) -> 
	{ok, Port} = application:get_env(port),
    {ok, Sock} = gen_udp:open(Port, [list, inet]),
    {ok, File} = disk_log:open([{name, test}, {file, "disk_log.log"}, {format, external}]),
    {ok, #state{port = Port, socket = Sock, file = File}}.

% Log message and return logged message
handle_call({subscribe, Client, ClientPid, AppName, MsgType, TimeStamp}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | ~p with ~p subscribed to server.>>\r\n", [AppName, TimeStamp, MsgType, Client, ClientPid]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({unsubscribe, Client, ClientPid, AppName, MsgType, TimeStamp}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | ~p with ~p unsubscribed from server.>>\r\n", [AppName, TimeStamp, MsgType, Client, ClientPid]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({server_start, AppName, MsgType, TimeStamp}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | Server started.>>\r\n", [AppName, TimeStamp, MsgType]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({server_stop, AppName, MsgType, TimeStamp}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | Server stopped normally.>>\r\n", [AppName, TimeStamp, MsgType]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({broadcast, From, Msg, AppName, MsgType, TimeStamp}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | Message broadcast -- ~p : ~p>>\r\n", [AppName, TimeStamp, MsgType, From, Msg]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({server_crash, AppName, MsgType, TimeStamp, Reason}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | SERVER CRASHED - REASON: ~p>>\r\n", [AppName, TimeStamp, MsgType, Reason]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call({client_crash, AppName, Client, ClientPid, MsgType, TimeStamp, Reason}, _From, State) ->
	FormattedMessage = io_lib:format("<<~p | ~s | ~p | ~p (~p) CRASHED - REASON: ~p >>\r\n", [AppName, TimeStamp, MsgType, Client, ClientPid, Reason]),
    write_log(FormattedMessage, State#state.file),
    {reply, FormattedMessage, State};

handle_call(Msg, _From, State) ->
	write_log(Msg, State#state.file),
    {reply, Msg, State}.

% Stop server upon request
handle_cast(stop, State) ->
    {stop, normal, State}.

% Log raw data packets arriving at the UDP socket
handle_info({udp, _Socket, _ClientIP, _ClientPort, RawData}, State) ->
    Message = RawData ++ "\n",
    write_log(Message, State#state.file),
    {noreply, State}.

terminate(Reason, State) -> 
    disk_log:close(State#state.file),
    io:format("Log file closed. Terminate ~p~n", [Reason]),
    ok.

write_log(Message, File) ->
    disk_log:blog(File, Message).
