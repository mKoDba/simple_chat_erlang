%% @author ekdbmrx
%% @doc @todo Add description to chat_app.


-module(chat).
-behaviour(application).

-export([start/2, 
		 stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

start(_Type, _Args) ->
    chat_supervisor:start().
        

stop(_State) ->
	ok.	