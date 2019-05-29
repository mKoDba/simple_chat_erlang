SETUP
- set erlang path
- position yourself to simple_chat_OTP/src
- run powershell there and enter commands:
	erl -make
	erl -pa ebin/

erlang shell will start
	application:start(chat) - to start chat application with log server
	chat_client:subscribe(ClientName) - to start client
	chat_client:unsubscribe(ClientName) - to close client
	chat_client:send(ClientName, Msg) - to broadcast msg
	log_udp_server:get_logs() - to print log file (stored on disk on a file named 'disk_log.log')
	application:stop(chat) - to close chat application with log server