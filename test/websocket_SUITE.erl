-module(websocket_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.
-export([ws_upgrade/1]).

%% ct.

all() ->
	[ws_upgrade].

init_per_suite(Config) ->
	ok = application:start(ranch),
	ok = application:start(crypto),
	ok = application:start(cowlib),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(gun),
	Config.

end_per_suite(_) ->
	ok = application:stop(gun),
	ok = application:stop(ssl),
	ok = application:stop(public_key),
	ok = application:stop(asn1),
	ok = application:stop(cowlib),
	ok = application:stop(crypto),
	ok = application:stop(ranch),
	ok.

ws_upgrade(_) ->
	{ok, Pid} = gun:open("echo.websocket.org", 80),
	gun:ws_upgrade(Pid, "/"),
	receive
		{gun_ws_upgrade, Pid, ok} -> ok
	after 5000 ->
		error(timeout)
	end.
