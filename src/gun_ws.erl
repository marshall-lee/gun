%% Copyright (c) 2014, Vladimir Kochnev <hashtable@yandex.ru>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(gun_ws).

-export([init/3]).
-export([handle/2]).
%-export([ws_send/2]).

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.
-type mask_key() :: 0..16#ffffffff.

-record(ws_state, {
	owner :: pid(),
	socket :: inet:socket() | ssl:sslsocket(),
	transport :: module(),
	buffer = <<>> :: binary(),
	message = <<>> :: binary(),
	in = head :: head | {fin, non_neg_integer()} | {nofin, non_neg_integer()}
}).

init(Owner, Socket, Transport) ->
	#ws_state{owner=Owner, socket=Socket, transport=Transport}.

handle(Data, State=#ws_state{in=head, buffer=Buffer}) ->
	Data2 = << Buffer/binary, Data/binary >>,
	State2 = State#ws_state{buffer=Data2},
	if
		byte_size(Data2) < 16 ->
			State2;
		true ->
			handle_head(Data2, State2)
	end;

handle(Data, State=#ws_state{in={_, Len}, buffer=Buffer}) ->
	Data2 = << Buffer/binary, Data/binary >>,
	State2 = State#ws_state{buffer=Data2},
	handle_payload(Data2, State2).

%% RSV bits MUST be 0 unless an extension is negotiated
%% that defines meanings for non-zero values.
handle_head(<< _:1, Rsv:3, _/bits >>, _)
		when Rsv =/= 0 ->
	close;
%% Invalid opcode. Note that these opcodes may be used by extensions.
handle_head(<< _:4, Opcode:4, _/bits >>, _)
		when Opcode > 2, Opcode =/= 8, Opcode =/= 9, Opcode =/= 10 ->
	close;
%% Control frames MUST NOT be fragmented.
handle_head(<< 0:1, _:3, Opcode:4, _/bits >>, _)
		when Opcode >= 8 ->
	close;
% A frame MUST NOT use the zero opcode unless fragmentation was initiated.
handle_head(<< _:4, 0:4, _/bits >>, #ws_state{message=Message})
		when Message =:= <<>> ->
	close;
%% Non-control opcode when expecting control message or next fragment.
handle_head(<< _:4, Opcode:4, _/bits >>, #ws_state{message=Message})
		when Message =/= <<>>, Opcode =/= 0, Opcode < 8 ->
	close;
%% Close control frame length MUST be 0 or >= 2.
handle_head(<< _:4, 8:4, _:1, 1:7, _/bits >>, _) ->
	close;
%% Close control frame with incomplete close code. Need more data.
handle_head(Data = << _:4, 8:4, 0:1, Len:7, _/bits >>, State)
		when Len > 1, byte_size(Data) < 8 ->
	State;
%% 7 bits payload length.
handle_head(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, Len:7, Rest/bits >>, State)
		when Len < 126 ->
	handle_head_finalize(Rest, Opcode, Rsv, Fin, Len, State);
%% 16 bits payload length.
handle_head(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, 126:7, Len:16, Rest/bits >>, State)
		when Len > 125 ->
	handle_head_finalize(Rest, Opcode, Rsv, Fin, Len, State);
%% 63 bits payload length.
handle_head(<< Fin:1, Rsv:3/bits, Opcode:4, 0:1, 127:7, 0:1, Len:63, Rest/bits >>, State)
		when Len > 16#ffff, Opcode < 8 ->
	handle_head_finalize(Rest, Opcode, Rsv, Fin, Len, State);
%% When payload length is over 63 bits, the most significant bit MUST be 0.
handle_head(<< _:8, 0:1, 127:7, 1:1, _:7, _/bits >>, _) ->
	close;
%% All frames sent from the server to the client are not masked.
handle_head(<< _:8, 1:1, _/bits >>, _) ->
	close;
%% For the next two clauses, it can be one of the following:
%%
%%  *  The minimal number of bytes MUST be used to encode the length
%%  *  All control frames MUST have a payload length of 125 bytes or less
handle_head(<< _:9, 126:7, _:48, _/bits >>, _) ->
	close;
handle_head(<< _:9, 127:7, _:96, _/bits >>, _) ->
	close;
%% Need more data.
handle_head(_, State) ->
	State.

handle_head_finalize(Rest, Opcode, Rsv, 0, Len, State) ->
	handle_payload(Rest, State#ws_state{in={nofin, Len, Opcode, Rsv}, buffer= <<>>});
handle_head_finalize(Rest, Opcode, Rsv, 1, Len, State) ->
	handle_payload(Rest, State#ws_state{in={fin, Len, Opcode, Rsv}, buffer= <<>>}).

handle_payload(Data, State) -> todo.
