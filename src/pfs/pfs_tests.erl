-module(pfs_tests).
-include_lib("eunit/include/eunit.hrl").
-include("pfs.hrl").

-define(SERVER, pfs_server).
-define(TEST_FILE, "pfs_file_tests").
-define(PAGES_IN_FILE, 10).

% tests

do_test_() ->
	{ setup, fun prepare/0, fun stop_server/1,
	  { inorder,
		[
		 fun reading/0,
		 fun writing/0,
		 fun appending/0
		]
	  }
	}.

reading() ->
	?assertMatch([{ error, _ }], read_request(-1, 1)),
	?assertMatch([{ error, _ }], read_request(?PAGES_IN_FILE, 1)),
	?assertEqual([{ ok, <<0>> }], read_request(0, 1)),
	?LET(R, rand:uniform(?PAGES_IN_FILE - 1), ?assertEqual([{ ok, <<R>> }], read_request(R, 1))),
	?LET(R, rand:uniform(?PAGES_IN_FILE - 1),
		 ?assertEqual([{ partial, <<X>> } || X <- lists:seq(0, R - 2, 1)] ++ [{ ok, <<(R - 1)>> }], read_request(0, R))).

writing() ->
	?assertMatch([{ error, _ }], write_request(-1, <<0>>)),
	?assertMatch([{ error, _ }], write_request(?PAGES_IN_FILE, <<0>>)),
	?assertEqual([{ ok, 0 }], write_request(0, <<0>>)),
	?LET(R, rand:uniform(?PAGES_IN_FILE - 1), ?assertEqual([{ ok, R }], write_request(R, <<R>>))),
	?LET(R, rand:uniform(?PAGES_IN_FILE - 1),
		 ?assertEqual([{ partial, X } || X <- lists:seq(0, R - 1, 1)] ++ [{ ok, R }],
					  write_request(0, <<0:(8 * R * ?PAGE_DATA_SIZE)>>))).

appending() ->
	?assertEqual([{ ok, ?PAGES_IN_FILE }], append_request(<<0>>)),
	?assertEqual([{ ok, ?PAGES_IN_FILE + 1 }], append_request(<<0>>)),
	?assertEqual([{ ok, ?PAGES_IN_FILE + 2 }], append_request(<<0>>)),
	?LET(R, rand:uniform(5),
		 ?assertEqual([{ partial, X } || X <- lists:seq(?PAGES_IN_FILE + 3, ?PAGES_IN_FILE + R + 2, 1)] ++ [{ ok, ?PAGES_IN_FILE + R + 3 }],
					  append_request(<<0:(8 * R * ?PAGE_DATA_SIZE)>>))).

% auxiliry functions

prepare() ->
	% the file is always truncated
	create_flat_file(),
	initialize_server().

initialize_server() ->
	?assertMatch({ ok, _PID }, pfs:start_link(?SERVER, ?TEST_FILE)).

stop_server(_) ->
	gen_server:stop(?SERVER).

create_flat_file() ->
	{ ok, File } = file:open(?TEST_FILE, [ raw, binary, write ]),
	create_flat_file(File, ?PAGES_IN_FILE),
	file:close(File).
create_flat_file(File, N) ->
	create_flat_file(File, N, N).
create_flat_file(_File, _N, 0) ->
	ok;
create_flat_file(File, N, K) ->
	{ ok, Pos } = file:position(File, eof),
	ok = file:pwrite(File, Pos, int_to_bin(N - K)),
	create_flat_file(File, N, K - 1).

int_to_bin(PageId) ->
	Data = <<PageId>>,
	DataSize = byte_size(Data),
	Header = <<DataSize:(?PAGE_HEADER_SIZE * 8)>>,
	<<Header/binary, Data/binary, 0:((?PAGE_DATA_SIZE - DataSize) * 8)>>.

read_request(PageId, N) ->
	gen_server:cast(?SERVER, { read, self(), { PageId, N } }),
	receive_response(read).

write_request(PageId, Data) ->
	gen_server:cast(?SERVER, { write, self(), { PageId, Data } }),
	receive_response(write).

append_request(Data) ->
	gen_server:cast(?SERVER, { append, self(), { Data } }),
	receive_response(append).

receive_response(read) ->
	receive
		{ partial, Response } ->
			[ { partial, unpack_page(Response) } ] ++ receive_response(read);
		{ ok, Response } ->
			[ { ok, unpack_page(Response) } ];
		Message ->
			[ Message ]
	end;
receive_response(Type) ->
	receive
		{ partial, Response } ->
			[ { partial, Response } ] ++ receive_response(Type);
		Message ->
			[ Message ]
	end.

% get data from page without padding
unpack_page(PageData) ->
	<<Header:?PAGE_HEADER_SIZE/binary, DataPadded:?PAGE_DATA_SIZE/binary>> = PageData,
	binary:part(DataPadded, 0, binary:decode_unsigned(Header, big)).
