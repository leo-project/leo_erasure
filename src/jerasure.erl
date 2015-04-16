-module(jerasure).

-export([encode/1,decode/2]).
-export([encode_test/2, decode_test/3]).

-on_load(init/0).

-define(APPNAME, jerasure).
-define(LIBNAME, jerasure_drv).
-define(BLOCKSTOR, "blocks/").

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    filelib:ensure_dir(?BLOCKSTOR),
    erlang:load_nif(SoName, 0).

write_blocks(_, [], Cnt) ->
    Cnt;
write_blocks(FileName, [H | T], Cnt) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    file:write_file(BlockPath, H),
    write_blocks(FileName, T, Cnt + 1).

encode(FileName) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            io:format("File Content Length: ~p~n", [byte_size(FileContent)]),
%%            Blocks = encode_test(FileContent, byte_size(FileContent)),
            {Time, Blocks} = timer:tc(?MODULE, encode_test, [FileContent, byte_size(FileContent)]),
            io:format("Duration ~p us~n", [Time]),
            io:format("Number of Blocks: ~p~n", [length(Blocks)]);
        {error, Reason} ->
            Blocks = [],
            erlang:error(Reason)
    end,
    write_blocks(FileName, Blocks, 0).

check_available_blocks(_, -1, List) ->
    List;
check_available_blocks(FileName, Cnt, List) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    case filelib:is_regular(BlockPath) of      
        true ->
            check_available_blocks(FileName, Cnt - 1, [Cnt | List]);
        _ ->
            check_available_blocks(FileName, Cnt - 1, List)
    end.

read_blocks(FileName, AvailableList) ->
    read_blocks(FileName, lists:reverse(AvailableList), []).
read_blocks(_, [], BlockList) ->
    BlockList;
read_blocks(FileName, [Cnt | T], BlockList) ->
    BlockName = FileName ++ "." ++ integer_to_list(Cnt),
    BlockPath = filename:join(?BLOCKSTOR, BlockName),
    {ok, Block} = file:read_file(BlockPath),
    read_blocks(FileName, T, [Block | BlockList]).


decode(FileName, FileSize) ->
    AvailableList = check_available_blocks(FileName, 14, []),
    BlockList = read_blocks(FileName, AvailableList),
   % FileContent = decode_test(AvailableList, BlockList, FileSize),
    {Time, FileContent} = timer:tc(?MODULE, decode_test, [AvailableList, BlockList, FileSize]),
    io:format("Duration ~p~n", [Time]),
    DecodeName = FileName ++ ".dec",
    io:format("Decoded file at ~p~n", [DecodeName]),
    file:write_file(DecodeName, FileContent).

encode_test(_,_) ->
    exit(nif_library_not_loaded).

decode_test(_,_,_) ->
    exit(nif_library_not_loaded).
