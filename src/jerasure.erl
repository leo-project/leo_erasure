-module(jerasure).

-export([encode_test/1,encode_test/2, decode_test/2]).

-on_load(init/0).

-define(APPNAME, jerasure).
-define(LIBNAME, jerasure_drv).
-define(BLOCKSTOR, "blocks/").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
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
            Blocks = encode_test(FileContent, byte_size(FileContent)),
            io:format("Number of Blocks: ~p~n", [length(Blocks)]);
        {error, Reason} ->
            Blocks = [],
            erlang:error(Reason)
    end,
    write_blocks(FileName, Blocks, 0).

decode(FileName, FileSize) ->
    

encode_test(_,_) ->
    exit(nif_library_not_loaded).

decode_test(_,_) ->
    exit(nif_library_not_loaded).
