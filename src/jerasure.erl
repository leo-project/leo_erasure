-module(jerasure).

-export([encode_test/2, decode_test/2]).

-on_load(init/0).

-define(APPNAME, jerasure).
-define(LIBNAME, jerasure_drv).

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
    erlang:load_nif(SoName, 0).

encode_test(_,_) ->
    exit(nif_library_no_loaded).

decode_test(_,_) ->
    exit(nif_library_no_loaded).
