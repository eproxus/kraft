-module(kraft_cache_ets).

-behaviour(kraft_cache).

% Callbacks
-export([init/2]).
-export([get/2]).
-export([update/3]).
-ignore_xref([{?MODULE, clear, 2}]).
-export([clear/2]).
-export([clean_cache/1]).

%--- Types ---------------------------------------------------------------------

-type config() :: #{
    memory => unit(memory()),
    age => unit(time()),
    clean_interval => unit(time())
}.
-export_type([config/0]).
-type unit(Unit) :: {Unit, pos_integer()} | infinity.
-type memory() ::
    tb
    | gb
    | mb
    | kb
    | tib
    | gib
    | mib
    | kib
    | terabyte
    | gigabyte
    | megabyte
    | kilobyte
    | tebibyte
    | gibibyte
    | mebibyte
    | kibibyte
    | byte.
-type time() ::
    week
    | day
    | hour
    | minute
    | erlang:time_unit().
-opaque ref() :: {ets:tab(), map()}.
-export_type([ref/0]).

%--- Callbacks -----------------------------------------------------------------

-spec init(kraft_cache:type(), config()) -> ref().
init(Type, Config) ->
    State = #{
        memory => convert_memory(maps:get(memory, Config, infinity)),
        age => convert_time_unit(maps:get(age, Config, infinity))
    },
    {ets:new(Type, [public]), State}.

get({Table, _Config}, Keypath) ->
    case ets:lookup(Table, Keypath) of
        [] -> miss;
        [{_Hash, _Time, Content}] -> {hit, Content}
    end.

update({Table, _Config}, Keypath, Content) ->
    ets:insert(Table, {Keypath, erlang:system_time(), Content}),
    ok.

clear({Table, _Config}, Keypath) ->
    clear(Table, Keypath, ets:first(Table)).

clean_cache({Table, Config}) ->
    Memory = maps:get(memory, Config, infinity),
    Age = maps:get(age, Config, infinity),
    Time =
        case Age of
            infinity ->
                infinity;
            Age ->
                NativeAge = convert_time_unit(Age),
                erlang:system_time() - NativeAge
        end,
    Words =
        case Memory of
            infinity -> infinity;
            Memory -> round(Memory / erlang:system_info(wordsize))
        end,
    Rest = clean_cache_age(Table, ets:first(Table), Time, []),
    clean_cache_memory(Table, Words, Rest).

%--- Internal ------------------------------------------------------------------

clean_cache_age(_Table, '$end_of_table', _Age, Rest) ->
    lists:sort(Rest);
clean_cache_age(Table, Key, Age, Rest) ->
    Next = ets:next(Table, Key),
    case ets:lookup_element(Table, Key, 2) of
        TS when TS < Age, Age =/= infinity ->
            ets:delete(Table, Key),
            clean_cache_age(Table, Next, Age, Rest);
        TS ->
            clean_cache_age(Table, Next, Age, [{TS, Key} | Rest])
    end.

clean_cache_memory(_Table, _Target, []) ->
    ok;
clean_cache_memory(Table, Target, [{_TS, Key} | Oldest]) ->
    case ets:info(Table, memory) of
        Memory when Memory > Target ->
            ets:delete(Table, Key),
            clean_cache_memory(Table, Target, Oldest);
        _Memory ->
            ok
    end.

clear(_Table, _Prefix, '$end_of_table') ->
    ok;
clear(Table, Prefix, Key) ->
    % next must be called before delete, otherwise it crashes
    Next = ets:next(Table, Key),
    case lists:prefix(Prefix, Key) of
        true -> ets:delete(Table, Key);
        false -> ok
    end,
    clear(Table, Prefix, Next).

convert_memory(infinity) -> infinity;
% No one will ever need more than a 1 T(i)B of cache :-)
convert_memory({tb, Size}) -> convert_memory({terabyte, Size});
convert_memory({gb, Size}) -> convert_memory({gigabyte, Size});
convert_memory({mb, Size}) -> convert_memory({megabyte, Size});
convert_memory({kb, Size}) -> convert_memory({kilobyte, Size});
convert_memory({tib, Size}) -> convert_memory({tebibyte, Size});
convert_memory({gib, Size}) -> convert_memory({gibibyte, Size});
convert_memory({mib, Size}) -> convert_memory({mebibyte, Size});
convert_memory({kib, Size}) -> convert_memory({kibibyte, Size});
convert_memory({terabyte, Size}) -> convert_memory({gigabyte, 1000 * Size});
convert_memory({gigabyte, Size}) -> convert_memory({megabyte, 1000 * Size});
convert_memory({megabyte, Size}) -> convert_memory({kilobyte, 1000 * Size});
convert_memory({kilobyte, Size}) -> convert_memory({byte, 1000 * Size});
convert_memory({tebibyte, Size}) -> convert_memory({gibibyte, 1024 * Size});
convert_memory({gibibyte, Size}) -> convert_memory({mebibyte, 1024 * Size});
convert_memory({mebibyte, Size}) -> convert_memory({kibibyte, 1024 * Size});
convert_memory({kibibyte, Size}) -> convert_memory({byte, 1024 * Size});
convert_memory({byte, Size}) -> round(Size / erlang:system_info(wordsize)).

convert_time_unit(infinity) -> infinity;
convert_time_unit({week, Time}) -> convert_time_unit({day, 7 * Time});
convert_time_unit({day, Time}) -> convert_time_unit({hour, 24 * Time});
convert_time_unit({hour, Time}) -> convert_time_unit({minute, 60 * Time});
convert_time_unit({minute, Time}) -> convert_time_unit({second, 60 * Time});
convert_time_unit({Unit, Time}) -> erlang:convert_time_unit(Time, Unit, native).
