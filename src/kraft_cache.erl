-module(kraft_cache).

% API
-export([init/1]).
-export([retrieve/3]).
-export([update/3]).
-ignore_xref([{?MODULE, clear, 2}]).
-export([clear/2]).

%--- Types ---------------------------------------------------------------------

-type type() :: render | template.
-type config() :: any().
-type ref() :: any().
-type keypath() :: [any()].

-callback init(type(), config()) -> ref().
-callback get(ref(), keypath()) -> {hit, iodata()} | miss.
-callback update(ref(), keypath(), iodata()) -> ok.
-callback clean_cache(ref()) -> ok.

-export_type([type/0]).

%--- API -----------------------------------------------------------------------

init(Type) ->
    Settings = application:get_env(kraft, cache, #{}),
    {Module, Config} = maps:get(Type, Settings),
    Ref = Module:init(Type, Config),
    persistent_term:put({?MODULE, Type}, {Module, Ref}).

retrieve(Type, Keypath, Refresh) ->
    {Module, Ref} = persistent_term:get({?MODULE, Type}),
    case Module:get(Ref, Keypath) of
        {hit, Entry} ->
            Entry;
        miss ->
            Entry = Refresh(),
            Module:update(Ref, Keypath, Entry),
            Entry
    end.

update(Type, Keypath, Entry) ->
    {Module, Ref} = persistent_term:get({?MODULE, Type}),
    Module:update(Ref, Keypath, Entry).

clear(Type, Keypath) ->
    {Module, Ref} = persistent_term:get({?MODULE, Type}),
    Module:clear(Ref, Keypath).
