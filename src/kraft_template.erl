-module(kraft_template).

-moduledoc """
Template engine for Kraft framework.

This module provides template rendering capabilities using BBMustache templates.
It handles template parsing, compilation, caching, and hot-reloading for development.
Templates are stored in the `priv/web/templates/` directory of each application.

## Features

* **Template Parsing**: Parse Mustache template files
* **Compilation**: Compile templates with context data
* **Caching**: Automatic template caching for performance
* **Hot Reloading**: Development mode template updates
* **Partial Support**: Include other templates with `{{> partial.html }}`
* **Dependency Tracking**: Track template dependencies for reloading

## Template Location

Templates are stored in:

```
priv/web/templates/
|-- index.html.mustache
|-- users/
|   |-- list.html.mustache
|   |-- detail.html.mustache
|   `-- themes/
|       |-- default/
|       |   |-- header.html.mustache
|       |   `-- footer.html.mustache
|       `-- admin/
|           |-- header.html.mustache
|           `-- footer.html.mustache
```

## Usage

```erlang
% Render a template with context
kraft_template:render(Conn, "users/list.html", #{
    users => Users,
    title => "User List"
}).

% Create a complete HTTP response
kraft_template:response(Conn, "users/list.html", #{
    users => Users,
    title => "User List"
}).

% Hot reload in development
kraft_template:reload(my_app, "users/list.html").
```

## See Also

- `m:kraft_handler` - Base handler module
- `m:kraft_conn` - Connection module
- `m:bbmustache` - BBMustache template engine
""".

-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

% API
-export([start_link/0]).
-ignore_xref(start_link/0).
-export([render/3]).
-export([reload/2]).
-export([remove/2]).
-export([response/3]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- Types ---------------------------------------------------------------------

-export_type([body_template/0]).

-type body_template() :: {template, file:name(), bbmustache:data()}.

%--- API -----------------------------------------------------------------------

-doc """
Start the template engine process.

This function starts the template engine as a gen_server process.
It initializes the ETS table for dependency tracking and the template cache.
""".
-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-doc """
Render a template with context data.

This function renders a template file with the provided context data.
The template is automatically cached for performance and can include
partials using `{{> partial.html }}` syntax.

Example:
```mustache
{{> themes/default/header.html }}

<main>
    <h2>Lista de Posts</h2>
    
    {{#posts}}
    {{> themes/default/post_slug.html }}
    {{/posts}}
</main>

{{> themes/default/footer.html }}
```
""".
-spec render(kraft_conn:conn(), file:name(), bbmustache:data()) -> binary().
render(Conn0, RawFile, Context) ->
    App = kraft_conn:'_meta'(Conn0, app),
    File = trim(RawFile),
    Template = kraft_cache:retrieve(template, [App, File], fun() ->
        parse(App, File)
    end),
    compile(Template, Context).

-doc """
Reload a template file (development mode).

This function reloads a template file and updates the cache.
It's useful during development when you want to see template changes without
restarting the application. Dependencies are also reloaded.
""".
-spec reload(atom(), file:name()) -> ok.
reload(App, RawFile) -> reload(App, trim(RawFile), main).

-doc """
Remove a template from cache, forcing it to be re-parsed on the next render call.
""".
remove(App, RawFile) ->
    File = trim(RawFile),
    kraft_cache:clear(template, [App, File]),
    ?LOG_INFO(#{template => File, event => removed}, #{kraft_app => App}),
    ok.

-doc """
Create a complete HTTP response with template.

This function renders a template and creates a complete HTTP response
with proper content-type headers. It's a convenience function that
combines `render/3` with response headers.
""".
-spec response(kraft_conn:conn(), file:name(), bbmustache:data()) ->
    kraft_conn:conn().
response(Conn0, Template, Context) ->
    Body = render(Conn0, Template, Context),
    Conn1 = kraft_conn:response_body(Conn0, Body),
    kraft_conn:response_headers(Conn1, #{
        <<"content-type">> => mime_type(Template)
    }).

%--- Callbacks -----------------------------------------------------------------

init(undefined) ->
    ets:new(?MODULE, [named_table, bag, public]),
    kraft_cache:init(template),
    {ok, #{}}.

handle_call(Request, From, _State) -> error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

reload(App, File, Level) ->
    Template = parse(App, File),
    kraft_cache:update(template, [App, File], Template),
    ?LOG_INFO(#{template => File, event => reloaded, level => Level}, #{
        kraft_app => App
    }),
    [reload(App, D, dep) || [D] <- deps(App, File)],
    ok.

trim(File) -> string:trim(iolist_to_binary(File), leading, [$/]).

parse(App, File) ->
    Path = kraft_file:path(App, template, File),
    try
        PartialFileReader = fun(Dir, Key) ->
            deps_insert(App, File, Key),
            bbmustache:default_partial_file_reader(Dir, Key)
        end,
        bbmustache:parse_file(Path, [{partial_file_reader, PartialFileReader}])
    catch
        error:file_not_found ->
            error({missing_template, App, kraft_file:relative(App, Path)})
    end.

compile(Template, Context) ->
    bbmustache:compile(Template, Context, [{key_type, atom}]).

deps_insert(App, File, Key) ->
    Relative =
        case filename:dirname(File) of
            <<".">> -> Key;
            Dir -> filename:join(Dir, Key)
        end,
    ets:insert(?MODULE, {{App, Relative}, File}).

deps(App, File) -> ets:match(?MODULE, {{App, File}, '$1'}).

mime_type(File) ->
    {Type, SubType, []} = cow_mimetypes:all(iolist_to_binary(File)),
    [Type, $/, SubType].
