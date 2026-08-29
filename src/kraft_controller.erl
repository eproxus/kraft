-module(kraft_controller).

-moduledoc """
Template-based controller handler for Kraft framework.

This module provides a simplified controller for rendering HTML templates.
It implements the `kraft_handler` behaviour and is designed for pages that need
to render templates with dynamic data.

Templates are rendered using the `m:kraft_template` module.

## How It Works

The controller follows a simple pattern:
1. Receives a request with optional state data
2. Calls the `init/2` callback with the connection and state
3. Returns a template response with variables

## Usage

```erlang
-module(my_controller).
-behaviour(kraft_controller).

-export([init/2]).

init(Conn, #{user_id := UserId}) ->
    User = fetch_user(UserId),
    Posts = fetch_user_posts(UserId),
    Vars = #{user  => User,
             posts => Posts,
             title => "User Dashboard"
       },
       {respond, Conn, {template, "dashboard.html", Vars}}.
```

## State Data

The state parameter can contain any data passed from the route definition, such
as:
* Global variables for templates
* Configuration settings
* User authentication data
* Pre-fetched data

## See Also
- `m:kraft_handler` - Base handler module
- `m:kraft_template` - Template rendering module
""".

-behaviour(kraft_handler).

% API
-export([exec/1]).

%--- Types ---------------------------------------------------------------------

-doc """
Callback for initializing the controller.

This callback is called when a request is received. It receives the connection
object and optional state data, and should return a response (typically a 
template response).
""".
-callback init(kraft:conn(), State :: any()) -> kraft:response().

%--- API -----------------------------------------------------------------------

-spec exec(kraft_conn:conn()) -> kraft:response().
exec(Conn0) ->
    #{handler := Module, state := State} = kraft_conn:'_meta'(Conn0),
    Module:init(Conn0, State).
