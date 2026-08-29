-module(kraft).

-moduledoc """
Core API module for the Kraft web framework.

Kraft module provides the primary interface for starting, stopping, and managing
Kraft web servers, as well as rendering templates programmatically.
 
""".

% API
-export([start/1]).
-export([start/2]).
-export([stop/1]).
-export([render/3]).

% Public API
-ignore_xref(start/1).
-ignore_xref(render/3).
-ignore_xref(start/2).
-ignore_xref(stop/0).
-ignore_xref(stop/1).

%--- Types ---------------------------------------------------------------------

-export_type([
    conn/0,
    status/0,
    params/0,
    headers/0,
    body/0,
    response/0,
    route_def/0
]).

-type body() :: kraft_json:body_json() | cowboy_req:resp_body().
-type conn() :: kraft_conn:conn().
-type headers() :: #{binary() => iolist()}.
-type params() :: kraft_conn:params().
-type response() :: kraft_handler:response().
-type route_def() :: kraft_instance:route_def().
-type status() :: kraft_conn:status().

%--- API -----------------------------------------------------------------------

-doc #{equiv => start(Opts, [])}.
start(Opts) -> start(Opts, []).

-doc """
Starts a web server with the specified `Opts` options and `Routes` route definitions.

When running in `dev` mode, the following features are activated
automatically: template reloading, hot reloading, debug mode, etc. Check
`m:kraft_dev` for more information.
""".
-spec start(Opts, Routes) -> Result when
    Opts :: #{
        mode => dev | prod,
        app => atom(),
        port := pos_integer(),
        ssl_opts => [ranch_ssl:opt()]
    },
    Routes :: [kraft_instance:route_def()],
    Result :: kraft_instance:ref().
start(Opts, Routes) ->
    Owner = self(),
    kraft_dev:maybe_start(Opts),
    App = detect_app(Opts),
    {ok, Ref} = kraft_instance:start(#{
        app => App,
        routes => Routes,
        owner => Owner,
        opts => Opts
    }),
    Ref.

%%------------------------------------------------------------------------------
-doc """
Stops the Kraft server instance.
""".
-spec stop(kraft_instance:ref()) -> ok.
stop(Ref) ->
    kraft_instance:stop(Ref).

%%------------------------------------------------------------------------------
-doc """
Renders a template with the given context.

This function allows you to render templates programmatically outside
of the request/response cycle. Useful for generating emails, reports,
or other content.

## Arguments

- `Conn` - Kraft connection object (can be minimal)
- `Template` - Template name (e.g., "email.html")
- `Context` - Data context for template variables

## Returns

- `iodata()` - Rendered HTML content

## Examples

```erlang
% Render a welcome email
EmailHtml = kraft:render(Conn, "welcome_email.html", #{
    name => "John",
    activation_link => "https://example.com/activate/123"
}).

% Render a report template
ReportHtml = kraft:render(Conn, "monthly_report.html", #{
    month => "January",
    data => get_monthly_data()
}).
```

## Template Location

Templates should be located in:
`priv/web/templates/{Template}.mustache`

## See Also

- `kraft_template:render/3` - For advanced template operations
""".
-spec render(kraft_conn:conn(), binary(), map()) -> iodata().
render(Conn0, Template, Context) ->
    kraft_template:render(Conn0, Template, Context).

%--- Internal ------------------------------------------------------------------

detect_app(Opts) ->
    case maps:find(app, Opts) of
        error ->
            case application:get_application() of
                undefined -> error(could_not_determine_app);
                {ok, A} -> A
            end;
        {ok, A} ->
            A
    end.
