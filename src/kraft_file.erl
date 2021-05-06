-module(kraft_file).

% API
-export([path/2]).
-export([path/3]).

%--- API -----------------------------------------------------------------------

path(App, template) -> filename:join([priv_dir(App), "web/templates"]);
path(App, static) -> filename:join([priv_dir(App), "web/static"]).

path(App, template, File) ->
    filename:join([path(App, template), File ++ ".mustache"]);
path(App, static, File) ->
    filename:join([path(App, static), File]).

%--- Internal ------------------------------------------------------------------

priv_dir(App) ->
    case code:priv_dir(App) of
        Dir when is_list(Dir) -> Dir;
        {error, Reason} -> error({priv_dir, Reason})
    end.
