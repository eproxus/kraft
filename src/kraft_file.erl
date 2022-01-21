-module(kraft_file).

% API
-export([path/2]).
-export([path/3]).
-export([relative/2]).

%--- API -----------------------------------------------------------------------

path(App, template) -> filename:join(priv_dir(App), "web/templates");
path(App, static) -> filename:join(priv_dir(App), "web/static").

path(App, template, File) ->
    Template = <<(iolist_to_binary(File))/binary, ".mustache">>,
    filename:join(path(App, template), Template);
path(App, static, File) ->
    filename:join(path(App, static), File).

relative(App, Path) ->
    case string:prefix(Path, priv_dir(App) ++ "/") of
        nomatch -> error({invalid_app_path, App, Path});
        Relative -> filename:join("priv", Relative)
    end.

%--- Internal ------------------------------------------------------------------

priv_dir(App) ->
    case code:priv_dir(App) of
        Dir when is_list(Dir) -> Dir;
        {error, Reason} -> error({priv_dir, Reason})
    end.
