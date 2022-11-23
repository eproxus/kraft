-module(kraft_file).

% API
-export([path/2]).
-export([path/3]).
-export([relative/2]).

%--- API -----------------------------------------------------------------------

path(App, static) -> path(App, static, "").

path(App, template, File) ->
    Template = <<(iolist_to_binary(File))/binary, ".mustache">>,
    safe_priv_path(App, "web/templates", Template);
path(App, static, File) ->
    safe_priv_path(App, "web/static", File).

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

safe_priv_path(App, Path, File) ->
    Dir = filename:join(priv_dir(App), Path),
    case filelib:safe_relative_path(File, Dir) of
        unsafe -> error({unsafe_include, File});
        FinalPath -> filename:join(Dir, FinalPath)
    end.
