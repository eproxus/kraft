-module(kraft_template).

% API
-export([render/3]).

%--- API -----------------------------------------------------------------------

render(App, File, Context) ->
    Path = kraft_file:path(App, template, File),
    try
        Template = bbmustache:parse_file(Path),
        bbmustache:compile(Template, Context, [{key_type, atom}])
    catch
        error:file_not_found ->
            error({missing_template, App, kraft_file:relative(App, Path)})
    end.
