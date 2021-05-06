-module(kraft_template).

% API
-export([render/3]).

%--- API -----------------------------------------------------------------------

render(App, File, Context) ->
    Template = bbmustache:parse_file(kraft_file:path(App, template, File)),
    bbmustache:compile(Template, Context, [{key_type, atom}]).
