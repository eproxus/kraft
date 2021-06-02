-module(blog_index).

-behaviour(kraft_controller).

% API
-export([init/1]).

%--- API -----------------------------------------------------------------------

init(Conn) ->
    kraft:render(Conn, "index.html", #{
        title => "foo",
        author => "bar",
        posts => [P#{id => ID} || {ID, P} <- ets:tab2list(blog_posts)]
    }).
