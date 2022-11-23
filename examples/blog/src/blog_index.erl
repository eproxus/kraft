-module(blog_index).

-behaviour(kraft_controller).

% API
-export([init/3]).

%--- API -----------------------------------------------------------------------

init(Conn, _Params, #{message := Message}) ->
    Vars = #{
        message => Message,
        posts => [Post#{id => ID} || {ID, Post} <- ets:tab2list(blog_posts)]
    },
    kraft:render(Conn, "index.html", blog:global_vars(Vars)).
