-module(blog_index).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, _Params) ->
    kraft:render(Conn, "index.html", blog:global_vars(#{
        posts => [Post#{id => ID} || {ID, Post} <- ets:tab2list(blog_posts)]
    })).
