-module(blog_post).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, #{id := ID}) ->
    {200, #{}, kraft:render(Conn, "post.html",
        blog:global_vars(
            ets:lookup_element(blog_posts, binary_to_integer(ID), 2)
        )
    )}.
