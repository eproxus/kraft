-module(blog_post).

-behaviour(kraft_controller).

% API
-export([init/3]).

%--- API -----------------------------------------------------------------------

init(Conn, #{id := ID}, _State) ->
    Vars = ets:lookup_element(blog_posts, binary_to_integer(ID), 2),
    {200, #{}, kraft:render(Conn, "post.html", blog:global_vars(Vars))}.
