-module(blog_page).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, #{id := ID}) ->
    Vars = ets:lookup_element(blog_pages, binary_to_list(ID), 2),
    {200, #{}, kraft:render(Conn, "page.html", blog:global_vars(Vars))}.
