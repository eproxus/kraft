-module(blog_page).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn0, _State) ->
    #{id := ID} = kraft_conn:params(Conn0),
    Vars = ets:lookup_element(blog_pages, binary_to_list(ID), 2),
    {respond, Conn0, {template, "page.html", blog:global_vars(Vars)}}.
