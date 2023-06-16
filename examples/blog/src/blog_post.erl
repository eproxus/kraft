-module(blog_post).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn0, _State) ->
    {#{id := ID}, Conn1} = kraft_conn:params(Conn0),
    Vars = ets:lookup_element(blog_posts, binary_to_integer(ID), 2),
    {respond, Conn1, {template, "post.html", blog:global_vars(Vars)}}.
