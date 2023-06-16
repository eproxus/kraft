-module(blog_index).

-behaviour(kraft_controller).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(Conn, #{message := Message}) ->
    Vars = #{
        message => Message,
        posts => [Post#{id => ID} || {ID, Post} <- ets:tab2list(blog_posts)]
    },
    {respond, Conn, {template, "index.html", blog:global_vars(Vars)}}.
