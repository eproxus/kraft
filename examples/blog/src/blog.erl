-module(blog).

-behaviour(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    create_posts(),
    kraft:start(#{port => 8091}, [
        {"/", blog_index, #{}},
        {"/", kraft_static, #{}}
    ]),
    blog_sup:start_link().

stop(_State) ->
    ok.

%--- Internal ------------------------------------------------------------------

create_posts() ->
    ets:new(blog_meta, [named_table]),
    ets:insert(blog_meta, [
        {author, "Adam Lindberg"},
        {email, "hello@alind.io"},
        {description, "My personal blog."}
    ]),

    ets:new(blog_pages, [named_table]),
    ets:insert(blog_pages, [
        {1, #{
            title => "About",
            content => "I'm a blogger."
        }}
    ]),

    ets:new(blog_posts, [named_table, ordered_set]),
    ets:insert_new(blog_posts, [
        {1, #{
            title => "My first post",
            slug => "The beginning.",
            content => "So much content!"
        }},
        {2, #{
            title => "The next article",
            slug => "Additional news.",
            content => "Very profound."
        }},
        {3, #{
            title => "Follow up",
            slug => "Really, the follow up.",
            content => "I had more to say."
        }}
    ]).
