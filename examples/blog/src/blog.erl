-module(blog).

-behaviour(application).

% API
-export([global_vars/1]).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    create_posts(),
    kraft:start(#{port => 8091}, [
        {"/", blog_index, #{}},
        {"/posts/:id", blog_post, #{}},
        {"/pages/:id", blog_page, #{}},
        {"/", kraft_static, #{}}
    ]),
    blog_sup:start_link().

stop(_State) ->
    kraft:stop().

global_vars(Vars) ->
    Meta = maps:from_list(ets:tab2list(blog_meta)),
    Pages = [Page#{id => ID} || {ID, Page} <- ets:tab2list(blog_pages)],
    maps:merge(Meta#{pages => Pages}, Vars).

%--- Internal ------------------------------------------------------------------

create_posts() ->
    ets:new(blog_meta, [named_table]),
    ets:insert(blog_meta, [
        {blog_title, "My Blog"},
        {blog_subtitle, "My personal blog."},
        {blog_author, "Erlang Shen"}
    ]),

    ets:new(blog_pages, [named_table]),
    ets:insert(blog_pages, [
        {"about", #{
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
