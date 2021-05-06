-module(kraft_fallback_handler).

% API
-export([init/2]).

%--- API -----------------------------------------------------------------------

init(#{path := Path, method := Method} = Req, State) ->
    Headers = #{<<"content-type">> => <<"text/html">>},
    Body = kraft_template:render(kraft, "404.html", #{
        req => #{
            url => Path,
            method => Method
        }
    }),
    Resp = cowboy_req:reply(404, Headers, Body, Req),
    {ok, Resp, State}.
