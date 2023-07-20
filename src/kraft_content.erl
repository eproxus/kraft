-module(kraft_content).

% API
-export([default_handlers/0]).

%--- API -----------------------------------------------------------------------

default_handlers() ->
    #{
        {<<"application">>, <<"json">>} => #{
            shorthand => json,
            decode => fun decode_json/2,
            encode => fun encode_json/1
        },
        {<<"application">>, <<"x-www-form-urlencoded">>} => #{
            shorthand => form,
            decode => fun decode_form/2,
            encode => fun encode_form/1
        }
    }.

decode_json({<<"application">>, <<"json">>, _Params}, Value) ->
    kraft_json:decode(Value).

encode_json(Term) ->
    {<<"application/json; charset=utf8">>, kraft_json:encode(Term)}.

%--- Internal ------------------------------------------------------------------

decode_form(_Type, Value) -> uri_string:dissect_query(Value).

encode_form(Term) when is_map(Term) ->
    encode_form(maps:to_list(Term));
encode_form(Term) when is_list(Term) ->
    {
        <<"application/x-www-form-urlencoded; charset=utf-8">>,
        uri_string:compose_query(Term, [{encoding, utf8}])
    }.
