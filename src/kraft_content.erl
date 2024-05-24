-module(kraft_content).

% API
-export([init_handlers/1]).
-export([decode/3]).

%--- API -----------------------------------------------------------------------

init_handlers(Key) ->
    persistent_term:put({?MODULE, Key}, #{
        {<<"application">>, <<"json">>} => #{
            shorthand => json,
            decode => fun decode_json/2,
            encode => fun encode_json/2
        },
        {<<"application">>, <<"x-www-form-urlencoded">>} => #{
            shorthand => form,
            decode => fun decode_form/2,
            encode => fun encode_form/2
        }
    }).

decode(Key, {Type, SubType, _Params} = ContentType, Content) ->
    case maps:find({Type, SubType}, persistent_term:get({?MODULE, Key})) of
        {ok, #{shorthand := Short, decode := Decode}} ->
            {Short, Decode(ContentType, Content)};
        error ->
            {raw, ContentType, Content}
    end.

decode_json({<<"application">>, <<"json">>, _Params}, Value) ->
    kraft_json:decode(Value).

encode_json(Term, _Opts) ->
    {<<"application/json; charset=utf8">>, kraft_json:encode(Term)}.

%--- Internal ------------------------------------------------------------------

decode_form({<<"application">>, <<"x-www-form-urlencoded">>, _Params}, Value) ->
    uri_string:dissect_query(Value).

encode_form(Term, Opts) when is_map(Term) ->
    encode_form(maps:to_list(Term), Opts);
encode_form(Term, _Opts) when is_list(Term) ->
    {
        <<"application/x-www-form-urlencoded; charset=utf-8">>,
        uri_string:compose_query(Term, [{encoding, utf8}])
    }.
