-module(kraft_controller).

-behaviour(cowboy_handler).

% API
-export([init/2]).

%--- Types ---------------------------------------------------------------------

-callback init(kraft:conn(), kraft:params(), Args :: any()) ->
    {kraft:status(), kraft:headers(), kraft:body()}
    | {kraft:status(), kraft:headers(), kraft:body(), NewArgs :: any()}.

%--- API -----------------------------------------------------------------------

init(Req0, State) ->
    Conn0 = kraft_conn:new(Req0, State),
    try
        Conn1 = init_exec(Conn0),
        {cowboy_req, Req1} = kraft_conn:'_adapter'(Conn1),
        {ok, Req1, kraft_conn:'_meta'(Conn1)}
    catch
        Class:Reason:Stacktrace ->
            ReqErr = render_error(500, Conn0, Req0, Class, Reason, Stacktrace),
            {ok, ReqErr, State}
    end.

%--- Internal ------------------------------------------------------------------

init_exec(Conn0) ->
    #{handler := Handler, state := MState0} = kraft_conn:'_meta'(Conn0),
    Result = Handler:init(Conn0, kraft_conn:params(Conn0), MState0),
    try
        init_verify(Conn0, Result, MState0)
    catch
        invalid_return -> error({kraft, ?MODULE, {invalid_return, Result}})
    end.

init_verify(Conn0, {kraft_template, Headers, Body}, S0) ->
    init_verify(Conn0, {200, Headers, Body}, S0);
init_verify(Conn0, {Code, Headers, Body}, S0) ->
    init_verify(Conn0, {Code, Headers, Body, S0}, undefined);
init_verify(Conn0, {Code, Headers, Body, S1}, _S0) when
    is_integer(Code), is_map(Headers)
->
    {NewHeaders, NewBody} = collect_headers(Headers, Body),
    Conn1 = kraft_conn:merge_resp_headers(Conn0, NewHeaders),
    Conn2 = kraft_conn:send_resp(Conn1, Code, NewBody),
    kraft_conn:'_set_meta'(Conn2, state, S1);
init_verify(_Conn, _Result, _S0) ->
    throw(invalid_return).

collect_headers(Headers, {kraft_template, TemplateHeaders, TemplateBody}) ->
    {maps:merge(TemplateHeaders, Headers), TemplateBody};
collect_headers(Headers, Body) when is_binary(Body) ->
    {Headers, Body};
collect_headers(_Headers, _Body) ->
    throw(invalid_return).

render_error(Code, Conn, Req, Class, Reason, Stacktrace) ->
    {Template, Properties, ExtraContext} = render_error(Class, Reason),
    ReasonString = io_lib:format("~p", [Reason]),
    Context = ExtraContext#{
        title => kraft_http:status(Code),
        message => message(Class, Reason, Stacktrace),
        properties => [
            #{name => method, value => maps:get(method, Req)},
            #{name => path, value => maps:get(path, Req)},
            #{
                name => app,
                value => kraft_conn:'_meta'(Conn, app)
            },
            #{name => class, value => Class},
            #{name => reason, value => ReasonString}
        ] ++ Properties,
        class => Class,
        reason => ReasonString,
        stacktrace => stack_trace(Stacktrace)
    },
    Body = kraft:render(
        kraft_conn:'_set_meta'(Conn, app, kraft), Template, Context
    ),
    init_verify(Conn, {Code, #{}, Body}, #{}).

render_error(error, {missing_template, _App, Path}) ->
    {"error_missing_template.html", [], #{template => Path, warning => true}};
render_error(_Class, _Reason) ->
    {"error_exception.html", [], #{error => true}}.

message(Class, Reason, [Call | _]) ->
    Formatted = erl_error:format_exception(Class, Reason, [Call]),
    BClass = atom_to_binary(Class),
    Pattern = <<"^exception ", BClass/binary, ": (?<error>.*?):?\s*$">>,
    Opts = [{capture, all_names, binary}, anchored, multiline],
    {match, [Message]} = re:run(Formatted, Pattern, Opts),
    Message.

stack_trace(Stacktrace) ->
    #{stack => #{items => lists:map(fun stack_call/1, Stacktrace)}}.

stack_call({M, F, A, Attrs}) ->
    Call = #{module => M, func => F},
    lists:foldl(fun stack_attr/2, Call, [{args, A} | Attrs]).

stack_attr({args, Arity}, Call) when is_integer(Arity) ->
    Call#{arity => Arity};
stack_attr({args, Args}, #{module := M, func := F} = Call) when is_list(Args) ->
    ArgsFormat = lists:join(",", lists:duplicate(length(Args), "~p")),
    Format = lists:flatten(["~p:~p(", ArgsFormat, ")"]),
    Pretty = io_lib:format(Format, [M, F] ++ Args),
    FormattedArgs = string:prefix(Pretty, io_lib:format("~p:~p", [M, F])),
    Call#{args => iolist_to_binary(FormattedArgs)};
stack_attr({file, "/" ++ _ = File}, Call) ->
    Call#{
        file => #{path => iolist_to_binary(File)},
        dir => strip_prefix(iolist_to_binary(filename:dirname(File))),
        name => iolist_to_binary(filename:basename(File))
    };
stack_attr({file, OTPFile}, Call) ->
    Call#{file => #{name => iolist_to_binary(OTPFile)}, otp => true};
stack_attr({line, Line}, Call) ->
    Call#{line => Line};
stack_attr(_Attr, Call) ->
    Call.

strip_prefix(Path) ->
    {ok, CWD} = file:get_cwd(),
    strip_if_prefix(Path, CWD ++ "/").

strip_if_prefix(String, Prefix) ->
    case string:prefix(String, Prefix) of
        nomatch -> String;
        Short -> Short
    end.
