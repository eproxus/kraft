-module(kraft_handler).

-behaviour(cowboy_handler).

% Cowboy Handler Callbacks
-export([init/2]).

%--- Types ---------------------------------------------------------------------

-export_type([response/0]).

-type body_raw() :: {raw, iodata()} | iodata().
-type body() :: kraft_template:body_template() | kraft_json:body_json() | body_raw().
-type response_body() :: {respond, kraft_conn:conn(), body()}.
-type response() :: response_body() | kraft_conn:conn().

-callback exec(kraft:conn()) -> response().

%--- Cowboy Handler Callbacks --------------------------------------------------

init(Req0, #{mod := Mod} = State) ->
    Conn0 = kraft_conn:new(Req0, State),
    Conn1 = try
        handle(Mod:exec(Conn0))
    catch
        throw:Reply ->
            kraft_conn:respond(response(Conn0, Reply));
        Class:Reason:Stacktrace ->
            render_error(500, Conn0, Class, Reason, Stacktrace)
    end,
    {cowboy_req, Req1} = kraft_conn:'_adapter'(Conn1),
    {ok, Req1, kraft_conn:'_meta'(Conn1)}.

%--- API -----------------------------------------------------------------------

handle({respond, Conn0, Response}) ->
    kraft_conn:respond(response(Conn0, Response));
handle(Conn0) ->
    kraft_conn:respond(Conn0).

response(Conn0, {template, _, _} = Body) ->
    response(Conn0, {200, #{}, Body});
response(Conn0, {json, _} = Body) ->
    response(Conn0, {200, #{}, Body});
response(Conn0, Body) when is_binary(Body); is_list(Body) ->
    response(Conn0, {200, #{}, Body});
response(Conn0, {Status, Headers, Body}) when is_binary(Body) ->
    kraft_conn:response(Conn0, Status, Headers, Body);
response(Conn0, {Status, Headers, Body}) ->
    Conn1 = body(Conn0, Body),
    Conn2 = kraft_conn:response_headers(Conn1, Headers),
    kraft_conn:response_status(Conn2, Status);
response(Conn0, {Status, Headers}) ->
    response(Conn0, {Status, Headers, <<>>});
response(Conn0, Status) when is_integer(Status) ->
    response(Conn0, {Status, #{}});
response(_Conn0, Reply) ->
    error({invalid_reply, Reply}).

%--- Internal ------------------------------------------------------------------

render_error(Status, Conn0, Class, Reason, Stacktrace) ->
    {Template, Properties, ExtraContext} = render_error(Class, Reason),
    ReasonString = io_lib:format("~p", [Reason]),
    {Method, Conn1} = kraft_conn:method(Conn0),
    {Path, Conn2} = kraft_conn:path(Conn1),
    Context = ExtraContext#{
        title => kraft_http:status(Status),
        message => message(Class, Reason, Stacktrace),
        properties => [
            #{name => method, value => Method},
            #{name => path, value => Path},
            #{name => app, value => kraft_conn:'_meta'(Conn2, app)},
            #{name => class, value => Class},
            #{name => reason, value => ReasonString}
        ] ++ Properties,
        class => Class,
        reason => ReasonString,
        stacktrace => stack_trace(Stacktrace)
    },
    Conn3 = kraft_template:response(
        kraft_conn:'_set_meta'(Conn2, app, kraft), Template, Context
    ),
    Conn4 = kraft_conn:response_status(Conn3, Status),
    kraft_conn:respond(Conn4).

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

body(Conn0, {template, Template, Context}) ->
    kraft_template:response(Conn0, Template, Context);
body(Conn0, {json, JSON}) ->
    kraft_json:response(Conn0, JSON);
body(Conn0, Body) when is_binary(Body); is_list(Body) ->
    kraft_conn:response_body(Conn0, Body).
