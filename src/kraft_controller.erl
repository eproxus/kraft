-module(kraft_controller).

-behaviour(cowboy_handler).

% API
-export([init/2]).

%--- Types ---------------------------------------------------------------------

-callback init(kraft:conn(), kraft:params()) ->
    {kraft:status(), kraft:headers(), kraft:body()}.

%--- API -----------------------------------------------------------------------

init(#{path := Path, method := Method} = Req0, #{handler := Handler} = State) ->
    try
        Conn0 = kraft_conn:new(Req0, #{app => maps:get(app, State)}),
        {Status, Headers, Body} =
            case Handler:init(Conn0, kraft_conn:params(Conn0)) of
                {C, H, {kraft_template, TH, B}} when is_integer(C), is_map(H) ->
                    {C, maps:merge(TH, H), B};
                {C, H, B} when is_binary(B), is_integer(C), is_map(H) ->
                    {C, H, B};
                Invalid ->
                    error({kraft, ?MODULE, {invalid_return, Invalid}})
            end,
        Conn1 = kraft_conn:merge_resp_headers(Conn0, Headers),
        Conn2 = kraft_conn:send_resp(Conn1, Status, Body),
        {cowboy_req, Req1} = kraft_conn:'_adapter'(Conn2),
        {ok, Req1, State}
    catch
        Class:Reason:StackTrace ->
            EHeaders = #{<<"content-type">> => <<"text/html">>},
            Exception = erl_error:format_exception(Class, Reason, StackTrace),
            EBody = kraft_template:render(kraft, "500.html", #{
                req => #{url => Path, method => Method},
                exception => Exception
            }),
            EResp = cowboy_req:reply(500, EHeaders, EBody, Req0),
            {ok, EResp, State}
    end.
