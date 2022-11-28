-module(kraft_logger).

% API
-ignore_xref(log/2).
-export([log/2]).
-ignore_xref(format/2).
-export([format/2]).
-ignore_xref(filter/2).
-export([filter/2]).

%--- API -----------------------------------------------------------------------

log(Event, _Config) ->
    io:format("~p", [Event]).

format(
    #{level := Level, msg := Msg, meta := #{kraft_app := _} = Meta}, _Config
) ->
    io_lib:format("[~s] ~s ~s~n", [
        format_level(Level),
        format_kraft_app(Meta),
        format_kraft(Msg)
    ]);
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    io_lib:format("[~s] ~s ~s~n", [
        format_level(Level),
        format_otp(Msg),
        format_meta(Meta)
    ]).

filter(#{msg := {report, #{label := {supervisor, progress}}}}, _Config) ->
    stop;
filter(Event, _Config) ->
    Event.

%--- Internal ------------------------------------------------------------------

format_level(Level) -> string:uppercase(atom_to_binary(Level)).

format_kraft_app(#{kraft_app := kraft}) ->
    <<"kraft:">>;
format_kraft_app(#{kraft_app := App}) ->
    [<<"kraft/">>, atom_to_binary(App), <<":">>].

format_kraft({report, Report}) -> kraft_report(Report);
format_kraft({Msg, Format}) -> io_lib:format(Msg, Format).

kraft_report(#{monitor := _Dir, event := started}) ->
    <<"template monitoring started">>;
kraft_report(#{monitor := Dir, event := duplicate}) ->
    [<<"already watched, ignoring ">>, $", relative(Dir), $"];
kraft_report(#{monitor := _Dir, event := {watcher_error, _, {exit, Status}}}) ->
    io_lib:format("watcher exited with status ~b", [Status]);
kraft_report(#{template := File, event := reloaded, level := main}) ->
    io_lib:format("template \"~s\" reloaded", [File]);
kraft_report(#{template := File, event := reloaded, level := dep}) ->
    io_lib:format("dependent template \"~s\" reloaded", [File]);
kraft_report(#{started := #{port := Port}}) ->
    URI = uri_string:recompose(#{
        scheme => <<"http">>,
        host => <<"localhost">>,
        port => Port,
        path => []
    }),
    io_lib:format("listening on ~s", [URI]);
kraft_report(Report) ->
    io_lib:format("~p", [Report]).

format_otp({report, Report}) -> otp_report(Report);
format_otp({Msg, Format}) -> io_lib:format(Msg, Format).

otp_report(#{label := {application_controller, progress}, report := Report}) ->
    App = proplists:get_value(application, Report),
    case proplists:get_value(started_at, Report) of
        undefined -> error({unknown_app_event, Report});
        _Node -> io_lib:format("application ~p started", [App])
    end;
otp_report(#{label := {supervisor, progress}, report := Report}) ->
    {_, Name} = proplists:get_value(supervisor, Report),
    case proplists:get_value(started, Report) of
        undefined ->
            error({unknown_supervsior_event, Report});
        Started ->
            Pid = proplists:get_value(pid, Started),
            io_lib:format("supervisor ~p started as ~p", [Name, Pid])
    end;
otp_report(Report) ->
    io_lib:format("~p", [Report]).

format_meta(#{pid := Pid}) ->
    io_lib:format("[pid=~p]", [Pid]).

relative(Dir) ->
    {ok, CWD} = file:get_cwd(),
    case string:prefix(Dir, CWD) of
        nomatch -> Dir;
        Relative -> string:trim(Relative, leading, "/")
    end.
