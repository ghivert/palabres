-module(palabres_test_ffi).

-export([destroy/0, sleep/2, encode/1, format_event/3, format_event_with_globals/2]).

% Timestamp of every synthetic event: 2024-12-15T15:59:17.123Z, in
% microseconds since the epoch, as `logger` stores it in `meta`.
-define(TIME, 1734278357123456).

% Used only for testing purposes.
destroy() ->
  logger:remove_handler(palabres_logger),
  logger:update_handler_config(default,
    #{formatter => {logger_formatter, #{legacy_header => true,single_line => false}},
      filters =>
        [{remote_gl, {fun logger_filters:remote_gl/2, stop}},
         {domain, {fun logger_filters:domain/2, {log, super, [otp, sasl]}}},
         {no_domain, {fun logger_filters:domain/2, {log, undefined, []}}}]}),
  nil.

sleep(Timeout, Continuation) ->
  timer:sleep(Timeout),
  Continuation(),
  nil.

encode(A) ->
  B = json:encode(A),
  iolist_to_binary(B).

% Run the Palabres formatter on a synthetic `logger` event, exactly as
% `logger` would call it. The formatter is a pure function of the event and
% the formatter options, which makes it testable without going through the
% handler. Color is never activated, to keep the expectations readable.
% JSON output is decoded and encoded again, to compare it with sorted keys
% and without trailing newline.
format_event(Kind, Json, AutoWhen) ->
  format_event(Kind, Json, AutoWhen, []).

% Same, with a global field `service=api`, and without timestamp.
format_event_with_globals(Kind, Json) ->
  Globals = [palabres@field:string(<<"service">>, <<"api">>)],
  format_event(Kind, Json, false, Globals).

format_event(Kind, Json, AutoWhen, Globals) ->
  {Msg, Meta} = event(Kind),
  Event = #{level => info, msg => Msg, meta => Meta#{time => ?TIME}},
  Options = #{color => false, json => Json, auto_when => AutoWhen, fields => Globals},
  Formatted = unicode:characters_to_binary(palabres_ffi:format(Event, Options)),
  case Json of
    false -> Formatted;
    true -> iolist_to_binary(json:encode(json:decode(string:trim(Formatted))))
  end.

% Palabres log, as sent by `palabres_ffi:log/4`.
event(<<"palabres">>) ->
  Fields = [{<<"user">>, [{int_field, 42}]}],
  {{report, #{palabres => {Fields, <<"hello">>, none}}}, #{}};
% String message as a charlist, as sent by `logger:info("…")`.
event(<<"string_charlist">>) ->
  {{string, "plain string"}, #{}};
% `io:format/2` style message, as sent by `logger:info("…", [...])`.
event(<<"format_args">>) ->
  {{"user ~p logged in", [42]}, #{}};
% Format string with a missing argument: must not crash the logger.
event(<<"bad_format_args">>) ->
  {{"user ~p logged in ~p", [-42]}, #{}};
% Report as a map, as sent by `logger:info(#{...})`.
event(<<"map_report">>) ->
  {{report, #{user => 42, action => <<"login">>}}, #{}};
% Report holding charlists, the strings of Erlang code, as a key and as
% values, next to a list that is not a string and to an empty list.
event(<<"charlist_report">>) ->
  {{report, [{"name", "hello"}, {ids, [1, 2]}, {empty, []}]}, #{}};
% Report as a key-value list, as sent by `logger:info([{...}])`. The handler
% injects the default `report_cb` on every report without one before calling
% the formatter: it must not take over the structured rendering.
event(<<"list_report">>) ->
  {{report, [{user, 42}, {action, <<"login">>}]},
   #{report_cb => fun logger:format_otp_report/1}};
% Report with a `report_cb/1`, returning a format and its arguments, as OTP
% crash reports do.
event(<<"report_cb_1">>) ->
  Cb = fun(#{user := User}) -> {"user ~p crashed", [User]} end,
  {{report, #{user => 42}}, #{report_cb => Cb}};
% Report with a `report_cb/2`, returning chardata.
event(<<"report_cb_2">>) ->
  Cb = fun(#{user := User}, _Options) -> io_lib:format("user ~p crashed", [User]) end,
  {{report, #{user => 42}}, #{report_cb => Cb}}.
