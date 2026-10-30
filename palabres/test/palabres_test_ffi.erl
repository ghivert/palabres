-module(palabres_test_ffi).

-export([destroy/0, sleep/2, encode/1, format_event/3]).

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

format_event(Kind, Json, AutoWhen, Defaults) ->
  {Msg, Meta} = event(Kind),
  Event = #{level => info, msg => Msg, meta => Meta#{time => ?TIME}},
  Options = #{color => false, json => Json, auto_when => AutoWhen, default_fields => Defaults},
  Formatted = unicode:characters_to_binary(palabres_ffi:format(Event, Options)),
  case Json of
    false -> Formatted;
    true -> iolist_to_binary(json:encode(json:decode(string:trim(Formatted))))
  end.

% `io:format/2` style message, as sent by `logger:info("…", [...])`.
event(<<"format_args">>) ->
  {{"user ~p logged in", [42]}, #{}};
% Format string with a missing argument: must not crash the logger.
event(<<"bad_format_args">>) ->
  {{"user ~p logged in ~p", [-42]}, #{}}.
