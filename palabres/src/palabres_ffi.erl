-module(palabres_ffi).

-export([configure/1, format/2, log/4, locate/1, allowed/1]).

-include_lib("palabres/include/palabres@options_Options.hrl").

-define(AUTO_AT_KEY, {palabres, auto_at}).
-define(FORMATTING_KEY, {palabres, formatting}).

% Configure Palabres logger, by updating the primary config, and setting things
% up according to the Palabres configuration.
% `configure/1` should be called once at startup, and should probably never be
% called anymore. If the modification of the logger is required, it is advised
% to use the functions of the `logger` module.
% The palabres logger is identified with the atom `palabres_logger`.
-spec configure(#options{}) -> nil.
configure(Options) ->
  update_primary_config(Options),
  add_palabres_logger(Options),
  style_default_logger(Options),
  store_auto_at(Options).

% Store the automatic location flag. It is read on every log creation, which
% is exactly the use case of `persistent_term`: written once at startup, read
% often. The value is only written when it changes, as every update of a
% persistent term triggers a global garbage collection.
-spec store_auto_at(#options{}) -> nil.
store_auto_at(Options) ->
  AutoAt = Options#options.auto_at,
  case persistent_term:get(?AUTO_AT_KEY, undefined) of
    AutoAt -> nil;
    _ -> persistent_term:put(?AUTO_AT_KEY, AutoAt), nil
  end.

% Update the primary config of logger, setting the log level, and filtering
% some domains by default: progress and supervisor reports of OTP, which live
% in the `[otp, sasl]` domain. The filter gets a distinct id, to avoid
% clobbering a `domain` filter installed by the user.
-spec update_primary_config(#options{}) -> nil.
update_primary_config(Options) ->
  Filter = {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}},
  logger:update_primary_config(#{
    level => Options#options.level,
    filter_default => log,
    filters => [{palabres_domain, Filter}]}),
  nil.

% Add the `palabres_logger` to the logger, living besides the `default` logger.
% `palabres_logger` accepts only logs emitted by Palabres, and will simply
% ignore every logs coming from the outside world. Every logs flowing in the
% Palabres logger should be sent using `log/4`.
% The Palabres logger can output on standard IO, but can also log in a file,
% depending on Palabres configuration.
% When the handler already exists, because `configure/1` has been called
% before, it is reconfigured in place. `logger_std_h` refuses to change its
% output once started: in that case, the handler is replaced.
-spec add_palabres_logger(#options{}) -> nil.
add_palabres_logger(Options) ->
  Color = read_color(Options#options.color),
  Config =
    #{formatter => {palabres_ffi, format_options(Color, Options)},
      filters => [{palabres_filter, {fun handler_filter/2, continue}}],
      config =>
        case Options#options.output of
          stdout -> #{type => standard_io};
          {file, FileName, Interval} ->
            #{file => unicode:characters_to_list(FileName),
              filesync_repeat_interval =>
                case Interval of
                  {some, Millis} -> Millis;
                  none -> 5000
                end}
        end},
  case logger:add_handler(palabres_logger, logger_std_h, Config) of
    {error, {already_exist, palabres_logger}} ->
      case logger:update_handler_config(palabres_logger, Config) of
        ok -> nil;
        {error, _} ->
          logger:remove_handler(palabres_logger),
          logger:add_handler(palabres_logger, logger_std_h, Config),
          nil
      end;
    _ -> nil
  end.

% Style the default logger if desired. Palabres is opinionated, and as such,
% format the logs in query string or in JSON. By default, Palabres styles every
% logs, from those flowing to the Palabres logger to those going to the default
% logger.
% Styling logs in the default logger can be deactivated though. As a
% consequence, `style_default_logger` will apply the Palabres styling to the
% logger if desired, and will not do anything otherwise.
% In any case, Palabres logs are disabled in the default logger.
-spec style_default_logger(#options{}) -> nil.
style_default_logger(Options) ->
  Color = read_color(Options#options.color),
  Filters = [{palabres_filter, {fun handler_filter/2, stop}}],
  case Options#options.style_default of
    false -> logger:update_handler_config(default, #{filters => Filters});
    true ->
      Formatter = {palabres_ffi, format_options(Color, Options)},
      HandlerOptions = #{formatter => Formatter, filters => Filters},
      logger:update_handler_config(default, HandlerOptions)
  end,
  nil.

% Options given to the formatter, shared by the Palabres and default handlers.
-spec format_options(boolean(), #options{}) -> map().
format_options(Color, Options) ->
  #{color => Color,
    json => Options#options.json,
    auto_when => Options#options.auto_when,
    default_fields => Options#options.default_fields}.

% Filter events for the loggers. Behaves differently for Palabres and for the
% default logger if default style has been overriden.
% - For Palabres logger (`Extra =:= continue`), let Palabres logs flow to the
%   logger and prevent other logs to flow.
% - For default logger, (`Extra =:= stop`), prevent Palabres logs to flow to the
%   logger, and allow other logs to flow.
handler_filter(Event, Extra) ->
  case {Event, Extra} of
    {#{msg := {report, #{palabres := {_, _, _}}}}, stop} -> stop;
    {#{msg := {report, #{palabres := {_, _, _}}}}, continue} -> Event;
    {_, stop} -> Event;
    {_, continue} -> stop
  end.

% Read if color is activated or not. In case color activation has been set by
% hand by the user, the choice is immediately returned. Otherwise, the
% environment variables `NO_COLOUR` and `NO_COLOR` will be read, and will
% deactivate color when set. By default, color is activated.
-spec read_color({some, boolean()} | none) -> boolean().
read_color(Color) ->
  case Color of
    {some, Clr} -> Clr;
    none ->
      NO_COLOUR = read_variable("NO_COLOUR"),
      NO_COLOR = NO_COLOUR orelse read_variable("NO_COLOR"),
      not NO_COLOR
  end.

% Read a flag variable from the environment, following the `NO_COLOR`
% convention (https://no-color.org): the flag is set when the variable is
% defined with any non-empty value, whatever it is.
-spec read_variable(Name :: os:env_var_name()) -> boolean().
read_variable(Name) ->
  case os:getenv(Name) of
    false -> false;
    "" -> false;
    _ -> true
  end.

% Helper to run `log` from Gleam `palabres` module. The log is sent as a
% `logger:report()`, a map, holding the Palabres data under the `palabres`
% key, so that any other handler receives a valid report.
log(Level, Fields, Msg, At) ->
  logger:log(Level, #{palabres => {Fields, Msg, At}}),
  nil.

% Locate the function creating the log, by reading the current stack trace and
% skipping Palabres frames. The stack is only read when the log is going to be
% printed, i.e. when the level is allowed by the primary configuration, which
% is what Palabres configures. `logger:allow/2` would be cheaper, but it is
% not part of the public API.
% `current_stacktrace` on `self()` does not raise nor suspend anything, and is
% bounded by `backtrace_depth` (8 by default), which is more than enough as
% the caller sits at most 3 frames away.
-spec locate(logger:level()) -> {some, {location, binary(), {some, binary()} | none}} | none.
locate(Level) ->
  case persistent_term:get(?AUTO_AT_KEY, true) andalso allowed(Level) of
    false -> none;
    true ->
      {current_stacktrace, Frames} = erlang:process_info(self(), current_stacktrace),
      first_location(Frames)
  end.

% Whether the primary configuration lets a log at that level through. Also
% used by `palabres:log/1` to skip deferred computations on dropped logs.
-spec allowed(logger:level()) -> boolean().
allowed(Level) ->
  #{level := Primary} = logger:get_primary_config(),
  % `lt` means the log is less severe than the primary level, i.e. filtered.
  logger:compare_levels(Level, Primary) =/= lt.

first_location([{Module, _, _, _} | Rest]) when Module =:= palabres; Module =:= palabres_ffi ->
  first_location(Rest);
first_location([{Module, Function, _, _} | _]) ->
  {some, {location, module_name(Module), {some, function_name(Function)}}};
first_location([]) ->
  none.

% Gleam modules are compiled to `my_app@web@router`. Display them as the
% Gleam import path, `my_app/web/router`.
-spec module_name(module()) -> binary().
module_name(Module) ->
  binary:replace(atom_to_binary(Module), <<"@">>, <<"/">>, [global]).

% Anonymous functions are compiled to `-function/arity-fun-0-`. Recover the
% name of the enclosing function, which is what the user is looking for.
-spec function_name(atom()) -> binary().
function_name(Function) ->
  case atom_to_binary(Function) of
    <<"-", Rest/binary>> ->
      [Name | _] = binary:split(Rest, <<"/">>),
      Name;
    Name -> Name
  end.

% Default entrypoint for the logger module. `palabres_ffi` acts as a logger
% module, and defines a standard format entrypoint.
format(#{level := Level, msg := Msg, meta := Meta}, Options) ->
  case maps:get(json, Options) of
    false ->
      Level1 = format_level(Level, Options),
      Msg1 = format_msg(Msg, Meta, Options),
      [Level1, Msg1, $\n];
    true ->
      JsonOptions = maps:put(color, false, Options),
      JsonData0 = format_msg(Msg, Meta, JsonOptions),
      JsonData1 = maps:put(<<"level">>, Level, JsonData0),
      JsonIo = json:encode(JsonData1),
      JsonString = iolist_to_binary(JsonIo),
      [JsonString, $\n]
  end.

% Format the level indicator for query string, with an appropriate color. In
% case color is deactivated (because it has been deactivated by user, or because
% `NO_COLOUR` or `NO_COLOR` variable has been set), the level is returned
% without any color. `logger` levels are the atoms of the Gleam `Level` type.
format_level(Level, #{color := Color}) ->
  palabres@internals@converter:format_level(Level, Color).

% Default log formatter. Format logs in JSON or query string, depending on the
% mode setup at start. Any Palabres log will go through a specific formatting,
% while every other log will go through a standard formatter. In both case,
% Palabres tries to format the log in a nicely way to be easily parsed by a
% machine, or read by a human.
format_msg(Report0, Meta, Options0) ->
  Options = Options0#{timestamp => format_time(Meta)},
  % A lazy default field can itself log: the formatter then runs again, in
  % the same process, while this log is being formatted. The nested log is
  % formatted without default fields, otherwise the lazy field would be
  % evaluated again, forever.
  case erlang:put(?FORMATTING_KEY, true) of
    true -> do_format_msg(Report0, Meta, Options#{default_fields => []});
    undefined ->
      try do_format_msg(Report0, Meta, Options)
      after erlang:erase(?FORMATTING_KEY)
      end
  end.

do_format_msg(Report0, Meta, Options) ->
  case Report0 of
    {string, Msg} ->
      format_string_msg(Msg, Options);
    {report, #{palabres := {Fields, Msg, At}}} ->
      format_palabres_report(Fields, Msg, At, Options);
    {report, Report1} when is_map(Report1); is_list(Report1) ->
      format_report(Report1, Meta, Options);
    {Format, Args} when is_list(Args) ->
      format_string_msg(format_io(Format, Args), Options);
    % `logger` only emits the three forms above: anything else is printed
    % raw, rather than crashing the formatter.
    _ ->
      format_string_msg(inspect(Report0), Options)
  end.

% Format reports not emitted by Palabres: maps or key-value lists sent to
% `logger` by Erlang code, or by OTP itself. When the event carries a
% `report_cb`, OTP's convention to render a report as text, it is used to get
% a readable message, as for crash reports. Otherwise, the report is displayed
% as structured fields, exactly like a Palabres log without message. A
% malformed report never crashes the logger: it is then printed raw.
format_report(Report, Meta, Options) ->
  case report_callback(Report, Meta) of
    {ok, Msg} -> format_string_msg(Msg, Options);
    error ->
      try report_fields(Report) of
        Fields -> format_palabres_report(Fields, <<>>, none, Options)
      catch
        error:_ -> format_string_msg(inspect(Report), Options)
      end
  end.

% Render a report with its `report_cb`, when the event has one. `report_cb/1`
% returns a format and its arguments, `report_cb/2` returns chardata, and
% takes the formatting options of `logger_formatter`. A failing or misbehaving
% callback is ignored, and the report falls back to its structured rendering.
% The handler injects `logger:format_otp_report/1` when the event has no
% callback: it dumps the report as `Key: Value` lines, which is exactly what
% the structured rendering does better, so it is ignored as well.
-spec report_callback(logger:report(), logger:metadata()) -> {ok, binary()} | error.
report_callback(Report, #{report_cb := Callback}) when is_function(Callback, 1) ->
  case Callback =:= fun logger:format_otp_report/1 of
    true -> error;
    false ->
      try Callback(Report) of
        {Format, Args} when is_list(Args) -> {ok, format_io(Format, Args)};
        _ -> error
      catch
        _:_ -> error
      end
  end;
report_callback(Report, #{report_cb := Callback}) when is_function(Callback, 2) ->
  FormatOptions = #{depth => unlimited, chars_limit => unlimited, single_line => true},
  try unicode:characters_to_binary(Callback(Report, FormatOptions)) of
    Msg when is_binary(Msg) -> {ok, Msg};
    _ -> error
  catch
    _:_ -> error
  end;
report_callback(_Report, _Meta) ->
  error.

% Turn a report in Palabres fields. Keys and values are typed in Gleam when
% they are primitives, so they end up as proper strings and numbers in JSON.
% Erlang-only terms are rendered here: atoms by their name, printable
% charlists as the string they spell, anything else inspected on a single
% line, in Erlang syntax. Maps have no reliable key order, so their fields
% are sorted, while lists keep the order of their author.
-spec report_fields(logger:report()) -> palabres@internals@field:fields().
report_fields(Report) when is_map(Report) ->
  Pairs = report_pairs(maps:to_list(Report)),
  palabres@internals@converter:from_report(lists:keysort(1, Pairs));
report_fields(Report) when is_list(Report) ->
  palabres@internals@converter:from_report(report_pairs(Report)).

-spec report_pairs([{term(), term()}]) -> [{binary(), term()}].
report_pairs(Report) ->
  [{report_key(Key), report_value(Value)} || {Key, Value} <- Report].

-spec report_key(term()) -> binary().
report_key(Key) when is_atom(Key) -> atom_to_binary(Key);
report_key(Key) when is_binary(Key) -> Key;
report_key(Key) ->
  case charlist_to_binary(Key) of
    {ok, Key1} -> Key1;
    error -> inspect(Key)
  end.

-spec report_value(term()) -> term().
report_value(Value) when is_boolean(Value) -> Value;
report_value(Value) when is_atom(Value) -> atom_to_binary(Value);
report_value(Value) when is_binary(Value); is_number(Value) -> Value;
report_value(Value) ->
  case charlist_to_binary(Value) of
    {ok, Value1} -> Value1;
    error -> inspect(Value)
  end.

% Charlists are the strings of Erlang code: `"hello"` is `[104, 101, …]`.
% A non-empty list of printable characters is read as the string it spells,
% as `~tp` does. The empty list stays `[]`, as it is more often a list.
-spec charlist_to_binary(term()) -> {ok, binary()} | error.
charlist_to_binary([]) ->
  error;
charlist_to_binary(List) when is_list(List) ->
  case io_lib:printable_unicode_list(List) of
    true -> {ok, unicode:characters_to_binary(List)};
    false -> error
  end;
charlist_to_binary(_) ->
  error.

% Inspect an Erlang term on a single line, in Erlang syntax.
-spec inspect(term()) -> binary().
inspect(Term) ->
  unicode:characters_to_binary(io_lib:format("~0tp", [Term])).

% Format the timestamp of the event, the `time` of its metadata in
% microseconds since the epoch, as ISO 8601 in UTC with milliseconds, like
% the JavaScript target: `2024-12-15T15:59:17.123Z`. Every event carries a
% `time`, as `logger` adds it when missing, so the fallback is theoretical.
-spec format_time(logger:metadata()) -> binary().
format_time(Meta) ->
  Time = maps:get(time, Meta, logger:timestamp()),
  Options = [{unit, millisecond}, {offset, "Z"}],
  Formatted = calendar:system_time_to_rfc3339(Time div 1000, Options),
  unicode:characters_to_binary(Formatted).

% Format `io:format/2` style messages, the most common form of Erlang logs,
% like `logger:info("user ~p logged in", [Id])`. A bad format string or
% mismatched arguments must never crash the logger: the format and its
% arguments are then printed raw, on a single line, like `logger_formatter`
% does.
-spec format_io(io:format(), [term()]) -> binary().
format_io(Format, Args) ->
  try unicode:characters_to_binary(io_lib:format(Format, Args)) of
    Msg when is_binary(Msg) -> Msg;
    _ -> format_error(Format, Args)
  catch
    error:_ -> format_error(Format, Args)
  end.

-spec format_error(term(), term()) -> binary().
format_error(Format, Args) ->
  <<"FORMAT ERROR: ", (inspect(Format))/binary, " - ", (inspect(Args))/binary>>.

% Format standard string reports. Because of the nature of Palabres, it does
% not override the standard logger, and as such, user can still send plain text
% to the logger, as well as any module can do. In that case, Palabres will still
% try to format the log as the user desires: a string message is a Palabres
% log without fields nor location. `Msg` can be any `unicode:chardata()`,
% like a charlist, which `json` would encode as an array of integers.
format_string_msg(Msg, Options) ->
  format_palabres_report([], chardata_to_binary(Msg), none, Options).

% Convert chardata to a binary, falling back to its inspection when it is not
% valid chardata, so that a malformed message never crashes the formatter.
-spec chardata_to_binary(unicode:chardata()) -> binary().
chardata_to_binary(Chardata) ->
  case unicode:characters_to_binary(Chardata) of
    Binary when is_binary(Binary) -> Binary;
    _ -> inspect(Chardata)
  end.

% Format standard Palabres reports. Palabres reports are identified by their
% `palabres` key. It is expected that no one will send a report with a
% `palabres` key holding a 3-tuple.
format_palabres_report(Fields, Msg, At, Options) ->
  #{color := Color, json := Json, auto_when := AutoWhen, default_fields := DefaultFields} = Options,
  #{timestamp := When} = Options,
  Msg1 = palabres@internals@converter:format_message(Msg, Color),
  Fields1 = lists:append(Fields, DefaultFields),
  Fields2 = prepend_when(Fields1, When, AutoWhen),
  Fields3 = palabres@internals@converter:prepend_at(Fields2, At, Color, Json),
  case Json of
    true -> palabres@internals@converter:to_json(Fields3, Msg1);
    false ->
      Qs = palabres@internals@converter:to_spaced_query_string(Fields3, Color),
      join_spaced([Qs, Msg1])
  end.

% Join the parts of a text log with single spaces, skipping empty parts: both
% the fields and the message can be empty.
-spec join_spaced([unicode:chardata()]) -> iolist().
join_spaced(Parts) ->
  [[$\s, Part] || Part <- Parts, Part =/= <<>>, Part =/= []].

% Prepend the `when` field, unless automatic timestamps are deactivated.
prepend_when(Fields, When, true) ->
  palabres@internals@converter:prepend_when(Fields, When);
prepend_when(Fields, _When, false) ->
  Fields.

