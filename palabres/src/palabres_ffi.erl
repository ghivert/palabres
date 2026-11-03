-module(palabres_ffi).

-export([configure/1, format/2, log/4, locate/1, allowed/1]).

-include_lib("palabres/include/palabres@options_Options.hrl").

-define(AUTO_AT_KEY, {palabres, auto_at}).

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
% some domains by default.
-spec update_primary_config(#options{}) -> nil.
update_primary_config(Options) ->
  logger:update_primary_config(#{
    level => Options#options.level,
    filter_default => log,
    filters =>
      [{domain, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}},
       {domain, {fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}]}),
  nil.

% Add the `palabres_logger` to the logger, living besides the `default` logger.
% `palabres_logger` accepts only logs emitted by Palabres, and will simply
% ignore every logs coming from the outside world. Every logs flowing in the
% Palabres logger should be sent using `log/4`.
% The Palabres logger can output on standard IO, but can also log in a file,
% depending on Palabres configuration.
-spec add_palabres_logger(#options{}) -> nil.
add_palabres_logger(Options) ->
  Color = read_color(Options#options.color),
  logger:add_handler(palabres_logger, logger_std_h,
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
        end}),
  nil.

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
    fields => Options#options.fields}.

% Filter events for the loggers. Behaves differently for Palabres and for the
% default logger if default style has been overriden.
% - For Palabres logger (`Extra =:= continue`), let Palabres logs flow to the
%   logger and prevent other logs to flow.
% - For default logger, (`Extra =:= stop`), prevent Palabres logs to flow to the
%   logger, and allow other logs to flow.
handler_filter(Event, Extra) ->
  case {Event, Extra} of
    {#{msg := {report, [{palabres, _, _, _}]}}, stop} -> stop;
    {#{msg := {report, [{palabres, _, _, _}]}}, continue} -> Event;
    {_, stop} -> Event;
    {_, continue} -> stop
  end.

% Read if color is activated or not. In case color activation has been set by
% hand by the user, the choice is immediately returned. Otherwise, the
% environment variables `NO_COLOUR` and `NO_COLOR` will be read, and will take
% effect if not defined. By default, color is activated.
-spec read_color({some, boolean()} | none) -> boolean().
read_color(Color) ->
  case Color of
    {some, Clr} -> Clr;
    none ->
      NO_COLOUR = read_variable("NO_COLOUR"),
      NO_COLOR = NO_COLOUR orelse read_variable("NO_COLOR"),
      not NO_COLOR
  end.

% Read a boolean variable from the environment. If the variable is undefined or
% empty, returns `false`. Otherwise, returns its value.
-spec read_variable(Name :: os:env_var_name()) -> boolean().
read_variable(Name) ->
  case os:getenv(Name) of
    false -> false;
    "" -> false;
    "false" -> false;
    _ -> true
  end.

% Helper to run `log` from Gleam `palabres` module. Format the log accordingly
% to Palabres requirements in the logger.
log(Level, Msg, Text, At) ->
  logger:log(Level, [{palabres, Msg, Text, At}]),
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
format(#{level := Level, msg := Msg, meta := _Meta}, Options) ->
  case maps:get(json, Options) of
    false ->
      Level1 = format_level(Level, Options),
      Msg1 = format_msg(Msg, Options),
      [Level1, Msg1, $\n];
    true ->
      JsonOptions = maps:put(color, false, Options),
      JsonData0 = format_msg(Msg, JsonOptions),
      JsonData1 = maps:put(<<"level">>, Level, JsonData0),
      JsonIo = json:encode(JsonData1),
      JsonString = iolist_to_binary(JsonIo),
      [JsonString, $\n]
  end.

% Format the level indicator for query string, with an appropriate color. In
% case color is deactivated (because it has been deactivated by user, or because
% `NO_COLOUR` or `NO_COLOR` variable has been set to `true`), the level is
% returned with any color.
format_level(Level, #{color := Color}) ->
  case Level of
    emergency when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31memrg\x1b[0m";
    alert     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31malrt\x1b[0m";
    critical  when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31mcrit\x1b[0m";
    error     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;31meror\x1b[0m";
    warning   when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;33mwarn\x1b[0m";
    notice    when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;32mntce\x1b[0m";
    info      when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;34minfo\x1b[0m";
    debug     when Color -> "\x1b[32mlevel\x1b[31m=\x1b[1;36mdebg\x1b[0m";
    emergency -> "level=emrg";
    alert     -> "level=alrt";
    critical  -> "level=crit";
    error     -> "level=eror";
    warning   -> "level=warn";
    notice    -> "level=ntce";
    info      -> "level=info";
    debug     -> "level=debg"
  end.

% Default log formatter. Format logs in JSON or query string, depending on the
% mode setup at start. Any Palabres log will go through a specific formatting,
% while every other log will go through a standard formatter. In both case,
% Palabres tries to format the log in a nicely way to be easily parsed by a
% machine, or read by a human.
format_msg(Report0, Options) ->
  #{json := Json} = Options,
  case Report0 of
    {string, Msg} ->
      format_string_msg(Msg, Options);
    {report, [{palabres, Fields, Msg, At}]} ->
      format_palabres_report(Fields, Msg, At, Options);
    {report, Report1} when is_map(Report1) ->
      Report2 = maps:to_list(Report1),
      Report3 = format_orddict(Report2),
      json_wrap(Report3, Json);
    {report, Report1} when is_list(Report1) ->
      Report2 = format_orddict(Report1),
      json_wrap(Report2, Json);
    _ ->
      Report1 = gleam@string:inspect(Report0),
      json_wrap([$\s, Report1], Json)
  end.

% Format standard string reports. Because of the nature of Palabres, it does
% not override the standard logger, and as such, user can still send plain text
% to the logger, as well as any module can do. In that case, Palabres will still
% try to format the log as the user desires.
format_string_msg(Msg, Options) ->
  #{color := Color, json := Json, auto_when := AutoWhen, fields := Fields} = Options,
  When = palabres_utils_ffi:format_iso8601(),
  % Global fields are resolved on every log, as they can be lazy.
  Globals = palabres@field:resolve(Fields),
  case Json of
    true ->
      Globals1 = palabres@internals@converter:to_json(Globals, <<>>),
      JsonData0 = #{'__message' => Msg},
      JsonData1 =
        case AutoWhen of
          true -> maps:put(<<"when">>, When, JsonData0);
          false -> JsonData0
        end,
      JsonData2 = maps:merge(JsonData1, Globals1),
      JsonData2;
    false ->
      Fields1 = prepend_when([], When, AutoWhen),
      Fields2 = lists:append(Fields1, Globals),
      Qs = palabres@internals@converter:to_spaced_query_string(Fields2, Color),
      % `Msg` can be any `unicode:chardata()`, not only a binary.
      Msg1 =
        case Color of
          false -> Msg;
          true -> ["\x1b[1m", Msg, "\x1b[0m"]
        end,
      join_spaced([Qs, Msg1])
  end.

% Format standard Palabres reports. Palabres reports are identified in tuples
% starting with atom `palabres`. It is expected that no one will send a tuple
% starting with `palabres` as first argument.
format_palabres_report(Fields, Msg, At, Options) ->
  #{color := Color, json := Json, auto_when := AutoWhen, fields := Globals} = Options,
  Msg1 = palabres@internals@converter:format_message(Msg, Color),
  When = palabres_utils_ffi:format_iso8601(),
  % Global fields are resolved on every log, as they can be lazy. A field set
  % on the log wins over a global field with the same key.
  Globals1 = palabres@field:resolve(Globals),
  Fields1 = palabres@internals@field:merge(Fields, Globals1),
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

% Depending if JSON mode is activated or not:
% - Wrap a `string` (UTF-8, `binary()`) in a `map()` to use it as foundation.
% - Returns the string as-is if JSON mode is deactivated.
-spec json_wrap(binary(), true) -> map();
               (binary(), false) -> binary().
json_wrap(Content, Json) ->
  case {Json, Content} of
    {false, _} -> Content;
    {true, [$\s, Content1]} -> #{message => Content1};
    {true, _} -> #{message => Content}
  end.

% Used only for non-Palabres log formatting. Because external logs can arrive,
% a standard formatter runs on plain logs to turn them in a familiar Palabres
% format.
-spec format_orddict([{term(), term()}]) -> binary().
format_orddict(Pairs) ->
  case Pairs of
    [] ->
      [];
    [{Key, Value} | Rest] when is_atom(Key) ->
      Key1 = erlang:atom_to_binary(Key),
      Value1 = gleam@string:inspect(Value),
      [$\s, Key1, $=, Value1 | format_orddict(Rest)];
    [{Key, Value} | Rest] ->
      Key1 = gleam@string:inspect(Key),
      Value1 = gleam@string:inspect(Value),
      [$\s, Key1, $=, Value1 | format_orddict(Rest)];
    Rest ->
      gleam@string:inspect(Rest)
  end.
