-module(palabres_ffi).

-export([configure/1, format/2, log/4]).

-include_lib("palabres/include/palabres@options_Options.hrl").

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
  style_default_logger(Options).

% Update the primary config of logger, setting the log level, and filtering
% some domains by default.
-spec update_primary_config(#options{}) -> nil.
update_primary_config(Options) ->
  logger:update_primary_config(#{
    level => Options#options.level,
    filter_default => log,
    filters =>
      [{domain, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}},
       {domain, {fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}],
    metadata => #{}}),
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
    #{formatter => {palabres_ffi, #{color => Color, json => Options#options.json}},
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
      FormatOptions = #{color => Color, json => Options#options.json},
      Formatter = {palabres_ffi, FormatOptions},
      HandlerOptions = #{formatter => Formatter, filters => Filters},
      logger:update_handler_config(default, HandlerOptions)
  end,
  nil.

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

% Default entrypoint for the logger module. `palabres_ffi` acts as a logger
% module, and defines a standard format entrypoint.
format(#{level := Level, msg := Msg, meta := _Meta}, Options) ->
  case maps:get(json, Options) of
    false ->
      Level1 = format_level(Level, Options),
      Msg1 = format_msg(Msg, Options),
      [Level1, Msg1, $\n];
    true ->
      JsonOptions = #{color => false, json => true},
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
format_string_msg(Msg, #{color := Color, json := Json}) ->
  When = palabres_utils_ffi:format_iso8601(),
  Id = palabres_utils_ffi:uuid(),
  case Json of
    true ->
      JsonData0 = #{message => Msg},
      JsonData1 = maps:put("when", When, JsonData0),
      JsonData2 = maps:put("id", Id, JsonData1),
      JsonData2;
    false ->
      When1 = {string_field, When},
      Id1 = {string_field, Id},
      Defaults = [],
      Defaults1 = palabres@interneals@field:append(Defaults, <<"when"/utf8>>, When1),
      Defaults2 = palabres@internals@field:append(Defaults1, <<"id"/utf8>>, Id1),
      Qs = palabres@internals@converter:to_spaced_query_string(Defaults2, Color),
      case Color of
        false -> [$\s, Qs, $\s, Msg];
        true -> [$\s, Qs, $\s, "\x1b[1m", Msg, "\x1b[0m"]
      end
  end.

% Format standard Palabres reports. Palabres reports are identified in tuples
% starting with atom `palabres`. It is expected that no one will send a tuple
% starting with `palabres` as first argument.
format_palabres_report(Fields, Msg, At, #{color := Color, json := Json}) ->
  Msg1 = palabres@internals@converter:format_message(Msg, Color),
  When = {string_field, palabres_utils_ffi:format_iso8601()},
  Id = {string_field, palabres_utils_ffi:uuid()},
  Fields1 = palabres@internals@field:append(Fields, <<"when"/utf8>>, When),
  Fields2 = palabres@internals@field:append(Fields1, <<"id"/utf8>>, Id),
  Fields3 = palabres@internals@converter:append_at(Fields2, At, Color, Json),
  case Json of
    true -> palabres@internals@converter:to_json(Fields3, Msg1);
    false ->
      Qs = palabres@internals@converter:to_spaced_query_string(Fields3, Color),
      [$\s, Qs, $\s, Msg1]
  end.

% Depending if JSON mode is activated or not:
% - Wrap a `string` (UTF-8, `binary()`) in a `map()` to use it as foundation.
% - Returns the string as-is if JSON mode is deactivated.
-spec json_wrap(binary(), true) -> map(); (binary(), false) -> binary().
json_wrap(Content, Json) ->
  case {Json, Content} of
    {false, _} -> Content;
    {true, [$\s, Content1]} -> #{message => Content1};
    {true, _} -> #{message => Content}
  end.

% Used only for non-Palabres log formatting. Because external logs can arrive,
% a standard formatter runs on plain logs to turn them in a familiar Palabres
% format.
-spec format_orddict(list({term(), term()})) -> binary().
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
