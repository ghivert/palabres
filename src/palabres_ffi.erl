-module(palabres_ffi).

-export([configure/1, format/2, format_iso8601/0, log/4, uuid/0]).

-include_lib("palabres/include/palabres@options_Options.hrl").

configure(#options{
  level = Level,
  json = Json,
  color = Clr,
  output = Output,
  style_default = StyleDefault
}) ->
  Color = read_color(Clr),
  logger:update_primary_config(#{
    level => Level,
    filter_default => log,
    filters =>
      [{domain, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}},
       {domain, {fun logger_filters:domain/2, {stop, sub, [supervisor_report]}}}],
    metadata => #{}}),
  logger:add_handler(palabres_logger, logger_std_h,
    #{formatter => {palabres_ffi, #{color => Color, json => Json}},
      filters => [{palabres_filter, {fun handler_filter/2, continue}}],
      config => configure_output(Output)}),
  case StyleDefault of
    false ->
      logger:update_handler_config(default,
      #{filters => [{palabres_filter, {fun handler_filter/2, stop}}]});
    true ->
      logger:update_handler_config(default,
        #{formatter => {palabres_ffi, #{color => Color, json => Json}},
          filters => [{palabres_filter, {fun handler_filter/2, stop}}]})
  end,
  nil.

configure_output(Output) ->
  case Output of
    stdout -> #{type => standard_io};
    {file, FileName, Interval} ->
      #{file => unicode:characters_to_list(FileName),
        filesync_repeat_interval =>
          case Interval of
            {some, Millis} -> Millis;
            none -> 5000
          end}
  end.

handler_filter(Event, Extra) ->
  case Event of
    #{msg := {report, [{palabres, _Fields, _Text, _At}]}} ->
      case Extra of
        stop -> stop;
        continue -> Event
      end;
    _ ->
      case Extra of
        stop -> Event;
        continue -> stop
      end
  end.

read_color(Color) ->
  case Color of
    {some, Clr} -> Clr;
    _ ->
      not (read_variable("NO_COLOUR", false)
        orelse read_variable("NO_COLOR", false))
  end.

read_variable(Name, Default) ->
  case os:getenv(Name) of
    false -> Default;
    "" -> false;
    "false" -> false;
    _ -> true
  end.

log(Level, Msg, Text, At) ->
  logger:log(Level, [{palabres, Msg, Text, At}]),
  nil.

format(#{level := Level, msg := Msg, meta := _Meta}, #{json := Json, color := Color}) ->
  case Json of
    false -> [format_level(Level, #{color => Color}), format_msg(Msg, #{color => Color, json => false}), $\n];
    true -> [thoas:encode(maps:put("level", Level, format_msg(Msg, #{color => false, json => true}))), $\n]
  end.

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

format_msg(Report0, #{color := Color, json := Json}) ->
  case Report0 of
    {string, Msg} ->
      case Json of
        true -> maps:put("id", uuid(), maps:put("when", format_iso8601(), json_wrap([$\s, Msg], Json)));
        false ->
          Defaults = [{<<"when"/utf8>>, [format_iso8601()]}, {<<"id"/utf8>>, [uuid()]}],
          Converted = palabres@internals@converter:to_spaced_query_string(Defaults, Color),
          case Color of
            false -> json_wrap([$\s, Converted, $\s, Msg], Json);
            true -> json_wrap([$\s, Converted, $\s, "\x1b[1m", Msg, "\x1b[0m"], Json)
          end
      end;
    {report, [{palabres, Fields, Text, At}]} ->
      Text1 = palabres@internals@converter:format_message(Text, Color),
      Fields1 = [{<<"when"/utf8>>, [format_iso8601()]}, {<<"id"/utf8>>, [uuid()]} | Fields],
      Fields2 = case At of
        {some, {Mod, Fun}} -> [{<<"at"/utf8>>, [palabres@internals@converter:format_at(Mod, Fun, Color, Json)]} | Fields1];
        _ -> Fields1
      end,
      case Json of
        false -> [$\s, palabres@internals@converter:to_spaced_query_string(Fields2, Color), $\s, Text1];
        true -> palabres@internals@converter:to_json(Fields2, Text1)
      end;
    {report, Report1} when is_map(Report1) -> json_wrap(format_kv(maps:to_list(Report1)), Json);
    {report, Report1} when is_list(Report1) -> json_wrap(format_kv(Report1), Json);
    _ -> json_wrap([$\s, gleam@string:inspect(Report0)], Json)
  end.

json_wrap(Content, Json) ->
  case {Json, Content} of
    {false, _} -> Content;
    {true, [$\s, Content1]} -> #{message => Content1};
    {true, _} -> #{message => Content}
  end.

format_kv(Pairs) ->
  case Pairs of
    [] -> [];
    [{K, V} | Rest] when is_atom(K) ->
      [$\s, erlang:atom_to_binary(K), $=, gleam@string:inspect(V) | format_kv(Rest)];
    [{K, V} | Rest] ->
      [$\s, gleam@string:inspect(K), $=, gleam@string:inspect(V) | format_kv(Rest)];
    Other ->
      gleam@string:inspect(Other)
  end.

format_iso8601() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(
      io_lib:format(
        "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
        [Year, Month, Day, Hour, Min, Sec])).

uuid() ->
  Uuid0 = uuid:get_v4(),
  Uuid1 = uuid:uuid_to_string(Uuid0),
  unicode:characters_to_binary(Uuid1).
