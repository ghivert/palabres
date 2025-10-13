-module(palabres_utils_ffi).

-export([format_iso8601/0, null/0]).

%% Format the current date in string, following the ISO-8601 (or RFC-3339) format
%% to fully identify a log. Time is grabbed from the machine, in UTC format.
%% By definition, `format_iso8601() =/= format_iso8601()` as `format_iso8601`
%% always returns the _current_ time.
-spec format_iso8601() -> binary().
format_iso8601() ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
  Format = "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
  Values = [Year, Month, Day, Hour, Min, Sec],
  Formatted = io_lib:format(Format, Values),
  iolist_to_binary(Formatted).

null() ->
  null.
