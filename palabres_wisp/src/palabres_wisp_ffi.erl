-module(palabres_wisp_ffi).

-export([monotonic_time/0]).

% Monotonic clock, in microseconds. Unlike the system time, it never jumps
% backwards, so it is safe to measure a duration with it.
-spec monotonic_time() -> integer().
monotonic_time() ->
  erlang:monotonic_time(microsecond).
