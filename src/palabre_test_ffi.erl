-module(palabre_test_ffi).

-export([destroy/0]).

% Used only for testing purposes.
destroy() ->
  logger:remove_handler(palabre_logger),
  logger:update_handler_config(default,
    #{formatter => {logger_formatter, #{legacy_header => true,single_line => false}},
      filters =>
        [{remote_gl, {fun logger_filters:remote_gl/2, stop}},
         {domain, {fun logger_filters:domain/2, {log, super, [otp, sasl]}}},
         {no_domain, {fun logger_filters:domain/2, {log, undefined, []}}}]}),
  nil.
