-module(wasm_test).

-include_lib("eunit/include/eunit.hrl").

compiler_test() ->
  wasm:compile("./fixtures/test.beam", "./fixtures/test.wasm")
