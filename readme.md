
# erl_wasm

**Does not work yet. Looking for a collaborative effort.**

A compiler for WebAssembly from Erlang code.  It works by compiling BEAM binaries into WebAssembly binaries, which can be executed portably or on the web.

## High-level goal

See if the same concepts used in Erlang applications are useful in graphical, web, or portable applications with WebAssembly.  See what stuff like animations, service workers, fault-toleratent applications, blockchain, etc. would look like using Erlang's concurrency.

Any language which [targets BEAM](https://github.com/llaisdy/beam_languages) should be supported.
