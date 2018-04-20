
-module(wasm).
-export([compile/1, compile/2]).
-include("wasm.hrl").

compile(Input) ->
  {beam_file, Module, _Exports, _Att, _Comp, Code} = beam_disasm:file(Input),
  {ok, Pid} = wasm_state:start_link(Code),
  % ok = assemble_builtins(Pid, Code),
  io:format("before"),
  ok = assemble_types(Pid, Code),
  io:format("finished types"),
  % ok = assemble_funcs(Pid, Code),
  % ok = assemble_imports(Pid, Code),
  % ok = assemble_exports(Pid, Code, Exports),
  ok = assemble_code(Pid, Module, Code),
  io:format("finished code"),
  wasm_binary:encode(wasm_state:get_results(Pid)).
  
compile(Input, Output) ->
  {ok, IoDevice} = file:open(Output, [write]),
  file:write(IoDevice, compile(Input)).

%% No built-in code yet

assemble_types(Pid, Code) ->
  lists:foreach(fun
    ({function, Name, Arity, _Entry, _Code}) ->
      io:format("yesseir"),
      ok = wasm_state:add_type(Pid, Name, Arity);
    (_) -> nil
  end, Code).

% assemble_import({function, Name, Arity, _Entry, Code}) ->
%   lists:flatmap(fun assemble_import/1, Code);

% assemble_import({call, X, M}) ->
%   [ {import, X, M, {importdesc, }} ];

% assemble_import(_N) -> [].

% assemble_funcs(Code, Wasm) -> Wasm.

% assemble_exports(Code, Exports, Wasm) -> Wasm.

% assemble_builtins(Code, Wasm) -> Wasm.

assemble_code(Pid, Module, Code) ->
  lists:foreach(fun (Instr) -> assemble_function(Pid, Module, Instr) end, Code).

assemble_function(Pid, Module, {function, Name, Arity, _Entry, Code}) ->
  MFA = {Module, Name, Arity},
  Instrs = lists:flatmap(fun assemble_instr/1, Code),
  wasm_state:add_function(MFA, Instrs, Pid).

assemble_instr({move, {x, _X1}, {x, _X2}}) ->
  [];

assemble_instr({move, {x, _X}, {y, _Y}}) ->
  [];

assemble_instr({move, {y, _Y}, {x, _X}}) ->
  [];

assemble_instr({call, _Arity, {_Module, _Fn, _Arity}}) ->
  [];

assemble_instr({gc_bif, _BifName, _F, _Live, _Args,_Reg}) ->
  [];

assemble_instr({allocate, _N, _Y}) ->
  [];

assemble_instr({deallocate, _N}) ->
  [];

assemble_instr(return) ->
  [];

assemble_instr({func_info, _M, _F, _A}) ->
  [];

assemble_instr({line, _N}) ->
  [];

assemble_instr({label, _N}) ->
  [];

assemble_instr(_U) -> [].
