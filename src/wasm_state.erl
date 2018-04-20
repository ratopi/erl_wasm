-module(wasm_state).
-behavior(gen_server).

-export([
  %% behavior
  start_link/1,
  init/1,
  handle_call/3,
  %% methods
  get_results/1,
  set_module/2,
  add_type/3,
  add_function/3,
  add_import/3,
  add_export/2
]).

-include("wasm.hrl").

start_link(Code) ->
  gen_server:start_link(?MODULE, [Code], []).

init(Code) ->
  {ok, #{
    code => Code,
    module => #wasm_module{},
    types => [],
    mfa_labels => [],
    current_label => 0
  }}.

get_results(Pid) ->
  Res = gen_server:call(Pid, get_module),
  gen_server:stop(Pid),
  Res.

set_module(Pid, Module) ->
  gen_server:call(Pid, {set_module, Module}).

add_type(Pid, Name, Arity) ->
  gen_server:call(Pid, {add_type, Name, Arity}, infinity).

get_type(Pid, Arity) ->
  gen_server:call(Pid, {get_type, Arity}, infinity).

get_typeidx(State, Arity) ->
  Module = maps:get(module, State),
  TypeSec = Module#wasm_module.typesec,
  io:format("\nTypeSec: ~p\n", [TypeSec]),
  Possible = lists:filter(fun (FuncType) ->
    length(element(2, element(2, FuncType))) == Arity
  end, TypeSec),
  case Possible of
    [TypeIdx] -> {ok, TypeIdx};
    [] -> nil
  end.

add_function(Pid, MFA, Func) ->
  gen_server:call(Pid, {add_function, MFA, Func}, infinity).

add_import(Pid, Type, MFA) ->
  gen_server:call(Pid, {get_module, Type, MFA}).

add_export(Pid, MFA) ->
  gen_server:call(Pid, {add_export, MFA}).

handle_call(get_module, _From, State) ->
  {reply, maps:get(module, State), State};

handle_call({set_module, Module}, _From, State) ->
  {reply, ok, State#{ module => Module }};

handle_call({add_type, _Name, Arity}, _From, State) ->
  io:format("\nArity: ~p\n", [Arity]),
  case get_typeidx(State, Arity) of
    {ok, _T} ->
      {reply, ok, State};
    nil ->
      FuncType = {functype,
        {vec, lists:map(fun (_) -> i32 end, lists:seq(1, Arity))},
        {vec, [i32]}
      },
      Module = maps:get(module, State),
      TypeSec = Module#wasm_module.typesec ++ [ FuncType ],
      {reply, ok, State#{
        module => Module#wasm_module{ typesec = TypeSec },
        types => maps:get(types, State) ++ [ Arity ]
      }}
    end;

handle_call({add_function, {_M, _F, Arity} = MFA, Func}, _From, State) ->
  Module = maps:get(module, State),
  LabelIdx = maps:get(current_label, State) + 1,
  {ok, TypeIdx} = get_typeidx(State, Arity),
  FuncSec = Module#wasm_module.funcsec ++ [ TypeIdx ],
  CodeSec = Module#wasm_module.codesec ++ [ Func ],
  {reply, ok, State#{
    module => Module#wasm_module{ funcsec = FuncSec, codesec = CodeSec },
    mfa_labels => maps:get(mfa_labels, State) ++ [ {MFA, LabelIdx} ]
  }};

handle_call({add_import, _MFA, _TypeIdx}, _From, State) -> State;

handle_call({add_export, _MFA}, _From, State) -> State.
