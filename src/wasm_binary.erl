-module(wasm_binary).

-include("wasm.hrl").

-export([encode/1, encode_section/2, encode_sequence/1]).

%% Module
encode({wasm_module,_,_,_,_,_,_,_,_,_,_,_} = Module) ->
  [ _Name | Sections ] = erlang:tuple_to_list(Module),
  EncodedSections = encode_sequence(Sections),
  <<"\0asm", 1, 0, 0, 0, EncodedSections/binary>>;

%% Sections
encode({customsec, Contents}) -> encode_section(0, Contents);
encode({typesec, Contents}) -> encode_section(1, {vec, Contents});
encode({importsec, Contents}) -> encode_section(2, {vec, Contents});
encode({funcsec, Contents}) -> encode_section(3, {vec, Contents});
encode({tablesec, Contents}) -> encode_section(4, {vec, Contents});
encode({memsec, Contents}) -> encode_section(5, {vec, Contents});
encode({globalsec, Contents}) -> encode_section(6, {vec, Contents});
encode({exportsec, Contents}) -> encode_section(7, {vec, Contents});
encode({startsec, Contents}) -> encode_section(8, Contents);
encode({elemsec, Contents}) -> encode_section(9, {vec, Contents});
encode({codesec, Contents}) -> encode_section(10, {vec, Contents});
encode({datasec, Contents}) -> encode_section(11, {vec, Contents});

%% Value types
encode(i32) -> <<16#7F>>;
encode(i64) -> <<16#FE>>;
encode(f32) -> <<16#7D>>;
encode(f64) -> <<16#7C>>;

%% Result types
encode({block_type, []}) -> <<16#40>>;
encode({block_type, Types}) -> encode_sequence(Types);

%% Function types
encode({func_type, ParamTypes, ResultTypes}) ->
  encode_instr2(16#60, {vec, ParamTypes}, {vec, ResultTypes});

%% Memory types
encode({mem_type, Limit}) -> encode(Limit);
encode({limits, Min}) -> encode_instr1(16#00, {u32, Min});
encode({limits, Min, Max}) -> encode_instr2(16#01, {u32, Min}, {u32, Max});

%% Table types
encode({table_type, ElemType, Limit}) ->
  EncodedElemType = encode(ElemType),
  EncodedLimit = encode(Limit),
  <<EncodedElemType/binary, EncodedLimit/binary>>;
encode(elem_type) -> <<16#70>>;

%% Global types
encode({global_type, const, ValType}) ->
  EncodedValType = encode(ValType),
  <<16#00, EncodedValType/binary>>;
encode({global_type, var, ValType}) ->
  EncodedValType = encode(ValType),
  <<16#01, EncodedValType/binary>>;

%% Parametric instructions
encode(drop) -> <<16#A1>>;
encode(select) -> <<16#1B>>;

%% Control instructions
encode(return) -> <<16#0F>>;
encode(unreachable) -> <<16#00>>;
encode(nop) -> <<16#01>>;
encode({loop, ResultType, Body}) ->
  encode_instr2(16#03, ResultType, Body);
encode({if_no_else, ResultType, Body}) ->
  EncodedFront = encode_instr2(16#04, ResultType, Body),
  <<EncodedFront/binary, 16#0B>>;
encode({if_else, ResultType, Consequent, Alternate}) ->
  EncodedFront = encode_instr2(16#04, ResultType, Consequent),
  EncodedAlternate = encode_sequence(Alternate),
  <<EncodedFront/binary, 16#05, EncodedAlternate/binary, 16#0B>>;
encode({br_table, LabelIndices, LabelIdx}) ->
  encode_instr2(16#0D, LabelIndices, LabelIdx);
encode({br, LabelIdx}) ->
  encode_instr1(16#05, LabelIdx);
encode({br_if, LabelIdx}) ->
  encode_instr1(16#0C, LabelIdx);
encode({call, FuncIdx}) ->
  encode_instr1(16#10, FuncIdx);
encode({call_indirect, TypeIdx}) ->
  EncodedFront = encode_instr1(16#11, TypeIdx),
  <<EncodedFront/binary, 16#00>>;

%% Memory instructions
encode({i32_load, Align, Offset}) -> encode_instr2(16#28, Align, Offset);
encode({i64_load, Align, Offset}) -> encode_instr2(16#29, Align, Offset);
encode({f32_load, Align, Offset}) -> encode_instr2(16#2A, Align, Offset);
encode({f64_load, Align, Offset}) -> encode_instr2(16#2B, Align, Offset);
encode({i32_load8_s, Align, Offset}) -> encode_instr2(16#2C, Align, Offset);
encode({i32_load8_u, Align, Offset}) -> encode_instr2(16#2D, Align, Offset);
encode({i32_load16_s, Align, Offset}) -> encode_instr2(16#2E, Align, Offset);
encode({i32_load16_u, Align, Offset}) -> encode_instr2(16#2F, Align, Offset);
encode({i64_load8_s, Align, Offset}) -> encode_instr2(16#30, Align, Offset);
encode({i64_load8_u, Align, Offset}) -> encode_instr2(16#31, Align, Offset);
encode({i64_load16_s, Align, Offset}) -> encode_instr2(16#32, Align, Offset);
encode({i64_load16_u, Align, Offset}) -> encode_instr2(16#33, Align, Offset);
encode({i64_load32_s, Align, Offset}) -> encode_instr2(16#34, Align, Offset);
encode({i64_load32_u, Align, Offset}) -> encode_instr2(16#35, Align, Offset);
encode({i32_store, Align, Offset}) -> encode_instr2(16#36, Align, Offset);
encode({i64_store, Align, Offset}) -> encode_instr2(16#37, Align, Offset);
encode({f32_store, Align, Offset}) -> encode_instr2(16#38, Align, Offset);
encode({f64_store, Align, Offset}) -> encode_instr2(16#39, Align, Offset);
encode({i32_store8, Align, Offset}) -> encode_instr2(16#3A, Align, Offset);
encode({i32_store16, Align, Offset}) -> encode_instr2(16#3B, Align, Offset);
encode({i64_store8, Align, Offset}) -> encode_instr2(16#3C, Align, Offset);
encode({i64_store16, Align, Offset}) -> encode_instr2(16#3D, Align, Offset);
encode({i64_store32, Align, Offset}) -> encode_instr2(16#3E, Align, Offset);
encode({current_memory, Align, Offset}) -> encode_instr2(16#3F, Align, Offset);
encode({grow_memory, Align, Offset}) -> encode_instr2(16#40, Align, Offset);

%% Variable instructions
encode({get_local, X}) -> encode_instr1(16#20, X);
encode({set_local, X}) -> encode_instr1(16#21, X);
encode({tee_local, X}) -> encode_instr1(16#22, X);
encode({get_global, X}) -> encode_instr1(16#23, X);
encode({set_global, X}) -> encode_instr1(16#24, X);

%% Numeric instructions
encode(i32_eqz) -> encode_instr0(16#45);
encode(i32_eq) -> encode_instr0(16#46);
encode(i32_ne) -> encode_instr0(16#47);
encode(i32_lt_s) -> encode_instr0(16#48);
encode(i32_lt_u) -> encode_instr0(16#49);
encode(i32_gt_s) -> encode_instr0(16#4A);
encode(i32_gt_u) -> encode_instr0(16#4B);
encode(i32_le_s) -> encode_instr0(16#4C);
encode(i32_le_u) -> encode_instr0(16#4D);
encode(i32_ge_s) -> encode_instr0(16#4E);
encode(i32_ge_u) -> encode_instr0(16#4F);
encode(i64_eqz) -> encode_instr0(16#50);
encode(i64_eq) -> encode_instr0(16#51);
encode(i64_ne) -> encode_instr0(16#52);
encode(i64_lt_s) -> encode_instr0(16#53);
encode(i64_lt_u) -> encode_instr0(16#54);
encode(i64_gt_s) -> encode_instr0(16#55);
encode(i64_gt_u) -> encode_instr0(16#56);
encode(i64_le_s) -> encode_instr0(16#57);
encode(i64_le_u) -> encode_instr0(16#58);
encode(i64_ge_s) -> encode_instr0(16#59);
encode(i64_ge_u) -> encode_instr0(16#5A);
encode(f32_eq) -> encode_instr0(16#5B);
encode(f32_ne) -> encode_instr0(16#5C);
encode(f32_lt) -> encode_instr0(16#5D);
encode(f32_gt) -> encode_instr0(16#5E);
encode(f32_le) -> encode_instr0(16#5F);
encode(f32_ge) -> encode_instr0(16#60);
encode(f64_eq) -> encode_instr0(16#61);
encode(f64_ne) -> encode_instr0(16#62);
encode(f64_lt) -> encode_instr0(16#63);
encode(f64_gt) -> encode_instr0(16#64);
encode(f64_le) -> encode_instr0(16#65);
encode(f64_ge) -> encode_instr0(16#66);
encode(i32_clz) -> encode_instr0(16#67);
encode(i32_ctz) -> encode_instr0(16#68);
encode(i32_popcnt) -> encode_instr0(16#69);
encode(i32_add) -> encode_instr0(16#6A);
encode(i32_sub) -> encode_instr0(16#6B);
encode(i32_mul) -> encode_instr0(16#6C);
encode(i32_div_s) -> encode_instr0(16#6D);
encode(i32_div_u) -> encode_instr0(16#6E);
encode(i32_rem_s) -> encode_instr0(16#6F);
encode(i32_rem_u) -> encode_instr0(16#70);
encode(i32_and) -> encode_instr0(16#71);
encode(i32_or) -> encode_instr0(16#72);
encode(i32_xor) -> encode_instr0(16#73);
encode(i32_shl) -> encode_instr0(16#74);
encode(i32_shr_s) -> encode_instr0(16#75);
encode(i32_shr_u) -> encode_instr0(16#76);
encode(i32_rotl) -> encode_instr0(16#77);
encode(i32_rotr) -> encode_instr0(16#78);
encode(i64_clz) -> encode_instr0(16#79);
encode(i64_ctz) -> encode_instr0(16#7A);
encode(i64_popcnt) -> encode_instr0(16#7B);
encode(i64_add) -> encode_instr0(16#7C);
encode(i64_sub) -> encode_instr0(16#7D);
encode(i64_mul) -> encode_instr0(16#7E);
encode(i64_div_s) -> encode_instr0(16#7F);
encode(i64_div_u) -> encode_instr0(16#80);
encode(i64_rem_s) -> encode_instr0(16#81);
encode(i64_rem_u) -> encode_instr0(16#82);
encode(i64_and) -> encode_instr0(16#83);
encode(i64_or) -> encode_instr0(16#84);
encode(i64_xor) -> encode_instr0(16#85);
encode(i64_shl) -> encode_instr0(16#86);
encode(i64_shr_s) -> encode_instr0(16#87);
encode(i64_shr_u) -> encode_instr0(16#88);
encode(i64_rotl) -> encode_instr0(16#89);
encode(i64_rotr) -> encode_instr0(16#8A);
encode(f32_abs) -> encode_instr0(16#8B);
encode(f32_neg) -> encode_instr0(16#8C);
encode(f32_ceil) -> encode_instr0(16#8D);
encode(f32_floor) -> encode_instr0(16#8E);
encode(f32_trunc) -> encode_instr0(16#8F);
encode(f32_nearest) -> encode_instr0(16#90);
encode(f32_sqrt) -> encode_instr0(16#91);
encode(f32_add) -> encode_instr0(16#92);
encode(f32_sub) -> encode_instr0(16#93);
encode(f32_mul) -> encode_instr0(16#94);
encode(f32_div) -> encode_instr0(16#95);
encode(f32_min) -> encode_instr0(16#96);
encode(f32_max) -> encode_instr0(16#97);
encode(f32_copysign) -> encode_instr0(16#98);
encode(f64_abs) -> encode_instr0(16#99);
encode(f64_neg) -> encode_instr0(16#9A);
encode(f64_ceil) -> encode_instr0(16#9B);
encode(f64_floor) -> encode_instr0(16#9C);
encode(f64_trunc) -> encode_instr0(16#9D);
encode(f64_nearest) -> encode_instr0(16#9E);
encode(f64_sqrt) -> encode_instr0(16#9F);
encode(f64_add) -> encode_instr0(16#A0);
encode(f64_sub) -> encode_instr0(16#A1);
encode(f64_mul) -> encode_instr0(16#A2);
encode(f64_div) -> encode_instr0(16#A3);
encode(f64_min) -> encode_instr0(16#A4);
encode(f64_max) -> encode_instr0(16#A5);
encode(f64_copysign) -> encode_instr0(16#A6);
encode(i32_wrap_i64) -> encode_instr0(16#A7);
encode(i32_trunc_s_f32) -> encode_instr0(16#A8);
encode(i32_trunc_u_f32) -> encode_instr0(16#A9);
encode({i32_const, N}) -> encode_instr1(16#41, {i32, N});
encode({i64_const, N}) -> encode_instr1(16#42, {i64, N});
encode({f32_const, N}) -> encode_instr1(16#43, {f32, N});
encode({f64_const, N}) -> encode_instr1(16#44, {f64, N});

%% Expressions
encode({expr, Instrs}) ->
  EncodedInstrs = encode_sequence(Instrs),
  <<EncodedInstrs/binary>>;

%% Vector value
encode({vec, Items}) ->
  EncodedSize = encode({u32, length(Items)}),
  EncodedItems = encode_sequence(Items),
  <<EncodedSize/binary, EncodedItems/binary>>;

%% Name value
encode({name, Name}) -> <<Name/utf8>>;

%% Float value
encode({f32, Value}) -> <<Value:32/float>>;
encode({f64, Value}) -> <<Value:64/float>>;

%% Number values
encode({u32, Value}) -> eleb128:unsigned_encode(Value);
encode({u64, Value}) -> eleb128:unsigned_encode(Value);
encode({s32, Value}) -> eleb128:signed_encode(Value);
encode({s64, Value}) -> eleb128:signed_encode(Value);
encode({i32, Value}) -> eleb128:signed_encode(Value);
encode({i64, Value}) -> eleb128:signed_encode(Value);

encode({typeidx, Idx}) -> encode({u32, Idx});
encode({funcidx, Idx}) -> encode({u32, Idx});
encode({tableidx, Idx}) -> encode({u32, Idx});
encode({memidx, Idx}) -> encode({u32, Idx});
encode({globalidx, Idx}) -> encode({u32, Idx});
encode({localidx, Idx}) -> encode({u32, Idx});
encode({labelidx, Idx}) -> encode({u32, Idx});

encode({export, Name, Desc}) ->
  EncodedName = encode(Name),
  EncodedDesc = case Desc of
    {funcidx, _} -> encode_instr1(0, Desc);
    {tableidx, _} -> encode_instr1(1, Desc);
    {memidx, _} -> encode_instr1(2, Desc);
    {globalidx, _} -> encode_instr1(3, Desc)
  end,
  <<EncodedName/binary, EncodedDesc/binary>>;

encode({code, Func}) ->
  Body = encode(Func),
  Size = encode({u32, byte_size(Body)}),
  <<Size/binary, Body/binary>>;

encode({func, Locals, Expr}) ->
  LocalsEncoded = erlang:iolist_to_binary(lists:map(fun encode_sequence/1, Locals)),
  ExprEncoded = encode({expr, Expr}),
  <<LocalsEncoded/binary, ExprEncoded/binary>>;

encode(nil) -> <<>>;
encode([]) -> <<>>.

%% Encodes any section
encode_section(Id, Contents) ->
  EncodedContents = encode(Contents),
  EncodedSize = encode({u32, byte_size(EncodedContents)}),
  <<Id, EncodedSize/binary, EncodedContents/binary>>.

%% Encodes any sequence of terms
encode_sequence(Terms) ->
  lists:foldl(fun(Term, Rest) ->
    EncodedTerm = encode(Term),
    <<Rest/binary, EncodedTerm/binary>>
  end, <<>>, Terms).

%% Generic instruction encodings
encode_instr0(Id) -> <<Id>>.
encode_instr1(Id, Term1) ->
  EncodedTerm1 = encode(Term1),
  <<Id, EncodedTerm1/binary>>.
encode_instr2(Id, Term1, Term2) ->
  EncodedTerm1 = encode(Term1),
  EncodedTerm2 = encode(Term2),
  <<Id, EncodedTerm1/binary, EncodedTerm2/binary>>.
