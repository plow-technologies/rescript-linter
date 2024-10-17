/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */
@@warning("a")
/* type 'a option = None | Some of 'a */

/* Exceptions */

external register_named_value: (string, 'a) => unit = "?register_named_value"

let () = /* for asmrun/fail.c */
register_named_value("Pervasives.array_bound_error", Invalid_argument("index out of bounds"))

external raise: exn => 'a = "%raise"
external raise_notrace: exn => 'a = "%raise_notrace"

let failwith = s => raise(Failure(s))
let invalid_arg = s => raise(Invalid_argument(s))

exception Exit

/* Composition operators */

external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

/* Debugging */

external __LOC__: string = "%loc_LOC"
external __MODULE__: string = "%loc_FILE"
external __LINE__: int = "%loc_LINE"
external __MODULE__: string = "%loc_MODULE"
external __POS__: (string, int, int, int) = "%loc_POS"

external __LOC_OF__: 'a => (string, 'a) = "%loc_LOC"
external __LINE_OF__: 'a => (int, 'a) = "%loc_LINE"
external __POS_OF__: 'a => ((string, int, int, int), 'a) = "%loc_POS"

/* Comparisons */

external \"=": ('a, 'a) => bool = "%equal"
external \"<>": ('a, 'a) => bool = "%notequal"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external compare: ('a, 'a) => int = "%compare"

let min = (x, y) =>
  if x <= y {
    x
  } else {
    y
  }
let max = (x, y) =>
  if x >= y {
    x
  } else {
    y
  }

external \"==": ('a, 'a) => bool = "%eq"
external \"!=": ('a, 'a) => bool = "%noteq"

/* Boolean operations */

external not: bool => bool = "%boolnot"
external \"&": (bool, bool) => bool = "%sequand"
external \"&&": (bool, bool) => bool = "%sequand"
external or: (bool, bool) => bool = "%sequor"
external \"||": (bool, bool) => bool = "%sequor"

/* Integer operations */

external \"~-": int => int = "%negint"
external \"~+": int => int = "%identity"
external succ: int => int = "%succint"
external pred: int => int = "%predint"
external \"+": (int, int) => int = "%addint"
external \"-": (int, int) => int = "%subint"
external \"*": (int, int) => int = "%mulint"
external \"/": (int, int) => int = "%divint"
external mod: (int, int) => int = "%modint"

let abs = x =>
  if x >= 0 {
    x
  } else {
    -x
  }

external land: (int, int) => int = "%andint"
external lor: (int, int) => int = "%orint"
external lxor: (int, int) => int = "%xorint"

let lnot = x => lxor(x, -1)

external lsl: (int, int) => int = "%lslint"
external lsr: (int, int) => int = "%lsrint"
external asr: (int, int) => int = "%asrint"

let max_int = lsr(-1, 1)
let min_int = max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"
external \"**": (float, float) => float = "?power_float"
external exp: float => float = "?exp_float"
external expm1: float => float = "?expm1_float"
external acos: float => float = "?acos_float"
external asin: float => float = "?asin_float"
external atan: float => float = "?atan_float"
external atan2: (float, float) => float = "?atan2_float"
external hypot: (float, float) => float = "?hypot_float"
external cos: float => float = "?cos_float"
external cosh: float => float = "?cosh_float"
external log: float => float = "?log_float"
external log10: float => float = "?log10_float"
external log1p: float => float = "?log1p_float"
external sin: float => float = "?sin_float"
external sinh: float => float = "?sinh_float"
external sqrt: float => float = "?sqrt_float"
external tan: float => float = "?tan_float"
external tanh: float => float = "?tanh_float"
external ceil: float => float = "?ceil_float"
external floor: float => float = "?floor_float"
external abs_float: float => float = "%absfloat"
external copysign: (float, float) => float = "?copysign_float"
external mod_float: (float, float) => float = "?fmod_float"
external frexp: float => (float, int) = "?frexp_float"
external ldexp: (float, int) => float = "?ldexp_float"
external modf: float => (float, float) = "?modf_float"
external float: int => float = "%floatofint"
external float_of_int: int => float = "%floatofint"
external truncate: float => int = "%intoffloat"
external int_of_float: float => int = "%intoffloat"
external float_of_bits: int64 => float = "?int64_float_of_bits"
let infinity = float_of_bits(0x7F_F0_00_00_00_00_00_00L)
let neg_infinity = float_of_bits(0xFF_F0_00_00_00_00_00_00L)
let nan = float_of_bits(0x7F_F0_00_00_00_00_00_01L)
let max_float = float_of_bits(0x7F_EF_FF_FF_FF_FF_FF_FFL)
let min_float = float_of_bits(0x00_10_00_00_00_00_00_00L)
let epsilon_float = float_of_bits(0x3C_B0_00_00_00_00_00_00L)

type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan
external classify_float: float => fpclass = "?classify_float"

/* String and byte sequence operations -- more in modules String and Bytes */

external string_length: string => int = "%string_length"
external bytes_length: bytes => int = "%string_length"
external bytes_create: int => bytes = "?create_bytes"
external string_blit: (string, int, bytes, int, int) => unit = "?blit_string"
external bytes_blit: (bytes, int, bytes, int, int) => unit = "?blit_string"
external bytes_unsafe_to_string: bytes => string = "%identity"
external bytes_unsafe_of_string: string => bytes = "%identity"

let \"^" = (s1, s2) => {
  let l1 = string_length(s1) and l2 = string_length(s2)
  let s = bytes_create(l1 + l2)
  string_blit(s1, 0, s, 0, l1)
  string_blit(s2, 0, s, l1, l2)
  bytes_unsafe_to_string(s)
}

/* Character operations -- more in module Char */

external int_of_char: char => int = "%identity"
external unsafe_char_of_int: int => char = "%identity"
let char_of_int = n =>
  if n < 0 || n > 255 {
    invalid_arg("char_of_int")
  } else {
    unsafe_char_of_int(n)
  }

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* Pair operations */

external fst: (('a, 'b)) => 'a = "%field0"
external snd: (('a, 'b)) => 'b = "%field1"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makemutable"
external \"!": ref<'a> => 'a = "%bs_ref_field0"
external \":=": (ref<'a>, 'a) => unit = "%bs_ref_setfield0"
external incr: ref<int> => unit = "%incr"
external decr: ref<int> => unit = "%decr"

/* String conversion functions */

external format_int: (string, int) => string = "?format_int"
external format_float: (string, float) => string = "?format_float"

let string_of_bool = b =>
  if b {
    "true"
  } else {
    "false"
  }
let bool_of_string = x =>
  switch x {
  | "true" => true
  | "false" => false
  | _ => invalid_arg("bool_of_string")
  }

let string_of_int = n => format_int("%d", n)

external int_of_string: string => int = "?int_of_string"
external string_get: (string, int) => char = "%string_safe_get"

let valid_float_lexem = s => {
  let l = string_length(s)
  let rec loop = i =>
    if i >= l {
      s ++ "."
    } else {
      switch string_get(s, i) {
      | '0' .. '9' | '-' => loop(i + 1)
      | _ => s
      }
    }

  loop(0)
}

let string_of_float = f => valid_float_lexem(format_float("%.12g", f))

external float_of_string: string => float = "?float_of_string"

/* List operations -- more in module List */

let rec \"@" = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{hd, ...\"@"(tl, l2)}
  }

/* I/O operations */

type in_channel
type out_channel

external open_descriptor_out: int => out_channel = "?ml_open_descriptor_out"
external open_descriptor_in: int => in_channel = "?ml_open_descriptor_in"

/* General output functions */

type open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

external open_desc: (string, list<open_flag>, int) => int = "?sys_open"

let open_out_gen = (mode, perm, name) => open_descriptor_out(open_desc(name, mode, perm))

let open_out = name =>
  open_out_gen(list{Open_wronly, Open_creat, Open_trunc, Open_text}, 0o666, name)

let open_out_bin = name =>
  open_out_gen(list{Open_wronly, Open_creat, Open_trunc, Open_binary}, 0o666, name)

external flush: out_channel => unit = "?ml_flush"

external out_channels_list: unit => list<out_channel> = "?ml_out_channels_list"

let flush_all = () => {
  let rec iter = x =>
    switch x {
    | list{} => ()
    | list{a, ...l} =>
      try flush(a) catch {
      | _ => ()
      }
      iter(l)
    }
  iter(out_channels_list())
}

external unsafe_output: (out_channel, bytes, int, int) => unit = "?ml_output"
external unsafe_output_string: (out_channel, string, int, int) => unit = "?ml_output"

external output_char: (out_channel, char) => unit = "?ml_output_char"

let output_bytes = (oc, s) => unsafe_output(oc, s, 0, bytes_length(s))

let output_string = (oc, s) => unsafe_output_string(oc, s, 0, string_length(s))

let output = (oc, s, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > bytes_length(s) - len) {
    invalid_arg("output")
  } else {
    unsafe_output(oc, s, ofs, len)
  }

let output_substring = (oc, s, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > string_length(s) - len) {
    invalid_arg("output_substring")
  } else {
    unsafe_output_string(oc, s, ofs, len)
  }

external output_byte: (out_channel, int) => unit = "?ml_output_char"
external output_binary_int: (out_channel, int) => unit = "?ml_output_int"

external marshal_to_channel: (out_channel, 'a, list<unit>) => unit = "?output_value"
let output_value = (chan, v) => marshal_to_channel(chan, v, list{})

external seek_out: (out_channel, int) => unit = "?ml_seek_out"
external pos_out: out_channel => int = "?ml_pos_out"
external out_channel_length: out_channel => int = "?ml_channel_size"
external close_out_channel: out_channel => unit = "?ml_close_channel"
let close_out = oc => {
  flush(oc)
  close_out_channel(oc)
}
let close_out_noerr = oc => {
  try flush(oc) catch {
  | _ => ()
  }
  try close_out_channel(oc) catch {
  | _ => ()
  }
}
external set_binary_mode_out: (out_channel, bool) => unit = "?ml_set_binary_mode"

/* General input functions */

let open_in_gen = (mode, perm, name) => open_descriptor_in(open_desc(name, mode, perm))

let open_in = name => open_in_gen(list{Open_rdonly, Open_text}, 0, name)

let open_in_bin = name => open_in_gen(list{Open_rdonly, Open_binary}, 0, name)

external input_char: in_channel => char = "?ml_input_char"

external unsafe_input: (in_channel, bytes, int, int) => int = "?ml_input"

let input = (ic, s, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > bytes_length(s) - len) {
    invalid_arg("input")
  } else {
    unsafe_input(ic, s, ofs, len)
  }

let rec unsafe_really_input = (ic, s, ofs, len) =>
  if len <= 0 {
    ()
  } else {
    let r = unsafe_input(ic, s, ofs, len)
    if r == 0 {
      raise(End_of_file)
    } else {
      unsafe_really_input(ic, s, ofs + r, len - r)
    }
  }

let really_input = (ic, s, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > bytes_length(s) - len) {
    invalid_arg("really_input")
  } else {
    unsafe_really_input(ic, s, ofs, len)
  }

let really_input_string = (ic, len) => {
  let s = bytes_create(len)
  really_input(ic, s, 0, len)
  bytes_unsafe_to_string(s)
}

external input_scan_line: in_channel => int = "?ml_input_scan_line"

let input_line = chan => {
  let rec build_result = (buf, pos, x) =>
    switch x {
    | list{} => buf
    | list{hd, ...tl} =>
      let len = bytes_length(hd)
      bytes_blit(hd, 0, buf, pos - len, len)
      build_result(buf, pos - len, tl)
    }
  let rec scan = (accu, len) => {
    let n = input_scan_line(chan)
    if n == 0 {
      switch /* n = 0: we are at EOF */
      accu {
      | list{} => raise(End_of_file)
      | _ => build_result(bytes_create(len), len, accu)
      }
    } else if n > 0 {
      /* n > 0: newline found in buffer */
      let res = bytes_create(n - 1)
      ignore(unsafe_input(chan, res, 0, n - 1))
      ignore(input_char(chan)) /* skip the newline */
      switch accu {
      | list{} => res
      | _ =>
        let len = len + n - 1
        build_result(bytes_create(len), len, list{res, ...accu})
      }
    } else {
      /* n < 0: newline not found */
      let beg = bytes_create(-n)
      ignore(unsafe_input(chan, beg, 0, -n))
      scan(list{beg, ...accu}, len - n)
    }
  }
  bytes_unsafe_to_string(scan(list{}, 0))
}

external input_byte: in_channel => int = "?ml_input_char"
external input_binary_int: in_channel => int = "?ml_input_int"
external input_value: in_channel => 'a = "?input_value"
external seek_in: (in_channel, int) => unit = "?ml_seek_in"
external pos_in: in_channel => int = "?ml_pos_in"
external in_channel_length: in_channel => int = "?ml_channel_size"
external close_in: in_channel => unit = "?ml_close_channel"
let close_in_noerr = ic =>
  try close_in(ic) catch {
  | _ => ()
  }
external set_binary_mode_in: (in_channel, bool) => unit = "?ml_set_binary_mode"

/* Output functions on standard output */

/* Output functions on standard error */

/* Input functions on standard input */

/* Operations on large files */

module LargeFile = {
  external seek_out: (out_channel, int64) => unit = "?ml_seek_out_64"
  external pos_out: out_channel => int64 = "?ml_pos_out_64"
  external out_channel_length: out_channel => int64 = "?ml_channel_size_64"
  external seek_in: (in_channel, int64) => unit = "?ml_seek_in_64"
  external pos_in: in_channel => int64 = "?ml_pos_in_64"
  external in_channel_length: in_channel => int64 = "?ml_channel_size_64"
}

/* Formats */

external sys_exit: int => 'a = "?sys_exit"

let exit_function = ref(flush_all)

let at_exit = f => {
  let g = exit_function.contents
  exit_function :=
    (
      () => {
        f()
        g()
      }
    )
}

let do_at_exit = () => exit_function.contents()

let exit = retcode => {
  do_at_exit()
  sys_exit(retcode)
}

let _ = register_named_value("Pervasives.do_at_exit", do_at_exit)
