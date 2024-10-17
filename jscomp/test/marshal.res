@@warning("a")
/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1997 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

type extern_flags =
  | No_sharing
  | Closures
  | Compat_32
/* note: this type definition is used in 'byterun/debugger.c' */

external to_bytes: ('a, list<extern_flags>) => bytes = "?output_value_to_string"
external to_string: ('a, list<extern_flags>) => string = "?output_value_to_string"
external to_buffer_unsafe: (bytes, int, int, 'a, list<extern_flags>) => int =
  "?output_value_to_buffer"

let to_buffer = (buff, ofs, len, v, flags) =>
  if ofs < 0 || (len < 0 || ofs > Bytes.length(buff) - len) {
    invalid_arg("Marshal.to_buffer: substring out of bounds")
  } else {
    to_buffer_unsafe(buff, ofs, len, v, flags)
  }

/* The functions below use byte sequences as input, never using any
   mutation. It makes sense to use non-mutated [bytes] rather than
   [string], because we really work with sequences of bytes, not
   a text representation.
*/

external from_bytes_unsafe: (bytes, int) => 'a = "?input_value_from_string"
external data_size_unsafe: (bytes, int) => int = "?marshal_data_size"

let header_size = 20
let data_size = (buff, ofs) =>
  if ofs < 0 || ofs > Bytes.length(buff) - header_size {
    invalid_arg("Marshal.data_size")
  } else {
    data_size_unsafe(buff, ofs)
  }
let total_size = (buff, ofs) => header_size + data_size(buff, ofs)

let from_bytes = (buff, ofs) =>
  if ofs < 0 || ofs > Bytes.length(buff) - header_size {
    invalid_arg("Marshal.from_bytes")
  } else {
    let len = data_size_unsafe(buff, ofs)
    if ofs > Bytes.length(buff) - (header_size + len) {
      invalid_arg("Marshal.from_bytes")
    } else {
      from_bytes_unsafe(buff, ofs)
    }
  }

let from_string = (buff, ofs) =>
  /* Bytes.unsafe_of_string is safe here, as the produced byte
   sequence is never mutated */
  from_bytes(Bytes.unsafe_of_string(buff), ofs)
