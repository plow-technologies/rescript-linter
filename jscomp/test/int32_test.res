let f = x => Int32.logor(Int32.logor(x, 0l), 0l)

let f = x => (
  Int32.shift_right_logical(x, 0),
  Int32.shift_right_logical(x, 1),
  Int32.shift_right_logical(x, 2),
)

let shift_right_logical_tests = (
  Ext_array_test.range(0, 31) |> Array.map(x => Int32.shift_right_logical(-1l, x)),
  [
    -1l,
    2147483647l,
    1073741823l,
    536870911l,
    268435455l,
    134217727l,
    67108863l,
    33554431l,
    16777215l,
    8388607l,
    4194303l,
    2097151l,
    1048575l,
    524287l,
    262143l,
    131071l,
    65535l,
    32767l,
    16383l,
    8191l,
    4095l,
    2047l,
    1023l,
    511l,
    255l,
    127l,
    63l,
    31l,
    15l,
    7l,
    3l,
    1l,
  ],
)

let shift_right_tests = (
  Ext_array_test.range(0, 31) |> Array.map(x => Int32.shift_right(Int32.min_int, x)),
  [
    -2147483648l,
    -1073741824l,
    -536870912l,
    -268435456l,
    -134217728l,
    -67108864l,
    -33554432l,
    -16777216l,
    -8388608l,
    -4194304l,
    -2097152l,
    -1048576l,
    -524288l,
    -262144l,
    -131072l,
    -65536l,
    -32768l,
    -16384l,
    -8192l,
    -4096l,
    -2048l,
    -1024l,
    -512l,
    -256l,
    -128l,
    -64l,
    -32l,
    -16l,
    -8l,
    -4l,
    -2l,
    -1l,
  ],
)

let shift_left_tests = (
  Ext_array_test.range(0, 31) |> Array.map(x => Int32.shift_left(1l, x)),
  [
    1l,
    2l,
    4l,
    8l,
    16l,
    32l,
    64l,
    128l,
    256l,
    512l,
    1024l,
    2048l,
    4096l,
    8192l,
    16384l,
    32768l,
    65536l,
    131072l,
    262144l,
    524288l,
    1048576l,
    2097152l,
    4194304l,
    8388608l,
    16777216l,
    33554432l,
    67108864l,
    134217728l,
    268435456l,
    536870912l,
    1073741824l,
    -2147483648l,
  ],
)

let test_div = 61 / 2

let \"*~" = Int32.mul
let suites = ref(
  \"@"(
    {
      open Mt
      list{
        (__LOC__, _ => Eq(\"*~"(0xffff_ffffl, 0xffff_ffffl), 1l)),
        (__LOC__, _ => Eq(\"*~"(0xffff_ffffl, 0x7fff_ffffl), -2147483647l)),
      }
    },
    \"@"(
      {
        let (a, b) = shift_right_logical_tests
        Ext_array_test.map2i(
          (i, a, b) => ("shift_right_logical_cases " ++ __unsafe_cast(i), _ => Mt.Eq(a, b)),
          a,
          b,
        ) |> Array.to_list
      },
      \"@"(
        {
          let (a, b) = shift_right_tests
          Ext_array_test.map2i(
            (i, a, b) => ("shift_right_cases " ++ __unsafe_cast(i), _ => Mt.Eq(a, b)),
            a,
            b,
          ) |> Array.to_list
        },
        {
          let (a, b) = shift_left_tests
          Ext_array_test.map2i(
            (i, a, b) => ("shift_left_cases " ++ __unsafe_cast(i), _ => Mt.Eq(a, b)),
            a,
            b,
          ) |> Array.to_list
        },
      ),
    ),
  ),
)

let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let () = {
  eq(__LOC__, Int32.bits_of_float(0.3), 1050253722l)
  eq(__LOC__, Int32.float_of_bits(1050253722l), 0.300000011920928955)
}
Mt.from_pair_suites(__MODULE__, suites.contents)
