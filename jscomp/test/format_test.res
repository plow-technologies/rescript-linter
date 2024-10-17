let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

let eq3 = (loc, a, b, c) => {
  eq(loc, a, b)
  eq(loc, b, c)
  eq(loc, a, c)
}

module M = {
  @val @scope("Number") external infinity: float = "POSITIVE_INFINITY"
  @val @scope("Number") external neg_infinity: float = "NEGATIVE_INFINITY"
  @val @scope("Number") external nan: float = "NaN"
  @val @scope("Number") external max_float: float = "MAX_VALUE"
}

let () = {
  eq(__LOC__, 0x3.fp+1, 7.875)

  eq(__LOC__, -0x3.fp+1, -7.875)
  /* in standard, it is still infinity, but 
    ideally should give an warning
 */
  eq3(__LOC__, 0x1p2047, M.infinity, infinity)
  eq3(__LOC__, -0x1p2047, M.neg_infinity, neg_infinity)
  eq3(__LOC__, max_float, 0x1.ffff_ffff_ffff_fp+1023, M.max_float)
  eq(__LOC__, classify_float(0x1.2p2047), FP_infinite)
  eq(__LOC__, classify_float(0x1.1p2047), FP_infinite)

  eq(__LOC__, min_float, 0x1p-1022)
  eq(__LOC__, epsilon_float, 0x1p-52)
  eq(__LOC__, 0x0.0000_0000_0000_1p-1022, 5e-324)
  eq(__LOC__, 0x1.0000_0000_0000_1 -. 1., epsilon_float)
  eq(__LOC__, 0x1p-1023 /. 0x1p-1022, 0x1p-1)
  eq(__LOC__, classify_float(0x1p-1023), FP_subnormal)
  eq(__LOC__, 0x1p-1023, 0x0.8p-1022)
  eq(__LOC__, 0x0.ffff_ffff_ffff_ffff_ffp-1022, 0x1p-1022)

  eq(__LOC__, (1. +. 0xffp0 /. 0x100p0) *. 8., 0x1.ffp3)
  eq(__LOC__, (1. +. 0xfffp0 /. 0x1000p0) *. 8., 0x1.fffp3)
  eq(__LOC__, (1. +. 0xffffp0 /. 0x10000p0) *. 8., 0x1.ffffp3)
}

/* TODO: add scanf example */

let f = (loc, ls) => List.iter(((a, b)) => eq(loc, float_of_string(a), b), ls)

let () = f(__LOC__, list{("0x3.fp+1", 0x3.fp+1), (" 0x3.fp2", 0x3.fp2), (" 0x4.fp2", 0x4.fp2)})

external hexstring_of_float: (float, int, char) => string = "?hexstring_of_float"

let sl = (f: float): string => hexstring_of_float(f, -1, '-')

let aux_list = (loc, ls) => List.iter(((a, b)) => eq(loc, sl(a), b), ls)
let literals = list{
  (0x3.fp+1, "0x1.f8p+2") /* (1. +. 0xf8p0 /. 0x1_00p0) *. 4. */,
  (0.3, "0x1.3333333333333p-2"),
  (infinity, "infinity"),
  (0.4, "0x1.999999999999ap-2"),
  (0.5, "0x1p-1"),
  (0.6, "0x1.3333333333333p-1"),
  (0.7, "0x1.6666666666666p-1"),
  (0.8, "0x1.999999999999ap-1"),
  (0.9, "0x1.ccccccccccccdp-1"),
}

let () = aux_list(__LOC__, literals)

let () = eq(__LOC__, String.uppercase_ascii(hexstring_of_float(0x3.fp+1, -1, '-')), "0X1.F8P+2")
/* The internal does some thing similar with "%H" */
let scan_float = (loc, s, expect) => eq(loc, float_of_string(s), expect)
/* Scanf.sscanf s "%h" (fun result -> eq loc result expect) */

let () = {
  scan_float(__LOC__, "0x3f.p1", 0x3f.p1)
  scan_float(__LOC__, "0x1.3333333333333p-2", 0.3)
  List.iter(((a, b)) => scan_float(__LOC__, b, a), literals)
}

let () = {
  let f = -9.9
  let f1 = -.f
  eq(__LOC__, f1, 9.9)
}

let () = Mt.from_pair_suites(__MODULE__, suites.contents)
