// Pmod_ident
/* c0 */ module /* c1 */ X /* c2 */ = /* c3 */ Y /* c4 */ 

// Pmod_structure
/* c0 */ module /* c1 */ X /* c2 */ = /* c3 */ {
  let /* inside1 */ a /* inside2 */ = 2 /* trailing */
} // trailing

// Pmod_extension
/* c0 */ module /* c1 */ X /* c2 */ = /* c3 */ %ext(/* c4 */ "test" /*c5*/) /* c6 */ 

// Pmod_unpack
/* c0 */ module /* c1 */ New_three /* c2 */ = /* c3 */ unpack(
  /* c4 */three /* c5 */: /* c6 */ X_int /* c7 */ ) /* c8 */

// Pmod_constraint
/* c0 */ module /* c1 */ X /* c2 */ = (/* c3 */ X /* c4 */ : /* c5 */ Int /* c6 */)
/* c0 */ module /* c1 */ X /* c2 */: /* c4 */ Int /* c5 */ = /* c6 */ X /* c7 */

// Pmod_apply
/* c0 */ module /* c1 */ X /* c2 */ = /* c3 */ F /* c4 */(
  /* c4 */ Arg1 /* c5 */,
  /* c6 */ Arg2 /* c7 */,
  /* c8 */ Arg3 /* c9 */
) /* c10 */
/* c0 */ module /* c1 */ X /* c2 */ = /* c3 */F(
  /* c4 */ A /* c5 */ : /* c6 */ SetLike /* c7 */,
  /* c7 */ B /* c8 */: /* c9 */ TreeLike /* c10 */
) /* c11 */
/* c0 */ module /* c1 */ S0 /* c2 */ = /* c3 */ Make /* c4 */( /* lbrace */ {
  type t = int /* c5 */
  let eq /* c6 */ = (x, y) => x == y
} /* rbrace */) // trailing

// Pmod_functor
/* c0 */ module /* c1 */ F /* c2 */ = /* before parameters */ (
/* c3 */ A /* c4 */: /* c5 */ X /* c6 */,
/* c7 */ B /* c8 */: /* c9 */ Y /* c 10 */,
/* c7 */ C /* c8 */: /* c9 */ Z /* c 10 */
) => /* c11 */ ReturnMod /* c12 */ 

module Make = /* before parameters */ (
  /* before A */ A /* after A */: /* before X */ X /* after X */,
  /* before B */ B /* after B */: /* before Y */ Y /* after Y */
) : /* c1 */ Set /* c2 */ => {let a /* inside */ = A.a + B.b}

module Make = /* before parameters */ (
  /* before A */ A /* after A */: /* before X */ X /* after X */,
  /* before B */ B /* after B */: /* before Y */ Y /* after Y */
)  => ({let a /* inside */ = A.a + B.b}: /* c1 */ Set /* c2 */)
