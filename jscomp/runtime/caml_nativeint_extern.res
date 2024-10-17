external add: (int, int) => int = "?nativeint_add"
external div: (int, int) => int = "?nativeint_div"
external rem: (int, int) => int = "?nativeint_mod"
external shift_right_logical: (int, int) => int = "?nativeint_lsr"
external mul: (int, int) => int = "?nativeint_mul"

external to_float: int => float = "%identity"
external of_float: float => int = "?int_of_float"
/* TODO: this could be promoted to `#int_of_float` */
@val external to_string: int => string = "String"
