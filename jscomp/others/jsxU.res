/*** Internal: use Jsx directly. */

@@uncurried

type element
type ref

@val external null: element = "null"

external float: float => element = "%identity"
external int: int => element = "%identity"
external string: string => element = "%identity"

external array: array<element> => element = "%identity"

type componentLike<'props, 'return> = 'props => 'return
type component<'props> = componentLike<'props, element>

/* this function exists to prepare for making `component` abstract */
external component: componentLike<'props, element> => component<'props> = "%identity"
