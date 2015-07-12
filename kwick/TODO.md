# Todo — KWICK

## Parser

### Missing features

- [X] Extract statements
- [X] Lambda expressions
	- [X] Long form
	- [X] Short form
- [ ] Functions
	- [X] Standard functions
	- [ ] Getters
	- [ ] Setters
		- [ ] Constructive/destructive distinction
	- [ ] Reffers (?)
	- [ ] Methods
- [ ] Structures
	- [X] Value structs
	- [ ] Ref structs
	- [ ] Classes (?)
- [ ] Interfaces
	- [ ] `implements` declaration?
- [ ] Protocols
	- [ ] Standard functions
	- [ ] Getters / setters
		- [ ] Constructive / destructive distinction
		- [ ] `field` shorthand for getter/setter pair.
- [ ] Givens
- [ ] Imports
- [ ] `open` function declarations
- [ ] Static arguments
	- [ ] In declarations/definitions
	- [ ] In call expressions

### Planned modifications / fixes

- [ ] Change `@` precedence to bind more tightly than subscripting
- [ ] Some way to mix declarations, updates, `var` bindings and `let` bindings in a single destructuring assignment
- [ ] Be more permissive for return types declared without parenthes
- [ ] Multiple return values in short-form lambdas (requires parentheses to avoid ambiguity within argument lists)

### Potential additions

- [ ] Named infix operators, Haskell-style, like ``a `dot` b``
- [ ] Indentation-based strings, where all characters are interpreted literally (no escapes)

### Stylistic cleanup

- [ ] Normalize use of `fmap` vs `<$>` vs "bind-and-return style" for mapping functions over single parse results
- [ ] Normalize default parse results.  Some situations use `optional` and proceed to check for Nothing and replace it with a default value, either using `fromMaybe` or an explicit `case`.  Other situations use the `parseEither pat (return default)` pattern.
- [X] Factor out parenthesized comma-separated lists as their own combinator
