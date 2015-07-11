# Todo — KWICK

## Parser

### Missing features

- [X] Extract statements
- [ ] Lambda expressions
	- [ ] Long form
	- [ ] Short form
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

### Planned modifications / fixes

- [ ] Change `@` precedence to bind more tightly than subscripting
- [ ] Some way to mix declarations, updates, `var` bindings and `let` bindings in a single destructuring assignment
- [ ] Be more permissive for return types declared without parenthes

### Potential additions

- [ ] Named infix operators, Haskell-style, like ``a `dot` b``
- [ ] Indentation-based strings, where all characters are interpreted literally (no escapes)
