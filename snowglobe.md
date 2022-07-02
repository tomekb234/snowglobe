# Snowglobe programming language

## Types

Any type is either *copyable* or *non-copyable*.
The values of a copyable type can be copied.
The values of a non-copyable type can only be moved and cannot be copied.

A copyable type can also be *trivial*.
The values of a trivial type can be copied and deleted
without performing any additional actions.

## Basic types

`()` - Unit type. Its only value is `()`.

`bool` - Boolean type. Its only values are `true` and `false`.

`i8`, `i16`, `i32`, `i64` - Signed integer types.
The `iN` type is a subtype of `iM` if `N <= M`.

`u8`, `u16`, `u32`, `u64` - Unsigned integer types.
The `uN` type is a subtype of `uM` if `N <= M`.
The `uN` type is a subtype of `iM` if `N < M`.

`f32`, `f64` - Floating-point number types.
The `f32` type is a subtype of `f64`.

The `bool` type is a subtype of all integer types
with `true` as `1` and `false` as `0`.

`never` - A type without any values.
Subtype of all types.
Given to expressions which break the control-flow,
such as `break`, `return`, or a call to the `error` function.

All types above are copyable and trivial.

## Compound types

In the following definitions, `S`, `T` and `U` denote any types.

`?T` - Optional value type.
Its values are either `none` or `some x` for `x` of type `T`.

`(S, T, U)` - Tuple type.
A tuple must have at least two fields.
The type expression `(T)` is not a tuple type and is the same as `T`.

`[T; n]` - Array type with constant size `n`.
The size `n` must be a constant unsigned integer.

For all type constructors above,
the resulting type is copyable if and only all its components are copyable
and it is trivial if and only if all its components are trivial.

All type constructors above are covariant in their components.

## Pointer types

`&T` - Basic pointer type.
Copyable but not trivial.
Invariant in `T`.

`$T` - Global pointer type.
Copyable and trivial.
Subtype of `&T`.
Invariant in `T`.

`*T` - Strong shared pointer type.
Copyable but not trivial.
Invariant in `T`.
A pointer of this type has a reference counter attached.
When the last strong shared pointer is deleted,
the memory is deallocated.

`~T` - Weak shared pointer type.
Copyable but not trivial.
Supertype of `*T`.
Invariant in `T`.
A pointer of this type has a reference counter attached
and must be tested before dereferencing.
When the last shared pointer, either strong or weak, is deleted,
the reference counter itself is deallocated.

`@T` - Unique pointer type.
Non-copyable.
Subtype of `*T`.
Invariant in `T`.
When a unique pointer is deleted,
the memory is deallocated.

`&[T]`, `$[T]`, `*[T]`, `~[T]`, `@[T]` - Slice pointer types.
Defined as above, but point to multiple values.
Supertypes of `&[T; n]`, `$[T; n]`, `*[T; n]`, `~[T; n]`, `@[T; n]`
respectively, for any `n`.
Invariant in `T`, with the exception of `never`, for which it is covariant,
because a slice pointer to `never` must be empty anyway.
A pointer of one of these types has a size attached.

`&S.T`, `$S.T`, `*S.T`, `~S.T`, `@S.T`,
`&[S].T`, `&S.[T]`, `&[S].[T]`, etc. - Inner pointer types.
A value of one of these types is a pair of two pointers, outer and inner,
pointing to values of types `S` and `T` respectively.
The inner pointer points to data contained inside a larger structure
pointed by the outer pointer.
The inner pointer is the one which is dereferenced.
The outer pointer is the one which has any reference counters attached
and for which the memory can be deallocated.

For any `S` and `T`, the type `&S.T` is a subtype of `&T` and `$S.T` is a subtype of `$T`.

`func (S) -> T` - Function pointer type.
Copyable but not trivial.
Invariant in both parameter types and the result type.
A pointer of this type has an additional pointer attached.
This additional pointer has unknown type
and is passed along with arguments when calling the function.

`func &U (S) -> T`, `func *U (S) -> T`, `func ~U (S) -> T`, `func @U (S) -> T` -
Function pointer types with known types of additional attached pointers.
A value of one of these types
behaves both as a function pointer of type `func (S) -> T`
and as a respective pointer to `U`.

For any `S`, `T` and `U`, the type `func &U (S) -> T` is a subtype of `func (S) -> T`.

`func $(S) -> T` - Global function pointer type.
Copyable and trivial.
Invariant in both parameter types and the result type.
Unlike `func (S) -> T`, a pointer of this type has no additional pointer attached.

An expression which is a name of a global function
can be either of type `func (S) -> T` or `func $(S) -> T`.

## User-defined types

```
struct S {
    x: T,
    y: U
}
```

In global context, declares a struct type named `S` with two fields of types `T` and `U`.

A struct type is non-copyable by default.
It can be marked as copyable by appending `copyable` after its name,
in which case all its fields must be copyable.
If a struct type is copyable and all its fields are trivial,
then the struct type is also trivial.

```
enum E {
    A,
    B(T),
    C(T, U)
}
```

In global context, declares an enum type named `E` with variants `A`, `B` and `C`.
The `A` variant has no fields,
the `B` variant has one field of type `T`,
and the `C` variant has two fields of types `T` and `U`.
A variant with no fields cannot have empty parentheses after its name.

A value of an enum type is a tagged disjoint union of its variants.

In the same way as struct types, enum types are non-copyable by default.

A type of a field of a user-defined type cannot recursively refer
to the type being defined or to types defined below,
unless it is inside a pointer type.

## Confinement

All values inside a function are either *confined* or *non-confined*.
The confinement is marked by prefixing the type of a value with `!`,
with bare `T` meaning that the value is confined,
and `!T` meaning that the value is non-confined.
For trivial types the confinement is irrelevant and the `!` marker is unnecessary.

Confined and non-confined values are not interchangeable.

A value of a non-confined local variable can become confined
when passed as an argument to a function or when using a `locally` block.

A confined local variable is guaranteed to hold valid data in its entire scope.
This means that this data must have been obtained
from non-confined local variable as described above.

A non-confined value behaves normally.

The behavior of a confined value is altered in the following ways:

- can be copied even if its type is non-copyable,
- the copying and deletion never performs any additional actions,
- the pointer kinds `*` and `@` can be converted to `&`,
- cannot be copied outside of its scope.

## Constants

`()` - The unit value.

`true`, `false` - The boolean values.

An integer literal can be in one of these forms:

- `99` - decimal,
- `0xff`, `0xFF` - hexadecimal,
- `0o77` - octal,
- `0b11` - binary.

The digits can be separated with the `_` character.

An integer literal is given the smallest signed type in which it can fit,
for example `127` has type `i8` and `128` has type `i16`.
This can be altered by appending the desired type after the literal,
for example `123u32` has type `u32`,
or by appending just `i` or `u`,
for example `255u` has type `u8` and `256u` has type `u16`.

A floating-point lieral can be in any of these forms:

`1.2`, `1.2e3`, `1.2e+3`, `1.2e-3`, `123f`.

The digits can be separated with the `_` character.

The type of a floating-point literal is `f64`, unless `f32` is appended,
for example `1.2` has type `f64` and `1.2f32` has type `f32`.

## Compound values

`none` - Absent value.
Has type `?never`,
which makes it convertible to `?T` for any `T`.

`some x` - Present value `x`.
Has type `?T`, where `T` is the type of `x`.

`(x, y, z)` - Tuple with values `x`, `y` and `z`.
Must have at least two fields.
The expression `(x)` is not a tuple and is the same as `x`.

`[x, y, z]` - Array with values `x`, `y` and `z`.
The values must have a common supertype `T`
and the resulting array has type `[T; 3]` in this example case.
The empty array `[]` has type `[never; 0]`,
which makes it convertible to any other empty array type.
An array value is not a pointer and behaves in the same way as a tuple value.

`S(x, y)` - If `S` is a name of a struct type,
then constructs a value of this type
with `x` and `y` as values for consecutive fields of `S`.

The extraction syntax `x.y` and `x[i]`
is not available for bare struct, tuple and array values.
The components of these values can only be extracted
with destructuring assignment.

`E::A`, `E::B(x)`, `E::C(x,y)` - If `E` is a name of an enum type,
then constructs a value of this type with one of the variants.
If the variant has any fields,
then the values for the consecutive fields are passed in the parentheses.

The components of compound values can be indexed with indices to change their order,
for example `(1: y, 0: x)` is equivalent to `(x, y)`.
The components of struct value can also be indexed by their names,
for example `S(x: x, y: y)`, assuming that `x` and `y` are fields of `S`.

When constructing a compound value,
either all or none of components with non-trivial types must be confined,
and then the result has the same confinement.
In particular, if any component is confined,
then all else must also be confined,
which can be achieved with `locally` block.
If there are no components or all components have trivial types,
then the result is non-confined.

## Arithmetic

Standard boolean operations:

`!x`, `x && y`, `x || y`.

The `&&` and `||` operators are short-circuit.

Standard arithmetic operations:

`-x`, `x + y`, `x - y`, `x * y`, `x / y`, `x % y`.

Standard bitwise arithmetic operations:

`~x`, `x & y`, `x | y`, `x ^ y`, `x << y`, `x >> y`.

Standard compound assignment operations:

`x += y`, `x -= y`, `x *= y`, `x /= y`, `x %= y`, `x &= y`, `x |= y`, `x ^= y`, `x <<= y`, `x >>= y`.

Standard numeric comparsion operations:

`x == y`, `x != y`, `x < y`, `x <= y`, `x > y`, `x >= y`.

Conditional operator (the parentheses are required):

`(if b then x else y)`.

Only one of the expressions `x` and `y` is evaluated,
depending on the value of `b`.
The values `x` and `y` must have a common supertype,
which is the type of the whole expression.

`x as T` - Converts `x` to any other numeric type `T`,
which can be any of `bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32` or `f64`.
Can result in truncation or precision loss.

## Variables

```
const x: T = y;
```

In global context, declares a named constant `x` for value `y`,
which must be an expression computable at compile-time.
The value is then substituted at every use of `x`.
The type annotation is optional.

```
var x: T = y;
```

In global context, declares a global variable `x` with required initial value `y`,
which must be an expression computable at compile-time.
The type annotation is optional.
An expression which reads a global variable always has non-confined result.

In function context, declares a local variable `x` with type `T`.
The initial value `y` is optional.
The type annotatation is optional if the variable is initialized
and required if the variable is uninitialized.
The declaration can shadow a previous variable `x` with different type.

An uninitialized variable cannot be used before its first assignment.

If a variable has non-copyable type,
then its usage *moves out* the variable instead of copying its value.
A moved out variable cannot be used until it is assigned again.

When the scope of a variable with non-trivial type ends
and it was initialized and not moved out,
then the value of the variable is deleted.

The initialization and move-out status of a variable
may become ambiguous due to control-flow branching,
which is an error when it must be decided whether
the value of the variable will be deleted or not.

## Assignment

```
x = y;
```

Assigns value `y` to *left-value* `x`.

A left-value is a target of an assignment
and can be any one of these expressions:

- `_` - ignores and deletes the value,
- variable name `x` - assigns to the variable `x`
and deletes its previous value if it was initialized and not moved out,
- `var x: T` - declares and initializes local variable `x` with type `T`,
- `var x` - declares and initializes local variable `x` with inferred type,
- `(x, y)` - assigns consecutive fields of tuple to left-values `x` and `y`,
- `[x, y]` - assigns consecutive items of array to left-values `x` and `y`,
- `S(x, y)` - assigns consecutive fields of struct value to left-values `x` and `y`,
- `*p` - writes to pointer `p` and deletes its previous value,
- `p.x` - writes to field `x` of struct or tuple pointer `p`,
- `p[i]` - writes to item `i` of slice or array pointer `p`.

The components of compound left-values can be reordered
in the same way as components of regular compound values.

`p` can be arbitrary expression with pointer type
and it evaluates before the assignment.

A confined value cannot be assigned to:

- a non-confined local variable,
- any local variable outside of current `locally` block,
- a global variable,
- value pointed to by a pointer.

## Control flow statements

Standard control-flow statements:

```
if x {
    ...
} elif y {
    ...
} else {
    ...
}
```

```
while x {
    ...
    break;
    ...
    continue;
    ...
} else {
    ...
}
```

The `break` and `continue` are expressions of type `never`.

The `else` block after `while` statement is entered
when the loop was not interrupted with `break`.

```
if x in y {
    ...
}
```

Checks if optional value `y` is present.
If it is present, then its value is extracted to `x`
and the following block is entered.
`x` can be any left-value.
If the type of `x` is non-copyable, then `y` is moved out.

This expression can be used instead of `bool` value
in any branch of an `if`-`elif`-`else` statement and in a `while` statement.

```
match e with A {
    ...
} with B(s) {
    ...
} with C(s, t) {
    ...
} else {
    ...
}
```

Matches the enum value `e` with one of its variants.
`s` and `t` can be any left-value.
If the type of an extracted variant field is non-copyable, then `e` is moved out.
The `else` block is optional and entered when none of the variants matched.

```
for i in a..b {
    ...
}
```

Iteration through all integers from `a` to `b` exclusively.
`i` can be any left-value, for example `var i`.
The loop can be reversed by appending `reversed` after the range.

```
for x in a {
    ...
}
```

Iteration through all dereferenced items of a slice or array pointer.
`x` can be any left-value and the loop can be reversed.
If the result of dereference is non-copyable,
then the pointer `a` must be unique and is moved out by the loop.

```
for i, x in a {
    ...
}
```

Iteration with index.
`i` can by any left-value.

```
for x ref a {
    ...
}
```

```
for i, x ref a {
    ...
}
```

Iteration through pointers to items instead of their dereferenced values.
The pointer `a` cannot be unique.

## The `swap` statement

```
swap x with y;
```

Swaps the values of `x` and `y`.
Both `x` and `y` can be any left-value without `var` and `_`.
In particular, the expressions can be results of pointer dereference.

Note that this statement is useful in dealing with non-copyable types.

```
swap x with y {
    ...
}
```

Swaps the values of `x` and `y` before entering and after leaving the block.
The swap is performed even when leaving the block with `break`, `continue` or `return`.

## The `locally` block

```
locally x {
    ...
}
```

Inside a function, makes a confined copy of the local variable `x`.
The variable `x` becomes confined inside this block
and cannot be copied outside of it.
Any other confined value cannot be copied outside of this block as well.
The assignment to `x` inside this block
has no effect on the outside value of `x`,
analogously as with a function call.

Multiple variables can be confined with a single `locally` block.

The following convenience syntaxes for `locally` can be used:

```
if x in y locally {
    ...
}
```

```
match e locally with A {
    ...
}
```

```
for x in y locally {
    ...
}
```

```
swap x with y localy {
    ...
}
```

The expression directly before `locally` must be a local variable.
In all cases except `swap`, the `locally` block is inserted on the outside.
In case of `swap`, the `locally` block is inserted inside and only `y` is confined.

## Functions

```
func f(x: S, y: T) -> U {
    ...
}
```

In global context, defines a function `f`
with parameters `x` and `y` of types `S` and `T`
and return type `U`.
The type annotations for parameters are required.
The return type is optional and defaults to `()`.
All non-confined parameters must come before confined parameters.
The return type cannot have confinement marker.

The function body is a sequence of statements, which can be any of:

- expression evaluation,
- variable declaration,
- assignment,
- control-flow statement,
- `swap` statement or block,
- `locally` block,
- inner function.

Every statement ends with either `;` or `}`.

A `return x` expression can be used to return `x` as the result of the function.
The type of this expression is `never`.
The returned value cannot be confined.
The function body can optionally end with a single expression without `;`,
which is equivalent to using `return`
with this expression at the end of the function.

The function must return some value at its end,
unless all control-flow branches unambiguously return.

If the return type of the function is `()`,
then the value `()` is implicitly returned at the end of the function
and the `return` expression can be used without this value.

`f(x, y)` - Calls the function `f` with arguments `x` and `y` and returns its result.
The arguments of a function call can have their types
changed from non-confined to confined.
The result of a function call is always non-confined.

## Inner functions

TODO

## Pointers

TODO

## Built-in variables and functions

```
var argv: $[$[u8]]

func read(count: u64) -> @[u8]
func read_word() -> @[u8]
func read_line() -> @[u8]
func read_int() -> i64
func read_uint() -> u64
func read_float() -> f64

func print(output: &[u8])
func print_word(output: &[u8])
func print_line(output: &[u8])
func print_int(output: i64)
func print_uint(output: u64)
func print_float(output: f64)

func exit(status: i32) -> never
func error(message: &[u8]) -> never
func unreachable() -> never
```
