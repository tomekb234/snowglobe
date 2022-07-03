# Snowglobe programming language

## Types

Any type is either *copyable* or *non-copyable*.
The values of a copyable type can be copied.
The values of a non-copyable type can only be moved and cannot be copied.

A copyable type can also be *trivial*.
The values of a trivial type only contain data
which is valid for the whole run-time of the program.

### Basic types

`()` - The unit type. Its only value is `()`.

`bool` - The boolean type. Its only values are `true` and `false`.

`i8`, `i16`, `i32`, `i64` - Signed integer types.
The `iN` type is a subtype of `iM` for `N <= M`.

`u8`, `u16`, `u32`, `u64` - Unsigned integer types.
The `uN` type is a subtype of `uM` for `N <= M`.
The `uN` type is a subtype of `iM` for `N < M`.

`f32`, `f64` - Floating-point number types.
The `f32` type is a subtype of `f64`.

The `bool` type is a subtype of all integer types
with `true` as `1` and `false` as `0`.

`never` - A type without any values.
Subtype of all types.
Given to expressions which break the control-flow,
such as `break`, `return`, or a call to the `error` function.

All types above are copyable and trivial.

### Compound types

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

### Pointer types

`&T` - Basic pointer type.
Copyable but non-trivial.
Invariant in `T`.

`$T` - Global pointer type.
Copyable and trivial.
Subtype of `&T`.
Invariant in `T`.

`*T` - Strong shared pointer type.
Copyable but non-trivial.
Invariant in `T`.
A pointer of this type has a reference counter attached.
When the last strong shared pointer is deleted,
the memory is deallocated.

`~T` - Weak shared pointer type.
Copyable but non-trivial.
Supertype of `*T`.
Invariant in `T`.
A pointer of this type has a reference counter attached
which must be checked before dereferencing.
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
Supertypes of `&[T; n]`, `$[T; n]`, `*[T; n]`, `~[T; n]` and `@[T; n]`,
respectively, for any `n`.
Invariant in `T`, with the exception of `never`, for which it is covariant,
because a slice pointer to `never` must be empty anyway.
A pointer of one of these types has a size attached.

`&S.T`, `$S.T`, `*S.T`, `~S.T`, `@S.T`,
`&[S].T`, `&S.[T]`, `&[S].[T]`, etc. - Inner pointer types.
A value of one of these types is a pair of two pointers, outer and inner,
pointing to values of types `S` and `T` respectively.
The inner pointer points to data
contained in a larger structure,
which is pointed by the outer pointer.
The inner pointer is the one which is dereferenced.
The outer pointer is the one which has any reference counters attached to it
and for which the memory can be deallocated.

For any `S` and `T`, the type `&S.T` is a subtype of `&T` and `$S.T` is a subtype of `$T`.

`func (S) -> T` - Function pointer type.
Copyable but non-trivial.
Invariant in both parameter types and the result type.
A pointer of this type has an additional pointer attached.
This additional pointer has unknown type
and is passed along with arguments when calling the function.

`func &U (S) -> T`, `func *U (S) -> T`, `func ~U (S) -> T`, `func @U (S) -> T` -
Function pointer types with known types of additional attached pointers.
A value of one of these types
behaves both as a function pointer of type `func (S) -> T`
and as a pointer to `U` with respective kind.

For any `S`, `T` and `U`, the type `func &U (S) -> T` is a subtype of `func (S) -> T`.

`func $(S) -> T` - Global function pointer type.
Copyable and trivial.
Invariant in both parameter types and the result type.
Unlike `func (S) -> T`, a pointer of this type has no additional pointer attached.

An expression which is the name of a global function
can be either of type `func (S) -> T` or `func $(S) -> T`.

### User-defined types

```
struct S {
    x: T1,
    y: T2
}
```

In global context, declares a struct type named `S` with two fields of types `T1` and `T2`.

A struct type is non-copyable by default.
It can be marked as copyable by appending `copyable` after its name,
in which case all its fields must be copyable.
If a struct type is copyable and all its fields are trivial,
then the struct type is also trivial.

```
enum E {
    A,
    B(T),
    C(T1, T2)
}
```

In global context, declares an enum type named `E` with variants `A`, `B` and `C`.
The `A` variant has no fields,
the `B` variant has one field of type `T`,
and the `C` variant has two fields of types `T1` and `T2`.
A variant with no fields cannot have empty parentheses after its name.

A value of an enum type is a tagged disjoint union of its variants.

In the same way as struct types, enum types are non-copyable by default.

The type of a field of a user-defined type cannot recursively refer
to the user-defined type being declared or to types declared below,
unless it is used inside a pointer type.

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
from non-confined local variable in the way described above.

A non-confined value behaves normally.

The behavior of a confined value is altered in the following ways:

- can be copied even if its type is non-copyable,
- the copying and deletion never performs any additional actions
such as incrementing the reference counter or deallocating memory,
- the pointer kinds `*` and `@` can be converted to `&`,
- cannot be copied outside of its scope.

Note that it is never necessary to use the pointer kinds `@` and `*` instead of `&`
for a confined argument of a function.

## Expressions

There is no type inference in this version of the language,
and every expression has a type assigned to it immediately.

### Constants

`()` - The unit value.

`true`, `false` - The boolean values.

An integer literal can be in any of these forms:

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

A floating-point literal can be in any of these forms:

`1.2`, `1.2e3`, `1.2e+3`, `1.2e-3`, `123f`.

The digits can be separated with the `_` character.

The type of a floating-point literal is `f64`, unless `f32` is appended,
for example `1.2` has type `f64` and `1.2f32` has type `f32`.

`'a'` - A character literal.
Has type `u8` and must be a single ASCII character.
The following escape sequences are allowed: `\'`, `\"`, `\0`, `\n`, `\r`, `\t`, `\\ `.

### Compound values

In the following definitions, `x`, `y` and `z` denote any expressions.

`none` - An absent value.
Has type `?never`,
which makes it convertible to `?T` for any `T`.

`some x` - The value `x` marked as present.
Has type `?T`, where `T` is the type of `x`.

`(x, y, z)` - A tuple with field values `x`, `y` and `z`.
Must have at least two fields.
The expression `(x)` is not a tuple and is the same as `x`.

`[x, y, z]` - An array with item values `x`, `y` and `z`.
The values must have a common supertype `T`,
and the resulting array has type `[T; n]`,
where `n` is the number of items.
The empty array `[]` has type `[never; 0]`,
which makes it convertible to any other empty array type.
An array value is not a pointer and behaves in the same way as a tuple value.

`S(x, y)` - If `S` is the name of a struct type,
then constructs a value of this type
with `x` and `y` as values for consecutive fields of `S`.

The extraction syntax `x.y` and `x[i]`
is not available for bare struct, tuple and array values in this version of the language.
The components of these values can only be extracted with destructuring assignment.

`E::A`, `E::B(x)`, `E::C(x,y)` - If `E` is the name of an enum type,
then constructs a value of this type with one of the variants.
If the variant has any fields,
then the values for the consecutive fields are passed in the parentheses.

The components of compound values can be indexed with indices to change their order,
for example `(1: y, 0: x)` is equivalent to `(x, y)`.
The components of a struct value can also be indexed by their names,
for example `S(y: y, x: x)` is equivalent to `S(x, y)`,
given that `x` and `y` are consecutive fields of `S`.

When constructing a compound value,
either all or none of the components with non-trivial types must be confined,
and then the result has the same confinement.
In particular, if any component is confined,
then all other must also be confined,
which can be achieved with a `locally` block.
If there are no components or all components have trivial types,
then the result is non-confined.

### Arithmetic

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

Conditional operator:

`(if b then x else y)`.

The parentheses are required.
Only one of the expressions `x` and `y` is evaluated,
depending on the value of `b`.
The values `x` and `y` must have a common supertype,
which is the type of the whole expression.

`x as T` - Converts `x` to any other numeric type `T`,
which can be any of `bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32` or `f64`.
Can result in truncation or precision loss.

### Pointers

All pointers, except for weak shared pointers, always point to valid data.

Pointers to local variables are not available in this version of the language,
because they would break confinement safety invariants.

`$x` - Returns a pointer to the global variable `x`.

`"abc"` - Returns a pointer to new global array of `u8` with the text given in quotes.
The text can use any ASCII-extending encoding, in particular UTF-8.
The same escape sequences as with character literals are allowed.
Note that the value of the array is not recreated
at every evaluation of this expression.

`@x` - Allocates new heap memory with value `x`
and returns a unique pointer to it.
The value `x` must be non-confined.
If `x` is non-copyable, then it is moved out.

`@[x; n]` - Allocates new heap memory with multiple copies of `x`
and returns a unique slice pointer to it.
The value `x` must be non-confined and must be copyable.
`n` can be any expression with unsigned integer type.

The result of an allocation is always non-confined.

`p.&x` - Returns an inner pointer to the field `x` of the struct or tuple pointed by the pointer `p`.
`x` must be a field name in case of a struct,
or an integer literal in case of a tuple.

`p[ref i]` - Returns an inner pointer to the item `i` of the array or slice pointed by the pointer `p`.
`i` can be any expression with unsigned integer type.
If `i` is out of range, then the program aborts.

`p[ref i..j]` - Returns an inner pointer to a subslice of the array or slice pointed by the pointer `p`.
The range of the subslice is from `i` to `j` exclusively.
Both `i` and `j` are optional, with default values of `0` and the length of the array or slice, respectively.
If either `i` or `j` is out of range, then the program aborts.

`^p` - Returns the outer pointer associated with the inner pointer `p`.

When taking an inner or outer pointer from a pointer `p`,
the result of the expression has the same confinement as `p`,
and if `p` is non-copyable, then it is moved out.

`*p` - Dereferences the pointer `p`.
The pointer cannot be a weak shared pointer or a pointer to a slice.
The result of this expression is always non-confined.
If the result is non-copyable, then `p` must be a unique pointer and must be moved out.

`p.x`, `p[i]` - Equivalent to `*(p.&x)` and `*(p[ref i])`, respectively,
but do not move out `p` if it is non-copyable
and the result of the dereference is copyable.

`#p` - Returns the length of the array or slice pointed by the pointer `p`.

`?p` - Tests the weak shared pointer `p`.
The type of this expression is `?*T` for `p` of type `~T`.
If the memory pointed by `p` was deallocated because of lack of any strong references, returns `none`.
If the memory pointed by `p` still has any strong references,
returns a new strong shared pointer to it wrapped in `some`.
The result of this expression has the same confinement as `p`.

## Functions

```
func f(x: S1, y: S2) -> T {
    ...
}
```

In global context, defines a function named `f`
with parameters `x` and `y` of types `S1` and `S2`
and return type `T`.
The type annotations for parameters are required.
The return type is optional and defaults to `()`.
All non-confined parameters must come before confined parameters.
The return type cannot have a confinement marker.

The function body is a sequence of *statements*, which can be any of:

- expression evaluation,
- variable declaration,
- assignment,
- control-flow statement,
- `swap` statement or block,
- `locally` block,
- closure.

Every statement ends either with `;` or `}`.

The `return x` expression can be used to return `x` as the result of the function.
The returned value must be non-confined.
The type of `return` expression is `never`.
The function body can optionally end with a single expression without `;`,
which is equivalent to using `return`
with this expression at the end of the function.

The function must return some value at its end,
unless all control-flow branches unambiguously return.

If the return type of the function is `()`,
then the value `()` is implicitly returned at the end of the function
and the `return` expression can be used without this value.

`f(x, y)` - Calls the function `f` or the function pointer `f`
with arguments `x` and `y` and returns its result.
The non-confined arguments of a function call can become confined.
The result of a function call is always non-confined.

A function can call itself and any other function,
even if that function is defined below.

The entry point of a program is the function `main`,
which takes no arguments and returns `()`.

### Variables

```
const x: T = y;
```
In global context, declares a name `x` for constant `y`.
The constant `y` is substituted at every use of `x`.
The type annotation is optional.
A named constant with unsigned integer type
can be used as a size for an array type.

```
var x: T = y;
```

In global context, declares a global variable named `x`
with required initial value `y`, which must be a constant.
The type annotation is optional.
An expression which reads a global variable always has a non-confined result.

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
can become ambiguous due to control-flow branching,
which is an error when it must be decided whether
the value of the variable is deleted or not.

### Assignment

```
x = y;
```

Assigns the value `y` to the *left-value* `x`.

A left-value is the target of an assignment
and can be any one of these expressions:

- `_` - ignores and deletes the value,
- variable name `x` - assigns to the variable `x`
and deletes its previous value if it was initialized and not moved out,
- `var x: T` - declares and initializes new local variable `x` with type `T`,
- `var x` - declares and initializes new local variable `x` with the same type as the value,
- `(x, y)` - assigns consecutive fields of the tuple to the left-values `x` and `y`,
- `[x, y]` - assigns consecutive items of the array to the left-values `x` and `y`,
- `S(x, y)` - assigns consecutive fields of the struct to the left-values `x` and `y`,
- `*p` - writes to the pointer `p` and deletes its previous value,
- `p.x` - writes to the field `x` of the struct or tuple pointer `p`,
- `p[i]` - writes to the item `i` of the slice or array pointer `p`.

The components of compound left-values can be reordered
in the same way as components of regular compound values.

`p` can be any expression with pointer type
and it is evaluated before the assignment.

A confined value cannot be assigned to:

- a non-confined local variable,
- any local variable outside of current `locally` block,
- a global variable,
- the value pointed to by a pointer.

### Control flow statements

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

The `else` block after the `while` statement is entered
when the loop was not interrupted with `break`.

A variable cannot be moved out inside a loop
if it was assigned only before the loop.

```
if x in y {
    ...
}
```

Checks if the optional value `y` is present.
If it is present, then the value is extracted to `x`
and the following block is entered.
`x` can be any left-value, in particular `var x`.
If the type of `x` is non-copyable, then `y` is moved out.

This expression can be used instead of a `bool` value
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
`s` and `t` can be any left-values.
If the type of an extracted variant field is non-copyable, then `e` is moved out.
The `else` block is optional and only entered when none of the variants matched.

```
for i in a..b {
    ...
}
```

Iteration through all integers from `a` to `b` exclusively.
`i` can be any left-value, in particular `var i`.
The loop can be reversed by appending `reversed` after the range.

```
for x in p {
    ...
}
```

Iteration through all dereferenced items of the slice or array pointer `p`.
`x` can be any left-value and the loop can be reversed.
If the result of dereference is non-copyable,
then the pointer must be unique and it is moved out by the loop.

```
for i, x in p {
    ...
}
```

Iteration with index.
`i` can by any left-value.

```
for x ref p {
    ...
}
```

```
for i, x ref p {
    ...
}
```

Iteration through pointers to items instead of their dereferenced values.
The pointer `p` cannot be unique.

### The `swap` statement

```
swap x with y;
```

Swaps the values of `x` and `y`.
Both `x` and `y` can be any left-values without `var` and `_`.

Note that this statement is useful in dealing with non-copyable types.

```
swap x with y {
    ...
}
```

Swaps the values of `x` and `y` before entering and after leaving the block.
The swap is performed even when the block is left with `break`, `continue` or `return`.

### The `locally` block

```
locally x {
    ...
}
```

Makes a confined copy of the local variable `x`.
The variable `x` becomes confined inside this block
and cannot be copied outside of it.
Any other confined value cannot be copied outside of this block as well.
The assignment to `x` inside this block
has no effect on the outside value of `x`,
analogously to a function call.

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

The expression directly before `locally` must be the name of a local variable.
In all cases except for `swap`, the `locally` block is inserted on the outside.
In case of `swap`, the `locally` block is inserted inside and only `y` is confined.

### Closures

```
func f(x: S1, y: S2) -> T {
    ...
}
```

In function context, defines a *copying closure* assigned to new variable `f`.
Copying closures are defined in the same way as global functions.
A copying closure can use variables and arguments of the directly enclosing function.
All these variables must be non-confined
and they are copied or moved out at the point where the closure is defined.
Any writes to these copied variables inside the closure persist between multiple calls,
but have no effect on the values of the variables of the enclosing function.

If no variables from the enclosing function are used,
then the closure has type `func (S1, S2) -> T`.
If a single variable from the enclosing function is used,
then the closure has type `func @U (S1, S2) -> T`,
where `U` is the type of the variable.
If multiple variables from the enclosing function are used,
then the closure has type `func @(U1, U2, ...) (S1, S2) -> T`,
where `U1, U2, ...` are the types of the variables in the order of first appearance.

A copying closure is always non-confined.

`(func x: S1, y: S2 -> z)` - Returns a *non-copying closure*.
The parentheses are required.
`z` can by any single expression and it is the result of the closure.
There is no annotation of the return type.
A non-copying closure can use variables and arguments of the directly enclosing function.
All these variables must be confined and they are accesed directly by the closure.
The closure has type `func (S1, S2) -> T`.

A non-copying closure is always confined.

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
