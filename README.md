# Snowglobe ❄🔮

An experimental programming language with affine types, smart pointers, and *scope-confined values*.

## Introduction

Consider the following simple C++ program:

```C++
void print(int* p) {
    cout << *p << endl;
}

void test() {
    int* p = new int(123);
    print(p);
    delete p;
}
```

The `print` function takes a pointer to an integer and prints its value.
We cannot however deduce from its signature alone that this the only thing it does with the pointer.
It could as well have been implemented in this unexpected way:

```C++
int* g;

void print(int* p) {
    g = p;
    cout << *g << endl;
}
```

In this case, the value of `p` (which is a memory address) *leaks* out of the scope of `print`.
This causes the global variable `g` to hold an invalid value when the `test` function ends,
and an attempt to dereference it may lead to undefined behavior.

One way to prevent such problems is to only use C++'s shared or unique pointers instead of raw pointers.

With shared pointers, the memory is deallocated only when the last reference to it is removed,
which is convenient, but comes with some run-time overhead:

```C++
void print(shared_ptr<int> p) {
    cout << *p << endl;
    // reference counter decreases as the pointer goes out of scope
}

void test() {
    auto p = make_shared(123); // reference counter initialized with one
    print(p); // reference counter increases as the pointer is copied
    // memory deallocated as the reference counter drops to zero
}
```

With unique pointers, we uphold the invariant that there is only one pointer to a given memory address,
which removes the need for reference counters, but makes the pointers less convenient to use:

```C++
unique_ptr<int> print(unique_ptr<int> p) {
    cout << *p << endl;
    return p;
}

void test() {
    auto p = make_unique(123);
    p = print(move(p));
    // memory deallocated as the pointer goes out of scope
}
```

The type of a unique pointer is an example of an *affine* type,
which means that the value can only be *moved* and not copied.
In C++, the use of a bare name of a variable results with a copy of its value
(with exceptions such as when returning from a function),
and we can prevent this behavior by marking the usage with `move`.
In particular, passing the value as an argument to a function would count as a copy,
so we need to mark it with `move` as in the example above.
If the function does not move the pointer somewhere else
and we do not want it to deallocate the memory,
then we have no choice but to explicitly return the pointer.
This makes it necessary for the pointer
to be inconveniently interwoven by the caller of the function.

Since unique pointers are not interchangeable with shared pointers,
should we provide both versions of the `print` function?
It would be very inconvenient to make multiple versions of the same function
just to accomodate different kinds of pointers.
The first version of the `print` function with a raw pointer as an argument
clearly does not leak the pointer out of its scope,
so it is reasonably the most convenient implementation.
We can just explicitly state in its documentation that the pointer is not leaked,
and then the callers of this function who use either shared or unique pointers
can pass the underlying raw pointers without breaking the respective invariants about the number of references.

This assertion, that a given value does not leak outside of its scope
is precisely what this language allows to express at the *type level*.
It allows the function-local variables (in particular the arguments) to be marked as *confined*,
which inhibits copying them out of their scope,
but also relaxes their invariants about copying within this scope.
In particular, confined unique pointers can be safely copied anyway,
and the reference counters of a confined shared pointers can be safely ignored.

Note that this particular problem with memory safety has already been solved
by the [Rust](https://www.rust-lang.org/) language with its notions of *borrows* and *lifetimes*.
This language is somewhat inspired by Rust, but takes a simpler
(and probably less useful) approach to pointers at the type level.
It may be suitable for implementation of algorithms and data structures
if you care about performance and do not want to manage memory manually.
It is by no means a full general-purpose language,
but merely a demonstration of a concept.
We also had quite a lot of fun implementing it. :)

## Language characteristics

- Statically and strongly typed
- Safe (as we believe)
- With affine types
- With built-in smart pointers
- Without garbage collection
- Lacking module system
- Lacking type inference
- Lacking polymorphism

## Example

```
struct S {
    x: i64,
    p: @i64 // unique pointer
}

// the type S is non-copyable because it contains a unique pointer

var q: ?@i64 = none; // optional unique pointer
var z: i64 = 0;

func f1(s: !S) { // argument marked as non-confined
    var s1 = s; // moves out `s`
    // var s2 = s; // error: `s` is moved out

    S(var x, var p) = s1; // destructuring assignment

    q = some p; // ok, `p` is marked as non-confined
    z = x;
}

func f2(s: S) { // values are marked as confined by default
    var s1 = s;
    var s2 = s; // can make multiple copies of `s` despite it having non-copyable type

    // f1(s); // error: `f1` can potentially leak `s`

    S(var x, var p) = s;

    print_int(x);
    print_line("");
    print_int(*p);
    print_line("");

    // q = some p; // error: cannot leak `p`
    z = x; // ok, `x` has a trivial type and can be leaked anyway
}

func main() {
    var x = read_int();
    var y = read_int();
    var p = @y; // unique pointer to newly allocated memory
    var s = S(x, p);

    f2(s);
    f2(s); // can be called multiple times despite `s` having non-copyable type

    f1(s); // moves out `s`

    // f2(s); // error: `s` is moved out

    var r = some @y;

    swap q with r; // the only way to read a global variable with non-copyable type

    if var p in r { // extracts value from r if present
        print_int(*p);
        print_line("");

        // automatic deallocation of memory pointed by p
    }
}
```

## Documentation

See [snowglobe.md](snowglobe.md) for reference.

See [examples/](examples/) for more examples.

## Building

Prerequisites:

- C++ compiler with C++17 support
- [Re2c](https://re2c.org/), min. version 3.0
- [Bison](https://www.gnu.org/software/bison/), min. version 3.8
- [LLVM](https://llvm.org/), min. version 12

Use Make to build the project.
The resulting binary is `build/snowglobe`.

## Project status

The language documentation is in progress.

Some cases for type checker and LLVM code generation are not yet implemented.

The source code will probably need some refactoring and tidying up.

Although we believe that this type system guarantees memory safety (and we have no proof of that),
this implementation is almost certainly not free of bugs,
and so unfortunately it may be possible to compile a program which runs into undefined behavior.
