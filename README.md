# panic-lang

![static analysis](https://github.com/mspiegel/panic-lang/actions/workflows/static-analysis.yaml/badge.svg) ![unit tests](https://github.com/mspiegel/panic-lang/actions/workflows/unit-tests.yaml/badge.svg) [![codecov](https://codecov.io/gh/mspiegel/panic-lang/graph/badge.svg?token=7H2EY41PIE)](https://codecov.io/gh/mspiegel/panic-lang)

The Panic programming language will be a programming language that never panics.

Design goals for the Panic language:

- all operations are total operations
  - arithmetic operations can produce an error
  - heap allocation can produce an error
  - stack allocation can produce an error
    - 'inline' is not a hint. inline functions never have a separate stack allocation 
- errors are values
- all values are zeroized when no longer unused
  - every program will end with stack and heap full of 0s
- generic programming (generics) will not be supported with a separate syntax
- macros (compile-time evaluation) will not be supported with a separate syntax
- Panic will have an interpreter and a compiler
  - the interpreter language will be a superset of the compiler language
  - the interpreter language will support types as values (first-class types)
  - compilation will proceed by running the interpreter and then running the compiler
- operator precedence has a partial ordering. Ambiguities must be resolved with parenthesis.
  - as is the case in [Wuffs](https://github.com/google/wuffs), [Carbon](https://github.com/carbon-language/carbon-lang), [Austral](https://austral-lang.org/), etc.
- language keywords must be both prefixed and suffixed by the underscore ('\_') character
  - keywords and identifiers are future-proof to never overlap
  - your text editor or IDE can convert the unsightly characters to syntax highlighting

Panic has been inspired by [Rust](https://www.rust-lang.org/), [Zig](https://ziglang.org/), [Pony](https://www.ponylang.io/), [Hylo](https://www.hylo-lang.org/), and other languages.

## Current Status

The interpreter can **parse** and **run** the following example:

```
_decl_ ExpectedNonNegative _primitive_ _type_ _is_ (Error & Provenance) { }

_decl_ factorial _fn_ (n : i32) -> (i32 |
    ExpectedNonNegative | ArithmeticOverflow | 
    StackOverflow) {
    _if_ n < 0 {
        _return_ ExpectedNonNegative;
    } _else_ _if_ n == 0 {
        _return_ 1;
    } _else_ {
        _return_ n * factorial((n - 1)?)?;
    }
}

_decl_ main _fn_ () -> i32 {
    _debug(factorial(-1));
    _debug(factorial(0));
    _debug(factorial(1));
    _debug(factorial(2));
    _debug(factorial(3));
    _debug(factorial(4));
    _debug(factorial(5));
    _debug(factorial(6));
    _debug(factorial(7));
    _debug(factorial(8));
    _debug(factorial(9));
    _debug(factorial(10));
    _debug(factorial(11));
    _debug(factorial(12));
    _debug(factorial(13));
    _return_ 0;
}
```

Generates the following output:

```
ExpectedNonNegative at (213, 232)
1
1
2
6
24
120
720
5040
40320
362880
3628800
39916800
479001600
ArithmeticOverflow at (313, 337)
```

## Data Types

### Primitive Types

| Name         | Description                   |
|--------------|-------------------------------|
|bool          | boolean                       |
|i32           | 32-bit signed integer         |

#### Arithmetic Operators

| Operation   | Input Type | Output Type      |
|-------------|------------|------------------|
| add         | iN | (iN \| ArithmeticOverflow) |
| subtract    | iN | (iN \| ArithmeticOverflow) |
| multiply    | iN | (iN \| ArithmeticOverflow) |
| divide      | iN | (iN \| ArithmeticOverflow \| ArithmeticDivisionByZero) |
| negate      | iN | (iN \| ArithmeticOverflow) |

### User-Defined Primitive Types

A user-defined primitive type is a type for which
there is only one instance (for the purposes
of the equals operator). This is borrowed from
the [Pony language](https://www.ponylang.io/).
The following user-defined primitive types are
defined in the Panic standard library:

```
_decl_ ArithmeticOverflow
_primitive_ _type_ _is_ (Error & Provenance) { }

_decl_ ArithmeticDivisionByZero
_primitive_ _type_ _is_ (Error & Provenance) { }

_decl_ StackOverflow
_primitive_ _type_ _is_ (Error & Provenance) { }

_decl_ HeapOverflow
_primitive_ _type_ _is_ (Error & Provenance) { }

_decl_ None
_primitive_ _type_ { }
```

`Error` and `Provenance` are built-in traits. `Error` values are returned early from a function when encountered by the `?` operator. `Provenance` values store the lexical location of the code at which the value was created.

### Union Types

Union types are the union of other types. These are usually named Enum types in other programming languages.
A union type that is a union of a single type can be used to implement the 'newtype' pattern in Rust.

### Value Types

Value types are types that have value semantics.
The allowed components (fields) of a value type are primitive types,
user-defined primitive types, other value types,
array types of the allowed types, and union types of the allowed types.
Value types cannot be recursively defined.

An example value type:
```
_decl_ Point _value_ _type_
{
   x: i32,
   y: i32,
   z: i32,
}
```

### Reference Types

Reference types are types that have reference semantics.
References are collected by the garbage collector.
Reference types can be recursively defined.

An example reference type:

```
_decl_ LinkedList _union_ _type_ (None | Node) { }

_decl_ Node _reference_ _type_
{
  item: i32,
  next: (None | Node),
}
```

### Resource Types

Resource types are types that have reference semantics
and have a finalizer.

### Array Types

### Slice Types