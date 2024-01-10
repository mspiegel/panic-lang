# panic-lang

![static analysis](https://github.com/mspiegel/panic-lang/actions/workflows/static-analysis.yaml/badge.svg) ![unit tests](https://github.com/mspiegel/panic-lang/actions/workflows/unit-tests.yaml/badge.svg) [![codecov](https://codecov.io/gh/mspiegel/panic-lang/graph/badge.svg?token=7H2EY41PIE)](https://codecov.io/gh/mspiegel/panic-lang)

The Panic programming language will be a programming language that never panics.

Programming languages have a buffet of techniques to handle errors:

- errors can be propagated as values
- errors can be propagated as values that disrupt control flow (exceptions)
- errors can be unrecoverable

Design goals for the Panic language:

- all operations are total operations
- value-oriented programming
- errors are values
- arithmetic operations can produce an error
- heap allocation can produce an error
- stack allocation can produce an error
- all values are zeroized on drop

## Type System

Panic has three categories of types:

The Panic enum. A global set of values across the entire program. A value in the Panic enum can be declared more than once. Inspired by the Error Set type of [Zig](https://ziglang.org).

Nominal types. Nominal types represent values that are definitely not errors. The nominal types are what you would typically consider to be the types in other programming languages. Nominal types are identified by the keyword `nom`. Functions cannot return a nominal type.

Anxious types. Anxious types are the default types of the Panic type system. Anxious types are defined as sum types (tagged union) of nominal types and the Panic enum. 

### Primitive Types

- Signed integers: `i8`, `i16`, `i32`, `i64`, `i128` and `isize`
- Nominal signed integer: `nom i8`, `nom i16`, `nom i32`, `nom i64`, `nom i128` and `nom isize`
- Bitwise integers: `bi8`, `bi16`, `bi32`, `bi64`, `bi128` and `bisize`
- Nominal bitwise integers: `nom bi8`, `nom bi16`, `nom bi32`, `nom bi64`, `nom bi128` and `nom bisize`

- Unsigned integers: `u8`, `u16`, `u32`, `u64`, `u128` and `usize`
- Nominal unsigned integer: `nom u8`, `nom u16`, `nom u32`, `nom u64`, `nom u128` and `nom usize`
- Bitwise unsigned integers: `bu8`, `bu16`, `bu32`, `bu64`, `bu128` and `busize`
- Nominal bitwise unsigned integers: `nom bu8`, `nom bu16`, `nom bu32`, `nom bu64`, `nom bu128` and `nom busize`

- Unit type `()`, whose only possible values are empty tuple `()` and the Panic enums.
- Nominal unit type `nom ()`, whose only possible value is the empty tuple `()`

#### Integer Operations

| Operation           | i8  | nom i8 | bi8 | nom bi8 |
| ------------------- | --- | ------- | -- | ------- |
| `+`, `-`, `*`       | ✓  |          | ✓  | ✓      |
| `/`, `%`            | ✓  |          | ✓  |        |
| `==`                |    | ✓        |    | ✓      |
| `<`, `>`            |    | ✓        |    | ✓      |
| `&`, `\|`, `^`, `!` |    |          | ✓  | ✓      |
| `<<`, `>>`,         |    |          | ✓  | ✓      |
| Display             |    | ✓        |    | ✓      |
| Debug               | ✓  | ✓        | ✓ | ✓      |

`i8` can return Panic value `IntegerOverflow` on arithmetic operations `+`, `-`, `*`  
`i8` can return Panic value `IntegerOverflow` or `IntegerDivizionByZero` on arithmetic operations `/`, `%`  
`bi8` and `nom bi8` perform wrapped arithmetic on `+`, `-`, `*`  
`bi8` performs wrapped arithmetic or Panic value `IntegerDivizionByZero` on arithmetic operations `/`, `%`  
Equality and Comparison are defined on the nominal integers  
Display is defined on all nominal primitive types  
Debug is defined on all primitive types  