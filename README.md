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

_decl_ factorial _fn_ (n : i32) -> (i32 | ExpectedNonNegative | ArithmeticOverflow | StackOverflow) {
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
