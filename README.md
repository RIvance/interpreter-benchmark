# Interpreter Benchmark

A Simply Typed Lambda Calculus (STLC) interpreter with support for both direct and trampolined evaluation modes.

## Usage

```bash
interpreter-benchmark [OPTIONS] <FILE>
```

### Options

- `<FILE>` - Input file containing an STLC expression (required)
- `-t, --trampolined` - Use trampolined evaluation to prevent stack overflow on deep recursion
- `-v, --verbose` - Show the parsed expression and type before evaluation
- `-h, --help` - Print help information

## Benchmark Result
```
================================================================================
Benchmark: Simple recursive sum
================================================================================

         n |   Direct (s) |    Tramp (s) |    Speedup |               Status
--------------------------------------------------------------------------------
        16 |     0.000889 |     0.000880 |      1.01x |   Tramp 1.01x faster
        32 |     0.000610 |     0.000626 |      0.97x |  Direct 1.03x faster
        64 |     0.000706 |     0.000607 |      1.16x |   Tramp 1.16x faster
       128 |     0.000595 |     0.000711 |      0.84x |  Direct 1.20x faster
       256 |     0.000908 |     0.000897 |      1.01x |   Tramp 1.01x faster
       512 |     0.000998 |     0.001286 |      0.78x |  Direct 1.29x faster
      1024 |     0.001477 |     0.001503 |      0.98x |  Direct 1.02x faster
      2048 |     0.001908 |     0.002810 |      0.68x |  Direct 1.47x faster
      4096 |     0.003467 |     0.005417 |      0.64x |  Direct 1.56x faster
      8192 |     OVERFLOW |     0.011203 |        N/A | Tramp prevents overflow
     16384 |     OVERFLOW |     OVERFLOW |        N/A |        Both overflow

================================================================================
Benchmark: Nested recursive (bench n = sum n + bench (n-1))
================================================================================

         n |   Direct (s) |    Tramp (s) |    Speedup |               Status
--------------------------------------------------------------------------------
        16 |     0.000906 |     0.000863 |      1.05x |   Tramp 1.05x faster
        32 |     0.000778 |     0.001461 |      0.53x |  Direct 1.88x faster
        64 |     0.001637 |     0.002617 |      0.63x |  Direct 1.60x faster
       128 |     0.004009 |     0.008319 |      0.48x |  Direct 2.07x faster
       256 |     0.011659 |     0.029969 |      0.39x |  Direct 2.57x faster
       512 |     0.045552 |     0.103118 |      0.44x |  Direct 2.26x faster
      1024 |     0.139604 |     0.382918 |      0.36x |  Direct 2.74x faster
      2048 |     0.500373 |     1.501295 |      0.33x |  Direct 3.00x faster
      4096 |     2.051108 |     6.212348 |      0.33x |  Direct 3.03x faster
      8192 |     OVERFLOW |    26.804507 |        N/A | Tramp prevents overflow
     16384 |     OVERFLOW |     OVERFLOW |        N/A |        Both overflow

================================================================================
Benchmark: Tail-recursive countdown
================================================================================

         n |   Direct (s) |    Tramp (s) |    Speedup |               Status
--------------------------------------------------------------------------------
        16 |     0.000827 |     0.000615 |      1.34x |   Tramp 1.34x faster
        32 |     0.000555 |     0.001484 |      0.37x |  Direct 2.68x faster
        64 |     0.000525 |     0.000735 |      0.71x |  Direct 1.40x faster
       128 |     0.000805 |     0.000938 |      0.86x |  Direct 1.16x faster
       256 |     0.000629 |     0.001456 |      0.43x |  Direct 2.31x faster
       512 |     0.000979 |     0.000963 |      1.02x |   Tramp 1.02x faster
      1024 |     0.001136 |     0.001803 |      0.63x |  Direct 1.59x faster
      2048 |     0.001720 |     0.003021 |      0.57x |  Direct 1.76x faster
      4096 |     0.002909 |     0.004928 |      0.59x |  Direct 1.69x faster
      8192 |     0.005519 |     0.009624 |      0.57x |  Direct 1.74x faster
     16384 |     OVERFLOW |     OVERFLOW |        N/A |        Both overflow

================================================================================
Benchmark: Accumulator pattern
================================================================================

         n |   Direct (s) |    Tramp (s) |    Speedup |               Status
--------------------------------------------------------------------------------
        16 |     0.002257 |     0.000536 |      4.21x |   Tramp 4.21x faster
        32 |     0.000445 |     0.000606 |      0.74x |  Direct 1.36x faster
        64 |     0.000489 |     0.000941 |      0.52x |  Direct 1.92x faster
       128 |     0.000592 |     0.000854 |      0.69x |  Direct 1.44x faster
       256 |     0.000604 |     0.000942 |      0.64x |  Direct 1.56x faster
       512 |     0.000741 |     0.001075 |      0.69x |  Direct 1.45x faster
      1024 |     0.001271 |     0.001813 |      0.70x |  Direct 1.43x faster
      2048 |     0.002118 |     0.003087 |      0.69x |  Direct 1.46x faster
      4096 |     0.003397 |     0.005737 |      0.59x |  Direct 1.69x faster
      8192 |     OVERFLOW |     0.010931 |        N/A | Tramp prevents overflow
     16384 |     OVERFLOW |     OVERFLOW |        N/A |        Both overflow
```

## Language Syntax

### Types
- `Int` - Integer type
- `Bool` - Boolean type
- `T1 -> T2` - Function type (right-associative)

### Expressions

#### Literals
```
42          // Integer
-10         // Negative integer
true        // Boolean
false       // Boolean
```

#### Variables
```
x
my_var_123
```

#### Lambda Abstractions
```
\x: Int. x + 1              // Backslash syntax
fn x: Int => x + 1          // Alternative fn syntax
\f: Int -> Int. f 10        // Higher-order function
```

#### Function Application
```
f x                         // Application (left-associative)
f x y                       // Same as (f x) y
(\x: Int. x) 42            // Lambda application
```

#### Arithmetic
```
1 + 2                       // Addition
5 - 3                       // Subtraction
2 * 3                       // Multiplication
1 + 2 * 3                   // Respects precedence: 1 + (2 * 3)
(1 + 2) * 3                 // Parentheses for grouping
```

#### Comparisons
```
x == 42                     // Equality
x < 10                      // Less than
x > 0                       // Greater than
x + 1 < y * 2               // Works with arithmetic
```

#### Conditionals
```
if x < 0 then -x else x                            // Absolute value
if x == 0 then 1 else if x == 1 then 2 else 3      // Nested conditionals
```

#### Recursion (Fixpoint)
```
fix f: Int -> Int. \x: Int. if x == 0 then 1 else x * f (x - 1)
```

The fixpoint operator allows defining recursive functions. The syntax is:
```
fix name: Type. body
```

### Examples

#### Identity Function

File: `examples/identity.stlc`
```
(\x: Int. x) 42
```

#### Factorial (Recursive)

File: `examples/factorial.stlc`
```
(fix f: Int -> Int. \x: Int. if x == 0 then 1 else x * f (x - 1)) 5
```

#### Fibonacci (Recursive)

File: `examples/fibonacci.stlc`
```
(fix fib: Int -> Int. \n: Int. if n < 2 then n else fib (n - 1) + fib (n - 2)) 10
```

#### Curried Functions

File: `examples/curried.stlc`
```
(\x: Int. \y: Int. x + y) 10 20
```