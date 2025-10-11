# Interpreter Benchmark

A Simply Typed Lambda Calculus (STLC) interpreter with support for direct, trampoline, and worklist evaluation strategies. 

## Usage

```
Usage: stlc-interpreter [options] <source-file>

Arguments:
  <source-file>              Source file to evaluate (.stlc)

Options:
  -i, --interpreter <type>   Interpreter type: direct, trampoline, worklist (default: direct)
  -h, --help                 Show this help message
```

## Language Syntax

### Types
- `Int` - Integer type
- `Bool` - Boolean type
- `T1 -> T2` - Function type (right-associative)
- `{field1: T1, field2: T2, ...}` - Record type

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
fn x: Int => x + 1              // Primary fn syntax
\x: Int. x + 1                  // Alternative backslash syntax (with dot)
fn f: Int -> Int => f 10        // Higher-order function
\x: Int => x + 1                // Backslash with arrow also works
```

#### Function Application
```
f x                                         // Application (left-associative)
f x y                                       // Same as (f x) y
(fn x: Int => x) 42                         // Lambda application
(fn x: Int => fn y: Int => x + y) 10 20     // Curried application
```

#### Let Bindings
```
let x = 5 in
let y = 10 in
x + y
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

#### Recursion (Let Rec)
```
let rec fact: Int -> Int = fn n: Int =>
  if n == 0 then 1 else n * fact (n - 1)
in fact 5
```

The `let rec` construct allows defining recursive functions. The syntax is:
```
let rec name: Type = body in expression
```

Note: The `fix` operator is also supported with syntax `fix name: Type. body`, but `let rec` is the preferred style.

#### Records
```
{x = 10, y = 20}            // Record literal
{name = "value", count = 42}

let point = {x = 5, y = 10} in
point.x + point.y           // Record field access (projection)
```

### Examples

#### Simple Let Binding

File: `examples/simple.stlc`
```
let x = 10 in
let y = 32 in
x + y
```
Result: `42`

#### Factorial (Recursive)

File: `examples/factorial.stlc`
```
let rec fact: Int -> Int = fn n: Int =>
  if n == 0 then 1 else n * fact (n - 1)
in fact 5
```
Result: `120`

#### Fibonacci (Recursive)

File: `examples/fibonacci.stlc`
```
let rec fib: Int -> Int = fn n: Int =>
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)
in fib 10
```
Result: `55`

#### Large Recursion Test

File: `examples/largerec.stlc`
```
let rec sum: Int -> Int = fn n: Int =>
  if n < 1 then 0
  else n + sum (n - 1)
in sum 114514
```
Result: `6556785355`

This example tests stack overflow handling. The direct interpreter may overflow the stack, but the trampoline interpreter will handle it correctly.
