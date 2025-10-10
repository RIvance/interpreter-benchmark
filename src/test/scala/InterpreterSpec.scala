import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFlatSpec with Matchers {

  // Helper method to test both interpreters with the same term
  def testBothInterpreters(description: String, term: Term, expected: Value): Unit = {
    s"DirectInterpreter $description" should "produce the correct result" in {
      val result = DirectInterpreter.eval(term)(using Map.empty)
      result shouldEqual expected
    }

    s"TrampolineInterpreter $description" should "produce the correct result" in {
      val result = TrampolineInterpreter.eval(term)(using Map.empty)
      result shouldEqual expected
    }
  }

  // Test integer literals
  testBothInterpreters("evaluating integer literal", 
    Term.IntLit(42),
    Value.IntVal(42)
  )

  // Test boolean literals
  testBothInterpreters("evaluating boolean literal true",
    Term.BoolLit(true),
    Value.BoolVal(true)
  )

  testBothInterpreters("evaluating boolean literal false",
    Term.BoolLit(false),
    Value.BoolVal(false)
  )

  // Test integer addition
  testBothInterpreters("evaluating addition",
    Term.BinOpInt(IntOpKind.Add, Term.IntLit(10), Term.IntLit(32)),
    Value.IntVal(42)
  )

  // Test integer subtraction
  testBothInterpreters("evaluating subtraction",
    Term.BinOpInt(IntOpKind.Sub, Term.IntLit(50), Term.IntLit(8)),
    Value.IntVal(42)
  )

  // Test integer multiplication
  testBothInterpreters("evaluating multiplication",
    Term.BinOpInt(IntOpKind.Mul, Term.IntLit(6), Term.IntLit(7)),
    Value.IntVal(42)
  )

  // Test comparison equal
  testBothInterpreters("evaluating equality comparison (true)",
    Term.BinOpCmp(CmpOpKind.Eq, Term.IntLit(42), Term.IntLit(42)),
    Value.BoolVal(true)
  )

  testBothInterpreters("evaluating equality comparison (false)",
    Term.BinOpCmp(CmpOpKind.Eq, Term.IntLit(42), Term.IntLit(43)),
    Value.BoolVal(false)
  )

  // Test comparison less than
  testBothInterpreters("evaluating less than (true)",
    Term.BinOpCmp(CmpOpKind.Lt, Term.IntLit(10), Term.IntLit(20)),
    Value.BoolVal(true)
  )

  testBothInterpreters("evaluating less than (false)",
    Term.BinOpCmp(CmpOpKind.Lt, Term.IntLit(20), Term.IntLit(10)),
    Value.BoolVal(false)
  )

  // Test comparison greater than
  testBothInterpreters("evaluating greater than (true)",
    Term.BinOpCmp(CmpOpKind.Gt, Term.IntLit(20), Term.IntLit(10)),
    Value.BoolVal(true)
  )

  testBothInterpreters("evaluating greater than (false)",
    Term.BinOpCmp(CmpOpKind.Gt, Term.IntLit(10), Term.IntLit(20)),
    Value.BoolVal(false)
  )

  // Test boolean equality
  testBothInterpreters("evaluating boolean equality (true)",
    Term.BinOpCmp(CmpOpKind.Eq, Term.BoolLit(true), Term.BoolLit(true)),
    Value.BoolVal(true)
  )

  testBothInterpreters("evaluating boolean equality (false)",
    Term.BinOpCmp(CmpOpKind.Eq, Term.BoolLit(true), Term.BoolLit(false)),
    Value.BoolVal(false)
  )

  // Test if expression - then branch
  testBothInterpreters("evaluating if expression (then branch)",
    Term.If(Term.BoolLit(true), Term.IntLit(42), Term.IntLit(0)),
    Value.IntVal(42)
  )

  // Test if expression - else branch
  testBothInterpreters("evaluating if expression (else branch)",
    Term.If(Term.BoolLit(false), Term.IntLit(0), Term.IntLit(42)),
    Value.IntVal(42)
  )

  // Test if with condition evaluation
  testBothInterpreters("evaluating if with computed condition",
    Term.If(
      Term.BinOpCmp(CmpOpKind.Lt, Term.IntLit(5), Term.IntLit(10)),
      Term.IntLit(100),
      Term.IntLit(200)
    ),
    Value.IntVal(100)
  )

  // Test lambda and application - identity function
  testBothInterpreters("evaluating identity function", {
    // λx:Int. x applied to 42
    val identityFn = Term.Lam(Type.IntType, Term.Var(0))
    Term.App(identityFn, Term.IntLit(42))
  },
    Value.IntVal(42)
  )

  // Test lambda with computation in body
  testBothInterpreters("evaluating lambda with computation", {
    // (λx:Int. x + 10) 32
    val addTenFn = Term.Lam(
      Type.IntType,
      Term.BinOpInt(IntOpKind.Add, Term.Var(0), Term.IntLit(10))
    )
    Term.App(addTenFn, Term.IntLit(32))
  },
    Value.IntVal(42)
  )

  // Test nested lambdas (curried function)
  testBothInterpreters("evaluating curried function", {
    // (λx:Int. λy:Int. x + y) 30 12
    val addFn = Term.Lam(
      Type.IntType,
      Term.Lam(
        Type.IntType,
        Term.BinOpInt(IntOpKind.Add, Term.Var(1), Term.Var(0))
      )
    )
    val partial = Term.App(addFn, Term.IntLit(30))
    Term.App(partial, Term.IntLit(12))
  },
    Value.IntVal(42)
  )

  // Test closure captures environment
  testBothInterpreters("evaluating closure with captured variable", {
    // (λx:Int. (λy:Int. x * y) 7) 6
    val fn = Term.Lam(
      Type.IntType,
      Term.App(
        Term.Lam(
          Type.IntType,
          Term.BinOpInt(IntOpKind.Mul, Term.Var(1), Term.Var(0))
        ),
        Term.IntLit(7)
      )
    )
    Term.App(fn, Term.IntLit(6))
  },
    Value.IntVal(42)
  )

  // Test fixpoint - factorial function
  testBothInterpreters("evaluating factorial using fixpoint", {
    // fix f:(Int->Int). λn:Int. if (n == 0) then 1 else n * f(n-1)
    // Applied to 5
    val factorialBody = Term.Lam(
      Type.IntType,
      Term.If(
        Term.BinOpCmp(CmpOpKind.Eq, Term.Var(0), Term.IntLit(0)),
        Term.IntLit(1),
        Term.BinOpInt(
          IntOpKind.Mul,
          Term.Var(0),
          Term.App(
            Term.Var(1), // reference to the fixpoint
            Term.BinOpInt(IntOpKind.Sub, Term.Var(0), Term.IntLit(1))
          )
        )
      )
    )
    val factorial = Term.Fix(Type.Arrow(Type.IntType, Type.IntType), factorialBody)
    Term.App(factorial, Term.IntLit(5))
  },
    Value.IntVal(120) // 5! = 120
  )

  // Test fixpoint - sum from 1 to n
  testBothInterpreters("evaluating sum using fixpoint", {
    // fix f:(Int->Int). λn:Int. if (n == 0) then 0 else n + f(n-1)
    // Applied to 10
    val sumBody = Term.Lam(
      Type.IntType,
      Term.If(
        Term.BinOpCmp(CmpOpKind.Eq, Term.Var(0), Term.IntLit(0)),
        Term.IntLit(0),
        Term.BinOpInt(
          IntOpKind.Add,
          Term.Var(0),
          Term.App(
            Term.Var(1),
            Term.BinOpInt(IntOpKind.Sub, Term.Var(0), Term.IntLit(1))
          )
        )
      )
    )
    val sum = Term.Fix(Type.Arrow(Type.IntType, Type.IntType), sumBody)
    Term.App(sum, Term.IntLit(10))
  },
    Value.IntVal(55) // 1+2+3+...+10 = 55
  )

  // Test fixpoint - fibonacci function
  testBothInterpreters("evaluating fibonacci using fixpoint", {
    // fix f:(Int->Int). λn:Int. if (n < 2) then n else f(n-1) + f(n-2)
    // Applied to 10
    val fibonacciBody = Term.Lam(
      Type.IntType,
      Term.If(
        Term.BinOpCmp(CmpOpKind.Lt, Term.Var(0), Term.IntLit(2)),
        Term.Var(0),
        Term.BinOpInt(
          IntOpKind.Add,
          Term.App(
            Term.Var(1), // reference to the fixpoint
            Term.BinOpInt(IntOpKind.Sub, Term.Var(0), Term.IntLit(1))
          ),
          Term.App(
            Term.Var(1), // reference to the fixpoint
            Term.BinOpInt(IntOpKind.Sub, Term.Var(0), Term.IntLit(2))
          )
        )
      )
    )
    val fibonacci = Term.Fix(Type.Arrow(Type.IntType, Type.IntType), fibonacciBody)
    Term.App(fibonacci, Term.IntLit(10))
  },
    Value.IntVal(55) // fib(10) = 55
  )

  // Test complex expression
  testBothInterpreters("evaluating complex nested expression", {
    // ((λx:Int. x * 2) 10) + ((λy:Int. y - 8) 20)
    val leftPart = Term.App(
      Term.Lam(Type.IntType, Term.BinOpInt(IntOpKind.Mul, Term.Var(0), Term.IntLit(2))),
      Term.IntLit(10)
    )
    val rightPart = Term.App(
      Term.Lam(Type.IntType, Term.BinOpInt(IntOpKind.Sub, Term.Var(0), Term.IntLit(8))),
      Term.IntLit(20)
    )
    Term.BinOpInt(IntOpKind.Add, leftPart, rightPart)
  },
    Value.IntVal(32) // (10*2) + (20-8) = 20 + 12 = 32
  )

  // Test error cases
  "DirectInterpreter" should "throw error when applying non-closure" in {
    val term = Term.App(Term.IntLit(42), Term.IntLit(10))
    an[RuntimeException] should be thrownBy {
      DirectInterpreter.eval(term)(using Map.empty)
    }
  }

  "TrampolineInterpreter" should "throw error when applying non-closure" in {
    val term = Term.App(Term.IntLit(42), Term.IntLit(10))
    an[RuntimeException] should be thrownBy {
      TrampolineInterpreter.eval(term)(using Map.empty)
    }
  }

  "DirectInterpreter" should "throw error for integer operation on non-integers" in {
    val term = Term.BinOpInt(IntOpKind.Add, Term.BoolLit(true), Term.IntLit(10))
    an[RuntimeException] should be thrownBy {
      DirectInterpreter.eval(term)(using Map.empty)
    }
  }

  "TrampolineInterpreter" should "throw error for integer operation on non-integers" in {
    val term = Term.BinOpInt(IntOpKind.Add, Term.BoolLit(true), Term.IntLit(10))
    an[RuntimeException] should be thrownBy {
      TrampolineInterpreter.eval(term)(using Map.empty)
    }
  }

  "DirectInterpreter" should "throw error for if with non-boolean condition" in {
    val term = Term.If(Term.IntLit(42), Term.IntLit(1), Term.IntLit(2))
    an[RuntimeException] should be thrownBy {
      DirectInterpreter.eval(term)(using Map.empty)
    }
  }

  "TrampolineInterpreter" should "throw error for if with non-boolean condition" in {
    val term = Term.If(Term.IntLit(42), Term.IntLit(1), Term.IntLit(2))
    an[RuntimeException] should be thrownBy {
      TrampolineInterpreter.eval(term)(using Map.empty)
    }
  }

  // Test record functionality
  testBothInterpreters("evaluating simple record literal",
    Term.Record(Map("x" -> Term.IntLit(42), "y" -> Term.IntLit(100))),
    Value.RecordVal(Map("x" -> Value.IntVal(42), "y" -> Value.IntVal(100)))
  )

  testBothInterpreters("evaluating record field selection",
    Term.Proj(
      Term.Record(Map("x" -> Term.IntLit(42), "y" -> Term.IntLit(100))),
      "x"
    ),
    Value.IntVal(42)
  )

  testBothInterpreters("evaluating record with computed fields",
    Term.Record(Map(
      "sum" -> Term.BinOpInt(IntOpKind.Add, Term.IntLit(1), Term.IntLit(2)),
      "product" -> Term.BinOpInt(IntOpKind.Mul, Term.IntLit(3), Term.IntLit(4))
    )),
    Value.RecordVal(Map("sum" -> Value.IntVal(3), "product" -> Value.IntVal(12)))
  )

  testBothInterpreters("evaluating record with boolean fields",
    Term.Record(Map(
      "flag" -> Term.BoolLit(true),
      "comparison" -> Term.BinOpCmp(CmpOpKind.Lt, Term.IntLit(5), Term.IntLit(10))
    )),
    Value.RecordVal(Map("flag" -> Value.BoolVal(true), "comparison" -> Value.BoolVal(true)))
  )

  testBothInterpreters("evaluating computation with record field selection",
    Term.BinOpInt(
      IntOpKind.Add,
      Term.Proj(Term.Record(Map("x" -> Term.IntLit(10), "y" -> Term.IntLit(20))), "x"),
      Term.Proj(Term.Record(Map("x" -> Term.IntLit(10), "y" -> Term.IntLit(20))), "y")
    ),
    Value.IntVal(30)
  )

  testBothInterpreters("evaluating nested record",
    Term.Record(Map(
      "point" -> Term.Record(Map("x" -> Term.IntLit(5), "y" -> Term.IntLit(10))),
      "value" -> Term.IntLit(42)
    )),
    Value.RecordVal(Map(
      "point" -> Value.RecordVal(Map("x" -> Value.IntVal(5), "y" -> Value.IntVal(10))),
      "value" -> Value.IntVal(42)
    ))
  )

  testBothInterpreters("evaluating nested record field selection",
    Term.Proj(
      Term.Proj(
        Term.Record(Map(
          "point" -> Term.Record(Map("x" -> Term.IntLit(5), "y" -> Term.IntLit(10))),
          "value" -> Term.IntLit(42)
        )),
        "point"
      ),
      "x"
    ),
    Value.IntVal(5)
  )

  "DirectInterpreter" should "throw error when selecting non-existent field" in {
    val term = Term.Proj(Term.Record(Map("x" -> Term.IntLit(42))), "y")
    an[RuntimeException] should be thrownBy {
      DirectInterpreter.eval(term)(using Map.empty)
    }
  }

  "TrampolineInterpreter" should "throw error when selecting non-existent field" in {
    val term = Term.Proj(Term.Record(Map("x" -> Term.IntLit(42))), "y")
    an[RuntimeException] should be thrownBy {
      TrampolineInterpreter.eval(term)(using Map.empty)
    }
  }

  "DirectInterpreter" should "throw error when selecting field from non-record" in {
    val term = Term.Proj(Term.IntLit(42), "x")
    an[RuntimeException] should be thrownBy {
      DirectInterpreter.eval(term)(using Map.empty)
    }
  }

  "TrampolineInterpreter" should "throw error when selecting field from non-record" in {
    val term = Term.Proj(Term.IntLit(42), "x")
    an[RuntimeException] should be thrownBy {
      TrampolineInterpreter.eval(term)(using Map.empty)
    }
  }
}
