object RecursiveInterpreter extends Interpreter {

  override def name: String = "Recursive Big-Step Interpreter"
  
  override def eval(term: Term)(using env: Env): Value = term match {
    
    case Term.Var(index) => env.apply(index)

    case Term.Lam(parameterType, body) => Value.Closure(env, body)

    case Term.App(leftTerm, rightTerm) =>
      val leftValue = eval(leftTerm)
      val rightValue = eval(rightTerm)
      leftValue match {
        case Value.Closure(closureEnv, body) =>
          val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
          eval(body)(using newEnv)
        case fix @ Value.FixThunk(annotatedType, body, captured) =>
          // Unfold the fixpoint and apply it to the argument
          val unfoldedEnv = captured + (0 -> fix)
          val unfoldedValue = eval(body)(using unfoldedEnv)
          // Now apply the unfolded value to the argument
          unfoldedValue match {
            case Value.Closure(closureEnv, closureBody) =>
              val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
              eval(closureBody)(using newEnv)
            case _ => throw new RuntimeException("Runtime type error: fixpoint did not produce a function")
          }
        case _ => throw new RuntimeException("Runtime type error: expected closure")
      }

    case Term.IntLit(n) => Value.IntVal(n)
    case Term.BoolLit(b) => Value.BoolVal(b)

    case Term.BinOp(kind, leftTerm, rightTerm) =>
      val leftValue = eval(leftTerm)
      val rightValue = eval(rightTerm)
      (kind, leftValue, rightValue) match {
        case (OpKind.Add, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l + r)
        case (OpKind.Sub, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l - r)
        case (OpKind.Mul, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l * r)
        case (OpKind.Eq, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l == r)
        case (OpKind.Lt, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l < r)
        case (OpKind.Gt, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l > r)
        case (OpKind.Eq, Value.BoolVal(l), Value.BoolVal(r)) => Value.BoolVal(l == r)
        case _ => throw new RuntimeException("Binary operation on incompatible types")
      }

    case Term.If(cond, thenBranch, elseBranch) =>
      eval(cond) match {
        case Value.BoolVal(true) => eval(thenBranch)
        case Value.BoolVal(false) => eval(elseBranch)
        case _ => throw new RuntimeException("If condition must be boolean")
      }

    case Term.Fix(annotatedType, body) =>
      // Return a FixThunk value - it only unfolds when applied to an argument
      Value.FixThunk(annotatedType, body, env)

    case Term.Record(fields) =>
      val evaluatedFields = fields.map { case (name, term) => (name, eval(term)) }
      Value.RecordVal(evaluatedFields)

    case Term.Proj(record, field) =>
      eval(record) match {
        case Value.RecordVal(fields) =>
          fields.getOrElse(field, throw new RuntimeException(s"Field '$field' not found in record"))
        case _ => throw new RuntimeException("Select operation on non-record value")
      }
  }
}
