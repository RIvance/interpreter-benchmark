object DirectInterpreter extends Interpreter {
  override def eval(term: Term)(using env: Env): Value = term match {
    
    case Term.Var(index) => summon[Env].apply(index)

    case Term.Lam(parameterType, body) =>
      val captured = summon[Env]
      val bodyCopy = body
      Value.Closure { arg =>
        val newEnv = captured.extend(arg)
        eval(bodyCopy)(using newEnv)
      }

    case Term.App(leftTerm, rightTerm) =>
      val leftValue = eval(leftTerm)
      val rightValue = eval(rightTerm)
      leftValue match {
        case Value.Closure(fn) => fn(rightValue)
        case _ => throw new RuntimeException("Runtime type error: expected closure")
      }

    case Term.IntLit(n) => Value.IntVal(n)
    case Term.BoolLit(b) => Value.BoolVal(b)

    case Term.BinOpInt(kind, leftTerm, rightTerm) =>
      val leftValue = eval(leftTerm)
      val rightValue = eval(rightTerm)
      (leftValue, rightValue) match {
        case (Value.IntVal(l), Value.IntVal(r)) =>
          kind match {
            case IntOpKind.Add => Value.IntVal(l + r)
            case IntOpKind.Sub => Value.IntVal(l - r)
            case IntOpKind.Mul => Value.IntVal(l * r)
          }
        case _ => throw new RuntimeException("Integer operation on non-integers")
      }

    case Term.BinOpCmp(kind, leftTerm, rightTerm) =>
      val leftValue = eval(leftTerm)
      val rightValue = eval(rightTerm)
      (leftValue, rightValue) match {
        case (Value.IntVal(l), Value.IntVal(r)) =>
          kind match {
            case CmpOpKind.Eq => Value.BoolVal(l == r)
            case CmpOpKind.Lt => Value.BoolVal(l < r)
            case CmpOpKind.Gt => Value.BoolVal(l > r)
          }
        case (Value.BoolVal(l), Value.BoolVal(r)) if kind == CmpOpKind.Eq =>
          Value.BoolVal(l == r)
        case _ => throw new RuntimeException("Comparison on incompatible types")
      }

    case Term.If(cond, thenBranch, elseBranch) =>
      eval(cond) match {
        case Value.BoolVal(true) => eval(thenBranch)
        case Value.BoolVal(false) => eval(elseBranch)
        case _ => throw new RuntimeException("If condition must be boolean")
      }

    case Term.Fix(annotatedType, body) =>
      lazy val updatedEnv: Env = {
        val shifted = summon[Env].iterator.map { case (i, v) => (i + 1) -> v }.toMap
        shifted + (0 -> selfRef)
      }
      lazy val result: Value = eval(body)(using updatedEnv)
      lazy val selfRef: Value = Value.Closure { arg =>
        result match {
          case Value.Closure(fn) => fn(arg)
          case _ => throw new RuntimeException("Fixpoint should be a closure")
        }
      }
      result

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
