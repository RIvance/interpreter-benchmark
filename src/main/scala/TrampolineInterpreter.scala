object TrampolineInterpreter extends Interpreter {

  override def eval(term: Term)(using env: Env): Value = evalTramp(term).run

  private def evalTramp(term: Term)(using env: Env): Trampoline[Value] = term match {
    case Term.Var(index) =>
      done(summon[Env].apply(index))

    case Term.Lam(parameterType, body) =>
      // capture the current env inside the function
      val captured = summon[Env]
      val bodyCopy = body
      val closure = Value.Closure { arg =>
        val newEnv = captured.extend(arg)
        evalTramp(bodyCopy)(using newEnv).run
      }
      done(closure)

    case Term.App(leftTerm, rightTerm) =>
      val leftEval = evalTramp(leftTerm)
      val rightEval = evalTramp(rightTerm)
      leftEval.flatMap { leftValue =>
        rightEval.map { rightValue =>
          leftValue match {
            case Value.Closure(fn) => fn(rightValue)
            case _ => throw new RuntimeException("Runtime type error: expected closure")
          }
        }
      }

    case Term.IntLit(n) => done(Value.IntVal(n))
    case Term.BoolLit(b) => done(Value.BoolVal(b))

    case Term.BinOpInt(kind, leftTerm, rightTerm) =>
      val leftEval = evalTramp(leftTerm)
      val rightEval = evalTramp(rightTerm)
      leftEval.flatMap { leftValue =>
        rightEval.map { rightValue =>
          (leftValue, rightValue) match {
            case (Value.IntVal(l), Value.IntVal(r)) =>
              kind match {
                case IntOpKind.Add => Value.IntVal(l + r)
                case IntOpKind.Sub => Value.IntVal(l - r)
                case IntOpKind.Mul => Value.IntVal(l * r)
              }
            case _ => throw new RuntimeException("Integer operation on non-integers")
          }
        }
      }

    case Term.BinOpCmp(kind, leftTerm, rightTerm) =>
      val leftEval = evalTramp(leftTerm)
      val rightEval = evalTramp(rightTerm)
      leftEval.flatMap { leftValue =>
        rightEval.map { rightValue =>
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
        }
      }

    case Term.If(cond, thenBranch, elseBranch) =>
      val condEval = evalTramp(cond)
      condEval.flatMap {
        case Value.BoolVal(true) => evalTramp(thenBranch)
        case Value.BoolVal(false) => evalTramp(elseBranch)
        case _ => throw new RuntimeException("If condition must be boolean")
      }

    case Term.Fix(annotatedType, body) =>
      delay {
        val base = summon[Env]
        // We'll construct updatedEnv lazily because selfRef depends on result, and result depends on updatedEnv.
        lazy val updatedEnv: Env = {
          val shifted = base.iterator.map { case (i, v) => (i + 1) -> v }.toMap
          shifted + (0 -> selfRef)
        }
        lazy val result: Value = evalTramp(body)(using updatedEnv).run
        lazy val selfRef: Value = Value.Closure { arg =>
          result match {
            case Value.Closure(fn) => fn(arg)
            case _ => throw new RuntimeException("Fixpoint should be a closure")
          }
        }
        done(result)
      }

    case Term.Record(fields) =>
      // Evaluate all fields in sequence using trampolined evaluation
      val fieldsList = fields.toList
      def evalFields(remaining: List[(String, Term)], acc: Map[String, Value]): Trampoline[Value] = {
        remaining match {
          case Nil => done(Value.RecordVal(acc))
          case (name, term) :: rest =>
            evalTramp(term).flatMap { value =>
              delay(evalFields(rest, acc + (name -> value)))
            }
        }
      }
      delay(evalFields(fieldsList, Map.empty))

    case Term.Proj(record, field) =>
      evalTramp(record).map {
        case Value.RecordVal(fields) =>
          fields.getOrElse(field, throw new RuntimeException(s"Field '$field' not found in record"))
        case _ => throw new RuntimeException("Select operation on non-record value")
      }
  }
}
