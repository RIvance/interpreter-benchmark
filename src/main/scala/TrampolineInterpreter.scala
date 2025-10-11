import scala.util.control.TailCalls.*

object TrampolineInterpreter extends Interpreter {

  override def eval(term: Term)(using env: Env): Value = evalTramp(term).result

  private def evalTramp(term: Term)(using env: Env): TailRec[Value] = term match {

    case Term.Var(index) => done(summon[Env].apply(index))

    case Term.Lam(parameterType, body) =>
      // capture the current env inside the function
      val captured = summon[Env]
      val bodyCopy = body
      val closure = Value.Closure { arg =>
        val newEnv = captured.extend(arg)
        // TODO: Is this still stack unsafe?
        evalTramp(bodyCopy)(using newEnv).result
      }
      done(closure)

    case Term.App(leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
        result <- leftValue match {
          case Value.Closure(fn) => done(fn(rightValue))
          case _ => throw new RuntimeException("Runtime type error: expected closure")
        }
      } yield result

    case Term.IntLit(n) => done(Value.IntVal(n))
    case Term.BoolLit(b) => done(Value.BoolVal(b))

    case Term.BinOpInt(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
      } yield (leftValue, rightValue) match {
        case (Value.IntVal(l), Value.IntVal(r)) =>
          kind match {
            case IntOpKind.Add => Value.IntVal(l + r)
            case IntOpKind.Sub => Value.IntVal(l - r)
            case IntOpKind.Mul => Value.IntVal(l * r)
          }
        case _ => throw new RuntimeException("Integer operation on non-integers")
      }

    case Term.BinOpCmp(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
      } yield (leftValue, rightValue) match {
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
      for {
        condValue <- evalTramp(cond)
        result <- condValue match {
          case Value.BoolVal(true) => tailcall(evalTramp(thenBranch))
          case Value.BoolVal(false) => tailcall(evalTramp(elseBranch))
          case _ => throw new RuntimeException("If condition must be boolean")
        }
      } yield result

    case Term.Fix(annotatedType, body) =>
      val base = summon[Env]
      // We'll construct updatedEnv lazily because selfRef depends on result, and result depends on updatedEnv.
      lazy val updatedEnv: Env = {
        val shifted = base.iterator.map { case (i, v) => (i + 1) -> v }.toMap
        shifted + (0 -> selfRef)
      }
      lazy val selfRef: Value = Value.Closure { arg =>
        evalTramp(body)(using updatedEnv).result match {
          case Value.Closure(fn) => fn(arg)
          case _ => throw new RuntimeException("Fixpoint should be a closure")
        }
      }
      // Delay the evaluation of the body with the updated environment
      tailcall(evalTramp(body)(using updatedEnv))

    case Term.Record(fields) =>
      // Evaluate all fields in sequence using trampolined evaluation
      val fieldsList = fields.toList
      def evalFields(remaining: List[(String, Term)], acc: Map[String, Value]): TailRec[Value] = {
        remaining match {
          case Nil => done(Value.RecordVal(acc))
          case (name, term) :: rest =>
            for {
              value <- evalTramp(term)
              result <- tailcall(evalFields(rest, acc + (name -> value)))
            } yield result
        }
      }
      tailcall(evalFields(fieldsList, Map.empty))

    case Term.Proj(record, field) =>
      for {
        recordValue <- evalTramp(record)
      } yield recordValue match {
        case Value.RecordVal(fields) =>
          fields.getOrElse(field, throw new RuntimeException(s"Field '$field' not found in record"))
        case _ => throw new RuntimeException("Select operation on non-record value")
      }
  }
}
