import scala.util.control.TailCalls.*

object TrampolineInterpreter extends Interpreter {

  override def name: String = "Trampoline Interpreter"
  
  override def eval(term: Term)(using env: Env): Value = {
    evalTramp(term)(using env).result
  }

  private def evalTramp(term: Term)(using env: Map[Int, Value]): TailRec[Value] = term match {

    case Term.Var(index) => done(env(index))

    case Term.Lam(parameterType, body) => done(Value.Closure(env, body))

    case Term.App(leftTerm, rightTerm) => for {
      leftValue <- evalTramp(leftTerm)
      rightValue <- evalTramp(rightTerm)
      result <- leftValue match {
        case Value.Closure(closureEnv, body) =>
          val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
          tailcall(evalTramp(body)(using newEnv))
        case fix @ Value.FixThunk(annotatedType, body, captured) =>
          // Unfold the fixpoint and apply it to the argument
          val unfoldedEnv = captured + (0 -> fix)
          for {
            unfoldedValue <- tailcall(evalTramp(body)(using unfoldedEnv))
            finalResult <- unfoldedValue match {
              case Value.Closure(closureEnv, closureBody) =>
                val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
                tailcall(evalTramp(closureBody)(using newEnv))
              case _ => throw new RuntimeException("Runtime type error: fixpoint did not produce a function")
            }
          } yield finalResult
        case other => throw new RuntimeException(s"Runtime type error: expected closure, got $other")
      }
    } yield result

    case Term.IntLit(n) => done(Value.IntVal(n))
    case Term.BoolLit(b) => done(Value.BoolVal(b))

    case Term.BinOp(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
      } yield (kind, leftValue, rightValue) match {
        case (OpKind.Add, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l + r)
        case (OpKind.Sub, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l - r)
        case (OpKind.Mul, Value.IntVal(l), Value.IntVal(r)) => Value.IntVal(l * r)
        case (OpKind.Eq, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l == r)
        case (OpKind.Lt, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l < r)
        case (OpKind.Gt, Value.IntVal(l), Value.IntVal(r)) => Value.BoolVal(l > r)
        case (OpKind.Eq, Value.BoolVal(l), Value.BoolVal(r)) => Value.BoolVal(l == r)
        case _ => throw new RuntimeException("Binary operation on incompatible types")
      }

    case Term.If(cond, thenBranch, elseBranch) => for {
      condValue <- evalTramp(cond)
      result <- condValue match {
        case Value.BoolVal(true) => tailcall(evalTramp(thenBranch))
        case Value.BoolVal(false) => tailcall(evalTramp(elseBranch))
        case _ => throw new RuntimeException("If condition must be boolean")
      }
    } yield result

    case Term.Fix(annotatedType, body) =>
      // Return a FixThunk value - it only unfolds when applied to an argument
      done(Value.FixThunk(annotatedType, body, env))

    case Term.Record(fields) => {
      def evalFields(remaining: List[(String, Term)], acc: Map[String, Value]): TailRec[Value] = {
        remaining match {
          case Nil => done(Value.RecordVal(acc))
          case (name, term) :: rest => for {
            value <- evalTramp(term)
            result <- tailcall(evalFields(rest, acc + (name -> value)))
          } yield result
        }
      }
      tailcall(evalFields(fields.toList, Map.empty))
    }

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