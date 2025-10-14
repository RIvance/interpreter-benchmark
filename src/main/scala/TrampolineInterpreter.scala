import scala.util.control.TailCalls.*

object TrampolineInterpreter extends Interpreter {

  override def eval(term: Term)(using env: Env): Value = {
    evalTramp(term)(using env).result
  }

  private def evalTramp(term: Term)(using env: Map[Int, Value]): TailRec[Value] = term match {

    case Term.Var(index) =>
      env(index) match {
        case fix @ Value.FixThunk(annotatedType, body, captured) =>
          // When a FixThunk is looked up, evaluate its body in the captured environment
          //  and since we cannot have self-reference when building the captured env,
          //  we now put a simple thunk at index 0 that will evaluate to the fixpoint
          tailcall(evalTramp(body)(using captured + (0 -> fix)))
        case value => done(value)
      }

    case Term.Lam(parameterType, body) =>
      done(Value.Closure(env, body))

    case Term.App(leftTerm, rightTerm) => for {
      leftValue <- evalTramp(leftTerm)
      rightValue <- evalTramp(rightTerm)
      result <- leftValue match {
        case Value.Closure(closureEnv, body) =>
          val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
          tailcall(evalTramp(body)(using newEnv))
        case other => throw new RuntimeException(s"Runtime type error: expected closure, got $other")
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

    case Term.If(cond, thenBranch, elseBranch) => for {
      condValue <- evalTramp(cond)
      result <- condValue match {
        case Value.BoolVal(true) => tailcall(evalTramp(thenBranch))
        case Value.BoolVal(false) => tailcall(evalTramp(elseBranch))
        case _ => throw new RuntimeException("If condition must be boolean")
      }
    } yield result

    case Term.Fix(annotatedType, body) =>
      // Y-combinator approach: put a simple thunk at index 0
      // When it's looked up later, it will be evaluated in the environment where the lookup happens
      lazy val newEnv = env.map { case (i, v) => (i + 1) -> v } + (0 -> fixThunk)
      lazy val fixThunk = Value.FixThunk(annotatedType, body, env.map { case (i, v) => (i + 1) -> v })
      tailcall(evalTramp(body)(using newEnv))

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