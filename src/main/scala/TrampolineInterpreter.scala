import scala.util.control.TailCalls.*

object TrampolineInterpreter extends Interpreter {

  override def eval(term: Term)(using env: Env): Value = {
    val trampEnv = env.map { case (k, v) => k -> valueToTramp(v) }
    evalTramp(term)(using trampEnv).result.toValue
  }

  private enum TrampValue {
    case IntVal(n: Long)
    case BoolVal(b: Boolean)
    case Closure(env: Map[Int, TrampValue], body: Term)
    case RecordVal(fields: Map[String, TrampValue])
    case FixThunk(annotatedType: Type, body: Term, env: Map[Int, TrampValue] = Map.empty)

    def toValue: Value = this match {
      case IntVal(n) => Value.IntVal(n)
      case BoolVal(b) => Value.BoolVal(b)
      case Closure(env, body) => Value.Closure { arg =>
        val newEnv = env + (0 -> valueToTramp(arg))
        evalTramp(body)(using newEnv).result.toValue
      }
      case RecordVal(fields) => Value.RecordVal(fields.view.mapValues(_.toValue).toMap)
      case FixThunk(_, _, _) => throw new RuntimeException("FixThunk should not appear as final value")
    }
  }

  private def valueToTramp(v: Value): TrampValue = v match {
    case Value.IntVal(n) => TrampValue.IntVal(n)
    case Value.BoolVal(b) => TrampValue.BoolVal(b)
    case Value.Closure(_) => throw new RuntimeException("Cannot convert HOAS closure to TrampValue")
    case Value.RecordVal(fields) => TrampValue.RecordVal(fields.view.mapValues(valueToTramp).toMap)
  }

  private def evalTramp(term: Term)(using env: Map[Int, TrampValue]): TailRec[TrampValue] = term match {

    case Term.Var(index) =>
      env(index) match {
        case fix @ TrampValue.FixThunk(annotatedType, body, captured) =>
          // When a FixThunk is looked up, evaluate its body in the captured environment
          //  and since we cannot have self-reference when building the captured env,
          //  we now put a simple thunk at index 0 that will evaluate to the fixpoint
          tailcall(evalTramp(body)(using captured + (0 -> fix)))
        case value => done(value)
      }

    case Term.Lam(parameterType, body) =>
      done(TrampValue.Closure(env, body))

    case Term.App(leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
        result <- leftValue match {
          case TrampValue.Closure(closureEnv, body) =>
            val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
            tailcall(evalTramp(body)(using newEnv))
          case other => throw new RuntimeException(s"Runtime type error: expected closure, got $other")
        }
      } yield result

    case Term.IntLit(n) => done(TrampValue.IntVal(n))
    case Term.BoolLit(b) => done(TrampValue.BoolVal(b))

    case Term.BinOpInt(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
      } yield (leftValue, rightValue) match {
        case (TrampValue.IntVal(l), TrampValue.IntVal(r)) =>
          kind match {
            case IntOpKind.Add => TrampValue.IntVal(l + r)
            case IntOpKind.Sub => TrampValue.IntVal(l - r)
            case IntOpKind.Mul => TrampValue.IntVal(l * r)
          }
        case _ => throw new RuntimeException("Integer operation on non-integers")
      }

    case Term.BinOpCmp(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- evalTramp(leftTerm)
        rightValue <- evalTramp(rightTerm)
      } yield (leftValue, rightValue) match {
        case (TrampValue.IntVal(l), TrampValue.IntVal(r)) =>
          kind match {
            case CmpOpKind.Eq => TrampValue.BoolVal(l == r)
            case CmpOpKind.Lt => TrampValue.BoolVal(l < r)
            case CmpOpKind.Gt => TrampValue.BoolVal(l > r)
          }
        case (TrampValue.BoolVal(l), TrampValue.BoolVal(r)) if kind == CmpOpKind.Eq =>
          TrampValue.BoolVal(l == r)
        case _ => throw new RuntimeException("Comparison on incompatible types")
      }

    case Term.If(cond, thenBranch, elseBranch) =>
      for {
        condValue <- evalTramp(cond)
        result <- condValue match {
          case TrampValue.BoolVal(true) => tailcall(evalTramp(thenBranch))
          case TrampValue.BoolVal(false) => tailcall(evalTramp(elseBranch))
          case _ => throw new RuntimeException("If condition must be boolean")
        }
      } yield result

    case Term.Fix(annotatedType, body) =>
      // Y-combinator approach: put a simple thunk at index 0
      // When it's looked up later, it will be evaluated in the environment where the lookup happens
      lazy val newEnv = env.map { case (i, v) => (i + 1) -> v } + (0 -> fixThunk)
      lazy val fixThunk = TrampValue.FixThunk(annotatedType, body, env.map { case (i, v) => (i + 1) -> v })
      tailcall(evalTramp(body)(using newEnv))

    case Term.Record(fields) =>
      val fieldsList = fields.toList
      def evalFields(remaining: List[(String, Term)], acc: Map[String, TrampValue]): TailRec[TrampValue] = {
        remaining match {
          case Nil => done(TrampValue.RecordVal(acc))
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
        case TrampValue.RecordVal(fields) =>
          fields.getOrElse(field, throw new RuntimeException(s"Field '$field' not found in record"))
        case _ => throw new RuntimeException("Select operation on non-record value")
      }
  }
}