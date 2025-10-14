import scala.annotation.tailrec

object WorkListInterpreter extends Interpreter {

  override def eval(term: Term)(using env: Env): Value = {
    evalWorkList(term)(using env).run
  }

  private def evalWorkList(term: Term)(using env: Env): WorkList[Value] = term match {
    case Term.Var(index) => env(index) match {
      case fix @ Value.FixThunk(annotatedType, body, captured) =>
        WorkList.Eval(body, captured + (0 -> fix))
      case value =>
        WorkList.Return(value)
    }

    case Term.Lam(parameterType, body) => WorkList.Return(Value.Closure(env, body))

    case Term.App(leftTerm, rightTerm) => for {
      leftValue <- WorkList.Eval(leftTerm, env)
      rightValue <- WorkList.Eval(rightTerm, env)
      result <- leftValue match {
        case Value.Closure(closureEnv, body) =>
          val newEnv = closureEnv.map { case (i, v) => (i + 1) -> v } + (0 -> rightValue)
          WorkList.Eval(body, newEnv)
        case _ =>
          throw new RuntimeException("Runtime type error: expected closure")
      }
    } yield result

    case Term.IntLit(n) => WorkList.Return(Value.IntVal(n))
    case Term.BoolLit(b) => WorkList.Return(Value.BoolVal(b))

    case Term.BinOpInt(kind, leftTerm, rightTerm) =>
      for {
        leftValue <- WorkList.Eval(leftTerm, env)
        rightValue <- WorkList.Eval(rightTerm, env)
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
        leftValue <- WorkList.Eval(leftTerm, env)
        rightValue <- WorkList.Eval(rightTerm, env)
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
      condValue <- WorkList.Eval(cond, env)
      result <- condValue match {
        case Value.BoolVal(true) => WorkList.Eval(thenBranch, env)
        case Value.BoolVal(false) => WorkList.Eval(elseBranch, env)
        case _ => throw new RuntimeException("If condition must be boolean")
      }
    } yield result

    case Term.Fix(annotatedType, body) =>
      // Y-combinator approach: put a simple thunk at index 0
      // When it's looked up later, it will be evaluated in the environment where the lookup happens
      lazy val fixThunk: Value = Value.FixThunk(annotatedType, body, env.map { case (i, v) => (i + 1) -> v })
      val newEnv = env.map { case (i, v) => (i + 1) -> v } + (0 -> fixThunk)
      WorkList.Eval(body, newEnv)

    case Term.Record(fields) => {
      def evalFields(remaining: List[(String, Term)], acc: Map[String, Value]): WorkList[Value] = {
        remaining match {
          case Nil => WorkList.Return(Value.RecordVal(acc))
          case (name, term) :: rest => for {
            value <- WorkList.Eval(term, env)
            result <- evalFields(rest, acc + (name -> value))
          } yield result
        }
      }
      evalFields(fields.toList, Map.empty)
    }

    case Term.Proj(record, field) =>
      for {
        recordValue <- WorkList.Eval(record, env)
        result <- recordValue match {
          case Value.RecordVal(fields) =>
            fields.get(field) match {
              case Some(value) => WorkList.Return(value)
              case None => throw new RuntimeException(s"Field '$field' not found in record")
            }
          case _ => throw new RuntimeException("Select operation on non-record value")
        }
      } yield result
  }

  private sealed trait WorkList[+A] {

    final def flatMap[B](f: A => WorkList[B]): WorkList[B] = WorkList.Bind(this, f)

    final def map[B](f: A => B): WorkList[B] = flatMap(a => WorkList.Return(f(a)))

    final def withFilter(f: A => Boolean): WorkList[A] = flatMap { a =>
      if f(a) then WorkList.Return(a)
      else throw new RuntimeException("withFilter predicate failed")
    }

    final def run: A = {
      @tailrec
      def loop(current: WorkList[A]): A = current match {
        case WorkList.Return(value) => value
        case WorkList.Eval(term, env) => loop(evalWorkList(term)(using env))
        case WorkList.Bind(inner, cont) => inner match {
          case WorkList.Return(value) =>
            loop(cont(value))
          case WorkList.Eval(term, env) =>
            loop(evalWorkList(term)(using env).flatMap(cont))
          case WorkList.Bind(innerInner, innerCont) =>
            // Flatten nested binds: (inner >>= innerCont) >>= cont  ===  inner >>= (λx. innerCont(x) >>= cont)
            loop(innerInner.flatMap(a => innerCont(a).flatMap(cont)))
        }
      }
      loop(this)
    }
  }

  private object WorkList {
    case class Return[A](value: A) extends WorkList[A]
    case class Bind[A, B](workList: WorkList[A], cont: A => WorkList[B]) extends WorkList[B]
    case class Eval(term: Term, env: Env) extends WorkList[Value]
  }
}
