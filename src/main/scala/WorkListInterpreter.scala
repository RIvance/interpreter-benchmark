object WorkListInterpreter extends Interpreter {

  enum EnvTerm {
    case Empty
    case Var(index: Int)
    case Lam(paramType: Type, body: EnvTerm)
    case Closure(captured: EnvTerm, paramType: Type, body: EnvTerm)
    case App(fnTerm: EnvTerm, argTerm: EnvTerm)
    case IntLit(value: Long)
    case BoolLit(value: Boolean)
    case BinOpInt(kind: IntOpKind, leftTerm: EnvTerm, rightTerm: EnvTerm)
    case BinOpCmp(kind: CmpOpKind, leftTerm: EnvTerm, rightTerm: EnvTerm)
    case If(cond: EnvTerm, thenBranch: EnvTerm, elseBranch: EnvTerm)
    case Fix(annotatedType: Type, body: EnvTerm)
    case Merge(prevEnv: EnvTerm, current: EnvTerm)
    case Record(fields: Map[String, EnvTerm])
    case Proj(record: EnvTerm, field: String)

    def isValue: Boolean = this match {
      case Empty => true
      case IntLit(_) => true
      case BoolLit(_) => true
      case Closure(_, _, _) => true
      case Merge(prevEnv, current) if prevEnv.isValue && current.isValue => true
      case Record(fields) if fields.values.forall(_.isValue) => true
      case _ => false
    }

    def lookup(index: Int): EnvTerm = (index, this) match {
      case (0, Merge(prevEnv, current)) => current
      case (n, Merge(prevEnv, current)) => prevEnv.lookup(n - 1)
      case (_, Empty) => throw new RuntimeException(s"Unbound variable at index $index")
      case _ => throw new RuntimeException(s"Invalid environment lookup on $this for index $index")
    }

    // noinspection RedundantNewCaseClass <- just to keep IDE happy,
    //  otherwise will it throw a false warning
    infix def |-(term: EnvTerm) = new |-(this, term)

    infix def +(current: EnvTerm): EnvTerm = Merge(this, current)

    def eval: Value = WorkList.Eval(Empty |- this).eval match {
      case IntLit(n) => Value.IntVal(n)
      case BoolLit(b) => Value.BoolVal(b)
      case Record(fields) =>
        Value.RecordVal(fields.view.mapValues(_.eval).toMap)
      case Closure(captured, paramType, body) =>
        throw new RuntimeException("Cannot return a closure as a value")
      case _ => throw new RuntimeException("Evaluation did not result in a value")
    }
  }

  extension (term: Term) {
    def toEnvTerm: EnvTerm = term match {
      case Term.Var(index) => EnvTerm.Var(index)
      case Term.Lam(paramType, body) => EnvTerm.Lam(paramType, body.toEnvTerm)
      case Term.App(leftTerm, rightTerm) => EnvTerm.App(leftTerm.toEnvTerm, rightTerm.toEnvTerm)
      case Term.IntLit(n) => EnvTerm.IntLit(n)
      case Term.BoolLit(b) => EnvTerm.BoolLit(b)
      case Term.BinOpInt(kind, leftTerm, rightTerm) =>
        EnvTerm.BinOpInt(kind, leftTerm.toEnvTerm, rightTerm.toEnvTerm)
      case Term.BinOpCmp(kind, leftTerm, rightTerm) =>
        EnvTerm.BinOpCmp(kind, leftTerm.toEnvTerm, rightTerm.toEnvTerm)
      case Term.If(cond, thenBranch, elseBranch) =>
        EnvTerm.If(cond.toEnvTerm, thenBranch.toEnvTerm, elseBranch.toEnvTerm)
      case Term.Fix(annotatedType, body) => EnvTerm.Fix(annotatedType, body.toEnvTerm)
      case Term.Record(fields) =>
        EnvTerm.Record(fields.view.mapValues(_.toEnvTerm).toMap)
      case Term.Proj(record, field) => EnvTerm.Proj(record.toEnvTerm, field)
    }
  }

  override def eval(term: Term)(using _env: Env): Value = term.toEnvTerm.eval

  case class |-(env: EnvTerm, term: EnvTerm) {

    import EnvTerm.*

    private def evalThen[B](cont: EnvTerm => WorkList[B]): WorkList[B] = {
      WorkList.Eval(this).andThen(cont)
    }

    def step[B](f: EnvTerm => WorkList[B]): WorkList[B] = this match {
      case _   |- value if value.isValue => f(value)
      case env |- Var(index) =>
        val lookedUp = env.lookup(index)
        (env |- lookedUp).evalThen(f)

      // Binary operations
      case env |- BinOpInt(kind, left, right) => {
        if left.isValue then {
          if right.isValue then {
            (left, right) match {
              case (IntLit(l), IntLit(r)) =>
                kind match {
                  case IntOpKind.Add => f(IntLit(l + r))
                  case IntOpKind.Sub => f(IntLit(l - r))
                  case IntOpKind.Mul => f(IntLit(l * r))
                }
              case _ => throw new RuntimeException("Integer operation on non-integers")
            }
          } else (env |- right).evalThen { rightValue =>
            (env |- BinOpInt(kind, left, rightValue)).evalThen(f)
          }
        } else (env |- left).evalThen { leftValue =>
          (env |- BinOpInt(kind, leftValue, right)).evalThen(f)
        }
      }

      case env |- BinOpCmp(kind, left, right) => {
        if left.isValue then {
          if right.isValue then {
            (left, right) match {
              case (IntLit(l), IntLit(r)) =>
                kind match {
                  case CmpOpKind.Eq => f(BoolLit(l == r))
                  case CmpOpKind.Lt => f(BoolLit(l < r))
                  case CmpOpKind.Gt => f(BoolLit(l > r))
                }
              case (BoolLit(l), BoolLit(r)) if kind == CmpOpKind.Eq =>
                f(BoolLit(l == r))
              case _ => throw new RuntimeException("Comparison on incompatible types")
            }
          } else (env |- right).evalThen { rightValue =>
            (env |- BinOpCmp(kind, left, rightValue)).evalThen(f)
          }
        } else (env |- left).evalThen { leftValue =>
          (env |- BinOpCmp(kind, leftValue, right)).evalThen(f)
        }
      }

      case env |- If(cond, thenBranch, elseBranch) => {
        if cond.isValue then {
          cond match {
            case BoolLit(true)  => (env |- thenBranch).evalThen(f)
            case BoolLit(false) => (env |- elseBranch).evalThen(f)
            case _              => throw new RuntimeException("If condition must be boolean")
          }
        } else (env |- cond).evalThen { condValue =>
          (env |- If(condValue, thenBranch, elseBranch)).evalThen(f)
        }
      }

      case env |- App(fnTerm, argTerm) => {
        (env |- fnTerm).evalThen {
          case Closure(closureEnv, paramType, body) =>
            (env |- argTerm).evalThen { argValue => (closureEnv + argValue |- body).evalThen(f) }
          case _ => throw new RuntimeException("Runtime type error: expected closure")
        }
      }

      case env |- Merge(prevEnv, current) => {
        if prevEnv.isValue then {
          (env |- current).evalThen(currentValue => (env |- Merge(prevEnv, currentValue)).evalThen(f))
        } else {
          (env |- prevEnv).evalThen(prevEnvValue => (env |- Merge(prevEnvValue, current)).evalThen(f))
        }
      }
      // Capture the current environment inside the closure
      case env |- Lam(paramType, body) => f(Closure(env, paramType, body))

      case env |- (fix @ Fix(annotatedType, body)) => {
        // Y-combinator approach for fixpoint:
        // We evaluate the body in an environment that has the fix itself at index 0
        // When body is a lambda (which it should be for well-typed programs),
        // it will become a closure capturing this environment
        // Later, when Var(0) is accessed inside that closure, it will look up
        // the fix again, causing the recursion
        (env + fix |- body).evalThen(f)
      }

      case env |- Record(fields) => {
        // Evaluate all fields to values
        def evalFields(remaining: List[(String, EnvTerm)], acc: Map[String, EnvTerm]): WorkList[EnvTerm] = {
          remaining match {
            case Nil => WorkList.Return(Record(acc))
            case (name, fieldTerm) :: rest =>
              if fieldTerm.isValue then {
                evalFields(rest, acc + (name -> fieldTerm))
              } else {
                (env |- fieldTerm).evalThen { fieldValue =>
                  evalFields(rest, acc + (name -> fieldValue))
                }
              }
          }
        }
        evalFields(fields.toList, Map.empty).andThen(f)
      }

      case env |- Proj(record, field) => {
        if record.isValue then {
          record match {
            case Record(fields) =>
              fields.get(field) match {
                case Some(value) => f(value)
                case None => throw new RuntimeException(s"Field '$field' not found in record")
              }
            case _ => throw new RuntimeException("Projection on non-record value")
          }
        } else {
          (env |- record).evalThen { recordValue =>
            (env |- Proj(recordValue, field)).evalThen(f)
          }
        }
      }

      case _ |- _ => throw new RuntimeException(s"Invalid evaluation state: $this")
    }
  }

  sealed trait WorkList[+A] {
    def andThen[B](cont: A => WorkList[B]): WorkList[B] = WorkList.Bind(this, cont)

    def step[B](cont: A => WorkList[B]): WorkList[B] = this match {
      case WorkList.Return(value) => cont(value)
      case WorkList.Bind(workList, nextCont) => workList.andThen(a => nextCont(a).andThen(cont))
      case WorkList.Eval(work) => work.step(cont)
    }

    def eval: A = this match {
      case WorkList.Return(value) => value
      case WorkList.Bind(workList, cont) => workList.step(cont).eval
      case WorkList.Eval(work) => work.step(a => WorkList.Return(a)).eval
    }
  }

  object WorkList {
    case class Return[A](value: A) extends WorkList[A]
    case class Bind[A, B](workList: WorkList[A], cont: A => WorkList[B]) extends WorkList[B]
    case class Eval(work: |-) extends WorkList[EnvTerm]
  }

}
