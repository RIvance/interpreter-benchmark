object FirstClassEnvInterpreter extends Interpreter {

  enum EnvTerm {
    case EmptyEnv
    case Var(index: Int)
    case Lam(paramType: Type, body: EnvTerm)
    case Closure(captured: EnvTerm, paramType: Type, body: EnvTerm)
    case App(leftTerm: EnvTerm, rightTerm: EnvTerm)
    case IntLit(value: Long)
    case BoolLit(value: Boolean)
    case BinOpInt(kind: IntOpKind, leftTerm: EnvTerm, rightTerm: EnvTerm)
    case BinOpCmp(kind: CmpOpKind, leftTerm: EnvTerm, rightTerm: EnvTerm)
    case If(cond: EnvTerm, thenBranch: EnvTerm, elseBranch: EnvTerm)
    case Fix(annotatedType: Type, body: EnvTerm)
    case Box(env: EnvTerm, term: EnvTerm)
    case Merge(leftEnv: EnvTerm, rightEnv: EnvTerm)
    case Record(fields: Map[String, EnvTerm])
    case Proj(record: EnvTerm, field: String)

    def isValue: Boolean = this match {
      case EmptyEnv => true
      case IntLit(_) => true
      case BoolLit(_) => true
      case Closure(_, _, body) if body.isValue => true
      case Merge(left, right) if left.isValue && right.isValue => true
      case Record(fields) if fields.values.forall(_.isValue) => true
      case Fix(_, body) if body.isValue => true
      case _ => false
    }

    def lookup(index: Int): EnvTerm = (index, this) match {
      case (0, Merge(left, right)) => right
      case (n, Merge(left, right)) => left.lookup(n - 1)
      case (_, EmptyEnv) => throw new RuntimeException(s"Unbound variable at index $index")
      case _ => throw new RuntimeException(s"Invalid environment lookup on $this for index $index")
    }

    infix def |>(term: EnvTerm): EnvTerm = Box(this, term)

    infix def |-(term: EnvTerm): Work[EnvTerm] = Work |- (this, term)

    def eval: Value = WorkList.Step(EmptyEnv |- this).eval match {
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

  enum Work[+A] {
    case |-(env: EnvTerm, term: EnvTerm) extends Work[EnvTerm]

    import EnvTerm.*

    private def andThen[B](cont: A => WorkList[B]): WorkList[B] = {
      WorkList.Step(this).andThen(cont)
    }

    def step[B](f: A => WorkList[B]): WorkList[B] = this match {
      case _   |- value if value.isValue => f(value)
      case env |- Var(index) => f(env.lookup(index))

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
          } else (env |- right).andThen { rightValue =>
            (env |- BinOpInt(kind, left, rightValue)).andThen(f)
          }
        } else (env |- left).andThen { leftValue =>
          (env |- BinOpInt(kind, leftValue, right)).andThen(f)
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
          } else (env |- right).andThen { rightValue =>
            (env |- BinOpCmp(kind, left, rightValue)).andThen(f)
          }
        } else (env |- left).andThen { leftValue =>
          (env |- BinOpCmp(kind, leftValue, right)).andThen(f)
        }
      }

      case env |- If(cond, thenBranch, elseBranch) => {
        if cond.isValue then {
          cond match {
            case BoolLit(true)  => (env |- thenBranch).andThen(f)
            case BoolLit(false) => (env |- elseBranch).andThen(f)
            case _              => throw new RuntimeException("If condition must be boolean")
          }
        } else (env |- cond).andThen { condValue =>
          (env |- If(condValue, thenBranch, elseBranch)).andThen(f)
        }
      }

      // venv |- <\x. e, cenv> v    --->    ((cenv,v) |> e)  >>=  f
      // venv |- v e2               --->    (venv |- e2)     >>=  \v2 -> (venv |- v v2) >>= f
      case env |- App(closure @ Closure(captured, _, body), arg) => {
        if arg.isValue then {
          (env |- (Merge(captured, arg) |> body)).andThen(f)
        } else {
          (env |- arg).andThen { argValue =>
            (env |- App(closure, argValue)).andThen(f)
          }
        }
      }
      // venv |- e1 e2  --->  (venv |- e1)  >>=  \v -> (venv |- v e2) >>= f
      // An alternative rule:
      // venv |- e1 e2  --->  (venv |- e1)  >>=  \<\x.e,cenv> ->
      //                          (venv |- e2)  >>=  \v2 -> (venv |- cenv, v2 |> e) >>= f
      case env |- App(fnTerm, argTerm) => {
        WorkList.Step(env |- fnTerm).andThen { fnValue =>
          WorkList.Step(env |- App(fnValue, argTerm)).andThen(f)
        }
      }
      case env |- Merge(left, right) => {
        if left.isValue then {
          (env |- right).andThen(rightValue => (env |- Merge(left, rightValue)).andThen(f))
        } else {
          (env |- left).andThen(leftValue => (env |- Merge(leftValue, right)).andThen(f))
        }
      }
      // Capture the current environment inside the closure
      case env |- Lam(paramType, body) => f(Closure(env, paramType, body))
      case env |- Box(captured, term) => {
        if term.isValue then f(term) // discard the environment
        else (env |- term).andThen { termValue =>
          (env |- (captured |> termValue)).andThen(f)
        }
      }
      case env |- (fixValue @ Fix(annotatedType, body)) => {
        (env |- body).andThen { bodyValue =>
          (env |- (Merge(env, fixValue) |> bodyValue)).andThen(f)
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
      case WorkList.Step(work) => work.step(cont)
    }

    def eval: A = this match {
      case WorkList.Return(value) => value
      case WorkList.Bind(workList, cont) => workList.step(cont).eval
      case WorkList.Step(work) => work.step(a => WorkList.Return(a)).eval
    }
  }

  object WorkList {
    case class Return[A](value: A) extends WorkList[A]
    case class Bind[A, B](workList: WorkList[A], cont: A => WorkList[B]) extends WorkList[B]
    case class Step[A](work: Work[A]) extends WorkList[A]
  }

}
