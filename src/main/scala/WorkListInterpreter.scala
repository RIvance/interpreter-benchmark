import scala.annotation.tailrec

/**
 * Small-step semantics is good for reasoning. For instance, standard type
 *  soundness results such as progress and preservation are proved with
 *  small step semantics. However, small-step is bad for implementations:
 *  Two (performance) problems in small-step:
 *
 * 1) Substitution is bad as it requires traversing the terms each time it is used.
 *
 *  (\x. e) v ---> [v/x] e          </br>
 *
 * 2) AST keeps getting reconstructed
 *
 *  e1 ---> e1'                     </br>
 *  -----------------               </br>
 *  e1 e2 ---> e1' e2               </br>
 *
 *  Congruence rules like this one apply a rewrite step and rebuild the AST
 *
 *  (\x1 x2 x3 x4 x5. x1 + ... + x5) 1 ---> (\x2 x3 x4 x5. 1 + ... + x5) 1
 *  --------------------------------------------------------------------------              </br>
 *  (\x1 x2 x3 x4 x5. x1 + ... + x5) 1 2 ---> (\x2 x3 x4 x5. 1 + ... + x5) 1 2
 *  ------------------------------------------------------------------------------          </br>
 *  (\x1 x2 x3 x4 x5. x1 + ... + x5) 1 2 3 ---> (\x2 x3 x4 x5. 1 + ... + x5) 1 2 3
 *  ----------------------------------------------------------------------------------      </br>
 *  (\x1 x2 x3 x4 x5. x1 + ... + x5) 1 2 3 4 ---> (\x2 x3 x4 x5. 1 + ... + x5) 1 2 3 4
 *  ------------------------------------------------------------------------------------    </br>
 *  (\x1 x2 x3 x4 x5. x1 + ... + x5) 1 2 3 4 5 ---> (\x2 x3 x4 x5. 1 + ... + x5) 1 2 3 4
 *
 *  v = \x5. 1 + 2 + 3 + 4 + x5
 *
 * Big-step semantics does not have the performance problems of small-step. However,
 *  its (generally) recursive style is prone to stack-overflows. Furthermore,
 *  big-step has some problems for reasoning as it usually can only express properties
 *  about terminating programs.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * Our goal is to find a style of semantics that is good for reasoning and also
 *  good for implementations.
 *
 * What we show next is an AST for STLC with an environment-based semantics
 *  (basically the variant described in Section 6.2).
 *
 *  This is based on:
 *    A Case for first-class environments, OOPSLA 2024
 *
 * In an environment based semantics, there is no substitution. Beta reduction is:
 *
 *  venv |- <\x. e, cenv> v ---> (cenv,v) |> e        </br>
 *  venv |- (\x. e) ---> <\x. e, venv>                </br>
 *
 * The environment based semantics is still small step and good for reasoning, and it
 *  also eliminates substitution, which is one of the burdens for small-step. But
 *  it does not eliminate the problem of AST reconstruction. Furthermore, the notion
 *  of boxes (used in beta reduction) is needed to represent intermediate states in reduction.
 *
 * To address the problems we use inspiration from a previous work on type inference where
 *  we have employed a worklist approach:
 *
 * A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference, ICFP 2019
 *
 */
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

    final def eval: Value = WorkList.Eval(Empty |- this).eval match {
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

    private final def eval: WorkList[EnvTerm] = WorkList.Eval(this)

    private final def evalThen[B](cont: EnvTerm => WorkList[B]): WorkList[B] = {
      WorkList.Eval(this).andThen(cont)
    }

    final def step[B](f: EnvTerm => WorkList[B]): WorkList[B] = this match {
      case _   |- value if value.isValue => f(value)
      case env |- Var(index) =>
        val lookedUp = env.lookup(index)
        (env |- lookedUp).evalThen(f)

      // Binary operations
      case env |- BinOpInt(kind, left, right) => for {
        leftValue <- (env |- left).eval
        rightValue <- (env |- right).eval
        result <- (leftValue, rightValue) match {
          case (IntLit(l), IntLit(r)) =>
            kind match {
              case IntOpKind.Add => WorkList.pure(IntLit(l + r)).andThen(f)
              case IntOpKind.Sub => WorkList.pure(IntLit(l - r)).andThen(f)
              case IntOpKind.Mul => WorkList.pure(IntLit(l * r)).andThen(f)
            }
          case _ => throw new RuntimeException("Integer operation on non-integers")
        }
      } yield result

      case env |- BinOpCmp(kind, left, right) => for {
        leftValue <- (env |- left).eval
        rightValue <- (env |- right).eval
        result <- (leftValue, rightValue) match {
          case (IntLit(l), IntLit(r)) =>
            kind match {
              case CmpOpKind.Eq => WorkList.pure(BoolLit(l == r)).andThen(f)
              case CmpOpKind.Lt => WorkList.pure(BoolLit(l < r)).andThen(f)
              case CmpOpKind.Gt => WorkList.pure(BoolLit(l > r)).andThen(f)
            }
          case (BoolLit(l), BoolLit(r)) if kind == CmpOpKind.Eq =>
            WorkList.pure(BoolLit(l == r)).andThen(f)
          case _ => throw new RuntimeException("Comparison on incompatible types")
        }
      } yield result

      case env |- If(cond, thenBranch, elseBranch) => for {
        condValue <- (env |- cond).eval
        result <- condValue match {
          case BoolLit(true)  => (env |- thenBranch).evalThen(f)
          case BoolLit(false) => (env |- elseBranch).evalThen(f)
          case _              => throw new RuntimeException("If condition must be boolean")
        }
      } yield result

      case env |- App(fnTerm, argTerm) => for {
        case Closure(closureEnv, paramType, body) <- (env |- fnTerm).eval
        argValue <- (env |- argTerm).eval
        result <- (closureEnv + argValue |- body).evalThen(f)
      } yield result

      case env |- Merge(prevEnv, current) => for {
        prevEnvValue <- (env |- prevEnv).eval
        currentValue <- (env |- current).eval
      } yield f(prevEnvValue + currentValue).eval

      // Capture the current environment inside the closure
      case env |- Lam(paramType, body) => f(Closure(env, paramType, body))

      case env |- (fix @ Fix(annotatedType, body)) => {
        // Y-combinator approach for fixpoint:
        //  We evaluate the body in an environment that has the fix itself at index 0
        //  When body is a lambda (which it should be for well-typed programs),
        //  it will become a closure capturing this environment
        //  Later, when Var(0) is accessed inside that closure, it will look up
        //  the fix again, causing the recursion
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
    final def andThen[B](cont: A => WorkList[B]): WorkList[B] = WorkList.Bind(this, cont)

    final def step[B](cont: A => WorkList[B]): WorkList[B] = this match {
      case WorkList.Return(value) => cont(value)
      case WorkList.Bind(workList, nextCont) => workList.andThen(a => nextCont(a).andThen(cont))
      case WorkList.Eval(work) => work.step(cont)
    }

    @tailrec
    final def eval: A = this match {
      case WorkList.Return(value) => value
      case WorkList.Bind(workList, cont) => workList.step(cont).eval
      case WorkList.Eval(work) => work.step[A](a => WorkList.Return(a)).eval
    }

    final def flatMap[B](f: A => WorkList[B]): WorkList[B] = andThen(f)
    final def map[B](f: A => B): WorkList[B] = andThen(a => WorkList.Return(f(a)))
    final def withFilter(f: A => Boolean): WorkList[A] = andThen { a =>
      if f(a) then WorkList.Return(a)
      else throw new RuntimeException("withFilter predicate failed")
    }
  }

  object WorkList {
    case class Return[A](value: A) extends WorkList[A]
    case class Bind[A, B](workList: WorkList[A], cont: A => WorkList[B]) extends WorkList[B]
    case class Eval(work: |-) extends WorkList[EnvTerm]

    final def flatMap[A, B](workList: WorkList[A])(f: A => WorkList[B]): WorkList[B] = workList.andThen(f)
    final def map[A, B](workList: WorkList[A])(f: A => B): WorkList[B] = workList.andThen(a => Return(f(a)))
    final def pure[A](value: A): WorkList[A] = Return(value)
  }

}
