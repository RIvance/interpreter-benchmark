enum Type {
  case IntType
  case BoolType
  case Arrow(from: Type, to: Type)
  case RecordType(fields: Map[String, Type])
}

enum OpKind {
  case Add, Sub, Mul, Eq, Lt, Gt

  def resultType: Type = this match {
    case Add | Sub | Mul => Type.IntType
    case Eq | Lt | Gt => Type.BoolType
  }
}

enum Expr {
  case Var(name: String)
  case Lam(param: String, tpe: Type, body: Expr)
  case App(fn: Expr, arg: Expr)
  case IntLit(value: Long)
  case BoolLit(value: Boolean)
  case BinOp(kind: OpKind, lhs: Expr, rhs: Expr)
  case If(cond: Expr, thenBr: Expr, elseBr: Expr)
  case Fix(name: String, tpe: Type, body: Expr)
  case Record(fields: Map[String, Expr])
  case Proj(record: Expr, field: String)
  case Let(name: String, value: Expr, body: Expr)
  case LetRec(name: String, tpe: Type, value: Expr, body: Expr)

  /**
   * Convert this expression (with names) to a Term (with De Bruijn indices).
   * @param stack binding stack; head = most-recent binding
   */
  def toTerm(stack: List[String] = Nil): Term = {

    def lookup(name: String, ctx: List[String]): Int = {
      ctx.indexOf(name) match {
        case -1 => throw new RuntimeException(s"Unbound variable: $name")
        case idx => idx
      }
    }

    this match {
      case Expr.Var(name) => Term.Var(lookup(name, stack))

      case Expr.Lam(param, tpe, body) =>
        val extended = param :: stack
        Term.Lam(tpe, body.toTerm(extended))

      case Expr.App(fnExpr, argExpr) =>
        val fnTerm = fnExpr.toTerm(stack)
        val argTerm = argExpr.toTerm(stack)
        Term.App(fnTerm, argTerm)

      case Expr.IntLit(value) =>
        Term.IntLit(value)

      case Expr.BoolLit(value) =>
        Term.BoolLit(value)

      case Expr.BinOp(kind, lhs, rhs) =>
        val leftTerm = lhs.toTerm(stack)
        val rightTerm = rhs.toTerm(stack)
        Term.BinOp(kind, leftTerm, rightTerm)

      case Expr.If(cond, thenBr, elseBr) =>
        val condTerm = cond.toTerm(stack)
        val thenTerm = thenBr.toTerm(stack)
        val elseTerm = elseBr.toTerm(stack)
        Term.If(condTerm, thenTerm, elseTerm)

      case Expr.Fix(name, tpe, body) =>
        val extended = name :: stack
        val bodyTerm = body.toTerm(extended)
        Term.Fix(tpe, bodyTerm)

      case Expr.Record(fields) =>
        val termFields = fields.map { case (name, expr) => (name, expr.toTerm(stack)) }
        Term.Record(termFields)

      case Expr.Proj(record, field) =>
        val recordTerm = record.toTerm(stack)
        Term.Proj(recordTerm, field)

      case Expr.Let(name, value, body) =>
        // let x = e1 in e2  ~~>  (\x. e2) e1
        val valueTerm = value.toTerm(stack)
        val extended = name :: stack
        val bodyTerm = body.toTerm(extended)
        Term.App(Term.Lam(valueTerm.infer(), bodyTerm), valueTerm)

      case Expr.LetRec(name, tpe, value, body) =>
        // let rec f: T = e1 in e2  ~~>  (\f. e2) (fix f: T. e1)
        val extended = name :: stack
        val valueTerm = value.toTerm(extended)
        val fixTerm = Term.Fix(tpe, valueTerm)
        val bodyTerm = body.toTerm(extended)
        Term.App(Term.Lam(tpe, bodyTerm), fixTerm)
    }
  }
}

enum Term {
  case Var(index: Int)
  case Lam(parameterType: Type, body: Term)
  case App(leftTerm: Term, rightTerm: Term)
  case IntLit(n: Long)
  case BoolLit(b: Boolean)
  case BinOp(kind: OpKind, leftTerm: Term, rightTerm: Term)
  case If(cond: Term, thenBranch: Term, elseBranch: Term)
  case Fix(annotatedType: Type, body: Term)
  case Record(fields: Map[String, Term])
  case Proj(record: Term, field: String)

  /**
   * Infer the type of this term.
   * @param typeEnv type environment mapping De Bruijn indices to types
   */
  def infer(typeEnv: List[Type] = Nil): Type = this match {
    case Term.Var(index) =>
      if (index >= 0 && index < typeEnv.length) typeEnv(index)
      else throw new RuntimeException(s"Variable index $index out of bounds in type environment")

    case Term.Lam(parameterType, body) =>
      val extendedEnv = parameterType :: typeEnv
      Type.Arrow(parameterType, body.infer(extendedEnv))

    case Term.App(leftTerm, rightTerm) =>
      leftTerm.infer(typeEnv) match {
        case Type.Arrow(_, to) => to
        case other => throw new RuntimeException(s"Cannot apply non-function type: $other")
      }

    case Term.IntLit(_)         => Type.IntType
    case Term.BoolLit(_)        => Type.BoolType
    case Term.BinOp(kind, _, _) => kind.resultType

    case Term.If(_, thenBranch, _) => thenBranch.infer(typeEnv)

    case Term.Fix(annotatedType, _) => annotatedType

    case Term.Record(fields) =>
      Type.RecordType(fields.map { case (name, term) => (name, term.infer(typeEnv)) })

    case Term.Proj(record, field) =>
      record.infer(typeEnv) match {
        case Type.RecordType(fields) => fields.getOrElse(field,
          throw new RuntimeException(s"Field $field not found in record"))
        case _ => throw new RuntimeException("Cannot project from non-record type")
      }
  }
}

enum Value {
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case Closure(env: Map[Int, Value], body: Term)
  case FixThunk(annotatedType: Type, body: Term, env: Map[Int, Value] = Map.empty)
  case RecordVal(fields: Map[String, Value])

  override def toString: String = this match {
    case IntVal(n) => n.toString
    case BoolVal(b) => b.toString
    case Closure(_, body) => s"<closure>"
    case FixThunk(annotatedType, _, _) => s"<fixpoint: $annotatedType>"
    case RecordVal(fields) => {
      val fieldStr = fields.map { case (name, value) => s"$name: $value" }.mkString(", ")
      s"{ $fieldStr }"
    }
  }
}
