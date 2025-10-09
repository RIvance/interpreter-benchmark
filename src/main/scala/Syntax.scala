enum Type {
  case IntType
  case BoolType
  case Arrow(from: Type, to: Type)
}

enum IntOpKind {
  case Add, Sub, Mul
}

enum CmpOpKind {
  case Eq, Lt, Gt
}

enum Expr {
  case Var(name: String)
  case Lam(param: String, tpe: Type, body: Expr)
  case App(fn: Expr, arg: Expr)
  case IntLit(value: Long)
  case BoolLit(value: Boolean)
  case BinOpInt(kind: IntOpKind, lhs: Expr, rhs: Expr)
  case BinOpCmp(kind: CmpOpKind, lhs: Expr, rhs: Expr)
  case If(cond: Expr, thenBr: Expr, elseBr: Expr)
  case Fix(name: String, tpe: Type, body: Expr)

  /** Convert this expression (with names) to a Term (with De Bruijn indices).
   *
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

      case Expr.BinOpInt(kind, lhs, rhs) =>
        val leftTerm = lhs.toTerm(stack)
        val rightTerm = rhs.toTerm(stack)
        Term.BinOpInt(kind, leftTerm, rightTerm)

      case Expr.BinOpCmp(kind, lhs, rhs) =>
        val leftTerm = lhs.toTerm(stack)
        val rightTerm = rhs.toTerm(stack)
        Term.BinOpCmp(kind, leftTerm, rightTerm)

      case Expr.If(cond, thenBr, elseBr) =>
        val condTerm = cond.toTerm(stack)
        val thenTerm = thenBr.toTerm(stack)
        val elseTerm = elseBr.toTerm(stack)
        Term.If(condTerm, thenTerm, elseTerm)

      case Expr.Fix(name, tpe, body) =>
        val extended = name :: stack
        val bodyTerm = body.toTerm(extended)
        Term.Fix(tpe, bodyTerm)
    }
  }
}

enum Term {
  case Var(index: Int)
  case Lam(parameterType: Type, body: Term)
  case App(leftTerm: Term, rightTerm: Term)
  case IntLit(n: Long)
  case BoolLit(b: Boolean)
  case BinOpInt(kind: IntOpKind, leftTerm: Term, rightTerm: Term)
  case BinOpCmp(kind: CmpOpKind, leftTerm: Term, rightTerm: Term)
  case If(cond: Term, thenBranch: Term, elseBranch: Term)
  case Fix(annotatedType: Type, body: Term)
}

enum Value {
  case IntVal(n: Long)
  case BoolVal(b: Boolean)
  case Closure(apply: Value => Value)
}
