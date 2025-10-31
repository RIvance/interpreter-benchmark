import scala.annotation.tailrec

object SmallStepSubstInterpreter extends Interpreter {

  import Term.*

  override def name: String = "Small-Step Substitution Interpreter"

  override def eval(term: Term)(using _env: Env): Value = {
    @tailrec
    def reduce(term: Term): Value = step(term) match {
      case Some(nextTerm) => reduce(nextTerm)
      case None if term.isValue => term.toValue
      case None => throw RuntimeException(s"Stuck during evaluation: $term")
    }
    reduce(term)
  }

  extension (term: Term) {

    private def shift(index: Int, cutoff: Int = 0): Term = term match {

      case Var(k) =>
        if k >= cutoff then Var(k + index) else term

      case IntLit(_) | BoolLit(_) => term

      case Lam(paramType, body) =>
        Lam(paramType, body.shift(index, cutoff + 1))

      case App(fn, arg) =>
        App(fn.shift(index, cutoff), arg.shift(index, cutoff))

      case BinOp(kind, left, right) =>
        BinOp(kind, left.shift(index, cutoff), right.shift(index, cutoff))

      case If(cond, thenBranch, elseBranch) =>
        If(cond.shift(index, cutoff), thenBranch.shift(index, cutoff), elseBranch.shift(index, cutoff))

      case Fix(annotatedType, body) =>
        Fix(annotatedType, body.shift(index, cutoff + 1))

      case Record(fields) =>
        Record(fields.view.mapValues(_.shift(index, cutoff)).toMap)

      case Proj(record, field) =>
        Proj(record.shift(index, cutoff), field)
    }

    private def subst(index: Int, replacement: Term): Term = term match {

      case Var(k) =>
        if k == index then replacement.shift(index) else term

      case IntLit(_) | BoolLit(_) => term

      case Lam(paramType, body) =>
        Lam(paramType, body.subst(index + 1, replacement))

      case App(fn, arg) =>
        App(fn.subst(index, replacement), arg.subst(index, replacement))

      case BinOp(kind, left, right) =>
        BinOp(kind, left.subst(index, replacement), right.subst(index, replacement))

      case If(cond, thenBranch, elseBranch) =>
        If(cond.subst(index, replacement), thenBranch.subst(index, replacement), elseBranch.subst(index, replacement))

      case Fix(annotatedType, body) =>
        Fix(annotatedType, body.subst(index + 1, replacement))

      case Record(fields) =>
        Record(fields.view.mapValues(_.subst(index, replacement)).toMap)

      case Proj(record, field) =>
        Proj(record.subst(index, replacement), field)
    }

    private def isValue: Boolean = term match {
      case IntLit(_) => true
      case BoolLit(_) => true
      case Lam(_, _) => true
      case Fix(_, _) => true  // Fix is a value - only unfolds when applied
      case Record(fields) => fields.values.forall(_.isValue)
      case _ => false
    }

    // As this interpreter uses AST reconstruction and the evaluation result is also an AST,
    //  it's necessary to have a way to convert a Term that is a value into a Value directly.
    private def toValue: Value = term match {
      case IntLit(n) => Value.IntVal(n)
      case BoolLit(b) => Value.BoolVal(b)
      case Lam(_, _) => throw RuntimeException("Cannot convert lambda to value directly")
      case Fix(annotatedType, body) => Value.FixThunk(annotatedType, body, Map.empty)
      case Record(fields) =>
        val valueFields = fields.view.mapValues(_.toValue).toMap
        Value.RecordVal(valueFields)
      case _ => throw RuntimeException(s"Term is not a value: $term")
    }
  }

  private def step(term: Term): Option[Term] = term match {
    case App(fix @ Fix(_, body), arg) if arg.isValue =>
      // (fix x. t) v  ~~>  (t[x ↦ fix x. t]) v
      Some(App(body.subst(0, fix).shift(-1), arg))

    case App(Lam(_, body), arg) if arg.isValue =>
      // (λx. body) v  ~~>  body[x ↦ v]
      Some(body.subst(0, arg).shift(-1))

    case App(fn, arg) if fn.isValue =>
      // v1 t2  ~~>  v1 t2'
      step(arg).map(argStep => App(fn, argStep))

    case App(fn, arg) if !fn.isValue =>
      // t1 t2  ~~>  t1' t2
      step(fn).map(fnStep => App(fnStep, arg))

    case BinOp(kind, left, right) if !left.isValue =>
      // t1 op t2  ~~>  t1' op t2
      step(left).map(leftStep => BinOp(kind, leftStep, right))

    case BinOp(kind, left, right) if left.isValue && !right.isValue =>
      // v1 op t2  ~~>  v1 op t2'
      step(right).map(rightStep => BinOp(kind, left, rightStep))

    case BinOp(kind, IntLit(l), IntLit(r)) => kind match {
      case OpKind.Add => Some(IntLit(l + r))
      case OpKind.Sub => Some(IntLit(l - r))
      case OpKind.Mul => Some(IntLit(l * r))
      case OpKind.Eq  => Some(BoolLit(l == r))
      case OpKind.Lt  => Some(BoolLit(l < r))
      case OpKind.Gt  => Some(BoolLit(l > r))
    }

    case BinOp(OpKind.Eq, BoolLit(l), BoolLit(r)) => Some(BoolLit(l == r))

    case BinOp(op, l, r) =>
      throw RuntimeException(s"Binary operation $op on incompatible types: $l and $r")

    case If(cond, thenBranch, elseBranch) if !cond.isValue =>
      // if t1 then t2 else t3  ~~>  if t1' then t2 else t3
      step(cond).map(condStep => If(condStep, thenBranch, elseBranch))

    case If(BoolLit(true), thenBranch, _) =>
      // if true then t1 else t2  ~~>  t1
      Some(thenBranch)

    case If(BoolLit(false), _, elseBranch) =>
      // if false then t1 else t2  ~~>  t2
      Some(elseBranch)


    case Record(fields) if !fields.values.forall(_.isValue) =>
      // {l1 = t1, ..., ln = tn}  ~~>  {l1 = v1, ..., li = ti', ..., ln = tn}
      val steppedFields = fields.toList.map { case (name, term) =>
        if term.isValue then None else step(term).map(name -> _)
      }
      steppedFields.collectFirst { case Some((name, termStep)) =>
        val newFields = fields + (name -> termStep)
        Record(newFields)
      }

    case Term.Proj(record, field) if !record.isValue =>
      // t.l  ~~>  t'.l
      step(record).map(recordStep => Proj(recordStep, field))

    case Proj(Record(fields), field) =>
      // {l1 = v1, ..., ln = vn}.li  ~~>  vi
      fields.get(field).orElse(throw RuntimeException(s"Field '$field' not found in record"))

    // No rule applies
    case _ => None
  }

}

