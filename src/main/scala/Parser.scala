import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

// ---------- Exception ----------
final case class ParseException(msg: String, line: Int, column: Int)
  extends RuntimeException(s"Parse error at $line:$column: $msg")

// ---------- Parser ----------
object Parser extends RegexParsers with PackratParsers {

  // Skip whitespace and //-comments
  // (?s) enables dotall to consume newlines in \s
  override val whiteSpace: Regex = """(?s)(?:\s+|//[^\n]*\n?)+""".r

  // -------- Lexical --------
  private val keywords: Set[String] =
    Set("fn", "fix", "if", "then", "else", "true", "false", "Int", "Bool")

  lazy val ident: PackratParser[String] =
    """[A-Za-z][A-Za-z0-9_]*""".r flatMap { s =>
      if keywords.contains(s) then failure(s"identifier expected, got keyword '$s'") else success(s)
    }

  lazy val intLit: PackratParser[Long] =
    """-?\d+""".r ^^ (_.toLong)

  // -------- Types (right-assoc) --------
  lazy val typeExpr: PackratParser[Type] =
    typeAtom ~ opt("->" ~> typeExpr) ^^ {
      case a ~ Some(b) => Type.Arrow(a, b)
      case a ~ None    => a
    }

  lazy val typeAtom: PackratParser[Type] =
    ("Int"  ^^^ Type.IntType) |
      ("Bool" ^^^ Type.BoolType) |
      "(" ~> typeExpr <~ ")"

  // -------- Expressions --------
  lazy val program: PackratParser[Expr] =
    phrase(expr)

  lazy val expr: PackratParser[Expr] =
    lambda | fix | ifExpr | comparison

  // \x: T. e   |  fn x: T => e
  lazy val lambda: PackratParser[Expr] =
    (("\\" | "fn") ~> ident) ~ (":" ~> typeExpr) ~ (("." | "=>") ~> expr) ^^ {
      case name ~ tpe ~ body => Expr.Lam(name, tpe, body)
    }

  // fix f: T. e
  lazy val fix: PackratParser[Expr] =
    ("fix" ~> ident) ~ (":" ~> typeExpr) ~ ("." ~> expr) ^^ {
      case name ~ tpe ~ body => Expr.Fix(name, tpe, body)
    }

  // if e1 then e2 else e3
  lazy val ifExpr: PackratParser[Expr] =
    ("if" ~> expr) ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
      case c ~ t ~ e => Expr.If(c, t, e)
    }

  // comparison has lower precedence than arithmetic
  lazy val comparison: PackratParser[Expr] =
    arith ~ rep(compOp ~ arith) ^^ {
      case first ~ rest => rest.foldLeft(first) {
        case (acc, "==" ~ rhs) => Expr.BinOpCmp(CmpOpKind.Eq, acc, rhs)
        case (acc, "<"  ~ rhs) => Expr.BinOpCmp(CmpOpKind.Lt, acc, rhs)
        case (acc, ">"  ~ rhs) => Expr.BinOpCmp(CmpOpKind.Gt, acc, rhs)
        case (_, _ ~ _) => throw IllegalStateException("unreachable")
      }
    }

  lazy val compOp: PackratParser[String] =
    "==" | "<" | ">"

  // addition/subtraction
  lazy val arith: PackratParser[Expr] =
    term ~ rep(addOp ~ term) ^^ {
      case first ~ rest =>
        rest.foldLeft(first) {
          case (acc, "+" ~ rhs) => Expr.BinOpInt(IntOpKind.Add, acc, rhs)
          case (acc, "-" ~ rhs) => Expr.BinOpInt(IntOpKind.Sub, acc, rhs)
          case (_, _ ~ _) => throw IllegalStateException("unreachable")
        }
    }

  lazy val addOp: PackratParser[String] =
    "+" | "-"

  // multiplication
  lazy val term: PackratParser[Expr] =
    factor ~ rep("*" ~> factor) ^^ {
      case first ~ rest => rest.foldLeft(first)((acc, rhs) => Expr.BinOpInt(IntOpKind.Mul, acc, rhs))
    }

  // application: atom atom ... (left-assoc)
  lazy val factor: PackratParser[Expr] =
    atom ~ rep(atom) ^^ {
      case first ~ args => args.foldLeft(first)(Expr.App(_, _))
    }

  lazy val atom: PackratParser[Expr] =
    (intLit ^^ (n => Expr.IntLit(n))) |
    ("true"  ^^^ Expr.BoolLit(true)) |
    ("false" ^^^ Expr.BoolLit(false)) |
    (ident ^^ (s => Expr.Var(s))) |
    ("(" ~> expr <~ ")")

  // -------- Public API --------
  def parse(input: String): Expr = {
    val reader = new PackratReader(new CharSequenceReader(input))
    program(reader) match {
      case Success(ast, _) => ast
      case NoSuccess(msg, next) =>
        throw ParseException(msg, next.pos.line, next.pos.column)
      case Failure(msg, next) =>
        throw ParseException(msg, next.pos.line, next.pos.column)
      case Error(msg, next) =>
        throw ParseException(msg, next.pos.line, next.pos.column)
    }
  }
}
