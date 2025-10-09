type Env = Map[Int, Value]

// Utility: shift all env indices by +1 (De Bruijn "lift") and insert arg at index 0
extension (env: Env) def extend(arg: Value): Env = {
  val shifted = env.iterator.map { case (idx, value) => (idx + 1) -> value }.toMap
  shifted + (0 -> arg)
}

trait Interpreter {
  def eval(term: Term)(using env: Env): Value
}
