enum Trampoline[+A] {
  case Done(result: A)
  case Delay(thunk: () => Trampoline[A])

  // Run to completion
  def run: A = {
    var current: Trampoline[A] = this
    while (true) current match {
      case Trampoline.Done(result) => return result
      case Trampoline.Delay(th)    => current = th()
    }
    // unreachable
    throw new IllegalStateException("Unreachable in Trampoline.run")
  }

  // map / flatMap (without for-comprehensions)
  def map[B](f: A => B): Trampoline[B] =
    this.flatMap(a => Trampoline.Done(f(a)))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case Trampoline.Done(a) => f(a)
    case Trampoline.Delay(th) =>
      Trampoline.Delay(() => th().flatMap(f))
  }
}

def done[A](a: A): Trampoline[A] = Trampoline.Done(a)

def delay[A](thunk: => Trampoline[A]): Trampoline[A] = Trampoline.Delay(() => thunk)
