package arith

object Syntax {
  sealed trait Term
  case object TmTrue extends Term
  case object TmFalse extends Term
  case class TmIf(t1: Term, t2: Term, t3: Term) extends Term
  case object TmZero extends Term
  case class TmSucc(t1: Term) extends Term
  case class TmPred(t1: Term) extends Term
  case class TmIsZero(t1: Term) extends Term
}
