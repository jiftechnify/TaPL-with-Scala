package untyped

object Core {
  import Syntax._

  case class NoRuleAppliesException(t: Term) extends Exception

  def eval1(t: Term): Term = t match {
    case TmApp(TmAbs(_, t12), v2) if v2.isVal => termSubstTop(v2, t12)
    case TmApp(v1, t2) if v1.isVal => TmApp(v1, eval1(t2))
    case TmApp(t1, t2) => TmApp(eval1(t1), t2)
    case _ => throw NoRuleAppliesException(t)
  }

  def eval(t: Term): Term = {
    try {
      eval(eval1(t))
    }
    catch {
      case NoRuleAppliesException(t1) => t1
    }
  }
}
