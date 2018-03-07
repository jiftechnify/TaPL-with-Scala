package untyped

object Core {
  import Syntax._

  def eval1(t: Term): Option[Term] = t match {
    case TmApp(TmAbs(_, t12), v2) if v2.isVal => Some(termSubstTop(v2, t12))
    case TmApp(v1, t2) if v1.isVal => eval1(t2).map(t22 => TmApp(v1, t22))
    case TmApp(t1, t2) => eval1(t1).map(t12 => TmApp(t12, t2))

    case _ => None
  }

  def eval(t: Term): Option[Term] = {
    eval1(t).fold(Some(t).filter(_.isVal))(t2 => eval(t2))
  }
}