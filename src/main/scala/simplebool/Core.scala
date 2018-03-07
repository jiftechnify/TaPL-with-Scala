package simplebool

object Core {
  import Syntax._

  /* 評価 */
  def eval1(t: Term): Option[Term] = t match {
    case TmApp(TmAbs(_, _, t12), v2) if v2.isVal => Some(termSubstTop(v2, t12))
    case TmApp(v1, t2) if v1.isVal => eval1(t2).map(t22 => TmApp(v1, t22))
    case TmApp(t1, t2) => eval1(t1).map(t12 => TmApp(t12, t2))

    case _ => None
  }

  def eval(t: Term): Option[Term] = {
    eval1(t).fold(Some(t).filter(_.isVal))(t2 => eval(t2))
  }

  /* 型付け */
  class TypingError(val msg: String) extends Exception(msg)

  def typeOf(ctx: Context, t: Term): Type = t match {
    case TmVar(i, _) => getTypeFromContext(ctx, i)
    case TmAbs(name, ty1, t2) =>
      val ctx2 = addBinding(ctx, name, VarBind(ty1))
      val ty2 = typeOf(ctx2, t2)
      Arrow(ty1, ty2)
    case TmApp(t1, t2) =>
      val ty1 = typeOf(ctx, t1)
      val ty2 = typeOf(ctx, t2)
      ty1 match {
        case Arrow(ty11, ty12) =>
          if (ty2 == ty11) ty12
          else throw new TypingError("parameter type mismatch")
        case _ => throw new TypingError("arrow type expected")
      }
    case TmTrue | TmFalse => Bool
    case TmIf(tb, tt, tf) =>
      if (typeOf(ctx, tb) == Bool) {
        val ty2 = typeOf(ctx, tt)
        if (ty2 == typeOf(ctx, tf)) ty2 else throw new TypingError("arms of conditional have different types")
      }
      else throw new TypingError("guard of conditional not a boolean")
  }
}
