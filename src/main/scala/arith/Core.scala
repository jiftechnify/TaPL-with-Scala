package arith

object Core {
  import Syntax._

  case class NoRuleAppliesException(t: Term) extends Exception

  // 項は数値か?
  def isNumerical(t: Term): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumerical(t1)
    case _ => false
  }

  // 項は値(=それ以上評価できない)か?
  def isVal(t: Term): Boolean = t match {
    case TmTrue | TmFalse => true
    case t1 if isNumerical(t1) => true
    case _ => false
  }

  // 1ステップ評価を進める
  def eval1(t: Term): Option[Term] = t match {
    case TmIf(TmTrue , t2, _ )  => Some(t2)
    case TmIf(TmFalse, _ , t3) => Some(t3)
    case TmIf(t1     , t2, t3) => eval1(t1).map(t12 => TmIf(t12, t2, t3))

    case TmSucc(t1) => eval1(t1).map(t12 => TmSucc(t12))

    case TmPred(TmZero) => Some(TmZero)
    case TmPred(TmSucc(nv1)) if isNumerical(nv1) => Some(nv1)
    case TmPred(t1) => eval1(t1).map(t12 => TmPred(t12))

    case TmIsZero(TmZero) => Some(TmTrue)
    case TmIsZero(TmSucc(nv1)) if isNumerical(nv1) => Some(TmFalse)
    case TmIsZero(t1) => eval1(t1).map(t12 => TmIsZero(t12))

    case _ => None
  }

  // 全ステップ評価
  def eval(t: Term): Option[Term] = {
    eval1(t).fold(Some(t).filter(isVal))(t2 => eval(t2))
  }
}

object ArithApp {
  import Core._
  import Syntax._

  def main(args: Array[String]): Unit = {
    val zeroIfTrue = TmIf(TmTrue, TmZero, TmSucc(TmZero))
    val isNotZero = TmIf(TmIsZero(TmSucc(TmZero)), TmFalse, TmTrue)
    val oneOrTwo = TmSucc(TmIf(TmFalse, TmZero, TmSucc(TmZero)))
    val baka = TmIf(TmZero, TmFalse, TmFalse)

    println(eval(zeroIfTrue))
    println(eval(isNotZero))
    println(eval(oneOrTwo))
    println(eval(baka))
  }
}
