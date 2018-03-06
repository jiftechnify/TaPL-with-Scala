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
  def eval1(t: Term): Term = t match {
    case TmIf(TmTrue , t2, _ )  => t2
    case TmIf(TmFalse, _ , t3) => t3
    case TmIf(t1     , t2, t3) => TmIf(eval1(t1), t2, t3)

    case TmSucc(t1) => TmSucc(eval1(t1))

    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(nv1)) if isNumerical(nv1) => nv1
    case TmPred(t1) => TmPred(eval1(t1))

    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(nv1)) if isNumerical(nv1) => TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))

    case te => throw NoRuleAppliesException(te)
  }

  // 全ステップ評価
  // TODO 正常終了と異常終了を区別する
  def eval(t: Term): Term = {
    try {
      eval(eval1(t))
    }
    catch {
      case NoRuleAppliesException(te) => te
    }
  }
}

object ArithApp {
  import Core._
  import Syntax._

  def main(args: Array[String]): Unit = {
    val zeroIfTrue = TmIf(TmTrue, TmZero, TmSucc(TmZero))
    val isNotZero = TmIf(TmIsZero(TmSucc(TmZero)), TmFalse, TmTrue)
    val baka = TmIf(TmZero, TmFalse, TmFalse)

    println(eval(zeroIfTrue))
    println(eval(isNotZero))
    println(eval(baka))
  }
}
