package untyped

object Syntax {
  // 型無しラムダ計算の項
  sealed trait Term {
    // この項は値か?
    val isVal: Boolean

    // 項の文字列表現
    def printtm(ctx: Context): String
  }

  // i: この変数のde Bruijnインデックス
  // l: この変数が現れる文脈全体の長さ
  case class TmVar(i: Int, ctxLen: Int) extends Term {
    override val isVal: Boolean = false

    override def printtm(ctx: Context): String =
      if (ctx.lengthCompare(ctxLen) == 0) index2Name(ctx, i)
      else "[bad index]"
  }

  // bvHint: 束縛変数名のヒント(表示するときのみ使用)
  // t: 本体の項
  case class TmAbs(nameHint: String, t: Term) extends Term {
    // ラムダ抽象は値
    override val isVal: Boolean = true

    override def printtm(ctx: Context): String = {
      val (ctx2, x2) = pickFreshName(ctx, nameHint)
      s"(lambda $x2. ${t.printtm(ctx2)})"
    }
  }

  // t1: 関数部分, t2: 引数部分
  case class TmApp(t1: Term, t2: Term) extends Term {
    override val isVal: Boolean = false

    override def printtm(ctx: Context): String = {
      s"(${t1.printtm(ctx)} ${t2.printtm(ctx)})"
    }
  }

  sealed trait Binding
  case object NameBind extends Binding

  type Context = List[(String, Binding)]

  // de Brujinインデックスから変数名を引く
  def index2Name(context: Context, i: Int): String = context(i)._1

  // contextにnameがすでに含まれているか?
  def isNameBound(context: Context, name: String): Boolean =
    context.exists{ case (n, _) => n == name }

  // contextに含まれない新しい名前を作り、contextに追加する
  def pickFreshName(context: Context, nameHint: String): (Context, String) = {
    if (isNameBound(context, nameHint)) pickFreshName(context, nameHint + "'")
    else ((nameHint, NameBind) :: context, nameHint)
  }

  // 項をd個シフトする
  def termShift(d: Int, t: Term): Term = {
    // 1段階の操作。c: 自由変数と束縛変数を分ける閾値
    def walk(c: Int, t: Term): Term = t match {
      case TmVar(i, ctxLen) =>
        if (i >= c) TmVar(i + d, ctxLen + d)
        else TmVar(i, ctxLen + d)
      case TmAbs(hint, t1) => TmAbs(hint, walk(c + 1, t1))
      case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
    }
    walk(0, t)
  }

  // 代入
  // ラムダをくぐるごとにsをシフトするのではなく、変数の場合に一気にシフトする
  def termSubst(j: Int, s: Term, t: Term): Term = {
    def walk(c: Int, t: Term): Term = t match {
      case TmVar(i, ctxLen) =>
        if (i == j + c) termShift(c, s)
        else TmVar(i, ctxLen)
      case TmAbs(hint, t1) => TmAbs(hint, walk(c + 1, t1))
      case TmApp(t1, t2) => TmApp(walk(c, t1), walk(c, t2))
    }
    walk(0, t)
  }

  // ベータ簡約
  def termSubstTop(v: Term, f: Term): Term = termShift(-1, termSubst(0, termShift(1, v), f))
}
