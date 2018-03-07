package untyped

object Main {
  import Core._
  import Syntax._

  def main(args: Array[String]): Unit = {
    def l(name: String, v: Term): Term = v match {
      case TmVar(i, ctxLen) => TmAbs(name, TmVar(i, ctxLen + 1))
      case TmAbs(hint, t) =>
    }

    val id: Term = l("x", TmVar(0))

    val tru: Term = l("t", l("f", TmVar(1)))
    val fls: Term = l("t", l("f", TmVar(0)))
    val test: Term = l("b", l("ift", l("iff", TmApp(TmApp(TmVar(2), TmVar(1)), TmVar(0)))))

    val and: Term = TmAbs("b", TmAbs("c", TmApp(TmApp(TmVar(1, 2), TmVar(0, 2)), fls)))
    val not: Term = TmAbs("b", TmApp(TmApp(TmVar(0, 1), fls), tru))

    val zero: Term = TmAbs("s", TmAbs("z", TmVar(0, 2)))
    val one: Term = TmAbs("s", TmAbs("z", TmApp(TmVar(1, 2), TmVar(0, 2))))

    evalAndPrint(TmApp(id, tru))
    evalAndPrint(TmApp(id, one))

    evalAndPrint(TmApp(not, tru))
    evalAndPrint(TmApp(not, fls))

    evalAndPrint(appN(and, tru, tru))
    evalAndPrint(appN(and, tru, fls))

    evalAndPrint(appN(test, tru, one, zero))
    evalAndPrint(appN(test, fls, one, zero))
  }

  def evalAndPrint(t: Term, ctx: Context = List.empty): Unit = {
    println(eval(t).fold("fail")(_.printtm(ctx)))
  }

  def appN(ts: Term*): Term = ts.reduceLeft((apps, t) => TmApp(apps, t))
}
