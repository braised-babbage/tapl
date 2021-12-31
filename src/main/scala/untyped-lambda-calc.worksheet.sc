/* Untyped Lambda Calculus (ch 7) */
import scala.util.Random

case class Binding()

case class Context(contents: IndexedSeq[(String, Binding)]):
    def freshName(name: String): (Context, String) =
        def tryWithSuffix(suffix: String): (Context, String) =
            val combined = name + suffix
            contents.find(_._1 == combined) match {
                case None => 
                    val entry = (combined, Binding())
                    (Context(contents.+:(entry)), combined)
                case _ => tryWithSuffix(Random.nextInt().toString)
            }
        tryWithSuffix("")
        
    def size: Int = contents.size
    
    def name(n: Int): String = contents(n)._1

    def idx(name: String): Option[Int] =
        val idx = contents.indexWhere(_._1 == name)
        if idx == -1 then None else Some(idx)

object Context:
    def apply(contents: (String, Binding)*): Context =
        Context(IndexedSeq.from(contents))


enum Term:
    case Var(idx: Int, ctxSize: Int)
    case Abs(name: String, body: Term)
    case Appl(op: Term, arg: Term)

    def pretty(using ctx: Context): String = this match
        case Abs(name, body) => 
            val (newCtx, x) = ctx.freshName(name)
            s"(λ ${x} . ${body.pretty(using newCtx)})"
        case Appl(t1, t2) => s"(${t1.pretty} ${t2.pretty})"
        case Var(idx, ctxSize) =>
            if ctx.size == ctxSize then ctx.name(idx) else "[bad index]"

    def isVal(using ctx: Context): Boolean = this match
        case a: Abs => true
        case _ => false
    
    def shift(d: Int): Term = 
        def walk(c: Int, t: Term): Term = t match
            case Var(x, n) => if x >= c then Var(x+d, n+d) else Var(x, n+d)
            case Abs(x, t1) => Abs(x, walk(c+1, t1))
            case Appl(t1, t2) => Appl(walk(c, t1), walk(c, t2))
        walk(0, this)

    def subst(j: Int, s: Term): Term =
        def walk(c: Int, t: Term): Term = t match
            case Var(x, n) => if (x==j+c) then s.shift(c) else Var(x, n)
            case Abs(x, t1) => Abs(x, walk(c+1, t1))
            case Appl(t1, t2) => Appl(walk(c,t1), walk(c, t2))
        walk(0, this)

    def substTop(s: Term): Term = this.subst(0, s.shift(1)).shift(-1)

    def eval1(using ctx: Context): Option[Term] = this match
        case Appl(Abs(_, t12), v2) if v2.isVal => Some(t12.substTop(v2))
        case Appl(v1, t2) if v1.isVal => for t2e <- t2.eval1 yield Appl(v1, t2e)
        case Appl(t1, t2) => for t1e <- t1.eval1 yield Appl(t1e, t2)
        case _ => None

    def eval(using ctx: Context): Term = this.eval1 match
        case Some(t) => t.eval
        case None => this


import Term.*

given ctx: Context = Context(("foo", Binding()))

val expr = Appl(Abs("bar", Var(0, 2)), Abs("baz", Var(1,2)))

expr.pretty 
// : String = ((λ bar . bar) (λ baz . foo))
expr.eval.pretty
// : String = (λ baz . foo)