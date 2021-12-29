/* Untyped Arithmetic (ch. 4) */

enum Term:
    case True
    case False
    case If(pred: Term, ifTrue: Term, ifFalse: Term)
    case Zero
    case Succ(term: Term)
    case Pred(term: Term)
    case IsZero(term: Term)

    def isNumericVal: Boolean = this match
        case Zero => true
        case Succ(t1) => t1.isNumericVal
        case _ => false

import Term.*

/** Single-step evaluation.
  * 
  * @param t the term to evaluate.
  * @return the result of applying single evaluation rule, when possible.
  */
def eval1(t: Term): Option[Term] = t match
    case If(True, t2, t3) => Some(t2)
    case If(False, t2, t3) => Some(t3)
    case If(t1, t2, t3) => 
        for t1e <- eval1(t1) yield If(t1e, t2, t3)
    case Succ(t1) =>
        for t1e <- eval1(t1) yield Succ(t1e)
    case Pred(Zero) => Some(Zero)
    case Pred(Succ(nv1)) if nv1.isNumericVal => Some(nv1)
    case Pred(t1) =>
        for t1e <- eval1(t1) yield Pred(t1e)
    case IsZero(Zero) => Some(True)
    case IsZero(Succ(nv1)) => Some(False)
    case IsZero(t1) =>
        for t1e <- eval1(t1) yield IsZero(t1e)
    case _ => None

/** Multi-step evaluation.
  * 
  * @param t the term to evaluate
  * @return a normal form of the term.
  */
def eval(t: Term): Term = eval1(t) match
    case Some(t1) => eval(t1)
    case None => t

/* some examples */
eval(Succ(Pred(Succ(Zero))))
eval(IsZero(Pred(Succ(Zero))))
eval(IsZero(False))