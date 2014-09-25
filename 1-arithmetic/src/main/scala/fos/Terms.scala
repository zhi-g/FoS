package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def eval: Term
}

case object True extends Term {
  def eval = (
    True)
}
case object False extends Term {
  def eval = (
    False)
}
case object Zero extends Term {
  def eval = (
    Zero)
}

case class If(cond: Term, thn: Term, els: Term) extends Term {
  def eval = (
    cond.eval match {
      case True => thn.eval
      case False => els.eval
      case _ => StuckTerm(If(cond, thn, els))
    })
}

case class Succ(t: Term) extends Term {
  def eval = (
    t match {
      case Succ(_) => Succ(t.eval)
      case x: If => Succ(x.eval)
      case Zero => Succ(Zero)
      case _ => StuckTerm(Succ(t))
    })
}

case class Pred(t: Term) extends Term {
  def eval = (
    t match {
      case Succ(term) => (Succ(term.eval)).eval
      case Pred(Succ(x)) => x.eval
      case Pred(Zero) => Zero
      case Pred(term) => Pred(t.eval).eval
      case x: If => Pred(x.eval).eval
      case StuckTerm(term) => StuckTerm(t)
      case _ => StuckTerm(Pred(t))
    })
}

case class IsZero(t: Term) extends Term {
  def eval = (
    t.eval match {
      case Zero => True
      case Succ(term) => False
      case _ => StuckTerm(IsZero(t))
    })
}

case class StuckTerm(t: Term) extends Term {
  def eval = (
    StuckTerm(t))
}
