package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def eval: Term
}

case object True extends Term {
  def eval = (
    True)
  override def toString(): String = {
    "true"
  }
}

case object False extends Term {
  def eval = (
    False)

  override def toString(): String = {
    "false"
  }
}

case object Zero extends Term {
  def eval = (
    Zero)

  override def toString(): String = {
    "Zero"
  }
}

case class If(cond: Term, thn: Term, els: Term) extends Term {
  def eval = (
    cond.eval match {
      case True => thn.eval
      case False => els.eval
      case term: StuckTerm => term
      case _ => StuckTerm(If(cond, thn, els))
    })

  override def toString(): String = {
    "If " + cond.toString() + " then " + thn.toString() + " else " + els.toString()
  }
}

case class Succ(t: Term) extends Term {
  def eval = (
    t match {
      case Zero => Succ(Zero)
      case Succ(_) => Succ(t.eval)
      case x: If => Succ(x.eval)
      case StuckTerm(_) => t
      case _ => StuckTerm(Succ(t))
    })

  override def toString(): String = {
    "Succ(" + t.toString() + ")"
  }
}

case class Pred(t: Term) extends Term {
  def eval = (
    t match {
      case Zero => Zero
      case Succ(term) => term.eval match {
        case True => StuckTerm(Succ(term))
        case False => StuckTerm(Succ(term))
        case x => x
      }
      case Pred(_) => Pred(t.eval).eval
      case x: If => Pred(x.eval).eval
      case StuckTerm(_) => t
      case _ => StuckTerm(Pred(t))
    })

  override def toString(): String = {
    "Pred(" + t.toString() + ")"
  }
}

case class IsZero(t: Term) extends Term {
  def eval = (
    t.eval match {
      case Zero => True
      case Succ(_) => False
      case term: StuckTerm => term
      case _ => StuckTerm(IsZero(t))
    })

  override def toString(): String = {
    "IsZero(" + t.toString() + ")"
  }
}

case class StuckTerm(t: Term) extends Term {
  def eval = (
    this)

  override def toString(): String = {
    "Stuck term : " + t.toString()
  }
}
