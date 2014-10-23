package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional {
  def eval: Term
  def reduce: (Term, Int)
}

case object True extends Term {
  def eval = (
    True)
  def reduce = (
    True, 0)
  override def toString(): String = {
    "True"
  }
}

case object False extends Term {
  def eval = (
    False)
  def reduce = (
    False, 0)
  override def toString(): String = {
    "False"
  }
}

case object Zero extends Term {
  def eval = (
    Zero)
  def reduce = (
    Zero, 0)
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

  def reduce = (
    cond.reduce match {
      case (a, 1) => (If(a, thn, els), 1)
      case (a, 2) => (If(a, thn, els), 2)
      case (True, 0) => (thn, 1)
      case (False, 0) => (els, 1)
      case (a, _) => (If(a, thn, els), 2) // we are stuck
    })
  override def toString(): String = {
    "If(" + cond.toString() + ", " + thn.toString() + ", " + els.toString() + ")"
  }
}

case class Succ(t: Term) extends Term {
  def eval = (
    t match {
      case Zero => Succ(Zero)
      case _: Succ | _:  Pred => t.eval match {
        case x: StuckTerm => x
        case x => Succ(x)
      }
      case x: If => Succ(x.eval)
      case StuckTerm(_) => t
      case _ => StuckTerm(Succ(t))
    })
  def reduce = (
    t.reduce match {
      case (a, 1) => (Succ(a), 1)
      case (a, 2) => (Succ(a), 2)
      case (a @ True, 0) => (Succ(a), 2)
      case (a @ False, 0) => (Succ(a), 2)
      case (_, 0) => (this, 0)
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
      case _: Pred => Pred(t.eval).eval
      case x: If => Pred(x.eval).eval
      case StuckTerm(_) => t
      case _ =>
        StuckTerm(Pred(t))
    })
  def reduce = (
    t.reduce match {
      case (a, 1) => (Pred(a), 1)
      case (a, 2) => (Pred(a), 2)
      case (Succ(a), 0) => (a, 1)
      case (a @ True, 0) => (Pred(a), 2)
      case (a @ False, 0) => (Pred(a), 2)
      case (Zero, 0) => (Zero, 1)
      case (a, _) => (Pred(a), 2) // we are stuck?? 
    })
  override def toString(): String = {
    "Pred(" + t.toString() + ")"
  }
}

case class IsZero(t: Term) extends Term {
  def eval = (
    t.eval match {
      case Zero => True
      case _: Succ => False
      case term: StuckTerm => term
      case _ => StuckTerm(IsZero(t))
    })
  def reduce = (
    t.reduce match {
      case (a, 1) => (IsZero(a), 1)
      case (a, 2) => (IsZero(a), 2)
      case (Zero, 0) => (True, 1)
      case (a @ False, 0) => (IsZero(a), 2)
      case (a @ True, 0) => (IsZero(a), 2)
      case _ => (False, 1)
    })
  override def toString(): String = {
    "IsZero(" + t.toString() + ")"
  }
}

case class StuckTerm(t: Term) extends Term {
  def eval = (
    this)
  def reduce = (this, 2)

  override def toString(): String = {
    "Stuck term: " + t.toString()
  }
}
