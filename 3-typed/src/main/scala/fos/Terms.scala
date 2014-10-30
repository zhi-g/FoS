package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class True() extends Term {
  override def toString() = "true"
}

case class False() extends Term {
  override def toString() = "false"
}

case class Zero() extends Term {
  override def toString() = "0"
}

case class Succ(t: Term) extends Term {
  override def toString() = "Succ(" + t + ")"
}

case class Pred(t: Term) extends Term {
  override def toString() = "Pred(" + t + ")"
}

case class IsZero(t: Term) extends Term {
  override def toString() = "isZero(" + t + ")"
}

case class If(cond: Term, thn: Term, els: Term) extends Term {
  override def toString() = "if(" + cond + ") then(" + thn + ") else(" + els + ")"
}

case class Variable(name: String) extends Term {
  override def toString() = name
}
case class Abstraction(name: String, typ: Type, term: Term) extends Term {
  override def toString() = "(\\" + name + ": " + typ + ". " + term + ")"
}

case class Application(e1: Term, e2: Term) extends Term {
  override def toString() = "(" + e1 + " " + e2 + ")"
}

case class Paire(e1: Term, e2: Term) extends Term {
  override def toString() = "{" + e1 + ", " + e2 + "}"
}

case class First(term: Term) extends Term {
  override def toString() = "fst " + term
}

case class Second(term: Term) extends Term {
  override def toString() = "snd " + term
}

case class Inl(term: Term, tpe: Type) extends Term {
  override def toString() = "inl " + term + " as " + tpe
}

case class Inr(term: Term, tpe: Type) extends Term {
  override def toString() = "inr " + term + " as " + tpe
}

case class Case(term1: Term, name1: Variable, term2: Term, name2: Variable, term3: Term) extends Term {
  override def toString() = "case " + term1 + " of inl " + name1 + " => " + term2 + " | inr " + name2 + " => " + term3
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type {
  override def toString() = "Bool"
}

case object TypeNat extends Type {
  override def toString() = "Nat"
}

case class TypeFunc(tpe1: Type, tpe2: Type) extends Type {
  override def toString() = "(" + tpe1 + " -> " + tpe2 + ")"
}

case class TypePaire(tpe1: Type, tpe2: Type) extends Type {
  override def toString() = "{" + tpe1 + " * " + tpe2 + "}"
}

case class TypeSum(tpe1: Type, tpe2: Type) extends Type {
  override def toString() = tpe1 + " + " + tpe2
}
