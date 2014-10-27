package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}

case object False extends Term {
  override def toString() = "false"
}

case object Zero extends Term {
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
case class Abstraction(name: String, typ: Term, term: Term) extends Term {
  override def toString() = "(\\" + name + ": " + typ + ". " + term + ")"
}

case class Application(e1: Term, e2: Term) extends Term {
  override def toString() = "(" + e1 + " " + e2 + ")"
}

case class Paren(t: Term) extends Term {
  override def toString() = "(" + t + ")"
}

case class Let(name: String, typ: Type, min: Term, max: Term) extends Term {
  override def toString() = "Let " + name + ": " + typ + " in " + min + " to " + max
}

case class Paire(e1: Term, e2: Term) extends Term {
  override def toString() = "{" + e1 + ", " + e2 + "}"
}
//   ... To complete ... 
/** Abstract Syntax Trees for types. */
abstract class Type extends Term

case object TypeBool extends Type {
  override def toString() = "Bool"
}
//   ... To complete ... 
case object TypeNat extends Type {
  override def toString() = "Nat"
}

case class TypeFunc(typ1: Type, typ2: Type) extends Type {
  override def toString() = typ1 + " ->" + typ2
}