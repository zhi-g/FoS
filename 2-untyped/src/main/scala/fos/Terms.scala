package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class Variable(name: String) extends Term {
  override def toString(): String = {
    name
  }
}
case class Abstraction(name: String, term: Term) extends Term {
  override def toString(): String = {
    "\\" + name + ". " + term
  }
}
case class Application(term1: Term, term2: Term) extends Term {
  override def toString(): String = {
    "(" + term1 + " " + term2 + ")"
  }
}
case class Parenthesis(term: Term) extends Term {
  override def toString(): String = {
    "(" + term + ")"
  }
}
case class Value(abstr: Abstraction) {
  override def toString(): String = {
    abstr match {
      case Abstraction(name, term) => "\\" + name + ". " + term
      case _ => ""
    }
  }
}
