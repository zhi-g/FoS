package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 
case class True extends Term
case class False extends Term
case class If(cond:Term, then:Term, els:Term) extends Term
case class Zero extends Term
case class Succ(t:Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term
