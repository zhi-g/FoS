package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 

//to review
case class Variable(name: String) extends Term {}
case class Abstraction(name: String, term: Term) extends Term {}
case class Application(term1: Term, term2: Term) extends Term {}
case class Parentesis(term: Term) extends Term
