package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

  //   ... To complete ... 

//to review
case class Variable extends Term {}
case class Abstraction extends Term {}
case class Application extends Term {}
