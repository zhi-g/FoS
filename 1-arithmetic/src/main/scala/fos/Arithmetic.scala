package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  /**
   * Expr ::= 'true'
   * | 'false'
   * | 'if' Expr 'then' Expr 'else' Expr
   * | '0'
   * | 'succ' Expr
   * | 'pred' Expr
   * | 'iszero' Expr
   */
  def Expr: Parser[Term] = (
    "true" ^^^ True
    | "false" ^^^ False
    | ("if" ~> Expr) ~ ("then" ~> Expr) ~ ("else" ~> Expr) ^^ { case e1 ~ e2 ~ e3 => If(e1, e2, e3) }
    | numericLit ^^ { case n => intToTerm(n.toInt) }
    | "succ" ~> Expr ^^ { case e1 => Succ(e1) }
    | "pred" ~ Expr ^^ { case "pred" ~e1 => Pred(e1) }
    | "iszero" ~> Expr ^^ { case e1 => IsZero(e1) }
    | failure("illegal start of expression"))
  

  def intToTerm(n: Int): Term = n match {
    case 0 => Zero
    case _ => Succ(intToTerm(n-1))
  }

  def reduce(y: Term) = y match {
    case True => True
    case False => False
    case Zero => Zero
    case If(True, e1, e2) => e1
    case If(False, e1, e2) => e2
    case IsZero(Zero) => True
    case IsZero(Succ(x)) => False
    case Pred(Zero) => Zero
    case Pred(Succ(Zero)) => Zero
  }
    
  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Expr)(tokens) match {
      case Success(trees, _) =>
        
        println(trees.eval)
      case e =>
        println(e)
    }
  }
}
