package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
    "pred", "iszero", "let", "in", "fst", "snd")

  /**
   * Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = positioned(
    SimpleTerm ~ rep1(SimpleTerm) ^^ { case e1 ~ e2 => (e1 :: e2).reduceLeft((t1: Term, t2: Term) => Application(t1, t2)) }
      | failure("illegal start of term"))

  /**
   * SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   */
  def SimpleTerm: Parser[Term] = positioned(
    "true" ^^^ True
      | "false" ^^^ False
      | numericLit ^^ { case n => intToTerm(n.toInt) }
      | "succ" ~> Term ^^ { case e => Succ(e) }
      | "pred" ~> Term ^^ { case e => Pred(e) }
      | "iszero" ~> Term ^^ { case e => IsZero(e) }
      | ("if" ~> Term) ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ { case e1 ~ e2 ~ e3 => If(e1, e2, e3) }
      | ident ^^ { case e => Variable(e) }
      | ("\\" ~> ident) ~ (":" ~> Type) ~ ("." ~> Term) ^^ { case e1 ~ e2 ~ e3 => Abstraction(e1, e2, e3) }
      | "(" ~> Term <~ ")" ^^ { case e => e }
      | ("let" ~> ident) ~ (":" ~> Type) ~ ("=" ~> Term) ~ ("in" ~> Term) ^^ { case name ~ tpe ~ t1 ~ t2 => Application(Abstraction(name, tpe, t1), t2) }
      | ("{" ~> Term) ~ (Term <~ "}") ^^ { case e1 ~ e2 => Paire(e1, e2) }
      | "fst" ~> Term ^^ { case e => First(e) }
      | "snd" ~> Term ^^ { case e => Second(e) }
      | failure("illegal start of simple term"))

  def intToTerm(n: Int): Term = n match {
    case 0 => Zero
    case _ => Succ(intToTerm(n - 1))
  }
  /**
   * Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = positioned(
    SimpleType ~ ("->" ~> Type).* ^^ { case e1 ~ e2 => (e1 :: e2).reduceRight((tpe1: Type, tpe2: Type) => TypeFunc(tpe1, tpe2)) }
      | SimpleType ~ ("*" ~> Type).+ ^^ { case e1 ~ e2 => (e1 :: e2).reduceRight((tpe1: Type, tpe2: Type) => TypePaire(tpe1, tpe2)) }
      | failure("illegal start of type"))

  def SimpleType: Parser[Type] = positioned(
    "Bool" ^^^ TypeBool
      | "Nat" ^^^ TypeNat
      | "(" ~> Type <~ ")" ^^ { case tpe => tpe }
      | failure("illegal start of type"))

  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(pos: Position, msg: String) extends Exception(msg) {
    override def toString =
      msg + "\n" + pos.longString
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  /** Is the given term a numeric value? */
  def isNumericVal(t: Term): Boolean = t match {
    case Zero => true
    case Succ(x) => isNumericVal(x)
    case _ => false
  }

  /** Is the given term a value? */
  def isValue(t: Term): Boolean = t match {
    case True => true
    case False => true
    case _: Abstraction => true
    case Paire(e1, e2) => isValue(e1) && isValue(e2)
    case _ => isNumericVal(t)
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match {
    case True | False =>
      TypeBool
    //   ... To complete ... 
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    phrase(Term)(tokens) match {
      case Success(trees, _) =>
        try {
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
