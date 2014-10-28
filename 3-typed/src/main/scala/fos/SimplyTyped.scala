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

  /** Free variables computation */
  def fv(t: Term): Set[String] = (
    t match {
      case Variable(x) => Set(x)
      case Application(t1, t2) => fv(t1).union(fv(t2))
      case Abstraction(x, tpe, t) => fv(t) - x
      case Paire(e1, e2) => fv(e1).union(fv(e2))
      case First(e) => fv(e)
      case Second(e) => fv(e)
      case IsZero(e) => fv(e)
      case Pred(e) => fv(e)
      case Succ(e) => fv(e)
    })

  /**
   * Alpha-conversion:
   *  @param x new name
   *  @param y old name
   *
   */
  def alpha(t: Term, x: String, y: String): Term = (
    t match {
      case Application(t1, t2) => Application(alpha(t1, x, y), alpha(t2, x, y))
      case Abstraction(name, tpe, t1) => if (name == x || name == y) t else Abstraction(name, tpe, alpha(t1, x, y))
      case Variable(name) => if (name == y) new Variable(x) else t
      case Paire(e1, e2) => Paire(alpha(e1, x, y), alpha(e2, x, y))
      case First(e) => First(alpha(e, x, y))
      case Second(e) => Second(alpha(e, x, y))
      case IsZero(e) => IsZero(alpha(e, x, y))
      case Pred(e) => Pred(alpha(e, x, y))
      case Succ(e) => Succ(alpha(e, x, y))
    })

  /** Find a name that is not already in the term*/
  def newName(t: Term): String = {
    def newNameAcc(name: String, i: Int, t: Term): String = if (fv(t).contains(name + i)) newNameAcc(name, i + 1, t) else (name + i)
    t match {
      case Abstraction(name, tpe, term) =>
        newNameAcc(name, 1, term)
      case _ =>
        throw NoRuleApplies(t)
    }
  }

  /** Substitution rule */
  def subst(t: Term, x: String, s: Term): Term = {
    t match {
      case Variable(name) => if (x == name) s else t
      case Abstraction(name, tpe, term) => {
        if (x == name) {
          t
        } else if (!fv(s).contains(name)) {
          Abstraction(name, tpe, subst(term, x, s))
        } else {
          val name1 = newName(t)
          Abstraction(name1, tpe, subst(alpha(term, name1, name), x, s))
        }
      }
      case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
      case Paire(e1, e2) => Paire(subst(e1, x, s), subst(e2, x, s))
      case First(e) => First(subst(e, x, s))
      case Second(e) => Second(subst(e, x, s))
    }
  }

  /** Call by value reducer. */
  def reduce(t: Term): Term = t match {
    //Computation rules: 
    case Application(Abstraction(name, tpe, term), t2) if isValue(t2) => subst(term, name, t2)
    case Pred(Succ(x)) if isNumericVal(x) => x
    case Pred(Zero) => Zero
    case If(True, thn, _) => thn
    case If(False, _, els) => els
    case IsZero(Zero) => True
    case IsZero(Succ(x)) if isNumericVal(x) => False

    //Congruence rules:
    case Application(t1, t2) if isValue(t1) => Application(t1, reduce(t2))
    case Application(t1, t2) => Application(reduce(t1), t2)
    case If(cond, thn, els) => If(reduce(cond), thn, els)
    case IsZero(e) => IsZero(reduce(e))
    case Pred(e) => Pred(reduce(e))
    case Succ(e) => Succ(reduce(e))
    
    //Reduction rules for pairs: 
    case First(Paire(e1,e2)) if isValue(e1) && isValue(e2) => e1
    case Second(Paire(e1,e2)) if isValue(e1) && isValue(e2) => e2
    case First(e) => First(reduce(e))
    case Second(e)=> Second(reduce(e))
    case Paire(e1,e2) if !isValue(e1) => Paire(reduce(e1), e2)
    case Paire(e1,e2) => Paire(e1,reduce(e2))

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
