package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = (
    ("\\" ~> ident) ~ ("." ~> Term) ^^ { case e1 ~ e2 => Abstraction(e1, e2) }
    | Term2 ~ rep1(Term2) ^^ { case e1 ~ e2 => parseList((e1 :: e2).reverse) }
    | "(" ~> Term <~ ")" ^^ { case e1 => Parenthesis(e1) }
    | ident ^^ { case x => Variable(x) }
    | failure("illegal start of term"))

  def Term2: Parser[Term] = (
    ident ^^ { case x => Variable(x) }
    | ("\\" ~> ident) ~ ("." ~> Term) ^^ { case e1 ~ e2 => Abstraction(e1, e2) }
    | "(" ~> Term <~ ")" ^^ { case e1 => Parenthesis(e1) }
    | failure("illegal start of term"))

  //   ... To complete ... 

  def parseList(terms: List[Term]): Term = (
    terms match {
      case x :: Nil => x
      case x :: xs => Application(parseList(xs), x)
    })

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Free variables computation */
  def fv(t: Term): Set[String] = (
    t match {
      case Variable(x) => Set(x)
      case Application(t1, t2) => fv(t1).union(fv(t2))
      case Abstraction(x, t) => fv(t) - x
      case Parenthesis(t) => fv(t)
    })

  /** Alpha-conversion */
  def alpha(t: Term, x: String): Term = (
      t match {
        case Application(t1, t2) => Application(alpha(t1,x), alpha(t2,x))
        case Parenthesis(t1) => Parenthesis(alpha(t1,x))
        case Abstraction(name, t1) => if (name == x) t else Abstraction(name, alpha(t1, x))
        case Variable(name) => new Variable(x)    
      }
      
    )

  /** Find a name that is not already in the term*/
  def newName(t: Term): String = (
    t match {
      case Abstraction(name, term) =>
        var i = 1
        var name1 = name + i
      
        while(fv(t).contains(name1)) {
          i = i+1
          name1 = name + i
        }    
        return name1
      case _ =>
        throw NoRuleApplies(t)
    })

  /** Substitution rule */
  def subst(t: Term, x: String, s: Term): Term = (
    t match {
      case Variable(name) => if (x == name) s else t
      case Abstraction(name, term) => {
        if (x == name) {
          t
        } else if (!fv(s).contains(name)) {
          Abstraction(name, subst(term, x, s))
        } else {
          val name1 = newName(t)
          Abstraction(name1, subst(alpha(term, name1),x,t))
        }
      }
      case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))

    })

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case Variable(name) => t
    case Abstraction(name, term) => t
    case Application(t1, t2) => t
    case Parenthesis(term) => t

    case _ =>
      throw NoRuleApplies(t)
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    //   ... To complete ... 
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
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
        println(trees)
      //println("normal order: ")
      /*for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)*/

      case e =>
        println(e)
    }
  }
}
