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
    | Term2 ~ rep1(Term2) ^^ { case e1 ~ e2 => (e1 :: e2).reduceLeft((x: Term, y: Term) => Application(x, y)) }
    | "(" ~> Term <~ ")" ^^ { case e1 => Parenthesis(e1) }
    | ident ^^ { case x => Variable(x) }
    | failure("illegal start of term"))

  def Term2: Parser[Term] = (
    ident ^^ { case x => Variable(x) }
    | ("\\" ~> ident) ~ ("." ~> Term) ^^ { case e1 ~ e2 => Abstraction(e1, e2) }
    | "(" ~> Term <~ ")" ^^ { case e1 => Parenthesis(e1) } //No need of parenthesis => it adds more steps to our reduction
    | failure("illegal start of term"))

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

  /**
   * Alpha-conversion:
   *  @param x new name
   *  @param y old name
   *
   */
  def alpha(t: Term, x: String, y: String): Term = (
    t match {
      case Application(t1, t2) => Application(alpha(t1, x, y), alpha(t2, x, y))
      case Parenthesis(t1) => Parenthesis(alpha(t1, x, y))
      //?? what happens when we have 2 abstractions of old name? \x. (\x. x) x
      case Abstraction(name, t1) => if (name == x || name == y) t else Abstraction(name, alpha(t1, x, y))
      case Variable(name) => if (name == y) new Variable(x) else t
    })

  /** Find a name that is not already in the term*/
  def newName(t: Term): String = {
    def newNameAcc(name: String, i: Int, t: Term): String = if (fv(t).contains(name + i)) newNameAcc(name, i + 1, t) else (name + i)
    t match {
      case Abstraction(name, term) =>
        newNameAcc(name, 1, term)
      case _ =>
        throw NoRuleApplies(t)
    }
  }

  /** Substitution rule */
  def subst(t: Term, x: String, s: Term): Term = {
    t match {
      case Variable(name) => if (x == name) s else t
      case Abstraction(name, term) => {
        if (x == name) {
          t
        } else if (!fv(s).contains(name)) {
          Abstraction(name, subst(term, x, s))
        } else {
          val name1 = newName(t)
          Abstraction(name1, subst(alpha(term, name1, name), x, s))
        }
      }
      case Application(t1, t2) => Application(subst(t1, x, s), subst(t2, x, s))
      case Parenthesis(t1) => Parenthesis(subst(t1, x, s))
    }
  }

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case Variable(name) => {
      throw NoRuleApplies(t)
    }
    case Abstraction(name, term) => {
      Abstraction(name, reduceNormalOrder(term))
    }
    case Application(t1, t2) => {
      (t1, t2) match {
        case (Abstraction(name, term), _) => subst(term, name, t2)
        case (Parenthesis(term), _) => reduceNormalOrder(Application(term, t2))
        case (_, Parenthesis(term)) => reduceNormalOrder(Application(t1, term))
        case (Variable(_), Variable(_)) => throw NoRuleApplies(t)
        case (Variable(_), _) => Application(t1, reduceNormalOrder(t2))
        case _ => Application(reduceNormalOrder(t1), t2)
      }
    }
    case Parenthesis(term) => {
      Parenthesis(reduceNormalOrder(term))
    }
    case _ =>
      throw NoRuleApplies(t)
  }

  /**
   * Call by value reducer.
   * The outer most redex is reduced and its right handside is a value
   */
  def reduceCallByValue(t: Term): Term = t match {
    //Get rid of parenthesis
    case Application(Parenthesis(t1), t2) => reduceCallByValue(Application(t1, t2))
    case Application(t1, Parenthesis(t2)) => reduceCallByValue(Application(t1, t2))
    //If the right handside is a value, we can substitute in the abstraction
    case Application(Abstraction(name, term), t2) if isValue(t2) => subst(term, name, t2) //ok
    // if t1 is a value we reduce the right handside. Basically same as before we just need to reduce the right side
    case Application(t1, t2) if isValue(t1) => Application(t1, reduceCallByValue(t2))
    // otherwise we need to reduce the left side until it's a value.
    case Application(t1, t2) => Application(reduceCallByValue(t1), t2)
    //get rid of parenthesis
    case Parenthesis(t1) => reduceCallByValue(t1)
    case _ =>
      throw NoRuleApplies(t)
  }
  def isValue(t: Term): Boolean = t match {
    case Variable(_) => true
    case Abstraction(_, _) => true
    case Parenthesis(a) => isValue(a)
    case _ => false
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
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}
