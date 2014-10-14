package fos

import scala.Console._
import Untyped._
import scala.annotation.tailrec

object Repl {

    type Definition = (String, Abstraction)
    type Environment = Map[String, Abstraction]

    val Empty: Environment = Map.empty[String, Abstraction]
    val UsefulDefs: Seq[String] = Seq(
        """let tru = \t. \f. t""",
        """let fls = \t. \f. f""",
        """let test = \l. \m. \n. l m n""",
        """let and = \b. \c. b c fls""",
        """let or = \c. \b. c tru b""",
        """let not = \c. c fls tru""",
        """let pair = \f. \s. \b. b f s""",
        """let fst = \p. p tru""",
        """let snd = \p. p fls""",
        """let c0 = \s. \z. z""",
        """let scc = \n. \s. \z. s (n s z)""")
    val Prompt: String = "lambda> "

    /** Maps from ParseResult types to Option */
    def parse(s: String): Option[Term] = {
        val tokens = new lexical.Scanner(s)
        phrase(Term)(tokens) match {
            case Success(tree, _) => Some(tree)
            case _ => None
        }
    }

    /**
      * We define definitions as the keywork let followed by an equal sign
      * followed by a string that parses to an abstraction
      */
    object Definition {
        val Def = """\s*let\s+(\w+)\s*=(.*)""".r
        def unapply(s: String): Option[(String, Term)] = s match {
            case Def(name, body) => parse(body) map (term => name -> term)
            case _ => None
        }
    }

    /** Matches an expression that parses succesfully to a Term */
    object Expression {
        def unapply(s: String): Option[Term] = parse(s)
    }

    /**
      * Used to match a string that contains "exit" possibly surrounded by
      * spaces
      */
    object Exit extends Throwable {
        def unapply(s: String): Option[Unit] =
            if (s.trim == "exit") Some() else None
    }

    /** Update the environment with the new definition */
    def update(env: Environment, let: Definition): Environment = env + let

    /** Rewrites a term with the abstractions defined in the environmnet */
    def rewrite[T <: Term](t: T)(implicit env: Environment): Term = t match {
        case Variable(x) => env.get(x).getOrElse(Variable(x))
        case Application(fun, arg) => Application(rewrite(fun), rewrite(arg))
        case Abstraction(name, body) => {
            /** Don't rewrite bound variables */
            val newEnv: Environment = env - name.name
            Abstraction(name, rewrite(body)(newEnv))
        }
    }

    def pretty(t: Term, level: Int = 0): String = {
        val string = t match {
            case Variable(x) => x
            case Application(a, b) => pretty(a) + "\n" + pretty(b)
            case Abstraction(name, body) =>
                name + " => \n" + pretty(body, 2)
        }
        string.split("\n").map(" " * level + _).mkString("\n")
    }

    /**
      * Evaluate one line entered by the user and update the environment if
      * needed
      *
      * @param line The line to evaluate
      * @param reducer The term evaluation function
      * @param defs The environment that contains all the named abstractions
      */
    def eval(line: String)(implicit reducer: Term => Term, defs: Environment): Environment =
        line match {
            case Exit() => throw Exit
            case Definition(let) => {
                val (name, term) = let
                val rewritten = rewrite(term)
                val freeVariables = fv(rewritten)

                if (!freeVariables.isEmpty) {
                    val listing = freeVariables.mkString(" ")
                    println(s"Definitions can't have free variables : $listing")
                    defs
                } else {
                    path(rewritten, reducer).last match {
                        case a: Abstraction =>
                            println(s"new definition : $name => $term")
                            update(defs, name -> a)
                        case _ =>
                            println("Illegal definition")
                            defs
                    }
                }
            }
            case Expression(expr) => {
                println("Evaluating expression ...")
                val reductions = path(rewrite(expr), reducer)
                reductions.zipWithIndex foreach {
                    case (tree, i) => println(s"$i. $tree")
                }
                val names = possibleNames(reductions.last)
                println("Possible names for result : " + names.mkString(" "))
                //println(pretty(reductions.last))
                defs
            }
            case _ => {
                println("Couldn't understand what you want ...")
                defs
            }

        }

//    /** Checks that two expressions are structurally equivalent */
//    def equals(t1: Term, t2: Term): Boolean = (t1, t2) match {
//        case (Variable(v1), Variable(v2)) => v1 == v2
//        case (Application(f1, a1), Application(f2, a2)) =>
//            equals(f1, f2) && equals(a1, a2)
//        case (a: Abstraction, Abstraction(p2, b2)) =>
//            val Abstraction(p1, b1) = alpha(a)
//            equals(b1, subst(b2, p2, p1))
//        case (_, _) => false
//    }

    def possibleNames(t: Term)(implicit defs: Environment): Seq[String] = {
        defs.filter { case (name, body) => equals(t, body) }.map(_._1).toSeq
    }

    @tailrec def repl(inputSrc: => String)(implicit reducer: Term => Term, defs: Environment = Empty): Unit = {
        val newDefs = try {
            /* Evaluate one line of user input */
            eval(inputSrc)(reducer, defs)
        } catch {
            case Exit => throw Exit
            case e: Throwable =>
                e.printStackTrace()
                defs
        }
        /** Repeat loop */
        repl(inputSrc)(reducer, newDefs)
    }

    /**
      * To launch use command "console" in sbt and then:
      *     :power
      *     fos.Repl.launch(repl.in.readLine _)
      */
    def launch(readLine: (String => String),
               reducer: Term => Term = reduceCallByValue): Unit = {
        /** Ignore empty lines and exit on Ctrl+D */
        @tailrec def myReadLine: String = {
            val optLine = readLine(Prompt)
            if (optLine == null) throw Exit

            val line = optLine.trim
            if (line.isEmpty) myReadLine else line
        }

        try {
            val preFilled = UsefulDefs.foldLeft(Empty)((e, t) => eval(t)(reducer, e))
            repl(myReadLine)(reducer, preFilled)
        } catch {
            case Exit => println("Good bye")
        }
    }

}
