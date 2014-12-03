package fos

import org.scalatest.{ Matchers, FunSuite }
import scala.util.parsing.input._

class SimpleTest extends TestSuite {

    def parse(in: String): Term = {
        val tokens = new Infer.lexical.Scanner(in)
        Infer.phrase(Infer.Term)(tokens) match {
            case Infer.Success(res, _) => res
            case Infer.Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    /**
      * Creates a test to check that parsing a string and
      * determining its type yield the expected result.
      */
    def checkType(term: String, tpe: String) = testNoOut(s"$term has type $tpe") {
        val inferencer = new TwoPhaseInferencer
        inferencer.typeOf(parse(term)).toString should equal(tpe)
    }

    /**
      * Creates a test to check that parsing a string and
      * trying to determine its type fails.
      */
    def failsTypeChecking(term: String) = testNoOut(s"$term cannot be typed") {
        val inferencer = new TwoPhaseInferencer
        inferencer.typeOf(parse(term)) should equal(null)
    }

    checkType("\\x. succ x", "(Nat -> Nat)")
    checkType("\\x. if x then 1 else 2", "(Bool -> Nat)")
    checkType("\\x. if x then x else x", "(Bool -> Bool)")
    checkType("(\\b.if b then false else true)", "(Bool -> Bool)")
    checkType("\\x. \\y. if iszero (x y) then 1 else y", "((Nat -> Nat) -> (Nat -> Nat))")
    checkType("let double = \\f.\\x.f(f(x)) in if (double (\\x:Bool. if x then false else true) false) then double (\\x:Nat.succ x) 0 else 0", "Nat")
    failsTypeChecking("(\\f.\\x. let g = f in g(0)) (\\x.if x then false else true) true")
}
