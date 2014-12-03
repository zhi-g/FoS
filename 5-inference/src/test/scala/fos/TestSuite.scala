package fos

import fos.Infer._
import org.scalatest.{ Matchers, FunSuite }
import scala.language.implicitConversions

trait TestSuite extends FunSuite with Matchers with Magic {

    implicit def s2term(s: String): Term =
        parseOpt(Infer.Term)(s) getOrElse parse(Infer.Type)(s)

    def testNoOut[T](title: String)(thunk: => T) =
        test(title)(noSillyOutput(thunk))

    /** Return Some(term) on success or None if the parsing failed */
    def parseOpt[T <: Term](parser: Parser[T])(in: String): Option[T] = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => Some(res)
            case Failure(msg, _) => None
            case _ => None
        }
    }

    /** Return directly the element on successful parsing or fail the test */
    def parse[T <: Term](parser: Parser[T])(in: String): T = {
        val tokens = new lexical.Scanner(in)
        phrase(parser)(tokens) match {
            case Success(res, _) => res
            case Failure(msg, _) => fail(msg)
            case _ => fail("Error")
        }
    }

    /**
      * Test that parsing in returns something
      */
    def parseSuccess(in: String) = {
        test(s"parse($in) should succeed") {
            val termOpt = parseOpt(Infer.Term)(in) orElse parseOpt(Infer.Type)(in)
            termOpt.isDefined should be(true)
        }
    }

    /**
      * Test that parsing bad fails
      */
    def parseError(bad: String) = {
        test(s"parse($bad) should fail") {
            parseOpt(Infer.Type)(bad) should be(None)
            parseOpt(Infer.Term)(bad) should be(None)
        }
    }

    /**
      * Test that parsing in returns out
      */
    def testParse[T <: Term](in: String, out: T) = {
        val parser = if (out.isInstanceOf[TypeTree]) Infer.Type else Infer.Term
        test(s"parse($in) == $out") {
            parse(parser)(in) should equal(out)
        }
    }

    /**
      * Test that calling toString on a term returns the correct value
      */
    def testToString(in: Term, out: String) = {
        test(s"($in).toString == out") {
            in.toString should equal(out)
        }
    }

    /**
      * Test that parsing and calling toString returns the same term
      */
    def parseToString(in: String) = {
        test(s"parse($in).toString == $in") {
            val term = parseOpt(Infer.Term)(in) getOrElse parse(Infer.Type)(in)
            term.toString should equal(in)
        }
    }

    /**
      * Tests that a term is correctly typed
      */
    def testTypeOf(in: Term, tpe: TypeTree) = noSillyOutput {
        testNoOut(s"$in should typecheck to $tpe") {
            val inferencer = new TwoPhaseInferencer()
            val inferred = inferencer.typeOf(in)
            val expected = inferencer.toType(tpe)
            inferred should equal(expected)
        }
    }

    def testTypeOf(in: Term, tpe: String): Unit = testTypeOf(in, parse(Infer.Type)(tpe))

    /**
      * Tests that a given term does not type checks
      */
    def testBadType(in: Term) =
        test(s"$in should not typecheck") {
            val infer = new TwoPhaseInferencer()

            val thrown = an[infer.TypeError] should be thrownBy {
                val infer.TypingResult(tp, c) = infer.collect(Nil, in)
                infer.unify(c)
            }
        }

    def testCollectConstraints(in: Term, constTest: (Type, Type)*): Unit =
        testNoOut(s"collect($in) should return constraints $constTest") {
            val inferencer = new TwoPhaseInferencer()
            val constResult = inferencer.collect(Nil, in).c
            constResult diff constTest should equal(Seq())
            constTest diff constResult should equal(Seq())
        }
}
