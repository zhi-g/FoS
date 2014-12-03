package fos

import org.scalacheck.Prop._
import org.scalacheck._

object SuperTermsTest extends Properties("SuperMagicalTerms") with Magic {

    def runTest(test: String, gen: Gen[(Term, Type)]) = {
        property(test) = magic {
            forAllNoShrink(gen) {
                case (value, expected) => magic {

                    val inferencer = new TwoPhaseInferencer
                    val found = inferencer.typeOf(value)
                    if (found != expected) {
                        println(value)
                        println("Expected: " + expected)
                        println("Found   : " + found)
                        false
                    } else true
                }
            }
        }
    }

    runTest("Term", SuperTermGen.genTerm)

}
