package fos

import org.scalacheck.Prop._
import org.scalacheck._

object TypeInfTest extends Properties("Type Inference") with Magic {

    def runTest(test: String, gen: Gen[(Term, Type)]) = {
        property(test) = magic {
            forAllNoShrink(gen) {
                case (value, expected) => magic {
                    val inferencer = new TwoPhaseInferencer
                    inferencer.typeOf(value) == expected
                }
            }
        }
    }

    runTest("Abstractions", TreeGenerator.genAbs)
    runTest("Conditionals", TreeGenerator.genIf)
    runTest("Numbers", TreeGenerator.genInt(TreeGenerator.zz))
    runTest("Arbitrary term", TreeGenerator.genTerm)
}
