package fos

import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.Matchers

/**
  * Created by lukas on 27/11/14.
  */
class TypeInferenceTest extends Properties("TypeInference") with Matchers with Magic {

    //  runTestGen("Term from type", TermGen.genAnyBool(Set.empty, 3, false))
    //  runTestGen("Term from type", TermGen.genAnyNat(Set.empty, 3, false))

    runTestGen("Term of type", TermGen.genTerm)

    def runTestGen(str: String, gen: Gen[(Term, Type)]) = {

        val inf = new TwoPhaseInferencer
        property(str) = forAll[(Term, Type), Boolean](gen) {
            case t => magic {
                t match {
                    case (t: Term, tpe: Type) =>
                        println(t + " -- " + tpe)
                        val foundType = inf.typeOf(t)
                        println("Found type : " + foundType)
                        foundType equals tpe

                    case _ =>
                        false
                }
            }
        }
    }

}
