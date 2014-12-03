package fos

import org.scalacheck.Gen._
import org.scalacheck._

/**
  * Created by lukas on 29/11/14.
  */
object TypeGen {

    def genBool = wrap(TypeBool)

    def genNat = wrap(TypeNat)

    def genFun(maxDepth: Int): Gen[Type] = {

        for {
            from <- genType(maxDepth)
            to <- genType(maxDepth)
        } yield {
            TypeFun(from, to)
        }
    }

    def genType(maxDepth: Int) = {
        if (maxDepth == 0) oneOf(genBool, genNat)
        else oneOf(genBool, genNat, genFun(maxDepth - 1))
    }

    def genSimpleType: Gen[Type] = {
        //println("GetSimpleType")
        oneOf(genBool, genNat)
    }

}
