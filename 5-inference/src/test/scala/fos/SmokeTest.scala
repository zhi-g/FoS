package fos

class SmokeTest extends TestSuite {

    val terms = Seq("true", "false", "if true then true else true", "0",
        "pred 0", "succ 0", "iszero 0", "x", """(\x:Nat.x)""", "t e")

    val types = Seq("Nat", "Bool", "Nat->Bool", "(Nat->Nat)->Bool")

    (terms ++ types) foreach { elem =>
        parseSuccess(elem)
        parseToString(elem)
    }

    val special = Seq("""\x.x""", "let x = t1 in t2")
}
