package fos

class Typer extends TestSuite {
    val terms = Map(
        """\x.if x then x else x""" -> "Bool->Bool",
        """(\y.\x.if x then x else x) succ 0""" -> "Bool->Bool",
        """\x. let y = succ x in y""" -> "Nat->Nat",
        """\x. let x = x in succ x""" -> "Nat->Nat",
        """\x. x (let y = x in y 0)""" -> "(Nat->Nat)->Nat",
        """\x. (\y. succ y) x""" -> "Nat->Nat",
        """(\z.(\y.\x.if x then x else x)) true succ 0""" -> "Bool->Bool",
        """\x.\y. ((\a. (\b. succ b)) if true then x else y) y""" -> "Nat->Nat->Nat",
        """\b. (let x = succ b in (\y. 0)) true""" -> "Nat->Nat",
        """let double = \f.\x.f(f(x)) in 
            if (double (\x:Bool. if x then false else true) false) 
                then double (\x:Nat.succ x) 0 
                else 0""" -> "Nat",
        """let f= \x.x in let g = f in if g true then f 0 else f f 0""" -> "Nat",
        """let triple = \f.\x.f(f(f(x))) in let double = \f.\x.f(f(x)) in
            if (triple (\x:Bool. if x then false else true) false) then
                triple (\x:Nat. succ x) 0
                else 0""" -> "Nat",
        """let a = \x.x in
                let b = \x. \y. x in
                    let c = true in
                        a b c succ 0""" -> "Bool")

    for ((term, tpe) <- terms) {
        testTypeOf(term, tpe)
    }

    val badTerms = List(
        """succ true""",
        """let f= \g.g in if f true then f 0 else f f true""",
        """if true then 0 else false""",
        """let dummy = f in dummy""",
        """let dummy = 4 in let f = 45 in d""")

    for (t <- badTerms) testBadType(t)
}
