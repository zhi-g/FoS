package fos

import org.scalacheck.Gen._
import org.scalacheck._

/**
  * Created by lukas on 30/11/14.
  */
object TermGen {
    import TreeGenerator._

    val DEPTH = 2
    type TT = (Term, Type)
    type STT = Set[(Term, Type)]
    type GT = Gen[Term]

    var i = 0

    def freshName = {
        i = i + 1
        "x" + i
    }

    def chooseOneFrom(gen: Option[GT], tpe: Type)(implicit v: STT): GT = {

        val values = gen match {
            case Some(g) => v.filter((t: (Term, Type)) => t._2 == tpe).map(t => wrap(t._1)) + g
            case None => v.filter((t: (Term, Type)) => t._2 == tpe).map(t => wrap(t._1))
        }
        for {
            gen <- oneOf(values.toList)
            value <- gen
        } yield value
    }

    //Generates a simple boolean: true, false or iszero of a simple Nat
    def genBool(toUse: Option[(Term, Type)])(implicit v: STT, explicitType: Boolean): GT = {

        def genTrue: GT = wrap(True())
        def genFalse: GT = wrap(False())
        def genIsZero(toUse: Option[(Term, Type)])(implicit v: STT): GT = {
            for (natTerm <- genNat(toUse)(v, DEPTH, explicitType)) yield IsZero(natTerm)
        }

        toUse match {
            case None => chooseOneFrom(Some(oneOf(genTrue, genFalse, genIsZero(None))), TypeBool)
            case Some((variable, tpe)) => tpe match {
                case TypeBool => wrap(variable)
                case TypeNat => for (term <- genNat(toUse)(v, DEPTH, explicitType)) yield IsZero(term)
                case TypeFun(t1, TypeBool) => for (term <- getGenerator(t1, Set.empty)(v, DEPTH, explicitType)) yield App(variable, term)
                case TypeFun(t1, TypeNat) => for (term <- getGenerator(t1, Set.empty)(v, DEPTH, explicitType)) yield IsZero(App(variable, term))
                case TypeFun(t1, t2) => for {
                    term1 <- getGenerator(t1, Set.empty)(v, DEPTH, explicitType)
                    term2 <- getGenerator(TypeBool, Set((App(variable, term1), t2)))(v, DEPTH, explicitType)
                } yield term2
                case _ => sys.error(s"Type $tpe is not supported")
            }
        }
    }

    def genAnyBool(toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        if (toUse.size > 1) oneOf(genIf(TypeBool, toUse), genApp(TypeBool, toUse))
        else {
            val head = toUse.toList match {
                case (term, typ) :: Nil => Some(term, typ)
                case Nil => None
                case _ => sys.error("Invalid term")
            }
            if (maxDepth == 0) genBool(head)
            else oneOf(genIf(TypeBool, toUse)(v, maxDepth - 1, explicitType), genBool(head), genApp(TypeBool, toUse)(v, maxDepth - 1, explicitType))
        }
    }

    //Generates a simple nat: zero, succ or pred
    def genNat(toUse: Option[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {

        def genZero: GT = wrap(Zero())

        def genSucc(toUse: Option[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
            for (natTerm <- genNat(toUse)) yield Succ(natTerm)
        }

        def genPred(toUse: Option[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
            for (natTerm <- genNat(toUse)) yield Pred(natTerm)
        }

        toUse match {
            case None => if (maxDepth == 0) chooseOneFrom(Some(genZero), TypeNat) else chooseOneFrom(Some(oneOf(genZero, genSucc(toUse)(v, maxDepth - 1, explicitType), genPred(toUse)(v, maxDepth - 1, explicitType))), TypeNat)
            case Some((variable, tpe)) => tpe match {
                case TypeNat => if (maxDepth == 0) wrap(variable) else oneOf(genSucc(toUse)(v, maxDepth - 1, explicitType), genPred(toUse)(v, maxDepth - 1, explicitType), wrap(variable))
                case TypeBool => genIf(TypeNat, Set((variable, tpe)))
                case TypeFun(t1, TypeNat) => for (term <- getGenerator(t1, Set.empty)) yield App(variable, term)
                case TypeFun(t1, TypeBool) => for {
                    arg <- getGenerator(t1, Set.empty)
                    term <- genIf(App(variable, arg), TypeNat, Set.empty)
                } yield term
                case TypeFun(t1, t2) => for {
                    term1 <- getGenerator(t1, Set.empty)(v, DEPTH, explicitType)
                    term2 <- getGenerator(TypeNat, Set((App(variable, term1), t2)))(v, DEPTH, explicitType)
                } yield term2
                case _ => sys.error(s"Type $tpe is not supported")
            }
        }
    }

    def genAnyNat(toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        if (toUse.size > 1) oneOf(genIf(TypeNat, toUse), genApp(TypeNat, toUse))
        else {
            val head = toUse.toList match {
                case (term, typ) :: Nil => Some(term, typ)
                case Nil => None
                case _ => sys.error("Invalid term")
            }
            if (maxDepth == 0) genNat(head)
            else oneOf(genIf(TypeNat, toUse)(v, maxDepth - 1, explicitType), genNat(head), genApp(TypeNat, toUse)(v, maxDepth - 1, explicitType))
        }
    }

    def genIf(cond: Term, tpe: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        val list = toUse.toList

        for {
            s1 <- choose(0, list.length)
            thnTerm <- getGenerator(tpe, list.slice(0, s1).toSet)
            elsTerm <- getGenerator(tpe, list.slice(s1, list.length).toSet)
        } yield {
            If(cond, thnTerm, elsTerm)
        }
    }

    def genIf(tpe: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        val list = toUse.toList

        for {
            s1 <- choose(0, list.length)
            s2 <- choose(s1, list.length)
            cond <- genAnyBool(list.slice(0, s1).toSet)
            thnTerm <- getGenerator(tpe, list.slice(s1, s2).toSet)
            elsTerm <- getGenerator(tpe, list.slice(s2, list.length).toSet)
        } yield {
            If(cond, thnTerm, elsTerm)
        }
    }

    def genApp(tpe: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        val list = toUse.toList

        for {
            s1 <- choose(0, list.length)
            argType <- TypeGen.genSimpleType
            abs <- genAbs(argType, tpe, list.slice(0, s1).toSet)
            arg <- getGenerator(argType, list.slice(s1, list.length).toSet)
        } yield App(abs, arg)
    }

    def genAbs(from: Type, to: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        val varName = freshName
        val variable = (Var(varName), from)

        for {
            body <- getGenerator(to, toUse + variable)(v + variable, maxDepth, explicitType)
        } yield {
            Abs(varName, EmptyType, body)
        }
    }

    def genTopAbs(from: Type, to: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = {
        val varName = freshName
        val variable = (Var(varName), from)

        for {
            body <- getGenerator(to, toUse + variable)(v + variable, maxDepth, explicitType)
        } yield {
            Abs(varName, from, body)
        }
    }

    def getGenerator(tpe: Type, toUse: Set[(Term, Type)])(implicit v: STT, maxDepth: Int, explicitType: Boolean): GT = tpe match {
        case TypeBool => genAnyBool(toUse)
        case TypeNat => genAnyNat(toUse)
        case TypeFun(from, to) => genTopAbs(from, to, toUse)
        case _ => sys.error("Unknown Type : " + tpe)
    }

    def genTerm: Gen[(Term, Type)] = {
        val vars: Set[(Term, Type)] = Set.empty
        val v: STT = Set.empty
        for {
            tpe <- TypeGen.genType(5)
            gen <- getGenerator(tpe, vars)(v, DEPTH, true)
        } yield (gen, tpe)

    }

}
