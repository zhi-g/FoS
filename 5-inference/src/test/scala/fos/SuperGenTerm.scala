package fos

import org.scalacheck.Gen._
import org.scalacheck._

object SuperTermGen {

    var namesCounter = 0
    def freshName = {
        namesCounter += 1
        "x" + namesCounter
    }

    type VT = (Var, Type)
    type TT = (Term, Type)

    var variables: Map[Var, Type] = Map.empty
    var usedVariables: Map[Var, Boolean] = Map.empty

    implicit def Type2TypeTree(tpe: Type): TypeTree = tpe match {
        case TypeNat => NatType
        case TypeBool => BoolType
        case TypeFun(from, to) => FunType(Type2TypeTree(from), Type2TypeTree(to))
    }

    /**
      * Generate random types
      */
    def genType: Gen[Type] = oneOf(genSimpleType, genFunctionType)

    def genSimpleType: Gen[Type] = oneOf(TypeNat, TypeBool)

    def alternateFunType: Gen[Type] = for {
        n <- chooseNum(0, 5)
        first <- genSimpleType
        types <- listOfN(n, genSimpleType)
    } yield types.foldLeft(first) { case (a, t) => TypeFun(t, a) }

    def genFunctionType: Gen[Type] = for {
        from <- genSimpleType
        to <- genType
    } yield TypeFun(from, to)

    /**
      * Generate random terms whose type is Nat
      */
    def genTermNat: Gen[TT] =
        if (!variables.isEmpty) oneOf(genTermNatFromVariable, genTermNatFromScratch)
        else genTermNatFromScratch

    def genTermNatFromVariable: Gen[TT] = for {
        (v, vTpe) <- oneOf(variables.toSeq)
        term <- vTpe match {
            case TypeNat => oneOf(Succ(v), Pred(v))
            case TypeBool =>
                for {
                    (thn, _) <- genTermNat
                    (els, _) <- genTermNat
                } yield If(v, thn, els)
            case _ => ???
        }
    } yield {
        usedVariables += ((v, true))
        (term, TypeNat)
    }

    def genTermNatFromScratch: Gen[TT] = for {
        n <- chooseNum(1, 5)
        ops <- listOfN(n, (t: Term) => Succ(t))
    } yield (ops.foldLeft(Zero(): Term) { (a, e) => e(a) }, TypeNat)

    /**
      * Generate random terms whose type is Bool
      */
    def genTermBool: Gen[TT] =
        if (!variables.isEmpty) oneOf(genTermBoolFromVariable, genTermBoolFromScratch)
        else genTermBoolFromScratch

    def genTermBoolFromVariable: Gen[TT] = for {
        (v, vTpe) <- oneOf(variables.toSeq)
        term <- vTpe match {
            case TypeNat => IsZero(v)
            case TypeBool => If(v, v, v)
            case _ => ???
        }
    } yield {
        usedVariables += ((v, true))
        (term, TypeBool)
    }

    def genTermBoolFromScratch: Gen[TT] = for {
        t <- oneOf(True(), False())
    } yield (t, TypeBool)

    /**
      * Generate random abstractions
      */
    def genAbs(from: Type, to: Type): Gen[TT] = {
        val name = freshName
        for {
            variable <- { val v = Var(name); variables += ((v, from)); usedVariables += ((v, false)); v }
            (body, actualBodyTpe) <- genTerm(to)
            (arg, _) <- genTermWithoutVars(from)
            abs <- Abs(name, EmptyType, body)
        } yield {
            if (usedVariables(variable)) (abs, TypeFun(from, actualBodyTpe))
            else (App(abs, arg), actualBodyTpe)
        }
    }

    /**
      * Generate term of type `tpe`.
      */
    def genTerm(tpe: Type): Gen[TT] = tpe match {
        case TypeNat =>
            genTermNat

        case TypeBool =>
            genTermBool

        case TypeFun(from, to) =>
            genAbs(from, to)

        case _ => ???
    }

    def genTermWithoutVars(tpe: Type): Gen[TT] = tpe match {
        case TypeNat => genTermNatFromScratch
        case TypeBool => genTermBoolFromScratch
        case _ => ???
    }

    /**
      * Generates a term of an arbitrary type
      */
    def genTerm: Gen[TT] = for {
        tpe <- genType
        term <- genTerm(tpe)
    } yield {
        variables = Map.empty
        usedVariables = Map.empty
        term
    }
}
