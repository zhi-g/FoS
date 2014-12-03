package fos

import org.scalacheck._
import Arbitrary._
import Gen._

object TreeGenerator {

    object Fresh {
        var count = 0
        def name = {
            val n = "x" + count
            count += 1
            n
        }
    }

    def genFreshName: Gen[String] = Fresh.name

    type TT = (Term, Type)
    type TTT = ((Term, Type), Option[Type])
    val zz = (Zero(), TypeNat)

    sealed trait PredSuccGen extends (Term => Term)

    object PredGen extends PredSuccGen {
        def apply(t: Term): Term = Pred(t)
    }
    object SuccGen extends PredSuccGen {
        def apply(t: Term): Term = Succ(t)
    }

    def genPred(t: Term) = Pred(t)
    def genSucc(t: Term) = Succ(t)

    def genValue: Gen[TT] = oneOf((True(), TypeBool), (False(), TypeBool), (Zero(), TypeNat))

    def genPredOrSucc: Gen[PredSuccGen] = oneOf(PredGen, SuccGen)

    def genCondNumber: Gen[TT] = for {
        (cond, condRes) <- oneOf(genBool, genIsZero)
        (thn, thnRes) <- genInt(zz)
        (els, elsRes) <- genInt(zz)
    } yield {
        val res = if (condRes == TypeBool && thnRes == TypeNat && thnRes == TypeNat) TypeNat else null
        (If(cond, thn, els), res)
    }

    def genInt(base: TT): Gen[TT] = for {
        n <- chooseNum(1, 5)
        ops <- listOfN(n, genPredOrSucc)
    } yield {
        val (zero, zeroType) = base
        val term = ops.foldLeft(zero) { case (acc, op) => op(acc) }
        (term, if (zeroType == TypeNat) TypeNat else null)
    }

    def genBool: Gen[TT] = for {
        res <- oneOf(True(), False())
    } yield (res, TypeBool)

    def genIsZero: Gen[TT] = for {
        inner <- (Zero(), TypeNat)
        (term, termType) <- genInt(inner)
    } yield (IsZero(term), if (termType == TypeNat) TypeBool else null)

    def genIf: Gen[TT] = for {
        (cond, condRes) <- oneOf(genValue, genInt(zz), genCondNumber, genBool)
        (thn, thnRes) <- genTerm
        (els, elsRes) <- genTerm
        resType <- if (condRes == TypeBool && thnRes == elsRes) thnRes else null
    } yield (If(cond, thn, els), resType)

    def genTerm: Gen[TT] = oneOf(genValue, genInt(zz), genCondNumber, genBool, genIf, genAbs)

    /**
      * ======================================================================================
      */

    implicit def Type2TypeTree(t: Type): TypeTree = t match {
        case TypeBool => BoolType
        case TypeNat => NatType
        case TypeFun(from, to) => FunType(Type2TypeTree(from), Type2TypeTree(to))
    }

    def genIntUsing(v: Var): Gen[TTT] = for {
        n <- genInt((v, TypeNat))
    } yield (n, Some(TypeNat))

    def genIntVar(v: Var): Gen[TTT] = ((v, TypeNat), Some(TypeNat))

    def genIsZeroUsing(v: Var): Gen[TTT] = for {
        (n, _) <- genInt((v, TypeNat))
    } yield ((IsZero(n), TypeBool), Some(TypeNat))

    def genUnlinkedAbs: Gen[TTT] = for {
        abs <- genAbs
    } yield (abs, None)

    def genTermOfType(t: Type): Gen[TT] = t match {
        case TypeNat => genInt(zz)
        case TypeBool => genBool
        case TypeFun(from, to) =>
            for {
                name <- genFreshName
                variable <- Var(name)
                (body, _) <- genTermOfTypeUsing(to, variable, from)
            } yield (Abs(name, EmptyType, body), t)
    }

    def genTermOfTypeUsing(t: Type, variable: Var, varType: Type): Gen[TT] = (varType, t) match {
        case (TypeNat, TypeNat) =>
            for {
                op <- genPredOrSucc
            } yield (op(variable), TypeNat)

        case (TypeNat, TypeBool) => (IsZero(variable), TypeBool)

        case (TypeNat, fun @ TypeFun(_, _)) =>
            for {
                (thn, _) <- genTermOfType(fun)
                (els, _) <- genTermOfType(fun)
            } yield (If(IsZero(variable), thn, els), fun)

        case (TypeBool, TypeNat) =>
            for {
                (thn, _) <- genInt(zz)
                (els, _) <- genInt(zz)
            } yield (If(variable, thn, els), TypeNat)

        case (TypeBool, TypeBool) =>
            (If(variable, False(), True()), TypeBool)

        case (TypeBool, fun @ TypeFun(from, to)) =>
            for {
                (thn, _) <- genTermOfType(fun)
                (els, _) <- genTermOfType(fun)
            } yield (If(variable, thn, els), fun)

        case (fun @ TypeFun(from, to), target) =>
            for {
                name <- genFreshName
                absVar <- Var(name)
                (body, _) <- genTermOfType(target)
            } yield (App(Abs(name, EmptyType, body), variable), target)
    }

    def genBoolVar(v: Var): Gen[TTT] = ((v, TypeBool), Some(TypeBool))

    def genIfUsing(v: Var): Gen[TTT] = {

        def genConstant: Gen[TTT] = for {
            result <- genTerm
        } yield (result, None)

        def genUnlinkedIsZero = for {
            isz <- genIsZero
        } yield (isz, None)

        def genIfCond: Gen[TTT] = for {
            (cond, varType) <- oneOf(genIsZeroUsing(v), genBoolVar(v), genUnlinkedIsZero)
        } yield (cond, varType)

        for {
            ((condition, _), varType) <- genIfCond
            variableType <- genType
            branchesType <- genType

            ((thn, _), varType1) <- varType match {
                case Some(t) =>
                    for {
                        term <- genTermOfType(branchesType)
                    } yield (term, t)

                case None =>
                    for {
                        term <- genTermOfTypeUsing(branchesType, v, variableType)
                    } yield (term, variableType)
            }

            (els, _) <- genTermOfTypeUsing(branchesType, v, varType1)

        } yield ((If(condition, thn, els), branchesType), Some(varType1))

    }

    def genSimpleType: Gen[Type] = oneOf(TypeNat, TypeBool)

    def genFunType: Gen[Type] = for {
        from <- genSimpleType
        to <- genType
    } yield TypeFun(from, to)

    def genType: Gen[Type] = oneOf(genSimpleType, genFunType)

    def genAbsUsing(v: Var): Gen[TTT] = for {
        from <- genSimpleType
        to <- genType
        (body, _) <- genTermOfTypeUsing(to, v, from)
    } yield ((Abs(v.name, EmptyType, body), TypeFun(from, to)), Some(from))

    def genTermUsing(v: Var): Gen[TTT] = oneOf(genIfUsing(v), genIntUsing(v), genIsZeroUsing(v), genAbsUsing(v))

    def genAbs: Gen[TT] = for {
        name <- genFreshName
        v <- Var(name)
        ((body, bodyTpe), Some(varType)) <- genAbsUsing(v)
    } yield (body, bodyTpe)

}
