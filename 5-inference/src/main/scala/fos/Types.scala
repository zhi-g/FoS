package fos

import scala.collection.immutable.{ Set, ListSet }

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => s"($a->$b)"
    case TypeNat => "Nat"
    case TypeBool => "Bool"
  }
}

case class TypeVar(name: String) extends Type
case class TypeFun(tp1: Type, tp2: Type) extends Type
object TypeNat extends Type
object TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {

  def instantiate: Type = {
    def generaliseAcc(tpe: Type, args: List[TypeVar]): Type = {
      if (args.isEmpty) return tpe
      else generaliseAcc(generalise(tpe, args.head.name, Type.getFreshName(args.head.name)), args.tail)
    }

    def generalise(tpe: Type, oldName: String, newName: String): Type = {
      tpe match {
        case TypeVar(a) if a == oldName => TypeVar(newName)
        case TypeFun(tp1, tp2) => TypeFun(generalise(tp1, oldName, newName), generalise(tp2, oldName, newName))
        case _ => tpe
      }
    }

    if (args.isEmpty) tp // return tp
    else generaliseAcc(tp, args)
  }

  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  // We can use this object as a static utility class in Java

  var freshVariables: List[String] = List()
  
  def getFreshName(oldName: String): String = {
    def getFreshNameAcc(oldName: String, i: Int): String = {
      if (freshVariables.contains(oldName + i)) getFreshNameAcc(oldName, i+1)
      else (oldName + i)
    }
    freshVariables = getFreshNameAcc(oldName, 1) :: freshVariables
    freshVariables.head
  }
  
  def getTypeVars(t: Type): List[String]  = {
    t match {
      case TypeVar(x) => x::Nil
      case TypeFun(t1,t2) => getTypeVars(t1) ::: getTypeVars(t2)
      case _ => Nil
    }
  }

  /**
   * Alpha-conversion:
   *  @param x new name
   *  @param y old name
   *
   */
  def alpha(t: Term, x: String, y: String): Term = (
    t match {
      case _: True | _: False | _: Zero => t
      case Succ(e) => Succ(alpha(e, x, y))
      case Pred(e) => Pred(alpha(e, x, y))
      case IsZero(e) => IsZero(alpha(e, x, y))
      case If(cond, thn, els) => If(alpha(cond, x, y), alpha(thn, x, y), alpha(els, x, y))
      case Var(name) => if (name == y) new Var(x) else t
      case App(t1, t2) => App(alpha(t1, x, y), alpha(t2, x, y))
      case Abs(name, tpe, t1) => if (name == x || name == y) t else Abs(name, tpe, alpha(t1, x, y))
      case Let(x1, v, t) => if (y == x1) t else Let(x1, alpha(v, x, y), alpha(t, x, y))
    })
    
   def findGeneralTypes(env: List[Type], t: Type): List[TypeVar] = {
    val vars = (for( x<-env) yield getTypeVars(x)).flatten
    for (x<- getTypeVars(t) if !vars.contains(x)) yield TypeVar(x)
  }
}

abstract class Substitution extends (Type => Type) {

  var indent = 0
  var mappings: List[Pair[Type, Type]] = List()

  def apply(tp: Type): Type = {
    println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
      case TypeFun(a, b) => TypeFun(apply(a), apply(b))
      case TypeVar(name) => {
        for (constr <- mappings)
          constr._1 match {
            case TypeVar(x) if x == name => return apply(constr._2)
            case _ =>
          }
        tp
      }
      case _ => tp
    }
    indent = indent - 1
    println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = {
    (for (constr <- mappings) yield constr._1 + "->" + constr._2).mkString("[", ",", "]")
  }

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp)))
    }

  def extend(c: (Type, Type)): Substitution = {
    mappings = c :: mappings
    this
  }
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
}

/** The empty list of constraints. */
object emptyConstraintList {
  val constraints: List[(Type, Type)] = List()
}
