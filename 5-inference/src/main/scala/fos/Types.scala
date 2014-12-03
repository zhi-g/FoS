package fos

import scala.collection.immutable.{ Set, ListSet }

abstract class Type {
  override def toString() = this match {
    case TypeVar(a) => a
    case TypeFun(a, b) => s"($a->$b)"
    case _: TypeNat => "Nat"
    case _: TypeBool => "Bool"
  }
}

case class TypeVar(name: String) extends Type
case class TypeFun(tp1: Type, tp2: Type) extends Type
case class TypeNat extends Type
case class TypeBool extends Type

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  
  def instantiate: Type = {
    def newName(oldName: String, i: Int): String = if (args.contains(oldName + i.toString)) oldName + i else newName(oldName, i+1) // ça va pas marcher parce que args contient de TypeVars et pas strings
    tp match {
      case TypeNat() => tp
      case TypeBool() => tp
      case TypeVar(a) => TypeVar(newName(a, 1))
      case TypeFun(tp1, tp2) => TypeFun(TypeScheme(args, tp1).instantiate, TypeScheme(args, tp2).instantiate)
    }
  }
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  // We can use this object as a static utility class in Java
  
 
}

//abstract class Constraint { // (see: Implementation Hints)
//  val tp: Pair[Type, Type]
//  val pos: Int
//}

abstract class Substitution extends (Type => Type) {

  var indent = 0

  // Check if there is a way to do this without using a var
  var mappings: List[Pair[Type, Type]] = List()

  def apply(tp: Type): Type = {
    //println("  " * indent + "in: " + tp + "   subst: " + this)
    indent = indent + 1
    val result = tp match {
      case TypeFun(a, b) => TypeFun(apply(a), apply(b))
      case TypeVar(name) => {
        mappings.foreach { constr =>
          constr._1 match {
            case TypeVar(x) if x == name => constr._2
          }
        }
        tp
      }
      case _ => tp
    }
    indent = indent - 1
    //println("  " * indent + "out: " + result + "   subst: " + this)
    result
  }
  override def toString() = {
    (for (constr <- mappings) yield constr._1 + "->" + constr._2).mkString("[", ",", "]")
  }

  def apply(p: (Type, Type)): (Type, Type) = p match { // Should we replace '(Type, Type)' by 'Constraint' ?
    case Pair(t1, t2) => (this(t1), this(t2))
  } //what's this used for?

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) 
  //To complete  
  }

  def extend(c: (Type, Type)): Substitution = {
    mappings = c :: mappings // and unify?
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
