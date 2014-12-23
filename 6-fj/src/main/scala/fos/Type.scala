package fos

import scala.collection.mutable.{ Map, HashMap };

case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = List[Pair[Class, String]]

  def typeOf(tree: Tree, ctx: Context): Class = {
    //   ... To complete ...
    tree match {
      case Program(cls, expr) => typeOf(expr, ctx)
      case ClassDef(name, superclass, fields, ctor, methods) => Nil
      case FieldDef(tpe, name) => tpe
      case CtrDef(name, args, supers, body) => Nil
      case Assign(obj, field, rhs) => {
        try {
          val fieldTpe = getClassDef(obj).findField(field).get.tpe
          val rhsTpe = typeOf(rhs, ctx)
          if (fieldTpe == rhsTpe) fieldTpe else throw new TypeError(s"Type mismatch: expected $fieldTpe ; found $rhsTpe")
        } catch {
          case _: NoSuchElementException => throw new NonexistingFieldException(s"Field ${obj}.$field does not exist")
        }
      }
      case MethodDef(tpe, name, args, body) => {
        try{
          val bodyTpe = getClassDef(typeOf(body, ctx ::: (for (arg <- args) yield ((arg.tpe, arg.name)))))
          if (bodyTpe.isSubClassOf(tpe)){
            println(s"$name OK in $tpe")
            tpe
          } else {
            throw new MethodTypeMismatchException(s"Type of body did not match return type. Expected $tpe, found $bodyTpe")
          }
        } catch { // Means the body could not be typechecked, so keep throwing the error.
          case e: Throwable => throw e
        }
      }
      case e: Expr => e match {
        case Var(name) => {
          val c = ctx.find(_._2 == name)
          c match {
            case Some((cls, str)) => cls
            case None => throw new VarUndefinedException(s"Variable $name was not defined in the scope") // Can this ever happen ?
          }
        }
        case New(cls, args) => {
          val clas = getClassDef(cls)
          try {
            clas.checkTypeArguments(for (arg <- args) yield typeOf(arg, ctx))
          } catch {
            case e: Throwable => throw e
          }
          clas.name
        }
        case Select(obj, field) => {
          val clas = typeOf(obj, ctx)
          val fieldDef = getClassDef(clas).findField(field)
          fieldDef match {
            case Some(f) => typeOf(f, ctx)
            case None => throw new FieldAccessedUndefinedException(s"Field $field is undefined in class $clas")
          }
        }
        case Apply(obj, method, args) => {
          val clas = typeOf(obj, ctx)
          val meth = getClassDef(clas).findMethod(method)
          meth match {
            case Some(m) => {
              try {
                m.checkTypeArguments(for (arg <- args) yield typeOf(arg, ctx))
                m.tpe
              } catch {
                case e: Throwable => throw e // Should occur when m.checkTypeArguments(...) throws a MethodArgsException
              }
            }
            case None => throw new MethodUndefinedException(s"Method $method is undefined in class $clas")
          }
        }
        case Cast(cls, e) => {
          val eType = getClassDef(typeOf(e, ctx))
          if (eType.isSubClassOf(getClassDef(cls))) cls
          // else if (eType.isSuperclassOf(Option(getClassDef(cls))) && eType.name != cls) cls  //  This is the rule as defined in the paper, but its detail seems useless in our implementation
          else if (eType.isSuperclassOf(Option(getClassDef(cls)))) cls
          else { //if (!eType.isSuperclassOf(Option(getClassDef(cls))) && !eType.isSubClassOf(getClassDef(cls))) {   // This is necessary if we use the paper's exact rule above
            println(s"Warning: Stupid cast of expression $e to type $cls")
            cls
          }
        }
      }
    }
    CT.objectClass
  }
}

case class EvaluationException(msg: String) extends Exception

object Evaluate extends (Expr => Expr) {

  import Utils._

  def apply(expr: Expr): Expr = expr match {
    //   ... To complete ...

    // Computation rules
    case Var(name) => expr
    case New(cls, args) => expr
    case Cast(cls, e) if isValue(e) => e
    case Select(obj, field) if isValue(obj) => obj match {
      case New(cls, args) => expr // Need an access to a "global class table" to get which arg to return (see hints)
    }
    case Apply(obj, method, args) if isValue(obj) && isValueArg(args) => { //Need access to a global class table again to get method body
      val substList = args foreach (arg => (args)) // Get a mapping fieldName => arg, then call substituteInBody
      expr
    }

    //Congruence rules
    case Select(obj, field) => Select(apply(obj), field)
    case Apply(obj, method, args) if isValue(obj) => applyArgs(expr, args)
    case Apply(obj, method, args) => Apply(apply(obj), method, args)
    case New(cls, args) => applyArgs(expr, args)
    case Cast(cls, e) => Cast(cls, apply(e))
  }

  // Seems to me that only Var and New can be values
  def isValue(expr: Expr): Boolean = expr match {
    case Var(name) => true
    case New(cls, args) => isValueArg(args)
    case Cast(cls, e) => false
    case Select(obj, field) => false
    case Apply(obj, method, args) => false
  }

  def isValueArg(args: List[Expr]) = args map (arg => isValue(arg)) reduceLeft ((arg1, arg2) => arg1 && arg2) // So much Scalness!!

  def applyArgs(default: Expr, args: List[Expr]): Expr = args match {
    case x :: xs => if (!isValue(x)) apply(x) else applyArgs(default, xs)
    case Nil => default
  }

  def substituteInBody(exp: Expr, thiss: New, substs: List[(FieldDef, Expr)]): Expr = exp match {
    case Select(obj: Expr, field: String) => Select(substituteInBody(obj, thiss, substs), field)
    case New(cls, args) => New(cls, args map (arg => substituteInBody(arg, thiss, substs)))
    case Cast(cls, e) => Cast(cls, substituteInBody(e, thiss, substs))
    case Var("this") => thiss
    case Var(bd) => substs find (subs => subs._1.name == bd) match {
      case None => exp
      case Some((_, sub)) => sub
    }

    case Apply(obj, method, args) => Apply(substituteInBody(obj, thiss, substs), method, args map (arg => substituteInBody(arg, thiss, substs)))
    case _ => throw new EvaluationException("Apply: Forgot expression " + exp)
  }
}

object CT {

  val objectClass: String = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil), Nil)

  private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]

  add(objectClass, objectClassDef)

  def elements = ct iterator

  def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

  def add(key: String, element: ClassDef): Unit = ct += key -> element

  def delete(key: String) = ct -= key

  def clear(): Unit = {
    ct clear;
    add(objectClass, objectClassDef)
  }

}

object Utils {

  def getClassDef(className: String): ClassDef = CT lookup className match {
    case None => throw new TypeError("class " + className + " not declared")
    case Some(c: ClassDef) => c
  }
}
