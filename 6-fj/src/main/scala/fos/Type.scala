package fos

import scala.collection.mutable.{ Map, HashMap };

case class TypeError(msg: String) extends Exception(msg)

object Type {

  import CT._
  import Utils._

  type Class = String
  type Context = List[Pair[Class, String]]

  def typeOf(tree: Tree, ctx: Context): Class = {
    tree match {
      case Program(cls, expr) => typeOf(expr, ctx)
      case ClassDef(name, superclass, fields, ctor, methods) => {
        if (hasInheritanceCycle(name, getClassDef(superclass))) throw new InheritanceCycleException("Cycle in inheritance definition of class " + name)
        typeOf(ctor, ctx)
        for (m <- methods) typeOf(m, (name, "this") :: ctx)
        name
      }
      case FieldDef(tpe, name) => tpe
      case CtrDef(name, args, supers, body) =>
        val clas = getClassDef(name)
        val ctorNewfields = body.map(a => a.field)
        val supFields = clas.getFieldsSuperclass
        if (!args.filterNot(a => supers.map(y => y.name).contains(a.name)).map(a => a.name).equals(ctorNewfields) || !supFields.map(a => new Var(a.name)).equals(supers)) {
          println("Args not OK")
          throw new TypeError(s"Constructor of class $name does not initialise fields properly. @ ${tree.pos}")
        }
        try {
          clas.checkTypeArguments((for (arg <- args) yield typeOf(arg, ctx)))
        } catch {
          case e: ClassConstructorArgsException => {
            println(e.msg)
            throw new TypeError(s"Constructor of class $name does not initialise super fields properly. @ ${tree.pos}")
          }
        }
        for (a <- body) typeOf(a, (name, "this") :: (args.map(a => (a.tpe, a.name)) ::: ctx))
        name
      case Assign(obj, field, rhs) => {
        try {
          val fieldTpe = if (obj == "this") getClassDef(findVar("this", ctx)).findField(field).get.tpe else getClassDef(obj).findField(field).get.tpe
          val rhsTpe = typeOf(rhs, ctx)
          if (fieldTpe == rhsTpe) fieldTpe else throw new TypeError(s"Type mismatch: expected $fieldTpe ; found $rhsTpe. @ ${tree.pos}")
        } catch {
          case _: NoSuchElementException => throw new TypeError(s"Field ${obj}.$field does not exist. @ ${tree.pos}")
        }
      }
      case meth: MethodDef => {
        try {
          CT.lookupMethod(meth) match {
            case Some(v) => {
              val clas = v._1
              val innerCtx = (clas, "this") :: meth.args.map(arg => (arg.tpe, arg.name))
              val bodyTpe = getClassDef(typeOf(meth.body, innerCtx))
              if (bodyTpe.isSubClassOf(meth.tpe)) { // Check that body type is a subclass of expected return type
                val superclass = getClassDef(clas)
                val overriden = superclass.findMethod(meth.name)
                overriden match { // Check that return and argument types match
                  case Some(o) => {
                    val givenArgTypes = meth.args map (arg => arg.tpe)
                    val expectedArgTypes = o.args map (arg => arg.tpe)
                    if (!o.tpe.equals(meth.tpe)) throw new TypeError("Method " + meth.name + s" return type does not match that of overriden method. Expected ${o.tpe} but found " + meth.tpe + s". @ ${tree.pos}")
                    else if (!givenArgTypes.equals(expectedArgTypes)) throw new TypeError(s"Argument types of method " + meth.name + s" do not match that of overriden method. @ ${tree.pos}")
                  }
                  case None => meth.tpe
                }
                meth.tpe
              } else {
                throw new TypeError(s"Type of body did not match return type. Expected " + meth.tpe + s", found $bodyTpe. @ ${tree.pos}")
              }
            }
            case None => meth.tpe
          }
        } catch { // Means the body could not be type checked
          case e: TypeError => {
            println(e.msg)
            throw new TypeError(s"Body of method $meth could not be typechecked. @ ${tree.pos}")
          }
          case e: Throwable => throw new TypeError(s"Body of method $meth could not be typechecked. @ ${tree.pos}")
        }
      }
      case e: Expr => e match {
        case Var(name) => {
          val tpe = findVar(name, ctx)
          if (tpe == null) throw new TypeError(s"Variable $name was not defined in the scope. @ ${tree.pos}")

          return tpe
        }
        case New(cls, args) => {
          val clas = getClassDef(cls)
          try {
            clas.checkTypeArguments(for (arg <- args) yield typeOf(arg, ctx))
          } catch {
            case e: ClassConstructorArgsException => {
              println(e.msg)
              throw new TypeError(s"Arguments of New declaration did not match those expected. @ ${tree.pos}")
            }
          }
          return clas.name
        }
        case Select(obj, field) => {
          val clas = typeOf(obj, ctx)
          val fieldDef = getClassDef(clas).findField(field)
          fieldDef match {
            case Some(f) => typeOf(f, ctx)
            case None => throw new TypeError(s"Field $field is undefined in class $clas. @ ${tree.pos}")
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
                case e: MethodArgsException => {
                  println(s"${e.arg}. @ ${tree.pos}")
                  throw new TypeError(s"Arguments passed to method $m did not match those expected. @ ${tree.pos}")
                }
              }
            }
            case None => throw new TypeError(s"Method $method is undefined in class $clas. @ ${tree.pos}")
          }
        }
        case c: Cast => {
          checkStupidCast(c, ctx)
          val eType = getClassDef(typeOf(c.e, ctx))
          c.cls
        }
      }
    }
  }

  def findVar(v: String, ctx: Context) = {
    val c = ctx.find(_._2 == v)
    c match {
      case Some((cls, str)) => cls
      case None => null
    }
  }

  def hasInheritanceCycle(head: String, current: ClassDef): Boolean = current.name match {
    case "Object" => false
    case `head` => true
    case _ => hasInheritanceCycle(head, getClassDef(current.superclass))
  }

  def checkStupidCast(expr: Expr, ctx: Context): Class = expr match {
    case Cast(cl, e) => {
      val eType = getClassDef(checkStupidCast(e, ctx))
      if (!eType.isSubClassOf(getClassDef(cl)) && !eType.isSuperclassOf(Option(getClassDef(cl)))) {
        println(s"Warning: Stupid cast of expression $e to type $cl. @ ${expr.pos}")
      }
      eType.name
    }
    case _ => typeOf(expr, ctx)
  }
}

case class EvaluationException(msg: String) extends Exception
case class NoRuleApplies(msg: String) extends Exception

object Evaluate extends (Expr => Expr) {

  import Utils._

  def apply(expr: Expr): Expr = expr match {
    // Computation rules
    case Cast(cls, e) if isValue(e) => e
    case Select(obj, field) if isValue(obj) => obj match {
      case New(cls, args) => CT.lookup(cls) match {
        case Some(c) => args(c.getFieldIndex(field))
        case None => throw ClassUndefinedException("Using undefined class " + expr)
      }
    }
    case Apply(obj, method, args) if isValue(obj) && isValueArg(args) => obj match {
      case New(cls, cArgs) => CT.lookup(cls) match {
        case Some(v) => v.findMethod(method) match {
          case Some(m) => {
            substituteInBody(m.body, New(cls, cArgs), m.args zip args)
          }
          case None => throw new MethodUndefinedException("Method " + method + " in class " + cls + " is undefined")
        }
        case None => throw new ClassUndefinedException("Class " + cls + " is used but nor defined")
      }
      case _ => throw new EvaluationException("Method should be called on objects")
    }

    //Congruence rules
    case Select(obj, field) => Select(apply(obj), field)
    case Apply(obj, method, args) if isValue(obj) => Apply(obj, method, applyArgs(args))
    case Apply(obj, method, args) => Apply(apply(obj), method, args)
    case New(cls, args) if (!isValueArg(args)) => New(cls, applyArgs(args))
    case Cast(cls, e) => Cast(cls, apply(e))

    case _ => throw new NoRuleApplies("Couldn't apply any method to the expression " + expr)
  }

  def isValue(expr: Expr): Boolean = expr match {
    case Var(name) => true
    case New(cls, args) => isValueArg(args)
    case _ => false
  }

  def isValueArg(args: List[Expr]) = if (args.isEmpty) true else args map (arg => isValue(arg)) reduceLeft ((arg1, arg2) => arg1 && arg2)

  def applyArgs(args: List[Expr]): List[Expr] = args match {
    case x :: xs => if (!isValue(x)) apply(x) :: xs else x :: applyArgs(xs)
    case Nil => throw new EvaluationException("Couldn't evaluate any of the arguments passed to applyArgs: " + args.mkString("(", ",", ")")) // Should never happen (bad call to the method)
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
  }
}

object CT {

  val objectClass: String = "Object"
  private val objectClassDef = ClassDef(objectClass, null, Nil, CtrDef(objectClass, Nil, Nil, Nil), Nil)

  private var ct: Map[String, ClassDef] = new HashMap[String, ClassDef]

  add(objectClass, objectClassDef)

  def elements = ct iterator

  def lookup(classname: String): Option[ClassDef] = if (classname != null) ct get classname else None

  def lookupMethod(method: MethodDef): Option[(String, ClassDef)] = elements.find(a => a._2.containsMethod(method))

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
