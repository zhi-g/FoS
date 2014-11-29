package fos

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {
  import Type._

  type Constraint = (Type, Type)

  val noConstraints: List[Constraint] = Nil
  case class TypingResult(tpe: Type, c: List[Constraint])

  /**
   * Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    case Var(x) =>
      val t1 = lookup(env, x)
      if (t1 == null)
        throw TypeError("Unknown variable " + x)
      TypingResult(t1.instantiate, noConstraints)
    //   ... To complete ... 
  }

  /**
   */
  def unify(c: List[Constraint]): Substitution =
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeVar(a), TypeVar(b)) if (a == b) =>
        unify(c.tail)
      case (TypeVar(x), s) if (!includes(s, x)) =>
        unify(subst(x, s, c.tail)).extend(new Constraint(TypeVar(x), s)) //Should substitute [x->s]c.tail, so the new constraint is visible in the remaining constraints of c
      case (s, TypeVar(x)) if (!includes(s, x)) =>
        unify(subst(x, s, c.tail)).extend(new Constraint(TypeVar(x), s))
      case (TypeFun(arg1, res1), TypeFun(arg2, res2)) =>
        unify((arg1, arg2) :: (res1, res2) :: c.tail)
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }

  def includes(tp1: Type, name: String): Boolean = {
    tp1 match {
      case TypeVar(x) => x == name
      case TypeFun(a, b) => includes(a, name) || includes(b, name)
      case _ => false
    }
  }

  def subst(tpeVar: String, tpe: Type, constraints: List[Constraint]): List[Constraint] = {
    if (constraints.isEmpty) List()
    else {
      new Constraint(substSub(tpeVar, tpe, constraints.head._1), substSub(tpeVar, tpe, constraints.head._2)) :: subst(tpeVar, tpe, constraints.tail)
    }
  }

  // [tpeVar -> tpe1] tpe2
  def substSub(tpeVar: String, tpe1: Type, tpe2: Type): Type = {
    tpe2 match {
      case _: TypeNat => TypeNat()
      case _: TypeBool => TypeBool()
      case TypeFun(a, b) => TypeFun(substSub(tpeVar, tpe1, a), substSub(tpeVar, tpe2, b))
      case TypeVar(x) if x == tpeVar => tpe1
      case _ => tpe2
    }
  }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t)
    val s = unify(c)
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
