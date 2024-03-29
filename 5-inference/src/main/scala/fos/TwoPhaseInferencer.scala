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
  def collect(env: Env, t: Term): TypingResult = {
    t match {
      case Var(x) =>
        val t1 = lookup(env, x)
        if (t1 == null)
          throw TypeError("Unknown variable " + x)
        TypingResult(t1.instantiate, noConstraints)

      case _: True => TypingResult(TypeBool, noConstraints)
      case _: False => TypingResult(TypeBool, noConstraints)
      case _: Zero => TypingResult(TypeNat, noConstraints)

      case Succ(x) =>
        val r = collect(env, x)
        if (r.tpe == null)
          throw TypeError(s"Cannot typecheck $x")
        TypingResult(TypeNat, (new Constraint(r.tpe, TypeNat) :: r.c))

      case Pred(x) =>
        val r = collect(env, x)
        if (r.tpe == null)
          throw TypeError(s"Cannot typecheck $x")
        TypingResult(TypeNat, (new Constraint(r.tpe, TypeNat) :: r.c))

      case IsZero(x) =>
        val r = collect(env, x)
        if (r.tpe == null)
          throw TypeError(s"Cannot typecheck $x")
        TypingResult(TypeBool, (new Constraint(r.tpe, TypeNat) :: r.c))

      case If(cond, t1, t2) =>
        val r1 = collect(env, cond)
        val r2 = collect(env, t1)
        val r3 = collect(env, t2)
        if (r1.tpe == null || r2.tpe == null || r3.tpe == null)
          throw TypeError(s"Cannot typecheck $t")
        //check overlapping type variables
        val c1 = new Constraint(r1.tpe, TypeBool)
        val c2 = new Constraint(r2.tpe, r3.tpe)
        TypingResult(r2.tpe, (c1 :: c2 :: Nil) ::: r1.c ::: r2.c ::: r3.c)

      case Abs(v, tp, t1) =>
        val tp1 = tp match {
          case EmptyType => TypeVar(getFreshName("X"))
          case _ => toType(tp)
        }

        val r1 = collect((v, TypeScheme(List(), tp1)) :: env, t1)
        if (r1.tpe == null)
          throw TypeError(s"Cannot typecheck $t")
        TypingResult(TypeFun(tp1, r1.tpe), r1.c)

      case App(t1, t2) =>
        val r1 = collect(env, t1)
        val r2 = collect(env, t2)
        if (r1.tpe == null || r2.tpe == null)
          throw TypeError(s"Cannot typecheck $t")
        val tp2 = TypeVar(getFreshName("X"))
        val c1 = new Constraint(r1.tpe, TypeFun(r2.tpe, tp2))
        TypingResult(tp2, c1 :: r1.c ::: r2.c)

      case Let(x, v, t) =>
        val r1 = collect(env, v)
        val s = unify(r1.c)
        val T = s(r1.tpe)
        val newEnv = s(env)
        collect((x, new TypeScheme(findGeneralTypes(for( y <- newEnv) yield y._2.tp, T), T)) :: newEnv,t) 
   
    }
  }
  /**
   */
  def unify(c: List[Constraint]): Substitution = {
    if (c.isEmpty) emptySubst
    else c.head match {
      case (TypeNat, TypeNat) => unify(c.tail)
      case (TypeBool, TypeBool) => unify(c.tail)
      case (TypeVar(a), TypeVar(b)) if (a == b) =>
        unify(c.tail)
      case (TypeVar(x), s) if (!includes(s, x)) =>
        unify(subst(x, s, c.tail)).extend(new Constraint(TypeVar(x), s))
      case (s, TypeVar(x)) if (!includes(s, x)) =>
        unify(subst(x, s, c.tail)).extend(new Constraint(TypeVar(x), s))
      case (TypeFun(arg1, res1), TypeFun(arg2, res2)) =>
        unify((arg1, arg2) :: (res1, res2) :: c.tail)
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }
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

  def substSub(tpeVar: String, tpe1: Type, tpe2: Type): Type = {
    tpe2 match {
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
