package um

object Um {

  /** An expression. */
  trait Expr

  /** A runtime value. */
  trait Value

  /** An expression that also acts as a runtime value. */
  trait ValueExpr extends Value with Expr

  /** A value expression that is a type. */
  trait Typ extends ValueExpr

  /** The type of all types, often known as `*`. */
  object Ty extends ValueExpr {
    override def toString: String = "Ty"
  }

  /** The placeholder for an omitted type.
    *
    * Can be read as type &ldquo;dynamic&rdquo;&mdash;that is, checked at
    * runtime&mdash;or &ldquo;type to be inferred&rdquo.
    */
  object Om extends Typ {
    override def toString: String = "Om"
  }

  /** A variable reference. */
  case class Var (varName: String) extends Expr

  /** Dependent function type, where `varName` may appear in `from` and `to`. */
  case class DFT (varName: String, from: Expr, to: Expr) extends Typ

  /** By convention, a non-dependent function type. */
  def NFT (from: Expr, to: Expr) = DFT("",from,to)

  /** An application (function call). */
  case class App (fun: Expr, arg: Expr) extends Expr

  /** Lambda. */
  case class Lam (varName: String, varType: Expr, body: Expr) extends ValueExpr

  /** Lambda that specifies its paramter type as `Om`. */
  def OLm (varName: String, body: Expr) = Lam(varName,Om,body)

  /** Conditional. */
  case class If (cond: Expr, trueBranch: Expr, falseBranch: Expr) extends Expr

  /** Coercion of `term` to the type `toType`. */
  case class Cast (term: Expr, toType: Expr) extends Expr

  /** Recursion. */
  case class Fix (varName: String, varType: Expr, body: Expr) extends ValueExpr

  /** Variable names that appear free in `e`. */
  def freeVarNames (e: Expr): Set[String] = e match {
    case Var(varName) =>
      Set(varName)
    case DFT(varName,from,to) =>
      freeVarNames(from) ++ freeVarNames(to) - varName
    case App(fun,arg) =>
      freeVarNames(fun) ++ freeVarNames(arg)
    case Lam(varName,varType,body) =>
      freeVarNames(varType) ++ freeVarNames(body) - varName
    case If(cond,trueBranch,falseBranch) =>
      freeVarNames(cond) ++ freeVarNames(trueBranch) ++
        freeVarNames(falseBranch)
    case Cast(term,toType) =>
      freeVarNames(term) ++ freeVarNames(toType)
    case Fix(varName,varType,body) =>
      freeVarNames(varType) ++ freeVarNames(body) - varName
    case _ =>
      Set.empty
  }

  /** `numNames` names that do not occur in `names`, starting with `prefix`. */
  def freshNames (names: Set[String], prefix: String = "s#"): Stream[String] =
    Stream.from(1).filter(n => ! names.contains(prefix + n)).map(prefix + _)

  /** A name that does not occur free in `e`. */
  def freshVarName (e: Expr): String = freshNames(freeVarNames(e)).head

  /** Built-in type corresponding to an underlying Scala type. */
  trait UTyp [T] extends Typ
  /** Built-in unit type. */
  object UUnit extends UTyp[Unit]
  /** Built-in boolean type. */
  object UBool extends UTyp[Boolean]
  /** Built-in int type. */
  object UInt extends UTyp[Int]
  /** Type for underlying Scala function. */
  case class UFunTyp [A,B] (from: UTyp[A], to: UTyp[B]) extends UTyp[A => B]

  /** Literal. */
  case class Lit [T] (v: T, t: UTyp[T]) extends ValueExpr

  /** Built-in unary function maker. */
  def builtIns1 [A,B] (
      uta: UTyp[A],utb: UTyp[B],fs: A => B*): Seq[Lit[A => B]] =
    fs.map(f => Lit[A => B](a => f(a),UFunTyp(uta,utb)))

  /** Built-in binary function maker. */
  def builtIns2 [A,B,C] (
        uta: UTyp[A],utb: UTyp[B],utc: UTyp[C],fs: (A,B) => C*):
      Seq[Lit[A => B => C]] =
    fs.map { f =>
      Lit[A => B => C](a => b => f(a,b),UFunTyp(uta,UFunTyp(utb,utc)))
    }

  val Seq(neg) = builtIns1[Int,Int](UInt,UInt,- _)
  val Seq(not) = builtIns1[Boolean,Boolean](UBool,UBool,! _)
  val Seq(plus,times,minus,divide) =
    builtIns2[Int,Int,Int](UInt,UInt,UInt,_ + _,_ * _,_ - _,_ / _)
  val Seq(lt,eq,gt,ge,le,ne) =
    builtIns2[Int,Int,Boolean](
      UInt,UInt,UBool,_ < _,_ == _,_ > _,_ >= _,_ <= _,_ != _)

  /** Remove unnecessary function wrapping.
    *
    * Performed only for lambdas with parameter type `Om`; otherwise, would
    * have to perform typechecking to ensure the conversion was legal.
    */
  def etaConvert (e: Expr): Expr = e match {
    case Lam(v1,Om,body) =>
      etaConvert(body) match {
        case App(fun,Var(v2)) if v1 == v2 && ! freeVarNames(fun).contains(v1) =>
          fun
        case b =>
          Lam(v1,Om,b)
      }
    case _ => e
  }

}
