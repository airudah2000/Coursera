package calculator

import scala.collection.immutable.Iterable

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val value =
      for {
        (k, v) <- namedExpressions
        expression: Expr = getReferenceExpr(k, namedExpressions)
        value: Double = eval(expression, namedExpressions)
      }
        yield (k, Signal(value))
    value.toMap
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(value : Double) => value
    case Ref(name: String) => Double.NaN
    case Plus(a: Expr, b: Expr) => Double.NaN
    case Minus(a: Expr, b: Expr) => Double.NaN
    case Times(a: Expr, b: Expr) => Double.NaN
    case Divide(a: Expr, b: Expr) => Double.NaN
    case _ => Double.NaN
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
