package calculator

import calculator.{Literal, Signal}
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import Polynomial._
import Calculator._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = calculator. TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {

    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("polynomial delta") {
    val deltaValue1 = computeDelta(Signal(2.0), Signal(5.0), Signal(3.0))
    assert(deltaValue1.apply === 1.0)

    val deltaValue2 = computeDelta(Signal(4.0), Signal(2.0), Signal(1.0))
    assert(deltaValue2.apply === -12.0)
  }

  test("polynomial solution") {
    val deltaValue1 = computeDelta(Signal(2.0), Signal(5.0), Signal(3.0))
    val solution1 = computeSolutions(Signal(2.0), Signal(5.0), Signal(3.0), deltaValue1)
    assert(solution1.apply === Set(-1.0, -1.5))

    val deltaValue2 = computeDelta(Signal(4.0), Signal(2.0), Signal(1.0))
    val solution2 = computeSolutions(Signal(4.0), Signal(2.0), Signal(1.0), deltaValue2)
    assert(solution2.apply === Set())
  }

  test("Calculate expression literal") {
//    abstract class Expr
//    case class Literal(v: Double) extends Expr
//
//    def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {
//      references.get(name).fold[Expr] {
//        Literal(Double.NaN)
//      } { exprSignal =>
//        exprSignal()
//      }
//    }
//
//    val namedExpressionsA: Map[String, Nothing] = (List("a", "b") zip List(Signal(Literal(2)), Signal(Literal(3)))).toMap
//    val (key1, val1) = namedExpressions.head
//    val exp1 = getReferenceExpr(key1, namedExpressions)
//
//    val (key2, val2) = namedExpressions.tail
//    val exp2 = getReferenceExpr(key2, namedExpressions)
//
//    val namedExpressionsB: Map[String, Nothing] = (List("a", "b") zip List(Signal(exp1), Signal(exp2))).toMap
//
//    val computedValues1 = computeValues(Map(key1, Signal(namedExpressions)))
//    assert(computedValues1 === 1, s"values were $computedValues")
  }

}
