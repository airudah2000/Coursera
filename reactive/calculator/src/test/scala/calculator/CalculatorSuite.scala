package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import Polynomial._
import Calculator._
import Signal._

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

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
    val names = (0 until 10).map(i => ('a' + i).toChar.toString)
    val literalValue = Literal(2)
    assert(literalValue.v === 2.toDouble, "I don't know what literal is: " + literalValue.v)

  }

  test("Return a literal value") {
    val litVal = Literal(2)
    val namedExpression: Map[String, Signal[Expr]] = Map("a" -> Signal(litVal))
    System.out.print(namedExpression.map(_._2.toString))
  }

}

















