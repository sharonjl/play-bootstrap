package utils

import helpers.{DatabaseSupport, TestConfiguration}
import org.scalatestplus.play.PlaySpec
import utils.APIExceptions.{APIException, UnknownException}
import scala.util.control.Exception._
import scala.util._
/**
 * Created by sharon on 15-01-07.
 */
class FailFastChainSpec extends PlaySpec {

  import utils.Ops._

  def fFailsValidation(): Option[APIException] = Some(UnknownException())

  def fPassesValidation(): Option[APIException] = None

  def someCalculation = "Results"

  "Utils" when {

    "FailFastChaining" must {
      "return Left(APIException) when encountered (at beginning of chain)" in {
        fFailsValidation |> {
          someCalculation
        } mustBe Left(UnknownException())
      }

      "return Right(...) when no APIExceptions are encountered" in {
        fPassesValidation |> {
          someCalculation
        } mustBe Right(someCalculation)
      }

      "return Left(APIException) when encountered (in middle of chain with 1 link)" in {
        fPassesValidation |> fFailsValidation mustBe Left(UnknownException())
      }

      "return Left(APIException) when encountered (at end of chain with 1 link)" in {
        fPassesValidation |> {
          Left(UnknownException())
        } mustBe Left(UnknownException())
      }

      "return Left(APIException) when encountered (at end of chain with 2 links)" in {
        fPassesValidation |> fPassesValidation |> {
          Left(UnknownException())
        } mustBe Left(UnknownException())
      }

      "return Right(...) when no APIExceptions are encountered (multiple links)" in {
        fPassesValidation |> fPassesValidation |> fPassesValidation |> fPassesValidation |> {
          Right(someCalculation)
        } mustBe Right(someCalculation)
      }

      "return Right(...) when no APIExceptions are encountered (complex multiple links)" in {
        fPassesValidation |> fPassesValidation |> {
          Right("Cupcakes")
        } |> fPassesValidation |> {
          Right(someCalculation)
        } mustBe Right(someCalculation)
      }

      "for comphrension" in {
        def dangerousOp = throw new RuntimeException("never works")

        def f1(s: Int): Try[Int] = {
          println("evaluating f1")
          Success(s * 10)
        }
        def f2(s: Int): Try[Int] = {
          println("evaluating f2")
          catching(classOf[Exception]) toTry dangerousOp
        }
        def f3(s: Int): Try[Int] = {
          println("evaluating f3")
          Success(s * 30)
        }

        val result = for {
          x <- f1(10)
          y <- f2(x)
          z <- f3(y)
        } yield z

        println(result)
      }

    }

  }
}
