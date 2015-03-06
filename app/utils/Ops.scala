package utils

import utils.APIExceptions.APIException

import scala.language.implicitConversions

/**
 * Created by sharon on 15-02-09.
 */
object Ops {

  implicit def anyToAny[A](o: A): AnyOps[A] = new AnyOps[A](o)

  class AnyOps[A](val a: A) {
    def chain[B, C, D](c: C): Either[B, D] = |>[B,C,D](c)

    def |>[B, C, D](c: C): Either[B, D] = {
      a match {
        case Some(v: APIException) => Left(v.asInstanceOf[B])
        case v: APIException => Left(v.asInstanceOf[B])
        case Left(v) => Left(v.asInstanceOf[B])
        case _ => c match {
          case Some(v: APIException) => Left(v.asInstanceOf[B])
          case v: APIException => Left(v.asInstanceOf[B])
          case Left(v) => Left(v.asInstanceOf[B])
          case Right(v) => Right(v.asInstanceOf[D])
          case v => Right(v.asInstanceOf[D])
        }
      }
    }
  }

}
