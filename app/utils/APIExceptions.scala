package utils

import org.postgresql.util.PSQLException
import play.api.Play
import play.api.Play.current
import play.api.libs.json.{JsArray, JsValue, Json, Writes}
import play.api.mvc.Results._

object APIExceptions {
  implicit val stackTraceToJson = new Writes[Array[StackTraceElement]] {
    def writes(stacktrace: Array[StackTraceElement]) = {
      JsArray(stacktrace.map { case line =>
        Json.obj(
          "className" -> line.getClassName,
          "methodName" -> line.getMethodName,
          "fileName" -> line.getFileName,
          "lineNumber" -> line.getLineNumber
        )
      })
    }
  }

  implicit val throwableToJson = new Writes[Throwable] {
    def writes(throwable: Throwable) = {
      throwable match {
        case exception: PSQLException =>
          if (Play.isProd) {
            Json.obj(
              "message" -> throwable.getMessage
            )
          } else {
            Json.obj(
              "message" -> throwable.getMessage,
              "sqlState" -> exception.getSQLState,
              "cause" -> Json.toJson(if (throwable.getCause == null) "N/A" else throwable.getCause.toString),
              "stackTrace" -> Json.toJson(throwable.getStackTrace)
            )
          }
        case _ =>
          if (Play.isProd) {
            Json.obj(
              "message" -> throwable.getMessage
            )
          } else {
            Json.obj(
              "message" -> throwable.getMessage,
              "cause" -> Json.toJson(if (throwable.getCause == null) "N/A" else throwable.getCause.toString),
              "stackTrace" -> Json.toJson(throwable.getStackTrace)
            )
          }
      }
    }
  }

  implicit val apiExceptionToJson = new Writes[APIException] {
    def writes(throwable: APIException) = {
      if (Play.isProd) {
        Json.obj(
          "message" -> throwable.getMessage
        )
      } else {
        Json.obj(
          "message" -> throwable.getMessage,
          "cause" -> Json.toJson(if (throwable.getCause == null) "N/A" else throwable.getCause.toString),
          "stackTrace" -> Json.toJson(throwable.getStackTrace)
        )
      }
    }
  }

  class APIException(val message: String, val httpStatus: Status = BadRequest) extends Throwable(message) {
    var errorTag: String = "error"

    def asHttpStatus(errorTag: String, content: JsValue)(implicit writeable: play.api.http.Writeable[JsValue]) = {
      httpStatus(Json.obj(
        "requestTag" -> errorTag,
        "id" -> getClass.getSimpleName,
        "error" -> content
      ))
    }

    def asHttpStatus(errorTag: String)(implicit writeable: play.api.http.Writeable[JsValue]) = {
      httpStatus(Json.obj(
        "requestTag" -> errorTag,
        "id" -> getClass.getSimpleName,
        "error" -> Json.toJson(this)(apiExceptionToJson)
      ))
    }
  }

  case class DataValidationException(override val message: String = "Data validation exception") extends APIException(message, BadRequest) {
    override def asHttpStatus(errorTag: String, content: JsValue)(implicit writeable: play.api.http.Writeable[JsValue]) = {
      httpStatus(Json.obj(
        "requestTag" -> errorTag,
        "id" -> getClass.getSimpleName,
        "validation" -> content
      ))
    }
  }


  case class UserExistsException(override val message: String = "Username already exists") extends APIException(message, BadRequest)

  case class UserDoesNotExistException(override val message: String = "Username does not exist") extends APIException(message, NotFound)

  case class InvalidUsernameOrPasswordException(override val message: String = "Invalid username or password") extends APIException(message, BadRequest)

  case class UsernameRequirementsException(override val message: String = "Username does not meet requirements") extends APIException(message, BadRequest)

  case class PasswordRequirementsException(override val message: String = "Password does not meet requirements") extends APIException(message, BadRequest)

  case class UnknownException(override val message: String = "Unknown error occurred") extends APIException(message, InternalServerError)

  case class UnknownSQLException(sqlCode: String, override val message: String = "Unknown SQL error occurred") extends APIException(message, InternalServerError) {
    override def asHttpStatus(errorTag: String, content: JsValue)(implicit writeable: play.api.http.Writeable[JsValue]) = {
      httpStatus(Json.obj(
        "tag" -> errorTag,
        "sqlState" -> sqlCode,
        "error" -> content
      ))
    }
  }

  case class OAuthInvalidTokenException(override val message: String = "OAuth service invalid Access Token") extends APIException(message, BadRequest)

  case class OAuthInvalidServiceException(override val message: String = "OAuth service name is invalid") extends APIException(message, BadRequest)

  case class OAuthTokenVerificationException(override val message: String = "Unable to verify given service token") extends APIException(message, BadRequest)

  case class OAuthUserAlreadyRegistered(override val message: String = "User already registered for the service") extends APIException(message, BadRequest)

  case class OAuthForeignTokenException(override val message: String = "The given oauth token is not associated to a registered or the user has not registered for the oauth service.") extends APIException(message, BadRequest)

}