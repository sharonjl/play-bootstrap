package dao

import java.sql.Connection

import anorm.SqlParser._
import anorm._
import org.postgresql.util.PSQLException
import play.api.Logger
import utils.APIExceptions._
import utils.AnormHelper._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object OAuthServiceDAO {


  case class OAuthService(id: Option[Long],
                          username: String,
                          serviceName: String,
                          serviceUserId: String)

  case class MinimumOAuthService(serviceName: String, serviceUserId: String)

  private val basicService: RowParser[OAuthService] = {
    long("id") ~ str("username") ~ str("service_name") ~ str("service_user_id") map {
      case id ~ username ~ serviceName ~ serviceUserId => OAuthService(Some(id), username, serviceName, serviceUserId)
    }
  }

  /**
   * Register the user for the given service.
   *
   * @param username
   * @param serviceName
   * @param serviceUserId
   * @return
   */
  def registerUserForService(username: String, serviceName: String, serviceUserId: String)(implicit connection: Connection): Try[OAuthService] = {
    Try {
      val service = SQL(
        """INSERT INTO oauth_services(username, service_name, service_user_id)
          |VALUES ({username}, {serviceName}, {serviceUserId})
          |RETURNING *
        """.stripMargin
      ).on(
          'username -> username,
          'serviceName -> serviceName,
          'serviceUserId -> serviceUserId
        ).as(basicService.single)

      Success(service)
    } transform(s => Success(s.get), {
      case e: PSQLException => e.getSQLState match {
        case "23503" => Failure(UserDoesNotExistException() initCause e)
        case "23505" => Failure(OAuthUserAlreadyRegistered() initCause e)
        case s => {
          Logger.debug(s"SQL STATE ${e.getSQLState}" , e)
          Failure(UnknownSQLException(s) initCause e)
        }
      }
      case t: Throwable => Failure(UnknownException() initCause t)
    })
  }

  def findRegisteredOAuthService(serviceName: String, serviceUserId: String)(implicit connection: Connection): Try[Option[OAuthService]] = {
    Try {
      Success(SQL(
        """SELECT * FROM oauth_services
          |WHERE service_name = {serviceName} AND service_user_id = {serviceUserId}
        """.stripMargin
      ).on(
          'serviceName -> serviceName,
          'serviceUserId -> serviceUserId
        ).as(basicService.singleOpt))
    } transform(s => Success(s.get), {
      case e: PSQLException => e.getSQLState match {
        case s => {
          Logger.debug(s"SQL STATE ${e.getSQLState}" , e)
          Failure(UnknownSQLException(s) initCause e)
        }
      }
      case t: Throwable => Failure(UnknownException() initCause t)
    })
  }

  def findRegisteredOAuthService(username: String)(implicit connection: Connection): Try[List[OAuthService]] = {
    Try {
      Success(SQL(
        """SELECT * FROM oauth_services WHERE username = {username}
        """.stripMargin
      ).on(
          'username -> username
        ).as(basicService *).toList)
    } transform(s => Success(s.get), {
      case e: PSQLException => e.getSQLState match {
        case s => {
          Logger.debug(s"SQL STATE ${e.getSQLState}" , e)
          Failure(UnknownSQLException(s) initCause e)
        }
      }
      case t: Throwable => Failure(UnknownException() initCause t)
    })
  }

  def unregisterUserFromService() = {
    //TODO
  }
}
