package dao

import java.sql.Connection
import java.util.Date

import anorm.SqlParser._
import anorm._
import com.roundeights.hasher.Implicits._
import org.postgresql.util.PSQLException
import play.api.Logger
import utils.APIExceptions._
import utils.AnormHelper._

import scala.language.postfixOps
import scala.util.{Failure, Random, Success, Try}

object UserDAO {


  case class User(id: Option[Long],
                  username: String,
                  accessToken: Option[AccessToken] = None,
                  right: Option[Rights] = None)

  case class AccessToken(secret: String, expiry: Long, firebaseToken: Option[String] = None)

  case class Rights(admin: Boolean = false)

  private val basicUser: RowParser[User] = {
    long("id") ~ str("username") map {
      case id ~ username => User(Some(id), username)
    }
  }

  private val usernameRegex = """^([A-Za-z0-9\-\_]+){3,}$""".r

  def validateUsername(u: String): Try[String] = {
    if (usernameRegex.pattern.matcher(u).matches()) Success(u) else Failure(UsernameRequirementsException())
  }

  def checkPasswordLength(p: String): Try[String] = {
    if (p.length < 3) Failure(PasswordRequirementsException()) else Success(p)
  }

  /**
   *
   * @param username
   * @param password
   * @return
   */
  def createUser(username: String, password: String)(implicit connection: Connection): Try[User] = {
    for {
      u <- validateUsername(username)
      p <- checkPasswordLength(password)
      user <- {
        Try {
          val user = SQL(
            """INSERT INTO users(username, password_hash)
              |VALUES ({username}, crypt({password}, gen_salt('bf', 8)))
              |RETURNING *
            """.stripMargin
          ).on(
              'username -> u,
              'password -> p
            ).as(basicUser.single)
          Success(user)
        } transform(s => Success(s.get), {
          case e: PSQLException => e.getSQLState match {
            case "23505" => Failure(UserExistsException())
            case _ => Failure(UnknownException())
          }
          case t: Throwable => Failure(UnknownException())
        })
      }
    } yield user
  }
//
//  def setProfileProgress(username: String, completion: Int)(implicit connection: Connection): Try[User] = {
//    Try {
//      SQL(
//        """UPDATE users SET profile_progress = {completion}
//          |WHERE username = {username}
//          |RETURNING *
//        """.stripMargin
//      ).on(
//          'username -> username,
//          'completion -> completion
//        ).as(basicUser.singleOpt) map {
//        Success(_)
//      } getOrElse Failure(UserDoesNotExistException())
//    } transform(s => Success(s.get), {
//      case e: PSQLException => e.getSQLState match {
//        case _ => Failure(UnknownException())
//      }
//      case t: Throwable => Failure(UnknownException())
//    })
//  }

  def createAccount()(implicit connection: Connection): Try[User] = {
    Try {
      val user = SQL(
        """INSERT INTO users(username, password_hash)
          |VALUES (cast(uuid_generate_v4() as text), crypt(cast(uuid_generate_v4() as text), gen_salt('bf', 8)))
          |RETURNING *
        """.stripMargin
      ).as(basicUser.single)
      Success(user)
    } transform(s => Success(s.get), {
      case e: PSQLException => e.getSQLState match {
        case "23505" => Failure(UserExistsException())
        case s => {
          Logger.debug(s"SQL STATE ${e.getSQLState}", e)
          Failure(UnknownSQLException(s) initCause e)
        }
      }
      case t: Throwable => Failure(UnknownException() initCause(t))
    })
  }


  /**
   *
   * @param username
   * @param password
   * @return
   */
  def verifyUser(username: String, password: String)(implicit connection: Connection): Option[User] = {
    SQL(
      """SELECT * FROM users WHERE username = {username} AND password_hash = crypt({password}, password_hash)
      """.stripMargin
    ).on(
        'username -> username,
        'password -> password
      ).as(basicUser.singleOpt)
  }

  /**
   *
   * @param username
   * @param password
   * @return
   */
  def refreshAccessToken(username: String, password: String)(implicit connection: Connection): Option[User] = {
    val secret = generateUniqueSecretSHA(username)

    SQL(
      """UPDATE users SET access_token_hash = crypt({secret}, gen_salt('bf', 8)), access_token_expiry = now() + interval '1 day'
        |WHERE username = {username} AND password_hash = crypt({password}, password_hash)
        |RETURNING *
      """.stripMargin
    ).on(
        'username -> username,
        'password -> password,
        'secret -> secret
      ).map {
      row =>
        val id = row[Long]("id")
        val username = row[String]("username")
        val tokenExpiry = row[Date]("access_token_expiry")

        User(id = Some(id),
          username = username,
          accessToken = Some(AccessToken(
            secret = secret,
            expiry = tokenExpiry.getTime,
            firebaseToken = None
          )
          ))
    }.singleOpt()
  }

  def refreshAccessToken(serviceUserId: String)(implicit connection: Connection): Option[User] = {
    val secret = generateUniqueSecretSHA(serviceUserId)

    SQL(
      """UPDATE users SET
        | access_token_hash = crypt({secret}, gen_salt('bf', 8)),
        | access_token_expiry = now() + interval '1 day'
        |WHERE users.username = (
        |	SELECT username FROM oauth_services WHERE oauth_services.service_user_id = {serviceUserId}
        |) RETURNING *
      """.stripMargin
    ).on(
        'serviceUserId -> serviceUserId,
        'secret -> secret
      ).map {
      row =>
        val id = row[Long]("id")
        val username = row[String]("username")
        val tokenExpiry = row[Date]("access_token_expiry")

        User(id = Some(id),
          username = username,
          accessToken = Some(AccessToken(
            secret = secret,
            expiry = tokenExpiry.getTime,
            firebaseToken = None)
          ))
    }.singleOpt()
  }

  /**
   * TODO: FIX EXPIRY DATES
   *
   */
  //  private def generateFirebaseToken(username: String): String = {
  //    val payload = HashMap("uid"->username.asInstanceOf[AnyRef]).asJava
  //    val tokenGenerator: TokenGenerator = new TokenGenerator(Play.current.configuration.getString("firebase.secretKey").get)
  //
  //    tokenGenerator.createToken(payload)
  //  }

  /**
   *
   * @param username
   * @return
   */
  private def generateUniqueSecret(username: String): String = {
    Array(
      username, System.currentTimeMillis().toString, Random.nextString(12)
    ).mkString(",")
  }

  private def generateUniqueSecretSHA(username: String) = {
    val b = generateUniqueSecret(username).salt(Random.nextString(12)).sha256.bytes

    new sun.misc.BASE64Encoder().encode(b)
  }

  private val AccessTokenRegex = """([-0-9a-zA-Z.+_]+)\,([-0-9a-zA-Z.+_]+)\,([-0-9a-zA-Z.+_]+)""".r

  private def generateAccessToken(username: String, secret: String, expiry: Long) = {
    s"$secret,$expiry,$username"
  }

  /**
   *
   * @param secret
   * @return
   */
  def findUserFromSecret(secret: String)(implicit connection: Connection): Option[User] = {
    SQL(
      """SELECT * FROM users WHERE access_token_hash = crypt({secret}, access_token_hash) AND access_token_expiry >= now()
      """.stripMargin
    ).on(
        'secret -> secret
      ).as(basicUser.singleOpt)
  }

  def findUser(id: Long)(implicit connection: Connection): Option[User] = {
    SQL(
      """SELECT * FROM users WHERE id = {id}
      """.stripMargin
    ).on(
        'id -> id
      ).as(basicUser.singleOpt)
  }

  def findUser(username: String)(implicit connection: Connection): Try[User] = {
    for {
      u <- validateUsername(username)
      user <- {
        Try {
          SQL(
            """SELECT * FROM users WHERE username = {username}
            """.stripMargin
          ).on(
              'username -> username
            ).as(basicUser.singleOpt) map {
            Success(_)
          } getOrElse Failure(UserDoesNotExistException())
        } transform(s => Success(s.get), {
          case e: PSQLException => e.getSQLState match {
            case _ => Failure(UnknownException())
          }
          case t: Throwable => Failure(UnknownException())
        })
      }
    } yield user
  }
}
