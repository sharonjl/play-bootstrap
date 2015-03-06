package controllers.bootstrap

import actions.AuthenticatedAction
import dao.OAuthServiceDAO.OAuthService
import dao.UserDAO
import dao.UserDAO._
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.DB
import play.api.libs.json.Json
import play.api.libs.json.Json._
import play.api.mvc.{Action, Controller}
import utils.APIExceptions._

import scala.util.{Failure, Success}

/**
 * Created by sharon on 2014-07-25.
 */

object UserController extends Controller {

  implicit val jsonUserRightsWrites = Json.writes[Rights]
  implicit val jsonUserTokenWrites = Json.writes[AccessToken]
  implicit val jsonUserWrites = Json.writes[User]
  implicit val jsonOAuthServiceWrites = Json.writes[OAuthService]

  case class UserBody(username: String, password: String)

  val userForm = Form(
    mapping(
      "username" -> text(minLength = 3),
      "password" -> text(minLength = 3)
    )(UserBody.apply)(UserBody.unapply)
  )

  def create = Action { implicit request =>
    val errorTag = "user.create"
    userForm.bindFromRequest.fold(
      formWithErrors => {
        DataValidationException().asHttpStatus(errorTag, formWithErrors.errorsAsJson)
      },
      newUserData => {
        DB.withConnection { implicit c =>
          UserDAO.createUser(newUserData.username, newUserData.password) match {
            case Success(user) => Ok(toJson(user)) // Register user
            case Failure(e: APIException) => e.asHttpStatus(errorTag, toJson(e))
          }
        }
      }
    )
  }

  def login = Action { implicit request =>
    val errorTag = "user.login"
    userForm.bindFromRequest.fold(
      formWithErrors => {
        BadRequest(formWithErrors.errorsAsJson)
      },
      newUserData => {
        DB.withConnection { implicit c =>
          UserDAO.refreshAccessToken(newUserData.username, newUserData.password) match {
            case Some(user) => {
              Ok(toJson(user))
            }
            case None => InvalidUsernameOrPasswordException().asHttpStatus(errorTag)
          }
        }
      }
    )
  }

  def find(username: Option[String]) = AuthenticatedAction { implicit request =>
    val errorTag = "user.find"
    DB.withConnection { implicit c =>
      UserDAO.findUser(username getOrElse request.user.username) match {
        case Success(user) => Ok(toJson(user))
        case Failure(e: APIException) => e.asHttpStatus(errorTag, toJson(e))
      }
    }
  }

//  def profileProgress(progress: Int) = AuthenticatedAction { implicit request =>
//    val errorTag = "user.profileProgress"
//    DB.withConnection { implicit c =>
//      UserDAO.setProfileProgress(request.user.username, progress) match {
//        case Success(user) => NoContent
//        case Failure(e: APIException) => e.asHttpStatus(errorTag, toJson(e))
//      }
//    }
//  }
}