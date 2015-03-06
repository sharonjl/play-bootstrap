package controllers.bootstrap

;

import actions.AuthenticatedAction
import controllers.bootstrap.UserController._
import dao.OAuthServiceDAO.OAuthService
import dao.UserDAO._
import dao.{OAuthServiceDAO, UserDAO}
import play.api.Play
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.DB
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json._
import play.api.libs.json.{JsError, JsResult, JsSuccess, Json}
import play.api.mvc.{Action, Controller}
import utils.APIExceptions._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by sharon on 15-03-01.
 */
object OAuthController extends Controller {

  implicit val jsonOAuthServiceWrites = Json.writes[OAuthService]

  case class ServiceForm(serviceName: String, serviceUserId: String, serviceUserToken: String)

  val serviceForm = Form(
    mapping(
      "serviceName" -> nonEmptyText(3),
      "serviceUserId" -> nonEmptyText(3),
      "serviceUserToken" -> nonEmptyText(3)
    )(ServiceForm.apply)(ServiceForm.unapply)
  )

  case class ExchangeOAuthTokenForm(serviceName: String, serviceUserId: String, serviceUserToken: String)

  val exchangeOAuthTokenForm = Form(
    mapping(
      "serviceName" -> nonEmptyText(3),
      "serviceUserId" -> nonEmptyText(3),
      "serviceUserToken" -> nonEmptyText(3)
    )(ExchangeOAuthTokenForm.apply)(ExchangeOAuthTokenForm.unapply)
  )

  case class LoginOAuthTokenForm(serviceName: String, serviceUserId: String, serviceUserToken: String)

  val loginOAuthTokenForm = Form(
    mapping(
      "serviceName" -> nonEmptyText(3),
      "serviceUserId" -> nonEmptyText(3),
      "serviceUserToken" -> nonEmptyText(3)
    )(LoginOAuthTokenForm.apply)(LoginOAuthTokenForm.unapply)
  )

  case class FacebookAccessToken(app_id: String,
                                 application: String,
                                 expires_at: Long,
                                 is_valid: Boolean,
                                 user_id: String)

  implicit val fbAccessTokenReads = Json.reads[FacebookAccessToken]

  private def verifyFacebookAccessToken(inputToken: String, accessToken: String): Future[JsResult[FacebookAccessToken]] = {
    import play.api.Play.current
    import play.api.libs.ws._

    val holder: WSRequestHolder = WS.url("https://graph.facebook.com/debug_token")
      .withQueryString("input_token" -> inputToken)
      .withQueryString("access_token" -> accessToken)

    holder.get().map {
      response =>
        (response.json \ "data").validate[FacebookAccessToken]
    }
  }

  def registerService = AuthenticatedAction.async { implicit request =>
    val errorTag = "service.registerService"
    serviceForm.bindFromRequest.fold({
      formWithErrors => Future {
        DataValidationException().asHttpStatus(errorTag, formWithErrors.errorsAsJson)
      }
    }, data => {
      data.serviceName match {
        case "facebook" =>
          val fbAccessToken = Play.current.configuration.getString("facebook.accessToken") getOrElse "FACEBOOK_ACCESS_TOKEN"
          val fbAppId = Play.current.configuration.getString("facebook.appId") getOrElse "FACEBOOK_APP_ID"

          verifyFacebookAccessToken(data.serviceUserToken, fbAccessToken) map {
            case s: JsSuccess[FacebookAccessToken] =>
              val accessToken: FacebookAccessToken = s.get
              if (accessToken.is_valid
                && accessToken.expires_at < System.currentTimeMillis()
                && accessToken.app_id.contentEquals(fbAppId)) {

                DB.withConnection { implicit c =>
                  OAuthServiceDAO.registerUserForService(request.user.username, data.serviceName, data.serviceUserId) match {
                    case Success(service: OAuthService) => Ok
                    case Failure(e: APIException) => e.asHttpStatus(errorTag)
                  }
                }
              } else {
                OAuthInvalidTokenException().asHttpStatus(errorTag)
              }
            case e: JsError => OAuthTokenVerificationException().asHttpStatus(errorTag)
          }
        case _ => Future {
          OAuthInvalidServiceException().asHttpStatus(errorTag)
        }
      }
    })
  }

  def exchangeOAuthToken = Action.async { implicit request =>
    val errorTag = "service.exchangeOAuthToken"
    exchangeOAuthTokenForm.bindFromRequest.fold({
      formWithErrors => Future {
        DataValidationException().asHttpStatus(errorTag, formWithErrors.errorsAsJson)
      }
    }, data => {
      data.serviceName match {
        case "facebook" =>
          val fbAccessToken = Play.current.configuration.getString("facebook.accessToken") getOrElse "FACEBOOK_ACCESS_TOKEN"
          val fbAppId = Play.current.configuration.getString("facebook.appId") getOrElse "FACEBOOK_APP_ID"

          verifyFacebookAccessToken(data.serviceUserToken, fbAccessToken) map {
            case s: JsSuccess[FacebookAccessToken] =>
              val accessToken: FacebookAccessToken = s.get

              // Refresh user token if the access token is valid
              if (accessToken.is_valid
                && accessToken.expires_at < System.currentTimeMillis()
                && accessToken.app_id.contentEquals(fbAppId)
                && accessToken.user_id == data.serviceUserId) {

                DB.withConnection { implicit c =>
                  UserDAO.refreshAccessToken(data.serviceUserId) match {
                    case Some(user) => {
                      Ok(toJson(user))
                    }
                    case None => OAuthForeignTokenException().asHttpStatus(errorTag)
                  }
                }
              } else {
                OAuthInvalidTokenException().asHttpStatus(errorTag)
              }
            case e: JsError => OAuthTokenVerificationException().asHttpStatus(errorTag)
          }
        case _ => Future {
          OAuthInvalidServiceException().asHttpStatus(errorTag)
        }
      }
    })
  }

  def loginOAuthToken = Action.async { implicit request =>
    val errorTag = "service.loginOAuthToken"
    loginOAuthTokenForm.bindFromRequest.fold({
      formWithErrors => Future {
        DataValidationException().asHttpStatus(errorTag, formWithErrors.errorsAsJson)
      }
    }, data => {
      data.serviceName match {
        case "facebook" =>
          val fbAccessToken = Play.current.configuration.getString("facebook.accessToken") getOrElse "FACEBOOK_ACCESS_TOKEN"
          val fbAppId = Play.current.configuration.getString("facebook.appId") getOrElse "FACEBOOK_APP_ID"

          verifyFacebookAccessToken(data.serviceUserToken, fbAccessToken) map {
            case s: JsSuccess[FacebookAccessToken] =>
              val accessToken: FacebookAccessToken = s.get

              // Refresh user token if the access token is valid
              if (accessToken.is_valid
                && accessToken.expires_at < System.currentTimeMillis()
                && accessToken.app_id.contentEquals(fbAppId)
                && accessToken.user_id == data.serviceUserId) {

                DB.withTransaction { implicit c =>
                  OAuthServiceDAO.findRegisteredOAuthService(data.serviceName, data.serviceUserId) match {
                    case Success(serviceOpt: Option[OAuthService]) => serviceOpt match {
                      case Some(service) => // Refresh access token if service is already registered
                        UserDAO.refreshAccessToken(data.serviceUserId) match {
                          case Some(user: User) => Ok(Json.obj("newUser" -> false, "user" -> toJson(user)))
                          case None => OAuthForeignTokenException().asHttpStatus(errorTag)
                        }
                      case None =>
                        // The service is not registered so create an account
                        UserDAO.createAccount() match {
                          case Success(user: User) => {
                            // Register the current service for the user
                            OAuthServiceDAO.registerUserForService(user.username, data.serviceName, data.serviceUserId) match {
                              case Success(service: OAuthService) =>
                                // Refresh access token once service is registered to the account
                                UserDAO.refreshAccessToken(data.serviceUserId) match {
                                  case Some(user: User) => Ok(Json.obj("newUser" -> true, "user" -> toJson(user)))
                                  case None => OAuthForeignTokenException().asHttpStatus(errorTag)
                                }
                              case Failure(e: APIException) => e.asHttpStatus(errorTag)
                            }
                          }
                          case Failure(e: APIException) => e.asHttpStatus(errorTag)
                        }
                    }
                    case Failure(e: APIException) => e.asHttpStatus(errorTag)

                  }
                }
              } else {
                OAuthInvalidTokenException().asHttpStatus(errorTag)
              }
            case e: JsError => OAuthTokenVerificationException().asHttpStatus(errorTag)
          }
        case _ => Future {
          OAuthInvalidServiceException().asHttpStatus(errorTag)
        }
      }
    })
  }

}
