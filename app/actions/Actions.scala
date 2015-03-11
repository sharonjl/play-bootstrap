package actions

import com.google.common.base.Optional
import dao.UserDAO
import UserDAO._
import play.api.Play.current
import play.api.db.DB
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent._

/*
  Actions related to resource access
 */

/**
 * Indicates a request that has been verified, signed by a valid access token
 *
 * @param user
 * @param request
 * @tparam A
 */
class AuthenticatedRequest[A](val user: User, request: Request[A]) extends WrappedRequest[A](request)

/**
 * Action that only allows requests with a valid access token through. The access token should be
 * specified in the header property 'x-access-token'
 */
object AuthenticatedAction extends ActionBuilder[AuthenticatedRequest] with ActionRefiner[Request, AuthenticatedRequest] {
  def refine[A](request: Request[A]): Future[Either[Result, AuthenticatedRequest[A]]] = Future.successful {
    request.headers.get("x-access-token").map {
      token =>
        if(token.length > 0) {
          DB.withConnection { implicit c =>
            UserDAO.findUserFromSecret(secret = token).map(new AuthenticatedRequest(_, request)).toRight(Forbidden)
          }
        } else {
          Left(Unauthorized)
        }
    } getOrElse {
      Left(Unauthorized)
    }
  }
}


class OptionalAuthenticatedRequest[A](val user: Option[User], request: Request[A]) extends WrappedRequest[A](request)

object OptionalAuthenticatedAction extends ActionBuilder[OptionalAuthenticatedRequest] with ActionRefiner[Request, OptionalAuthenticatedRequest] {
  def refine[A](request: Request[A]): Future[Either[Result, OptionalAuthenticatedRequest[A]]] = Future.successful {
    request.headers.get("x-access-token").map {
      token =>
        if(token.length > 0) {
          DB.withConnection { implicit c =>
            Right(new OptionalAuthenticatedRequest(UserDAO.findUserFromSecret(secret = token), request))
          }
        } else {
          Right(new OptionalAuthenticatedRequest(None, request))
        }
    } getOrElse {
      Left(Unauthorized)
    }
  }
}

/**
 * A trait that extracts authenticated user from the request, if they exist.
 * @tparam A
 */
trait AuthenticatedUser[A] {
  def authenticatedUser: Option[User]
}

object ActionHelper {
  def getAuthenticatedUser[A](request: Request[A]): Option[User] = {
    request match {
      case a: AuthenticatedRequest[_] => Some(a.user)
      case _ => None
    }
  }
}

//// Ask Actions
//class AskRequest[A](val ask: Ask, request: Request[A]) extends WrappedRequest[A](request) with AuthenticatedUser[A] {
//  override def authenticatedUser: Option[User] = ActionHelper.getAuthenticatedUser(request)
//}
//
//case class AskAction(askId: Long) extends ActionBuilder[AskRequest] with ActionRefiner[AuthenticatedRequest, AskRequest] {
//    override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, AskRequest[A]]] = Future.successful {
//      ActionHelper.getAuthenticatedUser(request).flatMap { user =>
//      DB.withConnection { implicit connection =>
//          AskDAO.findAsk(askId).map(new AskRequest(_, request))
//        }
//      }.toRight(Forbidden)
//    }
//  //
//    override def invokeBlock[A](request: Request[A], block: (AskRequest[A]) => Future[Result]): Future[Result] = {
//      request match {
//        case a: AuthenticatedRequest[A] => refine(a).flatMap(_.fold(Future.successful, block))(executionContext)
//        case _ => Future.successful(NotFound)
//      }
//    }
//}
//case class AskOwnerAction() extends ActionFilter[AskRequest] {
//  def filter[A](request: AskRequest[A]): Future[Option[Result]] = Future.successful {
//    request.authenticatedUser.map { owner =>
//      if (owner.id.get == request.ask.userId) None else Some(Forbidden)
//    } getOrElse Some(Forbidden)
//  }
//}
//
//// Response Actions
//class AskResponseRequest[A](val response: Response, request: Request[A]) extends WrappedRequest[A](request) with AuthenticatedUser[A] {
//  override def authenticatedUser: Option[User] = ActionHelper.getAuthenticatedUser(request)
//}
//
//case class ResponseAction(askId: Long) extends ActionBuilder[AskResponseRequest] with ActionRefiner[AuthenticatedRequest, AskResponseRequest] {
//  override protected def refine[A](request: AuthenticatedRequest[A]): Future[Either[Result, AskResponseRequest[A]]] = Future.successful {
//    ActionHelper.getAuthenticatedUser(request).flatMap { user =>
//    DB.withConnection { implicit connection =>
//        AskDAO.findResponseToAsk(askId, user.id.get).map(new AskResponseRequest(_, request))
//      }
//    }.toRight(Forbidden)
//  }
////
//  override def invokeBlock[A](request: Request[A], block: (AskResponseRequest[A]) => Future[Result]): Future[Result] = {
//    request match {
//      case a: AuthenticatedRequest[A] => refine(a).flatMap(_.fold(Future.successful, block))(executionContext)
//      case _ => Future.successful(NotFound)
//    }
//  }
//}
//
//case class ResponseRelatedUsersAction() extends ActionFilter[AskResponseRequest] {
//  def filter[A](request: AskResponseRequest[A]): Future[Option[Result]] = Future.successful {
//    request.authenticatedUser.map { user =>
//      val userId = user.id getOrElse 0L
//      if (userId == request.response.askerId || userId == request.response.responderId) None else Some(Forbidden)
//    } getOrElse Some(Forbidden)
//  }
//}