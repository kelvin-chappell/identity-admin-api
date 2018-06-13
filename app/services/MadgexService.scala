package services

import javax.inject.{Inject, Singleton}

import com.gu.identity.util.Logging
import configuration.Config
import util.UserConverter._
import models.client.{GNMMadgexUser, MadgexUser}
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.Json

@Singleton class MadgexService @Inject() (
    ws: WSClient, requestSigner: RequestSigner)(implicit ec: ExecutionContext) extends Logging {

  implicit val format = Json.format[MadgexUser]

  def update(user: GNMMadgexUser): Future[Unit] = {
    require(user.madgexUser.receive3rdPartyMarketing == false, "do not set old consents")
    require(user.madgexUser.receiveGnmMarketing == false, "do not set old consents")

    val id = user.id
    requestSigner.sign(ws.url(s"${Config.Madgex.apiUrl}/updatessouser/$id"))
      .post(Json.toJson(user.madgexUser))
      .map { _.status match {
        case 200 =>
        case 401 => logger.error("Failed to authenticate with Madgex API.")
        case status => logger.error(s"Unexpected status code:  $status from Madgex API")
      }
      }.recover {
      case t: Throwable => logger.error("Error when updating Madgex", t)
    }
  }
}

