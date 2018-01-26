package services

import mockws.MockWS
import models.client.ApiError
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.mvc.Action
import play.api.mvc.Results._
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext.Implicits.global

class IdentityApiClientTest
  extends WordSpec with Matchers with BeforeAndAfterEach with GuiceOneServerPerSuite with BeforeAndAfterAll{

  val Port = 8698
  val Host = "localhost"
  val ClientToken = "TestToken"
  val UserId = "123"
  val sendValidationEmailUrl= s"http://$Host:$Port/user/$UserId/send-validation-email"
  val wsOk = MockWS { case (POST, `sendValidationEmailUrl`) => Action { Ok } }

  val errorJson =
    """
      {
       "status":"error",
       "errors":[
        {"message":"fatal error","description":"boom"}
       ]
      }
    """

  val wsError = MockWS { case (POST, `sendValidationEmailUrl`) => Action { InternalServerError(errorJson) } }

  val clientOk = new IdentityApiClient(wsOk) {
    override val baseUrl = s"http://$Host:$Port"
    override val clientToken = ClientToken
  }

  val clientError = new IdentityApiClient(wsError) {
    override val baseUrl = s"http://$Host:$Port"
    override val clientToken = ClientToken
  }

  "sendEmailValidation" should {
    "return Right when successful" in {
      clientOk.sendEmailValidation(UserId).map(_ shouldEqual Right(true))
    }
    
    "return Left when unsuccessful" in {
      clientError.sendEmailValidation(UserId).map(_ shouldEqual Left(ApiError("Internal Server Error", "fatal error")))
    }
  }

}
