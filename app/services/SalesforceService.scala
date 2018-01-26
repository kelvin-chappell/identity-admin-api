package services

import javax.inject.{Inject, Singleton}

import com.gu.identity.util.Logging
import configuration.Config.TouchpointSalesforce._
import models.client._
import play.api.libs.json.{JsArray, Json}
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.http.Status.OK

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, \/-}

case class SalesforceError(msg: String) extends Exception(msg)

case class SFAuthentication(access_token: String, instance_url: String)

object SFAuthentication {
  implicit val format = Json.format[SFAuthentication]
}

case class SFContact(Id: String, Name: String, Email: String, IdentityID__c: String)

object SFContact {
  implicit val format = Json.format[SFContact]
}

trait UniqueIdentifier {
  val value: String
  val fieldName: String
}
case class MembershipNumber(value: String, fieldName: String = "Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Contact_Number__c") extends UniqueIdentifier
case class IdentityId(value: String, fieldName: String = "Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.IdentityID__c") extends UniqueIdentifier
case class Email(value: String, fieldName: String = "Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Email") extends UniqueIdentifier
case class SubscriptionId(value: String, fieldName: String = "Subscription_Name__c") extends UniqueIdentifier

@Singleton class SalesforceService @Inject() (ws: WSClient)(implicit ec: ExecutionContext) extends Logging {

  private lazy val sfAuth: SFAuthentication = {
    logger.info("Authenticating with Salesforce...")
    val authEndpoint = s"${apiUrl}/services/oauth2/token"

    val param = Map(
      "client_id" -> consumerKey,
      "client_secret" -> consumerSecret,
      "username" -> apiUsername,
      "password" -> (apiPassword + apiToken),
      "grant_type" -> "password"
    ).map { case (k, v) => s"$k=$v" }.mkString("&")

    val request = ws.url(s"$authEndpoint?$param")

    val response = Try(Await.result(request.post(""), 10.second))

    response match {
      case Success(res) =>
        if (res.status == OK) Json.parse(res.body).as[SFAuthentication]
        else throw new SalesforceError(s"Authentication failure: ${res.body.toString}")
      case Failure(e) => throw new SalesforceError(s"${e.getMessage}")
    }
  }

  private val authHeader = ("Authorization", s"Bearer ${sfAuth.access_token}")

  private def extractSubscription(res: WSResponse): SalesforceSubscription = {
    val records: JsArray = (res.json \ "records").as[JsArray]

    SalesforceSubscription(
      tier = (records(0) \ "Zuora__ProductName__c").asOpt[String],
      subscriberId = (records(0) \ "Subscription_Name__c").asOpt[String],
      membershipNumber = (records(0) \ "Zuora__Subscription__r" \ "Zuora__CustomerAccount__r" \ "Contact__r" \ "Contact_Number__c").asOpt[String],
      joinDate = Some((records(0) \ "Zuora__EffectiveStartDate__c").as[String]),
      end = Some((records(0) \ "Zuora__EffectiveEndDate__c").as[String]),
      zuoraSubscriptionName = Some((records(0) \ "Subscription_Name__c").as[String]),
      identityId = (records(0) \ "Zuora__Subscription__r" \ "Zuora__CustomerAccount__r" \ "Contact__r" \ "IdentityID__c").asOpt[String].getOrElse("orphan"),
      email = (records(0) \ "Zuora__Subscription__r" \ "Zuora__CustomerAccount__r" \ "Contact__r" \ "Email").as[String],
      deliveryAddress = Some((records(0) \ "Zuora__Subscription__r" \ "Zuora__CustomerAccount__r" \ "Contact__r").as[SalesforceDeliveryAddress]),
      billingAddress = Some((records(0) \ "Zuora__Subscription__r" \ "Zuora__CustomerAccount__r" \ "Contact__r").as[SalesforceBillingAddress])
    )
  }

  private def querySalesforce(soql: String): ApiResponse[Option[SalesforceSubscription]] =
    ws.url(s"${sfAuth.instance_url}/services/data/v29.0/query?q=$soql").addHttpHeaders(authHeader).get().map { response =>
      if (response.status == OK) {
        if ((response.json \ "totalSize").as[Int] > 0)
          \/-(Some(extractSubscription(response)))
        else
          \/-(None)
      }
      else {
        val title = "Could not get subscriptions from Salesforce"
        val details = response.body
        logger.error(s"$title: $details")
        -\/(ApiError(title, details))
      }
    }.recover { case error => -\/(ApiError("Failed to communicate with Salesforce", error.getMessage)) }

  private val selectQuerySectionDeliveryAddress: String =
    """
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Delivery_Information__c,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.MailingStreet,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.MailingCity,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.MailingPostalCode,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.MailingCountry
    """.stripMargin

  private val selectQuerySectionBillingAddress: String =
    """
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.OtherStreet,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.OtherCity,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.OtherPostalCode,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.OtherCountry
    """.stripMargin


  private val selectQuerySection: String =
    s"""
      |SELECT
      |    Id,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.IdentityID__c,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Membership_Number__c,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Contact_Number__c,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Email,
      |    Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Name,
      |
      |    ${selectQuerySectionDeliveryAddress},
      |
      |    ${selectQuerySectionBillingAddress},
      |
      |    Zuora__ProductName__c,
      |    Subscription_Status__c,
      |    Subscription_Name__c,
      |    Zuora__EffectiveStartDate__c,
      |    Zuora__EffectiveEndDate__c,
      |    Zuora__Subscription__r.ActivationDate__c
    """.stripMargin

  private val fromQuerySection: String =
    """
      |FROM
      |  Zuora__SubscriptionProductCharge__c
    """.stripMargin

  private val orderByQuerySection: String =
    """
      |ORDER BY
      |  Zuora__EffectiveStartDate__c DESC NULLS LAST
    """.stripMargin


  private def getSubscriptionBy(uniqueIdentifier: UniqueIdentifier): ApiResponse[Option[SalesforceSubscription]] = {

    val sooqlQuery =
      s"""
         |$selectQuerySection
         |
         |$fromQuerySection
         |
         |WHERE
         |  (${uniqueIdentifier.fieldName}  = '${uniqueIdentifier.value}') AND
         |  (Subscription_Status__c = 'Active') AND
         |  (
         |    (Zuora__ProductName__c = 'Digital Pack') OR
         |    (Zuora__ProductName__c = 'Newspaper Voucher') OR
         |    (Zuora__ProductName__c = 'Newspaper Delivery') OR
         |    (Zuora__ProductName__c = 'Guardian Weekly Zone A') OR
         |    (Zuora__ProductName__c = 'Guardian Weekly Zone B') OR
         |    (Zuora__ProductName__c = 'Guardian Weekly Zone C')
         |  ) AND
         |  (Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Email  != null)
         |
         |$orderByQuerySection
      """.stripMargin

    querySalesforce(sooqlQuery)
  }



  private def getMembershipBy(uniqueIdentifier: UniqueIdentifier): ApiResponse[Option[SalesforceSubscription]] = {
    val sooqlQuery =
      s"""
         |$selectQuerySection
         |
         |$fromQuerySection
         |
         |WHERE
         |  (${uniqueIdentifier.fieldName} = '${uniqueIdentifier.value}') AND
         |  (Subscription_Status__c = 'Active') AND
         |  (
         |    (Zuora__ProductName__c = 'Friend') OR
         |    (Zuora__ProductName__c = 'Supporter') OR
         |    (Zuora__ProductName__c = 'Partner') OR
         |    (Zuora__ProductName__c = 'Patron') OR
         |    (Zuora__ProductName__c = 'Staff Membership')
         |  ) AND
         |  (Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.IdentityID__c  != null) AND
         |  (Zuora__Subscription__r.Zuora__CustomerAccount__r.Contact__r.Email  != null)
         |
         |$orderByQuerySection
      """.stripMargin

    querySalesforce(sooqlQuery)
  }

  def getMembershipByIdentityId(id: String): ApiResponse[Option[SalesforceSubscription]] =
    getMembershipBy(IdentityId(id))

  def getMembershipByMembershipNumber(membershipNumber: String): ApiResponse[Option[SalesforceSubscription]] =
    getMembershipBy(MembershipNumber(membershipNumber))

  def getMembershipByEmail(email: String): ApiResponse[Option[SalesforceSubscription]] =
    getMembershipBy(Email(email))

  def getMembershipBySubscriptionId(subscriptionId: String): ApiResponse[Option[SalesforceSubscription]] =
    getMembershipBy(SubscriptionId(subscriptionId))

  def getSubscriptionByIdentityId(id: String): ApiResponse[Option[SalesforceSubscription]] =
    getSubscriptionBy(IdentityId(id))

  def getSubscriptionByEmail(email: String): ApiResponse[Option[SalesforceSubscription]] =
    getSubscriptionBy(Email(email))

  def getSubscriptionBySubscriptionId(subscriptionId: String): ApiResponse[Option[SalesforceSubscription]] =
    getSubscriptionBy(SubscriptionId(subscriptionId))
}
