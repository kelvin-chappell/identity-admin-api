package models.client

import ClientJsonFormats._
import models.database.mongo.IdentityUser
import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json.Json

import scala.language.implicitConversions

/*
    These case class models are intended for serialisation to JSON for consumption by Identity Admin
 */

case class PersonalDetails(firstName: Option[String] = None,
                           lastName: Option[String] = None,
                           gender: Option[String] = None,
                           dateOfBirth: Option[LocalDate] = None,
                           aboutMe: Option[String] = None,
                           interests: Option[String] = None,
                           location: Option[String] = None)

object PersonalDetails {
  implicit val format = Json.format[PersonalDetails]
}

case class Address(addressLine1: Option[String] = None,
                   addressLine2: Option[String] = None,
                   addressLine3: Option[String] = None,
                   addressLine4: Option[String] = None,
                   country: Option[String] = None,
                   postcode: Option[String] = None)

object Address {
  implicit val format = Json.format[Address]
}

case class UserStatus(
  userEmailValidated: Option[Boolean] = None,
  hasRepermissioned: Option[Boolean] = None)

object UserStatus {
  implicit val format = Json.format[UserStatus]
}

case class UserGroup(packageCode: String,
                     path:String,
                     joinDate: Option[DateTime] = None)

object UserGroup {
  implicit val format = Json.format[UserGroup]
}

case class SocialLink(socialId: String,
                      network: String)

object SocialLink {
  implicit val format = Json.format[SocialLink]
}

case class LastActiveLocation(countryCode: Option[String] = None,
                              cityCode : Option[String] = None)

object LastActiveLocation {
  implicit val format = Json.format[LastActiveLocation]
}

case class SalesforceDeliveryAddress(
  Delivery_Information__c: Option[String],
  MailingStreet: Option[String],
  MailingCity: Option[String],
  MailingPostalCode: Option[String],
  MailingCountry: Option[String]
)

object SalesforceDeliveryAddress {
  implicit val format = Json.format[SalesforceDeliveryAddress]
}

case class SalesforceBillingAddress(
  OtherStreet: Option[String],
  OtherCity: Option[String],
  OtherPostalCode: Option[String],
  OtherCountry: Option[String]
)

object SalesforceBillingAddress {
  implicit val format = Json.format[SalesforceBillingAddress]
}

case class SalesforceSubscription(
  tier: Option[String] = None,
  subscriberId: Option[String] = None,
  membershipNumber: Option[String] = None,
  joinDate: Option[String] = None,
  end: Option[String] = None,
  activationDate: Option[String] = None,
  zuoraSubscriptionName: Option[String] = None,
  identityId: String = "orphan",
  email: String,
  deliveryAddress: Option[SalesforceDeliveryAddress] = None,
  billingAddress: Option[SalesforceBillingAddress] = None
)

object SalesforceSubscription {
  implicit val format = Json.format[SalesforceSubscription]
}

case class NewslettersSubscription(list: List[String])

object NewslettersSubscription {
  implicit val format = Json.format[NewslettersSubscription]
}

case class ConsumerEmail(
  emailName: String,
  deliveredDate: String,
  openTime: String
)

object ConsumerEmail {
  implicit val format = Json.format[ConsumerEmail]
}

case class ConsumerEmails(list: List[ConsumerEmail])

object ConsumerEmails {
  implicit val format = Json.format[ConsumerEmails]
}

case class ExactTargetSubscriber(
  status: String, newsletters:
  Option[NewslettersSubscription],
  consumerEmails: Option[ConsumerEmails])

object ExactTargetSubscriber {
  implicit val format = Json.format[ExactTargetSubscriber]
}

case class Contribution(date: String, currency: String, amount: String)

object Contribution {
  implicit val format = Json.format[Contribution]
}

case class Consent(
  actor: String,
  id: String,
  version: Int,
  consented: Boolean,
  timestamp: DateTime,
  privacyPolicyVersion: Int
)

object Consent {
  implicit val format = Json.format[Consent]
}

case class User(
  id: String,
  email: String,
  displayName: Option[String] = None,
  username: Option[String] = None,
  personalDetails: PersonalDetails = PersonalDetails(),
  deliveryAddress: Address = Address(),
  billingAddress: Address = Address(),
  lastActivityDate: Option[DateTime] = None,
  lastActivityIp: Option[String] = None,
  registrationType: Option[String] = None,
  registrationDate: Option[DateTime] = None,
  registrationIp: Option[String] = None,
  status: UserStatus = UserStatus(),
  consents: List[Consent] = Nil,
  groups: Seq[UserGroup] = Nil,
  socialLinks: Seq[SocialLink] = Nil
)

object User {
  implicit val format = Json.format[User]

  def apply(user: IdentityUser): User =
    User(
      id = user._id,
      email = user.primaryEmailAddress,
      displayName = user.publicFields.flatMap(_.displayName),
      username = user.publicFields.flatMap(_.username),
      personalDetails = PersonalDetails(
       firstName = user.privateFields.flatMap(_.firstName),
       lastName = user.privateFields.flatMap(_.secondName),
       gender = user.privateFields.flatMap(_.gender),
       dateOfBirth = user.dates.flatMap(_.birthDate.map(_.toLocalDate)),
       location = user.publicFields.flatMap(_.location),
       aboutMe = user.publicFields.flatMap(_.aboutMe),
       interests = user.publicFields.flatMap(_.interests)
      ),
      deliveryAddress = Address(
        addressLine1 = user.privateFields.flatMap(_.address1),
        addressLine2 = user.privateFields.flatMap(_.address2),
        addressLine3 = user.privateFields.flatMap(_.address3),
        addressLine4 = user.privateFields.flatMap(_.address4),
        country = user.privateFields.flatMap(_.country),
        postcode = user.privateFields.flatMap(_.postcode)
      ),
      billingAddress = Address(
        addressLine1 = user.privateFields.flatMap(_.billingAddress1),
        addressLine2 = user.privateFields.flatMap(_.billingAddress2),
        addressLine3 = user.privateFields.flatMap(_.billingAddress3),
        addressLine4 = user.privateFields.flatMap(_.billingAddress4),
        country = user.privateFields.flatMap(_.billingCountry),
        postcode = user.privateFields.flatMap(_.billingPostcode)
      ),
      lastActivityDate = user.dates.flatMap(_.lastActivityDate),
      lastActivityIp = user.privateFields.flatMap(_.lastActiveIpAddress),
      registrationDate = user.dates.flatMap(_.accountCreatedDate),
      registrationIp = user.privateFields.flatMap(_.registrationIp),
      registrationType = user.privateFields.flatMap(_.registrationType),
      status = UserStatus(
        userEmailValidated = user.statusFields.flatMap(_.userEmailValidated),
        hasRepermissioned = user.statusFields.flatMap(_.hasRepermissioned)
      ),
      consents = user.consents.map(c => Consent(c.actor, c.id, c.version, c.consented, c.timestamp, c.privacyPolicyVersion)),
      groups = user.userGroups.map(g => UserGroup(g.packageCode, g.path, g.joinedDate)),
      socialLinks = user.socialLinks.map(s => SocialLink(s.socialId, s.network))
    )
}

/**
  * Data view exposed to Identity Admin client
  */
case class GuardianUser(
  idapiUser: User,
  membershipDetails: Option[SalesforceSubscription] = None,
  subscriptionDetails: Option[SalesforceSubscription] = None,
  exactTargetSubscriber: Option[ExactTargetSubscriber] = None,
  hasCommented: Boolean = false,
  deleted: Boolean = false,
  orphan: Boolean = false,
  contributions: List[Contribution] = Nil)

object GuardianUser {
  implicit val format = Json.format[GuardianUser]
}

case class GNMMadgexUser(id: String, madgexUser: MadgexUser)

case class MadgexUser(
  primaryEmailAddress: String,
  firstName: Option[String] = None,
  secondName: Option[String] = None,
  receive3rdPartyMarketing: Boolean = false,
  receiveGnmMarketing: Boolean = false)

