package services

import javax.inject.{Inject, Singleton}
import actors.EventPublishingActor.{DisplayNameChanged, EmailValidationChanged}
import actors.EventPublishingActorProvider
import actors.metrics.{MetricsActorProvider, MetricsSupport}
import com.typesafe.scalalogging.LazyLogging
import configuration.Config.PublishEvents.eventsEnabled
import models.{client, _}
import models.client._
import models.database.mongo._
import models.database.postgres._
import uk.gov.hmrc.emailaddress.EmailAddress
import util.UserConverter._

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.scalaFuture._
import scalaz.{-\/, EitherT, \/, \/-}

@Singleton class UserService @Inject()(
    identityApiClient: IdentityApiClient,
    eventPublishingActorProvider: EventPublishingActorProvider,
    salesforceService: SalesforceService,
    salesforceIntegration: SalesforceIntegration,
    madgexService: MadgexService,
    discussionService: DiscussionService,
    postgresDeletedUserRepository: PostgresDeletedUserRepository,
    postgresReservedUsernameRepository: PostgresReservedUsernameRepository,
    postgresReservedEmailRepository: PostgresReservedEmailRepository,
    postgresUsersReadRepository: PostgresUserRepository,
    postgresSubjectAccessRequestRepository: PostgresSubjectAccessRequestRepository,
    val metricsActorProvider: MetricsActorProvider)
    (implicit ec: ExecutionContext) extends LazyLogging with MetricsSupport {

  private implicit val metricsNamespace = MetricsSupport.Namespace("identity-admin")

  def update(existingUser: User, userUpdateRequest: UserUpdateRequest): ApiResponse[User] = {
    val emailValid = isEmailValid(existingUser, userUpdateRequest)
    val usernameValid = isUsernameValid(existingUser, userUpdateRequest)

    postgresReservedEmailRepository.isReserved(userUpdateRequest.email).flatMap {
      case \/-(isReserved) => {
        if (emailValid && usernameValid && !isReserved) updateUser(existingUser, userUpdateRequest)
        else if (!emailValid && usernameValid) Future.successful(-\/(ApiError("Email is invalid")))
        else if (emailValid && !usernameValid) Future.successful(-\/(ApiError("Username is invalid")))
        else if (isReserved) Future.successful(-\/(ApiError("Email is reserved")))
        else Future.successful(-\/(ApiError("Email and username are invalid")))
      }
      case -\/(error) => Future.successful(-\/(ApiError(s"Cannot access reserved emails: $error")))
    }

  }

  private def updateUser(existingUser: User, userUpdateRequest: UserUpdateRequest): Future[ApiError \/ User] = {
    val userEmailChanged = !existingUser.email.equalsIgnoreCase(userUpdateRequest.email)
    val userEmailValidated = if (userEmailChanged) Some(false) else existingUser.status.userEmailValidated
    val userEmailValidatedChanged = isEmailValidationChanged(userEmailValidated, existingUser.status.userEmailValidated)
    val usernameChanged = isUsernameChanged(userUpdateRequest.username, existingUser.username)
    val displayNameChanged = isDisplayNameChanged(userUpdateRequest.displayName, existingUser.displayName)
    val updateRequestWithEmailValidation = userUpdateRequest.copy(userEmailValidated = userEmailValidated)

    val updatedUser = for {
      user <- EitherT(postgresUsersReadRepository.update(existingUser, updateRequestWithEmailValidation))
      _ = triggerEvents(existingUser.id, usernameChanged, displayNameChanged, userEmailValidatedChanged)
      _ <- if (userEmailChanged) updateEmailAddress(user.id, userUpdateRequest.email) else EitherT.right[Future, ApiError, Unit](Future.successful(()))
      _ = if (userEmailChanged && eventsEnabled) salesforceIntegration.enqueueUserUpdate(existingUser.id, userUpdateRequest.email)
      _ = if (isJobsUser(existingUser) && isJobsUserChanged(existingUser, userUpdateRequest)) madgexService.update(client.GNMMadgexUser(existingUser.id, userUpdateRequest))
    } yield user

    updatedUser.run
  }

  private def updateEmailAddress(userId: String, newEmail: String): EitherT[Future, ApiError, Unit] = {
    for {
      _ <- EitherT(identityApiClient.changeEmail(userId, newEmail))
      _ <- EitherT(identityApiClient.sendEmailValidation(userId))
    } yield ()
  }

  def isDisplayNameChanged(newDisplayName: Option[String], existingDisplayName: Option[String]): Boolean =
    newDisplayName != existingDisplayName

  def isUsernameChanged(newUsername: Option[String], existingUsername: Option[String]): Boolean =
    newUsername != existingUsername

  def isJobsUser(user: User) = isAMemberOfGroup("/sys/policies/guardian-jobs", user)

  def isAMemberOfGroup(groupPath: String, user: User): Boolean = user.groups.exists(_.path == groupPath)

  def isEmailValidationChanged(newEmailValidated: Option[Boolean], existingEmailValidated: Option[Boolean]): Boolean =
    newEmailValidated != existingEmailValidated

  def isJobsUserChanged(user: MadgexUser, userUpdateRequest: MadgexUser): Boolean = !user.equals(userUpdateRequest)

  private def triggerEvents(userId: String, usernameChanged: Boolean, displayNameChanged: Boolean, emailValidatedChanged: Boolean) = {
    if (eventsEnabled) {
      if (usernameChanged || displayNameChanged) {
        eventPublishingActorProvider.sendEvent(DisplayNameChanged(userId))
      }
      if (emailValidatedChanged) {
        eventPublishingActorProvider.sendEvent(EmailValidationChanged(userId))
      }
    }
  }

  private def isUsernameValid(user: User, userUpdateRequest: UserUpdateRequest): Boolean = {
    def validateUsername(username: Option[String]): Boolean =  username match {
      case Some(newUsername) => "[a-zA-Z0-9]{6,20}".r.pattern.matcher(newUsername).matches()
      case _ => true
    }

    user.username match {
      case None => validateUsername(userUpdateRequest.username)
      case Some(existing) => if(!existing.equalsIgnoreCase(userUpdateRequest.username.mkString)) validateUsername(userUpdateRequest.username) else true
    }
  }

  private def isEmailValid(user: User, userUpdateRequest: UserUpdateRequest): Boolean = {
    if (!user.email.equalsIgnoreCase(userUpdateRequest.email))
      EmailAddress.isValid(userUpdateRequest.email)
    else
      true
  }

  def search(query: String, limit: Option[Int] = None, offset: Option[Int] = None): ApiResponse[SearchResponse] = {
    def combineSearchResults(activeUsers: SearchResponse, deletedUsers: SearchResponse): SearchResponse = {
      val combinedTotal = activeUsers.total + deletedUsers.total
      val combinedResults = activeUsers.results ++ deletedUsers.results
      activeUsers.copy(total = combinedTotal, results = combinedResults)
    }

    // execute all these in parallel
    val usersByMemNumF = EitherT(searchIdentityByMembership(query))
    val orphansF = EitherT(searchOrphan(query))
    val usersBySubIdF = EitherT(searchIdentityBySubscriptionId(query))
    val activeUsersF = EitherT(postgresUsersReadRepository.search(query, limit, offset))
    val deletedUsersF = EitherT(postgresDeletedUserRepository.search(query))

    def searchThirdParties() = {
      for {
        usersByMemNum <- usersByMemNumF
        orphans <- orphansF
        usersBySubId <- usersBySubIdF
      } yield {
        if (usersBySubId.results.nonEmpty)
          usersBySubId
        else if (orphans.results.nonEmpty)
          orphans
        else
          usersByMemNum
      }
    }

    def searchThirdPartiesIfEmpty(idUsers: SearchResponse) = {
      if (idUsers.results.isEmpty)
        searchThirdParties()
      else
        EitherT.right[Future, ApiError, SearchResponse](Future.successful(idUsers))
    }

    val searchResult = for {
      activeUsers <- activeUsersF
      deletedUsers <- deletedUsersF
      idUsers = combineSearchResults(activeUsers, deletedUsers)
      foundUsers <- searchThirdPartiesIfEmpty(idUsers)
    } yield foundUsers

    searchResult.run
  }

  def unreserveEmail(id: String) = postgresDeletedUserRepository.remove(id)

  def blockEmail(id: String) = postgresDeletedUserRepository.setBlocklisted(id)

  def enrichUserWithBannedStatus(user: GuardianUser): client.ApiResponse[GuardianUser] =
    postgresDeletedUserRepository.isBlocklisted(user.idapiUser.id).map {
      case \/-(status) => \/-(user.copy(blocklisted = status))
      case -\/(_) => \/-(user)
    }

  def searchOrphan(email: String): ApiResponse[SearchResponse] = withMetricsFE("searchOrphan") {
    def isEmail(query: String) = query.matches("""^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r.toString())

    val orphanSearchResponse = SearchResponse.create(1, 0, List(Orphan(email = email)))
    val emptySearchResponse = SearchResponse.create(0, 0, Nil)

    if (isEmail(email)) {
      (for {
        subOrphanOpt <- EitherT(salesforceService.getSubscriptionByEmail(email))
      } yield {
        if (subOrphanOpt.isDefined)
          orphanSearchResponse
        else
          emptySearchResponse
      }).run
    } else Future.successful(\/-(emptySearchResponse))
  }

  private def salesforceSubscriptionToIdentityUser(sfSub: SalesforceSubscription) =
    SearchResponse.create(1, 0, List(IdentityUser(sfSub.email, sfSub.identityId)))

  def searchIdentityByMembership(membershipNumber: String): ApiResponse[SearchResponse] = withMetricsFE("searchIdentityByMembership") {
    def couldBeMembershipNumber(query: String) = query forall Character.isDigit

    if (couldBeMembershipNumber(membershipNumber)) {
      EitherT(salesforceService.getMembershipByMembershipNumber(membershipNumber)).map(memOpt =>
        memOpt.fold
          (SearchResponse.create(0, 0, Nil))
          (mem => salesforceSubscriptionToIdentityUser(mem))
      ).run
    } else Future.successful(\/-(SearchResponse.create(0, 0, Nil)))
  }

  def searchIdentityBySubscriptionId(subscriberId: String): ApiResponse[SearchResponse] = withMetricsFE("searchIdentityBySubscriptionId") {
    def isSubscriberId(query: String) = List("A-S", "GA0").contains(query.take(3))

    // execute these in parallel
    val memOptF = EitherT(salesforceService.getMembershipBySubscriptionId(subscriberId))
    val subOptF = EitherT(salesforceService.getSubscriptionBySubscriptionId(subscriberId))

    if (isSubscriberId(subscriberId)) {
      (for {
        memOpt <- memOptF
        subOpt <- subOptF
      } yield {
        if (memOpt.isDefined)
          salesforceSubscriptionToIdentityUser(memOpt.get)
        else if (subOpt.isDefined)
          salesforceSubscriptionToIdentityUser(subOpt.get)
        else
          SearchResponse.create(0, 0, Nil)
      }).run
    }
    else Future.successful(\/-(SearchResponse.create(0, 0, Nil)))
  }

  /* If it cannot find an active user, tries looking up a deleted one */
  def findById(id: String): ApiResponse[Option[GuardianUser]] = {
    def deletedUserToActiveUser(userOpt: Option[DeletedUser]): Option[GuardianUser] =
      userOpt.map(
        user =>
          GuardianUser(
            idapiUser = User(id = user.id, email = user.email, username = Some(user.username)),
            deleted = true)
      )

    val deletedUserOptF = EitherT(postgresDeletedUserRepository.findBy(id))
    val activeUserOptF = EitherT(postgresUsersReadRepository.findById(id))

    (for {
      activeUserOpt <- activeUserOptF
      deletedUserOpt <- deletedUserOptF.map(_.headOption)
    } yield {
      activeUserOpt.map(idapiUser => GuardianUser(idapiUser = idapiUser))
        .orElse(deletedUserToActiveUser(deletedUserOpt))
    }).run
  }

  def delete(user: GuardianUser): ApiResponse[ReservedUsernameList] = {
    val reserveUsernameResult = user.idapiUser.username.fold(postgresReservedUsernameRepository.loadReservedUsernames)(postgresReservedUsernameRepository.addReservedUsername)
    (for {
      _ <- EitherT(identityApiClient.deleteUserById(user.idapiUser.id))
      reservedUsernameList <- EitherT(reserveUsernameResult)
    } yield reservedUsernameList).run
  }

  def validateEmail(user: User, emailValidated: Boolean = true): ApiResponse[Unit] = {
    EitherT(postgresUsersReadRepository.updateEmailValidationStatus(user, emailValidated)).map { _ =>
      triggerEvents(userId = user.id, usernameChanged = false, displayNameChanged = false, emailValidatedChanged = true)
    }.run
  }

  def sendEmailValidation(user: User): ApiResponse[Unit] =
    (for {
      _ <- EitherT(validateEmail(user, emailValidated = false))
      _ <- EitherT(identityApiClient.sendEmailValidation(user.id))
    } yield()).run

  def enrichUserWithProducts(user: GuardianUser): ApiResponse[GuardianUser] = withMetricsFE("enrichUserWithProducts") {
    val subscriptionF = EitherT(salesforceService.getSubscriptionByIdentityId(user.idapiUser.id))
    val membershipF = EitherT(salesforceService.getMembershipByIdentityId(user.idapiUser.id))
    val hasCommentedF = EitherT(discussionService.hasCommented(user.idapiUser.id))
    val newslettersF = EitherT(identityApiClient.findNewsletterSubscriptions(user.idapiUser.id))

    (for {
      subscription <- subscriptionF
      membership <- membershipF
      hasCommented <- hasCommentedF
      newsletters <- newslettersF
    } yield {
      user.copy(
        subscriptionDetails = subscription,
        membershipDetails = membership,
        hasCommented = hasCommented,
        newsletters = newsletters)
    }).run
  }

  def getSubjectAccessRequest(id: String): ApiResponse[List[String]] = {
    postgresSubjectAccessRequestRepository.subjectAccessRequestById(id)
  }
}
