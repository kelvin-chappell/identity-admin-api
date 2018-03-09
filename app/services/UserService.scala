package services

import javax.inject.{Inject, Singleton}

import actors.EventPublishingActor.{DisplayNameChanged, EmailValidationChanged}
import actors.EventPublishingActorProvider
import ai.x.diff._
import ai.x.diff.conversions._
import com.gu.identity.util.Logging
import configuration.Config.PublishEvents.eventsEnabled
import models.{client, _}
import models.client._
import models.database.mongo._
import models.database.postgres.{PostgresDeletedUserRepository, PostgresReservedUsernameRepository, PostgresUserRepository}
import org.joda.time.DateTime
import uk.gov.hmrc.emailaddress.EmailAddress
import util.UserConverter._
import util.scientist.{Defaults, Experiment, ExperimentSettings}

import scala.concurrent.{ExecutionContext, Future}
import scalaz.std.scalaFuture._
import scalaz.{-\/, EitherT, \/-}

@Singleton class UserService @Inject()(
    usersReadRepository: UsersReadRepository,
    usersWriteRepository: UsersWriteRepository,
    identityApiClient: IdentityApiClient,
    eventPublishingActorProvider: EventPublishingActorProvider,
    salesforceService: SalesforceService,
    salesforceIntegration: SalesforceIntegration,
    madgexService: MadgexService,
    exactTargetService: ExactTargetService,
    discussionService: DiscussionService,
    postgresDeletedUserRepository: PostgresDeletedUserRepository,
    postgresReservedUsernameRepository: PostgresReservedUsernameRepository,
    postgresUsersReadRepository: PostgresUserRepository)
    (implicit ec: ExecutionContext) extends Logging {

  implicit val dateTimeDiffShow: DiffShow[DateTime] = new DiffShow[DateTime] {
    def show ( d: DateTime ) = "DateTime(" + d.toString + ")"
    def diff( l: DateTime, r: DateTime ) =
      if ( l isEqual r ) Identical( l ) else Different( l, r )
  }

  private implicit val futureMonad =
    cats.instances.future.catsStdInstancesForFuture(ec)

  def update(existingUser: User, userUpdateRequest: UserUpdateRequest): ApiResponse[User] = {
    val emailValid = isEmailValid(existingUser, userUpdateRequest)
    val usernameValid = isUsernameValid(existingUser, userUpdateRequest)

    (emailValid, usernameValid) match {
      case (true, true) =>
        val userEmailChanged = !existingUser.email.equalsIgnoreCase(userUpdateRequest.email)
        val userEmailValidated = if(userEmailChanged) Some(false) else existingUser.status.userEmailValidated
        val userEmailValidatedChanged = isEmailValidationChanged(userEmailValidated, existingUser.status.userEmailValidated)
        val usernameChanged = isUsernameChanged(userUpdateRequest.username, existingUser.username)
        val displayNameChanged = isDisplayNameChanged(userUpdateRequest.displayName, existingUser.displayName)
        val mongoWrite = usersWriteRepository.update(existingUser, userUpdateRequest)
        Experiment.async("UpdateUser", mongoWrite, postgresUsersReadRepository.update(existingUser, userUpdateRequest)).run
        EitherT(mongoWrite).map { result =>
          triggerEvents(
            userId = existingUser.id,
            usernameChanged = usernameChanged,
            displayNameChanged = displayNameChanged,
            emailValidatedChanged = userEmailValidatedChanged
          )

          if(userEmailChanged) {
            identityApiClient.sendEmailValidation(existingUser.id)
            exactTargetService.updateEmailAddress(existingUser.email, userUpdateRequest.email)
          }

          if (userEmailChanged && eventsEnabled) {
            salesforceIntegration.enqueueUserUpdate(existingUser.id, userUpdateRequest.email)
          }

          if (isJobsUser(existingUser) && isJobsUserChanged(existingUser, userUpdateRequest)) {
            madgexService.update(client.GNMMadgexUser(existingUser.id, userUpdateRequest))
          }

          result
        }.run

      case (false, true) => Future.successful(-\/(ApiError("Email is invalid")))
      case (true, false) => Future.successful(-\/(ApiError("Username is invalid")))
      case _ => Future.successful(-\/(ApiError("Email and username are invalid")))
    }
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
   def combineSearchResults(activeUsers: SearchResponse, deletedUsers: SearchResponse) = {
      val combinedTotal = activeUsers.total + deletedUsers.total
      val combinedResults = activeUsers.results ++ deletedUsers.results
      activeUsers.copy(total = combinedTotal, results = combinedResults)
    }

    // execute all these in parallel
    val usersByMemNumF = EitherT(searchIdentityByMembership(query))
    val orphansF = EitherT(searchOrphan(query))
    val usersBySubIdF = EitherT(searchIdentityBySubscriptionId(query))
    val mongoSearchResult = usersReadRepository.search(query, limit, offset)
    Experiment.async("SearchUser", mongoSearchResult, postgresUsersReadRepository.search(query, limit, offset)).run
    val activeUsersF = EitherT(mongoSearchResult)
    val deletedUsersF = EitherT(postgresDeletedUserRepository.search(query))

    (for {
      usersByMemNum <- usersByMemNumF
      orphans <- orphansF
      usersBySubId <- usersBySubIdF
      activeUsers <- activeUsersF
      deletedUsers <- deletedUsersF
    } yield {
      val idUsers = combineSearchResults(activeUsers, deletedUsers)

      if (idUsers.results.nonEmpty)
        idUsers
      else if (usersBySubId.results.nonEmpty)
        usersBySubId
      else if (orphans.results.nonEmpty)
        orphans
      else
        usersByMemNum
    }).run
  }

  def unreserveEmail(id: String) = postgresDeletedUserRepository.remove(id)

  def searchOrphan(email: String): ApiResponse[SearchResponse] = {
    def isEmail(query: String) = query.matches("""^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r.toString())

    val orphanSearchResponse = SearchResponse.create(1, 0, List(Orphan(email = email)))
    val emptySearchResponse = SearchResponse.create(0, 0, Nil)

    if (isEmail(email)) {
      val subOrphanOptF = EitherT(salesforceService.getSubscriptionByEmail(email))
      val newsOrphanOptF = EitherT(exactTargetService.newslettersSubscriptionByEmail(email))
      val contributionsListF = EitherT(exactTargetService.contributionsByEmail(email))

      (for {
        subOrphanOpt <- subOrphanOptF
        newsOrphanOpt <- newsOrphanOptF
        contributionsList <- contributionsListF
      } yield {
        if (subOrphanOpt.isDefined || newsOrphanOpt.isDefined || contributionsList.nonEmpty)
          orphanSearchResponse
        else
          emptySearchResponse
      }).run
    } else Future.successful(\/-(emptySearchResponse))
  }

  private def salesforceSubscriptionToIdentityUser(sfSub: SalesforceSubscription) =
    SearchResponse.create(1, 0, List(IdentityUser(sfSub.email, sfSub.identityId)))

  def searchIdentityByMembership(membershipNumber: String): ApiResponse[SearchResponse] = {
    def couldBeMembershipNumber(query: String) = query forall Character.isDigit

    if (couldBeMembershipNumber(membershipNumber)) {
      EitherT(salesforceService.getMembershipByMembershipNumber(membershipNumber)).map(memOpt =>
        memOpt.fold
          (SearchResponse.create(0, 0, Nil))
          (mem => salesforceSubscriptionToIdentityUser(mem))
      ).run
    } else Future.successful(\/-(SearchResponse.create(0, 0, Nil)))
  }

  def searchIdentityBySubscriptionId(subscriberId: String): ApiResponse[SearchResponse] = {
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
    val mongoResult = usersReadRepository.find(id)
    Experiment.async("FindUser", mongoResult, postgresUsersReadRepository.findById(id)).run
    val activeUserOptF = EitherT(mongoResult)

    (for {
      activeUserOpt <- activeUserOptF
      deletedUserOpt <- deletedUserOptF
    } yield {
      if (activeUserOpt.isDefined)
        activeUserOpt.map(idapiUser => GuardianUser(idapiUser = idapiUser))
      else
        deletedUserToActiveUser(deletedUserOpt)
    }).run
  }

  def delete(user: GuardianUser): ApiResponse[ReservedUsernameList] = {
    val reserveUsernameResult = user.idapiUser.username.fold(postgresReservedUsernameRepository.loadReservedUsernames)(postgresReservedUsernameRepository.addReservedUsername)
    val mongoResult = usersWriteRepository.delete(user.idapiUser)
    Experiment.async("DeleteUser", mongoResult, postgresUsersReadRepository.delete(user.idapiUser).map(_.map(_ => {}))).run
    (for {
      _ <- EitherT(mongoResult)
      reservedUsernameList <- EitherT(reserveUsernameResult)
    } yield reservedUsernameList).run
  }

  def validateEmail(user: User, emailValidated: Boolean = true): ApiResponse[Unit] = {
    val mongoResult = usersWriteRepository.updateEmailValidationStatus(user, emailValidated)
    Experiment.async("ValidateEmail", mongoResult.map(_ => 1),
      postgresUsersReadRepository.updateEmailValidationStatus(user, emailValidated)).run
    EitherT(mongoResult).map { _ =>
      triggerEvents(userId = user.id, usernameChanged = false, displayNameChanged = false, emailValidatedChanged = true)
    }.run
  }

  def sendEmailValidation(user: User): ApiResponse[Unit] =
    (for {
      _ <- EitherT(validateEmail(user, emailValidated = false))
      _ <- EitherT(identityApiClient.sendEmailValidation(user.id))
    } yield()).run

  def unsubscribeFromMarketingEmails(email: String): ApiResponse[User] = {
    val mongoResult = usersWriteRepository.unsubscribeFromMarketingEmails(email)
    Experiment.async("UnsubscribeFromMarketing", mongoResult, postgresUsersReadRepository.unsubscribeFromMarketingEmails(email)).run
    mongoResult
  }

  def enrichUserWithProducts(user: GuardianUser): ApiResponse[GuardianUser]  = {
    val subscriptionF = EitherT(salesforceService.getSubscriptionByIdentityId(user.idapiUser.id))
    val membershipF = EitherT(salesforceService.getMembershipByIdentityId(user.idapiUser.id))
    val hasCommentedF = EitherT(discussionService.hasCommented(user.idapiUser.id))
    val exactTargetSubF = EitherT(exactTargetService.subscriberByIdentityId(user.idapiUser.id))
    val contributionsF = EitherT(exactTargetService.contributionsByIdentityId(user.idapiUser.id))

    (for {
      subscription <- subscriptionF
      membership <- membershipF
      hasCommented <- hasCommentedF
      exactTargetSub <- exactTargetSubF
      contributions <- contributionsF
    } yield {
      user.copy(
        subscriptionDetails = subscription,
        membershipDetails = membership,
        hasCommented = hasCommented,
        exactTargetSubscriber = exactTargetSub,
        contributions = contributions)
    }).run
  }
}
