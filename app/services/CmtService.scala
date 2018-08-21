package services

import models.client.{ApiResponse, EmailSubscriptionStatus}

trait CmtService {
  def findUserSubscriptions(userId: String): ApiResponse[EmailSubscriptionStatus]
  def unsubscribeAll(userId: String, email: String): ApiResponse[Unit]
  def updateEmailAddress(identityId: String, oldEmail: String, newEmail: String): ApiResponse[Unit]
}
