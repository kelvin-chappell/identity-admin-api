package com.gu.identity.logging

import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.auth.{AWSCredentialsProviderChain, InstanceProfileCredentialsProvider}
import com.amazonaws.regions.Regions
import com.amazonaws.services.ec2.{AmazonEC2, AmazonEC2ClientBuilder}
import com.amazonaws.services.ec2.model.{DescribeTagsRequest, Filter}
import com.amazonaws.util.EC2MetadataUtils

import scala.collection.JavaConverters._

object AWS extends AwsInstanceTags {

  lazy val region: Regions = Regions.EU_WEST_1

  def credentials(profile: String): AWSCredentialsProviderChain = {
    new AWSCredentialsProviderChain(new ProfileCredentialsProvider(profile), InstanceProfileCredentialsProvider.getInstance())
  }

  def EC2Client(profile: String): AmazonEC2 = AmazonEC2ClientBuilder
    .standard()
    .withCredentials(credentials(profile))
    .withRegion(region)
    .build()
}

trait AwsInstanceTags {
  lazy val instanceId = Option(EC2MetadataUtils.getInstanceId)

  def readTag(tagName: String, profile: String): Option[String] = {
    instanceId.flatMap { id =>
      val tagsResult = AWS.EC2Client(profile).describeTags(
        new DescribeTagsRequest().withFilters(
          new Filter("resource-type").withValues("instance"),
          new Filter("resource-id").withValues(id),
          new Filter("key").withValues(tagName)
        )
      )
      tagsResult.getTags.asScala.find(_.getKey == tagName).map(_.getValue)
    }
  }
}
