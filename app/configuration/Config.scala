package configuration

import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.auth.{AWSCredentialsProviderChain, BasicAWSCredentials, InstanceProfileCredentialsProvider}
import com.amazonaws.regions.Regions
import com.typesafe.config.ConfigFactory

object Config {
  val config = ConfigFactory.load()

  val applicationName = "identity-admin-api"

  val stage = config.getString("stage")
  val stack = config.getString("stack")

  object SearchValidation {
    val minimumQueryLength = config.getInt("search-validation.minimumQueryLength")
    val maximumLimit = config.getInt("search-validation.maximumLimit")
  }

  object AWS {
    val profile = config.getString("aws-profile")
    val credentialsProvider = new AWSCredentialsProviderChain(new ProfileCredentialsProvider(profile), InstanceProfileCredentialsProvider.getInstance())
    val region = Regions.EU_WEST_1
  }

  val hmacSecret = config.getString("hmac.secret")

  object IdentityApi {
    val clientToken = config.getString("identity-api.client-token")
    val baseUrl = config.getString("identity-api.base-url")
  }

  object Monitoring {
    val mongoEnabled = config.getBoolean("monitoring.mongo.enabled")
  }

  object PublishEvents {
    val eventsEnabled = config.getBoolean("events.enabled")
    val emailValidationChangedEventSnsArn = config.getString("events.email-validation-changed-sns-topic-arn")
    val displayNameChangedEventSnsArn = config.getString("events.displayname-changed-sns-topic-arn")
  }

  object TouchpointSalesforce {
    val consumerKey = config.getString("touchpoint.salesforce.consumer.key")
    val consumerSecret = config.getString("touchpoint.salesforce.consumer.secret")
    val apiUrl = config.getString("touchpoint.salesforce.api.url")
    val apiUsername = config.getString("touchpoint.salesforce.api.username")
    val apiPassword = config.getString("touchpoint.salesforce.api.password")
    val apiToken = config.getString("touchpoint.salesforce.api.token")
  }

  object IdentitySalesforceQueue {
    private val awsAccessKey = config.getString("aws.queue.identity-salesforce.key")
    private val awsAccessSecret = config.getString("aws.queue.identity-salesforce.secret")
    val credentials = new BasicAWSCredentials(awsAccessKey, awsAccessSecret)
    val snsTopic = config.getString("aws.queue.identity-salesforce.sns.arn")
    val snsEndPoint = config.getString("aws.queue.identity-salesforce.sns.endpoint")
  }

  object ExactTarget {
    object Admin {
      val clientSecret = config.getString("exacttarget.admin.clientSecret")
      val clientId = config.getString("exacttarget.admin.clientId")
    }
    object Editorial {
      val clientSecret = config.getString("exacttarget.editorial.clientSecret")
      val clientId = config.getString("exacttarget.editorial.clientId")
    }
    object Consumer {
      val clientSecret = config.getString("exacttarget.consumer.clientSecret")
      val clientId = config.getString("exacttarget.consumer.clientId")
    }
  }

  object Discussion {
    val apiUrl = config.getString("discussion.api.url")
  }

  object Madgex {
    val apiUrl = config.getString("madgex.host")
  }

  object Postgres {
    val jdbcUrl = config.getString("postgres.jdbcUrl")
    val username = config.getString("postgres.username")
    val password = config.getString("postgres.password")
    val poolSize = config.getInt("postgres.maxPoolSize")
  }
}
