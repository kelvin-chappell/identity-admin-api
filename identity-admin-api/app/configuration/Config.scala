package configuration

import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.auth.{AWSCredentialsProviderChain, InstanceProfileCredentialsProvider}
import com.amazonaws.regions.Regions
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.filters.cors.CORSConfig

object Config {
  val config = ConfigFactory.load()

  val applicationName = "identity-admin-api"

  val stage = config.getString("stage")

  object AWS {
    val profile = config.getString("aws-profile")
    val credentialsProvider = new AWSCredentialsProviderChain(new ProfileCredentialsProvider(profile), new InstanceProfileCredentialsProvider())
    val region = Regions.EU_WEST_1
  }

  val corsConfig = CORSConfig.fromConfiguration(Configuration(config))
}