package configuration

import com.gu.identity.logging.KinesisAppenderConfig
import play.api.{Configuration, Environment, Logger}
import play.api.inject.{Binding, Module}

class LoggingModule extends Module {

  private val logger = Logger(this.getClass)

  logger.info("Adding Kinesis logger")
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    try {
      com.gu.identity.logging.LogStash.init(KinesisAppenderConfig(configuration.get[String]("logging.kinesisStream"), "identity"))
    } catch {
      case e: com.typesafe.config.ConfigException =>
        logger.error("Kinesis logging stream name not present in configuration", e)
      case e: Exception =>
        logger.error("Kinesis logging stream not correctly configured", e)
    }
    Nil
  }
}
