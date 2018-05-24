package com.gu.identity.logging

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Logger, LoggerContext}
import ch.qos.logback.core.joran.spi.JoranException
import ch.qos.logback.core.util.StatusPrinter
import com.amazonaws.regions.Regions
import com.gu.logback.appender.kinesis.KinesisAppender
import com.typesafe.scalalogging.LazyLogging
import net.logstash.logback.layout.LogstashLayout
import org.slf4j.{LoggerFactory, Logger => SLFLogger}

import scala.language.postfixOps
import scala.util.control.NonFatal

case class KinesisAppenderConfig(
  stream: String,
  profile: String,
  region: Regions = AWS.region,
  bufferSize: Int = 1000
)

object LogStash extends AwsInstanceTags with LazyLogging {

  private def getFacts(profile: String): Map[String, String] = try {
    val facts: Map[String, String] = {
      logger.info("Loading facts from AWS instance tags")
      Seq("App", "Stack", "Stage").flatMap(tag => readTag(tag, profile).map(tag.toLowerCase ->)).toMap
    }
    logger.info(s"Using facts: $facts")
    facts
  } catch {
    case NonFatal(e) =>
      logger.error("Failed to get facts", e)
      Map.empty
  }
  // assume SLF4J is bound to logback in the current environment
  lazy val context: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  def makeCustomFields(customFields: Map[String,String]): String = {
    "{" + (for((k, v) <- customFields) yield s""""$k":"$v"""").mkString(",") + "}"
  }

  private def makeLayout(customFields: String) = {
    val l = new LogstashLayout()
    l.setCustomFields(customFields)
    l
  }

  private def makeKinesisAppender(layout: LogstashLayout, context: LoggerContext, appenderConfig: KinesisAppenderConfig) = {
    val a = new KinesisAppender[ILoggingEvent]()
    a.setStreamName(appenderConfig.stream)
    a.setRegion(appenderConfig.region.getName)
    a.setCredentialsProvider(AWS.credentials(appenderConfig.profile))
    a.setBufferSize(appenderConfig.bufferSize)

    a.setContext(context)
    a.setLayout(layout)

    layout.start()
    a.start()
    a
  }

  def init(config: KinesisAppenderConfig, force: Boolean = false) {
    val facts = getFacts(config.profile)
    if (facts.isEmpty && !force) {
      logger.warn("Not constructing a Kinesis Appender as there are no facts for indexing")
      println("Not constructing a Kinesis Appender as there are no facts for indexing")
    } else {
      try {
        val layout = makeLayout(makeCustomFields(facts))
        val appender = makeKinesisAppender(layout, context, config)
        val rootLogger = LoggerFactory.getLogger(SLFLogger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
        rootLogger.addAppender(appender)
        logger.info("Kinesis logger added")
      } catch {
        case _: JoranException => // ignore, errors will be printed below
        case NonFatal(e) =>
          logger.error("Error whilst initialising LogStash", e)
      }
      StatusPrinter.printInCaseOfErrorsOrWarnings(context)
    }
  }
}
