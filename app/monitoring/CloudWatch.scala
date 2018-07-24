package monitoring

import com.amazonaws.services.cloudwatch.AmazonCloudWatchAsyncClientBuilder
import com.amazonaws.services.cloudwatch.model.{Dimension, MetricDatum, PutMetricDataRequest, StatisticSet}
import com.google.inject.ImplementedBy
import com.gu.identity.util.Logging
import configuration.Config
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class SampleId(namespace: String, name: String)
case class Sample(id: SampleId, value: Double)

object IncrementCounter {
  def apply(namespace: String, name: String) = Sample(namespace, name, 1.0)
}

object Sample {
  def apply(namespace: String, name: String, value: Double): Sample = Sample(SampleId(namespace, name), value)
}

case class Samples(samples: Map[SampleId, List[Sample]] = Map()) {
  def +(sample: Sample): Samples = copy(
    samples = samples + (sample.id -> (sample :: samples.getOrElse(sample.id, Nil)))
  )

  private def groupByNamespace: Map[String, Samples] = {
    samples.groupBy(_._1.namespace).mapValues(Samples.apply)
  }

  private def toAmazonStats(dimensions: Dimension*): Seq[MetricDatum] = {
    samples.map { case (key, samps) =>
      val values = samps.map(_.value)

      val stats = new StatisticSet()
        .withMaximum(values.max)
        .withMinimum(values.min)
        .withSampleCount(values.size.toDouble)
        .withSum(values.sum)

      new MetricDatum()
        .withMetricName(key.name)
        .withDimensions(dimensions:_*)
        .withStatisticValues(stats)
    }.toSeq
  }

  def toAmazonRequests(dimensions: Dimension*): Seq[PutMetricDataRequest] = {
    for {
      (namepsace, namespaceSamples) <- groupByNamespace.toSeq
      amazonStats = namespaceSamples.toAmazonStats(dimensions: _*)
      groupedStats <- amazonStats.grouped(20)
    } yield {
      new PutMetricDataRequest()
        .withNamespace(namepsace)
        .withMetricData(groupedStats.asJava)
    }
  }
}

@ImplementedBy(classOf[CloudWatch])
trait Metrics {
  def publishCount(name : String, count: Double): Unit
}

class CloudWatch extends Metrics with Logging {

  private val application = Config.applicationName
  private val stageDimension: Dimension = new Dimension().withName("Stage").withValue(Config.stage)
  private val appDimension: Dimension = new Dimension().withName("ApiMode").withValue(Config.applicationName)
  private val mandatoryDimensions:Seq[Dimension] = Seq(stageDimension)

  private val cloudWatchClient =
    AmazonCloudWatchAsyncClientBuilder.standard()
      .withCredentials(Config.AWS.credentialsProvider)
      .withRegion(Config.AWS.region)
      .build()

  def publishCount(name : String, count: Double): Unit = {
    val metric = new MetricDatum()
      .withValue(count)
      .withMetricName(name)
      .withUnit("Count")
      .withDimensions(mandatoryDimensions: _*)

    val request = new PutMetricDataRequest().withNamespace(application).withMetricData(metric)

    Try(cloudWatchClient.putMetricDataAsync(request)) match {
      case Success(_) => logger.debug(s"Published metric to CloudWatch: name=$name value=$count")
      case Failure(e) => logger.error(s"Could not publish metric to Cloudwatch: name=$name value=$count error=${e.getMessage}}")
    }
  }

  def publishMetrics(samples: Samples): Unit = {
    samples
      .toAmazonRequests(appDimension, stageDimension)
      .foreach(cloudWatchClient.putMetricData)
  }

}



