import sbt._
import play.sbt.PlayImport

object Dependencies {
  val value = Seq(
    PlayImport.guice,
    PlayImport.jodaForms,
    PlayImport.ws,
    PlayImport.cache,
    PlayImport.filters,

    "com.netaporter"              %%  "scala-uri"             % "0.4.16",
    "com.gu.identity"             %%  "identity-cookie"       % "3.121",
    "com.gu.identity"             %%  "identity-play-auth"    % "1.2",
    "com.gu"                      %%  "tip"                   % "0.3.2",
    "uk.gov.hmrc"                 %%  "emailaddress"          % "2.1.0",
    "com.github.dwhjames"         %%  "aws-wrap"              % "0.8.0",
    "org.scalaz"                  %%  "scalaz-core"           % "7.2.10",
    "org.reactivemongo"           %%  "play2-reactivemongo"   % "0.12.6-play26",
    "com.exacttarget"             %   "fuelsdk"               % "1.1.0",
    "com.amazonaws"               %   "aws-java-sdk"          % "1.11.105",
    "com.typesafe.play"           %%  "play-json"             % "2.6.3",
    "com.typesafe.play"           %%  "play-json-joda"        % "2.6.3",
    "org.scalikejdbc"             %%  "scalikejdbc"           % "3.1.0",
    "com.zaxxer"                  %   "HikariCP"              % "2.7.2",
    "ai.x"                        %%  "diff"                  % "1.2.0",
    "org.typelevel"               %%  "cats-core"             % "0.9.0",
    "org.postgresql"              %   "postgresql"            % "42.1.4",

    "ru.yandex.qatools.embed"     %   "postgresql-embedded"   % "2.4"             % "test",
    "org.scalatest"               %%  "scalatest"             % "3.0.5"           % "test",
    "com.github.simplyscala"      %%  "scalatest-embedmongo"  % "0.2.3"           % "test",
    "de.leanovate.play-mockws"    %%  "play-mockws"           % "2.6.0"           % "test",
    "org.scalatestplus.play"      %%  "scalatestplus-play"    % "3.1.1"           % "test",
    "com.typesafe.akka"           %%  "akka-slf4j"            % "2.5.4"           % "test",
    "com.typesafe.akka"           %%  "akka-testkit"          % "2.5.4"           % "test",
    PlayImport.specs2                                                             % "test"
  )
}
