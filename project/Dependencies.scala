import sbt._
import play.sbt.PlayImport

object Dependencies {
  val playWS =        PlayImport.ws
  val playCache =     PlayImport.cache
  val playFilters =   PlayImport.filters
  val guice =         PlayImport.guice
  val jodaForms =     PlayImport.jodaForms
  val specs2 =        PlayImport.specs2   % "test"

  val scalaUri =            "com.netaporter"                %% "scala-uri"                % "0.4.16"
  val identityCookie =      "com.gu.identity"               %% "identity-cookie"          % "3.121"
  val identityModel =       "com.gu.identity"               %% "identity-model"           % "3.121"
  val identityPlayAuth =    "com.gu.identity"               %% "identity-play-auth"       % "1.2"
  val awsWrap =             "com.github.dwhjames"           %% "aws-wrap"                 % "0.8.0"
  val aws =                 "com.amazonaws"                 %  "aws-java-sdk"             % "1.11.105"
  val scalaz =              "org.scalaz"                    %% "scalaz-core"              % "7.2.10"
  val reactiveMongo =       "org.reactivemongo"             %% "play2-reactivemongo"      % "0.12.6-play26"
  val emailValidation =     "uk.gov.hmrc"                   %% "emailaddress"             % "2.1.0"
  val exactTargetFuel =     "com.exacttarget"               %  "fuelsdk"                  % "1.1.0"
  val tip =                 "com.gu"                        %% "tip"                      % "0.3.2"
  val diff =                "ai.x"                          %% "diff"                     % "1.2.0"
  val playJson =            "com.typesafe.play"             %% "play-json"                % "2.6.3"
  val playJsonJoda =        "com.typesafe.play"             %% "play-json-joda"           % "2.6.3"
  val scalaTestPlus =       "org.scalatestplus.play"        %% "scalatestplus-play"       % "3.1.1"    % "test"
  val embeddedMongo =       "com.github.simplyscala"        %% "scalatest-embedmongo"     % "0.2.3"    % "test"
  val mockWs =              "de.leanovate.play-mockws"      %% "play-mockws"              % "2.6.0"    % "test"
  val scalaTest =           "org.scalatest"                 %% "scalatest"                % "3.0.1"    % "test"
  val akkaSlf4j =           "com.typesafe.akka"             %% "akka-slf4j"               % "2.5.4"    % "test"
  val akkaTestkit =         "com.typesafe.akka"             %% "akka-testkit"             % "2.5.4"    % "test"
  val scalikeJdbc =         "org.scalikejdbc"               %% "scalikejdbc"              % "3.1.0"
  val hikariCp =            "com.zaxxer"                    %  "HikariCP"                 % "2.7.2"
  val postgres = Seq(
    "org.postgresql" % "postgresql" % "42.1.4",
    "ru.yandex.qatools.embed" % "postgresql-embedded" % "2.4" % Test
  )
  val cats = "org.typelevel" %% "cats-core" % "0.9.0"

  val apiDependencies = Seq(scalaUri, identityCookie, identityModel, identityPlayAuth, emailValidation,
    playWS, playCache, playFilters, awsWrap, scalaz, reactiveMongo,
    specs2, scalaTest, embeddedMongo, mockWs, scalaTestPlus, akkaSlf4j, akkaTestkit,
    exactTargetFuel, tip, aws, guice, jodaForms, playJson, playJsonJoda, scalikeJdbc, hikariCp, diff, cats) ++ postgres

}
