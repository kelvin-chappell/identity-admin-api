lazy val root = (project in file(".")).enablePlugins(PlayScala, BuildInfoPlugin, RiffRaffArtifact, UniversalPlugin)

name := "identity-admin-api"
organization := "com.gu"
version := "1.0-SNAPSHOT"
scalaVersion := "2.12.6"

sources in (Compile,doc) := Seq.empty
publishArtifact in (Compile, packageDoc) := false
parallelExecution in Global := false
updateOptions := updateOptions.value.withCachedResolution(true)
javaOptions in Test += "-Dconfig.resource=TEST.conf"

// *****************************
// BuildInfo
// *****************************
buildInfoKeys := Seq[BuildInfoKey](
  name,
  BuildInfoKey.constant("buildNumber", Option(System.getenv("BUILD_NUMBER")) getOrElse "DEV"),
  BuildInfoKey.constant("buildTime", System.currentTimeMillis),
  BuildInfoKey.constant("gitCommitId", Option(System.getenv("BUILD_VCS_NUMBER")) getOrElse(try {
    "git rev-parse HEAD".!!.trim
  } catch {
    case e: Exception => "unknown"
  }))
)
buildInfoOptions += BuildInfoOption.ToMap
buildInfoPackage := "app"

// *****************************
// Dependencies
// *****************************
resolvers ++= Seq(
  "Guardian Github Releases" at "https://guardian.github.io/maven/repo-releases",
  "Guardian Github Snapshots" at "http://guardian.github.com/maven/repo-snapshots",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  Resolver.typesafeRepo("releases"),
  Resolver.bintrayRepo("dwhjames", "maven"),
  Resolver.bintrayRepo("guardian", "platforms"),
  Resolver.bintrayRepo("hmrc", "releases"),
  Resolver.sonatypeRepo("releases"))

libraryDependencies ++= Dependencies.value

// *****************************
// Deployment package
// *****************************
packageName in Universal := name.value
mappings in Universal ++= (baseDirectory.value / "deploy" * "*" get) map (x => x -> ("deploy/" + x.getName))
riffRaffPackageType := (packageBin in Universal).value
riffRaffUploadArtifactBucket := Option("riffraff-artifact")
riffRaffUploadManifestBucket := Option("riffraff-builds")
riffRaffManifestProjectName := s"identity:${name.value}"

// *****************************
// Command aliases
// *****************************
addCommandAlias("devrun", "run -Dconfig.resource=DEV.conf 9500")

