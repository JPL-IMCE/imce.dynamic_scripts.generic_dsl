import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._

useGpg := true

updateOptions := updateOptions.value.withCachedResolution(true)

developers := List(
  Developer(
    id="rouquett",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://gateway.jpl.nasa.gov/personal/rouquett/default.aspx")))

lazy val core = Project("imce-dynamic_scripts-generic_dsl", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2014-2016",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.cae,
    organization := "gov.nasa.jpl.imce.dynamic_scripts",

    buildInfoPackage := "gov.nasa.jpl.imce.dynamic_scripts.generic_dsl",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "tests",

    classDirectory in Compile := baseDirectory.value / "bin",
    cleanFiles += (classDirectory in Compile).value,

    classDirectory in Test := baseDirectory.value / "bin.tests",
    cleanFiles += (classDirectory in Test).value,

    extractArchives := {},

    libraryDependencies ++= Seq (
      "gov.nasa.jpl.imce.thirdParty" %% "other-scala-libraries" % Versions_other_scala_libraries.version %
        "compile" artifacts
        Artifact("other-scala-libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
