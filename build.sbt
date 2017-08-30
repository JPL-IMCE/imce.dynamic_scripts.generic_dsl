import sbt.Keys._
import sbt._

import gov.nasa.jpl.imce.sbt._

updateOptions := updateOptions.value.withCachedResolution(true)

lazy val core = Project("imce-dynamic_scripts-generic_dsl", file("."))
  .enablePlugins(IMCEGitPlugin)
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
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

    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    scalacOptions in (Compile, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Test, compile) += s"-P:artima-supersafe:config-file:${baseDirectory.value}/project/supersafe.cfg",
    scalacOptions in (Compile, doc) += "-Xplugin-disable:artima-supersafe",
    scalacOptions in (Test, doc) += "-Xplugin-disable:artima-supersafe",

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,
    // include all test artifacts
    publishArtifact in Test := true,
    scalaSource in Test := baseDirectory.value / "tests",

    classDirectory in Compile := baseDirectory.value / "bin",
    cleanFiles += (classDirectory in Compile).value,

    classDirectory in Test := baseDirectory.value / "bin.tests",
    cleanFiles += (classDirectory in Test).value,

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    libraryDependencies +=
      "gov.nasa.jpl.imce" %% "imce.third_party.other_scala_libraries" % Versions_other_scala_libraries.version
        % "provided"
        artifacts
        Artifact("imce.third_party.other_scala_libraries", "zip", "zip", Some("resource"), Seq(), None, Map())
  )
