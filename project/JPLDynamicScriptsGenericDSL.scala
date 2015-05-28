import java.io.File

import com.banno.license.Plugin.LicenseKeys._
import com.typesafe.sbt.SbtGit._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbt.Keys._
import sbt._
import xerial.sbt.Pack._

/**
 * sbt -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)> publish
 */
object JPLDynamicScriptsGenericDSL extends Build {

  object Versions {
    val scala = "2.11.6"

    // JPL MBEE release
    val jpl_mbee_release_prefix="1800.02-"

    // JPL MBEE Common Scala Libraries
    val jpl_mbee_common_scala_libraries_revision="60d5d55571aeff7e4fb884ecd8d4e02b0da27ce6"

    val jpl_mbee_core = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision
    val jpl_mbee_other = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision
  }

  lazy val jplSettings = Seq(
    scalaVersion := Versions.scala,
    organization := "gov.nasa.jpl.mbee",
    organizationName := "JPL, Caltech",
    organizationHomepage := Some(url("https://mbse.jpl.nasa.gov")),
    publishMavenStyle := false,
    publishTo := {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Some(Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns))
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    },
    resolvers += {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns)
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    }
  )

  lazy val commonSettings =
    Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ graphSettings

  lazy val aggregateDependenciesPublishSettings = Seq(
    // disable publishing the main jar produced by `package`
    publishArtifact in(Compile, packageBin) := true,

    // disable publishing the main API jar
    publishArtifact in(Compile, packageDoc) := true,

    // disable publishing the main sources jar
    publishArtifact in(Compile, packageSrc) := true
  )

  def mappingFromProject(mappings: ((Seq[TaskKey[File]], Seq[Configuration]), String)*)(currentProject: ProjectRef, structure: BuildStructure): Task[Seq[(File, String)]] = {
    (mappings flatMap { case ((targetTasks: Seq[TaskKey[File]], configs: Seq[Configuration]), where: String) =>
      targetTasks flatMap { t: TaskKey[File] =>
        configs map { c =>
          Def.task {
            val file = ((t in c) in currentProject).value
            (file, where + "/" + file.getName)
          } evaluate structure.data
        }
      }
    }).join
  }

  lazy val dependenciesPackSettings = packSettings ++ Seq(
    packExpandedClasspath := false,
    packLibJars := Seq.empty,
    packUpdateReports := Seq.empty,
    mappings in pack <<= (thisProjectRef, buildStructure) flatMap mappingFromProject(
      (Seq(packageBin), Seq(Compile, Test)) -> "lib",
      (Seq(packageSrc), Seq(Compile, Test)) -> "lib.srcs",
      (Seq(packageDoc), Seq(Compile, Test)) -> "lib.javadoc"
    )
  ) ++ publishPackZipArchive

  lazy val jpl_dynamicScripts_generic_dsl = Project("jpl-dynamic-scripts-generic-dsl", file(".")).
    settings(versionWithGit: _*).
    settings(showCurrentGitBranch: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(aggregateDependenciesPublishSettings: _*).
    settings(com.banno.license.Plugin.licenseSettings: _*).
    settings(dependenciesPackSettings: _*).
    settings(
      removeExistingHeaderBlock := true,
      scalaSource in Compile := baseDirectory.value / "src",
      scalaSource in Test := baseDirectory.value / "tests",
      classDirectory in Compile := baseDirectory.value / "bin",
      classDirectory in Test := baseDirectory.value / "bin.tests",


      libraryDependencies ++= Seq(
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_core" % Versions.jpl_mbee_core,
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_other" % Versions.jpl_mbee_other
      )
    )
}
