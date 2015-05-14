import sbt._
import sbt.RichURI.fromURI
import Keys._
import com.typesafe.sbt.SbtGit._
import com.banno.license.Plugin.LicenseKeys._
import java.io.File
import java.net.URI
import java.util.Locale
import scala.util.matching.Regex
import net.virtualvoid.sbt.graph.Plugin.graphSettings

/**
 * sbt -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)> extractArchives
 */
object JPLDynamicScriptsGenericDSL extends Build {
  
  object Versions {
    val scala = "2.11.6"
    val jpl_mbee_core = "1800.02-d11058e8e9188f79996054418a160cb9490707f2"
    val jpl_mbee_other = "1800.02-d11058e8e9188f79996054418a160cb9490707f2"
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

  lazy val root = Project("jpl-dynamic-scripts-generic-dsl", file( "." )).
    settings(versionWithGit: _*).
    settings(showCurrentGitBranch: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(aggregateDependenciesPublishSettings: _*).
    settings(com.banno.license.Plugin.licenseSettings: _*).
    settings(
          scalaVersion := Versions.scala,
          removeExistingHeaderBlock := true,
          scalaSource in Compile := baseDirectory.value / "src",
          scalaSource in Test := baseDirectory.value / "tests",
          classDirectory in Compile := baseDirectory.value / "bin",
          classDirectory in Test := baseDirectory.value / "bin.tests",

      resolvers += {
        Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
          case Some(dir) => Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns)
          case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
        }
      },

      libraryDependencies ++= Seq(
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_core" % Versions.jpl_mbee_core,
        "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_other" % Versions.jpl_mbee_other
      )
    )   
}
