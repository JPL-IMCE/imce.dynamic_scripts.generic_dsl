
sbtPlugin := false

name := "imce.dynamic_scripts.generic_dsl"

description := "A textual DSL for specifying browser, diagram & contextualized actions in a modeling tool."

moduleName := name.value

organization := "gov.nasa.jpl.imce"

homepage := Some(url(s"https://github.com/JPL-IMCE/${moduleName.value}"))

organizationName := "JPL-IMCE"

organizationHomepage := Some(url(s"https://github.com/JPL-IMCE"))

git.remoteRepo := s"git@github.com:JPL-IMCE/${moduleName.value}"

startYear := Some(2015)

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.com/JPL-IMCE/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))

