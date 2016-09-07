import _root_.sbtrelease.ReleasePlugin.autoImport.ReleaseStep
import _root_.sbtrelease.ReleasePlugin.autoImport._
import _root_.sbtrelease.ReleaseStateTransformations._
import de.heikoseeberger.sbtheader.HeaderPattern

scalaVersion in ThisBuild := "2.11.7"

val cogcompNLPVersion = "3.0.64"
val cogcompPipelineVersion = "0.1.25"
val ccgGroupId = "edu.illinois.cs.cogcomp"
val headerMsg =  """/** This software is released under the University of Illinois/Research and Academic Use License. See
                        |  * the LICENSE file in the root folder for details. Copyright (c) 2016
                        |  *
                        |  * Developed by: The Cognitive Computations Group, University of Illinois at Urbana-Champaign
                        |  * http://cogcomp.cs.illinois.edu/
                        |  */
                        |""".stripMargin


lazy val saulUser = System.getenv("SAUL_USER")
lazy val user = if(saulUser == null) System.getProperty("user.name") else saulUser
lazy val keyFile = new java.io.File(Path.userHome.absolutePath + "/.ssh/id_rsa")

lazy val docSettings = Seq(
  autoAPIMappings := true,
  apiURL := Some(url("http://cogcomp.cs.illinois.edu/software/doc/saul/")),
  scalacOptions in Test ++= Seq("-Yrangepos")
)

lazy val releaseSetting = Seq(
  releaseIgnoreUntrackedFiles := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    setReleaseVersion,
    commitReleaseVersion,                   // performs the initial git checks
    //tagRelease,
    publishArtifacts,                       // checks whether `publishTo` is properly set up
    setNextVersion,
    commitNextVersion//,
    //pushChanges                             // checks that an upstream branch is properly configured
  )
)

lazy val commonSettings = Seq(
  organization := ccgGroupId,
  name := "saul-project",
  version := "0.5",
  resolvers ++= Seq(
    Resolver.mavenLocal,
    "CogcompSoftware" at "http://cogcomp.cs.illinois.edu/m2repo/"
  ),
  javaOptions ++= List("-Xmx11g"),
  libraryDependencies ++= Seq(
    ccgGroupId % "LBJava" % "1.2.20" withSources,
    ccgGroupId % "illinois-core-utilities" % cogcompNLPVersion withSources,
    "com.gurobi" % "gurobi" % "6.0",
    "org.apache.commons" % "commons-math3" % "3.0",
    "org.scalatest" % "scalatest_2.11" % "2.2.4",
    "ch.qos.logback" % "logback-classic" % "1.1.7"
  ),
  fork := true,
  publishTo := Some(
    Resolver.ssh(
      "CogcompSoftwareRepo", "bilbo.cs.illinois.edu",
      "/mounts/bilbo/disks/0/www/cogcomp/html/m2repo/") as (user, keyFile)
  ),
  isSnapshot := true,
  headers := Map(
    "scala" -> (HeaderPattern.cStyleBlockComment, headerMsg),
    "java" -> (HeaderPattern.cStyleBlockComment, headerMsg)
  )
)

lazy val root = (project in file(".")).
  aggregate(saulCore, saulExamples).
  enablePlugins(AutomateHeaderPlugin).
  settings(releaseSetting: _*)

lazy val saulCore = (project in file("saul-core")).
  settings(commonSettings: _*).
  settings(docSettings: _*).
  settings(
    name := "saul",
    libraryDependencies ++= Seq(
      "com.typesafe.play" % "play_2.11" % "2.4.3"
    )
  ).enablePlugins(AutomateHeaderPlugin)

lazy val saulExamples = (project in file("saul-examples")).
  settings(commonSettings: _*).
  settings(
    name := "saul-examples",
    libraryDependencies ++= Seq(
      ccgGroupId % "illinois-nlp-pipeline" % cogcompPipelineVersion withSources,
      ccgGroupId % "illinois-curator" % cogcompNLPVersion,
      ccgGroupId % "illinois-edison" % cogcompNLPVersion,
      ccgGroupId % "illinois-corpusreaders" % cogcompNLPVersion,
      ccgGroupId % "saul-pos-tagger-models" % "1.3",
      ccgGroupId % "saul-er-models" % "1.3",
      ccgGroupId % "saul-srl-models" % "1.1"
    )
  ).dependsOn(saulCore)
  .aggregate(saulCore)
  .enablePlugins(AutomateHeaderPlugin)

lazy val saulWebapp = (project in file("saul-webapp")).
  enablePlugins(PlayScala).
  settings(commonSettings: _*).
  settings(
    name := "saul-webapp",
    libraryDependencies ++= Seq(
      "org.webjars" %% "webjars-play" % "2.4.0-1",
      "org.webjars" % "bootstrap" % "3.3.6",
      "org.webjars.bower" % "tether-shepherd" % "1.1.3",
      "org.webjars" % "ace" % "1.2.2",
      "org.webjars" % "sigma.js" % "1.0.3",
      "org.webjars" % "d3js" % "3.5.16",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      jdbc,
      cache,
      ws,
      specs2 % Test
    ),
    resolvers ++= Seq("scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"),
    routesGenerator := InjectedRoutesGenerator
  ).dependsOn(saulExamples).aggregate(saulExamples)
  .enablePlugins(AutomateHeaderPlugin)
