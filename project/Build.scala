import sbt._
import Keys._
import play.Project._


object ApplicationBuild extends Build {

    val appName         = "thewallstreetnerdplay2"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.mongodb" % "mongo-java-driver" % "2.7.3",
      "org.scalaj" %% "scalaj-http" % "0.3.14",
      "commons-net" % "commons-net" % "3.2"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here
    )

}
