import sbt._
import Keys._
import PlayProject._


object ApplicationBuild extends Build {

    val appName         = "thewallstreetnerdplay2"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.mongodb" % "mongo-java-driver" % "2.7.3"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
    )

}
