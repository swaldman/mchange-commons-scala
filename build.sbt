val mainProjectName = "mchange-commons-scala"

val dependencies = Seq(
  "com.mchange"  %  "mchange-commons-java" % "0.2.15",
  "com.mchange"  %% "mlog-scala"           % "0.3.10",
  "com.mchange"  %% "failable"             % "0.0.1",
  "com.mchange"  %% "yinyang"              % "0.0.2",
  "com.typesafe" %  "config"               % "1.2.1"  % "compile,optional",
  "org.specs2"   %% "specs2-core"          % "2.4.17" % "test"
);

lazy val mainProject = (project in file(".")).settings(
  organization := "com.mchange",
  name := mainProjectName,
  version := "0.4.7-SNAPSHOT",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6"),
  scalacOptions ++= Seq("-deprecation", "-feature"),

  // UGH!
  //
  // add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
  // this mechanism supports cross-version publishing
  // taken from: http://github.com/scala/scala-module-dependency-sample
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, add dependency on scala-xml module
      case Some((2, scalaMajor)) if scalaMajor >= 11 => {
        libraryDependencies.value ++ Seq(
          //"org.scala-lang.modules" %% "scala-xml" % "1.0.1",
          //"org.scala-lang.modules" %% "scala-swing" % "1.0.1",
          "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "com.typesafe.akka" %% "akka-actor" % "2.4.18"
        )
      }
      case _ => {
        // or just libraryDependencies.value if you don't depend on scala-swing
        //libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
        libraryDependencies.value ++ Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value,
          "com.typesafe.akka" %% "akka-actor" % "2.3.15"
        )
      }
    }
  },

  libraryDependencies ++= dependencies,
  publishResolveSettings
)



lazy val publishResolveSettings = {
  val nexus = "https://oss.sonatype.org/"
  val nexusSnapshots = nexus + "content/repositories/snapshots";
  val nexusReleases = nexus + "service/local/staging/deploy/maven2";

  Seq(
    resolvers += ("releases" at nexusReleases),
    resolvers += ("snapshots" at nexusSnapshots),
    resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"),
    resolvers += ("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"),
    publishTo := {
      val v = version.value
      if (v.trim.endsWith("SNAPSHOT")) {
        Some("snapshots" at nexusSnapshots )
      }
      else {
        Some("releases"  at nexusReleases )
      }
    },
    pomExtra := {
      <url>https://github.com/swaldman/{mainProjectName}</url>
      <licenses>
        <license>
          <name>GNU Lesser General Public License, Version 2.1</name>
          <url>http://www.gnu.org/licenses/lgpl-2.1.html</url>
          <distribution>repo</distribution>
        </license>
        <license>
          <name>Eclipse Public License, Version 1.0</name>
          <url>http://www.eclipse.org/org/documents/epl-v10.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:swaldman/{mainProjectName}.git</url>
        <connection>scm:git:git@github.com:swaldman/{mainProjectName}</connection>
      </scm>
      <developers>
        <developer>
          <id>swaldman</id>
          <name>Steve Waldman</name>
          <email>swaldman@mchange.com</email>
        </developer>
      </developers>
    }
  )
}
