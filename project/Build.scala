import sbt._

object MchangeCommonsScalaBuild extends Build {

  val nexus = "https://oss.sonatype.org/"
  val nexusSnapshots = nexus + "content/repositories/snapshots";
  val nexusReleases = nexus + "service/local/staging/deploy/maven2";

  val mySettings = Seq( 
    Keys.organization := "com.mchange",
    Keys.name := "mchange-commons-scala", 
    Keys.version := "0.4.3", 
    Keys.crossScalaVersions := Seq("2.10.4", "2.11.6", "2.12.4"),
    Keys.scalaVersion := "2.11.6",
    Keys.publishTo <<= Keys.version { 
      (v: String) => {
	if (v.trim.endsWith("SNAPSHOT"))
	  Some("snapshots" at nexusSnapshots )
	else
	  Some("releases"  at nexusReleases )
      }
    },
    Keys.resolvers += ("snapshots" at nexusSnapshots ),
    Keys.resolvers += ("Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"),
    Keys.scalacOptions ++= Seq("-deprecation", "-feature"),
    Keys.pomExtra := pomExtraXml,

    // UGH!
    //
    // add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
    // this mechanism supports cross-version publishing
    // taken from: http://github.com/scala/scala-module-dependency-sample
    Keys.libraryDependencies := {
      CrossVersion.partialVersion(Keys.scalaVersion.value) match {
        // if scala 2.11+ is used, add dependency on scala-xml module
        case Some((2, scalaMajor)) if scalaMajor >= 11 => {
          Keys.libraryDependencies.value ++ Seq(
            //"org.scala-lang.modules" %% "scala-xml" % "1.0.1",
            //"org.scala-lang.modules" %% "scala-swing" % "1.0.1",
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
            "org.scala-lang" % "scala-reflect" % Keys.scalaVersion.value,
            "com.typesafe.akka" %% "akka-actor" % "2.4.18"
          )
        }
        case _ => {
          // or just libraryDependencies.value if you don't depend on scala-swing
          //libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
          Keys.libraryDependencies.value ++ Seq(
            "org.scala-lang" % "scala-reflect" % "2.10.4",
            "com.typesafe.akka" %% "akka-actor" % "2.3.15"
          )
        }
      }
    }
  );

  val dependencies = Seq(
    "com.typesafe" % "config" % "1.2.1" % "compile,optional",
    "org.specs2"  %% "specs2-core" % "2.4.17" % "test",
    "com.mchange" %% "mlog-scala" % "0.3.10",
    "com.mchange" %% "yinyang" % "0.0.2",
    "com.mchange" % "mchange-commons-java" % "0.2.14"
  );

  override lazy val settings = super.settings ++ mySettings;

  lazy val mainProject = Project(
    id = "mchange-commons-scala",
    base = file("."),
    settings = Defaults.coreDefaultSettings ++ (Keys.libraryDependencies ++= dependencies)
  ); 

  val pomExtraXml = (
      <url>https://github.com/swaldman/mchange-commons-scala</url>
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
       <url>git@github.com:swaldman/mchange-commons-scala.git</url>
       <connection>scm:git:git@github.com:swaldman/mchange-commons-scala.git</connection>
     </scm>
     <developers>
       <developer>
         <id>swaldman</id>
         <name>Steve Waldman</name>
         <email>swaldman@mchange.com</email>
       </developer>
     </developers>
  );
}

