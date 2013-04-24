import sbt._

object MchangeCommonsScalaBuild extends Build {

  val nexus = "https://oss.sonatype.org/"
  val nexusSnapshots = nexus + "content/repositories/snapshots";
  val nexusReleases = nexus + "service/local/staging/deploy/maven2";

  val mySettings = Seq( 
    Keys.organization := "com.mchange",
    Keys.name := "mchange-commons-scala", 
    Keys.version := "0.4.0-SNAPSHOT", 
    Keys.scalaVersion := "2.10.1",
    Keys.publishTo <<= Keys.version { 
      (v: String) => {
	if (v.trim.endsWith("SNAPSHOT"))
	  Some("snapshots" at nexusSnapshots )
	else
	  Some("releases"  at nexusReleases )
      }
    },
    Keys.resolvers += ("snapshots" at nexusSnapshots ),
    Keys.scalacOptions ++= Seq("-deprecation", "-feature"),
    Keys.pomExtra := pomExtraXml
  );

  val dependencies = Seq(
    "org.scala-lang" % "scala-reflect" % "2.10.1",
    "org.scala-lang" % "scala-actors" % "2.10.1",
    "com.typesafe.akka" %% "akka-actor" % "2.1+",
    "com.typesafe" % "config" % "1.0.0" % "compile,optional",
    "org.specs2"  %% "specs2" % "1.14+" % "test",
    "com.mchange" % "mchange-commons-java" % "0.2.6-SNAPSHOT" changing(),
    "com.mchange" %% "mchange-commons-scala-macro" % "0.0.1-SNAPSHOT" changing()
  );

  override lazy val settings = super.settings ++ mySettings;

  lazy val mainProject = Project(
    id = "mchange-commons-scala",
    base = file("."),
    settings = Project.defaultSettings ++ (Keys.libraryDependencies ++= dependencies)
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

