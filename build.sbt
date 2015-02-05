/** Project */
name := "signal-collect"

version := "3.0.2-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.11.4"

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

/** 
 * See https://github.com/sbt/sbt-assembly/issues/123
 */
mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList(ps @ _*) if ps.last == ".DS_Store" => MergeStrategy.discard
    case other => old(other)
  }
}

scalacOptions ++= Seq("-optimize", "-Ydelambdafy:inline", "-Yclosure-elim", "-Yinline-warnings", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-feature", "-deprecation", "-Xelide-below", "INFO")

//, "-Ylog:icode"
//, "-Ydebug"

assembleArtifact in packageScala := true

parallelExecution in Test := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

jarName in assembly := "signal-collect.jar"

/** Dependencies */
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.8" % "compile",
  "com.typesafe.akka" %% "akka-remote" % "2.3.8" % "compile",
  "org.scala-lang" % "scala-library" % scalaVersion.value % "compile",
  "com.github.romix.akka" %% "akka-kryo-serialization-custom" % "0.3.5" % "compile",
  "org.json4s" %% "json4s-native" % "3.2.11" % "compile",
  "org.java-websocket" % "Java-WebSocket" % "1.3.0" % "compile",
  "org.webjars" % "d3js" % "3.5.2",
  "org.webjars" % "jquery" % "2.1.3",
  "org.webjars" % "reconnecting-websocket" % "23d2fbc",
  "org.webjars" % "intro.js" % "1.0.0",
  "junit" % "junit" % "4.12"  % "test",
  "org.scalatest" %% "scalatest" % "2.2.3" % "compile", // 'compile' in order to share TestAnnouncement with other projects.
  "org.specs2" % "classycle" % "1.4.3" % "test",
  "org.mockito" % "mockito-all" % "1.10.17"  % "test",
  "org.specs2" %% "specs2" % "2.3.13"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
  "org.easymock" % "easymock" % "3.3" % "test"
)

resolvers += "Scala-Tools Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"

resolvers += "Sonatype Snapshots Repository" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Ifi Public" at "https://maven.ifi.uzh.ch/maven2/content/groups/public/"

transitiveClassifiers := Seq("sources")

seq(bintraySettings:_*)

pomExtra := (
 <url>https://github.com/uzh/signal-collect</url>
 <scm>
   <url>git@github.com:uzh/signal-collect.git</url>
   <connection>scm:git:git@github.com:uzh/signal-collect.git</connection>
 </scm>
 <developers>
   <developer>
     <id>pstutz</id>
     <name>Philip Stutz</name>
     <url>https://github.com/pstutz</url>
   </developer>
   <developer>
     <id>cshapeshifter</id>
     <name>Carol Alexandru</name>
     <url>https://github.com/cshapeshifter</url>
   </developer>
   <developer>
     <id>troxler</id>
     <name>Silvan Troxler</name>
     <url>https://github.com/troxler</url>
   </developer>
   <developer>
     <id>danistrebel</id>
     <name>Daniel Strebel</name>
     <url>https://github.com/danistrebel</url>
   </developer>
   <developer>
     <id>elaverman</id>
     <name>Mihaela Verman</name>
     <url>https://github.com/elaverman</url>
   </developer>
   <developer>
     <id>lorenzfischer</id>
     <name>Lorenz Fischer</name>
     <url>https://github.com/lorenzfischer</url>
   </developer>
   <developer>
     <id>tmsklr</id>
     <name>Thomas Keller</name>
     <url>https://github.com/tmsklr</url>
   </developer>
   <developer>
     <id>bibekp</id>
     <name>Bibek Paudel</name>
     <url>https://github.com/bibekp</url>
   </developer>
   <developer>
     <id>rampalli-github</id>
     <name>S. Rampalli</name>
     <url>https://github.com/rampalli-github</url>
   </developer>
 </developers>)
 
