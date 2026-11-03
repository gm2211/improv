name := "improv"
version := "0.1.0"
scalaVersion := "3.10.0"

// Vendored jars (jfugue, jMusic, javaml, neuroph) in lib/ are picked up automatically as unmanaged jars.
// neuroph stays on the vendored 2.9 jar: 2.98 (Maven Central) cannot read knowledgeBase/NN.nnet (serialVersionUID 6 vs 7) and changes DataSet.addRow/learn APIs.

val pekkoVersion = "1.7.1"
val jacksonVersion = "3.2.4"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor" % pekkoVersion,
  "org.apache.pekko" %% "pekko-slf4j" % pekkoVersion,
  "org.apache.pekko" %% "pekko-testkit" % pekkoVersion % Test,
  "tools.jackson.module" %% "jackson-module-scala" % jacksonVersion,
  "tools.jackson.core" % "jackson-databind" % jacksonVersion,
  // Jackson 3 keeps the annotations in com.fasterxml.jackson.annotation, released per minor version only
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.22",
  // mapdb pulls in net.jpountz.lz4:lz4:1.3.0, which has known CVEs: excluded below in favour of a maintained fork
  ("org.mapdb" % "mapdb" % "3.1.0").exclude("net.jpountz.lz4", "lz4"),
  "at.yawk.lz4" % "lz4-java" % "1.12.1",
  "com.google.guava" % "guava" % "33.7.2-jre",
  "ch.qos.logback" % "logback-classic" % "1.6.6",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.scalamock" %% "scalamock-scalatest" % "7.6.2" % Test
)

// Resource paths are derived from the system class loader (IOUtils.getResourcePath) and the demos
// write back into the knowledge base, so forked JVMs must see the class and resource directories,
// not jars or copies. sbt 2 otherwise packages classes into a jar (exportedProductJars,
// fullClasspathAsJars) and copies the classpath into target/bg-jobs (bgCopyClasspath).
exportJars := false
Compile / exportedProductJars := (Compile / exportedProducts).value
Test / exportedProductJars := (Test / exportedProducts).value
Compile / fullClasspathAsJars := (Compile / fullClasspath).value
Runtime / fullClasspathAsJars := (Runtime / fullClasspath).value
Test / fullClasspathAsJars := (Test / fullClasspath).value
bgCopyClasspath := false
fork := true
Test / fork := true
Compile / run / connectInput := true
outputStrategy := Some(StdoutOutput)
Compile / run / mainClass := Some("demos.DemoCreateTrainingDataANN")

Test / testOptions += Tests.Argument(
  TestFrameworks.ScalaTest,
  "-l", "com.gm2211.improv.test.tags.SlowTest",
  "-l", "com.gm2211.improv.test.tags.Manual"
)
Test / javaOptions += "-Djava.awt.headless=true"

javacOptions ++= Seq("--release", "25")
scalacOptions ++= Seq("-java-output-version", "25", "-deprecation", "-feature")

// Corpus exporter (issue #9): `sbt assembly` writes one jar with the vendored lib/ jars and all dependencies inside, which
// runs on a machine with only a JDK 25 (the build targets Java 25 bytecode):
//   java -cp improv-exporter.jar ExportCorpus <input>... <output>
assembly / assemblyJarName := "improv-exporter.jar"
assembly / mainClass := Some("ExportCorpus")
assembly / test := sbt.protocol.testing.TestResult.Passed // building the jar does not wait for the test suite
assembly / packageOptions += Package.ManifestAttributes("Multi-Release" -> "true")
assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
  case PathList(".idea", _*) => MergeStrategy.discard // IDE settings that the vendored jars were built with
  case "logback.xml" => MergeStrategy.preferProject // neuroph-core ships its own
  case other =>
    val defaultStrategy = (assembly / assemblyMergeStrategy).value
    defaultStrategy(other)
}
