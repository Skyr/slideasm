name := "SlideAsm"

version := "1.0"

scalaVersion := "2.10.0"

resolvers += "sonatype-public" at "http://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.clapper" % "grizzled-slf4j_2.10" % "1.0.1",
  "ch.qos.logback" % "logback-classic" % "1.0.9",
  "org.pegdown" % "pegdown" % "1.1.0",
  "org.jsoup" % "jsoup" % "1.6.3",
  "org.yaml" % "snakeyaml" % "1.14",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "org.clapper" % "scalasti_2.10" % "1.0.0"
)
