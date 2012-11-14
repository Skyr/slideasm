name := "SlideAsm"

version := "1.0"

scalaVersion := "2.9.2"

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
// "org.clapper" %% "argot" % "0.4"
  "org.clapper" %% "grizzled-scala" % "1.0.13",
  "org.pegdown" % "pegdown" % "1.1.0",
  "org.jsoup" % "jsoup" % "1.6.3",
  "org.yaml" % "snakeyaml" % "1.11",
  "com.github.scopt" %% "scopt" % "2.1.0"
)
