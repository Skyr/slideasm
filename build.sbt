name := "SlideAsm"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "sonatype-public" at "http://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq(
  "org.pegdown" % "pegdown" % "1.1.0",
  "org.jsoup" % "jsoup" % "1.6.3",
  "org.yaml" % "snakeyaml" % "1.11",
  "com.github.scopt" %% "scopt" % "2.1.0",
  "org.clapper" %% "scalasti" % "0.5.8"
)
