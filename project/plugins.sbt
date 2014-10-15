resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
   url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "maven" at "http://repo1.maven.org/maven2/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.6.2")

addSbtPlugin("no.arktekk.sbt" % "aether-deploy" % "0.13")
