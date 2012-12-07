resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "Typesafe Repository" at "http://nexus.carboniteinc.com/content/repositories/typesafe/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0")

