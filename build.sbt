
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello"
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0-MF"

