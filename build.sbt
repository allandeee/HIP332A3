/**
 * SBT build file for the Hipster cellular automaton language compiler.
 *
 * © 2017, Dominic Verity, Macquarie University, All rights reserved.
 */

// Name, rank and serial number.

name := "Hipster"
organization := "comp.mq.edu.au"
version := "0.1"

// Scala compiler settings

scalaVersion := "2.12.2"

scalacOptions :=
    Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xcheckinit",
        "-Xfatal-warnings",
        "-Xlint:-stars-align,_"
    )

// Interactive settings

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

// Execution

parallelExecution in Test := false

// Dependencies

libraryDependencies ++=
    Seq (
      "org.bitbucket.inkytonik.kiama" %% "kiama" %
        "2.1.0" withSources() withJavadoc(),
      "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.1.0" %
        "test" classifier ("tests"),
      "junit" % "junit" % "4.12" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
      "org.scalatest" %% "scalatest" % "3.0.3" % "test"
    )
