//> using scala 2.13
//> using dep com.typesafe::mima-core:1.1.6

// Backward + forward binary-compatibility check between a released artifact and
// the current build, via MiMa's core API (there is no scala-cli MiMa plugin).
//
// Usage: scala-cli run .mima/bin-compat-check.scala -- <oldJar> <newJar> <sharedClasspath>
//   oldJar          the previously released library JAR
//   newJar          the freshly built library JAR (scala-cli package --library)
//   sharedClasspath pathSeparator-joined dependency classpath (scala3-library, deps, …)
//
// Exit code: 0 if compatible, 1 if any problems were found.

import java.io.File
import com.typesafe.tools.mima.lib.MiMaLib
import com.typesafe.tools.mima.core.Problem

object BinCompatCheck {
  def main(args: Array[String]): Unit = {
    val Array(oldJar, newJar, sharedCp) = args
    val classpath = sharedCp.split(File.pathSeparator).iterator
      .filter(_.nonEmpty).map(new File(_)).toList

    def problems(prev: String, curr: String): List[Problem] =
      new MiMaLib(classpath).collectProblems(new File(prev), new File(curr), Nil)

    val backward = problems(oldJar, newJar)
    val forward  = problems(newJar, oldJar)

    def report(label: String, ps: List[Problem]): Unit =
      if (ps.isEmpty) println(s"[mima] $label: OK")
      else {
        println(s"[mima] $label: ${ps.size} problem(s)")
        ps.foreach(p => println(s"  - ${p.description("current")}"))
      }

    report("backward (code built against the release vs the new JAR)", backward)
    report("forward  (code built against the new JAR vs the release)", forward)

    if (backward.nonEmpty || forward.nonEmpty) sys.exit(1)
  }
}
