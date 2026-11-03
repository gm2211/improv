// Builds the exporter's single runnable jar (`sbt assembly`, see ExportCorpus). 2.5.0 was released 2026-08-27; it, its
// dependencies (jarjar-abrams-core and jarjar 1.18.0, asm 9.10.1, sbt2-compat 0.1.0) have no OSV advisories.
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.5.0")
