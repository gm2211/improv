# Improv
Distributed improvising orchestra.

[paper](https://github.com/gm2211/Improv/blob/develop/finalReport.pdf)

# Requirements
JDK 25 and sbt, e.g. `brew install openjdk@25 sbt` (openjdk@25 is keg-only: put
`/opt/homebrew/opt/openjdk@25/libexec/openjdk.jdk/Contents/Home/bin` first on your `PATH` or set `JAVA_HOME` to it).

# Compilation
Run `sbt compile` (and `sbt Test/compile` for the tests).
# Testing
Run `sbt test`. Tests tagged `SlowTest` or `Manual` are excluded by default.
# Development
Import the project in IntelliJ Idea as an sbt project (File > Open, pick `build.sbt`).
# Running the project
Run `sbt run` (starts `demos.DemoCreateTrainingDataANN`), or pick a demo with `sbt "runMain demos.DemoCBROrchestra"`.
To build the case base first, run `sbt "runMain demos.DemoPopulateDB"`.

Note: sbt 2 copies the classpath (including `src/main/resources`) for every `run`, so anything a demo
writes into the knowledge base (e.g. `DemoPopulateDB`) lands in the copy under `target/bg-jobs`. To persist
the result into `src/main/resources/knowledgeBase`, run the demo with plain `java` on the exported classpath
(`sbt "export Runtime/fullClasspath"`).

# Corpus export (real-versus-random evaluator)
`ExportCorpus` writes every (phrase, successor) pair that improv's case extraction yields for a set of MIDI files as
JSON Lines, with onsets and durations in beats (schema: scaladoc of `training.CorpusExporter`). Build the single jar
with `sbt assembly` (the vendored `lib/` jars are inside) and run it on any machine with JDK 25:

    java -cp target/out/jvm/scala-3.10.0/improv/improv-exporter.jar ExportCorpus <input>... <output>

`<input>` is a MIDI file or a directory (scanned recursively), `<output>` a `.jsonl` / `.jsonl.gz` file or a directory.
Run `ExportCorpus` without arguments for the options. The output goes to `corpus/` (gitignored).
