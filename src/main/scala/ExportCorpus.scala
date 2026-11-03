import training.{CorpusExporter, ExportConfig}

import java.io.{OutputStream, PrintStream}

/**
 * Entry point of the corpus exporter jar (`sbt assembly`):
 * {{{
 * java -cp improv-exporter.jar ExportCorpus [options] <input>... <output>
 * }}}
 * See [[training.CorpusExporter]] for what is exported and the JSONL schema, and `ExportConfig.USAGE` for the
 * arguments. Exit status: 0 when the run completed (also when some files failed: they are listed in the summary), 1 when
 * every file failed (a broken setup, not bad input), 2 on usage errors.
 *
 * It is kept out of the `training` package so that the command line stays short, and it does not read any resource
 * path from the class path, so it runs from a jar on a machine with only a JDK.
 */
object ExportCorpus {
  def main(args: Array[String]): Unit = {
    // The quiet logging setup (WARN to stderr, no log/message.log) must be chosen before logback initialises
    if (System.getProperty("logback.configurationFile") == null) {
      System.setProperty("logback.configurationFile", "logback-exporter.xml")
    }
    ExportConfig.parse(args.toList) match {
      case Left(error) =>
        System.err.println(s"error: $error\n${ExportConfig.USAGE}")
        sys.exit(2)
      case Right(config) =>
        val stdout = System.out
        // jMusic prints a progress banner per MIDI file on System.out: keep that off the real stdout
        System.setOut(new PrintStream(OutputStream.nullOutputStream()))
        val summary =
          try CorpusExporter.run(config)
          catch {
            case error: Exception =>
              System.setOut(stdout)
              System.err.println(s"error: ${error.getMessage}")
              sys.exit(2)
          } finally System.setOut(stdout)
        println(s"export done: $summary")
        if (summary.failures.size == summary.files) sys.exit(1)
    }
  }
}
