package training

import cbr.MusicalCase
import midi.JMusicMIDIParser
import representation.{Chord, MusicalElement, Note, Phrase}

import java.io.{BufferedWriter, OutputStream, OutputStreamWriter}
import java.math.RoundingMode
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.IdentityHashMap
import java.util.zip.GZIPOutputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

/**
 * One note of an exported phrase.
 * @param pitch MIDI pitch, 0 to 127
 * @param onsetBeats Start of the note relative to the start of its phrase, in beats (quarter notes), 6 decimals
 * @param durationBeats Length of the note in beats, 6 decimals
 * @param velocity Loudness class of the note as improv sees it (`Loudness.loudness`): 0, 10, 25, 50, 60, 70, 85, 100 or
 *                 120. The case library only keeps this class, not the raw MIDI velocity, so neither does the export.
 */
final case class CorpusNote(pitch: Int, onsetBeats: BigDecimal, durationBeats: BigDecimal, velocity: Int)

/**
 * One side (the phrase or its successor) of an exported pair.
 * @param instrument Instrument family from `InstrumentType.classify` (PIANO, GUITAR, BASS, ...)
 * @param program General MIDI program of the part, 0 to 127
 * @param channel MIDI channel of the part, 0 to 15 (never 9, the drum channel: the case library leaves drum parts out)
 * @param part Index of the part in the MIDI file as jMusic reads it
 * @param tempoBPM Tempo the phrase was converted with (nanoseconds back to beats)
 * @param durationBeats Length of the phrase in beats (longest voice)
 * @param voices The voices of the phrase: one list for a monophonic phrase, one per voice for a polyphonic one. Voices
 *               without notes are dropped. Notes are ordered by onset; rests are not exported (they are the gaps).
 */
final case class CorpusPhrase(
    instrument: String,
    program: Int,
    channel: Int,
    part: Int,
    tempoBPM: Double,
    durationBeats: BigDecimal,
    voices: List[List[CorpusNote]]) {
  /** JSON object of this phrase; cached because the same phrase is the `from` or `to` side of many pairs. */
  lazy val json: String = CorpusJson.phrase(this)
}

/**
 * A (phrase, successor) pair as `DefaultMusicalCaseExtractor` yields it.
 * @param song Stable id of the song: the path of the MIDI file relative to the input root, with `/` separators
 * @param position Index of `from` in the phrase sequence of its part; `to` is the phrase at `position + 1` of its part
 */
final case class CorpusPair(song: String, position: Int, from: CorpusPhrase, to: CorpusPhrase) {
  /** True when the successor is the next phrase of the same part (the extractor also yields these). */
  def samePart: Boolean = from.part == to.part

  def toJson: String = CorpusJson.pair(this)
}

/** JSON encoding of the corpus records (hand written: the shape is tiny and the lines are written in bulk). */
object CorpusJson {
  val DECIMALS = 6

  /** Rounds a time in beats to the precision of the export. */
  def beats(value: BigDecimal): BigDecimal = value.setScale(DECIMALS, scala.math.BigDecimal.RoundingMode.HALF_UP)

  /** Plain decimal notation with at least one fractional digit, so the value always parses as a float. */
  def number(value: BigDecimal): String = {
    val plain = value.bigDecimal.setScale(DECIMALS, RoundingMode.HALF_UP).stripTrailingZeros.toPlainString
    if (plain.contains('.')) plain else plain + ".0"
  }

  def string(value: String): String = {
    val sb = new StringBuilder("\"")
    value.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case '\r' => sb ++= "\\r"
      case '\t' => sb ++= "\\t"
      case c if c < ' ' =>
        sb ++= "\\u"
        sb ++= "%04x".format(c.toInt)
      case c => sb += c
    }
    sb += '"'
    sb.result()
  }

  def note(n: CorpusNote): String = s"[${n.pitch},${number(n.onsetBeats)},${number(n.durationBeats)},${n.velocity}]"

  def phrase(p: CorpusPhrase): String = {
    val voices = p.voices.map(_.map(note).mkString("[", ",", "]")).mkString("[", ",", "]")
    s"""{"instrument":${string(p.instrument)},"program":${p.program},"channel":${p.channel},"part":${p.part},""" +
      s""""tempoBPM":${number(BigDecimal(p.tempoBPM))},"durationBeats":${number(p.durationBeats)},"voices":$voices}"""
  }

  def pair(p: CorpusPair): String =
    s"""{"v":${CorpusExporter.SCHEMA_VERSION},"song":${string(p.song)},"position":${p.position},""" +
      s""""samePart":${p.samePart},"from":${p.from.json},"to":${p.to.json}}"""
}

/**
 * What to export and where to write it.
 * @param inputs MIDI files and directories (scanned recursively for `.mid` / `.midi`)
 * @param listFile Optional text file with one more input path per line (`#` comments and blank lines ignored)
 * @param root Song ids are paths relative to this directory. Default: the input directory a file was found under, or
 *             the file name for a file given directly or through `listFile`.
 * @param output A `.jsonl` or `.jsonl.gz` file, or a directory (then `pairs.jsonl` is written inside it)
 */
final case class ExportConfig(inputs: List[Path], listFile: Option[Path], root: Option[Path], output: Path)

object ExportConfig {
  val USAGE: String =
    """Usage: java -cp improv-exporter.jar ExportCorpus [options] <input>... <output>
      |  <input>   a MIDI file, or a directory scanned recursively for .mid / .midi files (repeatable)
      |  <output>  a .jsonl or .jsonl.gz file, or a directory (then <dir>/pairs.jsonl is written)
      |Options:
      |  --list <file>  read more input paths from <file>, one per line ('#' comments and blank lines are ignored)
      |  --root <dir>   song ids are paths relative to <dir> (default: the input directory, or the file name)
      |Writes <output without .jsonl[.gz]>.summary.json next to the output with the run's counts.""".stripMargin

  def parse(args: Seq[String]): Either[String, ExportConfig] = {
    var listFile: Option[Path] = None
    var root: Option[Path] = None
    val positional = mutable.ListBuffer[String]()
    val it = args.iterator
    var error: Option[String] = None
    while (it.hasNext && error.isEmpty) {
      it.next() match {
        case "--list" => if (it.hasNext) listFile = Some(Paths.get(it.next())) else error = Some("--list needs a file")
        case "--root" => if (it.hasNext) root = Some(Paths.get(it.next())) else error = Some("--root needs a directory")
        case flag if flag.startsWith("--") => error = Some(s"unknown option $flag")
        case value => positional += value
      }
    }
    error match {
      case Some(message) => Left(message)
      case None if positional.isEmpty => Left("missing <output>")
      case None if positional.size == 1 && listFile.isEmpty => Left("missing <input>")
      case None =>
        val paths = positional.toList.map(Paths.get(_))
        Right(ExportConfig(paths.init, listFile, root, paths.last))
    }
  }
}

/** Counts of one export run. `instruments` and `programs` are the distinct values that occur in the exported pairs. */
final case class ExportSummary(
    files: Int,
    failures: List[(String, String)],
    filesWithoutPairs: Int,
    songs: Int,
    pairs: Long,
    samePartPairs: Long,
    pairsByInstrument: Map[String, Long],
    programs: Set[Int]) {
  def instruments: Set[String] = pairsByInstrument.keySet

  def toJson: String = {
    val byInstrument = pairsByInstrument.toList.sortBy(_._1)
      .map { case (name, count) => s"${CorpusJson.string(name)}:$count" }.mkString("{", ",", "}")
    val failed = failures.map { case (song, error) =>
      s"""{"song":${CorpusJson.string(song)},"error":${CorpusJson.string(error)}}"""
    }.mkString("[", ",", "]")
    s"""{"files":$files,"failures":${failures.size},"filesWithoutPairs":$filesWithoutPairs,"songs":$songs,""" +
      s""""pairs":$pairs,"samePartPairs":$samePartPairs,"instruments":${instruments.size},""" +
      s""""pairsByInstrument":$byInstrument,"programs":${programs.toList.sorted.mkString("[", ",", "]")},""" +
      s""""failedFiles":$failed}"""
  }

  override def toString: String =
    s"files=$files failures=${failures.size} filesWithoutPairs=$filesWithoutPairs songs=$songs pairs=$pairs " +
      s"(samePart=$samePartPairs, otherPart=${pairs - samePartPairs}) instruments=${instruments.size} " +
      s"programs=${programs.size}"
}

/**
 * Exports the training corpus for the real-versus-random evaluator (issue #9): every (phrase, successor) pair that
 * improv's own case extraction yields for a MIDI file, as JSON Lines.
 *
 * The cases come from [[DefaultMusicalCaseExtractor]] on a parser built by `JMusicMIDIParser(filename)`, i.e. with the
 * splitter the case library uses at runtime (`PhraseSegmenter.byBars`, #27): a phrase is 2 bars, cut on the bar lines
 * of the file. Bar 1 starts at time 0, the bars follow the time signature events of the file (4/4 when there are none;
 * a change starts a new bar), and what is left after the last whole phrase of a part is dropped. A pickup bar is not
 * detected, so a file that starts with an anacrusis is cut that many beats before its real bar lines. Before #27 the
 * phrases were 5 second windows. If the library ever switches splitter again, re-export.
 *
 * Like the case library, the extractor pairs a phrase with the next phrase of every part, including its own part.
 * `samePart` tells them apart; a strictly "another instrument continues" corpus keeps `samePart == false`.
 *
 * == JSONL schema (version 1) ==
 * One line per pair, UTF-8, `\n` terminated:
 * {{{
 * {"v":1,"song":"country/crazy.mid","position":12,"samePart":false,
 *  "from":{"instrument":"PIANO","program":0,"channel":0,"part":1,"tempoBPM":120.0,"durationBeats":8.0,
 *          "voices":[[[60,0.0,1.0,70],[62,1.0,0.5,70]],[[36,0.0,4.0,60]]]},
 *  "to":{...same shape...}}
 * }}}
 *  - `song`: path of the MIDI file relative to the input root (or `--root`), `/` separated, extension included.
 *  - `position`: index of `from` in its part's phrase sequence. `to` is the phrase at `position + 1` of its own part.
 *  - `samePart`: `to` comes from the same part as `from`.
 *  - `instrument`, `program`, `channel`, `part`: where the phrase comes from. `instrument` is improv's family name,
 *    `program` the General MIDI program (0 to 127), `channel` the MIDI channel (never 9: drum parts are left out), `part`
 *    the part index in the file.
 *  - `tempoBPM`: tempo the phrase's nanosecond times were converted back to beats with.
 *  - `durationBeats`: length of the phrase in beats.
 *  - `voices`: a list of voices; a monophonic phrase has one, a polyphonic phrase one per voice. A voice is a list of
 *    notes `[pitch, onset, duration, velocity]` ordered by onset. `pitch` is a MIDI pitch, `onset` and `duration` are
 *    in beats relative to the phrase start (6 decimals), `velocity` is improv's loudness class (0, 10, 25, 50, 60,
 *    70, 85, 100, 120), not the raw MIDI velocity. Rests are not exported. Chord notes appear as simultaneous notes of
 *    one voice. Notes whose duration rounds to zero and voices left without notes are dropped (cutting a phrase no
 *    longer leaves slivers, #33, so this only catches zero length notes of the file itself); a pair with a side that
 *    has no notes at all is not exported.
 *
 * A phrase is as long in beats as its 2 bars add up to: `durationBeats` is 8 for 4/4 (and 2/2), 6 for 3/4, 4 for 2/4,
 * 10 for 5/4, and differs inside a song only where its time signature changes. In seconds, phrases follow the tempo of
 * the file (the tempo it spends the longest time in, see [[midi.MidiTempo]]). An `onset` of 0 is the start of a bar.
 */
object CorpusExporter {
  val SCHEMA_VERSION = 1

  private val MIDI_EXTENSIONS = Set(".mid", ".midi")

  /** Pairs of one MIDI file, extracted with the library's own extractor and splitter. */
  def extractPairs(songId: String, midiPath: String): List[CorpusPair] = {
    val parser = JMusicMIDIParser(midiPath)
    try {
      val parts = parser.score.getPartArray
      if (parts.isEmpty) throw new IllegalArgumentException("no parts found (unreadable or empty MIDI file)")

      // The extractor does not report where a phrase came from, but it hands back the very phrase objects the
      // parser memoizes, so an identity lookup recovers the part and the position.
      val positions = new IdentityHashMap[Phrase, (Int, Int)]()
      for (partIdx <- parts.indices; (phrase, idx) <- parser.getMultiVoicePhrases(partIdx).zipWithIndex) {
        positions.put(phrase, (partIdx, idx))
      }

      val converted = new IdentityHashMap[Phrase, CorpusPhrase]()
      def side(musicalCase: MusicalCase): (CorpusPhrase, Int) = {
        val (partIdx, idx) = Option(positions.get(musicalCase.phrase))
          .getOrElse(throw new IllegalStateException("extracted phrase does not belong to any part"))
        val corpusPhrase = Option(converted.get(musicalCase.phrase)).getOrElse {
          val part = parts(partIdx)
          val created = toCorpusPhrase(
            musicalCase.phrase,
            musicalCase.instrumentType.getClass.getSimpleName,
            part.getInstrument,
            part.getChannel,
            partIdx)
          converted.put(musicalCase.phrase, created)
          created
        }
        (corpusPhrase, idx)
      }

      MusicalCaseExtractors.getDefault(_ => parser).getCases(midiPath)
        .map { case (problem, solution) =>
          val (from, position) = side(problem)
          val (to, _) = side(solution)
          CorpusPair(songId, position, from, to)
        }
        .filter(pair => pair.from.voices.nonEmpty && pair.to.voices.nonEmpty)
    } finally {
      // Memoizers are global: without this every exported file would stay in memory for the rest of the run
      midi.JMusicParserUtils.convertPart.clear()
      midi.JMusicParserUtils.convertPhrase.clear()
    }
  }

  def toCorpusPhrase(phrase: Phrase, instrument: String, program: Int, channel: Int, part: Int): CorpusPhrase = {
    val tempo = BigDecimal(phrase.tempoBPM)
    val voices =
      if (phrase.polyphony) phrase.musicalElements.map(voice => notesOf(voice, tempo))
      else List(notesOf(phrase, tempo))
    CorpusPhrase(
      instrument,
      program,
      channel,
      part,
      phrase.tempoBPM,
      CorpusJson.beats(phrase.getDurationBPM(tempo)),
      voices.filter(_.nonEmpty))
  }

  /**
   * The notes of an element in order of onset, with times converted from nanoseconds to beats at `tempoBPM`. Notes
   * whose duration rounds to zero are dropped. Slivers left where a note was cut at the edge of a phrase used to be
   * what this caught; [[representation.MusicalElement.split]] no longer leaves them (#33), so it is a safety net now.
   */
  private def notesOf(element: MusicalElement, tempoBPM: BigDecimal): List[CorpusNote] = {
    def collect(elem: MusicalElement, tempo: BigDecimal): List[CorpusNote] = elem match {
      case note: Note =>
        val duration = CorpusJson.beats(note.getDurationBPM(tempo))
        Option.when(duration > 0)(CorpusNote(
          note.midiPitch,
          CorpusJson.beats(note.getStartTimeBPM(tempo)),
          duration,
          note.loudness.loudness)).toList
      case chord: Chord => chord.elements.flatMap(collect(_, tempo))
      case nested: Phrase => nested.musicalElements.flatMap(collect(_, BigDecimal(nested.tempoBPM)))
      case _ => Nil // rests are the gaps between onsets
    }
    collect(element, tempoBPM).sortBy(_.onsetBeats)
  }

  /** The MIDI files to export with their song ids, in song id order. */
  def collectFiles(config: ExportConfig): List[(String, Path)] = {
    val listed = config.listFile.toList.flatMap { listFile =>
      Files.readAllLines(listFile, StandardCharsets.UTF_8).asScala.map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#")).map(Paths.get(_))
    }
    val explicitRoot = config.root.map(_.toAbsolutePath.normalize)
    val found = (config.inputs ++ listed).flatMap { input =>
      val absolute = input.toAbsolutePath.normalize
      if (Files.isDirectory(absolute)) {
        val files = Using.resource(Files.walk(absolute))(_.iterator.asScala.filter(isMidiFile).toList)
        files.map(file => (file, explicitRoot.getOrElse(absolute)))
      } else if (Files.isRegularFile(absolute)) {
        List((absolute, explicitRoot.getOrElse(absolute.getParent)))
      } else {
        throw new IllegalArgumentException(s"input does not exist: $input")
      }
    }
    val withIds = found.distinctBy(_._1).map { case (file, root) =>
      if (!file.startsWith(root)) throw new IllegalArgumentException(s"$file is not under the root $root")
      (root.relativize(file).toString.replace('\\', '/'), file)
    }
    withIds.groupBy(_._1).collectFirst { case (id, files) if files.size > 1 => (id, files) }.foreach { case (id, files) =>
      throw new IllegalArgumentException(
        s"song id $id is used by ${files.map(_._2).mkString(" and ")}; pass --root so that ids are unique")
    }
    withIds.sortBy(_._1)
  }

  private def isMidiFile(path: Path): Boolean =
    Files.isRegularFile(path) && MIDI_EXTENSIONS.exists(path.getFileName.toString.toLowerCase.endsWith)

  /** The pairs file and the summary file for an `<output>` argument. */
  def outputPaths(output: Path): (Path, Path) = {
    val name = output.getFileName.toString
    val pairsFile =
      if (name.endsWith(".jsonl") || name.endsWith(".jsonl.gz")) output else output.resolve("pairs.jsonl")
    val pairsName = pairsFile.getFileName.toString
    val base = pairsName.stripSuffix(".gz").stripSuffix(".jsonl")
    (pairsFile, pairsFile.resolveSibling(base + ".summary.json"))
  }

  /**
   * Exports every file of the config. A file that fails (unreadable, or it makes the extractor throw) is logged and
   * skipped. The pairs file is written under a `.part` name and renamed when complete, so a crashed run never leaves
   * a file that looks finished.
   */
  def run(config: ExportConfig, log: String => Unit = message => System.err.println(message)): ExportSummary = {
    val files = collectFiles(config)
    require(files.nonEmpty, "no MIDI files found in the inputs")
    val (pairsFile, summaryFile) = outputPaths(config.output)
    Files.createDirectories(pairsFile.toAbsolutePath.getParent)
    val partFile = pairsFile.resolveSibling(pairsFile.getFileName.toString + ".part")

    val failures = mutable.ListBuffer[(String, String)]()
    val byInstrument = mutable.Map[String, Long]().withDefaultValue(0L)
    val programs = mutable.Set[Int]()
    var songs = 0
    var pairCount = 0L
    var samePartCount = 0L

    val writer = openWriter(partFile)
    try {
      files.zipWithIndex.foreach { case ((songId, file), index) =>
        try {
          val pairs = extractPairs(songId, file.toString)
          pairs.foreach { pair =>
            writer.write(pair.toJson)
            writer.write('\n')
            byInstrument(pair.from.instrument) += 1
            programs += pair.from.program
            programs += pair.to.program
          }
          pairCount += pairs.size
          samePartCount += pairs.count(_.samePart)
          if (pairs.nonEmpty) songs += 1
          if ((index + 1) % 25 == 0 || index + 1 == files.size) {
            log(s"[${index + 1}/${files.size}] $songId: ${pairs.size} pairs (total $pairCount)")
          }
        } catch {
          case error @ (NonFatal(_) | _: StackOverflowError | _: OutOfMemoryError) =>
            val message = s"${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("")}".trim
            log(s"[${index + 1}/${files.size}] FAILED $songId: $message")
            failures += ((songId, message))
        }
      }
    } finally {
      writer.close()
    }
    Files.move(partFile, pairsFile, StandardCopyOption.REPLACE_EXISTING)

    // Every file that failed counts as a failure, not as a file without pairs
    val summary = ExportSummary(
      files = files.size,
      failures = failures.toList,
      filesWithoutPairs = files.size - failures.size - songs,
      songs = songs,
      pairs = pairCount,
      samePartPairs = samePartCount,
      pairsByInstrument = byInstrument.toMap,
      programs = programs.toSet)
    Files.writeString(summaryFile, summary.toJson + "\n", StandardCharsets.UTF_8)
    summary
  }

  private def openWriter(path: Path): BufferedWriter = {
    val raw: OutputStream = Files.newOutputStream(path)
    val stream =
      if (path.getFileName.toString.stripSuffix(".part").endsWith(".gz")) new GZIPOutputStream(raw, 1 << 16) else raw
    new BufferedWriter(new OutputStreamWriter(stream, StandardCharsets.UTF_8), 1 << 16)
  }
}
