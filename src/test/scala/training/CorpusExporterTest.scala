package training

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.{Chord, FFF, Note, Phrase, Rest, F as Forte}
import tools.jackson.databind.json.JsonMapper

import java.io.{File, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

class CorpusExporterTest extends AnyFlatSpec with Matchers {
  private val mapper = JsonMapper.builder().build()

  // cajun-fiddle.mid: 200 BPM and 2 parts the case library sees (a fiddle on channel 0 = ETHNIC, program 110, and one on
  // channel 1 = PIANO, no program change), with 56 pairs: 28 where the successor is the next phrase of the same part,
  // 28 where it is the other part's. Its two drum tracks (channel 9) are left out (#22): the first used to add 38 pairs
  // (94 in all), the second took part in none.
  private val KNOWN_FILE = "trainingMIDIs/cajun-fiddle.mid"

  private def withTempDir[A](test: Path => A): A = {
    val dir = Files.createTempDirectory("corpus-exporter-test")
    try test(dir)
    finally {
      def delete(file: File): Unit = {
        Option(file.listFiles).foreach(_.foreach(delete))
        file.delete()
      }
      delete(dir.toFile)
    }
  }

  private def copyResource(resource: String, target: Path): Path = {
    Files.createDirectories(target.getParent)
    Using.resource(getClass.getClassLoader.getResourceAsStream(resource)) { (in: InputStream) =>
      Files.copy(in, target)
    }
    target
  }

  private def ns(seconds: Double): BigInt = BigInt((seconds * 1e9).toLong)

  private def note(pitch: Int, startSeconds: Double, durationSeconds: Double, loudness: representation.Loudness = Forte) =
    Note(midiPitch = pitch, startTimeNS = ns(startSeconds), durationNS = ns(durationSeconds), loudness = loudness)

  private def corpusNote(pitch: Int, onset: String, duration: String, velocity: Int) =
    CorpusNote(pitch, BigDecimal(onset), BigDecimal(duration), velocity)

  private def readLines(file: Path): List[String] = Files.readAllLines(file, StandardCharsets.UTF_8).asScala.toList

  "The exporter" should "convert nanoseconds to beats with the tempo of the phrase" in {
    // a rest of 1.5 s, then notes of 0.75 s and 0.5 s
    def phraseAt(tempoBPM: Double) = Phrase(
      List(
        Rest(startTimeNS = ns(0), durationNS = ns(1.5)),
        note(64, 1.5, 0.75),
        note(67, 2.25, 0.5, FFF)),
      tempoBPM = tempoBPM)

    // 120 BPM is 2 beats per second
    val fast = CorpusExporter.toCorpusPhrase(phraseAt(120), "PIANO", 0, 0, 0)
    fast.voices shouldBe List(List(corpusNote(64, "3.0", "1.5", 85), corpusNote(67, "4.5", "1.0", 120)))
    fast.durationBeats shouldBe BigDecimal("5.5")
    fast.tempoBPM shouldBe 120.0

    // 60 BPM is 1 beat per second
    val slow = CorpusExporter.toCorpusPhrase(phraseAt(60), "PIANO", 0, 0, 0)
    slow.voices shouldBe List(List(corpusNote(64, "1.5", "0.75", 85), corpusNote(67, "2.25", "0.5", 120)))
    slow.durationBeats shouldBe BigDecimal("2.75")
  }

  it should "keep the voices of a polyphonic phrase, flatten chords and drop slivers and empty voices" in {
    val melody = Phrase(List(note(72, 0, 0.5), Rest(startTimeNS = ns(0.5), durationNS = ns(0.5)), note(74, 1, 0.5)), tempoBPM = 120)
    val chords = Phrase(
      List(Chord(List(note(48, 0, 1), note(55, 0, 1))), Chord(List(note(50, 1, 1), note(57, 1, 1)))),
      tempoBPM = 120)
    val sliver = Phrase(List(Note(midiPitch = 60, startTimeNS = ns(0), durationNS = BigInt(100))), tempoBPM = 120)
    val silent = Phrase(List(Rest(startTimeNS = ns(0), durationNS = ns(2))), tempoBPM = 120)
    val polyphonic = Phrase(List(melody, chords, sliver, silent), polyphony = true, tempoBPM = 120)

    val exported = CorpusExporter.toCorpusPhrase(polyphonic, "ORGAN", 19, 3, 2)

    exported.voices shouldBe List(
      List(corpusNote(72, "0.0", "1.0", 85), corpusNote(74, "2.0", "1.0", 85)),
      List(
        corpusNote(48, "0.0", "2.0", 85),
        corpusNote(55, "0.0", "2.0", 85),
        corpusNote(50, "2.0", "2.0", 85),
        corpusNote(57, "2.0", "2.0", 85)))
    exported.durationBeats shouldBe BigDecimal("4.0")
    (exported.instrument, exported.program, exported.channel, exported.part) shouldBe ("ORGAN", 19, 3, 2)
  }

  it should "write a pair as one JSON line with the documented shape" in {
    val from = CorpusExporter.toCorpusPhrase(
      Phrase(List(note(60, 0, 0.5), note(62, 0.5, 0.25, FFF)), tempoBPM = 120), "PIANO", 0, 0, 0)
    val to = CorpusExporter.toCorpusPhrase(Phrase(List(note(36, 0, 1)), tempoBPM = 90), "BASS", 33, 1, 1)
    val line = CorpusPair("dir/\"quoted\" song.mid", 4, from, to).toJson

    line should not include "\n"
    val json = mapper.readTree(line)
    json.get("v").asInt() shouldBe 1
    json.get("song").asString() shouldBe "dir/\"quoted\" song.mid"
    json.get("position").asInt() shouldBe 4
    json.get("samePart").asBoolean() shouldBe false
    val jsonFrom = json.get("from")
    jsonFrom.get("instrument").asString() shouldBe "PIANO"
    jsonFrom.get("tempoBPM").asDouble() shouldBe 120.0
    jsonFrom.get("durationBeats").asDouble() shouldBe 1.5
    val voices = jsonFrom.get("voices")
    voices.size() shouldBe 1
    val second = voices.get(0).get(1)
    List(second.get(0).asInt(), second.get(1).asDouble(), second.get(2).asDouble(), second.get(3).asInt()) shouldBe
      List(62, 1.0, 0.5, 120)
    json.get("to").get("channel").asInt() shouldBe 1
    json.get("to").get("tempoBPM").asDouble() shouldBe 90.0
    line should include("\"tempoBPM\":120.0")
  }

  it should "export a known file with the pairs, splitter and conversion of the case library" in withTempDir { dir =>
    val midi = copyResource(KNOWN_FILE, dir.resolve("cajun-fiddle.mid"))

    val pairs = CorpusExporter.extractPairs("cajun-fiddle.mid", midi.toString)

    // the same number of pairs as improv's own extractor gives the case library
    pairs should have size MusicalCaseExtractors.getDefault().getCases(midi.toString).size
    pairs should have size 56
    pairs.count(_.samePart) shouldBe 28
    pairs.map(_.from.part).toSet shouldBe Set(0, 1)
    // no drum phrase: the library leaves the parts on MIDI channel 10 (index 9) out
    pairs.flatMap(pair => List(pair.from, pair.to)).map(_.channel).toSet shouldBe Set(0, 1)

    val first = pairs.head
    first.song shouldBe "cajun-fiddle.mid"
    first.position shouldBe 0
    (first.from.instrument, first.from.program, first.from.channel, first.from.part) shouldBe ("ETHNIC", 110, 0, 0)
    first.from.tempoBPM shouldBe 200.0
    // five seconds at 200 BPM
    first.from.durationBeats shouldBe BigDecimal("16.666667")
    // two voices; the MIDI file has notes 64 and 69 on beat 1 of the first fiddle track, at velocity 127
    first.from.voices.map(_.head).toSet shouldBe
      Set(corpusNote(64, "1.0", "0.5", 120), corpusNote(69, "1.0", "0.5", 120))
    // the successor starts with the 64 that was cut at the edge of the window: 1 beat, 2/3 before and 1/3 after it
    first.to.voices.map(_.head).toSet should contain(corpusNote(64, "0.0", "0.333333", 120))

    pairs.foreach { pair =>
      pair.from.voices should not be empty
      pair.to.voices should not be empty
      pair.from.voices.flatten.foreach { n =>
        n.onsetBeats should be >= BigDecimal(0)
        n.durationBeats should be > BigDecimal(0)
      }
    }
  }

  it should "skip a file that fails to parse, count it and still write the other files" in withTempDir { dir =>
    val in = dir.resolve("in")
    copyResource(KNOWN_FILE, in.resolve("country/cajun-fiddle.mid"))
    Files.createDirectories(in.resolve("broken"))
    Files.write(in.resolve("broken/garbage.mid"), "this is not a midi file".getBytes(StandardCharsets.UTF_8))
    val output = dir.resolve("out/pairs.jsonl")
    val logs = ListBuffer[String]()

    val summary = CorpusExporter.run(ExportConfig(List(in), None, None, output), logs += _)

    summary.files shouldBe 2
    summary.failures.map(_._1) shouldBe List("broken/garbage.mid")
    logs.exists(_.contains("FAILED broken/garbage.mid")) shouldBe true
    summary.songs shouldBe 1
    summary.filesWithoutPairs shouldBe 0
    summary.pairs shouldBe 56
    summary.samePartPairs shouldBe 28
    summary.instruments shouldBe Set("ETHNIC", "PIANO")
    summary.programs shouldBe Set(0, 110)

    val lines = readLines(output)
    lines should have size 56
    mapper.readTree(lines.head).get("song").asString() shouldBe "country/cajun-fiddle.mid"
    Files.exists(output.resolveSibling("pairs.jsonl.part")) shouldBe false

    val summaryJson = mapper.readTree(readLines(output.resolveSibling("pairs.summary.json")).mkString)
    summaryJson.get("pairs").asInt() shouldBe 56
    summaryJson.get("failures").asInt() shouldBe 1
    summaryJson.get("failedFiles").get(0).get("song").asString() shouldBe "broken/garbage.mid"
  }

  it should "write a directory output as pairs.jsonl and gzip when the name ends in .gz" in withTempDir { dir =>
    val midi = copyResource(KNOWN_FILE, dir.resolve("songs/cajun-fiddle.mid"))

    CorpusExporter.run(ExportConfig(List(midi), None, None, dir.resolve("out-dir")), _ => ())
    readLines(dir.resolve("out-dir/pairs.jsonl")) should have size 56
    mapper.readTree(readLines(dir.resolve("out-dir/pairs.jsonl")).head).get("song").asString() shouldBe "cajun-fiddle.mid"

    val gz = dir.resolve("out-gz/pairs.jsonl.gz")
    CorpusExporter.run(ExportConfig(List(midi), None, None, gz), _ => ())
    val lines = Using.resource(new GZIPInputStream(Files.newInputStream(gz))) { in =>
      new String(in.readAllBytes(), StandardCharsets.UTF_8).linesIterator.toList
    }
    lines should have size 56
    Files.exists(dir.resolve("out-gz/pairs.summary.json")) shouldBe true
  }

  "The input collection" should "use paths relative to the root as song ids and reject duplicates" in withTempDir { dir =>
    val one = copyResource(KNOWN_FILE, dir.resolve("songs/a/one.mid"))
    val two = copyResource(KNOWN_FILE, dir.resolve("songs/b/two.MID"))
    Files.writeString(dir.resolve("songs/readme.txt"), "not a MIDI file")
    val output = dir.resolve("out.jsonl")

    CorpusExporter.collectFiles(ExportConfig(List(dir.resolve("songs")), None, None, output)).map(_._1) shouldBe
      List("a/one.mid", "b/two.MID")

    val list = dir.resolve("list.txt")
    Files.writeString(list, s"# a comment\n\n$two\n")
    CorpusExporter.collectFiles(ExportConfig(Nil, Some(list), Some(dir.resolve("songs")), output)).map(_._1) shouldBe
      List("b/two.MID")
    CorpusExporter.collectFiles(ExportConfig(List(one), None, None, output)).map(_._1) shouldBe List("one.mid")

    val other = copyResource(KNOWN_FILE, dir.resolve("other/one.mid"))
    an[IllegalArgumentException] should be thrownBy
      CorpusExporter.collectFiles(ExportConfig(List(one, other), None, None, output))
    an[IllegalArgumentException] should be thrownBy
      CorpusExporter.collectFiles(ExportConfig(List(dir.resolve("missing.mid")), None, None, output))
  }

  "The command line" should "accept inputs followed by an output and reject anything else" in {
    ExportConfig.parse(List("in-dir", "out")).isRight shouldBe true
    val config = ExportConfig.parse(List("--root", "r", "a", "b.mid", "--list", "l.txt", "out.jsonl")).toOption.get
    config.inputs.map(_.toString) shouldBe List("a", "b.mid")
    config.output.toString shouldBe "out.jsonl"
    config.root.map(_.toString) shouldBe Some("r")
    config.listFile.map(_.toString) shouldBe Some("l.txt")

    ExportConfig.parse(Nil).isLeft shouldBe true
    ExportConfig.parse(List("only-one")).isLeft shouldBe true
    ExportConfig.parse(List("--list", "l.txt", "out")).isRight shouldBe true
    ExportConfig.parse(List("--bogus", "a", "b")).isLeft shouldBe true
    ExportConfig.parse(List("a", "--root")).isLeft shouldBe true
  }
}
