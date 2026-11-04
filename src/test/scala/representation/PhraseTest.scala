package representation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import representation.NoteName._
import training.segmentation.PhraseSegmenter

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.SECONDS

class PhraseTest extends AnyFlatSpec with Matchers {

  "Notes across the split time" should "be split among the two resulting phrases" in {
    val p = Phrase()
      .withMusicalElements(
        Phrase().withMusicalElements(Note(A).withStartTime(0, SECONDS).withDuration(1, SECONDS),
                                     Note(D).withStartTime(1, SECONDS).withDuration(2, SECONDS),
                                     Note(NoteName.F).withStartTime(3, SECONDS).withDuration(1, SECONDS)),
        Phrase().withMusicalElements(Note(B).withStartTime(0, SECONDS).withDuration(2, SECONDS),
                                     Note(E).withStartTime(2, SECONDS).withDuration(2, SECONDS)),
        Phrase().withMusicalElements(Note(C).withStartTime(0, SECONDS).withDuration(4, SECONDS))
      )
      .withPolyphony(polyphony = true).get
    val (p1, p2) = Phrase.split(p, SECONDS.toNanos(1))
    val bSplitP1 = p1.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]
    val bSplitP2 = p2.get.musicalElements(1).asInstanceOf[Phrase].musicalElements.head.asInstanceOf[Note]

    assert(bSplitP1.name.equals(B))
    assert(bSplitP1.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP1.getStartTime(SECONDS) == BigInt(0))

    assert(bSplitP2.name.equals(B))
    assert(bSplitP2.getDuration(SECONDS) == BigInt(1))
    assert(bSplitP2.getStartTime(SECONDS) == BigInt(0))
  }

  private def noteAt(pitch: Int, startSeconds: Int, seconds: Int): Note =
    Note(midiPitch = pitch, startTimeNS = SECONDS.toNanos(startSeconds), durationNS = SECONDS.toNanos(seconds))

  /** Two voices: 72 then 74 for a second each, and 48 for two seconds, all at the given tempo. */
  private def twoVoices(tempoBPM: Double): Phrase = {
    val high = new Phrase(List(noteAt(72, 0, 1), noteAt(74, 1, 1)), tempoBPM = tempoBPM)
    val low = new Phrase(List(noteAt(48, 0, 2)), tempoBPM = tempoBPM)
    new Phrase(List(high, low), polyphony = true, tempoBPM = tempoBPM)
  }

  "Merging the voices of a phrase" should "keep the tempo of the phrase (#34)" in {
    val merged = Phrase.mergePhrases(twoVoices(90))

    merged.tempoBPM shouldBe 90.0
    merged.polyphony shouldBe false
  }

  it should "give the same notes whatever the tempo: the times are nanoseconds" in {
    for (tempo <- List(60.0, 90.0, 120.0, 200.0)) {
      val merged = Phrase.mergePhrases(twoVoices(tempo))
      val chords = merged.musicalElements.collect { case chord: Chord => chord }

      withClue(s"at $tempo BPM: ") {
        chords should have size 2
        merged.musicalElements should have size 2
        chords.map(c => (c.getStartTimeNS, c.getDurationNS, c.notes.map(_.midiPitch).sorted)) shouldBe List(
          (BigInt(0), BigInt(SECONDS.toNanos(1)), List(48, 72)),
          (BigInt(SECONDS.toNanos(1)), BigInt(SECONDS.toNanos(1)), List(48, 74)))
        merged.getDurationNS shouldBe BigInt(SECONDS.toNanos(2))
      }
    }
  }

  it should "return a phrase with one voice as it is" in {
    val melody = new Phrase(List(noteAt(60, 0, 1)), tempoBPM = 75)

    Phrase.mergePhrases(melody) should be theSameInstanceAs melody
    Phrase.mergePhrases(melody).tempoBPM shouldBe 75.0
  }

  it should "give a list of phrases the tempo of the first one, and the default tempo when there is none" in {
    val voices = twoVoices(90).musicalElements.asInstanceOf[List[Phrase]]

    Phrase.mergePhrases(voices).tempoBPM shouldBe 90.0
    Phrase.mergePhrases(List[Phrase]()).tempoBPM shouldBe MusicalElement.DEFAULT_TEMPO_BPM
  }

  it should "not change the note times when the voices have different tempos: they are nanoseconds" in {
    val fast = new Phrase(List(noteAt(72, 0, 1), noteAt(74, 1, 1)), tempoBPM = 150)
    val slow = new Phrase(List(noteAt(48, 0, 2)), tempoBPM = 90)

    val merged = Phrase.mergePhrases(List(fast, slow))

    merged.tempoBPM shouldBe 150.0
    Phrase.mergePhrases(List(slow, fast)).tempoBPM shouldBe 90.0
    merged.getDurationNS shouldBe BigInt(SECONDS.toNanos(2))
    merged.musicalElements should have size 2
  }

  // A note that starts on a cut starts a few nanoseconds before it when jMusic's rounding noise has its say (#33)
  private val cut: BigInt = SECONDS.toNanos(4)
  private val second: BigInt = SECONDS.toNanos(1)

  private def noteNS(pitch: Int, startNS: BigInt, durationNS: BigInt): Note =
    Note(midiPitch = pitch, startTimeNS = startNS, durationNS = durationNS)

  private def pitchesAndTimes(phrase: Phrase): List[(Int, BigInt, BigInt)] =
    phrase.musicalElements.collect { case n: Note => (n.midiPitch, n.getStartTimeNS, n.getDurationNS) }

  "Cutting a phrase where a note starts a nanosecond early" should "leave the note whole on the right and no sliver on the left" in {
    val voice = new Phrase(List(
      noteNS(60, 0, cut - 1), // ends where the next note starts, 1 ns before the cut
      noteNS(61, cut - 1, second),
      noteNS(62, cut - 1 + second, second)))

    val (left, right) = Phrase.split(voice, cut)

    pitchesAndTimes(left.get) shouldBe List((60, BigInt(0), cut - 1))
    // the notes of the right side are the notes of the source, unchanged and in the same places relative to each other
    pitchesAndTimes(right.get) shouldBe List((61, BigInt(0), second), (62, second, second))
  }

  it should "give a note that ends a nanosecond after the cut whole to the left, not a sliver to the right" in {
    val voice = new Phrase(List(noteNS(60, second * 3, second + 1), noteNS(61, cut + 1, second)))

    val (left, right) = Phrase.split(voice, cut)

    pitchesAndTimes(left.get) shouldBe List((60, BigInt(0), second + 1))
    pitchesAndTimes(right.get) shouldBe List((61, BigInt(0), second))
  }

  it should "cut every voice of a polyphonic phrase the same way" in {
    val early = new Phrase(List(noteNS(60, 0, cut - 1), noteNS(61, cut - 2, second * 2)))
    val clean = new Phrase(List(noteNS(36, 0, second * 3), noteNS(37, second * 3, second * 3)))
    val phrase = new Phrase(List(early, clean), polyphony = true)

    val (left, right) = Phrase.split(phrase, cut)

    val leftVoices = left.get.musicalElements.asInstanceOf[List[Phrase]]
    val rightVoices = right.get.musicalElements.asInstanceOf[List[Phrase]]
    pitchesAndTimes(leftVoices(0)) shouldBe List((60, BigInt(0), cut - 1))
    pitchesAndTimes(rightVoices(0)) shouldBe List((61, BigInt(0), second * 2))
    // a note that the cut really goes through is cut as before
    pitchesAndTimes(leftVoices(1)) shouldBe List((36, BigInt(0), second * 3), (37, second * 3, second))
    pitchesAndTimes(rightVoices(1)) shouldBe List((37, BigInt(0), second * 2))
  }

  it should "leave no note shorter than a microsecond in any piece of a phrase that is cut every 4 seconds" in {
    // 1 second notes, each starting 1 ns before the second it belongs to
    val notes = (0 until 16).map { i =>
      if (i == 0) noteNS(40, 0, second - 1) else noteNS(40 + i, second * i - 1, second)
    }

    val pieces = training.segmentation.PhraseSegmenter.getDefault(cut).segment(new Phrase(notes.toList))

    pieces.map(pitchesAndTimes(_).map(_._1)) shouldBe List(40 to 43, 44 to 47, 48 to 51).map(_.toList)
    pieces.flatMap(pitchesAndTimes(_).map(_._3)).forall(_ >= 1000) shouldBe true
  }

  // The key signature (#36): a copy of a phrase, a piece of it, a voice or a merged phrase keeps it
  private val gMinor = KeySignature(flats = 2, sharps = 0, KeySignature.Minor)
  private val aMajor = KeySignature(flats = 0, sharps = 3, KeySignature.Major)

  private def keyedVoice(key: KeySignature, pitches: Int*): Phrase =
    new Phrase(pitches.zipWithIndex.map { case (pitch, i) => noteAt(pitch, i, 1) }.toList, keySignature = key)

  private def voicesOf(phrase: Phrase): List[Phrase] = phrase.musicalElements.asInstanceOf[List[Phrase]]

  "A copy of a phrase" should "keep the key signature" in {
    val phrase = keyedVoice(gMinor, 60, 62, 64, 65)
    val note = noteAt(67, 0, 1)

    phrase.withMusicalElements(List(note)).keySignature shouldBe gMinor
    phrase.withMusicalElements(note, note).keySignature shouldBe gMinor
    phrase.withStartTime(SECONDS.toNanos(7)).keySignature shouldBe gMinor
    phrase.withDuration(SECONDS.toNanos(8)).keySignature shouldBe gMinor
    phrase.scaled(BigDecimal(2), java.util.concurrent.TimeUnit.NANOSECONDS).keySignature shouldBe gMinor
    new Phrase(List(phrase), keySignature = gMinor).withPolyphony().get.keySignature shouldBe gMinor
    new Phrase(List(phrase), polyphony = true, keySignature = gMinor).withPolyphony(false).get.keySignature shouldBe gMinor
  }

  it should "keep the tempo and the key signature together" in {
    val phrase = new Phrase(List(noteAt(60, 0, 1)), tempoBPM = 90, keySignature = aMajor)

    val copy = phrase.withStartTime(SECONDS.toNanos(3))

    copy.tempoBPM shouldBe 90.0
    copy.keySignature shouldBe aMajor
  }

  it should "move every voice of a polyphonic phrase with the key signature of the voice" in {
    val phrase = new Phrase(List(keyedVoice(aMajor, 60, 62), keyedVoice(aMajor, 40)), polyphony = true, keySignature = aMajor)

    val moved = phrase.withStartTime(SECONDS.toNanos(5))

    moved.keySignature shouldBe aMajor
    voicesOf(moved).map(_.keySignature) shouldBe List(aMajor, aMajor)
  }

  "A phrase put in another key" should "carry the key on every voice, and keep its notes and tempo" in {
    val phrase = new Phrase(List(keyedVoice(gMinor, 60, 62), keyedVoice(gMinor, 40)), polyphony = true, tempoBPM = 90,
      keySignature = gMinor)
    val moved = phrase.withKeySignature(aMajor)

    moved.keySignature shouldBe aMajor
    moved.tempoBPM shouldBe 90.0
    voicesOf(moved).map(_.keySignature) shouldBe List(aMajor, aMajor)
    voicesOf(moved).map(_.musicalElements) shouldBe voicesOf(phrase).map(_.musicalElements)
    keyedVoice(gMinor, 60).withKeySignature(aMajor).keySignature shouldBe aMajor
  }

  "A segmenter" should "place every piece where it starts in the phrase it cuts" in {
    // 12 one-second notes cut every 4 seconds: pieces at 0, 4 and 8 seconds (the stretch after the last cut is dropped)
    val melody = new Phrase((0 until 12).map(i => noteAt(60 + i, i, 1)).toList)
    val segmenter = PhraseSegmenter.getDefault(SECONDS.toNanos(4))
    val pieces = segmenter.segmentWithOffsets(melody)

    pieces.map(_._1) shouldBe List(0L, 4L, 8L).map(s => BigInt(SECONDS.toNanos(s)))
    pieces.map(_._2) shouldBe segmenter.segment(melody)
    pieces.map(_._2.musicalElements.head.asInstanceOf[Note].midiPitch) shouldBe List(60, 64, 68)
    // a phrase too short to be cut is one piece at 0
    PhraseSegmenter.getDefault(SECONDS.toNanos(20)).segmentWithOffsets(melody) shouldBe List((BigInt(0), melody))
  }

  "Cutting a phrase" should "give both pieces the key signature of the phrase" in {
    val (left, right) = Phrase.split(keyedVoice(gMinor, 60, 62, 64, 65), SECONDS.toNanos(2))

    left.get.keySignature shouldBe gMinor
    right.get.keySignature shouldBe gMinor
  }

  it should "give both pieces and each of their voices the key signature of a polyphonic phrase" in {
    val phrase = new Phrase(
      List(keyedVoice(aMajor, 60, 62, 64, 65), keyedVoice(aMajor, 36, 38)),
      polyphony = true,
      keySignature = aMajor)

    val (left, right) = Phrase.split(phrase, SECONDS.toNanos(2))

    List(left.get, right.get).foreach { piece =>
      piece.keySignature shouldBe aMajor
      voicesOf(piece).map(_.keySignature).toSet shouldBe Set(aMajor)
    }
  }

  it should "give every piece the key signature when a segmenter cuts a phrase into many" in {
    val phrase = keyedVoice(gMinor, (60 until 76)*)

    val pieces = training.segmentation.PhraseSegmenter.getDefault(SECONDS.toNanos(4)).segment(phrase)

    pieces should have size 4
    pieces.map(_.keySignature).toSet shouldBe Set(gMinor)
  }

  "A polyphonic phrase built from voices" should "give the voices and the phrase the key signature" in {
    val voices = List(
      List[MusicalElement](noteAt(60, 0, 1), noteAt(62, 1, 1)),
      List[MusicalElement](noteAt(40, 0, 2)))

    val phrase = Phrase.createPolyphonicPhrase(voices, 100, gMinor).get

    phrase.polyphony shouldBe true
    phrase.keySignature shouldBe gMinor
    voicesOf(phrase).map(_.keySignature) shouldBe List(gMinor, gMinor)
    phrase.tempoBPM shouldBe 100.0
  }

  it should "give a single voice the key signature, and nothing when there is no voice" in {
    val single = Phrase.createPolyphonicPhrase(List(List[MusicalElement](noteAt(60, 0, 1))), 100, aMajor).get

    single.polyphony shouldBe false
    single.keySignature shouldBe aMajor
    single.tempoBPM shouldBe 100.0
    Phrase.createPolyphonicPhrase(List(Nil, Nil), 100, aMajor) shouldBe None
  }

  "Merging the voices of a phrase" should "keep the key signature of the phrase" in {
    val phrase = new Phrase(
      List(keyedVoice(gMinor, 72, 74), keyedVoice(gMinor, 48)),
      polyphony = true,
      tempoBPM = 90,
      keySignature = gMinor)

    val merged = Phrase.mergePhrases(phrase)

    merged.keySignature shouldBe gMinor
    merged.tempoBPM shouldBe 90.0
    merged.polyphony shouldBe false
  }

  it should "give a list of phrases the key signature of the first one, and the default one when there is none" in {
    val voices = List(keyedVoice(aMajor, 72, 74), keyedVoice(gMinor, 48))

    Phrase.mergePhrases(voices).keySignature shouldBe aMajor
    Phrase.mergePhrases(voices.reverse).keySignature shouldBe gMinor
    Phrase.mergePhrases(List[Phrase]()).keySignature shouldBe Phrase.DEFAULT_KEY_SIGNATURE
  }

  it should "return a phrase with one voice as it is, with its key signature" in {
    val melody = keyedVoice(aMajor, 60, 62)

    Phrase.mergePhrases(melody) should be theSameInstanceAs melody
    Phrase.mergePhrases(melody).keySignature shouldBe aMajor
  }

  // A gap between two elements of a microsecond or less is jMusic's rounding noise, not a rest (#39)
  private def addAll(elements: MusicalElement*): List[MusicalElement] = {
    val phraseElements = ListBuffer[MusicalElement]()
    elements.foreach(Phrase.addToPhraseElements(_, phraseElements))
    phraseElements.toList
  }

  // two notes in a row with the same name, accidental and octave are one note for addToPhraseElements, so give the notes names
  private def named(name: NoteName, startNS: BigInt, durationNS: BigInt): Note =
    Note(name = name, startTimeNS = startNS, durationNS = durationNS)

  "A gap of up to a microsecond between two notes" should "not become a rest, and leave both notes where they are" in {
    for (gap <- List(1L, 2L, 3L, 99L, 500L, 999L, 1000L)) {
      val first = named(C, 0, second)
      val next = named(D, second + gap, second)

      withClue(s"a gap of $gap ns: ") { addAll(first, next) shouldBe List(first, next) }
    }
  }

  it should "not become a rest after a rest, a chord or a note of the same pitch either" in {
    val note = named(C, 0, second)
    val chord = Chord.fromNotes(named(C, 0, second), named(E, 0, second))
    val rest = Rest(durationNS = second)
    val later = named(D, second + 5, second)

    addAll(rest, later) shouldBe List(rest, later)
    addAll(chord, later) shouldBe List(chord, later)
    addAll(note, later) shouldBe List(note, later)
    // two equal notes in a row are still one note
    addAll(note, named(C, second + 5, second)) shouldBe List(note.withDuration(second * 2))
  }

  "A gap of more than a microsecond" should "still be a rest that starts where the note ends" in {
    for (gap <- List(1001L, 1000000L, SECONDS.toNanos(1))) {
      val first = named(C, 0, second)
      val next = named(D, second + gap, second)

      val elements = addAll(first, next)

      withClue(s"a gap of $gap ns: ") {
        elements should have size 3
        elements.head shouldBe first
        elements(1) shouldBe a[Rest]
        elements(1).getStartTimeNS shouldBe second
        elements(1).getDurationNS shouldBe BigInt(gap - 1) // up to the nanosecond before the next note, as it always was
        elements(2) shouldBe next
      }
    }
  }

  "The first element of a phrase" should "get a rest before it only when it starts more than a microsecond late" in {
    for (start <- List(0L, 1L, 3L, 999L, 1000L)) {
      val note = named(C, start, second)

      withClue(s"starting at $start ns: ") { addAll(note) shouldBe List(note) }
    }
    for (start <- List(1001L, SECONDS.toNanos(2))) {
      val note = named(C, start, second)

      addAll(note) shouldBe List(Rest(durationNS = start), note)
    }
  }

  "Merging voices whose notes are a few nanoseconds apart" should "leave no rest between the chords" in {
    val high = new Phrase(List(named(A, 0, second)))
    val low = new Phrase(List(named(D, second + 5, second)))

    val merged = Phrase.mergePhrases(List(high, low))

    merged.musicalElements.collect { case n: Note => n.name } shouldBe List(A, D)
    merged.musicalElements.exists(_.isInstanceOf[Rest]) shouldBe false
    // and a gap that is a pause is still one
    val later = new Phrase(List(named(D, second + 1000000, second)))
    Phrase.mergePhrases(List(high, later)).musicalElements.exists(_.isInstanceOf[Rest]) shouldBe true
  }
}
