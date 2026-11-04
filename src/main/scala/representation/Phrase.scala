package representation

import java.util.concurrent.TimeUnit

import representation.KeySignature.CMaj

import utils.ImplicitConversions.{toEnhancedIterable,toFasterMutableList}
import utils.collections.CollectionUtils
import utils.functional.{FunctionalUtils, MemoizedFunc}

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.concurrent.duration.TimeUnit
import scala.concurrent.duration.{NANOSECONDS, TimeUnit}
import scala.language.postfixOps
import scala.math
import scala.util.Try

object Phrase {
  val DEFAULT_START_TIME = 0.0
  val DEFAULT_KEY_SIGNATURE: KeySignature = CMaj.keySignature

  /**
   * How long a gap between two elements has to be to be a rest: 1 microsecond. jMusic's times are doubles that
   * [[MusicalElement.fromBPM]] truncates to whole nanoseconds, so two notes that touch in a MIDI file come out a few
   * nanoseconds apart (in the 76 training files such gaps are mostly 1 or 2 ns and at most 99 ns), while the shortest
   * real rest or note there is 0.8 milliseconds (about one MIDI tick), 800 times more. [[addToPhraseElements]] is the only
   * place that decides whether a gap is a rest (#39).
   */
  val GAP_TOLERANCE_NS: BigInt = 1000

  def apply(): Phrase = {
    new Phrase()
  }

  def createPolyphonicPhrase(
    musicalElements: List[Iterable[MusicalElement]],
    tempoBPM: Double,
    keySignature: KeySignature): Option[Phrase] = {
    val phrases = musicalElements
      .withFilter(_.nonEmpty)
      .map(new Phrase(tempoBPM = tempoBPM, keySignature = keySignature).withMusicalElements)

    phrases match {
      case Nil =>
        None
      case phrase :: Nil =>
        Some(phrase)
      case phraseList =>
        Some(new Phrase(
          musicalElements = phraseList.toList,
          polyphony = true,
          tempoBPM = tempoBPM,
          keySignature = keySignature))
    }
  }

  def computeDuration(phrase: Phrase, timeUnit: TimeUnit): BigInt = {
    if (phrase.polyphony)
      phrase.map(p => computeDuration(p.asInstanceOf[Phrase], timeUnit)).max
    else {
      val duration: BigInt = phrase.sumBy(0, _.getDuration(timeUnit))
      duration
    }
  }

  def computeStartTime(phrase: Phrase, timeUnit: TimeUnit): BigInt =
    phrase.minBy(_.getStartTime(timeUnit)).getStartTime(timeUnit)

  def computeMelodicIntervals(phrase: Phrase): List[Double] = {
    require(! phrase.polyphony)
    var prevNote: Option[Note] = None
    val intervals = ListBuffer[Double]()
    for (elem <- phrase) {
      elem match {
        case note: Note =>
          if (prevNote.isDefined) {
            intervals += note.midiPitch - prevNote.get.midiPitch
          }
          prevNote = Some(note)
        case _ =>
      }
    }
    intervals.toList
  }

  def computePitchHistogram(phrase: Phrase): Array[Double] = {
    require(! phrase.polyphony)
    val histogram = Array.fill[Double](Note.MAX_MIDI_PITCH + 1)(0)
    for (elem <- phrase) {
      elem match {
        case note: Note =>
          histogram(note.midiPitch) += 1
        case _ =>
      }
    }
    histogram
  }

  def computeStartTimeIntervals(phrase: Phrase): Array[Double] = {
    require(! phrase.polyphony)
    val timesBetweenStartTimes = ListBuffer[Double]()
    var prevElem: Option[MusicalElement] = None
    for (elem <- phrase if ! elem.isInstanceOf[Rest]) {
      if (prevElem.isDefined) {
        timesBetweenStartTimes += elem.getStartTimeBPM(phrase.tempoBPM).toDouble -
          prevElem.get.getStartTimeBPM(phrase.tempoBPM).toDouble
      }
      prevElem = Some(elem)
    }
    timesBetweenStartTimes.toArray
  }


  def computeMaxChordSize(phrase: Phrase): Int = {
    phrase.musicalElements.foldLeft(1){
      case (i, c: Chord) => math.max(i, c.elements.size)
      case (i, _) => i
    }
  }

  def allRest(phrase: Phrase): Boolean = {
    if (phrase.polyphony) {
      phrase.musicalElements.asInstanceOf[List[Phrase]].forall(allRest)
    } else {
      phrase.musicalElements.forall(_.isInstanceOf[Rest])
    }
  }

  def unmerge(phrase: Phrase): Phrase = {
    def elemMatch(e1: Option[MusicalElement], e2: MusicalElement): Boolean = e1.exists{ e11 =>
      (e11, e2) match {
        case (n1: Note, n2: Note) =>
          Note.areEqual(n1, n2, duration = false)
        case (r1: Rest, r2: Rest) =>
          true
        case _ =>
          false
      }
    }

    var activeElements = List[MusicalElement]()
    val phrasesElements: List[ListBuffer[MusicalElement]] =
      (0 until phrase.getMaxChordSize).map(i => ListBuffer[MusicalElement]()).toList
    val musicalElements: List[MusicalElement] = phrase.musicalElements.sortBy(_.getStartTimeNS)

    for (musicalElement <- musicalElements) {
      musicalElement match {
        case chord: Chord =>
          activeElements = chord.elements
        case elem =>
          activeElements = List(elem)
      }

      val rest = Rest(
        durationNS = musicalElement.getDurationNS,
        startTimeNS = musicalElement.getStartTimeNS)

      val availablePhrases = mutable.HashSet[Int](phrasesElements.indices.toArray*)

      activeElements ++= (1 to phrasesElements.size - activeElements.size).map(i => rest)

      activeElements.to(LazyList).foreach{ element =>
        val listIdx = Option(phrasesElements.indexWhere(l => elemMatch(l.lastOption, element)))
          .filter(_ >= 0)
          .getOrElse(CollectionUtils.chooseRandom(availablePhrases).get)

        addToPhraseElements(element, phrasesElements(listIdx))
        availablePhrases.remove(listIdx)
      }
    }

    val phrases = phrasesElements.map(phraseElems => new Phrase(phraseElems.toList))
    Phrase()
      .withMusicalElements(phrases)
      .withPolyphony()
      .getOrElse(phrase)
  }

  val getNotesByStartTimeNS = FunctionalUtils.memoized { (phrase: Phrase) =>
    phrase.musicalElements.groupByMultiMap[BigInt](_.getStartTime(NANOSECONDS))
  }

  /**
   * @param musicalElements Iterable of stuff that either contains, is or can be converted to a jmData.Note
   * @param endTime Time at which all notes terminate
   * @return An optional musical element
   */
  def mergeNotes(
    musicalElements: List[MusicalElement],
    endTime: BigInt,
    timeUnit: TimeUnit = NANOSECONDS): Option[MusicalElement] = {
    def computeDuration(em: MusicalElement) = timeUnit.toNanos(endTime.toLong) - em.getStartTimeNS
    musicalElements match {
      case Nil =>
        None
      case element :: Nil =>
        Some(element.withDuration(computeDuration(element)))
      case elements =>
        Some(Chord(elements.map(elem => elem.withDuration(computeDuration(elem)))))
    }
  }

  /**
   * A polyphonic phrase as one monophonic phrase (chords where voices overlap); any other phrase is returned as it is.
   * The result has the tempo and the key signature of `phrase`.
   */
  def mergePhrases(phrase: Phrase): Phrase =
    if (phrase.polyphony) merge(phrase.musicalElements.asInstanceOf[List[Phrase]], phrase.tempoBPM, phrase.keySignature)
    else phrase

  /**
   * The phrases played together as one monophonic phrase. The note times are nanoseconds, so they are merged as they
   * are; the result is labelled with the tempo and the key signature of the first phrase (they all have the same ones
   * when they are the voices of one polyphonic phrase), and with the default tempo and key when there are none.
   */
  def mergePhrases(phrases: Iterable[Phrase]): Phrase =
    merge(
      phrases,
      phrases.headOption.map(_.tempoBPM).getOrElse(MusicalElement.DEFAULT_TEMPO_BPM),
      phrases.headOption.map(_.keySignature).getOrElse(DEFAULT_KEY_SIGNATURE))

  private def merge(phrases: Iterable[Phrase], tempoBPM: Double, keySignature: KeySignature): Phrase = {
    val notesByStartTime = CollectionUtils.mergeMultiMaps(phrases.toList*)(using getNotesByStartTimeNS)
    val phraseElements = ListBuffer[MusicalElement]()
    var activeNotes: List[MusicalElement] = List()
    val endTimes = notesByStartTime.flatMap { case (startTime, notes) =>
      notes.map(note => startTime + note.getDurationNS)
    }
    val times = notesByStartTime.keySet.toList.++(endTimes).distinct.sorted

    for (time <- times) {
      mergeNotes(activeNotes, time)
        .filter(_.getDuration(TimeUnit.MILLISECONDS) > 1) // Filter our spurious notes
        .foreach(addToPhraseElements(_, phraseElements))

      activeNotes = activeNotes.flatMap(MusicalElement.resizeIfActive(time, _))
      activeNotes ++= notesByStartTime.getOrElse(time, Set()).toList
    }

    require(activeNotes.isEmpty, "All active notes must be consumed")
    new Phrase(tempoBPM = tempoBPM, keySignature = keySignature).withMusicalElements(phraseElements)
  }

  /**
   * Adds `element` at the end of `phraseElements`. A gap of more than [[GAP_TOLERANCE_NS]] between the end of the last
   * element (or the start of the phrase) and the start of `element` is filled with a rest; a shorter gap is rounding noise
   * and is left as it is: no rest, and `element` keeps its start time (#39). Two equal notes in a row become one note, two
   * rests in a row one rest.
   */
  def addToPhraseElements(element: MusicalElement, phraseElements: ListBuffer[MusicalElement]): Unit = {
    if (phraseElements.isEmpty) {
      if (element.getStartTimeNS > GAP_TOLERANCE_NS) {
        phraseElements += new Rest(durationNS = element.getStartTimeNS)
      }
      phraseElements += element
    } else {
      val previousElem = phraseElements.last
      (previousElem, element) match {
        case (previousNote: Note, note: Note) if Note.areEqual(note, previousNote, duration = false) =>
          phraseElements.updateLast(previousNote.withDuration(previousNote.durationNS + note.durationNS))
        case (previousRest: Rest, rest: Rest) =>
          phraseElements.updateLast(previousRest.withDuration(previousRest.durationNS + rest.durationNS))
        case _ =>
          if (element.getStartTimeNS > phraseElements.last.getEndTimeNS + GAP_TOLERANCE_NS) {
            phraseElements += new Rest(startTimeNS = phraseElements.last.getEndTimeNS)
              .withEndTime(element.getStartTimeNS - 1)
          }
          phraseElements += element
      }
    }
  }


  /**
   * Cuts a phrase (each voice of a polyphonic one) in two at `splitTimeNS`. An element that the cut would divide into
   * pieces of which one is shorter than [[MusicalElement.SPLIT_TOLERANCE_NS]] is not divided: it goes whole to the
   * side where most of it lies (see [[MusicalElement.split]], #33).
   */
  def split(phrase: Phrase, splitTimeNS: BigInt): (Option[Phrase], Option[Phrase]) = phrase match {
    case p@Phrase(_, true, _, _) =>
      splitPolyphonic(p, splitTimeNS)
    case p@Phrase(_, false, _, _) =>
      splitNonPolyphonic(p, splitTimeNS)
  }

  def splitPolyphonic(phrase: Phrase, splitTimeNS: BigInt): (Option[Phrase], Option[Phrase]) = {
    require(phrase.polyphony)
    val left = ListBuffer[Phrase]()
    val right = ListBuffer[Phrase]()

    for (curPhrase <- phrase) {
      val (lPhrase, rPhrase) = split(curPhrase.asInstanceOf[Phrase], splitTimeNS)
      lPhrase.foreach(left.+=)
      rPhrase.foreach(right.+=)
    }

    (Option.when(left.nonEmpty)(phrase.withMusicalElements(left)),
      Option.when(right.nonEmpty)(phrase.withMusicalElements(right)))
  }

  def splitNonPolyphonic(phrase: Phrase, splitTimeNS: BigInt): (Option[Phrase], Option[Phrase]) = {
    require(! phrase.polyphony && phrase.nonEmpty)
    var curTimeNS = phrase.getStartTimeNS
    val leftElements = ListBuffer[MusicalElement]()
    val rightElements = ListBuffer[MusicalElement]()

    for (elem <- phrase) {
      val newTimeNS = curTimeNS + elem.getDurationNS

      if (curTimeNS <= splitTimeNS) {
        if (newTimeNS > splitTimeNS) {
          val (left, right) = MusicalElement.split(elem, splitTimeNS)
          left.foreach(leftElements.+=)
          right.foreach(rightElements.+=)
        } else {
          leftElements += elem
        }
      } else {
        rightElements += elem
      }

      curTimeNS = newTimeNS
    }

    (Option.when(leftElements.nonEmpty)(phrase.withMusicalElements(leftElements).withStartTime(0)),
     Option.when(rightElements.nonEmpty)(phrase.withMusicalElements(rightElements).withStartTime(0)))
  }

  /**
   * If it is a polyphonic phrase, it returns the sub-phrase with more elements. Otherwise, it returns the provided
   * phrase
   * @param phrase
   */
  def getLongestSubPhrase(phrase: Phrase, constraints: List[Phrase => Boolean]) = {
    if (phrase.polyphony)
      phrase.musicalElements.asInstanceOf[List[Phrase]].filter(constraints).maxBy(_.size)
    else
      phrase
  }
}

/**
 *
 * @param musicalElements List of musical elements that belong to the phrase
 * @param polyphony All the phrases in this phrases are to be played at the same time
 *                  (Pre: all the musical elements in the phrase must be phrases)
 * @param tempoBPM Tempo of the phrase
 * @param keySignature Key signature of the phrase. Every copy of a phrase (`withMusicalElements`, `withStartTime`,
 *                     `withPolyphony`, `scaled`, and so the pieces that `Phrase.split` cuts) keeps it.
 */
case class Phrase(
  musicalElements: List[MusicalElement] = List(),
  polyphony: Boolean = false,
  tempoBPM: Double = MusicalElement.DEFAULT_TEMPO_BPM,
  keySignature: KeySignature = Phrase.DEFAULT_KEY_SIGNATURE)
    extends MusicalElement with Iterable[MusicalElement] {
  // Memoizing closures are lambdas on Scala 2.13; keep them out of reflective (MapDB/Jackson) serialisation
  @transient private lazy val maxChordSize: MemoizedFunc[Phrase, Int] =
    FunctionalUtils.memoized(Phrase.computeMaxChordSize)

  @transient private lazy val duration: MemoizedFunc[(Phrase, TimeUnit), BigInt] =
    FunctionalUtils.memoized((Phrase.computeDuration).tupled)

  @transient private lazy val startTime: MemoizedFunc[(Phrase, TimeUnit), BigInt] =
    FunctionalUtils.memoized((Phrase.computeStartTime).tupled)

  require(! polyphony || canHavePolyphony)


  private def canHavePolyphony: Boolean =
    musicalElements.forall{ case p: Phrase => true; case _ => false }

  def withPolyphony(polyphony: Boolean = true): Option[Phrase] =
    Option.when(! polyphony || canHavePolyphony)(myCopy(polyphony = polyphony))

  /** The phrase in `keySignature`, and every voice of a polyphonic phrase with it. */
  def withKeySignature(keySignature: KeySignature): Phrase =
    if (polyphony)
      myCopy(
        musicalElements = musicalElements.map { case voice: Phrase => voice.withKeySignature(keySignature); case e => e },
        keySignature = keySignature)
    else myCopy(keySignature = keySignature)

  def withMusicalElements(musicalElements: Iterable[MusicalElement]) =
    myCopy(musicalElements = musicalElements.toList)

  def withMusicalElements(musicalElements: MusicalElement*) =
    myCopy(musicalElements = musicalElements.toList)

  override def withDuration(newDuration: BigInt, timeUnit: TimeUnit): Phrase =
    scaled(BigDecimal(newDuration / getDuration(timeUnit)), timeUnit)

  /**
   * Shifts all the elements within the phrase so that the first one starts at the new start time
   * and it preserves the gaps between the start times
   * @param startTime New start time for the phrase
   * @param timeUnit Time unit of the new start time
   * @return A phrase with all its elements shifted
   */
  override def withStartTime(startTime: BigInt, timeUnit: TimeUnit): Phrase = {
    val shiftedMusicalElements = getShiftedElements(startTime, timeUnit)
    myCopy(musicalElements = shiftedMusicalElements)
  }

  private def getShiftedElements(startTime: BigInt, timeUnit: TimeUnit): List[MusicalElement] = {
    if (polyphony) {
      return musicalElements.map(_.withStartTime(startTime, timeUnit))
    }

    val shiftedMusicalElements = ListBuffer[MusicalElement]()
    var curTimeNS: BigInt = timeUnit.toNanos(startTime.toLong)
    for (idx <- musicalElements.indices.dropRight(1)) {
      val startTimeDeltaNS = musicalElements(idx + 1).getStartTimeNS - musicalElements(idx).getStartTimeNS
      shiftedMusicalElements += musicalElements(idx).withStartTime(curTimeNS, NANOSECONDS)
      curTimeNS += startTimeDeltaNS
    }
    shiftedMusicalElements += musicalElements.last.withStartTime(curTimeNS, NANOSECONDS)
    shiftedMusicalElements.toList
  }

  def getMaxChordSize: Int = maxChordSize(this)

  def scaled(durationRatio: BigDecimal, timeUnit: TimeUnit): Phrase = {
    val scaledElems = musicalElements.map { elem =>
      elem.withDuration((BigDecimal(elem.getDuration(timeUnit)) * durationRatio).toBigInt, timeUnit)
    }

    myCopy(musicalElements = scaledElems)
  }

  override def getDuration(timeUnit: TimeUnit): BigInt = duration((this, timeUnit))
  override def iterator: Iterator[MusicalElement] = musicalElements.iterator

  override protected def className: String = "Phrase"

  override def foreach[U](f: (MusicalElement) => U): Unit = musicalElements.foreach(f)
  override def getStartTime(timeUnit: TimeUnit): BigInt = startTime((this, timeUnit))

  private def myCopy(
      musicalElements: List[MusicalElement] = musicalElements,
      polyphony: Boolean = polyphony,
      tempoBPM: Double = tempoBPM,
      keySignature: KeySignature = keySignature): Phrase = {
    new Phrase(musicalElements = musicalElements,
               polyphony = polyphony,
               tempoBPM = tempoBPM,
               keySignature = keySignature)
  }
}
