package instruments

import com.fasterxml.jackson.annotation.{JsonProperty, JsonTypeInfo}
import utils.collections.CollectionUtils

import scala.language.implicitConversions
import scala.reflect.ClassTag

object InstrumentType {
  type InstrumentType = InstrumentCategory

  implicit def toInstrumentNumber(instrumentType: InstrumentType): Int = instrumentType.instrumentNumber

  sealed abstract class InstrumentTypeObject[T <: InstrumentCategory](implicit m: ClassTag[T]) {
    val range: Range

    def apply(): InstrumentType = {
      val instrumentNumber = Integer.valueOf(CollectionUtils.chooseRandom(range).getOrElse(1))
      m.runtimeClass.getConstructors()(0).newInstance(instrumentNumber).asInstanceOf[InstrumentType]
    }
  }

  def availableInstruments = List(
    PIANO(), CHROMATIC_PERCUSSION(), ORGAN(), GUITAR(), BASS(), STRINGS(), ENSEMBLE(), BRASS(),
    PIPE(), SYNTH_LEAD(), SYNTH_PAD(), SYNTH_EFFECTS(), ETHNIC(), PERCUSSIVE(), SOUND_EFFECTS())

  def randomInstrument = CollectionUtils.chooseRandom(availableInstruments).get

  object PIANO extends InstrumentTypeObject[PIANO] {
    val range = 1 to 8
  }

  object CHROMATIC_PERCUSSION extends InstrumentTypeObject[CHROMATIC_PERCUSSION] {
    val range = 9 to 16
  }

  object ORGAN extends InstrumentTypeObject[ORGAN] {
    val range = 17 to 24
  }

  object GUITAR extends InstrumentTypeObject[GUITAR] {
    val range = 25 to 32
  }

  object BASS extends InstrumentTypeObject[BASS] {
    val range = 33 to 40
  }

  object STRINGS extends InstrumentTypeObject[STRINGS] {
    val range = 41 to 48
  }

  object ENSEMBLE extends InstrumentTypeObject[ENSEMBLE] {
    val range = 49 to 56
  }

  object BRASS extends InstrumentTypeObject[BRASS] {
    val range = 57 to 64
  }

  object REED extends InstrumentTypeObject[REED] {
    val range = 65 to 72
  }

  object PIPE extends InstrumentTypeObject[PIPE] {
    val range = 73 to 80
  }

  object SYNTH_LEAD extends InstrumentTypeObject[SYNTH_LEAD] {
    val range = 81 to 88
  }

  object SYNTH_PAD extends InstrumentTypeObject[SYNTH_PAD] {
    val range = 89 to 96
  }

  object SYNTH_EFFECTS extends InstrumentTypeObject[SYNTH_EFFECTS] {
    val range = 97 to 104
  }

  object ETHNIC extends InstrumentTypeObject[ETHNIC] {
    val range = 105 to 112
  }

  object PERCUSSIVE extends InstrumentTypeObject[PERCUSSIVE] {
    val range = 113 to 120
  }

  object SOUND_EFFECTS extends InstrumentTypeObject[SOUND_EFFECTS] {
    val range = 121 to 128
  }

  object UNKNOWN extends InstrumentTypeObject[UNKNOWN] {
    val range = 129 to Int.MaxValue
  }

  @JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "@class")
  sealed abstract class InstrumentCategory(
      @JsonProperty("instrumentNumber") val instrumentNumber: Int) extends Serializable {
    def range: Range
    require(range contains instrumentNumber, "Instrument number not in range")
    def sameInstrumentClass(instrumentType: InstrumentType): Boolean = getClass == instrumentType.getClass
  }

  case class PIANO(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = PIANO.range
  }

  case class CHROMATIC_PERCUSSION(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = CHROMATIC_PERCUSSION.range
  }

  case class ORGAN(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = ORGAN.range
  }

  case class GUITAR(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber){
    override def range: Range = GUITAR.range
  }

  case class BASS(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber){
    override def range: Range = BASS.range
  }

  case class STRINGS(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = STRINGS.range
  }

  case class ENSEMBLE(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = ENSEMBLE.range
  }

  case class BRASS(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = BRASS.range
  }

  case class REED(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = REED.range
  }

  case class PIPE(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = PIPE.range
  }

  case class SYNTH_LEAD(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = SYNTH_LEAD.range
  }

  case class SYNTH_PAD(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = SYNTH_PAD.range
  }

  case class SYNTH_EFFECTS(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = SYNTH_EFFECTS.range
  }

  case class ETHNIC(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = ETHNIC.range
  }

  case class PERCUSSIVE(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = PERCUSSIVE.range
  }

  case class SOUND_EFFECTS(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = SOUND_EFFECTS.range
  }

  case class UNKNOWN(@JsonProperty("instrumentNumber") override val instrumentNumber: Int)
    extends InstrumentCategory(instrumentNumber) {
    override def range: Range = UNKNOWN.range
  }

  def classify(instrNumber: Int): InstrumentCategory = instrNumber + 1 match {
    case it if PIANO.range contains it => PIANO(it)
    case it if CHROMATIC_PERCUSSION.range contains it => CHROMATIC_PERCUSSION(it)
    case it if ORGAN.range contains it => ORGAN(it)
    case it if GUITAR.range contains it => GUITAR(it)
    case it if BASS.range contains it => BASS(it)
    case it if STRINGS.range contains it => STRINGS(it)
    case it if ENSEMBLE.range contains it => ENSEMBLE(it)
    case it if BRASS.range contains it => BRASS(it)
    case it if REED.range contains it => REED(it)
    case it if PIPE.range contains it => PIPE(it)
    case it if SYNTH_LEAD.range contains it => SYNTH_LEAD(it)
    case it if SYNTH_PAD.range contains it => SYNTH_PAD(it)
    case it if SYNTH_EFFECTS.range contains it => SYNTH_EFFECTS(it)
    case it if ETHNIC.range contains it => ETHNIC(it)
    case it if PERCUSSIVE.range contains it => PERCUSSIVE(it)
    case it if SOUND_EFFECTS.range contains it => SOUND_EFFECTS(it)
    case _ => UNKNOWN(instrNumber + 1)
  }
}

