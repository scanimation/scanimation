package scanimation

import scanimation.common._

object model {
  /** Note labels within octave */
  private val OctaveNotes = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  /** White note offsets within octave */
  private val OctaveWhites = Array(0, 2, 4, 5, 7, 9, 11)
  /** Used in frequency calculations */
  private val TwoRootTwelve = Math.pow(2, 1 / 12.0)
  /** A4 vote with 440 frequency */
  val A4 = Note(octave = 4, offset = 9)

  /** Describes a music unit with name and frequency */
  case class Note(octave: Int, offset: Int) {
    /** Human readable note label */
    lazy val label: String = s"${OctaveNotes(offset)}$octave"

    /** Midi code for the note */
    lazy val midi: Int = (octave + 1) * 12 + offset

    /** True if the note represents a white key on piano */
    lazy val white: Boolean = OctaveWhites.contains(offset)

    /** The note frequency from https://pages.mtu.edu/~suits/NoteFreqCalcs.html */
    lazy val frequency: Double = 440.0 * Math.pow(TwoRootTwelve, midi - A4.midi)

    override def toString: String = label
  }

  /** Returns the distance between frequencies in cents http://hyperphysics.phy-astr.gsu.edu/hbase/Music/cents.html */
  def calculateCents(current: Double, target: Double): Double = 1200 * (current / target).log / 2.log

  /** Returns closest note to a given frequency */
  def calculateNote(frequency: Double): Note = {
    val distance = calculateCents(frequency, 440.0).toInt
    val distanceAbs = distance.abs
    val offset = (distanceAbs / 100 + (if (distanceAbs % 100 < 50) 0 else 1)) * distance.signum
    val octave = 5 + ((offset - 3) / 12.0).floor.toInt
    val octaveOffset = ((offset - 3) % 12 + 12) % 12
    Note(octave, octaveOffset)
  }

}