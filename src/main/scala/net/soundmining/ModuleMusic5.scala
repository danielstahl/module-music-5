package net.soundmining

import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument}
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth._
import net.soundmining.synth.Instrument.TAIL_ACTION
import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir
import net.soundmining.synth.Utils.absoluteTimeToMillis

/*
Module Music 5. Inspiration from folk music. Korean, Indian. (The Raga Guide).
Investigate how to get string-like tones with fm-synthesis. Percussive, short punchy attack.
Pitch bend. Mix sine-based tones, saw-based tones, play the same amp contour with noise, ring modulate.
Maybe mix with real samples.
* */
object ModuleMusic5 {
  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
  }

  def fmSawNote(startTime: Double, duration: Double, carrier: Double, modulator: Double,
                modAmount: () => ControlInstrument,
                amp: () => ControlInstrument,
                panPosition: () => ControlInstrument,
                extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0): Unit = {
    val fm = fmSawModulate(
      staticControl(carrier),
      sawOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(fm)
      .addAction(TAIL_ACTION)

    val pan = panning(monoSound.asInstanceOf[AudioInstrument], panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(outputBus)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }


  def fmSineNote(startTime: Double, duration: Double, carrier: Double, modulator: Double,
                 modAmount: () => ControlInstrument,
                 amp: () => ControlInstrument,
                 panPosition: () => ControlInstrument,
                 extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0): Unit = {

    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(fm)
      .addAction(TAIL_ACTION)

    val pan = panning(monoSound.asInstanceOf[AudioInstrument], panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(outputBus)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def noiseNote(startTime: Double, duration: Double,
                amp: () => ControlInstrument,
                panPosition: () => ControlInstrument,
                extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0):Unit = {
    val noise = whiteNoiseOsc(amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(noise)
      .addAction(TAIL_ACTION)

    val pan = panning(monoSound.asInstanceOf[AudioInstrument], panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(outputBus)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  /*
  Dark theme
  C2 FISS3
  D2 DISS3
  Bright theme
  A1 GISS3

  The Residents Commercial Album, Constantinople. Tuxedomoon Nazda, Fifth Column, Tritone, What Use

  Theme 7 notes from
  2 3 6 5 4 8 9
  * */


  def makeTime(millis: Double): Double = millis / 1000

  def slowThemeExposition(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): Double = {
    if(reset) client.resetClock

    val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
      .map {
        case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
      }
      .map {
        case (carr, mod) => (carr, mod, makeFact(carr, mod))
      }
      .map {
        case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
      }

    val theme = themes(themeIndex)

    val pulse = theme._4(3)

    val times = Melody.absolute(startTime, Seq(
      pulse * 34, pulse * 34, pulse * 34, pulse * 21,
      pulse * 34, pulse * 34, pulse * 34, pulse * 21,
    ))
      .map(makeTime)

    val duration = makeTime(pulse * 55)

    println(s"duration $duration")
    println(s"times $times")

    val sineModAmount = () => relativePercControl(100, 1000, 0.3, Right(Instrument.SINE))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    fmSineNote(times.head, duration, theme._4(4), theme._4(4) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(1), duration, theme._4(6), theme._4(6) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(2), duration, theme._4(10), theme._4(10) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(3), duration, theme._4(8), theme._4(8) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(4), duration, theme._4(11), theme._4(11) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(5), duration, theme._4(9), theme._4(9) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(6), duration, theme._4(5), theme._4(5) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(7), duration, theme._4(7), theme._4(7) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))

    absoluteTimeToMillis(times(5) + makeTime(pulse * 21))
  }

  def slowThemeDevelopment1(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): (Double, Double) = {
    if(reset) client.resetClock

    val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
      .map {
        case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
      }
      .map {
        case (carr, mod) => (carr, mod, makeFact(carr, mod))
      }
      .map {
        case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
      }

    val theme = themes(themeIndex)

    val pulse = theme._4(3)

    val times = Melody.absolute(startTime, Seq(
      pulse * 21, pulse * 34, pulse * 34, pulse * 34,
      pulse * 21, pulse * 34, pulse * 34, pulse * 34,
    ))
      .map(makeTime)

    val duration = makeTime(pulse * 55)

    println(s"duration $duration")
    println(s"times $times")

    val sineModAmount = () => relativePercControl(100, 1000, 0.3, Right(Instrument.SINE))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    // 8 10 6 4
    // 7 5 9 11
    fmSineNote(times.head, duration, theme._4(8), theme._4(8) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(1), duration, theme._4(10), theme._4(10) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(2), duration, theme._4(6), theme._4(6) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(3), duration, theme._4(4), theme._4(4) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(4), duration, theme._4(7), theme._4(7) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(5), duration, theme._4(5), theme._4(5) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))
    fmSineNote(times(6), duration, theme._4(9), theme._4(9) * theme._3, sineModAmount, sineAmp, () => lineControl(0.6, -0.6))
    fmSineNote(times(7), duration, theme._4(11), theme._4(11) * theme._3, sineModAmount, sineAmp, () => lineControl(-0.6, 0.6))

    (absoluteTimeToMillis(times(2) + makeTime(pulse * 21)),
    absoluteTimeToMillis(times(7) + makeTime(pulse * (34 + 21))))
  }


  def fmThemeExposition(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): Double = {
    if(reset) client.resetClock

    // 2 2 1 2 1
    // 3 3 1 3 1

    val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
      .map {
        case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
      }
      .map {
        case (carr, mod) => (carr, mod, makeFact(carr, mod))
      }
      .map {
        case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
      }

    val theme = themes(themeIndex)

    val percModAmount = () => relativePercControl(300, 3000, 0.001, Left(-8))
    val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
    val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (2, 3)
      //val first: Int = 2
      //val second: Int = 3
      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.2, 0.2))

      fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(0.2, -0.2))
    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (3, 2, 3)
      //val first: Int = 3
      //val second: Int = 2
      //val third: Int = 3
      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.3, -0.4))

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.3))

      fmSawNote(startTime + 0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
        percModAmount,
        percAmp,
        () => lineControl(0.3, 0.4))
    }

    val twoLongNote = (startTime: Double) => {
      //val first: Int = 2
      //val second: Int = 3
      val (first, second) = (2, 3)
      fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
        sineModAmount,
        sineAmp,
        () => lineControl(0.3, -0.2))

      fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.2))
    }

    val pulse = theme._4(3)

    val notes = Seq(
      twoShortNote, twoShortNote, twoLongNote, twoShortNote, twoLongNote,
      threeShortNote, threeShortNote, twoLongNote, threeShortNote, twoLongNote,
    )
    val times = Melody.absolute(startTime, Seq(
      pulse * 13, pulse * 13, pulse * 13, pulse * 8, pulse * 13,
      pulse * 13, pulse * 13, pulse * 8, pulse * 13, pulse * 13))
      .map(makeTime)

    println(s"Times $times")
    times.zipWithIndex.foreach {
      case (time, i) => notes(i)(time)
    }

    // next start
    absoluteTimeToMillis(makeTime(pulse * 34))
  }

  def fmThemeDevelopment1(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): Double = {
    if(reset) client.resetClock

    // 2 2 1 2 1
    // 3 3 1 3 1

    val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
      .map {
        case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
      }
      .map {
        case (carr, mod) => (carr, mod, makeFact(carr, mod))
      }
      .map {
        case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
      }

    val theme = themes(themeIndex)

    val percModAmount = () => relativePercControl(300, 3000, 0.001, Left(-8))
    val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
    val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (3, 2)
      //val first: Int = 2
      //val second: Int = 3
      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.2, 0.2))

      fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(0.2, -0.2))
    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (2, 3, 2)
      //val first: Int = 3
      //val second: Int = 2
      //val third: Int = 3
      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.3, -0.4))

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.3))

      fmSawNote(startTime + 0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
        percModAmount,
        percAmp,
        () => lineControl(0.3, 0.4))
    }

    val twoLongNote = (startTime: Double) => {
      //val first: Int = 2
      //val second: Int = 3
      val (first, second) = (3, 2)
      fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
        sineModAmount,
        sineAmp,
        () => lineControl(0.3, -0.2))

      fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.2))
    }

    val pulse = theme._4(3)

    val notes = Seq(
      twoShortNote, twoShortNote, twoLongNote, twoShortNote, twoLongNote,
      threeShortNote, threeShortNote, twoLongNote, threeShortNote, twoLongNote,
    )
    val times = Melody.absolute(startTime, Seq(
      pulse * 13, pulse * 13, pulse * 13, pulse * 8, pulse * 13,
      pulse * 13, pulse * 13, pulse * 8, pulse * 13, pulse * 13))
      .map(makeTime)

    println(s"Times $times")
    times.zipWithIndex.foreach {
      case (time, i) => notes(i)(time)
    }
    absoluteTimeToMillis(times(9) + makeTime(pulse * 8))
  }

  def fmThemeDevelopment2(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    // 2 2 1 2 1
    // 3 3 1 3 1

    val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
      .map {
        case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
      }
      .map {
        case (carr, mod) => (carr, mod, makeFact(carr, mod))
      }
      .map {
        case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
      }

    val theme = themes(themeIndex)

    val percModAmount = () => relativePercControl(300, 3000, 0.001, Left(-8))
    val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
    val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (2, 3)

      fmSawNote(startTime, 1, theme._4(first) * 1.01, theme._4(first) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(0.2, -0.2))

      fmSawNote(startTime + 0.1, 3, theme._4(second), theme._4(second) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.2, 0.2))
    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (3, 2, 3)


      fmSawNote(startTime, 2, theme._4(first) * 1.02, theme._4(first) * theme._3 * 0.98,
        percModAmount,
        percAmp,
        () => lineControl(0.3, 0.4))

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.3))

      fmSawNote(startTime + 0.25, 3, theme._4(third), theme._4(third) * theme._3,
        percModAmount,
        percAmp,
        () => lineControl(-0.3, -0.4))
    }

    val twoLongNote = (startTime: Double) => {
      val (first, second) = (2, 3)

      fmSawNote(startTime, 2, theme._4(first) * 1.01, theme._4(first) * theme._3 * 0.99,
        sineModAmount,
        sineAmp,
        () => lineControl(-0.3, 0.2))

      fmSawNote(startTime + 0.2, 2, theme._4(second), theme._4(second) * theme._3,
        sineModAmount,
        sineAmp,
        () => lineControl(0.3, -0.2))
    }

    val pulse = theme._4(3)

    val notes = Seq(
      twoShortNote, twoShortNote, twoLongNote, twoShortNote, twoLongNote,
      threeShortNote, threeShortNote, twoLongNote, threeShortNote, twoLongNote,
    )
    val times = Melody.absolute(startTime, Seq(
      pulse * 13, pulse * 13, pulse * 13, pulse * 8, pulse * 13,
      pulse * 13, pulse * 13, pulse * 8, pulse * 13, pulse * 13))
      .map(makeTime)

    println(s"Times $times")
    times.zipWithIndex.foreach {
      case (time, i) => notes(i)(time)
    }
  }

    def fmThemeRecapitulation(startTime: Double = 0, themeIndex: Int = 0, reset: Boolean = true): Unit = {
      if(reset) client.resetClock

      // 2 2 1 2 1
      // 3 3 1 3 1

      val themes = Seq(("c2", "fiss3"), ("d2", "diss3"), ("a1", "giss3"))
        .map {
          case (carr, mod) => (Note.noteToHertz(carr), Note.noteToHertz(mod))
        }
        .map {
          case (carr, mod) => (carr, mod, makeFact(carr, mod))
        }
        .map {
          case (carr, mod, fact) => (carr, mod, fact, makeSpectrum2(carr, fact, 50))
        }

      val theme = themes(themeIndex)

      val percModAmount = () => relativePercControl(300, 3000, 0.001, Left(-8))
      val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
      val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
      val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

      val twoShortNote = (startTime: Double) => {
        val (first, second) = (2, 3)
        //val first: Int = 2
        //val second: Int = 3
        fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
          percModAmount,
          percAmp,
          () => lineControl(-0.2, 0.2))

        fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
          sineModAmount,
          sineAmp,
          () => lineControl(0.2, -0.2))
      }

      val threeShortNote = (startTime: Double) => {
        val (first, second, third) = (3, 2, 3)
        //val first: Int = 3
        //val second: Int = 2
        //val third: Int = 3
        fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
          percModAmount,
          percAmp,
          () => lineControl(-0.3, -0.4))

        fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
          sineModAmount,
          sineAmp,
          () => lineControl(-0.3, 0.3))

        fmSawNote(startTime + 0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
          percModAmount,
          percAmp,
          () => lineControl(0.3, 0.4))
      }

      val twoLongNote = (startTime: Double) => {
        //val first: Int = 2
        //val second: Int = 3
        val (first, second) = (2, 3)
        fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
          sineModAmount,
          sineAmp,
          () => lineControl(0.3, -0.2))

        fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
          sineModAmount,
          sineAmp,
          () => lineControl(-0.3, 0.2))
      }

      val pulse = theme._4(3)

      val notes = Seq(
        twoShortNote, twoShortNote, twoLongNote/*, twoShortNote, twoLongNote,
        threeShortNote, threeShortNote, twoLongNote, threeShortNote, twoLongNote,*/
      )
      val times = Melody.absolute(startTime, Seq(
        pulse * 13, pulse * 13, pulse * 13/*, pulse * 8, pulse * 13,
        pulse * 13, pulse * 13, pulse * 8, pulse * 13, pulse * 13*/))
        .map(makeTime)

      println(s"Times $times")
      times.zipWithIndex.foreach {
        case (time, i) => notes(i)(time)
      }
    }

  def playThemes(): Unit = {
    client.resetClock
    val afterFmTheme = fmThemeExposition(startTime = 0 , reset = false)
    val afterSlowTheme = slowThemeExposition(startTime = afterFmTheme, reset = false)
    val afterReversedFmTheme = fmThemeDevelopment1(startTime = afterSlowTheme, reset = false)
    val (afterSlowThemeDevelopment1, fmThemeRecapitulationStart) = slowThemeDevelopment1(startTime = afterReversedFmTheme, reset = false)
    fmThemeDevelopment2(startTime = afterSlowThemeDevelopment1, reset = false)
    fmThemeRecapitulation(startTime = fmThemeRecapitulationStart, reset = false)
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
  }
}
