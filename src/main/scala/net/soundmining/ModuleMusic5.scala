package net.soundmining

import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.modular.ModularInstrument.{AudioInstrument, ControlInstrument}
import net.soundmining.modular.ModularSynth._
import net.soundmining.sound.{SoundPlay, SoundPlays}
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
  val SOUND_DIR = "/Users/danielstahl/Documents/Music/Pieces/Module Music/Module Music 5/sounds/module-music-5_sounds"

  val BOWL1 = "bowl1"
  val TABLE1 = "table1"

  val soundPlays = SoundPlays(
    soundPlays = Map(
      BOWL1 -> SoundPlay(s"$SOUND_DIR/bowl-1.flac", 0.134, 5.254),
      TABLE1 -> SoundPlay(s"$SOUND_DIR/table-1.flac", 0.0, 0.838),
    ),
    numberOfOutputBuses = 2)

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    soundPlays.init
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

    val pan = splay(monoSound.asInstanceOf[AudioInstrument], staticControl(0.2), panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(soundPlays.getRealOutputBus(outputBus))

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def fmPulseNote(startTime: Double, duration: Double, carrier: Double, modulator: Double,
                modAmount: () => ControlInstrument,
                amp: () => ControlInstrument,
                panPosition: () => ControlInstrument,
                extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0): Unit = {
    val fm = fmPulseModulate(
      staticControl(carrier),
      sawOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(fm)
      .addAction(TAIL_ACTION)

    val pan = splay(monoSound.asInstanceOf[AudioInstrument], staticControl(0.2), panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(soundPlays.getRealOutputBus(outputBus))

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def tableNote(startTime: Double, rate: Double = 1.0, center: Double, amp: Double = 1.0, outputBus: Int = 0): Unit =
    playSound(startTime, rate, center, amp, outputBus, TABLE1)

  def bowlNote(startTime: Double, rate: Double = 1.0, center: Double, amp: Double = 1.0, outputBus: Int = 0): Unit =
    playSound(startTime, rate, center, amp, outputBus, BOWL1)

  def playSound(startTime: Double, rate: Double = 1.0, center: Double, amp: Double = 1.0, outputBus: Int = 0, sound: String): Unit = {
    soundPlays.mono(sound)
      .playMono(rate, amp)
      .splay(spread = 0.2, center)
      .play(startTime, outputBus)
  }

  def fmSineNote(startTime: Double, duration: Double, carrier: Double, modulator: Double,
                 modAmount: () => ControlInstrument,
                 amp: () => ControlInstrument,
                 panPosition: () => ControlInstrument,
                 spread: Double = 0.4,
                 extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0): Unit = {

    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(fm)
      .addAction(TAIL_ACTION)

    val pan = splay(monoSound.asInstanceOf[AudioInstrument], staticControl(spread), panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(soundPlays.getRealOutputBus(outputBus))

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def fmTriangleNote(startTime: Double, duration: Double, carrier: Double, modulator: Double,
                     modAmount: () => ControlInstrument,
                     amp: () => ControlInstrument,
                     panPosition: () => ControlInstrument,
                     extra: AudioInstrument => AudioInstrument = a => a, outputBus: Int = 0): Unit = {

    val fm = fmTriangleModulate(
      staticControl(carrier),
      sineOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val monoSound = extra(fm)
      .addAction(TAIL_ACTION)

    val pan = splay(monoSound.asInstanceOf[AudioInstrument], staticControl(0.4), panPosition())
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(soundPlays.getRealOutputBus(outputBus))

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

    pan.getOutputBus.staticBus(soundPlays.getRealOutputBus(outputBus))

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

    val sineModAmount = (attack: Double) => () => relativePercControl(100, 2000, attack, Left(8))
    val sineAmpLow = (attack: Double) => () => relativePercControl(0.001, 0.2, attack, Right(Instrument.SINE))
    val sineAmpHigh = (attack: Double) => () => relativePercControl(0.001, 0.8, attack, Right(Instrument.SINE))

    val filter1: (Double, Double) = (200, 5)
    val filter2: (Double, Double) = (3000, 15)
    val filter3: (Double, Double) = (700, 12)
    val filter4: (Double, Double) = (1000, 14)

    def playSine(startTime: Double, carrier: Double, modulator: Double, attack: Double, pan: (Double, Double)): Unit = {
      fmSineNote(startTime, duration, carrier, modulator, sineModAmount(attack), sineAmpLow(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter1._1), staticControl(filter1._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 18)

      fmSineNote(startTime , duration, carrier, modulator, sineModAmount(attack), sineAmpHigh(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter2._1), staticControl(filter2._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 20)

      fmSineNote(startTime, duration, carrier, modulator, sineModAmount(attack), sineAmpLow(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter3._1), staticControl(filter3._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 22)

      fmSineNote(startTime , duration, carrier, modulator, sineModAmount(attack), sineAmpHigh(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter4._1), staticControl(filter4._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 24)
    }

    playSine(times.head, theme._4(4), theme._4(4) * theme._3, 0.3, (-0.6, 0.6))
    playSine(times(1), theme._4(6), theme._4(6) * theme._3, 0.3, (0.6, -0.6))
    playSine(times(2), theme._4(10), theme._4(10) * theme._3, 0.3, (-0.6, 0.6))
    playSine(times(3), theme._4(8), theme._4(8) * theme._3, 0.3, (0.6, -0.6))
    playSine(times(4), theme._4(11), theme._4(11) * theme._3, 0.3, (-0.6, 0.6))
    playSine(times(5), theme._4(9), theme._4(9) * theme._3, 0.3, (0.6, -0.6))
    playSine(times(6), theme._4(5), theme._4(5) * theme._3, 0.3, (-0.6, 0.6))
    playSine(times(7), theme._4(7), theme._4(7) * theme._3, 0.3, (0.6, -0.6))

    println(s"duration $duration")
    println(s"times $times")

    val bowl = (startTime: Double) => {
      bowlNote(startTime + 0.05, 1, -0.4, 0.5, outputBus = 26)
      bowlNote(startTime + 0.05, 1.01, 0.7, 0.5, outputBus = 28)
      bowlNote(startTime + 0.05, 0.99, -0.5, 0.5, outputBus = 30)

      bowlNote(startTime + 0.6, 0.5, -0.4, 0.5, outputBus = 26)
      bowlNote(startTime + 0.6, 0.51, 0.7, 0.5, outputBus = 28)
      bowlNote(startTime + 0.6, 0.49, -0.5, 0.5, outputBus = 30)
    }

    val bowlTimes = Seq(times(2), times(5))
    bowlTimes.foreach(bowl(_))

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

    val sineModAmount = (attack: Double) => () => relativePercControl(100, 2000, attack, Left(8))
    val sineAmpLow = (attack: Double) => () => relativePercControl(0.001, 0.2, attack, Right(Instrument.SINE))
    val sineAmpHigh = (attack: Double) => () => relativePercControl(0.001, 0.8, attack, Right(Instrument.SINE))

    val filter1: (Double, Double) = (200, 5)
    val filter2: (Double, Double) = (3000, 15)
    val filter3: (Double, Double) = (700, 12)
    val filter4: (Double, Double) = (1000, 14)

    def playSine(startTime: Double, carrier: Double, modulator: Double, attack: Double, pan: (Double, Double)): Unit = {
      fmSineNote(startTime, duration, carrier, modulator, sineModAmount(attack), sineAmpLow(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter1._1), staticControl(filter1._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 18)

      fmSineNote(startTime , duration, carrier, modulator, sineModAmount(attack), sineAmpHigh(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter2._1), staticControl(filter2._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 20)

      fmSineNote(startTime, duration, carrier, modulator, sineModAmount(attack), sineAmpLow(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter3._1), staticControl(filter3._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 22)

      fmSineNote(startTime , duration, carrier, modulator, sineModAmount(attack), sineAmpHigh(attack), () => lineControl(pan._1, pan._2),
        spread = 0.2,
        audio => lowPassFilter(bandRejectFilter(audio, staticControl(filter4._1), staticControl(filter4._2)).addAction(TAIL_ACTION), staticControl(6000)),
        outputBus = 24)
    }

    // 8 10 6 4
    // 7 5 9 11
    playSine(times.head, theme._4(8), theme._4(8) * theme._3, 0.6, (-0.6, 0.6))
    playSine(times(1), theme._4(10), theme._4(10) * theme._3, 0.6, (0.6, -0.6))
    playSine(times(2), theme._4(6), theme._4(6) * theme._3, 0.6, (-0.6, 0.6))
    playSine(times(3), theme._4(4), theme._4(4) * theme._3, 0.6, (0.6, -0.6))
    playSine(times(4), theme._4(7), theme._4(7) * theme._3, 0.6, (-0.6, 0.6))
    playSine(times(5), theme._4(5), theme._4(5) * theme._3, 0.6, (0.6, -0.6))
    playSine(times(6), theme._4(9), theme._4(9) * theme._3, 0.6, (-0.6, 0.6))
    playSine(times(7), theme._4(11), theme._4(11) * theme._3, 0.6, (0.6, -0.6))

    val bowl = (startTime: Double) => {
      bowlNote(startTime + 0.05, 0.5, -0.4, 0.5, outputBus = 26)
      bowlNote(startTime + 0.05, 0.51, 0.7, 0.5, outputBus = 28)
      bowlNote(startTime + 0.05, 0.49, -0.5, 0.5, outputBus = 30)

      bowlNote(startTime + 0.6, 1, -0.4, 0.5, outputBus = 26)
      bowlNote(startTime + 0.6, 1.01, 0.7, 0.5, outputBus = 28)
      bowlNote(startTime + 0.6, 0.99, -0.5, 0.5, outputBus = 30)
    }

    val bowlTimes = Seq(times(2), times(7))
    bowlTimes.foreach(bowl(_))

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
    println(theme)

    val percModAmount = () => relativePercControl(300, 3000, 0.001, Left(-8))
    val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
    val percAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.001, Left(-8))
    val sineAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.3, Right(Instrument.SINE))

    val filteredSound = (audio: AudioInstrument) =>
      lowPassFilter(bandRejectFilter(audio, staticControl(400), staticControl(2)).addAction(TAIL_ACTION), staticControl(8000))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (2, 3)
      //val first: Int = 2
      //val second: Int = 3
      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.2, 0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.2, -0.2),
        filteredSound,
        outputBus = 2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 3, lowFirst, lowFirst * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.4, 0.4),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.1, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        percModAmount,
        percAmp(0.6),
        () => lineControl(0.4, -0.4),
        filteredSoundLow,
        outputBus = 8)

    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (3, 2, 3)

      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.3, -0.4),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.3),
        filteredSound,
        outputBus = 2)

      fmSawNote(startTime + 0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
        percModAmount,
        percAmp(0.4),
        () => lineControl(0.3, 0.4),
        filteredSound,
        outputBus = 4)

      val invFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invFirst * invFirst * invFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val invThird = theme._4(third) / (theme._4(third) * theme._3)
      val lowThird = theme._4(second) * invThird * invThird * invThird

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 3, lowFirst, lowFirst * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.5, -0.6),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.075, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.5, 0.5),
        filteredSoundLow,
        outputBus = 8)

      fmSawNote(startTime + 0.25, 2, lowThird * 1.02, lowThird * theme._3 * 0.98,
        percModAmount,
        percAmp(0.6),
        () => lineControl(0.5, 0.6),
        filteredSoundLow,
        outputBus = 10)
    }

    val twoLongNote = (startTime: Double) => {

      val (first, second) = (2, 3)
      fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.3, -0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.2),
        filteredSound,
        outputBus = 2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 2, lowFirst, lowFirst * theme._3,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(0.4, -0.3),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.2, 2, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.4, 0.3),
        filteredSoundLow,
        outputBus = 8)
    }

    val table = (startTime: Double) => {
      tableNote(startTime + 0.1, 0.5, 0.3, 0.5, outputBus = 12)
      tableNote(startTime + 0.1, 0.51, -0.9, 0.5, outputBus = 14)
      tableNote(startTime + 0.1, 0.49, 0.7, 0.5, outputBus = 16)

      tableNote(startTime + 0.7, 1.0, 0.3, 0.5, outputBus = 12)
      tableNote(startTime + 0.7, 1.01, -0.9, 0.5, outputBus = 14)
      tableNote(startTime + 0.7, 0.99, 0.7, 0.5, outputBus = 16)

      tableNote(startTime + 0.00, 0.10, 0.3, 0.5, outputBus = 12)
      tableNote(startTime + 0.00, 0.11, -0.9, 0.5, outputBus = 14)
      tableNote(startTime + 0.00, 0.09, 0.7, 0.5, outputBus = 16)
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

    val tableTimes = Seq(times(2), times(6))
    tableTimes.foreach(table(_))

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
    val percAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.001, Left(-8))
    val sineAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.3, Right(Instrument.SINE))
    val filteredSound = (audio: AudioInstrument) => bandRejectFilter(audio, staticControl(400), staticControl(2))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (3, 2)

      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.2, 0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.2, -0.2),
        filteredSound,
        outputBus = 2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 3, lowFirst, lowFirst * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.4, 0.4),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.1, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(0.4, -0.4),
        filteredSoundLow,
        outputBus = 8)
    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (2, 3, 2)

      fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.3, -0.4),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.3),
        filteredSound,
        outputBus = 2)

      fmSawNote(startTime + 0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
        percModAmount,
        percAmp(0.4),
        () => lineControl(0.3, 0.4),
        filteredSound,
        outputBus = 4)

      val invFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invFirst * invFirst * invFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val invThird = theme._4(third) / (theme._4(third) * theme._3)
      val lowThird = theme._4(second) * invThird * invThird * invThird

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 3, lowFirst, lowFirst * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.5, -0.6),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.075, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.5, 0.5),
        filteredSoundLow,
        outputBus = 8)

      fmSawNote(startTime + 0.25, 2, lowThird * 1.02, lowThird * theme._3 * 0.98,
        percModAmount,
        percAmp(0.6),
        () => lineControl(0.5, 0.6),
        filteredSoundLow,
        outputBus = 10)
    }

    val twoLongNote = (startTime: Double) => {

      val (first, second) = (3, 2)
      fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.3, -0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.2),
        filteredSound,
        outputBus = 2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 2, lowFirst, lowFirst * theme._3,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(0.5, -0.4),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.2, 2, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.5, 0.4),
        filteredSoundLow,
        outputBus = 8)
    }

    val table = (startTime: Double) => {
      tableNote(startTime + 0.1, 1.0, 0.3, 0.5, 12)
      tableNote(startTime + 0.1, 1.01, -0.9, 0.5, 14)
      tableNote(startTime + 0.1, 0.99, 0.7, 0.5, 16)

      tableNote(startTime + 0.7, 0.5, 0.3, 0.5, 12)
      tableNote(startTime + 0.7, 0.51, -0.9, 0.5, 14)
      tableNote(startTime + 0.7, 0.49, 0.7, 0.5, 16)

      tableNote(startTime + 0.00, 0.10, 0.3, 0.5, 12)
      tableNote(startTime + 0.00, 0.11, -0.9, 1.0, 14)
      tableNote(startTime + 0.00, 0.09, 0.7, 0.5, 16)
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

    val tableTimes = Seq(times(1), times(7))
    tableTimes.foreach(table(_))

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
    val percAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.001, Left(-8))
    val sineAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.3, Right(Instrument.SINE))
    val filteredSound = (audio: AudioInstrument) => bandRejectFilter(audio, staticControl(400), staticControl(2))

    val twoShortNote = (startTime: Double) => {
      val (first, second) = (2, 3)

      fmSawNote(startTime, 1, theme._4(first) * 1.01, theme._4(first) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.2, -0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.1, 3, theme._4(second), theme._4(second) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.2, 0.2),
        filteredSound,
        outputBus = 2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 1, lowFirst * 1.01, lowFirst * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(0.4, -0.4),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.1, 3, lowSecond, lowSecond * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.4, 0.4),
        filteredSoundLow,
        outputBus = 8)

    }

    val threeShortNote = (startTime: Double) => {
      val (first, second, third) = (3, 2, 3)

      fmSawNote(startTime, 2, theme._4(first) * 1.02, theme._4(first) * theme._3 * 0.98,
        percModAmount,
        percAmp(0.4),
        () => lineControl(0.3, 0.4),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.3),
        filteredSound,
        outputBus = 2)

      fmSawNote(startTime + 0.25, 3, theme._4(third), theme._4(third) * theme._3,
        percModAmount,
        percAmp(0.4),
        () => lineControl(-0.3, -0.4),
        filteredSound,
        outputBus = 4)

      val invFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invFirst * invFirst * invFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val invThird = theme._4(third) / (theme._4(third) * theme._3)
      val lowThird = theme._4(second) * invThird * invThird * invThird

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 2, lowFirst * 1.02, lowFirst * theme._3 * 0.98,
        percModAmount,
        percAmp(0.6),
        () => lineControl(0.5, 0.6),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.075, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.5, 0.5),
        filteredSoundLow,
        outputBus = 8)

      fmSawNote(startTime + 0.25, 3, lowThird, lowThird * theme._3,
        percModAmount,
        percAmp(0.6),
        () => lineControl(-0.5, -0.6),
        filteredSoundLow,
        outputBus = 10)
    }

    val twoLongNote = (startTime: Double) => {
      val (first, second) = (2, 3)

      fmSawNote(startTime, 2, theme._4(first) * 1.01, theme._4(first) * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(-0.3, 0.2),
        filteredSound,
        outputBus = 0)

      fmSawNote(startTime + 0.2, 2, theme._4(second), theme._4(second) * theme._3,
        sineModAmount,
        sineAmp(0.4),
        () => lineControl(0.3, -0.2),
        filteredSound,
        2)

      val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
      val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

      val invSecond = theme._4(second) / (theme._4(second) * theme._3)
      val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

      val filteredSoundLow = (audio: AudioInstrument) =>
        lowPassFilter(audio, staticControl(75))

      fmSawNote(startTime, 2, lowFirst * 1.01, lowFirst * theme._3 * 0.99,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(-0.5, 0.4),
        filteredSoundLow,
        outputBus = 6)

      fmSawNote(startTime + 0.2, 2, lowSecond, lowSecond * theme._3,
        sineModAmount,
        sineAmp(0.6),
        () => lineControl(0.5, -0.4),
        filteredSoundLow,
        8)
    }

    val table = (startTime: Double) => {
      tableNote(startTime + 0.1, 1.0, 0.3, 0.5, 12)
      tableNote(startTime + 0.1, 1.01, -0.9, 0.5, 14)
      tableNote(startTime + 0.1, 0.99, 0.7, 0.5, 16)

      tableNote(startTime + 0.7, 0.5, 0.3, 0.5, 12)
      tableNote(startTime + 0.7, 0.51, -0.9, 0.5, 14)
      tableNote(startTime + 0.7, 0.49, 0.7, 0.5, 16)

      tableNote(startTime + 0.00, 0.10, 0.3, 0.5, 12)
      tableNote(startTime + 0.00, 0.11, -0.9, 0.5, 14)
      tableNote(startTime + 0.00, 0.09, 0.7, 0.5, 16)
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

    val tableTimes = Seq(times(3), times(5), times(9))
    tableTimes.foreach(table(_))

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
      val percAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.001, Left(-8))
      val sineAmp = (peak: Double) => () => relativePercControl(0.001, peak, 0.3, Right(Instrument.SINE))
      val filteredSound = (audio: AudioInstrument) => bandRejectFilter(audio, staticControl(400), staticControl(2))

      val twoShortNote = (startTime: Double) => {
        val (first, second) = (2, 3)

        fmSawNote(startTime, 3, theme._4(first), theme._4(first) * theme._3,
          percModAmount,
          percAmp(0.4),
          () => lineControl(-0.2, 0.2),
          filteredSound,
          outputBus = 0)

        fmSawNote(startTime + 0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
          sineModAmount,
          sineAmp(0.4),
          () => lineControl(0.2, -0.2),
          filteredSound,
          outputBus = 2)

        val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
        val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

        val invSecond = theme._4(second) / (theme._4(second) * theme._3)
        val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

        val filteredSoundLow = (audio: AudioInstrument) =>
          lowPassFilter(audio, staticControl(75))

        fmSawNote(startTime, 3, lowFirst, lowFirst * theme._3,
          percModAmount,
          percAmp(0.6),
          () => lineControl(-0.4, 0.4),
          filteredSoundLow,
          outputBus = 6)

        fmSawNote(startTime + 0.1, 1, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
          sineModAmount,
          sineAmp(0.6),
          () => lineControl(0.4, -0.4),
          filteredSoundLow,
          outputBus = 8)
      }

      val twoLongNote = (startTime: Double) => {

        val (first, second) = (2, 3)

        fmSawNote(startTime, 2, theme._4(first), theme._4(first) * theme._3,
          sineModAmount,
          sineAmp(0.4),
          () => lineControl(0.3, -0.2),
          filteredSound,
          outputBus = 0)

        fmSawNote(startTime + 0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
          sineModAmount,
          sineAmp(0.4),
          () => lineControl(-0.3, 0.2),
          filteredSound,
          outputBus = 2)

        val invertedFirst = theme._4(first) / (theme._4(first) * theme._3)
        val lowFirst = theme._4(first) * invertedFirst * invertedFirst * invertedFirst

        val invSecond = theme._4(second) / (theme._4(second) * theme._3)
        val lowSecond = theme._4(second) * invSecond * invSecond * invSecond

        val filteredSoundLow = (audio: AudioInstrument) =>
          lowPassFilter(audio, staticControl(75))

        fmSawNote(startTime, 2, lowFirst, lowFirst * theme._3,
          sineModAmount,
          sineAmp(0.6),
          () => lineControl(0.5, -0.4),
          filteredSoundLow,
          outputBus = 6)

        fmSawNote(startTime + 0.2, 2, lowSecond * 1.01, lowSecond * theme._3 * 0.99,
          sineModAmount,
          sineAmp(0.6),
          () => lineControl(-0.5, 0.4),
          filteredSoundLow,
          outputBus = 8)
      }

      val table = (startTime: Double) => {
        tableNote(startTime + 0.1, 0.5, 0.3, 0.5, 12)
        tableNote(startTime + 0.1, 0.51, -0.9, 0.5, 14)
        tableNote(startTime + 0.1, 0.49, 0.7, 0.5, 16)

        tableNote(startTime + 0.7, 1.0, 0.3, 0.5, 12)
        tableNote(startTime + 0.7, 1.01, -0.9, 0.5, 14)
        tableNote(startTime + 0.7, 0.99, 0.7, 0.5, 16)

        tableNote(startTime + 0.00, 0.10, 0.3, 0.5, 12)
        tableNote(startTime + 0.00, 0.11, -0.9, 0.5, 14)
        tableNote(startTime + 0.00, 0.09, 0.7, 0.5, 16)
      }

      val pulse = theme._4(3)

      val notes = Seq(
        twoShortNote, twoShortNote, twoLongNote
      )
      val times = Melody.absolute(startTime, Seq(
        pulse * 13, pulse * 13, pulse * 13))
        .map(makeTime)

      val tableTimes = Seq(times(2))
      tableTimes.foreach(table(_))

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
