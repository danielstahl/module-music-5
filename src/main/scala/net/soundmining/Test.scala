package net.soundmining

import net.soundmining.ModuleMusic5.{client, fmSawNote}
import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.modular.ModularInstrument.ControlInstrument
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth.{controlMultiply, controlSum, fmSawModulate, fmSineModulate, highPassFilter, lineControl, panning, relativePercControl, ringModulate, sawOsc, sineControl, sineOsc, staticControl, whiteNoiseOsc}
import net.soundmining.synth.Instrument
import net.soundmining.synth.Instrument.TAIL_ACTION
import net.soundmining.synth.Utils.absoluteTimeToMillis
import ModuleMusic5._

object Test {
  def testShortAttack(modAmountMin: Double = 300, modAmountMax: Double = 3000, carr: String = "c6", mod: String = "fiss6"): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val panPos = 0
    val startTime = 0
    val duration = 5
    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }


  def testPlayTable(rate: Double = 1.0): Unit = {
    client.resetClock

    soundPlays.mono(TABLE1)
      .playMono(rate - 0.02, 8.0)
      .lowPass(100)
      .splay(0.2, 0)
      .play(0, 0)

    soundPlays.mono(TABLE1)
      .playMono(rate + 0.02, 8.0)
      .highPass(400)
      .splay(-0.2, 0)
      .play(0, 0)
/*
    soundPlays.mono(TABLE1)
      .playMono(1.99, 1.0)
      .splay(0, 0)
      .play(0, 0)

    soundPlays.mono(TABLE1)
      .playMono(0.5, 1.0)
      .splay(-0.2, 0)
      .play(0, 0)*/
  }

  def testPlayBowl(rate: Double = 1.0): Unit = {
    client.resetClock

    soundPlays.mono(BOWL1)
      .playMono(rate, 1.0)
      .splay(0.2, 0)
      .play(0, 0)

  }

  // ModuleMusic5.testSawAttack(modAmountMin = 300, modAmountMax = 5000, carr = "c7", mod = "fiss7", duration = 0.5)
  def testSawAttack(modAmountMin: Double = 300, modAmountMax: Double = 3000, carr: String = "c2", mod: String = "fiss4", duration: Double = 5): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val panPos = 0
    val startTime = 0
    val fm = fmSawModulate(
      staticControl(carrier),
      sawOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def testSawRingAttack(modAmountMin: Double = 300, modAmountMax: Double = 3000, carr: String = "c2", mod: String = "fiss4", ring: String = "ciss5", duration: Double = 5): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val ringModulator = Note.noteToHertz(ring)
    val panPos = 0
    val startTime = 0
    val fm = fmSawModulate(
      staticControl(carrier),
      sawOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)

    val ringMod = ringModulate(fm, staticControl(ringModulator))
      .addAction(TAIL_ACTION)

    val pan = panning(ringMod, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def testSplay(spreadStart: Double = 0.1, spreadEnd: Double = 0.5): Unit = {
    client.resetClock

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

    val theme = themes(0)

    val duration = 25
    val carrier = theme._4(4)
    val modulator = theme._4(4) * theme._3
    val modAmount: () => ControlInstrument = () => relativePercControl(100, 300, 0.3, Right(Instrument.SINE))
    val amp: () => ControlInstrument = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))
    val centerPosition: () => ControlInstrument = () => lineControl(-0.9, 0.9)
    val spread: () => ControlInstrument = () => relativePercControl(spreadStart, spreadEnd, 0.3, Right(Instrument.SINE))
    val outputBus: Int = 0

    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(modAmount(),
        staticControl(modulator)), amp())
      .addAction(TAIL_ACTION)

    val splay = ModularSynth.splay(fm, spread(), centerPosition(), 1)
      .addAction(TAIL_ACTION).withNrOfChannels(2)
    //val pan = panning(fm.asInstanceOf[AudioInstrument], panPosition())
    //  .addAction(TAIL_ACTION).withNrOfChannels(2)

    splay.getOutputBus.staticBus(outputBus)

    val graph = splay.buildGraph(0, duration, splay.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(0), graph))
  }

  // carr = "c2", mod = "fiss4"
  // carr = "c2", mod = "ciss4"
  def testSlowSawAttack(modAmountMin: Double = 300, modAmountMax: Double = 3000, carr: String = "c2", mod: String = "fiss4", duration: Double = 5): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val panPos = 0
    val startTime = 0
    val fm = fmSawModulate(
      staticControl(carrier),
      sawOsc(relativePercControl(modAmountMin, modAmountMax, 0.2, Right(Instrument.SINE)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.2, Right(Instrument.SINE)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  /*
    * Hm. Short 2 3 works
    *
    * */
  def testTwoNotes(first: Int = 2, second: Int = 3, themeIndex: Int = 0): Unit = {
    client.resetClock

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
    val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    fmSawNote(0, 3, theme._4(first), theme._4(first) * theme._3,
      percModAmount,
      percAmp,
      () => lineControl(-0.2, 0.2))

    fmSawNote(0.1, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
      sineModAmount,
      sineAmp,
      () => lineControl(0.2, -0.2))

    // Variants with two notes, three notes, five notes
  }

  def testThreeNotes(first: Int = 3, second: Int = 2, third: Int = 3, themeIndex: Int = 0): Unit = {
    client.resetClock

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
    val percAmp = () => relativePercControl(0.001, 1, 0.001, Left(-8))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    fmSawNote(0, 3, theme._4(first), theme._4(first) * theme._3,
      percModAmount,
      percAmp,
      () => lineControl(-0.2, -0.5))

    fmSawNote(0.075, 1, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
      sineModAmount,
      sineAmp,
      () => lineControl(-0.2, 0.2))

    fmSawNote(0.25, 2, theme._4(third) * 1.02, theme._4(third) * theme._3 * 0.98,
      percModAmount,
      percAmp,
      () => lineControl(0.2, 0.5))

    // Variants with two notes, three notes, five notes
  }

  def testTwoSlowNotes(first: Int = 2, second: Int = 3, themeIndex: Int = 0): Unit = {
    client.resetClock

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

    val sineModAmount = () => relativePercControl(300, 3000, 0.3, Right(Instrument.SINE))
    val sineAmp = () => relativePercControl(0.001, 1, 0.3, Right(Instrument.SINE))

    fmSawNote(0, 2, theme._4(first), theme._4(first) * theme._3,
      sineModAmount,
      sineAmp,
      () => lineControl(-0.2, 0.2))

    fmSawNote(0.2, 2, theme._4(second) * 1.01, theme._4(second) * theme._3 * 0.99,
      sineModAmount,
      sineAmp,
      () => lineControl(0.2, -0.2))

    // Variants with two notes, three notes, five notes
  }

  def testMood(note: Int = 5): Unit = {
    client.resetClock

    val carr: String = "c6"
    val mod: String = "fiss6"

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val fact = makeFact(carrier, modulator)
    val spectrum = makeSpectrum2(carrier, fact, 50)

    val panPos = 0
    val startTime = 0
    val duration = 5
    val modAmountMin: Double = 300
    val modAmountMax: Double = 3000

    {
      val fm = fmSineModulate(
        staticControl(spectrum(note)),
        sineOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
          staticControl(spectrum(note) * fact)),
        relativePercControl(0.001, 1, 0.001, Left(-8)))
        .addAction(TAIL_ACTION)

      val pan = panning(fm, staticControl(panPos))
        .addAction(TAIL_ACTION).withNrOfChannels(2)

      pan.getOutputBus.staticBus(0)

      val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }
  }

  def testGraceNote(note: Int = 5, graceNote: Int = 9, carr: String = "c6", mod: String = "fiss6"): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val fact = makeFact(carrier, modulator)
    val spectrum = makeSpectrum2(carrier, fact, 50)

    val panPos = 0
    val startTime = 0
    val duration = 5
    val modAmountMin: Double = 300
    val modAmountMax: Double = 3000

    {
      val fm = fmSineModulate(
        staticControl(spectrum(graceNote)),
        sineOsc(relativePercControl(10000, 20000, 0.001, Left(-8)),
          staticControl(spectrum(graceNote) * fact)),
        relativePercControl(0.001, 1, 0.001, Left(-8)))
        .addAction(TAIL_ACTION)

      val pan = panning(fm, staticControl(panPos))
        .addAction(TAIL_ACTION).withNrOfChannels(2)

      pan.getOutputBus.staticBus(0)

      val graph = pan.buildGraph(startTime, 0.5, pan.graph(Seq()))

      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }

    {
      val fm = fmSineModulate(
        staticControl(spectrum(note)),
        sineOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
          staticControl(spectrum(note) * fact)),
        relativePercControl(0.001, 1, 0.001, Left(-8)))
        .addAction(TAIL_ACTION)

      val pan = panning(fm, staticControl(panPos))
        .addAction(TAIL_ACTION).withNrOfChannels(2)

      pan.getOutputBus.staticBus(0)

      val graph = pan.buildGraph(startTime + 0.02, duration, pan.graph(Seq()))

      client.send(client.newBundle(absoluteTimeToMillis(startTime + 0.02), graph))
    }
  }

  def testNoiseAttack(modAmountMin: Double = 300, modAmountMax: Double = 3000, carr: String = "c6", mod: String = "fiss6"): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val panPos = 0
    val startTime = 0
    val duration = 5
    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

    val noise = whiteNoiseOsc(relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)
    val pan2 = panning(noise, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan2.getOutputBus.staticBus(0)

    val graph2 = pan2.buildGraph(startTime, 0.1, pan2.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph2))

  }

  def testSlowAttack(carr: String = "c6", mod: String = "fiss6"): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz(carr)
    val modulator = Note.noteToHertz(mod)
    val panPos = 0
    val startTime = 0
    val duration = 5
    val modAmountMin: Double = 300
    val modAmountMax: Double = 3000

    {
      val fm = fmSineModulate(
        staticControl(carrier),
        sineOsc(relativePercControl(modAmountMin, modAmountMax, 0.001, Left(-8)),
          staticControl(modulator)),
        relativePercControl(0.001, 1, 0.001, Left(-8)))
        .addAction(TAIL_ACTION)

      val pan = panning(fm, staticControl(panPos))
        .addAction(TAIL_ACTION).withNrOfChannels(2)

      pan.getOutputBus.staticBus(0)

      val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }

    {
      val fm = fmSineModulate(
        staticControl(carrier * 1.01),
        sineOsc(relativePercControl(modAmountMax, modAmountMax * 3, 0.2, Right(Instrument.SINE)),
          staticControl(modulator * 0.99)),
        relativePercControl(0.001, 1, 0.2, Right(Instrument.SINE)))
        .addAction(TAIL_ACTION)

      val filtered = highPassFilter(fm, staticControl(modulator * 0.99))
        .addAction(TAIL_ACTION)

      val pan = panning(filtered, lineControl(-0.2, 0.5))
        .addAction(TAIL_ACTION).withNrOfChannels(2)

      pan.getOutputBus.staticBus(0)

      val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }
  }

  def testVibrato(speed: Double = 5, min: Double = -2, max: Double = 2, duration: Double = 5): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz("c6")
    val modulator = Note.noteToHertz("fiss6")
    val panPos = 0
    val startTime = 0
    val fm = fmSineModulate(
      controlSum(staticControl(carrier), sineControl(staticControl(speed), min, max)),
      sineOsc(relativePercControl(300, 3000, 0.001, Left(-8)),
        staticControl(modulator)),
      relativePercControl(0.001, 1, 0.001, Left(-8)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }

  def testTremolo(speed: Double = 5, min: Double = 0.9, max: Double = 1.1): Unit = {
    client.resetClock

    val carrier = Note.noteToHertz("c6")
    val modulator = Note.noteToHertz("fiss6")
    val panPos = 0
    val startTime = 0
    val duration = 5
    val fm = fmSineModulate(
      staticControl(carrier),
      sineOsc(relativePercControl(300, 3000, 0.001, Left(-8)),
        staticControl(modulator)),
      controlMultiply(relativePercControl(0.001, 1, 0.001, Left(-8)),
        sineControl(staticControl(speed), min, max)))
      .addAction(TAIL_ACTION)

    val pan = panning(fm, staticControl(panPos))
      .addAction(TAIL_ACTION).withNrOfChannels(2)

    pan.getOutputBus.staticBus(0)

    val graph = pan.buildGraph(startTime, duration, pan.graph(Seq()))

    client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
  }
}
