package net.soundmining

import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir

object ModuleMusic5 {
  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
  }
}
