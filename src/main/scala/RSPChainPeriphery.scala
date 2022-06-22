// SPDX-License-Identifier: Apache-2.0

package hyperspace.rspchain

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.{Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem

import dsptools.numbers._

/* RSPChain FixedPoint Key */
case object RSPChainKey extends Field[Option[RSPChainParameters[FixedPoint]]](None)

trait CanHavePeripheryRSPChain { this: BaseSubsystem =>
  private val portName = "rspChain"

  // Only build if we are using the TL (nonAXI4) version
  val rspchain = p(RSPChainKey) match {
    case Some(params) => {
      val rspchain = LazyModule(new AXI4RSPChain(params, pbus.beatBytes))
      pbus.toSlave(Some(portName)) {
        // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
        rspchain.mem.get := AXI4Buffer () := TLToAXI4 () := TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true)
      }
      // streamNode
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = pbus.beatBytes)))
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

      ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := rspchain.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = pbus.beatBytes)) := ioInNode

      val rspchain_in = InModuleBody { ioInNode.makeIO() }
      val rspchain_out = InModuleBody { ioOutNode.makeIO() }
      
      // return
      Some(Seq(rspchain_in, rspchain_out))
    }
    case None => None
  }
}

trait CanHavePeripheryRSPChainModuleImp extends LazyModuleImp{
  val outer: CanHavePeripheryRSPChain
}

class RSPChainIO[T <: Data](private val gen1: T, private val gen2: T) extends Bundle {
  val in = DataMirror.internal.chiselTypeClone[T](gen1)
  val out = Flipped(DataMirror.internal.chiselTypeClone[T](gen2))
}

/* Mixin to add RSPChain to rocket config */
class WithRSPChain extends Config((site, here, up) => {
  case RSPChainKey => Some((new RSPChainParams).params)
})


case object RSPChainAdapter {
  def tieoff(rspchain: Option[RSPChainIO[AXI4StreamBundle]]) {
    rspchain.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(rspchain: RSPChainIO[AXI4StreamBundle]) { tieoff(Some(rspchain)) }
}