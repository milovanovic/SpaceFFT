// SPDX-License-Identifier: Apache-2.0

package spacefft.splitter

import chisel3._
import dsptools._
import dsptools.numbers._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

abstract class Splitter[D, U, E, O, B <: Data](beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamNexusNode(
    masterFn = { seq => seq.reduce({ (a: AXI4StreamMasterPortParameters, b: AXI4StreamMasterPortParameters) => AXI4StreamMasterPortParameters(a.masterParams.union(b.masterParams))}) },
    slaveFn  = { seq => seq.reduce({ (_: AXI4StreamSlavePortParameters, b: AXI4StreamSlavePortParameters)  => AXI4StreamSlavePortParameters (b.slaveParams .union(b.slaveParams)) }) }
  )

  lazy val module = new LazyModuleImp(this) {
    
    // Registers
    val ctrlReg = RegInit(0.U((beatBytes*8).W))
    val maskReg = RegInit(0.U((beatBytes*8).W))
    // Define register fields
    val fields = Seq(
      // settable registers
      RegField(beatBytes*8, ctrlReg, RegFieldDesc(name = "ctrlReg", desc = "control register")),
      RegField(beatBytes*8, maskReg, RegFieldDesc(name = "maskReg", desc = "mask register"))
    )

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

    require(streamNode.in.length == 1, "Only one input to splitter allowed")
    require(streamNode.out.length <= beatBytes*4, "Number of outputs should be equal or smaller then number of bits in mask register")

    val (in, _)   = streamNode.in.head

    val outs = streamNode.out.map{case (out, edge) => out.ready}
    val readyOR  = outs.reduce(_ || _)
    val readyAND = outs.reduce(_ && _)
    val ready = Wire(Bool())

    when (ctrlReg === 0.U) {
      ready := readyAND
    }
    .elsewhen (ctrlReg === 1.U) {
      ready := readyOR
    }
    .otherwise {
      ready := false.B
    }

    in.ready := ready

    streamNode.out.zipWithIndex.foreach { case ((out, edge), i) =>
        out.valid := in.valid && !maskReg(i).asBool
        out.bits  := in.bits
    }
  }
}

class AXI4Splitter(address: AddressSet, beatBytes: Int = 8)(implicit p: Parameters) extends Splitter[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](beatBytes) with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes)) // use AXI4 memory mapped
}

object splitterApp extends App
{

  implicit val p: Parameters = Parameters.empty
  val standaloneModule = LazyModule(new AXI4Splitter(AddressSet(0x0000, 0xFF), 4) {

    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { m => {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }}

    val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until 1) yield {
      implicit val valName = ValName(s"inIOs_$i")
      val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 8)))
      streamNode :=
        BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters())) :=
        in
      InModuleBody { in.makeIO() }
    }
    val outIOs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until 2) yield {
      implicit val valName = ValName(s"outIOs_$o")
      val out = BundleBridgeSink[AXI4StreamBundle]()
      out :=
        AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) :=
        streamNode
      InModuleBody { out.makeIO() }
    }
  })
  
  chisel3.Driver.execute(Array("--target-dir", "verilog/Splitter", "--top-name", "Splitter"), ()=> standaloneModule.module) // generate verilog code
}