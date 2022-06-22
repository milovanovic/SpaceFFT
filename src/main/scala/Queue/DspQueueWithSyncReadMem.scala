package spacefft.queue

import chisel3._
import chisel3.util._

import dspblocks._ // included because of DspQueue

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

case class DspQueueCustomParams(
  queueDepth: Int = 16384,
  progFull: Boolean = false,
  useSyncReadMem: Boolean = true,
  useBlockRam: Boolean = true,
  addEnProgFullOut: Boolean = false,
  initEnProgFull: Boolean = true,
  genLastAfterN: Int = 256,
  enLastGen: Boolean = false
) {
  // require(enLastGen == true.B || progFull == true.B, "Enable last or programmable full register needs to be included")
}

trait AXI4DspQueueStandaloneBlock extends AXI4DspQueueWithSyncReadMem {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

// This block is implemented mostly for debug purposes

abstract class DspQueueWithSyncReadMem [D, U, E, O, B <: Data] (val params: DspQueueCustomParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
  val streamNode = AXI4StreamIdentityNode()
  val depth = params.queueDepth

  lazy val module = new LazyModuleImp(this) {
    val (streamIn, _)  = streamNode.in(0)
    val (streamOut, _) = streamNode.out(0)
    val queuedStream = Module(new QueueWithSyncReadMem(chiselTypeOf(streamIn.bits), entries = depth, useSyncReadMem = params.useSyncReadMem, useBlockRam = params.useBlockRam, flow = false, pipe = true)) // pipe was true here, be carefu

    queuedStream.io.enq.valid := streamIn.valid
    queuedStream.io.enq.bits.data := streamIn.bits.data
    queuedStream.io.enq.bits.strb := DontCare
    queuedStream.io.enq.bits.keep := DontCare
    queuedStream.io.enq.bits.id := DontCare
    queuedStream.io.enq.bits.dest := DontCare
    queuedStream.io.enq.bits.last := streamIn.bits.last
    queuedStream.io.enq.bits.user := DontCare

    streamIn.ready := queuedStream.io.enq.ready
    streamOut.bits := queuedStream.io.deq.bits

    //val enProgFullReg = if (params.addEnProgFullOut) Some(IO(Output(Bool()))) else None

    //if (params.addEnProgFullOut) {
      //enProgFullReg.get := enProgFull
    //}

    var commonFields = Seq[RegField]()

    if (params.progFull) {
      val queueProgFullVal = RegInit((depth-1).U(log2Ceil(depth).W))
      val enProgFull = RegInit(params.initEnProgFull.B)
      val progFull = RegInit(false.B)

      commonFields = commonFields :+ RegField(32, queueProgFullVal,
        RegFieldDesc("queueProgFullVal", "Fill queue even though output is ready to accept data"))
      commonFields = commonFields :+ RegField(1, enProgFull,
        RegFieldDesc("enProgFull", "Enable programable full logic"))

      when (queuedStream.io.count === queueProgFullVal) {
        progFull := true.B
      }
      when (queuedStream.io.count === 0.U) {
        progFull := false.B
      }
      when (enProgFull) {
        queuedStream.io.deq.ready := Mux(progFull, streamOut.ready, false.B)
        streamOut.valid := Mux(progFull, queuedStream.io.deq.valid, false.B)
      }
      .otherwise {
        streamOut.valid := queuedStream.io.deq.valid
        queuedStream.io.deq.ready := streamOut.ready
      }
    }
    else {
      streamOut.valid := queuedStream.io.deq.valid
      queuedStream.io.deq.ready := streamOut.ready
    }

    if (params.enLastGen) {
      val cntOut = RegInit(0.U(log2Ceil(params.genLastAfterN + 1).W))
      val genLastAfterNReg = RegInit(256.U(log2Ceil(params.genLastAfterN + 1).W))
      val outFire = streamOut.valid && streamOut.ready

      dontTouch(cntOut)
      cntOut.suggestName("cntOut")

      commonFields = commonFields :+ RegField(32, genLastAfterNReg,
        RegFieldDesc("genLastAfterNReg", "Fill queue even though output is ready to accept data"))

      when (cntOut === (genLastAfterNReg - 1.U) && outFire) {
        cntOut := 0.U
      }
      .elsewhen (outFire) {
        cntOut := cntOut + 1.U
      }
      when (cntOut === (genLastAfterNReg - 1.U)) {
        streamOut.bits.last := true.B
      }
      .otherwise {
        streamOut.bits.last := false.B
      }
    }

    val queueThr = RegInit(0.U(log2Ceil(params.queueDepth).W))
    val overflowThr = RegInit(false.B)

    commonFields = commonFields :+ RegField(32, queueThr,
                      RegFieldDesc("queueThr", "Threshold value for setting status register"))
    overflowThr := queueThr < queuedStream.io.count
    commonFields = commonFields :+ RegField.r(32, overflowThr,
                      RegFieldDesc("overflowThr", "Number of elements inside queue is higher than queueThr value indicator"))


    if (params.enLastGen || params.progFull)
      regmap(commonFields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)

    // TODO: Make whole memory mapped optional
    /*regmap(0 ->
      Seq(RegField(32, queueProgFullVal,
        RegFieldDesc("queueProgFullVal", "Fill queue even though output is ready to accept data"))),
      32 ->
      Seq(RegField(1, enProgFull,
        RegFieldDesc("enProgFull", "Enable programable full logic"))))*/

  }
}

class AXI4DspQueueWithSyncReadMem(params: DspQueueCustomParams, address: AddressSet, _beatBytes: Int = 4)(implicit p: Parameters) extends DspQueueWithSyncReadMem[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

object DspQueueApp extends App
{
  val params: DspQueueCustomParams = DspQueueCustomParams(queueDepth = 16534) //DspQueueCustomParams(queueDepth = 131074, progFull = true)

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val queueModule = LazyModule(new AXI4DspQueueWithSyncReadMem(params, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4) with AXI4DspQueueStandaloneBlock)
  chisel3.Driver.execute(args, ()=> queueModule.module)
}

