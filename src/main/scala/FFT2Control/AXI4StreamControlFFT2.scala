package spacefft.fft2control

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

case class FFT2ControlParams (
  rangeFFTSize: Int = 1024,
  dopplerFFTSize: Int =  256,
  addressGenDir: Boolean = true,
  numRxs: Int = 1,
  numTxs: Int = 1
)

trait AXI4FFT2ControlStandaloneBlock extends AXI4StreamFFT2ControlBlock {
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

abstract class FFT2ControlBlock [D, U, E, O, B <: Data] (params: FFT2ControlParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val rangeFFTSize = params.rangeFFTSize
    val dopplerFFTSize = params.dopplerFFTSize
    //First we fill ping and then pong, then it means that first we read from ping and then from pong
    //when pingPongWrite is on 1 pingPongRead is on 0 and we should read from ping

    //val rangeFFT = LazyModule(new AXI4FFTBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = beatBytes, configInterface = false))
    //val dopplerFFT = LazyModule(new AXI4FFTBlock(params.dopplerFFTParams, params.dopplerFFTAddress, _beatBytes = beatBytes, configInterface = false))

    val numChirps = RegInit(dopplerFFTSize.U(log2Ceil(dopplerFFTSize + 1).W))
    val numSamples = RegInit(rangeFFTSize.U(log2Ceil(rangeFFTSize + 1).W))
    val dopplerSize = RegInit(dopplerFFTSize.U(log2Ceil(dopplerFFTSize + 1).W))
    val numTxs = RegInit(params.numTxs.U(log2Ceil(params.numTxs + 1).W))
    // register for numChirps

    val cntSamplesRange = RegInit(0.U(log2Ceil(rangeFFTSize).W))
    val cntChirpsRange = RegInit(0.U(log2Ceil(dopplerFFTSize).W))
    val cntSamplesDoppler = RegInit(0.U(log2Ceil(dopplerFFTSize).W))
    val cntRepeatDoppler  = RegInit(0.U(log2Ceil(rangeFFTSize).W))

    val endOfFrame = RegInit(false.B)
    val radarDataMatrixSize = params.numTxs * dopplerFFTSize * rangeFFTSize //* params.numRxs - for each Rx separated block ram
    val pingPongWrite = RegInit(false.B)
    val pingPongRead = RegInit(false.B)
    val incrReadAddress = RegInit(false.B)

    val protoData = SInt(32.W) // params.rangeFFTParams.protoIQ //SInt(32.W)
    val radarDataMatrixPing = SyncReadMem(radarDataMatrixSize, protoData)
    val radarDataMatrixPong = SyncReadMem(radarDataMatrixSize, protoData)
    val writeAddress = WireDefault(0.U((log2Ceil(radarDataMatrixSize)).W))
    val readAddress =  RegInit(0.U((log2Ceil(radarDataMatrixSize)).W))
    val inFire = in.valid && in.ready // can be fft enable signal
    val triggerNewFrame = WireDefault(false.B)
    val writeFFT = WireDefault(false.B)
    val readFFT = WireDefault(false.B)
    val lastFlag = RegInit(false.B)

    when (cntSamplesRange === 0.U && cntChirpsRange === 0.U && inFire) {
      triggerNewFrame := true.B // duration is only one signal of clock
      writeFFT := true.B
    }
    .otherwise {
      triggerNewFrame := false.B
      writeFFT := false.B
    }

    when (inFire && ~pingPongWrite) {
      radarDataMatrixPing(writeAddress) := in.bits.data.asTypeOf(protoData)
    }
    when (inFire && pingPongWrite) {
      radarDataMatrixPong(writeAddress) := in.bits.data.asTypeOf(protoData)
    }

    when (inFire && cntSamplesRange === (numSamples - 1.U)) {
      cntSamplesRange := 0.U
      /*when (pingPongWrite === false.B) {
        radarDataMatrixPing(writeAddress) := in.bits.data.asTypeOf(protoData)
      }
      .otherwise {
        radarDataMatrixPong(writeAddress) := in.bits.data.asTypeOf(protoData)
      }*/
      when (cntChirpsRange === (numChirps - 1.U)) {
        cntChirpsRange := 0.U
        pingPongWrite := ~pingPongWrite // here we need to take care about correct PingPong
      }
      .otherwise {
        cntChirpsRange := cntChirpsRange + 1.U // for numTx equal to 1 or when beamforming mode is active
        // TODO: Adjust this logic to support multiple transcievers
      }
    }
    .elsewhen (inFire) {
      /*when (pingPongWrite === false.B) {
        radarDataMatrixPing(writeAddress) := in.bits.data.asTypeOf(protoData)
      }
      .otherwise {
        radarDataMatrixPong(writeAddress) := in.bits.data.asTypeOf(protoData)
      }*/
      cntSamplesRange := cntSamplesRange + 1.U
    }

    when ((cntSamplesDoppler === 0.U && cntRepeatDoppler === 0.U && pingPongRead =/= pingPongWrite) && out.ready === true.B) {
      readFFT := true.B
    }
    .otherwise {
      readFFT := false.B
    }

    when ((pingPongRead =/= pingPongWrite) && out.ready === true.B) { // simple Queue should be added at the output to handle
      when (cntSamplesDoppler === (numChirps - 1.U) && out.ready === true.B) {
        cntSamplesDoppler := 0.U
        when (cntRepeatDoppler === (numSamples - 1.U)) {
          cntRepeatDoppler := 0.U
          lastFlag := true.B
          pingPongRead := ~pingPongRead
        }
        .otherwise {
          lastFlag := false.B
          cntRepeatDoppler := cntRepeatDoppler + 1.U
        }
      }
      .otherwise {
        lastFlag := false.B
        cntSamplesDoppler := cntSamplesDoppler + 1.U
      }
    }
    .otherwise {
      lastFlag := false.B
    }

    if (params.addressGenDir) {
      writeAddress := cntSamplesRange + cntChirpsRange*numSamples
    }
    else {
      writeAddress := cntChirpsRange + cntSamplesRange*numChirps
    }

    if (params.addressGenDir) {
      readAddress := cntRepeatDoppler + cntSamplesDoppler * numSamples
    }
    else {
      readAddress := cntSamplesDoppler + cntRepeatDoppler * numChirps
    }
    // TODO: Make this optional so if run-time mode is off that
    val fields = Seq(
      // settable registers
      RegField(log2Ceil(rangeFFTSize + 1), numSamples,
        RegFieldDesc(name = "numSamples", desc = "Configure number of samples inside chirp")),
      RegField(log2Ceil(dopplerFFTSize + 1), numChirps,
        RegFieldDesc(name = "numChirps", desc = "Configure number of chirps")),
      RegField(log2Ceil(dopplerFFTSize + 1), dopplerSize,
        RegFieldDesc(name = "dopplerFFTSize", desc = "Configure doppler FFT size")) // this register is added because numChirps is not mandatory equal to numChirps - this should be next step!
    )

    regmap(
      fields.zipWithIndex.map({ case (f, i) =>
        i * beatBytes -> Seq(f)
      }): _*
    )
    val readAddressPing = Wire(readAddress.cloneType)
    val readAddressPong = Wire(readAddress.cloneType)

    readAddressPing := readAddress
    readAddressPong := readAddress
    dontTouch(readAddressPing)
    readAddressPing.suggestName("read_address_ping")
    dontTouch(readAddressPong)
    readAddressPong.suggestName("read_address_pong")
    // read from memory
    val memPingData = radarDataMatrixPing(readAddressPing)
    val memPongData = radarDataMatrixPong(readAddressPong)

    val outQueue =  Module(new Queue(chiselTypeOf(out.bits.data), entries = 2, pipe = true, flow = true))
    outQueue.io.enq.bits := Mux(RegNext(RegNext(pingPongRead, false.B)), memPongData, memPingData).asUInt

    outQueue.io.enq.valid := RegNext(RegNext(pingPongRead =/= pingPongWrite, false.B)) // only this can be
    outQueue.io.deq.ready := out.ready
    out.bits.data  := outQueue.io.deq.bits
    out.valid      := outQueue.io.deq.valid

    // generate last flag after each frame!
    val outQueueLast = Module(new Queue(chiselTypeOf(out.bits.last), entries = 1, pipe = true, flow = true))
    outQueueLast.io.enq.bits  := RegNext(lastFlag, false.B)
    outQueueLast.io.enq.valid := RegNext(RegNext(pingPongRead =/= pingPongWrite, false.B))
    outQueueLast.io.deq.ready := out.ready
    out.bits.last := outQueueLast.io.deq.bits

    in.ready     := true.B //out.ready -> maybe should be organized differently
  }
}

class AXI4StreamFFT2ControlBlock(params: FFT2ControlParams, address: AddressSet, _beatBytes: Int = 4) (implicit p: Parameters) extends FFT2ControlBlock[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

object FFT2ControlDspBlockAXI4 extends App
{
  val paramsFFT2Control: FFT2ControlParams = FFT2ControlParams()
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(new AXI4StreamFFT2ControlBlock(paramsFFT2Control, AddressSet(0x00000, 0xFF), _beatBytes = 4) with AXI4FFT2ControlStandaloneBlock)
  
  (new ChiselStage).execute(Array("--target-dir", "verilog/AXI4StreamFFT2ControlBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
