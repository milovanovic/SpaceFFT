// SPDX-License-Identifier: Apache-2.0

package spacefft

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._

/* SpaceFFT parameters */
case class SpaceFFTParameters[T <: Data: Real: BinaryRepresentation] (
  // Range parameters
  win1DParams  : Option[WinParamsAndAddresses[T]],
  fft1DParams  : Option[FFTParamsAndAddresses[T]],
  mag1DParams  : Option[MagParamsAndAddresses[T]],
  acc1DParams  : Option[AccParamsAndAddresses[T]],
  cfar1DParams : Option[CFARParamsAndAddresses[T]],
  // Doppler parameters
  splitParams  : Option[SplitParamsAndAddresses],
  fft2DParams  : Option[FFTParamsAndAddresses[T]],
)

/* Windows parameters and addresses */
case class WinParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  winParams     : WindowingParams[T],
  winRAMAddress : AddressSet,
  winCSRAddress : AddressSet
)
/* FFT parameters and addresses */
case class FFTParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  fftParams  : FFTParams[T],
  fftAddress : AddressSet
)
/* Magnitude parameters and addresses */
case class MagParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  magParams  : MAGParams[T],
  magAddress : AddressSet
)
/* Accumulator parameters and addresses */
case class AccParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  accParams    : AccParams[T],
  accAddress   : AddressSet,
  accQueueBase : BigInt
)
/* CFAR parameters and addresses */
case class CFARParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  cfarParams  : CFARParams[T],
  cfarAddress : AddressSet
)
/* Splitter parameters and addresses */
case class SplitParamsAndAddresses (
  splitAddress : AddressSet
)

class AXI4SpaceFFT[T <: Data : Real: BinaryRepresentation](params: SpaceFFTParameters[T], beatBytes: Int)(implicit p: Parameters) extends SpaceFFT[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  /* Optional memory mapped port */
  val bus = if (blocks_1D.isEmpty) None else Some(LazyModule(new AXI4Xbar))
  override val mem = if (blocks_1D.isEmpty) None else Some(bus.get.node)
  for (b <- blocks_1D) {
    b.mem.foreach { _ := bus.get.node }
  }
}

abstract class SpaceFFT [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: SpaceFFTParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

  /* Type of Blocks */
  type Block = AXI4DspBlock

  /* Range */
  val win_1D  : Option[Block] = if (params.win1DParams  != None) Some(LazyModule(new WindowingBlock(csrAddress = params.win1DParams.get.winCSRAddress, ramAddress = params.win1DParams.get.winRAMAddress, params.win1DParams.get.winParams, beatBytes = beatBytes))) else None
  val fft_1D  : Option[Block] = if (params.fft1DParams  != None) Some(LazyModule(new AXI4FFTBlock(address = params.fft1DParams.get.fftAddress, params = params.fft1DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag_1D  : Option[Block] = if (params.mag1DParams  != None) Some(LazyModule(new AXI4LogMagMuxBlock(params.mag1DParams.get.magParams, params.mag1DParams.get.magAddress, _beatBytes = beatBytes))) else None
  val acc_1D  : Option[Block] = if (params.acc1DParams  != None) Some(LazyModule(new AXI4AccChainBlock(params.acc1DParams.get.accParams, params.acc1DParams.get.accAddress, params.acc1DParams.get.accQueueBase, beatBytes))) else None
  val cfar_1D : Option[Block] = if (params.cfar1DParams != None) Some(LazyModule(new AXI4CFARBlock(params.cfar1DParams.get.cfarParams, params.cfar1DParams.get.cfarAddress, _beatBytes = beatBytes))) else None
  /* Doppler */
  val split   : Option[Block] = if (params.fft2DParams  != None) Some(LazyModule(new AXI4Splitter(address = params.splitParams.get.splitAddress, beatBytes))) else None
  val fft_2D  : Option[Block] = if (params.fft2DParams  != None) Some(LazyModule(new AXI4FFTBlock(address = params.fft2DParams.get.fftAddress, params = params.fft2DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  
  /* Blocks */
  val blocks_1D: Seq[Block]  = Seq(win_1D, fft_1D, split, mag_1D, acc_1D, cfar_1D).flatten
  require(blocks_1D.length >= 1, "At least one block should exist")
  
  /* Connect nodes */
  lazy val connections = for (i <- 1 until blocks_1D.length) yield (blocks_1D(i), blocks_1D(i-1))
  for ((lhs, rhs) <- connections) {
    lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
  }

  /* Optional streamNode */
  val streamNode = NodeHandle(blocks_1D.head.streamNode, blocks_1D.last.streamNode)

  lazy val module = new LazyModuleImp(this) {}
}

trait AXI4SpaceFFTPins extends AXI4SpaceFFT[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  // streamNode
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}


class SpaceFFTParams(rangeFFTSize: Int = 512, dopplerFFTSize: Int = 256) {
  val params : SpaceFFTParameters[FixedPoint] = SpaceFFTParameters (
    // Range parameters
    win1DParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        numPoints = rangeFFTSize,
        dataWidth = 16,
        binPoint  = 10,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/blacman.txt",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
      ),
      winRAMAddress = AddressSet(0x60000000, 0xFFF),
      winCSRAddress = AddressSet(0x60001000, 0xFF)
    )),
    fft1DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = rangeFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(rangeFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
        minSRAMdepth = rangeFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 10
      ),
      fftAddress = AddressSet(0x60001100, 0xFF)
    )),
    mag1DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 10.BP),
        protoOut = FixedPoint(16.W, 10.BP),
        protoLog = Some(FixedPoint(16.W, 10.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 10,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60001200, 0xFF),
    )),
    acc1DParams = Some(AccParamsAndAddresses(
      accParams = AccParams(
        proto    = FixedPoint(16.W, 10.BP),
        protoAcc = FixedPoint(32.W, 10.BP),
      ),
      accAddress   = AddressSet(0x60001300, 0xFF),
      accQueueBase = 0x60002000
    )),
    cfar1DParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 10.BP),
        protoThreshold = FixedPoint(16.W, 10.BP),
        protoScaler = FixedPoint(16.W, 10.BP),
        leadLaggWindowSize = 64,
        guardWindowSize = 8,
        logOrLinReg = false,
        fftSize = rangeFFTSize,
        sendCut = true,
        minSubWindowSize = Some(4),
        includeCASH = true, //true
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,                  // number of add pipeline registers
        numMulPipes = 1                   // number of mull pipeline registers
      ),
      cfarAddress   = AddressSet(0x60001400, 0xFF),
    )),
    // Doppler parameters
    splitParams = Some(SplitParamsAndAddresses(
      splitAddress = AddressSet(0x60001500, 0xFF)
    )),
    fft2DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = dopplerFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
        minSRAMdepth = dopplerFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 10
      ),
      fftAddress = AddressSet(0x60001600, 0xFF)
    )),
  )
}

object SpaceFFTApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTParams).params
  val lazyDut = LazyModule(new AXI4SpaceFFT(params, 4) with AXI4SpaceFFTPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/SpaceFFT"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

