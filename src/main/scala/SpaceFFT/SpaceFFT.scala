// SPDX-License-Identifier: Apache-2.0

package spacefft

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{IO, FixedPoint}

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import spacefft.ddrwrapper._

import dsputils._

import crc._
import xWRDataPreProc._
import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._

/* SpaceFFT parameters */
case class SpaceFFTParameters[T <: Data: Real: BinaryRepresentation] (
  // Range parameters
  crc1DParams  : Option[CRCParamsAndAddresses],
  prep1DParams : Option[PreprocParamsAndAddresses],
  win1DParams  : Option[WinParamsAndAddresses[T]],
  fft1DParams  : Option[FFTParamsAndAddresses[T]],
  mag1DParams  : Option[MagParamsAndAddresses[T]],
  acc1DParams  : Option[AccParamsAndAddresses[T]],
  cfar1DParams : Option[CFARParamsAndAddresses[T]],
  // Doppler parameters
  splitParams  : Option[SplitParamsAndAddresses],
  queueParams  : Option[QueueParamsAndAddresses],
  fft2DParams  : Option[FFTParamsAndAddresses[T]],
  mag2DParams  : Option[MagParamsAndAddresses[T]]
)

/* CRC parameters and addresses */
case class CRCParamsAndAddresses(
  crcParams  : MiltipleCrcBlockParams,
  crcAddress : AddressSet
)
/* Pre-processing parameters and addresses */
case class PreprocParamsAndAddresses(
  prepParams  : AXI4XwrDataPreProcParams,
  prepAddress : AddressSet
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
/* Queue parameters and addresses */
case class QueueParamsAndAddresses (
  queueParams  : DspQueueCustomParams,
  queueAddress : AddressSet
)
/* Splitter parameters and addresses */
case class SplitParamsAndAddresses (
  splitAddress : AddressSet
)

class AXI4SpaceFFT[T <: Data : Real: BinaryRepresentation](params: SpaceFFTParameters[T], beatBytes: Int)(implicit p: Parameters) extends SpaceFFT[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  /* Optional memory mapped port */
  val bus = if (blocks.isEmpty) None else Some(LazyModule(new AXI4Xbar))
  override val mem = if (blocks.isEmpty) None else Some(bus.get.node)
  for (b <- blocks) {
    b.mem.foreach { _ := bus.get.node }
  }
}

abstract class SpaceFFT [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: SpaceFFTParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

  /* Type of Blocks */
  type Block = AXI4DspBlock

  /* Range */
  val crc_1D  = if (params.crc1DParams  != None) Some(LazyModule(new AXI4MiltipleCrcBlock(params.crc1DParams.get.crcParams, params.crc1DParams.get.crcAddress, beatBytes){
    // pins
    def makeCustomIO(): MiltipleCrcBlockIO = {
      val io2: MiltipleCrcBlockIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })) else None
  val preproc : Option[Block] = if (params.prep1DParams != None) Some(LazyModule(new AXI4xWRdataPreProcBlock(params.prep1DParams.get.prepAddress, params.prep1DParams.get.prepParams, beatBytes))) else None
  val win_1D  : Option[Block] = if (params.win1DParams  != None) Some(LazyModule(new WindowingBlock(csrAddress = params.win1DParams.get.winCSRAddress, ramAddress = params.win1DParams.get.winRAMAddress, params.win1DParams.get.winParams, beatBytes = beatBytes))) else None
  val fft_1D  : Option[Block] = if (params.fft1DParams  != None) Some(LazyModule(new AXI4FFTBlock(address = params.fft1DParams.get.fftAddress, params = params.fft1DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag_1D  : Option[Block] = if (params.mag1DParams  != None) Some(LazyModule(new AXI4LogMagMuxBlock(params.mag1DParams.get.magParams, params.mag1DParams.get.magAddress, _beatBytes = beatBytes))) else None
  val acc_1D  : Option[Block] = if (params.acc1DParams  != None) Some(LazyModule(new AXI4AccChainBlock(params.acc1DParams.get.accParams, params.acc1DParams.get.accAddress, params.acc1DParams.get.accQueueBase, beatBytes))) else None
  val cfar_1D : Option[Block] = if (params.cfar1DParams != None) Some(LazyModule(new AXI4CFARBlock(params.cfar1DParams.get.cfarParams, params.cfar1DParams.get.cfarAddress, _beatBytes = beatBytes))) else None
  
  /* Doppler */
  val fft_2D : Option[Block] = if (params.fft1DParams != None && params.fft2DParams != None) Some(LazyModule(new AXI4FFTBlock(address = params.fft2DParams.get.fftAddress, params = params.fft2DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag_2D : Option[Block] = if (params.mag2DParams != None && params.fft1DParams != None && params.fft2DParams != None) Some(LazyModule(new AXI4LogMagMuxBlock(params.mag2DParams.get.magParams, params.mag2DParams.get.magAddress, _beatBytes = beatBytes))) else None
  
  val split = if (params.fft1DParams != None && params.fft2DParams != None) Some(LazyModule(new AXI4Splitter(address = params.splitParams.get.splitAddress, beatBytes){
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
    val out = InModuleBody { ioOutNode.makeIO() }
  })) else None
  
  val queue = if (params.fft1DParams != None && params.fft2DParams != None) Some(LazyModule(new AXI4DspQueueWithSyncReadMem(params.queueParams.get.queueParams, params.queueParams.get.queueAddress, _beatBytes = beatBytes){
    // streamNode
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
    val in = InModuleBody { ioInNode.makeIO() }
  })) else None

  /* Blocks */
  val blocks_1D: Seq[Block] = Seq(crc_1D, preproc, win_1D, fft_1D, split, mag_1D, acc_1D, cfar_1D).flatten
  val blocks_2D: Seq[Block] = Seq(queue, fft_2D, mag_2D).flatten
  val blocks   : Seq[Block] = blocks_1D ++ blocks_2D
  require(blocks_1D.length >= 1, "At least one block should exist")
  
  /* Connect nodes */
  lazy val connections_1D = for (i <- 1 until blocks_1D.length) yield (blocks_1D(i), blocks_1D(i-1))
  for ((lhs, rhs) <- connections_1D) {
    lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
  }
  lazy val connections_2D = for (i <- 1 until blocks_2D.length) yield (blocks_2D(i), blocks_2D(i-1))
  for ((lhs, rhs) <- connections_2D) {
    lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
  }

  /* Optional streamNode */
  val streamNode = NodeHandle(blocks_1D.head.streamNode, blocks_1D.last.streamNode)

  /* Optional CRC pins */
  lazy val io = if (crc_1D != None) Some(crc_1D.get.ioBlock) else None

  lazy val module = new LazyModuleImp(this) {
    val ddr4CtrlrWrapper = Module(new ddr4CtrlrWrapper)

    /////////////// RANGE ///////////////////////

    ddr4CtrlrWrapper.io.s_axis_tdata_r_0  := split.get.out.bits.data
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_0 := split.get.out.valid
    ddr4CtrlrWrapper.io.s_axis_tlast_r_0  := split.get.out.bits.last
    split.get.out.ready := true.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_1  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_1 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_1  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_2  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_2 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_2  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_3  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_3 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_3  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_4  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_4 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_4  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_5  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_5 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_5  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_6  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_6 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_6  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_7  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_7 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_7  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_8  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_8 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_8  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_9  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_9 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_9  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_10  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_10 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_10  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_11  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_11 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_11  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_12  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_12 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_12  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_13  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_13 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_13  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_14  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_14 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_14  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_r_15  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_15 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_r_15  := false.B

    ///////////////// DOPPLER /////////////////////

    ddr4CtrlrWrapper.io.s_axis_tdata_d_0  := 0.U //dopplerFFTWrapper.module.io.data_out
    // false if we do not want to to use virtual fifo
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_0 := false.B //dopplerFFTWrapper.module.io.valid_out //false.B //dopplerFFTWrapper.module.io.valid_out//false.B //dopplerFFTWrapper.module.io.valid_out // disable virtual fifo writing
    ddr4CtrlrWrapper.io.s_axis_tlast_d_0  := false.B //dopplerFFTWrapper.module.io.last_out


    ddr4CtrlrWrapper.io.s_axis_tdata_d_1  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_1 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_1  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_2  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_2 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_2  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_3  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_3 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_3  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_4  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_4 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_4  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_5  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_5 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_5  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_6  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_6 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_6  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_7  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_7 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_7  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_8  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_8 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_8  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_9  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_9 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_9  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_10  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_10 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_10  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_11  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_11 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_11  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_12  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_12 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_12  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_13  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_13 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_13  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_14  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_14 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_14  := false.B

    ddr4CtrlrWrapper.io.s_axis_tdata_d_15  := 0.U
    ddr4CtrlrWrapper.io.s_axis_tvalid_d_15 := false.B
    ddr4CtrlrWrapper.io.s_axis_tlast_d_15  := false.B

    ///////////////////////////////////////////////////
    ////////////////////// Doppler OUT /////////////////////////////////////////////
    // drive receiver one to doppler fft!

    queue.get.in.bits.data  := ddr4CtrlrWrapper.io.m_axis_tdata_d_0
    queue.get.in.valid := ddr4CtrlrWrapper.io.m_axis_tvalid_d_0
    queue.get.in.bits.last  := ddr4CtrlrWrapper.io.m_axis_tlast_d_0
    ddr4CtrlrWrapper.io.m_axis_tready_d_0 := queue.get.in.ready

    val mem_reset = IO(Input(Bool()))
    val reset_n = IO(Input(Bool()))

    val ddr4IO = IO(new ddr4IO())
    val ethIO = IO(new ethIO_ddr4())
    //val ethIO = IO(new ethIO())

    val clk_300_p = IO(Input(Bool()))
    val clk_300_n = IO(Input(Bool()))

    ddr4CtrlrWrapper.io.s_axis_aclk := clock
    ddr4CtrlrWrapper.io.m_axis_aclk := clock
    ddr4CtrlrWrapper.io.s_axis_aresetn := reset_n
    ddr4CtrlrWrapper.io.mem_reset := mem_reset

    // ethernet signals
    ethIO.o_data_eth := ddr4CtrlrWrapper.io.o_data_eth
    ethIO.o_start_eth := ddr4CtrlrWrapper.io.o_start_eth
    ethIO.o_we_eth := ddr4CtrlrWrapper.io.o_we_eth
    ddr4CtrlrWrapper.io.i_ready_eth := ethIO.i_ready_eth

    //////////////////////////////////////////// ddr4 memory specific signals///////////////////////

    // input connections
   // ddr4CtrlrWrapper.io.clk_ref := ddr4IO.clk_ref
    ddr4CtrlrWrapper.io.sys_clk := ddr4IO.sys_clk
    ddr4CtrlrWrapper.io.clk_300_p := clk_300_p
    ddr4CtrlrWrapper.io.clk_300_n := clk_300_n

    // inout connections
    ddr4CtrlrWrapper.io.c0_ddr4_dq <> ddr4IO.c0_ddr4_dq
    ddr4CtrlrWrapper.io.c0_ddr4_dm_dbi_n <> ddr4IO.c0_ddr4_dm_dbi_n
    ddr4CtrlrWrapper.io.c0_ddr4_dqs_c <> ddr4IO.c0_ddr4_dqs_c
    ddr4CtrlrWrapper.io.c0_ddr4_dqs_t <> ddr4IO.c0_ddr4_dqs_t

    // output connections
    ddr4IO.o_MemClk_p  := ddr4CtrlrWrapper.io.o_MemClk_p
    ddr4IO.c0_ddr4_ba := ddr4CtrlrWrapper.io.c0_ddr4_ba
    ddr4IO.c0_ddr4_reset_n := ddr4CtrlrWrapper.io.c0_ddr4_reset_n
    ddr4IO.c0_ddr4_cs_n := ddr4CtrlrWrapper.io.c0_ddr4_cs_n
    ddr4IO.c0_ddr4_odt := ddr4CtrlrWrapper.io.c0_ddr4_odt
    ddr4IO.c0_ddr4_bg := ddr4CtrlrWrapper.io.c0_ddr4_bg
    ddr4IO.c0_ddr4_act_n := ddr4CtrlrWrapper.io.c0_ddr4_act_n
    ddr4IO.c0_ddr4_cke := ddr4CtrlrWrapper.io.c0_ddr4_cke
    ddr4IO.c0_ddr4_ck_c := ddr4CtrlrWrapper.io.c0_ddr4_ck_c
    ddr4IO.c0_ddr4_ck_t := ddr4CtrlrWrapper.io.c0_ddr4_ck_t
    ddr4IO.c0_ddr4_adr := ddr4CtrlrWrapper.io.c0_ddr4_adr

    val c0_init_calib_complete = IO(Output(Bool()))

    c0_init_calib_complete := ddr4CtrlrWrapper.io.c0_init_calib_complete
  }
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
  val ioInNode_1D  = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode_1D = BundleBridgeSink[AXI4StreamBundle]()
  val ioOutNode_2D = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode_1D := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode_1D
  ioOutNode_2D := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := blocks_2D.last.streamNode

  val in_1D = InModuleBody { ioInNode_1D.makeIO() }
  val out_1D = InModuleBody { ioOutNode_1D.makeIO() }
  val out_2D = InModuleBody { ioOutNode_2D.makeIO() }

  // pins
  def makeCustomIO(): MiltipleCrcBlockIO = {
    val io2: MiltipleCrcBlockIO = IO(io.get.cloneType)
    io2.suggestName("io")
    io2 <> io.get
    io2
  }
  val ioBlock = if(crc_1D != None) InModuleBody { makeCustomIO() } else None
}


class SpaceFFTParams(rangeFFTSize: Int = 512, dopplerFFTSize: Int = 256) {
  val params : SpaceFFTParameters[FixedPoint] = SpaceFFTParameters (
    // Range parameters
    crc1DParams = Some(CRCParamsAndAddresses(
      crcParams = MiltipleCrcBlockParams(
        crcParams16 = Some(RadarCRCParams(dataWidth = 16)),
        crcParams12 = Some(RadarCRCParams(dataWidth = 12)),
        crcParams14 = Some(RadarCRCParams(dataWidth = 14)),
      ),
      crcAddress = AddressSet(0x60000000, 0xFF)
    )),
    prep1DParams = Some(PreprocParamsAndAddresses(
      prepParams = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFTSize, useBlockRam = true),
      prepAddress = AddressSet(0x60000100, 0xFF)
    )),
    win1DParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        numPoints = rangeFFTSize,
        dataWidth = 16,
        binPoint  = 14,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/blacman.txt",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
      ),
      winRAMAddress = AddressSet(0x60001000, 0xFFF),
      winCSRAddress = AddressSet(0x60000200, 0xFF)
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
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000300, 0xFF)
    )),
    mag1DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000400, 0xFF),
    )),
    acc1DParams = Some(AccParamsAndAddresses(
      accParams = AccParams(
        proto    = FixedPoint(16.W, 14.BP),
        protoAcc = FixedPoint(32.W, 14.BP),
      ),
      accAddress   = AddressSet(0x60000500, 0xFF),
      accQueueBase = 0x60002000
    )),
    cfar1DParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 14.BP),
        protoThreshold = FixedPoint(16.W, 14.BP),
        protoScaler = FixedPoint(16.W, 14.BP),
        leadLaggWindowSize = 64,
        guardWindowSize = 8,
        logOrLinReg = false,
        retiming = false,
        fftSize = rangeFFTSize,
        sendCut = true,
        minSubWindowSize = None,
        includeCASH = false,
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,                  // number of add pipeline registers
        numMulPipes = 1                   // number of mull pipeline registers
      ),
      cfarAddress   = AddressSet(0x60000600, 0xFF),
    )),
    // Doppler parameters
    queueParams = Some(QueueParamsAndAddresses(
      queueParams = DspQueueCustomParams(
        queueDepth = dopplerFFTSize, // should be the same as max dopplerFFTSize
        progFull = false,
        addEnProgFullOut = false,
        useSyncReadMem = false, // do not use distributed ram, trying to eliminate timing issues
        enLastGen = false
      ),
      queueAddress = AddressSet(0x60003000, 0xFFF)
    )),
    splitParams = Some(SplitParamsAndAddresses(
      splitAddress = AddressSet(0x60000700, 0xFF)
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
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000800, 0xFF)
    )),
    mag2DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000900, 0xFF),
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

