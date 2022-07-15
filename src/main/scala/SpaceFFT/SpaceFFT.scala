// SPDX-License-Identifier: Apache-2.0

package spacefft

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, FixedPoint}

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import spacefft.ddrwrapper._
import spacefft.fft2control._

import dsputils._

import lvdsphy._
import crc._
import xWRDataPreProc._
import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._
import hdmi.scope._

class AXI4SpaceFFT[T <: Data : Real: BinaryRepresentation](params: SpaceFFTParameters[T], beatBytes: Int)(implicit p: Parameters) extends SpaceFFT[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) {
  /* Optional memory mapped port */
  val bus = if (blocks.isEmpty) None else Some(LazyModule(new AXI4Xbar))
  val mem = if (blocks.isEmpty) None else Some(bus.get.node)
  for (b <- blocks) {
    b.mem.foreach { _ := bus.get.node }
  }
  if (scope != None) {
    scope.get.mem.get := bus.get.node
  }
}

class SpaceFFTIO(val phy: Boolean, val crc: Boolean, val scope: Boolean) extends Bundle {
  val i_data   = if (phy) Some(Input(UInt(8.W))) else None
  val i_valid  = if (phy) Some(Input(UInt(8.W))) else None
  val i_frame  = if (phy) Some(Input(UInt(8.W))) else None

  // asyncFIFO signals
  val i_async_clock = if (phy) Some(Input(Clock())) else None
  val i_async_reset = if (phy) Some(Input(Bool())) else None

  val word_size = if (crc == true && phy == false) Some(Input(UInt(2.W))) else None
  val crc_en    = if (crc == true && phy == false) Some(Input(UInt(1.W))) else None

  // tmds output ports
  val clk_p  = if (scope) Some(Output(Bool())) else None
  val clk_n  = if (scope) Some(Output(Bool())) else None
  val data_p = if (scope) Some(Output(UInt(3.W))) else None
  val data_n = if (scope) Some(Output(UInt(3.W))) else None

  // Reset and clock
  val clk_pixel  = if (scope) Some(Input(Clock())) else None
  val clk_serdes = if (scope) Some(Input(Clock())) else None
  val reset_hdmi = if (scope) Some(Input(Bool())) else None
}
object SpaceFFTIO {
  def apply(phy: Boolean, crc: Boolean, scope: Boolean): SpaceFFTIO = new SpaceFFTIO(phy, crc, scope)
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
  val ioInNode_1D  = if (lvdsphy == None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))) else None
  val ioOutNode_1D = if (scope == None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
  val ioOutNode_2D = if (scope == None) Some( BundleBridgeSink[AXI4StreamBundle]()) else None

  if (ioInNode_1D != None)  { streamNode.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode_1D.get }
  if (ioOutNode_1D != None) { ioOutNode_1D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode.get }
  if (ioOutNode_2D != None) { ioOutNode_2D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := blocks_2D.last.streamNode }

  val in_1D  = if (ioInNode_1D != None)  InModuleBody { ioInNode_1D.get.makeIO()  } else None
  val out_1D = if (ioOutNode_1D != None) InModuleBody { ioOutNode_1D.get.makeIO() } else None
  val out_2D = if (ioOutNode_2D != None) InModuleBody { ioOutNode_2D.get.makeIO() } else None

  // pins
  def makeSpaceFFTIO(): SpaceFFTIO = {
    val io2: SpaceFFTIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = if (lvdsphy != None || crc_1D != None || scope != None) Some(InModuleBody { makeSpaceFFTIO() }) else None
}

abstract class SpaceFFT [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: SpaceFFTParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) {
  /* 2D fft condition */
  val fft2DCond = params.fft1DParams != None && params.fft2DParams != None
  /* DDR condition */
  val ddrCond = fft2DCond && (params.ddrParams != None)

  /* Range */
  val lvdsphy = if (params.lvds1DParams != None) Some(LazyModule(new AXI4StreamDataRX(params.lvds1DParams.get.lvdsphyParams){
      def makeCustomIO(): DataRXIO = {
      val io2: DataRXIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })) else None
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
  val preproc = if (params.prep1DParams != None) Some(LazyModule(new AXI4xWRdataPreProcBlock(params.prep1DParams.get.prepAddress, params.prep1DParams.get.prepParams, beatBytes))) else None
  val win_1D  = if (params.win1DParams  != None) Some(LazyModule(new WindowingBlock(csrAddress = params.win1DParams.get.winCSRAddress, ramAddress = params.win1DParams.get.winRAMAddress, params.win1DParams.get.winParams, beatBytes = beatBytes))) else None
  val fft_1D  = if (params.fft1DParams  != None) Some(LazyModule(new AXI4FFTBlock(address = params.fft1DParams.get.fftAddress, params = params.fft1DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag_1D  = if (params.mag1DParams  != None) Some(LazyModule(new AXI4LogMagMuxBlock(params.mag1DParams.get.magParams, params.mag1DParams.get.magAddress, _beatBytes = beatBytes))) else None
  val acc_1D  = if (params.acc1DParams  != None) Some(LazyModule(new AXI4AccChainBlock(params.acc1DParams.get.accParams, params.acc1DParams.get.accAddress, params.acc1DParams.get.accQueueBase, beatBytes))) else None
  val cfar_1D = if (params.cfar1DParams != None) Some(LazyModule(new AXI4CFARBlock(params.cfar1DParams.get.cfarParams, params.cfar1DParams.get.cfarAddress, _beatBytes = beatBytes))) else None
  
  /* Doppler */
  val ctrl_2D = if (fft2DCond && params.ddrParams == None) Some(LazyModule(new AXI4StreamFFT2ControlBlock(params.ctrl2DParams.get.ctrl2DParams, params.ctrl2DParams.get.ctrl2DAddress, _beatBytes = beatBytes))) else None
  val fft_2D  = if (fft2DCond) Some(LazyModule(new AXI4FFTBlock(address = params.fft2DParams.get.fftAddress, params = params.fft2DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false))) else None
  val mag_2D  = if (params.mag2DParams != None && fft2DCond) Some(LazyModule(new AXI4LogMagMuxBlock(params.mag2DParams.get.magParams, params.mag2DParams.get.magAddress, _beatBytes = beatBytes))) else None
  val scope   = if (params.scopeParams != None && mag_2D != None && cfar_1D != None) Some(LazyModule(new AXI4Scope(params.scopeParams.get.scopeParams, beatBytes))) else None

  val split = if (fft2DCond) Some(LazyModule(new AXI4Splitter(address = params.splitParams.get.splitAddress, beatBytes){
    val out = if (ddrCond) Some({
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
      ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
      InModuleBody { ioOutNode.makeIO() }
    }) else None
  })) else None
  
  val queue = if (fft2DCond) Some(LazyModule(new AXI4DspQueueWithSyncReadMem(params.queueParams.get.queueParams, params.queueParams.get.queueAddress, _beatBytes = beatBytes){
    val in = if (ddrCond) Some({
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
      streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
      InModuleBody { ioInNode.makeIO() }
    }) else None
  })) else None

  if (fft2DCond && params.ddrParams == None) ctrl_2D.get.streamNode := split.get.streamNode

  /* Blocks */
  val blocks_1D = Seq(lvdsphy, crc_1D, preproc, win_1D, fft_1D, split, mag_1D, acc_1D, cfar_1D).flatten
  val blocks_2D = Seq(ctrl_2D, queue, fft_2D, mag_2D).flatten
  val blocks    = blocks_1D ++ blocks_2D
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

  // Connect scope
  if (scope != None) {
    scope.get.streamNode := blocks_1D.last.streamNode
    scope.get.streamNode2 := blocks_2D.last.streamNode
  }

  /* Optional streamNode */
  val streamNode: Option[AXI4StreamNodeHandle] = if (scope != None) None else if(lvdsphy != None && scope == None) Some(blocks_1D.last.streamNode) else Some(NodeHandle(blocks_1D.head.streamNode, blocks_1D.last.streamNode))

  /* Optional CRC and HDMI pins */
  lazy val io = Wire(new SpaceFFTIO(lvdsphy != None, crc_1D != None, scope != None))

  /* Module */
  lazy val module = new LazyModuleImp(this) {
    /* If Scope exists, connect pins */
    if (scope != None) {
      scope.get.module.io.clk_pixel  := io.clk_pixel.get
      scope.get.module.io.clk_serdes := io.clk_serdes.get
      scope.get.module.io.reset_hdmi := io.reset_hdmi.get

      io.clk_p.get  := scope.get.module.io.clk_p
      io.clk_n.get  := scope.get.module.io.clk_n
      io.data_p.get := scope.get.module.io.data_p
      io.data_n.get := scope.get.module.io.data_n
    }
    /* If LVDS PHY exists, connect pins */
    if (lvdsphy != None) {
      lvdsphy.get.ioBlock.i_data(0) := io.i_data.get
      lvdsphy.get.ioBlock.i_frame := io.i_frame.get
      lvdsphy.get.ioBlock.i_valid := io.i_valid.get

      lvdsphy.get.ioBlock.i_async_clock.get := clock
      lvdsphy.get.ioBlock.i_async_reset.get := reset

      lvdsphy.get.module.clock := io.i_async_clock.get
      lvdsphy.get.module.reset := io.i_async_reset.get
      /* If CRC exists, connect pins */
      if (crc_1D != None) {
        if (crc_1D.get.ioBlock.word_size != None) { crc_1D.get.ioBlock.word_size.get := lvdsphy.get.ioBlock.o_word_size }
        if (crc_1D.get.ioBlock.crc_en != None) { crc_1D.get.ioBlock.crc_en.get := lvdsphy.get.ioBlock.o_crc }
      }
    }
    /* If CRC exists, connect pins */
    else if (crc_1D != None) {
      if (crc_1D.get.ioBlock.word_size != None) { crc_1D.get.ioBlock.word_size.get := io.word_size.get }
      if (crc_1D.get.ioBlock.crc_en != None) { crc_1D.get.ioBlock.crc_en.get := io.crc_en.get }
    }

    /* If doppler FFT exists, generate wrapper */
    val mem_reset = if (ddrCond) Some(IO(Input(Bool()))) else None
    val reset_n   = if (ddrCond) Some(IO(Input(Bool()))) else None

    /* IOs */
    val ddrIO = if (ddrCond) Some(IO(new DDRIO(params.ddrParams.get.ddrParams == DDR4, params.ddrParams.get.ddrParams == DDR3))) else None

    /* DDR wrappers */
    if (ddrCond) {
      /* DDR4 */
      if (params.ddrParams.get.ddrParams == DDR4) {
        val ddr4CtrlrWrapper = Module(new ddr4CtrlrWrapper)

        /////////////// RANGE ///////////////////////

        ddr4CtrlrWrapper.io.s_axis_tdata_r_0  := split.get.out.get.bits.data
        ddr4CtrlrWrapper.io.s_axis_tvalid_r_0 := split.get.out.get.valid
        ddr4CtrlrWrapper.io.s_axis_tlast_r_0  := split.get.out.get.bits.last
        split.get.out.get.ready := true.B

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

        queue.get.in.get.bits.data  := ddr4CtrlrWrapper.io.m_axis_tdata_d_0
        queue.get.in.get.valid := ddr4CtrlrWrapper.io.m_axis_tvalid_d_0
        queue.get.in.get.bits.last  := ddr4CtrlrWrapper.io.m_axis_tlast_d_0
        ddr4CtrlrWrapper.io.m_axis_tready_d_0 := queue.get.in.get.ready

        ddr4CtrlrWrapper.io.s_axis_aclk := clock
        ddr4CtrlrWrapper.io.m_axis_aclk := clock
        ddr4CtrlrWrapper.io.s_axis_aresetn := reset_n.get
        ddr4CtrlrWrapper.io.mem_reset := mem_reset.get

        // ethernet signals
        ddrIO.get.eth4.get.o_data_eth := ddr4CtrlrWrapper.io.o_data_eth
        ddrIO.get.eth4.get.o_start_eth := ddr4CtrlrWrapper.io.o_start_eth
        ddrIO.get.eth4.get.o_we_eth := ddr4CtrlrWrapper.io.o_we_eth
        ddr4CtrlrWrapper.io.i_ready_eth := ddrIO.get.eth4.get.i_ready_eth

        //////////////////////////////////////////// ddr4 memory specific signals///////////////////////

        // input connections
        ddr4CtrlrWrapper.io.sys_clk := ddrIO.get.ddr4.get.sys_clk
        ddr4CtrlrWrapper.io.clk_300_p := ddrIO.get.clk_300_p.get
        ddr4CtrlrWrapper.io.clk_300_n := ddrIO.get.clk_300_n.get

        // inout connections
        ddr4CtrlrWrapper.io.c0_ddr4_dq <> ddrIO.get.ddr4.get.c0_ddr4_dq
        ddr4CtrlrWrapper.io.c0_ddr4_dm_dbi_n <> ddrIO.get.ddr4.get.c0_ddr4_dm_dbi_n
        ddr4CtrlrWrapper.io.c0_ddr4_dqs_c <> ddrIO.get.ddr4.get.c0_ddr4_dqs_c
        ddr4CtrlrWrapper.io.c0_ddr4_dqs_t <> ddrIO.get.ddr4.get.c0_ddr4_dqs_t

        // output connections
        ddrIO.get.ddr4.get.o_MemClk_p  := ddr4CtrlrWrapper.io.o_MemClk_p
        ddrIO.get.ddr4.get.c0_ddr4_ba := ddr4CtrlrWrapper.io.c0_ddr4_ba
        ddrIO.get.ddr4.get.c0_ddr4_reset_n := ddr4CtrlrWrapper.io.c0_ddr4_reset_n
        ddrIO.get.ddr4.get.c0_ddr4_cs_n := ddr4CtrlrWrapper.io.c0_ddr4_cs_n
        ddrIO.get.ddr4.get.c0_ddr4_odt := ddr4CtrlrWrapper.io.c0_ddr4_odt
        ddrIO.get.ddr4.get.c0_ddr4_bg := ddr4CtrlrWrapper.io.c0_ddr4_bg
        ddrIO.get.ddr4.get.c0_ddr4_act_n := ddr4CtrlrWrapper.io.c0_ddr4_act_n
        ddrIO.get.ddr4.get.c0_ddr4_cke := ddr4CtrlrWrapper.io.c0_ddr4_cke
        ddrIO.get.ddr4.get.c0_ddr4_ck_c := ddr4CtrlrWrapper.io.c0_ddr4_ck_c
        ddrIO.get.ddr4.get.c0_ddr4_ck_t := ddr4CtrlrWrapper.io.c0_ddr4_ck_t
        ddrIO.get.ddr4.get.c0_ddr4_adr := ddr4CtrlrWrapper.io.c0_ddr4_adr

        ddrIO.get.c0_init_calib_complete.get := ddr4CtrlrWrapper.io.c0_init_calib_complete
      }
      /* DDR3 */
      else {
        val ddr3CtrlrWrapper = Module(new ddr3CtrlrWrapper) // .io.signals

        /////////////// RANGE ///////////////////////

        ddr3CtrlrWrapper.io.s_axis_tdata_r_0  := split.get.out.get.bits.data
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_0 := split.get.out.get.valid
        ddr3CtrlrWrapper.io.s_axis_tlast_r_0  := split.get.out.get.bits.last
        split.get.out.get.ready := true.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_1  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_1 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_1  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_2  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_2 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_2  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_3  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_3 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_3  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_4  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_4 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_4  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_5  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_5 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_5  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_6  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_6 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_6  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_7  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_7 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_7  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_8  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_8 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_8  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_9  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_9 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_9  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_10  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_10 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_10  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_11  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_11 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_11  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_12  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_12 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_12  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_13  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_13 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_13  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_14  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_14 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_14  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_r_15  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_r_15 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_r_15  := false.B

        ///////////////// DOPPLER /////////////////////

        ddr3CtrlrWrapper.io.s_axis_tdata_d_0  := 0.U //dopplerFFTWrapper.module.io.data_out
        // false if we do not want to to use virtual fifo
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_0 := false.B//dopplerFFTWrapper.module.io.valid_out //false.B //dopplerFFTWrapper.module.io.valid_out//false.B //dopplerFFTWrapper.module.io.valid_out // disable virtual fifo writing
        ddr3CtrlrWrapper.io.s_axis_tlast_d_0  := false.B //dopplerFFTWrapper.module.io.last_out


        ddr3CtrlrWrapper.io.s_axis_tdata_d_1  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_1 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_1  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_2  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_2 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_2  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_3  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_3 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_3  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_4  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_4 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_4  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_5  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_5 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_5  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_6  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_6 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_6  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_7  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_7 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_7  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_8  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_8 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_8  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_9  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_9 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_9  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_10  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_10 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_10  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_11  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_11 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_11  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_12  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_12 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_12  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_13  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_13 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_13  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_14  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_14 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_14  := false.B

        ddr3CtrlrWrapper.io.s_axis_tdata_d_15  := 0.U
        ddr3CtrlrWrapper.io.s_axis_tvalid_d_15 := false.B
        ddr3CtrlrWrapper.io.s_axis_tlast_d_15  := false.B

        ///////////////////////////////////////////////////
        ////////////////////// Doppler OUT /////////////////////////////////////////////
        // drive receiver one to doppler fft!

        queue.get.in.get.bits.data := ddr3CtrlrWrapper.io.m_axis_tdata_d_0
        queue.get.in.get.valid     := ddr3CtrlrWrapper.io.m_axis_tvalid_d_0
        queue.get.in.get.bits.last := ddr3CtrlrWrapper.io.m_axis_tlast_d_0
        ddr3CtrlrWrapper.io.m_axis_tready_d_0 := queue.get.in.get.ready

        //// ethernet signals
        ddrIO.get.eth3.get.o_data_eth := ddr3CtrlrWrapper.io.o_data_eth
        ddrIO.get.eth3.get.o_start_eth := ddr3CtrlrWrapper.io.o_start_eth
        ddrIO.get.eth3.get.o_we_eth := ddr3CtrlrWrapper.io.o_we_eth
        ddr3CtrlrWrapper.io.i_ready_eth := ddrIO.get.eth3.get.i_ready_eth

        ddrIO.get.ddr3.get.o_MemClk_p := ddr3CtrlrWrapper.io.o_MemClk_p
        ddrIO.get.ddr3.get.ddr3_addr := ddr3CtrlrWrapper.io.ddr3_addr

        ddrIO.get.ddr3.get.ddr3_ba := ddr3CtrlrWrapper.io.ddr3_ba
        ddrIO.get.ddr3.get.ddr3_ras_n := ddr3CtrlrWrapper.io.ddr3_ras_n
        ddrIO.get.ddr3.get.ddr3_cas_n := ddr3CtrlrWrapper.io.ddr3_cas_n
        ddrIO.get.ddr3.get.ddr3_we_n := ddr3CtrlrWrapper.io.ddr3_we_n
        ddrIO.get.ddr3.get.ddr3_reset_n := ddr3CtrlrWrapper.io.ddr3_reset_n
        ddrIO.get.ddr3.get.ddr3_odt := ddr3CtrlrWrapper.io.ddr3_odt
        ddrIO.get.ddr3.get.ddr3_cke := ddr3CtrlrWrapper.io.ddr3_cke
        ddrIO.get.ddr3.get.ddr3_dm := ddr3CtrlrWrapper.io.ddr3_dm
        ddrIO.get.ddr3.get.ddr3_ck_p := ddr3CtrlrWrapper.io.ddr3_ck_p
        ddrIO.get.ddr3.get.ddr3_ck_n := ddr3CtrlrWrapper.io.ddr3_ck_n

        ddr3CtrlrWrapper.io.s_axis_aclk := clock
        ddr3CtrlrWrapper.io.m_axis_aclk := clock
        ddr3CtrlrWrapper.io.s_axis_aresetn := reset_n.get // just temporary solution for reset signal it should be active in 1, that needs to be changed
        ddr3CtrlrWrapper.io.mem_reset := mem_reset.get

        ddr3CtrlrWrapper.io.sys_clk := ddrIO.get.ddr3.get.sys_clk
        ddr3CtrlrWrapper.io.clk_ref := ddrIO.get.ddr3.get.clk_ref
        ddr3CtrlrWrapper.io.ddr3_dq <> ddrIO.get.ddr3.get.ddr3_dq        // this one should be replaced with inout
        ddr3CtrlrWrapper.io.ddr3_dqs_p <> ddrIO.get.ddr3.get.ddr3_dqs_p  // this one should be replaced with inout
        ddr3CtrlrWrapper.io.ddr3_dqs_n <> ddrIO.get.ddr3.get.ddr3_dqs_n  // this one should be replaced with inout
      }
    }
  }
}

object SpaceFFTApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTParams).params
  val lazyDut = LazyModule(new AXI4SpaceFFT(params, 4) with AXI4SpaceFFTPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/SpaceFFT"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object SpaceFFTnoDDRApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTnoDDRParams).params
  val lazyDut = LazyModule(new AXI4SpaceFFT(params, 4) with AXI4SpaceFFTPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/SpaceFFT"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object SpaceFFTScopeDDRApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTScopeParams).params
  val lazyDut = LazyModule(new AXI4SpaceFFT(params, 4) with AXI4SpaceFFTPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/SpaceFFT"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
