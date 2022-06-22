package opera.rspchain

import chisel3._
import chisel3.util._

import chisel3.experimental._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import dsptools.numbers._

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsputils._
import fft._
import magnitude._
import xWRDataPreProc._
// import jtag2mm._
// import uart._

class AXI4StreamOutIO extends Bundle {
  val valid_out = Output(Bool())
  val last_out  = Output(Bool())
  val ready_in  = Input(Bool())
  val data_out  = Output(UInt(32.W))
  //val data_out  = Output(UInt((beatBytes*8).W))
}

class AXI4StreamInIO extends Bundle {
  val valid_in = Input(Bool())
  val last_in  = Input(Bool())
  val ready_out  = Output(Bool())
 // val data_in  = Input(UInt((beatBytes*8).W))
  val data_in  = Input(UInt(32.W))
}

class AXI4StreamSlaveWrapper(val beatBytes: Int = 4) extends LazyModule()(Parameters.empty) {
  val slaveParams = AXI4StreamSlaveParameters()
  val slaveNode  = AXI4StreamSlaveNode(slaveParams)

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = slaveNode.in(0)

    val io = IO(new AXI4StreamOutIO)
    io.valid_out := in.valid
    io.last_out := in.bits.last
    in.ready := io.ready_in
    io.data_out := in.bits.data
  }
}

class AXI4StreamMasterWrapper(val beatBytes: Int = 4) extends LazyModule()(Parameters.empty) {

  val masterParams = AXI4StreamMasterParameters(
    name = "AXI4 Stream Master Wrapper",
    n = 4, // just 2*8 -> 16 bits
    numMasters = 1
  )
  val masterNode = AXI4StreamMasterNode(masterParams)

  lazy val module = new LazyModuleImp(this) {
    val (out, _)  = masterNode.out(0)

    val io = IO(new AXI4StreamInIO)
    out.valid := io.valid_in
    out.bits.last := io.last_in
    out.bits.data := io.data_in
    io.ready_out := out.ready
  }
}

class AXI4StreamIdentityWrapper(val beatBytes: Int = 4) extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val io = IO(new AXI4StreamOutIO)
    io.valid_out := in.valid
    io.last_out := in.bits.last
    io.data_out := in.bits.data

    out.valid := in.valid
    out.bits.last := in.bits.last
    out.bits.data := in.bits.data
    in.ready := out.ready
  }
}


case class fft_2d_with_ddr4_1_rx_no_jtag_parameters (
  dspQueueParams    : DspQueueCustomParams,
  preProcParams     : AXI4XwrDataPreProcParams,
  rangeFFTParams    : FFTParams[FixedPoint],
  dopplerFFTParams  : FFTParams[FixedPoint],
  magParams         : MAGParams[FixedPoint],
  rangeFFTAddress   : AddressSet,
  dopplerFFTAddress : AddressSet,
  dspQueueAddress   : AddressSet,
  preProcAddress    : AddressSet,
  magAddress        : AddressSet,
  beatBytes         : Int
)

trait fft_2d_with_ddr4_1_rx_no_jtag_Standalone extends fft_2d_with_ddr4_1_rx_no_jtag {
  // out stream node
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
   val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}


  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := magnitude.streamNode

  val out = InModuleBody { ioOutNode.makeIO() }

  // in stream node
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode
  val in = InModuleBody { ioInNode.makeIO() }
}


class fft_2d_with_ddr4_1_rx_no_jtag(val params: fft_2d_with_ddr4_1_rx_no_jtag_parameters) extends LazyModule()(Parameters.empty) {

  val rangeFFT          = LazyModule(new AXI4FFTBlock(params.rangeFFTParams, params.rangeFFTAddress, _beatBytes = params.beatBytes, configInterface = false))
  val dspQueue          = LazyModule(new AXI4DspQueueWithSyncReadMem(params.dspQueueParams, params.dspQueueAddress, _beatBytes = params.beatBytes))
  val dopplerFFT        = LazyModule(new AXI4FFTBlock(params.dopplerFFTParams, params.dopplerFFTAddress, _beatBytes = params.beatBytes, configInterface = false))
  val xAWRpreProc       = LazyModule(new AXI4xWRdataPreProcBlock(params.preProcAddress, params.preProcParams, params.beatBytes))
  val dspQueueWrapper   = LazyModule(new AXI4StreamMasterWrapper(4))
  val rangeFFTWrapper   = LazyModule(new AXI4StreamSlaveWrapper(4))
  val dopplerFFTWrapper = LazyModule(new AXI4StreamIdentityWrapper(4))
  val magnitude         = LazyModule(new AXI4LogMagMuxBlock(params.magParams, params.magAddress))

  val streamNode = NodeHandle(xAWRpreProc.streamNode, magnitude.streamNode)

  // define mem
  lazy val blocks = Seq(xAWRpreProc, dspQueue, rangeFFT, dopplerFFT, magnitude)
  val bus = LazyModule(new AXI4Xbar)

  val mem = Some(bus.node)

  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  rangeFFT.streamNode := xAWRpreProc.streamNode
  dopplerFFT.streamNode := dspQueue.streamNode
  dspQueue.streamNode := dspQueueWrapper.masterNode

  rangeFFTWrapper.slaveNode := rangeFFT.streamNode
  dopplerFFTWrapper.streamNode := dopplerFFT.streamNode
  magnitude.streamNode := AXI4StreamBuffer() := dopplerFFTWrapper.streamNode


  lazy val module = new LazyModuleImp(this) {

    val ddr4CtrlrWrapper = Module(new ddr4CtrlrWrapper)

    /////////////// RANGE ///////////////////////

    ddr4CtrlrWrapper.io.s_axis_tdata_r_0  := rangeFFTWrapper.module.io.data_out
    ddr4CtrlrWrapper.io.s_axis_tvalid_r_0 := rangeFFTWrapper.module.io.valid_out
    ddr4CtrlrWrapper.io.s_axis_tlast_r_0  := rangeFFTWrapper.module.io.last_out
    rangeFFTWrapper.module.io.ready_in := true.B

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

    dspQueueWrapper.module.io.data_in  := ddr4CtrlrWrapper.io.m_axis_tdata_d_0
    dspQueueWrapper.module.io.valid_in := ddr4CtrlrWrapper.io.m_axis_tvalid_d_0
    dspQueueWrapper.module.io.last_in  := ddr4CtrlrWrapper.io.m_axis_tlast_d_0
    ddr4CtrlrWrapper.io.m_axis_tready_d_0 := dspQueueWrapper.module.io.ready_out

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

object fft_2d_with_ddr4_1_rx_no_jtag_App extends App {

 val rangeFFT = 64
 val dopplerFFT = 16
 val totalData = rangeFFT * dopplerFFT
 val params = fft_2d_with_ddr4_1_rx_no_jtag_parameters (
    preProcParams  = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFT, useBlockRam = true),
    rangeFFTParams = FFTParams.fixed(
      dataWidth = 12,
      twiddleWidth = 16,
      numPoints = rangeFFT,
      useBitReverse  = true,
      runTime = true,
      numAddPipes = 1,
      numMulPipes = 1,
      use4Muls = true,
      expandLogic = Array.fill(log2Up(rangeFFT))(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
      sdfRadix = "2",
      trimType = Convergent,
      keepMSBorLSB = Array.fill(log2Up(rangeFFT))(true),
      minSRAMdepth = 64, // memories larger than 64 should be mapped on block ram
      binPoint = 10
    ),
    dopplerFFTParams = FFTParams.fixed(
      dataWidth = 16,
      twiddleWidth = 16,
      numPoints = dopplerFFT,
      useBitReverse  = true,
      runTime = false,
      numAddPipes = 1,
      numMulPipes = 1,
      use4Muls = true,
      sdfRadix = "2",
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(dopplerFFT))(0),
      keepMSBorLSB = Array.fill(log2Up(dopplerFFT))(true),
      minSRAMdepth = 128, // memories larger than 64 should be mapped on block ram
      binPoint = 10
    ),
    magParams = MAGParams(
      protoIn  = FixedPoint(16.W, 10.BP),
      protoOut = FixedPoint(16.W, 10.BP), // lets say identity node
      magType  = MagJPLandSqrMag,
      useLast = true,
      numAddPipes = 1,
      numMulPipes = 1
    ),
    dspQueueParams = DspQueueCustomParams(
                             queueDepth = dopplerFFT, // should be the same as max dopplerFFTSize
                             progFull = false,
                             addEnProgFullOut = false,
                             useSyncReadMem = false, // do not use distributed ram, trying to eliminate timing issues
                             enLastGen = false),
    rangeFFTAddress    = AddressSet(0x01000, 0xFFF),
    dopplerFFTAddress  = AddressSet(0x02000, 0xFFF),
    dspQueueAddress    = AddressSet(0x03000, 0xFFF),
    preProcAddress     = AddressSet(0x04000, 0xFF),
    magAddress         = AddressSet(0x05000, 0xFF),
    beatBytes          = 4)

  implicit val p: Parameters = Parameters.empty
  val standaloneModule = LazyModule(new fft_2d_with_ddr4_1_rx_no_jtag(params) with fft_2d_with_ddr4_1_rx_no_jtag_Standalone)
  (new ChiselStage).execute(Array("--target-dir", "verilog/fft_2d_with_ddr4_1_rx_no_jtag"), Seq(ChiselGeneratorAnnotation(() => standaloneModule.module)))
}
