// SPDX-License-Identifier: Apache-2.0

package spacefft.nexys

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{Analog, IO, FixedPoint, fromStringToStringParam, fromIntToIntParam}

import dsptools.numbers._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import spacefft._
import jtag2mm._

class NexysVideoShellIO(val scope: Boolean, val ddr3: Boolean) extends Bundle {
  val i_data_p  = Input(UInt(1.W))
  val i_data_n  = Input(UInt(1.W))
  val i_valid_p = Input(Bool())
  val i_valid_n = Input(Bool())
  val i_frame_p = Input(Bool())
  val i_frame_n = Input(Bool())
  val i_clk_p   = Input(Clock())
  val i_clk_n   = Input(Clock())
  val clock_n   = Input(Clock())
  // tmds output ports
  val clk_p  = if (scope) Some(Output(Bool())) else None
  val clk_n  = if (scope) Some(Output(Bool())) else None
  val data_p = if (scope) Some(Output(UInt(3.W))) else None
  val data_n = if (scope) Some(Output(UInt(3.W))) else None

  // JTAG input ports
  val TDI  = Input(Bool())
  val TMS  = Input(Bool())
  val TCK  = Input(Bool())
  val ARST = Input(Bool())

  // DDR3
  val ddr3_dq      = if (ddr3) Some(Analog((16.W))) else None
  val ddr3_addr    = if (ddr3) Some(Output(UInt(15.W))) else None
  val ddr3_ba      = if (ddr3) Some(Output(UInt(3.W))) else None
  val ddr3_ras_n   = if (ddr3) Some(Output(Bool())) else None
  val ddr3_cas_n   = if (ddr3) Some(Output(Bool())) else None
  val ddr3_we_n    = if (ddr3) Some(Output(Bool())) else None
  val ddr3_reset_n = if (ddr3) Some(Output(Bool())) else None
  val ddr3_odt     = if (ddr3) Some(Output(Bool())) else None
  val ddr3_cke     = if (ddr3) Some(Output(Bool())) else None
  val ddr3_dm      = if (ddr3) Some(Output(UInt(2.W))) else None
  val ddr3_dqs_p   = if (ddr3) Some(Analog((2.W))) else None
  val ddr3_dqs_n   = if (ddr3) Some(Analog((2.W))) else None
  val ddr3_ck_p    = if (ddr3) Some(Output(Bool())) else None
  val ddr3_ck_n    = if (ddr3) Some(Output(Bool())) else None
}

object NexysVideoShellIO {
  def apply(scope: Boolean, ddr3: Boolean): NexysVideoShellIO = new NexysVideoShellIO(scope, ddr3)
}

class NexysVideoShell(params: SpaceFFTParameters[FixedPoint], beatBytes: Int) extends LazyModule()(Parameters.empty) {
  // SpaceFFT
  val spacefft = LazyModule(new AXI4SpaceFFT(params, 4) {
    // streamNode
    val ioInNode_1D  = if (lvdsphy == None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))) else None
    val ioOutNode_1D = if (scope == None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val ioOutNode_2D = if (scope == None) Some( BundleBridgeSink[AXI4StreamBundle]()) else None

    if (ioInNode_1D != None)  { streamNode.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode_1D.get }
    if (ioOutNode_1D != None) { ioOutNode_1D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode.get }
    if (ioOutNode_2D != None) { ioOutNode_2D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := blocks_2D.last.streamNode }

    val in_1D  = if (ioInNode_1D != None)  InModuleBody { ioInNode_1D.get.makeIO() } else None
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
  })

  // JTAG
  val jtagModule = LazyModule(new JTAGToMasterAXI4(3, BigInt("0", 2), 4, AddressSet(0x00000, 0x3fff), 8){
    def makeIO2(): TopModuleIO = {
      val io2: TopModuleIO = IO(io.cloneType)
      io2.suggestName("ioJTAG")
      io2 <> io
      io2
    }
    val ioJTAG = InModuleBody { makeIO2() }
  })

  // Connect mem node
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  spacefft.mem.get := bus.node
  mem.get := jtagModule.node.get

  lazy val module = new LazyModuleImp(this) {
    // IO
    val io = IO(new NexysVideoShellIO(spacefft.scope != None, spacefft.ddrCond))
    // JTAG IO
    jtagModule.ioJTAG.jtag.TCK   := io.TCK
    jtagModule.ioJTAG.jtag.TMS   := io.TMS
    jtagModule.ioJTAG.jtag.TDI   := io.TDI
    jtagModule.ioJTAG.asyncReset := io.ARST
    // PLL & RESETS
    val pll_lvds = Module(new PLL_LVDS)
    val pll_dsp  = Module(new PLL_DSP)
    val pll_hdmi = Module(new PLL_HDMI)
    val rst_lvds = Module(new RESET_SYS)
    val rst_dsp  = Module(new RESET_SYS)
    val rst_hdmi = Module(new RESET_SYS)
    // Buffers
    val buff_clk = Module(new IBUFG)
    // val pixbufg = Module(new BUFG)
    // val bufio = Module(new BUFIO)
    // val bufr = Module(new BUFR())

    // iserdese's
    val selectio_frame = Module(new SelectIO)
    val selectio_valid = Module(new SelectIO)
    val selectio_data  = Module(new SelectIO)

    buff_clk.io.I := clock
    // pll_dsp
    pll_dsp.io.clk_in1 := buff_clk.io.O
    pll_dsp.io.reset := 0.U

    // pll_hdmi
    pll_hdmi.io.clk_in1 := buff_clk.io.O
    pll_hdmi.io.reset := 0.U

    // pll_lvds
    pll_lvds.io.clk_in1_p := io.i_clk_p
    pll_lvds.io.clk_in1_n := io.i_clk_n
    pll_lvds.io.reset := 0.U

    // rst_lvds
    rst_lvds.io.slowest_sync_clk     := pll_lvds.io.clk_out2
    rst_lvds.io.ext_reset_in         := reset
    rst_lvds.io.aux_reset_in         := 0.U
    rst_lvds.io.mb_debug_sys_rst     := 0.U
    rst_lvds.io.dcm_locked           := pll_lvds.io.locked
    rst_lvds.io.bus_struct_reset     := DontCare
    rst_lvds.io.interconnect_aresetn := DontCare
    rst_lvds.io.peripheral_aresetn   := DontCare

    // rst_dsp
    rst_dsp.io.slowest_sync_clk     := pll_dsp.io.clk_out1
    rst_dsp.io.ext_reset_in         := reset
    rst_dsp.io.aux_reset_in         := 0.U
    rst_dsp.io.mb_debug_sys_rst     := 0.U
    rst_dsp.io.dcm_locked           := pll_dsp.io.locked
    rst_dsp.io.bus_struct_reset     := DontCare
    rst_dsp.io.interconnect_aresetn := DontCare
    rst_dsp.io.peripheral_aresetn   := DontCare

    // rst_hdmi
    rst_hdmi.io.slowest_sync_clk     := pll_hdmi.io.clk_out2
    rst_hdmi.io.ext_reset_in         := reset
    rst_hdmi.io.aux_reset_in         := 0.U
    rst_hdmi.io.mb_debug_sys_rst     := 0.U
    rst_hdmi.io.dcm_locked           := pll_hdmi.io.locked
    rst_hdmi.io.bus_struct_reset     := DontCare
    rst_hdmi.io.interconnect_aresetn := DontCare
    rst_hdmi.io.peripheral_aresetn   := DontCare

    // selectIO frame
    selectio_frame.io.clk_in     := pll_lvds.io.clk_out1
    selectio_frame.io.clk_div_in := pll_lvds.io.clk_out2
    selectio_frame.io.io_reset   := rst_lvds.io.peripheral_reset
    selectio_frame.io.bitslip    := 0.U
    selectio_frame.io.data_in_from_pins_p := io.i_frame_p
    selectio_frame.io.data_in_from_pins_n := io.i_frame_n

    // selectIO valid
    selectio_valid.io.clk_in     := pll_lvds.io.clk_out1
    selectio_valid.io.clk_div_in := pll_lvds.io.clk_out2
    selectio_valid.io.io_reset   := rst_lvds.io.peripheral_reset
    selectio_valid.io.bitslip    := 0.U
    selectio_valid.io.data_in_from_pins_p := io.i_valid_p
    selectio_valid.io.data_in_from_pins_n := io.i_valid_n

    //selectIO  data
    selectio_data.io.clk_in     := pll_lvds.io.clk_out1
    selectio_data.io.clk_div_in := pll_lvds.io.clk_out2
    selectio_data.io.io_reset   := rst_lvds.io.peripheral_reset
    selectio_data.io.bitslip    := 0.U
    selectio_data.io.data_in_from_pins_p := io.i_data_p(0)
    selectio_data.io.data_in_from_pins_n := io.i_data_n(0)

    // lvds
    spacefft.ioBlock.get.i_async_clock.get := pll_lvds.io.clk_out2
    spacefft.ioBlock.get.i_async_reset.get := rst_lvds.io.mb_reset
    spacefft.ioBlock.get.i_data.get  := selectio_data.io.data_in_to_device
    spacefft.ioBlock.get.i_frame.get := selectio_frame.io.data_in_to_device
    spacefft.ioBlock.get.i_valid.get := selectio_valid.io.data_in_to_device

    if (spacefft.ddrCond) {
      // ethernet signals
      spacefft.module.ddrIO.get.eth3.get.i_ready_eth := 0.U 

      io.ddr3_addr.get    := spacefft.module.ddrIO.get.ddr3.get.ddr3_addr
      io.ddr3_ba.get      := spacefft.module.ddrIO.get.ddr3.get.ddr3_ba
      io.ddr3_ras_n.get   := spacefft.module.ddrIO.get.ddr3.get.ddr3_ras_n
      io.ddr3_cas_n.get   := spacefft.module.ddrIO.get.ddr3.get.ddr3_cas_n
      io.ddr3_we_n.get    := spacefft.module.ddrIO.get.ddr3.get.ddr3_we_n
      io.ddr3_reset_n.get := spacefft.module.ddrIO.get.ddr3.get.ddr3_reset_n
      io.ddr3_odt.get     := spacefft.module.ddrIO.get.ddr3.get.ddr3_odt
      io.ddr3_cke.get     := spacefft.module.ddrIO.get.ddr3.get.ddr3_cke
      io.ddr3_dm.get      := spacefft.module.ddrIO.get.ddr3.get.ddr3_dm
      io.ddr3_ck_p.get    := spacefft.module.ddrIO.get.ddr3.get.ddr3_ck_p
      io.ddr3_ck_n.get    := spacefft.module.ddrIO.get.ddr3.get.ddr3_ck_n

      spacefft.module.reset_n.get   := !rst_lvds.io.peripheral_reset // just temporary solution for reset signal it should be active in 1, that needs to be changed
      spacefft.module.mem_reset.get := rst_lvds.io.peripheral_reset

      spacefft.module.ddrIO.get.ddr3.get.sys_clk := pll_dsp.io.clk_out1
      spacefft.module.ddrIO.get.ddr3.get.clk_ref := pll_dsp.io.clk_out2
      spacefft.module.ddrIO.get.ddr3.get.ddr3_dq    <> io.ddr3_dq.get
      spacefft.module.ddrIO.get.ddr3.get.ddr3_dqs_p <> io.ddr3_dqs_p.get
      spacefft.module.ddrIO.get.ddr3.get.ddr3_dqs_n <> io.ddr3_dqs_n.get
    }
    

    // spacefft clock & reset
    spacefft.module.clock := pll_dsp.io.clk_out1
    spacefft.module.reset := rst_dsp.io.mb_reset
    spacefft.ioBlock.get.clk_pixel.get  := pll_hdmi.io.clk_out2
    spacefft.ioBlock.get.clk_serdes.get := pll_hdmi.io.clk_out1
    spacefft.ioBlock.get.reset_hdmi.get := rst_hdmi.io.mb_reset

    // HDMI pins
    io.clk_p.get  := spacefft.ioBlock.get.clk_p.get
    io.clk_n.get  := spacefft.ioBlock.get.clk_n.get
    io.data_p.get := spacefft.ioBlock.get.data_p.get
    io.data_n.get := spacefft.ioBlock.get.data_n.get

    // jtag clock and reset
    jtagModule.module.clock := pll_dsp.io.clk_out1
    jtagModule.module.reset := rst_dsp.io.mb_reset
  }
}

object NexysVideoShellApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTScopeParams(512, 256, DDR3)).params
  val lazyDut = LazyModule(new NexysVideoShell(params, 4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/NexysVideoShell"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object NexysVideoSmallShellApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTSmallScopeParams(512, 256, DDR3)).params
  val lazyDut = LazyModule(new NexysVideoShell(params, 4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/NexysVideoSmallShell"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}


object NexysVideoSmallNoDDRShellApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTSmallScopeNoDDRParams(256, 128)).params
  val lazyDut = LazyModule(new NexysVideoShell(params, 4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/NexysVideoSmallNoDDRShell"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}