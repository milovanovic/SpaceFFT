// SPDX-License-Identifier: Apache-2.0

package spacefft.fpgawrapper

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{Analog, IO, FixedPoint, fromStringToStringParam, fromIntToIntParam}

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import spacefft._
import jtag2mm._

class ZCU102ShellIO extends Bundle {
  val i_data_p  = Input(UInt(1.W))
  val i_data_n  = Input(UInt(1.W))
  val i_valid_p = Input(Bool())
  val i_valid_n = Input(Bool())
  val i_frame_p = Input(Bool())
  val i_frame_n = Input(Bool())
  val i_clk_p   = Input(Clock())
  val i_clk_n   = Input(Clock())
  val clock_n   = Input(Clock())
}

class ZCU102Shell(params: SpaceFFTParameters[FixedPoint], beatBytes: Int) extends LazyModule()(Parameters.empty) {
  // SpaceFFT
  val spacefft = LazyModule(new AXI4SpaceFFT(params, 4) {
    // streamNode
    val ioInNode_1D  = if (lvdsphy == None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))) else None
    val ioOutNode_1D = BundleBridgeSink[AXI4StreamBundle]()
    val ioOutNode_2D = BundleBridgeSink[AXI4StreamBundle]()

    if (ioInNode_1D != None) { streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode_1D.get }
    ioOutNode_1D := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
    ioOutNode_2D := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := blocks_2D.last.streamNode

    val in_1D = if (ioInNode_1D != None) InModuleBody { ioInNode_1D.get.makeIO() } else None
    val out_1D = InModuleBody { ioOutNode_1D.makeIO() }
    val out_2D = InModuleBody { ioOutNode_2D.makeIO() }

    // pins
    def makeSpaceFFTIO(): SpaceFFTIO = {
      val io2: SpaceFFTIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = if (lvdsphy != None || crc_1D != None) Some(InModuleBody { makeSpaceFFTIO() }) else None
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
  spacefft.mem.get    := bus.node
  mem.get := jtagModule.node.get

  lazy val module = new LazyModuleImp(this) {
    // IO
    val io = IO(new ZCU102ShellIO)
    // JTAG IO
    val ioJTAG = IO(jtagModule.ioJTAG.cloneType)
    ioJTAG <> jtagModule.ioJTAG
    
    val pll_lvds = Module(new PLL_LVDS)
    val pll_dsp  = Module(new PLL_DSP)
    val rst_lvds = Module(new RESET_SYS)
    val rst_dsp  = Module(new RESET_SYS)

    // input diff. buffers
    val ibuf_data  = Module(new IBUFDS())
    val ibuf_frame = Module(new IBUFDS())
    val ibuf_valid = Module(new IBUFDS())
    // input delays
    val idelay_data  = Module(new IDELAYE3(Map(
      "CASCADE"          -> fromStringToStringParam("NONE"),
      "DELAY_FORMAT"     -> fromStringToStringParam("COUNT"),
      "DELAY_SRC"        -> fromStringToStringParam("IDATAIN"),
      "DELAY_TYPE"       -> fromStringToStringParam("FIXED"),
      "DELAY_VALUE"      -> fromIntToIntParam(372),
      "IS_CLK_INVERTED"  -> fromIntToIntParam(0),
      "IS_RST_INVERTED"  -> fromIntToIntParam(0),
      "REFCLK_FREQUENCY" -> fromIntToIntParam(300),
      "UPDATE_MODE"      -> fromStringToStringParam("ASYNC"),
      "SIM_DEVICE"       -> fromStringToStringParam("ULTRASCALE_PLUS")
    )))
    val idelay_frame   = Module(new IDELAYE3(Map(
      "CASCADE"          -> fromStringToStringParam("NONE"),
      "DELAY_FORMAT"     -> fromStringToStringParam("COUNT"),
      "DELAY_SRC"        -> fromStringToStringParam("IDATAIN"),
      "DELAY_TYPE"       -> fromStringToStringParam("FIXED"),
      "DELAY_VALUE"      -> fromIntToIntParam(380),
      "IS_CLK_INVERTED"  -> fromIntToIntParam(0),
      "IS_RST_INVERTED"  -> fromIntToIntParam(0),
      "REFCLK_FREQUENCY" -> fromIntToIntParam(300),
      "UPDATE_MODE"      -> fromStringToStringParam("ASYNC"),
      "SIM_DEVICE"       -> fromStringToStringParam("ULTRASCALE_PLUS")
    )))
    val idelay_valid   = Module(new IDELAYE3(Map(
      "CASCADE"          -> fromStringToStringParam("NONE"),
      "DELAY_FORMAT"     -> fromStringToStringParam("COUNT"),
      "DELAY_SRC"        -> fromStringToStringParam("IDATAIN"),
      "DELAY_TYPE"       -> fromStringToStringParam("FIXED"),
      "DELAY_VALUE"      -> fromIntToIntParam(380),
      "IS_CLK_INVERTED"  -> fromIntToIntParam(0),
      "IS_RST_INVERTED"  -> fromIntToIntParam(0),
      "REFCLK_FREQUENCY" -> fromIntToIntParam(300),
      "UPDATE_MODE"      -> fromStringToStringParam("ASYNC"),
      "SIM_DEVICE"       -> fromStringToStringParam("ULTRASCALE_PLUS")
    )))
    // iserdese's
    val iserdes_data  = Module(new ISERDESE3())
    val iserdes_frame = Module(new ISERDESE3())
    val iserdes_valid = Module(new ISERDESE3())

    // pll_lvds
    pll_lvds.io.clk_in1_p := io.i_clk_p
    pll_lvds.io.clk_in1_n := io.i_clk_n
    pll_lvds.io.reset := 0.U

    // pll_dsp
    pll_dsp.io.clk_in1_p := clock
    pll_dsp.io.clk_in1_n := io.clock_n
    pll_dsp.io.reset := 0.U

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

    // data diff. buffer
    ibuf_data.io.I  := io.i_data_p(0)
    ibuf_data.io.IB := io.i_data_n(0)
    // frame diff. buffer
    ibuf_frame.io.I  := io.i_frame_p
    ibuf_frame.io.IB := io.i_frame_n
    // valid diff. buffer
    ibuf_valid.io.I  := io.i_valid_p
    ibuf_valid.io.IB := io.i_valid_n

    // idelay data0
    idelay_data.io.CASC_OUT    := DontCare
    idelay_data.io.CNTVALUEOUT := DontCare
    idelay_data.io.CASC_IN     := 0.U
    idelay_data.io.CASC_RETURN := 0.U
    idelay_data.io.CE          := 0.U
    idelay_data.io.CLK         := pll_lvds.io.clk_out2
    idelay_data.io.CNTVALUEIN  := 0.U
    idelay_data.io.DATAIN      := 0.U
    idelay_data.io.EN_VTC      := 1.U
    idelay_data.io.IDATAIN     := ibuf_data.io.O
    idelay_data.io.INC         := 0.U
    idelay_data.io.LOAD        := 0.U
    idelay_data.io.RST         := rst_lvds.io.peripheral_reset
    iserdes_data.io.D := idelay_data.io.DATAOUT
    // idelay frame
    idelay_frame.io.CASC_OUT    := DontCare
    idelay_frame.io.CNTVALUEOUT := DontCare
    idelay_frame.io.CASC_IN     := 0.U
    idelay_frame.io.CASC_RETURN := 0.U
    idelay_frame.io.CE          := 0.U
    idelay_frame.io.CLK         := pll_lvds.io.clk_out2
    idelay_frame.io.CNTVALUEIN  := 0.U
    idelay_frame.io.DATAIN      := 0.U
    idelay_frame.io.EN_VTC      := 1.U
    idelay_frame.io.IDATAIN     := ibuf_frame.io.O
    idelay_frame.io.INC         := 0.U
    idelay_frame.io.LOAD        := 0.U
    idelay_frame.io.RST         := rst_lvds.io.peripheral_reset
    iserdes_frame.io.D := idelay_frame.io.DATAOUT
    // idelay valid
    idelay_valid.io.CASC_OUT    := DontCare
    idelay_valid.io.CNTVALUEOUT := DontCare
    idelay_valid.io.CASC_IN     := 0.U
    idelay_valid.io.CASC_RETURN := 0.U
    idelay_valid.io.CE          := 0.U
    idelay_valid.io.CLK         := pll_lvds.io.clk_out2
    idelay_valid.io.CNTVALUEIN  := 0.U
    idelay_valid.io.DATAIN      := 0.U
    idelay_valid.io.EN_VTC      := 1.U
    idelay_valid.io.IDATAIN     := ibuf_valid.io.O
    idelay_valid.io.INC         := 0.U
    idelay_valid.io.LOAD        := 0.U
    idelay_valid.io.RST         := rst_lvds.io.peripheral_reset
    iserdes_valid.io.D := idelay_valid.io.DATAOUT

    //iserdese data0
    iserdes_data.io.FIFO_EMPTY      := DontCare
    iserdes_data.io.INTERNAL_DIVCLK := DontCare
    iserdes_data.io.CLK             := pll_lvds.io.clk_out1
    iserdes_data.io.CLK_B           := pll_lvds.io.clk_out1
    iserdes_data.io.CLKDIV          := pll_lvds.io.clk_out2
    iserdes_data.io.FIFO_RD_CLK     := pll_lvds.io.clk_out2
    iserdes_data.io.FIFO_RD_EN      := 1.U
    iserdes_data.io.RST             := rst_lvds.io.peripheral_reset
    //iserdese frame
    iserdes_frame.io.FIFO_EMPTY      := DontCare
    iserdes_frame.io.INTERNAL_DIVCLK := DontCare
    iserdes_frame.io.CLK             := pll_lvds.io.clk_out1
    iserdes_frame.io.CLK_B           := pll_lvds.io.clk_out1
    iserdes_frame.io.CLKDIV          := pll_lvds.io.clk_out2
    iserdes_frame.io.FIFO_RD_CLK     := pll_lvds.io.clk_out2
    iserdes_frame.io.FIFO_RD_EN      := 1.U
    iserdes_frame.io.RST             := rst_lvds.io.peripheral_reset
    //iserdese valid
    iserdes_valid.io.FIFO_EMPTY      := DontCare
    iserdes_valid.io.INTERNAL_DIVCLK := DontCare
    iserdes_valid.io.CLK             := pll_lvds.io.clk_out1
    iserdes_valid.io.CLK_B           := pll_lvds.io.clk_out1
    iserdes_valid.io.CLKDIV          := pll_lvds.io.clk_out2
    iserdes_valid.io.FIFO_RD_CLK     := pll_lvds.io.clk_out2
    iserdes_valid.io.FIFO_RD_EN      := 1.U
    iserdes_valid.io.RST             := rst_lvds.io.peripheral_reset

    // lvds
    spacefft.ioBlock.get.i_async_clock.get := pll_lvds.io.clk_out2
    spacefft.ioBlock.get.i_async_reset.get := rst_lvds.io.mb_reset
    spacefft.ioBlock.get.i_data.get  := iserdes_data.io.Q
    spacefft.ioBlock.get.i_frame.get := iserdes_frame.io.Q
    spacefft.ioBlock.get.i_valid.get := iserdes_valid.io.Q

    // DDR4 IOs
    val o_MemClk_p       = IO(Output(Bool()))
    val c0_ddr4_dq       = IO(Analog(64.W))
    val c0_ddr4_adr      = IO(Output(UInt(17.W)))
    val c0_ddr4_ba       = IO(Output(UInt(2.W)))
    val c0_ddr4_reset_n  = IO(Output(Bool()))
    val c0_ddr4_cs_n     = IO(Output(Bool()))
    val c0_ddr4_odt      = IO(Output(Bool()))
    val c0_ddr4_bg       = IO(Output(Bool()))
    val c0_ddr4_act_n    = IO(Output(Bool()))
    val c0_ddr4_cke      = IO(Output(Bool()))
    val c0_ddr4_dm_dbi_n = IO(Analog(8.W))
    val c0_ddr4_dqs_c    = IO(Analog(8.W))
    val c0_ddr4_dqs_t    = IO(Analog(8.W))
    val c0_ddr4_ck_c     = IO(Output(Bool()))
    val c0_ddr4_ck_t     = IO(Output(Bool()))

    // DDR4 signals
    spacefft.module.ddr4IO.get.sys_clk := 0.U // FIXME: Currently not in use
    spacefft.module.clk_300_p.get := clock
    spacefft.module.clk_300_n.get := io.clock_n

    // inout connections
    spacefft.module.ddr4IO.get.c0_ddr4_dq       <> c0_ddr4_dq
    spacefft.module.ddr4IO.get.c0_ddr4_dm_dbi_n <> c0_ddr4_dm_dbi_n
    spacefft.module.ddr4IO.get.c0_ddr4_dqs_c    <> c0_ddr4_dqs_c
    spacefft.module.ddr4IO.get.c0_ddr4_dqs_t    <> c0_ddr4_dqs_t

    // output connections
    o_MemClk_p      := spacefft.module.ddr4IO.get.o_MemClk_p
    c0_ddr4_ba      := spacefft.module.ddr4IO.get.c0_ddr4_ba
    c0_ddr4_reset_n := spacefft.module.ddr4IO.get.c0_ddr4_reset_n
    c0_ddr4_cs_n    := spacefft.module.ddr4IO.get.c0_ddr4_cs_n
    c0_ddr4_odt     := spacefft.module.ddr4IO.get.c0_ddr4_odt
    c0_ddr4_bg      := spacefft.module.ddr4IO.get.c0_ddr4_bg
    c0_ddr4_act_n   := spacefft.module.ddr4IO.get.c0_ddr4_act_n
    c0_ddr4_cke     := spacefft.module.ddr4IO.get.c0_ddr4_cke
    c0_ddr4_ck_c    := spacefft.module.ddr4IO.get.c0_ddr4_ck_c
    c0_ddr4_ck_t    := spacefft.module.ddr4IO.get.c0_ddr4_ck_t
    c0_ddr4_adr     := spacefft.module.ddr4IO.get.c0_ddr4_adr

    // // ethernet signals
    // ethIO.o_data_eth := spacefft.module.ethIO.o_data_eth
    // ethIO.o_start_eth := spacefft.module.ethIO.o_start_eth
    // ethIO.o_we_eth := spacefft.module.ethIO.o_we_eth
    spacefft.module.ethIO.get.i_ready_eth := 0.U //ethIO.i_ready_eth

    // spacefft clock & reset
    spacefft.module.clock := pll_dsp.io.clk_out1
    spacefft.module.reset := rst_dsp.io.mb_reset

    // jtag clock and reset
    jtagModule.module.clock := pll_dsp.io.clk_out1
    jtagModule.module.reset := rst_dsp.io.mb_reset
  }
}

object ZCU102ShellApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new SpaceFFTParams).params
  val lazyDut = LazyModule(new ZCU102Shell(params, 4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/ZCU102Shell"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

