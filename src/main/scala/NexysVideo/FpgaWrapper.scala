// SPDX-License-Identifier: Apache-2.0

package dissertation.nexys

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{Analog, IO, FixedPoint}

import dsptools.numbers._

import dsputils._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import lvdsphy._
import dissertation._
import jtag2mm._

class NexysVideoShellIO(val ddr3: Boolean, val meas: Boolean, val channels: Int) extends Bundle {
  // LVDS signals
  val i_data_p  = Input(Vec(channels, Bool()))
  val i_data_n  = Input(Vec(channels, Bool()))
  val i_valid_p = Input(Bool())
  val i_valid_n = Input(Bool())
  val i_frame_p = Input(Bool())
  val i_frame_n = Input(Bool())
  val i_clk_p   = Input(Clock())
  val i_clk_n   = Input(Clock())

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

  // Measurement
  val delayTime = if (meas) Some(Vec(channels + 1, Output(Bool()))) else None
  val trigger   = if (meas) Some(Output(Bool())) else None
}

object NexysVideoShellIO {
  def apply(ddr3: Boolean, meas: Boolean, channels: Int): NexysVideoShellIO = new NexysVideoShellIO(ddr3, meas, channels)
}

class NexysVideoShell(params: DissertationParameters[FixedPoint], lvdsphyParams: DataRXParams, beatBytes: Int) extends LazyModule()(Parameters.empty) {
  // LVDS connection and data processing
  val lvdsphy = LazyModule(new AXI4StreamDataRX(lvdsphyParams) with DataRXPins)
  // DISSERTATION
  val dissertation = LazyModule(new AXI4Dissertation(params, 4) {
    // streamNode
    val ioInNode_1D  = Seq.fill(streamNodeIn1D.length) { BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes/2))) }
    val ioOutNode_1D = if (detectSignal == None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val ioOutNode_2D = Seq.fill(streamNodeOut2D.length) { BundleBridgeSink[AXI4StreamBundle]() }

    (streamNodeIn1D, ioInNode_1D).zipped.map{ (out, in) => { out := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes/2)) := in }}
    if (ioOutNode_1D != None) { ioOutNode_1D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodeOut1D.get }
    (ioOutNode_2D, streamNodeOut2D).zipped.map{ (out, in) => { out := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := in }}

    /* 1D input streamNode pins */
    val in_1D = ioInNode_1D.zipWithIndex.map{ case (m, i) => {
        implicit val valName = ValName(s"in_1D_${i}")
        val in_1D = InModuleBody { m.makeIO() }
        in_1D
      }
    }
    /* 1D output streamNode pins */
    val out_1D = if (ioOutNode_1D != None) InModuleBody { ioOutNode_1D.get.makeIO() } else None
    /* 2D output streamNode pins */
    val out_2D = ioOutNode_2D.zipWithIndex.map{ case (m, i) => {
        implicit val valName = ValName(s"out_2D_${i}")
        val out_2D = InModuleBody { m.makeIO() }
        out_2D
      }
    }

    // pins
    def makeDissertationIO(): DissertationIO = {
      val io2: DissertationIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = Some(InModuleBody { makeDissertationIO() })
  })

  // JTAG
  val jtagModule = LazyModule(new JTAGToMasterAXI4(3, BigInt("0", 2), 4, AddressSet(0x00000000, 0x6FFFFFFF), 8){
    def makeIO2(): TopModuleIO = {
      val io2: TopModuleIO = IO(io.cloneType)
      io2.suggestName("ioJTAG")
      io2 <> io
      io2
    }
    val ioJTAG = InModuleBody { makeIO2() }
  })

  // Connect mem node
  dissertation.mem.get := jtagModule.node.get

  lazy val module = new LazyModuleImp(this) {
    // IO
    val io = IO(new NexysVideoShellIO(dissertation.ddrCond, dissertation.delayMeasurement != None, params.dataChannels))
    // JTAG IO
    jtagModule.ioJTAG.jtag.TCK   := io.TCK
    jtagModule.ioJTAG.jtag.TMS   := io.TMS
    jtagModule.ioJTAG.jtag.TDI   := io.TDI
    jtagModule.ioJTAG.asyncReset := io.ARST
    // PLL & RESETS
    val pll_lvds = Module(new PLL_LVDS)
    val pll_dsp  = Module(new PLL_DSP)
    val rst_lvds = Module(new RESET_SYS)
    val rst_dsp  = Module(new RESET_SYS)

    // iserdese's
    val selectio_frame = Module(new SelectIO)
    val selectio_valid = Module(new SelectIO)
    val selectio_data  = Seq.fill(params.dataChannels) { Module(new SelectIO) }

    // pll_dsp
    pll_dsp.io.clk_in1 := clock
    pll_dsp.io.reset := 0.U

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

    //selectIO data
    selectio_data.zipWithIndex.map{ case (m,i) =>
      m.io.clk_in     := pll_lvds.io.clk_out1
      m.io.clk_div_in := pll_lvds.io.clk_out2
      m.io.io_reset   := rst_lvds.io.peripheral_reset
      m.io.bitslip    := 0.U
      m.io.data_in_from_pins_p := io.i_data_p(i)
      m.io.data_in_from_pins_n := io.i_data_n(i)
    }
    // lvds
    lvdsphy.module.clock := pll_lvds.io.clk_out2
    lvdsphy.module.reset := rst_lvds.io.mb_reset
    lvdsphy.ioBlock.i_async_clock.get := pll_dsp.io.clk_out1
    lvdsphy.ioBlock.i_async_reset.get := rst_dsp.io.mb_reset
    lvdsphy.ioBlock.i_frame := selectio_frame.io.data_in_to_device
    lvdsphy.ioBlock.i_valid := selectio_valid.io.data_in_to_device
    selectio_data.zipWithIndex.map{ case (m,i) =>
      lvdsphy.ioBlock.i_data(i) := m.io.data_in_to_device
    }

    // Connect dissertation inputs to the lvds_phy
    dissertation.in_1D.zipWithIndex.map{ case (m,i) =>
      m.bits.data := lvdsphy.out.bits.data(16*(i+1)-1, 16*i)
      m.bits.last := lvdsphy.out.bits.last
      m.valid := lvdsphy.out.valid
    }
    lvdsphy.out.ready := dissertation.in_1D.map(m => m.ready).reduce((a, b) => a & b)

    // Connect dissertation outputs
    if(dissertation.delayMeasurement != None) {
      io.trigger.get := dissertation.ioBlock.get.trigger.get
      io.delayTime.get := dissertation.ioBlock.get.delayTime.get
      dissertation.out_2D.zipWithIndex.map{ case (m,i) =>
        m.bits.data := DontCare
        m.bits.last := DontCare
        m.valid := DontCare
        m.ready := 1.U
      }
    }

    if (dissertation.ddrCond) {
      // ethernet signals
      dissertation.module.ddrIO.get.eth3.get.i_ready_eth := 0.U 

      io.ddr3_addr.get    := dissertation.module.ddrIO.get.ddr3.get.ddr3_addr
      io.ddr3_ba.get      := dissertation.module.ddrIO.get.ddr3.get.ddr3_ba
      io.ddr3_ras_n.get   := dissertation.module.ddrIO.get.ddr3.get.ddr3_ras_n
      io.ddr3_cas_n.get   := dissertation.module.ddrIO.get.ddr3.get.ddr3_cas_n
      io.ddr3_we_n.get    := dissertation.module.ddrIO.get.ddr3.get.ddr3_we_n
      io.ddr3_reset_n.get := dissertation.module.ddrIO.get.ddr3.get.ddr3_reset_n
      io.ddr3_odt.get     := dissertation.module.ddrIO.get.ddr3.get.ddr3_odt
      io.ddr3_cke.get     := dissertation.module.ddrIO.get.ddr3.get.ddr3_cke
      io.ddr3_dm.get      := dissertation.module.ddrIO.get.ddr3.get.ddr3_dm
      io.ddr3_ck_p.get    := dissertation.module.ddrIO.get.ddr3.get.ddr3_ck_p
      io.ddr3_ck_n.get    := dissertation.module.ddrIO.get.ddr3.get.ddr3_ck_n

      dissertation.module.reset_n.get   := !rst_dsp.io.peripheral_reset // just temporary solution for reset signal it should be active in 1, that needs to be changed
      dissertation.module.mem_reset.get := rst_dsp.io.peripheral_reset

      dissertation.module.ddrIO.get.ddr3.get.sys_clk := pll_dsp.io.clk_out1
      dissertation.module.ddrIO.get.ddr3.get.clk_ref := pll_dsp.io.clk_out2
      dissertation.module.ddrIO.get.ddr3.get.ddr3_dq    <> io.ddr3_dq.get
      dissertation.module.ddrIO.get.ddr3.get.ddr3_dqs_p <> io.ddr3_dqs_p.get
      dissertation.module.ddrIO.get.ddr3.get.ddr3_dqs_n <> io.ddr3_dqs_n.get
    }

    // dissertation clock & reset
    dissertation.module.clock := pll_dsp.io.clk_out1
    dissertation.module.reset := rst_dsp.io.mb_reset

    // jtag clock and reset
    jtagModule.module.clock := pll_dsp.io.clk_out1
    jtagModule.module.reset := rst_dsp.io.mb_reset
  }
}

object NexysVideoMeasurementDDRShellApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val dataChannels  = 4
  val lvdsphyParams = DataRXParams(
        channels = dataChannels,
        asyncParams = Some(AXI4StreamAsyncQueueWithControlParams(
          ctrlBits = 3,
          sync     = 4,
          depth    = 8,
          safe     = true
        ))
      )
  val params = (new DissertationMeasurementParams(rangeFFTSize = 1024, dopplerFFTSize = 256, ddrType = DDR3, channels = dataChannels)).params
  val lazyDut = LazyModule(new NexysVideoShell(params, lvdsphyParams, 4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/NexysVideoMeasurementDDRShell"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}