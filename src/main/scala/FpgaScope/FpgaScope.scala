package spacefft.scope

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{IO}
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import hdmi.scope._
import jtag2mm._


// Scope IO
class FpgaScopeScopeIO extends Bundle {
  // tmds output ports
  val clk_p  = Output(Bool())
  val clk_n  = Output(Bool())
  val data_p = Output(UInt(3.W))
  val data_n = Output(UInt(3.W))

  // JTAG input ports
  val TDI  = Input(Bool())
  val TMS  = Input(Bool())
  val TCK  = Input(Bool())
  val ARST = Input(Bool())
}

class DataGen(dataSize: Int) extends LazyModule()(Parameters.empty) {
  val node = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "AXI4 Stream", n = 2, numMasters = 1))

  lazy val module = new LazyModuleImp(this) {
    val out = node.out(0)._1

    val counter = RegInit(0.U((log2Ceil(dataSize)).W))
    out.valid := 1.U
    out.bits.data := counter << (out.bits.data.getWidth - log2Ceil(dataSize))
    when(out.fire()) {counter := counter + 1.U}
    out.bits.last := counter === (dataSize - 1).U
  }
}

class DataGenCFAR(dataSize: Int) extends LazyModule()(Parameters.empty) {
  val node = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "AXI4 Stream", n = 6, numMasters = 1))

  lazy val module = new LazyModuleImp(this) {
    val out = node.out(0)._1

    val counter = RegInit(0.U((log2Ceil(dataSize)).W))
    out.valid := 1.U
    val w_cut   = Wire(UInt(16.W)) 
    w_cut := counter << (w_cut.getWidth - log2Ceil(dataSize))
    out.bits.data := Cat(0.U(21.W), w_cut, 0.U(11.W))
    when(out.fire()) {counter := counter + 1.U}
    out.bits.last := counter === (dataSize - 1).U
  }
}

class FpgaScope[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: ScopeParameters[T], dataSize: Int, beatBytes: Int) extends LazyModule()(Parameters.empty) {
  val scope = LazyModule(new AXI4Scope(params, beatBytes))
  val data1 = LazyModule(new DataGenCFAR(dataSize))
  val data2 = LazyModule(new DataGen(dataSize))
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
  scope.mem.get := jtagModule.node.get

  scope.streamNode := data1.node
  scope.streamNode2 := data2.node

  lazy val module = new LazyModuleImp(this) {
    // IO
    val io = IO(new FpgaScopeScopeIO)

    // JTAG IO
    jtagModule.ioJTAG.jtag.TCK   := io.TCK
    jtagModule.ioJTAG.jtag.TMS   := io.TMS
    jtagModule.ioJTAG.jtag.TDI   := io.TDI
    jtagModule.ioJTAG.asyncReset := io.ARST
    // PLL & RESETS
    val pll_hdmi = Module(new PLL_HDMI)
    val rst_hdmi = Module(new RESET_SYS)
    // // Buffers
    // val buff_clk = Module(new IBUFG)

    // // pll_hdmi
    // buff_clk.io.I := clock
    pll_hdmi.io.clk_in1 := clock
    pll_hdmi.io.reset := 0.U

    // rst_hdmi
    rst_hdmi.io.slowest_sync_clk     := pll_hdmi.io.clk_out2
    rst_hdmi.io.ext_reset_in         := reset
    rst_hdmi.io.aux_reset_in         := 0.U
    rst_hdmi.io.mb_debug_sys_rst     := 0.U
    rst_hdmi.io.dcm_locked           := pll_hdmi.io.locked
    rst_hdmi.io.bus_struct_reset     := DontCare
    rst_hdmi.io.interconnect_aresetn := DontCare
    rst_hdmi.io.peripheral_aresetn   := DontCare

    scope.module.io.clk_pixel  := pll_hdmi.io.clk_out2
    scope.module.io.clk_serdes := pll_hdmi.io.clk_out1
    scope.module.io.reset_hdmi := rst_hdmi.io.mb_reset

    io.clk_p := scope.module.io.clk_p
    io.clk_n := scope.module.io.clk_n
    io.data_p := scope.module.io.data_p
    io.data_n := scope.module.io.data_n
  }
}

object FpgaScopeApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val range = 512
  val doppler = 256
  val startAddress = 0x0000
  val params = (new ScopeParams(range, doppler, startAddress)).params
  val lazyDut = LazyModule(new FpgaScope(params, range,  4))

  (new ChiselStage).execute(Array("--target-dir", "verilog/Scope"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

