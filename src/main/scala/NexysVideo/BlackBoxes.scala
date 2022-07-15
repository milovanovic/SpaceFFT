// SPDX-License-Identifier: Apache-2.0

package spacefft.nexys

import chisel3._
import chisel3.experimental._

import chisel3._
import chisel3.experimental._

object booleanToVerilogVectorParam extends (Boolean => RawParam) {
  def apply(b : Boolean) : RawParam =  if(b) RawParam("1") else RawParam("0")
}

object booleanToVerilogStringParam extends (Boolean => StringParam) {
  def apply(b : Boolean) : StringParam = if(b) StringParam("""TRUE""") else StringParam("""FALSE""")
}

// SelectIO BlackBox for Vivado
class SelectIO extends BlackBox {
    val io = IO(new Bundle {
        val clk_in     = Input(Clock())
        val clk_div_in = Input(Clock())
        val io_reset   = Input(Bool())
        val bitslip    = Input(UInt(1.W))
        val data_in_from_pins_p = Input(UInt(1.W))
        val data_in_from_pins_n = Input(UInt(1.W))
        val data_in_to_device   = Output(UInt(8.W))
    })
}

class IDELAYE2(val paramsIDELAYE2: Map[String, Param] = Map(
    "IDELAY_TYPE"           -> fromStringToStringParam("FIXED"),
    "DELAY_SRC"             -> fromStringToStringParam("IDATAIN"),
    "HIGH_PERFORMANCE_MODE" -> fromStringToStringParam("TRUE"),
    "SIGNAL_PATTERN"        -> fromStringToStringParam("DATA"),
    "CINVCTRL_SEL"          -> fromStringToStringParam("FALSE"),
    "PIPE_SEL"              -> fromStringToStringParam("FALSE"),
    "IDELAY_VALUE"          -> fromIntToIntParam(24),
    "REFCLK_FREQUENCY"      -> fromIntToIntParam(300)
  )) extends BlackBox(paramsIDELAYE2){
  val io = IO(new Bundle {
    val CNTVALUEOUT = Output(UInt(5.W))
    val DATAOUT     = Output(Bool())
    val C           = Input(Clock())
    val REGRST      = Input(Bool())
    val LD          = Input(Bool())
    val CE          = Input(Bool())
    val INC         = Input(UInt(5.W))
    val CINVCTRL    = Input(Bool())
    val CNTVALUEIN  = Input(Bool())
    val IDATAIN     = Input(Bool())
    val DATAIN      = Input(Bool())
    val LDPIPEEN    = Input(Bool())
    val RST         = Input(Bool())
  })
}

// PLL BlackBox for Vivado
class PLL_LVDS extends BlackBox {
    val io = IO(new Bundle {
        val clk_in1_p = Input(Clock())
        val clk_in1_n = Input(Clock())
        val clk_out1  = Output(Clock())
        val clk_out2  = Output(Clock())
        val locked    = Output(Bool())
        val reset     = Input(Bool())
    })
}

// PLL BlackBox for Vivado
class PLL_DSP extends BlackBox {
    val io = IO(new Bundle {
        val clk_in1  = Input(Clock())
        val clk_out1 = Output(Clock())
        val clk_out2 = Output(Clock())
        val locked   = Output(Bool())
        val reset    = Input(Bool())
    })
}

// PLL BlackBox for Vivado
class PLL_HDMI extends BlackBox {
    val io = IO(new Bundle {
        val clk_in1  = Input(Clock())
        val clk_out1 = Output(Clock())
        val clk_out2 = Output(Clock())
        val locked   = Output(Bool())
        val reset    = Input(Bool())
    })
}

// RESET SYS BlackBox for Vivado
class RESET_SYS extends BlackBox {
  val io = IO(new Bundle {
    val slowest_sync_clk     = Input(Clock())
    val ext_reset_in         = Input(Bool())
    val aux_reset_in         = Input(Bool())
    val mb_debug_sys_rst     = Input(Bool())
    val dcm_locked           = Input(Bool())
    val mb_reset             = Output(Bool())
    val bus_struct_reset     = Output(Bool())
    val peripheral_reset     = Output(Bool())
    val interconnect_aresetn = Output(Bool())
    val peripheral_aresetn   = Output(Bool())
  })
}

// Xilinx differential buffer IBUFG
class IBUFG extends BlackBox{
  val io = IO(new Bundle {
    val O  = Output(Clock())
    val I  = Input(Clock())
  })
}

// Xilinx buffer BUFIO
class BUFIO extends BlackBox{
  val io = IO(new Bundle {
    val O  = Output(Clock())
    val I  = Input(Clock())
  })
}

// Xilinx BUFR
class BUFR(BUFR_DIVIDE : String = "5", SIM_DEVICE : String = "7SERIES") extends BlackBox(
  Map("BUFR_DIVIDE" -> StringParam(BUFR_DIVIDE), "SIM_DEVICE" -> StringParam(SIM_DEVICE))) {
  val io = IO(new Bundle {
    val O  = Output(Clock())
    val I  = Input(Clock())
    val CLR = Input(Bool())
    val CE = Input(Bool())
  })
}

// Xilinx differential buffer IBUFDS
class IBUFDS(
  CAPACITANCE : String = "DONT_CARE",
  DIFF_TERM : Boolean = false,
  DQS_BIAS : Boolean = false,
  IBUF_DELAY_VALUE : Int = 0,
  IBUF_LOW_PWR : Boolean = true,
  IFD_DELAY_VALUE : String = "AUTO",
  IOSTANDARD : String = "DEFAULT"
)
extends BlackBox(
  Map(
  "CAPACITANCE" -> StringParam(CAPACITANCE),
  "DIFF_TERM" -> booleanToVerilogStringParam(DIFF_TERM),
  "DQS_BIAS" -> booleanToVerilogStringParam(DQS_BIAS),
  "IBUF_DELAY_VALUE" -> IntParam(IBUF_DELAY_VALUE),
  "IBUF_LOW_PWR" -> booleanToVerilogStringParam(IBUF_LOW_PWR),
  "IFD_DELAY_VALUE" -> StringParam(IFD_DELAY_VALUE),
  "IOSTANDARD" -> StringParam(IOSTANDARD)
  )
) {
  val io = IO(new Bundle {
    val O  = Output(Bool())
    val I  = Input(Bool())
    val IB = Input(Bool())
  })
}

// Xilinx buffer BUFG
class BUFG extends BlackBox {
  val io = IO(new Bundle {
    val O = Output(Clock())
    val I = Input(Clock())
  })
}

// Xilinx differential buffer OBUFDS
class OBUFDS(val paramsOBUFDS: Map[String, Param]) extends BlackBox(paramsOBUFDS){
  val io = IO(new Bundle {
    val O  = Output(Bool())
    val OB = Output(Bool())
    val I  = Input(Bool())
  })
}


// This is a FPGA-Only construct, which uses
// 'initial' constructions
class PowerOnResetFPGAOnly extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val power_on_reset = Output(Bool())
  })
}