// SPDX-License-Identifier: Apache-2.0
package spacefft.scope 

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