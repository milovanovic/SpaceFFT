package spacefft.ddrwrapper

import chisel3._

class DDRIO (val ddr4cond: Boolean, val ddr3cond: Boolean) extends Bundle {
    // DDR4
    val ddr4 = if (ddr4cond) Some(new ddr4IO()) else None
    val eth4 = if (ddr4cond) Some(new ethIO_ddr4()) else None
    val clk_300_p = if (ddr4cond) Some(Input(Clock())) else None
    val clk_300_n = if (ddr4cond) Some(Input(Clock())) else None
    val c0_init_calib_complete = if (ddr4cond) Some(Output(Bool())) else None

    // DDR3
    val ddr3 = if (ddr3cond) Some(new ddr3IO()) else None
    val eth3 = if (ddr3cond) Some(new ethIO()) else None
}
object DDRIO {
  def apply(ddr4cond: Boolean, ddr3cond: Boolean): DDRIO = new DDRIO(ddr4cond, ddr3cond)
}