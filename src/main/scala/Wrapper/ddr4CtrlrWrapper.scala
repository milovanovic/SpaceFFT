package spacefft.ddrwrapper

import chisel3._
import chisel3.experimental._

class ddr4IO extends Bundle {
  val o_MemClk_p = Output(Bool())
  val sys_clk = Input(Bool())
 //val clk_ref = Input(Bool())
 // val clk_300_p = Input(Bool())
 // val clk_300_n = Input(Bool())
  val c0_ddr4_dq  = Analog(64.W)//Input(UInt(64.W)) // should be inout
  val c0_ddr4_adr = Output(UInt(17.W))
  val c0_ddr4_ba = Output(UInt(2.W))
  val c0_ddr4_reset_n = Output(Bool())
  val c0_ddr4_cs_n = Output(Bool())
  val c0_ddr4_odt = Output(Bool())
  val c0_ddr4_bg = Output(Bool())
  val c0_ddr4_act_n = Output(Bool())
  val c0_ddr4_cke = Output(Bool())
  val c0_ddr4_dm_dbi_n = Analog(8.W)//Input(UInt(8.W)) // should be inout
  val c0_ddr4_dqs_c = Analog(8.W)//Input(UInt(8.W)) // should be inout
  val c0_ddr4_dqs_t = Analog(8.W)//Input(UInt(8.W)) // should be inout
  val c0_ddr4_ck_c = Output(Bool())
  val c0_ddr4_ck_t = Output(Bool())
}


class ethIO_ddr4 extends Bundle {
  //////// signals to  ethernet fifo /////////////////////////
  val o_data_eth = Output(UInt(64.W))
  val o_start_eth = Output(Bool())
  val o_we_eth = Output(Bool())
  val i_ready_eth = Input(Bool())
}

class ddr4CtrlrWrapper extends BlackBox {
    val io = IO(new Bundle {
      val s_axis_aresetn = Input(Bool())
      val s_axis_aclk    = Input(Clock())
      val mem_reset      = Input(Bool())
        //////////////////////////////////////////////////////////////////////
        ////////////// range FFT in axi4 stream for all 16 Rxs ////////////////
        //////////////////////////////////////////////////////////////////////
      val s_axis_tvalid_r = Input(Vec(16,  Bool()))
      val s_axis_tready_r = Output(Vec(16, Bool()))
      val s_axis_tdata_r  = Input(Vec(16,  UInt(32.W)))
      val s_axis_tlast_r  = Input(Vec(16,  Bool()))

      //////////////////////////////////////////////////////////////////////
      ////////////// doppler FFT in axi4 stream for all 16 Rxs /////////////
      //////////////////////////////////////////////////////////////////////
      val s_axis_tvalid_d = Input(Vec(16,  Bool()))
      val s_axis_tready_d = Output(Vec(16, Bool()))
      val s_axis_tdata_d  = Input(Vec(16,  UInt(32.W)))
      val s_axis_tlast_d  = Input(Vec(16,  Bool()))

      //////////////////////////////////////////////////////////////////////
      ////////////// doppler FFT out axi4 stream for all 16 Rxs ////////////
      //////////////////////////////////////////////////////////////////////
      val m_axis_tvalid_d = Output(Vec(16, Bool()))
      val m_axis_tready_d = Input(Vec(16,  Bool()))
      val m_axis_tdata_d  = Output(Vec(16, UInt(32.W)))
      val m_axis_tlast_d  = Output(Vec(16, Bool()))

      val m_axis_aclk         = Input(Clock())

      val o_data_eth = Output(UInt(64.W))
      val o_start_eth = Output(Bool())
      val o_we_eth = Output(Bool())
      val i_ready_eth = Input(Bool())
      val c0_init_calib_complete = Output(Bool())

      val o_MemClk_p = Output(Bool())
      val sys_clk = Input(Bool())
      //val clk_ref = Input(Bool())
      val clk_300_p = Input(Clock())
      val clk_300_n = Input(Clock())
      val c0_ddr4_dq  = Analog(64.W) //Input(UInt(64.W)) // should be inout
      val c0_ddr4_adr = Output(UInt(17.W))
      val c0_ddr4_ba = Output(UInt(2.W))
      val c0_ddr4_reset_n = Output(Bool())
      val c0_ddr4_cs_n = Output(Bool())
      val c0_ddr4_odt = Output(Bool())
      val c0_ddr4_bg = Output(Bool())
      val c0_ddr4_act_n = Output(Bool())
      val c0_ddr4_cke = Output(Bool())
      val c0_ddr4_dm_dbi_n = Analog(8.W)//Input(UInt(8.W)) // should be inout
      val c0_ddr4_dqs_c = Analog(8.W)//Input(UInt(8.W)) // should be inout
      val c0_ddr4_dqs_t = Analog(8.W)//Input(UInt(8.W)) // should be inout
      val c0_ddr4_ck_c = Output(Bool())
      val c0_ddr4_ck_t = Output(Bool())
  })
}

