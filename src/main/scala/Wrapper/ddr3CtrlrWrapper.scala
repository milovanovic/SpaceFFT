package spacefft.ddrwrapper

import chisel3._
import chisel3.experimental._

class ddr3IO extends Bundle {
  //////// signals to ddr3 memory ////////////////////////////
  val o_MemClk_p = Output(Bool())
  val sys_clk = Input(Clock())
  val clk_ref = Input(Clock())
  val ddr3_dq  = Analog((16.W))  // this one should be replaced with inout
  val ddr3_addr = Output(UInt(15.W))
  //output wire [14:0] ddr3_addr,
  val ddr3_ba = Output(UInt(3.W))
  val ddr3_ras_n = Output(Bool())
  val ddr3_cas_n = Output(Bool())
  val ddr3_we_n = Output(Bool())
  val ddr3_reset_n = Output(Bool())
  val ddr3_odt = Output(Bool())
  val ddr3_cke = Output(Bool())
  val ddr3_dm = Output(UInt(2.W))
  val ddr3_dqs_p = Analog((2.W)) // this one should be replaced with inout
  val ddr3_dqs_n = Analog((2.W)) // this one should be replaced with inout

  val ddr3_ck_p = Output(Bool())
  val ddr3_ck_n = Output(Bool())
}

class ethIO extends Bundle {
  //////// signals to  ethernet fifo /////////////////////////
  val o_data_eth = Output(UInt(32.W))
  val o_start_eth = Output(Bool())
  val o_we_eth = Output(Bool())
  val i_ready_eth = Input(Bool())
}

class axi4StreamInRangeIO extends Bundle { // try to replace this one
  val s_axis_tvalid_r  = Input(Bool())
  val s_axis_tready_r  = Output(Bool())
  val s_axis_tdata_r   = Input(UInt(32.W))
  val s_axis_tlast_r   = Input(Bool())
}

class axi4StreamOutDopplerIO extends Bundle { // try to replace this one
  val m_axis_tvalid_d  = Output(Bool())
  val m_axis_tready_d  = Input(Bool())
  val m_axis_tdata_d   = Output(UInt(32.W))
  val m_axis_tlast_d   = Output(Bool())
}

class ddr3CtrlrWrapper extends BlackBox {
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

      val o_data_eth = Output(UInt(32.W))
      val o_start_eth = Output(Bool())
      val o_we_eth = Output(Bool())
      val i_ready_eth = Input(Bool())

      val o_MemClk_p = Output(Bool())
      val sys_clk = Input(Clock())
      val clk_ref = Input(Clock())
      val ddr3_dq  = Analog((16.W))  // this one should be replaced with inout
      val ddr3_addr = Output(UInt(15.W))
      //output wire [14:0] ddr3_addr,
      val ddr3_ba = Output(UInt(3.W))
      val ddr3_ras_n = Output(Bool())
      val ddr3_cas_n = Output(Bool())
      val ddr3_we_n = Output(Bool())
      val ddr3_reset_n = Output(Bool())
      val ddr3_odt = Output(Bool())
      val ddr3_cke = Output(Bool())
      val ddr3_dm = Output(UInt(2.W))
      val ddr3_dqs_p = Analog((2.W)) // this one should be replaced with inout
      val ddr3_dqs_n = Analog((2.W)) // this one should be replaced with inout

      val ddr3_ck_p = Output(Bool())
      val ddr3_ck_n = Output(Bool())


    })

    //val ddr3IO = IO(new ddr3IO())
    //val ethIO  = IO(new ethIO())
}


