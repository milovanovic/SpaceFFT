package spacefft.ddrwrapper

import chisel3._
import chisel3.experimental._

class ddr3IO extends Bundle {
  //////// signals to ddr3 memory ////////////////////////////
  val o_MemClk_p = Output(Bool())
  val sys_clk = Input(Bool())
  val clk_ref = Input(Bool())
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
      val s_axis_tvalid_r_0  = Input(Bool())
      val s_axis_tready_r_0  = Output(Bool())
      val s_axis_tdata_r_0   = Input(UInt(32.W))
      val s_axis_tlast_r_0   = Input(Bool())

      val s_axis_tvalid_r_1  = Input(Bool())
      val s_axis_tready_r_1  = Output(Bool())
      val s_axis_tdata_r_1   = Input(UInt(32.W))
      val s_axis_tlast_r_1   = Input(Bool())

      val s_axis_tvalid_r_2  = Input(Bool())
      val s_axis_tready_r_2  = Output(Bool())
      val s_axis_tdata_r_2   = Input(UInt(32.W))
      val s_axis_tlast_r_2   = Input(Bool())

      val s_axis_tvalid_r_3  = Input(Bool())
      val s_axis_tready_r_3  = Output(Bool())
      val s_axis_tdata_r_3   = Input(UInt(32.W))
      val s_axis_tlast_r_3   = Input(Bool())

      val s_axis_tvalid_r_4  = Input(Bool())
      val s_axis_tready_r_4  = Output(Bool())
      val s_axis_tdata_r_4   = Input(UInt(32.W))
      val s_axis_tlast_r_4   = Input(Bool())

      val s_axis_tvalid_r_5  = Input(Bool())
      val s_axis_tready_r_5  = Output(Bool())
      val s_axis_tdata_r_5   = Input(UInt(32.W))
      val s_axis_tlast_r_5   = Input(Bool())

      val s_axis_tvalid_r_6  = Input(Bool())
      val s_axis_tready_r_6  = Output(Bool())
      val s_axis_tdata_r_6   = Input(UInt(32.W))
      val s_axis_tlast_r_6   = Input(Bool())


      val s_axis_tvalid_r_7  = Input(Bool())
      val s_axis_tready_r_7  = Output(Bool())
      val s_axis_tdata_r_7   = Input(UInt(32.W))
      val s_axis_tlast_r_7   = Input(Bool())


      val s_axis_tvalid_r_8  = Input(Bool())
      val s_axis_tready_r_8  = Output(Bool())
      val s_axis_tdata_r_8   = Input(UInt(32.W))
      val s_axis_tlast_r_8   = Input(Bool())


      val s_axis_tvalid_r_9  = Input(Bool())
      val s_axis_tready_r_9  = Output(Bool())
      val s_axis_tdata_r_9   = Input(UInt(32.W))
      val s_axis_tlast_r_9   = Input(Bool())


      val s_axis_tvalid_r_10  = Input(Bool())
      val s_axis_tready_r_10  = Output(Bool())
      val s_axis_tdata_r_10   = Input(UInt(32.W))
      val s_axis_tlast_r_10   = Input(Bool())

      val s_axis_tvalid_r_11  = Input(Bool())
      val s_axis_tready_r_11  = Output(Bool())
      val s_axis_tdata_r_11   = Input(UInt(32.W))
      val s_axis_tlast_r_11   = Input(Bool())


      val s_axis_tvalid_r_12  = Input(Bool())
      val s_axis_tready_r_12  = Output(Bool())
      val s_axis_tdata_r_12   = Input(UInt(32.W))
      val s_axis_tlast_r_12   = Input(Bool())

      val s_axis_tvalid_r_13  = Input(Bool())
      val s_axis_tready_r_13  = Output(Bool())
      val s_axis_tdata_r_13   = Input(UInt(32.W))
      val s_axis_tlast_r_13   = Input(Bool())


      val s_axis_tvalid_r_14  = Input(Bool())
      val s_axis_tready_r_14  = Output(Bool())
      val s_axis_tdata_r_14   = Input(UInt(32.W))
      val s_axis_tlast_r_14   = Input(Bool())


      val s_axis_tvalid_r_15  = Input(Bool())
      val s_axis_tready_r_15  = Output(Bool())
      val s_axis_tdata_r_15   = Input(UInt(32.W))
      val s_axis_tlast_r_15   = Input(Bool())


      //////////////////////////////////////////////////////////////////////
      ////////////// doppler FFT in axi4 stream for all 16 Rxs /////////////
      //////////////////////////////////////////////////////////////////////
      val s_axis_tvalid_d_0  = Input(Bool())
      val s_axis_tready_d_0  = Output(Bool())
      val s_axis_tdata_d_0   = Input(UInt(32.W))
      val s_axis_tlast_d_0   = Input(Bool())

      val s_axis_tvalid_d_1  = Input(Bool())
      val s_axis_tready_d_1  = Output(Bool())
      val s_axis_tdata_d_1   = Input(UInt(32.W))
      val s_axis_tlast_d_1   = Input(Bool())


      val s_axis_tvalid_d_2  = Input(Bool())
      val s_axis_tready_d_2  = Output(Bool())
      val s_axis_tdata_d_2   = Input(UInt(32.W))
      val s_axis_tlast_d_2   = Input(Bool())


      val s_axis_tvalid_d_3  = Input(Bool())
      val s_axis_tready_d_3  = Output(Bool())
      val s_axis_tdata_d_3   = Input(UInt(32.W))
      val s_axis_tlast_d_3   = Input(Bool())


      val s_axis_tvalid_d_4  = Input(Bool())
      val s_axis_tready_d_4  = Output(Bool())
      val s_axis_tdata_d_4   = Input(UInt(32.W))
      val s_axis_tlast_d_4   = Input(Bool())


      val s_axis_tvalid_d_5  = Input(Bool())
      val s_axis_tready_d_5  = Output(Bool())
      val s_axis_tdata_d_5   = Input(UInt(32.W))
      val s_axis_tlast_d_5   = Input(Bool())


      val s_axis_tvalid_d_6  = Input(Bool())
      val s_axis_tready_d_6  = Output(Bool())
      val s_axis_tdata_d_6   = Input(UInt(32.W))
      val s_axis_tlast_d_6   = Input(Bool())


      val s_axis_tvalid_d_7  = Input(Bool())
      val s_axis_tready_d_7  = Output(Bool())
      val s_axis_tdata_d_7   = Input(UInt(32.W))
      val s_axis_tlast_d_7   = Input(Bool())


      val s_axis_tvalid_d_8  = Input(Bool())
      val s_axis_tready_d_8  = Output(Bool())
      val s_axis_tdata_d_8   = Input(UInt(32.W))
      val s_axis_tlast_d_8   = Input(Bool())


      val s_axis_tvalid_d_9  = Input(Bool())
      val s_axis_tready_d_9  = Output(Bool())
      val s_axis_tdata_d_9   = Input(UInt(32.W))
      val s_axis_tlast_d_9   = Input(Bool())


      val s_axis_tvalid_d_10  = Input(Bool())
      val s_axis_tready_d_10  = Output(Bool())
      val s_axis_tdata_d_10   = Input(UInt(32.W))
      val s_axis_tlast_d_10   = Input(Bool())


      val s_axis_tvalid_d_11  = Input(Bool())
      val s_axis_tready_d_11  = Output(Bool())
      val s_axis_tdata_d_11   = Input(UInt(32.W))
      val s_axis_tlast_d_11   = Input(Bool())

      val s_axis_tvalid_d_12  = Input(Bool())
      val s_axis_tready_d_12  = Output(Bool())
      val s_axis_tdata_d_12   = Input(UInt(32.W))
      val s_axis_tlast_d_12   = Input(Bool())


      val s_axis_tvalid_d_13  = Input(Bool())
      val s_axis_tready_d_13  = Output(Bool())
      val s_axis_tdata_d_13   = Input(UInt(32.W))
      val s_axis_tlast_d_13   = Input(Bool())


      val s_axis_tvalid_d_14  = Input(Bool())
      val s_axis_tready_d_14  = Output(Bool())
      val s_axis_tdata_d_14   = Input(UInt(32.W))
      val s_axis_tlast_d_14   = Input(Bool())


      val s_axis_tvalid_d_15  = Input(Bool())
      val s_axis_tready_d_15  = Output(Bool())
      val s_axis_tdata_d_15   = Input(UInt(32.W))
      val s_axis_tlast_d_15   = Input(Bool())


      //////////////////////////////////////////////////////////////////////
      ////////////// doppler FFT out axi4 stream for all 16 Rxs ////////////
      //////////////////////////////////////////////////////////////////////

      val m_axis_tvalid_d_0  = Output(Bool())
      val m_axis_tready_d_0  = Input(Bool())
      val m_axis_tdata_d_0   = Output(UInt(32.W))
      val m_axis_tlast_d_0   = Output(Bool())

      val m_axis_tvalid_d_1  = Output(Bool())
      val m_axis_tready_d_1  = Input(Bool())
      val m_axis_tdata_d_1   = Output(UInt(32.W))
      val m_axis_tlast_d_1   = Output(Bool())

      val m_axis_tvalid_d_2  = Output(Bool())
      val m_axis_tready_d_2  = Input(Bool())
      val m_axis_tdata_d_2   = Output(UInt(32.W))
      val m_axis_tlast_d_2   = Output(Bool())

      val m_axis_tvalid_d_3  = Output(Bool())
      val m_axis_tready_d_3  = Input(Bool())
      val m_axis_tdata_d_3   = Output(UInt(32.W))
      val m_axis_tlast_d_3   = Output(Bool())

      val m_axis_tvalid_d_4  = Output(Bool())
      val m_axis_tready_d_4  = Input(Bool())
      val m_axis_tdata_d_4   = Output(UInt(32.W))
      val m_axis_tlast_d_4   = Output(Bool())

      val m_axis_tvalid_d_5  = Output(Bool())
      val m_axis_tready_d_5  = Input(Bool())
      val m_axis_tdata_d_5   = Output(UInt(32.W))
      val m_axis_tlast_d_5   = Output(Bool())

      val m_axis_tvalid_d_6  = Output(Bool())
      val m_axis_tready_d_6  = Input(Bool())
      val m_axis_tdata_d_6   = Output(UInt(32.W))
      val m_axis_tlast_d_6   = Output(Bool())

      val m_axis_tvalid_d_7  = Output(Bool())
      val m_axis_tready_d_7  = Input(Bool())
      val m_axis_tdata_d_7   = Output(UInt(32.W))
      val m_axis_tlast_d_7   = Output(Bool())

      val m_axis_tvalid_d_8  = Output(Bool())
      val m_axis_tready_d_8  = Input(Bool())
      val m_axis_tdata_d_8   = Output(UInt(32.W))
      val m_axis_tlast_d_8   = Output(Bool())


      val m_axis_tvalid_d_9  = Output(Bool())
      val m_axis_tready_d_9  = Input(Bool())
      val m_axis_tdata_d_9   = Output(UInt(32.W))
      val m_axis_tlast_d_9   = Output(Bool())

      val m_axis_tvalid_d_10  = Output(Bool())
      val m_axis_tready_d_10  = Input(Bool())
      val m_axis_tdata_d_10   = Output(UInt(32.W))
      val m_axis_tlast_d_10   = Output(Bool())


      val m_axis_tvalid_d_11  = Output(Bool())
      val m_axis_tready_d_11  = Input(Bool())
      val m_axis_tdata_d_11   = Output(UInt(32.W))
      val m_axis_tlast_d_11   = Output(Bool())

      val m_axis_tvalid_d_12  = Output(Bool())
      val m_axis_tready_d_12  = Input(Bool())
      val m_axis_tdata_d_12   = Output(UInt(32.W))
      val m_axis_tlast_d_12   = Output(Bool())


      val m_axis_tvalid_d_13  = Output(Bool())
      val m_axis_tready_d_13  = Input(Bool())
      val m_axis_tdata_d_13   = Output(UInt(32.W))
      val m_axis_tlast_d_13   = Output(Bool())


      val m_axis_tvalid_d_14  = Output(Bool())
      val m_axis_tready_d_14  = Input(Bool())
      val m_axis_tdata_d_14   = Output(UInt(32.W))
      val m_axis_tlast_d_14   = Output(Bool())


      val m_axis_tvalid_d_15  = Output(Bool())
      val m_axis_tready_d_15  = Input(Bool())
      val m_axis_tdata_d_15   = Output(UInt(32.W))
      val m_axis_tlast_d_15   = Output(Bool())

      val m_axis_aclk         = Input(Clock())

      val o_data_eth = Output(UInt(32.W))
      val o_start_eth = Output(Bool())
      val o_we_eth = Output(Bool())
      val i_ready_eth = Input(Bool())

      val o_MemClk_p = Output(Bool())
      val sys_clk = Input(Bool())
      val clk_ref = Input(Bool())
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


