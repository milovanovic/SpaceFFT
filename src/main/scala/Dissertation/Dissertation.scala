// SPDX-License-Identifier: Apache-2.0

package dissertation

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, FixedPoint}

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._

import spacefft.ddrwrapper._
import spacefft.fft2control._
import spacefft.measurement._

import dsputils._

import xWRDataPreProc._
import fft._
import windowing._
import magnitude._
import accumulator._
import adder._
import cfar._

class AXI4Dissertation[T <: Data : Real: BinaryRepresentation](params: DissertationParameters[T], beatBytes: Int)(implicit p: Parameters) extends Dissertation[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) {
  /* Optional memory mapped port */
  val bus = if (blocks_mem.isEmpty) None else Some(LazyModule(new AXI4Xbar))
  val mem = if (blocks_mem.isEmpty) None else Some(bus.get.node)
  for (b <- blocks_mem) {
    b.mem.foreach { _ := AXI4Buffer() := bus.get.node }
  }
  if (detectSignal != None) {
    detectSignal.get.mem.get := AXI4Buffer() := bus.get.node
    delayMeasurement.get.mem.get := AXI4Buffer() := bus.get.node
  }
}

class DissertationIO(val meas: Boolean, val channels: Int) extends Bundle {
  // Measurements IOs
  val delayTime = if (meas) Some(Vec(channels, Output(Bool()))) else None
  val trigger   = if (meas) Some(Output(Bool())) else None
}
object DissertationIO {
  def apply(meas: Boolean, channels: Int): DissertationIO = new DissertationIO(meas, channels)
}

trait AXI4DissertationPins extends AXI4Dissertation[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  // streamNode
  val ioInNode_1D  = Seq.fill(streamNodeIn1D.length) { BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes/2))) }
  val ioOutNode_1D = if (detectSignal == None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
  val ioOutNode_2D = Seq.fill(streamNodeOut2D.length) { BundleBridgeSink[AXI4StreamBundle]() }

  (streamNodeIn1D, ioInNode_1D).zipped.map{ (out, in) => { out := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes/2)) := in }}
  if (ioOutNode_1D != None) { ioOutNode_1D.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNodeOut1D.get }
  (ioOutNode_2D, streamNodeOut2D).zipped.map{ (out, in) => { out := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := in }}

  /* 1D input streamNode pins */
  ioInNode_1D.zipWithIndex.map{ case (m, i) => {
      implicit val valName = ValName(s"in_1D_${i}")
      val in_1D = InModuleBody { m.makeIO() }
    }
  }
  /* 1D output streamNode pins */
  val out_1D = if (ioOutNode_1D != None) InModuleBody { ioOutNode_1D.get.makeIO() } else None
  /* 2D output streamNode pins */
  ioOutNode_2D.zipWithIndex.map{ case (m, i) => {
      implicit val valName = ValName(s"out_2D_${i}")
      val out_2D = InModuleBody { m.makeIO() }
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
}

/* DigItal Signal proceSsing for EaRly TArget deTectION - DISSERTATION */
abstract class Dissertation [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: DissertationParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) {
  /* 2D fft condition */
  val fft2DCond = params.fft1DParams != None && params.fft2DParams != None
  /* DDR condition */
  val ddrCond = fft2DCond && (params.ddrParams != None)

  /* Block Type */
  type Block = AXI4DspBlock

  /* Range */
  val preproc = if (params.prep1DParams != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4xWRdataPreProcBlock(AddressSet(params.prep1DParams.get.prepAddress + i*0x100, 0xFF), params.prep1DParams.get.prepParams, beatBytes)) }) else None
  val win_1D  = if (params.win1DParams  != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new WindowingBlock(csrAddress = AddressSet(params.win1DParams.get.winCSRAddress + i*0x100, 0xFF), ramAddress = AddressSet(params.win1DParams.get.winRAMAddress + i*0x1000, 0xFFF), params.win1DParams.get.winParams, beatBytes = beatBytes)) }) else None
  val fft_1D  = if (params.fft1DParams  != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4FFTBlock(address = AddressSet(params.fft1DParams.get.fftAddress + i*0x100, 0xFF), params = params.fft1DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false)) }) else None
  val mag_1D  = if (params.mag1DParams  != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4LogMagMuxBlock(params.mag1DParams.get.magParams, AddressSet(params.mag1DParams.get.magAddress + i*0x100, 0xFF), _beatBytes = beatBytes)) }) else None
  val acc_1D  = if (params.acc1DParams  != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4AccChainBlock(params.acc1DParams.get.accParams, AddressSet(params.acc1DParams.get.accAddress + i*0x100, 0xFF), params.acc1DParams.get.accQueueBase, beatBytes)) }) else None
  val cfar_1D = if (params.cfar1DParams != None) Some(LazyModule(new AXI4CFARBlock(params.cfar1DParams.get.cfarParams, AddressSet(params.cfar1DParams.get.cfarAddress, 0xFF), _beatBytes = beatBytes))) else None
  
  /* Non-coherent adder */
  val adder = LazyModule(new NonCoherentAdder(params.adderParams))

  /* Doppler */
  val ctrl_2D = if (fft2DCond && params.ddrParams == None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4StreamFFT2ControlBlock(params.ctrl2DParams.get.ctrl2DParams, AddressSet(params.ctrl2DParams.get.ctrl2DAddress + i*0x100, 0xFF), _beatBytes = beatBytes)) }) else None
  val fft_2D  = if (fft2DCond) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4FFTBlock(address = AddressSet(params.fft2DParams.get.fftAddress + i*0x100, 0xFF), params = params.fft2DParams.get.fftParams, _beatBytes = beatBytes, configInterface = false)) }) else None
  val mag_2D  = if (params.mag2DParams != None && fft2DCond) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4LogMagMuxBlock(params.mag2DParams.get.magParams, AddressSet(params.mag2DParams.get.magAddress + i*0x100, 0xFF), _beatBytes = beatBytes)) }) else None

  val split = if (fft2DCond) Some(Seq.tabulate(params.dataChannels) { i => LazyModule(new AXI4Splitter(address = AddressSet(params.splitParams.get.splitAddress + i*0x100, 0xFF), beatBytes){
    val out = if (ddrCond) Some({
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
      ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode
      InModuleBody { ioOutNode.makeIO() }
    }) else None
  })}) else None
  
  val queue = if (fft2DCond) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4DspQueueWithSyncReadMem(params.queueParams.get.queueParams, AddressSet(params.queueParams.get.queueAddress + i*0x1000, 0xFFF), _beatBytes = beatBytes){
    val in = if (ddrCond) Some({
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
      streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
      InModuleBody { ioInNode.makeIO() }
    }) else None
  })}) else None

  /* Measurement */
  val detectSignal = if (params.measurementParams != None) Some(LazyModule(new AXI4DetectSignal(params.measurementParams.get.detectSignalParams, AddressSet(params.measurementParams.get.detectSignalAddress, 0xFF), beatBytes){
    /* IOs */
    def makeCustomIO(): DetectSignalIO = {
      val io2: DetectSignalIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })) else None
  val splitMeasurement = if (params.measurementParams != None) Some(Seq.tabulate(params.dataChannels){ i => LazyModule(new AXI4Splitter(address = AddressSet(params.measurementParams.get.splitAddress + i*0x100, 0xFF), beatBytes))}) else None

  val delayMeasurement = if (params.measurementParams != None) Some(LazyModule(new AXI4DelayMeasurement(params.measurementParams.get.delayMeasurementParams, AddressSet(params.measurementParams.get.delayMeasurementAddress, 0xFF), beatBytes){
    /* IOs */
    def makeCustomIO(): DelayMeasurementIO = {
      val io2: DelayMeasurementIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })) else None

  if (fft2DCond && params.ddrParams == None) (ctrl_2D.get, split.get).zipped.map{ (o, i) => { o.streamNode := i.streamNode }}

  /* 1D Blocks */
  val blocks_1D = Seq.tabulate(params.dataChannels){ i => 
    Seq(splitMeasurement, preproc, win_1D, fft_1D, split, mag_1D, acc_1D).foldLeft(Seq[Block]()) { (block, elem) =>
      // If elem is None, don't append it to the chain
      if (elem != None) block :+ elem.get(i) else block
    }
  }
  /* 2D Blocks */
  val blocks_2D = Seq.tabulate(params.dataChannels){ i => 
    Seq(ctrl_2D, queue, fft_2D, mag_2D).foldLeft(Seq[Block]()) { (block, elem) =>
      // If elem is None, don't append it to the chain
      if (elem != None) block :+ elem.get(i) else block
    }
  }
  /* Mem Blocks */
  val blocks_mem = (Seq(cfar_1D).flatten ++ blocks_1D.flatten) ++ blocks_2D.flatten
  
  /* Connect 1D nodes */
  lazy val connections_1D = Seq.tabulate(blocks_1D.length) {i => for (j <- 1 until blocks_1D(i).length) yield (blocks_1D(i)(j), blocks_1D(i)(j-1))}
  connections_1D.foreach { m => 
    for ((lhs, rhs) <- m) {
      lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
    }
  }
  /* Sum all 1D chain outputs and connect adder to cfar */
  blocks_1D.foreach { m => adder.streamNode := m.last.streamNode }
  cfar_1D.get.streamNode := adder.streamNode

  /* Connect 2D nodes */
  lazy val connections_2D = Seq.tabulate(blocks_2D.length) {i => for (j <- 1 until blocks_2D(i).length) yield (blocks_2D(i)(j), blocks_2D(i)(j-1))}
  connections_2D.foreach { m => 
    for ((lhs, rhs) <- m) {
      lhs.streamNode := AXI4StreamBuffer() := rhs.streamNode
    }
  }

  // Connect measurement
  if (params.measurementParams != None) {
    for(i <- 0 until params.dataChannels) {
      detectSignal.get.inNodes(i) := splitMeasurement.get(i).streamNode
    }
    detectSignal.get.inNodes(params.dataChannels) := cfar_1D.get.streamNode
  }

  /* Optional streamNodes */
  val streamNodeIn1D : Seq[AXI4StreamNodeHandle] = blocks_1D.map{m => m.head.streamNode}
  val streamNodeOut1D: Option[AXI4StreamNodeHandle] = if (detectSignal == None) Some(cfar_1D.get.streamNode) else None
  val streamNodeOut2D: Seq[AXI4StreamNodeHandle] = blocks_2D.map{m => m.last.streamNode}

  /* Optional CRC and HDMI pins */
  lazy val io = Wire(new DissertationIO(delayMeasurement != None, if(delayMeasurement != None) params.measurementParams.get.delayMeasurementParams.channels else 0))

  /* Module */
  lazy val module = new LazyModuleImp(this) {
    if (params.measurementParams != None) {
      (delayMeasurement.get.ioBlock.data_first, detectSignal.get.ioBlock.out_data_first).zipped.map{ (o, i) => { o := i } }
      (delayMeasurement.get.ioBlock.data_last, detectSignal.get.ioBlock.out_data_last).zipped.map{ (i, o) => { i := o } }
      (io.delayTime.get, delayMeasurement.get.ioBlock.delayTime).zipped.map{ (i, o) => { i := o } }
      io.trigger.get := delayMeasurement.get.ioBlock.trigger
    }

    /* If doppler FFT exists, generate wrapper */
    val mem_reset = if (ddrCond) Some(IO(Input(Bool()))) else None
    val reset_n   = if (ddrCond) Some(IO(Input(Bool()))) else None

    /* IOs */
    val ddrIO = if (ddrCond) Some(IO(new DDRIO(params.ddrParams.get.ddrParams == DDR4, params.ddrParams.get.ddrParams == DDR3))) else None

    /* DDR wrappers */
    if (ddrCond) {
      /* DDR4 */
      if (params.ddrParams.get.ddrParams == DDR4) {
        val ddr4CtrlrWrapper = Module(new ddr4CtrlrWrapper)

        /////////////// RANGE ///////////////////////

        for (i <- 0 until ddr4CtrlrWrapper.io.s_axis_tdata_r.length) {
          if (i < split.get.length) {
            ddr4CtrlrWrapper.io.s_axis_tdata_r(i)  := split.get(i).out.get.bits.data
            ddr4CtrlrWrapper.io.s_axis_tvalid_r(i) := split.get(i).out.get.valid
            ddr4CtrlrWrapper.io.s_axis_tlast_r(i)  := split.get(i).out.get.bits.last
            split.get(i).out.get.ready := true.B
          }
          else {
            ddr4CtrlrWrapper.io.s_axis_tdata_r(i)  := 0.U
            ddr4CtrlrWrapper.io.s_axis_tvalid_r(i) := false.B
            ddr4CtrlrWrapper.io.s_axis_tlast_r(i)  := false.B
          }
        }

        ///////////////// DOPPLER /////////////////////
        for (i <- 0 until ddr4CtrlrWrapper.io.s_axis_tdata_d.length) {
          ddr4CtrlrWrapper.io.s_axis_tdata_d(i)  := 0.U
          ddr4CtrlrWrapper.io.s_axis_tvalid_d(i) := false.B
          ddr4CtrlrWrapper.io.s_axis_tlast_d(i)  := false.B
        }

        ///////////////////////////////////////////////////
        ////////////////////// Doppler OUT /////////////////////////////////////////////
        // drive receiver one to doppler fft!
        for (i <- 0 until ddr4CtrlrWrapper.io.s_axis_tdata_d.length) {
          if (i < queue.get.length) {
            queue.get(i).in.get.bits.data := ddr4CtrlrWrapper.io.m_axis_tdata_d(i)
            queue.get(i).in.get.valid     := ddr4CtrlrWrapper.io.m_axis_tvalid_d(i)
            queue.get(i).in.get.bits.last := ddr4CtrlrWrapper.io.m_axis_tlast_d(i)
            ddr4CtrlrWrapper.io.m_axis_tready_d(i) := queue.get(i).in.get.ready
          }
        }

        ddr4CtrlrWrapper.io.s_axis_aclk := clock
        ddr4CtrlrWrapper.io.m_axis_aclk := clock
        ddr4CtrlrWrapper.io.s_axis_aresetn := reset_n.get
        ddr4CtrlrWrapper.io.mem_reset := mem_reset.get

        // ethernet signals
        ddrIO.get.eth4.get.o_data_eth := ddr4CtrlrWrapper.io.o_data_eth
        ddrIO.get.eth4.get.o_start_eth := ddr4CtrlrWrapper.io.o_start_eth
        ddrIO.get.eth4.get.o_we_eth := ddr4CtrlrWrapper.io.o_we_eth
        ddr4CtrlrWrapper.io.i_ready_eth := ddrIO.get.eth4.get.i_ready_eth

        //////////////////////////////////////////// ddr4 memory specific signals///////////////////////

        // input connections
        ddr4CtrlrWrapper.io.sys_clk := ddrIO.get.ddr4.get.sys_clk
        ddr4CtrlrWrapper.io.clk_300_p := ddrIO.get.clk_300_p.get
        ddr4CtrlrWrapper.io.clk_300_n := ddrIO.get.clk_300_n.get

        // inout connections
        ddr4CtrlrWrapper.io.c0_ddr4_dq <> ddrIO.get.ddr4.get.c0_ddr4_dq
        ddr4CtrlrWrapper.io.c0_ddr4_dm_dbi_n <> ddrIO.get.ddr4.get.c0_ddr4_dm_dbi_n
        ddr4CtrlrWrapper.io.c0_ddr4_dqs_c <> ddrIO.get.ddr4.get.c0_ddr4_dqs_c
        ddr4CtrlrWrapper.io.c0_ddr4_dqs_t <> ddrIO.get.ddr4.get.c0_ddr4_dqs_t

        // output connections
        ddrIO.get.ddr4.get.o_MemClk_p  := ddr4CtrlrWrapper.io.o_MemClk_p
        ddrIO.get.ddr4.get.c0_ddr4_ba := ddr4CtrlrWrapper.io.c0_ddr4_ba
        ddrIO.get.ddr4.get.c0_ddr4_reset_n := ddr4CtrlrWrapper.io.c0_ddr4_reset_n
        ddrIO.get.ddr4.get.c0_ddr4_cs_n := ddr4CtrlrWrapper.io.c0_ddr4_cs_n
        ddrIO.get.ddr4.get.c0_ddr4_odt := ddr4CtrlrWrapper.io.c0_ddr4_odt
        ddrIO.get.ddr4.get.c0_ddr4_bg := ddr4CtrlrWrapper.io.c0_ddr4_bg
        ddrIO.get.ddr4.get.c0_ddr4_act_n := ddr4CtrlrWrapper.io.c0_ddr4_act_n
        ddrIO.get.ddr4.get.c0_ddr4_cke := ddr4CtrlrWrapper.io.c0_ddr4_cke
        ddrIO.get.ddr4.get.c0_ddr4_ck_c := ddr4CtrlrWrapper.io.c0_ddr4_ck_c
        ddrIO.get.ddr4.get.c0_ddr4_ck_t := ddr4CtrlrWrapper.io.c0_ddr4_ck_t
        ddrIO.get.ddr4.get.c0_ddr4_adr := ddr4CtrlrWrapper.io.c0_ddr4_adr

        ddrIO.get.c0_init_calib_complete.get := ddr4CtrlrWrapper.io.c0_init_calib_complete
      }
      /* DDR3 */
      else {
        val ddr3CtrlrWrapper = Module(new ddr3CtrlrWrapper) // .io.signals

        /////////////// RANGE ///////////////////////

        for (i <- 0 until ddr3CtrlrWrapper.io.s_axis_tdata_r.length) {
          if (i < split.get.length) {
            ddr3CtrlrWrapper.io.s_axis_tdata_r(i)  := split.get(i).out.get.bits.data
            ddr3CtrlrWrapper.io.s_axis_tvalid_r(i) := split.get(i).out.get.valid
            ddr3CtrlrWrapper.io.s_axis_tlast_r(i)  := split.get(i).out.get.bits.last
            split.get(i).out.get.ready := true.B
          }
          else {
            ddr3CtrlrWrapper.io.s_axis_tdata_r(i)  := 0.U
            ddr3CtrlrWrapper.io.s_axis_tvalid_r(i) := false.B
            ddr3CtrlrWrapper.io.s_axis_tlast_r(i)  := false.B
          }
        }

        ///////////////// DOPPLER /////////////////////
        for (i <- 0 until ddr3CtrlrWrapper.io.s_axis_tdata_d.length) {
          ddr3CtrlrWrapper.io.s_axis_tdata_d(i)  := 0.U
          ddr3CtrlrWrapper.io.s_axis_tvalid_d(i) := false.B
          ddr3CtrlrWrapper.io.s_axis_tlast_d(i)  := false.B
        }

        ///////////////////////////////////////////////////
        ////////////////////// Doppler OUT /////////////////////////////////////////////
        // drive receiver one to doppler fft!
        for (i <- 0 until ddr3CtrlrWrapper.io.s_axis_tdata_d.length) {
          if (i < queue.get.length) {
            queue.get(i).in.get.bits.data := ddr3CtrlrWrapper.io.m_axis_tdata_d(i)
            queue.get(i).in.get.valid     := ddr3CtrlrWrapper.io.m_axis_tvalid_d(i)
            queue.get(i).in.get.bits.last := ddr3CtrlrWrapper.io.m_axis_tlast_d(i)
            ddr3CtrlrWrapper.io.m_axis_tready_d(i) := queue.get(i).in.get.ready
          }
        }
        

        //// ethernet signals
        ddrIO.get.eth3.get.o_data_eth := ddr3CtrlrWrapper.io.o_data_eth
        ddrIO.get.eth3.get.o_start_eth := ddr3CtrlrWrapper.io.o_start_eth
        ddrIO.get.eth3.get.o_we_eth := ddr3CtrlrWrapper.io.o_we_eth
        ddr3CtrlrWrapper.io.i_ready_eth := ddrIO.get.eth3.get.i_ready_eth

        ddrIO.get.ddr3.get.o_MemClk_p := ddr3CtrlrWrapper.io.o_MemClk_p
        ddrIO.get.ddr3.get.ddr3_addr := ddr3CtrlrWrapper.io.ddr3_addr

        ddrIO.get.ddr3.get.ddr3_ba := ddr3CtrlrWrapper.io.ddr3_ba
        ddrIO.get.ddr3.get.ddr3_ras_n := ddr3CtrlrWrapper.io.ddr3_ras_n
        ddrIO.get.ddr3.get.ddr3_cas_n := ddr3CtrlrWrapper.io.ddr3_cas_n
        ddrIO.get.ddr3.get.ddr3_we_n := ddr3CtrlrWrapper.io.ddr3_we_n
        ddrIO.get.ddr3.get.ddr3_reset_n := ddr3CtrlrWrapper.io.ddr3_reset_n
        ddrIO.get.ddr3.get.ddr3_odt := ddr3CtrlrWrapper.io.ddr3_odt
        ddrIO.get.ddr3.get.ddr3_cke := ddr3CtrlrWrapper.io.ddr3_cke
        ddrIO.get.ddr3.get.ddr3_dm := ddr3CtrlrWrapper.io.ddr3_dm
        ddrIO.get.ddr3.get.ddr3_ck_p := ddr3CtrlrWrapper.io.ddr3_ck_p
        ddrIO.get.ddr3.get.ddr3_ck_n := ddr3CtrlrWrapper.io.ddr3_ck_n

        ddr3CtrlrWrapper.io.s_axis_aclk := clock
        ddr3CtrlrWrapper.io.m_axis_aclk := clock
        ddr3CtrlrWrapper.io.s_axis_aresetn := reset_n.get // just temporary solution for reset signal it should be active in 1, that needs to be changed
        ddr3CtrlrWrapper.io.mem_reset := mem_reset.get

        ddr3CtrlrWrapper.io.sys_clk := ddrIO.get.ddr3.get.sys_clk
        ddr3CtrlrWrapper.io.clk_ref := ddrIO.get.ddr3.get.clk_ref
        ddr3CtrlrWrapper.io.ddr3_dq <> ddrIO.get.ddr3.get.ddr3_dq        // this one should be replaced with inout
        ddr3CtrlrWrapper.io.ddr3_dqs_p <> ddrIO.get.ddr3.get.ddr3_dqs_p  // this one should be replaced with inout
        ddr3CtrlrWrapper.io.ddr3_dqs_n <> ddrIO.get.ddr3.get.ddr3_dqs_n  // this one should be replaced with inout
      }
    }
  }
}

object DissertationMeasurementDDRApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new DissertationMeasurementParams(rangeFFTSize = 1024, dopplerFFTSize = 256, ddrType = DDR3, channels = 4)).params
  val lazyDut = LazyModule(new AXI4Dissertation(params, 4) with AXI4DissertationPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/DissertationMeasurement"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
