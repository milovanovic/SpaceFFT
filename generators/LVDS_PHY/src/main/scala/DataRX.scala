// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.util.{Cat, ShiftRegister}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.IO

import dspblocks._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters

import dsputils._

// DataRX parameters
case class DataRXParams(
  channels    : Int = 4,
  asyncParams : Option[AXI4StreamAsyncQueueWithControlParams] = None,
) 
{
  require(channels >= 1, "Number of channels must be greater or equal to one!")
}

class DataRXIO(channels: Int, async: Boolean) extends Bundle {
  val i_data   = Input(Vec(channels, UInt(8.W)))
  val i_valid  = Input(UInt(8.W))
  val i_frame  = Input(UInt(8.W))
  val o_crc    = Output(Bool())
  val o_word_size = Output(UInt(2.W))
  // asyncFIFO signals
  val i_async_clock = if (async) Some(Input(Clock())) else None
  val i_async_reset = if (async) Some(Input(Bool())) else None

  override def cloneType: this.type = DataRXIO(channels, async).asInstanceOf[this.type]
}
object DataRXIO {
  def apply(channels: Int, async: Boolean): DataRXIO = new DataRXIO(channels, async)
}

class AXI4StreamDataRX(params: DataRXParams)(implicit p: Parameters) extends DataRX[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params) with AXI4DspBlock {
  override val mem = None
}

trait DataRXPins extends AXI4StreamDataRX{
  // output stream node
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode 

  val out = InModuleBody { ioOutNode.makeIO() }

  def makeCustomIO(): DataRXIO = {
    val io2: DataRXIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

abstract class DataRX[D, U, E, O, B <: Data] (val params: DataRXParams) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

  lazy val io = Wire(new DataRXIO(params.channels, params.asyncParams != None))

  val reorder_data  = Seq.fill(params.channels) {LazyModule(new BitReordering with BitReorderingPins)}
  val reorder_valid = LazyModule(new BitReordering with BitReorderingPins)
  val reorder_frame = LazyModule(new BitReordering with BitReorderingPins)
  val bitslip       = LazyModule(new BitSlipDetection with BitSlipDetectionPins)
  val word_detector = LazyModule(new DetectWordWidth(DetectWordWidthParams(params.channels)) with DetectWordWidthPins)
  val byte2word     = LazyModule(new Byte2Word(Byte2WordParams(params.channels)) with Byte2WordPins)
  val asyncQueue    = if (params.asyncParams != None ) Some(LazyModule(new AXI4StreamAsyncQueueWithControlBlock(params.asyncParams.get){
    def beatBytes: Int = params.channels*2
    val ioInNode  = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

    val in  = InModuleBody { ioInNode.makeIO() }
    val out = InModuleBody { ioOutNode.makeIO() }
  })) else None

  // StreamNode
  val streamNode = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "outStream", n = params.channels*2, u = 0, numMasters = 1))
  

  lazy val module = new LazyModuleImp(this) {
    val out = streamNode.out(0)._1

    // bitslip
    bitslip.ioBlock.i_data := io.i_valid
    
    for(i <- 0 until params.channels) {
      // input data
      reorder_data(i).ioBlock.i_data    := RegNext(io.i_data(i), 0.U)
      reorder_data(i).ioBlock.i_bitslip := bitslip.ioBlock.o_bitslip
      // detect word from frame
      word_detector.ioBlock.i_data(i) := reorder_data(i).ioBlock.o_data
      // alligned data to word
      byte2word.ioBlock.i_data(i) := word_detector.ioBlock.o_data(i)
    }
    // input data
    reorder_valid.ioBlock.i_data  := RegNext(io.i_valid, 0.U)
    reorder_frame.ioBlock.i_data  := RegNext(io.i_frame, 0.U)
    //bitslip
    reorder_valid.ioBlock.i_bitslip  := bitslip.ioBlock.o_bitslip
    reorder_frame.ioBlock.i_bitslip  := bitslip.ioBlock.o_bitslip
    // detect word from frame
    word_detector.ioBlock.i_frame  := reorder_frame.ioBlock.o_data
    word_detector.ioBlock.i_en     := ShiftRegister(bitslip.ioBlock.o_en, 3, 0.U, true.B)
    // alligned data to word
    byte2word.ioBlock.i_word_size := word_detector.ioBlock.o_word_size
    byte2word.ioBlock.i_en        := word_detector.ioBlock.o_en
    byte2word.ioBlock.i_crc       := word_detector.ioBlock.o_crc

    if(asyncQueue != None) {
      // AsyncQueue (Slave-Side)
      asyncQueue.get.module.io.write_clock := clock
      asyncQueue.get.module.io.write_reset := reset
      asyncQueue.get.in.bits.data  := Cat(byte2word.ioBlock.o_data)
      asyncQueue.get.in.valid := byte2word.ioBlock.o_en
      asyncQueue.get.module.io.in_ctrl := Cat(byte2word.ioBlock.o_crc, word_detector.ioBlock.o_word_size)
      // AsyncQueue (Master-Side)
      asyncQueue.get.module.clock := io.i_async_clock.get
      asyncQueue.get.module.reset := io.i_async_reset.get
      io.o_crc := asyncQueue.get.module.io.out_ctrl(byte2word.ioBlock.o_crc.getWidth + word_detector.ioBlock.o_word_size.getWidth - 1)
      io.o_word_size := asyncQueue.get.module.io.out_ctrl(word_detector.ioBlock.o_word_size.getWidth - 1, 0)
      out.bits.data  := asyncQueue.get.out.bits.data
      out.valid := asyncQueue.get.out.valid
      asyncQueue.get.out.ready := out.ready
    }
    else {
      io.o_crc := byte2word.ioBlock.o_crc
      io.o_word_size := word_detector.ioBlock.o_word_size
      out.bits.data  := Cat(byte2word.ioBlock.o_data)
      out.valid      := byte2word.ioBlock.o_en
    }
  }
}

object DataRXwithAsyncApp extends App
{
    implicit val p: Parameters = Parameters.empty

    val params = DataRXParams(
      channels = 4,
      asyncParams = Some(AXI4StreamAsyncQueueWithControlParams(
        ctrlBits   = 3,
        sync       = 4,
        depth      = 2048,
        safe       = true
      ))
    )
    
    val lazyDut = LazyModule(new AXI4StreamDataRX(params) with DataRXPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/DataRX"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object DataRXwithoutAsyncApp extends App
{
    implicit val p: Parameters = Parameters.empty
    
    val params = DataRXParams(
      channels = 4,
      asyncParams = None
    )
    
    val lazyDut = LazyModule(new AXI4StreamDataRX(params) with DataRXPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/DataRX"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}