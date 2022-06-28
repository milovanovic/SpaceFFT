// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.util.{Cat, ShiftRegister}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.IO

import freechips.rocketchip.diplomacy._
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
  val i_async_reset = if (async) Some(Input(Reset())) else None

  // ASYNC FIRE signal
  val fire = if (async) Some(Output(Bool())) else None

  override def cloneType: this.type = DataRXIO(channels, async).asInstanceOf[this.type]
}
object DataRXIO {
  def apply(channels: Int, async: Boolean): DataRXIO = new DataRXIO(channels, async)
}

class DataRX(val params: DataRXParams) extends LazyModule()(Parameters.empty) {

  lazy val io = Wire(new DataRXIO(params.channels, params.asyncParams != None))

  val reorder_data  = Seq.fill(params.channels) {LazyModule(new BitReordering with BitReorderingPins)}
  val reorder_valid = LazyModule(new BitReordering with BitReorderingPins)
  val reorder_frame = LazyModule(new BitReordering with BitReorderingPins)
  val bitslip       = LazyModule(new BitSlipDetection with BitSlipDetectionPins)
  val word_detector = LazyModule(new DetectWordWidth(DetectWordWidthParams(params.channels)) with DetectWordWidthPins)
  val byte2word     = LazyModule(new Byte2Word(Byte2WordParams(params.channels)) with Byte2WordPins)
  val asyncQueue    = if (params.asyncParams != None ) Some(LazyModule(new AXI4StreamAsyncQueueWithControl(params.asyncParams.get){
    // pins
    def makeCustomIO(): AXI4StreamAsyncQueueWithControlIO = {
      val io2: AXI4StreamAsyncQueueWithControlIO = IO(io.cloneType)
      io2.suggestName("io")
      io2 <> io
      io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
  })) else None

  // StreamNode
  lazy val streamNode = if(asyncQueue != None) asyncQueue.get.streamNode else Seq.fill(params.channels){AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "outStream", n = 2, u = 0, numMasters = 1))}

  lazy val module = new LazyModuleImp(this) {
    val out = if(asyncQueue == None) Some(streamNode.map(m => m.out(0)._1)) else None

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
      // Output
      if(asyncQueue == None) {
        out.get(i).bits.data := byte2word.ioBlock.o_data(i)
        out.get(i).valid     := byte2word.ioBlock.o_en
      }
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
      asyncQueue.get.ioBlock.write_clock := clock
      asyncQueue.get.ioBlock.write_reset := reset
      asyncQueue.get.ioBlock.in_data  := Cat(byte2word.ioBlock.o_data)
      asyncQueue.get.ioBlock.in_valid := byte2word.ioBlock.o_en
      asyncQueue.get.ioBlock.in_ctrl  := Cat(byte2word.ioBlock.o_crc, word_detector.ioBlock.o_word_size)
      // AsyncQueue (Master-Side)
      asyncQueue.get.module.clock := io.i_async_clock.get
      asyncQueue.get.module.reset := io.i_async_reset.get
      io.o_crc := asyncQueue.get.ioBlock.out_ctrl(byte2word.ioBlock.o_crc.getWidth + word_detector.ioBlock.o_word_size.getWidth - 1)
      io.o_word_size := asyncQueue.get.ioBlock.out_ctrl(word_detector.ioBlock.o_word_size.getWidth - 1, 0)

      io.fire.get := asyncQueue.get.ioBlock.fire
    }
    else {
      io.o_crc := byte2word.ioBlock.o_crc
      io.o_word_size := word_detector.ioBlock.o_word_size
    }
  }
}

trait DataRXPins extends DataRX{
  // output stream node
  val streamLen = streamNode.length

  val ioOutNode = Seq.fill(streamLen){BundleBridgeSink[AXI4StreamBundle]()}

  val outPins = for (i <- 0 until streamLen) yield {
    ioOutNode(i) := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode(i)
    val out = InModuleBody { 
      implicit val valName = ValName(s"out_$i")
      ioOutNode(i).makeIO()
    }
    out
  }

  def makeCustomIO(): DataRXIO = {
    val io2: DataRXIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object DataRXwithAsyncApp extends App
{
    val params = DataRXParams(
      channels = 4,
      asyncParams = Some(AXI4StreamAsyncQueueWithControlParams(
        channels   = 4,
        dataBytes  = 2,
        ctrlBits   = 3,
        isFullFlag = false,
        sync       = 4,
        depth      = 2048,
        safe       = true
      ))
    )
    
    val lazyDut = LazyModule(new DataRX(params) with DataRXPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/DataRX"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object DataRXwithoutAsyncApp extends App
{
    val params = DataRXParams(
      channels = 4,
      asyncParams = None
    )
    
    val lazyDut = LazyModule(new DataRX(params) with DataRXPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/DataRX"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}