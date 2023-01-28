// SPDX-License-Identifier: Apache-2.0

package spacefft.measurement

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, ChiselEnum}

import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

/* DelayMeasurement Bundle */
class DelayMeasurementIO(channels: Int) extends Bundle {
    val data_first = Vec(channels, Input(Bool()))
    val data_last  = Vec(channels, Input(Bool()))
    val delayTime  = Vec(channels, Output(Bool()))
    val trigger    = Output(Bool())

    override def cloneType: this.type = DelayMeasurementIO(channels).asInstanceOf[this.type]
}
object DelayMeasurementIO {
  def apply(channels: Int): DelayMeasurementIO = new DelayMeasurementIO(channels)
}

/* DelayMeasurement parameters */
case class DelayMeasurementParameters (
  channels : Int,
)

abstract class DelayMeasurement (params: DelayMeasurementParameters, beatBytes: Int) extends LazyModule()(Parameters.empty) with HasCSR {

  /* IO */
  lazy val io = Wire(new DelayMeasurementIO(params.channels))

  lazy val module = new LazyModuleImp(this) {

    /* control & status registers */
    val controlReg = RegInit(0.U(2.W))
    val counterReg = RegInit(0xFFFFFF.U(32.W))
    val triggerReg = RegInit(0x10000.U(32.W))
    val cntOverflowReg = RegInit(false.B)
    val delayFirstReg  = Seq.fill(params.channels){RegInit(0.U(32.W))}
    val delayLastReg   = Seq.fill(params.channels){RegInit(0.U(32.W))}
    
    /* counter registers */
    val counter = RegInit(0.U(32.W))
    val counterTrig = RegInit(0.U(32.W))

    /* change state registers*/
    val lastFlagReg     = Seq.fill(params.channels){RegInit(false.B)}
    val data_first_flag = Seq.fill(params.channels){RegInit(true.B)}
    val data_last_flag  = Seq.fill(params.channels){RegInit(true.B)}
    val changeStateReg  = RegInit(false.B)

    /* delayTime registers */
    val delayTimeReg = Seq.fill(params.channels){RegInit(false.B)}
    (io.delayTime, delayTimeReg).zipped.map{ (i, o) => {
      i := o
    }}
    /* FSM states */
    object State extends ChiselEnum {
      val sInit, sTrigger = Value
    }
    val state = RegInit(State.sInit)
    state.suggestName("stateOfFSM")

    /* State: Initial state */
    when(state === State.sInit){
      /* reset counter */
      counter := 0.U
      /* reset overflow flag */
      cntOverflowReg := 0.U
      /* reset change state reg */
      changeStateReg := 0.U
      /* reset last flag */
      lastFlagReg.map(m => m := 0.U)
      /* reset delayTime flag */
      delayTimeReg.map(m => m := 0.U)
      /* set flags */
      data_first_flag.map(m => m := 1.U)
      data_last_flag.map(m => m := 1.U)
      /* don't send trigger pulse */
      io.trigger := 0.U
      /* change state */
      when(controlReg === 1.U) {
        state := State.sTrigger
        counterTrig := 0.U
      }
      .elsewhen(controlReg === 2.U) {
        counterTrig := counterTrig + 1.U
        when(counterTrig === counterReg) {
          state := State.sTrigger
          counterTrig := 0.U
        }
      }
      .otherwise {
        state := State.sInit
        counterTrig := 0.U
      }
    }
    /* State: Trigger */
    .otherwise{
      /* Set counter overflow flag && increment counter */
      when (counter === 0xFFFFFFFFL.U) {cntOverflowReg := 1.U}
      .otherwise {counter := counter + 1.U}
      /* Send trigger pulse */
      io.trigger := counter < triggerReg
      /* Write to delay registers */
      io.data_first.zipWithIndex.map({case (m,i) => 
        when(m && data_first_flag(i)) {
          delayFirstReg(i) := counter
          data_first_flag(i) := 0.U
          /* Send delay flags */
          delayTimeReg(i) := 1.U
        }
      })
      io.data_last.zipWithIndex.map({case (m,i) => 
        when(m && data_last_flag(i)) {
          delayLastReg(i) := counter
          data_last_flag(i) := 0.U
          /* Write to delay flag to registers */
          lastFlagReg(i) := 1.U
          /* Send delay flags */
          delayTimeReg(i) := 0.U
        }
      })
      /* change state of FSM */
      changeStateReg := lastFlagReg.reduce((a,b) => a && b)
      /* Difference between single and auto trigger */
      when(changeStateReg || cntOverflowReg) {
        when (controlReg === 1.U) {
          controlReg := 0.U
        }
        /* set flags */
        data_first_flag.map(m => m := 1.U)
        data_last_flag.map(m => m := 1.U)
        /* change state */
        state := State.sInit
      }
    }

    // // ILA BlackBox for Vivado
    // class ILA_DELAY extends BlackBox {
    //   val io = IO(new Bundle {
    //     val clk     = Input(Clock())
    //     val probe0  = Input(UInt(2.W))
    //     val probe1  = Input(UInt(32.W))
    //     val probe2  = Input(UInt(1.W))
    //     val probe3  = Input(UInt(1.W))
    //     val probe4  = Input(UInt(32.W))
    //     val probe5  = Input(UInt(1.W))
    //     val probe6  = Input(UInt(32.W))
    //     val probe7  = Input(UInt(1.W))
    //     val probe8  = Input(UInt(32.W))
    //     val probe9  = Input(UInt(1.W))
    //     val probe10 = Input(UInt(32.W))
    //     val probe11 = Input(UInt(1.W))
    //     val probe12 = Input(UInt(2.W))
    //     val probe13 = Input(UInt(2.W))
    //     val probe14 = Input(UInt(1.W))
    //   })
    // }
    // val ila = Module(new ILA_DELAY)
    // ila.io.clk     := clock
    // ila.io.probe0  := controlReg
    // ila.io.probe1  := counter
    // ila.io.probe2  := io.trigger
    // ila.io.probe3  := cntOverflowReg
    // ila.io.probe4  := delayFirstReg(0)
    // ila.io.probe5  := io.data_first(0)
    // ila.io.probe6  := delayLastReg(0)
    // ila.io.probe7  := io.data_last(0)
    // ila.io.probe8  := delayFirstReg(1)
    // ila.io.probe9  := io.data_first(1)
    // ila.io.probe10 := delayLastReg(1)
    // ila.io.probe11 := io.data_last(1)
    // ila.io.probe12 := state.asUInt
    // ila.io.probe13 := chisel3.util.Cat(lastFlagReg(1), lastFlagReg(0))
    // ila.io.probe14 := changeStateReg

    /* define fields */
    val fields = Seq(
      RegField(controlReg.getWidth, controlReg, RegFieldDesc(name = "controlReg", desc = "Choose mod of operation, 0 - idle, 1 - single trigger, 2 - auto trigger")),
      RegField(counterReg.getWidth, counterReg, RegFieldDesc(name = "counterReg", desc = "If auto trigger is selected, this reg represents number of cycles between triggers")),
      RegField(triggerReg.getWidth, triggerReg, RegFieldDesc(name = "triggerReg", desc = "Trigger pulse duration")),
      RegField.r(cntOverflowReg.getWidth, cntOverflowReg, RegFieldDesc(name = "cntOverflowReg", desc = "Counter overflow flag")),
    ) ++
    delayFirstReg.zipWithIndex.map({ case (m,i) => RegField.r(m.getWidth, m , RegFieldDesc(name = s"delayFirstReg_$i", desc = s"Contains delay value of first trigger on channel $i"))}) ++
    delayLastReg.zipWithIndex.map({ case (m,i) => RegField.r(m.getWidth, m , RegFieldDesc(name = s"delayLastReg_$i", desc = s"Contains delay value of last trigger on channel $i"))})

    /* Define abstract register map so it can be AXI4, Tilelink, APB, AHB */
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
  }
}

class AXI4DelayMeasurement(params: DelayMeasurementParameters, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends DelayMeasurement(params, beatBytes) {
  /* override val mem: Some[AXI4RegisterNode] */
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}

trait AXI4DelayMeasurementPins extends AXI4DelayMeasurement {

  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  /* IOs */
  def makeCustomIO(): DelayMeasurementIO = {
    val io2: DelayMeasurementIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object DelayMeasurementApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = DelayMeasurementParameters(channels = 4)
  val lazyDut = LazyModule(new AXI4DelayMeasurement(params, AddressSet(0x0000, 0xFF), 4) with AXI4DelayMeasurementPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/DelayMeasurement"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

