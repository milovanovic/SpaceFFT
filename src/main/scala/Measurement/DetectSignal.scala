// SPDX-License-Identifier: Apache-2.0

package spacefft.measurement

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, ChiselEnum}

import dspblocks._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

/* DetectSignal Bundle */
class DetectSignalIO(channels: Int) extends Bundle {
    val out_data_first = Vec(channels, Output(Bool()))
    val out_data_last  = Vec(channels, Output(Bool()))

    override def cloneType: this.type = DetectSignalIO(channels).asInstanceOf[this.type]
}
object DetectSignalIO {
  def apply(channels: Int): DetectSignalIO = new DetectSignalIO(channels)
}

/* DetectSignal parameters */
case class DetectSignalParameters (
  numberOfData: Seq[Int],
  channels : Int,
)

class AXI4DetectSignal(params: DetectSignalParameters, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends DetectSignal(params, beatBytes) {
  /* override val mem: Some[AXI4RegisterNode] */
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}

trait AXI4DetectSignalPins extends AXI4DetectSignal {
  def beatBytes: Int = 4

  /* Mem port */
  def standaloneParams = AXI4BundleParameters(addrBits = 8*beatBytes, dataBits = 8*beatBytes, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  /* StreamNodes */
  val inIO: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until inNodes.size) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
      inNodes(i) := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))) := in
    InModuleBody { in.makeIO() }
  }

  /* IOs */
  def makeCustomIO(): DetectSignalIO = {
    val io2: DetectSignalIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

abstract class DetectSignal (params: DetectSignalParameters, beatBytes: Int) extends LazyModule()(Parameters.empty) with HasCSR {
  /* IO */
  val inNodes = Seq.fill(params.channels){AXI4StreamSlaveNode(AXI4StreamSlaveParameters())}

  /* IO */
  lazy val io = Wire(new DetectSignalIO(params.channels))

  lazy val module = new LazyModuleImp(this) {
    val ins = inNodes.map(m => m.in(0)._1)

    /* control memory mapped registers */
    val counterLastReg = if (params.channels == params.numberOfData.length) {
      params.numberOfData.map(m => RegInit((m).U(32.W)))
    }
    else {
      Seq.fill(params.channels){RegInit((params.numberOfData(0)).U(32.W))}
    }
    
    /* counter registers */
    val counter = Seq.fill(params.channels){RegInit(0.U(32.W))}
    /* registers */
    val dataFirstReg = Seq.fill(params.channels){RegInit(false.B)}
    val dataLastReg  = Seq.fill(params.channels){RegInit(false.B)}
    /* outputs */
    (io.out_data_first, dataFirstReg).zipped.map{ (o, r) => {
      o := r
    }}
    (io.out_data_last, dataLastReg).zipped.map{ (o, r) => {
      o := r
    }}

    /* FSM states */
    object State extends ChiselEnum {
      val sInit, sStart = Value
    }
    val state = Seq.fill(params.channels){RegInit(State.sInit)}
    state.zipWithIndex.map({case (m,i) => m.suggestName(s"state_$i")})

    state.zipWithIndex.map({case (m,i) =>
      /* State: Initial state */
      when(m === State.sInit){
        /* Input not ready */
        ins(i).ready := 0.U
        /* reset counter */
        counter(i) := 0.U
        /* reset dataFirstReg */
        dataFirstReg(i) := 0.U
        /* reset dataLastReg */
        dataLastReg(i) := 0.U
        /* change state */
        m := State.sStart
      }
      /* State: Start */
      .otherwise {
        /* Input ready */
        ins(i).ready := 1.U

        when(ins(i).fire()) {counter(i) := counter(i) + 1.U}
        /* First data happened */
        when(ins(i).fire() === 1.U && counter(i) === 0.U) { dataFirstReg(i) := 1.U }
        .otherwise { dataFirstReg(i) := 0.U }
        /* Last data happened */
        when(ins(i).fire() === 1.U && counter(i) === (counterLastReg(i)-1.U)) { 
          dataLastReg(i) := 1.U
          counter(i) := 0.U
        }
        .otherwise { dataLastReg(i) := 0.U }
      }
    })

    // // ILA BlackBox for Vivado
    // class ILA_DETECT extends BlackBox {
    //   val io = IO(new Bundle {
    //     val clk     = Input(Clock())
    //     val probe0  = Input(UInt(1.W))
    //     val probe1  = Input(UInt(1.W))
    //     val probe2  = Input(UInt(1.W))
    //     val probe3  = Input(UInt(32.W))
    //     val probe4  = Input(UInt(1.W))
    //     val probe5  = Input(UInt(1.W))
    //     val probe6  = Input(UInt(1.W))
    //     val probe7  = Input(UInt(1.W))
    //     val probe8  = Input(UInt(32.W))
    //     val probe9  = Input(UInt(1.W))
    //   })
    // }
    // val ila = Module(new ILA_DETECT)
    // ila.io.clk     := clock
    // ila.io.probe0  := ins(0).fire()
    // ila.io.probe1  := dataFirstReg(0)
    // ila.io.probe2  := dataLastReg(0)
    // ila.io.probe3  := counter(0)
    // ila.io.probe4  := state(0).asUInt
    // ila.io.probe5  := ins(1).fire()
    // ila.io.probe6  := dataFirstReg(1)
    // ila.io.probe7  := dataLastReg(1)
    // ila.io.probe8  := counter(1)
    // ila.io.probe9  := state(1).asUInt

    /* define fields */
    val fields = counterLastReg.zipWithIndex.map({ case (m,i) => RegField(m.getWidth, m , RegFieldDesc(name = s"counterLastReg_$i", desc = s"Contains expected data number in data frame for signal $i"))})

    /* Define abstract register map so it can be AXI4, Tilelink, APB, AHB */
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
  }
}

object DetectSignalApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = DetectSignalParameters(numberOfData = Seq(2048, 1024), channels = 2)
  val lazyDut = LazyModule(new AXI4DetectSignal(params, AddressSet(0x0000, 0xFF), 4) with AXI4DetectSignalPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/DetectSignal"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

