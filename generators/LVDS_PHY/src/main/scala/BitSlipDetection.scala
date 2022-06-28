// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, ChiselEnum}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters

class BitSlipDetectionIO extends Bundle {
    val i_data    =  Input(UInt(8.W))
    val o_bitslip = Output(UInt(3.W))
    val o_en      = Output(Bool())
}
class BitSlipDetection extends LazyModule()(Parameters.empty) {

    lazy val io = Wire(new BitSlipDetectionIO)

    lazy val module = new LazyModuleImp(this) {
        // Registers
        val r_bitslip = RegInit(0.U(3.W))
        val r_en      = RegInit(false.B)
        io.o_bitslip := r_bitslip
        io.o_en      := r_en

        // FSM
        object State extends ChiselEnum {
            val sInit, sData = Value
        }
        val state = RegInit(State.sInit)
        state.suggestName("stateOfFSM")

        when(state === State.sInit) {
            // bitslip 0
            when(io.i_data === "b1111_1111".U){
                r_bitslip := 0.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 1
            .elsewhen (io.i_data === "b1111_1110".U){ //|| io.i_data === "b1000_0000".U) {
                r_bitslip := 1.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 2
            .elsewhen (io.i_data === "b1111_1100".U){ // || io.i_data === "b1100_0000".U) {
                r_bitslip := 2.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 3
            .elsewhen (io.i_data === "b1111_1000".U){ // || io.i_data === "b1110_0000".U) {
                r_bitslip := 3.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 4
            .elsewhen (io.i_data === "b1111_0000".U){ // || io.i_data === "b1111_0000".U) {
                r_bitslip := 4.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 5
            .elsewhen (io.i_data === "b1110_0000".U){ // || io.i_data === "b1111_1000".U) {
                r_bitslip := 5.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 6
            .elsewhen (io.i_data === "b1100_0000".U){ // || io.i_data === "b1111_1100".U) {
                r_bitslip := 6.U
                state     := State.sData
                r_en      := true.B
            }
            // bitslip 7
            .elsewhen (io.i_data === "b1000_0000".U){ // || io.i_data === "b1111_1110".U) {
                r_bitslip := 7.U
                state     := State.sData
                r_en      := true.B
            }
            // Should not happen
            .otherwise {
                state := State.sInit
                r_en  := false.B
            }
        }
        .otherwise{
            when(io.i_data =/= "b1111_1111".U) {
                state := State.sInit
                r_en  := false.B
            }
            .otherwise {
                state := State.sData
                r_en  := true.B
            }
        }
    }
}

trait BitSlipDetectionPins extends BitSlipDetection{

  def makeCustomIO(): BitSlipDetectionIO = {
    val io2: BitSlipDetectionIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object BitSlipDetectionApp extends App
{
    val lazyDut = LazyModule(new BitSlipDetection with BitSlipDetectionPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/BitSlipDetection"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}