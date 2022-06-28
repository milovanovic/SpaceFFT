// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.IO
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters


class BitReorderingIO extends Bundle {
    val i_bitslip =  Input(UInt(3.W))
    val i_data    =  Input(UInt(8.W))
    val o_data    = Output(UInt(8.W))
}
class BitReordering extends LazyModule()(Parameters.empty) {

    lazy val io = Wire(new BitReorderingIO)

    lazy val module = new LazyModuleImp(this) {
        // Registers
        val r_a = RegInit(0.U(8.W))
        val r_b = RegInit(0.U(8.W))
        val r_c = RegInit(0.U(8.W))
        r_a       := io.i_data
        r_b       := r_a 
        io.o_data := r_c
        val r_bitslip = RegNext(RegNext(io.i_bitslip, 0.U), 0.U)

        // bitslip 0
        when(r_bitslip === 0.U){
            r_c := r_b
        }
        // bitslip 1
        .elsewhen (r_bitslip === 1.U) {
            r_c := Cat(r_a(0,0), r_b(7,1))
        }
        // bitslip 2
        .elsewhen (r_bitslip === 2.U) {
            r_c := Cat(r_a(1,0), r_b(7,2))
        }
        // bitslip 3
        .elsewhen (r_bitslip === 3.U) {
            r_c := Cat(r_a(2,0), r_b(7,3))
        }
        // bitslip 4
        .elsewhen (r_bitslip === 4.U) {
            r_c := Cat(r_a(3,0), r_b(7,4))
        }
        // bitslip 5
        .elsewhen (r_bitslip === 5.U) {
            r_c := Cat(r_a(4,0), r_b(7,5))
        }
        // bitslip 6
        .elsewhen (r_bitslip === 6.U) {
            r_c := Cat(r_a(5,0), r_b(7,6))
        }
        // bitslip 7
        .elsewhen (r_bitslip === 7.U) {
            r_c := Cat(r_a(6,0), r_b(7,7))
        }
        // should never happen
        .otherwise {
            r_c := r_c
        }
    }
}

trait BitReorderingPins extends BitReordering{

  def makeCustomIO(): BitReorderingIO = {
    val io2: BitReorderingIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}


object BitReorderingApp extends App
{
    val lazyDut = LazyModule(new BitReordering with BitReorderingPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/BitReordering"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
