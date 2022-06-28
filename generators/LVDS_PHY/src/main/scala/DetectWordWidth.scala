// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{IO, ChiselEnum}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters

// DetectWordWidth parameters
case class DetectWordWidthParams(
  channels : Int = 4,
) 
{
  require(channels >= 1, "Number of channels must be greater or equal to one!")
}

class DetectWordWidthIO(channels: Int) extends Bundle {
    val i_en     = Input(Bool())
    val i_data   = Input(Vec(channels, UInt(8.W)))
    val i_frame  = Input(UInt(8.W))
    val o_en     = Output(Bool())
    val o_crc    = Output(Bool())
    val o_data   = Output(Vec(channels, UInt(8.W)))
    val o_word_size = Output(UInt(2.W))
    
    override def cloneType: this.type = DetectWordWidthIO(channels).asInstanceOf[this.type]
}
object DetectWordWidthIO {
  def apply(channels: Int): DetectWordWidthIO = new DetectWordWidthIO(channels)
}

class DetectWordWidth(val params: DetectWordWidthParams) extends LazyModule()(Parameters.empty) {

    lazy val io = Wire(new DetectWordWidthIO(params.channels))

    lazy val module = new LazyModuleImp(this) {
        // Registers
        val r_data      = RegInit(0.U(32.W))
        val r_word_size = RegInit(0.U(2.W))
        val r_counter   = RegInit(0.U(3.W))
        val r_en        = RegInit(false.B)
        val r_crc       = RegInit(false.B)
        io.o_word_size := r_word_size
        io.o_crc    := r_crc
        io.o_en     := ShiftRegister(io.i_en,     3, 0.U, true.B)
        io.i_data.zipWithIndex.map({ case (m, i) =>
            io.o_data(i) := ShiftRegister(m, 3, 0.U, true.B)
        })

        // FSM
        object State extends ChiselEnum {
            val sInit, sLoadWord, sWord, sLoadCRC, sCRC, sWait = Value
        }
        val state = RegInit(State.sInit)
        state.suggestName("stateOfFSM")

        when(state === State.sInit){
            r_en  := false.B
            r_crc := false.B
            when(io.i_en === true.B) {
                state  := State.sLoadWord
                r_data := Cat(0.U(24.W), io.i_frame)
            }
        }
        .elsewhen(state === State.sLoadWord){
            r_en  := false.B
            r_crc := false.B
            when(io.i_en === true.B) {
                state  := State.sWord
                r_data := Cat(r_data(31, 16), io.i_frame, r_data(7, 0))
            }
        }
        .elsewhen(state === State.sWord){
            r_crc := false.B
            when(io.i_en === true.B) {
                r_en   := true.B
                state  := State.sLoadCRC
                r_data := Cat(r_data(31, 24), io.i_frame, r_data(15, 0))

                // 16 bit word
                when (r_data(15,0) === 0x00FF.U) {
                    r_word_size := 0.U
                }
                //12 bit word
                .elsewhen (r_data(15,0) === 0xF03F.U) {
                    r_word_size := 1.U
                }
                // 14 bit word
                .elsewhen (r_data(15,0) === 0xC07F.U) {
                    r_word_size := 2.U
                }
                // should not happen
                .otherwise {
                    r_word_size := r_word_size
                }
            }
            .otherwise {
                state := State.sInit
                r_en  := false.B
            }
        }
        .elsewhen(state === State.sLoadCRC){
            r_crc := false.B
            when(io.i_en === true.B) {
                r_en   := true.B
                state  := State.sCRC
                r_data := Cat(io.i_frame, r_data(31, 8))
            }
            .otherwise {
                state := State.sInit
                r_en  := false.B
            }
        }
        .elsewhen(state === State.sCRC){
            when(io.i_en === true.B) {
                r_en   := true.B
                r_data := Cat(io.i_frame, r_data(31, 8))
                // CRC
                when (r_word_size === 0.U) {
                    when (r_data === 0xFFFF00FFL.U) {
                        r_counter := 3.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .otherwise {
                        r_crc := false.B
                    }
                }
                .elsewhen(r_word_size === 1.U) {
                    when (r_data === 0xFFFF03F0L.U) {
                        r_counter := 3.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .elsewhen (r_data === 0xFFF03F03L.U) {
                        r_counter := 4.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .otherwise {
                        r_crc := false.B
                    }
                }
                .elsewhen(r_word_size === 2.U) {
                    when (r_data === 0xFFFF01FCL.U) {
                        r_counter := 3.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .elsewhen (r_data === 0xFFC07F01L.U) {
                        r_counter := 4.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .elsewhen (r_data === 0xFFF01FC0L.U) {
                        r_counter := 4.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .elsewhen (r_data === 0xFFFC07F0L.U) {
                        r_counter := 4.U
                        r_crc := true.B
                        state := State.sWait
                    }
                    .otherwise {
                        r_crc := false.B
                    }
                }
                // should not happen
                .otherwise {
                    state := State.sInit
                    r_en  := false.B
                    r_crc := false.B
                }
            }
            .otherwise {
                state := State.sInit
                r_en  := false.B
                r_crc := false.B
            }
        }
        .elsewhen(state === State.sWait){
            r_counter := r_counter - 1.U
            when(r_counter === 0.U) {
                state  := State.sInit
                r_en   := false.B
                r_crc  := false.B
            }
        }
        // should never happen
        .otherwise {
            state := State.sInit
            r_en  := false.B
            r_crc := false.B
        }
    }
}

trait DetectWordWidthPins extends DetectWordWidth{

  def makeCustomIO(): DetectWordWidthIO = {
    val io2: DetectWordWidthIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}


object DetectWordWidthApp extends App
{
    val lazyDut = LazyModule(new DetectWordWidth(DetectWordWidthParams()) with DetectWordWidthPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/DetectWordWidth"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
