// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._ 
import chisel3.util.Cat
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{IO, ChiselEnum}

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config._

// Byte2Word parameters
case class Byte2WordParams(
  channels : Int = 4,
) 
{
  require(channels >= 1, "Number of channels must be greater or equal to one!")
}

class Byte2WordIO(channels: Int) extends Bundle {
  val i_en        =  Input(Bool())
  val i_crc       =  Input(Bool())
  val i_word_size =  Input(UInt(2.W))
  val i_data      =  Input(Vec(channels, UInt(8.W)))
  val o_data      = Output(Vec(channels, UInt(16.W)))
  val o_crc       = Output(Bool())
  val o_en        = Output(Bool())
  
  override def cloneType: this.type = Byte2WordIO(channels).asInstanceOf[this.type]
}
object Byte2WordIO {
  def apply(channels: Int): Byte2WordIO = new Byte2WordIO(channels)
}

class Byte2Word(val params: Byte2WordParams) extends LazyModule()(Parameters.empty) {
  // IO
  lazy val io = Wire(new Byte2WordIO(params.channels))

  lazy val module = new LazyModuleImp(this) {

    // Data registers
    val counter = RegInit(0.U(3.W))
    val r_data  = Seq.fill(params.channels) {RegInit(0.U(16.W))}
    val r_en    = RegInit(false.B)
    val r_crc   = RegInit(false.B)
    val r_temp_data  = Seq.fill(params.channels) {RegInit(0.U(6.W))}

    // Assign IOs
    r_data.zipWithIndex.map({ case (m, i) =>
      io.o_data(i) := m
    })
    io.o_en     := r_en
    io.o_crc    := r_crc

    // FSM
    object State extends ChiselEnum {
      val sInit, s16, s12, s14, sCRC2, sCRC4, sCRC6 = Value
    }
    val state = RegInit(State.sInit)
    state.suggestName("stateOfFSM")
    
    // -----------------------------------------------------------------------------------------------
    // FSM
    // -----------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------
    // STATE: sInit
    // -----------------------------------------------------------------------------------------------
    when(state === State.sInit){
      r_en    := false.B
      r_crc   := false.B
      counter := 0.U
      when(io.i_en) {
        counter  := counter + 1.U
        // 16bit word
        when (io.i_word_size === 0.U) {
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(8.W), io.i_data(i))
          })
          // next state
          state := State.s16
        }
        // 12bit word
        .elsewhen(io.i_word_size === 1.U) {
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i), 0.U(4.W))
          })
          // next state
          state := State.s12
        }
        // 14bit word
        .elsewhen(io.i_word_size === 2.U) {
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(6.W), io.i_data(i), 0.U(2.W))
          })
          // next state
          state := State.s14
        }
        // shouldn't happen
        .otherwise {
          state := State.sInit
        }
      }
      // don't change state
      .otherwise {
        state := State.sInit
        r_en    := false.B
        r_crc   := false.B
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: s16
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.s16) {
      when (io.i_en) {
        when(counter === 0.U) {
          r_en     := 0.U
          counter  := counter + 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(8.W), io.i_data(i))
          })
        }
        .elsewhen(counter === 1.U) {
          // crc flag
          when(io.i_crc === 1.U) {
            r_crc := 1.U
          }
          .otherwise {
            r_crc := 0.U
          }
          r_en     := 1.U
          counter  := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i), r_data(i)(7,0))
          })
        }
        // shouldn't happen
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: s12
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.s12) {
      when(io.i_en) {
        when(counter === 0.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          // crc
          when(io.i_crc) {
            r_data.zipWithIndex.map({ case (m, i) =>
              m := Cat(0.U(8.W), io.i_data(i))
            })
            // next state
            state := State.s16
          }
          // data
          .otherwise {
            r_data.zipWithIndex.map({ case (m, i) =>
              m := Cat(0.U(4.W), io.i_data(i), 0.U(4.W))
            })
          }
        }
        .elsewhen(counter === 1.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(3,0), r_data(i)(11,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i)(7,4))
          })
          // crc
          when(io.i_crc) {
            counter := 0.U
            state   := State.sCRC4
          }
        }
        .elsewhen(counter === 2.U) {
          counter  := 0.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i), r_temp_data(i)(3,0), 0.U(4.W))
          })
        }
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: s14
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.s14) {
      when(io.i_en) {
        when(counter === 0.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          // crc
          when(io.i_crc) {
            r_data.zipWithIndex.map({ case (m, i) =>
              m := Cat(0.U(8.W), io.i_data(i))
            })
            // next state
            state := State.s16
          }
          // data
          .otherwise {
            r_data.zipWithIndex.map({ case (m, i) =>
              m := Cat(0.U(6.W), io.i_data(i), 0.U(2.W))
            })
            // next state
            state := State.s14
          }
        }
        .elsewhen(counter === 1.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(5,0), r_data(i)(9,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i)(7,6))
          })
          // crc
          when(io.i_crc) {
            counter := 0.U
            state   := State.sCRC2
          }
        }
        .elsewhen(counter === 2.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i), r_temp_data(i)(1,0), 0.U(2.W))
          })
        }
        .elsewhen(counter === 3.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(3,0), r_data(i)(11,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i)(7,4))
          })
          // crc
          when(io.i_crc) {
            counter := 0.U
            state   := State.sCRC4
          }
        }
        .elsewhen(counter === 4.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i), r_temp_data(i)(3,0), 0.U(2.W))
          })
        }
        .elsewhen(counter === 5.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(1,0), r_data(i)(13,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := io.i_data(i)(7,2)
          })
          // crc
          when(io.i_crc) {
            counter := 0.U
            state   := State.sCRC6
          }
        }
        .elsewhen(counter === 6.U) {
          counter  := 0.U
          r_en     := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i), r_temp_data(i), 0.U(2.W))
          })
        }
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: sCRC2
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.sCRC2) {
      when(io.i_en) {
        when(counter === 0.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(6.W), io.i_data(i), r_temp_data(i)(1,0))
          })
        }
        .elsewhen(counter === 1.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(5,0), r_data(i)(9,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i)(7,6))
          })
        }
        .elsewhen(counter === 2.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(6.W), io.i_data(i), r_temp_data(i)(1,0))
          })
        }
        .elsewhen(counter === 3.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(5,0), r_data(i)(9,0))
          })
          // next state
          state := State.sInit
        }
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: sCRC4
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.sCRC4) {
      when(io.i_en) {
        when(counter === 0.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i), r_temp_data(i)(3,0))
          })
        }
        .elsewhen(counter === 1.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(3,0), r_data(i)(11,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i)(7,4))
          })
        }
        .elsewhen(counter === 2.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(4.W), io.i_data(i), r_temp_data(i)(3,0))
          })
        }
        .elsewhen(counter === 3.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(3,0), r_data(i)(11,0))
          })
          // next state
          state := State.sInit
        }
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: sCRC6
    // -----------------------------------------------------------------------------------------------
    .elsewhen(state === State.sCRC6) {
      when(io.i_en) {
        when(counter === 0.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i), r_temp_data(i))
          })
        }
        .elsewhen(counter === 1.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(1,0), r_data(i)(13,0))
          })
          r_temp_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i)(7,6))
          })
        }
        .elsewhen(counter === 2.U) {
          counter  := counter + 1.U
          r_en     := 0.U
          r_crc    := 0.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(0.U(2.W), io.i_data(i), r_temp_data(i))
          })
        }
        .elsewhen(counter === 3.U) {
          counter  := counter + 1.U
          r_en     := 1.U
          r_crc    := 1.U
          r_data.zipWithIndex.map({ case (m, i) =>
            m := Cat(io.i_data(i)(1,0), r_data(i)(13,0))
          })
          // next state
          state := State.sInit
        }
        .otherwise {
          counter := 0.U
          r_en    := 0.U
          r_crc   := 0.U
          // next state
          state := State.sInit
        }
      }
      .otherwise {
        counter := 0.U
        r_en    := 0.U
        r_crc   := 0.U
        // next state
        state := State.sInit
      }
    }
    // -----------------------------------------------------------------------------------------------
    // STATE: shouldn't happen
    // -----------------------------------------------------------------------------------------------
    .otherwise {
      counter  := 0.U
      r_en     := 0.U
      r_crc    := 0.U
      r_data.zipWithIndex.map({ case (m, i) =>
        m := 0.U
      })
      r_temp_data.zipWithIndex.map({ case (m, i) =>
        m := 0.U
      })
      // next state
      state := State.sInit
    }
  }
}

trait Byte2WordPins extends Byte2Word{
  def makeCustomIO(): Byte2WordIO = {
    val io2: Byte2WordIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object Byte2WordApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(new Byte2Word(Byte2WordParams()) with Byte2WordPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Byte2Word"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}