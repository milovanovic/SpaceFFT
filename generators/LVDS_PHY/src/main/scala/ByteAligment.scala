// SPDX-License-Identifier: Apache-2.0

package lvdsphy

import chisel3._
import chisel3.util.ShiftRegister
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.{DataMirror, IO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.Parameters

// ByteAligment parameters
case class ByteAligmentParams(
    channels : Int = 6,
)

class ByteAligmentIO[T <: Data](private val gen1: T, private val gen2: T, private val channels: Int) extends Bundle {
  val valid = Input(UInt(8.W))
  val in  = Input(Vec(channels, DataMirror.internal.chiselTypeClone[T](gen1)))
  val out = Output(Vec(channels, DataMirror.internal.chiselTypeClone[T](gen2)))
  val en  = Output(Bool())
}

class ByteAligment(val params: ByteAligmentParams) extends LazyModule()(Parameters.empty) {

    val reorder = Seq.fill(params.channels) {LazyModule(new BitReordering with BitReorderingPins)}
    val bitslip = LazyModule(new BitSlipDetection with BitSlipDetectionPins)

    lazy val io = Wire(new ByteAligmentIO(Flipped(reorder(0).io.i_data.cloneType), Flipped(reorder(0).io.o_data.cloneType), params.channels))

    lazy val module = new LazyModuleImp(this) {

        bitslip.ioBlock.i_data := io.valid
        io.en := ShiftRegister(bitslip.ioBlock.o_en, 3, 0.U, true.B)
        
        reorder.zipWithIndex.map({ case (m, i) =>
          m.ioBlock.i_data := RegNext(io.in(i), 0.U)
          m.ioBlock.i_bitslip := bitslip.ioBlock.o_bitslip
          io.out(i) := m.ioBlock.o_data
        })
    }
}

trait ByteAligmentPins extends ByteAligment{

  def makeCustomIO(): ByteAligmentIO[UInt] = {
    val io2: ByteAligmentIO[UInt] = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }
  val ioBlock = InModuleBody { makeCustomIO() }
}

object ByteAligmentApp extends App
{
    val lazyDut = LazyModule(new ByteAligment(ByteAligmentParams()) with ByteAligmentPins)
    (new ChiselStage).execute(Array("--target-dir", "verilog/ByteAligment"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}














        // // 16 bit word
        //         when(io.i_controlReg === 0.U) {
        //             // bitslip 0
        //             when(io.i_data === "b1111_1111_0000_0000".U){
        //                 r_bitslip := 0.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1
        //             .elsewhen (io.i_data === "b1111_1110_0000_0001".U) {
        //                 r_bitslip := 1.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2
        //             .elsewhen (io.i_data === "b1111_1100_0000_0011".U) {
        //                 r_bitslip := 2.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3
        //             .elsewhen (io.i_data === "b1111_1000_0000_0111".U) {
        //                 r_bitslip := 3.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4
        //             .elsewhen (io.i_data === "b1111_0000_0000_1111".U) {
        //                 r_bitslip := 4.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5
        //             .elsewhen (io.i_data === "b1110_0000_0001_1111".U) {
        //                 r_bitslip := 5.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6
        //             .elsewhen (io.i_data === "b1100_0000_0011_1111".U) {
        //                 r_bitslip := 6.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7
        //             .elsewhen (io.i_data === "b1000_0000_0111_1111".U) {
        //                 r_bitslip := 7.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8
        //             .elsewhen (io.i_data === "b0000_0000_1111_1111".U) {
        //                 r_bitslip := 8.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9
        //             .elsewhen (io.i_data === "b0000_0001_1111_1110".U) {
        //                 r_bitslip := 9.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10
        //             .elsewhen (io.i_data === "b0000_0011_1111_1100".U) {
        //                 r_bitslip := 10.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11
        //             .elsewhen (io.i_data === "b0000_0111_1111_1000".U) {
        //                 r_bitslip := 11.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 12
        //             .elsewhen (io.i_data === "b0000_1111_1111_0000".U) {
        //                 r_bitslip := 12.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 13
        //             .elsewhen (io.i_data === "b0001_1111_1110_0000".U) {
        //                 r_bitslip := 13.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 14
        //             .elsewhen (io.i_data === "b0011_1111_1100_0000".U) {
        //                 r_bitslip := 14.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 15
        //             .elsewhen (io.i_data === "b0111_1111_1000_0000".U) {
        //                 r_bitslip := 15.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 0 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1111".U || io.i_data === ~("b1111_1111_1111_1111".U)) {
        //                 r_bitslip := 0.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1110".U || io.i_data === ~("b1111_1111_1111_1110".U)) {
        //                 r_bitslip := 1.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1100".U || io.i_data === ~("b1111_1111_1111_1100".U)) {
        //                 r_bitslip := 2.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1000".U || io.i_data === ~("b1111_1111_1111_1000".U)) {
        //                 r_bitslip := 3.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_0000".U || io.i_data === ~("b1111_1111_1111_0000".U)) {
        //                 r_bitslip := 4.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1110_0000".U || io.i_data === ~("b1111_1111_1110_0000".U)) {
        //                 r_bitslip := 5.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1100_0000".U || io.i_data === ~("b1111_1111_1100_0000".U)) {
        //                 r_bitslip := 6.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1000_0000".U || io.i_data === ~("b1111_1111_1000_0000".U)) {
        //                 r_bitslip := 7.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8 - crc
        //             .elsewhen (io.i_data === "b1111_1111_0000_0000".U || io.i_data === ~("b1111_1111_0000_0000".U)) {
        //                 r_bitslip := 8.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9 - crc
        //             .elsewhen (io.i_data === "b1111_1110_0000_0000".U || io.i_data === ~("b1111_1110_0000_0000".U)) {
        //                 r_bitslip := 9.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10 - crc
        //             .elsewhen (io.i_data === "b1111_1100_0000_0000".U || io.i_data === ~("b1111_1100_0000_0000".U)) {
        //                 r_bitslip := 10.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11 - crc
        //             .elsewhen (io.i_data === "b1111_1000_0000_0000".U || io.i_data === ~("b1111_1000_0000_0000".U)) {
        //                 r_bitslip := 11.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 12 - crc
        //             .elsewhen (io.i_data === "b1111_0000_0000_0000".U || io.i_data === ~("b1111_0000_0000_0000".U)) {
        //                 r_bitslip := 12.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 13 - crc
        //             .elsewhen (io.i_data === "b1110_0000_0000_0000".U || io.i_data === ~("b1110_0000_0000_0000".U)) {
        //                 r_bitslip := 13.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 14 - crc
        //             .elsewhen (io.i_data === "b1100_0000_0000_0000".U || io.i_data === ~("b1100_0000_0000_0000".U)) {
        //                 r_bitslip := 14.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 15 - crc
        //             .elsewhen (io.i_data === "b1000_0000_0000_0000".U || io.i_data === ~("b1000_0000_0000_0000".U)) {
        //                 r_bitslip := 15.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // error
        //             .otherwise {
        //                 r_bitslip := "b0000".U
        //                 r_crc     := false.B
        //                 r_error   := true.B
        //             }
        //         }
        //         // 12 bit word
        //         .elsewhen(io.i_controlReg === 1.U) {
        //             // bitslip 0
        //             when(io.i_data === "b1111_1100_0000_0000".U){
        //                 r_bitslip := 0.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1
        //             .elsewhen (io.i_data === "b1111_1000_0001_0000".U) {
        //                 r_bitslip := 1.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2
        //             .elsewhen (io.i_data === "b1111_0000_0011_0000".U) {
        //                 r_bitslip := 2.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3
        //             .elsewhen (io.i_data === "b1110_0000_0111_0000".U) {
        //                 r_bitslip := 3.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4
        //             .elsewhen (io.i_data === "b1100_0000_1111_0000".U) {
        //                 r_bitslip := 4.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5
        //             .elsewhen (io.i_data === "b1000_0001_1111_0000".U) {
        //                 r_bitslip := 5.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6
        //             .elsewhen (io.i_data === "b0000_0011_1111_0000".U) {
        //                 r_bitslip := 6.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7
        //             .elsewhen (io.i_data === "b0000_0111_1110_0000".U) {
        //                 r_bitslip := 7.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8
        //             .elsewhen (io.i_data === "b0000_1111_1100_0000".U) {
        //                 r_bitslip := 8.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9
        //             .elsewhen (io.i_data === "b0001_1111_1000_0000".U) {
        //                 r_bitslip := 9.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10
        //             .elsewhen (io.i_data === "b0011_1111_0000_0000".U) {
        //                 r_bitslip := 10.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11
        //             .elsewhen (io.i_data === "b0111_1110_0000_0000".U) {
        //                 r_bitslip := 11.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 0 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_0000".U || io.i_data === ~("b1111_1111_1111_0000".U)) {
        //                 r_bitslip := 0.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1110_0000".U || io.i_data === ~("b1111_1111_1110_0000".U)) {
        //                 r_bitslip := 1.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1100_0000".U || io.i_data === ~("b1111_1111_1100_0000".U)) {
        //                 r_bitslip := 2.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1000_0000".U || io.i_data === ~("b1111_1111_1000_0000".U)) {
        //                 r_bitslip := 3.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4 - crc
        //             .elsewhen (io.i_data === "b1111_1111_0000_0000".U || io.i_data === ~("b1111_1111_0000_0000".U)) {
        //                 r_bitslip := 4.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5 - crc
        //             .elsewhen (io.i_data === "b1111_1110_0000_0000".U || io.i_data === ~("b1111_1110_0000_0000".U)) {
        //                 r_bitslip := 5.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6 - crc
        //             .elsewhen (io.i_data === "b1111_1100_0000_0000".U || io.i_data === ~("b1111_1100_0000_0000".U)) {
        //                 r_bitslip := 6.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7 - crc
        //             .elsewhen (io.i_data === "b1111_1000_0000_0000".U || io.i_data === ~("b1111_1000_0000_0000".U)) {
        //                 r_bitslip := 7.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8 - crc
        //             .elsewhen (io.i_data === "b1111_0000_0000_0000".U || io.i_data === ~("b1111_0000_0000_0000".U)) {
        //                 r_bitslip := 8.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9 - crc
        //             .elsewhen (io.i_data === "b1110_0000_0000_0000".U || io.i_data === ~("b1110_0000_0000_0000".U)) {
        //                 r_bitslip := 9.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10 - crc
        //             .elsewhen (io.i_data === "b1100_0000_0000_0000".U || io.i_data === ~("b1100_0000_0000_0000".U)) {
        //                 r_bitslip := 10.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11 - crc
        //             .elsewhen (io.i_data === "b1000_0000_0000_0000".U || io.i_data === ~("b1000_0000_0000_0000".U)) {
        //                 r_bitslip := 11.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // error
        //             .otherwise {
        //                 r_bitslip := "b0000".U
        //                 r_crc     := false.B
        //                 r_error   := true.B
        //             }
        //         }
        //         // 14 bit word
        //         .elsewhen(io.i_controlReg === 2.U) {
        //             // bitslip 0
        //             when(io.i_data === "b1111_1110_0000_0000".U){
        //                 r_bitslip := 0.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1
        //             .elsewhen (io.i_data === "b1111_1100_0000_0100".U) {
        //                 r_bitslip := 1.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2
        //             .elsewhen (io.i_data === "b1111_1000_0000_1100".U) {
        //                 r_bitslip := 2.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3
        //             .elsewhen (io.i_data === "b1111_0000_0001_1100".U) {
        //                 r_bitslip := 3.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4
        //             .elsewhen (io.i_data === "b1110_0000_0011_1100".U) {
        //                 r_bitslip := 4.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5
        //             .elsewhen (io.i_data === "b1100_0000_0111_1100".U) {
        //                 r_bitslip := 5.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6
        //             .elsewhen (io.i_data === "b1000_0000_1111_1100".U) {
        //                 r_bitslip := 6.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7
        //             .elsewhen (io.i_data === "b0000_0001_1111_1100".U) {
        //                 r_bitslip := 7.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8
        //             .elsewhen (io.i_data === "b0000_0011_1111_1000".U) {
        //                 r_bitslip := 8.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9
        //             .elsewhen (io.i_data === "b0000_0111_1111_0000".U) {
        //                 r_bitslip := 9.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10
        //             .elsewhen (io.i_data === "b0000_1111_1110_0000".U) {
        //                 r_bitslip := 10.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11
        //             .elsewhen (io.i_data === "b0001_1111_1100_0000".U) {
        //                 r_bitslip := 11.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 12
        //             .elsewhen (io.i_data === "b0011_1111_1000_0000".U) {
        //                 r_bitslip := 12.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 13
        //             .elsewhen (io.i_data === "b0111_1111_0000_0000".U) {
        //                 r_bitslip := 13.U
        //                 r_crc     := false.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 0 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1100".U || io.i_data === ~("b1111_1111_1111_1100".U)) {
        //                 r_bitslip := 0.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 1 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_1000".U || io.i_data === ~("b1111_1111_1111_1000".U)) {
        //                 r_bitslip := 1.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 2 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1111_0000".U || io.i_data === ~("b1111_1111_1111_0000".U)) {
        //                 r_bitslip := 2.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 3 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1110_0000".U || io.i_data === ~("b1111_1111_1110_0000".U)) {
        //                 r_bitslip := 3.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 4 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1100_0000".U || io.i_data === ~("b1111_1111_1100_0000".U)) {
        //                 r_bitslip := 4.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 5 - crc
        //             .elsewhen (io.i_data === "b1111_1111_1000_0000".U || io.i_data === ~("b1111_1111_1000_0000".U)) {
        //                 r_bitslip := 5.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 6 - crc
        //             .elsewhen (io.i_data === "b1111_1111_0000_0000".U || io.i_data === ~("b1111_1111_0000_0000".U)) {
        //                 r_bitslip := 6.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 7 - crc
        //             .elsewhen (io.i_data === "b1111_1110_0000_0000".U || io.i_data === ~("b1111_1110_0000_0000".U)) {
        //                 r_bitslip := 7.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 8 - crc
        //             .elsewhen (io.i_data === "b1111_1100_0000_0000".U || io.i_data === ~("b1111_1100_0000_0000".U)) {
        //                 r_bitslip := 8.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 9 - crc
        //             .elsewhen (io.i_data === "b1111_1000_0000_0000".U || io.i_data === ~("b1111_1000_0000_0000".U)) {
        //                 r_bitslip := 9.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 10 - crc
        //             .elsewhen (io.i_data === "b1111_0000_0000_0000".U || io.i_data === ~("b1111_0000_0000_0000".U)) {
        //                 r_bitslip := 10.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 11 - crc
        //             .elsewhen (io.i_data === "b1110_0000_0000_0000".U || io.i_data === ~("b1110_0000_0000_0000".U)) {
        //                 r_bitslip := 11.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 12 - crc
        //             .elsewhen (io.i_data === "b1100_0000_0000_0000".U || io.i_data === ~("b1100_0000_0000_0000".U)) {
        //                 r_bitslip := 12.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // bitslip 13 - crc
        //             .elsewhen (io.i_data === "b1000_0000_0000_0000".U || io.i_data === ~("b1000_0000_0000_0000".U)) {
        //                 r_bitslip := 13.U
        //                 r_crc     := true.B
        //                 r_error   := false.B
        //             }
        //             // error
        //             .otherwise {
        //                 r_bitslip := "b0000".U
        //                 r_crc     := false.B
        //                 r_error   := true.B
        //             }
        //         }
        //         // error
        //         .otherwise {
        //             r_bitslip := "b0000".U
        //             r_crc     := false.B
        //             r_error   := true.B
        //         }