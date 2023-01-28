// SPDX-License-Identifier: Apache-2.0

package adder

import chisel3._
import chisel3.util.log2Ceil
import chisel3.experimental.FixedPoint
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import scala.util.Random

//---------------------------------------------------------------------------------------------------------------------------------------------------------------
// Non-Coherent adder
//---------------------------------------------------------------------------------------------------------------------------------------------------------------
class NonCoherentAdder_Tester[T <: Data: Real : BinaryRepresentation]
(
  dut: NonCoherentAdder[T] with NonCoherentAdderInOuts[T],
  params: NonCoherentAdderParams[T],
  beatBytes : Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{
  
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val dataSize = 100 // Data size

  val mod = dut.module // module
  val numOfInputs = mod.ins.length // Number of inputs

  // Bind masters and slave
  val inMasters = dut.inIO.map(in => bindMaster(in.getWrappedValue))
  val outSlaves = dut.outIO.map(out => bindSlave(out.getWrappedValue))

  // Input data
  var data = Seq[Seq[BigInt]]()
  for (i <- 0 until numOfInputs) {
    var temp = Seq[BigInt]()
    for (j <- 0 until dataSize) {
      temp = temp :+ BigInt(Random.nextInt((1 << beatBytes*8)-1) - (1 << (beatBytes*8-1)))
    }
    data = data :+ temp
  }

  // Expected data
  var expectedData = Seq[BigInt]()
  for (i <- 0 until dataSize) {
    if (params.trimType == NoTrim) {
      expectedData = expectedData :+ (data.map{case (in) => in(i)}.reduce((a, b) => a + b) >> log2Ceil(numOfInputs))
    }
    else {
      val binPos = (params.proto match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })
      require(binPos > 0, "Binary point of input fixed point number must be larger than zero when trimming")
      val rBP = binPos - 1
      val eBP = binPos + 1
      expectedData = expectedData :+ (data.map{
        case (in) => { 
          val sqr     = in(i) * in(i)
          val sqr_bp  = sqr >> binPos
          val sqr_rbp = sqr >> rBP
          val sqr_ebp = sqr >> eBP
          if ((sqr_bp == (sqr_ebp) << 1) && (sqr == (sqr_rbp << rBP))) {
            sqr_bp
          }
          else {
            ((sqr + (1 << (binPos-1)) >> binPos))
          }
        }
      }.reduce((a,b) => a + b) >> (params.proto.getWidth + log2Ceil(numOfInputs) - binPos))
    }
  }

  // Add expected data
  for ((out, outIdx) <- outSlaves.zipWithIndex) {
    out.addExpects((0 until expectedData.length).map(i => AXI4StreamTransactionExpect(
      data = Some(if (expectedData(i) < 0) (expectedData(i) + BigInt(1L << (8*beatBytes))) & BigInt((1L << (8*beatBytes))-1L) else expectedData(i)) // Convert negative numbers to UInt)
    )))
  }

  step(1) // step added in order for simulation to work correctly

  // Add input data
  for ((in, inIdx) <- inMasters.zipWithIndex) {
    in.addTransactions((0 until data(inIdx).length).map(i => AXI4StreamTransaction(
      data = if (data(inIdx)(i) < 0) (data(inIdx)(i) + BigInt(1L << (8*beatBytes))) & BigInt((1L << (8*beatBytes))-1L) else data(inIdx)(i) // Convert negative numbers to UInt
    )))
  }

  stepToCompletion(maxCycles = 2*dataSize, silentFail = silentFail)
}