// SPDX-License-Identifier: Apache-2.0

package adder

import chisel3._
import chisel3.util.log2Ceil
import dsptools._
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import scala.util.Random

//---------------------------------------------------------------------------------------------------------------------------------------------------------------
// Non-Coherent adder
//---------------------------------------------------------------------------------------------------------------------------------------------------------------
class NonCoherentAdder_RandomTester[T <: Data: Real : BinaryRepresentation]
(
  dut: NonCoherentAdder[T] with NonCoherentAdderInOuts[T],
  params: NonCoherentAdderParams[T],
  testSize : Int,
  beatBytes : Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{
  
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val dataSize = testSize // Data size

  val mod = dut.module // module
  val numOfInputs = mod.ins.length // Number of inputs

  // Input data
  var data = scala.collection.mutable.Seq[Seq[BigInt]]()
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
    expectedData = expectedData :+ (data.map{case (in) => in(i)}.reduce((a, b) => a + b) >> log2Ceil(numOfInputs))
  }

  step(1)
  var inValid = 0
  while(!expectedData.isEmpty) {

    inValid = Random.nextInt(2)
    poke(dut.outIO.head.ready, Random.nextInt(2))
    if (!data.head.isEmpty) {
      for ((in, inIdx) <- dut.inIO.zipWithIndex) {
        poke(in.valid, inValid)
        if (peek(in.ready) == true && peek(in.valid) == true) {
          poke(in.bits.data, if (data(inIdx).head < 0) (data(inIdx).head + BigInt(1L << (8*beatBytes))) & BigInt((1L << (8*beatBytes))-1L) else data(inIdx).head)
          data(inIdx) = data(inIdx).tail
        }
      }
    }
    if (peek(dut.outIO.head.ready) == true && peek(dut.outIO.head.valid) == true) {
      expect(dut.outIO.head.bits.data, if (expectedData.head < 0) (expectedData.head + BigInt(1L << (8*beatBytes))) & BigInt((1L << (8*beatBytes))-1L) else expectedData.head)
      expectedData = expectedData.tail
    }
    step(1)
  }
  stepToCompletion(maxCycles = 10*dataSize, silentFail = silentFail)
}