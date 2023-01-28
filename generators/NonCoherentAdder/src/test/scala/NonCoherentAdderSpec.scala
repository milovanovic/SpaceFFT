// SPDX-License-Identifier: Apache-2.0

package adder

import chisel3._
import chisel3.experimental._
import dsptools.numbers._
import dsptools._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class NonCoherentAdderSpec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  val numBytes = 2
  val numOfInputs = 4
  val testSize = 10000

  it should "test Non-Coherent Adder without pipe registers" in {
    val params: NonCoherentAdderParams[FixedPoint] = NonCoherentAdderParams(
      proto = FixedPoint(16.W, 14.BP),
      numAddPipes = 0,
      trimType = NoTrim
    )
    val lazyDut = LazyModule(new NonCoherentAdder(params) with NonCoherentAdderInOuts[FixedPoint]{
      override def nIn  = numOfInputs
      override def nOut = 1
      override def beatBytes = numBytes
    })

    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/NonCoherentAdder_NoPipe", "--top-name", "NonCoherentAdder")) {
      c => new NonCoherentAdder_RandomTester(lazyDut, params, testSize, numBytes, true)
    } should be (true)
  }

  it should "test Non-Coherent Adder with pipe registers" in {
    val params: NonCoherentAdderParams[FixedPoint] = NonCoherentAdderParams(
      proto = FixedPoint(16.W, 14.BP),
      numAddPipes = 1,
      trimType = NoTrim
    )
    val lazyDut = LazyModule(new NonCoherentAdder(params) with NonCoherentAdderInOuts[FixedPoint]{
      override def nIn  = numOfInputs
      override def nOut = 1
      override def beatBytes = numBytes
    })

    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/NonCoherentAdder_Pipe", "--top-name", "NonCoherentAdder")) {
      c => new NonCoherentAdder_RandomTester(lazyDut, params, testSize, numBytes, true)
    } should be (true)
  }
}