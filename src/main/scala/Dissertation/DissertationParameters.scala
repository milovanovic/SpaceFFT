// SPDX-License-Identifier: Apache-2.0

package dissertation

import chisel3._
import chisel3.util._
import chisel3.experimental.{FixedPoint}

import dsptools.numbers._

import spacefft.fft2control._
import spacefft.measurement._
import dsputils._
import xWRDataPreProc._
import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._
import adder._

/* DDR type trait */
sealed trait DDRType
/* DDR3 */
case object DDR3 extends DDRType
/* DDR4 */
case object DDR4  extends DDRType 

/* Dissertation parameters */
case class DissertationParameters[T <: Data: Real: BinaryRepresentation] (
  // Range parameters
  dataChannels : Int,
  prep1DParams : Option[PreprocParamsAndAddresses],
  win1DParams  : Option[WinParamsAndAddresses[T]],
  fft1DParams  : Option[FFTParamsAndAddresses[T]],
  mag1DParams  : Option[MagParamsAndAddresses[T]],
  acc1DParams  : Option[AccParamsAndAddresses[T]],
  cfar1DParams : Option[CFARParamsAndAddresses[T]],
  // Doppler parameters
  ctrl2DParams : Option[Ctrl2DParamsAndAddresses],
  ddrParams    : Option[DDRParamsAndAddresses],
  splitParams  : Option[SplitParamsAndAddresses],
  queueParams  : Option[QueueParamsAndAddresses],
  fft2DParams  : Option[FFTParamsAndAddresses[T]],
  mag2DParams  : Option[MagParamsAndAddresses[T]],
  // Adder parameters
  adderParams  : NonCoherentAdderParams[T],
  // Measurement parameters
  measurementParams: Option[MeasurementParamsAndAddresses]
)
/* Pre-processing parameters and addresses */
case class PreprocParamsAndAddresses(
  prepParams  : AXI4XwrDataPreProcParams,
  prepAddress : BigInt
)
/* Windows parameters and addresses */
case class WinParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  winParams     : WindowingParams[T],
  winRAMAddress : BigInt,
  winCSRAddress : BigInt
)
/* FFT parameters and addresses */
case class FFTParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  fftParams  : FFTParams[T],
  fftAddress : BigInt
)
/* Magnitude parameters and addresses */
case class MagParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  magParams  : MAGParams[T],
  magAddress : BigInt
)
/* Accumulator parameters and addresses */
case class AccParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  accParams    : AccParams[T],
  accAddress   : BigInt,
  accQueueBase : BigInt
)
/* CFAR parameters and addresses */
case class CFARParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  cfarParams  : CFARParams[T],
  cfarAddress : BigInt
)
/* Queue parameters and addresses */
case class QueueParamsAndAddresses (
  queueParams  : DspQueueCustomParams,
  queueAddress : BigInt
)
/* Splitter parameters and addresses */
case class SplitParamsAndAddresses (
  splitAddress : BigInt
)
/* 2D FFT control parameters and addresses */
case class Ctrl2DParamsAndAddresses (
  ctrl2DParams  : FFT2ControlParams,
  ctrl2DAddress : BigInt
)
/* DDR parameters and addresses */
case class DDRParamsAndAddresses (
  ddrParams : DDRType
)
/* Measurement parameters and addresses */
case class MeasurementParamsAndAddresses (
  detectSignalParams  : DetectSignalParameters,
  detectSignalAddress : BigInt,
  splitAddress        : BigInt,
  delayMeasurementParams  : DelayMeasurementParameters,
  delayMeasurementAddress : BigInt
)

/* DissertationParams declaration, DDR3, measurement */
class DissertationMeasurementParams(rangeFFTSize: Int = 1024, dopplerFFTSize: Int = 256, ddrType: DDRType = DDR3, channels: Int = 1) {
  val params : DissertationParameters[FixedPoint] = DissertationParameters (
    // Range parameters
    dataChannels = channels,
    prep1DParams = Some(PreprocParamsAndAddresses(
      prepParams = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFTSize, useBlockRam = true),
      prepAddress = 0x60000000
    )),
    win1DParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        dataWidth = 16,
        numPoints = rangeFFTSize,
        binPoint = 10,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/BlackmanRunTime.hex",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16),
        constWindow = true,
      ),
      winRAMAddress = 0x60010000,
      winCSRAddress = 0x60001000
    )),
    fft1DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = rangeFFTSize,
        useBitReverse  = true,
        runTime = false,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(rangeFFTSize))(0),
        keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
        minSRAMdepth = rangeFFTSize, 
        binPoint = 10
      ),
      fftAddress = 0x60002000
    )),
    mag1DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 10.BP),
        protoOut = FixedPoint(16.W, 10.BP),
        protoLog = Some(FixedPoint(16.W, 10.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 10,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = 0x60003000,
    )),
    acc1DParams = None,
    cfar1DParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 10.BP),
        protoThreshold = FixedPoint(16.W, 10.BP),
        protoScaler = FixedPoint(16.W, 10.BP),
        leadLaggWindowSize = 16,
        guardWindowSize = 4,
        logOrLinReg = false,
        retiming = false,
        fftSize = rangeFFTSize,
        sendCut = true,
        minSubWindowSize = None,
        includeCASH = false,
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,  // number of add pipeline registers
        numMulPipes = 1   // number of mull pipeline registers
      ),
      cfarAddress   = 0x60004000
    )),
    // Doppler parameters
    ctrl2DParams = Some(Ctrl2DParamsAndAddresses(
      ctrl2DParams =  FFT2ControlParams(rangeFFTSize = rangeFFTSize, dopplerFFTSize = dopplerFFTSize),
      ctrl2DAddress = 0x60005000
    )),
    ddrParams = Some(DDRParamsAndAddresses(
      ddrParams = ddrType
    )),
    queueParams = Some(QueueParamsAndAddresses(
      queueParams = DspQueueCustomParams(
        queueDepth = dopplerFFTSize, // should be the same as max dopplerFFTSize
        progFull = false,
        addEnProgFullOut = false,
        useSyncReadMem = false, // do not use distributed ram, trying to eliminate timing issues
        enLastGen = false
      ),
      queueAddress = 0x60020000
    )),
    splitParams = Some(SplitParamsAndAddresses(
      splitAddress = 0x60006000
    )),
    fft2DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = dopplerFFTSize,
        useBitReverse  = true,
        runTime = false,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),
        keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
        minSRAMdepth = dopplerFFTSize,
        binPoint = 10
      ),
      fftAddress = 0x60007000
    )),
    mag2DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 10.BP),
        protoOut = FixedPoint(16.W, 10.BP),
        protoLog = Some(FixedPoint(16.W, 10.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 10,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = 0x60008000,
    )),
    adderParams = NonCoherentAdderParams(
      proto = FixedPoint(16.W, 10.BP),
      numAddPipes = 0,
      trimType    = Convergent,
      verbose     = true
    ),
    measurementParams = Some(MeasurementParamsAndAddresses(
      detectSignalParams  = DetectSignalParameters(numberOfData = Seq.fill(channels){2*rangeFFTSize} ++ Seq(rangeFFTSize), channels = channels + 1),
      detectSignalAddress = 0x60009000,
      splitAddress        = 0x6000A000,
      delayMeasurementParams  = DelayMeasurementParameters(channels = channels + 1),
      delayMeasurementAddress = 0x6000B000
    ))
  )
}