// SPDX-License-Identifier: Apache-2.0

package spacefft

import chisel3._
import chisel3.util._
import chisel3.experimental.{FixedPoint}

import dsptools.numbers._
import freechips.rocketchip.diplomacy._

import spacefft.fft2control._
import dsputils._
import lvdsphy._
import crc._
import xWRDataPreProc._
import fft._
import windowing._
import magnitude._
import accumulator._
import cfar._

/* DDR type trait */
sealed trait DDRType
/* DDR3 */
case object DDR3 extends DDRType
/* DDR4 */
case object DDR4  extends DDRType 

/* SpaceFFT parameters */
case class SpaceFFTParameters[T <: Data: Real: BinaryRepresentation] (
  // Range parameters
  lvds1DParams : Option[LVDSPHYParamsAndAddresses],
  crc1DParams  : Option[CRCParamsAndAddresses],
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
  mag2DParams  : Option[MagParamsAndAddresses[T]]
)

/* LVDS-PHY parameters and addresses */
case class LVDSPHYParamsAndAddresses (
  lvdsphyParams : DataRXParams,
)
/* CRC parameters and addresses */
case class CRCParamsAndAddresses(
  crcParams  : MiltipleCrcBlockParams,
  crcAddress : AddressSet
)
/* Pre-processing parameters and addresses */
case class PreprocParamsAndAddresses(
  prepParams  : AXI4XwrDataPreProcParams,
  prepAddress : AddressSet
)
/* Windows parameters and addresses */
case class WinParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  winParams     : WindowingParams[T],
  winRAMAddress : AddressSet,
  winCSRAddress : AddressSet
)
/* FFT parameters and addresses */
case class FFTParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  fftParams  : FFTParams[T],
  fftAddress : AddressSet
)
/* Magnitude parameters and addresses */
case class MagParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  magParams  : MAGParams[T],
  magAddress : AddressSet
)
/* Accumulator parameters and addresses */
case class AccParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  accParams    : AccParams[T],
  accAddress   : AddressSet,
  accQueueBase : BigInt
)
/* CFAR parameters and addresses */
case class CFARParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  cfarParams  : CFARParams[T],
  cfarAddress : AddressSet
)
/* Queue parameters and addresses */
case class QueueParamsAndAddresses (
  queueParams  : DspQueueCustomParams,
  queueAddress : AddressSet
)
/* Splitter parameters and addresses */
case class SplitParamsAndAddresses (
  splitAddress : AddressSet
)
/* 2D FFT control parameters and addresses */
case class Ctrl2DParamsAndAddresses (
  ctrl2DParams  : FFT2ControlParams,
  ctrl2DAddress : AddressSet
)
/* DDR parameters and addresses */
case class DDRParamsAndAddresses (
  ddrParams : DDRType
)

/* SpaceFFTParams declaration, DDR4 */
class SpaceFFTParams(rangeFFTSize: Int = 512, dopplerFFTSize: Int = 256) {
  val params : SpaceFFTParameters[FixedPoint] = SpaceFFTParameters (
    // Range parameters
    lvds1DParams = Some(LVDSPHYParamsAndAddresses(
      lvdsphyParams = DataRXParams(
        channels = 1,
        asyncParams = Some(AXI4StreamAsyncQueueWithControlParams(
          ctrlBits   = 3,
          sync       = 4,
          depth      = 32,
          safe       = true
        ))
      )
    )),
    crc1DParams = Some(CRCParamsAndAddresses(
      crcParams = MiltipleCrcBlockParams(
        crcParams16 = Some(RadarCRCParams(dataWidth = 16)),
        crcParams12 = Some(RadarCRCParams(dataWidth = 12)),
        crcParams14 = Some(RadarCRCParams(dataWidth = 14)),
      ),
      crcAddress = AddressSet(0x60000000, 0xFF)
    )),
    prep1DParams = Some(PreprocParamsAndAddresses(
      prepParams = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFTSize, useBlockRam = true),
      prepAddress = AddressSet(0x60000100, 0xFF)
    )),
    win1DParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        numPoints = rangeFFTSize,
        dataWidth = 16,
        binPoint  = 14,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/blacman.txt",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
      ),
      winRAMAddress = AddressSet(0x60001000, 0xFFF),
      winCSRAddress = AddressSet(0x60000200, 0xFF)
    )),
    fft1DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = rangeFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(rangeFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
        minSRAMdepth = rangeFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000300, 0xFF)
    )),
    mag1DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000400, 0xFF),
    )),
    acc1DParams = Some(AccParamsAndAddresses(
      accParams = AccParams(
        proto    = FixedPoint(16.W, 14.BP),
        protoAcc = FixedPoint(32.W, 14.BP),
      ),
      accAddress   = AddressSet(0x60000500, 0xFF),
      accQueueBase = 0x60002000
    )),
    cfar1DParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 14.BP),
        protoThreshold = FixedPoint(16.W, 14.BP),
        protoScaler = FixedPoint(16.W, 14.BP),
        leadLaggWindowSize = 64,
        guardWindowSize = 8,
        logOrLinReg = false,
        retiming = false,
        fftSize = rangeFFTSize,
        sendCut = true,
        minSubWindowSize = None,
        includeCASH = false,
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,                  // number of add pipeline registers
        numMulPipes = 1                   // number of mull pipeline registers
      ),
      cfarAddress   = AddressSet(0x60000600, 0xFF)
    )),
    // Doppler parameters
    ctrl2DParams = Some(Ctrl2DParamsAndAddresses(
      ctrl2DParams =  FFT2ControlParams(rangeFFTSize = rangeFFTSize, dopplerFFTSize = dopplerFFTSize),
      ctrl2DAddress = AddressSet(0x60000700, 0xFF)
    )),
    ddrParams = Some(DDRParamsAndAddresses(
      ddrParams = DDR4
    )),
    queueParams = Some(QueueParamsAndAddresses(
      queueParams = DspQueueCustomParams(
        queueDepth = dopplerFFTSize, // should be the same as max dopplerFFTSize
        progFull = false,
        addEnProgFullOut = false,
        useSyncReadMem = false, // do not use distributed ram, trying to eliminate timing issues
        enLastGen = false
      ),
      queueAddress = AddressSet(0x60003000, 0xFFF)
    )),
    splitParams = Some(SplitParamsAndAddresses(
      splitAddress = AddressSet(0x60000800, 0xFF)
    )),
    fft2DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = dopplerFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
        minSRAMdepth = dopplerFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000900, 0xFF)
    )),
    mag2DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000A00, 0xFF),
    )),
  )
}

/* SpaceFFTParams declaration, no DDR */
class SpaceFFTnoDDRParams(rangeFFTSize: Int = 512, dopplerFFTSize: Int = 256) {
  val params : SpaceFFTParameters[FixedPoint] = SpaceFFTParameters (
    // Range parameters
    lvds1DParams = Some(LVDSPHYParamsAndAddresses(
      lvdsphyParams = DataRXParams(
        channels = 1,
        asyncParams = Some(AXI4StreamAsyncQueueWithControlParams(
          ctrlBits   = 3,
          sync       = 4,
          depth      = 32,
          safe       = true
        ))
      )
    )),
    crc1DParams = Some(CRCParamsAndAddresses(
      crcParams = MiltipleCrcBlockParams(
        crcParams16 = Some(RadarCRCParams(dataWidth = 16)),
        crcParams12 = Some(RadarCRCParams(dataWidth = 12)),
        crcParams14 = Some(RadarCRCParams(dataWidth = 14)),
      ),
      crcAddress = AddressSet(0x60000000, 0xFF)
    )),
    prep1DParams = Some(PreprocParamsAndAddresses(
      prepParams = AXI4XwrDataPreProcParams(maxFFTSize = rangeFFTSize, useBlockRam = true),
      prepAddress = AddressSet(0x60000100, 0xFF)
    )),
    win1DParams = Some(WinParamsAndAddresses(
      winParams = WindowingParams.fixed(
        numPoints = rangeFFTSize,
        dataWidth = 16,
        binPoint  = 14,
        numMulPipes = 1,
        dirName = "test_run_dir",
        memoryFile = "./test_run_dir/blacman.txt",
        windowFunc = windowing.WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
      ),
      winRAMAddress = AddressSet(0x60001000, 0xFFF),
      winCSRAddress = AddressSet(0x60000200, 0xFF)
    )),
    fft1DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = rangeFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(rangeFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(rangeFFTSize))(true),
        minSRAMdepth = rangeFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000300, 0xFF)
    )),
    mag1DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000400, 0xFF),
    )),
    acc1DParams = Some(AccParamsAndAddresses(
      accParams = AccParams(
        proto    = FixedPoint(16.W, 14.BP),
        protoAcc = FixedPoint(32.W, 14.BP),
      ),
      accAddress   = AddressSet(0x60000500, 0xFF),
      accQueueBase = 0x60002000
    )),
    cfar1DParams = Some(CFARParamsAndAddresses(
      cfarParams = CFARParams(
        protoIn = FixedPoint(16.W, 14.BP),
        protoThreshold = FixedPoint(16.W, 14.BP),
        protoScaler = FixedPoint(16.W, 14.BP),
        leadLaggWindowSize = 64,
        guardWindowSize = 8,
        logOrLinReg = false,
        retiming = false,
        fftSize = rangeFFTSize,
        sendCut = true,
        minSubWindowSize = None,
        includeCASH = false,
        CFARAlgorithm = CACFARType,
        numAddPipes = 1,                  // number of add pipeline registers
        numMulPipes = 1                   // number of mull pipeline registers
      ),
      cfarAddress   = AddressSet(0x60000600, 0xFF)
    )),
    // Doppler parameters
    ctrl2DParams = Some(Ctrl2DParamsAndAddresses(
      ctrl2DParams =  FFT2ControlParams(rangeFFTSize = rangeFFTSize, dopplerFFTSize = dopplerFFTSize),
      ctrl2DAddress = AddressSet(0x60000700, 0xFF)
    )),
    ddrParams = None,
    queueParams = Some(QueueParamsAndAddresses(
      queueParams = DspQueueCustomParams(
        queueDepth = dopplerFFTSize, // should be the same as max dopplerFFTSize
        progFull = false,
        addEnProgFullOut = false,
        useSyncReadMem = false, // do not use distributed ram, trying to eliminate timing issues
        enLastGen = false
      ),
      queueAddress = AddressSet(0x60003000, 0xFFF)
    )),
    splitParams = Some(SplitParamsAndAddresses(
      splitAddress = AddressSet(0x60000800, 0xFF)
    )),
    fft2DParams = Some(FFTParamsAndAddresses(
      fftParams = FFTParams.fixed(
        dataWidth = 16,
        twiddleWidth = 16,
        numPoints = dopplerFFTSize,
        useBitReverse  = true,
        runTime = true,
        numAddPipes = 1,
        numMulPipes = 1,
        use4Muls = true,
        //sdfRadix = "2",
        expandLogic = Array.fill(log2Up(dopplerFFTSize))(0),//(1).zipWithIndex.map { case (e,ind) => if (ind < 4) 1 else 0 }, // expand first four stages, other do not grow
        keepMSBorLSB = Array.fill(log2Up(dopplerFFTSize))(true),
        minSRAMdepth = dopplerFFTSize, // memories larger than 64 should be mapped on block ram
        binPoint = 14
      ),
      fftAddress = AddressSet(0x60000900, 0xFF)
    )),
    mag2DParams = Some(MagParamsAndAddresses(
      magParams = MAGParams(
        protoIn  = FixedPoint(16.W, 14.BP),
        protoOut = FixedPoint(16.W, 14.BP),
        protoLog = Some(FixedPoint(16.W, 14.BP)),
        magType  = MagJPLandSqrMag,
        log2LookUpWidth = 14,
        useLast = true,
        numAddPipes = 1,
        numMulPipes = 1
      ),
      magAddress = AddressSet(0x60000A00, 0xFF),
    )),
  )
}