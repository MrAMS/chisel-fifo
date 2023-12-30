package fifo

import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec

import scala.annotation.tailrec

class FifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def testFifo[T <: Fifo[_ <: Data]](dut: T) = {
    val batchSize = 100
    val timeOut = 5 * dut.depth

    val randUint = (x: Data) => BigInt(x.getWidth, scala.util.Random).U

    val randVal = randUint(dut.io.enq.bits)
    // init
    dut.io.enq.bits.asUInt.poke(0.U)
    dut.io.enq.valid.poke(false.B)
    dut.clock.step()
    // check init signals
    dut.io.deq.valid.expect(false.B)
    dut.io.enq.ready.expect(true.B)
    dut.clock.step()
    // push data into fifo
    dut.io.deq.ready.poke(false.B)
    dut.io.enq.bits.asUInt.poke(randVal)
    dut.io.enq.valid.poke(true.B)
    dut.clock.step()
    dut.io.enq.valid.poke(false.B)

    @tailrec // make sure tail call
    def check(cycles: Int, expectData: UInt, timeout: Int): Int = {
      if (dut.io.deq.valid.peek().litToBoolean){
        dut.io.deq.bits.asUInt.expect(expectData, s"Received wrong value after $cycles cycles")
        dut.io.deq.ready.poke(true.B)
        dut.clock.step()
        // println(s"Received expected value $expectData after $cycles cycles")
        cycles
      }else{
        assert(cycles != timeout, s"Timeout after $cycles cycles")
        dut.clock.step()
        check(cycles + 1, expectData, timeout)
      }
    }
    println(s"[OK] Single data test passed. Received after ${check(1, randVal, timeOut)} cycles")
    dut.io.deq.valid.expect(false.B)
    dut.io.enq.ready.expect(true.B)

    val datas = Array.fill(batchSize){randUint(dut.io.enq.bits)}

    // datas.map(x => print(s"$x,"))
    fork{
      for (i <- 0 until batchSize){
        while(!dut.io.enq.ready.peek().litToBoolean){
          dut.clock.step()
        }
        dut.io.enq.bits.asUInt.poke(datas(i))
        dut.io.enq.valid.poke(true.B)
        dut.clock.step()
      }
    }
    fork{
      var cycles = 1
      for (i <- 0 until batchSize)
        cycles = check(cycles+1, datas(i), batchSize * timeOut)
      println(s"[OK] $batchSize size data test passed. Received after $cycles cycles")
      val speed = cycles / batchSize
      assert(speed >= 1, "Cannot be faster than one clock cycle per word")
      println(s"$batchSize words in $cycles clock cycles, $speed clock cycles per word")
    }.join()
    
  }
  behavior of "BubbleFifo"
  it should "pass" in {
    test(new BubbleFifo(UInt(8.W), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testFifo(dut)
    }
  }
  behavior of "DoubleBufFifo"
  it should "pass" in {
    test(new DoubleBufFifo(UInt(8.W), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testFifo(dut)
    }
  }
}
