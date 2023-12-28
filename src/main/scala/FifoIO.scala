package fifo

import chisel3._
import chisel3.util._

class FifoIO[T <: Data](t: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(t))
  val deq = new DecoupledIO(t)
}

abstract class Fifo[T <: Data](val t: T, val depth: Int) extends Module {
    val io = IO(new FifoIO(t))
    assert(depth > 0)
}

