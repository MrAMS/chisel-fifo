package fifo
import chisel3._
import chisel3.util._

class BubbleFifo[T <: Data](t: T, depth: Int) extends Fifo(t: T, depth: Int){
    private class Buffer() extends Module{
        val io = IO(new FifoIO(t))
        val full = RegInit(false.B)
        val dataReg = Reg(t)

        when((!full)&io.enq.valid){
            dataReg := io.enq.bits
            full := true.B
        }.elsewhen(full&io.deq.ready){
            full := false.B
        }

        io.enq.ready := !full
        io.deq.valid := full
        io.deq.bits := dataReg
    }
    private val buffers = Array.fill(depth) {Module(new Buffer())}
    for (i <- 0 until depth-1) {
        buffers(i+1).io.enq <> buffers(i).io.deq
    }
    io.enq <> buffers(0).io.enq
    io.deq <> buffers(depth-1).io.deq
}