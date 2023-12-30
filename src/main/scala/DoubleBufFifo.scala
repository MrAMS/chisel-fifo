package fifo
import chisel3._
import chisel3.util._

class DoubleBufFifo[T <: Data](t: T, depth: Int) extends Fifo(t: T, depth: Int){
    private class Buffer() extends Module{
        val io = IO(new FifoIO(t))
        val dataReg = Reg(t)
        val shadowReg = Reg(t)
        object State extends ChiselEnum{
            val empty, one, two = Value
        }
        val stateReg = RegInit(State.empty)

        switch(stateReg){
            is(State.empty){
                when(io.enq.valid) {
                    stateReg := State.one
                    dataReg := io.enq.bits
                }
            }
            is(State.one){
                when((!io.deq.ready)&(io.enq.valid)){
                    stateReg := State.two
                    shadowReg := io.enq.bits
                }
                when(io.deq.ready&io.enq.valid){
                    stateReg := State.one
                    dataReg := io.enq.bits
                }
                when(io.deq.ready&(!io.enq.valid)){
                    stateReg := State.empty
                }
            }
            is(State.two){
                when(io.deq.ready){
                    stateReg := State.one
                    dataReg := shadowReg
                }
            }
        }
        io.enq.ready := stateReg =/= State.two
        io.deq.valid := stateReg =/= State.empty
        io.deq.bits := dataReg
    }

    private val buffers = Array.fill(depth){Module(new Buffer())}
    for(i <- 0 until depth-1){
        buffers(i+1).io.enq <> buffers(i).io.deq
    }
    io.enq <> buffers(0).io.enq
    io.deq <> buffers(depth-1).io.deq

}