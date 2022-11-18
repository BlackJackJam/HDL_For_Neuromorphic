package neuronT
import chisel3._
import chisel3.util._

// IO bundle
class neuronIO(N: Int, S: Int) extends Bundle{
  val firing_threshold = Input(SInt(N.W))
  val syn_weight = Input(SInt(S.W))
  val spike_in = Input(Bool())
  val spike_out = Output(Bool())
  val clk = Input(Clock())
  val rst = Input(AsyncReset())
}

// neuronReg, value in bundle
class neuronRegVal(pWidth: Int) extends Bundle{
  val spike_out = Bool()
  val membrane_potential = SInt(pWidth.W)
}

class neuron(fireRes: Int, synRes: Int) extends RawModule {
  // IO signal
  val io = IO(new neuronIO(N = fireRes, S = synRes))
  // Local Signal
  val sign_ext_syn_weight = Mux(io.spike_in, Cat(Fill((fireRes-synRes),io.syn_weight(synRes-1)),io.syn_weight).asSInt, 0.S(fireRes.W)) // Extension

  val neuronReg = withClockAndReset(io.clk, io.rst){RegInit({val bundle = Wire(new neuronRegVal(pWidth = fireRes))
                                              bundle.spike_out := false.B
                                              bundle.membrane_potential := 0.S
                                              bundle
                                              })}

  val membrane_potential_next_ovfl = Wire(SInt())
//  val mem_test = (~membrane_potential_next_ovfl(fireRes-1)).asBool
  membrane_potential_next_ovfl := neuronReg.membrane_potential + sign_ext_syn_weight
  val posOvflValue = Cat(0.U(1.W),Fill((fireRes-1),1.U(1.W))).asSInt
  val negOvflValue = (~posOvflValue).asSInt
  val negOvflFlag = (!membrane_potential_next_ovfl(fireRes-1))&neuronReg.membrane_potential(fireRes-1)&io.syn_weight(synRes-1)
  val posOvflFlag = membrane_potential_next_ovfl(fireRes-1)&(!neuronReg.membrane_potential(fireRes-1))&(!io.syn_weight(synRes-1))
  val membrane_potential_next = MuxCase(membrane_potential_next_ovfl, Array( negOvflFlag -> negOvflValue, posOvflFlag -> posOvflValue))
//  val membrane_potential_next = Mux(((!membrane_potential_next_ovfl(fireRes-1))&neuronReg.membrane_potential(fireRes-1)&io.syn_weight(synRes-1))
//    , Cat(1.U(1.W),Fill((fireRes-1),0.U(1.W))).asSInt
//    ,Mux((membrane_potential_next_ovfl(fireRes-1)&(!neuronReg.membrane_potential(fireRes-1))&(!io.syn_weight(synRes-1)))
//      ,Cat(0.U(1.W),Fill((fireRes-1),1.U(1.W))).asSInt,membrane_potential_next_ovfl))
  
  when(membrane_potential_next.asSInt >= io.firing_threshold){
    neuronReg.membrane_potential := 0.S
    neuronReg.spike_out := true.B
  } .otherwise{
    neuronReg.membrane_potential := membrane_potential_next
    neuronReg.spike_out := false.B
  }
  io.spike_out := neuronReg.spike_out
}

//object neuron extends App {
//  println(getVerilogString(new neuron(16,4)))
//}
