import spinal.core._

class IFNeuron_Comp (fireRes: Int, synRes: Int) extends Component{
  val io = new Bundle{
    val firing_threshold  = in SInt(fireRes bits)
    val syn_weight        = in SInt(synRes bits)
    val spike_in          = in Bool()
    val spike_out         = out Bool()
  }

//  val membrane_potential_next = SInt(fireRes bits)
//  val membrane_potential_ovfl SInt(fireRes bits)
//  val sign_ext_syn_weight     SInt(fireRes bits)
  val spike_output = Reg(Bool()) init(False)
  val membrane_potential = Reg(SInt(fireRes bits)) init(S(0,fireRes bits))


  // use resize() function to get extension
  val sign_ext_syn_weight = Mux(io.spike_in, io.syn_weight.resize(fireRes), S(0,fireRes bits))
  val membrane_potential_next_ovfl = (membrane_potential + sign_ext_syn_weight)
  val membrane_potential_next = (~membrane_potential_next_ovfl.msb & membrane_potential.msb & io.syn_weight.msb) ?
    S(fireRes bits, fireRes-1 -> true, default -> false) | ((membrane_potential_next_ovfl.msb & ~membrane_potential.msb & ~io.syn_weight.msb) ?
    S(fireRes bits, fireRes-1 -> false, default -> true) | membrane_potential_next_ovfl)

  when (membrane_potential_next >= io.firing_threshold){
    spike_output := True
    membrane_potential := S(0, fireRes bits)
  }
    .otherwise{
      spike_output := False
      membrane_potential := membrane_potential_next
    }

  io.spike_out <> spike_output
}

class IFNeuron (fireRes: Int, synRes: Int) extends Component{
  val iclk, irst = in Bool()
  val IFClockDomain = ClockDomain(iclk, irst, config = ClockDomainConfig(
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = HIGH
  ))
  val ifNeuron = IFClockDomain(new IFNeuron_Comp(fireRes, synRes))
}