// Copyright (C) 2022 - Delft University of Technology
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Licensed under the Solderpad Hardware License v 2.1 (the “License”); you may not use this file except in compliance
// with the License, or, at your option, the Apache License version 2.0. You may obtain a copy of the License at
// https://solderpad.org/licenses/SHL-2.1/
//
// Unless required by applicable law or agreed to in writing, any work distributed under the License is distributed on
// an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

// Reconfigurable neuron in SpinalHDL
// Author: Jinhuang Lin
// This is the reconfigurable neuron written in SpinalHDL. To configure the width of threshold and synaptic weight, specify the corresponding widths when instancing the module.
// This file is a scala class, use a main object to instancing a neuron.

import spinal.core._

class IFNeuron_Comp (fireRes: Int, synRes: Int) extends Component{
  val io = new Bundle{
    val firing_threshold  = in SInt(fireRes bits)
    val syn_weight        = in SInt(synRes bits)
    val spike_in          = in Bool()
    val spike_out         = out Bool()
  }

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