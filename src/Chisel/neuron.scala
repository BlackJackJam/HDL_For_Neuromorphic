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

// Reconfigurable neuron in Chisel
// Author: Jinhuang Lin
// This is the reconfigurable neuron written in Chisel. To configure the width of threshold and synaptic weight, specify "N" and "S" when instancing this module.
// This file is a scala class, use a main object to instancing a neuron.

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

  membrane_potential_next_ovfl := neuronReg.membrane_potential + sign_ext_syn_weight
  val posOvflValue = Cat(0.U(1.W),Fill((fireRes-1),1.U(1.W))).asSInt
  val negOvflValue = (~posOvflValue).asSInt
  val negOvflFlag = (!membrane_potential_next_ovfl(fireRes-1))&neuronReg.membrane_potential(fireRes-1)&io.syn_weight(synRes-1)
  val posOvflFlag = membrane_potential_next_ovfl(fireRes-1)&(!neuronReg.membrane_potential(fireRes-1))&(!io.syn_weight(synRes-1))
  val membrane_potential_next = MuxCase(membrane_potential_next_ovfl, Array( negOvflFlag -> negOvflValue, posOvflFlag -> posOvflValue))
  
  when(membrane_potential_next.asSInt >= io.firing_threshold){
    neuronReg.membrane_potential := 0.S
    neuronReg.spike_out := true.B
  } .otherwise{
    neuronReg.membrane_potential := membrane_potential_next
    neuronReg.spike_out := false.B
  }
  io.spike_out := neuronReg.spike_out
}
