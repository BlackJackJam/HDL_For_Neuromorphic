'''
Copyright (C) 2022 - Delft University of Technology
 SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1

 Licensed under the Solderpad Hardware License v 2.1 (the “License”); you may not use this file except in compliance
 with the License, or, at your option, the Apache License version 2.0. You may obtain a copy of the License at
 https://solderpad.org/licenses/SHL-2.1/

 Unless required by applicable law or agreed to in writing, any work distributed under the License is distributed on
 an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 specific language governing permissions and limitations under the License.

 Reconfigurable neuron in MyHDL
 Author: Jinhuang Lin
 This is the reconfigurable neuron written in MyHDL. To configure the width of threshold and synaptic weight, specify these widths in the conversion function.
'''
from myhdl import block, Signal, intbv, always_comb, always_seq, instances, ResetSignal


@block
def neuron(spike_in, firing_threshold, syn_weight, clock, reset, spike_out, fir_res=16, syn_res=8):
        m = fir_res
        n = syn_res
        membrane_potential = Signal(intbv(0, min=-2 ** (m - 1), max=2 ** (m - 1)))
        membrane_potential_next = Signal(intbv(0, min=-2 ** (m - 1), max=2 ** (m - 1)))
        membrane_potential_ovfl_next = Signal(intbv(0, min=-2 ** (m - 1), max=2 ** (m - 1)))
        sign_ext_syn_weight = Signal(intbv(0, min=-2 ** (m - 1), max=2 ** (m - 1)))

        @always_seq(clock.posedge, reset)
        def spike_result():
            if (membrane_potential_next >= firing_threshold):
                membrane_potential.next = 0
                spike_out.next = True
            else:
                membrane_potential.next = membrane_potential_next
                spike_out.next = False

        @always_comb
        def sign_ext():
            if spike_in:
                sign_ext_syn_weight.next = syn_weight.signed()
            else:
                sign_ext_syn_weight.next = 0
        
        @always_comb
        def potential_next():
            membrane_potential_ovfl_next.next = membrane_potential + sign_ext_syn_weight
        
        @always_comb
        def spike_comb():
            if ((not membrane_potential_ovfl_next[m - 1]) and membrane_potential[m - 1] and syn_weight[n - 1]):
                membrane_potential_next.next = - 2 ** (m - 1)
            elif (membrane_potential_ovfl_next[m - 1] and (not membrane_potential[m - 1]) and (not syn_weight[n - 1])):
                membrane_potential_next.next = 2 ** (m - 1)-1
            else:
                membrane_potential_next.next = membrane_potential_ovfl_next
        return instances()

'''Conversion function: this function will directly generate a Verilog file.'''
def convert_inc(hdl, fir_res_arg=16, syn_res_arg=8):
    _fir_res_ = fir_res_arg
    _syn_res_ = syn_res_arg
    spike_in, clock, spike_out = [Signal(bool(0)) for __ in range (3)]
    reset = ResetSignal(0, 1, isasync=True)
    firing_threshold = Signal(intbv(0, min=-2 ** (_fir_res_ - 1), max=2 ** (_fir_res_ - 1)))
    syn_weight = Signal(intbv(0, min=-2 ** (_syn_res_ - 1), max=2 ** (_syn_res_ - 1)))

    convInst = neuron(spike_in, firing_threshold, syn_weight, clock, reset, spike_out, fir_res=_fir_res_, syn_res=_syn_res_)
    convInst.convert(hdl=hdl)


convert_inc(hdl='Verilog', fir_res_arg=6, syn_res_arg=4)
convert_inc(hdl='VHDL', fir_res_arg=6, syn_res_arg=4)