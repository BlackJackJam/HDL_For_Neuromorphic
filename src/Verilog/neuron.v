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

//
// Author:  C. Frenkel, Delft University of Technology
//
// Simple integrate-and-fire neuron for Jinhuang Lin's extra 15-ECTS project
//


module neuron  #(
	parameter N = 6,
	parameter S = 4
)(
	input  wire                clk,
	input  wire                rst,

    input  wire signed [N-1:0] firing_threshold,        // neuron firing threshold parameter
    input  wire signed [S-1:0] syn_weight,              // input synaptic weight
    input  wire                spike_in,                // neuron spike event input (the associated synaptic weight is syn_weight)

    output reg                 spike_out                // neuron spike event output
);

    reg  signed [N-1:0] membrane_potential;             // neuron membrane potential

    wire signed [N-1:0] membrane_potential_next;
    wire signed [N-1:0] membrane_potential_next_ovfl;

    wire signed [N-1:0] sign_ext_syn_weight;	// sign extended weight

    always @(posedge clk, posedge rst) begin
    	if (rst) begin
    		membrane_potential <= $signed ('d0);
    		spike_out          <= 1'b0;
    	end else if (membrane_potential_next >= firing_threshold) begin
    		membrane_potential <= $signed ('d0);
    		spike_out          <= 1'b1;
    	end else begin
    		membrane_potential <= membrane_potential_next;
    		spike_out          <= 1'b0;
    	end
    end

    assign sign_ext_syn_weight = spike_in ? $signed({ {(N-S){syn_weight[S-1]}}, syn_weight}) : $signed ('d0);

    assign membrane_potential_next_ovfl = membrane_potential + sign_ext_syn_weight;
    assign membrane_potential_next      =  (~membrane_potential_next_ovfl[N-1] &  membrane_potential[N-1] &  syn_weight[S-1]) // Underflow?
    	                                ? $signed({1'b1,{(N-1){1'b0}}})
    	                                : (( membrane_potential_next_ovfl[N-1] & ~membrane_potential[N-1] & ~syn_weight[S-1]) // Overflow?
    	                                ? $signed({1'b0,{(N-1){1'b1}}})
    	                                : membrane_potential_next_ovfl);


endmodule
