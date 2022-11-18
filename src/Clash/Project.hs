-- Copyright (C) 2022 - Delft University of Technology
-- SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
--
-- Licensed under the Solderpad Hardware License v 2.1 (the “License”); you may not use this file except in compliance
-- with the License, or, at your option, the Apache License version 2.0. You may obtain a copy of the License at
-- https://solderpad.org/licenses/SHL-2.1/
--
-- Unless required by applicable law or agreed to in writing, any work distributed under the License is distributed on
-- an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations under the License.
--

-- Reconfigurable neuron in Clash
-- Author: Jinhuang Lin
-- 
-- This is the reconfigurable neuron written in Clash. To configure the width of threshold and the width of synaptic weight, change the corresponding numbers in the topEntity (e.g. the width of threshold is 6 and the width of synaptic weight is 4).

module Project where

import Clash.Prelude
import Clash.Signal

createDomain vSystem{vName="Domi",vPeriod = 100}

topEntity
  :: Clock Domi
  -> Reset Domi
  -> Signal Domi (Signed 6)
  -> Signal Domi (Signed 4)
  -> Signal Domi (Bool)
  -> Signal Domi (Bool)
topEntity clk asyncRst = exposeClockResetEnable neuron clk asyncRst enableGen

{-# ANN topEntity
  (Synthesize
    { t_name   = "Neuron"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "firing_threshold"
                 , PortName "syn_weight"
                 , PortName "spike_in"
                 ]
    , t_output = PortName "spike_out"
    }) #-}

sign_ext_syn_weight::(KnownNat m, KnownNat n) => Signed m -> Signed n -> Bool -> Signed m
sign_ext_syn_weight m_p syn_w spk | spk == True = resize syn_w
                                  | spk == False = 0

membrane_p_next_ovfl::(KnownNat m, KnownNat n) => Signed m -> Signed n -> Bool -> Signed m
membrane_p_next_ovfl m_p syn_w spk = m_p + sign_ext_syn_weight m_p syn_w spk

negOvflFlag:: (KnownNat m, KnownNat n) => Signed m -> Signed m -> Signed n -> Bool
negOvflFlag add1 add2 add3 = (not$bitToBool$msb$pack add1) && (bitToBool$msb$pack add2) && (bitToBool$msb$pack add3)

posOvflFlag:: (KnownNat m, KnownNat n) => Signed m -> Signed m -> Signed n -> Bool
posOvflFlag add1 add2 add3 = (bitToBool$msb$pack add1) && (not$bitToBool$msb$pack add2) && (not$bitToBool$msb$pack add3)

negOvfl:: (KnownNat m, KnownNat n) => Signed m -> Signed n -> Bool -> Bool
negOvfl membrane_p syn_w spk = negOvflFlag membrane_n_ovfl membrane_p syn_w
  where membrane_n_ovfl = membrane_p_next_ovfl membrane_p syn_w spk

posOvfl:: (KnownNat m, KnownNat n) => Signed m -> Signed n -> Bool -> Bool
posOvfl membrane_p syn_w spk = posOvflFlag membrane_n_ovfl membrane_p syn_w
  where membrane_n_ovfl = membrane_p_next_ovfl membrane_p syn_w spk

membrane_p_next:: (KnownNat m, KnownNat n) => Signed m -> Signed n -> Bool -> Signed m
membrane_p_next m_p syn_w spk | negOvfl m_p syn_w spk == True = minBound
                              | posOvfl m_p syn_w spk == True = maxBound
                              | otherwise = membrane_p_next_ovfl m_p syn_w spk


membrane_potential:: (KnownNat m, KnownNat n, HiddenClockResetEnable dom) => Signal dom (Signed m) -> Signal dom (Signed n) -> Signal dom Bool -> Signal dom (Signed m)
membrane_potential fir_th syn_w spk = register 0 next
  where
    next = mux ((.>=.) m_p_n fir_th) 0 m_p_n
      where
        m_p_n = membrane_p_next<$>(membrane_potential fir_th syn_w spk)<*>syn_w<*>spk

neuron::(KnownNat m, KnownNat n, HiddenClockResetEnable dom) => Signal dom (Signed m) -> Signal dom (Signed n) -> Signal dom Bool -> Signal dom Bool
neuron firing_th syn_w spike_in = register False ((.>=.) membrane_p_n firing_th)
  where membrane_p_n = membrane_p_next<$>(membrane_potential firing_th syn_w spike_in)<*>syn_w<*>spike_in
