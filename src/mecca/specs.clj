(ns mecca.specs
  (:require [clojure.spec.alpha :as s]))

(s/def :opcode/code (s/int-in 0x00 0xFF))

(s/def :opcode/instruction
  #{:adc :and :asl :bcc :bcs :beq :bit :bmi :bne :bpl :brk
    :bvc :bvs :clc :cld :cli :clv :cmp :cpx :cpy :dec :dex
    :dey :eor :inc :inx :iny :jmp :jsr :lda :ldx :ldy :lsr
    :nop :ora :pha :php :pla :plp :rol :ror :rti :rts :sbc
    :sec :sed :sei :sta :stx :sty :tax :tay :tsx :txa :txs
    :tya})

(s/def :opcode/address-mode
  #{:immediate :zero :zero-x :zero-y
    :absolute :absolute-x :absolute-y
    :indirect :indirect-x :indirect-y
    :relative :implied :accumulator})

(s/def :opcode/bytes (s/int-in 1 4))

(s/def :opcode/cycles (s/int-in 1 8))

(s/def :opcode/op
  (s/keys :req-un
          [:opcode/instruction
           :opcode/address-mode :opcode/bytes
           :opcode/cycles]))

(s/def :opcode/opcodes (s/map-of :opcode/code :opcode/op))