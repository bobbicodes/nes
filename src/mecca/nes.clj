(ns mecca.nes
  (:require [clojure.java.io :as io]))

(def opcodes
  (into {}
        (for [[code instruction address-mode bytes cycles]
              [[0x0b :aac :immediate 2 2]
               [0x2b :aac :immediate 2 2]
               [0x87 :aax :zero 2 3]
               [0x97 :aax :zero-y 2 4]
               [0x83 :aax :indirect-x 2 6]
               [0x8f :aax :absolute 3 4]
               [0x69 :adc :immediate 2 2]
               [0x65 :adc :zero 2 3]
               [0x75 :adc :zero-x 2 4]
               [0x6D :adc :absolute 3 4]
               [0x7D :adc :absolute-x 3 4]
               [0x79 :adc :absolute-y 3 4]
               [0x61 :adc :indirect-x 2 6]
               [0x71 :adc :indirect-y 2 5]
               [0x29 :and :immediate 2 2]
               [0x25 :and :zero 2 3]
               [0x35 :and :zero-x 2 4]
               [0x2D :and :absolute 3 4]
               [0x3D :and :absolute-x 3 4]
               [0x39 :and :absolute-y 3 4]
               [0x21 :and :indirect-x 2 6]
               [0x31 :and :indirect-y 2 5]
               [0x0A :asl :accumulator 1 2]
               [0x06 :asl :zero 2 5]
               [0x16 :asl :zero-x 2 6]
               [0x0E :asl :absolute 3 6]
               [0x1E :asl :absolute-x 3 7]
               [0x4b :asr :immediate 2 2]
               [0xab :atx :immediate 2 2]
               [0x9f :axa :absolute-y 3 5]
               [0x93 :axa :indirect-y 2 6]
               [0x90 :bcc :relative 2 2]
               [0xB0 :bcs :relative 2 2]
               [0xF0 :beq :relative 2 2]
               [0x24 :bit :zero 2 3]
               [0x2C :bit :absolute 3 4]
               [0x30 :bmi :relative 2 2]
               [0xD0 :bne :relative 2 2]
               [0x10 :bpl :relative 2 2]
               [0x00 :brk :implied 1 7]
               [0x50 :bvc :relative 2 2]
               [0x70 :bvs :relative 2 2]
               [0x18 :clc :implied 1 2]
               [0xD8 :cld :implied 1 2]
               [0x58 :cli :implied 1 2]
               [0xB8 :clv :implied 1 2]
               [0xC9 :cmp :immediate 2 2]
               [0xC5 :cmp :zero 2 3]
               [0xD5 :cmp :zero-x 2 4]
               [0xCD :cmp :absolute 3 4]
               [0xDD :cmp :absolute-x 3 4]
               [0xD9 :cmp :absolute-y 3 4]
               [0xC1 :cmp :indirect-x 2 6]
               [0xD1 :cmp :indirect-y 2 5]
               [0xE0 :cpx :immediate 2 2]
               [0xE4 :cpx :zero 2 3]
               [0xEC :cpx :absolute 3 4]
               [0xC0 :cpy :immediate 2 2]
               [0xC4 :cpy :zero 2 3]
               [0xCC :cpy :absolute 3 4]
               [0xc7 :dcp :zero 2 5]
               [0xd7 :dcp :zero-x 2 6]
               [0xcf :dcp :absolute 3 6]
               [0xdf :dcp :absolute-x 3 7]
               [0xdb :dcp :absolute-y 3 7]
               [0xc3 :dcp :indirect-x 2 8]
               [0xd3 :dcp :indirect-y 2 8]
               [0xC6 :dec :zero 2 5]
               [0xD6 :dec :zero-x 2 6]
               [0xCE :dec :absolute 3 6]
               [0xDE :dec :absolute-x 3 7]
               [0xCA :dex :implied 1 2]
               [0x88 :dey :implied 1 2]
               [0x49 :eor :immediate 2 2]
               [0x45 :eor :zero 2 3]
               [0x55 :eor :zero-x 2 4]
               [0x4D :eor :absolute 3 4]
               [0x5D :eor :absolute-x 3 4]
               [0x59 :eor :absolute-y 3 4]
               [0x41 :eor :indirect-x 2 6]
               [0x51 :eor :indirect-y 2 5]
               [0xE6 :inc :zero 2 5]
               [0xF6 :inc :zero-x 2 6]
               [0xEE :inc :absolute 3 6]
               [0xFE :inc :absolute-x 3 7]
               [0xE8 :inx :implied 1 2]
               [0xC8 :iny :implied 1 2]
               [0xe7 :isb :zero 2 5]
               [0xf7 :isb :zero-x 2 6]
               [0xef :isb :absolute 3 6]
               [0xff :isb :absolute-x 3 7]
               [0xfb :isb :absolute-y 3 7]
               [0xe3 :isb :indirect-x 2 8]
               [0xf3 :isb :indirect-y 2 8]
               [0x4C :jmp :absolute 3 3]
               [0x6C :jmp :indirect 3 5]
               [0x20 :jsr :absolute 3 6]
               [0x02 :kil :implied 1 0]
               [0x12 :kil :implied 1 0]
               [0x22 :kil :implied 1 0]
               [0x32 :kil :implied 1 0]
               [0x42 :kil :implied 1 0]
               [0x52 :kil :implied 1 0]
               [0x62 :kil :implied 1 0]
               [0x72 :kil :implied 1 0]
               [0x92 :kil :implied 1 0]
               [0xb2 :kil :implied 1 0]
               [0xd2 :kil :implied 1 0]
               [0xf2 :kil :implied 1 0]
               [0xbb :lar :absolute-y 3 4]
               [0xa7 :lax :zero 2 3]
               [0xb7 :lax :zero-y 2 4]
               [0xaf :lax :absolute 3 4]
               [0xbf :lax :absolute-y 3 4]
               [0xa3 :lax :indirect-x 2 6]
               [0xb3 :lax :indirect-y 2 5]
               [0xA9 :lda :immediate 2 2]
               [0xA5 :lda :zero 2 3]
               [0xB5 :lda :zero-x 2 4]
               [0xAD :lda :absolute 3 4]
               [0xBD :lda :absolute-x 3 4]
               [0xB9 :lda :absolute-y 3 4]
               [0xA1 :lda :indirect-x 2 6]
               [0xB1 :lda :indirect-y 2 5]
               [0xA2 :ldx :immediate 2 2]
               [0xA6 :ldx :zero 2 3]
               [0xB6 :ldx :zero-y 2 4]
               [0xAE :ldx :absolute 3 4]
               [0xBE :ldx :absolute-y 3 4]
               [0xA0 :ldy :immediate 2 2]
               [0xA4 :ldy :zero 2 3]
               [0xB4 :ldy :zero-x 2 4]
               [0xAC :ldy :absolute 3 4]
               [0xBC :ldy :absolute-x 3 4]
               [0x4A :lsr :accumulator 1 2]
               [0x46 :lsr :zero 2 5]
               [0x56 :lsr :zero-x 2 6]
               [0x4E :lsr :absolute 3 6]
               [0x5E :lsr :absolute-x 3 7]
               [0x1a :nop :implied 1 2]
               [0x3a :nop :implied 1 2]
               [0x5a :nop :implied 1 2]
               [0x7a :nop :implied 1 2]
               [0xda :nop :implied 1 2]
               [0xEA :nop :implied 1 2]
               [0xfA :nop :implied 1 2]
               [0x5A :nop :implied 1 2]
               [0x04 :nop :zero 2 3]
               [0x14 :nop :zero-x 2 4]
               [0x34 :nop :zero-x 2 4]
               [0x44 :nop :zero 2 3]
               [0x54 :nop :zero-x 2 4]
               [0x64 :nop :zero 2 3]
               [0x74 :nop :zero-x 2 4]
               [0x80 :nop :immediate 2 2]
               [0x82 :nop :immediate 2 2]
               [0x89 :nop :immediate 2 2]
               [0xc2 :nop :immediate 2 2]
               [0xd4 :nop :zero-x 2 4]
               [0xe2 :nop :immediate 2 2]
               [0xf4 :nop :zero-x 2 4]
               [0x0c :nop :absolute 3 4]
               [0x1c :nop :absolute-x 3 4]
               [0x3c :nop :absolute-x 3 4]
               [0x5c :nop :absolute-x 3 4]
               [0x7c :nop :absolute-x 3 4]
               [0xdc :nop :absolute-x 3 4]
               [0xfc :nop :absolute-x 3 4]
               [0x09 :ora :immediate 2 2]
               [0x05 :ora :zero 2 3]
               [0x15 :ora :zero-x 2 4]
               [0x0D :ora :absolute 3 4]
               [0x1D :ora :absolute-x 3 4]
               [0x19 :ora :absolute-y 3 4]
               [0x01 :ora :indirect-x 2 6]
               [0x11 :ora :indirect-y 2 5]
               [0x48 :pha :implied 1 3]
               [0x08 :php :implied 1 3]
               [0x68 :pla :implied 1 4]
               [0x28 :plp :implied 1 4]
               [0x27 :rla :zero 2 5]
               [0x37 :rla :zero-x 2 6]
               [0x2f :rla :absolute 3 6]
               [0x3f :rla :absolute-x 3 7]
               [0x3b :rla :absolute-y 3 7]
               [0x23 :rla :indirect-x 2 8]
               [0x33 :rla :indirect-y 2 8]
               [0x2A :rol :accumulator 1 2]
               [0x26 :rol :zero 2 5]
               [0x36 :rol :zero-x 2 6]
               [0x2E :rol :absolute 3 6]
               [0x3E :rol :absolute-x 3 7]
               [0x6A :ror :accumulator 1 2]
               [0x66 :ror :zero 2 5]
               [0x76 :ror :zero-x 2 6]
               [0x6E :ror :absolute 3 6]
               [0x7E :ror :absolute-x 3 7]
               [0x67 :rra :zero 2 5]
               [0x77 :rra :zero-x 2 6]
               [0x6f :rra :absolute 3 6]
               [0x7f :rra :absolute-x 3 7]
               [0x7b :rra :absolute-y 3 7]
               [0x63 :rra :indirect-x 2 8]
               [0x73 :rra :indirect-y 2 8]
               [0x40 :rti :implied 1 6]
               [0x60 :rts :implied 1 6]
               [0x87 :sax :zero 2 3]
               [0x8f :sax :absolute 3 4]
               [0xE9 :sbc :immediate 2 2]
               [0xEb :sbc :immediate 2 2]
               [0xE5 :sbc :zero 2 3]
               [0xF5 :sbc :zero-x 2 4]
               [0xED :sbc :absolute 3 4]
               [0xFD :sbc :absolute-x 3 4]
               [0xF9 :sbc :absolute-y 3 4]
               [0xE1 :sbc :indirect-x 2 6]
               [0xF1 :sbc :indirect-y 2 5]
               [0x38 :sec :implied 1 2]
               [0xF8 :sed :implied 1 2]
               [0x78 :sei :implied 1 2]
               [0x03 :slo :indirect-x 2 8]
               [0x07 :slo :zero 2 5]
               [0x17 :slo :zero-x 2 6]
               [0x0f :slo :absolute 3 6]
               [0x1f :slo :absolute-x 3 7]
               [0x1b :slo :absolute-y 3 7]
               [0x03 :slo :indirect-x 2 8]
               [0x13 :slo :indirect-y 2 8]
               [0x47 :sre :zero 2 5]
               [0x57 :sre :zero-x 2 6]
               [0x4f :sre :absolute 3 6]
               [0x5f :sre :absolute-x 3 7]
               [0x5b :sre :absolute-y 3 7]
               [0x43 :sre :indirect-x 2 8]
               [0x53 :sre :indirect-y 2 8]
               [0x57 :sre :zero 2 6]
               [0x85 :sta :zero 2 3]
               [0x95 :sta :zero-x 2 4]
               [0x8D :sta :absolute 3 4]
               [0x9D :sta :absolute-x 3 5]
               [0x99 :sta :absolute-y 3 5]
               [0x81 :sta :indirect-x 2 6]
               [0x91 :sta :indirect-y 2 6]
               [0x86 :stx :zero 2 3]
               [0x96 :stx :zero-y 2 4]
               [0x8E :stx :absolute 3 4]
               [0x84 :sty :zero 2 3]
               [0x94 :sty :zero-x 2 4]
               [0x8C :sty :absolute 3 4]
               [0x9e :sxa :absolute-y 3 5]
               [0xAA :tax :implied 1 2]
               [0xA8 :tay :implied 1 2]
               [0xBA :tsx :implied 1 2]
               [0x8A :txa :implied 1 2]
               [0x9A :txs :implied 1 2]
               [0x98 :tya :implied 1 2]
               [0x8b :xaa :immediate 2 2]
               [0x9b :xas :absolute-y 3 5]]]
          [code {:instruction  instruction
                 :address-mode address-mode
                 :bytes        bytes
                 :cycles       cycles}])))

(defn file->bytes [file]
  (with-open [xin  (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defn bytes->hex [bytes]
  (apply str (for [b bytes] (format "%02x" b))))

(defn file->hex [file]
  (bytes->hex (file->bytes file)))

(defn get-byte [file n]
  (Integer/decode
   (str "0x" (apply str (first (take 1 (drop n (partition 2 (file->hex (io/file file))))))))))

(defn opcode [code]
  (get opcodes (Integer/decode (str "0x" code))))

(defn absolute-address
  "Retrieves the value at specified address in iNES file,
   ignoring 16-byte header."
  [file address]
  (get-byte file (- address 16)))

(defn relative-address [n]
  (if (< 127 (Integer/decode (str "0x" n)))
    (- (- 255 (Integer/decode (str "0x" n))))
    (Integer/decode (str "0x" n))))

(defn disassemble
  "Takes a string of hex bytes representing 6502 machine code,
   outputs assembly code as EDN data."
  [code]
    (loop [code         code
           instructions []]
      (cond
        (empty? code) instructions
        (and
         (= 1 (:bytes (opcode (subs code 0 2))))
         (= :accumulator (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 2)
               (conj instructions [(:instruction (opcode (subs code 0 2))) :a]))
        (= 1 (:bytes (opcode (subs code 0 2))))
        (recur (subs code 2)
               (conj instructions (:instruction (opcode (subs code 0 2)))))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :immediate (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "#$" (subs code 2 4))]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :relative (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (relative-address (subs code 2 4))]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :indirect-x (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "($" (subs code 2 4) ",x)")]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :indirect-y (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "($" (subs code 2 4) "),y")]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :zero (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 2 4))]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :zero-x (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 2 4) ",x")]))
        (and
         (= 2 (:bytes (opcode (subs code 0 2))))
         (= :zero-y (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 4)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 2 4) ",y")]))
        (and
         (= 3 (:bytes (opcode (subs code 0 2))))
         (= :absolute (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 6)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 4 6) (subs code 2 4))]))
        (and
         (= 3 (:bytes (opcode (subs code 0 2))))
         (= :absolute-x (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 6)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 4 6) (subs code 2 4) ",x")]))
        (and
         (= 3 (:bytes (opcode (subs code 0 2))))
         (= :absolute-y (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 6)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "$" (subs code 4 6) (subs code 2 4) ",y")]))
        (and
         (= 3 (:bytes (opcode (subs code 0 2))))
         (= :indirect (:address-mode (opcode (subs code 0 2)))))
        (recur (subs code 6)
               (conj instructions [(:instruction (opcode (subs code 0 2)))
                                   (str "($" (subs code 4 6) (subs code 2 4) ")")])))))

(comment
  
(file->hex "resources/smb.nsf")
  (subs (file->hex "resources/smb.nsf") 31870 31878)
  
  (opcode "6c")
          
  (disassemble (subs (file->hex "resources/smb.nsf") 30000 31872))

)