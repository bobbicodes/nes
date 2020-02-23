# nes

Clojure NES disassembler

## Usage

This library reads iNES files consisting of binary data representing 6502 machine code and emits assembly code as EDN data:

```clojure
(disassemble (subs (file->hex "resources/metroid.nsf") 256 512))
[[:nop "#$b4"]
 [:nop "$74"]
 [:ror :a]
 :kil
 [:nop "$78"]
 [:nop "$02,x"]
 [:isb "$b2c2,x"]
 :kil
 :nop
 [:ror "$6c56"]
 [:nop "$68,x"]
 [:bvc 110]
 [:lsr "$6c,x"]
 [:nop "$68,x"]
 [:bvc 100]
 [:jmp "$c4ff"]
 :kil
 :nop
 [:ror "$6c5a"]
 :nop
 :pla
 :nop
 [:ror "$6c56"]
 [:lsr "$68,x"]
 [:lsr "$64,x"]
 [:lsr "$ff,x"]
 :kil
 :nop
 [:lda "($42),y"]
 :kil
 [:lsr "$b1,x"]
 :kil
 :kil
 [:nop "$b1,x"]
 :kil
 :kil
 [:bvc -78]
 :kil
 :kil
 :nop
 [:lda "($42),y"]
 :kil
 [:lsr "$b1,x"]
 :kil
 :kil
 :kil
 [:lda "($42),y"]
 :kil
 [:bvc -78]
 :kil
 :kil
 :nop
 [:lda "($44),y"]
 :kil
 [:lsr "$b1,x"]
 [:nop "$b2"]
 :kil
 [:lda "($44),y"]
 :kil
 [:lsr "$b1,x"]
 [:nop "$c4"]
 :nop
 [:bvc 70]
 [:isb "$58c3,x"]
 [:bvc 70]
 [:isb "$5058,x"]
 [:bcs 70]
 :kil
 [:cpx "#$b6"]
 [:nop "$02b2,x"]
 [:isb "$d000,x"]
 [:ldx "$2a,y"]
 [:lda "($2a),y"]
 [:lda "($02),y"]
 [:isb "$4cb4,x"]]
```

## License

Copyright © 2020 Bobby Towers

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
