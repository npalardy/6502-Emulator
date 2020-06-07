#tag Window
Begin Window Window1
   BackColor       =   &cFFFFFF00
   Backdrop        =   0
   CloseButton     =   True
   Compatibility   =   ""
   Composite       =   False
   Frame           =   0
   FullScreen      =   False
   FullScreenButton=   False
   HasBackColor    =   False
   Height          =   400
   ImplicitInstance=   True
   LiveResize      =   True
   MacProcID       =   0
   MaxHeight       =   32000
   MaximizeButton  =   True
   MaxWidth        =   32000
   MenuBar         =   706566143
   MenuBarVisible  =   True
   MinHeight       =   64
   MinimizeButton  =   True
   MinWidth        =   64
   Placement       =   0
   Resizeable      =   True
   Title           =   "Untitled"
   Visible         =   True
   Width           =   600
End
#tag EndWindow

#tag WindowCode
	#tag Event
		Sub Open()
		  // ; TOLOWER:
		  // ;
		  // ;   Convert a null-terminated character String To all lower Case.
		  // ;   Maximum String length Is 255 characters, plus the null term-
		  // ;   inator.
		  // ;
		  // ; Parameters:
		  // ;
		  // ;   SRC - Source String address
		  // ;   DST - Destination String address
		  // ;
		  // 
		  // 0080         // ORG $0080
		  // 
		  // 0080  00 04 // SRC     .WORD $0400     ;source String pointer ($40)
		  // 0082  00 05 // DST     .WORD $0500     ;destination String pointer ($42)
		  // 
		  // 0600
		  // 
		  // 0600  A0 00 // TOLOWER LDY #$00        ;starting index
		  //             
		  // 0602  B1 80 // Loop    LDA (SRC),Y     ;get from source String
		  // 0604  F0 11 // BEQ DONE        ;end Of String
		  // 
		  // 0606  C9 41 // CMP #'A'        ;if lower than UC alphabet...
		  // 0608  90 06 // BCC SKIP        ;copy unchanged
		  // 
		  // 060A  C9 5B // CMP #'Z'+1      ;if greater than UC alphabet...
		  // 060C  B0 02 // BCS SKIP        ;copy unchanged
		  // 
		  // 060E  09 20 // ORA #%00100000  ;convert To lower Case
		  // 
		  // 0610  91 82 // SKIP    STA (DST),Y     ;store To destination String
		  // 0612  C8    // INY             ;bump index
		  // 0613  D0 ED // BNE Loop        ;next character
		  // 
		  // ; NOTE: If Y wraps the destination String will be Left In an undefined
		  // ;  state.  We set carry To indicate this To the calling Function.
		  // ;
		  // 
		  // 0615  38    // SEC             ;report String too long error &...
		  // 0616  60    // RTS             ;return To caller
		  // 
		  // 0617  91 82 // DONE    STA (DST),Y     ;terminate destination String
		  // 0618  18    // CLC             ;report conversion completed &...
		  // 0619  60    // RTS             ;return To caller
		  // 
		  // 061A
		  
		  
		  Dim c As New MOS6502
		  
		  // // note THIS example WILL bork becuase we JUMP into something thats _should_ be called by a JSR !
		  // c.bytecode(0) = ChrB(&h4C) // jmp $600
		  // c.bytecode(1) = ChrB(&h00)
		  // c.bytecode(2) = ChrB(&h06)
		  
		  c.bytecode(0) = ChrB(&h20) // jsr $600
		  c.bytecode(1) = ChrB(&h00)
		  c.bytecode(2) = ChrB(&h06)
		  
		  c.bytecode(3) = ChrB(&h00) // break !
		  
		  #If False
		    // pack data around our string and the result to see we do not get overruns !
		    // string IS nil terminated and we do copy the terminator
		    c.bytecode(&h20) = "aBcDeFgHi"+ChrB(0) // SRC
		    c.bytecode(&h50-10) = "0123456789"
		    c.bytecode(&h50) = "          0123456789" // DST
		    
		    c.bytecode(&h80) = ChrB(&h20) // $80 points to $0020 (little endian it ends up being input as $2000)
		    c.bytecode(&h81) = ChrB(&h0)
		    c.bytecode(&h82) = ChrB(&h50) // $82 points to $0050 (little endian it ends up being input as $5000)
		    c.bytecode(&h83) = ChrB(&h00)
		    
		    
		    c.bytecode(&h600) = ChrB(&hA0) + ChrB(&h00) + _
		    ChrB(&hB1) + ChrB(&h80) + _
		    ChrB(&hF0) + ChrB(&h11) + _
		    ChrB(&hC9) + ChrB(&h41) + _
		    ChrB(&h90) + ChrB(&h06) + _
		    ChrB(&hC9) + ChrB(&h5B) + _
		    ChrB(&hB0) + ChrB(&h02) + _
		    ChrB(&h09) + ChrB(&h20) + _
		    ChrB(&h91) + ChrB(&h82) + _
		    ChrB(&hC8) + _
		    ChrB(&hD0) + ChrB(&hED) + _
		    ChrB(&h38) + _
		    ChrB(&h60) + _
		    ChrB(&h91) + ChrB(&h82) + _
		    ChrB(&h18) + _
		    ChrB(&h60)
		    
		    c.run(&h600)
		  #EndIf
		  
		  #If False
		    c.bytecode(&h600) = ChrB(&ha2) + ChrB(&h01) + _  //LDX #$01
		    ChrB(&ha9) + ChrB(&h05) + _ // LDA #$05
		    ChrB(&h85) + ChrB(&h01) + _ // STA $01
		    ChrB(&ha9) + ChrB(&h07) + _ // LDA #$07
		    ChrB(&h85) + ChrB(&h02) + _ // STA $02
		    ChrB(&ha0) + ChrB(&h0a) + _ // LDY #$0a
		    ChrB(&h8c) + ChrB(&h05) + ChrB(&h07) + _ // STY $0705
		    ChrB(&ha1) + ChrB(&h00)  // LDA ($00,X)
		    
		    c.run(&h600)
		    
		    Break
		    
		    // a=&h0a x=&h01 y=&h0a SP= &f pc-&h0612 flags = &b0110000 ?
		    If c.a <> &h0a Then
		      Break
		    End If
		    If c.x <> &h01 Then
		      Break
		    End If
		    If c.y <> &h0a Then
		      Break
		    End If
		    If c.sp <> &hff Then
		      Break
		    End If
		    If c.pc <> &h0612 Then
		      Break
		    End If
		    If c.status <> &b00110000 Then
		      Break
		    End If
		    
		    
		  #EndIf
		  
		  
		  #If false
		    c.bytecode(&h600) = ChrB(&ha0) + ChrB(&h01) + _ //     LDY #$01
		    ChrB(&ha9 ) + ChrB(&h03) + _ //    LDA #$03
		    ChrB(&h85 ) + ChrB(&h01) + _ //    STA $01
		    ChrB(&ha9 ) + ChrB(&h07) + _ //   LDA #$07
		    ChrB(&h85 ) + ChrB(&h02) + _ //   STA $02
		    ChrB(&ha2 ) + ChrB(&h0a) + _ //   LDX #$0a
		    ChrB(&h8e ) + ChrB(&h04) + ChrB(&h07) + _ //  STX $0704
		    ChrB(&hb1 ) + ChrB(&h01)  //   LDA ($01),Y
		    
		    c.run(&h600)
		    
		    Break
		    
		    If c.A <> &h0a  Then
		      Break
		    End If
		    If c.X <> &h0a  Then
		      Break
		    End If
		    If c.Y <> &h01 Then
		      Break
		    End If
		    If c.SP <> &hff  Then
		      Break
		    End If
		    If c.PC <> &h0612 Then
		      Break
		    End If
		    If c.status <> &b00110000 Then
		      Break
		    End If
		    
		    
		  #EndIf
		  
		  #If false
		    // http://6502.org/source/general/SWN.html
		    // Address  Hexdump   Dissassembly
		    // -------------------------------
		    // $0600    0a        ASL A
		    // $0601    69 80     ADC #$80
		    // $0603    2a        ROL A
		    // $0604    0a        ASL A
		    // $0605    69 80     ADC #$80
		    // $0607    2a        ROL A
		    
		    c.bytecode(&h600) = ChrB(&h0a) + _ //       ASL A
		    ChrB(&h69) + ChrB(&h80) + _ // ADC #$80
		    ChrB(&h2a) + _ // ROL A
		    ChrB(&h0a) + _ // ASL A
		    ChrB(&h69) + ChrB(&h80) + _ // ADC #$80
		    ChrB(&h2a)  // ROL A
		    
		    c.a = &hAf
		    
		    c.run(&h600)
		    
		    Break
		    If c.a <> &hFa Then
		      Break
		    End If
		    
		  #EndIf
		  
		  #If False
		    // snake from http://skilldrick.github.io/easy6502/#snake
		    // Address  Hexdump   Dissassembly
		    // -------------------------------
		    // $0600    20 06 06  JSR $0606
		    // $0603    20 38 06  JSR $0638
		    // $0606    20 0d 06  JSR $060d
		    // $0609    20 2a 06  JSR $062a
		    // $060c    60        RTS 
		    // $060d    a9 02     LDA #$02
		    // $060f    85 02     STA $02
		    // $0611    a9 04     LDA #$04
		    // $0613    85 03     STA $03
		    // $0615    a9 11     LDA #$11
		    // $0617    85 10     STA $10
		    // $0619    a9 10     LDA #$10
		    // $061b    85 12     STA $12
		    // $061d    a9 0f     LDA #$0f
		    // $061f    85 14     STA $14
		    // $0621    a9 04     LDA #$04
		    // $0623    85 11     STA $11
		    // $0625    85 13     STA $13
		    // $0627    85 15     STA $15
		    // $0629    60        RTS 
		    // $062a    a5 fe     LDA $fe
		    // $062c    85 00     STA $00
		    // $062e    a5 fe     LDA $fe
		    // $0630    29 03     AND #$03
		    // $0632    18        CLC 
		    // $0633    69 02     ADC #$02
		    // $0635    85 01     STA $01
		    // $0637    60        RTS 
		    // $0638    20 4d 06  JSR $064d
		    // $063b    20 8d 06  JSR $068d
		    // $063e    20 c3 06  JSR $06c3
		    // $0641    20 19 07  JSR $0719
		    // $0644    20 20 07  JSR $0720
		    // $0647    20 2d 07  JSR $072d
		    // $064a    4c 38 06  JMP $0638
		    // $064d    a5 ff     LDA $ff
		    // $064f    c9 77     CMP #$77
		    // $0651    f0 0d     BEQ $0660
		    // $0653    c9 64     CMP #$64
		    // $0655    f0 14     BEQ $066b
		    // $0657    c9 73     CMP #$73
		    // $0659    f0 1b     BEQ $0676
		    // $065b    c9 61     CMP #$61
		    // $065d    f0 22     BEQ $0681
		    // $065f    60        RTS 
		    // $0660    a9 04     LDA #$04
		    // $0662    24 02     BIT $02
		    // $0664    d0 26     BNE $068c
		    // $0666    a9 01     LDA #$01
		    // $0668    85 02     STA $02
		    // $066a    60        RTS 
		    // $066b    a9 08     LDA #$08
		    // $066d    24 02     BIT $02
		    // $066f    d0 1b     BNE $068c
		    // $0671    a9 02     LDA #$02
		    // $0673    85 02     STA $02
		    // $0675    60        RTS 
		    // $0676    a9 01     LDA #$01
		    // $0678    24 02     BIT $02
		    // $067a    d0 10     BNE $068c
		    // $067c    a9 04     LDA #$04
		    // $067e    85 02     STA $02
		    // $0680    60        RTS 
		    // $0681    a9 02     LDA #$02
		    // $0683    24 02     BIT $02
		    // $0685    d0 05     BNE $068c
		    // $0687    a9 08     LDA #$08
		    // $0689    85 02     STA $02
		    // $068b    60        RTS 
		    // $068c    60        RTS 
		    // $068d    20 94 06  JSR $0694
		    // $0690    20 a8 06  JSR $06a8
		    // $0693    60        RTS 
		    // $0694    a5 00     LDA $00
		    // $0696    c5 10     CMP $10
		    // $0698    d0 0d     BNE $06a7
		    // $069a    a5 01     LDA $01
		    // $069c    c5 11     CMP $11
		    // $069e    d0 07     BNE $06a7
		    // $06a0    e6 03     INC $03
		    // $06a2    e6 03     INC $03
		    // $06a4    20 2a 06  JSR $062a
		    // $06a7    60        RTS 
		    // $06a8    a2 02     LDX #$02
		    // $06aa    b5 10     LDA $10,X
		    // $06ac    c5 10     CMP $10
		    // $06ae    d0 06     BNE $06b6
		    // $06b0    b5 11     LDA $11,X
		    // $06b2    c5 11     CMP $11
		    // $06b4    f0 09     BEQ $06bf
		    // $06b6    e8        INX 
		    // $06b7    e8        INX 
		    // $06b8    e4 03     CPX $03
		    // $06ba    f0 06     BEQ $06c2
		    // $06bc    4c aa 06  JMP $06aa
		    // $06bf    4c 35 07  JMP $0735
		    // $06c2    60        RTS 
		    // $06c3    a6 03     LDX $03
		    // $06c5    ca        DEX 
		    // $06c6    8a        TXA 
		    // $06c7    b5 10     LDA $10,X
		    // $06c9    95 12     STA $12,X
		    // $06cb    ca        DEX 
		    // $06cc    10 f9     BPL $06c7
		    // $06ce    a5 02     LDA $02
		    // $06d0    4a        LSR A
		    // $06d1    b0 09     BCS $06dc
		    // $06d3    4a        LSR A
		    // $06d4    b0 19     BCS $06ef
		    // $06d6    4a        LSR A
		    // $06d7    b0 1f     BCS $06f8
		    // $06d9    4a        LSR A
		    // $06da    b0 2f     BCS $070b
		    // $06dc    a5 10     LDA $10
		    // $06de    38        SEC 
		    // $06df    e9 20     SBC #$20
		    // $06e1    85 10     STA $10
		    // $06e3    90 01     BCC $06e6
		    // $06e5    60        RTS 
		    // $06e6    c6 11     DEC $11
		    // $06e8    a9 01     LDA #$01
		    // $06ea    c5 11     CMP $11
		    // $06ec    f0 28     BEQ $0716
		    // $06ee    60        RTS 
		    // $06ef    e6 10     INC $10
		    // $06f1    a9 1f     LDA #$1f
		    // $06f3    24 10     BIT $10
		    // $06f5    f0 1f     BEQ $0716
		    // $06f7    60        RTS 
		    // $06f8    a5 10     LDA $10
		    // $06fa    18        CLC 
		    // $06fb    69 20     ADC #$20
		    // $06fd    85 10     STA $10
		    // $06ff    b0 01     BCS $0702
		    // $0701    60        RTS 
		    // $0702    e6 11     INC $11
		    // $0704    a9 06     LDA #$06
		    // $0706    c5 11     CMP $11
		    // $0708    f0 0c     BEQ $0716
		    // $070a    60        RTS 
		    // $070b    c6 10     DEC $10
		    // $070d    a5 10     LDA $10
		    // $070f    29 1f     AND #$1f
		    // $0711    c9 1f     CMP #$1f
		    // $0713    f0 01     BEQ $0716
		    // $0715    60        RTS 
		    // $0716    4c 35 07  JMP $0735
		    // $0719    a0 00     LDY #$00
		    // $071b    a5 fe     LDA $fe
		    // $071d    91 00     STA ($00),Y
		    // $071f    60        RTS 
		    // $0720    a6 03     LDX $03
		    // $0722    a9 00     LDA #$00
		    // $0724    81 10     STA ($10,X)
		    // $0726    a2 00     LDX #$00
		    // $0728    a9 01     LDA #$01
		    // $072a    81 10     STA ($10,X)
		    // $072c    60        RTS 
		    // $072d    a2 00     LDX #$00
		    // $072f    ea        NOP 
		    // $0730    ea        NOP 
		    // $0731    ca        DEX 
		    // $0732    d0 fb     BNE $072f
		    // $0734    60        RTS 
		    
		    c.bytecode(&h600) = ChrB(&h20) + ChrB(&h06) + ChrB(&h06) + ChrB(&h20) + ChrB(&h38) + ChrB(&h06) + ChrB(&h20) + ChrB(&h0d) + ChrB(&h06) + ChrB(&h20) + ChrB(&h2a) + ChrB(&h06) + ChrB(&h60) + ChrB(&ha9) + ChrB(&h02) + ChrB(&h85) _
		    + ChrB(&h02) + ChrB(&ha9) + ChrB(&h04) + ChrB(&h85) + ChrB(&h03) + ChrB(&ha9) + ChrB(&h11) + ChrB(&h85) + ChrB(&h10) + ChrB(&ha9) + ChrB(&h10) + ChrB(&h85) + ChrB(&h12) + ChrB(&ha9) + ChrB(&h0f) + ChrB(&h85 ) _
		    + ChrB(&h14) + ChrB(&ha9) + ChrB(&h04) + ChrB(&h85) + ChrB(&h11) + ChrB(&h85) + ChrB(&h13) + ChrB(&h85) + ChrB(&h15) + ChrB(&h60) + ChrB(&ha5) + ChrB(&hfe) + ChrB(&h85) + ChrB(&h00) + ChrB(&ha5) + ChrB(&hfe ) _
		    + ChrB(&h29) + ChrB(&h03) + ChrB(&h18) + ChrB(&h69) + ChrB(&h02) + ChrB(&h85) + ChrB(&h01) + ChrB(&h60) + ChrB(&h20) + ChrB(&h4d) + ChrB(&h06) + ChrB(&h20) + ChrB(&h8d) + ChrB(&h06) + ChrB(&h20) + ChrB(&hc3 ) _
		    + ChrB(&h06) + ChrB(&h20) + ChrB(&h19) + ChrB(&h07) + ChrB(&h20) + ChrB(&h20) + ChrB(&h07) + ChrB(&h20) + ChrB(&h2d) + ChrB(&h07) + ChrB(&h4c) + ChrB(&h38) + ChrB(&h06) + ChrB(&ha5) + ChrB(&hff) + ChrB(&hc9 ) _
		    + ChrB(&h77) + ChrB(&hf0) + ChrB(&h0d) + ChrB(&hc9) + ChrB(&h64) + ChrB(&hf0) + ChrB(&h14) + ChrB(&hc9) + ChrB(&h73) + ChrB(&hf0) + ChrB(&h1b) + ChrB(&hc9) + ChrB(&h61) + ChrB(&hf0) + ChrB(&h22) + ChrB(&h60 ) _
		    + ChrB(&ha9) + ChrB(&h04) + ChrB(&h24) + ChrB(&h02) + ChrB(&hd0) + ChrB(&h26) + ChrB(&ha9) + ChrB(&h01) + ChrB(&h85) + ChrB(&h02) + ChrB(&h60) + ChrB(&ha9) + ChrB(&h08) + ChrB(&h24) + ChrB(&h02) + ChrB(&hd0 ) _
		    + ChrB(&h1b) + ChrB(&ha9) + ChrB(&h02) + ChrB(&h85) + ChrB(&h02) + ChrB(&h60) + ChrB(&ha9) + ChrB(&h01) + ChrB(&h24) + ChrB(&h02) + ChrB(&hd0) + ChrB(&h10) + ChrB(&ha9) + ChrB(&h04) + ChrB(&h85) + ChrB(&h02 )_
		    + ChrB(&h60) + ChrB(&ha9) + ChrB(&h02) + ChrB(&h24) + ChrB(&h02) + ChrB(&hd0) + ChrB(&h05) + ChrB(&ha9) + ChrB(&h08) + ChrB(&h85) + ChrB(&h02) + ChrB(&h60) + ChrB(&h60) + ChrB(&h20) + ChrB(&h94) + ChrB(&h06 ) _
		    + ChrB(&h20) + ChrB(&ha8) + ChrB(&h06) + ChrB(&h60) + ChrB(&ha5) + ChrB(&h00) + ChrB(&hc5) + ChrB(&h10) + ChrB(&hd0) + ChrB(&h0d) + ChrB(&ha5) + ChrB(&h01) + ChrB(&hc5) + ChrB(&h11) + ChrB(&hd0) + ChrB(&h07 ) _
		    + ChrB(&he6) + ChrB(&h03) + ChrB(&he6) + ChrB(&h03) + ChrB(&h20) + ChrB(&h2a) + ChrB(&h06) + ChrB(&h60) + ChrB(&ha2) + ChrB(&h02) + ChrB(&hb5) + ChrB(&h10) + ChrB(&hc5) + ChrB(&h10) + ChrB(&hd0) + ChrB(&h06 ) _
		    + ChrB(&hb5) + ChrB(&h11) + ChrB(&hc5) + ChrB(&h11) + ChrB(&hf0) + ChrB(&h09) + ChrB(&he8) + ChrB(&he8) + ChrB(&he4) + ChrB(&h03) + ChrB(&hf0) + ChrB(&h06) + ChrB(&h4c) + ChrB(&haa) + ChrB(&h06) + ChrB(&h4c ) _
		    + ChrB(&h35) + ChrB(&h07) + ChrB(&h60) + ChrB(&ha6) + ChrB(&h03) + ChrB(&hca) + ChrB(&h8a) + ChrB(&hb5) + ChrB(&h10) + ChrB(&h95) + ChrB(&h12) + ChrB(&hca) + ChrB(&h10) + ChrB(&hf9) + ChrB(&ha5) + ChrB(&h02 ) _
		    + ChrB(&h4a) + ChrB(&hb0) + ChrB(&h09) + ChrB(&h4a) + ChrB(&hb0) + ChrB(&h19) + ChrB(&h4a) + ChrB(&hb0) + ChrB(&h1f) + ChrB(&h4a) + ChrB(&hb0) + ChrB(&h2f) + ChrB(&ha5) + ChrB(&h10) + ChrB(&h38) + ChrB(&he9 ) _
		    + ChrB(&h20) + ChrB(&h85) + ChrB(&h10) + ChrB(&h90) + ChrB(&h01) + ChrB(&h60) + ChrB(&hc6) + ChrB(&h11) + ChrB(&ha9) + ChrB(&h01) + ChrB(&hc5) + ChrB(&h11) + ChrB(&hf0) + ChrB(&h28) + ChrB(&h60) + ChrB(&he6 ) _
		    + ChrB(&h10) + ChrB(&ha9) + ChrB(&h1f) + ChrB(&h24) + ChrB(&h10) + ChrB(&hf0) + ChrB(&h1f) + ChrB(&h60) + ChrB(&ha5) + ChrB(&h10) + ChrB(&h18) + ChrB(&h69) + ChrB(&h20) + ChrB(&h85) + ChrB(&h10) + ChrB(&hb0 ) _
		    + ChrB(&h01) + ChrB(&h60) + ChrB(&he6) + ChrB(&h11) + ChrB(&ha9) + ChrB(&h06) + ChrB(&hc5) + ChrB(&h11) + ChrB(&hf0) + ChrB(&h0c) + ChrB(&h60) + ChrB(&hc6) + ChrB(&h10) + ChrB(&ha5) + ChrB(&h10) + ChrB(&h29 ) _
		    + ChrB(&h1f) + ChrB(&hc9) + ChrB(&h1f) + ChrB(&hf0) + ChrB(&h01) + ChrB(&h60) + ChrB(&h4c) + ChrB(&h35) + ChrB(&h07) + ChrB(&ha0) + ChrB(&h00) + ChrB(&ha5) + ChrB(&hfe) + ChrB(&h91) + ChrB(&h00) + ChrB(&h60 ) _
		    + ChrB(&ha6) + ChrB(&h03) + ChrB(&ha9) + ChrB(&h00) + ChrB(&h81) + ChrB(&h10) + ChrB(&ha2) + ChrB(&h00) + ChrB(&ha9) + ChrB(&h01) + ChrB(&h81) + ChrB(&h10) + ChrB(&h60) + ChrB(&ha2) + ChrB(&h00) + ChrB(&hea ) _
		    + ChrB(&hea) + ChrB(&hca) + ChrB(&hd0) + ChrB(&hfb) + ChrB(&h60 )
		    
		    c.run(&h00)
		  #EndIf
		  Break
		End Sub
	#tag EndEvent


#tag EndWindowCode

#tag ViewBehavior
	#tag ViewProperty
		Name="Name"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Interfaces"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Super"
		Visible=true
		Group="ID"
		Type="String"
		EditorType="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Width"
		Visible=true
		Group="Size"
		InitialValue="600"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Height"
		Visible=true
		Group="Size"
		InitialValue="400"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinWidth"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinHeight"
		Visible=true
		Group="Size"
		InitialValue="64"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxWidth"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaxHeight"
		Visible=true
		Group="Size"
		InitialValue="32000"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Frame"
		Visible=true
		Group="Frame"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Document"
			"1 - Movable Modal"
			"2 - Modal Dialog"
			"3 - Floating Window"
			"4 - Plain Box"
			"5 - Shadowed Box"
			"6 - Rounded Window"
			"7 - Global Floating Window"
			"8 - Sheet Window"
			"9 - Metal Window"
			"11 - Modeless Dialog"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Visible=true
		Group="Frame"
		InitialValue="Untitled"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="CloseButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Resizeable"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MaximizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MinimizeButton"
		Visible=true
		Group="Frame"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreenButton"
		Visible=true
		Group="Frame"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Composite"
		Group="OS X (Carbon)"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MacProcID"
		Group="OS X (Carbon)"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="FullScreen"
		Group="Behavior"
		InitialValue="False"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="ImplicitInstance"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="LiveResize"
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Placement"
		Visible=true
		Group="Behavior"
		InitialValue="0"
		Type="Integer"
		EditorType="Enum"
		#tag EnumValues
			"0 - Default"
			"1 - Parent Window"
			"2 - Main Screen"
			"3 - Parent Window Screen"
			"4 - Stagger"
		#tag EndEnumValues
	#tag EndViewProperty
	#tag ViewProperty
		Name="Visible"
		Visible=true
		Group="Behavior"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="HasBackColor"
		Visible=true
		Group="Background"
		InitialValue="False"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="BackColor"
		Visible=true
		Group="Background"
		InitialValue="&hFFFFFF"
		Type="Color"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Backdrop"
		Visible=true
		Group="Background"
		Type="Picture"
		EditorType="Picture"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBar"
		Visible=true
		Group="Menus"
		Type="MenuBar"
		EditorType="MenuBar"
	#tag EndViewProperty
	#tag ViewProperty
		Name="MenuBarVisible"
		Visible=true
		Group="Deprecated"
		InitialValue="True"
		Type="Boolean"
		EditorType="Boolean"
	#tag EndViewProperty
#tag EndViewBehavior
