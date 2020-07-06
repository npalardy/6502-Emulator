#tag Class
Protected Class MOS6502
	#tag Method, Flags = &h1
		Protected Sub ADCAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCImm()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim oper As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  If a + oper + If(isCarryFlagSet,1,0) > 255 Then
		    a = a + oper + If(isCarryFlagSet,1,0)
		    setCarryFlag
		  Else 
		    a = a + oper + If(isCarryFlagSet,1,0)
		    clearCarryFlag
		  End If
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "ADC #$" + ProperToHex(oper)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCIndY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCXind()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ADCzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndImm()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim andValue As UInt8 = machinememory.UInt8Value( pc )
		  
		  incrementPC
		  
		  a = a And andValue
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "AND #$" + ProperToHex(andValue)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndIndY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndXind()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Andzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub AndzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ASLa()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim setC As Boolean = (a And CType(&b10000000,UInt8)) <> 0
		  
		  a = Bitwise.ShiftLeft(a,1)
		  
		  setZeroFlagFromRegister(a)
		  setNFlagFromRegister(a)
		  If setC Then
		    setCarryFlag
		  Else
		    clearCarryFlag
		  End If
		  
		  ReportTrace pcAtStart, "ASL A"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ASLAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ASLAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ASLzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ASLzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BCCrel()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim relBranch As Int8 = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  If isCarryFlagSet = False Then
		    pc = pc + relBranch
		  End If
		  
		  ReportTrace pcAtStart, "BCC $" + ProperToHex(relBranch)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BCSrel()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim relBranch As Int8 = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  If isCarryFlagSet = True Then
		    pc = pc + relBranch
		  End If
		  
		  ReportTrace pcAtStart, "BCS $" + ProperToHex(relBranch)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BEQrel()
		  Dim pcAtStart As UInt16 = pc
		  
		  If pcAtStart = &h04e5 Then
		    Break
		  End If
		  incrementPC
		  
		  // All branches are relative mode And have a length Of two bytes. Syntax Is "Bxx Displacement" Or (better) "Bxx Label
		  
		  // BEQ = branch if zero flag is set
		  Dim relOffset As Int8 = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  If isZeroFlagSet Then
		    pc = pc + relOffset
		  End If
		  
		  ReportTrace pcAtStart, "BEQ $" + ProperToHex(relOffset)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BITAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BITzpg()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim zpgAddr As UInt8 = machinememory.UInt8Value( pc ) 
		  IncrementPC
		  
		  
		  Dim result As UInt8 = a And machinememory.UInt8Value( zpgAddr )
		  
		  setNFlagFromRegister( result )
		  setZeroFlagFromRegister( result )
		  setVFlagFromRegister( result )
		  
		  
		  ReportTrace pcAtStart, "BIT $" + ProperToHex(zpgAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BMIrel()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BNErel()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  // BNE rel
		  Dim relBranch As Int8 = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  If isZeroFlagSet = False Then
		    pc = pc + relBranch
		  End If
		  
		  ReportTrace pcAtStart, "BNE $" + ProperToHex(relBranch)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BPLrel()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim brOffset As UInt8 = machinememory.UInt8Value( pc ) 
		  IncrementPC
		  
		  
		  If isNFlagSet = False Then
		    pc = pc + brOffset
		  End If
		  
		  
		  
		  ReportTrace pcAtStart, "BPL $" + ProperToHex(brOffset)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Brk()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  setBreakFlag
		  
		  ReportTrace pcAtStart, "BRK"
		  
		  #If debugbuild Then
		    mKeepRunning = False
		  #EndIf
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BVCrel()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub BVSrel()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub bytecode(loadAddress as Integer, assigns value as memoryblock)
		  machinememory.StringValue(loadAddress, value.Size) = value.StringValue(0, value.size)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CLC()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  clearCarryFlag
		  
		  ReportTrace pcAtStart, "CLC"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CLD()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  clearDecimalFlag
		  
		  ReportTrace pcAtStart, "CLD"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub clearBreakFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status And &b11101111
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub clearCarryFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status And &b11111110
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub clearDecimalFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status Or &b11111101
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CLI()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CLV()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPImm()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  // c flag set to 1 when value in mem <= accum
		  //   set To 0 otherwise
		  // n flag set of bit 7
		  // z set for =
		  
		  Dim cmpValue As UInt8 = machinememory.UInt8Value( pc ) 
		  incrementPC
		  
		  If cmpValue <= a Then
		    setCarryFlag
		  Else
		    clearCarryFlag
		  End If
		  
		  setNFlagFromRegister(a)
		  
		  setZeroFlagFromRegister(cmpValue - a)
		  
		  ReportTrace pcAtStart, "CMP $" + ProperToHex(cmpValue)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPIndY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPXind()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPzpg()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim oper As UInt8 
		  oper = machinememory.UInt8Value( pc )
		  
		  incrementPC
		  
		  ReportTrace pcAtStart, "CMP $" + ProperToHex(oper)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CMPzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  Init
		  
		  machinememory = New MemoryBlock(65536)
		  
		  sp = &hFF // sp is always on page 1 (&h100 - &h1FF) and is only 1 byte
		  
		  status = &b00100000
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPXAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPXImm()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPXzpg()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim zpgAddr As UInt8 = machinememory.UInt8Value( pc )
		  IncrementPC
		  
		  setZeroFlagFromRegister( x - machinememory.UInt8Value(zpgAddr) )
		  setNFlagFromRegister( x - machinememory.UInt8Value(zpgAddr) )
		  
		  If x > machinememory.UInt8Value(zpgAddr) Then
		    setCarryFlag
		  Else
		    clearCarryFlag
		  End If
		  
		  ReportTrace pcAtStart, "CPX $" + ProperToHex(zpgAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPYAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPYImm()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CPYzpg()
		  
		  break
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DECAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DECAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DECzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DECzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DEX()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  x = x - 1
		  
		  setNFlagFromRegister( x )
		  setZeroFlagFromRegister( x )
		  
		  ReportTrace pcAtStart, "DEX"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DEY()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  y = y - 1
		  
		  setNFlagFromRegister( y )
		  setZeroFlagFromRegister( y )
		  
		  ReportTrace pcAtStart, "DEY"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORImm()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim eorValue As UInt8 = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  a = a Xor eorValue
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "EOR #$" + ProperToHex(eorValue)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORIndY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORXind()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub EORzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INCAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INCAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub IncrementPC()
		  Const inc As UInt16 = 1
		  pc = CType(pc + 1, UInt16)
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INCzpg()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim zpgAddr As UInt8 = machinememory.UInt8Value( pc ) 
		  IncrementPC
		  
		  machinememory.UInt8Value( zpgAddr ) = machinememory.UInt8Value( zpgAddr ) + 1
		  
		  setNFlagFromRegister( machinememory.UInt8Value( zpgAddr ) )
		  
		  setZeroFlagFromRegister( machinememory.UInt8Value( zpgAddr ) )
		  
		  ReportTrace pcAtStart, "INC $" + ProperToHex(zpgAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INCzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub Init()
		  // 
		  // 
		  // HI                                          LO-NIBBLE
		  // HI    00          01      02     03   04          05          06        07  08          09         0A       0B   0C          0D         0E        0F
		  // 00  BRK impl  ORA X,ind  ---    ---  ---        ORA zpg     ASL zpg    ---  PHP impl  ORA #      ASL A     ---  ---        ORA Abs    ASL Abs    ---
		  // 10  BPL rel   ORA ind,Y  ---    ---  ---        ORA zpg,X   ASL zpg,X  ---  CLC impl  ORA Abs,Y  ---       ---  ---        ORA Abs,X  ASL Abs,X  ---
		  // 20  JSR Abs   And X,ind  ---    ---  BIT zpg    And zpg     ROL zpg    ---  PLP impl  And #      ROL A     ---  BIT Abs    And Abs    ROL Abs    ---
		  // 30  BMI rel   And ind,Y  ---    ---  ---        And zpg,X   ROL zpg,X  ---  SEC impl  And Abs,Y  ---       ---  ---        And Abs,X  ROL Abs,X  ---
		  // 40  RTI impl  EOR X,ind  ---    ---  ---        EOR zpg     LSR zpg    ---  PHA impl  EOR #      LSR A     ---  JMP Abs    EOR Abs    LSR Abs    ---
		  // 50  BVC rel   EOR ind,Y  ---    ---  ---        EOR zpg,X   LSR zpg,X  ---  CLI impl  EOR Abs,Y  ---       ---  ---        EOR Abs,X  LSR Abs,X  ---
		  // 60  RTS impl  ADC X,ind  ---    ---  ---        ADC zpg     ROR zpg    ---  PLA impl  ADC #      ROR A     ---  JMP ind    ADC Abs    ROR Abs    ---
		  // 70  BVS rel   ADC ind,Y  ---    ---  ---        ADC zpg,X   ROR zpg,X  ---  SEI impl  ADC Abs,Y  ---       ---  ---        ADC Abs,X  ROR Abs,X  ---
		  // 80  ---       STA X,ind  ---    ---  STY zpg    STA zpg     STX zpg    ---  DEY impl  ---        TXA impl  ---  STY Abs    STA Abs    STX Abs    ---
		  // 90  BCC rel   STA ind,Y  ---    ---  STY zpg,X  STA zpg,X   STX zpg,Y  ---  TYA impl  STA Abs,Y  TXS impl  ---  ---        STA Abs,X  ---        ---
		  // A0  LDY #     LDA X,ind  LDX #  ---  LDY zpg    LDA zpg     LDX zpg    ---  TAY impl  LDA #      TAX impl  ---  LDY Abs    LDA Abs    LDX Abs    ---
		  // B0  BCS rel   LDA ind,Y  ---    ---  LDY zpg,X  LDA zpg,X   LDX zpg,Y  ---  CLV impl  LDA Abs,Y  TSX impl  ---  LDY Abs,X  LDA Abs,X  LDX Abs,Y  ---
		  // C0  CPY #     CMP X,ind  ---    ---  CPY zpg    CMP zpg     DEC zpg    ---  INY impl  CMP #      DEX impl  ---  CPY Abs    CMP Abs    DEC Abs    ---
		  // D0  BNE rel   CMP ind,Y  ---    ---  ---        CMP zpg,X   DEC zpg,X  ---  CLD impl  CMP Abs,Y  ---       ---  ---        CMP Abs,X  DEC Abs,X  ---
		  // E0  CPX #     SBC X,ind  ---    ---  CPX zpg    SBC zpg     INC zpg    ---  INX impl  SBC #      NOP impl  ---  CPX Abs    SBC Abs    INC Abs    ---
		  // F0  BEQ rel   SBC ind,Y  ---    ---  ---        SBC zpg,X   INC zpg,X  ---  SED impl  SBC Abs,Y  ---       ---  ---        SBC Abs,X  INC Abs,X  ---
		  
		  // 
		  // 
		  // Address Modes:
		  // 
		  // A        ....    Accumulator         OPC A         operand Is AC (implied Single Byte instruction)
		  // Abs        ....    absolute         OPC $LLHH         operand Is address $HHLL *
		  // Abs,X        ....    absolute, X-indexed         OPC $LLHH,X         operand Is address; effective address Is address incremented by X With carry **
		  // Abs,Y        ....    absolute, Y-indexed         OPC $LLHH,Y         operand Is address; effective address Is address incremented by Y With carry **
		  // #        ....    immediate         OPC #$BB         operand Is Byte BB
		  // impl        ....    implied         OPC         operand implied
		  // ind        ....    indirect         OPC ($LLHH)         operand Is address; effective address Is contents Of word at address: C.w($HHLL)
		  // X,ind        ....    X-indexed, indirect         OPC ($LL,X)         operand Is zeropage address; effective address Is word In (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
		  // ind,Y        ....    indirect, Y-indexed         OPC ($LL),Y         operand Is zeropage address; effective address Is word In (LL, LL + 1) incremented by Y With carry: C.w($00LL) + Y
		  // rel        ....    relative         OPC $BB         branch target Is PC + signed offset BB ***
		  // zpg        ....    zeropage         OPC $LL         operand Is zeropage address (hi-Byte Is zero, address = $00LL)
		  // zpg,X        ....    zeropage, X-indexed         OPC $LL,X         operand Is zeropage address; effective address Is address incremented by X without carry **
		  // zpg,Y        ....    zeropage, Y-indexed         OPC $LL,Y         operand Is zeropage address; effective address Is address incremented by Y without carry **
		  
		  // Indexed indirect: ($c0,X)
		  // 
		  // This one’s kinda weird. It’s like a cross between zero page,X And indirect. 
		  // Basically, you take the zero page address, add the value Of the X register 
		  // To it, Then use that To look up a two-Byte address. For example:
		  // 
		  // 
		  // Debugger
		  // A=$00 X=$00 Y=$00
		  // SP=$ff PC=$0600
		  // NV-BDIZC
		  // 00110000
		  // 
		  // Monitor   Start: $   Length: $  
		  // Memory locations $01 And $02 contain the values $05 And $07 respectively. 
		  // Think Of ($00,X) As ($00 + X). In this Case X Is $01, so this simplifies To ($01). 
		  // From here things proceed like standard indirect addressing - the two bytes at $01 And $02 ($05 And $07)
		  //are looked up To form the address $0705. This Is the address that the Y register was stored into 
		  // In the previous instruction, so the A register gets the same value As Y, albeit through a much 
		  // more circuitous route. You won’t see this much.
		  
		  // Indirect Indexed: ($c0),Y
		  
		  // Indirect indexed Is like indexed indirect but less insane. Instead Of adding the X 
		  // register To the address before dereferencing, the zero page address Is dereferenced, 
		  // And the Y register Is added To the resulting address.
		  // 
		  // 
		  // Debugger
		  // A=$00 X=$00 Y=$00
		  // SP=$ff PC=$0600
		  // NV-BDIZC
		  // 00110000
		  // 
		  // Monitor   Start: $   Length: $  
		  // In this Case, ($01) looks up the two bytes at $01 And $02: $03 And $07. These form 
		  // the address $0703. The value Of the Y register Is added To this address To give the final address $0704.
		  
		  For i As Integer = 0 To 255
		    opCodeHandlers(i) = AddressOf InvalidInstruction
		  Next
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 0               BRK      ORA X,ind                    ORA zpg    ASL zpg    PHP  ORA #      ASL A             ORA Abs    ASL Abs
		  opCodeHandlers(&h00) = AddressOf Brk // 00
		  opCodeHandlers(&h01) = AddressOf OraXInd // 01
		  opCodeHandlers(&h05) = AddressOf OraZpg // 05
		  opCodeHandlers(&h06) = AddressOf ASLzpg // 06
		  opCodeHandlers(&h08) = AddressOf PHP // 08
		  opCodeHandlers(&h09) = AddressOf OraImm // 09 
		  opCodeHandlers(&h0A) = AddressOf ASLa // 0A
		  opCodeHandlers(&h0D) = AddressOf ORAAbs // 0D
		  opCodeHandlers(&h0E) = AddressOf ASLAbs // 0E
		  
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 1               BPL rel  ORA (ind),Y                    ORA zpg,X  ASL zpg,X  CLC  ORA Abs,Y                    ORA Abs,X  ASL Abs,X
		  opCodeHandlers(&h10) = AddressOf BPLrel // 10
		  opCodeHandlers(&h11) = AddressOf ORAindY // 11
		  opCodeHandlers(&h11) = AddressOf ORAzpgX  // 15
		  opCodeHandlers(&h16) = AddressOf ASLzpgX //16
		  opCodeHandlers(&h18) = AddressOf CLC // 18
		  opCodeHandlers(&h19) = AddressOf ORAAbsY // 19
		  opCodeHandlers(&h1D) = AddressOf ORAAbsX  // 1D
		  opCodeHandlers(&h1E) = AddressOf ASLAbsX // 1E
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 2               JSR Abs  And (ind,X)         BIT zpg    And zpg    ROL zpg    PLP  And #      ROL A  BIT Abs    And Abs    ROL Abs
		  opCodeHandlers(&h20) = AddressOf JSRAbs 
		  opCodeHandlers(&h21) = AddressOf AndXind
		  opCodeHandlers(&h24) = AddressOf BITzpg  
		  opCodeHandlers(&h25) = AddressOf Andzpg   
		  opCodeHandlers(&h26) = AddressOf ROLzpg   
		  opCodeHandlers(&h28) = AddressOf PLP  
		  opCodeHandlers(&h29) = AddressOf AndImm    
		  opCodeHandlers(&h2A) = AddressOf ROLA  
		  opCodeHandlers(&h2C) = AddressOf BITAbs    
		  opCodeHandlers(&h2D) = AddressOf AndAbs    
		  opCodeHandlers(&h2E) = AddressOf ROLAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 3               BMI rel  And (ind),Y                    And zpg,X  ROL zpg,X  SEC  And Abs,Y                    And Abs,X  ROL Abs,X
		  opCodeHandlers(&h30) = AddressOf BMIrel  
		  opCodeHandlers(&h31) = AddressOf AndIndY                    
		  opCodeHandlers(&h35) = AddressOf AndzpgX  
		  opCodeHandlers(&h36) = AddressOf ROLzpgX  
		  opCodeHandlers(&h38) = AddressOf SEC  
		  opCodeHandlers(&h39) = AddressOf AndAbsY                    
		  opCodeHandlers(&h3D) = AddressOf AndAbsX  
		  opCodeHandlers(&h3E) = AddressOf ROLAbsX
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 4               RTI      EOR X,ind                    EOR zpg    LSR zpg    PHA  EOR #      LSR A  JMP Abs    EOR Abs    LSR Abs
		  opCodeHandlers(&h40) = AddressOf RTI      
		  opCodeHandlers(&h41) = AddressOf EORXind
		  opCodeHandlers(&h45) = AddressOf EORzpg    
		  opCodeHandlers(&h46) = AddressOf LSRzpg
		  opCodeHandlers(&h48) = AddressOf PHA  
		  opCodeHandlers(&h49) = AddressOf EORImm
		  opCodeHandlers(&h4A) = AddressOf LSRA
		  opCodeHandlers(&h4C) = AddressOf JMPAbs
		  opCodeHandlers(&h4D) = AddressOf EORAbs    
		  opCodeHandlers(&h4E) = AddressOf LSRAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 5               BVC rel  EOR (ind),Y                    EOR zpg,X  LSR zpg,X  CLI  EOR Abs,Y                    EOR Abs,X  LSR Abs,X
		  opCodeHandlers(&h50) = AddressOf BVCrel  
		  opCodeHandlers(&h51) = AddressOf EORIndY 
		  opCodeHandlers(&h55) = AddressOf EORzpgX  
		  opCodeHandlers(&h56) = AddressOf LSRzpgX 
		  opCodeHandlers(&h58) = AddressOf CLI  
		  opCodeHandlers(&h59) = AddressOf EORAbsY                    
		  opCodeHandlers(&h5D) = AddressOf EORAbsX  
		  opCodeHandlers(&h5E) = AddressOf LSRAbsX
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 6               RTS      ADC X,ind                    ADC zpg    ROR zpg    PLA  ADC #      ROR A  JMP (ind)  ADC Abs    ROR Abs
		  opCodeHandlers(&h60) = AddressOf RTS      
		  opCodeHandlers(&h61) = AddressOf ADCXind
		  opCodeHandlers(&h65) = AddressOf ADCzpg    
		  opCodeHandlers(&h66) = AddressOf RORzpg    
		  opCodeHandlers(&h68) = AddressOf PLA  
		  opCodeHandlers(&h69) = AddressOf ADCImm      
		  opCodeHandlers(&h6A) = AddressOf RORA  
		  opCodeHandlers(&h6C) = AddressOf JMPind  
		  opCodeHandlers(&h6D) = AddressOf ADCAbs    
		  opCodeHandlers(&h6E) = AddressOf RORAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 7               BVS rel  ADC (ind),Y                    ADC zpg,X  ROR zpg,X  SEI  ADC Abs,Y                    ADC Abs,X  ROR Abs,X
		  opCodeHandlers(&h70) = AddressOf BVSrel  
		  opCodeHandlers(&h71) = AddressOf ADCIndY                    
		  opCodeHandlers(&h75) = AddressOf ADCzpgX  
		  opCodeHandlers(&h76) = AddressOf RORzpgX  
		  opCodeHandlers(&h78) = AddressOf SEI  
		  opCodeHandlers(&h79) = AddressOf ADCAbsY                    
		  opCodeHandlers(&h7D) = AddressOf ADCAbsX  
		  opCodeHandlers(&h7E) = AddressOf RORAbsX
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 8                        STA X,ind         STY zpg    STA zpg    STX zpg    DEY             TXA    STY Abs    STA Abs    STX Abs
		  opCodeHandlers(&h81) = AddressOf STAXind
		  opCodeHandlers(&h84) = AddressOf STYzpg    
		  opCodeHandlers(&h85) = AddressOf STAzpg    
		  opCodeHandlers(&h86) = AddressOf STXzpg    
		  opCodeHandlers(&h88) = AddressOf DEY             
		  opCodeHandlers(&h8A) = AddressOf TXA    
		  opCodeHandlers(&h8C) = AddressOf STYAbs    
		  opCodeHandlers(&h8D) = AddressOf STAAbs    
		  opCodeHandlers(&h8E) = AddressOf STXAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // 9               BCC rel  STA (ind),Y         STY zpg,X  STA zpg,X  STX zpg,Y  TYA  STA Abs,Y  TXS               STA Abs,X    
		  opCodeHandlers(&h90) = AddressOf BCCrel  
		  opCodeHandlers(&h91) = AddressOf STAIndY         
		  opCodeHandlers(&h94) = AddressOf STYzpgX  
		  opCodeHandlers(&h95) = AddressOf STAzpgX  
		  opCodeHandlers(&h96) = AddressOf STXzpgY  
		  opCodeHandlers(&h98) = AddressOf TYA  
		  opCodeHandlers(&h99) = AddressOf STAAbsY  
		  opCodeHandlers(&h9A) = AddressOf TXS               
		  opCodeHandlers(&h9D) = AddressOf STAAbsX    
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // A               LDY #    LDA X,ind  LDX #  LDY zpg    LDA zpg    LDX zpg    TAY  LDA #      TAX    LDY Abs    LDA Abs    LDX Abs
		  opCodeHandlers(&hA0) = AddressOf LDYImm    
		  opCodeHandlers(&hA1) = AddressOf LDAXind
		  opCodeHandlers(&hA2) = AddressOf LDXImm  
		  opCodeHandlers(&hA4) = AddressOf LDYzpg    
		  opCodeHandlers(&hA5) = AddressOf LDAzpg    
		  opCodeHandlers(&hA6) = AddressOf LDXzpg    
		  opCodeHandlers(&hA8) = AddressOf TAY  
		  opCodeHandlers(&hA9) = AddressOf LDAImm      
		  opCodeHandlers(&hAA) = AddressOf TAX    
		  opCodeHandlers(&hAC) = AddressOf LDYAbs    
		  opCodeHandlers(&hAD) = AddressOf LDAAbs    
		  opCodeHandlers(&hAE) = AddressOf LDXAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // B               BCS rel  LDA (ind),Y         LDY zpg,X  LDA zpg,X  LDX zpg,Y  CLV  LDA Abs,Y  TSX    LDY Abs,X  LDA Abs,X  LDX Abs,Y
		  opCodeHandlers(&hb0) = AddressOf BCSrel  
		  opCodeHandlers(&hb1) = AddressOf LDAIndY         
		  opCodeHandlers(&hb4) = AddressOf LDYzpgX  
		  opCodeHandlers(&hb5) = AddressOf LDAzpgX  
		  opCodeHandlers(&hb6) = AddressOf LDXzpgY  
		  opCodeHandlers(&hb8) = AddressOf CLV  
		  opCodeHandlers(&hb9) = AddressOf LDAAbsY  
		  opCodeHandlers(&hbA) = AddressOf TSX    
		  opCodeHandlers(&hbC) = AddressOf LDYAbsX  
		  opCodeHandlers(&hbD) = AddressOf LDAAbsX  
		  opCodeHandlers(&hbE) = AddressOf LDXAbsY
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // C               CPY #    CMP X,ind         CPY zpg    CMP zpg    DEC zpg    INY  CMP #      DEX    CPY Abs    CMP Abs    DEC Abs
		  opCodeHandlers(&hc0) = AddressOf CPYImm    
		  opCodeHandlers(&hc1) = AddressOf CMPXind
		  opCodeHandlers(&hc4) = AddressOf CPYzpg    
		  opCodeHandlers(&hc5) = AddressOf CMPzpg    
		  opCodeHandlers(&hc6) = AddressOf DECzpg    
		  opCodeHandlers(&hc8) = AddressOf INY  
		  opCodeHandlers(&hc9) = AddressOf CMPImm      
		  opCodeHandlers(&hcA) = AddressOf DEX    
		  opCodeHandlers(&hcC) = AddressOf CPYAbs    
		  opCodeHandlers(&hcD) = AddressOf CMPAbs    
		  opCodeHandlers(&hcE) = AddressOf DECAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // D               BNE rel  CMP (ind),Y                    CMP zpg,X  DEC zpg,X  CLD  CMP Abs,Y                    CMP Abs,X  DEC Abs,X
		  opCodeHandlers(&hD0) = AddressOf BNErel  
		  opCodeHandlers(&hD1) = AddressOf CMPIndY                    
		  opCodeHandlers(&hD5) = AddressOf CMPzpgX  
		  opCodeHandlers(&hD6) = AddressOf DECzpgX  
		  opCodeHandlers(&hD8) = AddressOf CLD  
		  opCodeHandlers(&hD9) = AddressOf CMPAbsY                    
		  opCodeHandlers(&hDD) = AddressOf CMPAbsX  
		  opCodeHandlers(&hDE) = AddressOf DECAbsX
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // E               CPX #    SBC X,ind         CPX zpg    SBC zpg    INC zpg    INX  SBC #      NOP    CPX Abs    SBC Abs    INC Abs
		  opCodeHandlers(&hE0) = AddressOf CPXImm    
		  opCodeHandlers(&hE1) = AddressOf SBCXind
		  opCodeHandlers(&hE4) = AddressOf CPXzpg    
		  opCodeHandlers(&hE5) = AddressOf SBCzpg    
		  opCodeHandlers(&hE6) = AddressOf INCzpg    
		  opCodeHandlers(&hE8) = AddressOf INX  
		  opCodeHandlers(&hE9) = AddressOf SBCImm      
		  opCodeHandlers(&hEA) = AddressOf NOP    
		  opCodeHandlers(&hEC) = AddressOf CPXAbs    
		  opCodeHandlers(&hED) = AddressOf SBCAbs    
		  opCodeHandlers(&hEE) = AddressOf INCAbs
		  
		  // Low nibble
		  // High nibble      0       1            2      4          5          6          8    9          A      C          D          E
		  // F               BEQ rel  SBC (ind),Y                    SBC zpg,X  INC zpg,X  SED  SBC Abs,Y                    SBC Abs,X  INC Abs,X
		  opCodeHandlers(&hF0) = AddressOf BEQrel  
		  opCodeHandlers(&hF1) = AddressOf SBCIndY                    
		  opCodeHandlers(&hF5) = AddressOf SBCzpgX  
		  opCodeHandlers(&hF6) = AddressOf INCzpgX  
		  opCodeHandlers(&hF8) = AddressOf SED  
		  opCodeHandlers(&hF9) = AddressOf SBCAbsY                    
		  opCodeHandlers(&hFD) = AddressOf SBCAbsX  
		  opCodeHandlers(&hFE) = AddressOf INCAbsX
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub InvalidInstruction()
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INX()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  X = X + 1
		  
		  setNFlagFromRegister(x)
		  
		  setZeroFlagFromRegister(x)
		  
		  ReportTrace pcAtStart, "INX"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub INY()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Y = Y + 1
		  
		  setZeroFlagFromRegister( y )
		  
		  setNFlagFromRegister( y )
		  
		  ReportTrace pcAtStart, "INY"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function isBreakFlagSet() As boolean
		  // 
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  Return (status And &b00010000) <> 0
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function isCarryFlagSet() As boolean
		  // 
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  Return (status And &b00000001) <> 0
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function isNFlagSet() As boolean
		  // 
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  Return (status And &b10000000) <> 0
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function isZeroFlagSet() As boolean
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  Return (status And &b00000010) <> 0
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub JMPAbs()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  // next 2 bytes are address to jump to
		  
		  Dim addressToJumpTo As UInt16 = machinememory.UInt16Value(pc)
		  incrementPC
		  
		  pc = addressToJumpTo
		  
		  ReportTrace pcAtStart, "JMP $" + ProperToHex(addressToJumpTo)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub JMPind()
		  // next 2 bytes hold adress that holds address to jump to
		  
		  break
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub JSRAbs()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  // next 2 bytes are address to jump to
		  
		  Dim addressToJumpTo As UInt16 = machinememory.UInt16Value(pc)
		  
		  incrementPC
		  
		  //store return address in stack
		  
		  sp = sp - 2
		  machinememory.UInt16Value(&h100 + sp) = pc
		  
		  pc = addressToJumpTo
		  
		  ReportTrace pcAtStart, "JSR $" + ProperToHex(addressToJumpTo)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAAbs()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  Dim absAddr As UInt16 = machinememory.UInt16Value(pc)
		  IncrementPC
		  IncrementPC
		  
		  a = machinememory.UInt8Value(absAddr) 
		  
		  ReportTrace pcAtStart, "LDA $" + ProperToHex(absAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAAbsX()
		  
		  
		  break
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAAbsY()
		  
		  
		  break
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAImm()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  a = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "LDA #$" + ProperToHex(a)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAIndY()
		  Dim pcAtStart As UInt16 = pc
		  
		  incrementPC
		  
		  //  LDA (SRC),Y     ;get from source String
		  // src is a 1 byte address
		  // pull that into a 16 bit reg + Y
		  
		  Dim base As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  Dim actualAddress As Integer = machinememory.UInt16Value(base) + y
		  
		  a = machineMemory.UInt8Value(actualAddress)
		  
		  // n bit should be = a &b1000 0000
		  setNFlagFromRegister(a)
		  
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "LDA ($" + ProperToHex(base) + ") , Y"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAXind()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim zpgAddr As Integer = machinememory.Int8Value(pc)
		  IncrementPC
		  
		  Dim actualAddress As Int16 = machinememory.Int16Value(zpgAddr + x)
		  
		  a = machinememory.Int8Value( actualAddress )
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "LDA ($" + zpgAddr.ProperToHex + ",X)"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAzpg()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  a = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "LDA $" + ProperToHex(a)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDAzpgX()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim zpgBase As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  a = machinememory.UInt8Value(zpgBase + x)
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtstart, "LDA $" + ProperToHex(zpgBase) + ", X"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDXAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDXAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDXImm()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  x = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  setNFlagFromRegister( x )
		  setZeroFlagFromRegister( x )
		  
		  ReportTrace pcAtStart, "LDX #$" + ProperToHex(x)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDXzpg()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim zpgAddr As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  x = machinememory.UInt8Value(zpgAddr)
		  
		  setNFlagFromRegister( x )
		  setZeroFlagFromRegister( x )
		  
		  ReportTrace pcAtStart, "LDX $" + ProperToHex(zpgAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDXzpgY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDYAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDYAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDYImm()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  y = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  setNFlagFromRegister( y )
		  setZeroFlagFromRegister( y )
		  
		  ReportTrace pcAtStart, "LDY #$" + ProperToHex(y)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDYzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LDYzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LSRA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  If (a And &b00000001) <> 0 Then
		    setCarryFlag
		  Else
		    clearCarryFlag
		  End If
		  
		  a = a \ 2
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "LSR A"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LSRAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LSRAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LSRzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub LSRzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub NOP()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  ReportTrace pcAtStart, "NOP"
		  
		  
		End Sub
	#tag EndMethod

	#tag DelegateDeclaration, Flags = &h1
		Protected Delegate Sub OpCodeHandler()
	#tag EndDelegateDeclaration

	#tag Method, Flags = &h1
		Protected Sub ORAAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ORAAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ORAAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub OraImm()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  // logical Or between Accum & memory
		  
		  Dim immedValue As UInt8 = machinememory.UInt8Value( pc )
		  incrementPC
		  
		  a = a Or immedValue
		  
		  setZeroFlagFromRegister( a )
		  
		  setNFlagFromRegister(a)
		  
		  ReportTrace pcAtStart, "ORA #" + ProperToHex(immedValue)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ORAindY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub OraXInd()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub OraZpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ORAzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub PHA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  sp = sp - 1
		  machinememory.UInt16Value(&h100 + sp) = a
		  
		  ReportTrace pcAtStart, "PHA"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub PHP()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  sp = sp - 1
		  machinememory.UInt16Value(&h100 + sp) = status
		  
		  ReportTrace pcAtStart, "PHP"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub PLA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  a = machinememory.UInt16Value(&h100 + sp) 
		  
		  sp = sp + 1
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "PLA"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub PLP()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  status = machinememory.UInt16Value(&h100 + sp)
		  
		  sp = sp - 1
		  
		  ReportTrace pcAtStart, "PLP"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ReportTrace(pcAtStart as Uint16, instruction as string)
		  
		  If traceMode = False Then
		    Return
		  End If
		  
		  // so we want to report the instruction + registers + pc + status
		  //      INSTRUCTION_   A  X  Y NV-BDIZC  PC
		  //      max 13        02 02 02 xxxxxxxx  04
		  //      ....5....0... .. .. .. ........ .... 
		  
		  Const maxInstructionLen = 13
		  Const maxAregLength = 2
		  Const maxXregLength = 2
		  Const maxYregLength = 2
		  Const maxStatusLength = 8
		  Const maxPCLength = 4
		  
		  Dim line() As String
		  If instruction.Len > maxInstructionLen Then
		    Break
		  End If
		  
		  If (tracelines = 0)  Or ((tracelines Mod 100) = 0) Then
		    
		    line.Append Left(" PC                ", maxPCLength)
		    line.append Left("                   ", maxInstructionLen)
		    line.Append Left(" A                 ", maxAregLength)
		    line.Append Left(" X                 ", maxXregLength)
		    line.Append Left(" Y                 ", maxYregLength)
		    line.Append Left("NV-BDIZC           ", maxStatusLength)
		    
		    System.debuglog Join(line, " ")
		    
		    Redim line(-1)
		    
		  End If
		  
		  line.Append Left(pcAtStart.ProperToHex, maxPCLength)
		  line.append Left(instruction + "                 ", maxInstructionLen)
		  line.Append Left(a.ProperToHex, maxAregLength)
		  line.Append Left(x.ProperToHex, maxXregLength)
		  line.Append Left(y.ProperToHex, maxYregLength)
		  line.Append Left(ProperConversions.ProperToBinary(status), maxStatusLength)
		  
		  System.debuglog Join(line, " ")
		  
		  traceLines = traceLines + 1
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ROLA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim willSetC As Boolean = (a And CType(&b10000000,UInt8)) <> 0
		  
		  a = Bitwise.ShiftLeft(a,1)
		  If isCarryFlagSet Then
		    a = a Or CType(&b00000001, UInt8)
		  End If
		  
		  setZeroFlagFromRegister(a)
		  setNFlagFromRegister(a)
		  
		  If willSetC Then
		    setCarryFlag
		  Else
		    clearCarryFlag
		  End If
		  
		  
		  ReportTrace pcAtStart, "ROL A"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ROLAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ROLAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ROLzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ROLzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RORA()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RORAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RORAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RORzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RORzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RTI()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RTS()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim addressToReturnTo As UInt16 = machinememory.UInt16Value( &h100 + sp )
		  
		  sp = sp + 2
		  
		  pc = addressToReturnTo 
		  
		  incrementPC
		  
		  ReportTrace pcAtStart, "RTS to $" + ProperToHex(pc) +""
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Run(initialPC as integer)
		  traceMode = True
		  
		  pc = initialPC
		  
		  While mKeepRunning
		    
		    Dim opcode As UInt8 = machinememory.UInt8Value(pc)
		    Dim handler As OpCodeHandler = opCodeHandlers( opcode )
		    
		    handler.Invoke
		    
		  Wend
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCAbs()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCImm()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCIndY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCXind()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCzpg()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SBCzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SEC()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  // 
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  // 
		  
		  setCarryFlag
		  
		  ReportTrace pcAtStart, "SEC"
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SED()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub SEI()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setBreakFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status Or &b00010000
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setCarryFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status Or &b00000001
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setDecimalFlag()
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status Or &b00000010
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setNFlagFromRegister(register as Uint8)
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  // N flag should always be MSB of the accumulator
		  status = status Or ( register And &b10000000 )
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setVFlagFromRegister(register as Uint8)
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  status = status Or ( register And &b01000000 )
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub setZeroFlagFromRegister(register as Uint8)
		  // 8 bits
		  // bit 6 Is Not used
		  // 7 6 5 4 3 2 1 0
		  // N V   B D I Z C
		  // 
		  // N - negative - same As bit 7 Of accumulator
		  // V - overflow
		  // B - Break command
		  // indicates If BRK instruction was run Or interrupt signal
		  // D - decimal mode
		  // I - interrupt disable
		  // Z - zero
		  // C - carry
		  
		  If register = 0 Then
		    status = status Or &b00000010
		  Else
		    status = status And &b11111101
		  End If
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAAbs()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim absAddr As UInt16 = machinememory.UInt16Value(pc)
		  IncrementPC
		  IncrementPC
		  
		  If absAddr = &h04e6 Then
		    Break
		  End If
		  
		  machinememory.UInt8Value(absAddr) = A
		  
		  ReportTrace pcAtStart, "STA $" + ProperToHex(absAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAAbsX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAAbsY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAIndY()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  //  STA (DST),Y
		  // src is a 1 byte address
		  // pull that into a 16 bit reg + Y
		  
		  Dim base As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  Dim actualAddress As Integer = machinememory.UInt16Value(base) + y
		  
		  If actualAddress = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(actualAddress) = a
		  
		  ReportTrace pcAtStart, "STA ($" + ProperToHex(base) + "), y " 
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAXind()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim base As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  Dim actualAddress As Integer = machinememory.UInt16Value(base + x)
		  
		  If actualAddress = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(actualAddress) = a
		  
		  ReportTrace pcAtstart, "STA ($" + ProperToHex(base) +"), X"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAzpg()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  //  STA oper (zero page)
		  
		  Dim addr As UInt8 = machinememory.UInt8Value(pc)
		  incrementPC
		  
		  If addr = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(addr) = a
		  
		  ReportTrace pcAtStart, "STA $" + ProperToHex(addr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STAzpgX()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  //  STA oper (zero page)
		  
		  Dim zpgAddr As UInt8 = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  If zpgAddr + x = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(zpgAddr + x) = a
		  
		  ReportTrace pcAtStart, "STA $" + ProperToHex(zpgAddr) + ", X"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STXAbs()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim absAddr As UInt16 = machinememory.UInt16Value(pc)
		  IncrementPC
		  IncrementPC
		  
		  If absAddr = &h04e6 Then
		    Break
		  End If
		  
		  machinememory.UInt8Value(absAddr) = x
		  
		  ReportTrace pcAtStart, "STX $" + ProperToHex(absAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STXzpg()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  //  STX oper (zero page)
		  
		  Dim addr As UInt8 = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  If addr = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(addr) = x
		  
		  ReportTrace pcAtStart, "STX $" + ProperToHex(addr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STXzpgY()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STYAbs()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  Dim absAddr As UInt16 = machinememory.UInt16Value(pc)
		  IncrementPC
		  IncrementPC
		  
		  If absAddr = &h04e6 Then
		    Break
		  End If
		  
		  machinememory.UInt8Value(absAddr) = y
		  
		  ReportTrace pcAtStart, "STY $" + ProperToHex(absAddr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STYzpg()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  //  STY oper (zero page)
		  
		  Dim addr As UInt8 = machinememory.UInt8Value(pc)
		  
		  incrementPC
		  
		  If addr = &h04e6 Then
		    Break
		  End If
		  
		  machineMemory.UInt8Value(addr) = y
		  
		  ReportTrace pcAtStart, "STY $" + ProperToHex(addr)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub STYzpgX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TAX()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  x = a
		  
		  setNFlagFromRegister( x )
		  setZeroFlagFromRegister( x )
		  
		  ReportTrace pcAtStart, "TAX"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TAY()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  y = a
		  
		  setNFlagFromRegister( y )
		  setZeroFlagFromRegister( y )
		  
		  ReportTrace pcAtStart, "TAY"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TSX()
		  
		  Break  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TXA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  a = x
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "TXA"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TXS()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  sp = x
		  
		  ReportTrace pcAtStart, "TXS"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub TYA()
		  Dim pcAtStart As UInt16 = pc
		  incrementPC
		  
		  a = x
		  
		  setNFlagFromRegister( a )
		  setZeroFlagFromRegister( a )
		  
		  ReportTrace pcAtStart, "TYA"
		  
		End Sub
	#tag EndMethod


	#tag Note, Name = Status flags
		
		8 bits
		bit 6 is not used
		7 6 5 4 3 2 1 0
		N V   B D I Z C
		
		N - negative - same as bit 7 of accumulator
		V - overflow 
		- - unused - always 1 in some texts
		B - break command indicates if BRK instruction was run or interrupt signal
		D - decimal mode
		I - interrupt disable
		Z - zero
		C - carry
		
		
	#tag EndNote


	#tag Property, Flags = &h0
		a As Uint8
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected machinememory As memoryblock
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected mKeepRunning As boolean = true
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mstatus As uint8
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected opCodeHandlers(255) As OpCodeHandler
	#tag EndProperty

	#tag Property, Flags = &h0
		pc As Uint16
	#tag EndProperty

	#tag Property, Flags = &h0
		sp As Uint8
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return mstatus
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  mstatus = value
			  
			  
			  // 8 bits
			  // bit 6 Is Not used
			  // 7 6 5 4 3 2 1 0
			  // N V   B D I Z C
			  //
			  // N - negative - same As bit 7 Of accumulator
			  // V - overflow
			  // B - Break command
			  // indicates If BRK instruction was run Or interrupt signal
			  // D - decimal mode
			  // I - interrupt disable
			  // Z - zero
			  // C - carry
			  
			End Set
		#tag EndSetter
		status As UInt8
	#tag EndComputedProperty

	#tag Property, Flags = &h21
		Private traceLines As Integer
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected traceMode As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		x As Uint8
	#tag EndProperty

	#tag Property, Flags = &h0
		y As uint8
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="a"
			Group="Behavior"
			Type="Uint8"
		#tag EndViewProperty
		#tag ViewProperty
			Name="pc"
			Group="Behavior"
			Type="Uint16"
		#tag EndViewProperty
		#tag ViewProperty
			Name="sp"
			Group="Behavior"
			Type="Uint8"
		#tag EndViewProperty
		#tag ViewProperty
			Name="status"
			Group="Behavior"
			Type="UInt8"
		#tag EndViewProperty
		#tag ViewProperty
			Name="x"
			Group="Behavior"
			Type="Uint8"
		#tag EndViewProperty
		#tag ViewProperty
			Name="y"
			Group="Behavior"
			Type="uint8"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
