









































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































	OPT	2 










































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































;++
;
; Copyright (c) Microsoft Corporation.  All rights reserved.
;
;
; Module:
;
;   kxarm64.w
;
; Abstract:
;
;   Contains ARM architecture constants and assembly macros.
;
;--

;
; The ARM assembler uses a baroque syntax that is documented as part
; of the online Windows CE documentation.  The syntax derives from
; ARM's own assembler and was chosen to allow the migration of
; specific assembly code bases, namely ARM's floating point runtime.
; While this compatibility is no longer strictly necessary, the
; syntax lives on....
;
; Highlights:
;      * Assembler is white space sensitive.  Symbols are defined by putting
;        them in the first column
;      * The macro definition mechanism is very primitive
;
; To augment the assembler, assembly files are run through CPP (as they are
; on IA64).  This works well for constants but not structural components due
; to the white space sensitivity.
;
; For now, we use a mix of native assembler and CPP macros.
;








;++
;
; Copyright (c) Microsoft Corporation.  All rights reserved.
;
;
; Module:
;
;   kxarm64unw.w
;
; Abstract:
;
;   Contains ARM64 unwind code helper macros
;
;   This file is not really useful on its own without the support from
;   kxarm64.h.
;
;--

;
; The following macros are defined here:
;
;   PROLOG_STACK_ALLOC <amount>
;   PROLOG_SAVE_REG
;   PROLOG_SAVE_REG_PAIR
;   PROLOG_NOP <operation>
;   PROLOG_SAVE_NEXT_PAIR <operation>
;   PROLOG_PUSH_TRAP_FRAME
;   PROLOG_PUSH_MACHINE_FRAME
;   PROLOG_PUSH_CONTEXT
;   PROLOG_PUSH_EC_CONTEXT
;   PROLOG_SIGN_RETURN_ADDRESS
;
;   EPILOG_STACK_FREE <amount>
;   EPILOG_RECOVER_SP <offset>
;   EPILOG_RESTORE_REG
;   EPILOG_RESTORE_REG_PAIR
;   EPILOG_NOP <operation>
;   EPILOG_RESTORE_NEXT_PAIR <operation>
;   EPILOG_AUTHENTICATE_RETURN_ADDRESS
;   EPILOG_RETURN
;

        ;
        ; Global variables
        ;

        ; result from __ParseRegisterNumber
        GBLA __ParsedRegNumber

        ; result from __ParseOffset
        GBLA __ParsedOffsetAbs
        GBLA __ParsedOffsetShifted3
        GBLA __ParsedOffsetShifted4
        GBLA __ParsedOffsetPreinc
        GBLS __ParsedOffsetRawString
        GBLS __ParsedOffsetString

        ; results from __ComputeCodes[...]
        GBLS __ComputedCodes
        GBLL __RegPairWasFpLr

        ; global state and accumulators
        GBLS __PrologUnwindString
        GBLS __PrologLastLabel
        GBLA __EpilogUnwindCount
        GBLS __Epilog1UnwindString
        GBLS __Epilog2UnwindString
        GBLS __Epilog3UnwindString
        GBLS __Epilog4UnwindString
        GBLL __EpilogStartNotDefined
        GBLA __RunningIndex
        GBLS __RunningLabel
        GBLS __FuncExceptionHandler


        ;
        ; Helper macro: emit an opcode with a generated label
        ;
        ; Output: Name of label is in $__RunningLabel
        ;

        MACRO
        __EmitRunningLabelAndOpcode $O1,$O2,$O3,$O4,$O5,$O6

__RunningLabel SETS "|Temp.$__RunningIndex|"
__RunningIndex SETA __RunningIndex + 1

        IF "$O6" != ""
$__RunningLabel $O1,$O2,$O3,$O4,$O5,$O6
        ELIF "$O5" != ""
$__RunningLabel $O1,$O2,$O3,$O4,$O5
        ELIF "$O4" != ""
$__RunningLabel $O1,$O2,$O3,$O4
        ELIF "$O3" != ""
$__RunningLabel $O1,$O2,$O3
        ELIF "$O2" != ""
$__RunningLabel $O1,$O2
        ELIF "$O1" != ""
$__RunningLabel $O1
        ELSE
$__RunningLabel
        ENDIF

        MEND


        ;
        ; Helper macro: append unwind codes to the prolog string
        ;
        ; Input is in __ComputedCodes
        ;

        MACRO
        __AppendPrologCodes

__PrologUnwindString SETS "$__ComputedCodes,$__PrologUnwindString"

        MEND


        ;
        ; Helper macro: append unwind codes to the epilog string
        ;
        ; Input is in __ComputedCodes
        ;

        MACRO
        __AppendEpilogCodes

        IF __EpilogUnwindCount == 1
__Epilog1UnwindString SETS "$__Epilog1UnwindString,$__ComputedCodes"
        ELIF __EpilogUnwindCount == 2
__Epilog2UnwindString SETS "$__Epilog2UnwindString,$__ComputedCodes"
        ELIF __EpilogUnwindCount == 3
__Epilog3UnwindString SETS "$__Epilog3UnwindString,$__ComputedCodes"
        ELIF __EpilogUnwindCount == 4
__Epilog4UnwindString SETS "$__Epilog4UnwindString,$__ComputedCodes"
        ENDIF

        MEND


        ;
        ; Helper macro: detect prolog end
        ;

        MACRO
        __DeclarePrologEnd

__PrologLastLabel SETS "$__RunningLabel"

        MEND


        ;
        ; Helper macro: detect epilog start
        ;

        MACRO
        __DeclareEpilogStart

        IF __EpilogStartNotDefined
__EpilogStartNotDefined SETL {false}
__EpilogUnwindCount SETA __EpilogUnwindCount + 1
        IF __EpilogUnwindCount == 1
$__FuncEpilog1StartLabel
        ELIF __EpilogUnwindCount == 2
$__FuncEpilog2StartLabel
        ELIF __EpilogUnwindCount == 3
$__FuncEpilog3StartLabel
        ELIF __EpilogUnwindCount == 4
$__FuncEpilog4StartLabel
        ELSE
        INFO    1, "Too many epilogues!"
        ENDIF
        ENDIF

        MEND


        ;
        ; Helper macro: specify epilog end
        ;

        MACRO
        __DeclareEpilogEnd

__EpilogStartNotDefined SETL {true}

        MEND


        ;
        ; Parse a register number
        ;
        ; Calling macro name is in $Name
        ; Input is in $Reg
        ; Output is placed in __ParsedRegNumber
        ;

        MACRO
        __ParseRegisterNumber $Name, $Reg

        LCLS    RString

RString SETS    "$Reg"

        IF (RString:LEFT:1 == "q") || (RString:LEFT:1 == "v")

RString  SETS    RString:RIGHT:(:LEN:RString - 1)
__ParsedRegNumber SETA  64 + $RString

        ELIF RString:LEFT:1 == "d"

RString  SETS    RString:RIGHT:(:LEN:RString - 1)
__ParsedRegNumber SETA  32 + $RString

        ELSE

__ParsedRegNumber SETA  :RCONST:$RString

        ENDIF

        MEND


        ;
        ; Parse a stack offset
        ;
        ; Calling macro name is in $Name
        ; Input is in $Offset
        ; Which is "Prolog" or "Epilog"
        ; Output is placed in __ParsedOffsetAbs, __ParsedOffsetPreinc, __ParsedOffsetString
        ;

        MACRO
        __ParseOffset $Name, $Offset, $Which

        ; copy to local string
        LCLS    OffsStr
OffsStr SETS    "$Offset"

        ; strip opening # if present
        IF OffsStr:LEFT:1 == "#"
OffsStr SETS    OffsStr:RIGHT:(:LEN:OffsStr - 1)
        ENDIF

        ; look for pre/postincrement forms
        IF OffsStr:RIGHT:1 == "!"

        ; prolog must be preincrement with a negative offset
        IF "$Which" == "Prolog"

        IF OffsStr:LEFT:1 != "-"
        INFO    1, "$Name: Preincrement offsets must be negative"
        MEXIT
        ENDIF

OffsStr SETS    OffsStr:LEFT:(:LEN:OffsStr - 1)
__ParsedOffsetAbs SETA $OffsStr
__ParsedOffsetAbs SETA -__ParsedOffsetAbs
__ParsedOffsetPreinc SETA 1
__ParsedOffsetRawString SETS "#":CC:OffsStr
__ParsedOffsetString SETS "[sp, #":CC:OffsStr:CC:"]!"

        ; epilog must be postincrement with a positive offset
        ELSE

        IF OffsStr:LEFT:1 == "-"
        INFO    1, "$Name: Postincrement offsets must not be negative"
        MEXIT
        ENDIF

OffsStr SETS    OffsStr:LEFT:(:LEN:OffsStr - 1)
__ParsedOffsetAbs SETA $OffsStr
__ParsedOffsetPreinc SETA 1
__ParsedOffsetRawString SETS "#":CC:OffsStr
__ParsedOffsetString SETS "[sp], #":CC:OffsStr

        ENDIF

        ; standard form
        ELSE

        IF OffsStr:LEFT:1 == "-"
        INFO    1, "$Name: Stack offsets must not be negative"
        MEXIT
        ENDIF

__ParsedOffsetAbs SETA $OffsStr
__ParsedOffsetPreinc SETA 0
__ParsedOffsetRawString SETS "#":CC:OffsStr
__ParsedOffsetString SETS "[sp, #":CC:OffsStr:CC:"]"

        ENDIF

__ParsedOffsetShifted3 SETA __ParsedOffsetAbs:SHR:3 - __ParsedOffsetPreinc
__ParsedOffsetShifted4 SETA __ParsedOffsetAbs:SHR:4 - __ParsedOffsetPreinc

        IF __ParsedOffsetAbs != ((__ParsedOffsetShifted3 + __ParsedOffsetPreinc):SHL:3) || __ParsedOffsetShifted3 >= 0x40
        INFO    1, "$Name: invalid offset $Offset"
        MEXIT
        ENDIF


        MEND


        ;
        ; Compute unwind codes for a register save operation
        ;
        ; Calling macro name is in $Name
        ; Input is in $Reg1, $Offset
        ; Which specifies "Prolog" or "Epilog"
        ; Output is placed in __ComputedCodes
        ;

        MACRO
        __ComputeSaveRegCodes $Name, $Reg1, $Offset, $Which

        LCLA    ByteVal
        LCLA    ByteVal2
        LCLA    ByteVal3
        LCLA    RegNum

        __ParseRegisterNumber $Name, $Reg1
RegNum  SETA    __ParsedRegNumber

        __ParseOffset $Name, $Offset, $Which

        IF (RegNum >= 19) && (RegNum <= 30)
ByteVal SETA 0xd0:OR:(__ParsedOffsetPreinc:SHL:2):OR:((RegNum - 19):SHR:2)
ByteVal2 SETA (((RegNum - 19):AND:3):SHL:6):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2):CC:",0x":CC:((:STR:ByteVal2):RIGHT:2)

        ELIF (RegNum >= 40) && (RegNum <= 47)
ByteVal SETA 0xdc:OR:(__ParsedOffsetPreinc:SHL:1):OR:((RegNum - 40):SHR:2)
ByteVal2 SETA (((RegNum - 40):AND:3):SHL:6):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2):CC:",0x":CC:((:STR:ByteVal2):RIGHT:2)




















        ELSE
        INFO    1, "$Name: Unsupported register: $Reg1"
        ENDIF

        MEND


        ;
        ; Compute unwind codes for a register pair save operation
        ;
        ; Calling macro name is in $Name
        ; Input is in $Reg1, $Reg2, $Offset
        ; Which specifies "Prolog" or "Epilog"
        ; Output is placed in __ComputedCodes
        ;

        MACRO
        __ComputeSaveRegPairCodes $Name, $Reg1, $Reg2, $Offset, $Which

        LCLA    ByteVal
        LCLA    ByteVal2
        LCLA    ByteVal3
        LCLA    RegNum1
        LCLA    RegNum2

        __ParseRegisterNumber $Name, $Reg1
RegNum1 SETA    __ParsedRegNumber

        __ParseRegisterNumber $Name, $Reg2
RegNum2 SETA    __ParsedRegNumber

        __ParseOffset $Name, $Offset, $Which

__RegPairWasFpLr SETL {false}

        IF (RegNum1 == 29) && (RegNum2 == 30)
ByteVal SETA    (0x40+(__ParsedOffsetPreinc*0x40)):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2)
__RegPairWasFpLr SETL {true}

        ELIF (RegNum1 == 19) && (RegNum2 == 20) && (__ParsedOffsetPreinc != 0)
ByteVal SETA    0x20:OR:(__ParsedOffsetShifted3 + 1)
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2)

        ELIF (RegNum1 >= 19) && (RegNum1 <= 30) && (((RegNum1 - 19):AND:1) == 0) && (RegNum2 == 30) && (__ParsedOffsetPreinc == 0)
ByteVal SETA 0xd6:OR:((RegNum1 - 19):SHR:3)
ByteVal2 SETA ((((RegNum1 - 19):SHR:1):AND:3):SHL:6):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2):CC:",0x":CC:((:STR:ByteVal2):RIGHT:2)

        ELIF (RegNum1 >= 19) && (RegNum1 <= 30) && (RegNum2 == (RegNum1 + 1))
ByteVal SETA 0xc8:OR:(__ParsedOffsetPreinc:SHL:2):OR:((RegNum1 - 19):SHR:2)
ByteVal2 SETA (((RegNum1 - 19):AND:3):SHL:6):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2):CC:",0x":CC:((:STR:ByteVal2):RIGHT:2)

        ELIF (RegNum1 >= 40) && (RegNum1 <= 47) && (RegNum2 == (RegNum1 + 1))
ByteVal SETA 0xd8:OR:(__ParsedOffsetPreinc:SHL:1):OR:((RegNum1 - 40):SHR:2)
ByteVal2 SETA (((RegNum1 - 40):AND:3):SHL:6):OR:__ParsedOffsetShifted3
__ComputedCodes SETS "0x":CC:((:STR:ByteVal):RIGHT:2):CC:",0x":CC:((:STR:ByteVal2):RIGHT:2)















        ELSE
        INFO    1, "$Name: Unsupported register pair: $Reg1, $Reg2"
        ENDIF

        MEND


        ;
        ; Compute unwind codes for a stack alloc/dealloc operation
        ;
        ; Output is placed in __ComputedCodes
        ;

        MACRO
        __ComputeStackAllocCodes $Name, $Amount

        LCLA    Shifted
        LCLA    Byte1
        LCLA    Byte2
        LCLA    Byte3

Shifted SETA  ($Amount):SHR:4

        IF Shifted < 0x20
__ComputedCodes SETS "0x":CC:((:STR:Shifted):RIGHT:2)

        ELIF Shifted < 0x800
Byte1   SETA  0xC0:OR:((Shifted:SHR:8):AND:0x7)
Byte2   SETA  Shifted:AND:0xFF
__ComputedCodes SETS "0x":CC:((:STR:Byte1):RIGHT:2):CC:",0x":CC:((:STR:Byte2):RIGHT:2)

        ELIF Shifted < 0x1000000
Byte1   SETA  ((Shifted:SHR:16):AND:0xFF)
Byte2   SETA  ((Shifted:SHR:8):AND:0xFF)
Byte3   SETA  (Shifted:AND:0xFF)
__ComputedCodes SETS "0xE0,0x":CC:((:STR:Byte1):RIGHT:2):CC:",0x":CC:((:STR:Byte2):RIGHT:2):CC:",0x":CC:((:STR:Byte3):RIGHT:2)

        ELSE
        INFO    1, "$Name too large for unwind code encoding"
        ENDIF

        MEND


        ;
        ; Macro for allocating space on the stack in the prolog
        ;

        MACRO
        PROLOG_STACK_ALLOC $Amount

        __ComputeStackAllocCodes "PROLOG_STACK_ALLOC", $Amount

        __EmitRunningLabelAndOpcode sub sp, sp, #$Amount
        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for a single register save operation in a prologue
        ;

        MACRO
        PROLOG_SAVE_REG $Reg1, $Offset

        IF "$Offset" == ""
        INFO    1, "Must specify offset in PROLOG_SAVE_REG"
        MEXIT
        ENDIF

        __ComputeSaveRegCodes "PROLOG_SAVE_REG", $Reg1, $Offset, "Prolog"

        __EmitRunningLabelAndOpcode str $Reg1, $__ParsedOffsetString
        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for an register pair save operation in a prologue
        ;

        MACRO
        PROLOG_SAVE_REG_PAIR $Reg1, $Reg2, $Offset

        IF "$Offset" == ""
        INFO    1, "Must specify offset in PROLOG_SAVE_REG_PAIR"
        MEXIT
        ENDIF

        __ComputeSaveRegPairCodes "PROLOG_SAVE_REG_PAIR", $Reg1, $Reg2, $Offset, "Prolog"

        IF __RegPairWasFpLr







        __EmitRunningLabelAndOpcode stp fp, lr, $__ParsedOffsetString



        IF (__ParsedOffsetAbs != 0) && (__ParsedOffsetPreinc == 0)
        __EmitRunningLabelAndOpcode add fp, sp, $__ParsedOffsetRawString
__ComputedCodes SETS "0xe2,0x":CC:((:STR:__ParsedOffsetShifted3):RIGHT:2):CC:",":CC:__ComputedCodes
        ELSE

        __EmitRunningLabelAndOpcode mov fp, sp
__ComputedCodes SETS "0xe1,":CC:__ComputedCodes
        ENDIF

        ELSE

        __EmitRunningLabelAndOpcode stp $Reg1, $Reg2, $__ParsedOffsetString

        ENDIF

        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Same as above but don't treat FP specially
        ;

        MACRO
        PROLOG_SAVE_REG_PAIR_NO_FP $Reg1, $Reg2, $Offset

        IF "$Offset" == ""
        INFO    1, "Must specify offset in PROLOG_SAVE_REG_PAIR"
        MEXIT
        ENDIF

        __ComputeSaveRegPairCodes "PROLOG_SAVE_REG_PAIR", $Reg1, $Reg2, $Offset, "Prolog"

        IF __RegPairWasFpLr







        __EmitRunningLabelAndOpcode stp fp, lr, $__ParsedOffsetString



        ELSE

        __EmitRunningLabelAndOpcode stp $Reg1, $Reg2, $__ParsedOffsetString

        ENDIF

        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for including an arbitrary operation in the prolog
        ;

        MACRO
        PROLOG_NOP $O1,$O2,$O3,$O4

__ComputedCodes SETS "0xE3"

        __EmitRunningLabelAndOpcode $O1,$O2,$O3,$O4
        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for saving the next pair of registers
        ;

        MACRO
        PROLOG_SAVE_NEXT_PAIR $O1,$O2,$O3,$O4

__ComputedCodes SETS "0xE6"

        __EmitRunningLabelAndOpcode $O1,$O2,$O3,$O4
        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for indicating a trap frame lives above us
        ;

        MACRO
        PROLOG_PUSH_TRAP_FRAME
        __DeclarePrologEnd

__ComputedCodes SETS "0xE8"
        __AppendPrologCodes

        MEND


        ;
        ; Macro for indicating a machine frame lives above us
        ;

        MACRO
        PROLOG_PUSH_MACHINE_FRAME
        __DeclarePrologEnd

__ComputedCodes SETS "0xE9"
        __AppendPrologCodes

        MEND


        ;
        ; Macro for indicating a ARM64_NT_CONTEXT lives above us
        ;

        MACRO
        PROLOG_PUSH_CONTEXT
        __DeclarePrologEnd

__ComputedCodes SETS "0xEA"
        __AppendPrologCodes

        MEND


        ;
        ; Macro for indicating a ARM64EC_NT_CONTEXT lives above us
        ;

        MACRO
        PROLOG_PUSH_EC_CONTEXT
        __DeclarePrologEnd

__ComputedCodes SETS "0xEB"
        __AppendPrologCodes

        MEND


        ;
        ; Macro for signing the return address in the prolog
        ;

        MACRO
        PROLOG_SIGN_RETURN_ADDRESS

__ComputedCodes SETS "0xFC"

        __EmitRunningLabelAndOpcode pacibsp
        __DeclarePrologEnd
        __AppendPrologCodes

        MEND


        ;
        ; Macro for restoring the stack pointer.
        ;

        MACRO
        EPILOG_STACK_RESTORE

__ComputedCodes SETS "0xE1"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode mov sp, fp
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for deallocating space on the stack in the prolog
        ;

        MACRO
        EPILOG_STACK_FREE $Amount

        __ComputeStackAllocCodes "EPILOG_STACK_FREE", $Amount

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode add sp, sp, #$Amount
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for a single integer register restore operation in an epilogue
        ;

        MACRO
        EPILOG_RESTORE_REG $Reg1, $Offset

        IF "$Offset" == ""
        INFO    1, "Must specify offset in EPILOG_RESTORE_REG"
        MEXIT
        ENDIF

        __ComputeSaveRegCodes "EPILOG_RESTORE_REG", $Reg1, $Offset, "Epilog"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode ldr $Reg1, $__ParsedOffsetString
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for an integer register pair restore operation in an epilogue
        ;

        MACRO
        EPILOG_RESTORE_REG_PAIR $Reg1, $Reg2, $Offset

        IF "$Offset" == ""
        INFO    1, "Must specify offset in EPILOG_RESTORE_REG_PAIR"
        MEXIT
        ENDIF

        __ComputeSaveRegPairCodes "EPILOG_RESTORE_REG_PAIR", $Reg1, $Reg2, $Offset, "Epilog"

        __DeclareEpilogStart

        IF __RegPairWasFpLr







        __EmitRunningLabelAndOpcode ldp fp, lr, $__ParsedOffsetString




        ELSE

        __EmitRunningLabelAndOpcode ldp $Reg1, $Reg2, $__ParsedOffsetString

        ENDIF

        __AppendEpilogCodes

        MEND


        ;
        ; Macro for including an arbitrary operation in the epilog
        ;

        MACRO
        EPILOG_NOP $O1,$O2,$O3,$O4

__ComputedCodes SETS "0xE3"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode $O1,$O2,$O3,$O4
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for restoring the next pair of registers
        ;

        MACRO
        EPILOG_RESTORE_NEXT_PAIR $O1,$O2,$O3,$O4

__ComputedCodes SETS "0xE6"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode $O1,$O2,$O3,$O4
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for authenticating the return address in the epilog
        ;

        MACRO
        EPILOG_AUTHENTICATE_RETURN_ADDRESS

__ComputedCodes SETS "0xFC"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode autibsp
        __AppendEpilogCodes

        MEND


        ;
        ; Macro for a bx lr-style return in the epilog
        ;

        MACRO
        EPILOG_RETURN

__ComputedCodes SETS "0xE4"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode ret x30
        __AppendEpilogCodes
        __DeclareEpilogEnd

        MEND

        ;
        ; Emit an opcode indicating that the unwind address of this function
        ; will be at the next instruction after a call rather than the call
        ; instruction. An example where this is necessary is the security cookie
        ; pop code, which modifies the caller's frame.
        ;

        MACRO
        EPILOG_RETURN_CLEAR_UNWIND_TO_CALLER

__ComputedCodes SETS "0xEC, 0XE4"

        __DeclareEpilogStart
        __EmitRunningLabelAndOpcode ret x30
        __EmitRunningLabelAndOpcode nop
        __AppendEpilogCodes
        __DeclareEpilogEnd

        MEND

        ;
        ; Macro to reset the internal uwninding states
        ;

        MACRO
        __ResetUnwindState $ExceptHandler
__PrologUnwindString SETS ""
__EpilogUnwindCount SETA 0
__Epilog1UnwindString SETS ""
__Epilog2UnwindString SETS ""
__Epilog3UnwindString SETS ""
__Epilog4UnwindString SETS ""
__EpilogStartNotDefined SETL {true}
__FuncExceptionHandler SETS ""
        IF "$ExceptHandler" != ""
__FuncExceptionHandler SETS "|$ExceptHandler|"
        ENDIF
        MEND


        ;
        ; Macro to emit the xdata for unwinding
        ;

        MACRO
        __EmitUnwindXData

        LCLA    XBit

XBit    SETA    0
        IF "$__FuncExceptionHandler" != ""
XBit    SETA    1:SHL:20
        ENDIF

        ;
        ; Append terminators where necessary
        ;
        IF __EpilogUnwindCount >= 1
__Epilog1UnwindString SETS __Epilog1UnwindString:RIGHT:(:LEN:__Epilog1UnwindString - 1)
        IF (:LEN:__Epilog1UnwindString) >= 5
        IF __Epilog1UnwindString:RIGHT:4 < "0xE4"
__Epilog1UnwindString SETS __Epilog1UnwindString:CC:",0xE4"
        ENDIF
        ENDIF
        ENDIF

        IF __EpilogUnwindCount >= 2
__Epilog2UnwindString SETS __Epilog2UnwindString:RIGHT:(:LEN:__Epilog2UnwindString - 1)
        IF (:LEN:__Epilog2UnwindString) >= 5
        IF __Epilog2UnwindString:RIGHT:4 < "0xE4"
__Epilog2UnwindString SETS __Epilog2UnwindString:CC:",0xE4"
        ENDIF
        ENDIF
        ENDIF

        IF __EpilogUnwindCount >= 3
__Epilog3UnwindString SETS __Epilog3UnwindString:RIGHT:(:LEN:__Epilog3UnwindString - 1)
        IF (:LEN:__Epilog3UnwindString) >= 5
        IF __Epilog3UnwindString:RIGHT:4 < "0xE4"
__Epilog3UnwindString SETS __Epilog3UnwindString:CC:",0xE4"
        ENDIF
        ENDIF
        ENDIF

        IF __EpilogUnwindCount >= 4
__Epilog4UnwindString SETS __Epilog4UnwindString:RIGHT:(:LEN:__Epilog4UnwindString - 1)
        IF (:LEN:__Epilog4UnwindString) >= 5
        IF __Epilog4UnwindString:RIGHT:4 < "0xE4"
__Epilog4UnwindString SETS __Epilog4UnwindString:CC:",0xE4"
        ENDIF
        ENDIF
        ENDIF

        IF "$__PrologUnwindString" != ""
__PrologUnwindString SETS __PrologUnwindString:CC:"0xE4"
        ELSE
__PrologUnwindString SETS "0xE4"
        ENDIF

        ; optimize out the prolog string if it matches
;        IF (:LEN:__Epilog1UnwindString) >= 6
;        IF __Epilog1UnwindString:LEFT:(:LEN:__Epilog1UnwindString - 4) == __PrologUnwindString:LEFT:(:LEN:__PrologUnwindString - 4)
;__PrologUnwindString SETS ""
;        ENDIF
;        ENDIF

        ;
        ; Switch to the .xdata section, aligned to a DWORD
        ;

        IF __FuncComDat != ""
        AREA    $__FuncXDataArea,ALIGN=2,READONLY,ASSOC=$__FuncArea
        ELSE
        AREA    $__FuncXDataArea,ALIGN=2,READONLY
        ENDIF

        ALIGN   4

        ; declare the xdata header with unwind code size, epilog count,
        ; exception bit, and function length
$__FuncXDataLabel
        DCD     ((($__FuncXDataEndLabel - $__FuncXDataPrologLabel)/4):SHL:27) :OR: (__EpilogUnwindCount:SHL:22) :OR: XBit :OR: (($__FuncEndLabel - $__FuncStartLabel)/4)

        ; if we have an epilogue, output a single scope record
        IF __EpilogUnwindCount >= 1
        DCD     (($__FuncXDataEpilog1Label - $__FuncXDataPrologLabel):SHL:22) :OR: (($__FuncEpilog1StartLabel - $__FuncStartLabel)/4)
        ENDIF
        IF __EpilogUnwindCount >= 2
        DCD     (($__FuncXDataEpilog2Label - $__FuncXDataPrologLabel):SHL:22) :OR: (($__FuncEpilog2StartLabel - $__FuncStartLabel)/4)
        ENDIF
        IF __EpilogUnwindCount >= 3
        DCD     (($__FuncXDataEpilog3Label - $__FuncXDataPrologLabel):SHL:22) :OR: (($__FuncEpilog3StartLabel - $__FuncStartLabel)/4)
        ENDIF
        IF __EpilogUnwindCount >= 4
        DCD     (($__FuncXDataEpilog4Label - $__FuncXDataPrologLabel):SHL:22) :OR: (($__FuncEpilog4StartLabel - $__FuncStartLabel)/4)
        ENDIF

        ; output the prolog unwind string
$__FuncXDataPrologLabel
        DCB     $__PrologUnwindString

        ; if we have an epilogue, output the epilog unwind codes
        IF __EpilogUnwindCount >= 1
$__FuncXDataEpilog1Label
        DCB     $__Epilog1UnwindString
        ENDIF
        IF __EpilogUnwindCount >= 2
$__FuncXDataEpilog2Label
        DCB     $__Epilog2UnwindString
        ENDIF
        IF __EpilogUnwindCount >= 3
$__FuncXDataEpilog3Label
        DCB     $__Epilog3UnwindString
        ENDIF
        IF __EpilogUnwindCount >= 4
$__FuncXDataEpilog4Label
        DCB     $__Epilog4UnwindString
        ENDIF

        ALIGN   4
$__FuncXDataEndLabel

        ; output the exception handler information
        IF "$__FuncExceptionHandler" != ""
        DCD     $__FuncExceptionHandler
        RELOC   2                                       ; make this relative to image base
        DCD     0                                       ; append a 0 for the data (keeps Vulcan happy)
        ENDIF

        ; switch back to the original area
        AREA    $__FuncArea,CODE,READONLY

        MEND



;
; For assembly files that are built for both ARM64 and ARM64EC (discouraged
; since the files might not have been ported to use X64 behavior in the ARM64EC
; paths), use this macro to wrap all references to function names (ASM and C).
;
; For ARM64, this does nothing.
;
; For ARM64EC, this changes FuncName to |#FuncName|.
;












        ;
        ; Global variables
        ;

        ; Current function names and labels
        GBLS    __FuncNameNoBars
        GBLS    __FuncStartLabel
        GBLS    __FuncEpilog1StartLabel
        GBLS    __FuncEpilog2StartLabel
        GBLS    __FuncEpilog3StartLabel
        GBLS    __FuncEpilog4StartLabel
        GBLS    __FuncPDataLabel
        GBLS    __FuncXDataLabel
        GBLS    __FuncXDataPrologLabel
        GBLS    __FuncXDataEpilog1Label
        GBLS    __FuncXDataEpilog2Label
        GBLS    __FuncXDataEpilog3Label
        GBLS    __FuncXDataEpilog4Label
        GBLS    __FuncXDataEndLabel
        GBLS    __FuncEndLabel
        GBLS    __FuncEntryThunkLabel
        GBLS    __FuncExitThunkLabel

        ; other globals relating to the current function
        GBLS    __FuncComDat
        GBLS    __FuncArea
        GBLS    __FuncPDataArea
        GBLS    __FuncXDataArea
        GBLA    __FuncAlignment
__FuncAlignment SETA 4

        ;
        ; Helper macro: generate the various labels we will use internally
        ; for a function
        ;
        ; Output is placed in the various __Func*Label globals
        ;

        MACRO
        __DeriveFunctionLabels $FuncName, $AreaName

__FuncNameNoBars        SETS "$FuncName"
        IF ("$FuncName":LEFT:1 == "|") && ("$FuncName":RIGHT:1 == "|")
__FuncNameNoBars        SETS ("$FuncName":LEFT:(:LEN:"$FuncName" - 1):RIGHT:(:LEN:"$FuncName" - 2))
        ENDIF
__FuncStartLabel        SETS "|$__FuncNameNoBars|"
__FuncEndLabel          SETS "|$__FuncNameNoBars._end|"
__FuncEpilog1StartLabel SETS "|$__FuncNameNoBars._epilog1_start|"
__FuncEpilog2StartLabel SETS "|$__FuncNameNoBars._epilog2_start|"
__FuncEpilog3StartLabel SETS "|$__FuncNameNoBars._epilog3_start|"
__FuncEpilog4StartLabel SETS "|$__FuncNameNoBars._epilog4_start|"
__FuncPDataLabel        SETS "|$__FuncNameNoBars._pdata|"
__FuncXDataLabel        SETS "|$__FuncNameNoBars._xdata|"
__FuncXDataPrologLabel  SETS "|$__FuncNameNoBars._xdata_prolog|"
__FuncXDataEpilog1Label SETS "|$__FuncNameNoBars._xdata_epilog1|"
__FuncXDataEpilog2Label SETS "|$__FuncNameNoBars._xdata_epilog2|"
__FuncXDataEpilog3Label SETS "|$__FuncNameNoBars._xdata_epilog3|"
__FuncXDataEpilog4Label SETS "|$__FuncNameNoBars._xdata_epilog4|"
__FuncXDataEndLabel     SETS "|$__FuncNameNoBars._xdata_end|"
__FuncEntryThunkLabel   SETS "|$__FuncNameNoBars._entry_thunk|"
__FuncArea              SETS "|.text|"
__FuncPDataArea         SETS "|.pdata|"
__FuncXDataArea         SETS "|.xdata|"
        IF "$AreaName" != ""
__FuncArea              SETS "$AreaName"
        ENDIF
        IF __FuncComDat != ""
__FuncArea              SETS __FuncArea:CC:"{|$__FuncNameNoBars|}"
__FuncPDataArea         SETS __FuncPDataArea:CC:"{$__FuncPDataLabel}"
__FuncXDataArea         SETS __FuncXDataArea:CC:"{$__FuncXDataLabel}"
        ENDIF

        MEND


        ;
        ; Helper macro: create a global label for the given name,
        ; decorate it, and export it for external consumption.
        ;

        MACRO
        __ExportName $FuncName

        LCLS    Name

        IF ("$FuncName":LEFT:1 == "|") && ("$FuncName":RIGHT:1 == "|")
Name    SETS    "$FuncName"
        ELSE
Name    SETS    "|$FuncName|"
        ENDIF

        ALIGN   4
        EXPORT  $Name
$Name
        MEND

        MACRO
        __ExportProc $FuncName

        LCLS    Name
Name    SETS    "|$FuncName|"
        ALIGN   4
        EXPORT  $Name
$Name   PROC
        MEND


        ;
        ; Helper macro to set the AREA to the correct answer
        ; for the current function, and configure the alignment.
        ;

        MACRO
        __SetFunctionAreaAndAlign $Alignment

        LCLS    AreaAlign
        LCLS    AlignStmt

        ;
        ; "NOALIGN" is supported to just set the area
        ;

        IF "$Alignment" == "NOALIGN"
        AREA    $__FuncArea

        ;
        ; COMDAT functions must set alignment in the AREA
        ; statement
        ;

        ELIF __FuncComDat != ""

AreaAlign SETS "4"
        IF "$Alignment" != ""
        IF $Alignment > 4
AreaAlign SETS "$Alignment"
        ENDIF
        ENDIF

        AREA    $__FuncArea,CODE,READONLY,ALIGN=$AreaAlign

        ELSE

AlignStmt SETS ""
        IF "$Alignment" != ""
AlignStmt SETS "ALIGN 0x":CC: :STR:(1 << $Alignment)
        ENDIF

        AREA    $__FuncArea,CODE,READONLY
        $AlignStmt

        ENDIF

        MEND


        ;
        ; Helper macro to emit the special DWORD prior to the start of a
        ; function which contains an offset to the ARM64EC entry thunk.
        ; The entry thunk must have been previously defined or else this
        ; macro is a no-op.
        ;

        MACRO
        __AddEntryThunkPointer

        IF :DEF:$__FuncEntryThunkLabel
        ASSERT(__FuncComDat != "")
        ALIGN   (1 << $__FuncAlignment),(1 << $__FuncAlignment) - 4
        dcd     ($__FuncEntryThunkLabel - ({PC} + 4)) + 1
        ENDIF

        MEND


        ;
        ; Declare that all following code/data is to be put in the .text segment
        ;
        ; N.B. The ALIGN attribute here specifies an exponent of base 2; not a
        ;      direct byte count. Thus ALIGN=4 specifies a 16 byte alignment.
        ;

        MACRO
        TEXTAREA
        AREA    |.text|,ALIGN=4,CODE,READONLY
        MEND


        ;
        ; Declare that all following code/data is to be put in the .data segment
        ;

        MACRO
        DATAAREA
        AREA    |.data|,DATA
        MEND


        ;
        ; Declare that all following code/data is to be put in the .rdata segment
        ;

        MACRO
        RODATAAREA
        AREA    |.rdata|,DATA,READONLY
        MEND


        ;
        ; Set/reset the alignment for COMDAT functions that follow.
        ;
        MACRO
        SET_COMDAT_ALIGNMENT $Alignment

__FuncAlignment SETA $Alignment
        IF __FuncAlignment < 4
__FuncAlignment SETA 4
        ENDIF

        MEND


        MACRO
        RESET_COMDAT_ALIGNMENT

__FuncAlignment SETA 4

        MEND


        ;
        ; Macro for indicating the start of a nested function. Nested functions
        ; imply a prolog, epilog, and unwind codes. This macro presumes that
        ; __DeriveFunctionLabels and __SetFunctionAreaAndAlign have already been
        ; called as appropriate
        ;

        MACRO
        NESTED_ENTRY $FuncName, $AreaName, $ExceptHandler, $Alignment

__FuncComDat SETS ""
        __DeriveFunctionLabels $FuncName,$AreaName
        __SetFunctionAreaAndAlign $Alignment
        ASSERT (!:DEF:$__FuncEntryThunkLabel)
        __ResetUnwindState $ExceptHandler
        __ExportProc $__FuncNameNoBars
        ROUT

        MEND


        MACRO
        NESTED_ENTRY_COMDAT $FuncName, $AreaName, $ExceptHandler, $Alignment

        IF ("$Alignment" != "")
        SET_COMDAT_ALIGNMENT $Alignment
        ENDIF

__FuncComDat SETS "COMDAT"
        __DeriveFunctionLabels $FuncName,$AreaName
        __SetFunctionAreaAndAlign $__FuncAlignment
        __AddEntryThunkPointer
        __ResetUnwindState $ExceptHandler
        __ExportProc $__FuncNameNoBars
        ROUT

        MEND


        ;
        ; Generate an ARM64EC entry thunk for an upcoming function.
        ; Note that only COMDAT functions are supported.
        ;


















































































































































        MACRO
        ARM64EC_ENTRY_THUNK $FuncName, $Parameters, $SaveQCount=10, $AreaName
        MEND



        ;
        ; Macro for indicating the end of a nested function. We generate the
        ; .pdata and .xdata records here as necessary.
        ;
        ; Note that the $FuncName parameter is vestigial and not consumed.
        ;

        MACRO
        NESTED_END $FuncName

        ; mark the end of the function
$__FuncEndLabel
        LTORG
        ENDP

        ; generate .pdata

        IF __FuncComDat != ""
        AREA    $__FuncPDataArea,ALIGN=2,READONLY,ASSOC=$__FuncArea
        ELSE
        AREA    $__FuncPDataArea,ALIGN=2,READONLY
        ENDIF

        DCD     $__FuncStartLabel
        RELOC   2                                       ; make this relative to image base

        DCD     $__FuncXDataLabel
        RELOC   2                                       ; make this relative to image base

        ; generate .xdata
        __EmitUnwindXData

        ; back to the original area
        __SetFunctionAreaAndAlign NOALIGN

        ; reset the labels
__FuncStartLabel SETS    ""
__FuncEndLabel  SETS    ""

        MEND


        ;
        ; Macro for indicating the start of a leaf function.
        ;

        MACRO
        LEAF_ENTRY $FuncName, $AreaName, $Alignment

        NESTED_ENTRY $FuncName, $AreaName, "", $Alignment

        MEND


        MACRO
        LEAF_ENTRY_COMDAT $FuncName, $AreaName, $Alignment

        NESTED_ENTRY_COMDAT $FuncName, $AreaName, "", $Alignment

        MEND


        ;
        ; Macro for indicating the end of a leaf function.
        ;

        MACRO
        LEAF_END $FuncName

        NESTED_END $FuncName

        MEND










        ;
        ; Macro for indicating the start of a leaf function.
        ;

        MACRO
        LEAF_ENTRY_NO_PDATA $FuncName, $AreaName

        ; compute the function's labels
        __DeriveFunctionLabels $FuncName,$AreaName
        __SetFunctionAreaAndAlign

        ; export the function name
        __ExportProc $__FuncNameNoBars

        ; flush any pending literal pool stuff
        ROUT

        MEND


        ;
        ; Macro for indicating the end of a leaf function.
        ;

        MACRO
        LEAF_END_NO_PDATA $FuncName

        ; mark the end of the function
$__FuncEndLabel
        LTORG
        ENDP

        ; reset the labels
__FuncStartLabel SETS    ""
__FuncEndLabel  SETS    ""

        MEND




        ;
        ; Macro for indicating an alternate entry point into a function.
        ;

        MACRO
        ALTERNATE_ENTRY $FuncName

        ; export the entry point's name
        __ExportName $FuncName

        ; flush any pending literal pool stuff
        ROUT

        MEND


        ;
        ; Macro for getting the address of a data item.
        ;
        
        MACRO
        ADDROF $Reg, $Variable
        
        adrp    $Reg, $Variable                 ; get the page address first
        add     $Reg, $Reg, $Variable           ; add in the low bits
        
        MEND


        ;
        ; Macro for loading a 32-bit constant.
        ;
        
        MACRO
        MOVL32 $Reg, $Variable
        
        IF ((($Variable):SHR:16):AND:0xffff) == 0
        movz    $Reg, #$Variable
        ELIF ((($Variable):SHR:0):AND:0xffff) == 0
        movz    $Reg, #((($Variable):SHR:16):AND:0xffff), lsl #16
        ELSE
        movz    $Reg, #(($Variable):AND:0xffff)
        movk    $Reg, #((($Variable):SHR:16):AND:0xffff), lsl #16
        ENDIF
        
        MEND


















































        MACRO
        CAPSTART $arg1, $arg2
        MEND

        MACRO
        CAPEND $arg1
        MEND




        ;
        ; Macro to align a Control Flow Guard valid call target.
        ; Not necessary to use this before functions anymore as
        ; it is the default for NESTED_ENTRY/LEAF_ENTRY macros.
        ;

        MACRO
        CFG_ALIGN
        ALIGN 16
        MEND


        ;
        ; Macro to perform a Control Flow Guard check on a live call target.
        ;
        ; $TargetReg - Target address register
        ; x16 - Bitmap address
        ; $FailLabel - Label to jump to in the event of a failure
        ; 
        ; N.B. x16-x17 are free, other registers should be treated as live.
        ;
        ; N.B. This macro expects the $FailLabel to be right after this macro.
        ;

        MACRO
        CFG_ICALL_CHECK_BITMAP $TargetReg, $ValidTarget, $FailLabel

        LCLS    ValidLabel

ValidLabel SETS ""

        IF ("$ValidTarget":LEFT:1 != "x" && "$ValidTarget":LEFT:1 != "w" && "$ValidTarget" != "lr")

ValidLabel SETS "$ValidTarget"

        ENDIF

;
; Bitmap is an array of 2-bit values. Each 2-bit value represents 16 bytes, with the low
; bit set if jumps are permitted, and the upper bit set if misaligned jumps are permitted.
;
; The bit index is (address >> 4).
; Each byte holds 4 entries, so the byte index is (address >> 6).
; The shift amount is computed as 2*((address >> 4) & 3), or (address >> 3) & 6
; If address is aligned, (address >> 3) & 7 is equivalent, and ubfx can be used to extract.
;

        lsr     x17, $TargetReg, #6             ; compute bitmap byte index
        tst     $TargetReg, #15                 ; misaligned address?
        ldrb    w17, [x16, x17]                 ; load byte from bitmap
        ubfx    x16, $TargetReg, #3, #3         ; compute bit index*2
        bne     %F2                             ; if misaligned, account for extra bits
        lsr     x17, x17, x16                   ; shift bitmap chunk over to valid align bit
        tbz     x17, #0, %F3                    ; if low bit not set, verify the upper bit to
                                                ; allow an export suppressed target
1 ; Valid

;
; "ret lr" and "br lr" have different encodings and use of "ret lr" is
; preferred to hint that it is returning to the caller.
;

        IF "$ValidLabel" != ""
            b       $ValidLabel                 ; jump to valid target
        ELIF "$ValidTarget" != "lr"
            br      $ValidTarget                ; jump to valid target
        ELSE
            ret     lr                          ; return
        ENDIF

2 ; Misaligned
        ;
        ; Code on ARM64 should always be 16 byte aligned if address taken.
        ;
        ; TODO:  Compress bitmap format to 1 bit per address on ARM64?
        ; TODO:  We are seeing 8-byte aligned code now. (1/22/14)
        ;
        and     x16, x16, #0xfffffffffffffffe   ; force low bit of shift to 0
        lsr     x17, x17, x16                   ; shift bitmap chunk down
        tbz     x17, #0, $FailLabel             ; invalid if the low bit was clear
3 ; FailOpen
        tbnz    x17, #1, %B1                    ; valid if upper bit was set as well

        MEND


        ;
        ; Macro to perform a Control Flow Guard check on a live call target with export suppression.
        ;
        ; $TargetReg - Target address register
        ; x16 - Bitmap address
        ; $FailLabel - Label to jump to in the event of a failure
        ; 
        ; N.B. x16-x17 are free, other registers should be treated as live.
        ;
        ; N.B. This macro expects the $FailLabel to be right after this macro.
        ;

        MACRO
        CFG_ICALL_CHECK_BITMAP_ES $TargetReg, $ValidTarget, $FailLabel

        LCLS    ValidLabel

ValidLabel SETS ""

        IF ("$ValidTarget":LEFT:1 != "x" && "$ValidTarget":LEFT:1 != "w" && "$ValidTarget" != "lr")

ValidLabel SETS "$ValidTarget"

        ENDIF

;
; Bitmap is an array of 2-bit values. Each 2-bit value represents 16 bytes, with the low
; bit set if jumps are permitted, and the upper bit set if misaligned jumps are permitted.
;
; The bit index is (address >> 4).
; Each byte holds 4 entries, so the byte index is (address >> 6).
; The shift amount is computed as 2*((address >> 4) & 3), or (address >> 3) & 6
; If address is aligned, (address >> 3) & 7 is equivalent, and ubfx can be used to extract.
;

        lsr     x17, $TargetReg, #6             ; compute bitmap byte index
        tst     $TargetReg, #15                 ; misaligned address?
        ldrb    w17, [x16, x17]                 ; load byte from bitmap
        ubfx    x16, $TargetReg, #3, #3         ; compute bit index*2
        bne     %F2                             ; if misaligned, account for extra bits
        lsr     x17, x17, x16                   ; shift bitmap chunk over to valid align bit
        tbz     x17, #0, $FailLabel             ; if low bit not set, either invalid or export
                                                ; suppressed target
1 ; Valid

;
; "ret lr" and "br lr" have different encodings and use of "ret lr" is
; preferred to hint that it is returning to the caller.
;

        IF "$ValidLabel" != ""
            b       $ValidLabel                 ; jump to valid target
        ELIF "$ValidTarget" != "lr"
            br      $ValidTarget                ; jump to valid target
        ELSE
            ret     lr                          ; return
        ENDIF

2 ; Misaligned
        ;
        ; Code on ARM64 should always be 16 byte aligned if address taken.
        ;
        ; TODO:  Compress bitmap format to 1 bit per address on ARM64?
        ; TODO:  We are seeing 8-byte aligned code now. (1/22/14)
        ;
        and     x16, x16, #0xfffffffffffffffe   ; force low bit of shift to 0
        lsr     x17, x17, x16                   ; shift bitmap chunk down
        tbz     x17, #0, $FailLabel             ; invalid if the low bit was clear
        tbnz    x17, #1, %B1                    ; valid if upper bit was set as well
        MEND


        ;
        ; Macro to acquire a spin lock at address $Reg + $Offset. Clobbers {r0-r2}
        ;

; ARM64_WORKITEM : should we use acquire/release semantics instead of DMB?

        MACRO
        ACQUIRE_SPIN_LOCK $Reg, $Offset

        mov     x0, #1                                  ; we want to exchange with a 1
        dmb                                             ; memory barrier ahead of the loop
1
        ldxr    x1, [$Reg, $Offset]                     ; load the new value
        stxr    x2, x0, [$Reg, $Offset]                 ; attempt to store the 1
        cbnz    x2, %B1                                 ; did we succeed before someone else did?
        cbz     x1, %F3                                 ; was the lock previously owned? if not, we're done
        yield                                           ; yield execution
        b       %B1                                     ; and try again
3
        dmb

        MEND


        ;
        ; Macro to release a spin lock at address $Reg + $Offset.
        ;

; ARM64_WORKITEM : should we use acquire/release semantics instead of DMB?

        MACRO
        RELEASE_SPIN_LOCK $Reg, $Offset

        dmb
        str     xzr, [$Reg, $Offset]                    ; store 0

        MEND


        ;
        ; Macro to increment a 64-bit statistic.
        ;

        MACRO
        INCREMENT_STAT $AddrReg, $Temp1, $Temp2

1       ldxr    $Temp1, [$AddrReg]                      ; load current value
        add     $Temp1, $Temp1, #1                      ; increment
        stxr    $Temp2, $Temp1, [$AddrReg]              ; attempt to store
        cbnz    $Temp2, %B1                             ; loop until it works?

        MEND


        ;
        ; Macros to enable/disable interrupts.
        ;
        
        MACRO
        ENABLE_INTERRUPTS
        msr     DAIFClr, #2                             ; enable interrupts
        MEND

        MACRO
        DISABLE_INTERRUPTS
        msr     DAIFSet, #2                             ; disable interrupts
        MEND


        ;
        ; Macros to read/write the current IRQL
        ;
        ; N.B. These macros do not do hardware and software IRQL processing.
        ;

        MACRO
        GET_IRQL $Irql
        ldrb    $Irql, [x18, #0x38]            ; read IRQL
        MEND

        MACRO
        RAISE_IRQL $Reg, $NewIrql











        mov     $Reg, #$NewIrql                         ; get new IRQL
        strb    $Reg, [x18, #0x38]             ; update IRQL
        MEND


        ;
        ; Macros to output special undefined opcodes that indicate breakpoints
        ; and debug services.
        ;

        MACRO
        EMIT_BREAKPOINT
        brk     #0xf000
        MEND


        MACRO
        EMIT_DEBUG_SERVICE
        brk     #0xf002
        MEND

        MACRO
        FASTFAIL $FastFailCode
        mov     x0, $FastFailCode
        brk     #0xf003
        MEND


        ;
        ; Macro to generate an exception frame; this is intended to
        ; be used within the prolog of a function.
        ;

        MACRO
        GENERATE_EXCEPTION_FRAME
        PROLOG_SAVE_REG_PAIR x19, x20, #-96!
        PROLOG_SAVE_REG_PAIR x21, x22, #16
        PROLOG_SAVE_REG_PAIR x23, x24, #32
        PROLOG_SAVE_REG_PAIR x25, x26, #48
        PROLOG_SAVE_REG_PAIR x27, x28, #64
        PROLOG_SAVE_REG_PAIR fp, lr, #80
        MEND


        ;
        ; Macro to restore from an exception frame; this is intended to
        ; be used within the epilog of a function.
        ;

        MACRO
        RESTORE_EXCEPTION_STATE
        EPILOG_RESTORE_REG_PAIR fp, lr, #80
        EPILOG_RESTORE_REG_PAIR x27, x28, #64
        EPILOG_RESTORE_REG_PAIR x25, x26, #48
        EPILOG_RESTORE_REG_PAIR x23, x24, #32
        EPILOG_RESTORE_REG_PAIR x21, x22, #16
        EPILOG_RESTORE_REG_PAIR x19, x20, #96!
        MEND


        ;
        ; Macro to ensure that any eret is followed by barriers to
        ; prevent speculation
        ;
        MACRO
        ERET_FIX
        eret
        dsb sy
        isb sy
        MEND




        ;
        ; Given the Address the EC code range bitmap to determine if it is EC or X64.
        ;
        ; Sets the Zero Flag for X64 targets, clear the Zero Flag for EC targets.
        ;
        ; T1 T2 are scratch registers provided by the caller.
        ;

        MACRO
        EC_BITMAP_LOOKUP $xAddress, $xT1, $wT1, $xT2

        ldr    $xT1, [x18, #0x60]      ; PEB
        lsr    $xT2, $xAddress, #15     ; each byte of bitmap indexes 8*4K = 2^15 byte span
        ldr    $xT1, [$xT1, #0x368]
        ldrb   $wT1, [$xT1, $xT2]       ; load the bitmap byte for the 8*4K span
        ubfx   $xT2, $xAddress, #12, #3 ; index to the 4K page within the 8*4K span
        lsr    $xT1, $xT1, $xT2
        tst    $xT1, #1                 ; test the specific page

        MEND

        ;
        ; Convert NZCV flags in the $Nzcv register into equivalent
        ; AMD64 EFLAGS in the $Eflags register. The source register
        ; is consumed as part of this operation.
        ;

        MACRO
        CONVERT_NZCV_TO_EFLAGS $Eflags, $Nzcv

        mov     $Eflags, #0x0202                ; start with IF=1 and hardcoded bit 1 set

        lsr     $Nzcv, $Nzcv, #21               ; move SS (21) flag to bottom
        bfi     $Eflags, $Nzcv, #8, #1          ; insert at bit 8 (T)
        lsr     $Nzcv, $Nzcv, #7                ; move V (28) flag to bottom
        bfi     $Eflags, $Nzcv, #11, #1         ; insert at bit 11 (O)
        lsr     $Nzcv, $Nzcv, #1                ; move C (29) flag to bottom
        bfi     $Eflags, $Nzcv, #0, #1          ; insert at bit 0 (C)
        lsr     $Nzcv, $Nzcv, #1                ; move Z:N (30:31) flags to bottom
        bfi     $Eflags, $Nzcv, #6, #2          ; insert at bits 6:7 (Z:S)

        MEND

        ;
        ; Convert control and status flags in FPCR and FPSR formats
        ; (in $Fpcr and $Fpsr registers) into a combined AMD64 MXCSR
        ; register. The two source registers are consumed as part of
        ; this operation.
        ;

        MACRO
        CONVERT_FPCR_FPSR_TO_MXCSR $Mxcsr, $Fpcr, $Fpsr

        ;
        ; Status flags map 1:1, if set indicates an exception occured.
        ;

        and     $Mxcsr, $Fpsr, #1               ; MXCSR.IE[0] = FPSR.IOC[0]
        lsr     $Fpsr, $Fpsr, #1                ; remove low bit
        bfi     $Mxcsr, $Fpsr, #2, #4           ; MXCSR.ZE/OE/UE/PE[2:6] = FPSR.DZC/OFC/UFC/IXC[1:4]
        lsr     $Fpsr, $Fpsr, #6                ; remove those bits and the next 2
        bfi     $Mxcsr, $Fpsr, #1, #1           ; MXCSR.DE[1] = FPSR.IDC[7]

        ;
        ; Exception enable bit map 1:1, however on X64 set means mask (disable)
        ; while on ARM64 set means enable the exception.  Thus the bit inversion.
        ;

        lsr     $Fpcr, $Fpcr, #8                ; skip low 8 bits
        eor     $Fpcr, $Fpcr, #0xff             ; invert the masking bits
        bfi     $Mxcsr, $Fpcr, #7, #1           ; MXCSR.IM[7] = ~FPCR.IOE[8]
        lsr     $Fpcr, $Fpcr, #1                ; remove low bit
        bfi     $Mxcsr, $Fpcr, #9, #4           ; MXCSR.ZM/OM/UM/PM[9:12] = ~FPCR.DZE/OFE/UFE/IXE[9:12]
        lsr     $Fpcr, $Fpcr, #6                ; remove those bits and the next 2
        bfi     $Mxcsr, $Fpcr, #8, #1           ; MXCSR.DM[8] = ~FPCR.IDE[15]

        ;
        ; Denormals Are Zeros has not direct mapping on ARM64, use the FZ16 bit
        ; since half-precision floats do not exist in SSE.
        ;

        lsr     $Fpcr, $Fpcr, #4                ; move bit 19 down low
        bfi     $Mxcsr, $Fpcr, #6, #1           ; MXCSR.DAZ[6] = FPCR.FZ[19]

        ;
        ; Rounding modes are the same on X64 and ARM64 except bit swapped in representation.
        ; X64: 00=nearest 01=down 10=up 11=truncate
        ; A64: 00=nearest 10=down 01=up 11=truncate
        ;

        lsr     $Fpcr, $Fpcr, #3                ; move bit 22 down low
        bfi     $Mxcsr, $Fpcr, #14, #1          ; MXCSR.RC[13:14](upper) = FPCR.RMODE[22:23](lower)
        lsr     $Fpcr, $Fpcr, #1                ; get upper bit of rmode in LSB
        bfi     $Mxcsr, $Fpcr, #13, #1          ; MXCSR.RC[13:14](lower) = FPCR.RMODE[22:23](upper)

        ;
        ; Flush To Zero bit maps 1:1
        ;

        lsr     $Fpcr, $Fpcr, #1                ; shift bit 24 down low
        bfi     $Mxcsr, $Fpcr, #15, #1          ; MXCSR.FZ = FPCR.FZ

        MEND

;

	OPT	1 





	IMPORT ffi_closure_SYSV_inner
	EXPORT	ffi_call_SYSV
	EXPORT	ffi_closure_SYSV_V
	EXPORT	ffi_closure_SYSV
	EXPORT	extend_hfa_type
	EXPORT	compress_hfa_type





	TEXTAREA, ALIGN=8














	NESTED_ENTRY ffi_call_SYSV_fake

	
	PROLOG_SAVE_REG_PAIR	x29, x30, #-32!

	ALTERNATE_ENTRY ffi_call_SYSV
	
	stp	x29, x30, [x1]
	mov	x29, x1
	mov	sp, x0

	mov	x9, x2			
	mov	x8, x3			



	stp	x3, x4, [x29, #16]	
	
	
	tbz	x4, #7, ffi_call_SYSV_L1
	ldp	q0, q1, [sp, #0]
	ldp	q2, q3, [sp, #32]
	ldp	q4, q5, [sp, #64]
	ldp	q6, q7, [sp, #96]

ffi_call_SYSV_L1
	

	ldp     x0, x1, [sp, #16*8 + 0]
	ldp     x2, x3, [sp, #16*8 + 16]
	ldp     x4, x5, [sp, #16*8 + 32]
	ldp     x6, x7, [sp, #16*8 + 48]

	
	add	sp, sp, #(8 * 16 + 8 * 8)	

	blr     x9			

	ldp	x3, x4, [x29, #16]	

	
	mov     sp, x29 
	ldp     x29, x30, [x29]

	
	adr	x5, ffi_call_SYSV_return
	and	w4, w4, #31
	add	x5, x5, x4, lsl #3
	br	x5
	
	



	ALIGN 4
ffi_call_SYSV_return
	ret				
	nop
	str	x0, [x3]		
	ret
	stp	x0, x1, [x3]		
	ret
	brk	#1000			
	ret
	brk	#1000			
	ret
	brk	#1000			
	ret
	brk	#1000			
	ret
	brk	#1000			
	ret
	st4	{ v0.s, v1.s, v2.s, v3.s }[0], [x3]	
	ret
	st3	{ v0.s, v1.s, v2.s }[0], [x3]	
	ret
	stp	s0, s1, [x3]		
	ret
	str	s0, [x3]		
	ret
	st4	{ v0.d, v1.d, v2.d, v3.d }[0], [x3]	
	ret
	st3	{ v0.d, v1.d, v2.d }[0], [x3]	
	ret
	stp	d0, d1, [x3]		
	ret
	str	d0, [x3]		
	ret
	str	q3, [x3, #48]		
	nop
	str	q2, [x3, #32]		
	nop
	stp	q0, q1, [x3]		
	ret
	str	q0, [x3]		
	ret
	uxtb	w0, w0			
	str	x0, [x3]
	ret				
	nop
	uxth	w0, w0			
	str	x0, [x3]
	ret				
	nop
	mov	w0, w0			
	str	x0, [x3]
	ret				
	nop
	sxtb	x0, w0			
	str	x0, [x3]
	ret				
	nop
	sxth	x0, w0			
	str	x0, [x3]
	ret				
	nop
	sxtw	x0, w0			
	str	x0, [x3]
	ret				
	nop
	
	
	NESTED_END ffi_call_SYSV_fake
	














	NESTED_ENTRY	ffi_closure_SYSV_V
	PROLOG_SAVE_REG_PAIR	x29, x30, #-(8*2 + (8 * 16 + 8 * 8) + 64)!

	
	stp	q0, q1, [sp, #16 + 0]
	stp	q2, q3, [sp, #16 + 32]
	stp	q4, q5, [sp, #16 + 64]
	stp	q6, q7, [sp, #16 + 96]

	b	ffi_closure_SYSV_save_argument
	NESTED_END	ffi_closure_SYSV_V

	NESTED_ENTRY	ffi_closure_SYSV
	PROLOG_SAVE_REG_PAIR	x29, x30, #-(8*2 + (8 * 16 + 8 * 8) + 64)!

ffi_closure_SYSV_save_argument
	
	stp     x0, x1, [sp, #16 + 16*8 + 0]
	stp     x2, x3, [sp, #16 + 16*8 + 16]
	stp     x4, x5, [sp, #16 + 16*8 + 32]
	stp     x6, x7, [sp, #16 + 16*8 + 48]

	
	ldp	x0, x1, [x17, #24]	
	ldr	x2, [x17, #24+8*2]	

do_closure
	add	x3, sp, #16							
	add	x4, sp, #(8*2 + (8 * 16 + 8 * 8) + 64)		
	add	x5, sp, #16+(8 * 16 + 8 * 8)		
	mov	x6, x8					

	bl	ffi_closure_SYSV_inner

	
	adr	x1, ffi_closure_SYSV_return_base
	and	w0, w0, #31
	add	x1, x1, x0, lsl #3
	add	x3, sp, #16+(8 * 16 + 8 * 8)
	br	x1

	
	ALIGN	8
ffi_closure_SYSV_return_base
	b	ffi_closure_SYSV_epilog			
	nop
	ldr	x0, [x3]		
	b	ffi_closure_SYSV_epilog
	ldp	x0, x1, [x3]		
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	brk	#1000			
	nop
	brk	#1000			
	nop
	brk	#1000			
	nop
	brk	#1000			
	nop
	ldr	s3, [x3, #12]		
	nop
	ldr	s2, [x3, #8]		
	nop
	ldp	s0, s1, [x3]		
	b	ffi_closure_SYSV_epilog
	ldr	s0, [x3]		
	b	ffi_closure_SYSV_epilog
	ldr	d3, [x3, #24]		
	nop
	ldr	d2, [x3, #16]		
	nop
	ldp	d0, d1, [x3]		
	b	ffi_closure_SYSV_epilog
	ldr	d0, [x3]		
	b	ffi_closure_SYSV_epilog
	ldr	q3, [x3, #48]		
	nop
	ldr	q2, [x3, #32]		
	nop
	ldp	q0, q1, [x3]		
	b	ffi_closure_SYSV_epilog
	ldr	q0, [x3]		
	b	ffi_closure_SYSV_epilog
	ldrb	w0, [x3, #0]	
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	ldrh	w0, [x3, #0]	
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	ldr	w0, [x3, #0]	
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	ldrsb	x0, [x3, #0]	
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	ldrsh	x0, [x3, #0]	
	b	ffi_closure_SYSV_epilog
	brk	#1000			
	nop
	ldrsw	x0, [x3, #0]	
	nop
					

ffi_closure_SYSV_epilog
	EPILOG_RESTORE_REG_PAIR	x29, x30, #(8*2 + (8 * 16 + 8 * 8) + 64)!
	EPILOG_RETURN
	NESTED_END	ffi_closure_SYSV



































	LEAF_ENTRY	extend_hfa_type

	adr	x3, extend_hfa_type_jump_base
	and	w2, w2, #31
	sub	x2, x2, #8
	add	x3, x3, x2, lsl #4
	br	x3

	ALIGN	4
extend_hfa_type_jump_base
	ldp	s16, s17, [x1]		
	ldp	s18, s19, [x1, #8]
	b	extend_hfa_type_store_4
	nop

	ldp	s16, s17, [x1]		
	ldr	s18, [x1, #8]
	b	extend_hfa_type_store_3
	nop

	ldp	s16, s17, [x1]		
	b	extend_hfa_type_store_2
	nop
	nop

	ldr	s16, [x1]		
	b	extend_hfa_type_store_1
	nop
	nop

	ldp	d16, d17, [x1]		
	ldp	d18, d19, [x1, #16]
	b       extend_hfa_type_store_4
	nop

	ldp     d16, d17, [x1]		
	ldr     d18, [x1, #16]
	b	extend_hfa_type_store_3
	nop

	ldp	d16, d17, [x1]		
	b	extend_hfa_type_store_2
	nop
	nop

	ldr	d16, [x1]		
	b	extend_hfa_type_store_1
	nop
	nop

	ldp	q16, q17, [x1]		
	ldp	q18, q19, [x1, #16]
	b	extend_hfa_type_store_4
	nop

	ldp	q16, q17, [x1]		
	ldr	q18, [x1, #16]
	b	extend_hfa_type_store_3
	nop

	ldp	q16, q17, [x1]		
	b	extend_hfa_type_store_2
	nop
	nop

	ldr	q16, [x1]		
	b	extend_hfa_type_store_1

extend_hfa_type_store_4
	str	q19, [x0, #48]
extend_hfa_type_store_3
	str	q18, [x0, #32]
extend_hfa_type_store_2
	str	q17, [x0, #16]
extend_hfa_type_store_1
	str	q16, [x0]
	ret

	LEAF_END	extend_hfa_type




	LEAF_ENTRY	compress_hfa_type

	adr	x3, compress_hfa_type_jump_base
	and	w2, w2, #31
	sub	x2, x2, #8
	add	x3, x3, x2, lsl #4
	br	x3

	ALIGN	4
compress_hfa_type_jump_base
	ldp	q16, q17, [x1]		
	ldp	q18, q19, [x1, #32]
	st4	{ v16.s, v17.s, v18.s, v19.s }[0], [x0]
	ret

	ldp	q16, q17, [x1]		
	ldr	q18, [x1, #32]
	st3	{ v16.s, v17.s, v18.s }[0], [x0]
	ret

	ldp	q16, q17, [x1]		
	st2	{ v16.s, v17.s }[0], [x0]
	ret
	nop

	ldr	q16, [x1]		
	st1	{ v16.s }[0], [x0]
	ret
	nop

	ldp	q16, q17, [x1]		
	ldp	q18, q19, [x1, #32]
	st4	{ v16.d, v17.d, v18.d, v19.d }[0], [x0]
	ret

	ldp	q16, q17, [x1]		
	ldr	q18, [x1, #32]
	st3	{ v16.d, v17.d, v18.d }[0], [x0]
	ret

	ldp	q16, q17, [x1]		
	st2	{ v16.d, v17.d }[0], [x0]
	ret
	nop

	ldr	q16, [x1]		
	st1	{ v16.d }[0], [x0]
	ret
	nop

	ldp	q16, q17, [x1]		
	ldp	q18, q19, [x1, #32]
	b	compress_hfa_type_store_q4
	nop

	ldp	q16, q17, [x1]		
	ldr	q18, [x1, #32]
	b	compress_hfa_type_store_q3
	nop

	ldp	q16, q17, [x1]		
	stp	q16, q17, [x0]
	ret
	nop

	ldr	q16, [x1]		
	str	q16, [x0]
	ret

compress_hfa_type_store_q4
	str	q19, [x0, #48]
compress_hfa_type_store_q3
	str	q18, [x0, #32]
	stp	q16, q17, [x0]
	ret

	LEAF_END	compress_hfa_type

	END
