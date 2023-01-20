; ============================================================================
; FIQ code.
; ============================================================================

.include "config.h.asm"

.org 0x18

FIQ_startofcode:
   TEQP      PC,#0b11<<26 | 0b01  ; 1 18 keep IRQs and FIQs off, change to FIQ mode
   MOV       R0,R0                ; 2 1C nop to sync FIQ registers

   ; FIQ registers
   ;
   ; R8_FIQ=temp reg 1
   ; R9_FIQ=table 1
   ; R10_FIQ=table 2
   ; R11_FIQ=table 3 & table 4
   ; R12_FIQ=memc table
   ; R13_FIQ=line count
   ; R14_FIQ=temp reg 2/set to IOC addr on exit

   LDRB      R8,[R14,#0x14+0]     ; 3 20 load irq_A triggers ***BUG to v0.13*** v0.14 read 0x14+0
                                 ;      was reading status at 0x10, which ignores IRQ mask!!!
   TST       R8,#0b01000000       ; 4 24 bit 3 = Vsync, bit 6 = T1 trigger (HSync)
   LDREQ     PC,FIQ_notHSync     ; 5 28 *v0.14 if not T1, then go to VSync/Keyboard code*

   STRB      R8,[R14,#0x14+2]     ; 6 2C (v0.14 moved past branch) clear all interrupt triggers

   MOV       R14,#FIQ_tempstack  ; 7 30 ** n/r if only 4 VIDC
   STMIA     R14,{R4-R7}         ; 8 34 ** n/r if only 4 VIDC
   MOV       R8,#0x3400000        ; 9 38
   ;LDMIA     R9!,{R10,R11,R12,R14}         ;10 3C load 4 VIDC parameters
   ;STMIA     R8,{R10,R11,R12,R14}          ;11 40 store 4
   LDMIA     R9!,{R4-R7}         ;10 3C load 4 VIDC parameters
   STMIA     R8,{R4-R7}          ;11 40 store 4
   LDMIA     R10!,{R4-R7}        ;12 44
   STMIA     R8,{R4-R7}          ;13 48 ...8
   LDMIA     R11!,{R4-R7}        ;14 4C
   STMIA     R8,{R4-R7}          ;15 50 ...12
   LDMIA     R11!,{R4-R7}        ;16 54
   STMIA     R8,{R4-R7}          ;17 58 ...16

   LDMIA     R12!,{R4-R5}        ;18 5C load 2 MEMC paramters
   CMP       R4,#0x3600000        ;19 60
   STRGE     R4,[R4]             ;20 64 it's a MEMC reg, so write
   CMP       R5,#0x3600000        ;21 68
   STRGE     R5,[R5]             ;22 6C it's a MEMC reg, so write

   LDMIA     R14,{R4-R7}         ;23 70
   MOV       R14,#0x3200000       ;24 74 reset R14 to IOC address
   STRB      R14,[R14,#0x28+2]    ;25 78 *v0.14* set IRQB mask to 0b00000000 = no STx, SRx IRQs now
   ;*************************************************************************

   SUBS      R13,R13,#1             ;26 7C
   TEQGTP    PC,#0b000011<<26 | 0b10 ;27 80 back to IRQ mode, maintain 'GT', Z clear
   MOV       R0,R0                  ;28 84 sync IRQ registers
   SUBGTS    PC,R14,#4              ;29 88 return to foreground

   ; only get here (EQ) if at last line on screen

   MOV       R8,#0b00001000       ;30 8C
   STRB      R8,[R14,#0x18+2]     ;31 90 set IRQA mask to 0b00001000 = VSync only n/r unless likely to do <256?

   MOV       R8,#vsyncreturn_lowbyte;32 94   or ldr r8,vsyncvalue
   STRB      R8,[R14,#0x50+2]               ;33 98 T1 low byte, +2 for write
   MOV       R8,#vsyncreturn_highbyte;34 9C   or mov r8,r8,lsr#8
   STRB      R8,[R14,#0x54+2]               ;35 A0 T1 high byte, +2 for write
   STRB      R8,[R14,#0x58+2]               ;36 A4 T1_go = reset T1

FIQ_exitcode:
   TEQP      PC,#0b000011<<26 | 0b10 ;37 A8 back to IRQ mode
   MOV       R0,R0                  ;38 AC sync IRQ registers
   SUBS      PC,R14,#4              ;39 90 return to foreground

.org FIQ_notHSync - 0x18 

   .long      0                      ;43 0xC0 pointer to notHSync ***quad aligned***
   .long      0                      ;44 0xC4 n/r
   .long      0                      ;45 0xC8 n/r
   .long      0                      ;46 0xCC n/r

    ; FIQ_tempstack:

   .long      0                      ;47 0xD0 R4 ***quad aligned***
   .long      0                      ;48 0xD4 R5
   .long      0                      ;49 0xD8 R6
   .long      0                      ;50 0xDC R7

   .long      0                      ;51 0xE0 n/r
   .long      0                      ;52 0xE4 n/r
   .long      0                      ;53 0xE8 n/r
   .long      0                      ;54 0xEC n/r
   .long      0                      ;55 0xF0 n/r
   .long      0                      ;56 0xF4 n/r
   .long      0                      ;57 0xF8 n/r

   .byte      "RstM"                 ;58 0xFC

FIQ_endofcode:

;.equ fiqinsts, (fiqend-fiqbase)/4
; PRINT"Number of FIQ instructions: ";fiqinsts
;.if fiqinsts>58
;.error "FIQ code too large"
;.endif
