; ============================================================================
; Rasterman Config.
; ============================================================================

.equ _VERSION_NUMBER, 300
.equ _VERSION_STRING, 0         ; TODO: How to do this?
.equ _DATE_STRING, 0            ; TODO: How to do this?

.equ irqtest, 0
.equ alphatest, 1
.equ vga, 0

.equ QTMblock, 44+128

; ============================================================================
.if vga

; PRINT"Assembling VGA version";

 .equ hsyncline, 64-1            ; 63 = timing for 1 line (decreases at 2MHz)
                                 ; (latch+1)/2uS = 64/2 = 32 uS = 0.000032 S
                                 ; 1/0.000032 = 31250 Hz = LCDgm VGA line rate :)
 
  .if irqtest
  .equ ylines, 1                  ; only 1 line for the test
  .equ synctest, (hsyncline+1)*0  ; counts include 0? so add one if multiplying lines
  .equ syncoffset, 48/2
                                 ; -48/2 is ~ time from end of prev to start of next
  .equ vsyncreturn, 12160+16/2-1  ; 12160=190 lines=exact time to start of top line (VCR 446-VDER 294+VDSR 38=190)
                                 ;          +16/2 is time from start of line to start of first pixel

  .equ vsyncreturn, vsyncreturn+synctest     ; move a few lines down, so we can see easily...
  .equ vsyncreturn, vsyncreturn-syncoffset   ; -48/2 = end of previous line
 .else
  .equ ylines, 256                     ; skip last few
  .equ vsyncreturn, 12160+16/2-1-48/2  ; 7168+16-1-48 = end of previous line (Arm2)
  .equ vsyncreturn, vsyncreturn+6/2    ; was 7/2 .. 12 = either side of 1/2 way v0.07 fudge to shift colours right a bit
 .endif
; ============================================================================
.else
; ============================================================================

; PRINT"Assembling TV resolution version";

 .equ hsyncline, 128-1            ; 127 = timing for 1 line (decreases at 2MHz)
                                 ; (latch+1)/2uS = 128/2 = 64 uS = 0.000064 S
                                 ; 1/0.000064 = 15625 Hz = TV line rate :)

 .if irqtest
  .equ ylines, 1                 ; only 1 line for the test
  .equ synctest, (hsyncline+1)*0 ; counts include 0? so add one if multiplying lines
  .equ syncoffset, 48
                                 ; -48 is ~ time from end of prev to start of next
  .equ vsyncreturn, 7168+16-1    ; 7168 = 56 lines = exact time to start of top line (VCR-VDER+VDSR = 56)
                                 ;        +16 is time from start of line to start of first pixel

  .equ vsyncreturn,vsyncreturn+synctest   ; move a few lines down, so we can see easily...
  .equ vsyncreturn,vsyncreturn-syncoffset ; -48 = end of previous line
 .else
  .equ ylines, 256                        ; skip last few
  .equ vsyncreturn, 7168+16-1-48          ; 7168+16-1-48 = end of previous line (Arm2)
  .equ vsyncreturn, vsyncreturn+25        ; 12 = either side of 1/2 way v0.07 fudge to shift colours right a bit

  ; for 4 VIDC changes only per scanline:
  ; ARM2 MEMC1a            7=last pix RHS  8-37(H-retrace) 38=first pix LHS
  ; ARM250 12MHz RAM       9=last pix RHS 10-39(H-retrace) 40=first pix LHS
  ; ARM3 25MHz 12MHz RAM  13=last pix RHS 14-40(H-retrace) 41=first pix LHS
  ; ARM3 c-off 12MHz RAM  10=last pix RHS 11-39(H-retrace) 40=first pix LHS
  ; target -> 14-37 = 23/2=11.5 +14=**25**
 .endif
; ============================================================================
.endif

; A440 MEMC1 timings: vsyncreturn = 7175-127-43 (works ok)
;                     hsyncline = 127
; A5000 Arm3 25MHz timings: vsyncreturn = 7175-127-40 (tested ok A5000, A3020 and A440)

; v0.28 vsyncreturn = 7142 (7168+16-1-48+7)
;       ...has been like this for many versions, and OK
;       56 lines * 128 uS = 7168
;                          +  16 gets to first displayed pixel line
;                          -  48 gets back to start of border on previous line
;                          +   7 gets a bit further into pervious line's border
;                          -   1 allow for timer trigger
;                          -----
;                           7142

; ============================================================================
; Helpers for vasm vs BASIC assembler.

.equ vsyncreturn_lowbyte, (vsyncreturn & 0x00FF)>>0
.equ vsyncreturn_highbyte, (vsyncreturn & 0xFF00)>>8

.equ hsyncline_lowbyte, (hsyncline & 0x00FF)>>0
.equ hsyncline_highbyte, (hsyncline & 0xFF00)>>8

; ============================================================================
; FIQ handler addresses.

.equ FIQ_notHSync, 0x00C0 ;*NEED TO ADJUST REF. IN swi_install IF THIS MOVES FROM 0xC0*
.equ FIQ_tempstack, 0x00D0

; ============================================================================
