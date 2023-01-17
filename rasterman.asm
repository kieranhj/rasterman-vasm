; ============================================================================
; aim to support 4, 8, 16 or 32 changes per line + 2 MEMC per interrupt!
; ============================================================================
; v0.12 allows 3 VIDC tables and 1 MEMC table, one IRQ/line = 16 changes+2
; v0.13 first attempt at VGA monitor-compatible rasters
; v0.14 first attempt at a keyboard handler...
; v0.15 need a direct SWI call address, to avoid SWIs turning on IRQs?
; v0.16 everything from v0.15 but back to TV res
; v0.17 add support for Arthur and RISC OS 2 (0.17a - fixed Arthur bug)
; v0.18 [reduce to only single colour change per line, test version] 22/6/17
; v0.19 test HSyncWait added
; v0.20 trial improved key handling
; v0.21 test insert keys to buffer
; v0.22 as v0.21 but only 4 VIDC changes, every 4 lines [21/8/19]
; v0.23 as v0.22 but only 4 VIDC changes, every 3 lines [21/8/19]
; ....top line flickers M97_demo+ with intense music...
; v0.24 removing all T1 code and all MEMC/VIDC HSync change code (for XLT) 25/9/20
; v0.25 based on v0.21, added mouse position and button buffering, keyboard reset detect, increment TIME in ZeroPage, corrected QTM detection to allow QTM_Start after RM init, RM_Mode added, also added keyboard reset on exit [7/1/22]
; v0.26 reverted sound handler to calling QTM in IRQ mode with IRQs disabled
; v0.27 based on v0.25, removed mouse buffer insert, added keyboard reset on RM_init [8/1/22]
; v0.28 as v0.27, but only 4 VIDC changes per line, added SWI RM_Configure template
; v0.29 as v0.28, optimum position of scanline VIDC write for ARM2/250/3, added 16uS delay to kbd (ref. RO3 source) to avoid lock-up, added test sound synchronisation
; v0.30 back to 16 VIDC changes per line and 2 MEMC changes [7/7/22]

; - check whether SWI calls enable interrupts and which ones
;   (use wurzel)
; - handler sometimes returns 0xC0C0 when pressing z x " ? ret shift etc
;   ..why? byte order wrong? can we fix this somehow?

; RasterMan for RISC OS v0.25 4th January 2022
; ============================================================================
; (C) Pnx/QTM 2013-2022
; ============================================================================
; Thanks to Xavier Tardy for the encouragement to make this happen :-)

; RISC OS 2 version removes use of RO3-only SWI OS_FindMemMapEntries
; Arthur version removes use of RO2+ SWI OS_ReadMemMapInfo / ReadMemMapEntries
;   "       "    also enables interrupts on SWI (next need to patch SWI vector)
; ...Arthur version now with SWI vector patch!

; ============================================================================
; Includes
; ============================================================================

.include "lib/swis.h.asm"
.include "config.h.asm"


; ============================================================================
; keyboard protocol codes
; ============================================================================

.equ bACK, 0b00111111    ; = 1st byte acknowleged
.equ sACK, 0b00110001    ; = keyboard scan only
.equ sMAK, 0b00110011    ; = keyboard and non-zero mouse scan
.equ HRST, 0b11111111    ; = keyboard hard reset
.equ KbAck1, 0b11111110  ; = reset response 1
.equ KbAck2, 0b11111101  ; = reset response 2


; ============================================================================
; Module Header
; ============================================================================

.org 0x00                   ; fully relocatable.

modulebase:
   .long    0               ;00 start code
   .long    init            ;04 init code
   .long    exit            ;08 exit code
   .long    service         ;0C service code
   .long    title           ;10 title
   .long    help            ;14 help
   .long    commands        ;18 commands
   .long    0x47E80         ;1C SWI number QTM+64
   .long    SWIcode         ;20 SWI handler code
   .long    SWItable        ;24 SWI table
   .long    0               ;28 SWI decoding code
   .long    0               ;2C Messages

; ============================================================================

title:
   .byte    "RasterMan", 0


help:
   .byte    "RasterMan"
   .byte    0x09
   .byte    "_VERSION", " (", "_DATE", ") (C) Steve Harrison"
.if alphatest
   .byte " !TEST VERSION NOT FOR RELEASE!"
.endif
   .byte    0


commands:
   .byte    "RasterMan"
   .byte    0
   .p2align 2
   .long    0
   .byte    0
   .byte    0
   .byte    0
   .byte    0
   .long    0
   .long    help_rasterman
   .long    0


help_rasterman:
   .byte    "Raster Manager v", "_VERSION"," adds more colour and music to your Archimedes, with true Raster Bars and HSync interrupt control for TV-resolution screen modes.", 0
   .p2align 2

; ============================================================================

.equ numSWIs, (endSWItable-startSWItable)/4

SWIcode:                     ;note - Arthur enters with IRQs disabled, fix by patching SWI vec
   CMP     R11,#numSWIs
   ADDLO   PC,PC,R11,LSL#2
   B       UnknownSWIerror
startSWItable:
   B       swi_install          ;0
   B       swi_release          ;1
   B       swi_wait             ;2
   B       swi_settables        ;3
   B       swi_version          ;4
   B       swi_readscanline     ;5
   B       swi_setVIDCreg       ;6
   B       swi_setMEMCreg       ;7
   B       swi_QTMParamAddr     ;8
   B       swi_scankeyboard     ;9
   B       swi_clearkeybuffer   ;10
   B       swi_readscanaddr     ;11
   B       swi_hsyncwaitaddr    ;12
   B       swi_configure        ;13
   B       swi_mode             ;14

   ;B       swi_offsettable      ;4
   ;B       swi_screenbank       ;7
   ;B       swi_screenstart      ;8
endSWItable:

UnknownSWIerror:
   ADR     R0,swierror
   ORRS    PC,R14,#1<<28
swierror:
   .long    486
   .byte    "Unknown RasterMan operation", 0
   .p2align 2


SWItable:
   .byte    "RasterMan", 0
   .byte    "Install", 0
   .byte    "Release", 0
   .byte    "Wait", 0
   .byte    "SetTables", 0
   .byte    "Version", 0
   .byte    "ReadScanline", 0
   .byte    "SetVIDCRegister", 0
   .byte    "SetMEMCRegister", 0
   .byte    "QTMParamAddr", 0
   .byte    "ScanKeyboard", 0
   .byte    "ClearKeyBuffer", 0
   .byte    "ReadScanAddr", 0
   .byte    "HSyncWaitAddr", 0
   .byte    "Configure", 0
   .byte    "Mode", 0
   ;.byte    "OffsetTable", 0
   ;.byte    "ScreenBank", 0
   ;.byte    "ScreenStart", 0
   .byte    0
   .p2align 2


; ***********************************************************************************
;
;                                 Code starts here...
;
; ***********************************************************************************


init:
   STMFD   R13!,{R0-R6,R14}
   MOV     R0,#129
   MOV     R1,#0
   MOV     R2,#0xFF
   SWI     OS_Byte

   STRB    R1,os_version
   CMP     R1,#0xA5
   LDMGEFD R13!,{R0-R6,R14}
   ADRGE   R0,regtablerror
   ORRGES  PC,R14,#1<<28     ;can't run on RISC OS 3.5 hardware

   SWI     XOS_WriteS
   .byte    "RasterMan v", "_VERSION", " alpha-Test version loaded.", 13, 10, 13, 10
   .byte    "** THIS IS A TEST VERSION AND IS NOT FOR PUBLIC RELEASE **", 13, 10, 0
   .p2align 2

   LDMFD   R13!,{R0-R6,PC}


rman_error:
   .long      255
   .byte      "Sorry, RasterMan v", "_VERSION", " can only run on RISC OS 3.1 or earlier", 0
   .p2align 2


exit:
   STMFD   R13!,{R0-R6,R14}
   LDMFD   R13!,{R0-R6,PC}


service:
   ;CMP       R1,#0x50
   ;MOVNE     PC,R14
   MOV       PC,R14


; SH decoded IRQ and FIQ masks
;
; to load/set/store IRQ and FIQ masks use:
;
; Rx=mask
; Ry=0x3200000 (IOC base)
;
;
; LDRB Rx,[Ry,#0x18+0]      ;load irqa mask (+0)
; STRB Rx,oldirqa          ;store original mask
; MOV  Rx,#0b00100000       ;only allow timer 0 interrupt
; STRB Rx,[Ry,#0x18+2]      ;(note +2 on storing)
;
; LDRB Rx,[Ry,#0x28+0]      ;load irqb mask (+0)
; STRB Rx,oldirqb          ;store original mask
; MOV  Rx,#0b00000010       ;only allow sound interrupt
; STRB Rx,[Ry,#0x28+2]      ;(note +2 on storing)
;
;

;irqa mask = IOC (0x3200000) + 0x18
;
;bit 0   - il6 0 printer busy / printer irq
;    1   - il7 0 serial port ringing / low battery
;    2   - if  0 printer ack / floppy index
;    3s  - ir  1 vsync
;    4   - por 0 power on
;    5c  - tm0 0 timer 0
;    6   - tm1 1 timer 1
;    7   - 1   0 n/c      (fiq downgrade?)
;
;irqb mask = IOC (0x3200000) + 0x28
;
;bit 0   - il0 0 expansion card fiq downgrade
;    1   - il1 0 sound system buffer change
;    2   - il2 0 serial port controller
;    3   - il3 0 hdd controller / ide controller
;    4   - il4 0 floppy changed / floppy interrupt
;    5   - il5 0 expansion card interrupt
;    6   - stx 1 keyboard transmit empty
;    7cs - str 1 keyboard recieve full
;
; c = cmdline critical
; s = desktop critical
;
;fiq mask (none are critical) = IOC (0x3200000) + 0x38
;
;bit 0  - fh0 0 floppy data request / floppy dma
;    1  - fh1 0 fdc interrupt / fh1 pin on ioc
;    2  - fl  0 econet interrupt
;    3  - c3  0 c3 on ioc
;    4  - c4  0 c4 on ioc / serial interrupt (also IRQB bit2)
;    5  - c5  0 c5 on ioc
;    6  - il0 0 expansion card interrupt
;    7  - 1   0 force fiq (always 1)
;
;cr
;
;bit 0 - c0 IIC data
;    1 - c1 IIC clock
;    2 - c2 floppy ready / density
;    3 - c3 reset enable / unique id
;    4 - c4 aux i/o connector / serial fiq
;    5 - c5 speaker
;    6 - if printer ack or floppy index
;    7 - ir vsync
;

os_version:
   .long      0         ;1 byte 0xA0 for Arthur 0.3/1.2, 0xA1 for RO2, 0xA3 for RO3.0, 0xA4 for RO3.1
   rasterinstalled:
   .long      0
   fiqoriginal:
   .long      0         ;R8
   .long      0         ;R9
   .long      0         ;R10
   .long      0         ;R11
   .long      0         ;R12
   .long      0         ;R13
   .long      0         ;R14
   oldIRQa:
   .long      0
   oldIRQb:
   .long      0
   oldIRQbranch:
   .long      0
   oldIRQaddress:
   .long      0
   newIRQfirstinst:
   .long      0


qtmseerror:
   .long      255
   .byte      "QTM_se failed to initialise correctly"
   .byte      0
   .p2align 2


; SWI RasterMan_Configure
;
; R0=0, 4, 8, 16 VIDC register changes per H-interrupt, -1 to read
; R1=0, 1, 2 MEMC register changes per H-interrupt, -1 to read
; R2=number of H-interrupts per screen draw (minimum 1, max 256)
; R3=number of lines from VSync (bottom of screen) to first H-interrupt (default 55), -1 to read
; R4=number of lines from one H-interrupt to the next
;      * Default 1, for one H-interrupt on every line
;      * Increase to 2-255 for wider rasters
;      * ***Set to 0*** for 2 H-interrupts per line
;      * or use -1 to read
; xx R5=time offset for first interrupt (0-127, default 0 = end of previous line), -1 to read
; xx R6=time offset for second interrupt (0-127, default is 64 = centre), -1 to read
;
;Note - You must call RasterMan_Wait, followed by RasterMan_ColourTable and
;RasterMan_VideoTable (if necessary) to ensure the correct tables are
;available, *before* making changes by calling RasterMan_Configure.
;
;Current version only - RasterMan_Configure will return an error if it is
;called while RasterMan is currently active (this restriction will be
;removed in future).
;


MEMCperline:
   .long      2
VIDCperline:
   .long      16


swi_configure:             ;for v0.30, if R0<>16 or R1<>2 give an error
   STMFD     R13!,{R14}       ;returns R2,R3,R4 as fixed values
   LDR       R14,VIDCperline
   CMN       R0,#1
   MOVEQ     R0,R14           ;if reading, place current value in R0
   TEQ       R0,#16
   BNE       error_configure
   STR       R0,VIDCperline   ;store new value
   MOV       R0,R14           ;return previous value in R0

   LDR       R14,MEMCperline
   CMN       R1,#1
   MOVEQ     R1,R14           ;if reading, place current value in R0
   TEQ       R1,#2
   BNE       error_configure
   STR       R1,MEMCperline   ;store new value
   MOV       R0,R14           ;return previous value in R0

   MOV       R2,#256          ;number of HSync interrupts per screen (default=screen y lines)
   MOV       R3,#1            ;lines from one HSync to the next (0=2/line)
   MOV       R4,#56           ;lines from VSync to top of screen
   LDMFD     R13!,{PC}


error_configure:
   ADR       R0,configerror
   LDMFD     R13!,{R14}
   ORRS      PC,R14,#1<<28


configerror:
   .long      255
   .byte      "RasterMan v", "_VERSION", " is restricted to 16 VIDC changes/line and 2 MEMC changes", 0
   .p2align 2


swi_version:
   MOV       R0,#_VERSION_NUMBER/10
   MOVS      PC,R14


swi_install:                ;returns in USER mode, with IRQs and FIQs enabled but our code managing
                            ;all IRQs and SWIs, disc access is now suspended and any call to
                            ;a SWI which is not ours, or selected QTM SWIs, releases our FIQ
   STMFD     R13!,{R0-R12,R14}
   LDR       R0,rasterinstalled
   CMP       R0,#0
   LDMNEFD   R13!,{R0-R12,PC}  ;we're already enabled

   ADRL      R0,regtable
   LDMIA     R0,{R0-R3}
   CMP       R0,#0
   CMPGT     R1,#0
   CMPGT     R2,#0
   CMPGT     R3,#0
   LDMLEFD   R13!,{R0-R12,R14}
   ADRLE     R0,regtablerror
   ORRLES    PC,R14,#1<<28     ;no colour table!

   BL        checkQTMspecialrelease
   LDMVSFD   R13!,{R0-R12,R14}
   ADRVS     R0,qtmseerror
   ORRVSS    PC,R14,#1<<28     ;qtm se error

   MOV       R0,#0x0C           ;claim FIQ
   SWI       XOS_ServiceCall
   STRVS     R0,[R13]
   LDMVSFD   R13!,{R0-R12,PC}

   ; reset keyboard

   MOV       R0,#0x10
   STR       R0,keycounter     ;set keycounter to 0x10=reset and clear keyboard bytes

   MOV       R0,#19
   SWI       XOS_Byte          ;added for sound sync code v0.29

   ; we own FIQs

   TEQP      PC,#0b11<<26 | 0b01;disable IRQs and FIQs, change to FIQ mode
   MOV       R0,R0

   ADR       R0,fiqoriginal
   STMIA     R0,{R8-R14}

   ; v0.29 attempt to re-sync sound to VSync

   LDR       R1,physicaldma1     ;dma2 was last read from DMABuffer but we had one VSync since then...
   LDR       R2,physicaldma2
   LDR       R3,qtmdmasize
   ADD       R4,R2,R3            ;R4=SendN for dma2
   SUB       R4,R4,#16           ; fixit ;-)
   ADD       R3,R1,R3            ;R3=SendN for dma1
   SUB       R3,R3,#16           ; fixit ;-)
   MOV       R1,R1,LSR#2         ;(dma1 Sstart/16) << 2
   MOV       R3,R3,LSR#2         ;(dma1 SendN/16) << 2
   MOV       R2,R2,LSR#2         ;(dma2 Sstart/16) << 2
   MOV       R4,R4,LSR#2         ;(dma2 SendN/16) << 2
   MOV          R0,#0x3600000     ;memc base
   ADD       R0,R0,#0x0080000     ;Sstart base
   ORR       R1,R1,R0            ;R1=dma1 Sstart
   ORR       R2,R2,R0            ;R2=dma2 Sstart
   ADD       R0,R0,#0x0020000     ;SendN base
   ORR       R3,R3,R0            ;R3=dma1 SendN
   ORR       R4,R4,R0            ;R4=dma2 SendN
   ADD       R0,R0,#0x0020000     ;R0=Sptr_rst

   STR       R3,[R3]             ;dma1 SendN
   STR       R1,[R1]             ;dma1 Sstart
   STR       R0,[R0]             ;force buffer swap!

   STR       R4,[R4]             ;dma2 SendN
   STR       R2,[R2]             ;dma2 Sstart set buffer 2 up for next swap

   MOV       R0,#0
   STR       R0,dmabank_num      ;0 after install = fill buffer 1 next

   ; sound buffer should now be sync'd to swap from b1 to b2 at ~VSync
   ; next sound buffer fill should be to buffer 1 after next VSync

   MOV       R1,#0x3200000
   LDRB      R0,[R1,#0x18]
   STR       R0,oldIRQa
   LDRB      R0,[R1,#0x28]
   STR       R0,oldIRQb

   ; When installing, we will start on the next VSync, so set IRQ for VSync only
   ; and set T1 to contain 'vsyncvalue', so everything in place for VSync int...

   MOV       R0,#0b00001000
   STRB      R0,[R1,#0x18+2]    ;set IRQA mask to 0b00001000 = VSync only
   MOV       R0,#0
   STRB      R0,[R1,#0x28+2]    ;set IRQB mask to 0
   STRB      R0,[R1,#0x38+2]    ;set FIQ mask to 0 (disable FIQs)

   MOV       R0,#0xFF           ;*v0.14* set max T1 - ensure T1 doesn't trigger before first VSync!
   STRB      R0,[R1,#0x50+2]    ;T1 low byte, +2 for write
   STRB      R0,[R1,#0x54+2]    ;T1 high byte, +2 for write
   STRB      R1,[R1,#0x58+2]    ;T1_go = reset T1

   MOV       R0,#vsyncreturn_lowbyte;or ldr r8,vsyncval  - will reload with this on VSync...
   STRB      R0,[R1,#0x50+2]    ;T1 low byte, +2 for write
   MOV       R0,#vsyncreturn_highbyte;or mov r8,r8,lsr#8
   STRB      R0,[R1,#0x54+2]    ;T1 high byte, +2 for write

   ; poke our IRQ/FIQ code into 0x1C-0xFC

   MOV       R0,#0
   LDR       R1,[R0,#0x18]      ;load current IRQ vector
   STR       R1,oldIRQbranch

   BIC       R1,R1,#0xFF000000
   MOV       R1,R1,LSL#2
   ADD       R1,R1,#0x18+8
   STR       R1,oldIRQaddress

   ;copy IRQ/FIQ code to 0x18 onwards

   ADRL      R0, fiqbase
   MOV       R1,#0x18
   LDMIA     R0!,{R2-R12}
   STMIA     R1!,{R2-R12}      ;11 pokey codey
   LDMIA     R0!,{R2-R12}
   STMIA     R1!,{R2-R12}      ;22 pokey codey
   LDMIA     R0!,{R2-R12}
   STMIA     R1!,{R2-R12}      ;33 pokey codey
   LDMIA     R0!,{R2-R12}
   STMIA     R1!,{R2-R12}      ;44 pokey codey
   LDMIA     R0!,{R2-R12}
   STMIA     R1!,{R2-R12}      ;55 pokey codey
   LDMIA     R0!,{R2-R4}
   STMIA     R1!,{R2-R4}       ;58 pokey codey (58 max)

   ADRL      R0, notHSync     ;set up VSync code after copying
   MOV       R1,#FIQ_notHSync ;ref. works if assembling on RO3, note 'FIQ_notHSync' is 0-relative!
   STR       R0,[R1]

   MOV       R0,#0
   LDR       R1,[R0,#0x18]      ;first IRQ instruction from our code
   STR       R1,newIRQfirstinst

   ; R8_FIQ=temp reg 1
   ; R9_FIQ=table 1
   ; R10_FIQ=table 2
   ; R11_FIQ=table 3 0x table 4
   ; R12_FIQ=memc table
   ; R13_FIQ=line count
   ; R14_FIQ=temp reg 2/set to IOC addr 0x3200000 on entry/exit

   ; set up our FIQ mode registers

   MOV       R8,#0
   ADRL      R9,regtable
   LDMIA     R9,{R9,R10,R11,R12}
   MOV       R13,#ylines       ;256
   MOV       R14,#0x3200000

   ; poke our SWI controller - N/R

   MOV       R0,#1
   STR       R0,rasterinstalled

   LDRB      R2,os_version
   CMP       R2,#0xA1
   BGE       notArthur_install

   ; Arthur version ONLY - patch SWI jump address to call our code and enable IRQs!

   MOV       R0,#0
   LDR       R1,[R0,#8]        ;load "B os_swi_code"
   STR       R1,oldswivector
   BIC       R1,R1,#0xFF<<24    ;clear B instruction bits
   MOV       R1,R1,LSL#2       ;shift 2 bits
   ADD       R1,R1,#8+8        ;now R1 has addr of OS SWI routine

   ADR       R2,os_swi_jump+8  ;destination addr +8 (for pipelining)
   SUB       R1,R1,R2          ;make address into relative offset (+8)
   MOV       R1,R1,LSR#2       ;shift 2 bits
   ORR       R1,R1,#0xEA<<24    ;add B (EA) [BL (EB)] instruction bits
   STR       R1,os_swi_jump    ;store

   ADR       R1,os_swi_patch   ;target addr
   SUB       R1,R1,#8+8        ;subtract store addr +8 (pipelining)
   MOV       R1,R1,LSR#2       ;shift 2 bits
   ORR       R1,R1,#0xEA<<24    ;add B (EA) instruction bits
   STR       R1,[R0,#8]        ;store

notArthur_install:
   TEQP      PC,#0b00<<26 | 0b11;enable IRQs and FIQs, change to SVC mode
   MOV       R0,R0
   LDMFD     R13!,{R0-R12,PC}^ ;exit in USER mode and with IRQs and FIQs on


oldswivector:
   .long      0


os_swi_patch:               ;only use for Arthur, n/r for RISC OS 2+
   TEQP      PC,#0b11           ;enable IRQs and FIQs, stay in SVC mode (NOP n/r) [bug fixed 0.17a]
os_swi_jump:
   B         os_swi_jump       ;(overwritten)


regtablerror:
   .long      255
   .byte      "RasterMan VIDC/MEMC tables not defined", 0
   .p2align 2


checkQTMspecialrelease:     ;*NOT* RO2 - SWI OS_FindMemMapEntries
   STMFD     R13!,{R0-R8,R14}
   ;SWI       "XQTM_SongStatus"
   ;MOVVS     R0,#0             ;if error (no QTM module) then treat as no song playing
   ;TST       R0,#0b0100
   ;MOVEQ     R0,#0
   ;STREQB    R0,qtmcontrol     ;no music playing ***but we want to start music after RM    init!***
   ;LDMEQFD   R13!,{R0-R8,PC}   ;v0.25...we shouldn't use QTM_SongStatus at all - we should use SoundControl to read if we're enabled!!!

   ; new v0.25...

   MVN       R0,#0
   MVN       R1,#0
   MVN       R2,#0
   SWI       XQTM_SoundControl  ;new v0.25, should work if no music playing
   MOVVS     R0,#0              ;no QTM module present, so set as if QTM not enabled
   CMP       R0,#0              ;QTM sound not enabled? (or QTM not present?)
   STREQ     R0,qtmcontrol      ;QTM not enabled
   LDMEQFD   R13!,{R0-R8,PC}

   STR       R0,qtmchannels

   MVN       R0,#0
   MVN       R1,#0
   MOV       R2,#123
   SWI       XQTM_Debug        ;return R0=(!QTMblock) QTM dma handler addr, R1=QTM's R12, R2=-1
   MOVVS     R0,#0
   STRVSB    R0,qtmcontrol     ;no QTM special release present
   LDMVSFD   R13!,{R0-R8,PC}   ;exit with error, because QTM present but not SE

   CMP       R0,#0
   CMPGE     R1,#0
   MOVLT     R0,#0
   STRLTB    R0,qtmcontrol     ;no QTM special release present
   LDMLTFD   R13!,{R0-R8,R14}
   ORRLTS    PC,R14,#1<<28     ;exit with error, because QTM present but not SE

   CMN       R2,#1
   MOVNE     R0,#0
   STRNEB    R0,qtmcontrol     ;no QTM special release present
   LDMNEFD   R13!,{R0-R8,R14}
   ORRNES    PC,R14,#1<<28     ;exit with error, because QTM present but not SE

   ; if we get here, QTM sound is on, QTM_Debug works and has provided two values >=0
   ; ...so we're most probably using QTM SE for RM

   SWI       XQTM_DMABuffer    ;read to R0
   MOV       R6,R0
   MOV       R7,#50            ;1 second max wait
dmabuffer:
   MOV       R0,#19
   SWI       XOS_Byte

   SWI       XQTM_DMABuffer
   CMP       R0,R6
   MOVNE     R5,R0
   BNE       gottwoDMAbuffers

   SUBS      R7,R7,#1
   BNE       dmabuffer

   ; if we get here, we failed to find two DMA buffers

   MOV       R0,#0
   STRB      R0,qtmcontrol
   LDMFD     R13!,{R0-R8,R14}  ;***should really return an error***
   ORRS      PC,R14,#1<<28

gottwoDMAbuffers:           ;in R5 and R6
   STR       R6,qtmdmabuffer1
   STR       R5,qtmdmabuffer2

   SWI       XQTM_Debug
   STR       R0,qtmdmahandler
   STR       R1,qtmr12pointer
   ADD       R1,R1,#QTMblock
   STR       R1,dmaentry_r9

   MOV       R0,#0
   MOV       R1,#0
   MOV       R2,#0
   MOV       R3,#0
   MOV       R4,#0
   SWI       Sound_Configure

   MUL       R0,R1,R0
   STR       R0,qtmdmasize     ;should be 416x4 or x8 " 48uS...(24uS, 4c = 832x4)

   LDRB      R0,os_version     ;check OS version
   CMP       R0,#0xA3           ;RISC OS 3.0?
   BGE       use_os_locate_DMA ;if so, do things properly... (probably of no value, but code works)

   ; for RISC OS 2.01 and below...

   CMP       R5,R6             ;this is a fudge to use fixed physical addrs for sound buffer
   MOVLT     R1,#0x7E000        ;it avoids using RISC OS 2-only or RISC OS 3-only SWIs
   MOVGE     R1,#0x7F000        ;so works with Arthur, which doesn't have an (easy) method.
   STR       R1,physicaldma2   ;got the correct phys addr of buf2 (R5)
   RSB       R1,R1,#0x7E000+0x7F000
   STR       R1,physicaldma1   ;got the correct phys addr of buf1 (R6)
   B         got_DMA_addr

use_os_locate_DMA:
   SWI       OS_ReadMemMapInfo ;not Arthur
   STR       R0,pagesize
   STR       R1,numpages

   SUB       R4,R0,#1
   BIC       R7,R5,R4          ;page for dmabuffer2
   BIC       R8,R6,R4          ;page for dmabuffer1

   SUB       R5,R5,R7          ;offset into page
   SUB       R6,R6,R8          ;offset into page

   ADR       R0,pagefindblk
   MOV       R1,#0
   STR       R1,[R0,#0]
   STR       R1,[R0,#8]
   MVN       R1,#0
   STR       R1,[R0,#12]
   STR       R7,[R0,#4]
   SWI       XOS_FindMemMapEntries ;not RISC OS 2 or earlier
   LDR       R1,[R0,#0]
   LDR       R4,pagesize
   MUL       R1,R4,R1
   ADD       R1,R1,R5
   STR       R1,physicaldma2 ;got the correct phys addr of buf2 (R7)

   MOV       R1,#0
   STR       R1,[R0,#0]
   STR       R1,[R0,#8]
   MVN       R1,#0
   STR       R1,[R0,#12]
   STR       R8,[R0,#4]
   SWI       XOS_FindMemMapEntries ;not RISC OS 2 or earlier
   LDR       R1,[R0,#0]
   LDR       R4,pagesize
   MUL       R1,R4,R1
   ADD       R1,R1,R6
   STR       R1,physicaldma1 ;got the correct phys addr of buf1 (R8)
   ;                          ...on RO2/Arthur we assume fixed sound DMA addr

got_DMA_addr:
   MOV       R0,#1
   STRB      R0,qtmcontrol
   LDMFD     R13!,{R0-R8,PC}^


swi_QTMParamAddr:
   ADRL      R0,qtmcontrol
   MOVS      PC,R14


qtmcontrol:
   .byte      0               ;=0 if QTM sound handler not enabled
vsyncbyte:
   .byte      0
   .byte      0
   .byte      0
pagesize:
   .long      0
numpages:
   .long      0
pagefindblk:
   .long      0 ;0
   .long      0 ;4
   .long      0 ;8
   .long      0 ;12
qtmdmabuffer2:                ;buffer 2 must be immediately before buffer 1
   .long      0               ;
qtmdmabuffer1:                ;keep after buffer 2
   .long      0               ;


swi_release:                  ;
   STMFD     R13!,{R0-R3,R14}
   LDR       R0,rasterinstalled
   CMP       R0,#1

   LDMNEFD   R13!,{R0-R3,R14}
   ADRNE     R0,releaseerror
   ORRNES    PC,R14,#1<<28       ;exit with error if we're not enabled

   ; we own FIQs

   TEQP      PC,#0b11<<26 | 0b01            ;disable IRQs and FIQs, switch FIQ mode
   MOV       R0,R0

   MOV       R0,#0
   LDR       R1,[R0,#0x18]        ;load current IRQ vector
   LDR       R2,newIRQfirstinst  ;our expected first instruction
   CMP       R1,R2
   LDMNEFD   R13!,{R0-R3,R14}
   ADRNE     R0,releaseerror
   ORRNES    PC,R14,#1<<28       ;preserves flags (IRQs on, FIQs off, SVC mode)

   LDR       R1,oldIRQbranch
   STR       R1,[R0,#0x18]        ;restore original IRQ controller

   MOV       R0,#0
   MOV       R1,#0x3200000
   STRB      R0,[R1,#0x38+2]      ;set FIQ mask to 0 (disable FIQs)

   LDR       R0,oldIRQa
   STRB      R0,[R1,#0x18+2]
   LDR       R0,oldIRQb
   STRB      R0,[R1,#0x28+2]      ;restore IRQ masks

   ADRL      R0,fiqoriginal
   LDMIA     R0,{R8-R14}

   LDRB      R0,os_version
   TEQ       R0,#0xA0

   ; if running on Arthur, restore original SWI controller (n/r RISC OS 2+)

   MOVEQ     R0,#0
   LDREQ     R1,oldswivector
   STREQ     R1,[R0,#8]          ;SWI jump restored (Arthur only)

   TEQP      PC,#0b00<<26 | 0b11          ;enable IRQs and FIQs, stay SVC mode
   MOV       R0,R0

   MOV       R0,#0
   STR       R0,rasterinstalled

   MOV       R0,#0x0B             ;release FIQ
   SWI       XOS_ServiceCall
   STRVS     R0,[R13]

   MOV       R14,#0x3200000       ;added keyboard reset v0.25
   LDRB      R0,[R14,#0x04+0]     ;load byte (clears SRx in IRQB)
   MOV       R0,#0b11111111       ;HRST for keyboard
   STRB      R0,[R14,#0x04+2]     ;send reset

   LDMFD     R13!,{R0-R3,PC}     ;restore flags (return USER mode, leave IRQs and FIQs on)


releaseerror:
   .long    255
   .byte    "RasterMan not enabled", 0
   .p2align 2



swi_hsyncwaitaddr:    ;returns addr in R0
   ADR       R0,hsyncwait_direct
   MOVS      PC,R14

   ;temp .p2align 2 space
   .long 0

hsyncwait_direct:     ;R4, R11, R12 corrupted
   MOV       R12,#FIQ_tempstack
   STRB      R12,[R12]   ;set non-zero
   MOV       R4,#0       ;HSync code will store R4 (0) at tempstack
waithsyncloop:
   LDRB      R11,[R12]   ;load our byte from FIQ address
   TEQ       R11,#0
   BNE       waithsyncloop
   MOVS      PC,R14


swi_wait:             ;can corrupt R11 and R12
   LDR       R12,rasterinstalled
   TEQ       R12,#0
   BEQ       osbyte19    ;we're not enabled

   LDRB      R11,vsyncbyte   ;load our byte from FIQ address, if enabled
waitloop:
   LDRB      R12,vsyncbyte
   TEQ       R12,R11
   BEQ       waitloop
   MOVS      PC,R14

osbyte19:
   STMFD     R13!,{R0-R2,R14}
   MOV       R0,#19
   SWI       OS_Byte
   LDMFD     R13!,{R0-R2,PC}^


swi_settables:        ;3 R0=colourtable 1, R1=ct2, R2=ct3, R3=memc
   STMFD     R13!,{R4-R7,R14}
   MOV       R4,R0
   MOV       R5,R1
   MOV       R6,R2
   MOV       R7,R3

   ADR       R12,regtable
   LDMIA     R12,{R0-R3}          ;original values for return

   CMP       R4,#0
   STRGT     R4,table1addr        ;store if >0
   CMP       R5,#0
   STRGT     R5,table2addr        ;store if >0
   CMP       R6,#0
   STRGT     R6,table3addr        ;store if >0
   CMP       R7,#0
   STRGT     R7,memctable         ;store if >0

   LDMFD     R13!,{R4-R7,PC}^


swi_readscanline:     ;5
   LDR       R12,rasterinstalled
   TEQ       R12,#0
   ADREQ     R0,releaseerror
   ORREQS    PC,R14,#1<<28       ;exit with error if we're not enabled

   TEQP      PC,#0b11<<26 | 0b01  ;disable IRQs and FIQs, switch FIQ mode
   MOV       R0,R0
   MOV       R0,R13              ;put current line counter in R0 (256=retrace, 255=line0, 0=line255)
   TEQP      PC,#0b00<<26 | 0b11;enable IRQs and FIQs, stay SVC mode
   RSB       R0,R0,#255          ;255-256=-1 = retrace, 255-255=0 line0, 255-0=255 line255
   MOVS      PC,R14


swi_setVIDCreg:       ;6
   MOV       R12,#0x3400000       ;R12=VIDC address
   STR       R0,[R12]            ;set register
   MOVS      PC,R14


swi_setMEMCreg:       ;7
   CMP       R0,#0x3600000        ;is R0 a MEMC register?
   STRGE     R0,[R0]             ;if so, set register
   MOVGES    PC,R14

   ADR       R0,memcerror
   ORRS      PC,R14,#1<<28       ;exit with error if register is wildly incorrect


memcerror:
   .long    255
   .byte    "MEMC register out of range", 0
   .p2align 2


swi_scankeyboard:     ;9
   STMFD     R13!,{R12,R14}
   LDRB      R12,keybyte2
   LDRB      R0,keybyte1
   TEQ       R12,#0      ;keybyte2=0 if awaiting next byte (so data invalid)
   MOVEQ     R0,#0       ;if data invald, return 0
   ORRNE     R0,R0,R12,LSL#8 ;**should we also clear data once read?**
   LDMFD     R13!,{R12,PC};flags not preserved


swi_clearkeybuffer:   ;10 - temp SWI, probably not needed in future once full handler done
   MOV       R12,#0
   STRB      R12,keybyte1
   STRB      R12,keybyte2
   MOV       PC,R14      ;flags not preserved


swi_readscanaddr:     ;11
   ADR       R0,swi_scankeyboard
   MOV       PC,R14      ;flags not preserved


swi_mode:             ;on entry R0=mode number, on exit R0=corrupted (could return scraddr)
   STMFD     R13!,{R14}  ;sets mode to R0
   CMP       R0,#255
   BGT       modeerror
   SWI       256+22
   SWI       OS_WriteC
   LDMFD     R13!,{PC}
modeerror:
   ADR       R0,err_modeerror
   ORRS      PC,R14,#1<<28       ;exit with error if register is wildly incorrect


err_modeerror:
   .long    255
   .byte    "MODE number out of range", 0
   .p2align 2


;.swi_offsettable      ;4
;.swi_vsync            ;6
;.swi_screenbank       ;7
;.swi_screenstart      ;8
MOV       PC,R14      ;flags not preserved


.global fiqbase
fiqbase:              ;copy to 0x18 onwards, 57 instructions max
                      ;this pointer must be relative to module
   .incbin "build/fiq_code.bin"

fiqend:



regtable:
table1addr: .long 0       ;r9
table2addr: .long 0       ;r10
table3addr: .long 0       ;r11
memctable:  .long 0       ;r12



kbd_stack:
   .long      0 ;R4
   .long      0 ;R5
   .long      0 ;R6
   .long      0 ;R7


checkkeyboard:                ;only called during retrace
   ADR       R8,kbd_stack
   STMIA     R8,{R4-R7}          ;some regs to play with

   LDRB      R4,[R14,#0x24+0]     ;load irq_B triggers [R14=0x3200000, IOC base]
   TST       R4,#0b10000000       ;is it bit 7 = SRx? (cleared by a read from 04)
   LDMEQIA   R8,{R4-R7}          ;restore regs
   BEQ       exitVScode          ;back to IRQ mode and exit

kbd_received:                    ;store key byte, and transmit ack value
   LDRB      R6,keycounter       ;0=no byte, so that 1-0=1->NE = first byte read
                                 ;If keycounter>1 then counting reset sequence 0x10,0x20,0x30...
   
   ; ~36 cycles since IRQ when we reach here, do we need a delay? RO waits 128 cycles to cover 16 uS...
   ;     we're getting several kbd crashes on ARM3, so maybe we do need to wait a bit...

   MOV       R5,#128-36          ;v0.29
kbd_RO_delay:
   SUBS      R5,R5,#5
   BGT       kbd_RO_delay

   LDRB      R5,[R14,#0x04+0]     ;load byte (clears SRx in IRQB)
   CMP       R5,#HRST            ;is it keyboard reset?
   BEQ       startkbdreset       ;...if so we've hit a protocol problem!
   CMP       R6,#1               ;if keycounter>1 then we're mid way through a reset
   BGT       midkbdreset         ;

   RSBS      R6,R6,#1            ;if =1 (NE), then this is the first byte, else (EQ)=second byte
   STRB      R6,keycounter

   STRNEB    R5,keybyte1
   STRNEB    R14,keybyte2        ;clear byte 2!!! (was key-bug until v0.20)
   MOVNE     R6,#bACK            ;if first byte, reply with bACK
   STREQB    R5,keybyte2
   MOVEQ     R6,#sMAK            ;if second byte, reply with sACK, **updated to SMAK 3/1/22**
                                 ; sACK=0b00110001 keyboard scan only
                                 ; sMAK=0b00110011 keyboard and non-zero mouse

   STRB      R6,[R14,#0x04+2]     ;transmit response
   LDMNEIA   R8,{R4-R7}          ;if not second byte, restore regs
   BNE       exitVScode          ;...and back to IRQ mode and exit

   TST       R5,#0b10000000       ;test bit 7 of 2nd byte
   BEQ       gotmousemoved       ;...if bit 7 clear, it's MDAT!

   MOV       R5,R5,LSR#4         ;shift to top nibble of R5
   ;TEQ       R5,#0b1101           ;0xD=KUDA is 2nd byte the "new key up" code?
   ;BEQ       gotnewkeyreleased
   TEQ       R5,#0b1100           ;0xC=KDDA is 2nd byte the "new key down" code?
   LDMNEIA   R8,{R4-R7}          ;if not, restore regs
   BNE       exitVScode          ;...and back to IRQ mode and exit

   ; get here if key-pressed and both bytes received

gotnewkeypressed:
   LDMIA     R8,{R4-R7}          ;restore regs
   TEQP      PC,#0b11<<26 | 0b11  ;enter SVC mode, IRQs/FIQs off
   MOV       R0,R0               ;sync
   STMFD     R13!,{R0-R4,R14}    ;stack R13_SVC
   MOV       R0,#138

   LDRB      R1,keybyte1         ;high nibble
   AND       R1,R1,#0xF
   LDRB      R2,keybyte2         ;low nibble
   AND       R2,R2,#0xF
   ORR       R2,R2,R1,LSL#4
   ;CMP       R2,#0x70
   ;BGE       mousebuttonpress
   ADR       R1,keytable
   LDRB      R2,[R1,R2]          ;load ascii value

   MOV       R1,#0
   SWI       XOS_Byte
   LDMFD     R13!,{R0-R4,R14}

   B         exitVScode          ;back to IRQ mode and exit


;
; v0.27 this buffer code works, but multiple SWIs = slow, and can impact
; raster stability if lots of mouse buttons and keys pressed repeatidly...
; so remove for now. Read mouse position with OS_Mouse and mouse buttons
; with RM keyboard scan!
;
;.gotnewkeyreleased            ;only really interested if this is a mouse release
;LDMIA     R8,{R4-R7}          ;restore regs
;TEQP      PC,#0b11<<26 | 0b11  ;enter SVC mode, IRQs/FIQs off
;MOV       R0,R0               ;sync
;STMFD     R13!,{R0-R4,R14}    ;stack R13_SVC
;MOV       R0,#138
;:
;LDRB      R1,keybyte1         ;high nibble
;AND       R1,R1,#0xF
;LDRB      R2,keybyte2         ;low nibble
;AND       R2,R2,#0xF
;ORR       R2,R2,R1,LSL#4
;CMP       R2,#0x70             ;is mouse button released?
;LDMLTFD   R13!,{R0-R4,R14}    ;if not, restore regs
;BLT       exitVScode          ;...and back to IRQ mode and exit
;:
;MOV       R4,#0x500
;LDRB      R3,[R4,#0x96]        ;mouse buttons byte [bit0=R, bit1=M, bit2=L]
;:                             ;0x70=left, 0x71=middle, 0x72=right
;TEQ       R2,#0x70
;BICEQ     R3,R3,#0b100
;TEQ       R2,#0x71
;BICEQ     R3,R3,#0b010
;TEQ       R2,#0x72
;BICEQ     R3,R3,#0b001
;B         domousebuffer
;
;
;.mousebuttonpress             ;in SVC and able to push to mouse buffer with OS_Byte 138
;MOV       R4,#0x500
;LDRB      R3,[R4,#0x96]        ;mouse buttons byte [bit0=R, bit1=M, bit2=L]
;:                             ;0x70=left, 0x71=middle, 0x72=right
;TEQ       R2,#0x70
;ORREQ     R3,R3,#0b100
;TEQ       R2,#0x71
;ORREQ     R3,R3,#0b010
;TEQ       R2,#0x72
;ORREQ     R3,R3,#0b001
;:
;.domousebuffer
;STRB      R3,[R4,#0x96]        ;mouse buttons byte [bit0=R, bit1=M, bit2=L]
;:
;;MOV       R0,#138            ;already 138 above
;MOV       R1,#9               ;mouse buffer = 9
;LDR       R2,[R4,#0xB4]        ;MouseX
;SWI       "XOS_Byte"
;BCS       mbufferfull
;MOV       R2,R2,LSR#8
;SWI       "XOS_Byte"
;:
;LDR       R2,[R4,#0xB8]        ;MouseY
;SWI       "XOS_Byte"
;MOV       R2,R2,LSR#8
;SWI       "XOS_Byte"
;:
;MOV       R2,R3               ;MouseButtons
;SWI       "XOS_Byte"
;:
;MOV       R4,#0
;LDR       R2,[R4,#0x10C]       ;TIME
;SWI       "XOS_Byte"
;MOV       R2,R2,LSR#8
;SWI       "XOS_Byte"
;MOV       R2,R2,LSR#8
;SWI       "XOS_Byte"
;MOV       R2,R2,LSR#8
;SWI       "XOS_Byte"
;:
;.mbufferfull
;LDMFD     R13!,{R0-R4,R14}
;B         exitVScode          ;back to IRQ mode and exit
;


; keyboard protocol reset code added v0.25...
; ...but doesn't seem to stop the occasional keyboard freeze

startkbdreset:                ;we've just read 0xFF in R5, from SRx, keycounter in R6
   ;
   ; Just received unexpected HRST 0xFF from kbd, we should respond with KbAck1
   ; set keycounter to 0x10 (expecting HRST) and continue...

   MOV       R6,#0x10

midkbdreset:
   ; if keycounter=0x10 we're expecting HRST from keyboard
   ;    if R5=HRST, respond KbAck1 and set keycounter=0x20 else send HRST
   ; if keycounter=0x20 we're expecting KbAck1 from keyboard
   ;    if R5=KbAck1, respond KbAck2 and set keycounter=0x30 else send HRST and set keycounter=0x10
   ; if keycounter=0x30 we're expecting KbAck2 from keyboard to complete reset
   ;    if R5=KbAck2, respond sMAK and set keycounter=0 else send HRST and set keycounter=0x10

   CMP       R6,#0x10
   CMPEQ     R5,#HRST
   MOVEQ     R6,#0x20
   MOVEQ     R5,#KbAck1
   BEQ       exitkbdreset

   CMP       R6,#0x20
   CMPEQ     R5,#KbAck1
   MOVEQ     R6,#0x30
   MOVEQ     R5,#KbAck2
   BEQ       exitkbdreset

   CMP       R6,#0x30
   CMPEQ     R5,#KbAck2
   MOVEQ     R6,#0
   MOVEQ     R5,#sMAK

   ; reset sequence failed, so send HRST back

   MOVNE     R6,#0x10
   MOVNE     R5,#HRST
exitkbdreset:
   STRB      R6,keycounter
   STRB      R5,[R14,#0x04+2]     ;transmit response
   LDMIA     R8,{R4-R7}          ;if not second byte, restore regs
   B         exitVScode          ;...and back to IRQ mode and exit


gotmousemoved:                ;borrowed much of these calcs from RO source...
   LDMIA     R8,{R4-R7}          ;restore regs
   STMIA     R8,{R0,R2,R3,R4}
   MOV       R0,#0x500            ;zero page workspace
   LDRB      R2,keybyte1         ;byte1 = X delta
   MOVS      R2,R2,LSL#25
   BEQ       doYmouse
   MOV       R2,R2,ASR#25        ;sign extend
   MOV       R2,R2,LSL#16
   LDR       R4,[R0,#0xC0]        ;zp,MouseXMult
   MUL       R2,R4,R2
   LDR       R4,[R0,#0xB4]        ;zp,MouseX
   ADD       R2,R2,R4,LSL#16
   MOV       R2,R2,ASR#16
   LDR       R4,[R0,#0xCC]        ;zp,MouseBoundLCol
   CMP       R2,R4
   MOVLT     R2,R4
   LDR       R4,[R0,#0xD4]        ;zp,MouseBoundRCol
   CMP       R4,R2
   MOVLT     R2,R4
   STR       R2,[R0,#0xB4]        ;zp,mouseX

doYmouse:
   LDRB      R3,keybyte2         ;byte2 = Y delta
   MOVS      R3,R3,LSL#25
   LDMEQIA   R8,{R0,R2,R3,R4}
   BEQ       exitVScode
   MOV       R3,R3,ASR#25        ;sign extend
   MOV       R3,R3,LSL#16
   LDR       R4,[R0,#0xC4]        ;zp,MouseYMult
   MUL       R3,R4,R3
   LDR       R4,[R0,#0xB8]        ;zp,MouseY
   ADD       R3,R3,R4,LSL#16
   MOV       R3,R3,ASR#16
   LDR       R4,[R0,#0xD0]        ;zp,MouseBoundBRow
   CMP       R3,R4
   MOVLT     R3,R4
   LDR       R4,[R0,#0xD8]        ;zp,MouseBoundTRow
   CMP       R4,R3
   MOVLT     R3,R4
   STR       R3,[R0,#0xB8]        ;zp,mouseX
   ;
   LDMIA     R8,{R0,R2,R3,R4}
   B         exitVScode          ;back to IRQ mode and exit



;   BACK=0b00111111 = 1st byte acknowleged
;   SACK=0b00110001 = keyboard only
;   SMAK=0b00110011 = keyboard and non-zero mouse
;   HRST=0b11111111 = keyboard hard reset
; KbAck1=0b11111110 = reset response 1
; KbAck2=0b11111101 = reset response 2


keycounter:  .byte 0 ; \   -> 1 or 0, or 0x10,0x20,0x30 = reset
keybyte1:    .byte 0 ;  }_ this block of 4 bytes must keep together..
keybyte2:    .byte 0 ;  }
nextkeybyte: .byte 0 ; /


; v0.20 now monitoring only receive SRx, assuming transmit is always ok...


notHSync:
   TST       R8,#0b00001000       ;retest R8 is it bit 3 = Vsync? (bit 6 = T1 trigger/HSync)
   STRNEB    R14,[R14,#0x58+2]    ;if VSync, reset T1 (latch should already have the vsyncvalue...)

   ; that's the high-priority stuff done, now we can check keyboard too...

   BEQ       checkkeyboard       ;if not VSync, check IRQ_B for SRx interrupt

   STRB      R8,[R14,#0x14+2]     ;if VSync, clear all IRQ_A interrupt triggers

   MOV       R8,#0b01000000       ; (removed VSync trigger v0.05)
   STRB      R8,[R14,#0x18+2]     ;set IRQA mask to 0b01000000 = T1 only
   MOV       R8,#0b10000000
   STRB      R8,[R14,#0x28+2]     ;set IRQB mask to 0b10000000 = SRx only

   MOV       R8,#hsyncline_lowbyte
   STRB      R8,[R14,#0x50+2]              ;T1 low byte, +2 for write
   MOV       R8,#hsyncline_highbyte
   STRB      R8,[R14,#0x54+2]              ;T1 high byte, +2 for write

   LDRB      R8,vsyncbyte
   RSB       R8,R8,#3
   STRB      R8,vsyncbyte

   MOV       R8,#0xA00                     ;v0.25 increment TIME
   LDR       R9,[R8,#0x1C]                 ;load centisecond TIME counter TimerAlpha
   ADDS      R9,R9,#2                     ;
   STR       R9,[R8,#0x1C]                 ;increment centisecond TIME!
   LDRCSB    R9,[R8,#0x1C+4]               ;
   ADDCS     R9,R9,#1                     ;
   STRCSB    R9,[R8,#0x1C+4]               ;if overflowed from 0xFFFFFFFF then inc 5th byte
   LDR       R9,[R8,#0x24]                 ;load second counter TimerBeta
   ADDS      R9,R9,#2                     ;
   STR       R9,[R8,#0x24]                 ;increment
   LDRCSB    R9,[R8,#0x24+4]               ;
   ADDCS     R9,R9,#1                     ;
   STRCSB    R9,[R8,#0x24+4]               ;if overflowed from 0xFFFFFFFF then inc 5th byte
   LDR       R9,[R8,#0x38]                 ;load third counter IntervalTimer
   ADDS      R9,R9,#2                     ;
   STR       R9,[R8,#0x38]                 ;increment
   LDRCSB    R9,[R8,#0x38+4]               ;
   ADDCS     R9,R9,#1                     ;
   STRCSB    R9,[R8,#0x38+4]               ;if overflowed from 0xFFFFFFFF then inc 5th byte
   MOV       R8,#0x100
   LDR       R9,[R8,#0x0C]                 ;load fourth counter MetroGnome = 4-byte MonotonicTime
   ADD       R9,R9,#2                     ;
   STR       R9,[R8,#0x0C]                 ;increment

   ADR       R8,regtable
   LDMIA     R8,{R9,R10,R11,R12}          ;reset table registers to defaults

   MOV       R13,#ylines                  ;reset yline counter

   LDRB      R8,qtmcontrol
   TEQ       R8,#1
   BNE       exitVScode                   ;back to IRQ mode and exit

rastersound:                     ;entered in FIQ mode, must exit via IRQ mode with SUBS PC,R14,#4
   TEQP      PC,#0b11<<26 | 0b10  ;enter IRQ mode, IRQs/FIQs off
   MOV       R0,R0               ;sync
;
; this switches to SVC mode, to allow QTM_DMA fill code to run with IRQs enabled
; this might be critical for keypress tracking, but not confirmed yet...does
; the keyboard crash (more often?) if we run QTM_DMA with IRQs off???
; ...is there a need to issue a keyboard reset on exit from RM...?
;
   STMFD     R13!,{R14}          ;stack R14_IRQ at stack R13_IRQ
   TEQP      PC,#0b11<<26 | 0b11  ;enter SVC mode, IRQs/FIQs off
   MOV       R0,R0               ;sync

   STR       R13,tempr13         ;
   LDRB      R13,dma_in_progress ;
   TEQ       R13,#0              ;
   LDRNE     R13,tempr13         ;
   BNE       exitysoundcode      ;
   STRB      PC,dma_in_progress  ;

   ADRL      R13,startofstack
   STMFD     R13!,{R14}          ;stack R14_SVC
   LDR       R14,tempr13         ;
   STMFD     R13!,{R14}          ;stack R13_SVC - we are now reentrant!!!
   BL        rastersound_1       ;call rastersound routine - enables IRQs

   MOV       R14,#0              ;...on return IRQs/FIQs will be off
   STRB      R14,dma_in_progress ;
   LDMFD     R13,{R13,R14}       ;restore R14_SVC and R13_SVC

exitysoundcode:
   TEQP      PC,#0b11<<26 | 0b10  ;back to IRQ mode
   MOV       R0,R0               ;sync

   LDMFD     R13!,{R14}
   SUBS      PC,R14,#4           ;return to foreground


dma_in_progress:
   .byte      0
   .byte      0
   .byte      0
   .byte      0


tempr13:
   .long      0


rastersound_1:                ;entered in SVC mode, with IRQs/FIQs disabled
   STMFD     R13!,{R0-R12,R14}   ;can enable IRQs, but must exit via MOVS PC,R14

      TEQP      PC,#0b00<<26 | 0b11  ;** IRQs/FIQs on
      MOV       R0,R0               ;** remove '**'d for 'NoIRQ'

      ; ***** note calling QTM in SVC mode will crash TSS (uses TEQP to return to IRQ mode!)
      ; ***** and play code if speed 0 (stop song)

   LDR       R9,dmaentry_r9      ;R9=ptr to initblock (QTMblock) = song_data+44+128
   LDR       R11,qtmchannels     ;R11=DMA gap
   LDR       R12,dmabank_num     ;0 after install
   RSBS      R12,R12,#1          ;1=bank1, 0=bank2
   STR       R12,dmabank_num

   LDREQ     R12,qtmdmabuffer2   ;R12=DMA start \_ use logical addrs
   LDRNE     R12,qtmdmabuffer1   ;R12=DMA start /  for QTM to fill...

   LDR       R10,qtmdmasize
   ADD       R10,R10,R12         ;R10=DMA end

   MOV       R14,PC
   LDR       PC,qtmdmahandler    ;R13=stack *R12* preserved

   LDR       R10,dmabank_num     ;1 or 0
   ADR       R12,physicaldma2    ;dma2 is immediately before dma1
   LDR       R12,[R12,R10,LSL#2] ;R12=DMA start

   LDR       R10,qtmdmasize
   ADD       R10,R10,R12         ;SendN
   SUB       R10,R10,#16         ; fixit ;-)

   MOV       R12,R12,LSR#2       ;(Sstart/16) << 2
   MOV       R10,R10,LSR#2       ;(SendN/16) << 2
   MOV          R0,#0x3600000     ;memc base
   ADD       R1,R0,#0x0080000     ;Sstart
   ADD       R2,R0,#0x00A0000     ;SendN
   ORR       R1,R1,R12           ;Sstart
   ORR       R2,R2,R10           ;SendN
   STR       R2,[R2]
   STR       R1,[R1]

   LDMFD     R13!,{R0-R12,PC}^   ;return to IRQ calling routine


exitVScode:
   TEQP      PC,#0b000011<<26 | 0b10 ;36 A4 back to IRQ mode
   MOV       R0,R0                  ;37 A8 sync IRQ registers
   SUBS      PC,R14,#4              ;38 AC return to foreground

dmabank_num:
   .long      0
physicaldma2: ;dma buffer 2 must be immediately before dma buffer 1
   .long      0   ;
physicaldma1: ;keep these together
   .long      0   ;
qtmdmasize:
   .long      0
qtmdmahandler:
   .long      0
qtmr12pointer:
   .long      0
dmaentry_r9:
   .long      0
qtmchannels:
   .long      0


addrtable:
   .long    0x000fff ;1


.equ unknown, 0

keytable:
   .byte 27      ; "Esc" ;0x00
   .byte unknown ; "f1"
   .byte unknown ; "f2"
   .byte unknown ; "f3"
   .byte unknown ; "f4"
   .byte unknown ; "f5"
   .byte unknown ; "f6"
   .byte unknown ; "f7"
   .byte unknown ; "f8"
   .byte unknown ; "f9"
   .byte unknown ; "f10"
   .byte unknown ; "f11"
   .byte unknown ; "f12"
   .byte unknown ; "Prt"
   .byte unknown ; "Scl"
   .byte 27      ; "Brk" ;0x0f

   .byte "`"               ; 0x10
   .byte "1"
   .byte "2"
   .byte "3"
   .byte "4"
   .byte "5"
   .byte "6"
   .byte "7"
   .byte "8"
   .byte "9"
   .byte "0"
   .byte "-"
   .byte "="
   .byte unknown ;"FALSE"
   .byte 8       ; "Bks"
   .byte unknown ; "Ins"   ; 0x1f

   .byte 30      ; "Hom"   ; 0x20
   .byte unknown ; "pUp"
   .byte unknown ; "Num"
   .byte "/"  ; "Kp/"
   .byte "*"  ; "Kp*"
   .byte "#"  ; "Kp#"
   .byte 9     ; "Tab"
   .byte "Q"
   .byte "W"
   .byte "E"
   .byte "R"
   .byte "T"
   .byte "Y"
   .byte "U"
   .byte "I"
   .byte "O"               ; 0x2f

   .byte "P"               ; 0x30
   .byte "["
   .byte "]"
   .byte "\"
   .byte 127     ; "Del"
   .byte unknown ; "Cpy"
   .byte unknown ; "pDn"
   .byte "7"  ; "Kp7"
   .byte "8"  ; "Kp8"
   .byte "9"  ; "Kp9"
   .byte "-"  ; "Kp-"
   .byte unknown ; "LCt"
   .byte "A"
   .byte "S"
   .byte "D"
   .byte "F"               ; 0x3f

   .byte "G"               ; 0x40
   .byte "H"
   .byte "J"
   .byte "K"
   .byte "L"
   .byte ";"
   .byte "'"
   .byte 13     ; "Ret"
   .byte "4"  ; "Kp4"
   .byte "5"  ; "Kp5"
   .byte "6"  ; "Kp6"
   .byte "+"  ; "Kp+"
   .byte unknown ; "LSh"
   .byte "Z"
   .byte "X"               ; 0x4f

   .byte "C"               ; 0x50
   .byte "V"
   .byte "B"
   .byte "N"
   .byte "M"
   .byte ","
   .byte "."
   .byte "/"
   .byte unknown ; "RSh"
   .byte unknown ; "cUp"
   .byte "1"  ; "Kp1"
   .byte "2"  ; "Kp2"
   .byte "3"  ; "Kp3"
   .byte unknown ; "Cps"
   .byte unknown ; "LAl"
   .byte " "  ; "Spc"      ; 0x5f

   .byte unknown ; "RAl"   ; 0x60
   .byte unknown ; "RCt"
   .byte unknown ; "cLf"
   .byte unknown ; "cDn"
   .byte unknown ; "cRt"
   .byte "0"  ; "Kp0"
   .byte "."  ; "Kp."
   .byte 10     ; "Ent"    ; 0x67

   .skip 9

   .byte unknown ; "mLf"   ; 0x70
   .byte unknown ; "mMd"
   .byte unknown ; "mRt"

   .skip 256-0x73
   ; TODO: Check this is 256 entries.

endofstack:
   .skip 1024
startofstack:
