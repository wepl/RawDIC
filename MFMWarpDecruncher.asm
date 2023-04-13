;----------------------------------------------------------------------------------------
;# Double Index UnPack Routine V1.1a
;# Version:	V1.1a by Ferox! (06-02-95)
;#
;# a0.l	^SRC
;# a1.l	^DEST SPACE
;#>a0.l	^DEPACKED DATA				;Removed via stack save
;#>d0.l	^PLEN					;# 0=ERR!

DoubleIndexUnPack:
	movem.l	d1-d7/a0-a6,-(a7)		; we better save a0 aswell

	move.l	a0,a6
	move.l	a1,a5				;# BASE 4 CLONE!
	moveq	#0,d0
	cmp.l	#"DIP!",(a0)+			;# ID
	bne.s	.QUIT
	move.l	(a0)+,-(a7)			;# ULEN
	add.l	(a0)+,a6			;# PLEN a6=^END

.Loop:
	cmp.l	a6,a0
	bcc.s	.END
	move.w	(a0)+,d0
	bpl.s	.COPY

;# Clone Part... if NEAGTIVE

.CLONE:
	neg.w	d0				;# BYTES TO CLONE!
	asr.l	#1,d0
	move.l	a5,a2	
	add.w	(a0)+,a2			;# a2.l shift this bytes!
	move.l	a5,a1
	add.w	(a0)+,a1			;# copy to this!
	move.w	(a0)+,d4			;# SHIFT LEFT
	bra.s	.CLi

.CL0:
	move.l	(a2),d1
	addq.w	#2,a2
	lsr.l	d4,d1
	move.w	d1,(a1)+

.CLi:
	dbf	d0,.CL0
	bra.s	.Loop

;# 'AS IT IS' MODE if POSTIVE!

.COPY:
	btst	#14,d0				;# if 1= PACKED:..(JUST BYTE RUN 1!!)
	bne.s	.COPY_RUN1
	and.w	#$3fff,d0
	bra.s	.COi

.CO0:
	move.w	(a0)+,(a1)+

.COi:
	dbf	d0,.CO0
	bra.s	.Loop

;# 'AS IT IS' MODE if POSTIVE! but WORDRun1 packed...(4 best result.../ usually GAP PACKING!)

.COPY_RUN1:
	and.w	#$3fff,d0
	move.l	a1,a3
	add.w	d0,a3
	add.w	d0,a3				;# DEST-END!

.BR1.L0:
	cmp.l	a3,a1
	bcc.s	.Loop
	move.w	(a0)+,d0
	bpl.s	.BR1.COi


;# if negative...repeat word...

.BR1.CLONE:
	neg.w	d0
	move.w	(a0)+,d1
	bra.s	.BR1.CLi

.BR1.CL0:
	move.w	d1,(a1)+

.BR1.CLi:
	dbf	d0,.BR1.CL0
	bra.s	.BR1.L0

;# if postive just copy..

.BR1.COPY:
	move.w	(a0)+,(a1)+

.BR1.COi:
	dbf	d0,.BR1.COPY
	bra.s	.BR1.L0

.END:
	move.l	(a7)+,d0			;# ORG-LEN!
	sub.l	a5,a1
	cmp.l	d0,a1
	beq.s	.QUIT
	moveq	#0,d0				;# DIFF LEN ??
		
;# Now the Real Routines...

.QUIT:
	movem.l	(a7)+,d1-d7/a0-a6		; we saved A0
	rts
;----------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------
;# MFMDeCruncher Routine
;# Version:	V1.0a by Ferox! (31-5-95)
;#
;# a0.l	^SRC
;# a1.l	^DEST SPACE
;#>a0.l	^DEPACKED DATA				;Removed
;#>d0.l	^PLEN					;# 0=ERR!

MFMDeCrunch:
	movem.l	d1-d7/a0-a6,-(a7)		; we better save a0 aswell

	move.l	a0,a6
	move.l	a1,a5				;# BASE 4 CLONE!
	moveq	#0,d0
	cmp.l	#"MC1!",(a0)+			;# ID
	bne.w	.QUIT
	move.l	(a0)+,-(a7)			;# ULEN
	add.l	(a0)+,a6			;# PLEN a6=^END
	move.l	#$55555555,d0			;# MASK!

.Loop:
	cmp.l	a6,a0
	bcc.w	.END
	move.w	(a0)+,d7
	bmi.w	.Copy
	btst	#14,d7
	bne.w	.MAA

;# M55 Coding..
	and.w	#$3fff,d7
	subq.w	#1,d7				;# at least 1
	bmi.s	.M55.END
	move.w	(a0)+,-(a7)			;# FIRST BIT ALWAYS RIGHT!

	move.l	(a0)+,d5			;# BOTH HALVES!
	move.l	d5,d1
	lsr.l	#1,d1
	and.l	d0,d1
	move.l	d1,d2				;# TRY: CODE AS MFM!(55-MODE)

	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	and.l	d3,d4
	or.l	d4,d2
	move.l	d2,(a1)+			;# FIRST HALF!
	move.w	(a7)+,-4(a1)			;# FRIST BIT RIGHT!
	bra.s	.M55.C5

.M55.L0:
	move.l	(a0)+,d5			;# BOTH HALVES!
	move.l	d5,d1
	lsr.l	#1,d1
	and.l	d0,d1
	move.l	d1,d2				;# TRY: CODE AS MFM!(55-MODE)

	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	bset	#31,d4
	and.l	d3,d4
	or.l	d4,d2
	btst	#0,-1(a1)			;# fix a bit ?
	beq.s	.M55.C0
	bclr	#31,d2

.M55.C0:
	move.l	d2,(a1)+			;# FIRST HALF!

.M55.C5:
	move.l	d5,d1
	and.l	d0,d1
	move.l	d1,d2			;# TRY: CODE AS MFM!(55-MODE)

	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	bset	#31,d4
	and.l	d3,d4
	or.l	d4,d2
	btst	#0,-1(a1)			;# fix a bit ?
	beq.s	.M55.C1
	bclr	#31,d2

.M55.C1:	
	move.l	d2,(a1)+			;# FIRST HALF!
	dbf	d7,.M55.L0

.M55.END:
	bra.w	.Loop

;# MAA Coding..
.MAA:
	and.w	#$3fff,d7
	subq.w	#1,d7				;# 1 at least!
	bmi	.MAA.END

	move.l	(a0)+,d5
	move.l	d5,d1
	and.l	d0,d1
	move.l	d1,d2				;# TRY: CODE AS MFM!(aa-MODE)

	move.l	d1,d2	
	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	and.l	d3,d4
	or.l	d4,d2
	add.l	d2,d2
	move.l	d2,(a1)+
	bra.s	.MAA.C0

;# With Fixing...
.MAA.L0:
	move.l	(a0)+,d5
	move.l	d5,d1
	and.l	d0,d1
	move.l	d1,d2				;# TRY: CODE AS MFM!(aa-MODE)

	move.l	d1,d2	
	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	and.l	d3,d4
	or.l	d4,d2
	add.l	d2,d2
	move.l	d2,(a1)+

	tst.b	-4(a1)				;# FIX BETWEEN L1 L2!
	bmi.s	.MAA.C0
	btst	#1,-1-4(a1)
	bne.s	.MAA.C0	
	bset	#0,-1-4(a1)			;# SET

.MAA.C0:
	move.l	d5,d1				;# SECOND!
	lsr.l	#1,d1
	and.l	d0,d1
	move.l	d1,d2				;# TRY: CODE AS MFM!(aa-MODE)

	move.l	d1,d2	
	and.l	d0,d2
	move.l	d2,d3
	eor.l	d0,d3
	move.l	d3,d4
	add.l	d3,d3
	lsr.l	#1,d4
	and.l	d3,d4
	or.l	d4,d2
	add.l	d2,d2
	move.l	d2,(a1)+

	tst.b	-4(a1)				;# FIX BETWEEN L1 L2!
	bmi.s	.MAA.C1
	btst	#1,-1-4(a1)
	bne.s	.MAA.C1	
	bset	#0,-1-4(a1)			;# SET

.MAA.C1:
	dbf	d7,.MAA.L0

.MAA.END:
	move.w	(a0)+,-2(a1)			;# STORE BORDER BITS!
	bra.w	.Loop

;# Read -(N) Longs...

.Copy:	neg.w	d7
	bra.s	.Copy.In

.COPY.L0:
	move.l	(a0)+,(a1)+

.Copy.In:
	dbf	d7,.COPY.L0
	bra.w	.Loop

.END:
	move.l	(a7)+,d0			;# ORG-LEN!
	sub.l	a5,a1				;# LEN MAY VARY UP TO 8 BYTES!!

;# Now the Real Routines...

.QUIT:
	move.l	a5,a0
	movem.l	(a7)+,d1-d7/a0-a6		; we saved a0
	rts
;----------------------------------------------------------------------------------------

;----------------------------------------------
;## Calculate CheckSum TYPE: FeRoX-Warp
;# a0.l	^SRC
;# d0.w	#BYTES TO CALC CHKSUM ABOUT (WORD SIZED!)
;#>d0.l	CHKSUM..

Calc_FRXCHKSUM:	movem.l	d1/a0,-(a7)
		move.l	d0,d1
		asr.w	#1,d1
		and.l	#$ffff,d0			;# USE WORD ONLY!
		bra.s	.Li
.L0:		add.l	d0,d0
		add.w	(a0)+,d0
		swap	d0
.Li:		dbf	d1,.L0
		movem.l	(a7)+,d1/a0
		rts
