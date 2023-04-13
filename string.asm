;*---------------------------------------------------------------------------
; Program:	string.asm
; Contents:	String routines for "RawDIC" (c) 1999 John Selck
; Author:	John Selck
; History:	12.10.02 - v1.1 (Codetapper)
;		         - SearchString routine modified to allow command
;		           line arguments to be supplied separated by a
;		           space rather than the equals sign
; Copyright:	Public Domain
; Language:	68000 Assembler
; Translator:	Barfly
;---------------------------------------------------------------------------*

SearchString:	; A0=String
		; A1=SearchString
		; => A0=Addy of SearchString in String
		; => A1=Addy of SearchString end in String

		movem.l	d0-d1/a2-a3,-(sp)
		move.l	a0,a2
		move.l	a1,a3
.l0		move.b	(a1)+,d1
		beq.b	.end
		cmp.b	#"a",d1
		blo.b	.n0
		cmp.b	#"z",d1
		bhi.b	.n0
		and.b	#$df,d1
.n0		move.b	(a0)+,d0
		beq.b	.not
		cmp.b	#$22,d0		; skip "..."
		bne.b	.s0
.l1		move.b	(a0)+,d0
		beq.b	.not
		cmp.b	#$22,d0
		bne.b	.l1
		move.l	a0,a2		; set after "..."
		move.l	a3,a1		; restart searchstring
		bra.b	.l0
.s0		cmp.b	#"(",d0		; skip (...)
		bne.b	.s1
.l2		move.b	(a0)+,d0
		beq.b	.not
		cmp.b	#")",d0
		bne.b	.l2
		move.l	a0,a2		; set after (...)
		move.l	a3,a1		; restart searchstring
		bra.b	.l0
.s1		cmp.b	#"a",d0
		blo.b	.n1
		cmp.b	#"z",d0
		bhi.b	.n1
		and.b	#$df,d0
.n1		cmp.b	d0,d1
		beq.b	.l0

		cmp.b	#"=",d1		; Codetapper added so that you can
		bne	.n2		; supply command line arguments 
		cmp.b	#" ",d0		; without the equals sign
		beq	.l0		; eg. "INPUT=" will return "File.mfm" in the string "INPUT File.mfm" 

.n2		addq.l	#1,a2
		move.l	a2,a0
		move.l	a3,a1
		bra.b	.l0
.not		sub.l	a2,a2
.end		move.l	a0,a1
		move.l	a2,a0
		movem.l	(sp)+,d0-d1/a2-a3
		rts

CopyFileName:	; A0=Source
		; A1=Target

		movem.l	d0-d1/a0-a1,-(sp)
		move.w	#$1f7,d1
		move.b	(a0),d0
		cmp.b	#$22,d0
		beq.b	.yo
.l0		move.b	(a0)+,d0
		cmp.b	#$20,d0
		bls.b	.end
		move.b	d0,(a1)+
		dbra	d1,.l0
.end		clr.b	(a1)
		movem.l	(sp)+,d0-d1/a0-a1
		rts
.yo		addq.l	#1,a0
.l1		move.b	(a0)+,d0
		cmp.b	#$20,d0
		blo.b	.end
		cmp.b	#$22,d0
		beq.b	.end
		move.b	d0,(a1)+
		dbra	d1,.l1
		bra.b	.end

GetNumber:	; A0=Source
		; => D0.l=value

		movem.l	d1-d2/a0,-(sp)

		moveq	#0,d0
		moveq	#0,d1
		move.b	(a0),d1
		cmp.b	#"$",d1
		beq.b	.hex
.dec		move.b	(a0)+,d1
		cmp.b	#"0",d1
		blo.b	.end
		cmp.b	#"9",d1
		bhi.b	.end
		move.l	d0,d2
		lsl.l	#2,d2
		add.l	d2,d0
		add.l	d0,d0
		and.b	#$0f,d1
		add.l	d1,d0
		bra.b	.dec
.hex		addq.l	#1,a0
.l0		move.b	(a0)+,d1
		cmp.b	#"0",d1
		blo.b	.n0
		cmp.b	#"9",d1
		bhi.b	.n0
.in		lsl.l	#4,d0
		and.b	#$0f,d1
		or.b	d1,d0
		bra.b	.l0
.n0		and.b	#$df,d1
		cmp.b	#"A",d1
		blo.b	.end
		cmp.b	#"F",d1
		bhi.b	.end
		add.b	#9,d1
		bra.b	.in
.end
		movem.l	(sp)+,d1-d2/a0
		rts
