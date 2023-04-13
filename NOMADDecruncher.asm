
;## NOMAD-WARP
;# DepackerRoutine
;# a0.l	^SRC (no shit anymore...like the first $40!)
;# a1.l	^DST
;# a2.l	^DePackBuffer ($1200)
;# d0.l	#SLEN
;# d1.l	#DLEN

NOMAD_UnPack:
	movem.l	d1-a6,-(a7)
	lea	.DATA,a4
	move.l	a0,a5
	move.l	a2,16(a4)			;# $2000 free bytes..
	move.l	a0,(a4)
	move.l	d1,4(a4)
	move.l	d0,12(a4)
	move.l	a1,d0
	move.l	d0,8(a4)
	move.l	12(a4),d2
	move.l	a4,a0
	bsr.s	.Start
	movem.l	(a7)+,d1-a6
	rts	

.DATA:	dc.l	0,0,0,0			;# ^SRC,#SLEN,^DST,#DLEN
	dc.l	0,0

.Start:	movem.l	d2-d7/a2-a6,-(a7)
	pea	(a0)
	bsr	.Main
	move.l	a1,d0
	move.l	(a7)+,a0
	sub.l	8(a0),d0
	move.l	d0,12(a0)
	movem.l	(a7)+,d2-d7/a2-a6
	rts	

	REPT	60
	move.b	(a2)+,(a1)+
	ENDR
	bra.w	.01ac

.Main:	pea	(a0)
	move.l	$0010(a0),a6
	move.l	a6,a0
	lea	$0c60(a6),a1
	lea	$076e(a6),a2
	moveq	#-$02,d0
	moveq	#$00,d3
	moveq	#$01,d1
	moveq	#$02,d2
	move.w	#$013c,d7
.0140	move.w	d1,(a0)+
	move.w	d0,(a1)+
	move.w	d3,-(a2)
	sub.w	d2,d0
	add.w	d2,d3
	dbf	d7,.0140
	moveq	#$00,d6
	move.l	#$0000027a,d7
	move.l	a6,a0
	lea	$027a(a6),a1
	lea	$0eda(a6),a2
	lea	$076e(a6),a3
	moveq	#$04,d4
	moveq	#$02,d2
	move.w	#$013b,d1
.016c	move.w	(a0)+,d0
	add.w	(a0)+,d0
	move.w	d0,(a1)+
	move.w	d6,(a2)+
	move.w	d7,(a3)+
	move.w	d7,(a3)+
	add.w	d4,d6
	add.w	d2,d7
	dbf	d1,.016c
	move.w	d1,(a1)
	clr.w	$0c5e(a6)
	move.l	(a7)+,a5
	move.l	(a5),a0
	move.l	$0008(a5),a1
	move.l	$0010(a5),a6
	move.l	a6,a5
	lea	$076e(a6),a4
	lea	$0c60(a6),a3
	moveq	#$01,d4
	moveq	#$0f,d5
	move.w	(a0)+,d6
	bra.b	.01ac
.01a4	neg.w	d7
	lsr.w	d4,d7
	sub.w	d4,d7
	move.b	d7,(a1)+
.01ac	move.w	$04f0(a3),d7
.01b0	add.w	d6,d6
	bcc.w	.01c8
	move.w	$02(a3,d7.w),d7
	dbmi	d5,.01b0
	bmi.w	.01d2
.01c2	moveq	#$0f,d5
	move.w	(a0)+,d6
	bra.b	.01b0
.01c8	move.w	$00(a3,d7.w),d7
	dbmi	d5,.01b0
	bpl.b	.01c2
.01d2	dbf	d5,.01da
	move.w	(a0)+,d6
	moveq	#$0f,d5
.01da	cmpi.w	#$8000,$04f0(a5)
	beq.w	.0238
	move.w	$00(a4,d7.w),d0
.01e8	lea	$00(a5,d0.w),a2
	move.w	(a2),d1
	add.w	d4,d1
	move.w	d1,(a2)+
	cmp.w	(a2)+,d1
	bls.b	.0232
.01f6	cmp.w	(a2)+,d1
	bhi.b	.01f6
	subq.l	#4,a2
	move.l	a2,d2
	sub.l	a5,d2
	move.w	(a2),$00(a5,d0.w)
	move.w	d1,(a2)
	move.w	$00(a3,d0.w),d1
	bmi.b	.0210
	move.w	d2,$02(a4,d1.w)
.0210	move.w	d2,$00(a4,d1.w)
	move.w	$00(a3,d2.w),d3
	bmi.b	.021e
	move.w	d0,$02(a4,d3.w)
.021e	move.w	d1,$00(a3,d2.w)
	move.w	d0,$00(a4,d3.w)
	move.w	d3,$00(a3,d0.w)
	move.w	$00(a4,d2.w),d0
	bne.b	.01e8
	bra.b	.0238
.0232	move.w	$00(a4,d0.w),d0
	bne.b	.01e8
.0238	cmp.w	#$fe00,d7
	bge.w	.01a4
	cmp.w	#$fd86,d7
	bgt.b	.0248
	rts	

.0248	rol.w	#8,d6
	subq.w	#7,d5
	bcc.b	.0268
	moveq	#$00,d3
	move.w	(a0)+,d3
	swap	d3
	neg.w	d5
	rol.l	d5,d3
	move.w	d3,d1
	or.b	d6,d1
	swap	d3
	move.w	d3,d6
	neg.w	d5
	moveq	#$0f,d2
	add.w	d2,d5
	bra.b	.0276
.0268	moveq	#$00,d1
	move.b	d6,d1
	clr.b	d6
	dbf	d5,.0276
	move.w	(a0)+,d6
	moveq	#$0f,d5
.0276	move.w	d1,d3
	add.w	d3,d3
	move.w	.02a4(pc,d3.w),d3
	clr.w	d2
	move.b	d3,d2
	clr.b	d3
	lsr.w	#2,d3
.0286	add.w	d6,d6
	addx.w	d1,d1
	dbf	d5,.0292
	moveq	#$0f,d5
	move.w	(a0)+,d6
.0292	dbf	d2,.0286
	andi.w	#$003f,d1
	or.w	d3,d1
	lea	(a1),a2
	suba.w	d1,a2
	jmp	.02a4+$7c(pc,d7.w)

.02a4:	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0101,$0101,$0101,$0101,$0101,$0101,$0101,$0101
	dc.w	$0101,$0101,$0101,$0101,$0101,$0101,$0101,$0101
	dc.w	$0201,$0201,$0201,$0201,$0201,$0201,$0201,$0201
	dc.w	$0201,$0201,$0201,$0201,$0201,$0201,$0201,$0201
	dc.w	$0301,$0301,$0301,$0301,$0301,$0301,$0301,$0301
	dc.w	$0301,$0301,$0301,$0301,$0301,$0301,$0301,$0301
	dc.w	$0402,$0402,$0402,$0402,$0402,$0402,$0402,$0402
	dc.w	$0502,$0502,$0502,$0502,$0502,$0502,$0502,$0502
	dc.w	$0602,$0602,$0602,$0602,$0602,$0602,$0602,$0602
	dc.w	$0702,$0702,$0702,$0702,$0702,$0702,$0702,$0702
	dc.w	$0802,$0802,$0802,$0802,$0802,$0802,$0802,$0802
	dc.w	$0902,$0902,$0902,$0902,$0902,$0902,$0902,$0902
	dc.w	$0a02,$0a02,$0a02,$0a02,$0a02,$0a02,$0a02,$0a02
	dc.w	$0b02,$0b02,$0b02,$0b02,$0b02,$0b02,$0b02,$0b02
	dc.w	$0c03,$0c03,$0c03,$0c03,$0d03,$0d03,$0d03,$0d03
	dc.w	$0e03,$0e03,$0e03,$0e03,$0f03,$0f03,$0f03,$0f03
	dc.w	$1003,$1003,$1003,$1003,$1103,$1103,$1103,$1103
	dc.w	$1203,$1203,$1203,$1203,$1303,$1303,$1303,$1303
	dc.w	$1403,$1403,$1403,$1403,$1503,$1503,$1503,$1503
	dc.w	$1603,$1603,$1603,$1603,$1703,$1703,$1703,$1703
	dc.w	$1804,$1804,$1904,$1904,$1a04,$1a04,$1b04,$1b04
	dc.w	$1c04,$1c04,$1d04,$1d04,$1e04,$1e04,$1f04,$1f04
	dc.w	$2004,$2004,$2104,$2104,$2204,$2204,$2304,$2304
	dc.w	$2404,$2404,$2504,$2504,$2604,$2604,$2704,$2704
	dc.w	$2804,$2804,$2904,$2904,$2a04,$2a04,$2b04,$2b04
	dc.w	$2c04,$2c04,$2d04,$2d04,$2e04,$2e04,$2f04,$2f04
	dc.w	$3005,$3105,$3205,$3305,$3405,$3505,$3605,$3705
	dc.w	$3805,$3905,$3a05,$3b05,$3c05,$3d05,$3e05,$3f05
	dc.w	$0000
