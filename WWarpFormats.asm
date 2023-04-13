;----------------------------------------
; encode a long to mfm
; IN:	D2 = ULONG data long to encode
;	D3 = ULONG $55555555
;	A0 = APTR  destination mfm buffer
; OUT:	D0/D1 destroyed
;	A0 = A0 + 4

_encode_longodd	lsr.l	#1,d2
_encode_long	and.l	d3,d2
		move.l	d2,d0
		eor.l	d3,d0
		move.l	d0,d1
		add.l	d0,d0
		lsr.l	#1,d1
		bset	#31,d1
		and.l	d0,d1
		or.l	d1,d2
		btst	#0,-1(a0)
		beq	.ok
		bclr	#31,d2
.ok		move.l	d2,(a0)+
		rts

;----------------------------------------
; encode gremlin track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_grem	movem.l	d2-d7,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#$3000+20,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap
		move.l	#$44894489,(a0)+	;4
		move.l	#$44895555,(a0)+	;4
		lea	(gl_tmpbuf,GL),a1
		moveq	#0,d5
		move.w	#$1800/2-1,d6
.data		move.w	(a1)+,d2		;$3000
		add.w	d2,d5
		move.w	d2,d0
		swap	d2
		lsr.w	#1,d0
		move.w	d0,d2
		bsr	_encode_long
		dbf	d6,.data
		move.w	d5,d2
		swap	d2
		lsr.w	#1,d5
		move.w	d5,d2
		bsr	_encode_long		;4
		move.w	(gl_trk+wth_num,GL),d2
		bchg	#0,d2
		move.w	d2,d0
		swap	d2
		lsr.w	#1,d0
		move.w	d0,d2
		bsr	_encode_long		;4
		moveq	#0,d2
		bsr	_encode_long		;4
		
		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode rob track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_rob	movem.l	d2-d7/a2-a3,-(a7)

		move.w	d0,a3			;A3 = track number
		move.l	(gl_chipbuf,GL),a0
		lea	(gl_tmpbuf,GL),a1
		move.l	(a1)+,d6		;D6 = diskkey
		move.l	#$55555555,d3

		move.l	(gl_writelen,GL),d0
		sub.w	#2+(12*$40c)+4,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.w	#$1448,(a0)+		;2

		moveq	#0,d7			;D7 = sector

.sector		move.w	#$4891,(a0)+		;2

		move.l	a1,a2
		moveq	#512/4-1,d2
		moveq	#0,d4
.chksum		move.l	(a2)+,d0
		eor.l	d0,d4
		dbf	d2,.chksum
		
		move.l	d4,d0
		lsr.l	#1,d0
		eor.l	d0,d4
		and.l	d3,d4
		move.l	d4,d0
		swap	d0
		add.w	d0,d0
		or.w	d0,d4			;chksum
		
		move.b	d7,d2			;sector
		lsl.w	#8,d2
		add.l	a3,d2			;track
		swap	d2
		move.w	d4,d2			;chksum
		eor.l	d6,d2			;diskkey
		
		move.l	d2,d4
		bsr	_encode_longodd		;4
		move.l	d4,d2
		bsr	_encode_long		;4

		bclr	#31,d6

		move.l	a1,a2
		moveq	#512/4-1,d4
.odd		move.l	(a2)+,d2
		eor.l	d6,d2
		bsr	_encode_longodd		;$200
		dbf	d4,.odd

		moveq	#512/4-1,d4
.even		move.l	(a1)+,d2
		eor.l	d6,d2
		bsr	_encode_long		;$200
		dbf	d4,.even
		
		bset	#31,d6
		
		move.w	#$aaaa,d0
		btst	#0,-1(a0)
		beq	.ok
		bclr	#15,d0
.ok		move.w	d0,(a0)+		;2

		addq.w	#1,d7
		cmp.w	#12,d7
		bne	.sector
		
		moveq	#0,d2
		bsr	_encode_long		;4
		
		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode psygnosis track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_pmover	movem.l	d2-d7,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($18A0*2)+16+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap
		move.l	#$448A448A,(a0)+	;4
		move.l	d3,(a0)+		;D3 = $55555555
		lea	(gl_tmpbuf,GL),a1

		move.l	a0,-(sp)
		move.l	d3,d2			;skip checksum (it will be calculated later)
		bsr	_encode_long
		move.l	d3,d2
		bsr	_encode_long

		moveq	#0,d5
		move.w	#$18A0/2-1,d6
.data		move.w	(a1)+,d2
		move.w	d2,d0
		swap	d2
		lsr.w	#1,d0
		move.w	d0,d2
		bsr	_encode_long
		add.w	-4(a0),d5
		add.w	-2(a0),d5
		dbf	d6,.data

		moveq	#0,d2
		bsr	_encode_long		;4
		move.l	(sp)+,a0

		move.w	d5,d2			;D5 = checksum
		swap	d2
		lsr.w	#1,d5
		move.w	d5,d2
		bsr	_encode_long		;4

		move.w	#$000A,d2
		btst	#0,-1(a0)
		beq	.even
		or.w	#$4000,d2
.even		move.w	d2,d0
		swap	d2
		lsr.w	#1,d0
		move.w	d0,d2
		bsr	_encode_long

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode psygnosis track (beast 1)
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_beast1	movem.l	d2-d7,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1838*2)+10+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap
		move.w	#$4489,(a0)+		;2

		lea	(gl_tmpbuf,GL),a1

		move.l	#"SOTB",d4		;encode "SOTB"
		move.l	d4,d2
		bsr	_encode_longodd		;4
		move.l	d4,d2
		bsr	_encode_long		;4

		moveq	#0,d5
		move.w	#$1838/4-1,d6
.data		move.l	(a1)+,d4
		move.l	d4,d2
		bsr	_encode_longodd
		move.l	d4,d2
		bsr	_encode_long
		dbf	d6,.data

		moveq	#0,d2
		bsr	_encode_long		;4

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode psygnosis track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_beast2	movem.l	d2-d7,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($189C*2)+10+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap
		move.w	#$4489,(a0)+		;2

		lea	(gl_tmpbuf,GL),a1

		move.l	#"BST2",d4		;encode "BST2"
		move.l	d4,d2
		bsr	_encode_longodd		;4
		move.l	d4,d2
		bsr	_encode_long		;4

		moveq	#0,d5
		move.w	#$189C/4-1,d6
.data		move.l	(a1)+,d4
		move.l	d4,d2
		bsr	_encode_longodd
		move.l	d4,d2
		bsr	_encode_long
		dbf	d6,.data

		moveq	#0,d2
		bsr	_encode_long		;4

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode Blood Money track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_bloodmoney
		movem.l	d2-d7,-(a7)
		move.l	d0,d4
		lsr.w	#1,d4			;D4 = track number/2
		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#6+($1838*2)+4+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.w	#$4489,(a0)+		;2
		move.l	#$552A2A55,(a0)+	;4
		lea	(gl_tmpbuf,GL),a1

		moveq	#0,d5
		move.w	#$1838/2-1,d6
.encode		move.w	(a1)+,d0
		add.w	d0,d5			;D5 = checksum
		eor.w	d4,d0
		move.w	d0,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d0,d2
		bsr	_encode_long
		dbf	d6,.encode

	;encode checksum
		eor.w	d4,d5
		move.w	d5,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d5,d2
		bsr	_encode_long		;4

		moveq	#0,d2
		bsr	_encode_long		;4
		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode psygnosis1 track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_psygnosis1
		movem.l	d2-d7,-(a7)
		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#(6*$402*2)+6+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.w	#$4489,(a0)+		;2
		move.l	#$552AAAAA,(a0)+	;4
		lea	(gl_tmpbuf,GL),a1

		moveq	#6-1,d6
.track		bsr	.checksum
		move.w	d0,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d0,d2
		bsr	_encode_long		;4

		move.w	#$400/2-1,d5
.sector		move.w	(a1)+,d0
		move.w	d0,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d0,d2
		bsr	_encode_long
		dbf	d5,.sector
		dbf	d6,.track

		moveq	#0,d2
		bsr	_encode_long		;4
		movem.l	(a7)+,_MOVEMREGS
		rts

.checksum	move.l	a1,-(sp)
		move.w	#$400/2-1,d1
		moveq	#0,d0
.1		add.w	(a1)+,d0
		dbf	d1,.1
		move.l	(sp)+,a1
		rts

;----------------------------------------
; encode Turrican track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_turrican1
		move.l	#$1978,d1

_encode_tur	movem.l	d2-d7,-(a7)
		move.l	d1,d7			;D7 = decoded length

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		move.w	d7,d2			;D7 = datalen
		add.w	d2,d2
		add.w	#12+2,d2
		sub.w	d2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$95212AAA,(a0)+	;4
		lea	(gl_tmpbuf,GL),a1
		moveq	#0,d5
		move.w	d7,d6			;D7 = datalen
		lsr.w	#2,d6
		subq.w	#1,d6
.data		move.l	(a1)+,d4
		move.l	d4,d2
		bsr	_encode_longodd
		eor.l	d2,d5
		move.l	d4,d2
		bsr	_encode_long
		eor.l	d2,d5			;D5 = checksum
		dbf	d6,.data

	;encode checksum
		and.l	d3,d5
		move.l	d5,d2
		bsr	_encode_longodd
		move.l	d5,d2
		bsr	_encode_long

		moveq	#0,d2
		bsr	_encode_long		;4
		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode Turrican 2 track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_turrican2
		move.l	#$1A90,d1
		bra	_encode_tur

;----------------------------------------
; encode Turrican 3 track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_turrican3a
		movem.l	d2-d7,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1800*2)+12+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$44892AAA,(a0)+	;4
		lea	(gl_tmpbuf,GL),a1
		moveq	#0,d5
		move.w	#$1800/4-1,d6
.data		move.l	(a1)+,d4
		add.l	d4,d5			:D5 = checksum
		move.l	d4,d2
		bsr	_encode_longodd
		move.l	d4,d2
		bsr	_encode_long
		dbf	d6,.data

	;encode checksum
		move.l	d5,d2
		bsr	_encode_longodd
		move.l	d5,d2
		bsr	_encode_long

		moveq	#0,d2
		bsr	_encode_long		;4

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode Turrican 3 track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_turrican3b
		movem.l	d2-d7,-(a7)

		move.l	d0,d4

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#4+4+($1A00*2)+8+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$44892AA5,(a0)+	;4

	;encode	track number
		lsr.w	#1,d4
		move.w	d4,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d4,d2
		bsr	_encode_long		;4

		lea	(gl_tmpbuf,GL),a1
		moveq	#0,d5
		move.w	#$1A00/4-1,d6
.data		move.l	(a1)+,d4
		add.l	d4,d5
		move.l	d4,d2
		bsr	_encode_longodd
		move.l	d4,d2
		bsr	_encode_long
		dbf	d6,.data

	;encode checksum
		move.l	d5,d2
		bsr	_encode_longodd		;4
		move.l	d5,d2
		bsr	_encode_long		;4

		moveq	#0,d2
		bsr	_encode_long		;4

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode ocean track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_ocean	movem.l	d2-d7/a0-a6,-(a7)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($404*12)+6+4,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.w	#$4489,(a0)+		;2
		move.l	#$2aaaaaaa,(a0)+	;4

		lea	(gl_tmpbuf,GL),a1
		move.l	a1,a2

		moveq	#12-1,d5		;d5 = Sectors left to encode

.next_sector	move.w	#($200/4)-1,d6		;Encode data
		moveq	#0,d7

.data_sector	move.l	(a1)+,d4		;$400
		eor.l	d4,d7
		move.l	d4,d2
		bsr	_encode_longodd
		move.l	d4,d2
		bsr	_encode_long
		dbf	d6,.data_sector

		move.w	d7,d2
		lsr.w	#1,d2
		swap	d2
		move.w	d7,d2
		bsr	_encode_long		;4

		dbf	d5,.next_sector

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7/a0-a6
		rts

;----------------------------------------
; encode vision track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_vision	movem.l	d2-d7/a0-a6,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number
		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1800*2)+20+4,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$44892aaa,(a0)+	;4

		moveq	#0,d7
		move	#0,ccr			;clear x-flag
		move.l	#($1800/4)-1,d0
		lea	(gl_tmpbuf,GL),a1
.chksum		move.l	(a1)+,d1
		addx.l	d1,d7
		dbf	d0,.chksum
		neg.l	d7
		subq.l	#1,d7			;d7 = chksum
		
		lea	(gl_tmpbuf,GL),a1
		move.w	#($1800/4)-1,d6		;Encode odd data
.data_odd	move.l	(a1)+,d2		;$1800
		bsr	_encode_longodd
		dbf	d6,.data_odd

		moveq	#0,d2			;unknown longword (not used)
		bsr	_encode_long		;4

		move.l	d7,d2			;encode odd checksum
		bsr	_encode_longodd		;4

		lea	(gl_tmpbuf,GL),a1
		move.w	#($1800/4)-1,d6		;Encode even data
.data_even	move.l	(a1)+,d2		;$1800
		bsr	_encode_long
		dbf	d6,.data_even

		moveq	#0,d2			;unknown longword (not used)
		bsr	_encode_long		;4

		move.l	d7,d2			;encode even checksum
		bsr	_encode_long		;4

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7/a0-a6
		rts

;----------------------------------------
; encode twilight track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_twilight
		movem.l	d2-d7,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1400*2)+16+4+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$44894489,(a0)+	;4

		move.l	d4,d2			;Encode track number
		bsr	_encode_longodd		;4
		move.l	d4,d2
		bsr	_encode_long		;4

	;chksum
		moveq	#0,d2
		bsr	_encode_long
		lea	(gl_tmpbuf,GL),a1
		move.w	#$1400/4-2,d0
		move.l	(a1)+,d2
.chksum		move.l	(a1)+,d1
		eor.l	d1,d2
		dbf	d0,.chksum
		move.l	d2,d0
		lsr.l	#1,d0
		eor.l	d0,d2
		bsr	_encode_long

	;data
		lea	(gl_tmpbuf,GL),a1
		move.w	#$1400/4-1,d6
.data1		move.l	(a1)+,d2
		bsr	_encode_longodd
		dbf	d6,.data1
		lea	(gl_tmpbuf,GL),a1
		move.w	#$1400/4-1,d6
.data2		move.l	(a1)+,d2
		bsr	_encode_long
		dbf	d6,.data2

		moveq	#0,d2
		bsr	_encode_long		;4

		movem.l	(a7)+,_MOVEMREGS
		rts

;----------------------------------------
; encode zzkj A/B/C track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data


_encode_zzkja	movem.l	d2-d7,-(sp)
		move.l	#$800,d4		;D4 = data length
		bra	_encode_zzkj

_encode_zzkjb	movem.l	d2-d7,-(sp)
		move.l	#$1000,d4		;D4 = data length
		bra	_encode_zzkj

_encode_zzkjc	movem.l	d2-d7,-(sp)
		move.l	#$1600,d4		;D4 = data length

_encode_zzkj
		move.l	d0,d5
		ext.l	d5
		lsr.l	#1,d5			;D5 = cylinder number

		lea	(gl_tmpbuf,GL),a0
		move.l	d5,d6			;D6 = checksum
		move.l	d4,d7
		lsr.l	#2,d7
		subq.l	#1,d7
.checksum	add.l	(a0)+,d6
		dbf	d7,.checksum

	;header = 32*4 byte + 5*4 on format B
		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		moveq	#30-1,d7
.sync		move.l	d3,(a0)+
		dbf	d7,.sync
		move.w	d3,(a0)+
		cmp.l	#$1600,d4		;format C?
		bne	.skip
		move.l	#$44894489,(a0)+
		move.l	#$2aaaaaaa,(a0)+
		move.l	#$aaaaaaaa,(a0)+
		move.l	#$aaaaaaaa,(a0)+
		move.l	#$aaaaaaaa,(a0)+
.skip		move.l	#$44894489,(a0)+
		move.w	#$2aaa,(a0)+
		
		move.l	d5,d2			;cylinder
		bsr	_encode_long

		lea	(gl_tmpbuf,GL),a1
		move.l	d4,d7
		lsr.l	#2,d7
		subq.l	#1,d7
.odd		move.l	(a1)+,d2
		bsr	_encode_long
		dbf	d7,.odd

		move.l	d6,d2			;checksum
		bsr	_encode_long

		move.l	d5,d2			;cylinder
		bsr	_encode_longodd

		lea	(gl_tmpbuf,GL),a1
		move.l	d4,d7
		lsr.l	#2,d7
		subq.l	#1,d7
.even		move.l	(a1)+,d2
		bsr	_encode_longodd
		dbf	d7,.even

		move.l	d6,d2			;checksum
		bsr	_encode_longodd

		move.l	(gl_writelen,GL),d7
		sub.l	d4,d7
		sub.l	d4,d7
	; we had to sub some bytes for the sync here, but it doesnt matter cause the buffer is large enough
		lsr.l	#2,d7
		btst	#0,(a0)
		bne	.gap
		add.l	d3,d3
.gap		move.l	d3,(a0)+
		dbf	d7,.gap

		movem.l	(sp)+,_MOVEMREGS
		rts

;----------------------------------------
; encode zzkj D track ($1600 bytes/track)
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_zzkjd	movem.l	d0-d7,-(sp)

		move.w	d0,d6
		ext.l	d6
		bclr	#0,d6
		lsl.l	#7,d6			;D6 = 0000ccss cylinder + sector number

		moveq	#0,d5
		move.w	d0,d5			;d5 = Track number
		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($41c*11)+4,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		lea	(gl_tmpbuf,GL),a1

.sector		move.l	#$44894489,(a0)+	;4
		move.w	#$2aaa,(a0)+		;2

		move.l	d6,d4
		move.l	#$200/4-1,d7
.checksum	add.l	(a1)+,d4		;D4 = chksum
		dbf	d7,.checksum
		sub.w	#512,a1

		move.l	d6,d2
		bsr	_encode_long		;4
		move.l	#$200/4-1,d7
.odd		move.l	(a1)+,d2
		bsr	_encode_long		;$200
		dbf	d7,.odd
		sub.w	#512,a1
		move.l	d4,d2
		bsr	_encode_long		;4

		move.l	d6,d2
		bsr	_encode_longodd		;4
		move.l	#$200/4-1,d7
.even		move.l	(a1)+,d2
		bsr	_encode_longodd		;$200
		dbf	d7,.even
		move.l	d4,d2
		bsr	_encode_longodd		;4

		moveq	#0,d2
		bsr	_encode_long		;4 (sector gap)
		move.w	#$5555,(a0)+		;2 (sector gap)

		addq.b	#1,d6
		cmp.b	#11,d6
		bne	.sector

		movem.l	(sp)+,_MOVEMREGS
		rts

;----------------------------------------
; calc specialfx sector checksum
; IN:	A2 = Sector data
;	_tmpbuf  data to encode
; OUT:	D1 = Checksum (all other registers preserved)

_sfx_checksum	movem.l	d0/d3/d7/a2,-(sp)
		move	#0,ccr			;clear x-flag
		moveq	#0,d1
		moveq	#0,d3
		move.l	#($200/2)-1,d7
.checksum_loop	andi.w	#15,d3
		move.w	(a2)+,d0
		rol.w	d3,d0
		addx.w	d0,d1
		addq.w	#1,d3
		dbra	d7,.checksum_loop
		ext.l	d1
		rol.l	#8,d1
		movem.l	(sp)+,d0/d3/d7/a2
		rts

;----------------------------------------
; encode specialfx track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_specialfx
		movem.l	d2-d7/a0-a6,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Physical track number
		sub.w	#2,d4			;Adjust track number
		bchg	#0,d4			;Flip track sides
		and.l	#$ff,d4			;d4 = Game track number
		rol.l	#8,d4			
		swap	d4
		move.w	#$c00,d4		;d4 = $xx000c00 (xx = Game track number)

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($418*12)+4,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	#$aaaa,(a0)+
		dbf	d0,.gap

		moveq	#3,d5			;d5 = First sector to write

.enc_next_sctor	move.l	#$8944aaaa,(a0)+	;4

		move.l	d4,d6
		swap	d6			;d6 = $0c00xx00 (xx = Game track number)
		move.b	d5,d6
		swap	d6			;d6 = trk.b sec.b len.w
		move.l	d6,d2
		bsr	_encode_longodd		;4
		move.l	d6,d2
		bsr	_encode_long		;4

		move.l	d5,d0			;Compute checksum for sector
		mulu	#$200,d0
		lea	(gl_tmpbuf,GL),a1
		lea	(a1,d0.l),a2
		bsr	_sfx_checksum
		move.l	d1,d6
		move.l	d6,d2
		bsr	_encode_longodd		;4
		move.l	d6,d2
		bsr	_encode_long		;4
		
		move.l	a2,a1
		move.l	#($200/4)-1,d6		;Encode odd data
.data_odd	move.l	(a1)+,d2		;$400
		bsr	_encode_longodd
		dbf	d6,.data_odd

		move.l	a2,a1
		move.l	#($200/4)-1,d6		;Encode even data
.data_even	move.l	(a1)+,d2		;$400
		bsr	_encode_long
		dbf	d6,.data_even

		moveq	#0,d2			;Sector gap
		bsr	_encode_long		;4

		sub.w	#$100,d4		;Adjust bytes left in track count

		addq	#1,d5			;Set next sector to be written
		cmp.l	#12,d5			;Check if we have wrapped around
		bne	.not_wrapped		;via 3,4,5,6,7,8,9,10,11 now 12
		moveq	#0,d5			;Wrapped around so go back to 0
		bra	.enc_next_sctor

.not_wrapped	cmp.l	#3,d5
		bne	.enc_next_sctor

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

.done		movem.l	(a7)+,d2-d7/a0-a6
		rts

;----------------------------------------
; encode tiertex track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_tiertex	movem.l	d2-d7,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1804*2)+8+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$a2454489,(a0)+	;4 (sync + $4489)

		move.l	d4,d2			;4 (track number)
		swap	d2
		bsr	_encode_longodd
		subq.l	#2,a0
		move.l	d4,d2
		swap	d2
		bsr	_encode_long
		subq.l	#2,a0

		lea	(gl_tmpbuf,GL),a1
		moveq	#0,d4			;D4 = checksum

		move.l	#$600-1,d6
.data_odd	move.l	(a1)+,d2		;$1800 (odd data)
		add.l	d2,d4
		bsr	_encode_longodd
		dbf	d6,.data_odd

		not.l	d4
		move.l	d4,d2
		bsr	_encode_longodd		;4 (odd checksum)

		lea	(gl_tmpbuf,GL),a1
		move.l	#$600-1,d6
.data_even	move.l	(a1)+,d2		;$1800 (even data)
		bsr	_encode_long
		dbf	d6,.data_even

		move.l	d4,d2
		bsr	_encode_long		;4 (even checksum)

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7
		rts

;----------------------------------------
; encode elite track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_elite	movem.l	d2-d7,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1800*2)+$c+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$91228912,(a0)+	;4 (sync + $8912)

		move.l	d4,d2			;4 (track number)
		swap	d2
		bsr	_encode_longodd
		subq.l	#2,a0
		move.l	d4,d2
		swap	d2
		bsr	_encode_long
		subq.l	#2,a0

		lea	(gl_tmpbuf,GL),a1
		moveq	#-1,d5
		eor.w	d4,d5
		move.l	#($1800/2)-1,d7
.checksum	move.w	(a1)+,d0
		eor.w	d0,d5
		dbf	d7,.checksum
		
		move.w	d5,d2			;4 (checksum)
		swap	d2
		bsr	_encode_longodd
		subq.l	#2,a0
		move.w	d5,d2
		swap	d2
		bsr	_encode_long
		subq.l	#2,a0

		lea	(gl_tmpbuf,GL),a1
		move.l	#($1800/2)-1,d6
.data_loop	move.w	(a1)+,d4		;$1800 (odd data)
		move.w	d4,d2
		swap	d2
		bsr	_encode_longodd
		subq.l	#2,a0
		move.w	d4,d2
		swap	d2
		bsr	_encode_long		;$1800 (even data)
		subq.l	#2,a0
		dbf	d6,.data_loop

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7
		rts

;----------------------------------------
; encode goliath track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_goliath	movem.l	d2-d7,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1600*2)+$10+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$8a512aaa,(a0)+	;4 (sync + $2aaa)
		move.l	#$aaaaaaa5,(a0)+	;4 ($aaaaaaa5)

		moveq	#0,d5			;Checksum
		lea	(gl_tmpbuf,GL),a1
		move.l	#($1600/4)-1,d6
.data_loop	move.l	(a1)+,d4
		add.l	d4,d5
		move.l	d4,d2
		bsr	_encode_longodd		;$1600 (odd data)
		move.l	d4,d2
		bsr	_encode_long		;$1600 (even data)
		dbf	d6,.data_loop

		move.l	d5,d2
		bsr	_encode_longodd		;$4 checksum (odd data)
		move.l	d5,d2
		bsr	_encode_long		;$4 checksum (even data)

		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7
		rts

;----------------------------------------
; encode thalamus track
; IN:	D0 = UWORD track number
;	_tmpbuf  data to encode
; OUT:	_chipbuf encoded data

_encode_thalamus	
		movem.l	d2-d7,-(a7)

		moveq	#0,d4
		move.w	d0,d4			;d4 = Track number

		move.l	(gl_chipbuf,GL),a0
		move.l	#$55555555,d3
		move.l	(gl_writelen,GL),d0
		sub.w	#($1810*2)+$8+2,d0
		lsr.w	#1,d0
		subq.w	#1,d0
.gap		move.w	d3,(a0)+
		dbf	d0,.gap

		move.l	#$22912291,(a0)+	;4 (sync + sync)

		lea	(gl_tmpbuf,GL),a1
		move.l	#($1810/4)-1,d6
.data_odd	move.l	(a1)+,d2
		bsr	_encode_longodd		;$1810 (odd data)
		dbf	d6,.data_odd

		lea	(gl_tmpbuf,GL),a1
		move.l	#($1810/4)-1,d6
.data_even	move.l	(a1)+,d2		;$1810 (even data)
		bsr	_encode_long
		dbf	d6,.data_even

		lea	(gl_tmpbuf,GL),a1	;Calculate checksum
		moveq	#0,d5
		move.l	#$1810-1,d6
.calc_csum_loop	move.b	(a1)+,d1
		eor.b	d1,d5
		dbra	d6,.calc_csum_loop

		move.l	d5,d2			;Encode checksum ($0000xxyy)
		rol.l	#7,d2			;where $xx = odd data and 
		move.b	d5,d2			;$yy = even data
		bsr	_encode_long		;Already taken care of shift
		moveq	#0,d2			;Safety
		bsr	_encode_long		;4

		movem.l	(a7)+,d2-d7
		rts
