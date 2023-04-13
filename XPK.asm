;# xpk_unPACK
;# a0.l	^SRC
;# a1.l	^DEST
;# d0.l	#BYTES IN SRC AREA! ($8000 eg- just greater!)
;# d1.l	#BYTES FREE IN DEST AREA!
;# RESULT:
;# d0.l	LEN of DeCrunched data  (0=ERR)

XPK_unPACK:	movem.l	d1-d7/a0-a6,-(a7)
		lea	.xpktaglist(pc),a2
		move.l	a0,.SRC-.xpktaglist(a2)
		move.l	a1,.DST-.xpktaglist(a2)
		move.l	d0,.SLEN-.xpktaglist(a2)
		move.l	d1,.DLEN-.xpktaglist(a2)
		move.l	4(a0),d0
		addq.l	#8,d0
		exg	a0,a2
		move.l	xpkbase(pc),a6
		jsr	-48(a6)			;# xpkunPACK
		tst.l	d0
		bne.s	.ERR
		move.l	12(a2),d0		;# GET LEN
		bra.s	.END
.ERR:		moveq	#0,d0			;# 0 bytes packed!
.END:		movem.l	(a7)+,d1-d7/a0-a6
		rts

XPKTag		equ	$80000000+'XP'

.xpktaglist:	dc.l	XPKTag+3			;# ^source
.SRC:		dc.l	0
		dc.l	XPKTag+$20		;# #source len
.SLEN:		dc.l	0
		dc.l	XPKTag+$12		;# ^destination
.DST:		dc.l	0
		dc.l	XPKTag+$21		;# #destination len
.DLEN:		dc.l	0
		dc.l	0			;# end of tags
