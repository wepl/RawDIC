;*---------------------------------------------------------------------------
;  :Program.	test.islave.asm
;  :Contents.	Imager for testing
;  :Author.	Wepl
;  :Version.	$Id: Test.ISlave.asm 1.1 2006/01/30 19:05:20 wepl Exp wepl $
;  :History.	17.01.05 started
;  :Requires.	-
;  :Copyright.	Public Domain
;  :Language.	68000 Assembler
;  :Translator.	Barfly V2.9
;  :To Do.
;---------------------------------------------------------------------------*

	INCDIR	Includes:
	INCLUDE	RawDic.i

	IFD BARFLY
	OUTPUT	"Develop:Projects/RawDIC/Test.ISlave"
	BOPT	O+			;enable optimizing
	BOPT	OG+			;enable optimizing
	BOPT	ODd-			;disable mul optimizing
	BOPT	ODe-			;disable mul optimizing
	ENDC

;============================================================================

	SECTION a,CODE

		SLAVE_HEADER
		dc.b	1		; Slave version
		dc.b	0		; Slave flags
		dc.l	_disk1		; Pointer to the first disk structure
		dc.l	_text		; Pointer to the text displayed in the imager window

		dc.b	"$VER: "
_text		dc.b	"Test Imager",10
		dc.b	"Done by Wepl, Version 1.0 "
	DOSCMD	"WDate >T:date"
	INCBIN	"T:date"
		dc.b	".",0
_disk1name	dc.b	"the first disk of that game",0
;_disk1name	dc.b	"the first disk of that game123456789012345678.....",0
	EVEN

;============================================================================

	;original release
_disk1		dc.l	0		; Pointer to next disk structure
		dc.w	2		; Disk structure version
		dc.w	0		; Disk flags
		dc.l	_tl1		; List of tracks which contain data
		dc.l	0		; UNUSED, ALWAYS SET TO 0!
		dc.l	0		; List of files to be saved
		dc.l	0		; Table of certain tracks with CRC values
		dc.l	0		; Alternative disk structure, if CRC failed
		dc.l	0		; Called before a disk is read
		dc.l	0		; Called after a disk has been read
		dc.l	_disk1name	; Name of Disk

_tl1		TLENTRY	158,159,$1770,$4489,_decode1
		TLENTRY	164,164,$1770,$4489,_decode1
		TLEND

_decode1
		move.l	#$55555555,d3
		move.w	#$1770/4-1,d2
.1		movem.l	(a0)+,d0-d1
		and.l	d3,d0
		and.l	d3,d1
		add.l	d0,d0
		or.l	d1,d0
		move.l	d0,(a1)+
		dbf	d2,.1

		moveq	#IERR_OK,d0
		rts

;============================================================================

	END

