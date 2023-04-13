;*---------------------------------------------------------------------------
;  :Program.	test.islave.asm
;  :Contents.	Imager for testing
;  :Author.	Wepl
;  :Version.	$Id: Test.ISlave.asm 1.2 2017/11/09 00:40:11 wepl Exp wepl $
;  :History.	17.01.05 started
;		07.11.17 comments improved
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
		dc.b	5		;slv_Version   Slave & required RawDIC version
		dc.b	0		;slv_Flags
		dc.l	_disk1v1	;slv_FirstDisk
		dc.l	_text		;slv_Text      text displayed in the imager window

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
_disk1v1	dc.l	_disk2		;dsk_NextDisk  Pointer to next disk structure
		dc.w	2		;dsk_Version   structure version
		dc.w	0		;dsk_Flags
		dc.l	_tl1		;dsk_TrackList List of tracks which contain data
		dc.l	0		;UNUSED
		dc.l	FL_DISKIMAGE	;dsk_FileList  list of files to be saved or FL_NULL/FL_DISKIMAGE
		dc.l	_crc1v1		;dsk_CRCList   optional table of certain tracks with CRC values
		dc.l	_disk1v2	;dsk_AltDisk   Alternative disk structure, if CRC failed
		dc.l	0		;dsk_InitCode  Called before a disk is read
		dc.l	_savefiles	;dsk_DiskCode  Called after a disk has been read
		dc.l	_disk1name	;dsk_DiskName  Name of Disk, requires structure version 2

_disk1v2	dc.l	_disk2		;dsk_NextDisk  Pointer to next disk structure
		dc.w	2		;dsk_Version   structure version
		dc.w	0		;dsk_Flags
		dc.l	_tl1		;dsk_TrackList List of tracks which contain data
		dc.l	0		;UNUSED
		dc.l	FL_DISKIMAGE	;dsk_FileList  list of files to be saved or FL_NULL/FL_DISKIMAGE
		dc.l	_crc1v2		;dsk_CRCList   optional table of certain tracks with CRC values
		dc.l	0		;dsk_AltDisk   Alternative disk structure, if CRC failed
		dc.l	0		;dsk_InitCode  Called before a disk is read
		dc.l	_savefiles	;dsk_DiskCode  Called after a disk has been read
		dc.l	_disk1name	;dsk_DiskName  Name of Disk

_disk2		dc.l	0		;dsk_NextDisk  Pointer to next disk structure
		dc.w	1		;dsk_Version   structure version
		dc.w	0		;dsk_Flags
		dc.l	_tl1		;dsk_TrackList List of tracks which contain data
		dc.l	0		;UNUSED
		dc.l	FL_DISKIMAGE	;dsk_FileList  list of files to be saved or FL_NULL/FL_DISKIMAGE
		dc.l	0		;dsk_CRCList   optional table of certain tracks with CRC values
		dc.l	0		;dsk_AltDisk   Alternative disk structure, if CRC failed
		dc.l	0		;dsk_InitCode  Called before a disk is read
		dc.l	_savefiles	;dsk_DiskCode  Called after a disk has been read

_tl1		TLENTRY	0,0,$1600,$4489,DMFM_STD
		TLENTRY	1,1,$1770,$4489,_decode1
		TLENTRY	2,2,$1600,$4489,DMFM_STD
		TLEND

_crc1v1		CRCENTRY 0,$4e4d
		CRCEND

_crc1v2		CRCENTRY 0,$0df3
		CRCEND

_decode1	move.l	#$55555555,d3
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
; D0 = disk number
; A0 = pointer to disk structure
; A1 = pointer to disk image (requires RawDIC/ISlave v5)

_savefiles
		moveq	#IERR_OK,d0
		rts

;============================================================================

	END

