;*---------------------------------------------------------------------------
; Program:	readfromfile.asm
; Contents:	Input file routines for "RawDIC" (c) John Selck and Codetapper
; Author:	Codetapper
; History:	12.10.02 - v1.0 (Codetapper)
;		         - Supports ADF, MFMWarp and NOMADWarp files
;		         - Detects DMS, MOKWarp, PhilWarp and WWarp files
;		24.10.02 - WWarp Raw and Dos tracks support added
;		25.10.02 - Supports all packed track formats for WWarp files
;		26.10.02 - Added Vision packed track format for WWarp
;		28.10.02 - Fixed detection of ADF type
;		04.12.02 - Added Ocean, Vision, Twilight, ZZKJ, SpecialFX
;		           and DOSF track types
;		08.12.02 - Added Tiertex and Elite track types
;		         - Fixed bug with d0 not being passed as track num
;		03.01.04 - Added Goliath and Thalamus track types
;		01.02.04 - Select File now allows you to pick any file
;		           and switch disks etc
;		         - TFB_RAWSINGLE flag in WWarp now obeyed correctly
;		           (thanks to Psygore for the report and code!)
;		         - Drive light no longer turns on and off if you
;		           are reading from an input file
;		30.07.04 Wepl
;			 - requester handling rewritten, remember last file
;			   in env "RawDIC.InputFile"
; Copyright:	Public Domain
; Language:	68000 Assembler
; Translator:	Barfly
; To Do:	Add new flag SLEQ
;---------------------------------------------------------------------------*

; MFM error messages

TYPE_DISK	equ	0
TYPE_ADF	equ	1
TYPE_MFMWARP	equ	2
TYPE_NOMADWARP	equ	3
TYPE_WWARP	equ	4

BUFFER_EMPTY	equ	0
BUFFER_ADOS	equ	1
BUFFER_MFM	equ	2

NO_MFM_OFFSET	equ	-1			;Track has no known offset (yet?)

MAX_TRK_OFFSETS	equ	170

xx_ReadFromErr	dc.l	0			;Error number returned
xx_BufferWanted	dc.l	0			;What kind of data we want in the buffer (BUFFER_ADOS, BUFFER_MFM etc)
xx_BufferType	dc.l	0			;Indicate what kind of data is in the buffer
xx_ReadFromPtr	dc.l	0
xx_ReadFromType	dc.w	0			;File type (0 = Disk, 1 = ADF, 2 = MFMWarp)
xx_ReadFromTrk	dc.w	0			;Actual track we want
mfm_Header	ds.l	4			;FRX!WARP v1.0000
mfm_PackedLen	dc.l	0			;Bytes
mfm_TrackNum	dc.l	0			;$00000000
mfm_Checksum	dc.l	0			;$xxxxxxxx
mfm_ID		dc.l	0			;'MFM '
mfm_BestOffset	dc.l	0			;Best offset to read an unknown track
mfm_Offsets	ds.l	MAX_TRK_OFFSETS+1

;---------------------------------------------------------------------

		EVEN
nmd_TrackNum	dc.l	0
nmd_Header	dc.b	'Warp v1.1',0		;NOMADWarp Header ($62 bytes)
nmd_Cylinder	dc.w	0			;$a
nmd_TopBottom	dc.l	'TOP'<<8		;$c
		ds.l	4
		dc.w	0
nmd_11111111	dc.l	$11111111
nmd_DepackedLen	dc.l	0
nmd_PackedLen	dc.l	0
		dc.l	0
nmd_WhatMakesMe	dc.b	'What does not destroy me,only makes me stronger '

;---------------------------------------------------------------------

		EVEN
wwp_TableHeader	ds.b	10			;WWarp table header (10 bytes)
		EVEN
wwp_TrackHeader	ds.b	wth_data
wwp_TrackLength	dc.l	0			;Track length in bytes!
		EVEN

GL	EQUR	A4		;a4 ptr to Globals

	STRUCTURE	Globals,0
		APTR	gl_chipbuf
		ULONG	gl_writelen		;amount of bytes to write
		STRUCT	gl_trk,wth_data		;track
		STRUCT	gl_4syncs,8		;4 syncs before the buffer
		LABEL	gl_tmpbuf

;---------------------------------------------------------------------

InputFromFile	move.l	#BUFFER_EMPTY,xx_BufferType	;Mark the buffer as empty

		move.l	xx_InputName(pc),a0		;If there is no filename to read from, don't bother doing anything
		tst.b	(a0)
		beq	.NoReadFromFile

		move.l	xx_CurrentDecoder(pc),d0	;Work out if we should accept AmigaDos decoded or MFM data (or both)

		move.l	#BUFFER_ADOS|BUFFER_MFM,d1
		cmp.l	#DMFM_STD,d0			;Accept Dos or MFM data
		beq	.ReadToBuffer
		move.l	#BUFFER_MFM,d1			;Accept only MFM data if it's a raw track decoder

.ReadToBuffer	move.l	d1,xx_BufferWanted		;Store the type of buffer data we will accept

		bsr	GetAbsTrackNum			;Work out the actual track
		bsr	ReadMFMFromFile

		rts

.NoReadFromFile	moveq	#IERR_OK,d0			;No error and buffer empty (read from disk)
		rts

;---------------------------------------------------------------------

ReadMFMFromFile	movem.l	d1-d7/a0-a6,-(sp)

		lea	xx_ReadFromTrk(pc),a0	;Store the track we are after
		move.w	d0,(a0)

		move.l	xx_MFMBuffer(pc),a0
		bsr	ReadMFMTrack

		lea	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),(a0)
		lea	xx_RawBit(pc),a0
		clr.l	(a0)			;initialize bitpointer

		movem.l	(sp)+,d1-d7/a0-a6
		rts

;---------------------------------------------------------------------
; ReadMFMTrack	
; d0.w = Track we want loaded
; a0   = Buffer
;>d0.l = Number of bytes of data now in the buffer

ReadMFMTrack	move.l	d0,d4
		move.l	a0,a4

		move.l	xx_ReadFromPtr(pc),d0
		tst.l	d0
		bne	.FileIsOpen

		bsr	OpenReadFrom		;Open file and identify it

		tst.l	d0
		bne	.Rts

.FileIsOpen	move.w	xx_ReadFromType(pc),d1

		cmp.w	#TYPE_ADF,d1
		beq	.ReadADFTrack

		cmp.w	#TYPE_MFMWARP,d1
		beq	.ReadMFMWarpTrk

		cmp.w	#TYPE_NOMADWARP,d1
		beq	.ReadNOMADTrk

		cmp.w	#TYPE_WWARP,d1
		beq	.ReadWWarpTrk

		moveq	#IERR_FORMAT_UNS,d0
.Rts		rts

;---------------------------------------------------------------------

.ReadADFTrack	move.l	xx_BufferWanted(pc),d1	;If we are reading from an ADF, check 
		and.l	#BUFFER_ADOS,d1		;that AmigaDos data is allowed. Abort
		beq	.CantSupplyData		;if it isn't! (Game must want MFM)

		move.l	xx_ReadFromPtr(pc),d1	;Handle reading from an ADF file
		moveq	#0,d2
		move.w	d4,d2
		mulu	#$1600,d2
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6

		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr

		move.l	xx_Track(pc),a0
		move.l	xx_CurrentTLen(pc),d0
;		move.l	d0,$fc			;$fc = length game wanted <------------ CHECK!
		move.l	#$1600,d0
		move.l	d0,d2
		bsr	ReadBytes
		tst.l	d0			;If no bytes were read, it's not there! (end of file)
		beq	.TrkMissingErr
		cmp.l	d0,d2			;Check we read all bytes
		bne	.MFMReadErr

		move.l	#BUFFER_ADOS,xx_BufferType	;Mark the buffer as full of AmigaDos data
		moveq	#IERR_OK,d0
		rts

;---------------------------------------------------------------------

.ReadMFMWarpTrk	move.l	xx_BufferWanted(pc),d5	;If we are reading from a MFMWarp file,
		and.l	#BUFFER_MFM,d5		;check that MFM data is allowed. Abort
		beq	.CantSupplyData		;if it isn't!

		moveq	#0,d5			;d5 = Marker to show we have already seeked back to track 0 (stop an infinite loop)

		move.l	d4,d0
		bsr	FindOffset
		move.l	d0,d2			;d2 = Best offset to go to
		cmp.l	#NO_MFM_OFFSET,d2
		bne	.FirstSeek
		move.l	#$10,d2			;First track header in a MFMWarp file is always at $10

.FirstSeek	move.l	xx_ReadFromPtr(pc),d1	;Skip past this track
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6

		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr
		move.l	d2,d6			;d6 = Current position

.ReadNext	lea	mfm_PackedLen(pc),a0
		moveq	#$10,d0
		bsr	ReadBytes
		tst.l	d0			;If no bytes were read, it's not there! (end of file)
		beq	.TrkMissingErr
		cmp.l	#$10,d0			;Check we read all $10 header bytes
		bne	.MFMReadErr

		move.l	mfm_ID(pc),d0		;Check we have the MFM header bytes
		cmp.l	#'MFM ',d0
		bne	.MFMBadHeader

		move.l	mfm_PackedLen(pc),d0	;Check for a corrupt file (length > $6c00)
		move.l	xx_RawLength(pc),d1
		cmp.l	d0,d1
		ble	.IllegalLenErr

		move.l	mfm_TrackNum(pc),d0	;Store this offset
		move.l	d6,d1
		bsr	StoreOffset

		add.l	mfm_PackedLen(pc),d6
		add.l	#$10,d6
		move.l	d6,mfm_BestOffset

		move.l	mfm_TrackNum(pc),d0	;Check track in buffer with track we want
		cmp.w	d0,d4
		beq	.MFMRightTrack
		blt	.TrkMissingErr		;We wanted a track which isn't in the MFMWarp file

		move.l	xx_ReadFromPtr(pc),d1	;Skip past this track
		move.l	mfm_PackedLen(pc),d2
		move.l	#OFFSET_CURRENT,d3
		move.l	dosbase(pc),a6
		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr

;		add.l	#$10,d2
;		add.l	d2,d6
		bra	.ReadNext

;.SeekMFMStart	tst.w	d5			;Track is not in the MFMWarp file
;		bne	.TrkMissingErr
;
;		moveq	#-1,d5
;		move.l	xx_ReadFromPtr(pc),d1
;		moveq	#$10,d2			;Skip MFMWarp header
;		move.l	#OFFSET_BEGINNING,d3
;		move.l	dosbase(pc),a6
;		jsr	_LVOSeek(a6)
;		cmp.l	#-1,d0
;		beq	.MFMSeekErr
;		bra	.ReadNext

.MFMRightTrack	move.l	xx_RawBuffer(pc),a0
		move.l	mfm_PackedLen(pc),d0
		move.l	d0,d2
		bsr	ReadBytes
		cmp.l	d0,d2
		bne	.MFMReadErr

		move.l	xpkbase(pc),d1		;Abort if no xpkmaster.library
		beq	.XPKDepackErr

		move.l	xx_RawBuffer(pc),a0
		move.l	xx_MFMBuffer(pc),a1
		move.l	xx_RawLength(pc),d1	;<-- check this!
		cmp.l	#'XPKF',(a0)
		bne	.notxpk
		bsr	XPK_unPACK
		tst.l	d0
		beq	.XPKDepackErr
		bra	.depackmc1

.notxpk		bsr	CopyRawToMFMBuf		;Handle file not being XPK packed (rare)

.depackmc1	move.l	xx_MFMBuffer(pc),a0
		move.l	xx_RawBuffer(pc),a1
		cmp.l	#'MC1!',(a0)
		bne	.depackdip
		bsr	MFMDeCrunch
		tst.l	d0
		beq	.MC1DepackErr
		bsr	CopyRawToMFMBuf

.depackdip	move.l	xx_MFMBuffer(pc),a0
		move.l	xx_RawBuffer(pc),a1
		cmp.l	#'DIP!',(a0)
		bne	.verifymfm
		bsr	DoubleIndexUnPack
		tst.l	d0
		beq	.DIPDepackErr
;		bsr	CopyRawToMFMBuf		;No need to copy to MFM buffer

.verifymfm	;move.l	xx_MFMBuffer(pc),a0
		move.l	xx_RawBuffer(pc),a0
		bsr	Calc_FRXCHKSUM
		move.l	mfm_Checksum(pc),d1
		cmp.l	d0,d1
		bne	.MFMChecksumErr

;		move.l	mfm_TrackNum(pc),d0	;Store this offset
;		move.l	d6,d1
;		bsr	StoreOffset

		move.l	#BUFFER_MFM,xx_BufferType	;Mark the buffer as full of MFM data
		moveq	#IERR_OK,d0
		rts

.MFMOpenErr	moveq	#IERR_INP_OPEN,d0
		rts

.MFMSeekErr	moveq	#IERR_INP_SEEK,d0
		rts

.MFMReadErr	moveq	#IERR_INP_READ,d0
		rts

.TrkMissingErr	moveq	#IERR_INP_NOTRK,d0
		rts

.MFMBadHeader	moveq	#IERR_INP_BADHD,d0
		rts

.XPKDepackErr	moveq	#IERR_XPK_DEPACK,d0
		rts

.MC1DepackErr	moveq	#IERR_MC1_DEPACK,d0
		rts

.DIPDepackErr	moveq	#IERR_DIP_DEPACK,d0
		rts

.MFMChecksumErr	moveq	#IERR_MFM_CSUM,d0
		rts

.IllegalLenErr	moveq	#IERR_INP_ILLEN,d0
		rts

.CantSupplyData	moveq	#IERR_INP_INCOMP,d0
		rts

.NMDDepackErr	moveq	#IERR_NMD_DEPACK,d0
		rts

;---------------------------------------------------------------------

.ReadNOMADTrk	move.l	xx_BufferWanted(pc),d5	;If we are reading from a MFMWarp file,
		and.l	#BUFFER_MFM,d5		;check that MFM data is allowed. Abort
		beq	.CantSupplyData		;if it isn't!

		move.l	d4,d0
		bsr	FindOffset
		move.l	d0,d2			;d2 = Best offset to go to
		cmp.l	#NO_MFM_OFFSET,d2
		bne	.NOMADFirstSeek
		moveq	#0,d2			;First track header in a MFMWarp file is always at $0

.NOMADFirstSeek	move.l	xx_ReadFromPtr(pc),d1	;Skip past this track
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6

		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr
		move.l	d2,d6			;d6 = Current position

.NOMADReadNext	lea	nmd_Header(pc),a0
		move.l	#$62,d0
		bsr	ReadBytes
		tst.l	d0			;If no bytes were read, it's not there! (end of file)
		beq	.TrkMissingErr
		cmp.l	#$62,d0			;Check we read all $62 header bytes
		bne	.MFMReadErr

		move.l	nmd_Header(pc),d0
		cmp.l	#'Warp',d0
		bne	.MFMBadHeader
		move.l	nmd_WhatMakesMe(pc),d0
		cmp.l	#'What',d0
		bne	.MFMBadHeader

		move.l	nmd_DepackedLen(pc),d0	;Check for a corrupt file (length > $6c00)
		move.l	xx_RawLength(pc),d1
		cmp.l	d0,d1
		ble	.IllegalLenErr

		moveq	#0,d0
		move.w	nmd_Cylinder(pc),d0	;Store this offset
		add.w	d0,d0
		move.l	nmd_TopBottom(pc),d1
		cmp.l	#'BOT'<<8,d1
		bne	.NotBottom
		addq	#1,d0
.NotBottom	move.l	d0,nmd_TrackNum
		move.l	d6,d1
		bsr	StoreOffset

		add.l	nmd_PackedLen(pc),d6
		add.l	#$62,d6
		move.l	d6,mfm_BestOffset

		move.l	nmd_TrackNum(pc),d0	;Check track in buffer with track we want
		cmp.w	d0,d4
		beq	.NOMADRightTrk
		blt	.TrkMissingErr		;We wanted a track which isn't in the MFMWarp file

		move.l	xx_ReadFromPtr(pc),d1	;Skip past this track
		move.l	nmd_PackedLen(pc),d2
		move.l	#OFFSET_CURRENT,d3
		move.l	dosbase(pc),a6
		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr

;		add.l	#$62,d2
;		add.l	d2,d6
		bra	.NOMADReadNext

.NOMADRightTrk	move.l	xx_RawBuffer(pc),a0
		move.l	nmd_PackedLen(pc),d0
		move.l	d0,d2
		bsr	ReadBytes
		cmp.l	d0,d2
		bne	.MFMReadErr

		move.l	xx_RawBuffer(pc),a0
		move.l	xx_MFMBuffer(pc),a1
		lea	nmd_Buffer,a2
		move.l	nmd_PackedLen(pc),d0
		move.l	nmd_DepackedLen(pc),d1
		bsr	NOMAD_UnPack
		cmp.l	d0,d1
		bne	.NMDDepackErr

		move.l	xx_MFMBuffer(pc),a0
		move.l	4(a0),d0
		cmp.l	#'MFML',d0		;Check for MFM track
		beq	.NOMADDepckMFML
		cmp.l	#'MFMS',d0		;Check for single index MFM track
		beq	.NOMADDepckMFMS
		cmp.l	#'DOS.',d0		;Hopefully it's Dos!
		bne	.NMDDepackErr

		move.l	xx_BufferWanted(pc),d5	;We have decoded an AmigaDos track,
		and.l	#BUFFER_ADOS,d5		;so check that Dos data is allowed. 
		beq	.CantSupplyData		;Abort if it isn't!

		move.l	(a0),d0
		lea	8(a0),a0
		move.l	xx_Track(pc),a1
		move.l	xx_CurrentTLen(pc),d7
		move.l	#($1600/4)-1,d7
.CopyDosTrack	move.l	(a0)+,(a1)+
		dbf	d7,.CopyDosTrack

		move.l	#BUFFER_ADOS,xx_BufferType	;Mark the buffer as full of AmigaDos data
		moveq	#IERR_OK,d0
		rts

.NOMADDepckMFMS	move.l	(a0),d0			;length.l MFMS.l MFMData
		asr.l	#2,d0			;Convert length (bytes) to longwords
		subq	#3,d0			;Subtract length + MFMS + 1 for DBF loop
		tst.l	d0
		bmi	.BufferMFMOK
		lea	8(a0),a0
		move.l	a0,a2			;a2 = Data to repeat
		move.l	xx_RawBuffer(pc),a1
		move.l	d0,d7
.CopyMFMS_1	move.l	(a0)+,(a1)+
		dbf	d7,.CopyMFMS_1
		move.l	a2,a0			;Repeat the track data
		move.l	d0,d7
.CopyMFMS_2	move.l	(a0)+,(a1)+
		dbf	d7,.CopyMFMS_2
		bra	.BufferMFMOK

.NOMADDepckMFML	move.l	(a0),d0			;length.l (multiply by 2) MFML.l MFMData
		asr.l	#1,d0			;Convert length (words) to longwords
		subq	#3,d0			;Subtract length + MFML + 1 for DBF loop
		tst.l	d0
		bmi	.BufferMFMOK
		lea	8(a0),a0
		move.l	xx_RawBuffer(pc),a1
		move.l	d0,d7
.CopyMFML	move.l	(a0)+,(a1)+
		dbf	d7,.CopyMFML
				
.BufferMFMOK	move.l	#BUFFER_MFM,xx_BufferType	;Mark the buffer as full of MFM data
		moveq	#IERR_OK,d0
		rts

;---------------------------------------------------------------------

.ReadWWarpTrk	move.l	d4,d0
		bsr	FindOffset
		move.l	d0,d2			;d2 = Best offset to go to
		cmp.l	#NO_MFM_OFFSET,d2
		beq	.MFMSeekErr		;This shouldn't ever happen (we should have stored where the first track starts)

		move.l	xx_ReadFromPtr(pc),d1
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6

		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr
		move.l	d2,d6			;d6 = Current position

.WWReadNext	lea	wwp_TrackHeader(pc),a0
		move.l	#wth_data_v1,d0
		move.l	d0,d5			;d5 = Number of bytes we have read
		bsr	ReadBytes
		tst.l	d0			;If no bytes were read, it's not there! (end of file)
		beq	.TrkMissingErr
		cmp.l	#wth_data_v1,d0		;Check we read all header bytes
		bne	.MFMReadErr

		cmp.l	#TRACKID,wth_id(a0)	;Check for TRCK
		bne	.MFMBadHeader
		
		cmp.w	#1,wth_ver(a0)		;Check we have the MFM header bytes
		beq	.WWHeaderOK

		cmp.w	#2,wth_ver(a0)		;Check for extended v2 header
		bne	.WWPTrackUns

		lea	wth_data_v1(a0),a0
		move.l	#wth_data-wth_data_v1,d0
		add.l	d0,d5			;Add on extra track header bytes
		bsr	ReadBytes
		cmp.l	#wth_data-wth_data_v1,d0
		bne	.MFMReadErr

.WWHeaderOK	lea	wwp_TrackHeader(pc),a2
		move.l	wth_len(a2),d0
		move.l	d0,d1
		lsr.l	#3,d0			;Convert bit length to bytes
		and.l	#7,d1
		beq	.NoExtra
		addq	#1,d0

.NoExtra	move.l	d0,wwp_TrackLength	;Store the track length in bytes

		move.w	wth_num(a2),d0		;Store this offset
		move.l	d6,d1
		bsr	StoreOffset

		move.l	wwp_TrackLength,d0
		add.l	d0,d5
		add.l	d5,d6			;We now know where the next
		moveq	#0,d5			;track should start
		move.l	d6,mfm_BestOffset

		cmp.w	wth_num(a2),d4
		beq	.RightWWarpTrk	

		move.l	xx_ReadFromPtr(pc),d1
		move.l	wwp_TrackLength,d2	;Skip this track
		move.l	#OFFSET_CURRENT,d3
		move.l	dosbase(pc),a6

		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr
		bra	.WWReadNext

.RightWWarpTrk	move.l	xx_MFMBuffer(pc),a0	;Read the WWarp track
		move.l	wwp_TrackLength,d0
		move.l	d0,d2
		bsr	ReadBytes
		cmp.l	d0,d2
		bne	.MFMReadErr

		btst	#TFB_BZIP2,wth_flags+1(a2)	;BZip2 track compression
		bne	.WWPTrackFlag			;is not implemented!

		btst	#TFB_LEQ,wth_flags+1(a2)	;Check if all longs of the trackdata are equal, stored is only one long
		beq	.NotLEQ

		move.l	xx_MFMBuffer(pc),a0	;Unpack the data over itself
		move.l	(a0),d1
		move.l	#($3000/4)-1,d0		;D0 = Track length (longs) $3000 is quite safe
.LEQ		move.l	d1,(a0)+
		dbf	d0,.LEQ
		bra	.NotSLEQ

.NotLEQ		btst	#TFB_SLINC,wth_flags+1(a2)	;each sector (512 byte) contains longs which will incremented, stored are the first long for each sector
		beq	.NotSLINC
		moveq	#1,d7			;Set increment value
		bra	.DepackTrack

.NotSLINC	btst	#TFB_SLEQ,wth_flags+1(a2)	;each sector (512 byte) contains longs which are the same, stored are the first long for each sector
		beq	.NotSLEQ
		moveq	#0,d7			;Reset increment value

.DepackTrack	move.l	#($3000/$200)-1,d0	;$3000 is quite safe
		move.l	d0,d3			;D3 = Number of sectors

		move.l	xx_MFMBuffer(pc),a0	;Unpack the data into the
		lea	nmd_Buffer,a1
.CopySectLongs	move.l	(a0)+,(a1)+
		dbf	d3,.CopySectLongs

		lea	nmd_Buffer,a0		;Unpack data from the NOMAD
		move.l	xx_MFMBuffer(pc),a1	;buffer to the MFM buffer
		move.l	d0,d3
.SL_TrkLoop	move.l	(a0)+,d1
		move.l	#($200/4)-1,d2
.SL_SectLoop	move.l	d1,(a1)+
		add.l	d7,d1
		dbf	d2,.SL_SectLoop
		dbf	d3,.SL_TrkLoop

.NotSLEQ	btst	#TFB_RAWSINGLE,wth_flags+1(a2)
		beq	.NotRawSingle

		move.l	xx_MFMBuffer(pc),a0	;Make a copy of the data
		move.l	wth_len(a2),d0
		bsr	DoubleRaws

;-----( This was the old buggy routine that didn't double the individual bits )-----
;		move.l	xx_MFMBuffer(pc),a0	;Make a copy of the data
;		lea	(a0,d0.l),a1		;after the end of the data
;
;		move.l	d0,d7
;		move.l	xx_RawLength(pc),d2
;		sub.l	d0,d2
;		cmp.l	d7,d2
;		bgt	.LengthOK
;		move.l	d2,d7
;.LengthOK	subq	#1,d7
;.RawSingleLoop	move.b	(a0)+,(a1)+
;		dbf	d7,.RawSingleLoop
;-----( This was the old buggy routine that didn't double the individual bits )-----

.NotRawSingle	cmp.w	#TT_STD,wth_type(a2)	;Check if this track is dos
		beq	.WWarpDosTrack

		cmp.w	#TT_STDF,wth_type(a2)	;Check if this track is dos forced
		beq	.WWarpDosTrack

		lea	WWarpGlobals,GL		;Encoding routines require these variables to be valid
		move.l	xx_RawBuffer(pc),gl_chipbuf(GL)
		move.l	#$4000,gl_writelen(GL)	;I wish I had a drive like this! :)
		move.w	mfm_TrackNum,gl_trk+wth_num(GL)

		moveq	#0,d0			;d0 = Track number
		move.w	xx_ReadFromTrk,d0

		cmp.w	#TT_GREM,wth_type(a2)		;gremlin format ($1800 bytes)
		bne	.CheckRob
		bsr	_encode_grem
		bra	.WWarpDoneMFM

.CheckRob	cmp.w	#TT_ROB,wth_type(a2)		;rob northen format (4+$1800 bytes)
		bne	.CheckPMover
		bsr	_encode_rob
		bra	.WWarpDoneMFM

.CheckPMover	cmp.w	#TT_PMOVER,wth_type(a2)		;Prime Mover format ($18A0 bytes)
		bne	.CheckBeast1
		bsr	_encode_pmover
		bra	.WWarpDoneMFM

.CheckBeast1	cmp.w	#TT_BEAST1,wth_type(a2)		;Beast1 format ($1838 bytes)
		bne	.CheckBeast2
		bsr	_encode_beast1
		bra	.WWarpDoneMFM

.CheckBeast2	cmp.w	#TT_BEAST2,wth_type(a2)		;Beast2 format ($189C bytes)
		bne	.CheckBloodMon
		bsr	_encode_beast2
		bra	.WWarpDoneMFM

.CheckBloodMon	cmp.w	#TT_BLOODMONEY,wth_type(a2)	;Blood Money format ($1838 bytes)
		bne	.CheckPsygnos1
		bsr	_encode_bloodmoney
		bra	.WWarpDoneMFM

.CheckPsygnos1	cmp.w	#TT_PSYGNOSIS1,wth_type(a2)	;psygnosis1 format ($1800 bytes)
		bne	.CheckTurrican1
		bsr	_encode_psygnosis1
		bra	.WWarpDoneMFM

.CheckTurrican1	cmp.w	#TT_TURRICAN1,wth_type(a2)	;Turrican1 format ($1978 bytes)
		bne	.CheckTurrican2
		bsr	_encode_turrican1
		bra	.WWarpDoneMFM

.CheckTurrican2	cmp.w	#TT_TURRICAN2,wth_type(a2)	;Turrican2 format ($1A90 bytes)
		bne	.CheckTurricn3a
		bsr	_encode_turrican2
		bra	.WWarpDoneMFM

.CheckTurricn3a	cmp.w	#TT_TURRICAN3A,wth_type(a2)	;Turrican3 format ($1800 bytes)
		bne	.CheckTurricn3b
		bsr	_encode_turrican3a
		bra	.WWarpDoneMFM

.CheckTurricn3b	cmp.w	#TT_TURRICAN3B,wth_type(a2)	;Turrican3 format ($1A00 bytes)
		bne	.CheckOcean
		bsr	_encode_turrican3b
		bra	.WWarpDoneMFM

.CheckOcean	cmp.w	#TT_OCEAN,wth_type(a2)	;Ocean format ($1800 bytes)
		bne	.CheckVision
		bsr	_encode_ocean
		bra	.WWarpDoneMFM

.CheckVision	cmp.w	#TT_VISION,wth_type(a2)	;Vision format ($1800 bytes)
		bne	.CheckTwilight
		bsr	_encode_vision
		bra	.WWarpDoneMFM

.CheckTwilight	cmp.w	#TT_TWILIGHT,wth_type(a2)	;Twilight format ($1400 bytes)
		bne	.CheckZZKJa
		bsr	_encode_twilight
		bra	.WWarpDoneMFM

.CheckZZKJa	cmp.w	#TT_ZZKJA,wth_type(a2)		;ZZKJ format ($800 bytes)
		bne	.CheckZZKJb
		bsr	_encode_zzkja
		bra	.WWarpDoneMFM

.CheckZZKJb	cmp.w	#TT_ZZKJB,wth_type(a2)		;ZZKJ format ($1000 bytes)
		bne	.CheckZZKJc
		bsr	_encode_zzkjb
		bra	.WWarpDoneMFM

.CheckZZKJc	cmp.w	#TT_ZZKJC,wth_type(a2)		;ZZKJ format ($1600 bytes)
		bne	.CheckZZKJd
		bsr	_encode_zzkjc
		bra	.WWarpDoneMFM

.CheckZZKJd	cmp.w	#TT_ZZKJD,wth_type(a2)		;ZZKJ format ($1600 bytes)
		bne	.CheckSpecialFX
		bsr	_encode_zzkjd
		bra	.WWarpDoneMFM

.CheckSpecialFX	cmp.w	#TT_SPECIALFX,wth_type(a2)	;Special FX format ($1800 bytes)
		bne	.CheckTiertex
		bsr	_encode_specialfx
		bra	.WWarpDoneMFM

.CheckTiertex	cmp.w	#TT_TIERTEX,wth_type(a2)	;Tiertex format ($1800 bytes)
		bne	.CheckElite
		bsr	_encode_tiertex
		bra	.WWarpDoneMFM

.CheckElite	cmp.w	#TT_ELITE,wth_type(a2)	;Elite format ($1800 bytes)
		bne	.CheckGoliath
		bsr	_encode_elite
		bra	.WWarpDoneMFM

.CheckGoliath	cmp.w	#TT_GOLIATH,wth_type(a2)	;Goliath format ($1600 bytes)
		bne	.CheckThalamus
		bsr	_encode_goliath
		bra	.WWarpDoneMFM

.CheckThalamus	cmp.w	#TT_THALAMUS,wth_type(a2)	;Thalamus format ($1810 bytes)
		bne	.CheckRaw
		bsr	_encode_thalamus
		bra	.WWarpDoneMFM

.CheckRaw	cmp.w	#TT_RAW,wth_type(a2)	;If it's not raw, we have an
		bne	.WWPTrackType		;unsupported (ie. packed) track type :(

		bsr	CopyMFMToRawBuf

.WWarpDoneMFM	move.l	xx_BufferWanted(pc),d5	;Check that MFM data is allowed.
		and.l	#BUFFER_MFM,d5		;Abort if it isn't!
		beq	.CantSupplyData

		move.l	#BUFFER_MFM,xx_BufferType	;Mark the buffer as full of MFM data
		bra	.WWarpDone

.WWarpDosTrack	move.l	xx_BufferWanted(pc),d5	;Check that Dos data is allowed.
		and.l	#BUFFER_ADOS,d5		;Abort if it isn't!
		beq	.CantSupplyData

		move.l	xx_MFMBuffer(pc),a0	;Move the data from the buffer
		move.l	xx_Track(pc),a1		;into the Dos track buffer
		move.l	#($1600/4)-1,d7
.CopyLoop	move.l	(a0)+,(a1)+
		dbf	d7,.CopyLoop

.WWarpDoneDos	move.l	#BUFFER_ADOS,xx_BufferType	;Mark the buffer as full of Dos data
.WWarpDone	moveq	#IERR_OK,d0
		rts

.WWPTrackType	moveq	#IERR_TRCK_TYPE,d0
		rts

.WWPTrackUns	moveq	#IERR_TRCK_UNS,d0
		rts

.WWPTrackFlag	moveq	#IERR_TRCK_FLAG,d0
		rts

;---------------------------------------------------------------------

OpenReadFrom	moveq	#IERR_OK,d0
		move.l	xx_ReadFromPtr(pc),d1
		bne	.AlreadyOpen

		bsr	ClearOffsets

		move.l	xx_InputName(pc),a0
;		lea	readFromNameBuffer,a0
		tst.b	(a0)
		beq	.OpenErr		;//REMOVE .ReadFromNotFound
		move.l	a0,d1
		move.l	#MODE_OLDFILE,d2
		move.l	dosbase(pc),a6
		jsr	_LVOOpen(a6)

		tst.l	d0
		beq	.OpenErr

		lea	xx_ReadFromPtr(pc),a0	;Store the file pointer
		move.l	d0,(a0)

		bsr	IdentifyFile
.AlreadyOpen	rts

.OpenErr	moveq	#IERR_INP_OPEN,d0
		rts

;---------------------------------------------------------------------
; ClearOffsets
;
; Clear the offsets table so when we try and locate a track there will
; be no known offsets. This should be called when a new file is 
; selected.

ClearOffsets	movem.l	d0/a0,-(sp)
		lea	mfm_Offsets(pc),a0
		move.l	#MAX_TRK_OFFSETS-1,d0
.ClearLoop	move.l	#NO_MFM_OFFSET,(a0)+
		dbf	d0,.ClearLoop
		lea	mfm_BestOffset(pc),a0
		move.l	#NO_MFM_OFFSET,(a0)
		movem.l	(sp)+,d0/a0
		rts

;---------------------------------------------------------------------
; StoreOffset
; Store the offset (seek position) in a file for a particular track.
; d0 = Track number
; d1 = Offset (bytes)

StoreOffset	movem.l	d0-d1/a0,-(sp)
		and.l	#$ffff,d0
		cmp.l	#MAX_TRK_OFFSETS,d0
		bge	.TooHigh
		lea	mfm_Offsets(pc),a0
		asl.l	#2,d0
		move.l	d1,(a0,d0.l)
.TooHigh
;		movem.l	d0-d7/a0-a6,-(sp)
;		lea	mfm_Offsets(pc),a0
;		lea	$200,a1
;		move.l	#$40-1,d0
;.1		move.l	(a0)+,(a1)+
;		dbf	d0,.1
;		movem.l	(sp)+,d0-d7/a0-a6

		movem.l	(sp)+,d0-d1/a0
		rts

;---------------------------------------------------------------------
; FindOffset
; Look in the list of offsets we know and see if the current track is
; listed. For example if we have read track 140 of a MFMWarp file, we
; should know all tracks up until that point so if we want to read
; track 10 now we can seek directly to it.
; d0 = Track we want to find
;>d0 = Nearest seek position (byte offset) we know is correct

FindOffset	movem.l	d1-d2/a0,-(sp)
		moveq	#0,d2
		move.w	d0,d2
		lea	mfm_Offsets(pc),a0	;Check if we know the offset for this track
		cmp.l	#MAX_TRK_OFFSETS,d2
		bge	.ReturnBest

		move.l	d2,d1			;Check track in the array
		asl.l	#2,d1
		move.l	(a0,d1.l),d0
		tst.l	d0
		bge	.OffsetFound

.ReturnBest	move.l	mfm_BestOffset(pc),d0	;Wanted a really high track 

.OffsetFound	movem.l	(sp)+,d1-d2/a0
		rts

;---------------------------------------------------------------------

IdentifyFile	lea	xx_ReadFromType(pc),a1
		move.w	#TYPE_DISK,(a1)

		move.l	xx_ReadFromPtr(pc),d1	;Go to the start of this file
		moveq	#0,d2
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6
		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr

		lea	mfm_Header(pc),a0	;Read $10 bytes into mfm_Header
		moveq	#$10,d0
		bsr	ReadBytes
		cmp.l	#$10,d0
		bne	.MFMReadErr

		lea	mfm_Header(pc),a0	;Test the archive header
		lea	.IDDMS(pc),a1		;for known formats
		bsr	CheckStrMatch
		beq	.Unsupported

		lea	.IDMFMWarp(pc),a1
		bsr	CheckStrMatch
		beq	.MFMWarpFound

		lea	.IDMOKWarp(pc),a1
		bsr	CheckStrMatch
		beq	.Unsupported

		lea	.IDNOMADWarp(pc),a1
		bsr	CheckStrMatch
		beq	.NOMADWarpFound

		lea	.IDPhilWarp(pc),a1
		bsr	CheckStrMatch
		beq	.Unsupported

		lea	.IDWWarp(pc),a1
		bsr	CheckStrMatch
		beq	.WWarpFound

		move.w	#TYPE_ADF,xx_ReadFromType	;Treat is as an ADF
		moveq	#IERR_OK,d0
		rts

.Unsupported	move.w	#TYPE_DISK,xx_ReadFromType
		moveq	#IERR_FORMAT_UNS,d0
		rts

.MFMWarpFound	move.w	#TYPE_MFMWARP,xx_ReadFromType
		moveq	#IERR_OK,d0
		rts

.NOMADWarpFound	move.w	#TYPE_NOMADWARP,xx_ReadFromType
		moveq	#IERR_OK,d0
		rts

.WWarpFound	cmp.w	#1,wfh_ver(a0)		;Support WWRP v1.0 header only
		bne	.WWPUnsupported

		move.l	xx_ReadFromPtr(pc),d1	;Seek to WWarp v1.0 track header
		moveq	#$48,d2			;at $48
		move.l	#OFFSET_BEGINNING,d3
		move.l	dosbase(pc),a6
		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0
		beq	.MFMSeekErr

		lea	wwp_TableHeader(pc),a0	;Some spare workspace memory
		moveq	#10,d0			;Read 10 bytes of WWRP file
		bsr	ReadBytes
		cmp.l	#10,d0
		bne	.MFMReadErr
		
		cmp.l	#TABLEID,wtt_id(a0)	;We expect 'TABL' to be found
		bne	.WWPTableData
		cmp.w	#1,wtt_ver(a0)		;Support version 1 only
		bne	.WWPTableUns

		moveq	#0,d2
		move.w	wtt_last(a0),d2
		cmp.w	#MAX_TRK_OFFSETS,d2	;WWarp file has a really high track!
		bge	.WWPTableData
		sub.w	wtt_first(a0),d2
		addq	#1,d2			;d2 = Total tracks in the WWarp file (1 + last - first)

		move.l	d2,d1
		move.l	d2,d0
		divu.w	#8,d1
		ext.l	d1
		and.w	#7,d0			;Check if evenly divisible by 8
		beq	.NoExtraByte
		addq	#1,d1			;Not div 8 so we need to read an extra byte

.NoExtraByte	lea	nmd_Buffer,a0		;Read the table bits into the
		move.l	d1,d0			;nmd_Buffer and check we read
		bsr	ReadBytes		;them all
		cmp.l	d1,d0
		bne	.MFMReadErr

		bsr	ClearOffsets		;Clear the offsets so all tracks are unknown

		lea	mfm_BestOffset(pc),a0
		add.l	#$48+10,d1
		move.l	d1,(a0)

		move.w	#TYPE_WWARP,xx_ReadFromType
		moveq	#IERR_OK,d0
		rts

.WWPUnsupported	moveq	#IERR_WWP_UNS,d0
		rts

.WWPTableData	moveq	#IERR_TABL_DATA,d0
		rts

.WWPTableUns	moveq	#IERR_TABL_UNS,d0
		rts

.MFMReadErr	moveq	#IERR_INP_READ,d0
		rts

.MFMSeekErr	moveq	#IERR_INP_SEEK,d0
		rts

.IDDMS		dc.b	"DMS! PRO",0		;DMS! PRO
.IDMFMWarp	dc.b	"FRX!WARP v1.000",0	;FRX!WARP v1.000
.IDMOKWarp	dc.b	"MOK_WARP v"		;MOK_WARP v1.001
.IDNOMADWarp	dc.b	"Warp v1.1",0		;Warp v1.1
.IDPhilWarp	dc.b	"PHILWARP v"		;PHILWARP v2.000
.IDWWarp	dc.b	"WWRP",0		;WWRP
		EVEN

;---------------------------------------------------------------------
; Check that the string in a0 matches a1
; a0 = Source string
; a1 = String to compare
;>d0 = 0 if matches, -1 otherwise

CheckStrMatch	movem.l	d1/a0-a1,-(sp)
		moveq	#0,d0
.CheckNextByte	move.b	(a0)+,d0
		move.b	(a1)+,d1
		cmp.b	d0,d1
		bne	.NoMatch
		tst.b	d0
		bne	.CheckNextByte
.CheckStrDone	movem.l	(sp)+,d1/a0-a1
		tst.l	d0
		rts

.NoMatch	moveq	#-1,d0
		bra	.CheckStrDone

;---------------------------------------------------------------------
; Read bytes from the open file
; a0 = Destination
; d0 = Length
;>d0 = Bytes read (or -1 for an error)

ReadBytes	movem.l	d1-d3/a0-a6,-(sp)
		tst.l	d0
		beq	.NoReadFile
		move.l	xx_ReadFromPtr(pc),d1
		beq	.NoReadFile
		move.l	a0,d2
		move.l	d0,d3
		move.l	dosbase(pc),a6
		jsr	_LVORead(a6)
.NoReadFile	movem.l	(sp)+,d1-d3/a0-a6
		rts

;---------------------------------------------------------------------

CloseReadFrom	movem.l	d0-d1/a0-a6,-(sp)
		move.l	xx_ReadFromPtr(pc),d1
		beq	.Nothing
		move.l	dosbase(pc),a6
		jsr	_LVOClose(a6)
		lea	xx_ReadFromPtr(pc),a0
		move.l	#0,(a0)
.Nothing	movem.l	(sp)+,d0-d1/a0-a6
		rts

;---------------------------------------------------------------------

CopyMFMToRawBuf	movem.l	d0/a0-a1,-(sp)
		move.l	xx_MFMBuffer(pc),a0
		move.l	xx_RawBuffer(pc),a1
		bra	CopyBuf

CopyRawToMFMBuf	movem.l	d0/a0-a1,-(sp)
		move.l	xx_RawBuffer(pc),a0
		move.l	xx_MFMBuffer(pc),a1
CopyBuf		move.l	xx_RawLength(pc),d0
		lsr.l	#4,d0
.l0		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		subq.l	#1,d0
		bne.b	.l0
		movem.l	(sp)+,d0/a0-a1
		rts

;---------------------------------------------------------------------
; IN:  d0 = raw single track length in bits
;      a0 = MFM buffer
; OUT: d0 = double track length in bits

DoubleRaws	movem.l	d1-a1,-(sp)
		move.l	d0,-(sp)
		add.l	d0,(sp)

;		move.l	(gl_fastbuf,GL),a0
		move.l	d0,d5
		and.l	#%111,d5
		moveq	#32,d4
		sub.l	d5,d4

		lsr.l	#3,d0
		lea	(a0,d0.l),a1

		move.l	(a0),d2
		move.l	(a1),d3
		lsr.l	d4,d3
		lsl.l	d4,d3
		lsr.l	d5,d2
		or.l	d3,d2
		move.l	d2,(a1)+

		lsr.l	#2,d0
		subq.l	#1,d0

.loop		move.l	(a0)+,d2
		move.l	(a0),d3
		lsr.l	d5,d3
		lsl.l	d4,d2
		or.l	d3,d2
		move.l	d2,(a1)+
		dbf	d0,.loop

		move.l	(sp)+,d0
		movem.l	(sp)+,d1-a1
		rts

;---------------------------------------------------------------------

RequestAFile	movem.l	d0-d4/d7/a2-a3/a6,-(sp)

		move.l	reqbase(pc),d0
		beq	.norequester
		move.l	d0,a6			;a6 = reqbase

		move.l	#RT_FILEREQ,d0		;mode
		sub.l	a0,a0			;tags
		jsr	(_LVOrtAllocRequestA,a6)
		move.l	d0,d7			;d7 = filereq
		beq	.norequester

		cmp.l	#rtname,xx_InputName
		beq	.alreadyinit

.chkarg		move.l	(xx_InputName),d1
		bne	.fromargs
	;read environment variable
		lea	.varname,a0
		move.l	a0,d1
		move.l	#rtname,d2		;buffer
		move.l	#RTDIRNAMELEN+RTFILENAMELEN,d3	;length
		moveq	#0,d4			;flags
		move.l	dosbase,a6
		jsr	(_LVOGetVar,a6)
		tst.l	d0
		bmi	.alreadyinit
		move.l	#rtname,xx_InputName
		bra	.chkarg
.fromargs
		move.l	dosbase,a6
		jsr	(_LVOFilePart,a6)
		move.l	d0,a0
		lea	(rtfilename),a1
		move.w	#RTFILENAMELEN-2,d1
.cpyf		move.b	(a0)+,(a1)+
		dbeq	d1,.cpyf
		clr.b	(a1)
		move.l	(xx_InputName),a0
		lea	(rtdirname),a1
		sub.l	a0,d0
		subq.l	#1,d0
		bmi	.alreadyinit
		cmp.l	#RTDIRNAMELEN-2,d0
		blo	.cpyd
		move.w	#RTDIRNAMELEN-2,d0
.cpyd		move.b	(a0)+,(a1)+
		dbf	d0,.cpyd
		clr.b	(a1)
.alreadyinit
		clr.l	-(a7)
		pea	(rtdirname)
		pea	RTFI_Dir
		pea	.pattern
		pea	RTFI_MatchPat
		move.l	a7,a0
		move.l	d7,a1
		move.l	reqbase,a6
		jsr	(_LVOrtChangeReqAttrA,a6)
		add.w	#20,a7

		clr.l	-(a7)
		pea	(FREQF_NOBUFFER|FREQF_PATGAD)
		pea	RTFI_Flags
		move.l	a7,a0			;tags
		move.l	d7,a1			;filereq
		lea	rtfilename,a2		;Temporary buffer for filename
		lea	.title,a3
		jsr	(_LVOrtFileRequestA,a6)
		add.w	#12,a7
		tst.l	d0
		beq	.requesterfail

	;copy dirname for next requester
		move.l	d7,a0
		move.l	(rtfi_Dir,a0),a0
		lea	rtdirname,a1
		move.w	#RTDIRNAMELEN-2,d0
.cpydir		move.b	(a0)+,(a1)+
		dbeq	d0,.cpydir
		clr.b	(a1)
		
	;build full name
		move.l	d7,a0
		move.l	(rtfi_Dir,a0),a0
		lea	rtname,a1
		move.w	#RTDIRNAMELEN+RTFILENAMELEN-2,d0
.cpy		move.b	(a0)+,(a1)+
		dbeq	d0,.cpy
		clr.b	(a1)
		lea	(rtname),a0
		move.l	a0,d1
		lea	(rtfilename),a0
		move.l	a0,d2
		move.l	#RTDIRNAMELEN+RTFILENAMELEN,d3
		move.l	dosbase,a6
		jsr	(_LVOAddPart,a6)
	;remember in environment variable
		lea	.varname,a0
		move.l	a0,d1
		move.l	#rtname,d2		;buffer
		moveq	#-1,d3			;length
		move.l	#GVF_GLOBAL_ONLY|GVF_SAVE_VAR,d4	;flags
		jsr	(_LVOSetVar,a6)

		bsr	CloseReadFrom
		move.l	#rtname,xx_InputName
.requesterfail
		move.l	d7,a1
		move.l	reqbase,a6
		jsr	(_LVOrtFreeRequest,a6)
.norequester
		movem.l	(sp)+,_MOVEMREGS
		rts

.title		dc.b	"Select a file",0
.pattern	dc.b	"#?.(adf|mfm|wrp|wwp)",0
.varname	dc.b	"RawDIC.InputFile",0
		EVEN

