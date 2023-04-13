;*---------------------------------------------------------------------------
; Program:	slave.asm
; Contents:
; History:
;		20.08.04 Wepl
;			 debug output also if no disk matches
;		23.01.05 Wepl - disk name creation fixed
;		31.05.05 Wepl - _BitShiftMFM fixed
;		30.01.06 Psygore - _AppendFile creates a new file by calling
;			 _WriteFile if the file does not exist before
;		14.02.06 Psygore - _AppendFile fixed
;			 Wepl - _WriteFile, _AppendFile rewritten for correct
;				error checking
;		30.04.08 Wepl:
;			 lib_Print added
; Copyright:	Public Domain
; Language:	68000 Assembler
; Translator:	Barfly
;---------------------------------------------------------------------------*

rawdic_Library:

	BOPT OD6-			;disable branch optimizing
		bra.w	lib_ReadTrack	; may not be called while tle_Decoder
		bra.w	lib_NextSync
		bra.w	lib_NextMFMword
		bra.w	lib_SaveFile
		bra.w	lib_SaveDiskFile ; may not be called while dsk_InitCode or tle_Decoder
		bra.w	lib_AppendFile
		bra.w	lib_AppendDiskFile
		bra.w	lib_DMFM_STANDARD
		bra.w	lib_Print
		bra.w	lib_Reserved	; future use?
		bra.w	lib_Reserved
		bra.w	lib_Reserved
	BOPT OD6+			;enable branch optimizing

lib_ReadTrack:
		move.l	d0,-(sp)
		move.w	xx_ExternalCall(pc),d0
		cmp.w	#CALL_DECODER,d0
		beq.b	lib_AbsErrorFunc
		move.l	(sp)+,d0
		bsr	_ReadTrack
		bra.b	lib_ErrorHandler
lib_NextSync:
		move.w	xx_CurrentSync(pc),d0
lib_NextMFMword:
		bsr	_SearchMFMword
		move.l	xx_MFMBuffer(pc),a0
		bra.b	lib_ErrorHandler
lib_SaveFile:
		bsr	_WriteFile
		bra.b	lib_ErrorHandler
lib_SaveDiskFile:
		move.l	d0,-(sp)
		move.w	xx_ExternalCall(pc),d0
		cmp.w	#CALL_INIT,d0
		beq.b	lib_AbsErrorFunc
		cmp.w	#CALL_DECODER,d0
		beq.b	lib_AbsErrorFunc
		move.l	(sp)+,d0
		bsr	_SaveDiskFile
		bra.b	lib_ErrorHandler
lib_AppendDiskFile:
		move.l	d0,-(sp)
		move.w	xx_ExternalCall(pc),d0
		cmp.w	#CALL_INIT,d0
		beq.b	lib_AbsErrorFunc
		cmp.w	#CALL_DECODER,d0
		beq.b	lib_AbsErrorFunc
		move.l	(sp)+,d0
		bsr	_AppendDiskFile
		bra.b	lib_ErrorHandler
lib_AppendFile:
		bsr	_AppendFile
		bra.b	lib_ErrorHandler
lib_Reserved:
		rts

lib_DMFM_STANDARD:
		bra	_DecodeDOSTrack

lib_ErrorHandler:	; (no library function, but the library error handling!)
		; on error, return to main loop... (depending on DFLG_ERRORS)

		tst.l	d0
		bpl.b	.s0
		movem.l	d1/a0,-(sp)
		move.l	xx_Disk(pc),a0
		move.w	dsk_Flags(a0),d1
		and.w	#DFLG_ERRORS,d1
		beq.b	lib_AbsError
.no		movem.l	(sp)+,d1/a0
.s0		tst.l	d0
		rts
lib_AbsErrorFunc:
		moveq	#IERR_NOFUNCTION,d0
lib_AbsError:	move.l	xx_Stack(pc),sp	; exit with errormessage
		bra	SM_Error

		; => a0.l=formatstring a1.l=argarray
lib_Print	movem.l	d0-d2/a0-a1/a6,-(a7)
		move.l	dosbase,a6
		move.l	a0,d1
		move.l	a1,d2
		jsr	(_LVOVPrintf,a6)
		movem.l	(a7)+,d0-d2/a0-a1/a6
		rts

_TrackCRC16:	; calculates the CRC16 value of the track in trackbuffer

		; => D0.w=CRC16

		movem.l	d1-d3/a0,-(sp)
		moveq	#-1,d0
		move.l	xx_Track(pc),a0
		move.l	xx_CurrentTLen(pc),d2
		beq.b	.no
.l0		move.b	(a0)+,d1
		eor.b	d1,d0
		moveq	#7,d3
.l1		add.w	d0,d0
		bcc.b	.s0
		eor.w	#$1021,d0	; $1021 = standard crc 16 value
.s0		dbra	d3,.l1
		subq.l	#1,d2
		bne.b	.l0
.no		movem.l	(sp)+,d1-d3/a0
		rts

_ReadTrack:
		; D0.w=tracknumber
		; =>A1=trackbuffer
		; =>D0.l=errorcode

		movem.l	d1-d7/a0/a2-a6,-(sp)
		lea	xx_CurrentTrack(pc),a1
		move.w	d0,(a1)
		bsr.w	_GetTrackInfo
		tst.l	d0
		bmi.b	.notrack
		move.w	xx_DiskFinished(pc),d1
		beq.b	.read
		move.l	xx_CurrentTLen(pc),d1
		move.l	xx_Track(pc),a0
		bsr	_GetDiskData
		bra.b	.end
.read		bsr	_ClearTrackBuffer
		move.l	xx_CurrentDecoder(pc),d0
		cmp.l	#DMFM_NULL,d0
		bne.b	.dec
		moveq	#IERR_OK,d0
		bra.b	.end
.dec		bsr	_ReadTrackCore
.end		movem.l	(sp)+,d1-d7/a0/a2-a6
		move.l	xx_Track(pc),a1
		tst.l	d0
		rts
.notrack	moveq	#IERR_NOTRACK,d0
		bra.b	.end

_GetTrackInfo:	; calculates the offset of a track in the diskimage
		; and configures rawdic's globals with the track config

		; D0.w=track
		; => D0.l=offset (-1 when track not present)

		movem.l	d1-d4/a0-a1,-(sp)

		move.l	xx_Disk(pc),a1
		move.w	dsk_Flags(a1),d4
		and.w	#DFLG_DOUBLEINC2,d4

		moveq	#0,d3
		move.l	xx_Disk(pc),a0
		move.l	dsk_TrackList(a0),a0	; get default pointer to tracklist
		moveq	#TL_END,d1
.l0		cmp.w	tle_FirstTrack(a0),d1	; end of tracklist reached?
		beq	.error
		tst.w	d4
		beq.b	.ss
		move.w	tle_FirstTrack(a0),d2
		add.w	d0,d2
		and.w	#1,d2
		bne.b	.skip
.ss		cmp.w	tle_FirstTrack(a0),d0
		blt.b	.try
		cmp.w	tle_LastTrack(a0),d0
		bgt.b	.try
		bra.b	.get
.try
.skip		move.w	tle_LastTrack(a0),d2
		sub.w	tle_FirstTrack(a0),d2
		bpl.b	.s0
		neg.w	d2
.s0		addq.w	#1,d2
		tst.w	d4
		beq.b	.s1
		lsr.w	#1,d2
.s1		mulu.w	tle_BlockLength(a0),d2
		add.l	d2,d3
		add.w	#tle_SIZEOF,a0
		bra.b	.l0
.get		move.w	d0,d2
		sub.w	tle_FirstTrack(a0),d2
		bpl.b	.s2
		neg.w	d2
.s2		tst.w	d4
		beq.b	.s3
		lsr.w	#1,d2
.s3		mulu.w	tle_BlockLength(a0),d2
		add.l	d2,d3
		lea	xx_CurrentSync(pc),a1
		move.w	tle_Sync(a0),(a1)
		lea	xx_CurrentDecoder(pc),a1
		move.l	tle_Decoder(a0),(a1)
		lea	xx_CurrentTLen(pc),a1
		moveq	#0,d1
		move.w	tle_BlockLength(a0),d1
		move.l	d1,(a1)
		move.l	d3,d0
		movem.l	(sp)+,d1-d4/a0-a1
		tst.l	d0
		rts
.error		movem.l	(sp)+,d1-d4/a0-a1
		moveq	#-1,d0
		rts

_GetDiskData:	; copies data from the disk image to a memory adress

		; A0=target adress
		; D0.l=offset
		; D1.l=length
		; => D0.l=errorcode

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	xx_DiskLen(pc),d2
		cmp.l	d0,d2
		bls.b	.error
		cmp.l	d1,d2
		blo.b	.error
		sub.l	d0,d2
		sub.l	d1,d2
		bmi.b	.error
		move.l	xx_DiskImage(pc),a1
		add.l	d0,a1
		tst.l	d1
		beq.b	.s0
.l0		move.b	(a1)+,(a0)+
		subq.l	#1,d1
		bne.b	.l0
.s0		movem.l	(sp)+,d0-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.error		movem.l	(sp)+,d0-d7/a0-a6
		moveq	#IERR_DISKRANGE,d0
		rts

_TestDisk:	; tests a disks validity

		; A0=Disk
		; => D0.l=errorcode

		movem.l	a0/d1,-(sp)
		move.l	dsk_TrackList(a0),a0
		bsr.b	_TrackListLength
		movem.l	(sp)+,a0/d1
		tst.w	d0
		bne.b	.tracksok
		moveq	#IERR_TRACKLIST,d0
		rts
.tracksok	move.w	dsk_Version(a0),d0
		beq	.dskversion
		cmp.w	#2,d0
		bls.b	.version1
.dskversion	moveq	#IERR_DSKVERSION,d0
		rts
.version1	move.w	#DFLG_VERSION1,d0
		not.w	d0
		and.w	dsk_Flags(a0),d0
		beq.b	.ok
		moveq	#IERR_FLAGS,d0
		rts
.ok		moveq	#IERR_OK,d0
		rts

_TrackListLength:	; calculates the number of tracks in a tracklist
			; and returns the size of the largest block

			; if tracklist is corrupted, number of tracks is 0

		; A0=tracklist
		; => D0.w=# of tracks in tracklist
		; => D1.l=largest block length

		movem.l	d2-d6/a0-a1,-(sp)

		move.l	xx_Disk(pc),a1
		move.w	dsk_Flags(a1),d5
		move.w	d5,d6
		and.w	#DFLG_NORESTRICTIONS,d5
		and.w	#DFLG_DOUBLEINC2,d6

		; tle_FirstTrack[i]<=tle_LastTrack[i]

		moveq	#0,d0			; 0 tracks
		moveq	#0,d1
		moveq	#0,d4

		tst.w	tle_FirstTrack(a0)
		bmi.b	.error			; atleast 1 segment
.l0		tst.w	tle_LastTrack(a0)
		bmi.b	.error
		move.w	tle_LastTrack(a0),d3
		move.w	d3,d2
		sub.w	tle_FirstTrack(a0),d2	; starttrack <= endtrack
		bpl.b	.calc
		tst.w	d5
		beq.b	.error
		neg.w	d2
.calc		tst.w	d6
		beq.b	.nodi
		lsr.w	#1,d2
.nodi		addq.w	#1,d2
		add.w	d2,d0
		move.w	tle_BlockLength(a0),d4
		beq.b	.error
		cmp.l	d4,d1
		bge.b	.s0
		move.l	d4,d1
.s0		add.w	#tle_SIZEOF,a0
		tst.w	tle_FirstTrack(a0)
		bmi.b	.ok
		cmp.w	tle_FirstTrack(a0),d3	; endtrack0 < starttrack1
		blt.b	.l0
		tst.w	d5
		bne.b	.l0

.error		movem.l	(sp)+,d2-d6/a0-a1
		moveq	#0,d0			; 0 tracks
		rts
.ok		movem.l	(sp)+,d2-d6/a0-a1
		tst.w	d0
		rts

_DiskImageLength:	; calculates the length of the diskimage

		; A0=TrackList
		; => D0.l=length

		movem.l	d1-d3/a0-a1,-(sp)
		move.l	xx_Disk(pc),a1
		move.w	dsk_Flags(a1),d2
		move.w	d2,d3
		and.w	#DFLG_NORESTRICTIONS,d2
		and.w	#DFLG_DOUBLEINC2,d3
		moveq	#0,d0
.l0		tst.w	tle_FirstTrack(a0)
		bmi.b	.exit
		tst.w	tle_LastTrack(a0)
		bmi.b	.error
		move.w	tle_LastTrack(a0),d1
		sub.w	tle_FirstTrack(a0),d1
		bpl.b	.calc
		tst.w	d2
		beq.b	.error	; no negative track increment
		neg.w	d1
.calc		tst.w	d3
		beq.b	.nodi
		lsr.w	#1,d1
.nodi		addq.w	#1,d1
		mulu.w	tle_BlockLength(a0),d1
		add.l	d1,d0
		add.w	#tle_SIZEOF,a0
		bra.b	.l0
.exit		movem.l	(sp)+,d1-d3/a0-a1
		tst.l	d0
		rts
.error		movem.l	(sp)+,d1-d3/a0-a1
		moveq	#0,d0	; on error: diskimage size = 0
		rts

_AllocDiskImage:	; allocates the memory for the diskimage

		movem.l	d1-d7/a0-a6,-(sp)
		lea	xx_DiskImage(pc),a0
		clr.l	(a0)
		lea	xx_DiskLen(pc),a0
		clr.l	(a0)
		move.l	xx_Disk(pc),a0
		move.l	dsk_TrackList(a0),a0
		bsr.b	_DiskImageLength
		tst.l	d0
		beq.b	.error
		lea	xx_DiskLen(pc),a0
		move.l	d0,(a0)
		move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
		move.l	4.w,a6
		jsr	_LVOAllocMem(a6)
		lea	xx_DiskImage(pc),a0
		move.l	d0,(a0)
		tst.l	d0
		beq.b	.nomem
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.nomem		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OUTOFMEM,d0
		rts
.error		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_TRACKLIST,d0	; no matter what happened, no
		rts				; memory has been allocated here...

_FreeDiskImage:	; deallocates the memory used for the diskimage

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	xx_DiskImage(pc),d0
		beq.b	.noimage
		move.l	d0,a1
		move.l	xx_DiskLen(pc),d0
		move.l	4.w,a6
		jsr	_LVOFreeMem(a6)
		lea	xx_DiskImage(pc),a0
		clr.l	(a0)
		lea	xx_DiskLen(pc),a0
		clr.l	(a0)
.noimage	movem.l	(sp)+,d0-d7/a0-a6
		rts

_AllocTrackBuffer:	; allocates the memory for the trackbuffer
		; trackbuffer memory size will always allocated with mod 8 !!!
		; (no memory allocated on error!)
		; size always >= $1800

		movem.l	d1-d7/a0-a6,-(sp)
		move.l	xx_Disk(pc),a0
		move.l	dsk_TrackList(a0),a0
		lea	xx_TLPosition(pc),a1
		move.l	a0,(a1)
		bsr	_TrackListLength
		tst.w	d0
		beq.b	.nosize
		lea	xx_NumTracks(pc),a0
		move.w	d0,(a0)
		addq.l	#7,d1
		and.w	#$fff8,d1		; always mod 8 !!!
		cmp.l	#$1800,d1
		bge.b	.yo
		move.l	#$1800,d1		; atleast $1600 size (because DOS decoder may be called with < $1600 buffersize)
.yo		lea	xx_TrackLen(pc),a0
		move.l	d1,(a0)
		lea	xx_Track(pc),a0
		clr.l	(a0)			; security
		move.l	d1,d0
		beq.b	.nosize
		move.l	#MEMF_PUBLIC,d1
		move.l	4.w,a6
		jsr	_LVOAllocMem(a6)
		lea	xx_Track(pc),a0
		move.l	d0,(a0)
		tst.l	d0
		beq.b	.nomem
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.nomem		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OUTOFMEM,d0
		rts
.nosize		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_TRACKLIST,d0
		rts

_FreeTrackBuffer:	; deallocates the memory used for the trackbuffer

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	xx_Track(pc),d0
		beq.b	.nomem
		move.l	d0,a1
		move.l	xx_TrackLen(pc),d0
		move.l	4.w,a6
		jsr	_LVOFreeMem(a6)
		lea	xx_Track(pc),a0
		clr.l	(a0)
		lea	xx_TrackLen(pc),a0
		clr.l	(a0)
.nomem		movem.l	(sp)+,d0-d7/a0-a6
		rts

_ClearTrackBuffer:	; fills the trackbuffer with zeros

		movem.l	d0-d1/a0,-(sp)
		move.l	xx_Track(pc),d0
		beq.b	.no
		move.l	d0,a0
		move.l	xx_TrackLen(pc),d0
		lsr.l	#3,d0			; trackbuffer is always mod 8
		tst.l	d0
		beq.b	.no
		moveq	#0,d1
.l0		move.l	d1,(a0)+
		move.l	d1,(a0)+
		subq.l	#1,d0
		bne.b	.l0
.no		movem.l	(sp)+,d0-d1/a0
		rts

_StoreToDiskImage:	; stores an array of bytes into the diskimage
			; if length exceeds diskimage size, data will be cropped

		; A0=pointer to bytearray
		; D0.l=number of bytes to be stored to the diskimage

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	xx_DiskImage(pc),a1
		add.l	xx_DiskPos(pc),a1
		move.l	xx_DiskLen(pc),d1
		sub.l	xx_DiskPos(pc),d1
		cmp.l	d0,d1
		bge.b	.ok
		move.l	d1,d0
.ok		move.l	a0,d1
		and.w	#1,d1		; if data is not word alligned,
		bne.b	.n0		; copy byte-by-byte.
		move.l	d0,d1
		lsr.l	#4,d1
		and.w	#$0f,d0
		tst.l	d1
		beq.b	.n0
.l0		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		subq.l	#1,d1
		bne.b	.l0
.n0		tst.l	d0
		beq.b	.n1
.l1		move.b	(a0)+,(a1)+
		subq.l	#1,d0
		bne.b	.l1
.n1		sub.l	xx_DiskImage(pc),a1
		lea	xx_DiskPos(pc),a0
		move.l	a1,(a0)
		movem.l	(sp)+,d0-d7/a0-a6
		rts

_StoreTrackToImage:	; stores the actual trackbuffer into the diskimage

		movem.l	a0/d0,-(sp)
		move.l	xx_Track(pc),a0
		move.l	a0,d0
		beq.b	.no
		move.l	xx_CurrentTLen(pc),d0
		bsr.b	_StoreToDiskImage
.no		movem.l	(sp)+,a0/d0
		rts

_SaveDiskImage:	; stores a diskimage from memory to hd

		movem.l	d1-d7/a0-a6,-(sp)
		bsr.b	_GetDiskName
		move.l	xx_DiskImage(pc),a1
		move.l	a1,d1
		beq.b	.error
		move.l	xx_DiskLen(pc),d0
		bsr.b	_WriteFile
.error		movem.l	(sp)+,d1-d7/a0-a6
		tst.l	d0
		rts

_GetDiskName:	lea	_DInum(pc),a0
		move.w	xx_CurrentDisk(pc),d0
		divu	#10,d0
		beq	.1
		add.b	#"0",d0
		move.b	d0,(a0)+
.1		swap	d0
		add.b	#"0",d0
		move.b	d0,(a0)+
		clr.b	(a0)
		lea	_DIname(pc),a0
		rts

_DIname:	dc.b	"Disk."
_DInum:		dc.b	0,0,0
		cnop	0,2

_IoErr		jsr	(_LVOIoErr,a6)
		move.w	d0,(xx_LastIoErr)
		rts


_WriteFile:	; A0=filename
		; A1=memory adress
		; D0.l=length
		; => D0.l=errorcode

		movem.l	d1-d5/a0-a1/a6,-(a7)
		move.l	a1,d4			;D4 = buffer
		move.l	d0,d3			;D3 = length
		move.l	(dosbase,pc),a6		;A6 = dosbase
		move.l	a0,d1			;name
		move.l	#MODE_NEWFILE,d2	;mode
		jsr	(_LVOOpen,a6)
		move.l	d0,d5			;D5 = fh
		beq	.openerr
		move.l	d5,d1			;fh
		move.l	d4,d2			;buffer
		jsr	(_LVOWrite,a6)
		cmp.l	d0,d3
		bne	.writeerr
		move.l	d5,d1			;fh
		jsr	(_LVOClose,a6)
		movem.l	(sp)+,_MOVEMREGS
		moveq	#IERR_OK,d0
		rts

.writeerr	bsr	_IoErr
		move.l	d5,d1			;fh
		jsr	(_LVOClose,a6)
.err		movem.l	(sp)+,_MOVEMREGS
		moveq	#IERR_NOWFILE,d0
		rts

.openerr	bsr	_IoErr
		bra	.err


_AppendFile:	; A0=filename
		; A1=memory adress
		; D0.l=length
		; => D0.l=errorcode

		movem.l	d1-d6/a0-a1/a6,-(a7)
		move.l	a1,d4			;D4 = buffer
		move.l	d0,d6			;D6 = length
		move.l	(dosbase,pc),a6		;A6 = dosbase
		move.l	a0,d1			;name
		move.l	#MODE_READWRITE,d2	;mode
		jsr	(_LVOOpen,a6)
		move.l	d0,d5			;D5 = fh
		beq	.openerr
		move.l	d5,d1			;fh
		moveq	#0,d2			;position
		moveq	#OFFSET_END,d3		;mode
		jsr	_LVOSeek(a6)
		cmp.l	#-1,d0			;buggy on v36/37
		beq	.seekerr
		move.l	d5,d1			;fh
		move.l	d4,d2			;buffer
		move.l	d6,d3			;length
		jsr	(_LVOWrite,a6)
		cmp.l	d0,d3
		bne	.writeerr
		move.l	d5,d1			;fh
		jsr	(_LVOClose,a6)
		movem.l	(sp)+,_MOVEMREGS
		moveq	#IERR_OK,d0
		rts

.writeerr	bsr	_IoErr
		move.l	d5,d1			;fh
		jsr	(_LVOClose,a6)
.err		movem.l	(sp)+,_MOVEMREGS
		moveq	#IERR_NOWFILE,d0
		rts

.seekerr
.openerr	bsr	_IoErr
		bra	.err


_CheckFileFitDisk:	; tests parameters if they stay inside the diskimage

		; A1=Offset
		; D1.l=Length

		move.l	d1,-(sp)
		move.l	a1,d0
		bmi.b	.error
		cmp.l	xx_DiskLen(pc),d0
		bge.b	.error
		tst.l	d1
		bmi.b	.error
		add.l	d0,d1
		cmp.l	xx_DiskLen(pc),d1
		bgt.b	.error
		move.l	(sp)+,d1
		moveq	#IERR_OK,d0
		rts
.error		move.l	(sp)+,d1
		moveq	#IERR_DISKRANGE,d0
		rts

_SaveDiskFile:
		; A0=Filename
		; D0.l=Offset
		; D1.l=Length

		movem.l	d1/a0-a1,-(sp)
		move.l	d0,a1
		bsr.b	_CheckFileFitDisk
		tst.l	d0
		bmi.b	.end
		add.l	xx_DiskImage(pc),a1
		move.l	d1,d0
		bsr	_WriteFile
.end		movem.l	(sp)+,d1/a0-a1
		tst.l	d0
		rts

_AppendDiskFile:
		; A0=Filename
		; D0.l=Offset
		; D1.l=Length

		movem.l	d1/a0-a1,-(sp)
		move.l	d0,a1
		bsr.b	_CheckFileFitDisk
		tst.l	d0
		bmi.b	.end
		add.l	xx_DiskImage(pc),a1
		move.l	d1,d0
		bsr	_AppendFile
.end		movem.l	(sp)+,d1/a0-a1
		tst.l	d0
		rts


_SaveFileListEntries:	; save all files named in the filelist

		; A0=FileList

		movem.l	d1/a0-a2,-(sp)
		move.l	a0,a2
.l0		move.l	fle_Name(a2),a0
		cmp.l	#FL_END,a0
		beq.b	.end
		cmp.l	#FL_DISKNAME,a0
		bne.b	.std
		bsr	_GetDiskName
.std		move.l	fle_Offset(a2),d0
		move.l	fle_Length(a2),d1
		cmp.l	#FL_DISKLENGTH,d1
		bne.b	.no
		move.l	xx_DiskLen(pc),d1
.no		bsr.b	_SaveDiskFile
		tst.l	d0
		bmi.b	.end
		add.w	#fle_SIZEOF,a2
		bra.b	.l0
.end		movem.l	(sp)+,d1/a0-a2
		rts


_SaveFiles:	; save files, either diskimage or files

		move.l	a0,-(sp)
		move.l	xx_Disk(pc),a0
		move.l	dsk_FileList(a0),a0
		cmp.l	#FL_DISKIMAGE,a0
		bne.b	.s0
		lea	def_FL_DISKIMAGE(pc),a0
		bra.b	.go
.s0		cmp.l	#FL_NULL,a0
		bne.b	.go
		lea	def_FL_NULL(pc),a0
.go		bsr.b	_SaveFileListEntries
		move.l	(sp)+,a0
		rts

def_FL_DISKIMAGE:	dc.l	FL_DISKNAME,0,FL_DISKLENGTH
def_FL_NULL:		dc.l	FL_END

_SearchMFMword_plain:	; searches for a 16-bit pattern in the MFM bitstream
		; when found, all 16-bit patterns which match are skipped
		; until a not-matching pattern is found

		; D0.w=MFM word
		; => D0.l=errorcode

		movem.l	d1-d3/a0-a1,-(sp)

		move.l	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),a1
		add.l	xx_RawLength(pc),a1
		subq.l	#4,a1
		move.l	xx_RawBit(pc),d2

.word		move.l	(a0),d1
		swap	d1
.bit		move.l	d1,d3
		rol.l	d2,d3
		cmp.w	d0,d3
		beq.b	.found
		addq.w	#1,d2
		and.w	#$000f,d2
		bne.b	.bit
		addq.l	#2,a0
		cmp.l	a1,a0
		bls.b	.word
.error
		movem.l	(sp)+,d1-d3/a0-a1
		moveq	#IERR_NOSYNC,d0
		rts
.found
.word2		move.l	(a0),d3
		swap	d3
		move.l	d3,d3
		rol.l	d2,d3
		cmp.w	d0,d3
		bne.b	.found2
		addq.l	#2,a0
		cmp.l	a1,a0
		bls.b	.word2
		bra.b	.error
.found2
		lea	xx_RawPointer(pc),a1
		move.l	a0,(a1)
		lea	xx_RawBit(pc),a1
		move.l	d2,(a1)

		movem.l	(sp)+,d1-d3/a0-a1
		moveq	#IERR_OK,d0
		rts

_AddSyncToBuffer:	; adds 4 syncwords infront of the MFM buffer

		movem.l	d0/a0,-(sp)
		move.l	xx_MFMBuffer(pc),a0
		move.w	xx_CurrentSync(pc),d0
		bne.b	.ok
		move.w	#SYNC_STD,d0
.ok		move.w	d0,-(a0)
		move.w	d0,-(a0)
		move.w	d0,-(a0)
		move.w	d0,-(a0)
		movem.l	(sp)+,d0/a0
		rts

_SearchMFMword:	; searches the end of a sync signal and updates the MFM buffer

		; D0.w=MFM word
		; => D0.l=errorcode

		bsr.b	_AddSyncToBuffer
		bsr	_SearchMFMword_plain
		tst.l	d0
		bne.b	.error
		move.l	d0,-(sp)
		move.l	xx_RawLength(pc),d0
		bsr.b	_BitShiftMFM
		move.l	(sp)+,d0
.error		rts

_SearchMFMword_fast:	; searches the end of a sync signal and updates
		; the first $0440 bytes of the MFM buffer

		; D0.w=MFM word
		; => D0.l=errorcode

		bsr.b	_AddSyncToBuffer
		bsr	_SearchMFMword_plain
		tst.l	d0
		bne.b	.error
		move.l	d0,-(sp)
		move.l	#$0440,d0
		bsr.b	_BitShiftMFM
		move.l	(sp)+,d0
.error		rts

_BitShiftMFM:	; updates the MFM buffer with the actual bit offset

		; D0.l=target buffer size

		movem.l	d1-d2/a0-a3,-(sp)

		move.l	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),a1
		add.l	xx_RawLength(pc),a1
	;	subq.l	#4,a1			;Wepl: confilcts with reading wwarp files and exact mfm data length
		move.l	xx_MFMBuffer(pc),a2
		move.l	a2,a3
		add.l	d0,a3
		move.l	xx_RawBit(pc),d2

.l0		cmp.l	a2,a3
		bls.b	.end
		move.l	(a0),d1
		lsl.l	d2,d1
		swap	d1
		move.w	d1,(a2)+
		addq.l	#2,a0
		cmp.l	a1,a0
		bls.b	.l0
.l1		cmp.l	a2,a3
		bls.b	.end
		clr.w	(a2)+
		bra.b	.l1
.end
		movem.l	(sp)+,d1-d2/a0-a3
		rts

_DecodeDOSblock:	; decodes a block of a standard DOS track

		movem.l	d1-d5/a0-a2,-(sp)

		move.l	#$55555555,d2		; MFM mask

		moveq	#0,d5
		moveq	#9,d3
.l8		move.l	(a0)+,d0		; calculate header checksum
		eor.l	d0,d5
		dbra	d3,.l8
		and.l	d2,d5
		sub.w	#40,a0

		bsr.b	getMFMlong		; get track/sector info
		move.l	d0,d1
		lsr.w	#8,d1
		cmp.w	_DOSsectors(pc),d1	; max # sectors
		bhs.b	.xxx
		lea	_DOSFlags(pc),a2
		st	(a2,d1.w)

		and.l	#$ff00,d0
		add.l	d0,d0
		add.l	d0,a1			; sector offset

		add.w	#$20,a0
		bsr.b	getMFMlong		; get header checksum
		cmp.l	d0,d5			; ok?
		bne.b	.csum

		bsr.b	getMFMlong		; get data checksum
		move.l	d0,d4

		moveq	#0,d5
		moveq	#$7f,d3
.l0		move.l	(a0)+,d0
		move.l	$1fc(a0),d1
		and.l	d2,d0
		and.l	d2,d1
		eor.l	d0,d5
		eor.l	d1,d5
		add.l	d0,d0
		add.l	d1,d0
		move.l	d0,(a1)+
		dbra	d3,.l0

		cmp.l	d4,d5			; checksum ok?
		bne.b	.csum
.ok
		movem.l	(sp)+,d1-d5/a0-a2
		moveq	#IERR_OK,d0
		rts
.csum		movem.l	(sp)+,d1-d5/a0-a2
		moveq	#IERR_CHECKSUM,d0
		rts
.xxx		movem.l	(sp)+,d1-d5/a0-a2
		moveq	#IERR_UNDEFINED,d0
		rts

getMFMlong:
		move.l	(a0)+,d0
		move.l	(a0)+,d1
		and.l	d2,d0
		and.l	d2,d1
		add.l	d0,d0
		add.l	d1,d0
		rts

_DecodeDOSTrack:	; decodes a standard dos track

		; D0.b=# of sectors

		movem.l	d1-d7/a0-a6,-(sp)

		and.w	#$00ff,d0
		beq.b	.s1		; 0 sectors... skip
		cmp.w	#32,d0
		bls.b	.s0		; max 32 sectors
		moveq	#32,d0
.s0
		lea	_DOSsectors(pc),a2
		move.w	d0,(a2)

		lea	_DOSFlags(pc),a2
		move.w	_DOSsectors(pc),d1
		subq.w	#1,d1
.l0		sf	(a2)+
		dbra	d1,.l0

		lea	_DOSerror(pc),a2
		clr.l	(a2)

		move.w	_DOSsectors(pc),d1
		subq.w	#1,d1
.l1		move.w	xx_CurrentSync(pc),d0
		bne.b	.ok
		move.w	#SYNC_STD,d0	; default: standard sync word
.ok		bsr	_SearchMFMword_fast
		tst.l	d0
		bne.b	.error		; no sync
		bsr	_DecodeDOSblock
		cmp.b	#IERR_UNDEFINED,d0	; sector number too high?
		beq.b	.l1
		tst.l	d0
		beq.b	.noerr
		cmp.b	#IERR_CHECKSUM,d0
		bne.b	.error
		lea	_DOSerror(pc),a2
		move.l	d0,(a2)
.noerr		dbra	d1,.l1

		lea	_DOSFlags(pc),a2
		move.w	_DOSsectors(pc),d1
		subq.w	#1,d1
.l2		tst.b	(a2)+
		dbeq	d1,.l2
		beq.b	.nosect

		move.l	_DOSerror(pc),d0
		bne.b	.error

.s1		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.nosect		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_NOSECTOR,d0
		rts
.error		movem.l	(sp)+,d1-d7/a0-a6
		tst.l	d0
		rts
_DOSFlags:	ds.b	32
_DOSsectors:	dc.w	11
_DOSerror:	dc.l	0

_TestCRC:	; compares the CRC values of the CRClist with the tracks.

		movem.l	d1-d7/a0-a6,-(sp)

		bsr	_AllocTrackBuffer
		tst.l	d0
		bne.b	.errormem

		move.l	xx_Disk(pc),a0
		move.l	dsk_CRCList(a0),d0
		beq.b	.nocrc
		move.l	d0,a0

.do		move.w	crc_Track(a0),d0
		cmp.w	#CRC_END,d0
		beq.b	.nocrc
		bsr	_ReadTrack
		tst.l	d0
		bne.b	.error
		bsr	_TrackCRC16
		bsr	OutputDebug
		cmp.w	crc_Checksum(a0),d0
		bne.b	.csum
		addq.l	#crc_SIZEOF,a0
		bra.b	.do

.nocrc		bsr	_FreeTrackBuffer
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.error		bsr	_FreeTrackBuffer
.errormem	movem.l	(sp)+,d1-d7/a0-a6
		tst.l	d0
		rts
.csum		bsr	_FreeTrackBuffer
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_CRCFAIL,d0
		rts
