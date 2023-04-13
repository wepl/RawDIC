;*---------------------------------------------------------------------------
;  :Program.	RawDIC.asm
;  :Contents.	create diskimages using parameter file
;  :Author.	Graham, Codetapper, Wepl, Psygore
;  :Version	$Id: RawDIC.asm 1.23 2022/06/07 00:24:49 wepl Exp wepl $
;  :History.	xx.xx.xx initial work upto v1.7 done by Graham
;		xx.xx.xx enhancements for reading from file done by Codetapper
;		16.07.04 cleanup, repacking (Wepl)
;		20.08.04 Wepl
;			 debug output also if no disk matches
;		17.01.05 Wepl
;			 variable disk names added
;			 cleanup for txt1
;		23.01.05 Wepl - revision bumped
;               24.04.05 JOTD
;                        no more drive click when reading from file
;                        cleanup, better error handling
;                        no retries if file is selected
;                        "Select File" button sensitivity management added
;		25.05.05 Wepl
;			 version bumped to 4.1
;		30.01.06 Wepl
;			 version bumped to 4.2
;		14.02.06 Wepl
;			 version bumped to 4.3
;		03.09.06 Psygore
;			 track buffer (io_Data) is located in fastmem
;			 (needs at least Kickstart V36), readfromfile will use
;			 fastmem too for track buffer instead of chipmem
;		07.09.06 Psygore
;			 io_Data located in chipmem (TD_RAWREAD)
;		02.11.06 Wepl
;			 version bumped to 4.4
;		23.12.07 Wepl
;			 version bumped to 4.5
;		29.04.08 Wepl
;			 bufferlength increased to $7c04 to read wwarps with $7c00
;		18.09.13 Wepl
;			 version bumped to 5.0
;		21.10.17 Wepl
;			 slave version matches rawdic version
;			 fixed display of 'Select File' button without trackwarp.library
;		05.11.17 Wepl
;			 xx_DiskFinished cleared before TestCRC to fix 'file exceeds
;			 diskimage' when crc is used on disk 2 or later
;			 provide pointer to loaded diskimage on DiskImageCode
;		08.11.17 loop on errors instead quit to allow retry of current disk,
;			 this fixes issue #2924, before it started completely new
;			 after a no disk error
;			 some code cleanup
;		28.05.22 debug output when switching struct because crc-list
;		06.06.22 fix reverse tracklists
;			 don't retry if reading from warp-file
;  :Requires.	OS V37+, MC68000+
;  :Copyright.	?
;  :Language.	68000 Assembler
;  :Translator.	Barfly V2.9
;  :To Do.
;---------------------------------------------------------------------------*

Version		= 6
Revision	= 0

	; the IMSG tags are used to define certain signals in the program
	; i.e. a pressed button or a failure while reading a track

 ; IMSG return codes are the codes used for the basic state machine of RawDIC.
 ; IERR return codes are error codes

	; return values: (may be errors or messages)

IMSG_NULL		equ	0	; NULL message, nothing happened
IMSG_CLOSEWINDOW	equ	1	; closewindow was pressed
IMSG_Start		equ	2	; start button was pressed
IMSG_Stop		equ	3	; stop button was pressed
IMSG_DiskFinished	equ	4	; disk reading is finished
IMSG_Retry		equ	5	; Retry pressed in error requester
IMSG_SelectFile		equ	6	; Select file button was pressed

	; xx_ExternalCall is set to one of these depending on what
	; current state the program is in:

CALL_NONE	equ	0	; RawDIC is running
CALL_INIT	equ	1	; dsk_InitCode is running
CALL_DECODER	equ	2	; tle_Decoder is running
CALL_DISK	equ	3	; dsk_DiskCode is running

MainWinXSize	equ	400

		INCDIR	Include:
		INCLUDE	dos/dos.i
		INCLUDE	dos/dosextens.i
		INCLUDE	dos/var.i
		INCLUDE	devices/trackdisk.i
		INCLUDE	exec/exec.i
		INCLUDE	graphics/gfx.i
		INCLUDE	graphics/gfxbase.i
		INCLUDE	intuition/intuition.i
		INCLUDE	lvo/asl.i
		INCLUDE	lvo/dos.i
		INCLUDE	lvo/exec.i
		INCLUDE	lvo/graphics.i
		INCLUDE	lvo/intuition.i

		INCLUDE	libraries/asl.i
		INCLUDE	rawdic.i
		INCLUDE	libraries/trackwarp.i
		INCLUDE	libraries/trackwarp_lib.i

	OUTPUT	C:RawDIC
	BOPT	O+		;enable optimizing
	BOPT	OG+		;enable optimizing
	BOPT	ODd-		;disable mul optimizing
	BOPT	ODe-		;disable mul optimizing
	BOPT	wo-		;no optimize warnings
	BOPT	sa+		;write symbol hunks

	IFND	.passchk
	DOSCMD	"WBuild >T:build"
	DOSCMD	"WDate  >T:date"
.passchk
	ENDC
		include	systeminit.asm

DFLG_DOUBLEINC2	equ	DFLG_DOUBLEINC&(~DFLG_NORESTRICTIONS)

		dc.b	"$VER: "
		sprintx	"RawDIC %ld.%ld [build ",Version,Revision
		INCBIN	"T:build"
		dc.b	"] "
		INCBIN	"T:date"
		dc.b	0
		dc.b	"$Id: RawDIC.asm 1.23 2022/06/07 00:24:49 wepl Exp wepl $",0
	EVEN

main:
		movem.l	d0-d7/a0-a6,-(sp)
		bsr.b	SM_Init
		movem.l	(sp)+,d0-d7/a0-a6
		rts	; exit RawDIC

	; /-------------------------------\
	; |###############################|
	; |##				##|
	; |## The RawDIC State Machine: ##|
	; |##				##|
	; |###############################|
	; \-------------------------------/

SM_Init:
		lea	xx_ExternalCall(pc),a0
		move.w	#CALL_NONE,(a0)		; rawdic is running

		move.l	xx_SlvStruct(pc),a0
		move.b	slv_Version(a0),d0
		cmp.b	#Version,d0		; slave version not greater than rawdic
		bls.b	.vok
		moveq	#IERR_VERSION,d0
		bra	SM_Error
.vok
		moveq	#SFLG_VERSION1,d0
		not.b	d0
		and.b	slv_Flags(a0),d0
		beq.b	.fok
		moveq	#IERR_FLAGS,d0
		bra	SM_Error
.fok
		btst	#0,(slv_Flags,a0)
		sne	xx_Debug

		lea	xx_Stack(pc),a0
		move.l	sp,(a0)			; save stack to exit

		move.l	PrBar0(pc),a0
		move.w	#1,prb_MaxFill(a0)
		moveq	#0,d0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		move.l	PrBar1(pc),a0
		move.w	#1,prb_MaxFill(a0)
		moveq	#0,d0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar	; both bars at 0%

		lea	xx_CurrentDisk(pc),a0
		move.w	#1,(a0)			; always start with disk 1

		lea	xx_Disk(pc),a1
		move.l	xx_SlvStruct(pc),a0
		move.l	slv_FirstDisk(a0),(a1)
SM_NextDisk:
		lea	xx_Cancel(pc),a0
		clr.w	(a0)

		move.w	xx_CurrentDisk(pc),d1
		move.l	xx_Disk(pc),d0
		beq	SM_Finished

		move.l	ButtonStart(pc),a0
		bsr	OnButton
		bsr	RefreshGadgets			; activate start button

		move.l	aslbase(pc),d0			;If asl and twarplib found
		beq	.noselect			;enable the Select button
		move.l	ButtonSelectFile(pc),a0
		bsr	OnButton
		bsr	RefreshGadgets			; activate select file button
.noselect
		move.l	(xx_Disk),a0
		cmp.w	#2,(dsk_Version,a0)
		blo	.dsknum
		tst.l	(dsk_DiskName,a0)
		beq	.dsknum
		lea	(dsk_DiskName,a0),a1
		lea	(Txt1_InsDskTxt),a0
		bsr	DisplayText
		bra	.l1
.dsknum		lea	(Txt1_InsDskNum),a0
		lea	(xx_CurrentDisk),a1
		bsr	DisplayText

.l1		bsr	WaitIMSG
		cmp.b	#IMSG_CLOSEWINDOW,d0
		beq.b	SM_Exit
		cmp.b	#IMSG_Start,d0
		beq	SM_Start
		cmp.b	#IMSG_SelectFile,d0
		bne	.l1
		bsr	RequestAFile
		bra	.l1

SM_Exit:	; State Machine is in endstate

		rts

; arg1: error code suffix
; arg2: message argument

HANDLE_ERROR:MACRO
	IFGT	NARG-1
	lea	\2,a1
	ENDC
	cmp.b	#IERR_\1,d0
	bne.b	.n\@
	lea	Txt1_\1(pc),a0
	bra	.display
.n\@
	ENDM

SM_Error:	; d0=Error occured, print errormessage and wait for CLOSEWINDOW, period.

		bsr	DriveMotorOff	; in case that custom error occured
					; while reading a disk.

		move.l	ButtonStart(pc),a0
		bsr	OffButton
		bsr	RefreshGadgets	; deactivate start button
		move.l	ButtonStop(pc),a0
		bsr	OffButton
		bsr	RefreshGadgets	; deactivate stop button
		move.l	ButtonSelectFile(pc),a0
		bsr	OffButton
		bsr	RefreshGadgets	; deactivate select file button

		move.w	d0,xx_LastError	; store error code

		HANDLE_ERROR	CHECKSUM,xx_CurrentTrack
		HANDLE_ERROR	NOSYNC,xx_CurrentTrack
		HANDLE_ERROR	NOSECTOR,xx_CurrentTrack
		HANDLE_ERROR	NOWFILE,xx_LastIoErr
		HANDLE_ERROR	OUTOFMEM
		HANDLE_ERROR	DISKRANGE
		HANDLE_ERROR	NOTRACK,xx_CurrentTrack
		HANDLE_ERROR	NODISK
		HANDLE_ERROR	CRCFAIL
		HANDLE_ERROR	TRACKLIST
		HANDLE_ERROR	VERSION
		HANDLE_ERROR	FLAGS
		HANDLE_ERROR	DSKVERSION
		HANDLE_ERROR	NOFUNCTION
		HANDLE_ERROR	SLAVEVER
		HANDLE_ERROR	TWLIB,xx_CurrentTrack

		lea	xx_LastError,a1
		lea	Txt1_UEC(pc),a0		; unknown/forgotten error code
.display	bsr	DisplayText
		move.l	dosbase(pc),a6
		moveq	#100,d1			;wait 2 seconds
		jsr	_LVODelay(a6)
		bra	SM_NextDisk		;allow retry of current disk

SM_Finished:	; All disks finished, wait 1 second and exit

		lea	Txt1_F(pc),a0
		bsr	DisplayText
		move.l	dosbase(pc),a6
		moveq	#50,d1
		jsr	_LVODelay(a6)
		bra	SM_Exit

SM_Stop:	; Stop button pressed, wait 2 seconds and restart

		lea	Txt1_C(pc),a0
		bsr	DisplayText
		move.l	dosbase(pc),a6
		moveq	#100,d1
		jsr	_LVODelay(a6)
		bra	SM_Init

SM_Start:	; Start button pressed, start reading disk x

		move.l	TextDisplay1(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshTextDisplay	; clear text area

		lea	xx_DiskFinished(pc),a0
		clr.w	(a0)			; diskimage not finished

.redo		bsr	_TestCRC
		cmp.b	#IERR_OK,d0
		beq.b	.doit
		cmp.b	#IERR_CRCFAIL,d0
		bne	SM_Error
		move.l	xx_Disk(pc),a0
		move.l	dsk_AltDisk(a0),d1
		beq	SM_Error
		lea	xx_Disk(pc),a0
		move.l	d1,(a0)
		lea	.txt_newstruct,a0
		bsr	Debug
		bra.b	.redo
.doit
		move.w	xx_CurrentDisk(pc),d1
		move.l	xx_Disk(pc),a0
		moveq	#127,d2
.l0		move.l	(a0),d0		; count disks left to read
		beq.b	.s0
		addq.w	#1,d1
		move.l	d0,a0
		dbra	d2,.l0		; avoid endless loop
.s0
		move.w	d1,xx_NumDisks
		move.l	PrBar1(pc),a0
		move.w	d1,prb_MaxFill(a0)

		move.l	xx_Disk(pc),a0
		bsr	_TestDisk
		tst.l	d0
		bmi	SM_Error

		bsr	ResetDrive
		tst.l	d0
		bmi	SM_Error

		move.l	ButtonStop(pc),a0
		bsr	OnButton
		bsr	RefreshGadgets	; activate stop button

		bsr	_AllocTrackBuffer	; ... trackbuffer
		tst.l	d0
		bmi.b	.notb
		bsr	_CallInitCode
		tst.l	d0
		bmi.b	.noic
		bsr	_FreeTrackBuffer
		move.l	xx_Disk(pc),a0
		bsr	_TestDisk
		tst.l	d0
		bmi.b	.notb2
		bsr	_AllocTrackBuffer	; ... trackbuffer
		tst.l	d0
		bmi.b	.notb2
		bsr	_AllocDiskImage	; try to allocate memory for diskimage
		tst.l	d0
		bmi.b	.notl
		bsr.b	ReadDisk
		cmp.b	#IMSG_DiskFinished,d0
		bne.b	.nodc
		bsr	Txt1_CLR
		bsr	_CallDiskCode
		tst.l	d0
		bmi.b	.nodc
		bsr	_SaveFiles
		tst.l	d0
		bmi.b	.nodc
		moveq	#IMSG_DiskFinished,d0
.nodc
		bsr	_FreeDiskImage
.notl
.noic		bsr	_FreeTrackBuffer
.notb2		bsr	DriveMotorOff
.notb
		move.l	ButtonStop(pc),a0
		bsr	OffButton
		bsr	RefreshGadgets	; deactivate stop button

		cmp.b	#IMSG_Stop,d0
		beq	SM_Stop
		cmp.b	#IMSG_CLOSEWINDOW,d0
		beq	SM_Exit
		tst.l	d0
		bmi	SM_Error

		lea	xx_Disk(pc),a0
		move.l	(a0),a1
		move.l	dsk_NextDisk(a1),(a0)

		lea	xx_CurrentDisk(pc),a0
		move.w	(a0),d0
		addq.w	#1,(a0)

		move.l	PrBar1(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		bra	SM_NextDisk

.txt_newstruct	db	"trying alternate struct",10,0
	EVEN

	; #################################

ReadDisk:
		movem.l	d1-d7/a0-a6,-(sp)

		lea	xx_DiskFinished(pc),a0
		clr.w	(a0)			; diskimage not finished

		lea	xx_DiskPos(pc),a0
		clr.l	(a0)			; start at offset 0

		bsr	DriveMotorOn

		move.l	PrBar0(pc),a0
		move.w	xx_NumTracks(pc),prb_MaxFill(a0)

		lea	xx_TrackCount(pc),a0
		clr.w	(a0)			; progress bar at 0%
.next
		lea	xx_TLPosition(pc),a2
		move.l	(a2),a0
		move.w	tle_FirstTrack(a0),d0
		bmi	.out
		move.w	tle_LastTrack(a0),d1
		moveq	#1,d2
		cmp.w	d0,d1
		bge.b	.s0
		neg.l	d2
.s0		lea	xx_CurrentTrack(pc),a1
		move.w	d0,(a1)
		lea	xx_LastTrack(pc),a1
		move.w	d1,(a1)
		move.l	xx_Disk(pc),a1
		move.w	dsk_Flags(a1),d0
		and.w	#DFLG_DOUBLEINC2,d0
		beq.b	.s1
		add.l	d2,d2
.s1		lea	xx_TrackInc(pc),a1
		move.w	d2,(a1)
		lea	xx_CurrentTLen(pc),a1
		moveq	#0,d0
		move.w	tle_BlockLength(a0),d0
		move.l	d0,(a1)
		lea	xx_CurrentSync(pc),a1
		move.w	tle_Sync(a0),(a1)
		lea	xx_CurrentDecoder(pc),a1
		move.l	tle_Decoder(a0),(a1)
		add.w	#tle_SIZEOF,a0
		move.l	a0,(a2)
.l1
		bsr.b	SM_ReadTrack
		tst.l	d0
		bmi.b	.exit
		bsr	GetIMSG
		cmp.b	#IMSG_CLOSEWINDOW,d0
		beq.b	.exit
		cmp.b	#IMSG_Stop,d0
		beq.b	.exit

		lea	xx_TrackCount(pc),a0
		addq.w	#1,(a0)
		lea	xx_CurrentTrack(pc),a0
		move	(a0),d0
		cmp	(xx_LastTrack,pc),d0
		beq	.next
		add	(xx_TrackInc,pc),d0
		move	d0,(a0)
		bra	.l1
.out
		move.w	xx_NumTracks(pc),d0
		move.l	PrBar0(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		lea	xx_DiskFinished(pc),a0
		move.w	#1,(a0)			; disk finished

		moveq	#IMSG_DiskFinished,d0
.exit
		bsr	DriveMotorOff
		movem.l	(sp)+,d1-d7/a0-a6
		tst.l	d0
		rts
SM_ReadTrack:
		bsr	_ClearTrackBuffer
		move.l	xx_CurrentDecoder(pc),d0
		cmp.l	#DMFM_NULL,d0
		beq.b	.nodec

		move.w	xx_TrackCount(pc),d0
		move.l	PrBar0(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		bsr.b	_ReadTrackCore
		move.l	xx_Debug,d1
		beq	.nodebug
	;on debug
		tst.l	d0
		bpl	.debugok
	;on debug fail
		move.l	d0,-(a7)
		move.l	xx_CurrentTLen,-(a7)
		move	xx_CurrentTrack,-(a7)
		move	xx_CurrentDisk,-(a7)
		lea	.fmt,a0
		bsr	Debug
		add.w	#12,a7
		bra	.nodec
	;on debug success
.debugok	bsr	_TrackCRC16
		bra	.nodec
	;normal
.nodebug	tst.l	d0
		bmi.b	.error
.nodec		moveq	#IERR_OK,d0
		bsr	_StoreTrackToImage
.error		tst.l	d0
		rts

.fmt		dc.b	"Disk=%d Track=%d Length=%lx RC=%ld failed",10,0
	EVEN

_ReadTrackCore:
.retryall
		move.w	xx_Retries(pc),xx_Retry	; retries on error
.retry
		move.w	xx_CurrentTrack(pc),d0
		bsr	Txt1ReadingTrack

		bsr	InputFromFile		; Read from an input file
		move.l	xx_BufferType(pc),d1	; Check the buffer type and
		cmp.l	#BUFFER_ADOS,d1		; skip disk reading if done
		beq	.error
		cmp.l	#BUFFER_MFM,d1
		beq	.doraw
		cmp.l	#IERR_OK,d0
		bne	.error

		move.w	xx_Retry(pc),d0
		cmp.w	xx_Retries(pc),d0	; on first retry
		bne.b	.do

		move.l	xx_CurrentDecoder(pc),d0
		cmp.l	#DMFM_STD,d0		; on standard amiga format
		bne.b	.do
		move.w	xx_CurrentSync(pc),d0
		cmp.w	#SYNC_STD,d0		; on sync $4489
		bne.b	.do
		move.w	xx_CurrentTLen+2(pc),d0
		cmp.w	#$1600,d0		; on 11 sectors per track
		bne.b	.do
		move.l	xx_Disk(pc),a0
		move.w	dsk_Flags(a0),d0
		and.w	#DFLG_RAWREADONLY,d0	; only when DFLG is not set
		bne.b	.do
		bsr	DriveReadDisplay
		bra.b	.error
.do
		bsr	DriveRawReadDisplay
.doraw		tst.l	d0
		bne.b	.error
		move.w	xx_CurrentSync(pc),d0
		cmp.w	#SYNC_INDEX,d0
		beq.b	.nosync
		bsr	_SearchMFMword
		tst.l	d0
		bne.b	.error
.nosync
		lea	xx_ExternalCall(pc),a2
		move.w	#CALL_DECODER,(a2)
		moveq	#0,d0
		move.w	xx_CurrentTrack(pc),d0
		move.l	xx_MFMBuffer(pc),a0
		move.l	xx_Track(pc),a1
		move.l	xx_CurrentDecoder(pc),a2
		cmp.l	#DMFM_STD,a2
		bne.b	.doit
		move.l	xx_CurrentTLen(pc),d0
		lsr.l	#8,d0
		lsr.l	#1,d0
		lea	_DecodeDOSTrack(pc),a2
.doit		lea	rawdic_Library(pc),a5
		lea	xx_ExternalCall(pc),a3
		move.w	#CALL_NONE,(a3)
		jsr	(a2)

.error
		tst.l	d0
		bpl.b	.ok
		move.l	(xx_InputName,pc),d1
		bne.b	.ok		; don't retry if from file

		lea	xx_Retry(pc),a0
		subq.w	#1,(a0)
		bgt	.retry
.ok
		move.l	xx_SlvStruct(pc),a0
		move.b	slv_Flags(a0),d1
		and.b	#SFLG_DEBUG,d1
		bne.b	.s0
		bsr	ParseErrorRequest
		cmp.b	#IMSG_Retry,d0
		beq	.retryall
.s0
		tst.l	d0
		rts


; call user init code

_CallInitCode:
		lea	xx_ExternalCall(pc),a0
		move.w	#CALL_INIT,(a0)
		move.l	xx_Disk(pc),a0
		move.l	dsk_InitCode(a0),d0
		bra.b	_Call

; call user disk code

_CallDiskCode:
		lea	xx_ExternalCall(pc),a0
		move.w	#CALL_DISK,(a0)

		move.l	xx_Disk(pc),a0
		move.l	dsk_DiskCode(a0),d0
_Call:		beq.b	.s0
		move.l	d0,a2

		lea	(Txt1_Working),a0
		bsr	DisplayText

		moveq	#0,d0
		move.w	xx_CurrentDisk(pc),d0

	; starting RawDIC v5, provide address of diskimage
		move.l	xx_SlvStruct,a1
		cmp.b	#5,(slv_Version,a1)
		blo	.skip			;force higher slave version
		move.l	xx_DiskImage,a1
.skip
		lea	rawdic_Library(pc),a5
		jsr	(a2)			; A0=Disk/D0.l=#/A1=image
		lea	xx_ExternalCall(pc),a2
		move.w	#CALL_NONE,(a2)
		tst.l	d0
		rts
.s0		moveq	#IERR_OK,d0
		rts

xx_Stack:	dc.l	0		; save stack
xx_Slave:	dc.l	0		; BCPL (!) pointer to the imager slave
xx_SlvStruct:	dc.l	0		; pointer to the slave structure
xx_Text:	dc.l	0		; pointer to the short info text
xx_RawBuffer:	dc.l	Space		; pointer to the raw mfm buffer (for DMA)
xx_RawBufferChip: dc.l	BufferChip		; pointer to the raw mfm buffer (for DMA)
xx_MFMBuffer:	dc.l	Buffer		; pointer to the raw mfm buffer (for decoder)
xx_RawLength:	dc.l	BUFFERLENGTH	; length of raw buffer (Codetapper)
xx_RawPointer:	dc.l	0		; pointer to the raw buffer
xx_RawBit:	dc.l	0		; bit pointer to raw data
xx_Disk:	dc.l	0		; pointer to the current disk structure
xx_DiskImage:	dc.l	0		; pointer to the diskimage
xx_DiskLen:	dc.l	0		; length of the diskimage
xx_DiskPos:	dc.l	0		; position in diskimage (offset, no pointer)
xx_CurrentDisk:	dc.w	0		; number of current disk
xx_NumDisks:	dc.w	0		; total number of disks (may change while reading!!!)
xx_Track:	dc.l	0		; pointer to thme trackbuffer
xx_TrackLen:	dc.l	0		; length of the trackbuffer (greatest tracklength)
xx_CurrentTLen:	dc.l	0		; length of the current track
xx_TLPosition:	dc.l	0		; pointer to a tracklist entry
xx_Unit:	dc.l	0		; trackdisk device unit
xx_Retry:	dc.w	0		; retry counter for read errors
xx_Retries:	dc.w	5		; retries on error
xx_CurrentTrack: dc.w	0		; current track
xx_LastTrack:	dc.w	0		; last track of a tracklist entry + 1
xx_TrackCount:	dc.w	0		; trackcounter (for progress bar)
xx_NumTracks:	dc.w	0		; total number of tracks
xx_CurrentSync:	dc.w	0		; current sync word
xx_CurrentDecoder: dc.l	0		; current decoderroutine
xx_FLPosition:	dc.l	0		; pointer to a filelist entry
xx_ExternalCall: dc.l	0		; here is the current state for external calls
xx_DiskFinished: dc.w	0		; 1 when diskimage is completely read
xx_Cancel:	dc.w	0		; 1 when error exit on Cancel pressed
xx_TrackInc:	dc.w	0		; -2 , -1 , 1 , 2
_rdargs		dc.l	0
_rdarray
xx_SlaveName:	dc.l	DefaultSlave	; pointer to the name of the imager slave
xx_Retries_args	dc.l	0		; retries on error
xx_SourceName:	dc.l	DefaultSource	; pointer to "DF0:"
xx_InputName:	dc.l	0		; Pointer to the name of the input file
xx_Ignore:	dc.l	0		; 1 when IGNOREERRORS is set
xx_Debug:	dc.l	0		; 1 when DEBUG is set
xx_LastError:   dc.w	0		; last error code from user function call
xx_LastIoErr: dc.w	0		; last error code from user function call

WinMove:	; D0=x-offset/D1=y-offset

		movem.l	a1/a6,-(sp)

		move.l	WinRastPort(pc),a1
		move.l	gfxbase(pc),a6
		jsr	_LVOMove(a6)

		movem.l	(sp)+,a1/a6
		rts

WinPrint:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	a0,a1
.l0		move.b	(a1)+,d0
		bne.b	.l0
		move.l	a1,d0
		sub.l	a0,d0
		subq.l	#1,d0

		move.l	WinRastPort(pc),a1
		move.l	gfxbase(pc),a6
		jsr	_LVOText(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

GetIDCMP:	; D4=IDCMP flags/D5=Code/A4=adress

		movem.l	d0-d3/d6-d7/a0-a3/a5-a6,-(sp)
		move.l	4.w,a6
		move.l	winptr(pc),a0
		move.l	wd_UserPort(a0),a0
		jsr	_LVOGetMsg(a6)		; Msg abholen
		tst.l	d0
		bne.b	.msg
		moveq	#0,d4
		moveq	#0,d5
		sub.l	a4,a4
		bra.b	.exit
.msg
		move.l	d0,a1
		move.l	im_Class(a1),d4		; message bits
		move.w	im_Code(a1),d5		; gruppe
		move.l	im_IAddress(a1),a4	; adresse
		;jsr	_LVOReplyMsg(a6)	; Msg beantworten
.exit
		movem.l	(sp)+,d0-d3/d6-d7/a0-a3/a5-a6
		rts



WaitIDCMP:	; D4=IDCMP flags/D5=Code/A4=adress

		movem.l	d0-d3/d6-d7/a0-a3/a5-a6,-(sp)
		move.l	4.w,a6
		move.l	winptr(pc),a0
		move.l	wd_UserPort(a0),a0
		jsr	_LVOWaitPort(a6)	; auf Msg warten
		move.l	winptr(pc),a0
		move.l	wd_UserPort(a0),a0
		jsr	_LVOGetMsg(a6)		; Msg abholen
		move.l	d0,a1
		move.l	im_Class(a1),d4		; message bits
		move.w	im_Code(a1),d5		; gruppe
		move.l	im_IAddress(a1),a4	; adresse
		;jsr	_LVOReplyMsg(a6)	; Msg beantworten

		movem.l	(sp)+,d0-d3/d6-d7/a0-a3/a5-a6
		rts
ParseIDCMP:
		move.l	d4,d0
		and.l	#IDCMP_GADGETUP,d0
		beq.b	.nogup
		cmp.l	ButtonStart(pc),a4
		beq.b	.startpressed
		cmp.l	ButtonStop(pc),a4
		beq.b	.stoppressed
		cmp.l	ButtonSelectFile(pc),a4		; Select File added by Codetapper
		beq.b	.selectpressed
.nogup		move.l	d4,d0
		and.l	#IDCMP_CLOSEWINDOW,d0
		bne.b	.windowclose
		moveq	#IMSG_NULL,d0
.exit		rts
.windowclose	moveq	#IMSG_CLOSEWINDOW,d0
		bra.b	.exit
.startpressed	moveq	#IMSG_Start,d0
		bra.b	.exit
.stoppressed	moveq	#IMSG_Stop,d0
		bra.b	.exit
.selectpressed	moveq	#IMSG_SelectFile,d0	; Select File added by Codetapper
		bra.b	.exit


OFF_BUTTON:MACRO
	cmp.b	#IMSG_\1,d0
	bne.b	.s\@
	move.l	Button\1(pc),a0
	bsr	OffButton		; turn off start button
	bsr.b	RefreshGadgets
.s\@
	ENDM

GetIMSG:	; gets an action

		movem.l	d1-d7/a0-a6,-(sp)
		bsr	GetIDCMP
		bsr	ParseIDCMP

GetIMSG_end
		OFF_BUTTON	Start
		OFF_BUTTON	Stop
	;	OFF_BUTTON	SelectFile

		movem.l	(sp)+,d1-d7/a0-a6
		rts

WaitIMSG:	; waits for an action to occur

		movem.l	d1-d7/a0-a6,-(sp)
.l0		bsr	WaitIDCMP
		bsr	ParseIDCMP

		cmp.b	#IMSG_NULL,d0
		beq.b	.l0

		bra	GetIMSG_end

RefreshGadgets:	; A0=first gadget

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	winptr(pc),a1
		sub.l	a2,a2
		move.l	intbase(pc),a6
		jsr	_LVORefreshGadgets(a6)
		movem.l	(sp)+,d0-d7/a0-a6
		rts

	include	slave.asm
	include	readfromfile.asm

	section	"stuff",BSS

BUFFERLENGTH	= $7c04
RTFILENAMELEN	= 108
RTDIRNAMELEN	= 512
rtfilename		ds.b	RTFILENAMELEN	; filename buffer for aslreq
rtdirname		ds.b	RTDIRNAMELEN	; dirname buffer for aslreq
rtname			ds.b	RTFILENAMELEN+RTDIRNAMELEN
			ds.w	4		; space for 4 syncs
Buffer:			ds.b	BUFFERLENGTH
stringBuffer:
sourceNameBuffer:	ds.b	$0200
slaveNameBuffer:	ds.b	$0200		; 512 chars for filename + path should be enough
Space:			ds.b	BUFFERLENGTH

	section	"chipmem",BSS_C

BufferChip:		ds.b	BUFFERLENGTH

