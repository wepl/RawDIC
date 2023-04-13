;*---------------------------------------------------------------------------
; Program:	readfromfile.asm
; Contents:	Input file routines for "RawDIC" (c) John Selck and Codetapper
; Author:	Codetapper/JOTD/Wepl
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
;               24.04.05 JOTD
;                        - removed all specific MFM code to replace by direct
;                          trackwarp.library support
;                        - replaced reqtools.library by asl.library
;		27.04.05 Wepl
;			 - asl-lvo's removed
;			 - twInfo removed, tries twReadForm if allowed aand twReadRaw afterwards
;			 - cleanup
;
; Copyright:	Public Domain
; Language:	68000 Assembler
; Translator:	Barfly
; To Do:
;---------------------------------------------------------------------------*

BUFFER_EMPTY	equ	0
BUFFER_ADOS	equ	1
BUFFER_MFM	equ	2

xx_BufferType	dc.l	0			;Indicate what kind of data is in the buffer
xx_ReadFromPtr	dc.l	0

;---------------------------------------------------------------------

;---------------------------------------------------------------------

InputFromFile	move.l	#BUFFER_EMPTY,xx_BufferType	;Mark the buffer as empty

		move.l	xx_InputName(pc),d0		;If there is no filename to read from, don't bother doing anything
		beq	.NoReadFromFile

		bsr	GetAbsTrackNum			;Work out the actual track -> d0
		move.l	xx_MFMBuffer(pc),a0
		movem.l	d1-d7/a0-a6,-(sp)
		bsr	ReadMFMTrack
		movem.l	(sp)+,d1-d7/a0-a6

		move.l	xx_RawBuffer(pc),xx_RawPointer
		clr.l	xx_RawBit			;initialize bitpointer

		rts

.NoReadFromFile	moveq	#IERR_OK,d0			;No error and buffer empty (read from disk)
		rts

;---------------------------------------------------------------------


;---------------------------------------------------------------------
; ReadMFMTrack	
; d0.w = Track we want loaded
; a0   = Buffer
;>d0.l = error code (was incorrect: Number of bytes of data now in the buffer)

ReadMFMTrack	move.l	d0,d4
		ext.l	d4
		move.l	a0,a4

		move.l	xx_ReadFromPtr(pc),d0
		bne	.FileIsOpen

		move.l	xx_InputName(pc),d1
		bne	.nameok
		moveq	#IERR_INP_OPEN,d0
		rts

.nameok		move.l	d1,a0
		sub.l	a1,a1
		move.l	twbase(pc),d1
		bne	.twok
		moveq	#IERR_TWLIB,d0
		rts

.twok		move.l	d1,a6
		jsr	_LVOtwOpen(a6)
		move.l	d0,xx_ReadFromPtr	;Store the file pointer (tw handle)
		beq	.error

.FileIsOpen	
		cmp.l	#DMFM_STD,xx_CurrentDecoder	;Accept Dos or MFM data
		bne	.raw

.dos		move.l	xx_ReadFromPtr(pc),d0	;Handle reading from an ADF file
		move.l	d4,d1			;track number
		move.l	xx_Track(pc),a0
		move.l	#TWTT_DOS,d2
		move.l	twbase(pc),a6
		jsr	(_LVOtwReadForm,a6)
		tst.l	d0
		bne	.formok
		move.l	dosbase,a6
		jsr	(_LVOIoErr,a6)
		cmp.l	#TWE_NoReadForm,d0
		beq	.raw
		bra	.error

.formok		move.l	#BUFFER_ADOS,xx_BufferType	;Mark the buffer as full of AmigaDos data
		moveq	#IERR_OK,d0
		rts

.raw		move.l	xx_ReadFromPtr(pc),d0	;Handle reading
		move.l	d4,d1			;track number
		move.l	#BUFFERLENGTH,d2
		move.l	xx_RawBuffer(pc),a0
		move.l	twbase(pc),a6
		jsr	(_LVOtwReadRaw,a6)
		tst.l	d0
		beq	.error

.rawok		move.l	d0,xx_RawLength
		move.l	#BUFFER_MFM,xx_BufferType	;Mark the buffer as MFM
		moveq	#IERR_OK,d0
		rts
	
.error		move.l	dosbase,a6
		jsr	(_LVOIoErr,a6)
		move.l	d0,d1			; code
		move.l	#.header,d2
		move.l	#Txt1_TWLIB,d3
		move.l	#90,d4
		move.l	twbase(pc),a6
		jsr	(_LVOtwFault,a6)
		move.l	#IERR_TWLIB,d0
		rts

.header:
	dc.b	"Track %d",0
	even

Txt1_TWLIB
	blk.b	101,0
	even

;---------------------------------------------------------------------

CloseReadFrom	movem.l	d0-d1/a0-a6,-(sp)
		move.l	xx_ReadFromPtr(pc),d0
		beq	.Nothing
		move.l	twbase(pc),d1
		beq	.Nothing
		move.l	d1,a6
		jsr	_LVOtwClose(a6)
		clr.l	xx_ReadFromPtr
.Nothing	movem.l	(sp)+,d0-d1/a0-a6
		rts

;---------------------------------------------------------------------

RequestAFile	movem.l	d0-d4/d7/a2-a3/a6,-(sp)

		move.l	aslbase(pc),d0
		beq	.norequester
		move.l	d0,a6			;a6 = reqbase

		move.l	#ASL_FileRequest,d0		;mode
		sub.l	a0,a0			;tags
		jsr	(_LVOAllocAslRequest,a6)
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
		pea	ASLFR_InitialDrawer
		pea	.pattern
		pea	ASLFR_InitialPattern
		pea	.title(pc)
		pea	ASLFR_TitleText
		pea	1
		pea	ASLFR_DoPatterns

		move.l	d7,a0			;filereq
		move.l	a7,a1			;tags

		move.l	aslbase,a6
		jsr	(_LVOAslRequest,a6)
		add.w	#9*4,a7
		tst.l	d0
		beq	.requesterfail


	;copy dirname for next requester
		move.l	d7,a0
		move.l	(fr_Drawer,a0),a0
		lea	rtfilename,a1		;Temporary buffer for filename
		move.w	#RTDIRNAMELEN-2,d0
.cpydir		move.b	(a0)+,(a1)+
		dbeq	d0,.cpydir
		clr.b	(a1)
		
	;build full name
		move.l	d7,a0
		move.l	(fr_Drawer,a0),a0
		lea	rtname,a1
		move.w	#RTDIRNAMELEN+RTFILENAMELEN-2,d0
.cpy		move.b	(a0)+,(a1)+
		dbeq	d0,.cpy
		clr.b	(a1)
		lea	(rtname),a0
		move.l	a0,d1

		move.l	d7,a0
		move.l	(fr_File,a0),d2

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
.freereq
		move.l	d7,a0
		move.l	aslbase,a6
		jsr	(_LVOFreeAslRequest,a6)
.exitreq
		movem.l	(sp)+,_MOVEMREGS
		rts

.requesterfail
		bra.b	.freereq
.norequester
		bra.b	.exitreq

.title		dc.b	"Select a file",0
.pattern	dc.b	"((#?.(adf|dic|mfm|wrp|wwp|ipf))|disk.[0-9])",0
.varname	dc.b	"RawDIC.InputFile",0
		EVEN

