;*---------------------------------------------------------------------------
; Program:	systeminit.asm
; Contents:	System initialisation routines for "RawDIC"
; Author:	John Selck
; History:	12.10.02 - v1.1 (Codetapper)
;		         - OpenDebug routine modified to check if the 
;		           tooltype DEBUG was set and if so, open the
;		           debug file
;		01.02.04 - v1.9 (Codetapper)
;		         - Motor on/off commands are skipped if there is 
;		           an input file
; Copyright:	Public Domain
; Language:	68000 Assembler
; Translator:	Barfly
;---------------------------------------------------------------------------*

	; this include features all init code and gui stuff

;		include	startupcode.asm
Start:
		lea	xx_ToolTypes(pc),a1
		move.l	a0,(a1)
		bsr	ParseToolTypes

		move.l	4.w,a6

		moveq	#0,d0
		lea	dosname(pc),a1
		jsr	_LVOOpenLibrary(a6)
		tst.l	d0
		beq	.nodos
		lea	dosbase(pc),a1
		move.l	d0,(a1)

		moveq	#0,d0
		lea	intname(pc),a1
		jsr	_LVOOpenLibrary(a6)
		tst.l	d0
		beq	.noint
		lea	intbase(pc),a1
		move.l	d0,(a1)

		moveq	#0,d0
		lea	gfxname(pc),a1
		jsr	_LVOOpenLibrary(a6)
		tst.l	d0
		beq.b	.nogfx
		lea	gfxbase(pc),a1
		move.l	d0,(a1)

		moveq	#0,d0			; Codetapper
		lea	reqname(pc),a1
		jsr	_LVOOpenLibrary(a6)
		lea	reqbase(pc),a1
		move.l	d0,(a1)

		moveq	#0,d0			; Codetapper
		lea	xpkname(pc),a1
		jsr	_LVOOpenLibrary(a6)
		lea	xpkbase(pc),a1
		move.l	d0,(a1)

		move.l	dosbase(pc),a6
		jsr	_LVOOutput(a6)
		lea	output(pc),a0
		move.l	d0,(a0)

		move.l	xx_SlaveName(pc),d0
		bne.b	.doit
		lea	rawdicInfo(pc),a0
		bsr	WriteStdOut
		bra.b	.skip
.doit
		bsr.b	Next1		; libraries ok, continue...
.skip
		bsr	CloseReadFrom		;Close the read from file if there was one
		move.l	4.w,a6
		move.l	xpkbase(pc),d0
		beq	.closereq
		move.l	d0,a1
		jsr	_LVOCloseLibrary(a6)
.closereq	move.l	reqbase(pc),d0
		beq	.closegfx
		move.l	d0,a1
		jsr	_LVOCloseLibrary(a6)
.closegfx	move.l	gfxbase(pc),a1
		jsr	_LVOCloseLibrary(a6)
.nogfx		move.l	intbase(pc),a1
		jsr	_LVOCloseLibrary(a6)
.noint		move.l	dosbase(pc),a1
		jsr	_LVOCloseLibrary(a6)
.nodos		rts

Next1:
		bsr	OpenSlave
		beq.b	.noslv
		bsr.b	CreatePort
		beq.b	.noport
		bsr	OpenDevice
		bne.b	.nodev
		bsr	OpenMainWindow
		beq.b	.nowindow
		bsr	main
		bsr	CloseMainWindow
.nowindow	bsr	CloseDevice
.nodev		bsr.b	DeletePort
.noport		bsr	CloseSlave
.noslv		rts

CreatePort:
		move.l	4.w,a6
		moveq	#-1,d0
		jsr	_LVOAllocSignal(a6)
		tst.b	d0
		bmi.b	.noport

		lea	ReplyPort(pc),a1
		clr.l	LN_NAME(a1)
		clr.b	LN_PRI(a1)
		move.b	#NT_MSGPORT,LN_TYPE(a1)
		move.b	#PA_SIGNAL,MP_FLAGS(a1)
		move.b	d0,MP_SIGBIT(a1)

		sub.l	a1,a1
		jsr	_LVOFindTask(a6)
		lea	ReplyPort(pc),a1
		move.l	d0,MP_SIGTASK(a1)

		lea	MP_MSGLIST(a1),a1
		NEWLIST	a1

		lea	IORequest(pc),a1
		move.b	#NT_MESSAGE,LN_TYPE(a1)
		move.w	#IOSTD_SIZE,MN_LENGTH(a1)
		lea	ReplyPort(pc),a2
		move.l	a2,MN_REPLYPORT(a1)

		moveq	#1,d0
		rts
.noport		lea	txt2_noport(pc),a0
		bsr	WriteStdOut
		moveq	#0,d0
		rts

DeletePort:
		move.l	4.w,a6
		lea	ReplyPort(pc),a1
		move.l	LN_NAME(a1),d0
		beq.b	.s0
		REMOVE
.s0
		lea	ReplyPort(pc),a1
		moveq	#0,d0
		move.b	MP_SIGBIT(a1),d0
		jsr	_LVOFreeSignal(a6)
		rts

OpenDevice:
		move.l	4.w,a6
		moveq	#0,d0
		move.w	xx_Unit(pc),d0		; device #
		moveq	#TDB_ALLOW_NON_3_5,d1			; flags
		lea	trackdiskname(pc),a0
		lea	IORequest(pc),a1
		jsr	_LVOOpenDevice(a6)
		tst.l	d0
		beq.b	.ok
		lea	txt2_nodev(pc),a0
		bsr	WriteStdOut
.ok		tst.l	d0
		rts

CloseDevice:
		move.l	4.w,a6
		lea	IORequest(pc),a1
		jsr	_LVOCloseDevice(a6)
		rts

	; ####################
	; ###		   ###
	; ### Window stuff ###
	; ###		   ###
	; ####################

OpenMainWindow:
		movem.l	d1-d7/a0-a6,-(sp)

	; Get screen data

		move.l	4.w,a6
		move.l	#sc_SIZEOF,d0
		move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
		jsr	_LVOAllocMem(a6)
		tst.l	d0
		beq	.nowindow

		move.l	d0,-(sp)

		move.l	intbase(pc),a6
		move.l	(sp),a0
		sub.l	a1,a1
		move.l	#sc_SIZEOF,d0
		move.l	#WBENCHSCREEN,d1
		jsr	_LVOGetScreenData(a6)
		move.l	(sp)+,d1
		tst.l	d0
		beq	.nowindow
		move.l	d1,-(sp)

	; Get default font for window

		move.l	gfxbase(pc),a6
		lea	TextAttrRP(pc),a0
		move.l	(sp),a1
		lea	sc_RastPort(a1),a1
		jsr	_LVOAskFont(a6)

		lea	TextAttrRP(pc),a0
		jsr	_LVOOpenFont(a6)
		lea	textfontrp(pc),a0
		move.l	d0,(a0)

		move.l	(sp),a1
		moveq	#0,d0
		moveq	#0,d1
		move.b	sc_WBorLeft(a1),d0
		move.b	sc_WBorRight(a1),d1
		add.w	d0,d1
		lea	windowstruct(pc),a1
		move.w	nw_Width(a1),d0
		sub.w	d1,d0
		lea	wininnerxsize(pc),a1
		move.w	d0,(a1)

	; Initialise textfields

		move.l	intbase(pc),a6

		moveq	#4,d0
		moveq	#2,d1
		move.w	wininnerxsize(pc),d2
		subq.w	#8,d2	; 4 pixels distance left and right
		moveq	#6,d3
		move.l	textfontrp(pc),a0
		add.w	tf_YSize(a0),d3
		add.w	tf_YSize(a0),d3		; 2 textlines
		;moveq	#22,d3
		move.w	#160,d4
		bsr	NewTextDisplay
		move.l	a0,TextDisplay0

		moveq	#4,d0
		moveq	#2,d1
		move.l	TextDisplay0(pc),a0
		add.w	ty_TopEdge(a0),d1
		add.w	ty_Height(a0),d1
		;moveq	#26,d1
		move.w	wininnerxsize(pc),d2
		subq.w	#8,d2	; 4 pixels distance left and right
		moveq	#6,d3
		move.l	textfontrp(pc),a0
		add.w	tf_YSize(a0),d3		; 1 textlines
		;moveq	#14,d3
		move.w	#160,d4
		bsr	NewTextDisplay
		move.l	a0,TextDisplay1
		move.b	#4,ty_BackPen(a0)

	; pre-init buttons (no y-position yet)

		move.l	winptr(pc),a0
		lea	String0(pc),a1
		moveq	#4,d0
		moveq	#4,d1
		bsr	NewButton
		move.l	a0,Button0

		move.l	winptr(pc),a0
		lea	String1(pc),a1
		moveq	#4,d0
		moveq	#4,d1
		bsr	NewButton
		move.l	a0,Button1

		move.l	winptr(pc),a0		;Select File button added by Codetapper
		lea	String2(pc),a1
		moveq	#4,d0
		moveq	#4,d1
		bsr	NewButton
		move.l	a0,Button2

		move.l	Button1(pc),a0
		move.w	wininnerxsize(pc),d0
		subq.w	#4,d0	; 4 pixels distance right
		sub.w	bt_gg_OFFS+gg_Width(a0),d0
		move.w	d0,bt_gg_OFFS+gg_LeftEdge(a0)

		move.l	Button1(pc),a0
		bsr	OffButton

		move.l	Button2(pc),a0		; Center the Select button
		move.w	wininnerxsize(pc),d0	; Added by Codetapper
		sub.w	bt_gg_OFFS+gg_Width(a0),d0
		asr.w	#1,d0
		move.w	d0,bt_gg_OFFS+gg_LeftEdge(a0)

		move.l	reqbase(pc),d0		;If reqtools or XPK are not found
		beq	.noselect		;disable the Select button
		move.l	xpkbase(pc),d0
		bne	.selectok

.noselect	move.l	Button2(pc),a0
		bsr	OffButton

.selectok

	; initialise progress bars

		moveq	#4,d0
		moveq	#2,d1
		move.l	TextDisplay1(pc),a0
		add.w	ty_TopEdge(a0),d1
		add.w	ty_Height(a0),d1
		;moveq	#42,d1
		move.w	wininnerxsize(pc),d2
		subq.w	#8,d2	; 4 pixels distance left and right
		moveq	#10,d3
		moveq	#1,d4
		bsr	NewProgressBar
		move.l	a0,PrBar0

		moveq	#4,d0
		moveq	#2,d1
		move.l	PrBar0(pc),a0
		add.w	prb_TopEdge(a0),d1
		add.w	prb_Height(a0),d1
		;moveq	#54,d1
		move.w	wininnerxsize(pc),d2
		subq.w	#8,d2	; 4 pixels distance left and right
		moveq	#10,d3
		moveq	#1,d4
		bsr	NewProgressBar
		move.l	a0,PrBar1

		moveq	#2,d1
		move.l	PrBar1(pc),a0
		add.w	prb_TopEdge(a0),d1
		add.w	prb_Height(a0),d1
		move.l	Button0(pc),a0
		move.w	d1,bt_gg_OFFS+gg_TopEdge(a0)
		move.l	Button1(pc),a0
		move.w	d1,bt_gg_OFFS+gg_TopEdge(a0)

		move.l	Button2(pc),a0		; Added by Codetapper
		move.w	d1,bt_gg_OFFS+gg_TopEdge(a0)

	; resize window

		moveq	#0,d0
		moveq	#1,d1
		move.l	(sp),a1
		add.b	sc_WBorTop(a1),d1
		add.b	sc_BarHeight(a1),d1
		add.b	sc_WBorBottom(a1),d1
		;add.b	sc_BarHBorder(a1),d1

		move.l	Button1(pc),a0
		add.w	bt_gg_OFFS+gg_TopEdge(a0),d1
		add.w	bt_gg_OFFS+gg_Height(a0),d1

		lea	windowstruct(pc),a0
		move.w	d1,nw_Height(a0)

		move.w	sc_Width(a1),d0
		sub.w	nw_Width(a0),d0
		asr.w	#1,d0
		move.w	d0,nw_LeftEdge(a0)
		move.w	sc_Height(a1),d0
		sub.w	nw_Height(a0),d0
		asr.w	#1,d0
		move.w	d0,nw_TopEdge(a0)

	; free screen data

		move.l	4.w,a6
		move.l	(sp)+,a1
		move.l	#sc_SIZEOF,d0
		jsr	_LVOFreeMem(a6)

	; Open Window

		lea	windowstruct(pc),a0
		move.l	intbase(pc),a6
		jsr	_LVOOpenWindow(a6)
		tst.l	d0
		beq	.nowindow
		lea	winptr(pc),a1
		move.l	d0,(a1)
		move.l	d0,a1
		move.l	wd_RPort(a1),d0
		lea	WinRastPort(pc),a1
		move.l	d0,(a1)

		move.l	gfxbase(pc),a6
		move.l	textfontrp(pc),a0
		move.l	WinRastPort(pc),a1
		jsr	_LVOSetFont(a6)

		move.l	intbase(pc),a6

		move.l	winptr(pc),a0
		move.l	Button0(pc),a1
		lea	bt_gg_OFFS(a1),a1
		moveq	#-1,d0
		jsr	_LVOAddGadget(a6)

		move.l	winptr(pc),a0
		move.l	Button1(pc),a1
		lea	bt_gg_OFFS(a1),a1
		moveq	#-1,d0
		jsr	_LVOAddGadget(a6)

		move.l	winptr(pc),a0		;Added by Codetapper
		move.l	Button2(pc),a1
		lea	bt_gg_OFFS(a1),a1
		moveq	#-1,d0
		jsr	_LVOAddGadget(a6)

	; draw everything

		move.l	winptr(pc),a1
		move.l	Button0(pc),a0
		sub.l	a2,a2
		jsr	_LVORefreshGadgets(a6)

		move.w	#0,d0
		move.l	PrBar0(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		move.w	#0,d0
		move.l	PrBar1(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshProgressBar

		move.l	TextDisplay0(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshTextDisplay

		move.l	TextDisplay1(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshTextDisplay

		move.l	gfxbase(pc),a6

		moveq	#1,d0
		move.l	WinRastPort(pc),a1
		jsr	_LVOSetAPen(a6)
		moveq	#0,d0
		move.l	WinRastPort(pc),a1
		jsr	_LVOSetBPen(a6)

		moveq	#8,d0
		moveq	#1,d1
		move.l	TextDisplay0(pc),a0
		add.w	ty_TopEdge(a0),d1
		move.l	textfontrp(pc),a0
		add.w	tf_YSize(a0),d1
		bsr	WinMove

		move.l	xx_SlvStruct(pc),a0
		move.l	slv_Text(a0),a0
		move.l	a0,a1
.l0		move.b	(a1)+,d0
		cmp.b	#$20,d0
		bhs.b	.l0
		move.l	a1,d0
		sub.l	a0,d0
		subq.l	#1,d0
		move.l	a1,-(sp)

		move.l	WinRastPort(pc),a1
		move.l	gfxbase(pc),a6
		jsr	_LVOText(a6)

		moveq	#8,d0
		moveq	#1,d1
		move.l	TextDisplay0(pc),a0
		add.w	ty_TopEdge(a0),d1
		move.l	textfontrp(pc),a0
		add.w	tf_YSize(a0),d1
		add.w	tf_YSize(a0),d1
		bsr	WinMove

		move.l	(sp)+,a0
		tst.b	-1(a0)
		beq.b	.noline
		move.l	a0,a1
.l1		move.b	(a1)+,d0
		cmp.b	#$20,d0
		bhs.b	.l1
		move.l	a1,d0
		sub.l	a0,d0
		subq.l	#1,d0

		move.l	WinRastPort(pc),a1
		move.l	gfxbase(pc),a6
		jsr	_LVOText(a6)
.noline
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#-1,d0
		rts
.nowindow
		lea	txt2_nowin(pc),a0
		bsr	WriteStdOut
		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#0,d0
		rts


CloseMainWindow:
		move.l	TextDisplay1(pc),a0
		bsr	RemTextDisplay
		move.l	TextDisplay0(pc),a0
		bsr	RemTextDisplay
		move.l	PrBar0(pc),a0
		bsr	RemProgressBar
		move.l	PrBar1(pc),a0
		bsr	RemProgressBar
		move.l	winptr(pc),a0
		move.l	Button0(pc),a1
		bsr	RemButton
		move.l	winptr(pc),a0
		move.l	Button1(pc),a1
		bsr	RemButton

		move.l	winptr(pc),a0		; Added by Codetapper
		move.l	Button2(pc),a1
		bsr	RemButton

		move.l	textfontrp(pc),a1
		move.l	gfxbase(pc),a6
		jsr	_LVOCloseFont(a6)
		move.l	winptr(pc),a0
		move.l	intbase(pc),a6
		jsr	_LVOCloseWindow(a6)
		rts
OpenSlave:
		move.l	dosbase(pc),a6
		move.l	xx_SlaveName(pc),d1
		jsr	_LVOLoadSeg(a6)
		lea	xx_Slave(pc),a0
		move.l	d0,(a0)
		lsl.l	#2,d0
		addq.l	#8,d0
		move.l	d0,a0
		move.l	(a0)+,d0
		cmp.l	#"RAWD",d0	; header test
		bne.b	.error
		move.w	(a0)+,d0
		cmp.w	#"IC",d0
		bne.b	.error
		move.l	a0,d0
		lea	xx_SlvStruct(pc),a0
		move.l	d0,(a0)
		move.l	xx_Slave(pc),d0
		rts
.error		bsr.b	CloseSlave
		lea	txt2_noslv(pc),a0
		bsr	WriteStdOut
		lea	xx_Slave(pc),a0
		clr.l	(a0)
		moveq	#0,d0
		rts
CloseSlave:
		move.l	dosbase(pc),a6
		move.l	xx_Slave(pc),d1
		beq.b	.no
		jsr	_LVOUnLoadSeg(a6)
.no		rts

		cnop	0,4
winptr:		dc.l	0
WinRastPort:	dc.l	0
wininnerxsize:	dc.w	0
Button0:	dc.l	0
Button1:	dc.l	0
Button2:	dc.l	0		; Select File button added by Codetapper
PrBar0:		dc.l	0
PrBar1:		dc.l	0
TextDisplay0:	dc.l	0
TextDisplay1:	dc.l	0

dosbase:	dc.l	0		; dos.library
intbase:	dc.l	0		; intuition.library
gfxbase:	dc.l	0		; graphics.library
reqbase:	dc.l	0		; reqtools.library
xpkbase:	dc.l	0		; xpkmaster.library
trackbase:	dc.l	0		; trackdisk.device
textfontrp:	dc.l	0		; TextFont of default font
slaveseg:	dc.l	0		; slave
output:		dc.l	0		; output handle

		cnop	0,8
IORequest:	ds.b	IOSTD_SIZE	; disk io structure
ReplyPort:	ds.b	MP_SIZE		; reply port for this task
TextAttrRP:	ds.b	ta_SIZEOF	; TextAttr structure of font

dosname:	dc.b	"dos.library",0
intname:	dc.b	"intuition.library",0
gfxname:	dc.b	"graphics.library",0
reqname:	dc.b	"reqtools.library",0
xpkname:	dc.b	"xpkmaster.library",0
trackdiskname:	dc.b	"trackdisk.device",0
		cnop	0,8
windowstruct:
		dc.w	32,32	; top left
		dc.w	MainWinXSize,160 	; width height
		dc.b	1,0	; detail/block pen
		dc.l	IDCMP_CLOSEWINDOW|IDCMP_GADGETUP	; IDCMP Flag
		dc.l	WFLG_ACTIVATE|WFLG_CLOSEGADGET|WFLG_DEPTHGADGET|WFLG_DRAGBAR|WFLG_GIMMEZEROZERO	;WFLG_SIZEGADGET
		dc.l	0	; Gadget Ptr
		dc.l	0	; Check Mark Gfx Ptr
		dc.l	title	; Title Ptr
		dc.l	0	; Screen Ptr
		dc.l	0	; Bitmap Ptr
		dc.w	0,0	; Min Size
		dc.w	0,0	; Max Size
		dc.w	WBENCHSCREEN	; Type

title:		sprintx	"RawDIC V%ld.%ld ",Version,Revision
		INCBIN	"T:date"
		dc.b	0
String0:	dc.b	"Start",0
String1:	dc.b	"Stop",0
String2:	dc.b	"Select File",0
DefaultSlave:	dc.b	"Default.islave",0
DefaultSource:	dc.b	"DF0:",0
;Font_Topaz_Name:	dc.b	"topaz.font",0
;		cnop	0,8
;Font_Topaz:	dc.l	Font_Topaz_Name
;		dc.w	TOPAZ_EIGHTY
;		dc.b	FS_NORMAL
;		dc.b	FPF_ROMFONT
		cnop	0,2

Txt1InvalidSlave:	; displays "Invalid slave!"
		lea	Txt1_IS(pc),a0
		bra.b	Txt1_1
Txt1Cancelled:
		lea	Txt1_C(pc),a0
		bra.b	Txt1_1
Txt1Finished:
		lea	Txt1_F(pc),a0
Txt1_1:
		movem.l	d0/a0-a1,-(sp)
		bsr.b	ClearTxt1Buffer
		lea	Txt1Buffer(pc),a1
		bsr	CopyCString
		bsr	PrintOnTextDisplay1
		movem.l	(sp)+,d0/a0-a1
		rts
Txt1InsertDisk:	; displays "Insert disk ??? and press Start."
		; D0.b=disknumber (0-255)
		movem.l	d0/a0-a1,-(sp)
		bsr.b	ClearTxt1Buffer
		lea	Txt1_ID(pc),a0
		lea	Txt1Buffer(pc),a1
		bsr.b	CopyCString
		and.w	#$00ff,d0
		bsr.b	ConvertWord2Asc
		lea	Txt1_APS(pc),a0
		bsr.b	CopyCString
		bsr	PrintOnTextDisplay1
		movem.l	(sp)+,d0/a0-a1
		rts
Txt1ReadingTrack:	; displays "Reading track ???."
		; D0.w=tracknumber (0-65535)
		movem.l	d0/a0-a1,-(sp)
		bsr.b	ClearTxt1Buffer
		lea	Txt1_RT(pc),a0
		lea	Txt1Buffer(pc),a1
		bsr.b	CopyCString
		bsr.b	ConvertWord2Asc
		move.b	#".",(a1)+
		bsr	PrintOnTextDisplay1_noclr
		movem.l	(sp)+,d0/a0-a1
		rts
Txt1_NumPoint:
		movem.l	d0/a0-a1,-(sp)
		bsr.b	Txt1_DoNumPoint
		movem.l	(sp)+,d0/a0-a1
		bra	PrintOnTextDisplay1
Txt1_DoNumPoint:
		bsr.b	ClearTxt1Buffer
		lea	Txt1Buffer(pc),a1
		bsr.b	CopyCString
		bsr.b	ConvertWord2Asc
		move.b	#".",(a1)+
		rts
ClearTxt1Buffer:
		movem.l	d0/a0,-(sp)
		lea	Txt1Buffer(pc),a0
		moveq	#31,d0
.l0		move.b	#$20,(a0)+
		dbra	d0,.l0
		movem.l	(sp)+,d0/a0
		rts

CopyCString:	; copies a 0-terminated C-String. (0 will not be copied)
		move.l	d0,-(sp)
.l0		move.b	(a0)+,d0
		beq.b	.fin
		move.b	d0,(a1)+
		bra.b	.l0
.fin		move.l	(sp)+,d0
		rts
ConvertWord2Asc:	; A1=Destination
		; D0.w=value

		movem.l	d0-d1/a0,-(sp)
		lea	Dec(pc),a0
		moveq	#4,d1
		swap	d0
.l0		swap	d0
		and.l	#$0000ffff,d0
		divu.w	(a0)+,d0
		tst.w	d0
		dbne	d1,.l0
.l1		or.b	#"0",d0
		move.b	d0,(a1)+
		swap	d0
		and.l	#$0000ffff,d0
		divu.w	(a0)+,d0
		subq.w	#1,d1
		bpl.b	.l1
		movem.l	(sp)+,d0-d1/a0
		rts
Dec:		dc.w	10000,1000,100,10,1

ConvertWord2Hex:	; A1=Destination
		; D0.w=value

		movem.l	d0-d1/a0,-(sp)

		moveq	#3,d2
.l0		rol.w	#4,d0
		move.w	d0,d1
		and.w	#$000f,d1
		or.w	#"0",d1
		cmp.w	#"9",d1
		ble.b	.s0
		addq.w	#"A"-"9"-1,d1
.s0		move.b	d1,(a1)+
		dbra	d2,.l0

		movem.l	(sp)+,d0-d1/a0
		rts

PrintOnTextDisplay1:
		bsr.b	Txt1_CLR
PrintOnTextDisplay1_noclr:
		movem.l	d0-d7/a0-a6,-(sp)
		move.l	gfxbase(pc),a6
		move.l	WinRastPort(pc),a1
		moveq	#2,d0
		jsr	_LVOSetAPen(a6)
		moveq	#4,d0
		jsr	_LVOSetBPen(a6)
		moveq	#4,d0
		moveq	#1,d1
		move.l	TextDisplay1(pc),a0
		add.w	ty_LeftEdge(a0),d0
		add.w	ty_TopEdge(a0),d1
		move.l	textfontrp(pc),a0
		add.w	tf_YSize(a0),d1
		jsr	_LVOMove(a6)
		moveq	#32,d0
		lea	Txt1Buffer(pc),a0
		jsr	_LVOText(a6)
		movem.l	(sp)+,d0-d7/a0-a6
		rts
Txt1_CLR:
		movem.l	a0-a1,-(sp)
		move.l	TextDisplay1(pc),a0
		move.l	winptr(pc),a1
		bsr	RefreshTextDisplay	; clear text area
		movem.l	(sp)+,a0-a1
		rts

Txt1Buffer:	ds.b	32	; empty buffer for textdisplay
Txt1_NF:	dc.b	"Illegal RawDIC function call!",0
Txt1_NC:	dc.b	"Unknown disk version!",0
Txt1_TLT:	dc.b	"Track not in TrackList!",0
Txt1_TLI:	dc.b	"TrackList invalid!",0
Txt1_OOM:	dc.b	"Out of memory!",0
Txt1_ND:	dc.b	"No disk in drive!",0
Txt1_OD:	dc.b	"File exceeds diskimage!",0
Txt1_UDV:	dc.b	"Unknown disk structure version!",0
Txt1_UV:	dc.b	"Unknown slave version!",0
Txt1_UF:	dc.b	"Undefined flags set!",0
Txt1_IS:	dc.b	"Invalid slave!",0
Txt1_ID:	dc.b	"Insert disk ",0
Txt1_APS:	dc.b	" and press Start.",0
Txt1_RT:	dc.b	"Reading track ",0
Txt1_CE:	dc.b	"Checksum error on track ",0
Txt1_SE:	dc.b	"No sync signal on track ",0
Txt1_SM:	dc.b	"Sector missing on track ",0
Txt1_C:		dc.b	"Cancelled!",0
Txt1_F:		dc.b	"Finished.",0
Txt1_INP_OPEN:	dc.b	"Input file would not open on track ",0
Txt1_INP_SEEK:	dc.b	"Input file seek error on track ",0
Txt1_INP_READ:	dc.b	"Input file read error on track ",0
Txt1_INP_NOTRK:	dc.b	"Input file does not contain track ",0
Txt1_INP_BADHD:	dc.b	"Input file has a bad header on track ",0
Txt1_XPK_DEPACK: dc.b	"MFMWarp XPK depack error on track ",0
Txt1_MC1_DEPACK: dc.b	"MFMWarp MC1 depack error on track ",0
Txt1_DIP_DEPACK: dc.b	"MFMWarp DIP depack error on track ",0
Txt1_MFM_CSUM:	dc.b	"MFMWarp checksum error on track ",0
Txt1_INP_ILLEN:	dc.b	"Input file illegal length on track ",0
Txt1_FORMAT_UNS: dc.b	"Input file format is unsupported on track ",0
Txt1_INP_INCOMP: dc.b	"Input file format is incompatible with decoder on track ",0
Txt1_NMD_DEPACK: dc.b	"NOMADWarp depack error on track ",0
Txt1_WWP_UNS:	dc.b	"WWarp file format v2+ is unsupported on track ",0
Txt1_TABL_DATA:	dc.b	"WWarp table data error on track ",0
Txt1_TABL_UNS:	dc.b	"WWarp table header version unsupported on track ",0
Txt1_TRCK_UNS:	dc.b	"WWarp track header version unsupported on track ",0
Txt1_TRCK_TYPE:	dc.b	"WWarp track type unsupported on track ",0
Txt1_TRCK_FLAG:	dc.b	"WWarp track flags unsupported on track ",0
		cnop	0,2

 STRUCTURE Button,0

	STRUCT	bt_gg_OFFS,gg_SIZEOF	; Gadget for Button
	STRUCT	bt_bd_OFFS0,bd_SIZEOF	; Border
	STRUCT	bt_bd_OFFS1,bd_SIZEOF
	STRUCT	bt_bd_OFFS0c,bd_SIZEOF	; Border (when pressed)
	STRUCT	bt_bd_OFFS1c,bd_SIZEOF
	STRUCT	bt_bd_XY_OFFS0,(5*4)	; Border lines
	STRUCT	bt_bd_XY_OFFS1,(5*4)
	STRUCT	bt_it_OFFS,it_SIZEOF	; IntuiText
	LABEL	bt_SIZEOF

 STRUCTURE ProgressBar,0

	WORD	prb_LeftEdge		; Position in RastPort
	WORD	prb_TopEdge
	WORD	prb_Width		; Size
	WORD	prb_Height
	WORD	prb_Fill		; the actual state of completion
	WORD	prb_MaxFill		; when prb_Fill=prb_MaxFill, the bar is 100%
	STRUCT	prb_bd_OFFS0,bd_SIZEOF	; Border
	STRUCT	prb_bd_OFFS1,bd_SIZEOF
	STRUCT	prb_bd_XY_OFFS0,(5*4)	; Border lines
	STRUCT	prb_bd_XY_OFFS1,(5*4)
	LABEL	prb_SIZEOF

 STRUCTURE TextDisplay,0

	WORD	ty_LeftEdge		; position
	WORD	ty_TopEdge
	WORD	ty_Width		; size
	WORD	ty_Height
	BYTE	ty_FrontPen		; pens
	BYTE	ty_BackPen
	APTR	ty_TextLines		; pointer to a pointertable
	STRUCT	ty_bd_OFFS0,bd_SIZEOF	; Border
	STRUCT	ty_bd_OFFS1,bd_SIZEOF
	STRUCT	ty_bd_XY_OFFS0,(5*4)	; Border lines
	STRUCT	ty_bd_XY_OFFS1,(5*4)
	LABEL	ty_SIZEOF


NewButton:	; Create a new Button (font sensitive size)

		; A0=Window/A1=Button Text/D0&D1=Position in Window
		; -> A0=Button

		movem.l	d0-d7/a1-a6,-(sp)

		movem.l	d0-d1/a0-a1,-(sp)

		move.l	#bt_SIZEOF,d0
		move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
		move.l	4.w,a6
		jsr	_LVOAllocMem(a6)
		move.l	d0,a2			; memory
		lea	_NewButton(pc),a0
		move.l	d0,(a0)
		movem.l	(sp)+,d0-d1/a0-a1	; pos/window/text

		move.w	d0,bt_gg_OFFS+gg_LeftEdge(a2)
		move.w	d1,bt_gg_OFFS+gg_TopEdge(a2)
		move.w	#GFLG_GADGHIMAGE,bt_gg_OFFS+gg_Flags(a2)
		move.w	#GACT_RELVERIFY,bt_gg_OFFS+gg_Activation(a2)
		move.w	#GTYP_BOOLGADGET,bt_gg_OFFS+gg_GadgetType(a2)
		lea	bt_bd_OFFS0(a2),a3
		move.l	a3,bt_gg_OFFS+gg_GadgetRender(a2)
		lea	bt_bd_OFFS0c(a2),a3
		move.l	a3,bt_gg_OFFS+gg_SelectRender(a2)
		lea	bt_it_OFFS(a2),a3
		move.l	a3,bt_gg_OFFS+gg_GadgetText(a2)

		move.w	_ButtonID(pc),bt_gg_OFFS+gg_GadgetID(a2)

		move.b	#1,bt_bd_OFFS0+bd_FrontPen(a2)
		move.b	#RP_JAM1,bt_bd_OFFS0+bd_DrawMode(a2)
		move.b	#5,bt_bd_OFFS0+bd_Count(a2)
		lea	bt_bd_XY_OFFS0(a2),a3
		move.l	a3,bt_bd_OFFS0+bd_XY(a2)
		lea	bt_bd_OFFS1(a2),a3
		move.l	a3,bt_bd_OFFS0+bd_NextBorder(a2)

		move.b	#2,bt_bd_OFFS1+bd_FrontPen(a2)
		move.b	#RP_JAM1,bt_bd_OFFS1+bd_DrawMode(a2)
		move.b	#5,bt_bd_OFFS1+bd_Count(a2)
		lea	bt_bd_XY_OFFS1(a2),a3
		move.l	a3,bt_bd_OFFS1+bd_XY(a2)

		move.b	#2,bt_bd_OFFS0c+bd_FrontPen(a2)
		move.b	#RP_JAM1,bt_bd_OFFS0c+bd_DrawMode(a2)
		move.b	#5,bt_bd_OFFS0c+bd_Count(a2)
		lea	bt_bd_XY_OFFS0(a2),a3
		move.l	a3,bt_bd_OFFS0c+bd_XY(a2)
		lea	bt_bd_OFFS1c(a2),a3
		move.l	a3,bt_bd_OFFS0c+bd_NextBorder(a2)

		move.b	#1,bt_bd_OFFS1c+bd_FrontPen(a2)
		move.b	#RP_JAM1,bt_bd_OFFS1c+bd_DrawMode(a2)
		move.b	#5,bt_bd_OFFS1c+bd_Count(a2)
		lea	bt_bd_XY_OFFS1(a2),a3
		move.l	a3,bt_bd_OFFS1c+bd_XY(a2)

		move.b	#1,bt_it_OFFS+it_FrontPen(a2)
		move.b	#RP_JAM1,bt_it_OFFS+it_DrawMode(a2)
		move.w	#4,bt_it_OFFS+it_LeftEdge(a2)
		move.w	#5,bt_it_OFFS+it_TopEdge(a2)
		lea	TextAttrRP(pc),a3
		move.l	a3,bt_it_OFFS+it_ITextFont(a2)

	; window->wd_BorderRPort->rp_Font = titlefont
	; window->wd_RPort->rp_Font = titlefont

		move.l	a1,bt_it_OFFS+it_IText(a2)

		move.l	a2,-(sp)

		move.l	intbase(pc),a6
		lea	bt_it_OFFS(a2),a0
		jsr	_LVOIntuiTextLength(a6)
		addq.w	#8,d0

		move.l	(sp)+,a2

		move.w	d0,bt_gg_OFFS+gg_Width(a2)
		move.l	textfontrp(pc),a3
		moveq	#9,d1
		add.w	tf_YSize(a3),d1
		move.w	d1,bt_gg_OFFS+gg_Height(a2)

		lea	bt_bd_XY_OFFS0(a2),a0
		lea	bt_bd_XY_OFFS1(a2),a1
		bsr	InitBorderLines

		lea	_ButtonID(pc),a0
		addq.w	#1,(a0)

		move.l	_NewButton(pc),a0

		movem.l	(sp)+,d0-d7/a1-a6
		rts
_NewButton:	dc.l	0
_ButtonID:	dc.w	$4000

RemButton:	; Removes a Button from a Window and from the System
		; A0=Window/A1=Button

		movem.l	d0-d7/a0-a6,-(sp)

		move.l	a1,-(sp)
		move.l	intbase(pc),a6
		jsr	_LVORemoveGadget(a6)
		move.l	(sp)+,a1

		move.l	#bt_SIZEOF,d0
		move.l	4.w,a6
		jsr	_LVOFreeMem(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

OnButton:	; enables a button

		; A0=Button

		movem.l	d0-d7/a0-a6,-(sp)
		lea	bt_gg_OFFS+gg_Flags(a0),a2
		move.w	#GFLG_DISABLED,d0
		not.w	d0
		and.w	(a2),d0
		move.w	d0,(a2)
		movem.l	(sp)+,d0-d7/a0-a6
		rts

OffButton:	; disables a button

		; A0=Button

		movem.l	d0-d7/a0-a6,-(sp)
		lea	bt_gg_OFFS+gg_Flags(a0),a2
		move.w	#GFLG_DISABLED,d0
		or.w	(a2),d0
		move.w	d0,(a2)
		movem.l	(sp)+,d0-d7/a0-a6
		rts

NewProgressBar:	; Create a new ProgressBar

		; D0&D1=Position in Window
		; D2&D3=Size
		; D4=value for 100%
		; -> A0=Progress Bar

		movem.l	d0-d7/a1-a6,-(sp)

		movem.l	d0-d4,-(sp)

		move.l	#prb_SIZEOF,d0
		move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
		move.l	4.w,a6
		jsr	_LVOAllocMem(a6)
		move.l	d0,a2			; memory
		lea	_NewButton(pc),a0
		move.l	d0,(a0)
		movem.l	(sp)+,d0-d4		; pos/window/text

		move.w	d0,prb_LeftEdge(a2)
		move.w	d1,prb_TopEdge(a2)
		move.w	d2,prb_Width(a2)
		move.w	d3,prb_Height(a2)
		move.w	d4,prb_MaxFill(a2)

		move.b	#2,prb_bd_OFFS0+bd_FrontPen(a2)
		move.b	#RP_JAM1,prb_bd_OFFS0+bd_DrawMode(a2)
		move.b	#5,prb_bd_OFFS0+bd_Count(a2)
		lea	prb_bd_XY_OFFS0(a2),a3
		move.l	a3,prb_bd_OFFS0+bd_XY(a2)
		lea	prb_bd_OFFS1(a2),a3
		move.l	a3,prb_bd_OFFS0+bd_NextBorder(a2)

		move.b	#1,prb_bd_OFFS1+bd_FrontPen(a2)
		move.b	#RP_JAM1,prb_bd_OFFS1+bd_DrawMode(a2)
		move.b	#5,prb_bd_OFFS1+bd_Count(a2)
		lea	prb_bd_XY_OFFS1(a2),a3
		move.l	a3,prb_bd_OFFS1+bd_XY(a2)

		move.l	d2,d0
		move.l	d3,d1

		lea	prb_bd_XY_OFFS0(a2),a0
		lea	prb_bd_XY_OFFS1(a2),a1
		bsr	InitBorderLines

		;lea	_ButtonID(pc),a0
		;addq.w	#1,(a0)

		move.l	_NewButton(pc),a0

		movem.l	(sp)+,d0-d7/a1-a6
		rts

RefreshProgressBar:	; draws a progressbar

		; A0=ProgressBar/A1=Window/D0=actual fill

		movem.l	d0-d7/a0-a6,-(sp)

		tst.w	d0
		bpl.b	.ok
		moveq	#0,d0
.ok
		cmp.w	prb_MaxFill(a0),d0
		ble.b	.ok2
		move.w	prb_MaxFill(a0),d0
.ok2

		move.w	d0,prb_Fill(a0)
		move.l	wd_RPort(a1),a1

		movem.l	d0/a0-a1,-(sp)

		move.l	gfxbase(pc),a6

		moveq	#3,d0
		jsr	_LVOSetAPen(a6)

		movem.l	(sp),d0/a0-a1

		move.w	prb_Width(a0),d4
		subq.w	#4,d4
		mulu.w	d0,d4
		divu.w	prb_MaxFill(a0),d4

		move.w	prb_LeftEdge(a0),d0
		move.w	prb_TopEdge(a0),d1
		move.w	d0,d2
		move.w	d1,d3
		add.w	prb_Width(a0),d2
		add.w	prb_Height(a0),d3
		addq.w	#2,d0
		addq.w	#1,d1
		subq.w	#3,d2
		subq.w	#2,d3
		add.w	d0,d4
		cmp.w	d1,d3
		blt.b	.norect2

		movem.l	d0-d4,-(sp)

		move.w	d4,d2
		subq.w	#1,d2
		cmp.w	d0,d2
		blt.b	.norect
		bsr	RectFill
.norect
		moveq	#0,d0
		jsr	_LVOSetAPen(a6)

		movem.l	(sp)+,d0-d4

		move.w	d4,d0
		cmp.w	d0,d2
		blt.b	.norect2
		bsr	RectFill
.norect2
		movem.l	(sp)+,d0/a0-a1

		move.w	prb_LeftEdge(a0),d0
		move.w	prb_TopEdge(a0),d1

		lea	prb_bd_OFFS0(a0),a0
		exg.l	a0,a1
		move.l	intbase(pc),a6
		jsr	_LVODrawBorder(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

RemProgressBar:	; Removes a ProgressBar from the System (the image in the window still exists)
		; A0=ProgressBar

		movem.l	d0-d7/a0-a6,-(sp)

		move.l	a0,a1
		move.l	#prb_SIZEOF,d0
		move.l	4.w,a6
		jsr	_LVOFreeMem(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

NewTextDisplay:	; Create a new ProgressBar

		; D0&D1=Position in Window
		; D2&D3=Size
		; D4=value for 100%
		; -> A0=Progress Bar

		movem.l	d0-d7/a1-a6,-(sp)

		movem.l	d0-d3,-(sp)

		move.l	#ty_SIZEOF,d0
		move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
		move.l	4.w,a6
		jsr	_LVOAllocMem(a6)
		move.l	d0,a2			; memory
		lea	_NewButton(pc),a0
		move.l	d0,(a0)
		movem.l	(sp)+,d0-d3		; pos/window/text

		move.w	d0,ty_LeftEdge(a2)
		move.w	d1,ty_TopEdge(a2)
		move.w	d2,ty_Width(a2)
		move.w	d3,ty_Height(a2)

		move.b	#1,ty_FrontPen(a2)

		move.b	#2,ty_bd_OFFS0+bd_FrontPen(a2)
		move.b	#RP_JAM1,ty_bd_OFFS0+bd_DrawMode(a2)
		move.b	#5,ty_bd_OFFS0+bd_Count(a2)
		lea	ty_bd_XY_OFFS0(a2),a3
		move.l	a3,ty_bd_OFFS0+bd_XY(a2)
		lea	ty_bd_OFFS1(a2),a3
		move.l	a3,ty_bd_OFFS0+bd_NextBorder(a2)

		move.b	#1,ty_bd_OFFS1+bd_FrontPen(a2)
		move.b	#RP_JAM1,ty_bd_OFFS1+bd_DrawMode(a2)
		move.b	#5,ty_bd_OFFS1+bd_Count(a2)
		lea	ty_bd_XY_OFFS1(a2),a3
		move.l	a3,ty_bd_OFFS1+bd_XY(a2)


		move.l	d2,d0
		move.l	d3,d1

		lea	ty_bd_XY_OFFS0(a2),a0
		lea	ty_bd_XY_OFFS1(a2),a1
		bsr	InitBorderLines

		;lea	_ButtonID(pc),a0
		;addq.w	#1,(a0)

		move.l	_NewButton(pc),a0

		movem.l	(sp)+,d0-d7/a1-a6
		rts

RefreshTextDisplay:	; draws a TextDisplay

		; A0=TextDisplay/A1=Window

		movem.l	d0-d7/a0-a6,-(sp)

		move.l	wd_RPort(a1),a1

		movem.l	d0/a0-a1,-(sp)

		move.l	gfxbase(pc),a6

		move.b	ty_BackPen(a0),d0
		jsr	_LVOSetAPen(a6)

		movem.l	(sp),d0/a0-a1

		move.w	ty_LeftEdge(a0),d0
		move.w	ty_TopEdge(a0),d1
		move.w	d0,d2
		move.w	d1,d3
		add.w	ty_Width(a0),d2
		add.w	ty_Height(a0),d3
		addq.w	#2,d0
		addq.w	#1,d1
		subq.w	#3,d2
		subq.w	#2,d3
		add.w	d0,d4
		cmp.w	d1,d3
		blt.b	.norect
		cmp.w	d0,d2
		blt.b	.norect
		bsr	RectFill
.norect
		movem.l	(sp)+,d0/a0-a1

		move.w	ty_LeftEdge(a0),d0
		move.w	ty_TopEdge(a0),d1

		lea	ty_bd_OFFS0(a0),a0
		exg.l	a0,a1
		move.l	intbase(pc),a6
		jsr	_LVODrawBorder(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

RemTextDisplay:	; Removes a ProgressBar from the System (the image in the window still exists)
		; A0=ProgressBar

		movem.l	d0-d7/a0-a6,-(sp)

		move.l	a0,a1
		move.l	#ty_SIZEOF,d0
		move.l	4.w,a6
		jsr	_LVOFreeMem(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts


InitBorderLines:	; Initialises the lines of a border with width/height

		; A0=Border lines 0/A1=Border lines 1/D0=Width/D1=Height

		movem.l	d0-d2/a2-a3,-(sp)

		lea	_BrdrDiffs(pc),a2

		move.l	a0,a3
		moveq	#4,d2
.l0		move.l	(a2)+,(a3)+
		dbra	d2,.l0

		move.l	a1,a3
		moveq	#4,d2
.l1		move.l	(a2)+,(a3)+
		dbra	d2,.l1

		subq.w	#2,d0
		add.w	d0,4(a0)
		add.w	d0,8(a0)
		add.w	d0,12(a0)
		add.w	d0,16(a0)
		add.w	d0,(a1)

		subq.w	#2,d1
		add.w	d1,2(a0)
		add.w	d1,6(a0)
		add.w	d1,18(a0)
		add.w	d1,10(a1)
		add.w	d1,14(a1)

		movem.l	(sp)+,d0-d2/a2-a3
		rts
_BrdrDiffs:
		dc.w	1,1	; 1 pixel diffs for 3d look of the border
		dc.w	1,1
		dc.w	1,0
		dc.w	0,1
		dc.w	0,0

		dc.w	0,0
		dc.w	0,0
		dc.w	0,1
		dc.w	1,0
		dc.w	1,1


DriveMotorOn:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	xx_InputName(pc),a0	;Codetapper added this!
		tst.b	(a0)			;If there is an input file
		bne	.MotorOnDone		;don't turn on the motor

		move.l	4.w,a6
		lea	IORequest(pc),a1
		move.w	#TD_MOTOR,IO_COMMAND(a1)
		move.l	#1,IO_LENGTH(a1)
		jsr	_LVODoIO(a6)
.MotorOnDone		movem.l	(sp)+,d0-d7/a0-a6
		rts

DriveMotorOff:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	xx_InputName(pc),a0	;Codetapper added this!
		tst.b	(a0)			;If there is an input file
		bne	.MotorOffDone		;don't turn on the motor

		move.l	4.w,a6
		lea	IORequest(pc),a1
		move.w	#TD_MOTOR,IO_COMMAND(a1)
		move.l	#0,IO_LENGTH(a1)
		jsr	_LVODoIO(a6)
.MotorOffDone	movem.l	(sp)+,d0-d7/a0-a6
		rts

GetAbsTrackNum:
		movem.l	d1/a0,-(sp)
		move.w	xx_CurrentTrack(pc),d0
		move.l	xx_Disk(pc),a0
		move.w	dsk_Flags(a0),d1
		and.w	#DFLG_SINGLESIDE,d1
		beq.b	.n0
		add.w	d0,d0
		move.w	dsk_Flags(a0),d1
		and.w	#DFLG_ERRORSWAP,d1
		beq.b	.n0
		move.w	xx_Retry(pc),d1
		and.w	#1,d1
		or.w	d1,d0
.n0		move.w	dsk_Flags(a0),d1
		and.w	#DFLG_SWAPSIDES,d1
		beq.b	.n1
		eor.w	#1,d0
.n1		movem.l	(sp)+,d1/a0
		rts

DriveRawReadDisplay:
		bsr.b	GetAbsTrackNum

		;bsr.b	DriveRawRead
		;rts
DriveRawRead:
		; D0.w=track
		; => D0.l=errorcode

		movem.l	d1-d7/a0-a6,-(sp)
		move.l	4.w,a6

		lea	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),d1
		add.l	xx_RawLength(pc),d1
		subq.l	#4,d1
		move.l	d1,(a0)
		lea	xx_RawBit(pc),a0
		clr.l	(a0)			; set invalid bitpointer

		lea	IORequest(pc),a1
		move.w	#TD_CHANGESTATE,IO_COMMAND(a1)
		move.w	d0,-(sp)
		jsr	_LVODoIO(a6)
		moveq	#0,d0
		move.w	(sp)+,d0
		lea	IORequest(pc),a1
		tst.l	IO_ACTUAL(a1)
		bne.b	.nodisk

		;lea	IORequest(pc),a1
		move.l	xx_RawBuffer(pc),IO_DATA(a1)
		move.l	xx_RawLength(pc),IO_LENGTH(a1)
		move.l	d0,IO_OFFSET(a1)
		move.w	#TD_RAWREAD,IO_COMMAND(a1)
		move.b	#IOTDB_INDEXSYNC,IO_FLAGS(a1)
		clr.b	IO_ERROR(a1)
		jsr	_LVODoIO(a6)
		lea	IORequest(pc),a1
		move.b	IO_ERROR(a1),d0
		bne.b	.nomfm

		move.l	xx_RawBuffer(pc),a0
		move.l	xx_MFMBuffer(pc),a1
		move.l	xx_RawLength(pc),d0
		lsr.l	#4,d0
.l0		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		subq.l	#1,d0
		bne.b	.l0

		lea	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),(a0)
		lea	xx_RawBit(pc),a0
		clr.l	(a0)			; initialize bitpointer

		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.nomfm
.nodisk		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_NODISK,d0
		rts

DriveReadDisplay:
		bsr	GetAbsTrackNum
		;bsr.b	DriveRead
		;rts
DriveRead:
		; D0.w=track
		; => D0.l=errorcode

		movem.l	d1-d7/a0-a6,-(sp)

		move.l	4.w,a6

		lea	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),d1
		add.l	xx_RawLength(pc),d1
		subq.l	#4,d1
		move.l	d1,(a0)
		lea	xx_RawBit(pc),a0
		clr.l	(a0)			; set invalid bitpointer

		lea	IORequest(pc),a1
		move.w	#TD_CHANGESTATE,IO_COMMAND(a1)
		move.w	d0,-(sp)
		jsr	_LVODoIO(a6)
		moveq	#0,d0
		move.w	(sp)+,d0
		lea	IORequest(pc),a1
		tst.l	IO_ACTUAL(a1)
		bne.b	.nodisk

		;lea	IORequest(pc),a1
		move.l	xx_RawBuffer(pc),IO_DATA(a1)
		move.l	xx_CurrentTLen(pc),IO_LENGTH(a1)
		mulu.w	#$1600,d0
		move.l	d0,IO_OFFSET(a1)
		move.w	#CMD_READ,IO_COMMAND(a1)
		clr.b	IO_ERROR(a1)
		jsr	_LVODoIO(a6)
		lea	IORequest(pc),a1
		move.b	IO_ERROR(a1),d0
		bne.b	.parse

		move.l	xx_RawBuffer(pc),a0
		move.l	xx_Track(pc),a1
		move.l	xx_CurrentTLen(pc),d0
		lsr.l	#4,d0
.l0		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		move.l	(a0)+,(a1)+
		subq.l	#1,d0
		bne.b	.l0

		lea	xx_RawPointer(pc),a0
		move.l	xx_RawBuffer(pc),(a0)
		lea	xx_RawBit(pc),a0
		clr.l	(a0)			; initialize bitpointer

		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_OK,d0
		rts
.nodisk		movem.l	(sp)+,d1-d7/a0-a6
		moveq	#IERR_NODISK,d0
		rts
.parse
		cmp.b	#TDERR_NoSecHdr,d0
		beq.b	.ns
		cmp.b	#TDERR_BadSecPreamble,d0
		beq.b	.ns
		cmp.b	#TDERR_BadSecID,d0
		beq.b	.ns
		cmp.b	#TDERR_TooFewSecs,d0
		beq.b	.ns
		cmp.b	#TDERR_BadSecHdr,d0
		beq.b	.ns
		cmp.b	#TDERR_SeekError,d0
		bne.b	.s0
.ns		moveq	#IERR_NOSECTOR,d0
		bra.b	.end
.s0
		cmp.b	#TDERR_BadHdrSum,d0
		beq.b	.cs
		cmp.b	#TDERR_BadSecSum,d0
		bne.b	.s1
.cs		moveq	#IERR_CHECKSUM,d0
		bra.b	.end
.s1
		cmp.b	#TDERR_DiskChanged,d0
		bne.b	.s2
		moveq	#IERR_NODISK,d0
		bra.b	.end
.s2
		cmp.b	#TDERR_NoMem,d0
		bne.b	.s3
		moveq	#IERR_OUTOFMEM,d0
		bra.b	.end
.s3
		moveq	#IERR_UNDEFINED,d0
.end		movem.l	(sp)+,d1-d7/a0-a6
		tst.l	d0
		rts
ResetDrive:
		movem.l	d0-d7/a0-a6,-(sp)

		lea	IORequest(pc),a1
		move.w	#TD_SEEK,IO_COMMAND(a1)
		clr.l	IO_OFFSET(a1)
		clr.b	IO_ERROR(a1)
		move.l	4.w,a6
		jsr	_LVODoIO(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts


ParseToolTypes:
		move.l	xx_ToolTypes(pc),a0
		lea	tt_Source(pc),a1
		bsr	SearchString
		move.l	a0,d0
		beq.b	.n0
		move.l	a1,a0
		lea	sourceNameBuffer,a1
		lea	xx_SourceName(pc),a2
		move.l	a1,(a2)
		bsr	CopyFileName
.n0
		move.l	xx_SourceName(pc),a0
		bsr	ParseSource

		lea	xx_SlaveName(pc),a0
		clr.l	(a0)
		move.l	xx_ToolTypes(pc),a0
		lea	tt_Slave(pc),a1
		bsr	SearchString
		move.l	a0,d0
		beq.b	.n1
		move.l	a1,a0
		lea	slaveNameBuffer,a1
		lea	xx_SlaveName(pc),a2
		move.l	a1,(a2)
		bsr	CopyFileName
.n1
		move.l	xx_ToolTypes(pc),a0
		lea	tt_Retries(pc),a1
		bsr	SearchString
		move.l	a0,d0
		beq.b	.n2
		move.l	a1,a0
		bsr	GetNumber
		lea	xx_Retries(pc),a0
		move.w	d0,(a0)
.n2
		move.l	xx_ToolTypes(pc),a0
		lea	tt_Ignore(pc),a1
		bsr	SearchString
		move.l	a0,d0
		sne	d0
		ext.w	d0
		lea	xx_Ignore(pc),a0
		move.w	d0,(a0)		; = 0 when not set

		move.l	xx_ToolTypes(pc),a0
		lea	tt_Debug(pc),a1
		bsr	SearchString
		move.l	a0,d0
		sne	d0
		ext.w	d0
		lea	xx_Debug(pc),a0
		move.w	d0,(a0)		; = 0 when not set

.n3
		move.l	xx_ToolTypes(pc),a0	;Check INPUT tooltype to read from a mfmwarp/wwarp/adf file
		lea	tt_Input(pc),a1
		bsr	SearchString
		move.l	a0,d0
		beq.b	.n4
		move.l	a1,a0
		lea	inputNameBuffer,a1
		lea	xx_InputName(pc),a2
		move.l	a1,(a2)
		bsr	CopyFileName

.n4		rts

ParseSource:
		movem.l	d0-d7/a0-a6,-(sp)

		lea	xx_Unit(pc),a1
		move.w	#-1,(a1)

		move.b	(a0)+,d0
		and.b	#$df,d0
		cmp.b	#"D",d0
		bne.b	.exit
		move.b	(a0)+,d0
		and.b	#$df,d0
		cmp.b	#"F",d0
		bne.b	.exit
		move.b	(a0)+,d1
		cmp.b	#"0",d1
		blo.b	.exit
		cmp.b	#"3",d1
		bhi.b	.exit
		move.b	(a0)+,d0
		cmp.b	#":",d0
		bne.b	.exit
		move.b	(a0)+,d0
		bne.b	.exit
		and.w	#$000f,d1
		move.w	d1,(a1)
.exit
		movem.l	(sp)+,d0-d7/a0-a6
		rts

tt_Source:	dc.b	"SOURCE=",0
tt_Slave:	dc.b	"SLAVE=",0
tt_Retries:	dc.b	"RETRIES=",0
tt_Input:	dc.b	"INPUT=",0
tt_Debug:	dc.b	"DEBUG",0
tt_Ignore:	dc.b	"IGNOREERRORS",0
		cnop	0,2


OpenDebug:

		movem.l	d0-d7/a0-a6,-(sp)

		move.w	xx_Debug(pc),d0		;If DEBUG tooltype set, open
		bne	.debugon		;the debug file

		move.l	xx_SlvStruct(pc),a0
		move.b	slv_Flags(a0),d0
		and.b	#SFLG_DEBUG,d0
		beq.b	.s0

.debugon	move.l	xx_DebugCRC(pc),d1
		bne.b	.s0

		move.l	dosbase(pc),a6
		lea	name_crc(pc),a0
		move.l	a0,d1
		move.l	#MODE_NEWFILE,d2
		jsr	_LVOOpen(a6)
		lea	xx_DebugCRC(pc),a0
		move.l	d0,(a0)
		move.l	d0,d1
		beq.b	.s0

		lea	text_crc(pc),a0
		move.l	a0,d2
		moveq	#text_crc_x-text_crc,d3
		jsr	_LVOWrite(a6)
.s0
		movem.l	(sp)+,d0-d7/a0-a6
		rts
CloseDebug:
		movem.l	d0-d7/a0-a6,-(sp)
		move.l	dosbase(pc),a6
		move.l	xx_DebugCRC(pc),d1
		beq.b	.s0
		jsr	_LVOClose(a6)
		lea	xx_DebugCRC(pc),a0
		clr.l	(a0)
.s0		movem.l	(sp)+,d0-d7/a0-a6
		rts
OutputDebug:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	xx_DebugCRC(pc),d1
		beq.b	.s0

		move.l	d0,-(sp)

		bsr	_GetDiskName
		lea	stringBuffer,a1
		move.l	a1,a2
		lea	_DIname(pc),a0
		bsr	CopyCString
		lea	text_track(pc),a0
		bsr	CopyCString
		move.w	xx_CurrentTrack(pc),d0
		bsr	ConvertWord2Asc
		lea	text_c(pc),a0
		bsr	CopyCString
		bsr	_TrackCRC16
		bsr	ConvertWord2Hex
		lea	text_e(pc),a0
		bsr	CopyCString
		move.l	(sp)+,d0
		bpl.b	.s1
		neg.l	d0
		move.b	#"-",(a1)+
.s1		bsr	ConvertWord2Asc
		move.b	#10,(a1)+
		sub.l	a2,a1
		move.l	dosbase(pc),a6
		move.l	xx_DebugCRC(pc),d1
		move.l	a2,d2
		move.l	a1,d3
		jsr	_LVOWrite(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		moveq	#IERR_OK,d0		; destroy tle_Decoder errorcode
		rts
.s0		movem.l	(sp)+,d0-d7/a0-a6
		rts
OutputDebug2:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	xx_DebugCRC(pc),d1
		beq.b	.s0

		move.l	dosbase(pc),a6
		move.l	xx_DebugCRC(pc),d1
		lea	name_crc(pc),a0
		move.l	a0,d2
		moveq	#1,d3
		jsr	_LVOWrite(a6)
.s0
		movem.l	(sp)+,d0-d7/a0-a6
		rts

xx_DebugCRC:	dc.l	0	; filehandle of ".RawDIC_CRC16"

name_crc:	dc.b	".RawDIC_Debug",0
text_crc:	dc.b	"RawDIC CRC16 Debug:",10,10
text_crc_x:
text_track:	dc.b	" Track.",0
text_c:		dc.b	" CRC16: $",0
text_e:		dc.b	" Error: ",0

txt2_noslv:	dc.b	"Could not open slave!",10,0
txt2_nowin:	dc.b	"Could not open Window!",10,0
txt2_noport:	dc.b	"Could not open Message Port!",10,0
txt2_nodev:	dc.b	"Could not open trackdisk.device!",10,0
rawdicInfo:	sprintx	"RawDIC V%ld.%ld ",Version,Revision
		INCBIN	"T:date"
		dc.b	" © 1999 by John Selck, © 2002-2004 by Codetapper/Wepl",10,10
		dc.b	"Usage: RawDIC SLAVE={slave} [RETRIES={retries}] [SOURCE={source}] [INPUT={inputfile}] [IGNOREERRORS] [DEBUG]",10,10,0
		cnop	0,2
WriteStdOut:
		movem.l	d0-d7/a0-a6,-(sp)

		move.l	output(pc),d1
		move.l	a0,d2
.l0		tst.b	(a0)+
		bne.b	.l0
		move.l	a0,d3
		sub.l	d2,d3
		subq.l	#1,d3
		move.l	dosbase(pc),a6
		jsr	_LVOWrite(a6)

		movem.l	(sp)+,d0-d7/a0-a6
		rts

ParseErrorRequest:
		move.l	a0,-(sp)
		cmp.b	#IERR_CHECKSUM,d0
		bne.b	.n0
		lea	Txt1_CE(pc),a0
		bra	.do
.n0		cmp.b	#IERR_NOSYNC,d0
		bne.b	.n1
		lea	Txt1_SE(pc),a0
		bra	.do
.n1		cmp.b	#IERR_NOSECTOR,d0
		bne.b	.n2
		lea	Txt1_SM(pc),a0
		bra	.do
.n2		cmp.b	#IERR_INP_OPEN,d0
		bne.b	.n3
		lea	Txt1_INP_OPEN(pc),a0
		bra	.do
.n3		cmp.b	#IERR_INP_SEEK,d0
		bne.b	.n4
		lea	Txt1_INP_SEEK(pc),a0
		bra	.do
.n4		cmp.b	#IERR_INP_READ,d0
		bne.b	.n5
		lea	Txt1_INP_READ(pc),a0
		bra	.do
.n5		cmp.b	#IERR_INP_NOTRK,d0
		bne.b	.n6
		lea	Txt1_INP_NOTRK(pc),a0
		bra	.do
.n6		cmp.b	#IERR_INP_BADHD,d0
		bne.b	.n7
		lea	Txt1_INP_BADHD(pc),a0
		bra	.do
.n7		cmp.b	#IERR_XPK_DEPACK,d0
		bne.b	.n8
		lea	Txt1_XPK_DEPACK(pc),a0
		bra	.do
.n8		cmp.b	#IERR_MC1_DEPACK,d0
		bne.b	.n9
		lea	Txt1_MC1_DEPACK(pc),a0
		bra	.do
.n9		cmp.b	#IERR_DIP_DEPACK,d0
		bne.b	.n10
		lea	Txt1_DIP_DEPACK(pc),a0
		bra	.do
.n10		cmp.b	#IERR_MFM_CSUM,d0
		bne.b	.n11
		lea	Txt1_MFM_CSUM(pc),a0
		bra	.do
.n11		cmp.b	#IERR_INP_ILLEN,d0
		bne.b	.n12
		lea	Txt1_INP_ILLEN(pc),a0
		bra	.do
.n12		cmp.b	#IERR_FORMAT_UNS,d0
		bne.b	.n13
		lea	Txt1_FORMAT_UNS(pc),a0
		bra	.do
.n13		cmp.b	#IERR_INP_INCOMP,d0
		bne.b	.n14
		lea	Txt1_INP_INCOMP(pc),a0
		bra	.do
.n14		cmp.b	#IERR_WWP_UNS,d0
		bne.b	.n15
		lea	Txt1_WWP_UNS(pc),a0
		bra	.do
.n15		cmp.b	#IERR_TABL_DATA,d0
		bne.b	.n16
		lea	Txt1_TABL_DATA(pc),a0
		bra	.do
.n16		cmp.b	#IERR_TABL_UNS,d0
		bne.b	.n17
		lea	Txt1_TABL_UNS(pc),a0
		bra	.do
.n17		cmp.b	#IERR_TRCK_UNS,d0
		bne.b	.n18
		lea	Txt1_TRCK_UNS(pc),a0
		bra	.do
.n18		cmp.b	#IERR_TRCK_TYPE,d0
		bne.b	.n19
		lea	Txt1_TRCK_TYPE(pc),a0
		bra	.do
.n19		cmp.b	#IERR_TRCK_FLAG,d0
		bne.b	.s0
		lea	Txt1_TRCK_FLAG(pc),a0
;		bra	.do
.do		bsr.b	ErrorRequest
.s0		move.l	(sp)+,a0
		rts

ErrorRequest:	movem.l	d1-d7/a0-a6,-(sp)
		lea	xx_Cancel(pc),a1
		clr.w	(a1)
		move.l	d0,-(sp)
		move.w	xx_CurrentTrack(pc),d0
		bsr	Txt1_DoNumPoint
		clr.b	(a1)
		lea	Txt1Buffer(pc),a0
		lea	req_Int2(pc),a1
		move.l	a0,it_IText(a1)
		lea	req_Int0(pc),a2
		lea	req_Int1(pc),a3
		lea	req_Txt1(pc),a4
		move.w	xx_Ignore(pc),d0
		beq.b	.s6
		lea	req_Txt1i(pc),a4
.s6		move.l	a4,it_IText(a3)
		move.l	winptr(pc),a0
		moveq	#0,d0
		moveq	#0,d1
		moveq	#0,d2
		moveq	#0,d3
		move.l	intbase(pc),a6
		jsr	_LVOAutoRequest(a6)
		move.l	d0,d1
		beq.b	.s0
		addq.l	#4,sp
		moveq	#IMSG_Retry,d0
		bra.b	.s1
.s0		move.w	xx_Ignore(pc),d0
		beq.b	.s2
		addq.l	#4,sp
		moveq	#IERR_OK,d0
		bra.b	.s1
.s2		lea	xx_Cancel(pc),a0
		move.w	#1,(a0)
		move.l	(sp)+,d0
.s1		movem.l	(sp)+,d1-d7/a0-a6
		rts

req_Int0:	dc.b	0
		dc.b	0
		dc.b	RP_JAM1
		dc.b	0
		dc.w	0	; leftedge
		dc.w	0	; topedge
		dc.l	0	; font
		dc.l	req_Txt0
		dc.l	0

req_Int1:	dc.b	0
		dc.b	0
		dc.b	RP_JAM1
		dc.b	0
		dc.w	0	; leftedge
		dc.w	0	; topedge
		dc.l	0	; font
		dc.l	req_Txt1
		dc.l	0

req_Int2:	dc.b	0
		dc.b	0
		dc.b	RP_JAM1
		dc.b	0
		dc.w	0	; leftedge
		dc.w	4	; topedge
		dc.l	0	; font
		dc.l	0
		dc.l	0

req_Txt0:	dc.b	"Retry",0
req_Txt1:	dc.b	"Cancel",0
req_Txt1i:	dc.b	"Ignore",0
		cnop	0,2

RectFill:	; replacement for _LVORectFill

		; A1=RastPort
		; D0.w=x1
		; D1.w=y1
		; D2.w=x2
		; D3.w=y2

		movem.l	d0-d7/a0-a6,-(sp)
		tst.w	d0
		bmi.b	.norect
		tst.w	d1
		bmi.b	.norect
.l0		cmp.w	d0,d2
		blt.b	.norect
		cmp.w	d1,d3
		blt.b	.norect
		movem.l	d0-d3/a1,-(sp)
		move.l	gfxbase(pc),a6
		jsr	_LVOMove(a6)
		movem.l	(sp),d0-d3/a1
		move.l	d2,d0
		move.l	gfxbase(pc),a6
		jsr	_LVODraw(a6)
		movem.l	(sp)+,d0-d3/a1
		addq.w	#1,d1
		bra.b	.l0
.norect		movem.l	(sp)+,d0-d7/a0-a6
		rts
