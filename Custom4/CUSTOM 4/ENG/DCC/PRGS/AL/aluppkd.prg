*:*********************************************************************************
*: Program file  : ALUPPKD.PRG
*: Program desc. : UpDate Checked by and Picked by
*:        System : Aria4 XP.
*:        Module : AL(Allocation).
*:     Developer : Mariam Mazhar (MMT)
*:     Entry     : C201335==>A27,C201334==>A40[T20110401.0003]
*:*********************************************************************************
*: Modifications:
*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[T20110401.0003]
*:*********************************************************************************
lcExpr = gfOpGrid('ALUPPICK' , .T.)
*:**************************************************************************
*:* Name        : lfrefrsh
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Refresh grid
*:***************************************************************************
FUNCTION lfrefrsh
=loOGScroll.RefreshScroll()
*:**************************************************************************
*:* Name        : lfvPktk
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Validate Scanned PIKTKT#
*:***************************************************************************
FUNCTION lfvPktk

lnPosPKTKT = ASCAN(loOGScroll.laogfxflt,"PICKPACK.PIKTKT")
IF lnPosPKTKT> 0 
  lnPosPKTKT= ASUBSCRIPT(loOGScroll.laogfxflt,lnPosPKTKT,1)
  lcPiktkt =IIF(!EMPTY(loOGScroll.laogfxflt[lnPosPKTKT,6]),loOGScroll.laogfxflt[lnPosPKTKT,6],'')
ENDIF
IF EMPTY(lcPiktkt)
  RETURN 
ENDIF

lcPickBy = ''
lcCheckBy  =''

IF lcRpSelect $ 'PI'
  lnPosPick = ASCAN(loOGScroll.laogfxflt,"PICKPACK.CPICKBY")
  IF lnPosPick > 0 
    lnPosPick = ASUBSCRIPT(loOGScroll.laogfxflt,lnPosPick ,1)
    lcPickBy =IIF(!EMPTY(loOGScroll.laogfxflt[lnPosPick ,6]),loOGScroll.laogfxflt[lnPosPick ,6],'')
    IF EMPTY(lcPickBy)
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"You have to select Picked by first")
      loOGScroll.laogfxflt[lnPosPKTKT,6]= SPACE(6)
      RETURN  
    ENDIF 
  ENDIF
ELSE
  lnPosChck = ASCAN(loOGScroll.laogfxflt,"PICKPACK.CHKEDBY")
  IF lnPosChck > 0 
    lnPosChck = ASUBSCRIPT(loOGScroll.laogfxflt,lnPosChck ,1)
    lcCheckBy  =IIF(!EMPTY(loOGScroll.laogfxflt[lnPosChck ,6]),loOGScroll.laogfxflt[lnPosChck ,6],'')
    IF EMPTY(lcCheckBy)
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"You have to select Checked by first")
      loOGScroll.laogfxflt[lnPosPKTKT,6] = SPACE(6)
      RETURN  
    ENDIF 
  ENDIF
ENDIF 

LNSLCT = SELECT()
IF !USED('PICKPACK')
  =gfOpenTable('PICKPACK','PICKPACK')
ENDIF 
IF !USED('PIKTKT')
  =gfOpenTable('PIKTKT','PIKTKT')
ENDIF  
llContinue = .F.

IF !llContinue
  llContinue = gfSEEK(lcPiktkt,'PIKTKT') and PIKTKT.Status <> 'X'
ENDIF


IF !llContinue AND !EMPTY(lcPiktkt)
  llContinue = lfPiktktBr()
ENDIF

IF llContinue
  =gfSeek(lcPiktkt,'PICKPACK')
  
  IF INLIST(ALLTRIM(PICKpack.CPSTATUS),  'Checked', 'Despatched' ,'Cancelled')
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"This pick ticket is "+ALLTRIM(PICKpack.CPSTATUS)+". Cannot proceed")
    loOGScroll.laogfxflt[lnPosPKTKT,6]= SPACE(6)
	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]
	KEYBOARD '{BACKTAB}' 
	*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]
    
    RETURN      
  ENDIF
  
  IF lcRpSelect $ 'P'  
    IF ALLTRIM(PICKPACK.CPICKBY) <> ALLTRIM(lcPickBy)
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"This pick ticket was not issued to you - Please check with the administrator")
      loOGScroll.laogfxflt[lnPosPKTKT,6]= SPACE(6)
  	  *: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]
	  KEYBOARD '{BACKTAB}' 
	  *: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]
		      
      RETURN 
    ENDIF
  ENDIF
  lcTimeNow =gfGetTime()
  SELECT PICKPACK
  DO CASE 
    CASE lcRpSelect =   'I' AND ALLTRIM(PICKpack.CPSTATUS) = 'Not Issued'
      =gfReplace("CPICKBY   with lcPickBy")
      =gfReplace("CPSTATUS  with 'Issued'")
      =gfReplace("DPICKBY   with oAriaApplication.systemdate")
      =gfReplace("CPICKTIME with lcTimeNow")
    CASE lcRpSelect =   'P' AND ALLTRIM(PICKpack.CPSTATUS) ='Issued'
      =gfReplace("LCOMPPICK  WITH .T.")
    CASE lcRpSelect =   'C' AND ALLTRIM(PICKpack.CPSTATUS) ='Issued' AND PICKpack.LCOMPPICK 
      =gfReplace("CPSTATUS  with 'Checked'")
      =gfReplace("DCHKBY    with oAriaApplication.systemdate")
      =gfReplace("CCHKBYTIME with lcTimeNow")
      =gfReplace("CHKEDBY  WITH lcCheckBy ")
    OTHERWISE 
      *: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]
      *WAIT WINDOW 'The scanned Piktkt cannot be updated' NOWAIT
      WAIT WINDOW 'This Pick ticket has already been issued' TIMEOUT 3
      *: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[ENd]
  ENDCASE 
  =gfTableUpdate()
  loOGScroll.laogfxflt[lnPosPKTKT,6] = SPACE(6)
ENDIF
*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]
KEYBOARD '{BACKTAB}' 
*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]
*:**************************************************************************
*:* Name        : lfPiktktBr
*! Developer    : Mariam Mazhar
*! Date         : 05/11/2011
*:* Purpose     : Browse PIKTKT 
*:***************************************************************************
FUNCTION  lfPiktktBr
PRIVATE LCBRFIELDS, LCFILE_TTL, LABROW, LCFIELDS, LLRETURN, LCBRFIELDS, LNSLCT, LCFORCond 
LNSLCT = SELECT()
DIMENSION LABROW[ 7]
STORE SPACE(0) TO LABROW
LCFIELDS = 'PikTkt,Order,Store,Date,Account'
LCBRFIELDS = "PikTkt:H='PikTkt'  ,"+"Account:H='Account',"+"Store :H='Store'   ,"+"Order :H='Order#'  ,"+"Date:H='PikDate'    "
LCFILE_TTL = 'Pick Tickets'
SELECT PIKTKT
=gfSeek("")
LOCATE
lcBrCond = "'' FOR PikTkt.Status <> 'X'"
LLRETURN = ARIABROW(lcBrCond,LCFILE_TTL,GNBRFSROW1,GNBRFSCOL1,GNBRFSROW2,GNBRFSCOL2,.F.,.F.,LCFIELDS,'laBrow',.F.,'PikTkt',.F.)
lcPiktkt = laBrow[1]
SELECT (LNSLCT)
RETURN LLRETURN
*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[Start]
FUNCTION lfvFocus
KEYBOARD '{TAB}' 
*: B609855,1 MMT 03/05/2012 Fix bug of wrong cursor position in custom Update Pick ticket program OG[End]