*:***************************************************************************
*: Program file  : ARPINVIW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR INDUSTRIE WEAR, INC.
*: Date          : 11/21/2006
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : TMI - TAREK MOHAMMED IBRAHIM
*: Task Number   : C200704                           ( Ticket T20061009.0004 )
* Note : the functions in the OG will be called from the arpinviw program
*:***************************************************************************


           *--Section of Initializing the variables --*

*--THE COLOR LENGTH
STORE "" TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnCd  = LEN(laItemSeg[lnCount,3])
    lnClrPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

               *--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
                  *--End Section to get the FromZone --*

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF


PRIVATE lnSlct,lcOrder
lnSlct = SELECT()

DIMENSION laPer[3,3]
STORE 0 TO laPer
STORE 0 TO lnDim,lnCount

lnPerCnt = 0
IF !EMPTY(laRpTarget)
  
  SELECT CODES
  lcOrder = ORDER()
  SET ORDER TO CODES
  
  FOR lnK = 1 TO ALEN(laRpTarget,1)
    SEEK 'N'+laRpTarget[1,lnK]
    laPer[lnPerCnt+1,1] = CODES.ccode_no
    laPer[lnPerCnt+1,2] = CODES.cdiscrep 
    SKIP
    laPer[lnPerCnt+1,3] = VAL(CODES.crltd_vlu)
    IF laPer[lnPerCnt+1,3]>0
      lnPerCnt = lnPerCnt + 1
    ENDIF
  ENDFOR
  SET ORDER TO (lcOrder)
  SELECT (lnSlct)
ENDIF


DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*- To disable redisplaying the report from ARPINV.PRG report program
llArPinv = .F.
*!*************************************************************
*! Name      : lfPrtSku
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/11/2001
*! Purpose   : To get the sku # of the style/color if exist.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtSku()
*!*************************************************************
*T20061120.0009 TMI [Start] 
FUNCTION lfPrtSku

lcStrToPrn = ALLT(MLINE(INVLINE.NOTE_MEM,1))
llPrtSku = !EMPTY(lcStrToPrn)
RETURN llPrtSku

*--End of lfPrtSku.

*:**************************************************************************
*:* Name        : lfResetLn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/30/2006
*:* Purpose     : reset the variable llPrtSku to .F.
*:***************************************************************************
*:* Called from : arpinvir.frx
*:***************************************************************************
FUNCTION lfResetLn
llPrtSku = .F.
RETURN ''
*-- end of lfResetLn.