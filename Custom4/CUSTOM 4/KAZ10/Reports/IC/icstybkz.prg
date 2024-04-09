*:***************************************************************************
*: Program file  : ICSTYBKZ
*: Program desc. : Style Booking report for Kazu
*: For Report    : ICSTYBKZ.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : TMI - TAREK MOHAMMED IBRAHIM
*: Date          : 07/05/2007
*: Tracking #    : C200814 - Ticket # (T20070502.0041)
*:***************************************************************************
*
#INCLUDE r:\Aria4xp\reports\ICSTYREP.H
lcTime     =  gfGetTime()

IF !USED(lcTmpStyle)
  =lfCrTemp()
ENDIF

IF llOGFltCh
  CLEAR TYPEAHEAD
  IF !lfCollect()
    RETURN
  ENDIF
ELSE
  IF !USED(lcTmpStyle)
    USE &lcTmpStyle ORDER STYLE       
  ENDIF
ENDIF

SELECT &lcTmpStyle
IF !'"S"+SCALE INTO SCALE'$UPPER(SET('RELATION'))
  SET RELATION TO 'S'+SCALE INTO SCALE ADDI
  LOCATE
ENDIF  
DO gfDispRe WITH EVAL('lcRPFormNa')

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2007
*:* Purpose     : collect data
*:***************************************************************************
FUNCTION lfCollect
PRIVATE lcExp,lcDivFlt,lcGrpFit,lcSeaFlt,lcFabFLt,lcStyFlt,lcPatFlt,lcClrFlt
STORE '' TO lcExp,lcDivFlt,lcGrpFit,lcSeaFlt,lcFabFLt,lcStyFlt,lcPatFlt,lcClrFlt

lcExp = lfBldExpr()

lnTBookQty = 0  
SELECT &lcTmpStyle
ZAP  
SELECT SUMMARY
ZAP
APPEND BLANK

  
SELECT STYLE
=gfSeek('')
SCAN REST &lcExp
  IF INKEY()=27
    WAIT CLEAR
    llOGFltCh = .T.
    RETURN .F.
  ENDIF
  WAIT WINDOW NOWAIT 'Collecting data for style: '+STYLE
  SCATTER MEMVAR MEMO
  m.BookAmt = lpBookAmt(STYLE)
  INSERT INTO &lcTmpStyle FROM MEMVAR
  
    *- Calulate summary data
  DO CASE
  CASE STATUS='A'
      SELECT SUMMARY
      REPLACE lnTotA WITH lnTotA + &lcTmpStyle..TOTORD + &lcTmpStyle..TOTSHP ;
              lnTotAmA WITH lnTotAmA + &lcTmpStyle..BOOKAMT
  CASE STATUS='X'
      SELECT SUMMARY
      REPLACE lnTotX WITH lnTotX + &lcTmpStyle..TOTORD + &lcTmpStyle..TOTSHP ;
              lnTotAmX WITH lnTotAmX + &lcTmpStyle..BOOKAMT
  CASE STATUS='H'
      SELECT SUMMARY
      REPLACE lnTotH WITH lnTotH + &lcTmpStyle..TOTORD + &lcTmpStyle..TOTSHP ;
              lnTotAmH WITH lnTotAmH + &lcTmpStyle..BOOKAMT
  ENDCASE
ENDSCAN

SELECT SUMMARY
REPLACE BOOKQTY WITH lnTBookQty

*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfBldExpr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/08/2007
*:* Purpose     : build filter expression from the option grid filter array
*:***************************************************************************
FUNCTION lfBldExpr
LOCAL lcRet


lcRet = ''
lcStyTmpAls = loOGSCroll.laOgFxFlt[lnStyPos,6]
IF !EMPTY(lcStyTmpAls) .AND. USED(lcStyTmpAls) .AND. RECCOUNT(lcStyTmpAls)>0
  SELECT (lcStyTmpAls)
  lnCnt = 0
  COUNT TO lnCnt
  IF lnCnt>0
    lcStyFlt = 'SEEK(STYLE.CSTYMAJOR,['+lcStyTmpAls+'])'
    lcRet = lcStyFlt + '.AND.'
  ENDIF
ENDIF

lcFabTmpAls = loOGSCroll.laOgFxFlt[lnFabPos,6]
IF !EMPTY(lcFabTmpAls) .AND. USED(lcFabTmpAls) .AND. RECCOUNT(lcFabTmpAls)>0
  SELECT (lcFabTmpAls)
  lnCnt = 0
  COUNT TO lnCnt
  IF lnCnt>0
    lcFabFlt = 'SEEK(STYLE.FABRIC,['+lcFabTmpAls+'])'
    lcRet = lcRet + lcFabFlt + '.AND.'
  ENDIF
ENDIF


IF !EMPTY(loOGSCroll.laOgFxFlt[lnGrpPos,6])
  lcGrpFlt = 'STYLE.CSTYGROUP $ loOGSCroll.laOgFxFlt['+ALLTRIM(STR(lnGrpPos))+',6]'
  lcRet = lcRet + lcGrpFlt + '.AND.'
ENDIF

IF !EMPTY(loOGSCroll.laOgFxFlt[lnSeaPos,6])
  lcSeaFlt = 'STYLE.SEASON $ loOGSCroll.laOgFxFlt['+ALLTRIM(STR(lnSeaPos))+',6]'
  lcRet = lcRet + lcSeaFlt + '.AND.'
ENDIF

IF !EMPTY(loOGSCroll.laOgFxFlt[lnDivPos,6])
  lcDivFlt = 'STYLE.CDIVISION $ loOGSCroll.laOgFxFlt['+ALLTRIM(STR(lnDivPos))+',6]'
  lcRet = lcRet + lcDivFlt + '.AND.'
ENDIF

IF !EMPTY(loOGSCroll.laOgFxFlt[lnPatPos,6])
  lcPatFlt = loOGSCroll.laOgFxFlt[lnPatPos,8]
  lcRet = lcRet + lcPatFlt + '.AND.'
ENDIF

IF !EMPTY(loOGSCroll.laOgFxFlt[lnStaPos,6])
  lcStaPos = loOGSCroll.laOgFxFlt[lnStaPos,8]
  lcRet = lcRet + lcStaPos + '.AND.'
ENDIF

IF !EMPTY(loOGSCroll.laOgFxFlt[lnFClrPos,6])
  lcClrFlt = 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen) $ loOGSCroll.laOgFxFlt['+ALLTRIM(STR(lnFClrPos))+',6]'
  lcRet = lcRet + lcClrFlt + '.AND.'
ENDIF

IF lcRPDomImp = 'D'
  lcRet = lcRet + 'STYLE.MAKE' + '.AND.'
ENDIF

IF lcRPDomImp = 'I'
  lcRet = lcRet + '!STYLE.MAKE' + '.AND.'
ENDIF

IF !EMPTY(lcRet)
  lcRet = 'FOR '+LEFT(lcRet,LEN(lcRet)-5)
ENDIF

RETURN lcRet

*-- end of lfBldExpr.
*:**************************************************************************
*:* Name        : lpBookAmt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2007
*:* Purpose     : calculate booking amount for each style/color
*:***************************************************************************
FUNCTION lpBookAmt

PARAMETERS lcStyle, lcColor

STORE 0 TO lnOBookAmt,lnIBookAmt,lnIInv
SELECT ORDLINE
=gfSEEK(lcStyle)
SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcStyle
  =gfSeek(ORDLINE.CORDTYPE+ORDLINE.ORDER,'ORDHDR')
  IF ORDHDR.Status $ 'OH' 
    lnOBookAmt = lnOBookAmt + (TotQty*Price) 
    lnTBookQty = lnTBookQty + TOTQTY
  ENDIF
ENDSCAN

SELECT INVLINE
=gfSEEK(lcStyle)
SCAN WHILE STYLE+INVOICE+STR(LINENO,6) = lcStyle
  =gfSeek(INVLINE.INVOICE,'INVHDR')
  IF INVHDR.Consol<>'Y'
    lnIInv=(TotQty*Price)
    lnTBookQty = lnTBookQty + TOTQTY
  ELSE
    lcInvoice=INVLINE->Invoice    
    SELECT CONSINVL     
    =gfSEEK(INVLINE.INVOICE)
    SCAN REST WHILE INVOICE+STORE+ORDER+STYLE+STR(LINENO,6) = INVLINE.INVOICE FOR STYLE = lcStyle
      lnIInv = lnIInv + (TotQty*Price)
      lnTBookQty = lnTBookQty + TOTQTY
    ENDSCAN
  ENDIF
  lnIBookAmt = lnIBookAmt + lnIInv
ENDSCAN 

SELECT STYLE
RETURN lnIBookAmt+lnOBookAmt
*-- end of lpBookAmt.

*:**************************************************************************
*:* Name        : lfCrTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2007
*:* Purpose     : Create temp file to collect data
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrTemp()
*:***************************************************************************
FUNCTION lfCrTemp
LOCAL laFileStru,lnI
DIMENSION laFileStru[2,18]

SELECT STYLE
=AFIELDS(laFileStru)
lnLen = ALEN(laFileStru,1)

DIMENSION laFileStru[lnLen+1,18]
laFileStru[lnLen+1,1]= 'BOOKAMT'
laFileStru[lnLen+1,2]= 'N'
laFileStru[lnLen+1,3]= 13
laFileStru[lnLen+1,4]= 2

FOR lnI = 1 TO ALEN(laFileStru,1)-lnLen
  STORE ' ' TO  laFileStru[lnLen+lnI ,7],laFileStru[lnLen+lnI ,8],;
                laFileStru[lnLen+lnI ,9],laFileStru[lnLen+lnI ,10],;
                laFileStru[lnLen+lnI ,11],laFileStru[lnLen+lnI ,12],;
                laFileStru[lnLen+lnI ,13],laFileStru[lnLen+lnI ,14],;
                laFileStru[lnLen+lnI ,15],laFileStru[lnLen+lnI ,16]
  STORE 0 TO    laFileStru[lnLen+lnI ,17] ,laFileStru[lnLen+lnI ,18]
ENDFOR

CREATE TABLE (oAriaApplication.WorkDir+lcTmpStyle) FROM ARRAY laFileStru
INDEX ON STYLE TAG STYLE

CREATE CURSOR SUMMARY (BOOKQTY N(7),lnTotX N(6),lnTotA N(6),lnTotH N(6),lnTotAmX N(10,2),lnTotAmA N(10,2),lnTotAmH N(10,2))
APPEND BLANK

*-- end of lfCrTemp.

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2007
*:* Purpose     : the when funciotn for the report 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfwRepWhen
=gfOpenTable(oAriaApplication.DataDir + 'ORDHDR','ORDHDR','SH')
=gfOpenTable(oAriaApplication.DataDir + 'INVHDR','INVHDR','SH')
=gfOpenTable(oAriaApplication.DataDir + 'ORDLINE','ORDLINES','SH')
=gfOpenTable(oAriaApplication.DataDir + 'INVLINE','INVLINES','SH')
=gfOpenTable(oAriaApplication.DataDir + 'CONSINVL','CONSINVL','SH')
=gfOpenTable(oAriaApplication.DataDir + 'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir + 'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir + 'CODES','CODES','SH')

=gfSeek('S','SCALE')

LOCAL lnResult1

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF !llFirstTim
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC " 
  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  llFirstTim = .T.
ENDIF 

IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF 

*- Get the position of each filter in the array loOgScroll.laOgFxFlt
lnStyPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.CSTYMAJOR')
lnStyPos = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnStyPos,1)

lnFabPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.FABRIC')
lnFabPos = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnFabPos,1)

lnGrpPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.CSTYGROUP')
lnGrpPos  = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnGrpPos,1)

lnSeaPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.SEASON')
lnSeaPos  = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnSeaPos,1)

lnDivPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.CDIVISION')
lnDivPos  = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnDivPos,1)

lnPatPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.PATTERN')
lnPatPos  = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnPatPos,1)

lnStaPos = ASCAN(loOGSCroll.laOgFxFlt,'STYLE.STATUS')
lnStaPos = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnStaPos,1)

lnFClrPos = ASCAN(loOGSCroll.laOgFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')
lnFClrPos = ASUBSCRIPT(loOGSCroll.laOgFxFlt,lnFClrPos,1)

*-- end of lfwRepWhen.



*:**************************************************************************
*:* Name        : lfClearRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/05/2007
*:* Purpose     : clear report
*:***************************************************************************
FUNCTION lfClearRep

IF USED(lcTmpStyle)
  USE IN &lcTmpStyle
ENDIF
ERASE (oAriaApplication.WorkDir+lcTmpStyle+'.DBF')
ERASE (oAriaApplication.WorkDir+lcTmpStyle+'.CDX')
ERASE (oAriaApplication.WorkDir+lcTmpStyle+'.FPT')
*-- end of lfClearRep.


*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/10/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

*--Retrive the value of some Setups
DIMENSION laSetUp[5,2]
laSetUp[1,1] = 'M_WAREHOUS' &&
laSetUp[2,1] = 'M_WARELOC'  && 
laSetUp[3,1] = 'M_DYELOT'   &&
laSetUp[4,1] = 'M_COST_MET' && 
laSetUp[5,1] = 'M_STYCNFG' && 

=gfGetMemVar(@laSetUp)

llMultiWH = ALLTRIM(laSetUp[1,2]) = 'Y'
llTrakLoc = ALLTRIM(laSetUp[2,2]) = 'Y'
llDyelot  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcCstMeth = ALLTRIM(laSetUp[4,2])
llConfig  = ALLTRIM(UPPER(laSetUp[5,1])) = 'Y'

llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')

RETURN ''

*!*************************************************************
*! Name      : lfsrSty
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/11/2005
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style   IN  0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE 
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfPvRun
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : change color code in filter array
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfPvRun()
*!*************************************************************
FUNCTION lfPvRun
*--lcsty1 var to get exp
*--lcsty2 var to get colors and concatenate it to lcsty1
PRIVATE lcSty1,lcSty2
STORE 0 TO lcSty1,lcSty2

*-- get color length
DECLARE laItemSeg[1]
STORE 0 TO lncolorLen
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lncolorLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*-- get color from array and change it
*-- get color position
lnClrSgPo = ASCAN(loOGScroll.laOGFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)',1,ALEN(loOGScroll.laOGFxFlt,1),1,10)
IF lnClrSgPo = 0
  lcsty1 = ""
  RETURN .T.  
ENDIF 

*-- Get first color
lcsty1 = SUBSTR(loOGScroll.laOGFxFlt[lnClrSgPo,6],1,lnColorLen)

*-- loop for No. of Occurance of Separator "|" in Color exp. and add 1 to last color
FOR lnCounter = 1 TO OCCUR("|",loOGScroll.laOGFxFlt[lnClrSgPo,6])+1

  *--get from second color to rest color
  IF lnCounter > 1
    *-- get  position of "|"
        lnFirstPos  = ATC('|',loOGScroll.laOGFxFlt[lnClrSgPo,6],lnCounter-1)
    *-- we add one to positon to substr after "|"
        lcSty2      = SUBSTR(loOGScroll.laOGFxFlt[lnClrSgPo,6],lnFirstPos+1,lnColorLen)
  ENDIF

  IF !EMPTY(lcSty2)
    *--Concatenate expression
    lcSty1 = lcsty1 + '|' + lcSty2
  ELSE

    *--for chose first color only
    lcSty1 = lcsty1
  ENDIF
ENDFOR

loOGScroll.laOGFxFlt[lnClrSgPo,6] = lcSty1

*-- end of lfPvRun

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : To get major segment Picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic


*!*************************************************************
*! Name      : lfSumFab1
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 09/21/2004
*! Purpose   : sum a specific field for the current fabric 
*!                  in fabric file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************
FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias

lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE 
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
SELECT(lnAlias)
RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfStySum
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 01/16/2005
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)

lnTotcomp = 0
SELECT Style_X
SET ORDER TO Style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF 

SELECT Style
GO lnStyRec

DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

RETURN INT(lnTotcomp)

*-- end of lfStySum.

*!*************************************************************
*! Name      : lfNonMjDes
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 02/09/2005
*! Purpose   : Evaluate Non Major Code and Description
*!*************************************************************
*! Called from : lfStyHeadr()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNonMjDes()
*!*************************************************************
FUNCTION lfNonMjDes
LOCAL lnI , lcTemp

lcTemp = ''
lnI = 0


*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lcTemp = ''
  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSeg[lnI,1] $ "FOTQ"
      IF SEEK(STR(lnI,1)+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcSegVal)
        lcTemp = ALLTRIM(&lcSegVal..cISgValSd)
      ENDIF
    *-- Season, Color, Division, or Style group Segment.
    CASE laMajSeg[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSeg[lnI,1] = "Z"
          lcCodeExpr = "SEASON"
        CASE laMajSeg[lnI,1] = "C"
          lcCodeExpr = "COLOR"
        CASE laMajSeg[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"
      ENDCASE
      
      lcTemp = ALLTRIM(gfCodDes(SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcCodeExpr,.T.))
      
    *-- Size Seqment case.
    OTHERWISE

      IF SEEK("S"+SUBSTR(STYLE,laMajSeg[lnI,4],LEN(laMajSeg[lnI,3])),lcscal)
        lcTemp = ALLTRIM(&lcscal..cScl_desc)
      ENDIF

  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
ENDFOR    && end Loop Around Non Major elements.
*-- end of lfNonMjDes.
