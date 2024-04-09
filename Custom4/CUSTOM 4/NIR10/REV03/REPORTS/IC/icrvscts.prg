*:***************************************************************************
*: Program file  : icrvscts.PRG
*: Program desc. : Custom Special Cut & Sold For Revue Based on standard
*: TRACK NO      : C201061
*: System        : Aria4XP
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSCUTSO
*:***************************************************************************
*: Modification:
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[T20080422.0042]
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[T20100304.0024]
*:***************************************************************************

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF oAriaApplication.MULTIINST 
  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\IC\ICSCTSRV.FXP ADDITIVE
  DO X:\ARIA4XP\SRVRPTS\IC\ICSCTSRV.FXP  
  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"IC\ICSCTSRV.FXP" WITH .F.,.F.
ENDIF   

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
RETURN 


*--Initialize the variables.

IF llOgFltCh

  STORE 0 TO GRAOTS,GRAORD,GRASTK,GRAWIP
  STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
  =lfChkStrct()
  llExtSize = gfGetMemVar('M_USEEXSSC')
  llDontPrn = .F.
  STORE SPACE(0) TO XPRTWIP,XWIPRPT,;
                    XPRTORD,XORDRPT,XORDRPT,XSTAT

  STORE "" TO lcCustAcnt
  STORE "" TO lcComplete
  STORE "" TO lcStart

  lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
  XBYSIZE  = IIF(llRPBySize,'Y','N')
  XPRTWIP  = lcRPWIPSta
  XWIPSORT = lcRPWIPSor
  XPRTORD  = lcRPSalSta
  XORDSORT = lcRPSALSor
  XSTATHLD = IIF(llRPHolOrd,'Y','N')
  XALLOCAT = lcRPAllQty
  XTITLE   = lcRPTitle
  =lfcreatfilter()
  XSLDATE  = lcGetdate(lcStart,'1')
  XSHDATE  = lcGetdate(lcStart,'2')
  XPLDATE  = lcGetdate(lcComplete,'1')
  XPHDATE  = lcGetdate(lcComplete,'2')
  XFILTER = lcRPExp
  WAIT WINDOW 'Collecting data....' NOWAIT
  FStyle= loOGScroll.gfTempName()
  WORKTEMP = loOGScroll.gfTempName()
  IF !USED('CONTRACTOR')
  USE oAriaApplication.DATADIR +  "APVENDOR.DBF"  AGAIN IN 0 ALIAS 'CONTRACTOR'
    SELECT CONTRACTOR
    SET ORDER TO VENCODE   && CVENDCODE 
  ENDIF

*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
*lcSQLStmtC ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,"
lcSQLStmtC ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,POSLN.CRSESSION,"
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]

lcSQLStmtC =lcSQLStmtC + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
lcSQLStmtC =lcSQLStmtC + " posln.SCALE,posln.TRANCD "
lcSQLStmtC =lcSQLStmtC + " ,MFGOPRHD.cContCode AS VENDOR  From "
lcSQLStmtC =lcSQLStmtC + " posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
lcSQLStmtC =lcSQLStmtC + " LEFT OUTER JOIN MFGOPRHD(INDEX=TktOper) ON MFGOPRHD.cimtyp='M' AND POSLN.PO=MFGOPRHD.CTKTNO AND MFGOPRHD.lInHouse = 0 "
lcSQLStmtc =lcSQLStmtc+" Where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'U' AND POSLN.CINVTYPE='0001' "


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
*lcSQLStmtP ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,"
lcSQLStmtP ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,POSLN.CRSESSION,"
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[END]

lcSQLStmtP =lcSQLStmtP + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
lcSQLStmtP =lcSQLStmtP + " posln.SCALE,posln.TRANCD "
lcSQLStmtP =lcSQLStmtP +" ,POSHDR.VENDOR  From "
lcSQLStmtP =lcSQLStmtP +" posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
lcSQLStmtp =lcSQLStmtp +" where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'P' AND POSLN.CINVTYPE='0001' "
* MAH
*IF XPLDATE<>CTOD('  /  /    ') 
IF !EMPTY(XPLDATE)
* MAH
  *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [Start]
  *lcSQLStmtc1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'" 

  *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
  *lcSQLStmtc1 =" AND poshdr.STATUS not in ('A','S','X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'" 
  lcSQLStmtc1 =" AND poshdr.STATUS not in ('S','X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'" 
  *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]

  *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [End]
  
  lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'" 
ELSE
  *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [Start]
  *lcSQLStmtc1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  "  

  *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
  *lcSQLStmtc1 =" AND poshdr.STATUS not in ('A','S','X', 'C') AND posln.TOTQTY<>0  "  
  lcSQLStmtc1 =" AND poshdr.STATUS not in ('S','X', 'C') AND posln.TOTQTY<>0  "  
  *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]
  
  *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [End]
  
  lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  "  
ENDIF

 XADD = "PosLn.Po<>'*****' "
  DO CASE
    CASE XALLOCAT = 'L'
     * XADD = XADD +' AND  PosLn.Po=CutPick2.CtktNo '
      XADD = XADD +" AND  SEEK('2'+Po+style,'CutPick') "
    CASE XALLOCAT = 'N'
     * XADD = XADD +' AND   PosLn.Po <> CutPick2.CtktNo'
      XADD = XADD +" AND  !SEEK('2'+Po+style,'CutPick') "

  ENDCASE

XADDCUT = "CUTTKTL.Po<>'*****' "
  DO CASE
    CASE XALLOCAT = 'L'
     * XADDCUT = XADDCUT +' AND  CUTTKTL.Po=CutPick2.CtktNo  '
      XADDCUT = XADDCUT +" AND  SEEK('1'+Po+style,'CutPick') "

      
    CASE XALLOCAT = 'N'
     * XADDCUT = XADDCUT +' AND   CUTTKTL.Po<>CutPick2.CtktNo  '
       XADDCUT = XADDCUT +" AND  !SEEK('1'+Po+style,'CutPick') "

  ENDCASE

      
XADDORD = ''
IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
  DO CASE
    CASE XALLOCAT = 'L'
      *XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' +   ' OrdLine.Order = CutPick3.Order) '
      XADDORD = " .AND.  ( SEEK('1'+Order+STR(LineNo,6),'CutPick') .OR.  SEEK('2'+Order+STR(LineNo,6),'CutPick')) "

    CASE XALLOCAT = 'N'
     * XADDORD = ' .AND.  (OrdLine.Order <>CutPick.Order .AND.' +   ' OrdLine.Order <> CutPick3.Order) '
       XADDORD = " .AND.  ( !SEEK('1'+Order+STR(LineNo,6),'CutPick') .AND.  !SEEK('2'+Order+STR(LineNo,6),'CutPick')) "

  ENDCASE
ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules     
  
* MAH  
* IF XSLDATE<>CTOD('  /  /    ')
IF !EMPTY(XSLDATE)
* MAH
   lcLinescan=" BETWEEN(Complete , xSLDate , xSHDate)   .AND. "
*:*B607855,1 12/20/2006 AYM ORDER COMPLETE FILTER DOES NOT AFFECT THE REPORT,T20061219.0009 BEGIN
*!*	   lcLinescan=" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
   lcLinescan=lcLinescan+" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
*:*B607855,1 12/20/2006 AYM ORDER COMPLETE FILTER DOES NOT AFFECT THE REPORT,T20061219.0009 END

   lcLinescan= lcLinescan+ " IIF(lcRPAloct = 'L' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'N' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
   lcLinescan= lcLinescan+  " " + XADDORD +" "+lcCustAcnt

 ELSE
   lcLinescan= " ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O' .AND. "
   lcLinescan= lcLinescan+" IIF(lcRPAloct = 'L' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'N' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
   lcLinescan= lcLinescan+ " " + XADDORD +" "+lcCustAcnt 
ENDIF

SELECT ordline
IF !(UPPER('ORDLINE.ACCOUNT INTO CUSTOMER') $ UPPER(SET('RELATION')))
      SET RELATION TO "M" + ORDLINE.ACCOUNT INTO Customer ADDITIVE
ENDIF  
IF XBYSIZE='Y' 
  lcWorkfile  = loOGScroll.gfTempName()
  TMPSCAL     = loOGScroll.gfTempName()
  LCTMPFILE   = loOGScroll.gfTempName()
  =lfBuildTmp()
  IF lfCreatFiles()
    IF lpCollecData_yes()
    ELSE
      RETURN .F.
    ENDIF
  ELSE
   RETURN .F.
  ENDIF
  WAIT CLEAR
ELSE
  lcTransfile = loOGScroll.gfTempName()
  lcColorfile = loOGScroll.gfTempName()
  LCTMPcolor  = loOGScroll.gfTempName()
  LCTMPtrans  = loOGScroll.gfTempName()
  =lfBuildTmp()
  IF lfCreatFiles()
    IF lfCreatecolors() AND  lpCollecData_no()
    ELSE
      RETURN .f.
    ENDIF
  ELSE
    RETURN .F.
  ENDIF  
  WAIT CLEAR
ENDIF
  
  
  
IF USED(FStyle)
    USE IN (FStyle)
ENDIF
  IF !llDontPrn
   IF XBYSIZE='Y'
     IF USED(LCTMPFILE)
       USE IN (LCTMPFILE)
     ENDIF
   ELSE
     IF USED(LCTMPtrans  )
       USE IN (LCTMPtrans  )
     ENDIF
     IF USED(LCTMPcolor  )
       USE IN (LCTMPcolor  )
     ENDIF
   ENDIF 
  =gfDispRe()
  ENDIF
ELSE  &&FILTERCHANGE
  IF  llDontPrn
    Wait wind 'NO RECORDS SELECTED!!!'
  ELSE
    =gfDispRe()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*-- Check the cost access

DIMENSION laRPPrnItm[14]

llCostAccs = gfUserPriv('IC','ICSTYLE')


*: B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[Start]
*!*	lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
*!*	            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*!*	lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
*!*	            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

IF ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)') > 0
  lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
    ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
ENDIF 

  IF ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)') > 0
 lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
       ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)
ENDIF 
            
*: B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[End]
*-- Disable/enable Only This colors, Free Segment. [begin]

*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[Start]
IF TYPE('lnClrSgPo') <> 'U' AND TYPE('lnFreSgPo')<> 'U'
*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[End]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  OTHERWISE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
ENDCASE

*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[Start]
ENDIF
*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[End]

*-- Disable/enable Only This colors, Free Segment. [end]

=lfvWIPSuDt()
=lfvSalSuDt()

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
gfOpenTable('ordhdr','ordhdr','SH')
gfOpenTable('ORDLINE','ORDLINES','SH')
gfOpenTable('CUSTOMER','CUSTOMER','SH')
gfOpenTable('CUTPICK','cutpick','SH',,,,.T.)
gfOpenTable('STYLE','STYLE','SH')
gfOpenTable('SCALE','SCALE','SH')
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[Start]
IF !USED('CUTTKTL')
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[End]
gfOpenTable('POSLN','POSLN','SH','CUTTKTL',,,.T.)
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[Start]
ENDIF 
IF !USED('POSLN')
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[End]
gfOpenTable('POSLN','POSLN','SH','POSLN',,,.T.)
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[Start]
ENDIF
*: B609189,1 MMT 03/24/2010 Fix bug of Error when user reset to default criteria after preview[End]
gfOpenTable('APVENDOR','VENCODE','SH')
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfvWIPSuDt
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate print WIP Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWIPSuDt()
*!*************************************************************

FUNCTION lfvWIPSuDt
*-Disable Tow Option of WIP in Optoin Grid[Begin]
IF ('MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
  lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LCRPWIPSOR'),1)
  laOGObjCnt[lcWIPSorPo] = lcRPWIPSta = 'D'
  = lfOGShowGet('lcRPWIPSor')
ENDIF


*!*************************************************************
*! Name      : lfvSalSuDt
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : Validate print Sales Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalSuDt()
*!*************************************************************

FUNCTION lfvSalSuDt

lcSalSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LCRPSALSOR'),1)
laOGObjCnt[lcSalSorPo] = lcRPSalSta = 'D'
= lfOGShowGet('lcRPSalSor')



*!*************************************************************
*! Name      : lfvFabric
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric
lcFabObj = VARREAD()

lcFab    = &lcFabObj

llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')

SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  


*!*************************************************************
*! Name      : lfTermnate
*! Developer : Haytham El_Sheltawi
*! Date      : 04/21/1999
*! Purpose   : Function to terminate the report if neither
*!             the "Sales Order" nor the "Point of Sale" modules
*!             is installed.
*!*************************************************************
*! Called from : Default value of the report variable (llTermnate).
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTermnate

*-- If neither the "Sales Order" nor the "Point of Sale" module is installed
IF !('SO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
  *** Message : "Since neither the Sales Order nor the Point of Sale module"
  ***           "is installed, you cannot run this report.                 "
  ***
  *** Buttons : "                         < Ok >                           "
  =gfModalgen("TRM42171B00000","ALERT")
  
  *-- Terminate the Option Grid.
  llOgTrmnat = .T.
ENDIF    && End of IF !('SO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*B802399,1 - WAB - END
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 03/05/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
PRIVATE lcAliasCst

lcAliasCst = SELECT(0)
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/18/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : ALPKLSNK.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.

*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

lcExcStat = SET('EXACT')
SET EXACT ON

IF  XBYSIZE='Y' &&CASE SIZE=YES


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
*DIMENSION laTempStru[61,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]

*MMT22
*DIMENSION laTempStru[62,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]
DIMENSION laTempStru[78,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]
*MMT22

*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[ENd]

STORE '' TO laTempStru,laTempTran,laTempCust,laTempsty,laTemplINE
PRIVATE lnFileCnt , lnFldRow


*-- Fields from Customer File.
SELECT ORDLINE
= OGAFIELDS(@laTempCust)
laTempStru[1,1]  = 'STYLE'
laTempStru[2,1]  = 'PO'
laTempStru[3,1]  = 'ACCOUNT'
laTempStru[4,1]  = 'COMPLETE'
*!*  laTempStru[5,1]  = 'QTY1'
*!*  laTempStru[6,1]  = 'QTY2'
*!*  laTempStru[7,1]  = 'QTY3'
*!*  laTempStru[8,1]  = 'QTY4'
*!*  laTempStru[9,1]  = 'QTY5'
*!*  laTempStru[10,1] = 'QTY6'
*!*  laTempStru[11,1] = 'QTY7'
*!*  laTempStru[12,1] = 'QTY8'
*!*  laTempStru[13,1] = 'TOTQTY'
laTempStru[14,1] = 'PRICE'
laTempStru[15,1] = 'ORDER'
laTempStru[16,1] = 'SCALE'
*-- Loop to get other dimensions of Customer included fields (Like master file)
FOR lnFileCnt = 1 TO 16
  lnFldRow = ASCAN(laTempCust,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCust,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCust[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCust[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCust[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Customer included fields (Like master file)
SELECT ORDHDR
=OGAFIELDS(@laTempTran)

laTempStru[17,1] = 'START'
laTempStru[18,1] = 'STATUS'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 17 TO 18
  lnFldRow = ASCAN(laTempTran,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempTran,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempTran[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempTran[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempTran[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

*-- Add Fields from PostDchq File.
*!*  laTempStru[5,1]  = 'QTY1'
*!*  laTempStru[6,1]  = 'QTY2'
*!*  laTempStru[7,1]  = 'QTY3'
*!*  laTempStru[8,1]  = 'QTY4'
*!*  laTempStru[9,1]  = 'QTY5'
*!*  laTempStru[10,1] = 'QTY6'
*!*  laTempStru[11,1] = 'QTY7'
*!*  laTempStru[12,1] = 'QTY8'

laTempStru[5 ,1] = 'QTY1'
laTempStru[5 ,2] = 'N'
laTempStru[5 ,3] = 7
laTempStru[5 ,4] = 0

laTempStru[6 ,1] = 'QTY2'
laTempStru[6 ,2] = 'N'
laTempStru[6 ,3] = 7
laTempStru[6 ,4] = 0

laTempStru[7 ,1] = 'QTY3'
laTempStru[7 ,2] = 'N'
laTempStru[7 ,3] = 7
laTempStru[7 ,4] = 0

laTempStru[8 ,1] = 'QTY4'
laTempStru[8 ,2] = 'N'
laTempStru[8 ,3] = 7
laTempStru[8 ,4] = 0

laTempStru[9 ,1] = 'QTY5'
laTempStru[9 ,2] = 'N'
laTempStru[9 ,3] = 7
laTempStru[9 ,4] = 0

laTempStru[10 ,1] = 'QTY6'
laTempStru[10 ,2] = 'N'
laTempStru[10 ,3] = 7
laTempStru[10 ,4] = 0

laTempStru[11,1] = 'QTY7'
laTempStru[11 ,2] = 'N'
laTempStru[11 ,3] = 7
laTempStru[11 ,4] = 0

laTempStru[12 ,1] = 'QTY8'
laTempStru[12 ,2] = 'N'
laTempStru[12 ,3] = 7
laTempStru[12 ,4] = 0

laTempStru[13 ,1] = 'TOTQTY'
laTempStru[13 ,2] = 'N'
laTempStru[13 ,3] = 8
laTempStru[13 ,4] = 0


laTempStru[19 ,1] = 'VENDOR'
laTempStru[19 ,2] = 'C'
laTempStru[19 ,3] = 8
laTempStru[19 ,4] = 0

laTempStru[20 ,1] = 'CSTYTYPE'
laTempStru[20 ,2] = 'C'
laTempStru[20 ,3] = 1
laTempStru[20 ,4] = 0


laTempStru[21 ,1] = 'TRANCD'
laTempStru[21 ,2] = 'C'
laTempStru[21 ,3] = 1
laTempStru[21 ,4] = 0

laTempStru[22 ,1] = 'VENDNAME'
laTempStru[22 ,2] = 'C'
laTempStru[22 ,3] = 30
laTempStru[22 ,4] = 0

laTempStru[23 ,1] = 'CUSTOMER'
laTempStru[23 ,2] = 'C'
laTempStru[23 ,3] = 30
laTempStru[23 ,4] = 0

laTempStru[24 ,1] = 'CTRANTYPE'
laTempStru[24 ,2] = 'C'
laTempStru[24 ,3] = 1
laTempStru[24 ,4] = 0

laTempStru[25 ,1] = 'COLOR'
laTempStru[25 ,2] = 'C'
laTempStru[25 ,3] = 50
laTempStru[25 ,4] = 0

laTempStru[26 ,1] = 'DESC'
laTempStru[26 ,2] = 'C'
laTempStru[26 ,3] = 50
laTempStru[26 ,4] = 0

laTempStru[27 ,1] = 'TRANSNO'
laTempStru[27 ,2] = 'C'
laTempStru[27 ,3] = 6
laTempStru[27 ,4] = 0

SELECT style
=OGAFIELDS(@laTempsty)

laTempStru[28,1] = 'CDIVISION'
laTempStru[29,1] = 'SEASON'
laTempStru[30,1] = 'CSTYGROUP'
laTempStru[31,1] = 'CSTYMAJOR'

laTempStru[32,1] = 'STK1'
laTempStru[33,1] = 'STK2'
laTempStru[34,1] = 'STK3'
laTempStru[35,1] = 'STK4'
laTempStru[36,1] = 'STK5'
laTempStru[37,1] = 'STK6'
laTempStru[38,1] = 'STK7'
laTempStru[39,1] = 'STK8'
laTempStru[40,1] = 'TOTSTK'

laTempStru[41,1] = 'ORD1'
laTempStru[42,1] = 'ORD2'
laTempStru[43,1] = 'ORD3'
laTempStru[44,1] = 'ORD4'
laTempStru[45,1] = 'ORD5'
laTempStru[46,1] = 'ORD6'
laTempStru[47,1] = 'ORD7'
laTempStru[48,1] = 'ORD8'
laTempStru[49,1] = 'TOTORD'

laTempStru[50,1] = 'WIP1'
laTempStru[51,1] = 'WIP2'
laTempStru[52,1] = 'WIP3'
laTempStru[53,1] = 'WIP4'
laTempStru[54,1] = 'WIP5'
laTempStru[55,1] = 'WIP6'
laTempStru[56,1] = 'WIP7'
laTempStru[57,1] = 'WIP8'
laTempStru[58,1] = 'TOTWIP'


*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 28 TO 58
  lnFldRow = ASCAN(laTempsty,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempsty,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempsty[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempsty[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempsty[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

SELECT ORDLINE
=OGAFIELDS(@laTemplINE)

laTempStru[59,1] = 'STORE'
laTempStru[60,1] = 'LINENO'
laTempStru[61,1] = 'PICKED'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 59 TO 61
  lnFldRow = ASCAN(laTemplINE,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTemplINE,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTemplINE[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTemplINE[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTemplINE[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
laTempStru[62,1] = 'CRSESSION'
laTempStru[62,2] = 'C'
laTempStru[62,3] = 6
laTempStru[62,4] = 0
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]

*MMT22
laTempStru[63,1] = 'ALO1'
laTempStru[63,2] = 'N'
laTempStru[63,3] = 7
laTempStru[63,4] = 0

laTempStru[64,1] = 'ALO2'
laTempStru[64,2] = 'N'
laTempStru[64,3] = 7
laTempStru[64,4] = 0

laTempStru[65,1] = 'ALO3'
laTempStru[65,2] = 'N'
laTempStru[65,3] = 7
laTempStru[65,4] = 0

laTempStru[66,1] = 'ALO4'
laTempStru[66,2] = 'N'
laTempStru[66,3] = 7
laTempStru[66,4] = 0

laTempStru[67,1] = 'ALO5'
laTempStru[67,2] = 'N'
laTempStru[67,3] = 7
laTempStru[67,4] = 0

laTempStru[68,1] = 'ALO6'
laTempStru[68,2] = 'N'
laTempStru[68,3] = 7
laTempStru[68,4] = 0

laTempStru[69,1] = 'ALO7'
laTempStru[69,2] = 'N'
laTempStru[69,3] = 7
laTempStru[69,4] = 0

laTempStru[70,1] = 'ALO8'
laTempStru[70,2] = 'N'
laTempStru[70,3] = 7
laTempStru[70,4] = 0

laTempStru[72,1] = 'UnAlo1'
laTempStru[72,2] = 'N'
laTempStru[72,3] = 7
laTempStru[72,4] = 0

laTempStru[73,1] = 'UnAlo2'
laTempStru[73,2] = 'N'
laTempStru[73,3] = 7
laTempStru[73,4] = 0

laTempStru[74,1] = 'UnAlo3'
laTempStru[74,2] = 'N'
laTempStru[74,3] = 7
laTempStru[74,4] = 0

laTempStru[75,1] = 'UnAlo4'
laTempStru[75,2] = 'N'
laTempStru[75,3] = 7
laTempStru[75,4] = 0

laTempStru[76,1] = 'UnAlo5'
laTempStru[76,2] = 'N'
laTempStru[76,3] = 7
laTempStru[76,4] = 0

laTempStru[77,1] = 'UnAlo6'
laTempStru[77,2] = 'N'
laTempStru[77,3] = 7
laTempStru[77,4] = 0

laTempStru[78,1] = 'UnAlo7'
laTempStru[78,2] = 'N'
laTempStru[78,3] = 7
laTempStru[78,4] = 0

laTempStru[71,1] = 'UnAlo8'
laTempStru[71,2] = 'N'
laTempStru[71,3] = 7
laTempStru[71,4] = 0

*MMT22


gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)

*B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]
SELECT(lcWorkfile)

*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
*INDEX on CTRANTYPE+TRANSNO+STYLE TAG (lcWorkfile) 
INDEX on CTRANTYPE+TRANSNO+STYLE+TRANCD+CRSESSION TAG (lcWorkfile) 
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]

*B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]

ELSE     && CASE SIZE = NO

*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
*DIMENSION laTempStru[44,18] ,laTempStru1[24,18], laTempTran[1,18] , laTempsty[1,18]
DIMENSION laTempStru[44,18] ,laTempStru1[34,18], laTempTran[1,18] , laTempsty[1,18]
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[ENd]

STORE '' TO laTempStru,laTempTran,laTempsty,laTempStru1
PRIVATE lnFileCnt , lnFldRow

*! BUILD STYLE COLORS TEMP -- BEGIN
SELECT style
=OGAFIELDS(@laTempsty)
laTempStru[1 ,1] = 'STYLE'
laTempStru[2 ,1] = 'DESC'
laTempStru[3 ,1] = 'CSTYGROUP'
laTempStru[4 ,1] = 'CSTYMAJOR'
laTempStru[5 ,1] = 'STK1'
laTempStru[6 ,1] = 'STK2'
laTempStru[7 ,1] = 'STK3'
laTempStru[8 ,1] = 'STK4'
laTempStru[9 ,1] = 'STK5'
laTempStru[10 ,1] = 'STK6'
laTempStru[11 ,1] = 'STK7'
laTempStru[12 ,1] = 'STK8'
laTempStru[13 ,1] = 'ORD1'
laTempStru[14 ,1] = 'ORD2'
laTempStru[15 ,1] = 'ORD3'
laTempStru[16 ,1] = 'ORD4'
laTempStru[17 ,1] = 'ORD5'
laTempStru[18 ,1] = 'ORD6'
laTempStru[19 ,1] = 'ORD7'
laTempStru[20 ,1] = 'ORD8'
laTempStru[21 ,1] = 'WIP1'
laTempStru[22 ,1] = 'WIP2'
laTempStru[23 ,1] = 'WIP3'
laTempStru[24 ,1] = 'WIP4'
laTempStru[25 ,1] = 'WIP5'
laTempStru[26 ,1] = 'WIP6'
laTempStru[27 ,1] = 'WIP7'
laTempStru[28 ,1] = 'WIP8'
*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 28
  lnFldRow = ASCAN(laTempsty,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempsty,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempsty[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempsty[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempsty[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

*-- Add Fields from PostDchq File.
lnIndex=29
laTempStru[lnIndex,1] = 'COL1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1  &&30
laTempStru[lnIndex,1] = 'COL2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&31
laTempStru[lnIndex,1] = 'COL3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&32
laTempStru[lnIndex,1] = 'COL4'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&33
laTempStru[lnIndex,1] = 'COL5'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1  
laTempStru[lnIndex,1] = 'COL6'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&34
laTempStru[lnIndex,1] = 'COL7'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&35
laTempStru[lnIndex,1] = 'COL8'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&36
laTempStru[lnIndex,1] = 'COL9'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&37
laTempStru[lnIndex,1] = 'COL10'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&38
laTempStru[lnIndex,1] = 'STK9'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&39
laTempStru[lnIndex,1] = 'STK10'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0


lnIndex=lnIndex+1  &&40
laTempStru[lnIndex,1] = 'ORD9'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&41
laTempStru[lnIndex,1] = 'ORD10'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&42
laTempStru[lnIndex,1] = 'WIP9'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1 &&43+1
laTempStru[lnIndex,1] = 'WIP10'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0


gfCrtTmp(lcColorfile ,@laTempstru,,"",.f.)
*! BUILD STYLE COLORS TEMP -- END

*! BUILD TRASACTIONS COLOR TEMP -- BEGIN
lnIndex=1  &&1
laTempStru1[lnIndex,1] = 'ORDER'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 6
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1  &&2
laTempStru1[lnIndex,1] = 'PO'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 6
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1  &&3
laTempStru1[lnIndex,1] = 'CTRANTYPE'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 1
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1  &&4
laTempStru1[lnIndex,1] = 'TRANSNO'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 6
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL1'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL2'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL3'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL4'
 laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL5'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL6'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL7'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL8'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL9'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1 
laTempStru1[lnIndex,1] = 'QCOL10'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 10
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'TRANCD'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 1
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'VENDOR'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 8
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'VENDNAME'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 30
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'CUSTOMER'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 30
laTempStru1[lnIndex,4] = 0
***
lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'CSTYMAJOR'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 19
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'STORE'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 8
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'LINENO'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 6
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'COMPLETE'
laTempStru1[lnIndex,2] = 'D'
laTempStru1[lnIndex,3] = 0
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'STATUS'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 1
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'ACCOUNT'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 5
laTempStru1[lnIndex,4] = 0


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'CRSESSION'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 6
laTempStru1[lnIndex,4] = 0


lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY1'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY2'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY3'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY4'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY5'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY6'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY7'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'QTY8'
laTempStru1[lnIndex,2] = 'N'
laTempStru1[lnIndex,3] = 7
laTempStru1[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru1[lnIndex,1] = 'STYLE'
laTempStru1[lnIndex,2] = 'C'
laTempStru1[lnIndex,3] = 19
laTempStru1[lnIndex,4] = 0
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]



gfCrtTmp(lcTransfile ,@laTempstru1,,"",.f.)


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
SELECT(lcTransfile)
INDEX on CTRANTYPE+TRANSNO+Style+TRANCD+CRSESSION TAG (lcTransfile) 
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]


*! BUILD TRASACTIONS COLOR TEMP -- END

ENDIF

SET EXACT &lcExcStat  



*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]

IF  XBYSIZE='Y'
DIMENSION loOgScroll.laCRParams[6,2]
ELSE
DIMENSION loOgScroll.laCRParams[10,2]
ENDIF

loOGScroll.cCROrientation='L'
IF  XBYSIZE='Y'
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  TMPSCAL+ ".DBF"
  loOgScroll.lcOGLastForm ='ICSCUTRV'
ELSE
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  LCTMPTRANS+ ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  LCTMPCOLOR+ ".DBF"
  loOgScroll.lcOGLastForm ='ICSCURVB'
ENDIF

loOgScroll.laCRParams[1,1] = 'Layout'
IF XPRTWIP='S'
  loOgScroll.laCRParams[1,2] = 'Summary'
ELSE
  loOgScroll.laCRParams[1,2] = 'Detail'
ENDIF

loOgScroll.laCRParams[2,1] = 'OpTitle'
loOgScroll.laCRParams[2,2] = lcRpTitle

loOgScroll.laCRParams[3,1] = 'Layout2'
IF XPRTORD='S'
loOgScroll.laCRParams[3,2] = 'Summary'
ELSE
loOgScroll.laCRParams[3,2] = 'Detail'
ENDIF

loOgScroll.laCRParams[4,1] = 'ReportName'
IF XWIPRPT .OR. XORDRPT
  DO CASE
    CASE XALLOCAT = 'L'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'N'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - NOT ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'A'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - DETAIL REPORT'
  ENDCASE
ELSE
  DO CASE
    CASE XALLOCAT = 'L'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'N'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - NOT ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'A'
      loOgScroll.laCRParams[4,2]= 'SPECIAL CUT & SOLD - SUMMARY REPORT'
  ENDCASE
ENDIF

loOgScroll.laCRParams[5,1] = 'wipsort'
  DO CASE
    CASE XWIPSORT= 'P'
      loOgScroll.laCRParams[5,2]= 'Po/Ct'
    CASE XWIPSORT= 'F'
      loOgScroll.laCRParams[5,2]= 'Factory'
    CASE XWIPSORT= 'D'
      loOgScroll.laCRParams[5,2]= 'Date'
  ENDCASE
loOgScroll.laCRParams[6,1] = 'ordsort'
  DO CASE
    CASE XORDSORT= 'O'
      loOgScroll.laCRParams[6,2]= 'Order'
    CASE XORDSORT= 'A'
      loOgScroll.laCRParams[6,2]= 'Account'
    CASE XORDSORT= 'D'
      loOgScroll.laCRParams[6,2]= 'Date'
  ENDCASE

IF  !XBYSIZE='Y'
loOgScroll.laCRParams[7,1] = 'GRASTK'
loOgScroll.laCRParams[7,2]= GRASTK

loOgScroll.laCRParams[8,1] = 'GRAWIP'
loOgScroll.laCRParams[8,2]= GRAWIP

loOgScroll.laCRParams[9,1] = 'GRAORD'
loOgScroll.laCRParams[9,2]= GRAORD

loOgScroll.laCRParams[10,1] = 'GRAOTS'
loOgScroll.laCRParams[10,2]= GRAOTS
ENDIF

*!*************************************************************
*! Name      : lfCreatFiles
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Creating files and relation
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCreatFiles
*!*************************************************************
FUNCTION lfCreatFiles

IF XPRTWIP='D'
  XWIPRPT=.T.
ELSE
  XWIPRPT=.F.
ENDIF

IF XPRTORD='D'
  XORDRPT=.T.
ELSE
  XORDRPT=.F.
ENDIF

XSTAT=IIF(XSTATHLD='N','O','HO')

*!*  SELECT style
*!*  SET ORDER TO style
XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'


*: B608130,1 MMT 06/18/2007 Fix bug of error if Select more than 25 Colors[Start]
*Royalty
llUseRol  = .F.
lnRolPos = ASCAN(loOgScroll.laOgFXFlt,"STYLE.ROYALTY")
IF lnRolPos > 0 
  lnRolPos  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnRolPos,1)
  lcRolSel =IIF(!EMPTY(laOgFxFlt[lnRolPos,6]),laOgFxFlt[lnRolPos,6],'')
  IF !EMPTY(lcRolSel) 
    lcRolFile = loOGScroll.gfTempName()
    llUseRol = IIF(LEN(lcRolSel)>0,.T.,.F.) AND lfConvertToCursor(lcRolSel,'ROYALTY',lcRolFile)
    IF llUseRol 
      lnRolStart = AT('INLIST(STYLE.ROYALTY',XFILTER)
      IF lnRolStart  > 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnRolStart))+lnRolStart-1
         lnNumChar = lnEndPos -lnRolStart+1
         XFILTER = STUFF(XFILTER,lnRolStart,lnNumChar,"Seek(STYLE.ROYALTY,'&lcRolFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   

*SEASON
llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.SEASON")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnSeaPos,6]),loOgScroll.laOgVRFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
    IF llUseSeason 
      lnSEAStart = AT('INLIST(STYLE.SEASON',XFILTER)
      IF lnSEAStart > 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnSEAStart))+lnSEAStart-1
         lnNumChar = lnEndPos -lnSEAStart+1
         XFILTER = STUFF(XFILTER,lnSEAStart,lnNumChar,"Seek(STYLE.SEASON,'&lcSeaFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   

*DIVISION
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CDIVISION")
IF lnDivPos > 0 
  lnDivPos = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnDivPos,6]),loOgScroll.laOgVRFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel) 
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
    IF llUseDiv  
      lnDivStart = AT('INLIST(STYLE.CDIVISION',XFILTER)
      IF lnDivStart > 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnDivStart))+lnDivStart-1
         lnNumChar = lnEndPos -lnDivStart+1
         XFILTER = STUFF(XFILTER,lnDivStart,lnNumChar,"Seek(STYLE.CDIVISION,'&lcDivFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   


*Style Group
llUseGrp  = .F.
lnGrpPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0 
  lnGrpPos  = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnGrpPos ,6]),loOgScroll.laOgVRFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel) 
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
    IF llUseGrp 
      lnGrpStart = AT('INLIST(STYLE.CSTYGROUP',XFILTER)
      IF lnGrpStart > 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnGrpStart))+lnGrpStart-1
         lnNumChar = lnEndPos -lnGrpStart+1
         XFILTER = STUFF(XFILTER,lnGrpStart,lnNumChar,"Seek(STYLE.CSTYGROUP,'&lcGrpFile')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   

*Color
llUseClr1  = .F.
lnClr1Pos = ASCAN(loOgScroll.laOgVRFlt,"SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)")
IF lnClr1Pos > 0 
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnClr1Pos ,6]),loOgScroll.laOgVRFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel ) 
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File )
    IF llUseClr1 
      lnClr1Start = AT('INLIST(SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)',XFILTER)
      IF lnClr1Start> 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnClr1Start),2) +lnClr1Start -1
         lnNumChar = lnEndPos - lnClr1Start+1
         XFILTER = STUFF(XFILTER,lnClr1Start,lnNumChar,"Seek(SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen),'&lcClr1File')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF   

*Color2
llUseClr2  = .F.
lnClr2Pos = ASCAN(loOgScroll.laOgVRFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnClr2Pos > 0 
  lnClr2Pos  = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnClr2Pos,1)
  lcClr2Sel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnClr2Pos ,6]),loOgScroll.laOgVRFlt[lnClr2Pos,6],'')
  IF !EMPTY(lcClr2Sel) 
    lcClr2File = loOGScroll.gfTempName()
    llUseClr2= IIF(LEN(lcClr2Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr2Sel,'CSTYCLR2',lcClr2File )
    IF llUseClr2 
      lnClr2Start = AT('INLIST(SUBSTR(STYLE.Style,lnClrPo,lnColorLen)',XFILTER)
      IF lnClr2Start> 0
         lnEndPos = AT(")",SUBSTR(XFILTER,lnClr2Start),2)+lnClr2Start-1
         lnNumChar = lnEndPos -lnClr2Start+1
         XFILTER = STUFF(XFILTER,lnClr2Start,lnNumChar,"Seek(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),'&lcClr2File')")
      ENDIF 
    ENDIF     
  ENDIF   
ENDIF 
*: B608130,1 MMT 06/18/2007 Fix bug of error if Select more than 25 Colors[End]



lcSylefields= "STYLE,SCALE, CDIVISION,SEASON,CSTYGROUP,CSTYMAJOR, DESC , "
lcSylefields= lcSylefields+ " stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8, " 
lcSylefields= lcSylefields+ " ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8, "
lcSylefields= lcSylefields+ " WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8, "
lcSylefields= lcSylefields+ " TOTSTK, TOTORD, TOTWIP "

*MMT22
lcSylefields= lcSylefields+ " ,ALO1,ALO2, ALO3, ALO4, ALO5, ALO6, ALO7, ALO8, TOTALO"
*MMT22

SELECT &lcSylefields. FROM STYLE WHERE STYLE.STYLE <>'********' AND  &XFILTER  INTO CURSOR &FStyle. READWRITE

SELECT (FStyle)
GO TOP
IF EOF()
  WAIT  WINDOW  'NO RECORDS SELECTED!!!'
  llDontPrn = .T.
  RETURN
ENDIF
INDEX on style TAG style
SET ORDER TO style
********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************
SELECT ORDHDR
SELECT ORDLINE

*B607847,1 11/30/2006 MMT Wrong account Name Displayed ,T20061129.0035[Start]
*SET RELATION TO cOrdType + Order INTO ORDHDR
SET RELATION TO cOrdType + Order INTO ORDHDR ADDITIVE  
*B607847,1 11/30/2006 MMT Wrong account Name Displayed ,T20061129.0035[END]

lcSQLStmt  = "Select TRANCD,[ORDER],CORDLINE,CTKTNO,STYLE From CutPick(INDEX=CutOrd) " 
lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CutPick' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
IF lnResult = 1
  SELECT CutPick
  =CURSORSETPROP("Buffering" ,3)
  IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
     SELECT CutPick
     INDEX on TRANCD+ORDER+CORDLINE TAG CutPick
     
*B608033,1 MMT 04/10/07 fix bug of error if Po is not installed [Start]   
*!*	  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules

*!*	  *-- If the "Style Purchase Order" or the "Point of Sale" modules is installed
*!*	  IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules 
*!*	     SELECT CutPick
*B608033,1 MMT 04/10/07 fix bug of error if Po is not installed [End]   

     INDEX on TRANCD+CTKTNO+STYLE TAG CutPick2
  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
ELSE
 *-- SQL connection error. can't open the report
  =gfModalGen('TRM00416B40011','ALERT')
  RETURN .F.
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfCreatecolors
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Creating Style colors
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfCreatecolors
*!*************************************************************
FUNCTION lfCreatecolors
DIMENSION XCLR[10],XSTK[10],XORD[10],XWIP[10]

STORE 0 TO XCLRCNT,XSTK,XORD,XWIP
STORE '' TO XCLR

SELECT (FStyle)
lnI=1
xstyle=cstymajor
xdesc=''
SCAN 
  IF cstymajor<>xstyle
    SELECT (lcColorfile)
    APPEND BLANK
    REPLACE cstymajor WITH xstyle, desc WITH xdesc
    FOR i =1 TO 10
      Z=allTRIM(str(i))
      REPLACE col&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
    ENDFOR
    
    SELECT (FStyle)
    xstyle=cstymajor
    xdesc=desc
    lnI=1
    STORE '' TO XCLR
    STORE 0 TO XSTK,XORD,XWIP
  ENDIF
  
  SELECT (FStyle)
  IF !(lnI>10)
    XCLR(lnI)   = SUBSTR(Style,lnNonMajPo)
    XSTK(lnI)   = TOTSTK
    XORD(lnI)   = TOTORD
    XWIP(lnI)   = TOTWIP
    lnI=lnI+1
  ELSE
    XCLR(10) = 'OTHER'
    XSTK(10)=XSTK(10)+TOTSTK
    XORD(10)=XORD(10)+TOTORD
    XWIP(10)=XWIP(10)+TOTWIP
  ENDIF
ENDSCAN
SELECT (lcColorfile)
  APPEND BLANK
  REPLACE cstymajor WITH xstyle, desc WITH xdesc
  FOR i =1 TO 10
     Z=allTRIM(str(i))
     REPLACE col&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
  ENDFOR
SELECT &lcColorfile  
INDEX ON  cstymajor TAG &lcColorfile 

SELECT (FStyle)
SET RELATION TO cstymajor INTO &lcColorfile
RETURN .T.

*!*************************************************************
*! Name      : lfcreatfilter
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Creating filter
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfcreatfilter
*!*************************************************************
FUNCTION lfcreatfilter
*Cut the customer filter from the lcrpexp to use it in filtering on the  ORDLINE files


llMore24 = .F.
lnDatPost = 0

lnDataPos = ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  IF AT('INLIST(CUSTOMER.ACCOUNT',LCRPEXP) > 0
    lnDatPost = AT('INLIST(CUSTOMER.ACCOUNT',LCRPEXP)
  ELSE
    IF AT('BETWEEN(CUSTOMER.ACCOUNT',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(CUSTOMER.ACCOUNT',LCRPEXP)
      llMore24 = .T.
    ENDIF
  ENDIF

  IF lnDatPost > 0
    IF llMore24
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) , 2)
    ELSE
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
    ENDIF

    IF lnPos1 > 0
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
    ELSE
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost)

      *B607847,1 11/30/2006 MMT Wrong account Name Displayed [Start]
      *lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. )")
      *lcCustAcnt = "AND (" + lcCustAcnt
      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
      *B607847,1 11/30/2006 MMT Wrong account Name Displayed [End]
      
    ENDIF
  ENDIF
ENDIF
*Cut the ordhdr complete filter from the lcrpexp to use it in filtering on the  ORDLINE files
lnDatPost = 0
lnDataPos = ASCAN(laOgFxFlt,'ORDHDR.COMPLETE')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  IF AT('BETWEEN(DTOS(ORDHDR.COMPLETE',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(DTOS(ORDHDR.COMPLETE',LCRPEXP)
  ENDIF
  IF lnDatPost > 0
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) )
    IF lnPos1 > 0
      lcComplete= SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcComplete, " .T. ")
      lcComplete= "AND " + lcComplete
    ELSE
      lcComplete= SUBSTR(lcRpExp ,lnDatPost)
      lcRPExp = STRTRAN(lcRPExp, lcComplete, " .T. ")
      lcComplete= "AND " + lcComplete
    ENDIF
  ENDIF
ENDIF
*Cut the ordhdr statr filter from the lcrpexp to use it in filtering on the  ORDLINE files
lnDatPost = 0
lnDataPos = ASCAN(laOgFxFlt,'ORDHDR.START')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  IF AT('BETWEEN(DTOS(ORDHDR.START',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(DTOS(ORDHDR.START',LCRPEXP)
  ENDIF

  IF lnDatPost > 0
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) )
    IF lnPos1 > 0
      lcStart= SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcStart, " .T. ")
      lcStart= "AND " + lcStart
    ELSE
      lcStart= SUBSTR(lcRpExp ,lnDatPost)
      lcRPExp = STRTRAN(lcRPExp, lcStart, " .T. ")
      lcStart= "AND " + lcStart
    ENDIF
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lcGetdate
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Creating filter
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lcGetdate(lcdate,'1')
*!*************************************************************


FUNCTION lcGetdate
LPARAMETERS lcExpr,ln1or2
lnpos1=AT(['],lcExpr,1)
lnpos2=AT(['],lcExpr,2)

lcdate1=SUBSTR(lcExpr,lnpos1+1,lnpos2-lnpos1)
lcdate2=SUBSTR(lcExpr,lnpos2+3,lnpos2-lnpos1)

* MAH T20071219.0002 BRITISH Format Date Problem
*ldDate1=CTOD(SUBSTR(lcdate1,5,2)+"/"+SUBSTR(lcdate1,7,2)+"/"+SUBSTR(lcdate1,1,4))
*ldDate2=CTOD(SUBSTR(lcdate2,5,2)+"/"+SUBSTR(lcdate2,7,2)+"/"+SUBSTR(lcdate2,1,4))
LOCAL lcTempDate, lcTempCentury
lcTempDate    = SET("Date")
lcTempCentury = SET("Century")

SET DATE AMERICAN
SET CENTURY ON

ldDate1=CTOD(SUBSTR(lcdate1,5,2)+"/"+SUBSTR(lcdate1,7,2)+"/"+SUBSTR(lcdate1,1,4))
ldDate2=CTOD(SUBSTR(lcdate2,5,2)+"/"+SUBSTR(lcdate2,7,2)+"/"+SUBSTR(lcdate2,1,4))

SET DATE &lcTempDate.
SET CENTURY &lcTempCentury.

* MAH T20071219.0002 END



IF ln1or2='1'
  RETURN ldDate1
ELSE
  RETURN ldDate2
ENDIF

*!*************************************************************
*! Name      : lpCollecData_yes
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Collecting data in Temp. File
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollecData_yes
*!*************************************************************

PROCEDURE lpCollecData_yes
PRIVATE llHastran
SELECT (FSTYLE)
*!*  GO TOP
XDESC=SPACE(20)
XSTYLE=SPACE(19)         
XCOLOR=SPACE(19)         
SCAN
  IF INKEY()=32
    RETURN
  ENDIF
  xStyle = Style
  WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
  llHastran=.f.
  *-- If the "Manufacturing" module is installed
  IF 'MF' $ oAriaApplication.CompanyInstalledModules
  SELECT CutPick
  SET ORDER TO CutPick2 
    lcSQLStmt=lcSQLStmtC+" AND POSLN.STYLE='"+xStyle +"' "+lcSQLStmtC1
    lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CUTTKTL' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
    IF lnResult = 1
      SELE CUTTKTL
      IF RECCOUNT('CUTTKTL')>0
        IF !UPPER('CUTTKTL.VENDOR INTO CONTRACTOR') $ UPPER(SET('RELATION'))
          SET RELATION TO CUTTKTL.VENDOR INTO CONTRACTOR ADDITIVE 
        ENDIF  
        llHastran=.t.
        SELECT CUTTKTL
        SCAN FOR &XADDCUT
          SCATTER MEMVAR MEMO
          SELECT &LCWORKFILE
          M.VENDNAME=CONTRACTOR.CVENCOMP
          *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [Begin]
          IF m.trancd<>'1'
            m.qty1=m.qty1*-1
            m.qty2=m.qty2*-1
            m.qty3=m.qty3*-1
            m.qty4=m.qty4*-1
            m.qty5=m.qty5*-1
            m.qty6=m.qty6*-1
            m.qty7=m.qty7*-1
            m.qty8=m.qty8*-1
            m.totqty=m.totqty*-1
          ENDIF
          *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [End]
          m.transno=m.po
          M.CTRANTYPE='1'  
          M.CDIVISION=&FSTYLE..CDIVISION
          M.SEASON=&FSTYLE..SEASON
          M.CSTYGROUP=&FSTYLE..CSTYGROUP
          M.CSTYMAJOR=&FSTYLE..CSTYMAJOR
          M.DESC=&FSTYLE..DESC
          M.TOTSTK=&FSTYLE..TOTSTK
          M.TOTWIP=M.TOTQTY
          M.TOTORD=0
          FOR I=1 TO 8
            Z=STR(I,1)
            M.STK&Z=&FSTYLE..STK&Z
            *MMT22
            m.ALO&Z = &FSTYLE..ALO&Z
            m.UnAlo&Z = MAX(&FSTYLE..Ord&Z - &FSTYLE..Alo&Z,0)
            *MMT22
          ENDFOR
          FOR I=1 TO 8
            Z=STR(I,1)
            M.ORD&Z=0
          ENDFOR
          FOR I=1 TO 8
            Z=STR(I,1)
          M.WIP&Z=M.QTY&Z
          ENDFOR
          *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [Begin]
          *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]

         *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
         && IF !SEEK('1'+m.transno+m.style,lcWorkfile,lcWorkfile)
         IF !SEEK('1'+m.transno + m.style + m.trancd+ m.CRSESSION,lcWorkfile,lcWorkfile)
         *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]
         
          *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]
          *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [End]
            APPEND BLANK 
            m.vendor=IIF(!ISNULL(m.vendor),m.vendor,"     ")
            GATH MEMVAR MEMO
           *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [Begin]
          *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]
          
         *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
         && ENDIF   
          ENDIF   
         *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]
         
          *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]
          *B608164,1 WLD Fix bug of open WIP qty,we need to subtract any receipts,canceled,damaged 07/15/07 [End]
        ENDSCAN
      ENDIF  
    ELSE
      *-- SQL connection error. can't open the report
       =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules
  *-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed
  IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
    SELECT CutPick
    SET ORDER TO CutPick2 
    lcSQLStmt=lcSQLStmtP+" AND  POSLN.STYLE='"+xStyle +"' " +lcSQLStmtP1
    lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'POSLN' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
    IF lnResult = 1
      SELECT POSLN
      IF RECCOUNT('POSLN')>0
        IF !UPPER('POSLN.VENDOR INTO APVENDOR') $ UPPER(SET('RELATION'))
          SET RELATION TO POSLN.VENDOR INTO APVENDOR ADDITIVE
        ENDIF   
        llHastran=.t.
        SELECT POSLN 
        *B608033,1 MMT 04/10/07 fix bug of wrong WIP in case of there is shipments [Start]   
        *SCAN FOR &XADD 
        SCAN FOR &XADD AND !INLIST(Trancd,'3','6')
        *B608033,1 MMT 04/10/07 fix bug of wrong WIP in case of there is shipments [End]   
          SCATTER MEMVAR MEMO
          M.VENDNAME=APVENDOR.CVENCOMP
          IF m.trancd<>'1'
            m.qty1=m.qty1*-1
            m.qty2=m.qty2*-1
            m.qty3=m.qty3*-1
            m.qty4=m.qty4*-1
            m.qty5=m.qty5*-1
            m.qty6=m.qty6*-1
            m.qty7=m.qty7*-1
            m.qty8=m.qty8*-1
            m.totqty=m.totqty*-1
          ENDIF
          SELECT &LCWORKFILE
          m.transno=m.po
          M.CTRANTYPE='2'
          M.CDIVISION=&FSTYLE..CDIVISION
          M.SEASON=&FSTYLE..SEASON
          M.CSTYGROUP=&FSTYLE..CSTYGROUP
          M.CSTYMAJOR=&FSTYLE..CSTYMAJOR
          M.DESC=&FSTYLE..DESC
          M.TOTSTK=&FSTYLE..TOTSTK
          M.TOTWIP=M.TOTQTY
          M.TOTORD=0
          FOR I=1 TO 8
            Z=STR(I,1)
            M.STK&Z=&FSTYLE..STK&Z
            
            *MMT22
            m.ALO&Z = &FSTYLE..ALO&Z
            m.UnAlo&Z = MAX(&FSTYLE..Ord&Z - &FSTYLE..Alo&Z,0)
            *MMT22
            
          ENDFOR
          FOR I=1 TO 8
            Z=STR(I,1)
            M.ORD&Z=0
          ENDFOR
          FOR I=1 TO 8
            Z=STR(I,1)
            M.WIP&Z=M.QTY&Z
          ENDFOR
          APPEND BLANK 
          GATH MEMVAR MEMO
        ENDSCAN
      ENDIF  
    ELSE
      *-- SQL connection error. can't open the report
       =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
  SELECT CutPick
  SET ORDER TO CutPick
  SELE ORDLINE
  SEEK XSTYLE
  SCAN  REST  WHILE Style = xStyle FOR   &lcLinescan 
    llHastran=.t.
    SCATTER MEMVAR MEMO
    M.STATUS=IIF(ORDHDR->APPROVAL='DECLINE','D',ORDHDR->STATUS)
    M.CUSTOMER=CUSTOMER.BTNAME
    SELECT &LCWORKFILE
    m.transno=m.order
    M.CTRANTYPE='3'
    M.CDIVISION=&FSTYLE..CDIVISION
    M.SEASON=&FSTYLE..SEASON
    M.CSTYGROUP=&FSTYLE..CSTYGROUP
    M.CSTYMAJOR=&FSTYLE..CSTYMAJOR
    M.DESC=&FSTYLE..DESC
    M.TOTSTK=&FSTYLE..TOTSTK
    M.TOTORD=M.TOTQTY
    M.TOTWIP=0
    FOR I=1 TO 8
      Z=STR(I,1)
      M.STK&Z=&FSTYLE..STK&Z
      *MMT22
      m.ALO&Z = &FSTYLE..ALO&Z
      m.UnAlo&Z = MAX(&FSTYLE..Ord&Z - &FSTYLE..Alo&Z,0)
      *MMT22
    ENDFOR
    
    FOR I=1 TO 8
      Z=STR(I,1)
      M.ORD&Z=M.QTY&Z
    ENDFOR
    FOR I=1 TO 8
      Z=STR(I,1)
      M.WIP&Z=0
    ENDFOR
    APPEND BLANK 
    GATH MEMVAR MEMO
  ENDSCAN && ordline
  IF !llHastran
    SELECT &LCWORKFILE
    APPEND BLANK 
     REPLACE STYLE WITH XSTYLE,;
       TRANSNO WITH '*****',;
       SCALE WITH &FSTYLE..SCALE,;
       CTRANTYPE WITH '0',;
       CDIVISION WITH &FSTYLE..CDIVISION,;
       SEASON WITH &FSTYLE..SEASON,;
       CSTYGROUP WITH &FSTYLE..CSTYGROUP,;
       CSTYMAJOR WITH &FSTYLE..CSTYMAJOR,;
       DESC WITH &FSTYLE..DESC,;
       TOTSTK WITH &FSTYLE..TOTSTK,;
       TOTORD WITH 0,;
       TOTWIP WITH 0
      FOR I=1 TO 8
        Z=STR(I,1)
        REPLACE   STK&Z WITH &FSTYLE..STK&Z
      
        *MMT22
        REPLACE ALO&Z   WITH &FSTYLE..ALO&Z,;
                UnAlo&Z WITH MAX(&FSTYLE..Ord&Z - &FSTYLE..Alo&Z,0)
        *MMT22
      
      ENDFOR
      FOR I=1 TO 8
        Z=STR(I,1)
      REPLACE  ORD&Z WITH 0
      ENDFOR
      FOR I=1 TO 8
        Z=STR(I,1)
       REPLACE  WIP&Z WITH 0
      ENDFOR
    ENDIF 
  SELECT(FSTYLE)
ENDSCAN

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF    && End of IF USED('CUTPICK')

SELECT (LCWORKFILE)


*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
lcDeleted =SET("Deleted")
SET DELETED OFF 
SCAN FOR CTRANTYPE $ '12' AND !DELETED()
  lnCurrrec = RECNO()
  lcStyle = Style 
  lcTRANSNO  = TRANSNO
  lcCTRANTYPE = CTRANTYPE 
  
  STORE 0 TO lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8,lntotwip
  
  SUM WIP1,WIP2,WIP3,WIP4,WIP5 ,wip6,wip7,wip8,totwip TO ;
   lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8,lntotwip FOR ;
    CTRANTYPE + Style+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  
   
*B608239,1 MMT 09/06/2007 fix bug of not printing some styles [Start] 
*!*	   IF (lnWIP1 <= 0)  AND (lnWIP2<= 0) AND (lnWIP3<= 0)  AND (lnWIP4<= 0)  AND (lnWIP5 <= 0)  AND (lnwip6<= 0)  AND (lnwip7<= 0)  AND (lnwip8<= 0)  AND (lntotwip<= 0)
*!*	      DELETE ALL FOR  CTRANTYPE + Style+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  
*!*	   ELSE
   IF (lnWIP1 <= 0)  OR (lnWIP2<= 0) OR (lnWIP3<= 0) OR (lnWIP4<= 0) OR (lnWIP5 <= 0) OR (lnwip6<= 0) OR (lnwip7<= 0) OR (lnwip8<= 0)  OR (lntotwip<= 0)
*B608239,1 MMT 09/06/2007 fix bug of not printing some styles [End] 
     llModified = .F.
     FOR lnCount =1 TO 8
       lcCount = ALLTRIM(STR(lnCount,1)) 
       IF lnWIP&lcCount. <= 0
         REPLACE ALL WIP&lcCount. WITH 0 ,QTY&lcCount. WITH 0 FOR  CTRANTYPE + Style+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  
         llModified  = .T.
       ENDIF    
     ENDFOR   
     IF llModified 
       REPLACE ALL TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+wip6+wip7+wip8 ,TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8 FOR  CTRANTYPE + Style+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  
     ENDIF 
   ENDIF   
   GO RECORD lnCurrrec
ENDSCAN 
SET DELETED &lcDeleted
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]


IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
ENDIF

COPY TO oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"

*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[Start] 
*USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0 SHARED 
USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0  EXCLUSIVE   
*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[End] 
SELECT &LCTMPFILE
DO CASE
 CASE   XPRTWIP='S' .AND.   XPRTORD='S'
   INDEX ON STYLE + TRANSNO + TRANCD + STR(RECNO(),7) TAG LCTMPFILE
 CASE   XPRTWIP='S' .AND.  XORDSORT='D'
   INDEX ON STYLE + DTOS(COMPLETE) + TRANSNO  + STORE + STR(LINENO , 6) TAG LCTMPFILE
 CASE   XPRTWIP='S' .AND. XORDSORT='A'
   INDEX ON STYLE + ACCOUNT + TRANSNO  + DTOS(COMPLETE) + STORE +STR(LINENO , 6) TAG LCTMPFILE
 CASE   XPRTWIP='S' .AND. XORDSORT='O'
   INDEX ON STYLE + TRANSNO  + DTOS(COMPLETE) + STORE +STR(LINENO , 6) TAG LCTMPFILE
 CASE   XWIPSORT='D'  .AND. XPRTORD='S'
   INDEX ON STYLE + DTOS(COMPLETE) + TRANSNO + TRANCD +STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='D'   .AND. XORDSORT='D'
   INDEX ON STYLE + DTOS(COMPLETE) + TRANSNO + TRANCD + STORE + STR(LINENO , 6) +STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='D'  .AND. XORDSORT='A'
   INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + PO + TRANCD  + STORE +STR(LINENO , 6)+STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='D' .AND. XORDSORT='O'
   INDEX ON STYLE + DTOS(COMPLETE) + PO + TRANCD + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6)+STR(RECNO() , 7) TAG LCTMPFILE                              
 CASE   XWIPSORT='F'   .AND. XPRTORD='S'
   INDEX ON STYLE + VENDOR + TRANSNO + TRANCD + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='F'  .AND. XORDSORT='D'
   INDEX ON STYLE + VENDOR + PO + TRANCD + DTOS(COMPLETE) + ORDER  + STORE + STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='F'  .AND. XORDSORT='A'
   INDEX ON STYLE + VENDOR + PO + TRANCD+ ACCOUNT + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='F' .AND. XORDSORT='O'
   INDEX ON STYLE + VENDOR + TRANSNO + TRANCD+ DTOS(COMPLETE) + STORE +STR(LINENO , 6)  + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='P'  .AND. XPRTORD='S'
   INDEX ON STYLE + TRANSNO + TRANCD + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='P'  .AND. XORDSORT='D'
   INDEX ON STYLE + PO + TRANCD + DTOS(COMPLETE) + ORDER  + STORE + STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='P'   .AND. XORDSORT='A'
   INDEX ON STYLE + PO + TRANCD + ACCOUNT + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6)+ STR(RECNO() , 7) TAG LCTMPFILE
 CASE   XWIPSORT='P'   .AND. XORDSORT='O'
   INDEX ON STYLE + TRANSNO+ DTOS(COMPLETE)  + TRANCD + STORE +STR(LINENO , 6)+ STR(RECNO() , 7) TAG LCTMPFILE
ENDCASE

SELECT * FROM SCALE WHERE TYPE='S' INTO CURSOR LCSCALE READWRITE 
SELECT LCSCALE 
IF loOgScroll.FileExist(oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
ENDIF
COPY TO oAriaApplication.WorkDir +  TMPSCAL+ ".DBF"

=lfAdjustCRSettings()


*!*************************************************************
*! Name      : lpCollecData_no
*! Developer : AYMAN MAHMOUD AHMED (AHM)
*! Date      : 06/04/2006
*! Purpose   : Collecting data in Temp. File
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollecData_no
*!*************************************************************

PROCEDURE lpCollecData_no

PRIVATE llHastran
SELECT (FSTYLE)
XDESC=SPACE(20)
XSTYLE=SPACE(19)         
XCOLOR=SPACE(19)         
GRASTK=0
GRAWIP=0
GRAORD=0

SCAN 
  IF INKEY()=32
    RETURN
  ENDIF
  xStyle = Style
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
  GRASTK=GRASTK+TOTSTK
  GRAWIP=GRAWIP+TOTWIP
  GRAORD=GRAORD+TOTORD
  llHastran=.f.
  *-- If the "Manufacturing" module is installed
  IF  'MF' $ oAriaApplication.CompanyInstalledModules 
    SELECT CutPick
    SET ORDER TO CutPick2 
    lcSQLStmt=lcSQLStmtC+" AND POSLN.STYLE='"+xStyle +"' "+lcSQLStmtC1
    lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CUTTKTL' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
    IF lnResult = 1
      SELE CUTTKTL
      IF RECCOUNT('CUTTKTL')>0
   
        IF !UPPER('CUTTKTL.VENDOR INTO CONTRACTOR') $ UPPER(SET('RELATION'))
          SET RELATION TO CUTTKTL.VENDOR INTO CONTRACTOR ADDITIVE
        ENDIF  
        llHastran=.t.
        SELECT CUTTKTL
        SCAN FOR &XADDCUT 
          SCATTER MEMVAR MEMO
          STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10
          IF m.trancd<>'1'
            m.totqty=m.totqty*-1
            
            *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
            FOR lnCount = 1  TO 8
              lcCount = ALLTRIM(STR(lnCount,1))
              m.Qty&lcCount = m.Qty&lcCount * -1 
            ENDFOR
            *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]
            
          ENDIF
          m.transno=m.po
          M.CTRANTYPE='1'
          M.VENDNAME=CONTRACTOR.CVENCOMP
          DO CASE
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col1
              m.qcol1= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
              m.qcol2= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
              m.qcol3= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
              m.qcol4= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
              m.qcol5= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
              m.qcol6= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
              m.qcol7= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
              m.qcol8= m.totqty
           CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
              m.qcol9= m.totqty
           CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
              m.qcol10= m.totqty
          OTHERWISE 
             m.qcol10=  m.totqty
             REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile
          ENDCASE
          
          SELECT &lcTransfile 
          M.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)
          
    		  *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
    		  IF !SEEK(m.CTRANTYPE+m.TRANSNO+m.Style+m.TRANCD+m.CRSESSION,lcTransfile,lcTransfile)	
          *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]
            APPEND BLANK 
             m.vendor=IIF(!ISNULL(m.vendor),m.vendor,"     ")
            GATH MEMVAR MEMO
          *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
          ENDIF
          *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[ENd]
        ENDSCAN
      ENDIF
     ELSE
      *-- SQL connection error. can't open the report
       =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF

  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules

  *-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed
 IF  'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules 
   SELECT CutPick
   SET ORDER TO CutPick2 
    *** Store off Records Into Second Temp File POTEMP
    lcSQLStmt=lcSQLStmtP+" AND  POSLN.STYLE='"+xStyle +"' " +lcSQLStmtP1
    lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'POSLN' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
    IF lnResult = 1
      SELECT POSLN
      IF RECCOUNT('POSLN')>0
        IF !UPPER('POSLN.VENDOR INTO APVENDOR') $ UPPER(SET('RELATION'))
          SET RELATION TO POSLN.VENDOR INTO APVENDOR ADDITIVE
        ENDIF 
        llHastran=.t.
        SELECT POSLN
        
        *B608033,1 MMT 04/10/07 fix bug of wrong WIP in case of there is shipments [Start]   
        *SCAN FOR &XADD 
        SCAN FOR &XADD AND !INLIST(Trancd,'3','6')
        *B608033,1 MMT 04/10/07 fix bug of wrong WIP in case of there is shipments [End]   
        
          SCATTER MEMVAR MEMO
          STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10

          IF m.trancd<>'1'
            m.totqty=m.totqty*-1
           
            *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
            FOR lnCount = 1  TO 8
              lcCount = ALLTRIM(STR(lnCount,1))
              m.Qty&lcCount = m.Qty&lcCount * -1 
            ENDFOR
            *: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[End]

          ENDIF
          m.transno=m.po
          M.CTRANTYPE='2'
          M.VENDNAME=APVENDOR.CVENCOMP
         DO CASE
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col1
              m.qcol1= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
              m.qcol2= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
              m.qcol3= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
              m.qcol4= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
              m.qcol5= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
              m.qcol6= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
              m.qcol7= m.totqty
            CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
              m.qcol8= m.totqty
           CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
              m.qcol9= m.totqty
           CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
              m.qcol10= m.totqty
          OTHERWISE 
             m.qcol10=  m.totqty
             REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile
          ENDCASE
          SELECT &lcTransfile 
          M.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)

          APPEND BLANK 
          GATH MEMVAR MEMO
        ENDSCAN
      ENDIF  
     ELSE
      *-- SQL connection error. can't open the report
       =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF   
   
  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
  SELECT CutPick
  SET ORDER TO CutPick
  SELE ORDLINE
  SEEK XSTYLE
 
  SCAN  REST  WHILE Style = xStyle FOR   &lcLinescan 
    llHastran=.t.
    SCATTER MEMVAR MEMO
    STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10
    M.CUSTOMER=CUSTOMER.BTNAME
    DO CASE
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col1
        m.qcol1=totqty
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
        m.qcol2=totqty
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
        m.qcol3=totqty
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
        m.qcol4=totqty
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
        m.qcol5=totqty
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
        m.qcol6=totqty  
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
        m.qcol7=totqty      
      CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
        m.qcol8=totqty     
     CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
        m.qcol9=totqty
     CASE SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
        m.qcol10=totqty  
     OTHERWISE 
       m.qcol10=  m.totqty
       REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile
    
    ENDCASE
    SELECT &lcTransfile 
    M.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)
    m.transno=m.order
    M.CTRANTYPE='3'
    
    APPEND BLANK 
    GATH MEMVAR MEMO
  ENDSCAN
     
  IF !llHastran
    SELECT &lcTransfile 
    APPEND BLANK 
    REPLACE CSTYMAJOR WITH SUBSTR(xstyle,1,lnMajLen),transno WITH '*****',CTRANTYPE WITH '0'
  ENDIF 
ENDSCAN
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF    && End of IF USED('CUTPICK')

SELECT (lcColorfile )
IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
ENDIF
COPY TO oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF"

*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[Start] 
*USE oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" IN 0 SHARED 
USE oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" IN 0 EXCLUSIVE 
*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[End] 

INDEX ON CSTYMAJOR TAG &LCTMPcolor  


SELECT (lcTransfile )
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
FOR lnCounter = 1 TO 10
lcCounter = ALLTRIM(STR(lnCounter))

SCAN FOR CTRANTYPE $ '12' AND qcol&lcCounter. <> 0
  lnCurrrec = RECNO()
  lcStyle = CSTYMAJOR
  lcTRANSNO  = TRANSNO
  lcCTRANTYPE = CTRANTYPE 
  
  
    STORE 0 TO lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8
    SUM Qty1,Qty2,Qty3,Qty4,Qty5 ,Qty6,Qty7,Qty8 TO ;
	    lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8 FOR ;
	    CTRANTYPE + cStyMajor +TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  AND qcol&lcCounter. <> 0
    
    IF (lnWIP1 <= 0)   OR (lnWIP2<= 0)  OR(lnWIP3<= 0)   OR (lnWIP4<= 0)   OR (lnWIP5 <= 0)  OR (lnwip6<= 0)  OR (lnwip7<= 0)  OR (lnwip8<= 0)
      FOR lnCount =1 TO 8
       lcCount = ALLTRIM(STR(lnCount,1)) 
       IF lnWIP&lcCount. <= 0
         REPLACE ALL QTY&lcCount. WITH 0,qcol&lcCounter. WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 ;
            FOR  CTRANTYPE + CSTYMAJOR +TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  AND qcol&lcCounter. <> 0
       ENDIF    
      ENDFOR   
    ENDIF   
   GO RECORD lnCurrrec
  ENDSCAN 
ENDFOR    
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[END]



IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
ENDIF
COPY TO oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF"

*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[Start] 
*USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 SHARED 
USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 EXCLUSIVE 
*: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[End] 

SELECT &LCTMPtrans  
DO CASE
    CASE  XPRTWIP='S' .AND.  XPRTORD='S'
      INDEX ON CSTYMAJOR+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE XPRTWIP='S' .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+DTOS(COMPLETE)+TRANSNO +STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE  XPRTWIP='S' .AND.  XORDSORT='A'
      INDEX ON CSTYMAJOR+ACCOUNT+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE  XPRTWIP='S' .AND.  XORDSORT='O'
      INDEX ON CSTYMAJOR+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE XWIPSORT='D'   .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+DTOS(COMPLETE)+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='D'   .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE         
    CASE   XWIPSORT='D'   .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+ACCOUNT+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='D'   .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+ORDER+DTOS(COMPLETE)+PO +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE                   
    CASE   XWIPSORT='F'  .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+VENDOR+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'   .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+VENDOR+PO+DTOS(COMPLETE)+ORDER +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'  .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+VENDOR+PO +ACCOUNT+ORDER+DTOS(COMPLETE) +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'    .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+VENDOR+PO +TRANCD+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'  .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'    .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+PO +TRANCD+DTOS(COMPLETE)+ORDER+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'  .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+PO +TRANCD+ACCOUNT+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P' .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+PO +TRANCD+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
ENDCASE
GRAOTS=GRASTK+GRAWIP-GRAORD
=lfAdjustCRSettings()

RETURN .T.


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608130
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'ROYALTY'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR2'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0



ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

