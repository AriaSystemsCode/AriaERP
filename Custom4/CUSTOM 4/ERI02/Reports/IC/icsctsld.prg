*:***************************************************************************
*: Program file  : ICSCTSLD.PRG
*: Program desc. : Custom Special Cut & Sold
*: TRACK NO      : C200765
*: System        : Aria4XP
*: Module        : Inventory Control (IC)
*: Developer     : Walid Hamed (WLD)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSCTSLD
*:***************************************************************************
*: Modification:
*:*B607847,1 11/30/2006 MMT Wrong account Name Displayed ,T20061129.0035
*:*B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report,T20061203.0001
*:*B607855,1 12/20/2006 AYM ORDER COMPLETE FILTER DOES NOT AFFECT THE REPORT,T20061219.0009
*: B607950,1 MMT 01/29/2007 fix bug of printing closed C/T T20070103.0023
*: C200765,1 04/17/2007 WLD Include all customs and fixes  of Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold  for ERIC JAVITS, INC to Aria4XP
*: B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[T20070723.0008]
*: B608277,1 MMT 09/23/2007 fix bug of error when Select Hats ALL setting from OG [T20070829.0011]
*:***************************************************************************

*--Initialize the variables.
*-- llDontPrn  variable that showes that no records match report criteria
*-- WORKTEMP   variable that hold the name of work temp. file
*-- CUTTTEMP   variable that hold the name of cuttkt temp. file
*-- POTEMP     variable that hold the name of PO temp. file
*-- ORDTEMP    variable that hold the name of ORDHDR temp. file
*-- XPRTWIP    variable that showes print WIP Summary or Detail
*-- XBYSIZE    variable that showes print by size
*-- XSLDATE    variable that hold from sales order comp. date
*-- XSHDATE    variable that hold to   sales order comp. date
*-- XPLDATE    variable that hold from Prod. order comp. date
*-- XPHDATE    variable that hold to   Prod. order comp. date
*-- XWIPSORT   variable that showes if it is sorted by WIP or not
*-- XPRTORD    variable that showes print Sales Summary or Detail
*-- XORDSORT   variable that hold the sales order Sort
*-- XSTATHLD   variable that showes if include Orders on hold
*-- XALLOCAT   variable that showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable that hold optional title
*-- XFILTER    variable that hold report filter expression


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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  STORE "" TO lcShpDt,lcBokDt
  llRtnSty = .F.
  *Declare array that hold previos totals and warecode .
  DIMENSION xTotStkPg[90,2]
  STORE 0 TO lnNewPos    && hold the max used element of the array
  STORE 0 TO lnTotRetSt
  STORE 0 TO xTotStkPg
  STORE '' TO BOOKTEMP,SHIPTEMP,StyWareH,lcStoreTmp && Variables that will hold temporary file
  STORE 0 TO lnGTotShp , lnGTotRtrn && *Hold the Grand total, Shipped and Returned Qty.
  STORE '' TO RETTEMP && hold the name of retline temp. file to print the shipped qty - return qty.
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
  XBYSIZE  = IIF(llRPBySize,'Y','N')
  *Using the new option print open PO's instead of Print WIP
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *XPRTWIP  = lcRPWIPSta
  XPRTWIP  = lcRpPODtSm
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  XSHPLDT  = lcGetdate(lcShpDt,'1')
  XSHPHDT  = lcGetdate(lcShpDt,'2')
  ldSoSEntDt = lcGetdate(lcBokDt,'1')
  ldSoHEntDt = lcGetdate(lcBokDt,'2')
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

  XFILTER = lcRPExp
  WAIT WINDOW 'Collecting data....' NOWAIT
  FStyle= loOGScroll.gfTempName()
  WORKTEMP = loOGScroll.gfTempName()
  IF !USED('CONTRACTOR')
    USE oAriaApplication.DATADIR +  "APVENDOR.DBF"  AGAIN IN 0 ALIAS 'CONTRACTOR'
    SELECT CONTRACTOR
    SET ORDER TO VENCODE   && CVENDCODE
  ENDIF
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *lcSQLStmtC ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,"
  lcSQLStmtC ="  Select poshdr.STATUS,posln.STYLE,posln.[LINENO],posln.cSTYTYPE,posln.cBUSDOCU,posln.CINVTYPE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,poshdr.ENTERED,"
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  lcSQLStmtC =lcSQLStmtC + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
  lcSQLStmtC =lcSQLStmtC + " posln.SCALE,posln.TRANCD "
  lcSQLStmtC =lcSQLStmtC + " ,MFGOPRHD.cContCode AS VENDOR  From "
  lcSQLStmtC =lcSQLStmtC + " posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
  lcSQLStmtC =lcSQLStmtC + " LEFT OUTER JOIN MFGOPRHD(INDEX=TktOper) ON MFGOPRHD.cimtyp='M' AND POSLN.PO=MFGOPRHD.CTKTNO AND MFGOPRHD.lInHouse = 0 "
  lcSQLStmtc =lcSQLStmtc+" Where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'U' AND POSLN.CINVTYPE='0001' "
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *lcSQLStmtP ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,"
  lcSQLStmtP ="  Select poshdr.STATUS,posln.[LINENO],posln.STYLE,posln.cSTYTYPE,posln.cBUSDOCU,posln.CINVTYPE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,poshdr.ENTERED,"
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  lcSQLStmtP =lcSQLStmtP + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
  lcSQLStmtP =lcSQLStmtP + " posln.SCALE,posln.TRANCD "
  lcSQLStmtP =lcSQLStmtP +" ,POSHDR.VENDOR  From "
  lcSQLStmtP =lcSQLStmtP +" posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
  lcSQLStmtp =lcSQLStmtp +" where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'P' AND POSLN.CINVTYPE='0001' "
  IF XPLDATE<>CTOD('  /  /    ')
    *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [Start]
    *lcSQLStmtc1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'"
    lcSQLStmtc1 =" AND poshdr.STATUS not in ('A','S','X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'"
    *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [End]

    lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'"
  ELSE
    *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [Start]
    *lcSQLStmtc1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  "
    lcSQLStmtc1 =" AND poshdr.STATUS not in ('A','S','X', 'C') AND posln.TOTQTY<>0  "
    *B607950,1 MMT 01/29/2007 fix bug of printing closed C/T [End]

    lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  "
  ENDIF

  XADD = "PosLn.Po<>'*****' "
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  XADD = XADD + " AND IIF(lcRpPODtSm = 'D' AND XBYSIZE='N', TranCd <> '6', !INLIST(TranCd , '3' , '6'))"
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  DO CASE
    CASE XALLOCAT = 'L'
      * XADD = XADD +' AND  PosLn.Po=CutPick2.CtktNo '
      XADD = XADD +" AND  gfSeek('2'+Po+style,'CutPick') "
    CASE XALLOCAT = 'N'
      * XADD = XADD +' AND   PosLn.Po <> CutPick2.CtktNo'
      XADD = XADD +" AND  !gfSeek('2'+Po+style,'CutPick') "

  ENDCASE

  XADDCUT = "CUTTKTL.Po<>'*****' "
  DO CASE
    CASE XALLOCAT = 'L'
      * XADDCUT = XADDCUT +' AND  CUTTKTL.Po=CutPick2.CtktNo  '
      XADDCUT = XADDCUT +" AND  gfSeek('1'+Po+style,'CutPick') "


    CASE XALLOCAT = 'N'
      * XADDCUT = XADDCUT +' AND   CUTTKTL.Po<>CutPick2.CtktNo  '
      XADDCUT = XADDCUT +" AND  !gfSeek('1'+Po+style,'CutPick') "

  ENDCASE


  XADDORD = ''
  IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
    DO CASE
      CASE XALLOCAT = 'L'
        *XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' +   ' OrdLine.Order = CutPick3.Order) '
        XADDORD = " .AND.  ( gfSeek('1'+Order+STR(LineNo,6),'CutPick') .OR.  gfSeek('2'+Order+STR(LineNo,6),'CutPick')) "

      CASE XALLOCAT = 'N'
        * XADDORD = ' .AND.  (OrdLine.Order <>CutPick.Order .AND.' +   ' OrdLine.Order <> CutPick3.Order) '
        XADDORD = " .AND.  ( !gfSeek('1'+Order+STR(LineNo,6),'CutPick') .AND.  !gfSeek('2'+Order+STR(LineNo,6),'CutPick')) "

    ENDCASE
  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules


  IF XSLDATE<>CTOD('  /  /    ')
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *!*	   lcLinescan=" BETWEEN(Complete , xSLDate , xSHDate)   .AND. "
    *!*	*:*B607855,1 12/20/2006 AYM ORDER COMPLETE FILTER DOES NOT AFFECT THE REPORT,T20061219.0009 BEGIN
    *!*	*!*	   lcLinescan=" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
    *!*	   lcLinescan=lcLinescan+" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
    *!*	*:*B607855,1 12/20/2006 AYM ORDER COMPLETE FILTER DOES NOT AFFECT THE REPORT,T20061219.0009 END

    *!*	   lcLinescan= lcLinescan+ " IIF(lcRPAloct = 'L' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'N' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
    *!*	   lcLinescan= lcLinescan+  " " + XADDORD +" "+lcCustAcnt

    lcLinescan=" BETWEEN(Start , xSLDate , xSHDate)   .AND. "
    lcLinescan=lcLinescan+" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
    lcLinescan= lcLinescan+ " IIF(lcRPAloct = 'A' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'U' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
    lcLinescan= lcLinescan+  " " + XADDORD +" "+lcCustAcnt
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ELSE
    lcLinescan= " ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O' .AND. "
    lcLinescan= lcLinescan+" IIF(lcRPAloct = 'A' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'U' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
    lcLinescan= lcLinescan+ " " + XADDORD +" "+lcCustAcnt
  ENDIF

  SELECT ordline
  IF !(UPPER('ORDLINE.ACCOUNT INTO CUSTOMER') $ UPPER(SET('RELATION')))
    SET RELATION TO "M" + ORDLINE.ACCOUNT INTO Customer ADDITIVE
  ENDIF
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  IF ldSoSEntDt <> CTOD('  /  /    ')
    lcBKscan = " BETWEEN(OrdHdr.Entered , ldSoSEntDt , ldSoHEntDt)   .AND. "
    lcBKScan = lcBKScan + " ORDHDR.Status <> 'X' .AND. cOrdType = 'O' "
  ELSE
    lcBKScan = " cOrdType = 'O' AND ORDHDR.Status <> 'X' "
  ENDIF
  *Getting the shipped qty.
  
  
  IF XSHPLDT <> CTOD('  /  /    ')
    lcSHscan = " BETWEEN(InvHdr.ShipDate , XSHPLDT , XSHPHDT)   .AND. "
    lcSHScan = lcSHscan + " TotQty <> 0 .AND. InvHdr.Status <> 'V' "
  ELSE
    lcSHscan = " InvHdr.Status <> 'V' AND TotQty <> 0 "
  ENDIF
  *get the retline temp. file to print the shipped qty - return qty.
  IF XSHPLDT <> CTOD('  /  /    ')
    *: B608277,1 MMT 09/23/2007 fix bug of error when Select Hats ALL setting from OG[Start]
    *lcRetScan = " BETWEEN(retline.crdate , XSHPLDT , XSHPHDT) .AND. retline.TotQty <> 0 .AND. "
    lcRetScan = " retline.crdate BETWEEN  '"+DTOC(XSHPLDT)+ "' AND '" + DTOC(XSHPHDT)+"' AND retline.TotQty <> 0 AND "
    *: B608277,1 MMT 09/23/2007 fix bug of error when Select Hats ALL setting from OG[End]
    lcRetScan = lcRetScan +  " rethdr.status <> 'V' "
  ELSE
    lcRetScan = " rethdr.status <> 'V' AND retline.TotQty <> 0 "
  ENDIF

  BOOKTEMP = loOGScroll.gfTempName()
  SHIPTEMP = loOGScroll.gfTempName()
  RETTEMP =  loOGScroll.gfTempName() && Hold the name of retline temp. file to print the shipped qty - return qty.
  lcStoreTmp  =  loOGScroll.gfTempName()
  StyWareH    =   loOGScroll.gfTempName()
  SELECT InvLine
  SET RELATION TO Invoice INTO InvHdr ADDITIVE


  SELECT ORDLINE
  lcoldSOOrd = ORDER('ORDLINE')
  =gfSetOrder('ORDLINE')
  lcBkTmp = oAriaApplication.WorkDir+BOOKTEMP+'.dbf'
  COPY STRUCTURE TO &lcBkTmp.
  USE &lcBkTmp. IN 0 EXCLUSIVE

  SELECT ORDHDR
  SCAN FOR &lcBKScan.
    SELECT ordline
    IF gfseek(ordhdr.cordtype+ordhdr.ORDER)
      SCAN REST WHILE cordtype+ORDER+STR(LINENO,6)=ordhdr.cordtype+ordhdr.ORDER
        SCATTER MEMVAR MEMO
        INSERT INTO &BOOKTEMP. FROM MEMVAR 
      ENDSCAN
    ENDIF
  ENDSCAN

  SELECT ORDLINE
  =gfSetOrder(lcoldSOOrd)

  SELECT (BOOKTEMP)
  INDEX ON STYLE+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &BOOKTEMP
  SELECT InvLine
  lcShTmp = oAriaApplication.WorkDir+SHIPTEMP+'.dbf'
  COPY TO &lcShTmp. FOR &lcSHscan.
  USE &lcShTmp. IN 0 EXCLUSIVE
  SELECT (SHIPTEMP)
  INDEX ON STYLE+Invoice+DTOS(InvDate) TAG &SHIPTEMP

  lcSQLStmtRet  = "Select RETLINE.* FROM RETHDR,RETLINE WHERE RETHDR.crmemo=RETLINE.crmemo AND " + lcRetScan
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmtRet, 'RETLINE' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    llRtnSty = .T.
    SELECT RETLINE
    lcRtTmp = oAriaApplication.WorkDir+RETTEMP+'.dbf'
    COPY TO &lcRtTmp.
    USE &lcRtTmp. IN 0 EXCLUSIVE
    SELECT (RETTEMP)
    INDEX ON STYLE+crmemo+DTOS(crdate) TAG &RETTEMP
  ENDIF
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

  *----
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
        RETURN .F.
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
    WAIT WIND 'NO RECORDS SELECTED!!!'
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
  *C200765 WLd Get free color lcFreeClr [Begin]
  =lfNonMaj()
  *C200765 WLd Get free color lcFreeClr [End]
  *-- Check the cost access
  DIMENSION laRPPrnItm[14]

  llCostAccs = gfUserPriv('IC','ICSTYLE')


*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[Start]
*!*    lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
*!*      ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*!*    lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
*!*      ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)
    IF ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)') > 0
      lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
        ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
    ENDIF 
    
  IF ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)') > 0
     lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
           ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)
    ENDIF 
*B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[End]

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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  =gfOpenTable('RETLINE','RETLINES','SH','RETLINE')
  =gfOpenTable('RETHDR','RETHDR','SH','RETHDR')
  =gfOpenTable('POSHDR','POSHDR','SH','POSHDR')
  =gfOpenTable('POSLN','POSLN','SH','POSLN')
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

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
  *! Developer : WLD
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

  IF ('MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
    lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LCRPWIPSOR'),1)

    laOGObjCnt[lcWIPSorPo] = lcRpPODtSm = 'D'
    = lfOGShowGet('LCRPWIPSOR')

  ENDIF


  *!*************************************************************
  *! Name      : lfvSalSuDt
  *! Developer : WLD
  *! Date      : 04/17/2007
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
  = lfOGShowGet('LCRPSALSOR')



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
    IF gfSeek(lcFab,'FABRIC')
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
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *  DIMENSION laTempStru[61,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]
    DIMENSION laTempStru[63,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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

    SELECT STYLE
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
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    laTempstru[62,1] = 'StyTyp'
    laTempStru[62,2] = 'C'
    laTempStru[62,3] = 3
    laTempStru[62,4] = 0
    laTempStru[63,1] = 'StyPricA_C'
    laTempStru[63,2] = 'C'
    laTempStru[63,3] = 30
    laTempStru[63,4] = 0
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

    gfCrtTmp(lcWorkfile ,@laTempstru,,"",.F.)

    *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]
    SELECT(lcWorkfile)
    INDEX ON CTRANTYPE+TRANSNO+STYLE TAG (lcWorkfile)
    *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]

  ELSE     && CASE SIZE = NO

    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    * DIMENSION laTempStru[44,18] ,laTempStru1[24,18], laTempTran[1,18] , laTempsty[1,18]
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
*    DIMENSION laTempStru[76,18] ,laTempStru1[26,18], laTempTran[1,18] , laTempsty[1,18]
    DIMENSION laTempStru[77,18] ,laTempStru1[27,18], laTempTran[1,18] , laTempsty[1,18]
	*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    STORE '' TO laTempStru,laTempTran,laTempsty,laTempStru1
    PRIVATE lnFileCnt , lnFldRow

    *! BUILD STYLE COLORS TEMP -- BEGIN
    SELECT STYLE
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
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    laTempStru[29 ,1] = 'BOOK1'
    laTempStru[30 ,1] = 'BOOK2'
    laTempStru[31 ,1] = 'BOOK3'
    laTempStru[32 ,1] = 'BOOK4'
    laTempStru[33 ,1] = 'BOOK5'
    laTempStru[34 ,1] = 'BOOK6'
    laTempStru[35 ,1] = 'BOOK7'
    laTempStru[36 ,1] = 'BOOK8'
    laTempStru[37 ,1] = 'SHP1'
    laTempStru[38 ,1] = 'SHP2'
    laTempStru[39 ,1] = 'SHP3'
    laTempStru[40 ,1] = 'SHP4'
    laTempStru[41 ,1] = 'SHP5'
    laTempStru[42 ,1] = 'SHP6'
    laTempStru[43 ,1] = 'SHP7'
    laTempStru[44 ,1] = 'SHP8'
    laTempStru[45 ,1] = 'RET1'
    laTempStru[46 ,1] = 'RET2'
    laTempStru[47 ,1] = 'RET3'
    laTempStru[48 ,1] = 'RET4'
    laTempStru[49 ,1] = 'RET5'
    laTempStru[50 ,1] = 'RET6'
    laTempStru[51 ,1] = 'RET7'
    laTempStru[52 ,1] = 'RET8'
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    *-- Loop to get other dimensions of transaction included fields (Like master file)
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *FOR lnFileCnt = 1 TO 28
    FOR lnFileCnt = 1 TO 52
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      lnFldRow = ASCAN(laTempsty,laTempStru[lnFileCnt,1])
      IF lnFldRow > 0
        lnFldRow = ASUBSCRIPT(laTempsty,lnFldRow,1)
        laTempStru[lnFileCnt , 2 ] = laTempsty[lnFldRow , 2 ]
        laTempStru[lnFileCnt , 3 ] = laTempsty[lnFldRow , 3 ]
        laTempStru[lnFileCnt , 4 ] = laTempsty[lnFldRow , 4 ]
      ENDIF
    ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

    *-- Add Fields from PostDchq File.
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *lnIndex=29
    FOR J=29 TO 36
      laTempStru[J,2] = 'N'
      laTempStru[J,3] = 7
      laTempStru[J,4] = 0
    ENDFOR
    lnIndex=53
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    laTempStru[lnIndex,1] = 'COL1'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL2'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL3'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL4'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL5'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL6'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL7'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL8'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL9'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'COL10'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 6
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'STK9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'STK10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0


    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'ORD9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'ORD10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'WIP9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'WIP10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    lnIndex=lnIndex+1
    laTempstru[lnIndex,1] = 'StyTyp'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 3
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'StyPricA_C'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 30
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'BOOK9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'BOOK10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'SHP9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'SHP10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'RET9'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'RET10'
    laTempStru[lnIndex,2] = 'N'
    laTempStru[lnIndex,3] = 10
    laTempStru[lnIndex,4] = 0
    
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
    lnIndex=lnIndex+1
    laTempStru[lnIndex,1] = 'LINNUM'
    laTempStru[lnIndex,2] = 'C'
    laTempStru[lnIndex,3] = 24
    laTempStru[lnIndex,4] = 0
    
     STORE ' ' TO  laTempStru[lnIndex,7],laTempStru[lnIndex,8],;
                laTempStru[lnIndex,9],laTempStru[lnIndex,10],;
                laTempStru[lnIndex,11],laTempStru[lnIndex,12],;
                laTempStru[lnIndex,13],laTempStru[lnIndex,14],;
                laTempStru[lnIndex,15],laTempStru[lnIndex,16]
    STORE 0 TO    laTempStru[lnIndex,17] ,laTempStru[lnIndex,18]

    
	*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
    
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    gfCrtTmp(lcColorfile ,@laTempstru,,"",.F.)
    *gfCrtTmp(lcColorfile ,@laTempstru,,"CSTYMAJOR+STR(LINNUM,6)",lcColorfile)
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

    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    lnIndex=lnIndex+1
    laTempStru1[lnIndex,1] = 'ENTERED'
    laTempStru1[lnIndex,2] = 'D'
    laTempStru1[lnIndex,3] = 0
    laTempStru1[lnIndex,4] = 0

    lnIndex=lnIndex+1
    laTempStru1[lnIndex,1] = 'START'
    laTempStru1[lnIndex,2] = 'D'
    laTempStru1[lnIndex,3] = 0
    laTempStru1[lnIndex,4] = 0
    
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
    lnIndex=lnIndex+1
    laTempStru1[lnIndex,1] = 'LINNUM'
    laTempStru1[lnIndex,2] = 'C'
    laTempStru1[lnIndex,3] = 24
    laTempStru1[lnIndex,4] = 0
    
    STORE ' ' TO  laTempStru1[lnIndex,7],laTempStru1[lnIndex,8],;
                laTempStru1[lnIndex,9],laTempStru1[lnIndex,10],;
                laTempStru1[lnIndex,11],laTempStru1[lnIndex,12],;
                laTempStru1[lnIndex,13],laTempStru1[lnIndex,14],;
                laTempStru1[lnIndex,15],laTempStru1[lnIndex,16]
    STORE 0 TO    laTempStru1[lnIndex,17] ,laTempStru1[lnIndex,18]

	*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
    
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    gfCrtTmp(lcTransfile ,@laTempstru1,,"",.F.)

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

SET STEP ON 
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *DIMENSION loOgScroll.laCRTables[2]
  DIMENSION loOgScroll.laCRTables[3]
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  IF  XBYSIZE='Y'
    DIMENSION loOgScroll.laCRParams[6,2]
  ELSE
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *DIMENSION loOgScroll.laCRParams[10,2]
    DIMENSION loOgScroll.laCRParams[13,2]
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ENDIF

  loOGScroll.cCROrientation='L'
  IF  XBYSIZE='Y'
    loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  LCTMPFILE + ".DBF"
    loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  TMPSCAL + ".DBF"
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *  loOgScroll.lcOGLastForm ='ICSCUTSO'
    loOgScroll.laCRTables[3] = oAriaApplication.WorkDir +  StyWareh + ".DBF"
    lcReportFileName = 'ICSCTSLD'
    loOgScroll.lcOGLastForm = 'ICSCTSLD'
    lcReportFileName = loOgScroll.lcOGLastForm
    IF FILE(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\' + lcReportFileName + '.RPT' )
      lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID +'\'+ lcReportFileName +'.RPT'
    ENDIF
    loMainCr = CREATEOBJECT('CrystalRuntime.Application')
    loMain = CREATEOBJECT('CrystalRuntime.Report')
    loMain = loMainCr.OpenReport(lcReportFileName)
    loMain.DATABASE.TABLES.ITEM[1].Setlogoninfo ( oAriaApplication.ref4.WorkDir + LCTMPFILE + ".DBF")
    loMain.DATABASE.TABLES.ITEM[1].SetTableLocation ( oAriaApplication.ref4.WorkDir + LCTMPFILE + ".DBF",'','')
    loMain.DATABASE.TABLES.ITEM[2].Setlogoninfo ( oAriaApplication.ref4.WorkDir + TMPSCAL + ".DBF")
    loMain.DATABASE.TABLES.ITEM[2].SetTableLocation ( oAriaApplication.ref4.WorkDir + TMPSCAL + ".DBF",'','')
    loMain.DiscardSavedData()
    loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate
    loMain.CaseInsensitiveSQLData = .T.
    loMain.OpenSubreport ('ICSCTSLD_WH.rpt')
    loSub1 = loMain.OpenSubreport ('ICSCTSLD_WH.rpt')
    loSub1.DATABASE.TABLES.ITEM[1].Setlogoninfo ( oAriaApplication.ref4.WorkDir +  StyWareh + ".DBF" )
    loSub1.DATABASE.TABLES.ITEM[1].SetTableLocation ( oAriaApplication.ref4.WorkDir +  StyWareh + ".DBF" ,'','')
    loSub1.DATABASE.Verify() && verify database
    loSub1.DiscardSavedData()
    loSub1.ConvertDateTimeType = 1  && crConvertDateTimeToDate
    loSub1.CaseInsensitiveSQLData = .T.
    loMain.SAVE (oAriaApplication.ref4.WorkDir+lcTempCrFile+'.rpt')
    COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
    ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
    loMainCr  = NULL
    loMain    = NULL
    loSub1    = NULL
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ELSE
    loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  LCTMPTRANS+ ".DBF"
    loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  LCTMPCOLOR+ ".DBF"
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *loOgScroll.lcOGLastForm = 'ICSCUTSB'
    loOgScroll.laCRTables[3] = oAriaApplication.WorkDir +  StyWareh + ".DBF"
    lcReportFileName = 'ICSCTSBC'
    loOgScroll.lcOGLastForm  = lcReportFileName
    lcReportFileName = loOgScroll.lcOGLastForm
    IF FILE(oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID+'\' + lcReportFileName + '.RPT' )
      lcReportFileName = oAriaApplication.ReportHome + oAriaApplication.ActiveModuleID +'\'+ lcReportFileName +'.RPT'
    ENDIF
    loMainCr = CREATEOBJECT('CrystalRuntime.Application')
    loMain = CREATEOBJECT('CrystalRuntime.Report')
    loMain = loMainCr.OpenReport(lcReportFileName)
    loMain.DATABASE.TABLES.ITEM[1].Setlogoninfo ( oAriaApplication.ref4.WorkDir + LCTMPTRANS + ".DBF")
    loMain.DATABASE.TABLES.ITEM[1].SetTableLocation ( oAriaApplication.ref4.WorkDir + LCTMPTRANS + ".DBF",'','')
    loMain.DATABASE.TABLES.ITEM[2].Setlogoninfo ( oAriaApplication.ref4.WorkDir + LCTMPCOLOR + ".DBF")
    loMain.DATABASE.TABLES.ITEM[2].SetTableLocation ( oAriaApplication.ref4.WorkDir + LCTMPCOLOR + ".DBF",'','')
    loMain.DiscardSavedData()
    loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate
    loMain.CaseInsensitiveSQLData = .T.
    loMain.OpenSubreport ('ICSCTSBC_WH.rpt')
    loSub1 = loMain.OpenSubreport ('ICSCTSBC_WH.rpt')
    loSub1.DATABASE.TABLES.ITEM[1].Setlogoninfo ( oAriaApplication.ref4.WorkDir +  StyWareh + ".DBF" )
    loSub1.DATABASE.TABLES.ITEM[1].SetTableLocation ( oAriaApplication.ref4.WorkDir +  StyWareh + ".DBF" ,'','')

    loSub1.DATABASE.Verify() && verify database
    loSub1.DiscardSavedData()
    loSub1.ConvertDateTimeType = 1  && crConvertDateTimeToDate
    loSub1.CaseInsensitiveSQLData = .T.
    loMain.SAVE (oAriaApplication.ref4.WorkDir+lcTempCrFile+'.rpt')
    COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
    ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
    loMainCr  = NULL
    loMain    = NULL
    loSub1    = NULL
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      *loOgScroll.laCRParams[5,2]= 'Po/Ct'
      loOgScroll.laCRParams[5,2]= 'Po'
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    loOgScroll.laCRParams[11,1] = 'GRASHP'
    loOgScroll.laCRParams[11,2]= GRASHP

    loOgScroll.laCRParams[12,1] = 'GRARET'
    loOgScroll.laCRParams[12,2]= GRARET

    loOgScroll.laCRParams[13,1] = 'GRAVPO'
    loOgScroll.laCRParams[13,2]= GRAVPO
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *               Add an option to print styles with 0 quantities.
  *XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'

  *lcSylefields= "STYLE,SCALE, CDIVISION,SEASON,CSTYGROUP,CSTYMAJOR, DESC , "
  XFILTER=XFILTER+'.AND. IIF(llPrintZero,.T.,(TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0))'

  lcSylefields= "STYLE,SCALE, CDIVISION,SEASON,CSTYGROUP,CSTYMAJOR, DESC , Make , PriceA , PriceC , "
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  lcSylefields= lcSylefields+ " stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8, "
  lcSylefields= lcSylefields+ " ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8, "
  lcSylefields= lcSylefields+ " WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8, "
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *lcSylefields= lcSylefields+ " TOTSTK, TOTORD, TOTWIP "
  lcSylefields= lcSylefields+ " TOTSTK, TOTORD, TOTWIP, "
  lcSylefields= lcSylefields+ " SHP1,SHP2,SHP3,SHP4,SHP5,SHP6,SHP7,SHP8, "
  lcSylefields= lcSylefields+ " RET1,RET2,RET3,RET4,RET5,RET6,RET7,RET8, "
  lcSylefields= lcSylefields+ " TOTSHP, TOTRET "
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  
  
*: B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[Start]
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
*: B608212,1 MMT 08/08/2007 fix bug of error while restoring filters[End]
  
  
  SELECT &lcSylefields. FROM STYLE WHERE STYLE.STYLE <>'********' AND  &XFILTER  INTO CURSOR &FStyle. READWRITE

  SELECT (FStyle)
  GO TOP
  IF EOF()
    WAIT  WINDOW  'NO RECORDS SELECTED!!!'
    llDontPrn = .T.
    RETURN
  ENDIF
  INDEX ON STYLE TAG STYLE
  SET ORDER TO STYLE
  ********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************
  SELECT ORDHDR
  SELECT ORDLINE

  *B607847,1 11/30/2006 MMT Wrong account Name Displayed ,T20061129.0035[Start]
  *SET RELATION TO cOrdType + Order INTO ORDHDR
  SET RELATION TO cOrdType + ORDER INTO ORDHDR ADDITIVE
  *B607847,1 11/30/2006 MMT Wrong account Name Displayed ,T20061129.0035[END]

  lcSQLStmt  = "Select TRANCD,[ORDER],CORDLINE,CTKTNO,STYLE From CutPick(INDEX=CutOrd) "
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CutPick' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    SELECT CutPick
    =CURSORSETPROP("Buffering" ,3)
    IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
      SELECT CutPick
      INDEX ON TRANCD+ORDER+CORDLINE TAG CutPick
    ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules

    *-- If the "Style Purchase Order" or the "Point of Sale" modules is installed
    IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
      SELECT CutPick
      INDEX ON TRANCD+CTKTNO+STYLE TAG CutPick2
    ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules

    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *Create Temp table to hold the Warehouses total Stock per style/Color or style/Size
    CREATE TABLE (oAriaApplication.WorkDir+lcStoreTmp)(STYLE C(19),CSTYMAJOR C(19),cWarecode C(6),STK1 N(7),STK2 N(7),STK3 N(7),;
      STK4 N(7),STK5 N(7),STK6 N(7),STK7 N(7),STK8 N(7),STK9 N(7),STK10 N(7),LINNUM C(24))

    INDEX ON (STYLE+cWarecode) TAG (lcStoreTmp) OF (lcStoreTmp)
    INDEX ON (CSTYMAJOR+LINNUM +cWarecode) TAG 'StyMajWH' OF (lcStoreTmp) ADDITIVE
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *STORE 0 TO XCLRCNT,XSTK,XORD,XWIP
  DIMENSION XBOOK[10],XSHP[10],XRET[10]
  STORE 0 TO XCLRCNT,XSTK,XORD,XWIP,XBOOK,XSHP,XRET
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  STORE '' TO XCLR

  SELECT (FStyle)
  lnI=1
  xstyle=cstymajor
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *  xdesc=''
  xdesc = DESC
  m.StyTyp = IIF(MAKE,'DOM','IMP')
  m.StyPricA_C = 'PRICE A/C : '+ALLTRIM(STR(PriceA,13,2))+' / '+ALLTRIM(STR(PriceC,13,2))
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  
  *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
  STORE 0 TO lnClrCounter
  *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
LOCATE 
lnNumCount = 1
DO WHILE !EOF()
  xstyle =cstymajor
  COUNT FOR cstymajor = xstyle TO lnClrCounter
  lnLoops = CEILING(lnClrCounter/10)
  lnNumCount = 1
  LOCATE 
  =SEEK(SUBSTR(xstyle,1,lnClrLnGl))
  SCAN REST WHILE cstymajor =SUBSTR(xstyle,1,lnClrLnGl)
    xdesc = DESC
    m.StyTyp = IIF(MAKE,'DOM','IMP')
    m.StyPricA_C = 'PRICE A/C : '+ALLTRIM(STR(PriceA,13,2))+' / '+ALLTRIM(STR(PriceC,13,2))
    IF cstymajor<>xstyle
      SELECT (lcColorfile)
      APPEND BLANK
      REPLACE cstymajor WITH xstyle, DESC WITH xdesc
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      REPLACE StyTyp WITH m.StyTyp,;
        StyPricA_C WITH m.StyPricA_C,;
        LINNUM WITH PADR(cstymajor,19)+STR(lnNumCount,5)
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      FOR i =1 TO 10
        Z=ALLTRIM(STR(i))
        REPLACE COL&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
        *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
        
        *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
        *REPLACE SHP&z WITH XSHP(i), RET&z WITH XRET(i)
        *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
        
        *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      ENDFOR

      SELECT (FStyle)
      xstyle=cstymajor
      xdesc=DESC
      lnI=1
      STORE '' TO XCLR
      STORE 0 TO XSTK,XORD,XWIP
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      STORE 0 TO XSHP,XRET
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    ENDIF

    SELECT (FStyle)
    IF !(lnI>10) 	 OR xstyle <> cstymajor
      XCLR(lnI)   = SUBSTR(STYLE,lnNonMajPo)
      XSTK(lnI)   = TOTSTK
      XORD(lnI)   = TOTORD
      XWIP(lnI)   = TOTWIP
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      XSHP(lnI)   = TOTSHP
      XRET(lnI)   = TOTRET
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      lnI=lnI+1
    ELSE
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
      SELECT (lcColorfile)
      APPEND BLANK
      REPLACE cstymajor WITH xstyle, DESC WITH xdesc
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      REPLACE StyTyp WITH m.StyTyp,;
        StyPricA_C WITH m.StyPricA_C,;
        LINNUM WITH PADR(cstymajor,19)+STR(lnNumCount,5)
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      FOR i =1 TO 10
        Z=ALLTRIM(STR(i))
        REPLACE COL&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
        *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
        
		*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
        *REPLACE SHP&z WITH XSHP(i), RET&z WITH XRET(i)
        *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
        
        *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      ENDFOR

      SELECT (FStyle)
      SKIP -1
      xstyle=cstymajor
      xdesc=DESC
      lnI=1
      STORE '' TO XCLR
      STORE 0 TO XSTK,XORD,XWIP
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      STORE 0 TO XSHP,XRET
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

*!*	      XCLR(10) = 'OTHER'
*!*	      XSTK(10)=XSTK(10)+TOTSTK
*!*	      XORD(10)=XORD(10)+TOTORD
*!*	      XWIP(10)=XWIP(10)+TOTWIP
*!*	      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
*!*	      XSHP(10)=XSHP(10)+TOTSHP
*!*	      XRET(10)=XRET(10)+TOTRET
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
      *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
    lnNumCount = lnNumCount + 1
    ENDIF
  ENDSCAN
  IF (lnI-1) =< 10 AND !EMPTY(XCLR[1])
    SELECT (lcColorfile)
    APPEND BLANK
    REPLACE cstymajor WITH xstyle, DESC WITH xdesc
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    REPLACE StyTyp WITH m.StyTyp,;
      StyPricA_C WITH m.StyPricA_C,;
      LINNUM WITH PADR(cstymajor,19)+STR(lnNumCount,5)
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    FOR i =1 TO 10
      Z=ALLTRIM(STR(i))
      REPLACE COL&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
      
      *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
      *REPLACE SHP&z WITH XSHP(i), RET&z WITH XRET(i)
	 *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
	
      *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    ENDFOR
    SELECT (FStyle)
    xstyle=cstymajor
    xdesc=DESC
    lnI=1
    STORE '' TO XCLR
    STORE 0 TO XSTK,XORD,XWIP
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    STORE 0 TO XSHP,XRET
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ENDIF 
ENDDO   
  SELECT (lcColorfile)
  APPEND BLANK
  REPLACE cstymajor WITH xstyle, DESC WITH xdesc
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  REPLACE StyTyp WITH m.StyTyp ,;
    StyPricA_C WITH m.StyPricA_C,;
     LINNUM WITH PADR(cstymajor,19)+STR(lnNumCount,5)
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  FOR i =1 TO 10
    Z=ALLTRIM(STR(i))
    REPLACE COL&z WITH XCLR(i),STK&z WITH XSTK(i), ORD&z WITH XORD(i), WIP&z WITH XWIP(i)
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
    *REPLACE SHP&z WITH XSHP(i), RET&z WITH XRET(i)
    *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
    
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ENDFOR
  SELECT &lcColorfile
  
  *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
  *INDEX ON  cstymajor TAG &lcColorfile
  INDEX ON  CSTYMAJOR+LINNUM TAG &lcColorfile
  *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *Cut the invhdr shipdate filter from the lcrpexp to use it in filtering on the  INVLINE files
  lnDatPost = 0
  lnDataPos = ASCAN(laOgFxFlt,'INVHDR.SHIPDATE')
  IF lnDataPos > 0
    lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
    IF AT('BETWEEN(DTOS(INVHDR.SHIPDATE',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(DTOS(INVHDR.SHIPDATE',LCRPEXP)
    ENDIF

    IF lnDatPost > 0
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) )
      IF lnPos1 > 0
        lcShpDt= SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
        lcRPExp = STRTRAN(lcRPExp, lcShpDt, " .T. ")
        lcShpDt= "AND " + lcShpDt
      ELSE
        lcShpDt= SUBSTR(lcRpExp ,lnDatPost)
        lcRPExp = STRTRAN(lcRPExp, lcShpDt, " .T. ")
        lcShpDt= "AND " + lcShpDt
      ENDIF
    ENDIF
  ENDIF
  *Cut the Ordhdr EnterDate filter from the lcrpexp to use it in filtering on the  OrdLINE files
  lnDatPost = 0
  lnDataPos = ASCAN(laOgFxFlt,'ORDHDR.ENTERED')
  IF lnDataPos > 0
    lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
    IF AT('BETWEEN(DTOS(ORDHDR.ENTERED',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(DTOS(ORDHDR.ENTERED',LCRPEXP)
    ENDIF

    IF lnDatPost > 0
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) )
      IF lnPos1 > 0
        lcBokDt= SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
        lcRPExp = STRTRAN(lcRPExp, lcBokDt, " .T. ")
        lcBokDt= "AND " + lcBokDt
      ELSE
        lcBokDt= SUBSTR(lcRpExp ,lnDatPost)
        lcRPExp = STRTRAN(lcRPExp, lcBokDt, " .T. ")
        lcBokDt= "AND " + lcBokDt
      ENDIF
    ENDIF
  ENDIF
  *Cut the POShdr Complete Date filter from the lcrpexp to use it in filtering on the  POSLINE files
  lnDatPost = 0
  lnDataPos = ASCAN(laOgFxFlt,'POSHDR.COMPLETE')
  IF lnDataPos > 0
    lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
    IF AT('BETWEEN(DTOS(POSHDR.COMPLETE',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(DTOS(POSHDR.COMPLETE',LCRPEXP)
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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

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

  ldDate1=CTOD(SUBSTR(lcdate1,5,2)+"/"+SUBSTR(lcdate1,7,2)+"/"+SUBSTR(lcdate1,1,4))
  ldDate2=CTOD(SUBSTR(lcdate2,5,2)+"/"+SUBSTR(lcdate2,7,2)+"/"+SUBSTR(lcdate2,1,4))

  IF ln1or2='1'
    RETURN ldDate1
  ELSE
    RETURN ldDate2
  ENDIF

  *!*************************************************************
  *! Name      : lpCollecData_yes
  *! Developer : AYMAN MAHMOUD AHMED (AHM)
  *! Date      : 06/04/2006
  *! Purpose   : Collecting data in Temp. File - Breakdown In Size
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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  *variable for the Grand total of only Positive O.T.S numbers
  STORE  0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD,XGRDOTS
  STORE  0 TO XGRDPos && variable to print the total vendors Po's at the end of grand total.
  STORE "" TO lcStyleTyp , lcPricA_C
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

  SELECT (FSTYLE)
  *!*  GO TOP
  XDESC=SPACE(20)
  XSTYLE=SPACE(19)
  XCOLOR=SPACE(19)
  SCAN
    IF INKEY()=32
      RETURN
    ENDIF
    xStyle = STYLE
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    m.StyTyp = IIF(MAKE,'DOM','IMP')
    m.StyPricA_C = 'PRICE A/C : '+ALLTRIM(STR(PriceA,13,2))+' / '+ALLTRIM(STR(PriceC,13,2))
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    llHastran=.F.
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
          llHastran=.T.
          SELECT CUTTKTL
          SCAN FOR &XADDCUT
            SCATTER MEMVAR MEMO
            SELECT &LCWORKFILE
            
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

            
            m.VENDNAME=CONTRACTOR.CVENCOMP
            m.transno=m.po
            m.CTRANTYPE='1'
            m.CDIVISION=&FSTYLE..CDIVISION
            m.SEASON=&FSTYLE..SEASON
            m.CSTYGROUP=&FSTYLE..CSTYGROUP
            m.CSTYMAJOR=&FSTYLE..CSTYMAJOR
            m.DESC=&FSTYLE..DESC
            m.TOTSTK=&FSTYLE..TOTSTK
            m.TOTWIP=M.TOTQTY
            m.TOTORD=0
            FOR I=1 TO 8
              Z=STR(I,1)
              m.STK&Z=&FSTYLE..STK&Z
            ENDFOR
            FOR I=1 TO 8
              Z=STR(I,1)
              m.ORD&Z=0
            ENDFOR
            
            FOR I=1 TO 8
              Z=STR(I,1)
              m.WIP&Z= M.QTY&Z
            ENDFOR
            *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]
            IF !gfSeek('1'+m.transno+m.style,lcWorkfile,lcWorkfile)
              *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]
              APPEND BLANK
              m.vendor=IIF(!ISNULL(m.vendor),m.vendor,"     ")
              GATH MEMVAR MEMO
              *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[Start]
            ELSE
             
              FOR I=1 TO 8
                Z=STR(I,1)
                REPLACE  WIP&Z WITH WIP&Z+m.WIP&Z
                REPLACE  QTY&Z WITH QTY&Z+m.QTY&Z
              ENDFOR
              REPLACE TOTWIP WITH TOTWIP+m.TOTWIP
              REPLACE TOTQTY WITH TOTQTY+m.TOTQTY
              
            ENDIF
            *B607855,1 12/04/2006 MMT Double WIP in the special cut & sold report[End]
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
          llHastran=.T.
          SELECT POSLN
          SCAN FOR &XADD
            SCATTER MEMVAR MEMO
            m.VENDNAME=APVENDOR.CVENCOMP
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
            m.CTRANTYPE='2'
            m.CDIVISION=&FSTYLE..CDIVISION
            m.SEASON=&FSTYLE..SEASON
            m.CSTYGROUP=&FSTYLE..CSTYGROUP
            m.CSTYMAJOR=&FSTYLE..CSTYMAJOR
            m.DESC=&FSTYLE..DESC
            m.TOTSTK=&FSTYLE..TOTSTK
            m.TOTWIP=M.TOTQTY
            m.TOTORD=0
            FOR I=1 TO 8
              Z=STR(I,1)
              m.STK&Z=&FSTYLE..STK&Z
            ENDFOR
            FOR I=1 TO 8
              Z=STR(I,1)
              m.ORD&Z=0
            ENDFOR
            FOR I=1 TO 8
              Z=STR(I,1)
              m.WIP&Z=M.QTY&Z
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
    =gfSEEK(XSTYLE)
    SCAN  REST  WHILE STYLE = xStyle FOR   &lcLinescan
      llHastran=.T.
      SCATTER MEMVAR MEMO
      m.STATUS=IIF(ORDHDR->APPROVAL='DECLINE','D',ORDHDR->STATUS)
      m.CUSTOMER=CUSTOMER.BTNAME
      SELECT &LCWORKFILE
      m.transno=m.order
      m.CTRANTYPE='3'
      m.CDIVISION=&FSTYLE..CDIVISION
      m.SEASON=&FSTYLE..SEASON
      m.CSTYGROUP=&FSTYLE..CSTYGROUP
      m.CSTYMAJOR=&FSTYLE..CSTYMAJOR
      m.DESC=&FSTYLE..DESC
      m.TOTSTK=&FSTYLE..TOTSTK
      m.TOTORD=M.TOTQTY
      m.TOTWIP=0
      FOR I=1 TO 8
        Z=STR(I,1)
        m.STK&Z=&FSTYLE..STK&Z
      ENDFOR

      FOR I=1 TO 8
        Z=STR(I,1)
        m.ORD&Z=M.QTY&Z
      ENDFOR
      FOR I=1 TO 8
        Z=STR(I,1)
        m.WIP&Z=0
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
        TOTWIP WITH 0,;
        StyTyp WITH m.StyTyp ,;
	    StyPricA_C with m.StyPricA_C 

      FOR I=1 TO 8
        Z=STR(I,1)
        REPLACE   STK&Z WITH &FSTYLE..STK&Z
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
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *-Get stock per style/size warehouse
    =lfStoreStk()
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    SELECT(FSTYLE)
  ENDSCAN

  IF USED('CUTPICK')
    USE IN CUTPICK
  ENDIF    && End of IF USED('CUTPICK')


  SELECT (LCWORKFILE)
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"
*B611688,1 Heba 25-10-2018 - Prepare SaaS Environment [Begin]
  *USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0 SHARED
USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0 Exclusive
*B611688,1 Heba 25-10-2018 - Prepare SaaS Environment [End]
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

  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  IF USED(lcStoreTmp)
    SELECT (lcStoreTmp)
    IF loOgScroll.FileExist(oAriaApplication.WorkDir +  StyWareh+ ".DBF" )
      ERASE (oAriaApplication.WorkDir +  StyWareh+ ".DBF" )
    ENDIF
    COPY TO oAriaApplication.WorkDir +  StyWareh + ".DBF" WITH CDX
  ENDIF
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

  =lfAdjustCRSettings()


  *!*************************************************************
  *! Name      : lpCollecData_no
  *! Developer : AYMAN MAHMOUD AHMED (AHM)
  *! Date      : 06/04/2006
  *! Purpose   : Collecting data in Temp. File - No Breakdown In Size
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
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  GRAVPO=0
  GRABOOK=0
  GRASHP=0
  GRARET=0
  GRAVPO=0
  lnVndPOQty = 0
  lnGrRet = 0
  lnGrShp = 0
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  SCAN
    IF INKEY()=32
      RETURN
    ENDIF
    xStyle = STYLE
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xStyle)  NOWAIT

    GRASTK=GRASTK+TOTSTK
    GRAWIP=GRAWIP+TOTWIP
    GRAORD=GRAORD+TOTORD
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    GRASHP=GRASHP+TOTSHP
    GRARET=GRARET+TOTRET
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

    llHastran=.F.
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
          llHastran=.T.
          SELECT CUTTKTL
          SCAN FOR &XADDCUT
            SCATTER MEMVAR MEMO
            STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10
            IF m.trancd<>'1'
              m.totqty=m.totqty*-1
            ENDIF
            m.transno=m.po
            m.CTRANTYPE='1'
            m.VENDNAME=CONTRACTOR.CVENCOMP
            DO CASE
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 1 &&=&lccolorfile..col1
                m.qcol1= m.totqty
                m.LINNUM = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 2 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
                m.qcol2= m.totqty
                m.LINNUM = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 3 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
                m.qcol3= m.totqty
                m.LINNUM = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 4 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
                m.qcol4= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 5 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
                m.qcol5= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 6 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
                m.qcol6= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 7 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
                m.qcol7= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 8 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
                m.qcol8= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 9 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
                m.qcol9= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 10 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
                m.qcol10= m.totqty
                m.linnum = &lccolorfile..LINNUM
*!*                OTHERWISE
*!*                  m.qcol10=  m.totqty
*!*                  REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile
            ENDCASE

            SELECT &lcTransfile
            m.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)

            APPEND BLANK
            m.vendor=IIF(!ISNULL(m.vendor),m.vendor,"     ")
            GATH MEMVAR MEMO
            *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]

           * lnVndPOQty =   lnVndPOQty + (m.qcol1+m.qcol2+m.qcol3+m.qcol4+m.qcol5+m.qcol6+m.qcol7+m.qcol8+m.qcol9+m.qcol10)
            *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
          llHastran=.T.
          SELECT POSLN
          SCAN FOR &XADD
            SCATTER MEMVAR MEMO
            STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10

            IF m.trancd<>'1'
              m.totqty=m.totqty*-1
            ENDIF
            m.transno=m.po
            m.CTRANTYPE='2'
            m.VENDNAME=APVENDOR.CVENCOMP
            DO CASE
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 1 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col1
                m.qcol1= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 2 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
                m.qcol2= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 3 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
                m.qcol3= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 4 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
                m.qcol4= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 5 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
                m.qcol5= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 6 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
                m.qcol6= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 7 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
                m.qcol7= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 8 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
                m.qcol8= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 9 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
                m.qcol9= m.totqty
                m.linnum = &lccolorfile..LINNUM
              CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 10 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
                m.qcol10= m.totqty
                m.linnum = &lccolorfile..LINNUM
*!*                OTHERWISE
*!*                  m.qcol10=  m.totqty
*!*                  REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile
            ENDCASE
            SELECT &lcTransfile
            m.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)

            APPEND BLANK
            GATH MEMVAR MEMO
            *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
            *lnVndPOQty =   lnVndPOQty + IIF(TranCd = '1',1,-1) * (m.qcol1+m.qcol2+m.qcol3+m.qcol4+m.qcol5+m.qcol6+m.qcol7+m.qcol8+m.qcol9+m.qcol10)
            lnVndPOQty =   lnVndPOQty + (m.qcol1+m.qcol2+m.qcol3+m.qcol4+m.qcol5+m.qcol6+m.qcol7+m.qcol8+m.qcol9+m.qcol10)
            *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
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
    =gfSEEK(XSTYLE)

    SCAN  REST  WHILE STYLE = xStyle FOR   &lcLinescan
      llHastran=.T.
      SCATTER MEMVAR MEMO
      STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10
      m.CUSTOMER=CUSTOMER.BTNAME
      DO CASE
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 1 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col1
          m.qcol1=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 2 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col2
          m.qcol2=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 3 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col3
          m.qcol3=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 4 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col4
          m.qcol4=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 5 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col5
          m.qcol5=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 6 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col6
          m.qcol6=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 7 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col7
          m.qcol7=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 8 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col8
          m.qcol8=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 9 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col9
          m.qcol9=totqty
          m.linnum = &lccolorfile..LINNUM
        CASE lfFindColor(SUBSTR(m.Style,lnNonMajPo),SUBSTR(M.STYLE,1,lnMajLen)) = 10 &&SUBSTR(m.Style,lnNonMajPo)=&lccolorfile..col10
          m.qcol10=totqty
          m.linnum = &lccolorfile..LINNUM
*!*          OTHERWISE
*!*            m.qcol10=  m.totqty
*!*            REPLACE &lccolorfile..STYLE WITH 'OTHER' IN &lccolorfile

      ENDCASE
      SELECT &lcTransfile
      m.CSTYMAJOR= SUBSTR(M.STYLE,1,lnMajLen)
      m.transno=m.order
      m.CTRANTYPE='3'

      APPEND BLANK
      GATH MEMVAR MEMO
    ENDSCAN
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    *sum the booked ,shipped and returned qty.
    IF USED(BookTemp)
      =lfGetBookQ()
    ENDIF
    IF USED(ShipTemp)
      =lfGetShipQ()
    ENDIF
    IF USED(RETTEMP)
      =lfGetRtrnQ()
    ENDIF
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
    
    IF !llHastran
      SELECT &lcTransfile
      APPEND BLANK
      REPLACE CSTYMAJOR WITH SUBSTR(xstyle,1,lnMajLen),transno WITH '*****',CTRANTYPE WITH '0', LINNUM with &FSTYLE..CSTYMAJOR+STR(lfFindColor(SUBSTR(XSTYLE,lnNonMajPo),SUBSTR(XSTYLE,1,lnMajLen)),5)
    ENDIF
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
    =lfStoreStk()
    *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
  ENDSCAN
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  GRAVPO =   lnVndPOQty

  SELECT (lcColorfile)
  SCAN
    IF !llRtnSty
      REPLACE RET1 WITH 0,RET2 WITH 0,RET3 WITH 0,RET4 WITH 0,RET5 WITH 0,RET6 WITH 0,RET7 WITH 0,RET8 WITH 0,RET9 WITH 0,RET10 WITH 0
    ELSE
      lnGrRet = lnGrRet + (RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8+RET9+RET10)
    ENDIF
    lnGrShp  = lnGrShp + (Shp1+Shp2+Shp3+Shp4+Shp5+Shp6+Shp7+Shp8+Shp9+Shp10)
  ENDSCAN
  GRARET = IIF(llRtnSty,lnGrRet,0)
  GRASHP = lnGrShp &&IIF(lnGrShp <>0,lnGrShp ,GRASHP)
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]

  IF USED('CUTPICK')
    USE IN CUTPICK
  ENDIF    && End of IF USED('CUTPICK')


  SELECT (lcColorfile )
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" IN 0 SHARED
  INDEX ON CSTYMAJOR+LINNUM TAG &LCTMPcolor

  SELECT (lcTransfile )
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF"
  *XX
  *USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 SHARED
  USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 Excl 
  *XX

  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [Begin]
  IF USED(lcStoreTmp)
    SELECT (lcStoreTmp)
    IF loOgScroll.FileExist(oAriaApplication.WorkDir +  StyWareh+ ".DBF" )
      ERASE (oAriaApplication.WorkDir +  StyWareh+ ".DBF" )
    ENDIF
    COPY TO oAriaApplication.WorkDir +  StyWareh  + ".DBF"
  ENDIF
  *C200765,1 WLD Convert Custom Special Cut & Sold for ERIC JAVITS, INC to Aria4XP 04/17/2007 [End]
*INDEX ON CSTYMAJOR+STR(LINNUM,5) TAG &LCTMPcolor
  SELECT &LCTMPtrans
  DO CASE
    CASE  XPRTWIP='S' .AND.  XPRTORD='S'
      INDEX ON CSTYMAJOR+linnum+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE XPRTWIP='S' .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+linnum+DTOS(COMPLETE)+TRANSNO +STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE  XPRTWIP='S' .AND.  XORDSORT='A'
      INDEX ON CSTYMAJOR+linnum+ACCOUNT+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE  XPRTWIP='S' .AND.  XORDSORT='O'
      INDEX ON CSTYMAJOR+linnum+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
    CASE XWIPSORT='D'   .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+linnum+DTOS(COMPLETE)+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='D'   .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+linnum+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='D'   .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+linnum+ACCOUNT+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='D'   .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+linnum+ORDER+DTOS(COMPLETE)+PO +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'  .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+linnum+VENDOR+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'   .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+linnum+VENDOR+PO+DTOS(COMPLETE)+ORDER +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'  .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+linnum+VENDOR+PO +ACCOUNT+ORDER+DTOS(COMPLETE) +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='F'    .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+linnum+VENDOR+PO +TRANCD+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'  .AND. XPRTORD='S'
      INDEX ON CSTYMAJOR+linnum+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'    .AND. XORDSORT='D'
      INDEX ON CSTYMAJOR+linnum+PO +TRANCD+DTOS(COMPLETE)+ORDER+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P'  .AND. XORDSORT='A'
      INDEX ON CSTYMAJOR+linnum+PO +TRANCD+ACCOUNT+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
    CASE   XWIPSORT='P' .AND. XORDSORT='O'
      INDEX ON CSTYMAJOR+linnum+PO +TRANCD+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
  ENDCASE
  GRAOTS=GRASTK+GRAWIP-GRAORD
  =lfAdjustCRSettings()

  RETURN .T.

  *!*************************************************************
  *! Name      : lfGetBookQ
  *! Developer : Walid Hamed (WLD)
  *! Date      : 04/17/2007
  *! Purpose   : To calculate the booked qty
  *!*************************************************************
  *! Exaple   : =lfGetBookQ()
  *!*************************************************************
FUNCTION lfGetBookQ
  PRIVATE lnAlias
  DIMENSION laBookQty[10]
  STORE 0 TO laBookQty

  lnAlias = SELECT(0)
  SELECT &BookTemp
  IF SEEK(XSTYLE)
    SCAN REST WHILE STYLE+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) = XSTYLE
      DO CASE
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 1 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col1
          *laBookQty(1) = laBookQty(1) + TotBook
          REPLACE BOOK1 WITH BOOK1 + &BookTemp..TotBook IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 2 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col2
          REPLACE BOOK2 WITH BOOK2 + &BookTemp..TotBook IN (lccolorfile)
*          laBookQty(2) = laBookQty(2) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 3 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col3
        REPLACE BOOK3 WITH BOOK3 + &BookTemp..TotBook IN (lccolorfile)
 *         laBookQty(3) = laBookQty(3) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 4 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col4
          REPLACE BOOK4 WITH BOOK4 + &BookTemp..TotBook IN (lccolorfile)
  *        laBookQty(4) = laBookQty(4) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 5 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col5
          REPLACE BOOK5 WITH BOOK5 + &BookTemp..TotBook IN (lccolorfile)
        *  laBookQty(5) = laBookQty(5) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 6 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col6
          REPLACE BOOK6 WITH BOOK6 + &BookTemp..TotBook IN (lccolorfile)
    *      laBookQty(6) = laBookQty(6) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 7 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col7
          REPLACE BOOK7 WITH BOOK7 + &BookTemp..TotBook IN (lccolorfile)
        *  laBookQty(7) = laBookQty(7) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 8 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col8
          REPLACE BOOK8 WITH BOOK8 + &BookTemp..TotBook IN (lccolorfile)
*          laBookQty(8) = laBookQty(8) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 9 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col9
          REPLACE BOOK9 WITH BOOK9 + &BookTemp..TotBook IN (lccolorfile)
   *       laBookQty(9) = laBookQty(9) + TotBook
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 10 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col10
          REPLACE BOOK10 WITH BOOK10 + &BookTemp..TotBook IN (lccolorfile)
    *      laBookQty(10) = laBookQty(10) + TotBook
*!*          OTHERWISE
*!*            laBookQty(10) = laBookQty(10) + TotBook

      ENDCASE
    ENDSCAN
  ENDIF
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
*  REPLACE BOOK1 WITH laBOOKQty(1),;
    BOOK2 WITH laBOOKQty(2),;
    BOOK3 WITH laBOOKQty(3),;
    BOOK4 WITH laBOOKQty(4),;
    BOOK5 WITH laBOOKQty(5),;
    BOOK6 WITH laBOOKQty(6),;
    BOOK7 WITH laBOOKQty(7),;
    BOOK8 WITH laBOOKQty(8),;
    BOOK9 WITH laBOOKQty(9),;
    BOOK10 WITH laBOOKQty(10)   IN (lccolorfile)
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
  SELECT(lnAlias)
  *!*************************************************************
  *! Name      : lfGetShipQ
  *! Developer : Walid Hamed (WLD)
  *! Date      : 04/17/2007
  *! Purpose   : To calculate the shipped qty
  *!*************************************************************
  *! Exaple   : =lfGetShipQ()
  *!*************************************************************
FUNCTION lfGetShipQ

  PRIVATE lnAlias
  DIMENSION laShipQty[10]
  STORE 0 TO laShipQty
  lnAlias = SELECT(0)
  SELE &ShipTemp
  IF SEEK(XSTYLE)
    SCAN REST WHILE STYLE+Invoice+DTOS(InvDate) = XSTYLE
      DO CASE
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 1  &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col1
          REPLACE SHP1 WITH SHP1 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(1) = laShipQty(1) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 2 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col2
            REPLACE SHP2 WITH SHP2 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(2) = laShipQty(2) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 3 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col3
          REPLACE SHP3 WITH SHP3 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(3) = laShipQty(3) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 4  &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col4
          REPLACE SHP4 WITH SHP4 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(4) = laShipQty(4) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 5 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col5
          REPLACE SHP5 WITH SHP5 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(5) = laShipQty(5) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 6 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col6
          REPLACE SHP6 WITH SHP6 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(6) = laShipQty(6) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 7  &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col7
          REPLACE SHP7 WITH SHP7 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(7) = laShipQty(7) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 8 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col8
          REPLACE SHP8 WITH SHP8 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(8) = laShipQty(8) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 9 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col9
          REPLACE SHP9 WITH SHP9 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(9) = laShipQty(9) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 10 &&SUBSTR(STYLE,lnNonMajPo)=&lccolorfile..col10
          REPLACE SHP10 WITH SHP10 + &ShipTemp..TotQty IN (lccolorfile)
*          laShipQty(10) = laShipQty(10) + TotQty
*!*          OTHERWISE
*!*            laShipQty(10) = laShipQty(10) + TotQty
      ENDCASE
    ENDSCAN
  ENDIF
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
*!*    REPLACE SHP1 WITH laShipQty(1),;
*!*      SHP2 WITH laShipQty(2),;
*!*      SHP3 WITH laShipQty(3),;
*!*      SHP4 WITH laShipQty(4),;
*!*      SHP5 WITH laShipQty(5),;
*!*      SHP6 WITH laShipQty(6),;
*!*      SHP7 WITH laShipQty(7),;
*!*      SHP8 WITH laShipQty(8),;
*!*      SHP9 WITH laShipQty(9),;
*!*      SHP10 WITH laShipQty(10)   IN (lccolorfile)
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfGetRtrnQ
  *! Developer : Walid Hamed (WLD)
  *! Date      : 04/17/2007
  *! Purpose   : To calculate the return qty
  *!*************************************************************
  *! Called from :
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : =lfGetRtrnQ()
  *!*************************************************************
FUNCTION lfGetRtrnQ
  PRIVATE lnAlias
  DIMENSION laRtrnQty[10]
  STORE 0 TO laRtrnQty

  lnAlias = SELECT(0)
  SELECT (RETTEMP)
  IF SEEK(XSTYLE)

    SCAN REST WHILE STYLE+crmemo+DTOS(crdate) = XSTYLE
      DO CASE
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 1 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col1
 *         laRtrnQty(1) = laRtrnQty(1) + TotQty
           REPLACE RET1 WITH RET1 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 2 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col2
*          laRtrnQty(2) = laRtrnQty(2) + TotQty
           REPLACE RET2 WITH RET2 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 3 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col3
*          laRtrnQty(3) = laRtrnQty(3) + TotQty
           REPLACE RET3 WITH RET3 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 4 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col4
*          laRtrnQty(4) = laRtrnQty(4) + TotQty
           REPLACE RET4 WITH RET4 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 5  &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col5
*          laRtrnQty(5) = laRtrnQty(5) + TotQty
          REPLACE RET5 WITH RET5 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 6 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col6
*          laRtrnQty(6) = laRtrnQty(6) + TotQty
          REPLACE RET6 WITH RET6 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 7 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col7
*          laRtrnQty(7) = laRtrnQty(7) + TotQty
          REPLACE RET7 WITH RET7 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 8 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col8
           REPLACE RET8 WITH RET8 + &RETTEMP..TotQty IN (lccolorfile)
*          laRtrnQty(8) = laRtrnQty(8) + TotQty
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 9 &&SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col9
*          laRtrnQty(9) = laRtrnQty(9) + TotQty
          REPLACE RET9 WITH RET9 + &RETTEMP..TotQty IN (lccolorfile)
        CASE lfFindColor(SUBSTR(Style,lnNonMajPo),SUBSTR(STYLE,1,lnMajLen)) = 10 && SUBSTR(STYLE,lnNonMajPo) = &lccolorfile..col10
          REPLACE RET10 WITH RET10 + &RETTEMP..TotQty IN (lccolorfile)
*          laRtrnQty(10) = laRtrnQty(10) + TotQty
*!*          OTHERWISE
*!*            laRtrnQty(10) = laRtrnQty(10) + TotQty
      ENDCASE
    ENDSCAN
  ENDIF
  
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
*!*    REPLACE RET1 WITH laRtrnQty(1),;
*!*      RET2 WITH laRtrnQty(2),;
*!*      RET3 WITH laRtrnQty(3),;
*!*      RET4 WITH laRtrnQty(4),;
*!*      RET5 WITH laRtrnQty(5),;
*!*      RET6 WITH laRtrnQty(6),;
*!*      RET7 WITH laRtrnQty(7),;
*!*      RET8 WITH laRtrnQty(8),;
*!*      RET9 WITH laRtrnQty(9),;
*!*      RET10 WITH laRtrnQty(10)   IN (lccolorfile)
  SELECT(lnAlias)
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
  *--End of lfGetRtrnQ.

  *!*************************************************************
  *! Name      : lfStoreStk
  *! Developer : Walid Hamed (WLD)
  *! Date      : 04/17/2007
  *! Purpose   : Get Stock for the style/color in every Warehouse
  *!*************************************************************
  *! Exaple   : =lfStoreStk()
  *!*************************************************************

FUNCTION lfStoreStk

  PRIVATE lnAlias,LCI,LCR,lnStockClr
  lnAlias  = SELECT(0)

  IF gfSeek(XSTYLE,'STYDYE')
    SELECT STYDYE
    SCAN REST WHILE STYLE+cWarecode+Dyelot = XSTYLE
      IF XBYSIZE <> 'Y'
        SELECT(lcStoreTmp)
        SET ORDER TO StyMajWH
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
*!*          IF !gfSeek(&lccolorfile..CSTYMAJOR+STYDYE.cWarecode,lcStoreTmp)
*!*            APPEND BLANK
*!*            REPLACE STYLE 	 WITH XSTYLE ,;
*!*              CWARECODE  WITH STYDYE.cWarecode	,;
*!*              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen)
*!*          ENDIF
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
        DO CASE
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 1 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col1
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]
            
            REPLACE &lcStoreTmp..STK1 WITH &lcStoreTmp..STK1 + STYDYE.TOTSTK
            
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 2 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col2
          
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[ENd]

            REPLACE &lcStoreTmp..STK2 WITH &lcStoreTmp..STK2 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 3 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col3
          
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK3 WITH &lcStoreTmp..STK3 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 4 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col4
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK4 WITH &lcStoreTmp..STK4 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 5 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col5
          
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK5 WITH &lcStoreTmp..STK5 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 6 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col6
          
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK6 WITH &lcStoreTmp..STK6 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 7 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col7
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[ENd]

            REPLACE &lcStoreTmp..STK7 WITH &lcStoreTmp..STK7 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 8 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col8
          
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK8 WITH &lcStoreTmp..STK8 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 9 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col9
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[End]

            REPLACE &lcStoreTmp..STK9 WITH &lcStoreTmp..STK9 + STYDYE.TOTSTK
          CASE lfFindColor(SUBSTR(XStyle,lnNonMajPo),SUBSTR(XStyle,1,lnMajLen)) = 10 &&SUBSTR(XStyle,lnNonMajPo)=&lccolorfile..col9
            *: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]
            IF !Seek(&lccolorfile..CSTYMAJOR+&lccolorfile..LINNUM+STYDYE.cWarecode,lcStoreTmp)
              APPEND BLANK
              REPLACE STYLE    WITH XSTYLE ,;
              CWARECODE  WITH STYDYE.cWarecode  ,;
              CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen),;
              LINNUM     WITH &lccolorfile..LINNUM
            ENDIF
			*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[eND]

            REPLACE &lcStoreTmp..STK10 WITH &lcStoreTmp..STK10 + STYDYE.TOTSTK
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[Start]            
*!*            OTHERWISE
*!*              REPLACE &lcStoreTmp..STK10 WITH &lcStoreTmp..STK10 + STYDYE.TOTSTK
*: C200765,1 04/17/2007 MMT Include all customs and fixes  of Custom Special Cut & Sold[eND]
        ENDCASE

      ELSE
        SELECT(lcStoreTmp)
        IF !Seek(XStyle+STYDYE.cWarecode,lcStoreTmp)
          APPEND BLANK
          REPLACE STYLE 	 WITH XSTYLE			,;
            CWARECODE  WITH STYDYE.cWarecode ,;
            CSTYMAJOR  WITH SUBSTR(XSTYLE,1,lnMajLen)
        ENDIF
        FOR R = 1 TO 8
          LCR = ALLTRIM(STR(R))
          lnStkSz = EVALUATE('STYDYE.STK'+LCR)
          REPLACE STK&LCR WITH lnStkSz IN (lcStoreTmp)
        ENDFOR
      ENDIF
    ENDSCAN
  ENDIF
  SELECT(lnAlias)
  *--End of function lfStoreStk.

FUNCTION lfFindColor
PARAMETERS lcColorFind,lcStyMaj

STORE 0 TO lnRetvalue 
lcOlderAlias =SELECT()

SELECT (lcColorfile)
=SEEK(lcStyMaj)

SCAN REST WHILE CSTYMAJOR+LINNUM = lcStyMaj
  DO CASE
    CASE lcColorFind=&lccolorfile..col1
      lnRetvalue = 1
      EXIT 
    CASE lcColorFind=&lccolorfile..col2
      lnRetvalue = 2
      EXIT 

    CASE lcColorFind=&lccolorfile..col3
      lnRetvalue = 3
      EXIT 

    CASE lcColorFind=&lccolorfile..col4
      lnRetvalue = 4
      EXIT 

    CASE lcColorFind=&lccolorfile..col5
      lnRetvalue = 5
      EXIT 

    CASE lcColorFind=&lccolorfile..col6
      lnRetvalue = 6
      EXIT 

    CASE lcColorFind=&lccolorfile..col7
      lnRetvalue = 7
      EXIT 

    CASE lcColorFind=&lccolorfile..col8
      lnRetvalue = 8
      EXIT 

    CASE lcColorFind=&lccolorfile..col9
      lnRetvalue = 9
      EXIT 

    CASE lcColorFind=&lccolorfile..col10
      lnRetvalue = 10
      EXIT 
  ENDCASE
ENDSCAN 
SELECT(lcOlderAlias)
RETURN lnRetvalue 

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/08/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608212
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

