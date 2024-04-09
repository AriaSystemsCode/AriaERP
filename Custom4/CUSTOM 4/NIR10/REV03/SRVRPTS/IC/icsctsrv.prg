*:***************************************************************************
*: Program file  : icsctsrv.PRG
*: Program desc. : Custom Special Cut & Sold For Revue Based on standard
*: TRACK NO      : C201061[T20080422.0042]
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
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName


*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.Description = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")

  LOCAL lcCurrentProcedure
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)

  oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  oAriaEnvironment.Report.gcAct_Appl = "IC"
  oAriaEnvironment.ActiveModuleID  = "IC"

  PUBLIC gcAct_Appl 
  gcAct_Appl = "IC"

  oariaenvironment.activeModuleID = 'IC'

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF


  *!*  IF llOgFltCh
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'ordhdr','ordhdr','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'ORDLINE','ORDLINES','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'CUSTOMER','CUSTOMER','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'CUTPICK','cutpick','SH',,,,.T.)
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'STYLE','STYLE','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'SCALE','SCALE','SH')
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'POSLN','POSLN','SH','CUTTKTL',,,.T.)
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'POSLN','POSLN','SH','POSLN',,,.T.)
   oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'APVENDOR','VENCODE','SH')
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

  STORE 0 TO GRAOTS,GRAORD,GRASTK,GRAWIP
  STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
  =lfChkStrct()
  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
    llExtSize = oAriaEnvironment.setups.getSetting('M_USEEXSSC')
    
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    llExtSize = gfGetMemVar('M_USEEXSSC')
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
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
  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
    FStyle= oAriaEnvironment.Cursors.GetCursorTempName()
    WORKTEMP = oAriaEnvironment.Cursors.GetCursorTempName()
    
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    FStyle= loOGScroll.gfTempName()
    WORKTEMP = loOGScroll.gfTempName()
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
  IF !USED('CONTRACTOR')
  
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'APVENDOR','VENCODE','SH','CONTRACTOR')
      
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      gfOpenTable('APVENDOR','VENCODE','SH','CONTRACTOR')
    ENDIF
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  ENDIF

  lcSQLStmtC ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,POSLN.CRSESSION,"
  lcSQLStmtC =lcSQLStmtC + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
  lcSQLStmtC =lcSQLStmtC + " posln.SCALE,posln.TRANCD "
  lcSQLStmtC =lcSQLStmtC + " ,MFGOPRHD.cContCode AS VENDOR  From "
  lcSQLStmtC =lcSQLStmtC + " posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
  lcSQLStmtC =lcSQLStmtC + " LEFT OUTER JOIN MFGOPRHD(INDEX=TktOper) ON MFGOPRHD.cimtyp='M' AND POSLN.PO=MFGOPRHD.CTKTNO AND MFGOPRHD.lInHouse = 0 "
  lcSQLStmtC =lcSQLStmtC+" Where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'U' AND POSLN.CINVTYPE='0001' "


  lcSQLStmtP ="  Select poshdr.STATUS,posln.STYLE,posln.PO,posln.ACCOUNT,poshdr.COMPLETE,POSLN.CRSESSION,"
  lcSQLStmtP =lcSQLStmtP + " posln.QTY1,posln.QTY2,posln.QTY3,posln.QTY4,posln.QTY5,posln.QTY6,posln.QTY7,posln.QTY8,posln.TOTQTY, "
  lcSQLStmtP =lcSQLStmtP + " posln.SCALE,posln.TRANCD "
  lcSQLStmtP =lcSQLStmtP +" ,POSHDR.VENDOR  From "
  lcSQLStmtP =lcSQLStmtP +" posln(index=poslnS) INNER JOIN poshdr(INDEX=poshdr)  on poshdr.cBusDocu=posln.cBusDocu AND poshdr.cStyType = posln.cStyType and poshdr.po=posln.po  "
  lcSQLStmtP =lcSQLStmtP +" where POSLN.cBusDocu = 'P' AND POSLN.cStyType = 'P' AND POSLN.CINVTYPE='0001' "
  IF !EMPTY(XPLDATE)

    lcSQLStmtc1 =" AND poshdr.STATUS not in ('S','X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'"
    lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  and POSLN.COMPLETE BETWEEN '"+DTOC(XPLDATE)+"' and '"+DTOC(XPHDATE)+"'"
  ELSE
    lcSQLStmtc1 =" AND poshdr.STATUS not in ('S','X', 'C') AND posln.TOTQTY<>0  "
    lcSQLStmtp1 =" AND poshdr.STATUS not in ('X', 'C') AND posln.TOTQTY<>0  "
  ENDIF

  XADD = "PosLn.Po<>'*****' "
  DO CASE
  CASE XALLOCAT = 'L'
    XADD = XADD +" AND  SEEK('2'+Po+style,'CutPick') "
  CASE XALLOCAT = 'N'
    XADD = XADD +" AND  !SEEK('2'+Po+style,'CutPick') "
  ENDCASE

  XADDCUT = "CUTTKTL.Po<>'*****' "
  DO CASE
  CASE XALLOCAT = 'L'
    XADDCUT = XADDCUT +" AND  SEEK('1'+Po+style,'CutPick') "
  CASE XALLOCAT = 'N'
    XADDCUT = XADDCUT +" AND  !SEEK('1'+Po+style,'CutPick') "
  ENDCASE


  XADDORD = ''
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF 'MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C',('MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules),;
        ('MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules))
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
    DO CASE
    CASE XALLOCAT = 'L'
      XADDORD = " .AND.  ( SEEK('1'+Order+STR(LineNo,6),'CutPick') .OR.  SEEK('2'+Order+STR(LineNo,6),'CutPick')) "
    CASE XALLOCAT = 'N'
      XADDORD = " .AND.  ( !SEEK('1'+Order+STR(LineNo,6),'CutPick') .AND.  !SEEK('2'+Order+STR(LineNo,6),'CutPick')) "
    ENDCASE
  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules

  IF !EMPTY(XSLDATE)
    lcLinescan=" BETWEEN(Complete , xSLDate , xSHDate)   .AND. "
    lcLinescan=lcLinescan+" ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O'  .AND. "
    lcLinescan= lcLinescan+ " IIF(lcRPAloct = 'L' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'N' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
    lcLinescan= lcLinescan+  " " + XADDORD +" "+lcCustAcnt
  ELSE
    lcLinescan= " ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. cOrdType = 'O' .AND. "
    lcLinescan= lcLinescan+" IIF(lcRPAloct = 'L' , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = 'N' , EMPTY(ORDLINE.PIKTKT) , .T.)) "
    lcLinescan= lcLinescan+ " " + XADDORD +" "+lcCustAcnt
  ENDIF

  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    loProgress.Description = "Collecting Data..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

  SELECT ordline
  IF !(UPPER('ORDLINE.ACCOUNT INTO CUSTOMER') $ UPPER(SET('RELATION')))
    SET RELATION TO "M" + ordline.ACCOUNT INTO Customer ADDITIVE
  ENDIF
  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C' OR (TYPE('lcXMLFileName') <> 'C' AND llOgFltCh)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    IF XBYSIZE='Y'
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
        lcWorkfile  = oAriaEnvironment.Cursors.GetCursorTempName()
        TMPSCAL     = oAriaEnvironment.Cursors.GetCursorTempName()
        LCTMPFILE   = oAriaEnvironment.Cursors.GetCursorTempName()
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      ELSE
        lcWorkfile  = loOGScroll.gfTempName()
        TMPSCAL     = loOGScroll.gfTempName()
        LCTMPFILE   = loOGScroll.gfTempName()
      ENDIF 
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
        lcTransfile = oAriaEnvironment.Cursors.GetCursorTempName()
        lcColorfile = oAriaEnvironment.Cursors.GetCursorTempName()
        LCTMPcolor  = oAriaEnvironment.Cursors.GetCursorTempName()
        LCTMPtrans  = oAriaEnvironment.Cursors.GetCursorTempName()
        
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      ELSE
        lcTransfile = loOGScroll.gfTempName()
        lcColorfile = loOGScroll.gfTempName()
        LCTMPcolor  = loOGScroll.gfTempName()
        LCTMPtrans  = loOGScroll.gfTempName()
      ENDIF 
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      
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
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]    
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]


  IF USED(FStyle)
    USE IN (FStyle)
  ENDIF
  IF !llDontPrn
  
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C' OR (TYPE('lcXMLFileName') <> 'C' AND llOgFltCh)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    
      PRIVATE loProxy
      loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  	  oariaenvironment.report.llcrystal = .T.
      IF loProxy.GetRequest(lcRequestID).Status = 3
        oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

        loProgress.Percent = 1.0
        loProgress.Description = "Printing Report..."
        loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      ENDIF
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      =gfDispRe()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  ELSE  &&FILTERCHANGE
    IF TYPE('lcXMLFileName') <> 'C'

    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF  llDontPrn
      WAIT WIND 'NO RECORDS SELECTED!!!'
    ELSE
      =gfDispRe()
    ENDIF
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
ENDIF
************************************************************
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
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.ItemMask.Do(@laItemSeg)
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  =gfItemMask(@laItemSeg)
ENDIF   
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.ItemMask.Do(@laItemSeg)
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  =gfItemMask(@laItemSeg)
ENDIF   
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.ItemMask.Do(@laItemSeg)
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  =gfItemMask(@laItemSeg)
ENDIF   
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
  DIMENSION laTempStru[78,18] , laTempTran[1,18] , laTempCust[1,18], laTempsty[1,18],laTemplINE[1,18]
  STORE '' TO laTempStru,laTempTran,laTempCust,laTempsty,laTemplINE
  PRIVATE lnFileCnt , lnFldRow


*-- Fields from Customer File.
  SELECT ordline
  AFIELDS(laTempCust)
  laTempStru[1,1]  = 'STYLE'
  laTempStru[2,1]  = 'PO'
  laTempStru[3,1]  = 'ACCOUNT'
  laTempStru[4,1]  = 'COMPLETE'
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
  AFIELDS(laTempTran)

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
  =AFIELDS(laTempsty)

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

  SELECT ordline
  =AFIELDS(laTemplINE)

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


  laTempStru[62,1] = 'CRSESSION'
  laTempStru[62,2] = 'C'
  laTempStru[62,3] = 6
  laTempStru[62,4] = 0

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

  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  oAriaEnvironment.Cursors.createcursor(lcWorkfile ,@laTempStru,,"",.F.)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

  SELECT(lcWorkfile)

  INDEX ON CTRANTYPE+TRANSNO+STYLE+TRANCD+CRSESSION TAG (lcWorkfile)

ELSE     && CASE SIZE = NO

  DIMENSION laTempStru[44,18] ,laTempStru1[34,18], laTempTran[1,18] , laTempsty[1,18]

  STORE '' TO laTempStru,laTempTran,laTempsty,laTempStru1
  PRIVATE lnFileCnt , lnFldRow

*! BUILD STYLE COLORS TEMP -- BEGIN
  SELECT STYLE
  =AFIELDS(laTempsty)
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

  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    oAriaEnvironment.Cursors.createcursor(lcColorfile ,@laTempStru,,"",.F.)
    
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    gfCrtTmp(lcColorfile ,@laTempStru,,"",.f.)
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
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


  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
   oAriaEnvironment.Cursors.createcursor(lcTransfile ,@laTempStru1,,"",.F.)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    gfCrtTmp(lcTransfile ,@laTempStru1,,"",.f.)
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  


  SELECT(lcTransfile)
  INDEX ON CTRANTYPE+TRANSNO+STYLE+TRANCD+CRSESSION TAG (lcTransfile)
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

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  DIMENSION oAriaEnvironment.report.laCRTables[2]

  IF  XBYSIZE='Y'
    DIMENSION oAriaEnvironment.report.laCRParams[6,2]
  ELSE
    DIMENSION oAriaEnvironment.report.laCRParams[10,2]
  ENDIF

  oAriaEnvironment.report.cCROrientation='L'
  IF  XBYSIZE='Y'
    oAriaEnvironment.report.laCRTables[1] = oAriaEnvironment.WorkDir +  LCTMPFILE+ ".DBF"
    oAriaEnvironment.report.laCRTables[2] = oAriaEnvironment.WorkDir +  TMPSCAL+ ".DBF"
    oAriaEnvironment.report.OGLastForm ='ICSCUTRV'
  ELSE
    oAriaEnvironment.report.laCRTables[1] = oAriaEnvironment.WorkDir +  LCTMPtrans+ ".DBF"
    oAriaEnvironment.report.laCRTables[2] = oAriaEnvironment.WorkDir +  LCTMPcolor+ ".DBF"
    oAriaEnvironment.report.OGLastForm ='ICSCURVB'
  ENDIF

  oAriaEnvironment.report.laCRParams[1,1] = 'Layout'
  IF XPRTWIP='S'
    oAriaEnvironment.report.laCRParams[1,2] = 'Summary'
  ELSE
    oAriaEnvironment.report.laCRParams[1,2] = 'Detail'
  ENDIF

  oAriaEnvironment.report.laCRParams[2,1] = 'OpTitle'
  oAriaEnvironment.report.laCRParams[2,2] = lcRPTitle

  oAriaEnvironment.report.laCRParams[3,1] = 'Layout2'
  IF XPRTORD='S'
    oAriaEnvironment.report.laCRParams[3,2] = 'Summary'
  ELSE
    oAriaEnvironment.report.laCRParams[3,2] = 'Detail'
  ENDIF

  oAriaEnvironment.report.laCRParams[4,1] = 'ReportName'
  IF XWIPRPT .OR. XORDRPT
    DO CASE
    CASE XALLOCAT = 'L'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'N'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - NOT ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'A'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - DETAIL REPORT'
    ENDCASE
  ELSE
    DO CASE
    CASE XALLOCAT = 'L'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'N'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - NOT ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'A'
      oAriaEnvironment.report.laCRParams[4,2]= 'SPECIAL CUT & SOLD - SUMMARY REPORT'
    ENDCASE
  ENDIF

  oAriaEnvironment.report.laCRParams[5,1] = 'wipsort'
  DO CASE
  CASE XWIPSORT= 'P'
    oAriaEnvironment.report.laCRParams[5,2]= 'Po/Ct'
  CASE XWIPSORT= 'F'
    oAriaEnvironment.report.laCRParams[5,2]= 'Factory'
  CASE XWIPSORT= 'D'
    oAriaEnvironment.report.laCRParams[5,2]= 'Date'
  ENDCASE
  oAriaEnvironment.report.laCRParams[6,1] = 'ordsort'
  DO CASE
  CASE XORDSORT= 'O'
    oAriaEnvironment.report.laCRParams[6,2]= 'Order'
  CASE XORDSORT= 'A'
    oAriaEnvironment.report.laCRParams[6,2]= 'Account'
  CASE XORDSORT= 'D'
    oAriaEnvironment.report.laCRParams[6,2]= 'Date'
  ENDCASE

  IF  !XBYSIZE='Y'
    oAriaEnvironment.report.laCRParams[7,1] = 'GRASTK'
    oAriaEnvironment.report.laCRParams[7,2]= GRASTK

    oAriaEnvironment.report.laCRParams[8,1] = 'GRAWIP'
    oAriaEnvironment.report.laCRParams[8,2]= GRAWIP

    oAriaEnvironment.report.laCRParams[9,1] = 'GRAORD'
    oAriaEnvironment.report.laCRParams[9,2]= GRAORD

    oAriaEnvironment.report.laCRParams[10,1] = 'GRAOTS'
    oAriaEnvironment.report.laCRParams[10,2]= GRAOTS
  ENDIF
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
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
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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

XFILTER=XFILTER+'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'

*Royalty
llUseRol  = .F.
lnRolPos = ASCAN(laOgFXFlt,"STYLE.ROYALTY")
IF lnRolPos > 0
  lnRolPos  = ASUBSCRIPT(laOgFXFlt,lnRolPos,1)
  lcRolSel =IIF(!EMPTY(laOgFXFlt[lnRolPos,6]),laOgFXFlt[lnRolPos,6],'')
  IF !EMPTY(lcRolSel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
       lcRolFile = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcRolFile = loOGScroll.gfTempName()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
lnSeaPos = ASCAN(laOgVRFlt,"STYLE.SEASON")
IF lnSeaPos > 0
  lnSeaPos = ASUBSCRIPT(laOgVRFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(laOgVRFlt[lnSeaPos,6]),laOgVRFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lcSeaFile = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcSeaFile = loOGScroll.gfTempName()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
lnDivPos = ASCAN(laOgVRFlt,"STYLE.CDIVISION")
IF lnDivPos > 0
  lnDivPos = ASUBSCRIPT(laOgVRFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(laOgVRFlt[lnDivPos,6]),laOgVRFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lcDivFile = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcDivFile =  loOGScroll.gfTempName()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
lnGrpPos = ASCAN(laOgVRFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0
  lnGrpPos  = ASUBSCRIPT(laOgVRFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(laOgVRFlt[lnGrpPos ,6]),laOgVRFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lcGrpFile = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcGrpFile =  loOGScroll.gfTempName()
    ENDIF
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
lnClr1Pos = ASCAN(laOgVRFlt,"SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)")
IF lnClr1Pos > 0
  lnClr1Pos  = ASUBSCRIPT(laOgVRFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(laOgVRFlt[lnClr1Pos ,6]),laOgVRFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lcClr1File = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcClr1File = loOGScroll.gfTempName()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
lnClr2Pos = ASCAN(laOgVRFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnClr2Pos > 0
  lnClr2Pos  = ASUBSCRIPT(laOgVRFlt,lnClr2Pos,1)
  lcClr2Sel =IIF(!EMPTY(laOgVRFlt[lnClr2Pos ,6]),laOgVRFlt[lnClr2Pos,6],'')
  IF !EMPTY(lcClr2Sel)
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lcClr2File = oAriaEnvironment.Cursors.GetCursorTempName()
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lcClr2File = loOGScroll.gfTempName()
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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


lcSylefields= "STYLE,SCALE, CDIVISION,SEASON,CSTYGROUP,CSTYMAJOR, DESC , "
lcSylefields= lcSylefields+ " stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8, "
lcSylefields= lcSylefields+ " ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8, "
lcSylefields= lcSylefields+ " WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8, "
lcSylefields= lcSylefields+ " TOTSTK, TOTORD, TOTWIP "


lcSylefields= lcSylefields+ " ,ALO1,ALO2, ALO3, ALO4, ALO5, ALO6, ALO7, ALO8, TOTALO"


SELECT &lcSylefields. FROM STYLE WHERE STYLE.STYLE <>'********' AND  &XFILTER  INTO CURSOR &FStyle. READWRITE

SELECT (FStyle)
GO TOP
IF EOF()
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    WAIT  WINDOW  'NO RECORDS SELECTED!!!'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  llDontPrn = .T.
  RETURN
ENDIF
INDEX ON STYLE TAG STYLE
SET ORDER TO STYLE
********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************
SELECT ORDHDR
SELECT ordline

SET RELATION TO cOrdType + ORDER INTO ORDHDR ADDITIVE


lcSQLStmt  = "Select TRANCD,[ORDER],CORDLINE,CTKTNO,STYLE From CutPick(INDEX=CutOrd) "
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  lnResult   = oAriaEnvironment.remotetableaccess.sqlrun(lcSQLStmt, 'CutPick' )
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  lnResult1   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CutPick' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  lnResult = (lnResult1 = 1)
ENDIF   
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
IF lnResult 
  SELECT CutPick
  =CURSORSETPROP("Buffering" ,3)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF 'MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C',('MF' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules),;
     ('MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules))
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    SELECT CutPick
    INDEX ON TRANCD+ORDER+CORDLINE TAG CutPick
    INDEX ON TRANCD+CTKTNO+STYLE TAG CutPick2
  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
ELSE
*-- SQL connection error. can't open the report
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
       =gfModalGen('TRM00416B40011','ALERT')
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
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
    REPLACE cstymajor WITH xstyle, DESC WITH xdesc
    FOR i =1 TO 10
      Z=ALLTRIM(STR(i))
      REPLACE COL&Z WITH XCLR(i),STK&Z WITH XSTK(i), ORD&Z WITH XORD(i), WIP&Z WITH XWIP(i)
    ENDFOR

    SELECT (FStyle)
    xstyle=cstymajor
    xdesc=DESC
    lnI=1
    STORE '' TO XCLR
    STORE 0 TO XSTK,XORD,XWIP
  ENDIF

  SELECT (FStyle)
  IF !(lnI>10)
    XCLR(lnI)   = SUBSTR(STYLE,lnNonMajPo)
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
REPLACE cstymajor WITH xstyle, DESC WITH xdesc
FOR i =1 TO 10
  Z=ALLTRIM(STR(i))
  REPLACE COL&Z WITH XCLR(i),STK&Z WITH XSTK(i), ORD&Z WITH XORD(i), WIP&Z WITH XWIP(i)
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

lnDataPos = ASCAN(laOgFXFlt,'CUSTOMER.ACCOUNT')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
  IF AT('INLIST(CUSTOMER.ACCOUNT',lcRPExp) > 0
    lnDatPost = AT('INLIST(CUSTOMER.ACCOUNT',lcRPExp)
  ELSE
    IF AT('BETWEEN(CUSTOMER.ACCOUNT',lcRPExp) > 0
      lnDatPost = AT('BETWEEN(CUSTOMER.ACCOUNT',lcRPExp)
      llMore24 = .T.
    ENDIF
  ENDIF

  IF lnDatPost > 0
    IF llMore24
      lnPos1 = AT('AND' , SUBSTR(lcRPExp,lnDatPost) , 2)
    ELSE
      lnPos1 = AT('AND' , SUBSTR(lcRPExp,lnDatPost))
    ENDIF

    IF lnPos1 > 0
      lcCustAcnt = SUBSTR(lcRPExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
    ELSE
      lcCustAcnt = SUBSTR(lcRPExp ,lnDatPost)

      lcRPExp = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt

    ENDIF
  ENDIF
ENDIF
*Cut the ordhdr complete filter from the lcrpexp to use it in filtering on the  ORDLINE files
lnDatPost = 0
lnDataPos = ASCAN(laOgFXFlt,'ORDHDR.COMPLETE')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
  IF AT('BETWEEN(DTOS(ORDHDR.COMPLETE',lcRPExp) > 0
    lnDatPost = AT('BETWEEN(DTOS(ORDHDR.COMPLETE',lcRPExp)
  ENDIF
  IF lnDatPost > 0
    lnPos1 = AT('AND' , SUBSTR(lcRPExp,lnDatPost) )
    IF lnPos1 > 0
      lcComplete= SUBSTR(lcRPExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcComplete, " .T. ")
      lcComplete= "AND " + lcComplete
    ELSE
      lcComplete= SUBSTR(lcRPExp ,lnDatPost)
      lcRPExp = STRTRAN(lcRPExp, lcComplete, " .T. ")
      lcComplete= "AND " + lcComplete
    ENDIF
  ENDIF
ENDIF
*Cut the ordhdr statr filter from the lcrpexp to use it in filtering on the  ORDLINE files
lnDatPost = 0
lnDataPos = ASCAN(laOgFXFlt,'ORDHDR.START')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
  IF AT('BETWEEN(DTOS(ORDHDR.START',lcRPExp) > 0
    lnDatPost = AT('BETWEEN(DTOS(ORDHDR.START',lcRPExp)
  ENDIF

  IF lnDatPost > 0
    lnPos1 = AT('AND' , SUBSTR(lcRPExp,lnDatPost) )
    IF lnPos1 > 0
      lcStart= SUBSTR(lcRPExp ,lnDatPost , lnPos1-1)
      lcRPExp = STRTRAN(lcRPExp, lcStart, " .T. ")
      lcStart= "AND " + lcStart
    ELSE
      lcStart= SUBSTR(lcRPExp ,lnDatPost)
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
lnPos1=AT(['],lcExpr,1)
lnpos2=AT(['],lcExpr,2)

lcdate1=SUBSTR(lcExpr,lnPos1+1,lnpos2-lnPos1)
lcdate2=SUBSTR(lcExpr,lnpos2+3,lnpos2-lnPos1)

LOCAL lcTempDate, lcTempCentury
lcTempDate    = SET("Date")
lcTempCentury = SET("Century")

SET DATE AMERICAN
SET CENTURY ON

ldDate1=CTOD(SUBSTR(lcdate1,5,2)+"/"+SUBSTR(lcdate1,7,2)+"/"+SUBSTR(lcdate1,1,4))
ldDate2=CTOD(SUBSTR(lcdate2,5,2)+"/"+SUBSTR(lcdate2,7,2)+"/"+SUBSTR(lcdate2,1,4))

SET DATE &lcTempDate.
SET CENTURY &lcTempCentury.


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
SELECT (FStyle)
*!*  GO TOP
xdesc=SPACE(20)
xstyle=SPACE(19)
XCOLOR=SPACE(19)

lnCntRec = RECCOUNT()

SCAN
  IF INKEY()=32
    RETURN
  ENDIF
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    lnPerCent = RECNO()/lnCntRec 
    IF MOD(RECNO(),CEILING(lnCntRec / 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description = "Collecting Data for Style:"+STYLE
    	loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  xstyle = STYLE
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xstyle)  NOWAIT
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]  
  llHastran=.F.
*-- If the "Manufacturing" module is installed
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF 'MF' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C','MF' $ oAriaEnvironment.CompanyInstalledModules,'MF' $ oAriaApplication.CompanyInstalledModules)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    SELECT CutPick
    SET ORDER TO CutPick2
    lcSQLStmt=lcSQLStmtC+" AND POSLN.STYLE='"+xstyle +"' "+lcSQLStmtc1
    SELECT CUTTKTL
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lnResult   = oAriaEnvironment.remotetableaccess.sqlrun(lcSQLStmt, 'CUTTKTL' )
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lnResult1   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CUTTKTL' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
      lnResult = (lnResult1 = 1)
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    IF lnResult 
      SELE CUTTKTL
      IF RECCOUNT('CUTTKTL')>0
        IF !UPPER('CUTTKTL.VENDOR INTO CONTRACTOR') $ UPPER(SET('RELATION'))
          SET RELATION TO CUTTKTL.VENDOR INTO CONTRACTOR ADDITIVE
        ENDIF
        llHastran=.T.
        SELECT CUTTKTL
        SCAN FOR &XADDCUT
          SCATTER MEMVAR MEMO
          SELECT &lcWorkfile
          m.VENDNAME=CONTRACTOR.CVENCOMP
          IF m.TRANCD<>'1'
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
          m.TRANSNO=m.po
          m.CTRANTYPE='1'
          m.CDIVISION=&FStyle..CDIVISION
          m.SEASON=&FStyle..SEASON
          m.CSTYGROUP=&FStyle..CSTYGROUP
          m.cstymajor=&FStyle..cstymajor
          m.DESC=&FStyle..DESC
          m.TOTSTK=&FStyle..TOTSTK
          m.TOTWIP=M.totqty
          m.TOTORD=0
          FOR i=1 TO 8
            Z=STR(i,1)
            m.STK&Z=&FStyle..STK&Z
            m.ALO&Z = &FStyle..ALO&Z
            m.UnAlo&Z = MAX(&FStyle..ORD&Z - &FStyle..ALO&Z,0)
          ENDFOR
          FOR i=1 TO 8
            Z=STR(i,1)
            m.ORD&Z=0
          ENDFOR
          FOR i=1 TO 8
            Z=STR(i,1)
            m.WIP&Z=M.QTY&Z
          ENDFOR
          IF !SEEK('1'+m.TRANSNO + m.STYLE + m.TRANCD+ m.CRSESSION,lcWorkfile,lcWorkfile)
            APPEND BLANK
            m.VENDOR=IIF(!ISNULL(m.VENDOR),m.VENDOR,"     ")
            GATH MEMVAR MEMO
          ENDIF
        ENDSCAN
      ENDIF
    ELSE
*-- SQL connection error. can't open the report
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') <> 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
         =gfModalGen('TRM00416B40011','ALERT')
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
     ENDIF 
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      RETURN .F.
    ENDIF
  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules
*-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF 'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C','PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules,;
         'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules)
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    SELECT CutPick
    SET ORDER TO CutPick2
    lcSQLStmt=lcSQLStmtP+" AND  POSLN.STYLE='"+xstyle +"' " +lcSQLStmtp1
    SELECT POSLN
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    
      lnResult   = oAriaEnvironment.remotetableaccess.sqlrun(lcSQLStmt, 'POSLN') 
    
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lnResult1   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'POSLN' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
      lnResult = (lnResult1 = 1)
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    IF lnResult 
      SELECT POSLN
      IF RECCOUNT('POSLN')>0
        IF !UPPER('POSLN.VENDOR INTO APVENDOR') $ UPPER(SET('RELATION'))
          SET RELATION TO POSLN.VENDOR INTO APVENDOR ADDITIVE
        ENDIF
        llHastran=.T.
        SELECT POSLN
        SCAN FOR &XADD AND !INLIST(TRANCD,'3','6')
          SCATTER MEMVAR MEMO
          m.VENDNAME=APVENDOR.CVENCOMP
          IF m.TRANCD<>'1'
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
          SELECT &lcWorkfile
          m.TRANSNO=m.po
          m.CTRANTYPE='2'
          m.CDIVISION=&FStyle..CDIVISION
          m.SEASON=&FStyle..SEASON
          m.CSTYGROUP=&FStyle..CSTYGROUP
          m.cstymajor=&FStyle..cstymajor
          m.DESC=&FStyle..DESC
          m.TOTSTK=&FStyle..TOTSTK
          m.TOTWIP=M.totqty
          m.TOTORD=0
          FOR i=1 TO 8
            Z=STR(i,1)
            m.STK&Z=&FStyle..STK&Z
            m.ALO&Z = &FStyle..ALO&Z
            m.UnAlo&Z = MAX(&FStyle..ORD&Z - &FStyle..ALO&Z,0)
          ENDFOR
          FOR i=1 TO 8
            Z=STR(i,1)
            m.ORD&Z=0
          ENDFOR
          FOR i=1 TO 8
            Z=STR(i,1)
            m.WIP&Z=M.QTY&Z
          ENDFOR
          APPEND BLANK
          GATH MEMVAR MEMO
        ENDSCAN
      ENDIF
    ELSE
*-- SQL connection error. can't open the report
*      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
  SELECT CutPick
  SET ORDER TO CutPick
  SELECT  ordline

  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    oAriaEnvironment.remotetableaccess.SeekRecord(xstyle)
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ELSE
    gfSeek(xstyle)
  ENDIF   
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  SCAN  REST  WHILE STYLE = xstyle FOR   &lcLinescan
    llHastran=.T.
    SCATTER MEMVAR MEMO
    m.STATUS=IIF(ORDHDR->APPROVAL='DECLINE','D',ORDHDR->STATUS)
    m.Customer=Customer.BTNAME
    SELECT &lcWorkfile
    m.TRANSNO=m.order
    m.CTRANTYPE='3'
    m.CDIVISION=&FStyle..CDIVISION
    m.SEASON=&FStyle..SEASON
    m.CSTYGROUP=&FStyle..CSTYGROUP
    m.cstymajor=&FStyle..cstymajor
    m.DESC=&FStyle..DESC
    m.TOTSTK=&FStyle..TOTSTK
    m.TOTORD=M.totqty
    m.TOTWIP=0
    FOR i=1 TO 8
      Z=STR(i,1)
      m.STK&Z=&FStyle..STK&Z
      m.ALO&Z = &FStyle..ALO&Z
      m.UnAlo&Z = MAX(&FStyle..ORD&Z - &FStyle..ALO&Z,0)
    ENDFOR

    FOR i=1 TO 8
      Z=STR(i,1)
      m.ORD&Z=M.QTY&Z
    ENDFOR
    FOR i=1 TO 8
      Z=STR(i,1)
      m.WIP&Z=0
    ENDFOR
    APPEND BLANK
    GATH MEMVAR MEMO
  ENDSCAN && ordline
  IF !llHastran
    SELECT &lcWorkfile
    APPEND BLANK
    REPLACE STYLE WITH xstyle,;
      TRANSNO WITH '*****',;
      SCALE WITH &FStyle..SCALE,;
      CTRANTYPE WITH '0',;
      CDIVISION WITH &FStyle..CDIVISION,;
      SEASON WITH &FStyle..SEASON,;
      CSTYGROUP WITH &FStyle..CSTYGROUP,;
      cstymajor WITH &FStyle..cstymajor,;
      DESC WITH &FStyle..DESC,;
      TOTSTK WITH &FStyle..TOTSTK,;
      TOTORD WITH 0,;
      TOTWIP WITH 0
    FOR i=1 TO 8
      Z=STR(i,1)
      REPLACE   STK&Z WITH &FStyle..STK&Z
      REPLACE ALO&Z   WITH &FStyle..ALO&Z,;
        UnAlo&Z WITH MAX(&FStyle..ORD&Z - &FStyle..ALO&Z,0)
    ENDFOR
    FOR i=1 TO 8
      Z=STR(i,1)
      REPLACE  ORD&Z WITH 0
    ENDFOR
    FOR i=1 TO 8
      Z=STR(i,1)
      REPLACE  WIP&Z WITH 0
    ENDFOR
  ENDIF
  SELECT(FStyle)
ENDSCAN

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
IF USED('CUTPICK')
  USE IN CutPick
ENDIF    && End of IF USED('CUTPICK')
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
 IF USED('CUTPICK')
   gfCloseTable('CutPick')
 ENDIF    && End of IF USED('CUTPICK')
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

SELECT (lcWorkfile)


lcDeleted =SET("Deleted")
SET DELETED OFF
SCAN FOR CTRANTYPE $ '12' AND !DELETED()
  lnCurrrec = RECNO()
  lcStyle = STYLE
  lcTRANSNO  = TRANSNO
  lcCTRANTYPE = CTRANTYPE

  STORE 0 TO lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8,lntotwip

  SUM WIP1,WIP2,WIP3,WIP4,WIP5 ,wip6,wip7,wip8,TOTWIP TO ;
    lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8,lntotwip FOR ;
    CTRANTYPE + STYLE+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO

  IF (lnWIP1 <= 0)  OR (lnWIP2<= 0) OR (lnWIP3<= 0) OR (lnWIP4<= 0) OR (lnWIP5 <= 0) OR (lnwip6<= 0) OR (lnwip7<= 0) OR (lnwip8<= 0)  OR (lntotwip<= 0)
    llModified = .F.
    FOR lnCount =1 TO 8
      lcCount = ALLTRIM(STR(lnCount,1))
      IF lnWIP&lcCount. <= 0
        REPLACE ALL WIP&lcCount. WITH 0 ,QTY&lcCount. WITH 0 FOR  CTRANTYPE + STYLE+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO
        llModified  = .T.
      ENDIF
    ENDFOR
    IF llModified
      REPLACE ALL TOTWIP WITH WIP1+WIP2+WIP3+WIP4+WIP5+wip6+wip7+wip8 ,totqty WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 FOR  CTRANTYPE + STYLE+TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO
    ENDIF
  ENDIF
  GO RECORD lnCurrrec
ENDSCAN
SET DELETED &lcDeleted

*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

  IF File(oAriaEnvironment.WorkDir +  LCTMPFILE+ ".DBF" )
    ERASE (oAriaEnvironment.WorkDir +  LCTMPFILE+ ".DBF" )
  ENDIF
  COPY TO oAriaEnvironment.WorkDir +  LCTMPFILE+ ".DBF"
  USE oAriaEnvironment.WorkDir +  LCTMPFILE+ ".DBF" IN 0  EXCLUSIVE
  
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
      ERASE (oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPFILE+ ".DBF" IN 0  EXCLUSIVE   
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

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
  INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + po + TRANCD  + STORE +STR(LINENO , 6)+STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='D' .AND. XORDSORT='O'
  INDEX ON STYLE + DTOS(COMPLETE) + po + TRANCD + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6)+STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='F'   .AND. XPRTORD='S'
  INDEX ON STYLE + VENDOR + TRANSNO + TRANCD + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='F'  .AND. XORDSORT='D'
  INDEX ON STYLE + VENDOR + po + TRANCD + DTOS(COMPLETE) + ORDER  + STORE + STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='F'  .AND. XORDSORT='A'
  INDEX ON STYLE + VENDOR + po + TRANCD+ ACCOUNT + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='F' .AND. XORDSORT='O'
  INDEX ON STYLE + VENDOR + TRANSNO + TRANCD+ DTOS(COMPLETE) + STORE +STR(LINENO , 6)  + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='P'  .AND. XPRTORD='S'
  INDEX ON STYLE + TRANSNO + TRANCD + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='P'  .AND. XORDSORT='D'
  INDEX ON STYLE + po + TRANCD + DTOS(COMPLETE) + ORDER  + STORE + STR(LINENO , 6) + STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='P'   .AND. XORDSORT='A'
  INDEX ON STYLE + po + TRANCD + ACCOUNT + ORDER  + DTOS(COMPLETE) + STORE +STR(LINENO , 6)+ STR(RECNO() , 7) TAG LCTMPFILE
CASE   XWIPSORT='P'   .AND. XORDSORT='O'
  INDEX ON STYLE + TRANSNO+ DTOS(COMPLETE)  + TRANCD + STORE +STR(LINENO , 6)+ STR(RECNO() , 7) TAG LCTMPFILE
ENDCASE
SELECT Scale 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  =oAriaEnvironment.remotetableaccess.SeekRecord('S')
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
 gfSeek('S')
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
SELECT * FROM SCALE WHERE TYPE='S' INTO CURSOR LCSCALE READWRITE
SELECT LCSCALE
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  IF File(oAriaEnvironment.WorkDir +  TMPSCAL+ ".DBF" )
    ERASE (oAriaEnvironment.WorkDir +  TMPSCAL+ ".DBF" )
  ENDIF
  COPY TO oAriaEnvironment.WorkDir +  TMPSCAL+ ".DBF"
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
     ERASE (oAriaApplication.WorkDir +  TMPSCAL+ ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  TMPSCAL+ ".DBF"
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[END]
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
SELECT (FStyle)
xdesc=SPACE(20)
xstyle=SPACE(19)
XCOLOR=SPACE(19)
GRASTK=0
GRAWIP=0
GRAORD=0
lnCntRec = RECCOUNT()
SCAN
  IF INKEY()=32
    RETURN
  ENDIF
  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
    lnPerCent = RECNO()/lnCntRec 
    IF MOD(RECNO(),CEILING(lnCntRec / 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
    	loProgress.Description = "Collecting Data for Style:"+STYLE
    	loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF
    
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  ENDIF 
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
  xstyle = STYLE
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    WAIT WINDOW 'Collecting data for style '+ ALLTRIM(xstyle)  NOWAIT
  ENDIF  
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End] 
  GRASTK=GRASTK+TOTSTK
  GRAWIP=GRAWIP+TOTWIP
  GRAORD=GRAORD+TOTORD
  llHastran=.F.
*-- If the "Manufacturing" module is installed
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF  'MF' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C', 'MF' $ oAriaEnvironment.CompanyInstalledModules, 'MF' $ oAriaApplication.CompanyInstalledModules )
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    SELECT CutPick
    SET ORDER TO CutPick2
    lcSQLStmt=lcSQLStmtC+" AND POSLN.STYLE='"+xstyle +"' "+lcSQLStmtc1
    SELECT CUTTKTL
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lnResult   = oAriaEnvironment.remotetableaccess.sqlrun(lcSQLStmt, 'CUTTKTL')
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lnResult1   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'CUTTKTL' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
      lnResult = (lnResult1 = 1)
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    IF lnResult 
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
          IF m.TRANCD<>'1'
            m.totqty=m.totqty*-1

            FOR lnCount = 1  TO 8
              lcCount = ALLTRIM(STR(lnCount,1))
              m.QTY&lcCount = m.QTY&lcCount * -1
            ENDFOR


          ENDIF
          m.TRANSNO=m.po
          m.CTRANTYPE='1'
          m.VENDNAME=CONTRACTOR.CVENCOMP
          DO CASE
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col1
            m.qcol1= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col2
            m.qcol2= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col3
            m.qcol3= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col4
            m.qcol4= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col5
            m.qcol5= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col6
            m.qcol6= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col7
            m.qcol7= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col8
            m.qcol8= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col9
            m.qcol9= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col10
            m.qcol10= m.totqty
          OTHERWISE
            m.qcol10=  m.totqty
            REPLACE &lcColorfile..STYLE WITH 'OTHER' IN &lcColorfile
          ENDCASE

          SELECT &lcTransfile
          m.cstymajor= SUBSTR(M.STYLE,1,lnMajLen)

          IF !SEEK(m.CTRANTYPE+m.TRANSNO+m.STYLE+m.TRANCD+m.CRSESSION,lcTransfile,lcTransfile)

            APPEND BLANK
            m.VENDOR=IIF(!ISNULL(m.VENDOR),m.VENDOR,"     ")
            GATH MEMVAR MEMO

          ENDIF

        ENDSCAN
      ENDIF
    ELSE
    
*-- SQL connection error. can't open the report
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') <> 'C'
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
        =gfModalGen('TRM00416B40011','ALERT')
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      ENDIF 
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      RETURN .F.
    ENDIF

  ENDIF    && End of IF 'MF' $ oAriaApplication.CompanyInstalledModules

*-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
  *IF  'PO' $ oAriaEnvironment.CompanyInstalledModules .OR. 'PS' $ oAriaEnvironment.CompanyInstalledModules
  IF IIF(TYPE('lcXMLFileName') = 'C',('PO' $ oAriaEnvironment.CompanyInstalledModules .OR.;  
     'PS' $ oAriaEnvironment.CompanyInstalledModules),( 'PO' $ oAriaApplication.CompanyInstalledModules .OR. ;
     'PS' $ oAriaApplication.CompanyInstalledModules))
  *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  
    SELECT CutPick
    SET ORDER TO CutPick2
*** Store off Records Into Second Temp File POTEMP
    lcSQLStmt=lcSQLStmtP+" AND  POSLN.STYLE='"+xstyle +"' " +lcSQLStmtp1
    SELECT POSLN
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      lnResult   = oAriaEnvironment.remotetableaccess.sqlrun(lcSQLStmt, 'POSLN') 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
    ELSE
      lnResult1   = loOgScroll.oRDA.SqlRun(lcSQLStmt, 'POSLN' ,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
      lnResult = (lnResult1 = 1)
    ENDIF 
    *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
    IF lnResult 
      SELECT POSLN
      IF RECCOUNT('POSLN')>0
        IF !UPPER('POSLN.VENDOR INTO APVENDOR') $ UPPER(SET('RELATION'))
          SET RELATION TO POSLN.VENDOR INTO APVENDOR ADDITIVE
        ENDIF
        llHastran=.T.
        SELECT POSLN

        SCAN FOR &XADD AND !INLIST(TRANCD,'3','6')

          SCATTER MEMVAR MEMO
          STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10

          IF m.TRANCD<>'1'
            m.totqty=m.totqty*-1


            FOR lnCount = 1  TO 8
              lcCount = ALLTRIM(STR(lnCount,1))
              m.QTY&lcCount = m.QTY&lcCount * -1
            ENDFOR


          ENDIF
          m.TRANSNO=m.po
          m.CTRANTYPE='2'
          m.VENDNAME=APVENDOR.CVENCOMP
          DO CASE
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col1
            m.qcol1= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col2
            m.qcol2= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col3
            m.qcol3= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col4
            m.qcol4= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col5
            m.qcol5= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col6
            m.qcol6= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col7
            m.qcol7= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col8
            m.qcol8= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col9
            m.qcol9= m.totqty
          CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col10
            m.qcol10= m.totqty
          OTHERWISE
            m.qcol10=  m.totqty
            REPLACE &lcColorfile..STYLE WITH 'OTHER' IN &lcColorfile
          ENDCASE
          SELECT &lcTransfile
          m.cstymajor= SUBSTR(M.STYLE,1,lnMajLen)

          APPEND BLANK
          GATH MEMVAR MEMO
        ENDSCAN
      ENDIF
    ELSE
*-- SQL connection error. can't open the report
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') <> 'C'
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
        =gfModalGen('TRM00416B40011','ALERT')
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
      ENDIF 
      *: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
      RETURN .F.
    ENDIF

  ENDIF    && End of IF 'PO' $ oAriaApplication.CompanyInstalledModules .OR. 'PS' $ oAriaApplication.CompanyInstalledModules
  SELECT CutPick
  SET ORDER TO CutPick
  SELE ordline
  SEEK xstyle

  SCAN  REST  WHILE STYLE = xstyle FOR   &lcLinescan
    llHastran=.T.
    SCATTER MEMVAR MEMO
    STORE 0 TO m.qcol1,m.qcol2,m.qcol3,m.qcol4,m.qcol5,m.qcol6,m.qcol7,m.qcol8,m.qcol9,m.qcol10
    m.Customer=Customer.BTNAME
    DO CASE
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col1
      m.qcol1=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col2
      m.qcol2=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col3
      m.qcol3=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col4
      m.qcol4=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col5
      m.qcol5=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col6
      m.qcol6=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col7
      m.qcol7=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col8
      m.qcol8=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col9
      m.qcol9=totqty
    CASE SUBSTR(m.STYLE,lnNonMajPo)=&lcColorfile..col10
      m.qcol10=totqty
    OTHERWISE
      m.qcol10=  m.totqty
      REPLACE &lcColorfile..STYLE WITH 'OTHER' IN &lcColorfile

    ENDCASE
    SELECT &lcTransfile
    m.cstymajor= SUBSTR(M.STYLE,1,lnMajLen)
    m.TRANSNO=m.order
    m.CTRANTYPE='3'

    APPEND BLANK
    GATH MEMVAR MEMO
  ENDSCAN

  IF !llHastran
    SELECT &lcTransfile
    APPEND BLANK
    REPLACE cstymajor WITH SUBSTR(xstyle,1,lnMajLen),TRANSNO WITH '*****',CTRANTYPE WITH '0'
  ENDIF
ENDSCAN
IF USED('CUTPICK')
  USE IN CutPick
ENDIF    && End of IF USED('CUTPICK')

SELECT (lcColorfile )
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  IF File(oAriaEnvironment.WorkDir +  LCTMPcolor  + ".DBF" )
    ERASE (oAriaEnvironment.WorkDir +  LCTMPcolor  + ".DBF" )
  ENDIF
  COPY TO oAriaEnvironment.WorkDir +  LCTMPcolor  + ".DBF"
  USE oAriaEnvironment.WorkDir +  LCTMPcolor  + ".DBF" IN 0 EXCLUSIVE
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPcolor  + ".DBF" IN 0 EXCLUSIVE 
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
INDEX ON cstymajor TAG &LCTMPcolor


SELECT (lcTransfile )
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[Start]
FOR lnCounter = 1 TO 10
  lcCounter = ALLTRIM(STR(lnCounter))

  SCAN FOR CTRANTYPE $ '12' AND qcol&lcCounter. <> 0
    lnCurrrec = RECNO()
    lcStyle = cstymajor
    lcTRANSNO  = TRANSNO
    lcCTRANTYPE = CTRANTYPE


    STORE 0 TO lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8
    SUM qty1,qty2,qty3,qty4,qty5 ,qty6,qty7,qty8 TO ;
      lnWIP1,lnWIP2,lnWIP3,lnWIP4,lnWIP5 ,lnwip6,lnwip7,lnwip8 FOR ;
      CTRANTYPE + cstymajor +TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  AND qcol&lcCounter. <> 0

    IF (lnWIP1 <= 0)   OR (lnWIP2<= 0)  OR(lnWIP3<= 0)   OR (lnWIP4<= 0)   OR (lnWIP5 <= 0)  OR (lnwip6<= 0)  OR (lnwip7<= 0)  OR (lnwip8<= 0)
      FOR lnCount =1 TO 8
        lcCount = ALLTRIM(STR(lnCount,1))
        IF lnWIP&lcCount. <= 0
          REPLACE ALL QTY&lcCount. WITH 0,qcol&lcCounter. WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 ;
            FOR  CTRANTYPE + cstymajor +TRANSNO = lcCTRANTYPE + lcStyle +lcTRANSNO  AND qcol&lcCounter. <> 0
        ENDIF
      ENDFOR
    ENDIF
    GO RECORD lnCurrrec
  ENDSCAN
ENDFOR
*: B608199,1 MMT 08/05/2007 fix bug of wrong WIP in case of over-receive[END]


*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName')= 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  IF File(oAriaEnvironment.WorkDir +  LCTMPtrans  + ".DBF" )
    ERASE (oAriaEnvironment.WorkDir +  LCTMPtrans  + ".DBF" )
  ENDIF
  COPY TO oAriaEnvironment.WorkDir +  LCTMPtrans  + ".DBF"

  *: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[Start]
  *USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 SHARED
  USE oAriaEnvironment.WorkDir +  LCTMPtrans  + ".DBF" IN 0 EXCLUSIVE
  *: B608239,1 MMT 09/06/2007 fix bug of not printing some styles[End]
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  IF loOgScroll.FileExist(oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
    ERASE (oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" )
  ENDIF
  COPY TO oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF"
  USE oAriaApplication.WorkDir +  LCTMPtrans  + ".DBF" IN 0 EXCLUSIVE 
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
SELECT &LCTMPtrans
DO CASE
CASE  XPRTWIP='S' .AND.  XPRTORD='S'
  INDEX ON cstymajor+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
CASE XPRTWIP='S' .AND. XORDSORT='D'
  INDEX ON cstymajor+DTOS(COMPLETE)+TRANSNO +STORE+STR(LINENO,6) TAG LCTMPFILE
CASE  XPRTWIP='S' .AND.  XORDSORT='A'
  INDEX ON cstymajor+ACCOUNT+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
CASE  XPRTWIP='S' .AND.  XORDSORT='O'
  INDEX ON cstymajor+TRANSNO +DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG LCTMPFILE
CASE XWIPSORT='D'   .AND. XPRTORD='S'
  INDEX ON cstymajor+DTOS(COMPLETE)+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='D'   .AND. XORDSORT='D'
  INDEX ON cstymajor+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='D'   .AND. XORDSORT='A'
  INDEX ON cstymajor+ACCOUNT+DTOS(COMPLETE)+TRANSNO +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='D'   .AND. XORDSORT='O'
  INDEX ON cstymajor+ORDER+DTOS(COMPLETE)+po +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='F'  .AND. XPRTORD='S'
  INDEX ON cstymajor+VENDOR+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='F'   .AND. XORDSORT='D'
  INDEX ON cstymajor+VENDOR+po+DTOS(COMPLETE)+ORDER +TRANCD+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='F'  .AND. XORDSORT='A'
  INDEX ON cstymajor+VENDOR+po +ACCOUNT+ORDER+DTOS(COMPLETE) +TRANCD+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='F'    .AND. XORDSORT='O'
  INDEX ON cstymajor+VENDOR+po +TRANCD+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='P'  .AND. XPRTORD='S'
  INDEX ON cstymajor+TRANSNO +TRANCD+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='P'    .AND. XORDSORT='D'
  INDEX ON cstymajor+po +TRANCD+DTOS(COMPLETE)+ORDER+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='P'  .AND. XORDSORT='A'
  INDEX ON cstymajor+po +TRANCD+ACCOUNT+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6)+STR(RECNO(),7) TAG LCTMPFILE
CASE   XWIPSORT='P' .AND. XORDSORT='O'
  INDEX ON cstymajor+po +TRANCD+ORDER +DTOS(COMPLETE)+STORE+STR(LINENO,6) +STR(RECNO(),7) TAG LCTMPFILE
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
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]
  = oAriaEnvironment.Cursors.createcursor(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[Start]
ELSE
  = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
ENDIF 
*: C201061,1 MMT 08/24/2009 call request builder fxp to collect data[End]

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

