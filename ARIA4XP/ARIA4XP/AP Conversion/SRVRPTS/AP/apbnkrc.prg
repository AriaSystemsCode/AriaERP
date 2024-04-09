**************************************************************************************************
* Hesham Elmasry (HES) 28/09/2009 - Convert Bank Reconciliation from A27 to A40 -
* Tracking : 
* Ticket # : 
**************************************************************************************************
PARAMETERS lcRequestID, lcXMLFileName

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

oAriaEnvironment.Report.gcAct_Appl = "AP"
oAriaEnvironment.report.cCROrientation = "L"
oariaenvironment.activeModuleID = 'AP'
oAriaEnvironment.report.cCRPapersize = 'A4'

PUBLIC gcAct_Appl 
gcAct_Appl = "AP"

IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF

lnPrSel = 0
lcFstPrd = ''
lcFiscYear = ''

IF !USED('Apbanks')
  SELECT 0
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'APBANKS','Bankcode','SH',.F.,.F.)
ENDIF
SELECT APBANKS

STORE 0 TO lnWholCler,lnWholOpen, lnWholBook

STORE 0 TO lntClear,lntOpen,lnTBooks

IF lfColctData()
  SELECT(lcActTemp)
ELSE
  RETURN 
ENDIF 

SELECT(lcActTemp)
oAriaEnvironment.report.OGLastForm = lcRpForm

IF EOF()
  RETURN
ELSE
  loProgress.Percent = 0.9
  loProgress.Description = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

  IF loProxy.GetRequest(lcRequestID).Status = 3
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfgetTot
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 12/24/98
*! Purpose   : Function to get totals of the periods previous to 
*!             the chosen ones
*! Ref       : *B801832,1
*!*************************************************************
*! Called from : APBNKRC.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
FUNCTION lfgetTot

lnAlias = SELECT(0)
STORE 0 TO lntClear,lntOpen,lnTBooks

lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"
 
IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
   (lcRpActBy='P' .AND. lnPrSel = 0)
  RETURN 
ENDIF

IF lcRpActBy = 'D'                           && If filterring by date
  lcTotFlt =  lcTotFlt+" .AND. DPAYDATE < ldRpFrDat "
ELSE

  lcPrd = lcFstPrd
  lcTotFlt =  lcTotFlt+" .AND. (cFisfYear < lcFiscYear"+ ;
                     " .OR.  (cFisfYear = lcFiscYear .AND. VAL(cfspprdid) < VAL(lcPrd)))" 
ENDIF

lcTmpPmt = oAriaEnvironment.Cursors.GetCursorTempName()
IF !USED (lcTmpPmt)
  oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'APPAYMNT','CHKTMNO','SH',lcTmpPmt,.F.)
ENDIF 

SELECT (lcTmpPmt)
oAriaEnvironment.remotetableaccess.SeekRecord(&lcActTemp..CBNKCODE+&lcActTemp..CCHKACCT)
  
lcCondStr  = "&lcActTemp..CBNKCODE+&lcActTemp..CCHKACCT = &lcActTemp..CBNKCODE+&lcActTemp..CCHKACCT"
     
SCAN REST WHILE &lcCondStr FOR &lcTotFlt
  lnValue  = &lcTmpPmt..nPayAmnt
  lntClear = lntClear + IIF(&lcTmpPmt..CPAYRECST = 'C' , -lnValue  , 0 )
  lntOpen  = lntOpen  + IIF(&lcTmpPmt..CPAYRECST = 'O' , -lnValue  , 0 )
  lnTBooks = lnTBooks + IIF(&lcTmpPmt..CPAYRECST = 'V' , 0 , -lnValue  )
ENDSCAN

USE IN (lcTmpPmt)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfInsrtRow
*! Developer : Hesham Elmasry (HES) 
*! Date      : 10/08/2009
*! Purpose   : Copy the current data of APPATMNT row to temp. table 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) nAmtValue
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfInsrtRow
PARAMETER nAmtValue

PRIVATE lcCurrency , lnExRate , lnCurrUnit

SELECT APPAYMNT

lcCurrency = ApPaymnt.cCurrCode
lnExRate   = ApPaymnt.nExRate
lnCurrUnit = ApPaymnt.nCurrUnit

IF oAriaEnvironment.remotetableaccess.SeekRecord(ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo,'ApDist')
  SELECT ApDist
  LOCATE REST WHILE capdtrtyp+cbnkcode+cchkacct+capdref+cinvno+capdactid = ;
              ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo ;
              FOR capdactid = 'C'
  IF FOUND()
    lcCurrency = cCurrCode
    lnExRate   = nExRate
    lnCurrUnit = nCurrUnit
  ENDIF
ENDIF

SELECT APBANKS
oAriaEnvironment.remotetableaccess.SeekRecord(ApPaymnt.cBnkCode)

SELECT APVENDOR
oAriaEnvironment.remotetableaccess.SeekRecord(ApPaymnt.cpayclno)

SELECT APPAYMNT

m.cBnkCode  = ApPaymnt.cBnkCode
m.Cbnklndes = APbANKS.Cbnklndes
m.cchkacct  = ApPaymnt.cchkacct
m.Currency  = IIF(lcRpCurr = 'F',CCurrCode,oAriaApplication.BaseCurrency)
m.TrTyp     = IIF(ApPaymnt.CPAYMETH='P','Prn Chk',IIF(CPAYMETH='M','Man Chk', IIF(CPAYMETH='N', ;
              'Non Chk',IIF(ApPaymnt.CPAYMETH='B',IIF(ApPaymnt.CPAYMETH='B' AND NPAYAMNT < 0,'Deposit','Charge'),'A\R'))))
m.Payment   = ApPaymnt.cpaydocno
m.Date      = ApPaymnt.dpaydate
m.pr        = ApPaymnt.cfspprdid
m.year      = ApPaymnt.cfisfyear
m.CPayee    = IIF(EMPTY(ApPaymnt.cpaycomp) .AND. ApPaymnt.CPAYTYPE = 'P',APVENDOR.CVENCOMP,ApPaymnt.cpaycomp)
m.stat      = IIF(ApPaymnt.CPAYRECST='O','Open',IIF(ApPaymnt.CPAYRECST='C','Clrd','Hide'))
m.amount    = gfAmntDisp(-ApPaymnt.npayamnt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)  
m.BATCH     = ApPaymnt.Batch
m.CPAYMETH  = ApPaymnt.CPAYMETH
m.NPAYAMNT  = nAmtValue

INSERT INTO &LCACTTEMP FROM MEMVAR
	   		 			   
SELECT APPAYMNT
RETURN

*!*************************************************************
*! Name      : lfWholeTot
*! Developer : Amin Khodary 
*! Date      : 07/08/1999
*! Purpose   : 1) Sum book, cleared and open amount of whole transactions
*!*************************************************************
*! Called from : APBNKRCS.FRX 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  None
*!*************************************************************
FUNCTION lfWholeTot

lnAlias = SELECT(0)
STORE 0 TO lnWholCler,lnWholOpen, lnWholBook
lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"

lcTmpPmt = oAriaEnvironment.Cursors.GetCursorTempName()
oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + 'APPAYMNT','CHKTMNO','SH',lcTmpPmt,.F.)

SELECT (lcTmpPmt)
=oAriaEnvironment.remotetableaccess.SeekRecord(&lcActTemp..CBNKCODE+&lcActTemp..CCHKACCT)
 
lcCondStr  = "&lcTmpPmt..CBNKCODE+&lcTmpPmt..CCHKACCT = &lcActTemp..CBNKCODE+&lcActTemp..CCHKACCT"

SCAN REST WHILE &lcCondStr  FOR &lcTotFlt
  lnValue   =  &lcTmpPmt..nPayAmnt
  lnWholCler = lnWholCler + IIF(&lcTmpPmt..CPAYRECST = 'C', -lnValue , 0  )
  lnWholOpen = lnWholOpen + IIF(&lcTmpPmt..CPAYRECST = 'O', -lnValue , 0  ) 
  lnWholBook = lnWholBook - lnValue
ENDSCAN

USE IN (lcTmpPmt)
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfColctData
*! Developer : Hesham Elmasry(HES)
*! Date      : 10/01/2009
*! Purpose   : Collecting Data
*!*************************************************************
*! Called    : 
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfColcData()
*!*************************************************************
FUNCTION lfColctData

=lcCrtTemp() && Create the primary temp

PRIVATE     lcFiscPrd, lcBnkCode, lcChkAccount, lcReconStat, lcLstPrd, lcDateFltExp
STORE '' TO lcFiscYear, lcFiscPrd, lcBnkCode, lcChkAccount, lcReconStat, lcFstPrd, lcLstPrd, lcDateFltExp

lcFiscYear   = lfCheckFilter(1,"APPAYMNT.CFISFYEAR",1)
lcFiscPrd    = lfCheckFilter(1,"APPAYMNT.CFSPPRDID",1)
lcBnkCode    = lfCheckFilter(3,"APPAYMNT.CBNKCODE  ",1)
lcChkAccount = lfCheckFilter(3,"APPAYMNT.CCHKACCT",1)
lcReconStat  = lfCheckFilter(3,"APPAYMNT.CPAYRECST",1)
lnPrSel      = 0

IF !EMPTY(lcFiscPrd)
  SELECT (lcFiscPrd)
  COUNT TO lnPrSel FOR !DELETED()
  IF lnPrSel > 0
    SELECT (lcFiscPrd)
    GO TOP 
    lcFstPrd = &lcFiscPrd..CFSPPRDID
    GO BOTTOM 
    lcLstPrd = &lcFiscPrd..CFSPPRDID
  ENDIF 
ENDIF 

IF lcRpActBy='D' && If filterring by date
  DO CASE
    CASE !EMPTY(ldRpFrDat) AND !EMPTY(ldRpToDat)  
      lcDateFltExp = "BETWEEN(APPAYMNT.DPAYDATE,ldRpFrDat,ldRpToDat)"
    CASE !EMPTY(ldRpFrDat)
      lcDateFltExp = "APPAYMNT.DPAYDATE >= ldRpFrDat"   
    CASE !EMPTY(ldRpToDat)
      lcDateFltExp = "APPAYMNT.DPAYDATE <= ldRpToDat"
  ENDCASE
ENDIF

lcFltExpr = " IIF(!EMPTY(lcFiscYear   , APPAYMNT.CFISFYEAR $  lcFiscYear,   .T.)                                 AND " +;
            " IIF(!EMPTY(lcBnkCode)   , APPAYMNT.CBNKCODE  $  lcBnkCode,    .T.)                                 AND " +;
            " IIF(!EMPTY(lcChkAccount), APPAYMNT.CCHKACCT  $  lcChkAccount, .T.)                                 AND " +;
            " IIF(!EMPTY(lcReconStat) , APPAYMNT.CPAYRECST $  lcCurrCod,    .T.)                                 AND " +;
            " IIF( lnPrSel > 0        , APPAYMNT.CFSPPRDID >= lcFstPrd AND APPAYMNT.CFSPPRDID <= lcLstPrd , .T.)     " 

lcFltExpr = IIF(!EMPTY(lcFltExpr), lcFltExpr + IIF(!EMPTY(lcDateFltExp), " AND " + lcDateFltExp, '') , lcDateFltExp)
lcFltExpr = IIF(!EMPTY(lcFltExpr), lcFltExpr + " AND Appaymnt.Cpaystat <> 'V' .AND. Appaymnt.CPAYMETH <> 'H' " , ;
                                                   " Appaymnt.Cpaystat <> 'V' .AND. Appaymnt.CPAYMETH <> 'H' " )
SELECT APPAYMNT
lnPayCnt = RECCOUNT()
=oAriaEnvironment.remotetableaccess.SeekRecord(lcBnkCode+lcChkAccount)

SCAN FOR &lcFltExpr
  SELECT APPAYMNT 
 
  lnPerCent = RECNO()/lnPayCnt 
  loProgress.Percent = lnPerCent * 0.9
  loProgress.Description = "Collecting Data For Bank : "+APPAYMNT.CBNKCODE
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)    
  
  IF EMPTY(APPAYMNT.Batch)
    DO lfInsrtRow with APPAYMNT.NPAYAMNT 
  ELSE
    IF lcRpType = 'BATCH'
      lcBatchNo  = APPAYMNT.Batch
      nlRecNo = RECNO('APPAYMNT')
      lnAmount = 0 
      SCAN FOR APPAYMNT.BATCH = lcBatchNo .AND. !EOF('APPAYMNT')
        lnAmount = lnAmount  + APPAYMNT.NPAYAMNT
      ENDSCAN
      IF lnAmount <> 0
        GOTO nlRecNo
        DO lfInsrtRow WITH lnAmount
      ENDIF
    ELSE
      DO lfInsrtRow WITH APPAYMNT.NPAYAMNT
    ENDIF
  ENDIF
ENDSCAN
* End of lfColctData()

*!*************************************************************
*! Name      : lcCrtTemp
*! Developer : Hesham Elmasry(HES)
*! Date      : 10/01/2009
*! Purpose   : Creating the primary temp.
*!*************************************************************
*! Called    : 
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfColcData()
*!*************************************************************
FUNCTION lcCrtTemp

*========= Primary Temp ==========*
IF USED(lcActTemp) AND RECCOUNT(lcActTemp) > 0
  SELECT (lcActTemp)
  USE IN (lcActTemp)
ENDIF
*-- Create File
IF !USED(lcActTemp)
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cBnkCode'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1  
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CPAYRECST'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1  
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cchkacct'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Cbnklndes'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 40
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ncurrunit'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 4
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nexrate'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 9
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Currency'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 3
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CCurrCode'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 3
  laTempStru[lnI,4] = 0  

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CPAYMETH'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'NPAYAMNT'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 18
  laTempStru[lnI,4] = 2  

  lnI = ALEN(laTempStru,1)+1 
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TrTyp'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 7
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1 
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cpaytype'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0  
   
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Payment'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Date'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Pr'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 2
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'YEAR'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 4
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CPayee'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 60
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Stat'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 4
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Amount'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 14
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Cleared'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 14
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Open'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 14
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'BATCH'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0    
  
  DECLARE LAIndeces1[1,2]
  
  LAIndeces1[1,1] = 'cBnkCode+cchkacct+BATCH'
  LAIndeces1[1,2] = 'chkbtch'
  =oAriaEnvironment.Cursors.createcursor(lcActTemp,@laTempStru,@LAIndeces1,lcActTemp,.T.)
ENDIF
* End of lcCrtTemp()

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry
*! Date      : 10/07/2009
*! Purpose   : Return the select values from filters
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter, lnRetTyp

LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(laOGFxFlt,lnPos,1)    
      lcReturn = laOGFxFlt[lnPOS,6] 
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(laOGHDFlt,lnPos,1)
      lcReturn = laOGHDFlt[lnPOS,6]  
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(laOGvrFlt,lnPos,1)  
      lcReturn = laOGvrFlt[lnPOS,6] 
    ENDIF
ENDCASE

IF lnRetTyp = 1
  RETURN lcReturn 
ELSE 
  RETURN lnPos
ENDIF 
* End of lfCheckFilter

