*:***************************************************************************
*: Program file  : APCHKPRV
*: Program desc. : Print Check Report
*: System        : Aria Advantage Series.Aria4XP
*: Module        : AP
*: Developer     : Mariam Mazhar{MMT}
*: Date          : 10/19/2009
*: Reference     : N000635[Task:T20090907.0009 Project:AP Conversion]
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfModalGen,gfTempName
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO APCHKPRV
*!***************************************************************************
*! Modification:
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [T20091122.0011]
*:***************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]


IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  loEnvironment.ClientID = ClientID
  loEnvironment.CONNECIONSREFRESH()
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL lcCurrentProcedure
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)


  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
  *T20100512.0026 Hassan 2010 05 23 [END]

  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  lcRpBnkCod = PADR(lcRpBnkCod,8)
  lcRpChkAct = PADR(lcRpChkAct,12)
  oAriaEnvironment.REPORT.gcAct_Appl = "AP"
  oAriaEnvironment.REPORT.cCROrientation = "P"
  oAriaEnvironment.activeModuleID = 'AP'
  PRIVATE loAddUserInfo
  loAddUserInfo = CREATEOBJECT('ariamain.AddUserInfo')

  PUBLIC gcAct_Appl
  gcAct_Appl = "AP"

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF

  *Open Date Tables
  DIMENSION laOpenFile [10,3]
  laOpenFile = .F.

  laOpenFile [1,1] = 'APVENDOR'
  IF !USED('APVENDOR')
    =gfOpenTable('APVENDOR','VENCODE')
    laOpenFile [1,2] = .T.
  ENDIF

  SELECT APVENDOR
  laOpenFile [1,3] = ORDER()
  =gfSetOrder('VENCODE')

  laOpenFile [2,1] = 'APVENHST'
  IF !USED('APVENHST')
    =gfOpenTable('APVENHST','VENDYEAR')
    USE &gcDataDir.APVENHST IN 0
    laOpenFile [2,2] = .T.
  ENDIF

  SELECT APVENHST
  laOpenFile [2,3] = ORDER()
  =gfSetOrder('VENDYEAR')

  laOpenFile [3,1] = 'APBANKS'
  IF !USED('APBANKS')
    =gfOpenTable('APBANKS','BANKCODE')
    laOpenFile [3,2] = .T.
  ENDIF

  SELECT APBANKS
  laOpenFile [3,3] = ORDER()
  =gfSetOrder('BANKCODE')
  =gfSEEK(lcRpBnkCod)


  laOpenFile [4,1] = 'APCHECKS'
  IF !USED('APCHECKS')
    =gfOpenTable('APCHECKS','BANKCHECK')
    laOpenFile [4,2] = .T.
  ENDIF

  SELECT APCHECKS
  laOpenFile [4,3] = ORDER()
  =gfSetOrder('BANKCHECK')
  =gfSEEK(lcRpBnkCod+lcRpChkAct)

  laOpenFile [5,1] = 'APINVHDR'
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  IF USED('APINVHDR')
    =gfCloseTable('APINVHDR')
  ENDIF
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  IF !USED('APINVHDR')
    =gfOpenTable('APINVHDR','INVVEND')
    laOpenFile [5,2] = .T.
  ENDIF

  SELECT APINVHDR
  laOpenFile [5,3] = ORDER()
  =gfSetOrder('INVVEND')

  laOpenFile [6,1] = 'APPAYMNT'
  IF !USED('APPAYMNT')
    =gfOpenTable('APPAYMNT','TYPMETHDOC')
    laOpenFile [6,2] = .T.
  ENDIF

  SELECT APPAYMNT
  laOpenFile [6,3] = ORDER()
  gfSetOrder('TYPMETHDOC')

  laOpenFile [7,1] = 'APDIST'
  IF !USED('APDIST')
    =gfOpenTable('APDIST','PAYMNTS')
    laOpenFile [7,2] = .T.
  ENDIF

  SELECT APDIST
  laOpenFile [7,3] = ORDER()
  =gfSetOrder('PAYMNTS')

  laOpenFile [8,1] = 'SYCFACT'
  IF !USED('SYCFACT')
    =gfOpenTable('SYCFACT','CFACCODE')
    laOpenFile [8,2] = .T.
  ENDIF

  SELECT SYCFACT
  laOpenFile [8,3] = ORDER()
  =gfSetOrder('CFACCODE')

  laOpenFile [9,1] = 'SYCINT'
  IF !USED('SYCINT')
    =gfOpenTable('SYCINT','CCONTCODE')
    laOpenFile [9,2] = .T.
  ENDIF

  SELECT SYCINT
  laOpenFile [9,3] = ORDER()
  gfSetOrder('CCONTCODE')

  laOpenFile [10,1] = 'APDIV'
  IF !USED('APDIV')
    =gfOpenTable('APDIV','DIVISION')
    laOpenFile [10,2] = .T.
  ENDIF

  SELECT APDIV
  laOpenFile [10,3] = ORDER()
  =gfSetOrder('DIVISION')

  IF !USED('CODES')
    =gfOpenTable('CODES','CODES')
  ENDIF

  IF !USED('APSETUP')
    =gfOpenTable('APSETUP','APSETUP','SH')
    SELECT 'APSETUP'
    gfSEEK('')
  ENDIF
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
ELSE
  loOGScroll.cCROrientation = "P"
  IF USED('APINVHDR')
    =gfCloseTable('APINVHDR')
  ENDIF
  IF !USED('APINVHDR')
    =gfOpenTable('APINVHDR','INVVEND')
  ENDIF
  SELECT 'APINVHDR'
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
ENDIF


lcSavCurSm = SET('CURRENCY',1)    &&Varible to save the seting of the currency symbol

*Added check to see if the syccurr is open
PRIVATE llCurOpn
llCurOpn  = .F.
IF !USED('SYCCURR')
  llCurOpn  = .T.
  =gfOpenTable('SYCCURR','CCURRCODE')
  SELECT SYCCURR
  =gfSEEK('')
ENDIF

SELECT cCurrCode , cCurrSmbl ;
  FROM SYCCURR ;
  INTO ARRAY laCurrSmbl

IF !llCurOpn
  gfCloseTable('SYCCURR')
ENDIF

lcNetSmbl = ALLTRIM(lfGetNetSm())      &&Varible to save the currency symbol of the checking account currency

*** Check if user wants report to printer or file
IF TYPE('lcXMLFileName') <> 'C'
  llPrintChk = IIF(UPPER(oAriaApplication.gcDevice)='PRINTER' OR UPPER(oAriaApplication.gcDevice)='FILE',.T.,.F.)
ELSE
  llPrintChk = IIF(UPPER(oAriaEnvironment.gcDevice)='PRINTER' OR UPPER(oAriaEnvironment.gcDevice)='FILE',.T.,.F.)
ENDIF
*!*  IF TYPE('lcXMLFileName') <> 'C'
*!*    lcFisYear  = ''      && fiscal year
*!*    lcFisPrd   = ''      && fiscal period
*!*  ENDIF
lcLineTwo  = ''      && holds the total check amount (uesd in FRX)
lcInvRemit = ''      && invoice remit to
lcFactor   = ''      && factor code
lcTFactor  = ''      && 'factor code'

ldRPChkDat = ldChkDat

lcRpExp = lcRpExp + IIF(lcRpVenPay = 'A','',' AND APINVHDR.CVENPMETH = lcRpVenPay')
lcRpExp = lcRpExp + ' AND APINVHDR.DINVDATE <= ldChkDat '
lcRpExp = lcRpExp + ' AND APINVHDR.DPOSTDATE <= ldChkDat'
DIMENSION laFooter[lnRpStub,6]  && Creat a new array to hold the footer records.
laFooter  = ''
lnFooter  = 0
lcVendor  = '' && Variable to hold the vendor code.
lcPageNo  = ''
lcAddress  = '' && Variable to hold the invoice address.
PRIVATE lcCurErHnd


IF TYPE('lcXMLFileName') <> 'C'
  lcVoid = loOGScroll.gfTempName()
ELSE
  lcVoid = gfTempName()
ENDIF

DIMENSION laTempStru[1,4]
laTempStru[1,1] = 'gPic'
laTempStru[1,2] = 'G'
laTempStru[1,3] = 10
laTempStru[1,4] = 0
=gfCrtTmp(lcVoid,@laTempStru)
SELECT (lcVoid)
APPEND BLANK
IF TYPE('lcXMLFileName') <> 'C'
  APPEND GENERAL gPIC FROM (oAriaApplication.BitmapHome +'VOIDCHK.BMP')
ELSE
  APPEND GENERAL gPIC FROM (oAriaEnvironment.BitmapHome +'VOIDCHK.BMP')
ENDIF

IF TYPE('lcXMLFileName') <> 'C'
  IF llTestChk            && In case of allignment test.
    IF lfLokBank()        && If could lock the bank record
      SELECT (lcRpTargt)
      DO gfDispRe WITH EVAL('lcRepForm'),.F.,.F.,'R',.T.
      IF llPrintChk
        REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo))
        SELECT APCHECKS
        =gfReplace('')
        =gfTableUpdate()
      ENDIF
      =lfUnLokBank()      && unlock the bank record
    ENDIF
    RETURN
  ENDIF
ENDIF

lcPrExp    = lcRpExp + ' AND CBNKCODE+CCHKACCT = lcRpBnkCod+lcRpChkAct'
lnTotal    = 0         && Summation the total invoices in the main check
llEndGrp   = .T.       && if end of group
lcChkNo    = ''        && Hold the check number
lnPrnCh  =0 && the payment group number being printed ... used in reprinting only

lcPayPrd   = "NVNHPAY"+ALLTRIM(STR(VAL(lcFisPrd)))

* Declare some varibales will be used as a global in case of adv. pay.
* AP Invoice header fields  (Print checks currency)
lnCurrUnit  = 1            && Holds currency unit of printed checks
lnExRate    = 1   		   && Holds currency rate of printed checks

IF TYPE('lcXMLFileName') <> 'C'
  lcCurrCode  = oAriaApplication.BaseCurrency                     && Holds currency code of printed checks
ELSE
  lcCurrCode  = oAriaEnvironment.BaseCurrency                     && Holds currency code of printed checks
ENDIF

lnEqvAmnt   = lnPaymnt    && Holds eqv. amount

*** lcPrintMod variable that hold..
*** 'A' --> in case of 'advance check payment'.
*** 'V' --> in case of 'print approved checks'.
*** 'R' --> in case of 'reprinting check'.

lPrntCompl = .F.
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
lcNxtChk = ''
lcRpFields = lcRpFields + ",cFacCode,cOutComp,cInvRemit"
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
DO CASE
CASE lcPrintMod = 'A'  && Print advance checks
  IF lfLokBank()
    lPrntCompl = .T.
    =lfCreateCur()  && Create temp file
    =lfAdvPyCur()   && Get currency code, rate and unit of advanced payment
    =lfAdvPay()     && Fill the data into files
    SELECT (lcRpTargt)
    IF TYPE('lcXMLFileName') <> 'C'
      DO gfDispRe WITH EVAL('lcRepForm'),.F.,.F.,'R',.T.
    ELSE
      loProgress.Percent = 0.9
      loProgress.DESCRIPTION = "Printing Report..."
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      *T20100512.0026 Hassan 2010 05 23 [END]
      oAriaEnvironment.REPORT.OGLastForm = lcRepForm
      PRIVATE loProxy
      loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

      IF loProxy.GetRequest(lcRequestID).STATUS = 3
        oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)

        loProgress.Percent = 1.0
        loProgress.DESCRIPTION = "Printing Report..."
        *T20100512.0026 Hassan 2010 05 23 [BEGIN]
        *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        *T20100512.0026 Hassan 2010 05 23 [END]
      ENDIF
    ENDIF
    =lfUnLokBank()  && unlock the bank record
  ENDIF

CASE lcPrintMod = 'V'     && Print approved checks
  lcRpFiles = 'APINVHDR'
  SELECT APINVHDR
  DIMENSION laFileStru[1,18]
  lnFileStru= AFIELDS(laFileStru)
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  *DIMENSION laFileStru[lnFileStru+3,18]
  DIMENSION laFileStru[lnFileStru+4,18]
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  laFileStru[lnFileStru+1,1] = 'nNoOfInv'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'cPageNo'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 4
  laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+3,1] = 'CADDRESS'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 190
  laFileStru[lnFileStru+3,4] = 0

  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  *FOR lnCount = 1 TO 3
  laFileStru[lnFileStru+4,1] = 'cvencomp'
  laFileStru[lnFileStru+4,2] = 'C'
  laFileStru[lnFileStru+4,3] = 30
  laFileStru[lnFileStru+4,4] = 0
  FOR lnCount = 1 TO 4
    *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
    STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
      laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
      laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
      laFileStru[lnFileStru+lnCount,16]
    STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
  ENDFOR
  =gfCrtTmp(lcRpTargt,@laFileStru,'CVENDCODE+CADDRESS+CINVNO',lcRpTargt,.F.)

  *Vendor Code Selected
  lcCursorVend = ''
  llSelectVendor = .F.
  lnPosVend = ASCAN(laOgFXFlt,"APINVHDR.CVENDCODE")
  IF lnPosVend> 0
    lnPosVend= ASUBSCRIPT(laOgFXFlt,lnPosVend,1)
    lcCursorVend= laOgFXFlt[lnPosVend,6]
    IF !EMPTY(lcCursorVend)
      SELECT(lcCursorVend)
      LOCATE
      IF !EOF()
        llSelectVendor= .T.
      ENDIF
    ENDIF
  ENDIF


  *Division Selected
  lcDivCursor =  ''
  llDiviSelected  = .F.
  lnPosDivision = ASCAN(laOgFXFlt,"APINVHDR.CDIVISION")
  IF lnPosDivision > 0
    lnPosDivision = ASUBSCRIPT(laOgFXFlt,lnPosDivision,1)
    lcDivisions = laOgFXFlt[lnPosDivision,6]
    IF !EMPTY(lcDivisions)
      llDiviSelected  = .T.
      lcDivCursor = gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      =gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
      lnStart=1
      lnEnd=AT('|',lcDivisions)
      DO WHILE lnEnd <> 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
        lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcDivisions)
      ENDDO
      IF lnEnd = 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE CDIVISION WITH lcDivisions
      ENDIF
    ENDIF
  ENDIF

  *Due Date Selected
  llDueSelected  = .F.
  ldDueStrtDate = {}
  ldDueFnshDate = {}
  lnPosDue = ASCAN(laOgFXFlt,"APINVHDR.DINVDUDAT")
  IF lnPosDue > 0
    lnPosDue = ASUBSCRIPT(laOgFXFlt,lnPosDue ,1)
    IF !EMPTY(laOgFXFlt[lnPosDue ,6])
      llDueSelected  = .T.
      ldDueStrtDate = IIF(EMPTY(SUBSTR(laOgFXFlt[lnPosDue ,6],1,10)),CTOD(""),CTOD(SUBSTR(laOgFXFlt[lnPosDue ,6],1,10)))
      ldDueFnshDate =  IIF(EMPTY(SUBSTR(laOgFXFlt[lnPosDue ,6],12,21)),CTOD(""),CTOD(SUBSTR(laOgFXFlt[lnPosDue ,6],12,21)))
    ENDIF
  ENDIF

  *Priority Selected
  llPrioSelected  = .F.
  lcPStrt= ''
  lcPFnsh= ''
  lnPosPri = ASCAN(laOgFXFlt,"APINVHDR.CVENPRIOR")
  IF lnPosPri > 0
    lnPosPri = ASUBSCRIPT(laOgFXFlt,lnPosPri,1)
    IF !EMPTY(laOgFXFlt[lnPosPri,6])
      llPrioSelected  = .T.
      lcPStrt= LEFT(laOgFXFlt[lnPosPri,6],1)
      lcPFnsh= RIGHT(laOgFXFlt[lnPosPri,6],1)
    ENDIF
  ENDIF
  lcRpVenPay = IIF(LEFT(lcRpVenPay,1) = ",",SUBSTR(lcRpVenPay,2),lcRpVenPay)
  lcRpVenPay = IIF(RIGHT(lcRpVenPay,1) = ",",SUBSTR(lcRpVenPay,1,LEN(lcRpVenPay)-1),lcRpVenPay)

  DO CASE
  CASE llSelectVendor
    SELECT(lcCursorVend)

    lnPerCent = RECNO()/RECCOUNT(lcCursorVend)
    LOCATE
    SCAN
      IF TYPE('lcXMLFileName') = 'C'
        IF MOD(RECNO(lcCursorVend),CEILING(RECCOUNT(lcCursorVend) / 10)) = 0
          loProgress.Percent = lnPerCent * 0.9
          loProgress.DESCRIPTION = "Collecting Data For Vendor:"+ALLTRIM(&lcCursorVend..KeyExp)
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
          *T20100512.0026 Hassan 2010 05 23 [END]
        ENDIF
      ENDIF
      =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where  APINVHDR.CVENDCODE = '"+;
        ALLTRIM(&lcCursorVend..KeyExp)+"'"+;
        IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
        " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
        IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
        IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
        " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
      SELECT APINVHDR
      SCAN FOR IIF(llDiviSelected,SEEK(APINVHDR.CDIVISION,lcDivCursor),.T.)
        SCATTER MEMO MEMVAR
        *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
        =gfSEEK(m.CVENDCODE,'apvendor','VENCODE')
        m.cvencomp = APVENDOR.cvencomp
        *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
        INSERT INTO (lcRpTargt) FROM MEMVAR
      ENDSCAN
    ENDSCAN
  CASE llDiviSelected
    SELECT (lcDivCursor)
    lnPerCent = RECNO()/RECCOUNT(lcDivCursor)
    LOCATE

    SCAN
      IF TYPE('lcXMLFileName') = 'C'
        IF MOD(RECNO(lcDivCursor),CEILING(RECCOUNT(lcDivCursor) / 10)) = 0
          loProgress.Percent = lnPerCent * 0.9
          loProgress.DESCRIPTION = "Collecting Data For Division:"+ALLTRIM(&lcDivCursor..CDIVISION)
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
          *T20100512.0026 Hassan 2010 05 23 [END]
        ENDIF
      ENDIF


      =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where  APINVHDR.CDIVISION = '"+;
        ALLTRIM(&lcDivCursor..CDIVISION)+"'"+;
        IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
        " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
        IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
        IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
        " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')

      SELECT APINVHDR
      SCAN
        SCATTER MEMO MEMVAR
        *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
        =gfSEEK(m.CVENDCODE,'apvendor','VENCODE')
        m.cvencomp = APVENDOR.cvencomp
        *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [ENd]
        INSERT INTO (lcRpTargt) FROM MEMVAR
      ENDSCAN
    ENDSCAN

  OTHERWISE
    =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where "+;
      " APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
      IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
      IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
      IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
      " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')

    SELECT APINVHDR
    lnPerCent = RECNO()/RECCOUNT('APINVHDR')


    SCAN
      IF TYPE('lcXMLFileName') = 'C'
        IF MOD(RECNO('APINVHDR'),CEILING(RECCOUNT('APINVHDR') / 10)) = 0
          loProgress.Percent = lnPerCent * 0.9
          loProgress.DESCRIPTION = "Collecting Data For Invoice:"+ALLTRIM(APINVHDR.CinvNo)
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
          *T20100512.0026 Hassan 2010 05 23 [END]
        ENDIF
      ENDIF
      SCATTER MEMO MEMVAR
      *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
      =gfSEEK(m.CVENDCODE,'apvendor','VENCODE')
      m.cvencomp = APVENDOR.cvencomp
      *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
      INSERT INTO (lcRpTargt) FROM MEMVAR
    ENDSCAN
  ENDCASE
  SELECT (lcRpTargt)
  LOCATE
  IF EOF() && No records collected
    *** NO recoeds hove been collected
    IF TYPE('lcXMLFileName') <> 'C'
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE
      RETURN
    ENDIF
  ELSE
    IF TYPE('lcXMLFileName') = 'C' AND EMPTY(lcNxtChkNo)
      lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.NCHKNXTPN,8,'0'),'00000001')
    ENDIF
    IF lfLokBank()      && If could lock the bank record
      IF lfLockFile()   && If could lock the selected invoice records
        SELECT (lcRpTargt)
        IF llPrintChk
          GO TOP

          lcTmpChkNo= lcNxtChkNo
          lcChkPage = cPageNo
          SCAN
            =lfNxtChkUpd()
            =lfUpInv()
            *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
            IF (TYPE('lcXMLFileName') <> 'C' AND (oAriaApplication.gcDevice = "FILE" .AND. INLIST(loOGScroll.cTextRepType ,"EXCEL",'XML'))) OR ;
                (TYPE('lcXMLFileName') = 'C' AND (oAriaEnvironment.gcDevice  = "FILE" .AND. INLIST(oAriaEnvironment.REPORT.cTextRepType ,"EXCEL",'XML')))
              REPLACE CCHKNO WITH lcNxtChkNo IN (lcRpTargt)
            ENDIF
            *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
          ENDSCAN
          *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
          lcNxtChk  = lcNxtChkNo
          *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
          lcNxtChkNo = lcTmpChkNo
        ENDIF
        lPrntCompl = .T.
        IF TYPE('lcXMLFileName') <> 'C'
          DO gfDispRe WITH EVAL('lcRepForm'),.F.,.F.,'R',.T.
        ELSE
          oAriaEnvironment.REPORT.OGLastForm = lcRepForm
          loProgress.Percent = 0.9
          loProgress.DESCRIPTION = "Printing Report..."
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
          *T20100512.0026 Hassan 2010 05 23 [END]

          PRIVATE loProxy
          loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

          IF loProxy.GetRequest(lcRequestID).STATUS = 3
            oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)

            loProgress.Percent = 1.0
            loProgress.DESCRIPTION = "Printing Report..."
            *T20100512.0026 Hassan 2010 05 23 [BEGIN]
            *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
            loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
            *T20100512.0026 Hassan 2010 05 23 [END]
          ENDIF
        ENDIF

      ENDIF
      =lfUnLokBank()    && unlock the bank record
    ENDIF
  ENDIF

CASE lcPrintMod = 'R'  && Reprint Checks
  IF lfLokBank()
    llSelectCheck = .F.
    lnPosCheck = ASCAN(loOGScroll.laOgFXFlt,"lcRePrnChk")
    IF lnPosCheck  > 0
      lnPosCheck  = ASUBSCRIPT(loOGScroll.laOgFXFlt,lnPosCheck  ,1)
      lcCursorChk= loOGScroll.laOgFXFlt[lnPosCheck,6]
      IF !EMPTY(lcCursorChk)
        SELECT(lcCursorChk)
        LOCATE
        IF !EOF()
          llSelectCheck = .T.
        ENDIF
      ENDIF
    ENDIF
    && If could lock the bank record
    lPrntCompl = .T.
    =lfCreateCur()     && Create temp file
    =lfSelectRec()     && select all detail lines for selected check number.
    SELECT (lcRpTargt)

    IF TYPE('lcXMLFileName') <> 'C'
      DO gfDispRe WITH EVAL('lcRepForm'),.F.,.F.,'R',.T.
    ELSE
      oAriaEnvironment.REPORT.OGLastForm = lcRepForm
      loProgress.Percent = 0.9
      loProgress.DESCRIPTION = "Printing Report..."
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      *T20100512.0026 Hassan 2010 05 23 [END]

      PRIVATE loProxy
      loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

      IF loProxy.GetRequest(lcRequestID).STATUS = 3
        oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)

        loProgress.Percent = 1.0
        loProgress.DESCRIPTION = "Printing Report..."
        *T20100512.0026 Hassan 2010 05 23 [BEGIN]
        *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        *T20100512.0026 Hassan 2010 05 23 [END]
      ENDIF
    ENDIF
    IF llPrintChk          && in case output device = 'Printer' or 'File'
      =lfRePrnUpd()        && update master file.
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    =lfUnLokBank()         && unlock the bank record
  ENDIF

ENDCASE

IF llPrintChk AND lPrntCompl
  *** Printing is completed.
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen("INM04124B00000","DIALOG")
  ENDIF

  IF lcPrintMod = 'R' AND TYPE('lcXMLFileName') <> 'C'
    IF !USED('APDISTA')
      =gfOpenTable('APDIST','CHECKS','SH','APDISTA')
    ENDIF
    =gfSqlRun("Select * from apdist where "+;
      " capdtrtyp = 'P' and cApdActID = 'A' AND cApdStat <> 'V'",'APDISTA',.T.,lcApDist)
    SELECT (lcApDist)
    CURSORSETPROP("Buffering" ,3)
    INDEX ON capdtrtyp+ cbnkcode+cchkacct+ cstubchk TAG (lcApDist)

    lnPosCheck = ASCAN(loOGScroll.laOgFXFlt,"lcRePrnChk")
    IF lnPosCheck  > 0
      lnPosCheck  = ASUBSCRIPT(loOGScroll.laOgFXFlt,lnPosCheck  ,1)
      lcCursorChk= loOGScroll.laOgFXFlt[lnPosCheck,6]
      IF !EMPTY(lcCursorChk)
        SELECT(lcCursorChk)
        ZAP
      ENDIF
    ENDIF
  ENDIF
ENDIF

*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
IF TYPE('lcXMLFileName') <> 'C'
  IF (oAriaApplication.gcDevice = "FILE" .AND. INLIST(loOGScroll.cTextRepType ,"EXCEL",'XML')) AND llPrintChk  AND lcPrintMod = 'V'
    lcNxtChkNo =  PADL(ALLTRIM(STR(INT(VAL(lcNxtChk))+1)),8,'0')
  ENDIF
  =loOGScroll.catchchanges ()
ENDIF
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]


SET CURRENCY TO lcSavCurSm
IF USED(lcVoid)
  USE IN (lcVoid)
ENDIF


*!************************************************************************
*!
*!      FUNCTION lfLockFile
*!
*!************************************************************************
* Lock the invoice header file , Count no of checks to be printed for
* each group and determin ,mark the begining of each group.
FUNCTION lfLockFile

SELECT (lcRpTargt)

lnCount     = 0
lnTotInvApv = 0
lnInvCount  = 1

GO BOTTOM
lcGroup = CVENDCODE + CAddress
REPLACE nNoOfInv WITH 0
DO WHILE !BOF()
  IF lcGroup = CVENDCODE + CAddress   && check if same group

    lnTotInvApv = lnTotInvApv + nInvFAAp
    REPLACE nNoOfInv WITH CEILING(lnCount/lnRpStub)
  ELSE
    IF lnTotInvApv < 0

      IF llPrintChk
        *** unlock all locked records.
        =lfClearLok()
      ENDIF

      *=gfModalGen("INM04084B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))
      *** Message :"Approved payment for vendor ð can not be lees than zero"
      *** Choice  :"                         < OK >                        "
      IF TYPE('lcXMLFileName') <> 'C'
        =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))
      ENDIF
      RETURN .F.
    ENDIF

    REPLACE nNoOfInv WITH 0     && Indicate to End Of Group.
    lnCount     = 0

    lnTotInvApv = nInvFAAp

    lcGroup     = CVENDCODE + CAddress
  ENDIF
  lnCount     = lnCount     + 1

  =gfSEEK(CinvNo+CVENDCODE,'APINVHDR')
  SELECT APINVHDR
  *** If device = 'File' Or 'Printer' and can lock cureent record.
  IF llPrintChk AND !lfObj_Lock(.T.)
    =gfReplace('')
    =gfTableUpdate()
    *** Invoice ð for vendor ð is being edited.
    *** Check printing is canceled.
    SELECT (lcRpTargt)
    lcInvNo = APINVHDR.CinvNo
    lcVendCode = APINVHDR.CVENDCODE
    SCAN REST
      SELECT APINVHDR
      =gfSEEK(CinvNo+CVENDCODE)
      IF APINVHDR.CinvNo+APINVHDR.CVENDCODE <> lcInvNo +lcVendCode
        =gfObj_Lock(.F.)
        =gfReplace('')
        =gfTableUpdate()
      ENDIF
    ENDSCAN

    RETURN .F.
  ENDIF
  =gfReplace('')
  =gfTableUpdate()

  SELECT (lcRpTargt)
  SKIP -1

  IF BOF() AND lnTotInvApv < 0

    IF llPrintChk
      *** unlock all locked records.
      =lfClearLok()
    ENDIF


    *** Message :"Approved payment for vendor ð can not be lees than zero"
    *** Choice  :"                         < OK >                        "
    IF TYPE('lcXMLFileName') <> 'C'
      =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))
    ENDIF
    RETURN .F.
  ENDIF
ENDDO

SELECT (lcRpTargt)
lnPgCntr  = 1
lnInvCntr = 0
GO TOP
lcGroup = CVENDCODE + CAddress
SCAN
  IF lcGroup  = CVENDCODE + CAddress .AND. lnInvCntr < lnRpStub
    lnInvCntr = lnInvCntr+1
  ELSE
    lnPgCntr  = lnPgCntr+1
    lnInvCntr = 1
  ENDIF
  REPLACE cPageNo WITH PADL(lnPgCntr,4)
  lcGroup = CVENDCODE + CAddress
ENDSCAN
GO TOP

*!************************************************************************
*!
*!      FUNCTION lfClearLok
*!
*!************************************************************************
*
FUNCTION lfClearLok
* unlocking all locked records in invoice header file.

SCAN REST
  SELECT APINVHDR
  =gfObj_Lock(.F.)
  =gfReplace('')
  =gfTableUpdate()
ENDSCAN

*!************************************************************************
*!
*!      FUNCTION lfUpInv
*!
*!************************************************************************
* Update the informations inside certine files while printing
FUNCTION lfUpInv

IF llEndGrp
  lcAls = ALIAS()
  SELECT (lcRpTargt)
  lnRecNow = IIF(!EOF(),IIF(!BOF(),RECNO(),0),-1)
  lcExstAdd = CVENDCODE+CAddress
  COUNT FOR CVENDCODE+CAddress = lcExstAdd TO lnNoOfIvs
  lnNoOfChks = INT(VAL(lcNxtChkNo)) + CEILING(lnNoOfIvs/lnRpStub)
  IF !EMPTY(lcAls)
    SELECT (lcAls)
    IF lnRecNow > 0
      GOTO lnRecNow
    ELSE
      IF lnRecNow = 0
        GOTO TOP
      ELSE
        GOTO BOTTOM
      ENDIF
    ENDIF
  ENDIF
  REPLACE APCHECKS.NCHKNXTPN WITH lnNoOfChks

  =gfSEEK(CVENDCODE,'APVENDOR')

  lnStubChkNo = INT(VAL(lcNxtChkNo))-1
  lcChkNo = PADL(APCHECKS.NCHKNXTPN - 1,8,'0')


  SELECT APPAYMNT
  lcNewrec = lfGetNewRec('APPAYMNT')
  m.Rec_no = lcNewrec
  *APPEND BLANK
  gfAppend('APPAYMNT',.T.)
  REPLACE                                ;
    CPAYTYPE  WITH 'P'                   ;
    CPAYDOCNO WITH lcChkNo               ;
    CPAYMETH  WITH 'P'                   ;
    CPAYSTAT  WITH 'B'                   ;
    DPAYDATE  WITH ldChkDat            ;
    CFISFYEAR WITH lcFisYear             ;
    CFSPPRDID WITH lcFisPrd              ;
    DPAYVDATE WITH {}                    ;
    CPAYCLNO  WITH &lcRpTargt..CVENDCODE ;
    CPAYCOMP  WITH &lcRpTargt..COUTCOMP  ;
    NPAYAMNT  WITH 0                     ;
    NPAYDISC  WITH 0                     ;
    NPAYADJ   WITH 0                     ;
    LPAYADVAN WITH .F.                   ;
    NINV1099A WITH 0                     ;
    cbnkcode  WITH lcRpBnkCod            ;
    CPAYRECST WITH 'O'                   ;
    cchkacct  WITH lcRpChkAct,;
    Rec_no    WITH lcNewrec

  IF TYPE('lcXMLFileName') <> 'C'
    =gfAdd_Info('APPAYMNT')
  ELSE
    loAddUserInfo.DO('APPAYMNT',.NULL.)
  ENDIF
  =gfReplace('')

  REPLACE                                ;
    APCHECKS.DCHKLPDAT WITH ldChkDat   ;
    APCHECKS.NCHKLPAMT WITH 0

  SELECT  APCHECKS
  =gfReplace('')

  REPLACE                                ;
    APVENDOR.DVENLPAYD WITH ldChkDat   ;
    APVENDOR.NVENLPAYA WITH 0            ;
    APVENDOR.CVENLPAYN WITH lcChkNo

  SELECT APVENDOR
  =gfReplace('')

  SELECT (lcRpTargt)
  lnRpTrgRec = RECNO()
  lnStubCnt  = 0

  lnChkExUnt = 0
  lnChkExRat = 0
  IF gfGetMemVar('LLMULCURR')
    lnChkExRat = gfChkRate('lnChkExUnt',&lcRpTargt..CAPRCURCOD,ldChkDat,.T.,.F.)
    IF lnChkExRat = 0
      lnChkExUnt = 1
      lnChkExRat = 1
    ENDIF
  ELSE
    lnChkExUnt = 1
    lnChkExRat = 1
  ENDIF


  SCAN REST
    lnStubCnt = lnStubCnt + 1

    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,&lcRpTargt..cCurrCode)
    lcExSin4 = ' '
    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,&lcRpTargt..CAPRCURCOD)
    lnAprPayB  = ROUND(&lcRpTargt..nInvFAAp &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    lnAprDisB  = ROUND(&lcRpTargt..NINVDISAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnAprAdjB  = ROUND(&lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnApr1099B = ROUND(&lcRpTargt..NINVA1099 &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(&lcRpTargt..nInvFAAp,2);
      APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB;
      APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB;
      APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    REPLACE APPAYMNT.cCurrCode  WITH APCHECKS.cCurrCode ;
      APPAYMNT.NEXRATE   WITH lnChkExRat ;
      APPAYMNT.NCURRUNIT WITH lnChkExUnt

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APPAYMNT',.NULL.)
    ENDIF
    SELECT APPAYMNT
    =gfReplace("")

    REPLACE APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + lnAprPayB ;
      APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + lnApr1099B;
      APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnAprPayB ;
      APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - ROUND((&lcRpTargt..NINVAMTAP+&lcRpTargt..NINVDISAP+&lcRpTargt..NINVADJAP) &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2);
      APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + ROUND(IIF(&lcRpTargt..NINVAMNT < 0,&lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,0),2)

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APVENDOR')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APVENDOR',.NULL.)
    ENDIF
    SELECT APVENDOR
    =gfReplace("")

    REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(&lcRpTargt..nInvFAAp,2)

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APCHECKS')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APCHECKS',.NULL.)
    ENDIF
    SELECT APCHECKS
    =gfReplace("")

    =gfSEEK(CVENDCODE + lcFisYear,'APVENHST')
    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + lnAprDisB;
      APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnAprPayB;
      APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + lnAprAdjB;
      APVENHST.NVNHMCHKP  WITH APVENHST.NVNHMCHKP  + lnAprPayB;
      APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + lnAprPayB

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APVENHST')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APVENHST',.NULL.)
    ENDIF
    SELECT APVENHST
    =gfReplace("")
    lcNewrec = lfGetNewRec('APDIST')
    SELECT APDIST
    *APPEND BLANK
    m.Rec_no = lcNewrec
    gfAppend('APDIST',.T.)
    REPLACE CVENDCODE  WITH &lcRpTargt..CVENDCODE;
      CinvNo     WITH &lcRpTargt..CinvNo;
      capdtrtyp  WITH 'P';
      DAPDTRDAT  WITH ldChkDat;
      LAPDPOST   WITH  .F.;
      CAPDSTAT   WITH '';
      CAPDREF    WITH lcChkNo;
      cstubchk   WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
      CAPDGLACT  WITH &lcRpTargt..CAPACCT;
      CAPDACTID  WITH 'A';
      CBATCHNO   WITH '';
      CTRNSLEDN  WITH '';
      CFISFYEAR  WITH lcFisYear;
      CFSPPRDID  WITH lcFisPrd;
      CAPSESSNO  WITH lcSession;
      CTAXCODE   WITH '';
      cbnkcode   WITH lcRpBnkCod;
      cchkacct   WITH lcRpChkAct;
      NAPDAMNT   WITH &lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP;
      cCurrCode  WITH &lcRpTargt..cCurrCode;
      NEXRATE    WITH &lcRpTargt..NEXRATE;
      NCURRUNIT  WITH &lcRpTargt..NCURRUNIT;
      NEQVAMNT   WITH ROUND((&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT + lnAprDisB + lnAprAdjB),2),;
      Rec_no     WITH  lcNewrec

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF

    =gfReplace("")
    IF &lcRpTargt..NINVA1099 <> 0
      lcNewrec = lfGetNewRec('APDIST')
      *APPEND BLANK
      m.Rec_no = lcNewrec
      gfAppend('APDIST',.T.)
      REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
        CinvNo      WITH &lcRpTargt..CinvNo;
        capdtrtyp   WITH 'P';
        DAPDTRDAT   WITH ldChkDat;
        LAPDPOST    WITH .F.;
        CAPDSTAT    WITH 'V';
        CAPDREF     WITH lcChkNo;
        cstubchk    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
        CAPDGLACT   WITH '';
        NAPDAMNT    WITH &lcRpTargt..NINVA1099;
        CAPDACTID   WITH 'B';
        CBATCHNO    WITH '';
        CTRNSLEDN   WITH '';
        CFISFYEAR   WITH lcFisYear;
        CFSPPRDID   WITH lcFisPrd;
        CAPSESSNO   WITH lcSession;
        CTAXCODE    WITH '';
        cbnkcode    WITH lcRpBnkCod;
        cchkacct    WITH lcRpChkAct;
        cCurrCode   WITH &lcRpTargt..cCurrCode;
        NEXRATE     WITH &lcRpTargt..NEXRATE;
        NCURRUNIT   WITH &lcRpTargt..NCURRUNIT;
        NEQVAMNT    WITH &lcRpTargt..NINVA1099 ,;
        Rec_no     WITH  lcNewrec

      IF  TYPE('lcXMLFileName') <> 'C'
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF

      =gfReplace("")
    ENDIF

    *APPEND BLANK
    lcNewrec = lfGetNewRec('APDIST')
    m.Rec_no = lcNewrec
    gfAppend('APDIST',.T.)
    REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
      CinvNo      WITH &lcRpTargt..CinvNo;
      capdtrtyp   WITH 'P';
      DAPDTRDAT   WITH ldChkDat;
      LAPDPOST    WITH .F.;
      CAPDSTAT    WITH '';
      CAPDREF     WITH lcChkNo;
      cstubchk    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
      CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
      NAPDAMNT    WITH -&lcRpTargt..nInvFAAp;
      CAPDACTID   WITH 'C';
      CBATCHNO    WITH '';
      CTRNSLEDN   WITH '';
      CFISFYEAR   WITH lcFisYear;
      CFSPPRDID   WITH lcFisPrd;
      CAPSESSNO   WITH lcSession;
      CTAXCODE    WITH '';
      cbnkcode    WITH lcRpBnkCod;
      cchkacct    WITH lcRpChkAct;
      cCurrCode   WITH &lcRpTargt..CAPRCURCOD;
      NEXRATE     WITH lnChkExRat;
      NCURRUNIT   WITH lnChkExUnt;
      NEQVAMNT    WITH -lnAprPayB,;
      Rec_no     WITH  lcNewrec

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF

    =gfReplace("")
    IF &lcRpTargt..NINVDISAP <> 0
      *APPEND BLANK
      lcNewrec = lfGetNewRec('APDIST')
      m.Rec_no = lcNewrec
      gfAppend('APDIST',.T.)

      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
        CinvNo    WITH &lcRpTargt..CinvNo;
        capdtrtyp WITH 'P';
        DAPDTRDAT WITH ldChkDat;
        LAPDPOST  WITH .F.;
        CAPDSTAT  WITH '';
        CAPDREF   WITH lcChkNo;
        cstubchk  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
        NAPDAMNT  WITH -&lcRpTargt..NINVDISAP;
        CAPDACTID WITH 'S';
        CBATCHNO  WITH '';
        CTRNSLEDN WITH '';
        CFISFYEAR WITH lcFisYear;
        CFSPPRDID WITH lcFisPrd;
        CAPSESSNO WITH lcSession;
        CTAXCODE  WITH '';
        cbnkcode  WITH lcRpBnkCod;
        cchkacct  WITH lcRpChkAct;
        CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CDISCACCT),APCHECKS.CDISCACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),APDIV.CDISCACCT,APSETUP.CDISCACCT));
        cCurrCode WITH &lcRpTargt..cCurrCode;
        NEXRATE   WITH &lcRpTargt..NEXRATE;
        NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
        NEQVAMNT  WITH -lnAprDisB,;
        Rec_no     WITH  lcNewrec

      IF   TYPE('lcXMLFileName') <> 'C'
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF

      =gfReplace("")
    ENDIF

    IF &lcRpTargt..NINVADJAP <> 0

      lcNewrec = lfGetNewRec('APDIST')
      *APPEND BLANK
      m.Rec_no = lcNewrec
      gfAppend('APDIST',.T.)
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
        CinvNo    WITH &lcRpTargt..CinvNo;
        capdtrtyp WITH 'P';
        DAPDTRDAT WITH ldChkDat;
        LAPDPOST  WITH .F.;
        CAPDSTAT  WITH '';
        CAPDREF   WITH lcChkNo;
        cstubchk  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
        NAPDAMNT  WITH -&lcRpTargt..NINVADJAP;
        CAPDACTID WITH 'J';
        CBATCHNO  WITH '';
        CTRNSLEDN WITH '';
        CFISFYEAR WITH lcFisYear;
        CFSPPRDID WITH lcFisPrd;
        CAPSESSNO WITH lcSession;
        CTAXCODE  WITH '';
        cbnkcode  WITH lcRpBnkCod;
        cchkacct  WITH lcRpChkAct;
        CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CADJACCT),APCHECKS.CADJACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),APDIV.CADJACCT,APSETUP.CADJACCT));
        cCurrCode WITH &lcRpTargt..cCurrCode;
        NEXRATE   WITH &lcRpTargt..NEXRATE;
        NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
        NEQVAMNT  WITH -lnAprAdjB,;
        Rec_no     WITH lcNewrec

      IF   TYPE('lcXMLFileName') <> 'C'
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF

      =gfReplace("")
    ENDIF

    lnExchDiff = lnAprPayB - ROUND(&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    IF lnExchDiff <> 0
      *APPEND BLANK
      lcNewrec = lfGetNewRec('APDIST')
      m.Rec_no = lcNewrec
      gfAppend('APDIST',.T.)

      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
        CinvNo    WITH &lcRpTargt..CinvNo;
        capdtrtyp WITH 'P';
        DAPDTRDAT WITH ldChkDat;
        LAPDPOST  WITH .F.;
        CAPDSTAT  WITH '';
        CAPDREF   WITH lcChkNo;
        cstubchk  WITH PADL(lnStubChkNo+CEILING(lnStubCnt/lnRpStub),8,'0');
        NAPDAMNT  WITH lnExchDiff;
        CAPDACTID WITH 'J';
        CBATCHNO  WITH '';
        CTRNSLEDN WITH '';
        CFISFYEAR WITH lcFisYear;
        CFSPPRDID WITH lcFisPrd;
        CAPSESSNO WITH lcSession;
        CTAXCODE  WITH '';
        cbnkcode  WITH lcRpBnkCod;
        cchkacct  WITH lcRpChkAct;
        CAPDGLACT WITH lcExDifAcc;
        cCurrCode WITH gcBaseCurr;
        NEXRATE   WITH 1;
        NCURRUNIT WITH 1;
        NEQVAMNT  WITH lnExchDiff;
        NAPDLINNO WITH 1,;
        Rec_no     WITH  lcNewrec

      IF   TYPE('lcXMLFileName') <> 'C'
        =gfAdd_Info('APDIST')  && Add the audit information to the record.
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF

      =gfReplace('')
    ENDIF
    =gfSEEK(&lcRpTargt..CinvNo+&lcRpTargt..CVENDCODE,'APINVHDR')
    REPLACE APINVHDR.NINVPAID   WITH &lcRpTargt..NINVPAID  + &lcRpTargt..NINVAMTAP;
      APINVHDR.NINVDISTK  WITH &lcRpTargt..NINVDISTK + &lcRpTargt..NINVDISAP;
      APINVHDR.NINVADJ    WITH &lcRpTargt..NINVADJ   + &lcRpTargt..NINVADJAP;
      APINVHDR.NINV1099A  WITH &lcRpTargt..NINV1099A + &lcRpTargt..NINVA1099;
      APINVHDR.cbnkcode   WITH '';
      APINVHDR.cchkacct   WITH '';
      APINVHDR.CCHKGLACC  WITH '';
      APINVHDR.NINVAMTAP  WITH 0 ;
      APINVHDR.NINVDISAP  WITH 0 ;
      APINVHDR.NINVADJAP  WITH 0 ;
      APINVHDR.NINVA1099  WITH 0 ;
      APINVHDR.CAPRCURCOD WITH '';
      APINVHDR.NAPREXRAT  WITH 0 ;
      APINVHDR.NAPRCURUNT WITH 0 ;
      APINVHDR.DCHKDATE   WITH ldChkDat;
      APINVHDR.CCHKNO     WITH lcChkNo,;
      APINVHDR.nInvFAAp   WITH 0

    SELECT APINVHDR
    =gfReplace('')
    =gfTableUpdate()
    =gfSEEK(&lcRpTargt..CinvNo+&lcRpTargt..CVENDCODE,'APINVHDR')
    =gfObj_Lock(.F.)
    =gfReplace('')
    =gfTableUpdate()

    lnAprPayB  = 0
    lnAprDisB  = 0
    lnAprAdjB  = 0
    lnApr1099B = 0

    SELECT (lcRpTargt)
    IF &lcRpTargt..nNoOfInv = 0
      EXIT
    ENDIF
  ENDSCAN
  GO lnRpTrgRec
ENDIF

IF &lcRpTargt..nNoOfInv = 0
  REPLACE APPAYMNT.CPAYSTAT  WITH ' '
  llEndGrp = .T.
ELSE
  llEndGrp = .F.
ENDIF
SELECT APPAYMNT
=gfTableUpdate()
SELECT APINVHDR
=gfTableUpdate()
SELECT APDIST
=gfTableUpdate()
SELECT APVENDOR
=gfTableUpdate()
SELECT APCHECKS
=gfTableUpdate()
SELECT APVENHST
=gfTableUpdate()
RETURN ''

*!************************************************************************
*!
*!      FUNCTION lfIncNxtChk
*!
*!************************************************************************
* Incriment the check number
FUNCTION lfIncNxtChk
*B600710,1 MAN 10/02/95 the next check number.
*REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) + 1
*lcNxtChkNo = PADL(APCHECKS.NCHKNXTPN,8,'0')
lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')

RETURN ''

*!************************************************************************
*!
*!      FUNCTION lfIniTotal
*!
*!************************************************************************
*
FUNCTION lfIniTotal

IF llPrTot
  lnTotal = 0
  llPrTot = .F.
ENDIF
IF lcPrintMod = 'R'
  lcInvToPrn = CinvNo+CVENDCODE+'P'
  SELECT APDIST
  lnKeepRec=RECNO()
  PRIVATE lcOrder
  lcOrder = ORDER()
  =gfSetOrder('INVVEND')
  =gfSEEK(lcInvToPrn)
  LOCATE REST WHILE CinvNo+CVENDCODE+capdtrtyp=lcInvToPrn FOR CAPDSTAT<>'V'
  lcChkPrntd = cstubchk
  lcMastChk = CAPDREF
  gfSetOrder('PAYMNTS')
  =gfSEEK('P'+lcRpBnkCod+lcRpChkAct+lcMastChk)
  SUM  REST NAPDAMNT WHILE capdtrtyp+cbnkcode+cchkacct+CAPDREF='P'+lcRpBnkCod+lcRpChkAct+lcMastChk  ;
    AND cstubchk<>lcChkPrntd FOR CAPDSTAT <> 'V' AND CAPDACTID = 'C';
    TO lnTotal
  IF !EMPTY(lcOrder)
    gfSetOrder(lcOrder)
  ENDIF
  IF  BETWEEN(lnKeepRec,1,RECCOUNT())
    GO lnKeepRec
  ENDIF
  SELECT (lcRpTargt)
  lnTotal=ABS(lnTotal)
ENDIF
RETURN ''

*!************************************************************************
*!
*!      FUNCTION lfSumGrp
*!
*!************************************************************************
*
FUNCTION lfSumGrp

lnTotal = lnTotal + nInvFAAp
IF nNoOfInv = 0
  llPrTot = .T.
ENDIF
RETURN ''


*!**************************************************************************
*!
*!      Function lfvOkAdvPay
*!
*!**************************************************************************
*
FUNCTION lfvOkAdvPay
PARAMETERS loFormSet

IF EMPTY(lcDebMemN)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'debit memo number')
  RETURN .F.
ELSE
  SELECT APINVHDR
  gfSetOrder('VENDINV')
  IF gfSEEK(lcRpVenCod+lcDebMemN,'APINVHDR')
    *** ð exists for the vendor ð.
    =gfModalGen("TRM04024B00000","DIALOG",'Invoice '+ALLTRIM(lcDebMemN)+ ;
      '|'+ALLTRIM(lcRpVenCod))
    RETURN .F.
  ENDIF
  =gfSetOrder('INVVEND')
ENDIF

IF lnPaymnt <= 0
  *** The amount to be applied should be greater than zero.
  =gfModalGen("TRM04029B00000","DIALOG")
  RETURN .F.
ELSE
  IF !BETWEEN(ln1099Amnt,0,lnPaymnt)
    *** The 1099 amount must be between ð and ð.
    =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPaymnt))
    RETURN .F.
  ENDIF
ENDIF

IF EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0'))
  *** 'ð cannot be empty.'
  =gfModalGen("INM04074B00000","DIALOG",'AP account')
  RETURN .F.
ENDIF

IF ! lfValidAcct(lcApAcct)
  *** ð not found.
  =gfModalGen("INM04002B00000","DIALOG",'A/P account|'+ALLTRIM(lcApAcct))
  RETURN .F.
ENDIF

*** If remitting to factor, and there is no factor code,
*** do not proceed and present the following message
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  *** Message : "   You have to enter the ð.  "
  ***                 <  OK  >
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  RETURN .F.
ENDIF
llOkAdvPay = .T.

*!************************************************************************
*!
*!      Function lfSortAddr
*!
*!************************************************************************
* Return the address sorted by the user
FUNCTION lfSortAddr

DIMENSION laAddrOrd(3,2)
** Assign seq. no to the array by the order of the address fields in the file.
laAddrOrd[1,1]  = '3'
laAddrOrd[2,1]  = '4'
laAddrOrd[3,1]  = '5'
lcReturnVal     = ' '

IF gfSEEK(APBANKS.cCont_Code,'SYCINT')
  ** Assign the order of the address to the second column of the array.
  laAddrOrd[1,2]  = SYCINT.NPART3ORD
  laAddrOrd[2,2]  = SYCINT.NPART4ORD
  laAddrOrd[3,2]  = SYCINT.NPART5ORD
  =ASORT(laAddrOrd,2)  && Sort the Temp array.
  FOR lnCounter = 3 TO 5
    lcPostion   = laAddrOrd[lnCounter,1]
    lcReturnVal = ALLTRIM(lcReturnVal)+' '+ALLTRIM(APBANKS.CAddress&lcPostion)
  ENDFOR
ELSE
  ** We are going to take the address as it is.
  lcReturnVal = ALLTRIM(APBANKS.CADDRESS3) + ' ' + ;
    ALLTRIM(APBANKS.CADDRESS4) + ' ' + ;
    ALLTRIM(APBANKS.CADDRESS5)
ENDIF
RETURN lcReturnVal
*!************************************************************************
*!
*!      FUNCTION lfCreateCur
*!
*!************************************************************************
FUNCTION lfCreateCur

DIMENSION laFieldStr(1,18)
SELECT APINVHDR
lnFieldNo = AFIELD(laFieldStr)
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
*DIMENSION laFieldStr(lnFieldNo+3,18)
DIMENSION laFieldStr(lnFieldNo+4,18)
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]

laFieldStr[lnFieldNo+1,1] = 'CADDRESS'
laFieldStr[lnFieldNo+1,2] = 'C'
laFieldStr[lnFieldNo+1,3] = 240
laFieldStr[lnFieldNo+1,4] = 0

laFieldStr[lnFieldNo+2,1] = 'nNoOfInv'
laFieldStr[lnFieldNo+2,2] = 'N'
laFieldStr[lnFieldNo+2,3] = 6
laFieldStr[lnFieldNo+2,4] = 0

laFieldStr[lnFieldNo+3,1] = 'cPageNo'
laFieldStr[lnFieldNo+3,2] = 'C'
laFieldStr[lnFieldNo+3,3] = 4
laFieldStr[lnFieldNo+3,4] = 0

*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
*FOR lnCount = 1 TO 3
laFieldStr[lnFieldNo+4,1] = 'cvencomp'
laFieldStr[lnFieldNo+4,2] = 'C'
laFieldStr[lnFieldNo+4,3] = 30
laFieldStr[lnFieldNo+4,4] = 0
FOR lnCount = 1 TO 4
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  STORE '' TO laFieldStr[lnFieldNo+lnCount,7],laFieldStr[lnFieldNo+lnCount,8],laFieldStr[lnFieldNo+lnCount,9],;
    laFieldStr[lnFieldNo+lnCount,10],laFieldStr[lnFieldNo+lnCount,11],laFieldStr[lnFieldNo+lnCount,12],;
    laFieldStr[lnFieldNo+lnCount,13],laFieldStr[lnFieldNo+lnCount,14],laFieldStr[lnFieldNo+lnCount,15],;
    laFieldStr[lnFieldNo+lnCount,16]
  STORE 0 TO  laFieldStr[lnFieldNo+lnCount,17],laFieldStr[lnFieldNo+lnCount,18]
ENDFOR
=gfCrtTmp(lcRpTargt ,@laFieldStr)
RETURN lnFieldNo + 3
*!************************************************************************
*!
*!      FUNCTION lfLokBank
*!
*!************************************************************************
* Lock the bank record
FUNCTION lfLokBank

llRetVal = .T.

IF !llPrintChk
  RETURN llRetVal
ENDIF

llRetVal = .T.
SELECT APBANKS
IF gfSEEK(lcRpBnkCod)
  IF gfObj_Lock(.T.)
    =gfReplace('')
    =gfTableUpdate()

    IF gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
      IF INT(VAL(lcNxtChkNo)) < APCHECKS.NCHKNXTPN
        *** Next check number is ð. Do you wish to proceed with check printing?
        lnOption = TYPE('lcXMLFileName') <> 'C' AND  gfModalGen("QRM04086B00012","DIALOG",ALLTRIM(STR(APCHECKS.NCHKNXTPN)))
        lcNxtChkNo = PADL(APCHECKS.NCHKNXTPN,8,'0')
        IF lnOption = 2
          llRetVal = .F.
          =gfObj_Lock(.F.)
          =gfReplace('')
          =gfTableUpdate()
        ENDIF
      ENDIF
    ELSE
      *** ð has been deleted.
      IF TYPE('lcXMLFileName') <> 'C'
        =gfModalGen("QRM04095B00000","DIALOG",'Checking account')
      ENDIF
      llRetVal = .F.
      =gfObj_Lock(.F.)
      =gfReplace('')
      =gfTableUpdate()
    ENDIF
  ELSE
    *** Bank ð is being edited by user ð.
    IF TYPE('lcXMLFileName') <> 'C'
      =gfModalGen("INM04085B00000","DIALOG",lcRpBnkCod+'|'+CLOK_USER)
    ENDIF
    llRetVal = .F.
  ENDIF
ELSE
  *** ð has been deleted.
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen("QRM04095B00000","DIALOG",'Bank code')
  ENDIF
  llRetVal = .F.
ENDIF

RETURN llRetVal

*!************************************************************************
*!
*!      FUNCTION lfUnLokBank
*!
*!************************************************************************
* UnLock the bank record
FUNCTION lfUnLokBank

SELECT APBANKS
=gfSEEK(lcRpBnkCod)
=gfObj_Lock(.F.)
=gfReplace('')
=gfTableUpdate()
*!**************************************************************************
*!
*!      Function lfAdvPay
*!
*!**************************************************************************
*
FUNCTION lfAdvPay
SELECT (lcDebtCurs)
SCAN
  =gfSEEK(ALLTRIM(&lcDebtCurs..CVENDCODE),'APVENDOR')
  lcRpVenCod  = &lcDebtCurs..CVENDCODE
  lcDebMemN   = &lcDebtCurs..cDebMemN
  lcFactor    = &lcDebtCurs..CFactor
  lcAdvDiv    = &lcDebtCurs..CDIVISION
  lcRef       = &lcDebtCurs..REFERENCE
  lcRem1      = &lcDebtCurs..Rem1
  lcRem2      = &lcDebtCurs..Rem2
  lcRem3      = &lcDebtCurs..Rem3
  lcRem4      = &lcDebtCurs..Rem4
  lcRem5      = &lcDebtCurs..Rem5
  lcRem6      = &lcDebtCurs..Rem6
  lcApAcct    = &lcDebtCurs..CAPACCT
  lnPaymnt    = &lcDebtCurs..nPaymnt
  ln1099Amnt  = &lcDebtCurs..n1099Amnt
  lcInvRemit  = &lcDebtCurs..cInvRemit
  lnEqvAmnt   = lnPaymnt
  IF !lfvOkAdvPay()
    LOOP
  ENDIF
  SELECT (lcRpTargt)
  APPEND BLANK
  REPLACE CVENDCODE   WITH lcRpVenCod  ;
    CinvNo      WITH lcDebMemN   ;
    CDIVISION   WITH lcAdvDiv    ;
    DINVDATE    WITH ldChkDat    ;
    CINVREF     WITH lcRef       ;
    cInvRemit   WITH lcInvRemit  ;
    CFACCODE    WITH lcFactor    ;
    COUTCOMP    WITH lcRem1      ;
    COUTADDR1   WITH lcRem2      ;
    COUTADDR2   WITH lcRem3      ;
    COUTADDR3   WITH lcRem4      ;
    COUTADDR4   WITH lcRem5      ;
    COUTADDR5   WITH lcRem6      ;
    NINVAMNT    WITH 0           ;
    NINVDISOF   WITH 0           ;
    NINVAMTAP   WITH lnPaymnt    ;
    NINVDISAP   WITH 0           ;
    NINVADJAP   WITH 0           ;
    NINVPAID    WITH 0           ;
    NINVDISTK   WITH 0           ;
    NINVADJ     WITH 0           ;
    NINVA1099   WITH ln1099Amnt  ;
    NINV1099A   WITH 0           ;
    CVENPMETH   WITH 'P'         ;
    CTERMCODE   WITH ''          ;
    NTERDUED    WITH 0           ;
    NTERDISCD   WITH 0           ;
    NTERDISCR   WITH 0           ;
    DINVDUDAT   WITH ldChkDat    ;
    cbnkcode    WITH ''          ;
    cchkacct    WITH ''          ;
    CCHKGLACC   WITH ''          ;
    CCHKNO      WITH ''          ;
    DCHKDATE    WITH {}          ;
    CINVSTAT    WITH 'A'         ;
    CVENCCVEN   WITH ''          ;
    CVENCCINV   WITH ''          ;
    CAPACCT     WITH lcApAcct    ;
    CFISFYEAR   WITH lcFisYear   ;
    CFSPPRDID   WITH lcFisPrd    ;
    nNoOfInv    WITH 0           ;
    nInvFAAp    WITH lnPaymnt

  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  =gfSEEK(lcRpVenCod  ,'apvendor','VENCODE')
  m.cvencomp = APVENDOR.cvencomp
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]

  IF llPrintChk
    SELECT APINVHDR
    APPEND BLANK
    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      REPLACE CVENDCODE   WITH lcRpVenCod  ;
        CinvNo      WITH lcDebMemN   ;
        CDIVISION   WITH lcAdvDiv    ;
        DINVDATE    WITH ldChkDat    ;
        CINVREF     WITH lcRef       ;
        cInvRemit   WITH lcInvRemit  ;
        CFACCODE    WITH lcFactor    ;
        COUTCOMP    WITH lcRem1      ;
        COUTADDR1   WITH lcRem2      ;
        COUTADDR2   WITH lcRem3      ;
        COUTADDR3   WITH lcRem4      ;
        COUTADDR4   WITH lcRem5      ;
        COUTADDR5   WITH lcRem6      ;
        NINVAMNT    WITH 0 - lnPaymnt;
        NINVDISOF   WITH 0           ;
        NINVAMTAP   WITH 0           ;
        NINVDISAP   WITH 0           ;
        NINVADJAP   WITH 0           ;
        NINVPAID    WITH 0           ;
        NINVDISTK   WITH 0           ;
        NINVADJ     WITH 0           ;
        NINVA1099   WITH 0           ;
        CVENPMETH   WITH 'P'         ;
        CTERMCODE   WITH ''          ;
        NTERDUED    WITH 0           ;
        NTERDISCD   WITH 0           ;
        NTERDISCR   WITH 0           ;
        DINVDUDAT   WITH ldChkDat    ;
        cbnkcode    WITH ''          ;
        cchkacct    WITH ''          ;
        CCHKGLACC   WITH ''          ;
        CCHKNO      WITH ''          ;
        DCHKDATE    WITH {}          ;
        CINVSTAT    WITH 'A'         ;
        CVENCCVEN   WITH ''          ;
        CVENCCINV   WITH ''          ;
        CAPACCT     WITH lcApAcct    ;
        CFISFYEAR   WITH lcFisYear   ;
        CFSPPRDID   WITH lcFisPrd    ;
        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
        DCHKDATE    WITH ldChkDat    ;
        CCHKNO      WITH lcNxtChkNo  ;
        DPOSTDATE   WITH ldChkDat    ;
        nInvFAAp    WITH 0           ;
        cCurrCode   WITH lcCurrCode  ;
        NCURRUNIT   WITH lnCurrUnit  ;
        NEXRATE     WITH lnExRate


    ELSE
      REPLACE CVENDCODE   WITH lcRpVenCod  ;
        CinvNo      WITH lcDebMemN   ;
        CDIVISION   WITH lcAdvDiv    ;
        DINVDATE    WITH ldChkDat    ;
        CINVREF     WITH lcRef       ;
        cInvRemit   WITH lcInvRemit  ;
        CFACCODE    WITH lcFactor    ;
        COUTCOMP    WITH lcRem1      ;
        COUTADDR1   WITH lcRem2      ;
        COUTADDR2   WITH lcRem3      ;
        COUTADDR3   WITH lcRem4      ;
        COUTADDR4   WITH lcRem5      ;
        COUTADDR5   WITH lcRem6      ;
        NINVAMNT    WITH 0 - lnPaymnt;
        NINVDISOF   WITH 0           ;
        NINVAMTAP   WITH 0           ;
        NINVDISAP   WITH 0           ;
        NINVADJAP   WITH 0           ;
        NINVPAID    WITH 0           ;
        NINVDISTK   WITH 0           ;
        NINVADJ     WITH 0           ;
        NINVA1099   WITH 0           ;
        CVENPMETH   WITH 'P'         ;
        CTERMCODE   WITH ''          ;
        NTERDUED    WITH 0           ;
        NTERDISCD   WITH 0           ;
        NTERDISCR   WITH 0           ;
        DINVDUDAT   WITH ldChkDat    ;
        cbnkcode    WITH ''          ;
        cchkacct    WITH ''          ;
        CCHKGLACC   WITH ''          ;
        CCHKNO      WITH ''          ;
        DCHKDATE    WITH {}          ;
        CINVSTAT    WITH 'A'         ;
        CVENCCVEN   WITH ''          ;
        CVENCCINV   WITH ''          ;
        CAPACCT     WITH lcApAcct    ;
        CFISFYEAR   WITH lcFisYear   ;
        CFSPPRDID   WITH lcFisPrd    ;
        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
        DCHKDATE    WITH ldChkDat    ;
        CCHKNO      WITH lcNxtChkNo  ;
        nInvFAAp    WITH 0           ;
        cCurrCode   WITH lcCurrCode  ;
        NCURRUNIT   WITH lnCurrUnit  ;
        NEXRATE     WITH lnExRate
    ENDIF

    IF      TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APINVHDR')
    ELSE
      loAddUserInfo.DO('APINVHDR',.NULL.)
    ENDIF


    =gfReplace('')
    =gfTableUpdate()
    SELECT APPAYMNT
    *APPEND BLANK
    lcNewrec = lfGetNewRec('APPAYMNT')
    m.Rec_no = lcNewrec
    gfAppend('APPAYMNT',.T.)

    REPLACE CPAYTYPE  WITH 'P'      ;
      CPAYDOCNO WITH lcNxtChkNo  ;
      CPAYMETH  WITH 'P'         ;
      CPAYSTAT  WITH ''          ;
      DPAYDATE  WITH ldChkDat    ;
      CFISFYEAR WITH lcFisYear   ;
      CFSPPRDID WITH lcFisPrd    ;
      DPAYVDATE WITH {}          ;
      CPAYCLNO  WITH lcRpVenCod  ;
      CPAYCOMP  WITH lcRem1      ;
      NPAYAMNT  WITH lnPaymnt    ;
      NPAYDISC  WITH 0           ;
      NPAYADJ   WITH 0           ;
      LPAYADVAN WITH .T.         ;
      NINV1099A WITH ln1099Amnt  ;
      cbnkcode  WITH lcRpBnkCod  ;
      CPAYRECST WITH 'O'         ;
      cchkacct  WITH lcRpChkAct  ;
      cCurrCode WITH lcCurrCode  ;
      NCURRUNIT WITH lnCurrUnit  ;
      NEXRATE   WITH lnExRate,;
      Rec_no   WITH lcNewrec

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APPAYMNT')
    ELSE
      loAddUserInfo.DO('APPAYMNT',.NULL.)
    ENDIF

    =gfReplace('')
    =gfTableUpdate()
    REPLACE APCHECKS.DCHKLPDAT WITH ldChkDat ;
      APCHECKS.NCHKLPAMT WITH lnPaymnt


    REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) + 1
    SELECT APCHECKS
    =gfReplace('')
    =gfTableUpdate()

    REPLACE APVENDOR.DVENLPAYD WITH ldChkDat                        ;
      APVENDOR.NVENLPAYA WITH lnPaymnt                        ;
      APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + ln1099Amnt ;
      APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + lnPaymnt   ;
      APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnPaymnt   ;
      APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - lnPaymnt   ;
      APVENDOR.CVENLPAYN WITH lcNxtChkNo
    SELECT APVENDOR
    =gfReplace('')
    =gfTableUpdate()
    SELECT APVENHST
    =gfSEEK(ALLTRIM(lcRpVenCod) + lcFisYear)
    REPLACE APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnPaymnt ;
      APVENHST.NVNHPCHKP  WITH APVENHST.NVNHPCHKP  + lnPaymnt ;
      APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + lnPaymnt
    =gfReplace('')
    =gfTableUpdate()
    SELECT APDIST
    *APPEND BLANK
    lcNewrec = lfGetNewRec('APDIST')
    m.Rec_no = lcNewrec
    gfAppend('APDIST',.T.)

    REPLACE CVENDCODE   WITH lcRpVenCod ;
      CinvNo      WITH lcDebMemN  ;
      capdtrtyp   WITH 'P'        ;
      DAPDTRDAT   WITH ldChkDat   ;
      LAPDPOST    WITH .F.        ;
      CAPDSTAT    WITH ''         ;
      CAPDREF     WITH lcNxtChkNo ;
      cstubchk    WITH lcNxtChkNo ;
      CAPDGLACT   WITH lcApAcct   ;
      NAPDAMNT    WITH lnPaymnt   ;
      CAPDACTID   WITH 'A'        ;
      CBATCHNO    WITH ''         ;
      CTRNSLEDN   WITH ''         ;
      CFISFYEAR   WITH lcFisYear  ;
      CFSPPRDID   WITH lcFisPrd   ;
      CAPSESSNO   WITH lcSession  ;
      CTAXCODE    WITH ''         ;
      cbnkcode    WITH lcRpBnkCod ;
      cchkacct    WITH lcRpChkAct ;
      NAPDLINNO   WITH 0          ;
      NEQVAMNT    WITH lnEqvAmnt  ;
      NEXRATE     WITH lnExRate ;
      cCurrCode   WITH lcCurrCode ;
      NCURRUNIT   WITH lnCurrUnit,;
      Rec_no     WITH  lcNewrec

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF

    =gfReplace('')
    =gfTableUpdate()
    *APPEND BLANK
    lcNewrec = lfGetNewRec('APDIST')
    m.Rec_no = lcNewrec
    gfAppend('APDIST',.T.)

    REPLACE CVENDCODE   WITH lcRpVenCod   ;
      CinvNo      WITH lcDebMemN    ;
      capdtrtyp   WITH 'P'          ;
      DAPDTRDAT   WITH ldChkDat     ;
      LAPDPOST    WITH .F.          ;
      CAPDSTAT    WITH ''           ;
      CAPDREF     WITH lcNxtChkNo   ;
      cstubchk    WITH lcNxtChkNo   ;
      CAPDGLACT   WITH lcRpGlAcct   ;
      NAPDAMNT    WITH 0 - lnPaymnt ;
      CAPDACTID   WITH 'C'          ;
      CBATCHNO    WITH ''           ;
      CTRNSLEDN   WITH ''           ;
      CFISFYEAR   WITH lcFisYear    ;
      CFSPPRDID   WITH lcFisPrd     ;
      CAPSESSNO   WITH lcSession    ;
      CTAXCODE    WITH ''           ;
      cbnkcode    WITH lcRpBnkCod   ;
      cchkacct    WITH lcRpChkAct   ;
      NAPDLINNO   WITH 1            ;
      NEQVAMNT    WITH 0 - lnEqvAmnt;
      NEXRATE     WITH lnExRate   ;
      cCurrCode   WITH lcCurrCode   ;
      NCURRUNIT   WITH lnCurrUnit,;
      Rec_no     WITH  lcNewrec

    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF

    =gfReplace('')
    =gfTableUpdate()
  ENDIF
ENDSCAN

*!**************************************************************************
*!
*!      Function: lfFooter
*!
*!***************************************************************************
*
FUNCTION lfFooter

lcSetCent =SET("Century")
SET CENTURY ON
IF &lcRpTargt..CVENDCODE = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..CAddress = lcAddress AND &lcRpTargt..cPageNo = lcPageNo
  lcExactSt = SET("Exact")
  SET EXACT ON
  lnPosArr = ASCAN(laFooter,&lcRpTargt..CinvNo)
  IF lnPosArr>0
    lnPosArr = ASUBSCRIPT(laFooter,lnPosArr,1)
    IF laFooter[lnPosArr ,1] = &lcRpTargt..CinvNo AND laFooter[lnPosArr ,2] =  DTOC(&lcRpTargt..DINVDATE)
      SET EXACT &lcExactSt
      SET CENTURY &lcSetCent.
      RETURN
    ENDIF
  ENDIF
  SET EXACT &lcExactSt
  lcAddress= &lcRpTargt..CAddress
  lcPageNo = &lcRpTargt..cPageNo
  lnFooter = lnFooter + 1
  laFooter[lnFooter,1] = &lcRpTargt..CinvNo
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..DINVDATE)
  =lfSetCurSm(&lcRpTargt..cCurrCode)
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(NINVAMNT,;
    '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(NINVDISAP,;
    '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(NINVADJAP,;
    '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
    ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
    , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
    '@ 9999,999,999.99')) + lcNetSmbl , 15) )

ELSE
  lnFooter = 1
  laFooter = ''
  lcVendor = &lcRpTargt..CVENDCODE
  lcAddress = &lcRpTargt..CAddress
  lcPageNo = &lcRpTargt..cPageNo
  laFooter[lnFooter,1] = &lcRpTargt..CinvNo
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..DINVDATE)
  =lfSetCurSm(&lcRpTargt..cCurrCode)
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(NINVAMNT,;
    '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(NINVDISAP,;
    '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(NINVADJAP,;
    '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
    ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
    , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
    '@ 9999,999,999.99')) + lcNetSmbl , 15) )
ENDIF
SET CENTURY &lcSetCent.
RETURN ''
*!**************************************************************************
*!
*!      FUNCTION : lfSelectRec
*!
*!**************************************************************************
* Function to Select all the Checks within the range of the reprinted check
FUNCTION lfSelectRec
SELECT APDIST
lcOrder = ORDER()
=gfSetOrder('CHECKS')

SELECT(lcCursorChk)
LOCATE
lcRpChkFrm = &lcCursorChk..KeyExp
GO BOTTOM
lcRpChkTo =  &lcCursorChk..KeyExp



SELECT APDIST
* Seek for the First record that fall in the range of the checks
=gfSEEK('P'+lcRpBnkCod+lcRpChkAct,'APDIST')
STORE 0 TO lnPgCntr,lnCount
STORE '' TO lcInvoice,lcStubChk
=gfSEEK(CinvNo+CVENDCODE,'APINVHDR')
lcGroup = CVENDCODE + UPPER(APINVHDR.COUTCOMP+APINVHDR.COUTADDR1+APINVHDR.COUTADDR2+APINVHDR.COUTADDR3+;
  APINVHDR.COUTADDR4+APINVHDR.COUTADDR5)
lcApdRef = CAPDREF

* Scan through the APDIST file for all the checks that fall within the range
* of the checks to be reprinted and is not voided
SCAN REST WHILE capdtrtyp+cbnkcode+cchkacct='P'+lcRpBnkCod+lcRpChkAct FOR SEEK(cstubchk,lcCursorChk);
    AND CAPDSTAT <> 'V'

  * if the current invoice <> the old one
  IF lcInvoice <> APDIST.CinvNo OR lcGroup <> CVENDCODE + UPPER(APINVHDR.COUTCOMP+APINVHDR.COUTADDR1+APINVHDR.COUTADDR2+APINVHDR.COUTADDR3+APINVHDR.COUTADDR4+APINVHDR.COUTADDR5)
    lcGroup = CVENDCODE + UPPER(APINVHDR.COUTCOMP+APINVHDR.COUTADDR1+APINVHDR.COUTADDR2+APINVHDR.COUTADDR3+APINVHDR.COUTADDR4+APINVHDR.COUTADDR5)
    * if the stub check # has been chaned
    IF lcStubChk <> cstubchk

      * if the master check # <> the last master check #
      * then replace the last record in the temprory file with 0
      * so the printing can feel that this is a master check
      * increament the counter with 1 and change the variable that hold
      * the last master check #
      IF lcApdRef = lcStubChk
        lnCount  = 1
        REPLACE &lcRpTargt..nNoOfInv WITH 0
      ELSE
        lnCount = lnCount + 1
      ENDIF
      lcApdRef = CAPDREF
      lnPgCntr = lnPgCntr + 1
      lcStubChk = cstubchk
    ENDIF
    * Select the invoice header file get the values of the current record
    =gfSEEK(CinvNo+CVENDCODE,'APINVHDR')
    SELECT APINVHDR
    SCATTER MEMVAR MEMO
    * Select the temprory file and append the new record and store the values
    SELECT (lcRpTargt)
    APPEND BLANK
    GATHER MEMVAR MEMO

    *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
    =gfSEEK(m.CVENDCODE ,'apvendor','VENCODE')
    REPLACE cvencomp WITH  APVENDOR.cvencomp
    *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]

    * replace the page # that the check will be printed in with the needed page #
    * and the # of invoices that will be printed in the same check with
    REPLACE CAddress WITH UPPER(APINVHDR.COUTCOMP+APINVHDR.COUTADDR1+APINVHDR.COUTADDR2+APINVHDR.COUTADDR3+;
      APINVHDR.COUTADDR4+APINVHDR.COUTADDR5),;
      nNoOfInv WITH CEILING(lnCount/lnRpStub) ;
      cPageNo  WITH PADL(lnPgCntr,4)
    lcInvoice = APDIST.CinvNo
  ENDIF
  * Select the temprory file and start updating the ammount values
  SELECT (lcRpTargt)

  DO CASE
  CASE APDIST.CAPDACTID = 'C'

    REPLACE nInvFAAp     WITH -APDIST.NAPDAMNT


  CASE APDIST.CAPDACTID = 'B'
    REPLACE NINVA1099    WITH -APDIST.NAPDAMNT

  CASE (APDIST.CAPDACTID = 'J' .AND. APDIST.NAPDLINNO <> 1)

    REPLACE NINVADJAP    WITH -APDIST.NAPDAMNT
  CASE APDIST.CAPDACTID = 'S'
    REPLACE NINVDISAP    WITH -APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST
ENDSCAN

*If the last check is a master check
IF !BOF()
  SKIP -1
ENDIF

* if the last selected record in from APDIST file its master check =
* its stub check that means the last check is a master check so
* we have to update the temprory file nnoofinv value with 0
* so the printing can feel that this is a master check
IF CAPDREF = cstubchk
  REPLACE &lcRpTargt..nNoOfInv  WITH 0
ENDIF

SELECT APDIST
SET RELATION TO
IF !EMPTY(lcOrder)
  gfSetOrder(lcOrder)
ENDIF
GO TOP


*!************************************************************************
*!
*!      FUNCTION lfRePrnUpd
*!
*!************************************************************************
* function to update the nessecery files after the reprint checks operation
* complete
FUNCTION lfRePrnUpd
PRIVATE lcOrder
* Select the checks file and update the bank account next check number field
* with the new value
SELECT APCHECKS
=gfSEEK(lcRpBnkCod+lcRpChkAct)
REPLACE NCHKNXTPN WITH INT(VAL(lcNxtChkNo))
=gfReplace('')
=gfTableUpdate()
* Select the Temprory file to get the number of checks that printed
* to update the fields of the APDIST file and other files
SELECT (lcRpTargt)
GO BOTT   && To refresh the relation between files.
lnNoOfChk = INT(VAL(cPageNo))

* Select the APDIST file to start updating
SELECT APDIST
lcOrder = ORDER()
=gfSetOrder('CHECKS')
=gfSEEK('P'+lcRpBnkCod+lcRpChkAct)

lcCurChk = ''
lnChkNo  = 0
lcApDChkRec=''
* Scan through the APDIST file to update all the Invoices that was printed
* for a desired check number for specific banck account and not voided
SCAN REST FOR capdtrtyp+cbnkcode+cchkacct='P'+lcRpBnkCod+lcRpChkAct AND  ;
    SEEK(cstubchk,lcCursorChk) AND CAPDSTAT <> 'V'
  * Seek for the value of the next record if its in the range of checks
  * to be reprinted NOTE that this condition does not do any thing at
  * the first time entring the scan because the variable lcApDChkRec will
  * be empty
  IF !EMPTY(lcApDChkRec)
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcApDChkRec)
  ENDIF
  * Skip for the next record and if its in the range of the Checks to be
  * reprinted get the value of the record to be used in next loop
  * we did that because the scan function does not work properly with
  * changing the value of the index that is use by the scan loop itself
  SKIP 1
  IF capdtrtyp+cbnkcode+cchkacct='P'+lcRpBnkCod+lcRpChkAct AND ;
      SEEK(cstubchk,lcCursorChk) AND !EOF()
    lcApDChkRec = cstubchk
  ELSE
    lcApDChkRec = ''
  ENDIF
  * go to the previous record to update it
  SKIP -1
  * if the current record check # <> the Previous one
  IF lcCurChk <> cstubchk
    lcCurChk  = cstubchk
    lnChkNo   = lnChkNo + 1
    * if the stub check = the master check
    IF ALLTR(cstubchk) = ALLTR(CAPDREF)
      * seek in the APPAYMNT file to update the record of the payment
      * with the new check #
      SELECT APPAYMNT
      IF gfSEEK('PP'+APDIST.cstubchk+lcRpBnkCod+lcRpChkAct)
        SCATTER MEMVAR MEMO
        REPLACE CPAYSTAT  WITH 'V'

        IF  TYPE('lcXMLFileName') <> 'C'
          =gfAdd_Info('APPAYMNT')
        ELSE
          loAddUserInfo.DO('APPAYMNT',.NULL.)
        ENDIF


        =gfReplace('')
        *APPEND BLANK
        lcNewrec = lfGetNewRec('APPAYMNT')
        m.Rec_no = lcNewrec
        gfAppend('APPAYMNT',.T.)
        GATHER MEMVAR MEMO

        REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0'),;
          DPAYDATE WITH ldChkDat,;
          Rec_no   WITH lcNewrec

        IF  TYPE('lcXMLFileName') <> 'C'
          =gfAdd_Info('APPAYMNT')
        ELSE
          loAddUserInfo.DO('APPAYMNT',.NULL.)
        ENDIF
        =gfReplace('')
        * Update the APCHECKS check date and ammount
        SELECT APCHECKS
        REPLACE APCHECKS.DCHKLPDAT WITH APPAYMNT.DPAYDATE ;
          APCHECKS.NCHKLPAMT WITH APPAYMNT.NPAYAMNT
        =gfReplace('')
        * Update the Vendor file with the new values
        SELECT APVENDOR
        IF gfSEEK(APDIST.CVENDCODE,'APVENDOR')
          REPLACE APVENDOR.DVENLPAYD WITH APPAYMNT.DPAYDATE ;
            APVENDOR.NVENLPAYA WITH APPAYMNT.NPAYAMNT ;
            APVENDOR.CVENLPAYN WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
        ENDIF
        =gfReplace('')
        * Select APDIST file to Replace all the Stub checks that its master check is
        * the one that we are updating its #
        SELECT APDIST
        lnCurRec= RECNO()
        gfSetOrder('PAYMNTS')
        =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcCurChk)
        SCAN FOR  capdtrtyp+cbnkcode+cchkacct+CAPDREF+CinvNo+CAPDACTID='P'+lcRpBnkCod+lcRpChkAct+lcCurChk
          REPLACE CAPDREF WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
          =gfReplace('')
        ENDSCAN

        GO lnCurRec
        gfSetOrder('CHECKS')
      ENDIF
    ENDIF
    SELECT APDIST
  ENDIF
  REPLACE cstubchk WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
  =gfReplace('')
  IF gfSEEK(CinvNo+CVENDCODE,'APINVHDR')
    REPLACE APINVHDR.CCHKNO   WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0');
      APINVHDR.DCHKDATE WITH ldChkDat
  ENDIF
  SELECT APINVHDR
  =gfReplace('')
  * if there is any other records within the range of the checks to be reprinted
  * then to to the next one to be updated
  * Note that we seek for the next value to be updated and skip -1
  * because the Scan will Skip the record pointer after our skip -1
  SELECT APDIST
  IF !EMPTY(lcApDChkRec)
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcApDChkRec)
    SKIP -1
  ENDIF
ENDSCAN
* Restore the old Order for the APDIST file
IF !EMPTY(lcOrder)
  gfSetOrder(lcOrder)
ENDIF
SELECT APPAYMNT
=gfTableUpdate()
SELECT APINVHDR
=gfTableUpdate()
SELECT APDIST
=gfTableUpdate()
SELECT APVENDOR
=gfTableUpdate()
SELECT APCHECKS
=gfTableUpdate()
*!************************************************************************
*!
*!      FUNCTION lfInsVdChr
*!
*!************************************************************************
*! function to initialize the VOID sign in the dotmatrix report
FUNCTION lfInsVdChr
PARAMETERS lcString,lnStrLen,lcStrAtr,lcInterSect
lcStrAtr = IIF(TYPE('lcStrAtr')='C',lcStrAtr,'')
lcString = ALLTRIM(lcString)
IF EMPTY(LCVOIDCHR)
  RETURN IIF(lcStrAtr='C',PADC(lcString,lnStrLen),IIF(lcStrAtr='R',;
    PADL(lcString,lnStrLen),PADR(lcString,lnStrLen)))
ENDIF
lcString = IIF(lcStrAtr='C',PADC(lcString,lnStrLen),IIF(lcStrAtr='R',;
  PADL(lcString,lnStrLen),PADR(lcString,lnStrLen)))
lcInterRow=SUBSTR(lcInterSect,1,ATC('|',lcInterSect)-1)
lcInterLen=SUBSTR(lcInterSect,ATC('|',lcInterSect)+1)
lcInterRep = SUBSTR(lcInterLen,1,ATC(',',lcInterLen)-1)
lcInterRep = IIF(EMPTY(lcInterRep),lcInterLen,lcInterRep)
DO WHILE OCCURS(',',lcInterLen) < OCCURS(',',lcInterRow)
  lcInterLen = lcInterLen+','+lcInterRep
ENDDO
lcInterSect = lcInterRow+'|'+lcInterLen
DIMENSION laInterSec[1,2]
STORE '' TO laInterSec
=gfSubStr(lcInterSect,@laInterSec,',|')
FOR lnInterCount = 1 TO ALEN(laInterSec,1)
  lnStarPos1 = VAL(laInterSec[lnInterCount,1])
  lnVChrRpl = IIF(EMPTY(EVAL(laInterSec[lnInterCount,2])),EVAL(laInterSec[1,2]),EVAL(laInterSec[lnInterCount,2]))
  IF TYPE('lnStarPos1')='N' AND TYPE('lnVChrRpl')='N'
    FOR i = 0 TO lnVChrRpl - 1
      IF lnStarPos1+i <= LEN(lcString) AND EMPTY(SUBSTR(lcString,lnStarPos1+i,1))
        lcString = STUFF(lcString,lnStarPos1+i,1,LCVOIDCHR)
      ENDIF
    ENDFOR
  ENDIF
ENDFOR
RETURN lcString
*B601091,1 Hesham El-Sheltawi (End)

*B601091,1 Hesham El-Sheltawi (Start)
FUNCTION lfNxtChkUpd
IF !EOF(lcRpTargt) AND &lcRpTargt..cPageNo <> lcChkPage
  lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
  lcChkPage = &lcRpTargt..cPageNo
ENDIF
*B601091,1 Hesham El-Sheltawi (End)

*!*************************************************************
*! Name      : lfGetNetSm
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to get the Currency symbol of the Bank Checking
*!             account currency
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : APCHKPRV.RPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Currency symbol of the Bank Checking
*!                       account currency
*!*************************************************************
*! Example            :  =lfGetNetSm()
*!*************************************************************
*B601526,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfGetNetSm

PRIVATE lcOldAlias , lcExactSt , lcCurrency , lnCurSEl , llUsed

lcOldAlias = ALIAS()
lcExactSt = SET('EXACT')
SET EXACT ON
SELECT APCHECKS
=gfSetOrder('BANKCHECK')
=gfSEEK(lcRpBnkCod + lcRpChkAct)
lcCurrency = APCHECKS.cCurrCode
lnCurSEl = ASCAN(laCurrSmbl , lcCurrency)

llUsed = .F.
IF !USED('SYCCURR')
  =gfOpenTable('SYCCURR','CCURRCODE')
  llUsed = .T.
ENDIF
SELECT SYCCURR
=gfSEEK(SYCCURR.cCurrCode)
lcRpCurDes = ALLTRIM(SYCCURR.cCurrDesc)
lcRpCurDes = IIF(EMPTY(lcRpCurDes) , 'Dollars' , lcRpCurDes)
IF llUsed
  =gfCloseTable('SYCCURR')
ENDIF
SELECT (lcOldAlias)
SET EXACT &lcExactSt

RETURN IIF(lnCurSEl <> 0 , laCurrSmbl(lnCurSEl + 1) , '')

*!*************************************************************
*! Name      : lfSetCurSm
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to reset the Currency symbol
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfFooter() , APCHKPRDD.FRX , APCHKPRDL.FRX , APCHKPRWL.FRX
*!*************************************************************
*! Passed Parameters  : Currency code
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSetCurSm()
*!*************************************************************
*
FUNCTION lfSetCurSm

PARAMETER lcCrCod

PRIVATE lcExactSt , lnCurSEl
lcExactSt = SET('EXACT')
SET EXACT ON
lnCurSEl = ASCAN(laCurrSmbl , lcCrCod)
SET CURRENCY TO IIF(lnCurSEl <> 0 , ALLTRIM(laCurrSmbl(lnCurSEl + 1)) , '')
SET EXACT &lcExactSt
RETURN ''





*!*************************************************************
*! Name      : lfAdvPyCur
*! Developer : Amin Khodary
*! Date      : 09/08/1999
*! Purpose   : Function to get the currency code,unit, rate if adv. pay.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : APCHKPRV.PRG in case of avanced payment.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  lfAdvPyCur
*!*************************************************************

FUNCTION lfAdvPyCur
IF gfGetMemVar('LLMulCurr')
  * Get currency code of selected Bank No +Check Account
  lcCurrCode =IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),APCHECKS.cCurrCode,'')
  * If it's base currency no need to recalculate the equv. amount.
  IF lcCurrCode <>  oAriaApplication.BaseCurrency
    lnExRate = gfChkRate('lnCurrUnit',lcCurrCode,ldChkDat,.T.,oAriaApplication.ActiveCompanyID, oAriaApplication.BaseCurrency, .T.)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnEqvAmnt  = IIF(lnExRate > 0 AND lnCurrUnit > 0, ROUND(lnEqvAmnt  &lcExSin1  lnExRate &lcExSin2 lnCurrUnit,2),0)
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mohamed Shokry
*! Date      :
*! Purpose   : To object lock any record in any file
*!*************************************************************
*! Calls     :
*!      Called by: GFCPDELETE()             (function  in ARIA3.PRG)
*!      Called by: GFCPEDIT()               (function  in ARIA3.PRG)
*!      Called by: GFCPSAVE()               (function  in ARIA3.PRG)
*!      Called by: GFCPCLOSE()              (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : flage to lock or unlock
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   :
*!*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag


PRIVATE lnOldrpSt

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.

  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK()
    IF DELETED()
      UNLOCK
      =gfModalGen('INM00095B00000','ALERT')
      laScrMode     = .F.
      laScrMode [1] = .T.
      RETURN .F.
    ENDIF

  ENDIF

  *** Chek if the record is in use by another user
  IF lLok_Set
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(CLOK_USER)
        *IF ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)
        *  *!600396,1 Messaging the user that he cannot edit the same record
        *  *!600396,1 from more than one session and permit him from editing
        *  *!600396,1 the same record
        lcLok_User = CLOK_USER
        IF !USED('syuStatc')
          *!*	          IF oAriaApplication.multiinst
          *!*	            lcSQLDICPATH = STRTRAN(oAriaApplication.clientprogramhome,"Prgs\","SQLDictionary\")
          *!*	          ELSE
          *!*	            lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
          *!*	          ENDIF
          =gfOpenFile('syuStatc','Cuser_id','SH','syuStatc')
        ENDIF
        SELECT APINVHDR
        lnOldrpSt = SET('REPROCESS')
        SET REPROCESS TO 1
        IF SEEK ('INI'+'OLDVARS'+CLOK_USER,'syuStatc')
          UNLOCK

          *** Display the message "Record is in use by user AAAA"
          lcLok_User = oAriaApplication.getUserName(lcLok_User)
          *** Record is in use by user ????
          lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CinvNo)+" for vendor "+ ALLTRIM(CVENDCODE)+" is being edited by user " + lcLok_User+"."
          IF TYPE('lcXMLFileName') <> 'C' AND gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
            lcCinvno = APINVHDR.CinvNo
            lcCvendcode = APINVHDR.CVENDCODE
            =gfSEEK(lcCinvno +lcCvendcode ,'APINVHDR')
            LOOP
          ENDIF
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE
          lLok_It    = .T.
        ENDIF

        SET REPROCESS TO  lnOldrpSt
      ELSE
        *** Display the message "Record is in use by another"
        IF TYPE('lcXMLFileName') <> 'C' AND gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;
        CLOK_USER WITH ""  , ;
        dLok_Date WITH {}  , ;
        cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
IF lLok_It
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;
      CLOK_USER WITH gcUser_ID , ;
      dLok_Date WITH DATE()    , ;
      cLok_Time WITH gfGetTime()

    lRet_Flag  = .T.
  ENDIF
ENDIF


UNLOCK


RETURN lRet_Flag

*!************************************************************************
*!
*!      Function lfSayNumber
*!
*!************************************************************************
*
FUNCTION lfSayNumber
PARAMETERS lnSendNum,lcOtherLine
DIMENSION laWords(15)
laWords = SPACE(15)
DIMENSION laDigit(10)
laDigit[1]  = ' '
laDigit[2]  = 'One '
laDigit[3]  = 'Two '
laDigit[4]  = 'Three '
laDigit[5]  = 'Four '
laDigit[6]  = 'Five '
laDigit[7]  = 'Six '
laDigit[8]  = 'Seven '
laDigit[9]  = 'Eight '
laDigit[10] = 'Nine '

IF lnSendNum = 0
  RETURN '* Zero ' + lcRpCurDes + ' *'
ENDIF

lcSendNum = STR(lnSendNum,ALEN(laWords,1),2)

IF lnSendNum < 1

  RETURN  '* Zero ' + ' and '+ SUBSTR(lcSendNum,ALEN(laWords,1)-1,2) + '/100 ' + lcRpCurDes + ' *'

ENDIF

lcReturnWord = ''

FOR lnCount = 1 TO ALEN(laWords,1)
  DO CASE

  CASE SUBSTR(lcSendNum,lnCount,1) = '.'
    laWords[lnCount] = '~~~ '

  CASE SUBSTR(lcSendNum,lnCount,1) = ' '
    laWords[lnCount] = ' '

  OTHERWISE
    lnNumber = VAL(SUBSTR(lcSendNum,lnCount,1))
    laWords[lnCount] = laDigit(lnNumber + 1)

  ENDCASE
ENDFOR
lnCount = 1

DO WHILE .T.
  IF lnCount >= ALEN(laWords,1) - 2
    EXIT
  ENDIF

  IF laWords[lnCount] > ' '
    lcReturnWord = lcReturnWord + laDigit(VAL(SUBSTR(lcSendNum,lnCount,1))+1) + 'Hundred ~~~ '
  ENDIF
  lnCount = lnCount + 1

  IF laWords[lnCount] > ' '
    IF laWords[lnCount] = laDigit[2]
      lnCount = lnCount + 1
      =lfTeens()
      =lfThou()
      lnCount = lnCount + 1
      LOOP
    ELSE
      =lfTens()
    ENDIF
  ENDIF

  lnCount = lnCount + 1
  IF laWords[lnCount] > ' '
    lcReturnWord = lcReturnWord + laDigit(VAL(SUBSTR(lcSendNum,lnCount,1))+1)
  ENDIF

  =lfThou()
  lnCount = lnCount + 1
ENDDO



IF VAL(SUBSTR(lcSendNum,14,2)) > 0

  lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '')

  lcReturnWord = lcReturnWord + '&' + SUBSTR(lcSendNum,14,2) + '/100 '+ '~~~ '

ELSE

  DO WHILE ATC('!!! ',lcReturnWord,2) <> 0
    lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '',1,1)
  ENDDO
  lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '&')

ENDIF


lcReturnWord = STRTRAN(lcReturnWord, '~~~ ' ,'')

lcReturnWord = lcReturnWord + "" + lcRpCurDes


lcReturnWord='* '+UPPER(SUBSTR(lcReturnWord,1,1))+LOWER(SUBSTR(lcReturnWord,2))+' *'

IF LEN(lcReturnWord) > 72
  lcTempStr    = SUBSTR(lcReturnWord,1,71)
  lnEndOfLine  = RAT(' ',lcTempStr)
  lcTempStr    = SUBSTR(lcTempStr,1,lnEndOfLine) + '*'
  &lcOtherLine  = '* ' + SUBSTR(lcReturnWord,lnEndOfLine+1)
  lcReturnWord = lcTempStr

ELSE
  &lcOtherLine  = ''
ENDIF

RETURN lcReturnWord
*!***************************************************************
*!
*!   FUNCTION lfThou
*!
*!***************************************************************
*
FUNCTION lfThou

DO CASE

CASE lnCount = 3
  IF laWords[1] > ' ' OR laWords[2] > ' ' OR laWords[3] > ' '
    lcReturnWord = lcReturnWord + 'Billion !!! '

  ENDIF

CASE lnCount = 6
  IF laWords[4] > ' ' OR laWords[5] > ' ' OR laWords[6] > ' '
    lcReturnWord = lcReturnWord + 'Million !!! '

  ENDIF

CASE lnCount = 9
  IF laWords[7] > ' ' .OR. laWords[8] > ' ' .OR. laWords[9] > ' '
    lcReturnWord = lcReturnWord + 'Thousand !!! '

  ENDIF

ENDCASE


*!***************************************************************
*!
*!   FUNCTION lfTeens
*!
*!***************************************************************
*
FUNCTION lfTeens

DO CASE
CASE laWords[lnCount] == laDigit[1]
  lcReturnWord = lcReturnWord + 'Ten '

CASE laWords[lnCount] = laDigit[2]
  lcReturnWord = lcReturnWord + 'Eleven '

CASE laWords[lnCount] = laDigit[3]
  lcReturnWord = lcReturnWord + 'Twelve '

CASE laWords[lnCount] = laDigit[4]
  lcReturnWord = lcReturnWord + 'Thirteen '

CASE laWords[lnCount] = laDigit[5]
  lcReturnWord = lcReturnWord + 'Fourteen '

CASE laWords[lnCount] = laDigit[6]
  lcReturnWord = lcReturnWord + 'Fifteen '

CASE laWords[lnCount] = laDigit[7]
  lcReturnWord = lcReturnWord + 'Sixteen '

CASE laWords[lnCount] = laDigit[8]
  lcReturnWord = lcReturnWord + 'Seventeen '

CASE laWords[lnCount] = laDigit[9]
  lcReturnWord = lcReturnWord + 'Eighteen '

CASE laWords[lnCount] = laDigit[10]
  lcReturnWord = lcReturnWord + 'Nineteen '

ENDCASE


*!***************************************************************
*!
*!   FUNCTION lfTens
*!
*!***************************************************************
*
FUNCTION lfTens

DO CASE

CASE laWords[lnCount] == laDigit[1]
  RETURN

CASE laWords[lnCount] = laDigit[2]
  lcReturnWord = lcReturnWord + 'Ten '

CASE laWords[lnCount] = laDigit[3]
  lcReturnWord = lcReturnWord + 'Twenty '

CASE laWords[lnCount] = laDigit[4]
  lcReturnWord = lcReturnWord + 'Thirty '

CASE laWords[lnCount] = laDigit[5]
  lcReturnWord = lcReturnWord + 'Forty '

CASE laWords[lnCount] = laDigit[6]
  lcReturnWord = lcReturnWord + 'Fifty '

CASE laWords[lnCount] = laDigit[7]
  lcReturnWord = lcReturnWord + 'Sixty '

CASE laWords[lnCount] = laDigit[8]
  lcReturnWord = lcReturnWord + 'Seventy '

CASE laWords[lnCount] = laDigit[9]
  lcReturnWord = lcReturnWord + 'Eighty '

CASE laWords[lnCount] = laDigit[10]
  lcReturnWord = lcReturnWord + 'Ninety '

ENDCASE


*!***************************************************************
*!
*!   FUNCTION lfGetNewRec
*!
*!***************************************************************
FUNCTION  lfGetNewRec
LPARAMETERS lcTableName
lcOldAlis = SELECT()
llNID = gfSqlRun('Select NEWID() as NEWIDVAL',lcTableName,.T.,'NEWID_CUR')
lcNewrec = ''
IF llNID
  lcNewrec  = NEWID_CUR.NEWIDVAL
ELSE
  lcNewrec = ''
ENDIF
SELECT(lcOldAlis)
RETURN lcNewrec

