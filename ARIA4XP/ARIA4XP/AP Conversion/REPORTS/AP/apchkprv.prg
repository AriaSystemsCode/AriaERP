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
*!N000635,2 MMT 01/04/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [T20091122.0011]
*!N000635,5 TMI 07/28/2011 Fix some problmes while enabling the report to run on A4xp 
*!N000635,6 TMI 08/03/2011 Allow the VENDOR filter to work
*!N000635,7 TMI 08/14/2011 Allow other filters to work
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName


IF TYPE('lcXMLFileName') = 'C'
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
  lcRpBnkCod = PADR(lcRpBnkCod,8)
  lcRpChkAct = PADR(lcRpChkAct,12)
  oAriaEnvironment.Report.gcAct_Appl = "AP"
  oAriaEnvironment.report.cCROrientation = "P"
  oariaenvironment.activeModuleID = 'AP'
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
    gfSeek('')
  ENDIF 
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
ELSE
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[Start]
  IF !llTestChk
	IF !lfReValidate()    && Check if the data validated or not
	  RETURN .F.
	ENDIF
  ENDIF 
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]

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
  =gfSeek('')
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
  lcVoid = loogScroll.gfTempName()
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
      
      
      DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
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
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*lcRpFields = lcRpFields + ",cFacCode,cOutComp,cInvRemit"
lcRpFields = lcRpFields + ",APINVHDR.cFacCode,APINVHDR.cOutComp,APINVHDR.cInvRemit"
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [ENd] 
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
        DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
      ELSE
        loProgress.Percent = 0.9
        loProgress.Description = "Printing Report..."
        loAgent.UpdateObjectProgress(lcRequestID, loProgress)
        oAriaEnvironment.report.OGLastForm = lcRepForm
        PRIVATE loProxy
        loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

        IF loProxy.GetRequest(lcRequestID).Status = 3
          oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

          loProgress.Percent = 1.0
          loProgress.Description = "Printing Report..."
          loAgent.UpdateObjectProgress(lcRequestID, loProgress)
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
        lnPosVend= ASUBSCRIPT(laOgFxFlt,lnPosVend,1)
        lcCursorVend= laOgFxFlt[lnPosVend,6]
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
  	   lnPosDivision = ASUBSCRIPT(laOgFxFlt,lnPosDivision,1)
   	   lcDivisions = laOgFxFlt[lnPosDivision,6]
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
      lnPosDue = ASUBSCRIPT(laOgFxFlt,lnPosDue ,1)     
      IF !EMPTY(laOgFxFlt[lnPosDue ,6])
        llDueSelected  = .T.
        *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [Start]
        *ldDueStrtDate = IIF(EMPTY(SUBSTR(laOgFxFlt[lnPosDue ,6],1,10)),CTOD(""),CTOD(SUBSTR(laOgFxFlt[lnPosDue ,6],1,10)))
        *ldDueFnshDate =  IIF(EMPTY(SUBSTR(laOgFxFlt[lnPosDue ,6],12,21)),CTOD(""),CTOD(SUBSTR(laOgFxFlt[lnPosDue ,6],12,21)))
		lnSepPos = ATC('|',laOgFxFlt[lnPosDue ,6])
        ldDueStrtDate = CTOD(SUBSTR(laOgFxFlt[lnPosDue ,6],1,lnSepPos -1))
        ldDueFnshDate =  CTOD(SUBSTR(laOgFxFlt[lnPosDue ,6],lnSepPos +1))
		IF lnSepPos =1
          ldDueStrtDate  = CTOD('01/01/1900')
          ldDueFnshDate  = CTOD(SUBSTR(laOgFxFlt[lnPosDue ,6],2,11))
        ENDIF         
        *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [End]
      ENDIF 
    ENDIF 

    *Priority Selected 
    llPrioSelected  = .F.
    lcPStrt= ''
    lcPFnsh= ''
    lnPosPri = ASCAN(laOgFXFlt,"APINVHDR.CVENPRIOR")
    IF lnPosPri > 0 
      lnPosPri = ASUBSCRIPT(laOgFxFlt,lnPosPri,1)     
      IF !EMPTY(laOgFxFlt[lnPosPri,6])
        llPrioSelected  = .T.
        lcPStrt= LEFT(laOgFxFlt[lnPosPri,6],1)
        lcPFnsh= RIGHT(laOgFxFlt[lnPosPri,6],1)
      ENDIF 
    ENDIF 
    lcRpVenPay = IIF(LEFT(lcRpVenPay,1) = ",",SUBSTR(lcRpVenPay,2),lcRpVenPay)
    lcRpVenPay = IIF(RIGHT(lcRpVenPay,1) = ",",SUBSTR(lcRpVenPay,1,LEN(lcRpVenPay)-1),lcRpVenPay)
    
    *N000635,7 TMI 08/14/2011 [Start] check if APINVHDR is FOX
    llAPINVHDRisFOX = lfIsNative('APINVHDR')
    *N000635,7 TMI 08/14/2011 [End  ] 
    
    DO CASE 
      CASE llSelectVendor
       	SELECT(lcCursorVend)
      
        lnPerCent = RECNO()/RECCOUNT(lcCursorVend) 
        LOCATE
        SCAN 
          IF TYPE('lcXMLFileName') = 'C'
            IF MOD(RECNO(lcCursorVend),CEILING(RECCOUNT(lcCursorVend) / 10)) = 0
              loProgress.Percent = lnPerCent * 0.9
              loProgress.Description = "Collecting Data For Vendor:"+ALLTRIM(&lcCursorVend..KeyExp)
              loAgent.UpdateObjectProgress(lcRequestID, loProgress)
            ENDIF  
          ENDIF  
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	          =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where  APINVHDR.CVENDCODE = '"+;
*!*	                     ALLTRIM(&lcCursorVend..KeyExp)+"'"+;
*!*	                     IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
*!*	                     " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
*!*	                     IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
*!*	                     IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
*!*	                     " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
          *N000635,7 TMI 08/14/2011 [Start] If APINVHDR is not sql do not call sqlrun
          IF !llAPINVHDRisFOX
          *N000635,7 TMI 08/14/2011 [End  ] 
            =gfSqlRun("Select "+lcRpFields+",apvendor.cvencomp From APINVHDR Inner join apvendor on apvendor.Cvendcode  = APINVHDR.Cvendcode  Where "+;
          			 " APINVHDR.CINVSTAT <> 'V' ANd  APINVHDR.CVENPRIOR<>'0' AND  APINVHDR.CVENDCODE = '"+;
                     ALLTRIM(&lcCursorVend..KeyExp)+"'"+;
                     IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
                     " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
                     IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
                     IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
                     " AND APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
            *N000635,7 TMI 08/14/2011 [Start] close the IF
          ENDIF
          *N000635,7 TMI 08/14/2011 [End  ] 
          *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]     
          SELECT APINVHDR

          *N000635,6 TMI 08/03/2011 [Start] case of FOX table 
          IF llAPINVHDRisFOX
            lcSvOrd = ORDER('APINVHDR')
            lnSvRcno = RECNO('APINVHDR')
            SET ORDER TO VENDINV IN APINVHDR  && CVENDCODE+CINVNO
            SET KEY TO &lcCursorVend..KeyExp
            LOCATE
          ENDIF
          *N000635,6 TMI 08/03/2011 [End  ] 

          *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
          *SCAN FOR IIF(llDiviSelected,SEEK(APINVHDR.CDIVISION,lcDivCursor),.T.)
          *N000635,7 TMI 08/14/2011 [Start] add the checks in the SELECT statement
          *SCAN 
          SCAN FOR APINVHDR.CINVSTAT <> 'V' AND ;
               APINVHDR.CVENPRIOR<>'0' AND ;
               IIF(!EMPTY(lcRpVenPay),APINVHDR.CVENPMETH $ lcRpVenPay,.T.) AND ;
               APINVHDR.DINVDATE <= ldChkDat AND ;
               APINVHDR.DPOSTDATE <= ldChkDat AND ;
               IIF(llPrioSelected,BETWEEN(APINVHDR.CVENPRIOR,lcPStrt,lcPFnsh),.T.) AND ;
               IIF(llDueSelected ,BETWEEN(APINVHDR.DINVDUDAT,ldDueStrtDate,ldDueFnshDate),.T.) AND ;
               APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = lcRpBnkCod+lcRpChkAct
            *N000635,7 TMI 08/14/2011 [End  ] 
            IF llDiviSelected AND !SEEK(APINVHDR.CDIVISION,lcDivCursor)
              LOOP
            ENDIF
            *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
            
            SCATTER MEMO MEMVAR 
            *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
            *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
           * =gfSeek(m.CVENDCODE,'apvendor','VENCODE')
           * m.cvencomp = apvendor.cvencomp
			*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
			*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
            INSERT INTO (lcRpTargt) FROM MEMVAR 
          ENDSCAN 
          *N000635,6 TMI 08/03/2011 [Start] case of FOX table 
          IF llAPINVHDRisFOX
            SET ORDER TO &lcSvOrd IN APINVHDR  && CVENDCODE+CINVNO
            SET KEY TO 
            LOCATE
            IF BETWEEN(lnSvRcno,1,RECCOUNT('APINVHDR'))
              GOTO lnSvRcno
            ENDIF            
          ENDIF
          *N000635,6 TMI 08/03/2011 [End  ] 
        ENDSCAN 
      CASE llDiviSelected  
        SELECT (lcDivCursor)
        lnPerCent = RECNO()/RECCOUNT(lcDivCursor) 
        LOCATE 
        
        SCAN 
          IF TYPE('lcXMLFileName') = 'C'
            IF MOD(RECNO(lcDivCursor),CEILING(RECCOUNT(lcDivCursor) / 10)) = 0
              loProgress.Percent = lnPerCent * 0.9
              loProgress.Description = "Collecting Data For Division:"+ALLTRIM(&lcDivCursor..CDIVISION)
              loAgent.UpdateObjectProgress(lcRequestID, loProgress)
            ENDIF  
          ENDIF 
                    
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	          =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where  APINVHDR.CDIVISION = '"+;
*!*	                     ALLTRIM(&lcDivCursor..CDIVISION)+"'"+;
*!*	                     IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
*!*	                     " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
*!*	                     IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
*!*	                     IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
*!*	                     " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
          *N000635,7 TMI 08/14/2011 [Start] If APINVHDR is not sql do not call sqlrun
          IF !llAPINVHDRisFOX
            *N000635,7 TMI 08/14/2011 [End  ] 
            =gfSqlRun("Select "+lcRpFields+",apvendor.cvencomp From APINVHDR Inner join apvendor on apvendor.Cvendcode  = APINVHDR.Cvendcode Where "+;
         			 " APINVHDR.CINVSTAT <> 'V' ANd  APINVHDR.CVENPRIOR<>'0' AND   APINVHDR.CDIVISION = '"+;
                     ALLTRIM(&lcDivCursor..CDIVISION)+"'"+;
                     IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
                     " AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
                     IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
                     IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
                     " AND APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
             *N000635,7 TMI 08/14/2011 [Start]  close the IF
           ENDIF
           *N000635,7 TMI 08/14/2011 [End  ] 
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
          
          SELECT APINVHDR
          *N000635,7 TMI 08/14/2011 [Start] add the same criteria posted in sqlrun in case of the FOX tables
          *SCAN 
          LOCATE
          SCAN FOR APINVHDR.CINVSTAT <> 'V' AND ;
                APINVHDR.CVENPRIOR<>'0' AND ;
                APINVHDR.CDIVISION = &lcDivCursor..CDIVISION AND ;
                IIF(!EMPTY(lcRpVenPay),APINVHDR.CVENPMETH $ lcRpVenPay,.T.) AND ;
                APINVHDR.DINVDATE <= ldChkDat AND ;
                APINVHDR.DPOSTDATE <= ldChkDat AND ;
                IIF(llPrioSelected,BETWEEN(APINVHDR.CVENPRIOR,lcPStrt,lcPFnsh),.T.) AND ;
                IIF(llDueSelected ,BETWEEN(APINVHDR.DINVDUDAT,ldDueStrtDate,ldDueFnshDate),.T.) AND ;
                APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = lcRpBnkCod+lcRpChkAct
            *N000635,7 TMI 08/14/2011 [End  ] 
            SCATTER MEMO MEMVAR 
            *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
            *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
            *=gfSeek(m.CVENDCODE,'apvendor','VENCODE')
            *m.cvencomp = apvendor.cvencomp
            *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
			*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [ENd]
            INSERT INTO (lcRpTargt) FROM MEMVAR 
          ENDSCAN 
        ENDSCAN
        
      OTHERWISE 
         *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	         =gfSqlRun("Select "+lcRpFields+" From APINVHDR Where "+;
*!*	                    " APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
*!*	                    IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
*!*	                    IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
*!*	                    IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
*!*	                    " AND CBNKCODE+CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
          *N000635,7 TMI 08/14/2011 [Start] If APINVHDR is not sql do not call sqlrun
          IF !llAPINVHDRisFOX
          *N000635,7 TMI 08/14/2011 [End  ] 
            =gfSqlRun("Select "+lcRpFields+",apvendor.cvencomp From APINVHDR Inner join apvendor on apvendor.Cvendcode  = APINVHDR.Cvendcode Where "+;
                    "APINVHDR.CINVSTAT <> 'V' ANd  APINVHDR.CVENPRIOR<>'0' AND APINVHDR.DINVDATE <= '"+DTOS(ldChkDat)+"' AND APINVHDR.DPOSTDATE <= '"+DTOS(ldChkDat)+"'"+;
                    IIF(llPrioSelected," AND APINVHDR.CVENPRIOR BETWEEN '"+lcPStrt+"' AND '"+lcPFnsh+"'",'')+;
                    IIF(!EMPTY(lcRpVenPay)," AND APINVHDR.CVENPMETH IN ("+lcRpVenPay+")",'')+;
                    IIF(llDueSelected ," AND APINVHDR.DINVDUDAT BETWEEN '"+DTOS(ldDueStrtDate)+"' AND '"+DTOS(ldDueFnshDate)+"'",'')+;
                    " AND APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = '"+lcRpBnkCod+lcRpChkAct+"'",'APINVHDR')
           *N000635,7 TMI 08/14/2011 [Start]  close the IF
         ENDIF
         *N000635,7 TMI 08/14/2011 [End  ] 
         *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]                     
         SELECT APINVHDR
         lnPerCent = RECNO()/RECCOUNT('APINVHDR') 
         
         
         *N000635,7 TMI 08/14/2011 [Start] add the same criteria posted in sqlrun in case of the FOX tables
         *SCAN 
         SCAN FOR APINVHDR.CINVSTAT <> 'V' AND ;
               APINVHDR.CVENPRIOR<>'0' AND ;
               APINVHDR.DINVDATE <= ldChkDat AND ;
               APINVHDR.DPOSTDATE <= ldChkDat AND ;
               IIF(llPrioSelected,BETWEEN(APINVHDR.CVENPRIOR,lcPStrt,lcPFnsh),.T.) AND ;
               IIF(!EMPTY(lcRpVenPay),APINVHDR.CVENPMETH $ lcRpVenPay,.T.) AND ;
               IIF(llDueSelected ,BETWEEN(APINVHDR.DINVDUDAT,ldDueStrtDate,ldDueFnshDate),.T.) AND ;
               APINVHDR.CBNKCODE+APINVHDR.CCHKACCT = lcRpBnkCod+lcRpChkAct
           *N000635,7 TMI 08/14/2011 [End  ] 
           IF TYPE('lcXMLFileName') = 'C'
             IF MOD(RECNO('APINVHDR'),CEILING(RECCOUNT('APINVHDR') / 10)) = 0
              loProgress.Percent = lnPerCent * 0.9
              loProgress.Description = "Collecting Data For Invoice:"+ALLTRIM(APINVHDR.CinvNo)
              loAgent.UpdateObjectProgress(lcRequestID, loProgress)
            ENDIF  
          ENDIF 
          SCATTER MEMO MEMVAR 
          *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
          *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
          *=gfSeek(m.CVENDCODE,'apvendor','VENCODE')
          *m.cvencomp = apvendor.cvencomp
          *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
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
        lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
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
	             (TYPE('lcXMLFileName') = 'C' and (oAriaEnvironment.gcDevice  = "FILE" .AND. INLIST(oAriaEnvironment.Report.cTextRepType ,"EXCEL",'XML')))
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
            DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.       
          ELSE
            oAriaEnvironment.report.OGLastForm = lcRepForm
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
          
        ENDIF
        =lfUnLokBank()    && unlock the bank record
      ENDIF
    ENDIF
  
  CASE lcPrintMod = 'R'  && Reprint Checks
    IF lfLokBank()     
	    llSelectCheck = .F.
  		lnPosCheck = ASCAN(loOgScroll.laOgFXFlt,"lcRePrnChk")
  		IF lnPosCheck  > 0 
  		  lnPosCheck  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCheck  ,1)
  		  lcCursorChk= loOgScroll.laOgFxFlt[lnPosCheck,6]
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
        *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [Start]
        IF oAriaApplication.gcDevice="FILE" AND INLIST(loOGScroll.cTextRepType ,'XML','EXCEL')
          lcExpType  = loOGScroll.cTextRepType
          lcOutFile  = oAriaApplication.gcOutFile
          lcPDFPath = loOGScroll.gfTempName()
          
		  
          IF loOGScroll.lcOGPlatForm = 'WINDOWS'
           loOGScroll.cTextRepType = "PDF"
           lcPDFPath = oAriaApplication.WorkDir + lcPDFPath + ".PDF"
          ELSE
            loOGScroll.cTextRepType = ""
            lcPDFPath = oAriaApplication.WorkDir + lcPDFPath + ".TXT"
          ENDIF 
	      oAriaApplication.gcOutFile = lcPDFPath 
		  DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.
          loOGScroll.cTextRepType = lcExpType  
          oAriaApplication.gcOutFile = lcOutFile  
		ENDIF 
        *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [End]
        DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.        
      ELSE
        oAriaEnvironment.report.OGLastForm = lcRepForm
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
    INDEX on capdtrtyp+ cbnkcode+cchkacct+ cstubchk TAG (lcApDist) 
    
    lnPosCheck = ASCAN(loogscroll.laOgFXFlt,"lcRePrnChk")
    IF lnPosCheck  > 0 
      lnPosCheck  = ASUBSCRIPT(loogscroll.laOgFxFlt,lnPosCheck  ,1)
      lcCursorChk= loogscroll.laOgFxFlt[lnPosCheck,6]
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
lcGroup = CVendCode + CAddress 
REPLACE nNoOfInv WITH 0
DO WHILE !BOF()
  IF lcGroup = CVendCode + CAddress   && check if same group

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
    
    lcGroup     = CVendCode + CAddress
  ENDIF
  lnCount     = lnCount     + 1
  
  =gfSeek(Cinvno+Cvendcode,'APINVHDR')
  SELECT APINVHDR
  *** If device = 'File' Or 'Printer' and can lock cureent record.
  IF llPrintChk AND !lfObj_Lock(.T.)
    =gfReplace('')
    =gfTableUpdate()
    *** Invoice ð for vendor ð is being edited. 
    *** Check printing is canceled.
    SELECT (lcRpTargt)
    lcInvNo = APINVHDR.CINVNO
    lcVendCode = APINVHDR.CVendCode
    SCAN REST 
  	  SELECT APINVHDR
      =gfSeek(Cinvno+Cvendcode)
	  IF APINVHDR.CINVNO+APINVHDR.CVendCode <> lcInvNo +lcVendCode 
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
lcGroup = CVendCode + CAddress
SCAN 
  IF lcGroup  = CVendCode + CAddress .AND. lnInvCntr < lnRpStub
    lnInvCntr = lnInvCntr+1
  ELSE
    lnPgCntr  = lnPgCntr+1
    lnInvCntr = 1
  ENDIF
  REPLACE cPageNo WITH PADL(lnPgCntr,4)
  lcGroup = CVendCode + CAddress
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
  lcExstAdd = cvendcode+cAddress
  COUNT FOR cvendcode+cAddress = lcExstAdd TO lnNoOfIvs
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
  
  =gfSeek(Cvendcode,'APVENDOR')
  
  lnStubChkNo = INT(VAL(lcNxtChkNo))-1  
  lcChkNo = PADL(APCHECKS.NCHKNXTPN - 1,8,'0')
  
  
  SELECT APPAYMNT
  lcNewrec = lfGetNewRec('APPAYMNT')
  m.Rec_no = lcNewrec    
  *APPEND BLANK
  gfAppend('APPAYMNT',.T.)
  *N000635,5 TMI 07/28/2011 [Start] if table is native then do not update rec_no field  
  *REPLACE                                ;
  *  CPAYTYPE  WITH 'P'                   ;
  *  CPAYDOCNO WITH lcChkNo               ;
  *  CPAYMETH  WITH 'P'                   ;
  *  CPAYSTAT  WITH 'B'                   ;
  *  DPAYDATE  WITH ldChkDat            ;
  *  CFISFYEAR WITH lcFisYear             ;
  *  CFSPPRDID WITH lcFisPrd              ;
  *  DPAYVDATE WITH {}                    ;
  *  CPAYCLNO  WITH &lcRpTargt..CVendCode ;
  *  CPAYCOMP  WITH &lcRpTargt..COUTCOMP  ;
  *  NPAYAMNT  WITH 0                     ;
  *  NPAYDISC  WITH 0                     ;
  *  NPAYADJ   WITH 0                     ;
  *  LPAYADVAN WITH .F.                   ;
  *  NINV1099A WITH 0                     ;
  *  CBNKCODE  WITH lcRpBnkCod            ;
  *  CPAYRECST WITH 'O'                   ;    
  *  CCHKACCT  WITH lcRpChkAct,;
  *  Rec_no    With lcNewrec
  REPLACE  CPAYTYPE  WITH 'P'                   ;
           CPAYDOCNO WITH lcChkNo               ;
           CPAYMETH  WITH 'P'                   ;
           CPAYSTAT  WITH 'B'                   ;
           DPAYDATE  WITH ldChkDat            ;
           CFISFYEAR WITH lcFisYear             ;
           CFSPPRDID WITH lcFisPrd              ;
           DPAYVDATE WITH {}                    ;
           CPAYCLNO  WITH &lcRpTargt..CVendCode ;
           CPAYCOMP  WITH &lcRpTargt..COUTCOMP  ;
           NPAYAMNT  WITH 0                     ;
           NPAYDISC  WITH 0                     ;
           NPAYADJ   WITH 0                     ;
           LPAYADVAN WITH .F.                   ;
           NINV1099A WITH 0                     ;
           CBNKCODE  WITH lcRpBnkCod            ;
           CPAYRECST WITH 'O'                   ;    
           CCHKACCT  WITH lcRpChkAct
  IF !EMPTY(lcNewrec)
    REPLACE Rec_no    With lcNewrec
  ENDIF
  *N000635,5 TMI 07/28/2011 [End  ] 
  
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
    lcExSin5 = gfGetExSin(@lcExSin6,&lcRpTargt..cAprCurCod)
    lnAprPayB  = ROUND(&lcRpTargt..NINVFAAP &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    lnAprDisB  = ROUND(&lcRpTargt..NINVDISAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnAprAdjB  = ROUND(&lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnApr1099B = ROUND(&lcRpTargt..NINVA1099 &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(&lcRpTargt..NINVFAAP,2);
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    REPLACE APPAYMNT.CCURRCODE  WITH APCHECKS.CCURRCODE ;
            APPAYMNT.NEXRATE   WITH lnChkExRat ;
            APPAYMNT.NCURRUNIT WITH lnChkExUnt
            
    IF  TYPE('lcXMLFileName') <> 'C'        
      =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APPAYMNT',.NULL.)
    ENDIF 
    SELECT APPAYMNT
    =gfreplace("")

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
    =gfreplace("")                 
    
    REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(&lcRpTargt..NINVFAAP,2)
    
    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APCHECKS')  && Add the audit information to the record.
    ELSE
      loAddUserInfo.DO('APCHECKS',.NULL.)
    ENDIF   
    SELECT APCHECKS
    =gfreplace("")
    
    =gfSeek(Cvendcode + lcFisYear,'APVENHST')
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
    =gfreplace("")
    lcNewrec = lfGetNewRec('APDIST') 
    SELECT APDIST 
    *APPEND BLANK
    m.Rec_no = lcNewrec 
    gfAppend('APDIST',.T.)
    *N000635,5 TMI 07/28/2011 [Start] 
    *REPLACE CVENDCODE  WITH &lcRpTargt..CVENDCODE;
            CINVNO     WITH &lcRpTargt..CINVNO;
            CAPDTRTYP  WITH 'P';
            DAPDTRDAT  WITH ldChkDat;
            LAPDPOST   WITH  .F.;
            CAPDSTAT   WITH '';
            CAPDREF    WITH lcChkNo;
            CSTUBCHK   WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT  WITH &lcRpTargt..CAPACCT;
            CAPDACTID  WITH 'A';
            CBATCHNO   WITH '';
            CTRNSLEDN  WITH '';
            CFISFYEAR  WITH lcFisYear;
            CFSPPRDID  WITH lcFisPrd;
            CAPSESSNO  WITH lcSession;
            CTAXCODE   WITH '';
            CBNKCODE   WITH lcRpBnkCod;
            CCHKACCT   WITH lcRpChkact;
            NAPDAMNT   WITH &lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP;
            CCURRCODE  WITH &lcRpTargt..CCURRCODE;
            NEXRATE    WITH &lcRpTargt..NEXRATE;
            NCURRUNIT  WITH &lcRpTargt..NCURRUNIT;
            NEQVAMNT   WITH ROUND((&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT + lnAprDisB + lnAprAdjB),2),;
            Rec_no     WITH  lcNewrec
    REPLACE CVENDCODE  WITH &lcRpTargt..CVENDCODE;
            CINVNO     WITH &lcRpTargt..CINVNO;
            CAPDTRTYP  WITH 'P';
            DAPDTRDAT  WITH ldChkDat;
            LAPDPOST   WITH  .F.;
            CAPDSTAT   WITH '';
            CAPDREF    WITH lcChkNo;
            CSTUBCHK   WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT  WITH &lcRpTargt..CAPACCT;
            CAPDACTID  WITH 'A';
            CBATCHNO   WITH '';
            CTRNSLEDN  WITH '';
            CFISFYEAR  WITH lcFisYear;
            CFSPPRDID  WITH lcFisPrd;
            CAPSESSNO  WITH lcSession;
            CTAXCODE   WITH '';
            CBNKCODE   WITH lcRpBnkCod;
            CCHKACCT   WITH lcRpChkact;
            NAPDAMNT   WITH &lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP;
            CCURRCODE  WITH &lcRpTargt..CCURRCODE;
            NEXRATE    WITH &lcRpTargt..NEXRATE;
            NCURRUNIT  WITH &lcRpTargt..NCURRUNIT;
            NEQVAMNT   WITH ROUND((&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT + lnAprDisB + lnAprAdjB),2)
    IF !EMPTY(lcNewrec) 
      REPLACE Rec_no     WITH  lcNewrec
    ENDIF
    *N000635,5 TMI 07/28/2011 [End  ] 
    
    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF   

    =gfreplace("")
    IF &lcRpTargt..NINVA1099 <> 0
      lcNewrec = lfGetNewRec('APDIST') 
      *APPEND BLANK 
      m.Rec_no = lcNewrec 
      gfAppend('APDIST',.T.)
      *N000635,5 TMI 07/28/2011 [Start] 
      *REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
              CINVNO      WITH &lcRpTargt..CINVNO;
              CAPDTRTYP   WITH 'P';
              DAPDTRDAT   WITH ldChkDat;
              LAPDPOST    WITH .F.;
              CAPDSTAT    WITH 'V';
              CAPDREF     WITH lcChkNo;
              CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              CAPDGLACT   WITH '';
              NAPDAMNT    WITH &lcRpTargt..NINVA1099;
              CAPDACTID   WITH 'B';
              CBATCHNO    WITH '';
              CTRNSLEDN   WITH '';
              CFISFYEAR   WITH lcFisYear;
              CFSPPRDID   WITH lcFisPrd;
              CAPSESSNO   WITH lcSession;
              CTAXCODE    WITH '';
              CBNKCODE    WITH lcRpBnkCod;
              CCHKACCT    WITH lcRpChkact;
              CCURRCODE   WITH &lcRpTargt..CCURRCODE;
              NEXRATE     WITH &lcRpTargt..NEXRATE;
              NCURRUNIT   WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT    WITH &lcRpTargt..NINVA1099 ,;
              Rec_no     WITH  lcNewrec 
      REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
              CINVNO      WITH &lcRpTargt..CINVNO;
              CAPDTRTYP   WITH 'P';
              DAPDTRDAT   WITH ldChkDat;
              LAPDPOST    WITH .F.;
              CAPDSTAT    WITH 'V';
              CAPDREF     WITH lcChkNo;
              CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              CAPDGLACT   WITH '';
              NAPDAMNT    WITH &lcRpTargt..NINVA1099;
              CAPDACTID   WITH 'B';
              CBATCHNO    WITH '';
              CTRNSLEDN   WITH '';
              CFISFYEAR   WITH lcFisYear;
              CFSPPRDID   WITH lcFisPrd;
              CAPSESSNO   WITH lcSession;
              CTAXCODE    WITH '';
              CBNKCODE    WITH lcRpBnkCod;
              CCHKACCT    WITH lcRpChkact;
              CCURRCODE   WITH &lcRpTargt..CCURRCODE;
              NEXRATE     WITH &lcRpTargt..NEXRATE;
              NCURRUNIT   WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT    WITH &lcRpTargt..NINVA1099
      IF !EMPTY(lcNewrec)
        REPLACE Rec_no     WITH  lcNewrec 
      ENDIF  
      *N000635,5 TMI 07/28/2011 [End  ]         
      
      IF  TYPE('lcXMLFileName') <> 'C'              
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF   
      
      =gfreplace("")
    ENDIF

    *APPEND BLANK
  	lcNewrec = lfGetNewRec('APDIST') 
    m.Rec_no = lcNewrec 
    gfAppend('APDIST',.T.)
    *N000635,5 TMI 07/28/2011 [Start] 
    *REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
            CINVNO      WITH &lcRpTargt..CINVNO;
            CAPDTRTYP   WITH 'P';
            DAPDTRDAT   WITH ldChkDat;
            LAPDPOST    WITH .F.;
            CAPDSTAT    WITH '';
            CAPDREF     WITH lcChkNo;
            CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
            NAPDAMNT    WITH -&lcRpTargt..NINVFAAP;
            CAPDACTID   WITH 'C';
            CBATCHNO    WITH '';
            CTRNSLEDN   WITH '';
            CFISFYEAR   WITH lcFisYear;
            CFSPPRDID   WITH lcFisPrd;
            CAPSESSNO   WITH lcSession;
            CTAXCODE    WITH '';
            CBNKCODE    WITH lcRpBnkCod;
            CCHKACCT    WITH lcRpChkact;
            CCURRCODE   WITH &lcRpTargt..CAPRCURCOD;
            NEXRATE     WITH lnChkExRat;
            NCURRUNIT   WITH lnChkExUnt;
            NEQVAMNT    WITH -lnAprPayB,;
            Rec_no     WITH  lcNewrec 
    REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
            CINVNO      WITH &lcRpTargt..CINVNO;
            CAPDTRTYP   WITH 'P';
            DAPDTRDAT   WITH ldChkDat;
            LAPDPOST    WITH .F.;
            CAPDSTAT    WITH '';
            CAPDREF     WITH lcChkNo;
            CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
            NAPDAMNT    WITH -&lcRpTargt..NINVFAAP;
            CAPDACTID   WITH 'C';
            CBATCHNO    WITH '';
            CTRNSLEDN   WITH '';
            CFISFYEAR   WITH lcFisYear;
            CFSPPRDID   WITH lcFisPrd;
            CAPSESSNO   WITH lcSession;
            CTAXCODE    WITH '';
            CBNKCODE    WITH lcRpBnkCod;
            CCHKACCT    WITH lcRpChkact;
            CCURRCODE   WITH &lcRpTargt..CAPRCURCOD;
            NEXRATE     WITH lnChkExRat;
            NCURRUNIT   WITH lnChkExUnt;
            NEQVAMNT    WITH -lnAprPayB
    IF !EMPTY(lcNewrec)
      REPLACE Rec_no     WITH  lcNewrec 
    ENDIF
    *N000635,5 TMI 07/28/2011 [End  ] 
            
    IF  TYPE('lcXMLFileName') <> 'C'
      =gfAdd_Info('APDIST')
    ELSE
      loAddUserInfo.DO('APDIST',.NULL.)
    ENDIF   
    
    =gfreplace("")
    IF &lcRpTargt..NINVDISAP <> 0    
      *APPEND BLANK
      lcNewrec = lfGetNewRec('APDIST') 
      m.Rec_no = lcNewrec 
      gfAppend('APDIST',.T.)
      
      *N000635,5 TMI 07/28/2011 [Start] 
      *REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVDISAP;
              CAPDACTID WITH 'S';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CDISCACCT),APCHECKS.CDISCACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),APDIV.CDISCACCT,APSETUP.CDISCACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprDisB,;
			  Rec_no     WITH  lcNewrec 
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVDISAP;
              CAPDACTID WITH 'S';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CDISCACCT),APCHECKS.CDISCACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),APDIV.CDISCACCT,APSETUP.CDISCACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprDisB
      IF !EMPTY(lcNewrec)
        REPLACE Rec_no     WITH  lcNewrec 
      ENDIF
      *N000635,5 TMI 07/28/2011 [End  ] 
        
      IF   TYPE('lcXMLFileName') <> 'C'   
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF   
      
      =gfreplace("")
    ENDIF

    IF &lcRpTargt..NINVADJAP <> 0
    
      lcNewrec = lfGetNewRec('APDIST') 
      *APPEND BLANK 
      m.Rec_no = lcNewrec 
      gfAppend('APDIST',.T.)
      *N000635,5 TMI 07/28/2011 [Start] 
      *REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVADJAP;
              CAPDACTID WITH 'J';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CADJACCT),APCHECKS.CADJACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),APDIV.CADJACCT,APSETUP.CADJACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprAdjB,;
			  Rec_no     WITH lcNewrec 
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVADJAP;
              CAPDACTID WITH 'J';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CADJACCT),APCHECKS.CADJACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),APDIV.CADJACCT,APSETUP.CADJACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprAdjB
    IF !EMPTY(lcNewrec)
      REPLACE   Rec_no     WITH lcNewrec 
    ENDIF 
    *N000635,5 TMI 07/28/2011 [End  ] 
           
      IF   TYPE('lcXMLFileName') <> 'C'       
        =gfAdd_Info('APDIST')
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF   
      
      =gfreplace("")
    ENDIF

    lnExchDiff = lnAprPayB - ROUND(&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    IF lnExchDiff <> 0
      *APPEND BLANK
      lcNewrec = lfGetNewRec('APDIST')       
      m.Rec_no = lcNewrec 
      gfAppend('APDIST',.T.)

      *N000635,5 TMI 07/28/2011 [Start] 
      *REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO; 
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo+CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH lnExchDiff;
              CAPDACTID WITH 'J';
             CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
             CFISFYEAR WITH lcFisYear;
             CFSPPRDID WITH lcFisPrd;
             CAPSESSNO WITH lcSession;
             CTAXCODE  WITH '';
             CBNKCODE  WITH lcRpBnkCod;
             CCHKACCT  WITH lcRpChkact;
             CAPDGLACT WITH lcExDifAcc;
             CCURRCODE WITH gcBaseCurr;
             NEXRATE   WITH 1;
             NCURRUNIT WITH 1;
             NEQVAMNT  WITH lnExchDiff;
             NAPDLINNO WITH 1,;
             Rec_no     WITH  lcNewrec 
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO; 
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo+CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH lnExchDiff;
              CAPDACTID WITH 'J';
             CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
             CFISFYEAR WITH lcFisYear;
             CFSPPRDID WITH lcFisPrd;
             CAPSESSNO WITH lcSession;
             CTAXCODE  WITH '';
             CBNKCODE  WITH lcRpBnkCod;
             CCHKACCT  WITH lcRpChkact;
             CAPDGLACT WITH &lcRpTargt..capAcct;
             CCURRCODE WITH oAriaApplication.BaseCurrency;
             NEXRATE   WITH 1;
             NCURRUNIT WITH 1;
             NEQVAMNT  WITH lnExchDiff;
             NAPDLINNO WITH 1
      IF !EMPTY(lcNewrec)
        REPLACE   Rec_no     WITH  lcNewrec 
      ENDIF
      *N000635,5 TMI 07/28/2011 [End  ]
              
      IF   TYPE('lcXMLFileName') <> 'C'       
        =gfAdd_Info('APDIST')  && Add the audit information to the record.
      ELSE
        loAddUserInfo.DO('APDIST',.NULL.)
      ENDIF   
      
      =gfReplace('')
    ENDIF
    =gfSeek(&lcRpTargt..Cinvno+&lcRpTargt..Cvendcode,'APINVHDR')
    REPLACE APINVHDR.NINVPAID   WITH &lcRpTargt..NINVPAID  + &lcRpTargt..NINVAMTAP;
            APINVHDR.NINVDISTK  WITH &lcRpTargt..NINVDISTK + &lcRpTargt..NINVDISAP;
            APINVHDR.NINVADJ    WITH &lcRpTargt..NINVADJ   + &lcRpTargt..NINVADJAP;
            APINVHDR.NINV1099A  WITH &lcRpTargt..NINV1099A + &lcRpTargt..NINVA1099;
            APINVHDR.CBNKCODE   WITH '';
            APINVHDR.CCHKACCT   WITH '';
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
    =gfSeek(&lcRpTargt..Cinvno+&lcRpTargt..Cvendcode,'APINVHDR')
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
  lcInvToPrn = CINVNO+CVENDCODE+'P'
  SELECT APDIST  
  lnKeepRec=RECNO()
  PRIVATE lcOrder
  lcOrder = ORDER()
  =gfSetOrder('INVVEND')
  =gfSEEK(lcInvToPrn)  
  LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP=lcInvToPrn FOR capdstat<>'V'
  lcChkPrntd = cStubChk
  lcMastChk = cApdRef
  gfSETORDER('PAYMNTS')
  =gfSEEK('P'+lcRpBnkCod+lcRpChkAct+lcMastChk)
   SUM  REST nApdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF='P'+lcRpBnkCod+lcRpChkAct+lcMastChk  ;
             AND cStubChk<>lcChkPrntd FOR cApdStat <> 'V' AND cApdActId = 'C';
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
    lcReturnVal = ALLTRIM(lcReturnVal)+' '+ALLTRIM(APBANKS.cAddress&lcPostion)
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
        lnOption = TYPE('lcXMLFileName') <> 'C' and  gfModalGen("QRM04086B00012","DIALOG",ALLTRIM(STR(APCHECKS.NCHKNXTPN)))
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
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*  =gfSeek(ALLTRIM(&lcDebtCurs..cVendCode),'APVENDOR')
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
  lcRpVenCod  = &lcDebtCurs..cVendCode
	lcDebMemN   = &lcDebtCurs..cDebMemN
	lcFactor    = &lcDebtCurs..CFactor   
	lcAdvDiv    = &lcDebtCurs..CDIVISION  
	lcRef       = &lcDebtCurs..Reference 
	lcRem1      = &lcDebtCurs..Rem1      
	lcRem2      = &lcDebtCurs..Rem2     
	lcRem3      = &lcDebtCurs..Rem3      
	lcRem4      = &lcDebtCurs..Rem4      
	lcRem5      = &lcDebtCurs..Rem5    
	lcRem6      = &lcDebtCurs..Rem6    
	lcApAcct    = &lcDebtCurs..cApAcct   
	lnPaymnt    = &lcDebtCurs..nPaymnt   
	ln1099Amnt  = &lcDebtCurs..n1099Amnt 
  lcInvRemit  = &lcDebtCurs..cInvRemit
  lnEqvAmnt   = lnPaymnt    
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	  IF !lfvOkAdvPay()
*!*	    LOOP 
*!*	  ENDIF 
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
  SELECT (lcRpTargt)
  APPEND BLANK
  REPLACE CVENDCODE   WITH lcRpVenCod  ;
          CINVNO      WITH lcDebMemN   ;
          CDIVISION   WITH lcAdvDiv    ;
          DINVDATE    WITH ldChkDat    ;
          CINVREF     WITH lcRef       ;
          CINVREMIT   WITH lcInvRemit  ;
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
          CBNKCODE    WITH ''          ;
          CCHKACCT    WITH ''          ;
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
          nInvFaAp    WITH lnPaymnt
  
   *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
   =gfSeek(lcRpVenCod  ,'apvendor','VENCODE')
   m.cvencomp = apvendor.cvencomp
   *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  
  IF llPrintChk
    SELECT APINVHDR
    APPEND BLANK
    IF TYPE('APINVHDR.DPOSTDATE') = 'D'
      REPLACE CVENDCODE   WITH lcRpVenCod  ;
              CINVNO      WITH lcDebMemN   ;
              CDIVISION   WITH lcAdvDiv    ;
              DINVDATE    WITH ldChkDat    ;
              CINVREF     WITH lcRef       ;
              CINVREMIT   WITH lcInvRemit  ;
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
              CBNKCODE    WITH ''          ;
              CCHKACCT    WITH ''          ;
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
              nCurrUnit   WITH lnCurrUnit  ;
              nExRate     WITH lnExRate    
            

    ELSE
      REPLACE CVENDCODE   WITH lcRpVenCod  ;
              CINVNO      WITH lcDebMemN   ;
              CDIVISION   WITH lcAdvDiv    ;
              DINVDATE    WITH ldChkDat    ;
              CINVREF     WITH lcRef       ;
              CINVREMIT   WITH lcInvRemit  ;
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
              CBNKCODE    WITH ''          ;
              CCHKACCT    WITH ''          ;
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
              nCurrUnit   WITH lnCurrUnit  ;
              nExRate     WITH lnExRate    
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

      *N000635,5 TMI 07/28/2011 [Start] 
      *REPLACE CPAYTYPE  WITH 'P'      ;
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
              CBNKCODE  WITH lcRpBnkCod  ;
              CPAYRECST WITH 'O'         ;        
              CCHKACCT  WITH lcRpChkAct  ;
         	  cCurrCode WITH lcCurrCode  ; 
        	  nCurrUnit WITH lnCurrUnit  ;
        	  nExRate   WITH lnExRate,;
        	  Rec_no   With lcNewrec 
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
              CBNKCODE  WITH lcRpBnkCod  ;
              CPAYRECST WITH 'O'         ;        
              CCHKACCT  WITH lcRpChkAct  ;
         	  cCurrCode WITH lcCurrCode  ; 
        	  nCurrUnit WITH lnCurrUnit  ;
        	  nExRate   WITH lnExRate
      IF !EMPTY(lcNewrec)
        REPLACE Rec_no   With lcNewrec 
      ENDIF
    *N000635,5 TMI 07/28/2011 [End  ]             
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
    =gfSeek(ALLTRIM(lcRpVenCod) + lcFisYear)
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

    *N000635,5 TMI 07/28/2011 [Start] 
    *REPLACE CVENDCODE   WITH lcRpVenCod ;
            CINVNO      WITH lcDebMemN  ;
            CAPDTRTYP   WITH 'P'        ;
            DAPDTRDAT   WITH ldChkDat   ;
            LAPDPOST    WITH .F.        ;
            CAPDSTAT    WITH ''         ;
            CAPDREF     WITH lcNxtChkNo ;
            CSTUBCHK    WITH lcNxtChkNo ;
            CAPDGLACT   WITH lcApAcct   ;
            NAPDAMNT    WITH lnPaymnt   ;
            CAPDACTID   WITH 'A'        ;
            CBATCHNO    WITH ''         ;
            CTRNSLEDN   WITH ''         ;
            CFISFYEAR   WITH lcFisYear  ;
            CFSPPRDID   WITH lcFisPrd   ;
            CAPSESSNO   WITH lcSession  ;
            CTAXCODE    WITH ''         ;
            CBNKCODE    WITH lcRpBnkCod ;
            CCHKACCT    WITH lcRpChkAct ;
            NAPDLINNO   WITH 0          ;
            nEqvAmnt    WITH lnEqvAmnt  ; 
            nExRate     WITH lnExRate ;
            cCurrCode   WITH lcCurrCode ;
            nCurrUnit   WITH lnCurrUnit,;
		    Rec_no     WITH  lcNewrec 
    REPLACE CVENDCODE   WITH lcRpVenCod ;
            CINVNO      WITH lcDebMemN  ;
            CAPDTRTYP   WITH 'P'        ;
            DAPDTRDAT   WITH ldChkDat   ;
            LAPDPOST    WITH .F.        ;
            CAPDSTAT    WITH ''         ;
            CAPDREF     WITH lcNxtChkNo ;
            CSTUBCHK    WITH lcNxtChkNo ;
            CAPDGLACT   WITH lcApAcct   ;
            NAPDAMNT    WITH lnPaymnt   ;
            CAPDACTID   WITH 'A'        ;
            CBATCHNO    WITH ''         ;
            CTRNSLEDN   WITH ''         ;
            CFISFYEAR   WITH lcFisYear  ;
            CFSPPRDID   WITH lcFisPrd   ;
            CAPSESSNO   WITH lcSession  ;
            CTAXCODE    WITH ''         ;
            CBNKCODE    WITH lcRpBnkCod ;
            CCHKACCT    WITH lcRpChkAct ;
            NAPDLINNO   WITH 0          ;
            nEqvAmnt    WITH lnEqvAmnt  ; 
            nExRate     WITH lnExRate ;
            cCurrCode   WITH lcCurrCode ;
            nCurrUnit   WITH lnCurrUnit
      IF !EMPTY(lcNewrec)
        REPLACE Rec_no     WITH  lcNewrec 
      ENDIF
    *N000635,5 TMI 07/28/2011 [End  ] 
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

    *N000635,5 TMI 07/28/2011 [Start] 
    *REPLACE CVENDCODE   WITH lcRpVenCod   ;
            CINVNO      WITH lcDebMemN    ;
            CAPDTRTYP   WITH 'P'          ;
            DAPDTRDAT   WITH ldChkDat     ;
            LAPDPOST    WITH .F.          ;
            CAPDSTAT    WITH ''           ;
            CAPDREF     WITH lcNxtChkNo   ;
            CSTUBCHK    WITH lcNxtChkNo   ;
            CAPDGLACT   WITH lcRpGlAcct   ;
            NAPDAMNT    WITH 0 - lnPaymnt ;
            CAPDACTID   WITH 'C'          ;
            CBATCHNO    WITH ''           ;
            CTRNSLEDN   WITH ''           ;
            CFISFYEAR   WITH lcFisYear    ;
            CFSPPRDID   WITH lcFisPrd     ;
            CAPSESSNO   WITH lcSession    ;
            CTAXCODE    WITH ''           ;
            CBNKCODE    WITH lcRpBnkCod   ;
            CCHKACCT    WITH lcRpChkAct   ;
            NAPDLINNO   WITH 1            ; 
            nEqvAmnt    WITH 0 - lnEqvAmnt; 
            nExRate     WITH lnExRate   ;
            cCurrCode   WITH lcCurrCode   ;
            nCurrUnit   WITH lnCurrUnit,;
			Rec_no     WITH  lcNewrec 
    REPLACE CVENDCODE   WITH lcRpVenCod   ;
            CINVNO      WITH lcDebMemN    ;
            CAPDTRTYP   WITH 'P'          ;
            DAPDTRDAT   WITH ldChkDat     ;
            LAPDPOST    WITH .F.          ;
            CAPDSTAT    WITH ''           ;
            CAPDREF     WITH lcNxtChkNo   ;
            CSTUBCHK    WITH lcNxtChkNo   ;
            CAPDGLACT   WITH lcRpGlAcct   ;
            NAPDAMNT    WITH 0 - lnPaymnt ;
            CAPDACTID   WITH 'C'          ;
            CBATCHNO    WITH ''           ;
            CTRNSLEDN   WITH ''           ;
            CFISFYEAR   WITH lcFisYear    ;
            CFSPPRDID   WITH lcFisPrd     ;
            CAPSESSNO   WITH lcSession    ;
            CTAXCODE    WITH ''           ;
            CBNKCODE    WITH lcRpBnkCod   ;
            CCHKACCT    WITH lcRpChkAct   ;
            NAPDLINNO   WITH 1            ; 
            nEqvAmnt    WITH 0 - lnEqvAmnt; 
            nExRate     WITH lnExRate   ;
            cCurrCode   WITH lcCurrCode   ;
            nCurrUnit   WITH lnCurrUnit
     IF !EMPTY(lcNewrec)
       REPLACE Rec_no     WITH  lcNewrec 
     ENDIF
  *N000635,5 TMI 07/28/2011 [End  ] 

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
IF &lcRpTargt..cvendcode = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..caddress = lcaddress AND &lcRpTargt..cPageNo = lcPageNo
  lcExactSt = SET("Exact")
  SET EXACT ON 
  lnPosArr = ASCAN(laFooter,&lcRpTargt..cinvno)
  IF lnPosArr>0
    lnPosArr = ASUBSCRIPT(laFooter,lnPosArr,1)
    IF laFooter[lnPosArr ,1] = &lcRpTargt..cinvno AND laFooter[lnPosArr ,2] =  DTOC(&lcRpTargt..dinvdate)
      SET EXACT &lcExactSt
      SET CENTURY &lcSetCent.
      RETURN 
    ENDIF   
  ENDIF 
  SET EXACT &lcExactSt
  lcaddress= &lcRpTargt..caddress
  lcPageNo = &lcRpTargt..cPageNo  
  lnFooter = lnFooter + 1
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)
  =lfSetCurSm(&lcRpTargt..cCurrCode)
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,;
                                '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
                             ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
                             , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
                             '@ 9999,999,999.99')) + lcNetSmbl , 15) )
  
ELSE
  lnFooter = 1
  laFooter = ''
  lcVendor = &lcRpTargt..cvendcode
  lcaddress = &lcRpTargt..caddress
  lcPageNo = &lcRpTargt..cPageNo
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)
  =lfSetCurSm(&lcRpTargt..cCurrCode)
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,;
                                '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,;
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
Go BOTTOM  
lcRpChkTo =  &lcCursorChk..KeyExp

SELECT APDIST
* Seek for the First record that fall in the range of the checks
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 

*=gfSEEK('P'+lcRpBnkCod+lcRpChkAct,'APDIST')
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
*!*	=gfSqlRun("select APDIST.CAPDTRTYP,APDIST.CBNKCODE as CBNKCODE,APDIST.CCHKACCT as CCHKACCT,APDIST.cStubChk,APDIST.cApdStat,APDIST.cApdRef,APDIST.CAPDACTID,APDIST.NAPDAMNT,APDIST.NAPDLINNO,apvendor.cvencomp,APINVHDR.* "+;
*!*			   " from APINVHDR INNER join APDIST on APINVHDR.Cvendcode =APDIST.CVENDCODE AND APINVHDR.CINVNO= APDIST.CINVNO Inner join apvendor on apvendor.Cvendcode  = APINVHDR.Cvendcode "+;
*!*			   " where   APINVHDR.CVENPRIOR <>'0' and APDIST.CAPDTRTYP+APDIST.CBNKCODE+APDIST.CCHKACCT='P"+lcRpBnkCod+lcRpChkAct+"' AND APDIST.cApdStat <> 'V'",'APDIST',.F.,'APDIST')
*!*	LOCATE 
=gfSEEK('P'+lcRpBnkCod+lcRpChkAct,'APDIST')
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
STORE 0 TO lnPgCntr,lnCount
STORE '' TO lcInvoice,lcStubChk
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 

*=gfSeek(CINVNO+CVENDCODE,'APINVHDR')
*lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
          APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
*lcGroup = cVendCode + UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+;
          cOutAddr4+cOutAddr5)
=gfSeek(CINVNO+CVENDCODE,'APINVHDR')
lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
          APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
lcApdRef = cApdRef

* Scan through the APDIST file for all the checks that fall within the range
* of the checks to be reprinted and is not voided

SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct FOR SEEK(cStubChk,lcCursorChk);
    AND cApdStat <> 'V' 
 *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [Start]
  *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
  *=gfSeek(CINVNO+CVENDCODE,'APINVHDR')
  *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
  *!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [End]
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
  =gfSeek(CINVNO+CVENDCODE,'APINVHDR')
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
  * if the current invoice <> the old one
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	  IF lcInvoice <> APDIST.CINVNO OR lcGroup <> cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)  
*!*	    lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
*!*	  IF lcInvoice <> APDIST.CINVNO OR lcGroup <> cVendCode + UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+cOutAddr4+cOutAddr5)  
*!*	    lcGroup = cVendCode + UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+cOutAddr4+cOutAddr5)
  IF lcInvoice <> APDIST.CINVNO OR lcGroup <> cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)  
    lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
    * if the stub check # has been chaned   
    IF lcStubChk <> cStubChk
    
       * if the master check # <> the last master check #
       * then replace the last record in the temprory file with 0
       * so the printing can feel that this is a master check
       * increament the counter with 1 and change the variable that hold
       * the last master check #
        IF lcApdRef = lcStubChk
         lnCount  = 1
         REPLACE &lcRpTargt..nNoofInv WITH 0
       ELSE  
         lnCount = lnCount + 1
       ENDIF
      lcApdRef = cApdRef
      lnPgCntr = lnPgCntr + 1
      lcStubChk = cStubChk
    ENDIF
    * Select the invoice header file get the values of the current record
    *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
    *=gfSeek(Cinvno+Cvendcode,'APINVHDR')
    * SELECT APINVHDR
    *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
    =gfSeek(Cinvno+Cvendcode,'APINVHDR')
    SELECT APINVHDR
    *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
   *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
    SCATTER MEMVAR MEMO
    * Select the temprory file and append the new record and store the values
    SELECT (lcRpTargt)
    APPEND BLANK
    GATHER MEMVAR MEMO

    *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
    *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*		=gfSeek(m.Cvendcode ,'apvendor','VENCODE')
*!*		REPLACE cvencomp WITH  apvendor.cvencomp
    *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[START]
	*REPLACE cvencomp WITH  cvencomp
	=gfSeek(m.Cvendcode ,'apvendor','VENCODE')
	REPLACE cvencomp WITH  apvendor.cvencomp
    *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
	*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
    *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  
    * replace the page # that the check will be printed in with the needed page #  
    * and the # of invoices that will be printed in the same check with
    *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	    REPLACE cAddress WITH UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
*!*	            APINVHDR.cOutAddr4+APINVHDR.cOutAddr5),;
*!*	            nNoOfInv WITH CEILING(lnCount/lnRpStub) ;
*!*	            cPageNo  WITH PADL(lnPgCntr,4)
    *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start] 
*!*	    REPLACE cAddress WITH UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+;
*!*	            cOutAddr4+cOutAddr5),;
*!*	            nNoOfInv WITH CEILING(lnCount/lnRpStub) ;
*!*	            cPageNo  WITH PADL(lnPgCntr,4)
    REPLACE cAddress WITH UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
            APINVHDR.cOutAddr4+APINVHDR.cOutAddr5),;
            nNoOfInv WITH CEILING(lnCount/lnRpStub) ;
            cPageNo  WITH PADL(lnPgCntr,4)
    *!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [END]             
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End] 
    lcInvoice = APDIST.cInvNo        
  ENDIF  
  * Select the temprory file and start updating the ammount values 
  SELECT (lcRpTargt)
  
  DO CASE
    CASE APDIST.CAPDACTID = 'C'

      REPLACE NINVFAAP     WITH -APDIST.NAPDAMNT 

      
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
IF cApdRef = cStubChk
  REPLACE &lcRpTargt..nNoOfInv  WITH 0
ENDIF  

SELECT APDIST
SET RELATION TO 
IF !EMPTY(lcOrder)
  gfSETORDER(lcOrder)
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
=gfTAbleUpdate()
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
SCAN REST FOR CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND  ;
     SEEK(cStubChk,lcCursorChk) AND cApdStat <> 'V'
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
  IF CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND ;
     SEEK(cStubChk,lcCursorChk) AND !EOF()
     lcApDChkRec = cStubChk
  ELSE   
     lcApDChkRec = ''
  ENDIF
  * go to the previous record to update it
  SKIP -1      
  * if the current record check # <> the Previous one
  IF lcCurChk <> cStubChk
    lcCurChk  = cStubChk
    lnChkNo   = lnChkNo + 1  
    * if the stub check = the master check
    IF ALLTR(cStubChk) = ALLTR(cApdRef)
      * seek in the APPAYMNT file to update the record of the payment
      * with the new check #
      SELECT APPAYMNT
      IF gfSEEK('PP'+APDIST.cStubChk+lcRpBnkCod+lcRpChkAct)
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

        *N000635,5 TMI 07/28/2011 [Start] 
        *REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0'),;
                DPAYDATE WITH ldChkDat,;
                Rec_no   With lcNewrec 
        REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0'),;
                DPAYDATE WITH ldChkDat
        IF !EMPTY(lcNewrec)        
          REPLACE Rec_no   With lcNewrec 
        ENDIF
        *N000635,5 TMI 07/28/2011 [End  ] 

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
        IF gfSEEK(APDIST.cVendCode,'APVENDOR')
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
        SCAN FOR  CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID='P'+lcRpBnkCod+lcRpChkAct+lcCurChk        
          REPLACE cApdRef WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
          =gfReplace('')
        ENDSCAN   
        
        GO lnCurRec
        gfSetOrder('CHECKS')
      ENDIF  
    ENDIF
    SELECT APDIST    
  ENDIF
  REPLACE cStubChk WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
  =gfReplace('')
  IF gfSEEK(CINVNO+CVENDCODE,'APINVHDR')
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
=gfSeek(lcRpBnkCod + lcRpChkAct)
lcCurrency = APCHECKS.cCurrCode 
lnCurSEl = ASCAN(laCurrSmbl , lcCurrency)

llUsed = .F.
IF !USED('SYCCURR')
  =gfOpenTable('SYCCURR','CCURRCODE')
  llUsed = .T.
ENDIF 
SELECT SYCCURR
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [Start]
*=gfSeek(SYCCURR.cCurrCode)
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[Start]
*=gfSeek(lcCurrency)
=gfSeek(lcCurrency,'SYCCURR','CCURRCODE')
*!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [End]
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
      IF !EMPTY(cLok_User)
        *IF ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)
        *  *!600396,1 Messaging the user that he cannot edit the same record
        *  *!600396,1 from more than one session and permit him from editing
        *  *!600396,1 the same record
        lcLok_User = cLok_User
        IF !USED('syuStatc')
*!*	          IF oAriaApplication.multiinst
*!*	            lcSQLDICPATH = STRTRAN(oAriaApplication.clientprogramhome,"Prgs\","SQLDictionary\")
*!*	          ELSE
*!*	            lcSQLDICPATH = oAriaApplication.DefaultPath + 'SQLDictionary\'
*!*	          ENDIF 
          *N000635,5 TMI 07/28/2011 [Start] open syustatic from its path
          *=gfOpenFile('syuStatc','Cuser_id','SH','syuStatc')
          =gfOpenFile(oAriaApplication.cAria4SysPath+'syuStatc','Cuser_id','SH','syuStatc')          
          *N000635,5 TMI 07/28/2011 [End  ] 
        ENDIF  
        SELECT APINVHDR
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
            UNLOCK

              *** Display the message "Record is in use by user AAAA"
              lcLok_User = oAriaApplication.getUserName(lcLok_User)
              *** Record is in use by user ????    
              lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(CVendCode)+" is being edited by user " + lcLok_User+"."
              IF TYPE('lcXMLFileName') <> 'C' AND gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
                lcCinvno = APINVHDR.CINVNO
                lcCvendcode = APINVHDR.Cvendcode
                =gfSeek(lcCinvno +lcCvendcode ,'APINVHDR')
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
              cLok_User WITH ""  , ;
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
             cLok_User WITH gcUser_ID , ;
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
*N000635,5 TMI 07/28/2011 [Start] check if table is native
*llNID = gfSqlRun('Select NEWID() as NEWIDVAL',lcTableName,.T.,'NEWID_CUR')
llNID = IIF(lfIsNative(lcTableName),.F.,;
            gfSqlRun('Select NEWID() as NEWIDVAL',lcTableName,.T.,'NEWID_CUR'))
*N000635,5 TMI 07/28/2011 [End  ]             
lcNewRec = ''
IF llNID 
  lcNewRec  = NEWID_CUR.NEWIDVAL
ELSE
  lcNewRec = ''
ENDIF  
SELECT(lcOldAlis)
RETURN lcNewRec 


*!*************************************************************
*! Name      : lfIsNative
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 07/21/2011 
*! Purpose   : check if table is fox
*!*************************************************************
*N000636,3 TMI 07/21/2011 
FUNCTION lfIsNative
PARAMETERS lcAlias

lcAlias = UPPER(lcAlias)
LOCAL llNative,lcTempCurs,lnSlct
lnSlct = SELECT(0)
lcTempCurs = gfTempName()
llNative = .T.
*<Write here the code that checks if this table is native>
lnRemResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYDFILES WHERE CFILE_NAM = '&lcAlias'",'',"&lcTempCurs","",oAriaApplication.cAria4sysfiles,3,"",SET("Datasession"))
SELECT (lcTempCurs)
LOCATE 
llNative = !FOUND()
USE IN (lcTempCurs)

SELECT (lnSlct)
RETURN llNative
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[Start]
*!*************************************************************
*! Name      : lfIncNxtChk
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Update check number field
*!************************************************************************
FUNCTION lfIncNxtChk
lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
RETURN ''

*!*************************************************************
*! Name      : lfAllgTest
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Alignment test validation
*!************************************************************************
*
FUNCTION lfAllgTest

llEndGrp  = .F.
lnTotal   = 0

lnNoOfFields = lfCreateCur()
SELECT (lcRpTargt)
APPEND BLANK
FOR lnCount = 1 TO lnNoOfFields
  lcFieldName = FIELD(lnCount)
  DO CASE

    CASE TYPE(lcFieldName) = 'C'
      REPLACE &lcFieldName WITH REPLICATE('X',FSIZE(lcFieldName))

    CASE TYPE(lcFieldName) = 'N'
      REPLACE &lcFieldName WITH VAL(REPLICATE('9',MIN(FSIZE(lcFieldName),6)))

    CASE TYPE(lcFieldName) = 'L'
      REPLACE &lcFieldName WITH .T.

    CASE TYPE(lcFieldName) = 'D'
      REPLACE &lcFieldName WITH DATE()

  ENDCASE
ENDFOR

llPrintChk = .F.
llTestChk  = .T.
oAriaApplication.gcDevice = "PRINTER"
loOgScroll.ll2Printer = .T.
loogScroll.ReportPrint()
=loOGScroll.RefreshScroll()
llTestChk = .F.
USE IN (lcRpTargt)

*!*************************************************************
*! Name      : lfwGrid  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : When Function
*!************************************************************************
*
FUNCTION lfwGrid  


IF (TYPE('lcSession') = 'U' OR (TYPE('lcSession') = 'C' AND EMPTY(lcSession)))
  lcSession = gfSequence('CAPSESSNO')
ENDIF 

PRIVATE lcTitle
lcTitle = PROPER(ALLTRIM(WTITLE('GWDGRID')))
IF !('Session : ' $ loogScroll.Parent.Caption)
  loogScroll.Parent.Caption =  '(' + ALLTRIM(oAriaApplication.ActiveCompanyID) + ') ' + ;
                 IIF(EMPTY(oAriaApplication.ActiveCompanyName), ;
                     '', ;
                     '- ' + ALLTRIM(oAriaApplication.ActiveCompanyName) + ' - ') +" " + ALLTRIM(SYDREPRT.cRep_Name) + " " +'  Session : ' + lcSession
ENDIF
lcOGWinTitl = loogScroll.Parent.Caption 

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

IF APSETUP.CAPSGLLINK = 'Y'
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
  IF USED('SYCCOMP')
    =gfCloseTable('SYCCOMP')
  ENDIF 
  *!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [End]
  IF !USED('SYCCOMP')
    =gfOpenTable('SYCCOMP','CCOMP_ID','SH')   
    SELECT 'SYCCOMP'
    =GFSEEK('')
  ENDIF   
  SELECT 'SYCCOMP'
  =SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP','CCOMP_ID')
  lcDataDir = IIF(EMPTY(SYCCOMP.cCompPrnt)              , ;
              ALLTRIM(SYCCOMP.cCom_dDir), ;
              ALLTRIM(LOOKUP(SYCCOMP.CCOM_DDIR,APSETUP.cApSgLCom,SYCCOMP.CCOMP_ID,"CCOMP_ID")))
  lcDataDir = oAriaApplication.GetDataDir(lcDataDir)
  IF FILE(lcDataDir+'GLACCHAR.DBF')
    SELECT 0  
    =gfOpenTable(ADDBS(lcDataDir)+'GLACCHAR','ACCTCODE','SH','lcLinkChar')
    llOpenAcHar = .T.
    SELECT('lcLinkChar')
    gfSetOrder('ACCTCODE')
  ENDIF
ENDIF  

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
  *USE &gcDataDir.APVENHST IN 0
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

*****************************

laOpenFile [4,1] = 'APCHECKS'
IF !USED('APCHECKS')
  =gfOpenTable('APCHECKS','BANKCHECK')
  laOpenFile [4,2] = .T.
ENDIF

SELECT APCHECKS
laOpenFile [4,3] = ORDER()
=gfSetOrder('BANKCHECK')
=gfSEEK(lcRpBnkCod+lcRpChkAct)

*****************************

laOpenFile [5,1] = 'APINVHDR'
IF !USED('APINVHDR')
  =gfOpenTable('APINVHDR','INVVEND')
  laOpenFile [5,2] = .T.
ENDIF

SELECT APINVHDR
laOpenFile [5,3] = ORDER()
=gfSetOrder('INVVEND')


*****************************

laOpenFile [6,1] = 'APPAYMNT'
IF !USED('APPAYMNT')
  =gfOpenTable('APPAYMNT','TYPMETHDOC')
  laOpenFile [6,2] = .T.
ENDIF

SELECT APPAYMNT
laOpenFile [6,3] = ORDER()
gfSetOrder('TYPMETHDOC')
*****************************

laOpenFile [7,1] = 'APDIST'
IF !USED('APDIST')
  =gfOpenTable('APDIST','PAYMNTS')
  laOpenFile [7,2] = .T.
ENDIF

SELECT APDIST
laOpenFile [7,3] = ORDER()
=gfSetOrder('PAYMNTS')


*****************************

laOpenFile [8,1] = 'SYCFACT'
IF !USED('SYCFACT')
  =gfOpenTable('SYCFACT','CFACCODE')
  laOpenFile [8,2] = .T.
ENDIF

SELECT SYCFACT
laOpenFile [8,3] = ORDER()
=gfSetOrder('CFACCODE')


*****************************

laOpenFile [9,1] = 'SYCINT'
IF !USED('SYCINT')
  =gfOpenTable('SYCINT','CCONTCODE')
  laOpenFile [9,2] = .T.
ENDIF

SELECT SYCINT
laOpenFile [9,3] = ORDER()
gfSetOrder('CCONTCODE')

*****************************

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
lcPrintMod = lcRpChkMod
lcOldMode = lcRpChkMod
STORE '' TO lcRpChkFrm,lcRpChkTo

IF loogScroll.lcOGPrgName <>  'APCHKPR'+lcPrintMod
   lcOldMode = 'X'
   lfvPrintMode(.T.)
ENDIF 
IF loogScroll.lnOGSeting = 1
  lcOldMode = 'X'
  lcPrintMod = "V"
  loogScroll.lcOGPrgName = 'APCHKPR'+'V'
  lfvPrintMode(.T.)
ENDIF  
loogScroll.lcOGPrgName = 'APCHKPR'+'V'
loogScroll.lcOGManRep= 'APCHKPR'+'V'
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*lcRepForm = 'APCHKPWL'
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
*=ADIR(laFrxFiles,oAriaApplication.ReportHome+'*.FRX')
=ADIR(laFrxFiles,IIF(oAriaApplication.MULTIINST,oAriaApplication.clientreporthome,oAriaApplication.ReportHome)+'*.FRX')
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
    ELSE 
      **!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
      *lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')   
        *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[Start]
*!*	      lcSrvRpt = STRTRAN(UPPER(IIF(oAriaApplication.MULTIINST,oAriaApplication.clientreporthome,oAriaApplication.ReportHome)),'REPORTS','SRVRPTS')   
*!*	      **!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]
*!*	      DIME laFrxFiles[1,4]
*!*	      STORE '' TO laFrxFiles
*!*	      =ADIR(laFrxFiles,lcSrvRpt+'*.FRX')
*!*	      IF !EMPTY(laFrxFiles[1,1])
*!*	        IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
*!*	          lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
*!*	        ELSE
*!*	          IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
*!*	            lcRepForm = '__' + SUBSTR(lcRepForm , 3)
*!*	          ENDIF   
*!*	        ENDIF 
*!*	      ENDIF
        *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
    ENDIF
  ENDIF
ENDIF  
loogScroll.CheckPlatForm()

IF ASCAN(LAOGOBJTYPE,'LCRPGLACCT') # 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPGLACCT'),1)
  LAOGOBJCNT[LNPOS] = IIF(lcPrintMod = 'V',.F.,.T.)
  = LFOGSHOWGET('LCRPGLACCT')
ENDIF

IF lcPrintMod <> 'R'  
  lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
ENDIF


IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler
  IF ASCAN(LAOGOBJTYPE,'LCPRINTMOD') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCPRINTMOD'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LCPRINTMOD')
  ENDIF
  
  IF ASCAN(LAOGOBJTYPE,'LNALTEST') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LNALTEST'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LNALTEST')
  ENDIF
ENDIF 


*!*************************************************************
*! Name      : lfvGrid  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : validation Function
*!*************************************************************************
FUNCTION lfvGrid    
FOR lnCounter = 1 TO ALEN(laOpenFile,1)
  IF laOpenFile[lnCounter,2] 
    IF USED(ALIAS(laOpenFile[lnCounter,1]))
      =gfCloseTable(laOpenFile[lnCounter,1])
    ENDIF  
  ELSE
    SELECT (laOpenFile[lnCounter,1])
    =gfSetOrder(laOpenFile[lnCounter,3])
  ENDIF
ENDFOR
        
*!*************************************************************
*! Name      : lfvBnkCode   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate bank code
*!*************************************************************************
FUNCTION lfvBnkCode   
IF EMPTY(lcRpBnkCod)
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
  RETURN 
ENDIF


lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue


SELECT APBANKS
=gfSeek('')
IF '?' $ lcRpBnkCod .OR. !SEEK(lcRpBnkCod)
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
  laRpRetFld[1] = ''
  =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpBnkCod = lcOldVal
  ELSE
    lcRpBnkCod = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpBnkCod <> lcOldVal .AND. !EMPTY(lcRpBnkCod)  && in case of press cancel and Empty(lcOldVal)
  =gfSEEK(lcRpBnkCod,'APCHECKS')
  lcRpChkAct = APCHECKS.CCHKACCT
  IF gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ELSE
    *** The bank has no checking accounts setup.
    =gfModalGen("INM04023B00000","DIALOG")
    lcRpBnkCod = lcOldVal
  ENDIF
ENDIF
        
*!*************************************************************
*! Name      : lfvChkAcct            
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Checking account
*!*************************************************************************
*  
FUNCTION lfvChkAcct            

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
  lcRpChkAct = ''
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  RETURN
ENDIF
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue
SELECT APCHECKS
=gfSeek('')
*** Search for the current checking account code
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*IF '?' $ lcRpChkAct .OR.!SEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct))
IF '?' $ lcRpChkAct .OR.!SEEK(PADR(lcRpBnkCod,8)+PADR(lcRpChkAct,12))
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
  laRpRetFld[1] = ''
  =gfBrows([lcRpBnkCod],'CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpChkAct = lcOldVal
  ELSE
    lcRpChkAct = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpChkAct <> lcOldVal
  IF EMPTY(lcRpChkAct)  && in case of press cancel and Empty(lcOldVal)
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    lcNxtChkNo = ''
    lcRpGlAcct = ''
  ELSE
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfValidAcct    
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate GL account
*!*************************************************************************
*  
*
FUNCTION lfValidAcct    
PARAMETERS lcSendData

RETURN = IIF(APSETUP.CAPSGLLINK='Y', ;
         IIF(SEEK(ALLTRIM(lcSendData),'lcLinkChar'),.T.,.F.),.T.)

*!*************************************************************
*! Name      : lfvNxtChk   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Next Check Number
*!*************************************************************************
*
FUNCTION lfvNxtChk   

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
  lcNxtChkNo = ''
  RETURN
ENDIF

IF EMPTY(lcRpChkAct)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'checking account')
  lcNxtChkNo = ''
  RETURN
ENDIF

IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler AND EMPTY(lcNxtChkNo)
  RETURN   
ENDIF 

lcNxtChkNo = PADL(ALLTRIM(lcNxtChkNo),8,'0')
IF gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
  IF INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
  ENDIF
ELSE
  *** ð account not valid.
  =gfModalGen("INM04077B00000","DIALOG",'Checking')
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
ENDIF


*!*************************************************************
*! Name      : lfvChkDate   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate  Check date
*!*************************************************************************
FUNCTION lfvChkDate   


lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
IF !lfVDTMSG(oAriaApplication.PrntCompanyID,@lcFisPrd,@lcFisYear,ldChkDat)
  ldChkDat = lcCurVar.objects[5].objects[1].oldvalue
ENDIF

*!*************************************************************
*! Name      : lfReValidate  
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Before printing this function revalid the needed informations
*!*************************************************************************
* 
FUNCTION lfReValidate  


DO CASE
  CASE lcPrintMod = 'R' 
    llSelectCheck = .F.
    lnPosCheck = ASCAN(loOgScroll.laOgFXFlt,"lcRePrnChk")
    IF lnPosCheck  > 0 
      lnPosCheck  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosCheck  ,1)
      lcCursorChk= loOgScroll.laOgFxFlt[lnPosCheck,6]
      IF !EMPTY(lcCursorChk) AND USED(lcCursorChk)
         SELECT(lcCursorChk) 
         LOCATE
        IF !EOF()
          llSelectCheck = .T.
        ENDIF 
      ENDIF 
    ENDIF 
  
    IF !llSelectCheck 
    *** ð cannot be empty.
      =gfModalGen("INM04074B00000","DIALOG",'Reprint check number')
      RETURN .F.
  ENDIF 
  
  CASE EMPTY(lcRpBnkCod)            
    *** ð cannot be empty   
    =gfModalGen("INM04074B00000","DIALOG",'Bank code')
    RETURN .F.
      
  CASE !gfSEEK(lcRpBnkCod,'APBANKS')   
    *** ð not found.
    =gfModalGen("INM04002B00000","DIALOG",'Bank code')
    RETURN .F.

  CASE EMPTY(lcRpChkAct)                   
    *** ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Checking account')
    RETURN .F.

  CASE !gfSEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct),'APCHECKS')   
    *** ð account not valid.
    =gfModalGen("INM04077B00000","DIALOG",'Checking')
    RETURN .F.

  CASE INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn  AND !(TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler)  
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. !lfVlDate(oAriaApplication.PrntCompanyID ,@lcFisPrd,@lcFisYear,ldChkDat)
    *** Check date is not valid.
    =gfModalGen("INM04052B00000","DIALOG")
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. EMPTY(STRTRAN(STRTRAN(lcRpGlAcct,'-'),'0')) 
    *** ð cannot be empty..
    =gfModalGen("INM04074B00000","DIALOG",'G/L account')
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. ! lfValidAcct(lcRpGlAcct)  
    *** ð not found.
    =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(lcRpGlAcct))
    RETURN .F.
ENDCASE

IF lcPrintMod = 'A'   

  llSelectVendor = .F.
  lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
  IF lnPosVend> 0 
    lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
    lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
    IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
      SELECT(lcCursorVend)
      LOCATE
      IF !EOF()
        llSelectVendor= .T.
      ENDIF 
    ENDIF 
  ENDIF 


  IF !llSelectVendor
    *** ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Vendor code')
    RETURN .F.
  ELSE
    SELECT (lcCursorVend)
    SCAN 
      IF gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
        IF APVENDOR.CVENPRIOR = '0'
          *** Vendor ð has payment priority zero.  This vendor is on hold.
          =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(&lcCursorVend..KeyExp))
          RETURN .F.
        ENDIF
      ELSE
        *** ð not found.
        =gfModalGen("INM04002B00000","DIALOG",'Vendor code')
        RETURN .F.
      ENDIF
    ENDSCAN   
  ENDIF  
ENDIF

IF lcPrintMod = 'V'   
  lcRpVenCod = laOGFxFlt[1,6]
  
  llSelectVendor = .F.
  lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
  IF lnPosVend> 0 
    lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
    lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
    IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
      SELECT(lcCursorVend)
      LOCATE
      IF !EOF()
        llSelectVendor= .T.
      ENDIF 
    ENDIF 
  ENDIF 
  
  
  IF llSelectVendor
    SELECT(lcCursorVend)
    SCAN 
      =gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
      IF APVENDOR.CVENPRIOR = '0'
        *** Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(&lcCursorVend..KeyExp))
        RETURN .F.
      ENDIF
    ENDSCAN    
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfvRemit
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Remit to Address
*!*************************************************************************
*
FUNCTION lfvRemit
PARAMETERS loFormSet
lcInvRemit  = loFormSet.ariaForm1.cboInvRemit.Value
lcFactor  = loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  
lcFactor   = IIF(EMPTY(lcFactor), APVENDOR.cFacCode, lcFactor)
loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  = lcFactor   

lcInvRemit = gfRemit(lcInvRemit, .T. , lcRpVenCod, lcFactor,;
                @lcRemitStat, 'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4', 'lcRem5', 'lcRem6',;
                2, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
lcFactor   = IIF(lcInvRemit = 'F', lcFactor, SPACE(6))
lcFactStat = IIF(lcInvRemit = 'F', 'ENABLE', 'DISABLE')
loFormSet.AriaForm1.kbFacCode.ENABLED = (lcFactStat='ENABLE')
loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  = lcFactor  
loFormSet.ariaForm1.txtOutComp.Value = lcRem1       
loFormSet.ariaForm1.txtOutAddr1.Value = lcRem2       
loFormSet.ariaForm1.txtOutAddr2.Value = lcRem3       
loFormSet.ariaForm1.txtOutAddr3.Value = lcRem4       
loFormSet.ariaForm1.txtOutAddr4.Value = lcRem5       
loFormSet.ariaForm1.txtOutAddr5.Value = lcRem6

*!*************************************************************
*! Name      : lfvFactor
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Valid function for get field lcFactor
*!*************************************************************************
* 

FUNCTION lfvFactor
PARAMETERS loFormSet
lcFactor  = loFormSet.AriaForm1.kbFacCode.KeyTextBox.value  
llBrowse  = loFormSet.AriaForm1.kbFacCode.Selectedfrombrowse
lcOldVal = loFormSet.AriaForm1.kbFacCode.KeyTextBox.Oldvalue  
IF llBrowse .OR. !EMPTY(lcFactor) 
  IF lfGetFac(lcOldVal, llBrowse)
    lcRem1      = SYCFACT.cFacComp
    lcRem2      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,1)
    lcRem3      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,2)
    lcRem4      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,3)
    lcRem5      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,4)     
    lcRem6      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,5)
  ENDIF  
ELSE
  STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4, lcRem5, lcRem6
ENDIF   
llBrowse = .F.
loFormSet.ariaForm1.txtOutComp.Value = lcRem1       
loFormSet.ariaForm1.txtOutAddr1.Value = lcRem2       
loFormSet.ariaForm1.txtOutAddr2.Value = lcRem3       
loFormSet.ariaForm1.txtOutAddr3.Value = lcRem4       
loFormSet.ariaForm1.txtOutAddr4.Value = lcRem5       
loFormSet.ariaForm1.txtOutAddr5.Value = lcRem6

*!*************************************************************
*! Name      : lfClearRep   
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Clear report
*!*******************************************************************
*
FUNCTION lfClearRep   

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  ERASE(oAriaApplication.WorkDir+lcRpTargt+'DBF')
  ERASE(oAriaApplication.WorkDir+lcRpTargt+'FPT')
ENDIF

glEscPrs = .F.

*!*************************************************************
*! Name      : lfBeforeRev 
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Check  that user entered all required info.
*!*******************************************************************
* 
FUNCTION lfBeforeRev 
IF !llTestChk
  IF lcPrintMod = 'A'
    RETURN lfReValidate() .AND. lfAdvance()
  ELSE
    RETURN lfReValidate() 
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfAdvance
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Advance payment function
*!*******************************************************************
* 
* 
FUNCTION lfAdvance

llSelectVendor = .F.
lnPosVend = ASCAN(loOgScroll.laOgFXFlt,"APINVHDR.CVENDCODE")
IF lnPosVend> 0 
  lnPosVend= ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosVend,1)
  lcCursorVend= loOgScroll.laOgFxFlt[lnPosVend,6]
  IF !EMPTY(lcCursorVend) AND USED(lcCursorVend)
    SELECT(lcCursorVend)
    LOCATE
    IF !EOF()
      llSelectVendor= .T.
    ENDIF 
  ENDIF 
ENDIF 

DIMENSION laFlStru[16,4]
laFlStru[1,1] = 'cVendCode'
laFlStru[1,2] = 'C'
laFlStru[1,3] = 8
laFlStru[1,4] = 0
laFlStru[2,1] = 'cDebMemN'
laFlStru[2,2] = 'C'
laFlStru[2,3] = 12
laFlStru[2,4] = 0
laFlStru[3,1] = 'CDIVISION'
laFlStru[3,2] = 'C'
laFlStru[3,3] = 6
laFlStru[3,4] = 0
laFlStru[4,1] = 'Reference'
laFlStru[4,2] = 'C'
laFlStru[4,3] = 16
laFlStru[4,4] = 0
laFlStru[5,1] = 'CFactor'
laFlStru[5,2] = 'C'
laFlStru[5,3] = 6
laFlStru[5,4] = 0
laFlStru[6,1] = 'Rem1'
laFlStru[6,2] = 'C'
laFlStru[6,3] = 40
laFlStru[6,4] = 0
laFlStru[7,1] = 'Rem2'
laFlStru[7,2] = 'C'
laFlStru[7,3] = 40
laFlStru[7,4] = 0
laFlStru[8,1] = 'Rem3'
laFlStru[8,2] = 'C'
laFlStru[8,3] = 40
laFlStru[8,4] = 0
laFlStru[9,1] = 'Rem4'
laFlStru[9,2] = 'C'
laFlStru[9,3] = 40
laFlStru[9,4] = 0
laFlStru[10,1] = 'Rem5'
laFlStru[10,2] = 'C'
laFlStru[10,3] = 40
laFlStru[10,4] = 0
laFlStru[11,1] = 'Rem6'
laFlStru[11,2] = 'C'
laFlStru[11,3] = 40
laFlStru[11,4] = 0
laFlStru[12,1] = 'nPaymnt'
laFlStru[12,2] = 'N'
laFlStru[12,3] = 12  
laFlStru[12,4] = 2
laFlStru[13,1] = 'n1099Amnt'
laFlStru[13,2] = 'N'
laFlStru[13,3] = 15  
laFlStru[13,4] = 2
laFlStru[14,1] = 'cInvRemit '
laFlStru[14,2] = 'C'
laFlStru[14,3] = 1  
laFlStru[14,4] = 0
laFlStru[15,1] = 'cApAcct'
laFlStru[15,2] = 'C'
laFlStru[15,3] = 24 
laFlStru[15,4] = 0
laFlStru[16,1] = 'lOkAdvPay'
laFlStru[16,2] = 'L'
laFlStru[16,3] = 1 
laFlStru[16,4] = 0
=gfCrtTmp(lcDebtCurs,@laFlStru,'cVendCode',lcDebtCurs)  

SELECT(lcCursorVend)
LOCATE 
SCAN 
  lnRecN = RECNO()
  lcRpVenCod = ALLTRIM(&lcCursorVend..KeyExp)
  lcDebMemN  = SPACE(12)        && Debit memo number
  lcAdvDiv   = " "              && var. to holds active division code
  lcRef      = SPACE(16)        && Reference
  lcFactor   = SPACE(6)
  lnPaymnt   = 0                && Payment amount
  ln1099Amnt = 0                && 1099 amount
  lcRem1     = SPACE(40)        && Address 1
  lcRem2     = SPACE(40)        && Address 2
  lcRem3     = SPACE(40)        && Address 3
  lcRem4     = SPACE(40)        && Address 4
  lcRem5     = SPACE(40)        && Address 5
  lcRem6     = SPACE(40)        && Address 6
  lnRemit    = 1
  llOkAdvPay = .F.
  llBrowse   = .F.              && Variable to hold left mouse clicked or not. 
  =gfSEEK(ALLTRIM(&lcCursorVend..KeyExp),'APVENDOR')
  lc1099Stat = IIF(EMPTY(APVENDOR.cVen1099T),'DISABLE','ENABLE')

  DIMENSION  laRemitTo[3,2]

  *** Prepare Remit to array from SYDFIELD and get its maximum width
  lcFactStat   = 'DISABLE'
  lcRemitStat  = 'DISABLE'

  lnRemitLen   = gfGetVld('cInvRemit',@laRemitTo)
  lcRemitTo    = laRemitTo[1,1]   
  lcInvRemit   = laRemitTo[1,2]   
  lcRem1       = APVENDOR.cVenComp

  lcRem2       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,1)
  lcRem3       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,2)
  lcRem4       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,3)
  lcRem5       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,4)
  lcRem6       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,5)

  lcAdvDiv    = APVENDOR.cDivision
  IF gfSEEK('N'+APVENDOR.cDivision + 'N' + 'CDIVISION')
    lcAdvDiv    = APVENDOR.cDivision
  ENDIF

  IF !EMPTY(APVENDOR.cApAcct)
    lcApAcct   = APVENDOR.cApAcct
  ELSE
    IF !EMPTY(lcAdvDiv)  
      =gfSEEK(lcAdvDiv,'APDIV')
      IF EMPTY(APDIV.cApAcct)
        lcApAcct = APSETUP.CAPACCT    
      ELSE
        lcApAcct = APDIV.cApAcct
      ENDIF  
    Else
      lcApAcct   = APSETUP.CAPACCT       
    ENDIF
  ENDIF    
  DO FORM (oAriaApplication.ReportHome  + oAriaApplication.ActiveModuleID+ '\APCKADVP.SCX')   && Run the advance payment screen 
  IF llOkAdvPay
    SELECT (lcDebtCurs)
    APPEND BLANK 
    REPLACE cVendCode WITH lcRpVenCod ,;
            cDebMemN  WITH lcDebMemN  ,;
            CDIVISION WITH lcAdvDiv    ,;
            CFactor   WITH lcFactor   ,;
            Reference WITH lcRef      ,;
            Rem1      WITH lcRem1     ,;
            Rem2      WITH lcRem2     ,;
            Rem3      WITH lcRem3     ,;
            Rem4      WITH lcRem4     ,;
            Rem5      WITH lcRem5     ,;
            Rem6      WITH lcRem6     ,;
            lOkAdvPay WITH llOkAdvPay ,;
            cApAcct   WITH lcApAcct   ,;
            nPaymnt   WITH lnPaymnt   ,; 
            n1099Amnt with ln1099Amnt ,;
            cInvRemit WITH lcInvRemit    
  ENDIF     
  SELECT(lcCursorVend)  
  IF BETWEEN(lnRecN,1,RECCOUNT())
    GO RECORD lnRecN 
  ENDIF     
ENDSCAN 
RETURN llOkAdvPay
*!*************************************************************
*! Name      : lfvPrintMode
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Validate Print Mode
*!*******************************************************************
* 
FUNCTION lfvPrintMode
PARAMETERS llFromWhen
lcSessValue = lcSession
lcApDistOld = lcApDist  
lcDebtCursOld = lcDebtCurs               
IF lcPrintMod <> lcOldMode
  lcOldMode = lcPrintMod
  lcRpChkMod = lcPrintMod

  STORE '' TO loogScroll.aRepVrFlt, loogScroll.laOGFxFlt,loogScroll.laOGVRFlt,loogScroll.laOGHDFlt
  loogScroll.lcOGPrgName = 'APCHKPR'+lcPrintMod
  lnRemoteRslt = loogScroll.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27' and cRep_ID='"+PADR(ALLTRIM(loogScroll.lcOGPrgName),loogScroll.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
  IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
    RETURN .F.
  ENDIF 

  SELECT SYDREPRT
  loogScroll.lnRepIDLen = LEN(cRep_ID)
  WITH loogScroll
    STORE PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen) TO .lcOGPrgName, .lcOGRepID, .lcOGManRep
    IF (.lcOGPrgName != .lcOGRepID)  
      USE IN SYDREPRT    
    lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27' and cRep_ID='"+PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen)+"'",;
                                       '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                        3,"",loogScroll.Parent.DataSessionID)    
      IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
      .lcOGPrgName =  .lcOGRepID
        lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where cVer <> 'A27 AND 'cRep_ID='"+PADR(ALLTRIM(This.lcOGPrgName),This.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
      ENDIF 
    ENDIF 
  ENDWITH   
  SELECT SYDREPRT
  LOCATE 
  loogScroll.gcAct_Appl = cApp_ID
  IF !llFromWhen
    lcSession = lcSessValue  
    loogScroll.InitRepHeader()
  ENDIF   
  lcSession = lcSessValue  
  loOGScroll.GetRepVar()
  lcSession = lcSessValue  
  WITH loOGScroll.OuterContainer.InnerContainer
    DO WHILE .ControlCount > 0 AND !('PRINTMOD' $ UPPER(.Controls(.ControlCount).Name))
     .RemoveObject(.Controls(.ControlCount).Name)
    ENDDO 
  ENDWITH
  lcSession = lcSessValue  
  loOGScroll.defineobjects (.T.)
  lcSession = lcSessValue  
  =loOGScroll.RefreshScroll()
  loogScroll.lcOGPrgName = 'APCHKPR'+'V'
  loogScroll.lcOGManRep= 'APCHKPR'+'V'
ENDIF 
lcApDist   = lcApDistOld  
lcDebtCurs = lcDebtCursOld  
IF lcPrintMod = 'R'
  IF !USED('APDISTA')
    =gfOpenTable('APDIST','CHECKS','SH','APDISTA')
  ENDIF  
*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [Start]
*!*	  =gfSqlRun("Select * from apdist where "+;
*!*	        " capdtrtyp = 'P' and cApdActID = 'A' AND cApdStat <> 'V'",'APDISTA',.T.,lcApDist)
  =gfSqlRun("Select cstubchk,dApdTrDat,cApdRef,cVendCode,cInvNo,capdtrtyp,cbnkcode,cchkacct   from apdist where "+;
        " capdtrtyp = 'P' and cApdActID = 'A' AND cApdStat <> 'V'",'APDISTA',.T.,lcApDist)

*!N000635,4 MMT 04/08/2010 Convert AP Print Check report to Aria4 [End]  
  SELECT (lcApDist)
  
  CURSORSETPROP("Buffering" ,3)
  INDEX on capdtrtyp+ cbnkcode+cchkacct+ cstubchk TAG (lcApDist) 
ENDIF 
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [START]
*lcRepForm = 'APCHKPWL'
*!N000635,3 MMT 03/10/2010 Convert AP Print Check report to Aria4 [END]
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
=ADIR(laFrxFiles,oAriaApplication.ReportHome+'*.FRX')
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
    ELSE 
        *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[Start]
*!*	      lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')   
*!*	      DIME laFrxFiles[1,4]
*!*	      STORE '' TO laFrxFiles
*!*	      =ADIR(laFrxFiles,lcSrvRpt+'*.FRX')
*!*	      IF !EMPTY(laFrxFiles[1,1])
*!*	        IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__' + oAriaApplication.ActiveCompanyID)+'.FRX')>0 
*!*	          lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + oAriaApplication.ActiveCompanyID)
*!*	        ELSE
*!*	          IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
*!*	            lcRepForm = '__' + SUBSTR(lcRepForm , 3)
*!*	          ENDIF   
*!*	        ENDIF 
*!*	      ENDIF  
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]
    ENDIF
  ENDIF
ENDIF 
loogScroll.CheckPlatForm()

 
DECLARE laRpSource[5],laRpTarget[1]
STORE 'Printed Checks'    TO laRpSource[1] 
STORE ''               TO laRpTarget[1]
STORE 'Manual Checks'     TO laRpSource[2] &&,laRpTarget[2]
STORE 'Non Check Payments'TO laRpSource[3]
STORE 'Credit Card'     TO laRpSource[4]
STORE 'Cash Payments'     TO laRpSource[5]
lcRpStatus = "'P','M','N','C','H'"
IF ASCAN(LAOGOBJTYPE,'LCRPGLACCT') # 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPGLACCT'),1)
  LAOGOBJCNT[LNPOS] = IIF(lcPrintMod = 'V',.F.,.T.)
  = LFOGSHOWGET('LCRPGLACCT')
ENDIF

IF lcPrintMod <> 'R'  
  lcNxtChkNo = IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
ENDIF


IF TYPE('llCallFromRequestHandler') = 'L' .AND. llCallFromRequestHandler
  IF ASCAN(LAOGOBJTYPE,'LCPRINTMOD') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCPRINTMOD'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LCPRINTMOD')
  ENDIF
  
  IF ASCAN(LAOGOBJTYPE,'LNALTEST') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LNALTEST'),1)
    LAOGOBJCNT[LNPOS] = .F.
    = LFOGSHOWGET('LNALTEST')
  ENDIF
ENDIF 


*!*************************************************************
*! Name      : lfDefNxtChk
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Function to Get next check number
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcNxtChkNo")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Next check number
*!*************************************************************
*! Example            :  =lfDefNxtChk()
*!*************************************************************
FUNCTION lfDefNxtChk

=gfOpenTable('APCHECKS','Bankcheck','SH')
RETURN IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'), ;
           PADL(APCHECKS.NChkNxtPn,8,'0'),'00000001')

*!*************************************************************
*! Name      : lfDefGlAcc
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Function to Get Check GL account
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcRpGlAcct")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Check GL account
*!*************************************************************
*! Example            :  =lfDefGlAcc()
*!*************************************************************
FUNCTION lfDefGlAcc
=gfOpenTable('APCHECKS','Bankcheck','SH')
RETURN IIF(gfSEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),APCHECKS.CCHKGLACC,' ')


FUNCTION lfSetAproe


  *!*************************************************************
  *! Name      : lfVDtMsg
  *! Developer : Mariam Mazhar (MMT)
  *! Date      : 10/19/2009
  *! Purpose   : Validation function for date by giving messages.
  *!*************************************************************
  *! Parameters: The company that you want to validate from its periods.
  *!           : Logical parameter to accept the date in a locked period.
  *!*************************************************************
  *! Returns   : True or False
  *!*************************************************************
FUNCTION lfVDtMsg
  PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer


  PRIVATE llLockStat, lcCompYer, lcCompPrd
  PRIVATE lnYear, lcYerPer, lcMessage, lcMessgSel
  PRIVATE lcCurYear, lcCurPrd, llVaildDate

  lcCompYer  = ' '   && Variable to hlod the current year of the company.
  lcCompPrd  = ' '   && Variable to hlod the current period of the company.

  lcCurYear  = ' '
  lcCurPrd   = ' '

  llVaildDate= .F.   && Variable to indicate if the date is valid or not

  lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

  lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)

  llLockPer  = IIF(llLockPer,llLockPer,.F.)

  lnYear     = 0     && Variable to hold the year No.

  llLockStat = .F.   && Variable to hold the lock stat of the period.

  ldDateTVal = IIF(TYPE('ldDateTVal')='O',ldDateTVal.Text1.VALUE,ldDateTVal)

  IF lfVlDate(lcCompany,lcCrPrd,lcCrYer,ldDateTVal)  && Call the date validation function.

    lcCrPrd  = lcCurPrd
    lcCrYer  = lcCurYear
    lcYerPer = lcCurPrd+'-'+lcCurYear


    lnYear   = (INT(VAL(lcCurYear))-INT(VAL(lcCompYer))) + 3


    lnYear   = IIF(lnYear <= 0,1,lnYear)

    IF lnYear = 3
      lnYear  = lnYear + SIGN(VAL(lcCrPrd) - VAL(oAriaApplication.CurrentPeriod))
    ENDIF  && End Period Validation

    DO CASE
      CASE llLockStat


        =gfModalGen("TRM00134B00000","DIALOG",lcYerPer)
        llVaildDate = .F.

      CASE lnYear > 0

        lcMessage  = 'history prior   current  future  '

        lcMessgSel = ALLTRIM(SUBSTR(lcMessage,(lnYear*8)-7,8))


        IF lnYear <> 3
          =gfModalGen("TRM00136B00000","DIALOG",lcMessgSel+"|"+lcYerPer)
        ENDIF

        IF lnYear = 1   && If the year = 1 we are not going to accept.
          llVaildDate = .F.
        ELSE
          llVaildDate = .T.
        ENDIF
    ENDCASE
  ELSE

    =gfModalGen("TRM00133B00000","DIALOG")

    llVaildDate = .F.
  ENDIF

  RETURN llVaildDate

  *--end of lfVDtMsg
   *!*************************************************************
  *! Name      : lfVlDate
  *! Developer : Mariam Mazhar
  *! Date      : 10/19/2009
  *! Purpose   : To Validate dates from the periods file.
  *!*************************************************************
  *! Parameters: The company that you want to validate from its periods.
  *!           : Logical parameter to accept the date in a locked period.
  *!*************************************************************
  *! Returns   : True or False
  *!*************************************************************
FUNCTION lfVlDate
  PARAMETERS lcCompany, lcCrPrd, lcCrYer, ldDateTVal, llLockPer
  LOCAL ap1

  PRIVATE llOpenPrd, lcSavOrdPr, llValidDate, lcExactStat

  IF TYPE('lcCurYear') <> 'C' .AND. TYPE('lcCurPrd') <> 'C'
    PRIVATE lcCurYear, lcCurPrd, lLockStat

    lcCrYer    = IIF(TYPE('lcCrYer') <> 'C',' ',lcCrYer)

    lcCrPrd    = IIF(TYPE('lcCrPrd') <> 'C',' ',lcCrPrd)
  ENDIF

  llLockPer  = IIF(llLockPer,llLockPer,.F.)

  llValidDate= .F.      && Variable to return with from the function.

  lcCurYear  = ' '      && Variable to hold the current year.
  lcCurPrd   = ' '      && Varibale to hold the current period.
  lcExactStat= ' '      && Variable to hold the SET EXACT status.
  lLockStat  = .F.      && Variable to hold the lock stat of the record.

  lcSavSelct = ALIAS()  && Variable to save the currently selected file.

  llOpenPrd  = .F.      && Variable to indicate that the there is a file;
    OPEN BY the FUNCTION.

  ap1 = CREATEOBJECT("ap")

  lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)

  lcCompYer  = oAriaApplication.CurrentYear
  lcCompPrd  = oAriaApplication.CurrentPeriod



  lcCompany  = IIF(TYPE('lcCompany') <> 'C',APSETUP.CAPSGLCOM,lcCompany)


  lcCompYer  = oAriaApplication.CurrentYear
  lcCompPrd  = oAriaApplication.CurrentPeriod

  IF !USED('FSPRD')   && Check if the period file is open or not.
    llOpenPrd = .T.      && Indicates that the file is open by the function.
    SELECT 0             && Select an empty work area.
    lcdatadir = ap1.lcDataDir
    
    =gfOpenTable('fsprd','COMFYRPRDI','SH')
  ELSE
    SELECT FSPRD      && The file is open so we are going to select
    lcSavOrdPr = ORDER() && Save the file order
    =gfSetOrder('COMFYRPRDI')
  ENDIF

  IF TYPE('ldDateTVal') <> 'D'
    ldDate = IIF(TYPE('_screen.ActiveForm.ActiveControl.Value')='D',_SCREEN.ACTIVEFORM.ACTIVECONTROL.VALUE,{})
  ELSE
    ldDate = ldDateTVal
  ENDIF

  lcExactStat = SET('EXACT')
  SET EXACT OFF
  GO TOP

  SET EXACT &lcExactStat


  LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
  IF FOUND() .AND. BETWEEN(ldDate,ap1.ldPyBgDate,ap1.ldNyEnDate)

    llLockStat = lFspLocks  && Assign the variable with the period lock stat.
    lcCurYear  = cFisFYear  && Assign the variable with fiscal year of the period.
    lcCurPrd   = cFspprdid  && Assign the variable with the period no.
    lcCrYer    = cFisFYear  && Assign the variable with fiscal year of the period.
    lcCrPrd    = cFspprdid  && Assign the variable with the period no.
    llLockPer  = lFspLocks  && Assign the variable with the period lock stat.
    llValidDate= .T.        && Assign the variable with .T.

    IF USED('FSPRD') .AND. llOpenPrd
      llOpenPrd = .F.
      =gfCloseTable('FSPRD')
    ELSE
      SELECT FSPRD
      =gfSetOrder(lcSavOrdPr)
    ENDIF

    IF !EMPTY(lcSavSelct)
      SELECT(lcSavSelct)
    ENDIF
  ELSE
    IF USED('FSPRD') .AND. llOpenPrd
      llOpenPrd = .F.
      =gfCloseTable('FSPRD')
    ELSE
      SELECT FSPRD
      =gfSetOrder(lcSavOrdPr)
    ENDIF

    IF !EMPTY(lcSavSelct)
      SELECT(lcSavSelct)
    ENDIF
  ENDIF

  RETURN llValidDate

  *--end of lfVlDate
*!*************************************************************
  *! Name      : lfGetGLPIC
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Get GL format
  *!*************************************************************  
FUNCTION lfGetGLPIC
  IF !USED('APSETUP')
    =gfOpenTable('APSETUP','APSETUP','SH')
    SELECT 'APSETUP'
    gfSeek('')
  ENDIF 

  IF !USED('ACCOD')
  =gfOpenTable('ACCOD','ACCSEGNO','SH')
  SELECT 'ACCOD'
  gfSeek('')
ENDIF 
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
RETURN lcApsAcMas 
*!*************************************************************
  *! Name      : GetRpField
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Get Selected Fields
  *!*************************************************************
*
FUNCTION GetRpField
PRIVATE lcOGSelected,lcOGFiles,lnOGCount
lcOGSelected=SELECT()
SELECT SYDREPRT 
LOCATE FOR cRep_id= lcOGPrgName
IF FOUND()
  IF !EMPTY(mRepField)
    RESTORE FROM MEMO mRepField ADDITIVE 
    lcOGFiles=''
    FOR lnOGCount=1 TO ALEN(laselfield,1)
      lcOGFiles=lcOGFiles+IIF(lnOGCount>1,',','')+;
      laselfield[lnOGCount,1]
      IF TYPE('laselfield[lnOGCount,2]')='C'
        lcOGFiles=lcOGFiles+IIF(!EMPTY(laselfield[lnOGCount,2]),;
               ' AS "'+ALLTRIM(laselfield[lnOGCount,2])+'"','')
      ENDIF         
    ENDFOR
  ENDIF
ELSE
  lcOGFiles=''
ENDIF
SELECT(lcOGSelected)
RETURN lcOGFiles
*!*************************************************************
*! Name      : lfGetFac
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Global Function used by the validation function of the Factor Control.
*!*************************************************************
*! Parameters: lcOldVal, llBrowse
*!*************************************************************
*! Returns   : llFactFound
*!*************************************************************
FUNCTION lfGetFac
  PARAMETERS lcOldVal, llBrowse
  PRIVATE lcPrFactor, loFactObj, lcCurAlias, lnSavFacTg, lnClosRec,;
    llFactFound, llOpenFact, lcFile_Ttl, lcBrFields

  loFactObj    = IIF(TYPE('_Screen.ActiveForm.ActiveControl.value')='C', ;
    _SCREEN.ACTIVEFORM.ACTIVECONTROL, ;
    _SCREEN.ACTIVEFORM.ACTIVECONTROL.PARENT.KeyTextBox)
  lcPrFactor     = ALLTRIM(loFactObj.VALUE)
  llFactFound  = .F.
  IF llBrowse .OR. !EMPTY(lcPrFactor)
    lcCurAlias = ALIAS()
    IF !USED('SYCFACT')  && Check if the factors file is open or not.
      llOpenFact  = .T.     && Indicates that the file is open by the function.
      SELECT 0
      gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH')
    ELSE
      llOpenFact = .F.
      SELECT SYCFACT
      lnSavFacTg = ORDER()
      =gfSetOrder('cFacCode')
    ENDIF
    lcPrFactor   = PADR(ALLTRIM(lcPrFactor), FSIZE('cFacCode','SYCFACT'))
    loFactObj.VALUE = lcPrFactor
    IF llBrowse .OR. !gfSEEK(lcPrFactor,'SYCFACT')
      DIMENSION laTemp[1]
      laTemp = ''
      lcFile_Ttl = 'Factors'
      lcBrFields = ' '
      lnClosRec = RECNO(0)
      =GFGETBRF(@lcFile_Ttl, @lcBrFields, 'SYCFACT',;  &&lower
      "cFacCode,cFaccomp,cPhoneNo,cFaxNo,cFacTitle,cFacCont")
      IF BETWEEN(lnClosRec,1,RECCOUNT())
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF

      =gfBrows(.F.,'cFacCode','laTemp')
      IF !EMPTY(laTemp[1])
        loFactObj.VALUE = laTemp[1]
        llFactFound = .T.
      ELSE
        loFactObj.VALUE = loFactObj.OldValue
        llFactFound = .F.
      ENDIF
    ELSE
      llFactFound = .T.
    ENDIF

    IF USED('SYCFACT')
      IF llOpenFact
        =gfCloseTable('SYCFACT')
      ELSE
        SELECT SYCFACT
        =gfSetOrder(lnSavFacTg)
      ENDIF
    ENDIF
    SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  ENDIF
  RETURN llFactFound

  *!*************************************************************
  *! Name      : gfRemit
  *! Developer : Ahmad Shoukry Mohammed (ASM)
  *! Date      : 7/29/2004
  *! Purpose   : Global Function.
  *!*************************************************************
  *! Parameters: Many
  *!*************************************************************
  *! Returns   : lcRmtToCode
  *!*************************************************************
FUNCTION gfRemit
  PARAMETERS lcRmtToCode, llActPopup, lcVendCode, lcFactCode, lcRemitStat,;
    lcRemitCmp, lcRemitAd1, lcRemitAd2, lcRemitAd3,lcRemitAd4,lcRemitAd5, lnRow, lnCol,;
    lcArrName, lcDispStr, lnPopWdth
  *** lcRmtToCode : normally ('V', 'F', 'O')
  *** llActPopup  : .T. if the popup is to be activated
  *** lcVendCode  : vendor code
  *** lcFactCode  : factor code
  *** lcRemitStat : remit objects display status
  *** lcRemitCmp  : remit company object name
  *** lcRemitAd1  : remit address1 object name
  *** lcRemitAd2  : remit address2 object name
  *** lcRemitAd3  : remit address(3+4+5) object name
  *** lnRow       : DOS popup top left row
  *** lnCol       : DOS popup top left column
  *** lcArrName   : remit to array name
  *** lcDispStr   : display string of the popup
  *** lnPopWdth   : dos popup width
  *** example call from APPYINV.PRG
  *** laData[5]    = lfRemit(laData[5], !llJustShow, laData[1], laData[36],;
  ***              @lcRemitStat, 'laData[6]', 'laData[7]', 'laData[8]', 'laData[9]',;
  ***              6, 39, 'laRemitTo', @lcRemitTo, lnRemitLen)
  PRIVATE lcRemitFil, lcRemitTag, lcKeyCode
  lcRmtToCode = UPPER(ALLTRIM(lcRmtToCode))


  lcKeyCode = IIF(lcRmtToCode = 'V', lcVendCode, lcFactCode)
  STORE .F. TO llOpVen, llOpFact

  IF lcRmtToCode = 'V'
    lcRemitFil = 'APVENDOR_A'
    lcRemitTag = 'VENCODE'
    llOpVen    = gfOpenTable('APVENDOR','VENCODE','SH','APVENDOR_A')  &&lower
  ELSE
    lcRemitFil = 'SYCFACT_A'
    lcRemitTag = 'CFACCODE'
    llOpFact   = gfOpenTable(oAriaApplication.SysPath+'SYCFACT','CFACCODE','SH','SYCFACT_A')  &&lower
  ENDIF

  DO CASE
    CASE lcRmtToCode $ 'VF'
      
      IF gfSEEK(lcKeyCode, lcRemitFil)
        &lcRemitCmp  = PADR(IIF(lcRmtToCode = 'V',;
        APVENDOR_A.cVenComp, SYCFACT_A.cFacComp), 40)
        &lcRemitAd1 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,1)
        &lcRemitAd2 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,2)
        &lcRemitAd3 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,3)
        &lcRemitAd4 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,4)
        &lcRemitAd5 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,5)
      ELSE
        STORE SPACE(40) TO &lcRemitCmp, &lcRemitAd1,;
          (lcRemitAd2), (lcRemitAd3),;
          (lcRemitAd4), (lcRemitAd5)
      ENDIF
      lcRemitStat= 'DISABLE'
    OTHERWISE
      lcRemitStat= IIF(loFormSet.ActiveMode="V",'DISABLE','ENABLE')
  ENDCASE
  IF USED('APVENDOR_A') .AND. llOpVen
    =gfCloseTable('APVENDOR_A')
  ENDIF
  IF USED('SYCFACT_A') .AND. llOpFact
    =gfCloseTable('SYCFACT_A')
  ENDIF
  RETURN lcRmtToCode
  *--end of gfRemit
*!N000635,2 MMT 01/04/2009 Convert AP Print Check report to Aria4 [Start]
*!*************************************************************
  *! Name      : lfApAcs
  *! Developer : Mariam MAzhar
  *! Date      : 10/19/2009
  *! Purpose   : Validate GL Account
  *!*************************************************************
FUNCTION lfApAcs
PARAMETERS lcAcsDes,llBrowse, llValidAcs

llApGlLink = .F.
SELECT APSETUP
=gfSeek('')
*** This line is very important to refresh the station buffer with new 
*** information from the disk
GO TOP
IF !EOF()
  GO 1
  llApGlLink = (CAPSGLLINK = 'Y')
ENDIF  

** If the parameter is undefined.
lcAcsDes    = IIF(TYPE('lcAcsDes') $ 'UL','',lcAcsDes)
lcSavAlias  = ALIAS()  && Variable to save the selected alias.
llBrowse    = IIF(TYPE('llBrowse') $ 'U',.F.,llBrowse)
llValidAcs  = .T.

lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
lcOldVal = lcCurVar.oldvalue
lnApsAcLen = 0
lcPic = lfGetGLPIC()

lcFieldCont = lcCurVar  
*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

IF llApGlLink .AND. (llBrowse .OR. lcFieldCont <> lcEmptyAcs)

  SELECT('lcLinkChar')
  gfSetOrder('ACCTCODE')
  =gfSeek('')
  LOCATE 
  IF llBrowse .OR. !gfSEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
    llBrowse = .F. 
    DIMENSION laTemp[2]
    laTemp = ''
    lcSavBrFld=lcBrfields
    lcSavTitle=lcFile_Ttl

    lcBrfields="CACCTCODE :H= 'Account Code',;
                CACCNLDES :H= 'Account Description'"

    lcFile_Ttl="Chart of accounts"
        
    =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp',lcFile_Ttl)

    lcFile_Ttl=lcSavTitle
    lcBrfields=lcSavBrFld

    IF !EMPTY(laTemp[1])
      lcFieldCont = ALLTRIM(laTemp[1])
      lcAcsDes    = ALLTRIM(laTemp[2])
    ELSE
      lcFieldCont = REPLICATE('0',lnApsAcLen)  
      lcAcsDes    = ' '
      llValidAcs  = .F.
    ENDIF
  ELSE
    lcAcsDes      = ALLTRIM(CACCNLDES)
  ENDIF  

  IF !EOF() AND cSegActiv = "I"
    =gfModalGen("TRM04202B00000","DIALOG",ALLTRIM(lcFieldCont))
    lcFieldCont = REPLICATE('0',lnApsAcLen)  
    lcAcsDes    = ' '
    llValidAcs  = .F.
  ENDIF

  IF !EMPTY(lcSavAlias)
    SELECT(lcSavAlias)
  ENDIF  
  LCRPGLACCT  = lcFieldCont
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  

RETURN llValidAcs

*!*************************************************************
*! Name      : lfvPayMeth
*! Developer : Mariam MAzhar
*! Date      : 10/19/2009
*! Purpose   : Validate Payment Method
*!*************************************************************
FUNCTION lfvPayMeth
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpVenPay
= lfOGMover(@laRpSource,@laRpTarget,'Select Customer Status',.T.,'')  && call mover function.
lcRpVenPay = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpVenPay= lcRpVenPay+ IIF(laRpTarget[lnI] = 'Printed Checks',"'P'",;
                            IIF(laRpTarget[lnI] = 'Manual Checks',",'M'",;
                            IIF(laRpTarget[lnI] = 'Non Check Payments',",'N'",;
                            IIF(laRpTarget[lnI] = 'Credit Card',",'C'",IIF(laRpTarget[lnI] = 'Cash Payments' ,",'H'",'')))))

ENDFOR  && end Loop to make Status expression.
lcRpVenPay= IIF(EMPTY(lcRpVenPay),lcRpVenPay,ALLTRIM(lcRpVenPay))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpVenPay) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpVenPay)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- End of lfvOStatus.
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Return the selected Payment Method in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*!*************************************************************
*! Name      : lfGetBnkCode
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : get bank code
*!*************************************************************
FUNCTION lfGetBnkCode

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

RETURN APSETUP.cBnkCode 

*!*************************************************************
*! Name      : lfGetChkAcc
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Return the bank check account
*!*************************************************************
FUNCTION lfGetChkAcc

IF !USED('APSETUP')
  =gfOpenTable('APSETUP','APSETUP','SH')
  SELECT 'APSETUP'
  gfSeek('')
ENDIF 

RETURN APSETUP.cChkAcct 
*!*************************************************************
*! Name      : LFSRVCODE 
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 10/19/2009
*! Purpose   : Check Vendor Priority
*!*************************************************************
FUNCTION LFSRVCODE 
PARAMETERS lcParams

IF CVENPRIOR = '0'
  *** Vendor ð has payment priority zero.  This vendor is on hold.
  =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(CVENDCODE))
  RETURN .F.
ENDIF 
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
  *!N000635,8 MMT 10/11/2011 Fix bugs reported by testing[END]