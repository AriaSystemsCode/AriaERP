*:***************************************************************************
*: Program file  : ARSJORIL.PRG
*: Program desc. : Custom Sales Journal Report for Reliq
*: For Report    : (ARSJORIL.FRX)
*: System        : Aria 4xp.
*: Module        : Account Receivable (AR)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfwRepWhen,lfFormName,lfvOptMsg,lfwOldVal,lfClearRep,
*:               : lfCollData,lfCollTime,lfFltState,lfvSortBy,
*:               : lfvList,lfvCurr,lfStitle,lfvCurDisp,lfvAcc,lfvLoc,
*:               : lfvRepCode,lfGetOpExp,lfMakeOpt,lfvChFact,lfvStates,lfUserChTg.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : 1- All IF llFrTime Blocks executed one time in the option grid seasson.
*:         :    and if user selection opens any temp. files that may be used later
*:         :    in another selection I take this file open to use untill user choice
*:         :    is to press < Close > button, to save more time. 
*:         : 2- Any variable start by (llCh) means that some thing in 
*:         :    selected critria was changed, you must collect data again.
*:***************************************************************************
*: Example : DO ARSJORIL
*:***************************************************************************
*: This Report Program is due to CP121693 ...
*: Converted to Aria4 due to C200722 - T20060927.0067 
*:***************************************************************************

lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]

lnInvPos = lfItmPos('INVHDR.INVDATE')
lnPostDate= lfItmPos('DPOSTDATE')

STORE {  /  /  } TO ldStrtDate , ldEndDate
STORE {  /  /  } TO ldStrtDPst , ldEndDPst

IF !EMPTY(laOGFxFlt[lnPostDate,6])

  ldStrtDPst = CTOD(SUBSTR(laOGFxFlt[lnPostDate,6],1, ATC('|',laOGFxFlt[lnPostDate,6])-1))
  ldEndDPst  = CTOD(SUBSTR(laOGFxFlt[lnPostDate,6],   ATC('|',laOGFxFlt[lnPostDate,6])+1))

  lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)) OR (!BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))))]
ELSE
   lcVoidExpr = [llVoidOnly]
ENDIF

IF lcRpDateTP # "P" AND !EMPTY(laOGFxFlt[lnInvPos,6])
  ldStrtDate = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],1, ATC('|',laOGFxFlt[lnInvPos,6])-1))
  ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],   ATC('|',laOGFxFlt[lnInvPos,6])+1))
  lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]   
ELSE
   lcVoidExpr = [llVoidOnly]   
ENDIF

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),;
                 'Invoice Currency : ' + cCurrCode + " , ",'')] +;
              [+ IIF(llRpRepPrn,  "Sales Rep. " + Rep1 +  " " +"Comm. Payable =  " +;
                                 TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + "PST Amount = " +;
                              TRANSFORM(lnPstAmt,"99999999.99")+;
                              SPACE(10) + "HST Amount = " + ;
                              TRANSFORM(lnHstAmt,"99999999.99")," ")]
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
                                TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
                               TRANSFORM(lnPstAmtCr,"99999999.99")+;
                               SPACE(10) + "HST Total = " + ;
                               TRANSFORM(lnHstAmtCr,"99999999.99")," ")]
  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
                              TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
                              TRANSFORM(lnPstAmtGp,"99999999.99")+;
                              SPACE(10) + "HST Total = " + ;
                              TRANSFORM(lnHstAmtGp,"99999999.99")," ")]                                                   
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Grand =" +;
                                TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + "PST Grand  = " +;
                               TRANSFORM(lnPstAmtot,"99999999.99")+;
                               SPACE(10) + "HST Grand = " + ;
                               TRANSFORM(lnHstAmtot,"99999999.99")," ")]     
                               
   lcWorkFile=loOgScroll.gfTempName()                       
   = lfCreatCur(lcWorkFile)

  llFrTime = .F.  && After this time all of your variablrs have been defined,  you do not need to goto any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcWorkFile) OR !USED(lcWorkFile)
  *-- System Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24

  *-- System Setting for report [end]
  = lfCreatCur(lcWorkFile)  && Create work cursor.
ENDIF

*-- If user change report critria, Collect report data. 
*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection, which is hidden filter.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection, which is hidden filter.

IF llClearFn OR llOGFltCh
  llClearFn = .F.
  *-- If the file already have data, clear it.
  IF RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    = lfCreatCur(lcWorkFile)  && Create work cursor again.
    llNoIndex = .T.
  ENDIF  && end If the file already have data, clear it.

  *-- If User Change Index tag due to change sort by.
  IF llNoIndex OR (lcLastTag != lcIndexTg)
    = lfUserChTg()
  ENDIF		&& end if User Change Index tag.  
  = lfCollData()  && Scan around invHdr master file to collect specific critria records.

  *-- Calculate From and To dates.
  *-- If you print Void invoices only.
  IF llVoidOnly
    lcInvDateF = PADR(SUBSTR(laOGFxFlt[2,6],1,ATC('|',laOGFxFlt[2,6])-1),10)
    lcInvDateT = PADL(SUBSTR(laOGFxFlt[2,6],ATC('|',laOGFxFlt[2,6])+1),10)
  ELSE && else Print either Invoices or invoices and void .
    lcInvDateF = PADR(SUBSTR(laOGFxFlt[1,6],1,ATC('|',laOGFxFlt[1,6])-1),10)
    lcInvDateT = PADL(SUBSTR(laOGFxFlt[1,6],ATC('|',laOGFxFlt[1,6])+1),10)
  ENDIF
ELSE  &&  user does not change report critria, print from the same data.
  *-- If User Change Index tag due to change sort by.
  IF lcLastTag != lcIndexTg
    = lfUserChTg()
  ENDIF		&& end if User Change Index tag.  
ENDIF       && end If user change report critria, Collect report data. 

*-- Select Master report file.
SELECT (lcWorkFile)
*-- Relation Section [begin]
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO cWareCode INTO Warehous ADDITIVE
SET RELATION TO Rep1 INTO Salesrep ADDITIVE
SET RELATION TO ccurrcode INTO Syccurr ADDITIVE
*-- Relation Section [end]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 2

loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcRpForm')

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.

*!*************************************************************
*! Name      : lfStitle
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfOpenFile,lfFltState,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Country state title.
*!*************************************************************
*! Example     : = lfStitle()
*!*************************************************************
FUNCTION lfStitle

SET ORDER TO Ccomp_id IN SYCCOMP   && To use it to get state title.
IF !USED('SYCINT')
  = gfOpenFile(OARIAAPPLICATION.SysHome+'SYCINT',OARIAAPPLICATION.SysHome+'Ccontcode','SH')   
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(OARIAAPPLICATION.ACTIVECOMPANYID,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')

llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfFltState,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

*!*************************************************************
*! Name      : lfFormName
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Example     : = lfFormName()
*!*************************************************************
FUNCTION lfFormName
RETURN 'ARSJORIL'
*-- end of lfFormName.

*!*************************************************************
*! Name      : lfUserChTg 
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Change Work file Index Tag.
*!*************************************************************
*! Called From : Report Code section.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUserChTg()
*!*************************************************************
FUNCTION lfUserChTg
SELECT (lcWorkFile)
INDEX ON &lcIndexTg TAG (lcWorkFile)
IF llNoIndex
  llNoIndex = .F.
ELSE  
  lcLastTag = lcIndexTg
ENDIF  
*-- end of lfUserChTg.

*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Make optimized expression then Collecting data
*!           : from InvHdr file into Work file.
*!*************************************************************
*! Called From : Report Code section.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetOpExp
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCollData()
*!*************************************************************
FUNCTION lfCollData

lcFullExp=" .T. "
IF llMultCurr
	lcCURRFile = ''
	lcCURRFile = lfCheckFilter(1, 'INVHDR.CCURRCODE')
	LLCURR   = !EMPTY(lcCURRFile ) AND USED(lcCURRFile ) AND RECCOUNT(lcCURRFile ) > 0
	IF LLCURR   
	  SELECT (lcCURRFile )
	  INDEX ON CCURRCODE TAG (lcCURRFile )
	  lcFullExp=lcFullExp+" AND SEEK(CCURRCODE,'"+lcCURRFile +"')"
	ELSE
	  IF TYPE("lcCURRFile ") = "C" AND USED(lcCURRFile )
	    USE IN (lcCURRFile )
	  ENDIF
	  lcCURRFile = ''
	ENDIF

ENDIF 

*Customer Filter
lcCustFile = ''
lcCustFile = lfCheckFilter(3, 'INVHDR.ACCOUNT')
llAccFltr   = !EMPTY(lcCustFile ) AND USED(lcCustFile ) AND RECCOUNT(lcCustFile ) > 0
IF llAccFltr   
  SELECT (lcCustFile )
  INDEX ON ACCOUNT  TAG (lcCustFile )
  lcFullExp=lcFullExp+" AND SEEK(ACCOUNT,'"+lcCustFile +"')"
ELSE
  IF TYPE("lcCustFile ") = "C" AND USED(lcCustFile )
    USE IN (lcCustFile )
  ENDIF
  lcCustFile = ''
ENDIF

* Check if there is a filter on REGION
lcCurName = lfCheckFilter(3, 'CUSTOMER.REGION')  
lcREG   = loOgScroll.gfTempName()
llREG   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcREG,"CREG")
IF llREG   
  SELECT (lcREG)
  INDEX on CREG TAG (lcREG)
  lcFullExp=lcFullExp+"  AND SEEK(CUSTOMER.REGION,'"+lcREG+"')"
ENDIF


* Check if there is a filter on REGION
lcCurName = lfCheckFilter(3, 'CUSTOMER.CADDRESS4')  
lcSTAT  = loOgScroll.gfTempName()
llSTATE  = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSTAT  ,"CSTAT")
IF llSTATE  
  SELECT (lcSTAT  )
  INDEX on CSTAT TAG (lcSTAT  )
  lcFullExp=lcFullExp+" AND !EMPTY(CUSTOMER.CADDRESS4) AND SEEK(ALLTRIM(CUSTOMER.CADDRESS4),'"+lcSTAT  +"')"
ENDIF


* SalesRep Filter
lcRepFltr= ''
lcRepFltr= lfCheckFilter(3, 'INVHDR.REP1')
llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
IF llRepFltr   
  SELECT (lcRepFltr)
  INDEX ON REPCODE TAG (lcRepFltr)
  lcFullExp=lcFullExp+" AND (SEEK( INVHDR.REP1,'"+lcRepFltr+"') .OR. SEEK( INVHDR.REP2,'"+lcRepFltr+"'))"
ELSE
  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
    USE IN (lcRepFltr)
  ENDIF
  lcRepFltr= ''
ENDIF

*WAREHOUS Filter
IF llMultLoc
	lcWAREFile = ''
	lcWAREFile = lfCheckFilter(3, 'INVHDR.CWARECODE')
	llWAREFltr   = !EMPTY(lcWAREFile ) AND USED(lcWAREFile ) AND RECCOUNT(lcWAREFile ) > 0
	IF llWAREFltr   
	  SELECT (lcWAREFile )
	  INDEX ON CWARECODE TAG (lcWAREFile )
	  lcFullExp=lcFullExp+" AND SEEK(CWARECODE,'"+lcWAREFile +"')"
	ELSE
	  IF TYPE("lcWAREFile ") = "C" AND USED(lcWAREFile )
	    USE IN (lcWAREFile )
	  ENDIF
	  lcWAREFile = ''
	ENDIF
ENDIF 
* Check if there is a filter on DIVISION
lcCurName = lfCheckFilter(3, 'INVHDR.CDIVISION')  
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv   
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  lcFullExp=lcFullExp+" AND SEEK(INVHDR.CDIVISION,'"+lcDiv+"')"
ENDIF

* Check if there is a filter on TERMCODE
lcCurName = lfCheckFilter(3, 'INVHDR.CTERMCODE')  
lcTRM   = loOgScroll.gfTempName()
llTRM   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcTRM   ,"CDTRM")
IF llTRM   
  SELECT (lcTRM   )
  INDEX on CDTRM TAG (lcTRM   )
  lcFullExp=lcFullExp+" AND SEEK(CTERMCODE,'"+lcTRM   +"')"
ENDIF



* Check if there is a filter on CORDERCAT
lcCurName = lfCheckFilter(1, 'ORDHDR.CORDERCAT')  
lcCAT   = loOgScroll.gfTempName()
llCAT   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCAT ,"CCAT")
IF llCAT   
  SELECT (lcCAT)
  INDEX on CCAT TAG (lcCAT)
  lcFullExp=lcFullExp+" AND SEEK(ORDHDR.CORDERCAT,'"+lcCAT+"')"
ENDIF
	
* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(3, 'INVHDR.SEASON')   
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON") 
IF llSea   
  SELECT (lcSea  )
  INDEX on SEASON TAG (lcSea  )
  lcFullExp=lcFullExp+" AND SEEK(INVHDR.SEASON,'"+lcSea+"') "
ENDIF

* FACTORED AND VOIDED 
lcFullExp=lcFullExp+IIF(lcRpType = 'F'," AND !EMPTY(CFACCODE)",IIF(lcRpType = 'N'," AND EMPTY(CFACCODE) ",""))

lcFullExp=lcFullExp+IIF(lcRpList != 'B'," AND STATUS = lcRpList ", "")


IF !EMPTY(laOGFxFlt[lnInvPos,6])
  IF lcRpList = "V"
    lcFullExp=lcFullExp+" AND BETWEEN(VDATE,ldStrtDate,ldEndDate) "
  ELSE   
    IF lcRpList = "B"
      lcFullExp=lcFullExp+" AND ( BETWEEN(INVDATE,ldStrtDate,ldEndDate) OR BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate) )"
    ELSE
      lcFullExp=lcFullExp+" AND BETWEEN(INVDATE,ldStrtDate,ldEndDate) "
    ENDIF   
  ENDIF
ENDIF


IF  !EMPTY(laOGFxFlt[lnPostDate,6])
  IF lcRpList = "V" 
    IF lcRpDateTP = "P" 
      lcFullExp=lcFullExp+" AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) "
    ELSE
      lcFullExp=lcFullExp+" AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst) "
    ENDIF 
  ELSE 
      IF lcRpList = "B" AND lcRpDateTP = "P" 
        lcFullExp=lcFullExp+" AND ( BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst) OR BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDPst,ldEndDPst) )"
      ELSE  
        lcFullExp=lcFullExp+" AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst) "
      ENDIF   
  ENDIF
ENDIF 

SELECT INVHDR
SET ORDER TO  
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO 'O' + Invhdr.order INTO Ordhdr ADDITIVE

SCAN FOR &lcFullExp

  WAIT WINDOW 'Collecting Data for Invoice no : ' + Invoice NOWAIT
  SCATTER MEMVAR MEMO
  m.cTempKey = PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))

  IF lcRpDateTP = "P"
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))
      = lfNegValue() && Negative void values.
   ENDIF
  ELSE
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
      = lfNegValue() && Negative void values.
    ENDIF
  ENDIF

  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN
*-- end of lfCollData
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 65                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal =OGSYS18()      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!*************************************************************
*! Example   : = lfGetRepVr()
*!*************************************************************
FUNCTION lfGetRepVr

llSortGrp = llMultCurr AND (lcRpCurr = 'F') AND (lcRpSortBy !='U')
lcCurrGrp = IIF(llSortGrp,[CCURRCODE],'')

DO CASE
  CASE lcRpSortBy = 'I'  && Sort by invoice
    IF lcRpKind = 'D'
      lcIndexTg  = [INVOICE]    && Index expression.
    ELSE
      lcIndexTg  = [CCURRCODE + INVOICE]    && Index expression.
    ENDIF

    lcSubTitle = [Invoice]    && Sub Title
    lcGroup    = ''           && Report Group
    lcGrpFoot  = ['']         && Group title

  CASE lcRpSortBy = 'A'  && Sort by account
    IF llSortGrp
      lcIndexTg  = [ACCOUNT + CCURRCODE + INVOICE]
    ELSE  
      lcIndexTg  = [ACCOUNT + INVOICE]
    ENDIF
    
    lcSubTitle = [Account]
    lcGroup    = [ACCOUNT]
    lcGrpFoot  = ['Account# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]

  CASE lcRpSortBy = 'L'  && Sort by location
    IF llSortGrp
      lcIndexTg  = [CWARECODE + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Location]
    lcGroup    = [CWARECODE]
    lcGrpFoot  = ['Location# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]

  CASE lcRpSortBy = 'R'  && Sort by primary sales rep.
    IF llSortGrp
      lcIndexTg  = [REP1 + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    ENDIF
    

    lcSubTitle = [Primary Rep]
    lcGroup    = [REP1]
    lcGrpFoot  = ['Sales Rep.# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]

  CASE lcRpSortBy = 'C'  && Sort by country
    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,6) + ACCOUNT + INVOICE]    
    ENDIF  
    
    lcSubTitle = [Country]
    lcGroup    = [LEFT(cTempKey,6)]
    lcGrpFoot  = ['Country# ' + LEFT(cTempKey,6) + " - " + ALLTRIM(CUSTOMER.cAddress6)]

  CASE lcRpSortBy = 'S'  && Sort by state
    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,36) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,36) + ACCOUNT + INVOICE]
    ENDIF
    

    lcSubTitle = SUBSTR(lcSTitle,1,8)
    lcGroup    = [LEFT(cTempKey,36)]
    lcGrpFoot  = ['Country: ' + LEFT(cTempKey,6) + '  ' +;
                  IIF(SEEK(CUSTOMER.CCONT_CODE,'SYCINT'),;
                  ALLTRIM(SYCINT.CPART4LAB) + '# ','') +;
                  ALLTRIM(SUBSTR(cTempKey,7,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,7,30)),'STATE')]

  CASE lcRpSortBy = 'G'  && Sort by region
    IF llSortGrp
      lcIndexTg  = [RIGHT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    ENDIF
    
    lcSubTitle = [Region]
    lcGroup    = [RIGHT(cTempKey,6)]
    lcGrpFoot  = ['Region# ' + RIGHT(cTempKey,6) + '   ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]

  CASE lcRpSortBy = 'D'  && Sort by division
    IF llSortGrp
      lcIndexTg  = [cDIVISION + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    ENDIF
    
    lcSubTitle = [Division]
    lcGroup    = [cDIVISION]
    lcGrpFoot  = ['Division# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]

  CASE lcRpSortBy = 'T'  && Sort by terms
    IF llSortGrp
      lcIndexTg  = [cTermCode + CCURRCODE + ACCOUNT + INVOICE]
    ELSE 
      lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    ENDIF  
    
    lcSubTitle = [P. Terms]
    lcGroup    = [cTermCode]
    lcGrpFoot  = ['Terms# ' + cTermCode + '   ' + gfCodDes(cTermCode,'CTERMCODE')]

  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    lcGroup    = [CCURRCODE]
    lcGrpFoot  = ['Currency# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
    lcCurrGrp  = ''
ENDCASE

IF lcRpSort2 <> 'N'
  PRIVATE lcAddSort , lc2ndTitle
  DO CASE
    CASE lcRpSort2 = 'I'  && Sort by invoice
      lcAddSort  = [INVOICE]
      lc2ndTitle = [Invoice]
    CASE lcRpSort2 = 'A'  && Sort by account
      lcAddSort  = [ACCOUNT]
      lc2ndTitle = [Account]
    CASE lcRpSort2 = 'L'  && Sort by location
      lcAddSort  = [CWARECODE]
      lc2ndTitle = [Location]
    CASE lcRpSort2 = 'R'  && Sort by primary sales rep.
      lcAddSort  = [REP1]
      lc2ndTitle = [Primary Rep]
    CASE lcRpSort2 = 'C'  && Sort by country
      lcAddSort  = [LEFT(cTempKey,6)]
      lc2ndTitle = [Country]
    CASE lcRpSort2 = 'S'  && Sort by state
      lcAddSort  = [LEFT(cTempKey,36)]
      lc2ndTitle = SUBSTR(lcSTitle,1,8)
    CASE lcRpSort2 = 'G'  && Sort by region
      lcAddSort  = [RIGHT(cTempKey,6)]
      lc2ndTitle = [Region]      
    CASE lcRpSort2 = 'D'  && Sort by division
      lcAddSort  = [CDIVISION]
      lc2ndTitle = [Division]
    CASE lcRpSort2 = 'T'  && Sort by terms
      lcAddSort  = [CTERMCODE]
      lc2ndTitle = [P. Terms]
    CASE lcRpSort2 = 'U'  && Sort by currency
      lcAddSort  = [CCURRCODE]
      lc2ndTitle = [Currency]
  ENDCASE
  
  IF lcRpSortBy = 'I'
    lcIndexTg = lcIndexTg + [ + ] + lcAddSort
  ELSE
    lcIndexTg = SUBSTR(lcIndexTg,1,AT('+',lcIndexTg)) + [ ] + lcAddSort + [ + ] + SUBSTR(lcIndexTg,AT('+',lcIndexTg)+1)
  ENDIF  
  lcSubTitle = lcSubTitle + [+] + lc2ndTitle
ENDIF


*!*************************************************************
*! Name      : lfvList
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : 1- Change Invoices/Void Invoices logical variable To recollect data.
*!           : 2- Enable and disable Invoice date or Void date due to user selection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfFltState
*!*************************************************************
*! Called from : Option Grid [Invoices/Void Invoices/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvList()
*!*************************************************************
FUNCTION lfvList

llChInv = .T.  && Logical invoice variable.
llVoidOnly = (lcRpList='V')  && To use it in .FRX 

*-- end of lfvList.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRepCur
*!*************************************************************
*! Called from : Option Grid [Currency Display Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
= lfGetRepVr()
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfvCurr
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Currency Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurr()
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp


lcVar = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())      && Varible to hold the current field value

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CCURRCODE :R :H= 'Currency' , "   +;
               "CCURRSMBL :R :H= 'Symbol' ,"      +;
               "CCURRDESC :R :H= 'Description' ," +;
               "NCURRUNIT :R :H= 'Units' "
  
  lcFile_Ttl = "Currencies ..."
  = gfBrows('','CCURRCODE','laTemp')
    
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvCurr.




*!*************************************************************
*! Name      : lfCollTime
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Create cursor
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : Cursor Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCreatCur()
*!*************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName

DIMENSION laTempStru[1,18]
  laTempStru = ''

  SELECT INVHDR
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 18]

  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make. 
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 42
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  laTempStru[ALEN(laTempStru,1)  ,17] = 0
  laTempStru[ALEN(laTempStru,1)  ,18] = 0
  FOR lnI=7 TO 16
    laTempStru[ALEN(laTempStru,1)  ,lnI] =""
  ENDFOR  
  =gfCrtTmp(lcCurName,@laTempstru,,"",.f.)

*CREATE CURSOR (lcCurName) FROM ARRAY laTempStru
*-- end of lfCreatCur.


*!*************************************************************
*! Name      : lfPreRun
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetRepVr
*!*************************************************************
*! Called from : Report Generator when press <Preview> or <Run>
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!           : 2- While this function has one line of code calls another function
*!           :    I write it for other reasons and for any other to add any 
*!           :    enhancement code.
*!*************************************************************
*! Example   : = lfPreRun()
*!*************************************************************
FUNCTION lfPreRun
= lfGetRepVr()      && Get Report variables such as groups and index.
RETURN .T.
*-- end of lfPreRun.

*-- Fill Sort Arrays.
FUNCTION lfSortDumy
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 8
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDesc[lnArrElmnt,1],laSortVal[lnArrElmnt,1]
laSortDesc[1] = 'Invoice'
laSortDesc[2] = 'Account'
laSortDesc[3] = 'Sales Representative'
laSortDesc[4] = 'Country'
laSortDesc[5] = lcSTitle       && State variable Title
laSortDesc[6] = 'Region'
laSortDesc[7] = 'Division'
laSortDesc[8] = 'Payment Terms'

laSortVal[1] = 'I'
laSortVal[2] = 'A'
laSortVal[3] = 'R'
laSortVal[4] = 'C'
laSortVal[5] = 'S'
laSortVal[6] = 'G'
laSortVal[7] = 'D'
laSortVal[8] = 'T'

IF llMultLoc
  =AINS(laSortDesc,3)
  laSortDesc[3] = 'Location'

  =AINS(laSortVal,3)
  laSortVal[3] = 'L'
ENDIF

IF llMultCurr
  laSortDesc[ALEN(laSortDesc,1)] = 'Currency'
  laSortVal[ALEN(laSortDesc,1)]  = 'U'
ENDIF
*-- end of lfSortDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfNegValue
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : -Ve Void values
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : = lfNegValue()
*!*************************************************************
FUNCTION lfNegValue
PRIVATE lnFldsCnt , lcMemField
lcMemField = ''
lnFldsCnt = 0
FOR lnFldsCnt = 1 TO FCOUNT()
  IF TYPE(FIELD(lnFldsCnt)) = "N"
    lcMemField = "m." + FIELD(lnFldsCnt)
    &lcMemField = -1 * &lcMemField
  ENDIF
ENDFOR
*-- end of lfNegValue.
*!*************************************************************
*! Name      : lfRegion
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : To get the data from the store fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRegion()
*!*************************************************************
FUNCTION lfRegion
PRIVATE lcRegion , lnCUSRec
lcAlias = SELECT(0)
SELECT CUSTOMER
lnCUSRec = EVAL(KEY())

IF SEEK('M'+CUSTOMER.Account)
  lcRegion = PADR(CUSTOMER.REGION,6)
ELSE
  lcRegion = SPACE(6)
ENDIF

=SEEK(lnCUSRec , 'CUSTOMER')

SELECT(lcAlias)
RETURN lcRegion
*--End of lfRegion.

*!**************************************************************************
*! Name      : lfSortDum2
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Fill second sort bu option 
*!**************************************************************************
*! Example   : =lfSortDum2()
*!**************************************************************************

FUNCTION lfSortDum2
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 9
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDes2[lnArrElmnt,1],laSortVal2[lnArrElmnt,1]
laSortDes2[1] = 'None'
laSortDes2[2] = 'Invoice'
laSortDes2[3] = 'Account'
laSortDes2[4] = 'Sales Representative'
laSortDes2[5] = 'Country'
laSortDes2[6] = lcSTitle       && State variable Title
laSortDes2[7] = 'Region'
laSortDes2[8] = 'Division'
laSortDes2[9] = 'Payment Terms'

laSortVal2[1] = 'N'
laSortVal2[2] = 'I'
laSortVal2[3] = 'A'
laSortVal2[4] = 'R'
laSortVal2[5] = 'C'
laSortVal2[6] = 'S'
laSortVal2[7] = 'G'
laSortVal2[8] = 'D'
laSortVal2[9] = 'T'

IF llMultLoc
  =AINS(laSortDes2,4)
  laSortDes2[4] = 'Location'

  =AINS(laSortVal2,4)
  laSortVal2[4] = 'L'
ENDIF

IF llMultCurr
  laSortDes2[ALEN(laSortDes2,1)] = 'Currency'
  laSortVal2[ALEN(laSortDes2,1)]  = 'U'
ENDIF
*-- End of lfSortDum2.

*!**************************************************************************
*! Name      : lfvSort
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Valid function for both 1st , 2nd Sort by option. 
*!**************************************************************************
*! Example   : =lfvSort()
*!**************************************************************************
FUNCTION lfvSort



IF lcRpSortBy = lcRpSort2
  lcRpSort2 = 'N'
  CLEARREAD()
ENDIF
*-- End of lfvSort.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX , lcInvExp , lcVoidExp

lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- Define new Fixed filter array to hold one expression only.
DIMENSION laOGFxFlt[1,7]
laOGFxFlt = ""          

*-- Copy only Month Range expression to laOGFxFlt.
IF lcRpDateTP = "P"
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnPostDate,1),7)
ELSE
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnInvPos,1),7)
ENDIF

*-- Generate expression for Month Range.
lcInvExp = gfGenFlt('laOGFxFlt',.T.,.T.)
lcInvExp = STRTRAN(lcInvExp,"INVHDR.","")

lcVoidExp = "( " + lcInvExp + " OR BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate) )"

*-- Remove Style Group from lcRpExp.
lcRpExp = STRTRAN(lcRpExp,lcInvExp,lcVoidExp)
*-- If user selected Style.

*-- Restore original laOGFxFlt.
DIMENSION laOGFxFlt[1]
=ACOPY(laTempExpr,laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClrRead()
*!*************************************************************
FUNCTION lfClrRead
CLEARREAD()
*--End of lfClrRead.

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfStr2Curs 
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString



