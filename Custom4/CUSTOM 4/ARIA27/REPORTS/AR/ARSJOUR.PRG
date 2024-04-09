*:***************************************************************************
*: Program file  : ARSJOUR
*: Program desc. : Sales Journal Report
*: For Report    : (ARSJOUR.FRX)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Mohamed Atia Badran (MAB)
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
*: Example : DO ARSJOUR
*:***************************************************************************
*: This Report Program is due to E300881 ...
*:E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.
*:B130166,1 NNA 10/27/2005 Fix bug that if you have invoices with All seasons (Season Field hold '*')
*:B130166,1 NNA            and then ran this report and select a season you'll never get these invoices
*:B130166,1 NNA            that because it search for season you selected in the invhdr.season that hold '*'
*:B607833,1 TMI 11/15/2006 fix a bug that when a region is selected the report does not work properly (T20061103.0009)
*:***************************************************************************
*:Modifications :
*:B602542,1 Include void invoices amount and ship amount if upper invoice date is less than
*:B602542,1 void invoice date.
*:B802249,1 Increase the width of Merch. discount field
*:B603043,1 Make Filter on transaction date only and  [Begin
*:          Active Only -> InvDate									(+Ve Values)
*:          Void   Only -> VDate									(-Ve Values)
*: Active and Void -> Include both dates range (InvDate and Vdate)	(Sign Rule)
*: Sign Rule  : 1> Active (+Ve)
*:            : 2> Void and 
*:            :  a) Void and invoice dates within range 			(Zero Values) 
*:            :  b) Invoice date within range but void date is out 	(-Ve Values) 
*:            :  c) Void date within range but invoice date is out	(-Ve Values) 
*:B603043,1 Make Filter on transaction date only and  [End
*:B802741,1 RAMY 11/04/1999  When print only the non factored invoices that the report is
*:B802741,1                  printing factored invoices that been voided in the same date range
*:E301353,1 SSE 12/18/1999 Convert Report to Graphics format
*:B603413,1 SSE 03/02/2000 Adjust column width in Merch. Disc. and align grand totals
*:C101713,1 BWA 03/21/2000 A custom process to calculate the shipped amount and the Mrech. Discount.
*:B603532,1 BWA 03/28/2000 Fix this bug in the ARSJOUR.FRX
*:B603532,1                1) The report print black boxes.
*:B603532,1                2) The report print recoreds less then the recoreds view on the screen.
*:B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*:B603656,1                for the customized sales j. for (Fresh Perduce).
*:B803211,1 BWA 06/21/2000 Fix the bug if the date in the report doesn't print all.[FRX]
*:E301432,1 BWA 07/10/2000 Add a new filter to the criteria, which is posting date.
*:B603600,1 BWA 08/07/2000 Fix the following bugs:
*:B603600,1                1)The report gives wrong data when sorting by invoice.
*:B603600,1                2)The report gives there is no record when sorting by currency.
*:B603600,1                3)The report gives error when using the transaction date.
*:B603850,1 BWA 08/27/2000 Fix the bug of printed all factored and non-factored invoices even it selected factored only.
*:B603888,1 BWA 09/12/2000 When running the report to show both active and void invoices and for a specific
*:B603888,1                division - the report prints the void invoices from all divisions.
*:B603907,1 KAM 03/10/2000 1-When running the report to show factored invoice 
*:B603907,1                  non factored void also displayed.
*:B603907,1                2-The factored amount display **** instead of numeric value (changed in FRX).
*:B604326,1 BWA 04/02/2001 Fix the bug of "Period" dates are showing the first two digits of year instead of last two digits, ie, 01/01/20 - 12/31/20.[FIX IN FRX]
*:B604832,1 BWA 08/30/2001 Fix the bug of empty value for the region in case any transaction for stories.
*:B605122,1 SSE 11/22/2001   Fix bug of adding the HST Taxes to the report.
*:B605250,1 SSE 12/20/2001   Fix bug of some void invoices do not appear properly in sales journal. 
*:C102483,1 SSE 12/29/2001   Custom add second sort in Sales Journal Report.
*:B605359,1 BWA 01/15/2002 Fix the bug of changing the posted filter to fixed filter.
*:B606761,1 RAE 01/08/2003 Fix the bug of 'No Records to display' when change the Division filter
*:						   from Advanced option from Like to In List.
*:B607036,1 ABD 03/31/2003 Fix Error Variable LCVOIDEXPR"" Not found.
*:E037351,1 HBG 23/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*:E037351,1                Aria27 and records belong to Aria4.
*:**************************************************************************************************
*
*----------------------- Report Code Begin -----------------------------
lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]

*B602542,1 Include void invoices amount and ship amount if upper invoice date is less than [Begin]
*          void invoice date.
lnInvPos = lfItmPos('INVHDR.INVDATE')
STORE {  /  /  } TO ldStrtDate , ldEndDate

*B605359,1 BWA 01/15/2002 Fill the date variables.[START]
IF lcRpDateTP = "P"
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  *B607036,1 ABD - Fix Error Variable LCVOIDEXPR"" Not found, on case the user select posting
  *B607036,1 ABD - Date and posting date is empty, we should get the value of llVoidOnly, this
  *B607036,1 ABD - Variable Hold true If you print Void invoices only. [Begin]
  *IF !EMPTY(laOGFxFlt[lnPostDate,6])
  IF EMPTY(laOGFxFlt[lnPostDate,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    *B607036,1 ABD - [End]
    ldStrtDPst = CTOD(PADR(laOGFxFlt[lnPostDate,6],ATC('|',laOGFxFlt[lnPostDate,6])+1))
    ldEndDPst  = CTOD(SUBSTR(laOGFxFlt[lnPostDate,6],ATC('|',laOGFxFlt[lnPostDate,6])+1))
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)) OR (!BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))))]
  ENDIF
ENDIF
*B605359,1 BWA 01/15/2002.[END]

*B605359,1 BWA 01/15/2002 Don't get to this code in case posted date.[START]
IF lcRpDateTP # "P"
*B605359,1 BWA 01/15/2002.[END]
  IF EMPTY(laOGFxFlt[lnInvPos,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    ldStrtDate = CTOD(PADR(laOGFxFlt[lnInvPos,6],ATC('|',laOGFxFlt[lnInvPos,6])+1))
    ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],ATC('|',laOGFxFlt[lnInvPos,6])+1))

    *B603043,1 Add Void between to Range.
    *lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND VDATE < ldEndDate)]
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]
  ENDIF

*B605359,1 BWA 01/15/2002 Don't get to this code in case posted date.[START]
ENDIF
*B605359,1 BWA 01/15/2002.[END]

*B602542,1 Include void invoices amount and ship amount if upper invoice date is less than [End  ]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime

  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.
  
  *B605122,1 Display HST Tax amount for Canada. [Begin]
  *lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),;
  *               'Invoice Currency : ' + cCurrCode + " , ",'')] +;
  *            [+ IIF(llRpRepPrn,  "Sales Rep. " + Rep1 +  " " +"Comm. Payable =  " +;
  *                               TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
  *            [+IIF(llCanada, SPACE(10) + "PST Amount = " +;
  *                            TRANSFORM(lnPstAmt,"99999999.99")," ")]                   
  
  *lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
  *                              TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
  *             [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
  *                             TRANSFORM(lnPstAmtCr,"99999999.99")," ")]                   

  *lcGrpExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
  *                            TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
  *            [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
  *                            TRANSFORM(lnPstAmtGp,"99999999.99")," ")]                   
  
  *lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Grand =" +;
  *                              TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
  *             [+IIF(llCanada, SPACE(10) + "PST Grand  = " +;
  *                             TRANSFORM(lnPstAmtot,"99999999.99")," ")]                   
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
  *B605122,1 Display HST Tax amount for Canada. [End]

  *-- Create temporary file that holding order InvHdr data. [begin]
  lcWorkFile = gfTempName()
  DIMENSION laTempStru[1,4]
  laTempStru = ''

  SELECT INVHDR
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]

  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make. 
  laTempStru[ALEN(laTempStru,1)  ,1] = 'cTempKey'
  laTempStru[ALEN(laTempStru,1)  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)  ,3] = 42
  laTempStru[ALEN(laTempStru,1)  ,4] = 0
  
  *-- Create temporary file that holding order InvHdr data. [end]

  llFrTime = .F.  && After this time all of your variablrs have been defined,  you do not need to goto any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcWorkFile) OR !USED(lcWorkFile)

  *-- System Setting for report [begin]
  *lcCentury  = SET('CENTURY')  
  *SET CENTURY OFF

  lcSetHour = SET('HOURS')
  SET HOURS TO 24

  *-- System Setting for report [end]

  = lfCreatCur(lcWorkFile)  && Create work cursor.

ENDIF

*-- If user change report critria, Collect report data. 
*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection, which is hidden filter.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection, which is hidden filter.

*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[Begin
*IF llClearFn OR !(lcRpExp == lcLastExpr) OR ;
*   (llChFactor OR llChInv)
*B607833,1 TMI 11/15/2006 [Start] removr this after the issue is completed
*IF llClearFn OR llOGFltCh
if .t.
*B607833,1 TMI 11/15/2006 [End  ] 

*E301170,1 MAB 03/15/1999 Use variable llOGFltCh that detect OG filter changes.[End

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

  *-- Note that : Optimized expression shape is [ACCOUNT+INVOICE = lcAccVal]
  *-- lcAccOp   : Direct Operator of optimized expression.
  *-- lcAccVal  : Value of optimized expression.
  *-- lcOperator: True Operator of optimized expression.
  STORE '' TO lcAccOp,lcAccVal,lcOperator
  STORE .T. TO llOpLogic  && Logic of operator (if it's .F. its logic is NOT)

  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

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

  STORE .F. TO llChFactor,llChInv    && Unrise all hidden Critria variables.

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

*-- Call Report [lcRpForm = 'ARSJOUR.FRX']
DO gfDispRe WITH EVAL('lcRpForm')

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.
*B130166,1 NNA 10/27/2005 (Begin) Close invline if it opened.
IF USED('INVLINE')
  USE IN INVLINE
ENDIF
*B130166,1 NNA (End)

*----------------------- Report Code End -----------------------------


*-- Function Section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/13/1998
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
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')   
ELSE
  SET ORDER TO Ccontcode IN SYCINT   && To use it to get state title.
ENDIF
= SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')

llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
RETURN (SYCINT.CPART4LAB)
*-- end of lfStitle.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'

  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

  PRIVATE lcThAlias
  lcThAlias = ALIAS()
  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  lnVarbEnd = 0
  SELECT SYREPUVR
  *-- Calculate length of variables appears in option grid.
  *-- and items that we enable and disable.
  = SEEK('ARSJOUR V')
  SCAN REST WHILE cRep_ID+cExpType+STR(nVarPos) = 'ARSJOUR V' FOR nVarPos != 0
    *E037351,1 HBG 23/12/2003 Scan for records belong to Aria27 only [Begin]
    IF !EMPTY(CVER) AND CVER <> 'A27'
      LOOP
    ENDIF
    *E037351,1 [End]
    lnVarbEnd = lnVarbEnd + 1
  ENDSCAN
  *-- Compute Start of variable filter to control its apperance [end]
  SELECT (lcThAlias)

  IF EMPTY(laRpFltVal)
    *-- laRpFltVal : Array to hold D for disable and E for enable, to control
    *--            : Fixed filter appearance in option grid.
    *-- laRpVarNow : Array to hold .T. or .F., to control variables 
    *--            : appearance in option grid. 
    DECLARE laRpFltVal[ALEN(laOGFxFlt,1)],laRpVarNow[lnVarbEnd]
    STORE 'E' TO laRpFltVal  
    STORE .T. TO laRpVarNow

    llTaxes     = gfGetMemVar('M_TAX') = 'Y'  && .T., if company use taxes.

    IF llMultCurr
      SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    ENDIF

  ENDIF  
ENDIF  && END IF you first time enter when function.

*-- Disable/enable InvDate, VDate. [begin]
*-- note that disable and enable is according to value of laRpFltVal.

*B602542,1 Evaluate position using subscript refrence instead of ceiling function.
*lnI = CEILING(ASCAN(laOGFxFlt,'INVHDR.INVDATE')/7)
*lnI = lfItmPos('INVHDR.INVDATE')
*= lfFltState(lnI,laRpFltVal[lnI])  && Disable/Enable invdate.

*B603043,1 No Void date in OG Filter [Begin]
*B602542,1 Evaluate position using subscript refrence instead of ceiling function.
*lnI = CEILING(ASCAN(laOGFxFlt,'INVHDR.VDATE')/7)
*lnI = lfItmPos('INVHDR.VDATE')
*= lfFltState(lnI,laRpFltVal[lnI])  && Disable/Enable vdate.
*-- Disable/enable InvDate, VDate. [end]
*B603043,1 No Void date in OG Filter [End  ]

*E301432,1 BWA 07/26/2000 Check if the array of the variable filter hold the
*E301432,1                posting date due to the link to GL or not.[START]
&& LAOGFIELDN the name of the array in the report generator screen hold the filter options.

*B605359,1 BWA 01/15/2002 Comment this lines due to change the use of the filter to be in the fixed filters.[START]
*IF !EMPTY(LAOGVRFLT) 
*  SHOW GET pbAdvanced ENABLE
*ELSE
*  IF ALEN(LAOGFIELDN) < 2 AND ASCAN(LAOGFIELDN,'DPOSTDATE') = 1 AND ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y'
*    SHOW GET pbAdvanced ENABLE
*  ELSE
*    SHOW GET pbAdvanced DISABLE
*  ENDIF
*ENDIF
*B605359,1 BWA 01/15/2002.[END]

*E301432,1 [END]

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfFltState
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Passed Parameters  : 1- Filter Object Number
*!                    : 2- 'E' for enable, 'D' for disable
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFltState()
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState
IF lcObjState = 'D' AND !EMPTY(laOGFxFlt[lnObjNum,6])
  laOGFxFlt[lnObjNum,6] = ''
ENDIF  
laOGObjCnt[lnObjNum + lnVarbEnd] = (lcObjState = 'E')
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnObjNum)) + ',6]')  && Enable / Disable Object .
*-- end of lfFltState.

*!*************************************************************
*! Name      : lfFormName
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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

RETURN 'ARSJOUR'
*-- end of lfFormName.

*!*************************************************************
*! Name      : lfUserChTg 
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
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
PRIVATE lcOptimize
lcOptimize = ''
=lfGetOpExp()

*-- lcRpExp : Report expression, and I know that it must not be empty
*--         " but I check for empty because any one can remove hidden filter.

lcRpExp = lcOptimize + IIF(EMPTY(lcRpExp),'',[ AND ] + lcRpExp)
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")

*B603043,1 Add void date to filter expression  [Begin]

*B605359,1 BWA 01/15/2002 Check for the posted filter also.[START]
*IF !EMPTY(laOGFxFlt[lnInvPos,6])
IF !EMPTY(laOGFxFlt[lnInvPos,6]) OR IIF(lcRpDateTP = "P" , !EMPTY(laOGFxFlt[lnPostDate,6]) , .F.)
*B605359,1 BWA 01/15/2002.[END]
  
  *-- if Void Only.
  IF lcRpList = "V"
    lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
  ENDIF

  *B605359,1 BWA 01/15/2002 Check for the posted filter also.[START]
  IF lcRpList = "V" AND lcRpDateTP = "P"
    lcRpExp = STRTRAN(lcRpExp,"DPOSTDATE","VDATE")
  ENDIF
  *B605359,1 BWA 01/15/2002.[END]

  *-- if Print Both Active and Void invoices.
  IF lcRpList = "B"
   *:B802741,1 RAMY change the filter expr 'lcRpExp' to set the date range 
   *:B802741,1      in the same level [start]
   *lcRpExp = "(" + lcRpExp + ") OR BETWEEN(VDATE,ldStrtDate,ldEndDate)"
    
   *B603600,1 BWA 08/07/2000 The report gives error when using the transaction date.[START]
   *lcRpExp = SUBSTR(lcRpExp,1,LEN(lcRpExp)-1)
   *lcRpExp = lcRpExp + " OR BETWEEN(VDATE,ldStrtDate,ldEndDate))"
   *B603907,1 KAM 03/10/2000 Add check for date,change or to and in LCRPEXP.   [Start]
   *lcRpExp = lcRpExp + " OR BETWEEN(VDATE,ldStrtDate,ldEndDate)"

   *B605359,1 BWA 01/15/2002 Check for the posted filter also.[START]
   *IF !EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)
    IF (!EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)) OR IIF(lcRpDateTP = "P" , (!EMPTY(ldStrtDPst) OR !EMPTY(ldEndDPst)) , .F.)
   *B605359,1 BWA 01/15/2002.[END]

      *B605250,1 Change AND to OR. [Begin]
      *lcRpExp = lcRpExp + " AND BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate)"
      DO lpGenExpr
      *B605250,1 Change AND to OR. [End]
      
    ENDIF  
    *B603907,1 KAM [End]
    *B603600,1 [END]
    *B802741,1 RAMY [end]
  ENDIF
ENDIF
*B603043,1 Add void date to filter expression  [End  ]

SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
 
*-- cTempKey : Field in work file that contains Country code, plus State code,
*--          : plus region code.
*--          : this field serves in sort by [Country,State,Region]


*B603888,1 When running the report to show both active and void invoices and for a specific
*B603888,1 division - the report prints the void invoices from all divisions.[START]
lcDivsion = ".T."
IF  "V" $ lcRpList OR "B" $ lcRpList
  STORE 0 TO lnDataDiv
  lnDataDiv = ASCAN(laOgVrFlt,'INVHDR.CDIVISION')
  IF lnDataDiv > 0
    lnDataDiv  = ASUBSCRIPT(laOgVrFlt,lnDataDiv,1)
    
    *B606761,1 RAE Fix the bug of 'No Records to display' when change the Division filter
    *               from Advanced option from Like to In List. [start]
    *lcDivsion = IIF(!EMPTY(laOgVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION = laOgVrFlt[lnDataDiv,6]", ".T.")
    lcDivsion = IIF(!EMPTY(laOgVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION $ laOgVrFlt[lnDataDiv,6]", ".T.")
    *B606761,1 RAE [end]
  ENDIF
ENDIF  
*B603888,1 [END]

*B603888,1 When running the report to show both active and void invoices and for a specific
*B603888,1 division - the report prints the void invoices from all divisions.[START]
*SCAN FOR &lcRpExp

*B605359,1 BWA 01/15/2002 Replace the filter in case the posted date.[START]
IF lcRpDateTP = "P"
  lcRpExp = STRTRAN(lcRpExp,"INVDATE","DPOSTDATE")
  lcRpExp = STRTRAN(lcRpExp,"ldStrtDate","ldStrtDPst")
  lcRpExp = STRTRAN(lcRpExp,"ldEndDate","ldEndDPst")
ENDIF
*B605359,1 BWA 01/15/2002.[END]

*B130166,1 NNA 10/27/2005 (Begin) Search for season in the Invline File instead of Invhdr
STORE '' TO lnSeaPos
*--Get the Position of Season
lnSeaPos   = ASCAN(laOGVrFlt,'INVHDR.SEASON')
IF lnSeaPos > 0
  lnSeaPos = ASUBSCRIPT(laOGVrFlt,lnSeaPos,1)
ENDIF
*--If user Select a Season
IF lnSeaPos>0 AND !EMPTY(laOgVrFlt[lnSeaPos,6])
  IF !USED('INVLINE')
    =gfOpenFile(gcDataDir+'INVLINE', gcDataDir+'INVLINE','SH')
  ENDIF
  SELECT INVHDR
  =ACOPY(laOGVrFlt,laSeasnFlt,((lnSeaPos-1)*7)+1,7)
  DIME laSeasnFlt[1,7]
  lcRpExpSea = gfGenFlt('laSeasnFlt',.T.,.T.)
  lcRpExpSea = STRTRAN(lcRpExpSea,"INVHDR.SEASON","SEASON")
  lcRpExp = STRTRAN(lcRpExp,lcRpExpSea,".T.")
ENDIF
*B130166,1 NNA (End)

*B607833,1 TMI 11/15/2006 [Start] if a region is selected then change the lcRpExp varible 
=lfRegExp(.T.)
*B607833,1 TMI 11/15/2006 [End  ] 
 
SCAN FOR &lcRpExp AND &lcDivsion
*B603888,1 [END]

  *B130166,1 NNA 10/27/2005 (BEGIN) if user select a season then search in the invline for this season then get this invoice
  *B130166,1 NNA            even there are many seasons in this invoice.
  llFound = .F.
  IF USED('INVLINE') AND !EMPTY(laOgVrFlt[lnSeaPos,6]) AND SEEK(INVHDR.INVOICE,'INVLINE')
    SELECT INVLINE
    SCAN REST WHILE INVOICE=INVHDR.INVOICE
      IF INVLINE.SEASON = laOgVrFlt[lnSeaPos,6]
        llFound = .T.
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    llFound = .T.
  ENDIF
  SELECT INVHDR
  IF !llFound
    Loop
  ENDIF
  *B130166,1 NNA (END)
  
  *E301353,1 Add wait window to show collecting data [Begin.]
  WAIT WINDOW 'Collecting Data for Invoice no : ' + Invoice NOWAIT
  *E301353,1 Add wait window to show collecting data [End.]

  SCATTER MEMVAR MEMO

  *B604832,1 BWA 08/30/2001 Fix the bug of empty value for the region in case any transaction for stories.[START]
  *m.cTempKey = PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + PADR(CUSTOMER.REGION,6)
  m.cTempKey = PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
  *B604832,1 BWA 08/30/2001.[END]

  *B603656,1 BWA 05/28/2000 Moved to change sign
  *C101713,1 A custom process to calculate the shipped amount and the Mrech. Discount.[START]
  =gfDoTriger('ARSJOUR',PADR('SALJORPRIC',10))
  *C101713,1 [END]
  *B603656,1 BWA 03/28/2000 (End)

  *B605359,1 BWA 01/15/2002 Add the code that use the posted date.[START]
  *B603043,1 negative in void range only values  [Begin]
  *IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
  *                                  !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
  *  = lfNegValue() && Negative void values.
  *ENDIF
  *B603043,1 negative in void range only values  [End  ]

  IF lcRpDateTP = "P"
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))
      = lfNegValue() && Negative void values.
   ENDIF
  ELSE
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                     !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
      = lfNegValue() && Negative void values.
    ENDIF
  ENDIF
  *B605359,1 BWA 01/15/2002.[END]

  *B603656,1 BWA 03/28/2000 Commented out
  *C101713,1 A custom process to calculate the shipped amount and the Mrech. Discount.[START]
  *=gfDoTriger('ARSJOUR',PADR('SALJORPRIC',10))
  *C101713,1 [END]
  *B603656,1 BWA 03/28/2000 (End)

  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN

*B607833,1 TMI 11/15/2006 [Start] close the REG_CUST alias
=lfRegExp(.F.)
*B607833,1 TMI 11/15/2006 [End  ] 

*-- end of lfCollData

*!*************************************************************
*! Name      : lfGetOpExp
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
*! Purpose   : Make optimized expression then Collecting data
*!           : from InvHdr file into Work file.
*!*************************************************************
*! Called From : lfCollData.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfMakeOpt
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOpExp()
*!*************************************************************
FUNCTION lfGetOpExp
lcOptimize = ''

lnAccPos = CEILING(ASCAN(laOGVrFlt,'INVHDR.ACCOUNT')/7) && Position of Account in variable filter.

*-- if you find [Account is <In List>] in the variable filter section.
IF lnAccPos != 0
  llOpLogic  = laOGVrFlt[lnAccPos,4]
  lcAccOp    = laOGVrFlt[lnAccPos,5]
  lcAccVal   = laOGVrFlt[lnAccPos,6]

  lcOperator = ''

  *-- If user assign values for account selection.
  IF !EMPTY(lcAccVal)

    *-- if Logic of account filter "Is"
    IF llOpLogic

      DO CASE
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LIKE'
          lcOperator = '='
        CASE ALLTRIM(UPPER(lcAccOp)) == 'GREATER THAN'
          lcOperator = '>'
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LESS THAN'
          lcOperator = '<'
        CASE ALLTRIM(UPPER(lcAccOp)) == 'GREATER OR EQUAL'
          lcOperator = '>='
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LESS OR EQUAL'
          lcOperator = '<='
      ENDCASE    

    ELSE  && else Logic of account filter "Is Not"
    
      DO CASE
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LIKE'
          lcOperator = '!='
        CASE ALLTRIM(UPPER(lcAccOp)) == 'GREATER THAN'
          lcOperator = '<='
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LESS THAN'
          lcOperator = '>='
        CASE ALLTRIM(UPPER(lcAccOp)) == 'GREATER OR EQUAL'
          lcOperator = '<'
        CASE ALLTRIM(UPPER(lcAccOp)) == 'LESS OR EQUAL'
          lcOperator = '>'
      ENDCASE    

    ENDIF  && end if Logic of account filter "Is"
  
    *-- if Operator is [Contains or Between or InList]
    IF EMPTY(lcOperator)
      lcTrueFals = IIF(llOpLogic,'','!')  && Logic shape mask.
      IF ALLTRIM(UPPER(lcAccOp)) == 'CONTAINS'
        lcOptimize = [&lcTrueFals.(lcAccVal $ ACCOUNT+INVOICE)]
      
      ELSE  && Operator is either BETWEEN or INLIST.
      
        IF OCCUR('|',lcAccVal) = 0
          lcOptimize = [ACCOUNT+INVOICE &lcTrueFals.= lcAccVal]
        ELSE
          lcOperator = IIF(ALLTRIM(UPPER(lcAccOp)) == 'BETWEEN','BETWEEN','INLIST')
          lcOptimize = [&lcTrueFals.&lcOperator(ACCOUNT+INVOICE,] + lfMakeOpt(lcAccVal) + [)]
        ENDIF
      
      ENDIF
    
    ELSE  && else Operator is some thing rather than [Contains or Between or InList]
      lcOptimize = [ACCOUNT+INVOICE &lcOperator. lcAccVal]
    ENDIF  && end if Operator is [Contains or Between or InList]

  ENDIF  && end If user assign values for account selection.

ENDIF  && end if you find [Account is <In List>] in the variable filter section.
*-- When there is no optimize expression [user delete Account from OG]
*-- or does not assign any values for account filter,
*-- INVOICE Index is faster than ACCOUNT+INVOICE Index in collecting data.
lcOptimize = IIF(EMPTY(lcOptimize),[INVOICE = ''],lcOptimize)
*-- end of lfGetOpExp.

*!*************************************************************
*! Name      : lfMakeOpt
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
*! Purpose   : Make mask for optimize expression if operator
*!           : is either BETWEEN or INLIST.
*!*************************************************************
*! Called From : lfGetOpExp.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Optimized mask.
*!*************************************************************
*! Example   : = lfMakeOpt()
*!*************************************************************
FUNCTION lfMakeOpt
PARAMETERS lcString
PRIVATE lnPipeNo,lcExpr

lnPipeNo = OCCUR('|',lcString)

lcExpr = ''
FOR lnI = 1 TO lnPipeNo
  lcExpr    = IIF(EMPTY(lcExpr),"'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'",;
              lcExpr + "," + "'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'")
  lcString      = SUBSTR(lcString,ATC('|',lcString)+1)
ENDFOR
RETURN (lcExpr + "," + "'" + lcString + "'")
*-- end of lfMakeOpt.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/07/98
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
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [INVOICE]    && Index expression.
    IF lcRpKind = 'D'
      lcIndexTg  = [INVOICE]    && Index expression.
    ELSE
      lcIndexTg  = [CCURRCODE + INVOICE]    && Index expression.
    ENDIF
    *B603600,1 [END]

    lcSubTitle = [Invoice]    && Sub Title
    lcGroup    = ''           && Report Group
    lcGrpFoot  = ['']         && Group title

  CASE lcRpSortBy = 'A'  && Sort by account
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [ACCOUNT + CCURRCODE + INVOICE]
    ELSE  
      lcIndexTg  = [ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]
    
    lcSubTitle = [Account]
    lcGroup    = [ACCOUNT]
    lcGrpFoot  = ['Account# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]

  CASE lcRpSortBy = 'L'  && Sort by location
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [CWARECODE + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]

    lcSubTitle = [Location]
    lcGroup    = [CWARECODE]
    lcGrpFoot  = ['Location# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]

  CASE lcRpSortBy = 'R'  && Sort by primary sales rep.
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [REP1 + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]
    
    *C102483,1 Only few characters allowed. [Begin]
    *lcSubTitle = [Primary Sales Representative]
    lcSubTitle = [Primary Rep]
    *C102483,1 Only few characters allowed. [End]

    lcGroup    = [REP1]
    lcGrpFoot  = ['Sales Rep.# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]

  CASE lcRpSortBy = 'C'  && Sort by country
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [LEFT(cTempKey,6) + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,6) + ACCOUNT + INVOICE]    
    ENDIF  
    *B603600,1 [END]
    
    lcSubTitle = [Country]
    lcGroup    = [LEFT(cTempKey,6)]
    lcGrpFoot  = ['Country# ' + LEFT(cTempKey,6) + " - " + ALLTRIM(CUSTOMER.cAddress6)]

  CASE lcRpSortBy = 'S'  && Sort by state
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [LEFT(cTempKey,36) + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,36) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,36) + ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]
    
    *C102483,1 Only few characters allowed. [Begin]
    *lcSubTitle = lcSTitle
    lcSubTitle = SUBSTR(lcSTitle,1,8)
    *C102483,1 Only few characters allowed. [End]

    lcGroup    = [LEFT(cTempKey,36)]
    lcGrpFoot  = ['Country: ' + LEFT(cTempKey,6) + '  ' +;
                  IIF(SEEK(CUSTOMER.CCONT_CODE,'SYCINT'),;
                  ALLTRIM(SYCINT.CPART4LAB) + '# ','') +;
                  ALLTRIM(SUBSTR(cTempKey,7,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,7,30)),'STATE')]

  CASE lcRpSortBy = 'G'  && Sort by region
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [RIGHT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]
    
    lcSubTitle = [Region]
    lcGroup    = [RIGHT(cTempKey,6)]
    lcGrpFoot  = ['Region# ' + RIGHT(cTempKey,6) + '   ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]

  CASE lcRpSortBy = 'D'  && Sort by division
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [cDIVISION + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    ENDIF
    *B603600,1 [END]
    
    lcSubTitle = [Division]
    lcGroup    = [cDIVISION]
    lcGrpFoot  = ['Division# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]

  CASE lcRpSortBy = 'T'  && Sort by terms
    *B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
    *lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    IF llSortGrp
      lcIndexTg  = [cTermCode + CCURRCODE + ACCOUNT + INVOICE]
    ELSE 
      lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    ENDIF  
    *B603600,1 [END]
    
    *C102483,1 Only few characters allowed. [Begin]
    *lcSubTitle = [Payment Terms]
    lcSubTitle = [P. Terms]
    *C102483,1 Only few characters allowed. [End]

    lcGroup    = [cTermCode]
    lcGrpFoot  = ['Terms# ' + cTermCode + '   ' + gfCodDes(cTermCode,'CTERMCODE')]

  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    lcGroup    = [CCURRCODE]
    lcGrpFoot  = ['Currency# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
    lcCurrGrp  = ''
ENDCASE

*C102483,1 Add the second sort by option. [Begin]
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
*C102483,1 Add the second sort by option. [End]

*B603600,1 BWA 08/07/2000 The report gives wrong data when sorting by invoice.[START]
*lcIndexTg = IIF(llSortGrp AND (LEFT(lcIndexTg,9) != 'CCURRCODE'),;
            [CCURRCODE + ]+lcIndexTg,lcIndexTg)
*B603600,1 [END]

*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfvChFact
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/07/98
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Factored/Non Factored/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvChFact()
*!*************************************************************
FUNCTION lfvChFact
llChFactor = .T.
*-- end of lfvChFact.

*!*************************************************************
*! Name      : lfvList
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/07/98
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

*B603043,1 No Void date in OG Filter [Begin]
*PRIVATE lnInvItem,lnVoidItem
*lnInvItem  = CEILING(ASCAN(laOGFxFlt,'INVHDR.INVDATE')/7)  && Inv  date object number
*lnVoidItem = CEILING(ASCAN(laOGFxFlt,'INVHDR.VDATE')/7)    && Void date object number

*DO CASE
*  CASE lcRpList = 'C'    && List by invoice date.
*    = lfFltState(lnVoidItem,'D')   && Disable Void date.
*    laRpFltVal[lnVoidItem] = 'D'
*    IF laOldVal = 2  && Void
*      = lfFltState(lnInvItem,'E')  && Enable invoice date.
*      laRpFltVal[lnInvItem] = 'E'
*    ENDIF
*  CASE lcRpList = 'V'    && List by Void invoice date.
*    = lfFltState(lnInvItem,'D')    && Disable invoice date.
*    laRpFltVal[lnInvItem] = 'D'
*    IF laOldVal = 1  && Invoice
*      = lfFltState(lnVoidItem,'E') && Enable Void date.
*      laRpFltVal[lnVoidItem] = 'E'
*    ENDIF
*  CASE lcRpList = 'B'    && List by both Invoice and void invoice dates.
*    IF laOldVal = 1  && Invoice
*      = lfFltState(lnVoidItem,'E')  && Enable Void date.
*      laRpFltVal[lnVoidItem] = 'E'
*    ELSE
*      = lfFltState(lnInvItem,'E')   && Enable invoice date.
*      laRpFltVal[lnInvItem] = 'E'
*    ENDIF
*ENDCASE
*B603043,1 No Void date in OG Filter [End  ]

*-- end of lfvList.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
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

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
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
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvCurr.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/12/1998
*! Purpose   : Validate function for the Customer Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcc
PRIVATE lcItsName , lcItsVal , llObjRet

lcItsName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF
*-- end of lfvAcc.

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/12/1998
*! Purpose   : Validate location Code field
*!*************************************************************
*! Called from : Location field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the location he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*-- end of lfvLoc.

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRepCode()
*!*************************************************************
FUNCTION lfvRepCode
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SALESREP'))
  SELECT SALESREP
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvRepCode.

*!*************************************************************
*! Name      : lfvStates 
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/09/98
*! Purpose   : Validate the state code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [State  Object]- title is changes with country.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStates()
*!*************************************************************
FUNCTION lfvStates
PRIVATE lcStateObj , lcStateVal

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the state code he entered is not in the file.
*IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK(gcAct_Comp+PADR(lcStateVal,6)+'N'+'STATE','CODES'))
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]

   lcFile_Ttl = ALLTRIM(lcSTitle) + ' Codes'
   lcBrfields = 'cCode_No :H= ALLTRIM(lcSTitle) + " Code " ,cDiscrep :H="Description" :30'

   *IF gfBrows('FOR ccomp_id+cfld_name+ccode_no+cdiscrep = ;
   *  gcAct_Comp+"STATE" AND ;
   *  cRltField="N"','cCode_No,cDiscrep','laCodeFld')
   IF gfBrows('FOR cdefcode+cfld_name+ccode_no+cdiscrep = ;
     "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
     
     lcStateVal = laCodeFld[1]
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
lcStateVal    = IIF('?' $ lcStateVal,'',lcStateVal)
&lcStateObj = lcStateVal
*-- end of lfvStates.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
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
CREATE CURSOR (lcCurName) ;
   FROM ARRAY laTempStru
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/17/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.

*-- Close temp. opended files, if it used.

*-- Delete temporary work file.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF

*-- if user change setting [enter report <Preview> or <Run>]
IF !llFrTime
  *SET CENTURY &lcCentury
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfPreRun
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/21/98
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/24/99
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
*B602542,1 
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/18/98
*! Purpose   : -Ve Void values
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfNegValue()
*!*************************************************************
*!B603043,1
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 30/08/2001
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
ENDIF

=SEEK(lnCUSRec , 'CUSTOMER')

SELECT(lcAlias)
RETURN lcRegion
*--End of lfRegion.

*!**************************************************************************
*! Name      : lfSortDum2
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/29/2001
*! Purpose   : Fill second sort bu option 
*!**************************************************************************
*! Example   : =lfSortDum2()
*!**************************************************************************
*C102483,1
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
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/29/2001
*! Purpose   : Valid function for both 1st , 2nd Sort by option. 
*!**************************************************************************
*! Example   : =lfvSort()
*!**************************************************************************
*C102483,1
FUNCTION lfvSort
IF lcRpSortBy = lcRpSort2
  lcRpSort2 = 'N'
  CLEAR READ
ENDIF
*-- End of lfvSort.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/23/2001
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*B605250,1 Function to add the void expr. after the invoice date expr.
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

*B605359,1 BWA 01/15/2002 Modify the array to use the posted date filter.[START]
*=ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnInvPos,1),7)
IF lcRpDateTP = "P"
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnPostDate,1),7)
ELSE
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnInvPos,1),7)
ENDIF
*B605359,1 BWA 01/15/2002.[END]

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

*B605359,1 BWA 01/15/2002 Refresh the option grid in case the posted date.[START]
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/10/2002
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
CLEAR READ

*--End of lfClrRead.
*B605359,1 BWA 01/15/2002.[END]



*:**************************************************************************
*:* Name        : lfRegExp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 15/11/2006
*:* Purpose     : if a region is selected then change the lcRpExp varible 
*:***************************************************************************
FUNCTION lfRegExp
PARAMETERS llSet
PRIVATE lnSlct,lnRegPos
lnSlct = SELECT()

*- Using the relation expresson " iif(empty(store),'M','S')+account+store " between invhdr and customer file caused a problem when 
*- a single region is selected since the region is stored in the main record of the account in the customer file, so I will open the 
*- customer file in a new alias "REG_CUST" and set the relation between customer and "REG_CUST" as  " 'M'+account ",
*- I then will change the lcRpExp by removing the "CUSTOMER.REGION" string with "REG_CUST.REGION" so that the filter works correct

IF llSet

  lnRegPos = ASCAN(LAOGVRFLT,"CUSTOMER.REGION")
  IF lnRegPos>0 .AND. !EMPTY(laOgVrFlt[ASUBSCRIPT(laOgVrFlt,lnRegPos,1),6])
    IF !USED('REG_CUST')
      *- Open a new alias and set the relation 
      USE (gcDataDir+'CUSTOMER') AGAIN IN 0 ALIAS REG_CUST ORDER CUSTOMER
      SELECT CUSTOMER
      SET RELATION TO 'M'+ACCOUNT INTO REG_CUST
      
      *- Change the lcRpExp
      lcRpExp = STRTRAN(lcRpExp  , 'CUSTOMER.REGION' , 'REG_CUST.REGION' )
    ENDIF
  ENDIF

ELSE

  IF USED('REG_CUST')
    USE IN REG_CUST
  ENDIF
  
ENDIF

SELECT (lnSlct)
*-- end of lfRegExp.