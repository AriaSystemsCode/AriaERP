*:***************************************************************************
*: Program file        : ARSJOREN.PRG
*: Program desc.       : CUSTOMEZED SALES JOURNAL REPORT FOR ENGLAND.
*: For Report          : (ARSJOREN.FRX)
*: Date                : 05/16/2001
*: System              : Aria Advantage Series.
*: Module              : Account Receivable (AR)
*: Developer           : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number : C200190
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
*: Example : DO ARSJOREN
*:***************************************************************************

*----------------------- Report Code Begin -----------------------------
lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.

*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
*-- Show messages in status bar when collecting data. [begin]

lnInvPos = lfItmPos('INVHDR.INVDATE')
STORE {  /  /  } TO ldStrtDate , ldEndDate

IF EMPTY(laOGFxFlt[lnInvPos,6])
  lcVoidExpr = [llVoidOnly]
ELSE
  ldStrtDate = CTOD(PADR(laOGFxFlt[lnInvPos,6],ATC('|',laOGFxFlt[lnInvPos,6])+1))
  ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],ATC('|',laOGFxFlt[lnInvPos,6])+1))
  lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]
ENDIF

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime

  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.
  
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),;
                 'Invoice Currency : ' + cCurrCode + " , ",'')] +;
              [+ IIF(llRpRepPrn,  "Sales Rep. " + Rep1 +  " " +"Comm. Payable =  " +;
                                 TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + "PST Amount = " +;
                              TRANSFORM(lnPstAmt,"99999999.99")," ")]                   
  
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
                                TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
                               TRANSFORM(lnPstAmtCr,"99999999.99")," ")]                   

  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Total =" +;
                              TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + "PST Total  = " +;
                              TRANSFORM(lnPstAmtGp,"99999999.99")," ")]                   
  
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + "Sales Rep. Comm. Payable Grand =" +;
                                TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + "PST Grand  = " +;
                               TRANSFORM(lnPstAmtot,"99999999.99")," ")]                   

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

*-- Call Report [lcRpForm = 'ARSJOREN.FRX']
DO gfDispRe WITH EVAL('lcRpForm')

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.

*----------------------- Report Code End -----------------------------

*!*************************************************************
*! Name      : lfStitle
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfStitle.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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
  = SEEK('ARSJORENV')
  SCAN REST WHILE cRep_ID+cExpType+STR(nVarPos) = 'ARSJORENV' FOR nVarPos != 0
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

IF !EMPTY(LAOGVRFLT) 
  SHOW GET pbAdvanced ENABLE
ELSE
  IF ALEN(LAOGFIELDN) < 2 AND ASCAN(LAOGFIELDN,'DPOSTDATE') = 1 AND ALLTRIM(gfGetMemVar('M_LINK_GL')) = 'Y'
    SHOW GET pbAdvanced ENABLE
  ELSE
    SHOW GET pbAdvanced DISABLE
  ENDIF
ENDIF

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfFltState
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfFltState.
*!*************************************************************
*! Name      : lfFormName
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

RETURN 'ARSJOREN'

*--End of lfFormName.
*!*************************************************************
*! Name      : lfUserChTg 
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfUserChTg.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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
= lfGetOpExp()

*-- lcRpExp : Report expression, and I know that it must not be empty
*--         " but I check for empty because any one can remove hidden filter.

lcRpExp = lcOptimize + IIF(EMPTY(lcRpExp),'',[ AND ] + lcRpExp)
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")

IF !EMPTY(laOGFxFlt[lnInvPos,6])
  *-- if Void Only.
  IF lcRpList = "V"
    lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
  ENDIF

  *-- if Print Both Active and Void invoices.
  IF lcRpList = "B"
    IF !EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)
      lcRpExp = lcRpExp + " AND BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate)"     
    ENDIF  
  ENDIF
ENDIF

SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer

*-- cTempKey : Field in work file that contains Country code, plus State code,
*--          : plus region code.
*--          : this field serves in sort by [Country,State,Region]

lcDivsion = ".T."
IF  "V" $ lcRpList OR "B" $ lcRpList
  STORE 0 TO lnDataDiv
  lnDataDiv = ASCAN(laOgVrFlt,'INVHDR.CDIVISION')
  IF lnDataDiv > 0
    lnDataDiv  = ASUBSCRIPT(laOgVrFlt,lnDataDiv,1)
    lcDivsion = IIF(!EMPTY(laOgVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION = laOgVrFlt[lnDataDiv,6]", ".T.")
  ENDIF
ENDIF  

SCAN FOR &lcRpExp AND &lcDivsion

  WAIT WINDOW 'Collecting Data for Invoice no : ' + Invoice NOWAIT

  SCATTER MEMVAR MEMO
  m.cTempKey = PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + PADR(CUSTOMER.REGION,6)

  IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                    !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
    = lfNegValue() && Negative void values.
  ENDIF

  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN

*--End of lfCollData
*!*************************************************************
*! Name      : lfGetOpExp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfGetOpExp.
*!*************************************************************
*! Name      : lfMakeOpt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfMakeOpt.
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfvOptMsg.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfwOldVal.
*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

    lcSubTitle = [Primary Sales Representative]
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
    
    lcSubTitle = lcSTitle
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
    
    lcSubTitle = [Payment Terms]
    lcGroup    = [cTermCode]
    lcGrpFoot  = ['Terms# ' + cTermCode + '   ' + gfCodDes(cTermCode,'CTERMCODE')]

  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    lcGroup    = [CCURRCODE]
    lcGrpFoot  = ['Currency# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
    lcCurrGrp  = ''
ENDCASE

*--End of lfvSortBy.
*!*************************************************************
*! Name      : lfvChFact
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfvChFact.
*!*************************************************************
*! Name      : lfvList
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfvList.
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfvCurDisp.
*!*************************************************************
*! Name      : lfvCurr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfvCurr.
*!*************************************************************
*! Name      : lfvAcc
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('M' + lcItsVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcItsVal , '' , 'M')
  lcItsVal = IIF(llObjRet , lcItsVal , laOldVal)
  &lcItsName = lcItsVal
ENDIF    && End of IF

*--End of lfvAcc.
*!*************************************************************
*! Name      : lfvLoc
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*--End of lfvLoc.
*!*************************************************************
*! Name      : lfvRepCode
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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
    
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field

*--End of lfvRepCode.
*!*************************************************************
*! Name      : lfvStates 
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR(ALLTRIM(lcStateVal),6)+'N'+'STATE','CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]

   lcFile_Ttl = ALLTRIM(lcSTitle) + ' Codes'
   lcBrfields = 'cCode_No :H= ALLTRIM(lcSTitle) + " Code " ,cDiscrep :H="Description" :30'

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

*--End of lfvStates.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfCollTime.
*!*************************************************************
*! Name      : lfCreatCur
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfCreatCur.
*!*************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].

*--End of lfClearRep.
*!*************************************************************
*! Name      : lfPreRun
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfPreRun.
*!*************************************************************
*! Name      : lfPreRun
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetRepVr
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSortDumy()
*!*************************************************************
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

*--End of lfSortDumy.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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

*--End of lfItmPos.
*!*************************************************************
*! Name      : lfNegValue
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : -Ve Void values
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
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

*--End of lfNegValue.
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInvNo

PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = ALLTRIM(EVALUATE(SYS(18)))      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"

  lcFile_Ttl = 'Receivable invoices'

  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]

  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE
    &lcObjName = laOldVal
  ENDIF

  SET RELATION OFF INTO CUSTOMER
ELSE
    &lcObjName = lcObjVal
ENDIF

IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF

IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF

*--End of lfvInvNo.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/16/2001
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
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*--End of lfwOldVal.