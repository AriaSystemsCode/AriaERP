*:***************************************************************************
*: Program file  : SOORDHDG.PRG
*: Program desc. : Order Header Report For GMA
*: For Report    : (SOORDHGL.FRX, SOORDHGS.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry (MHM) 
*:***************************************************************************
*:               :This is copy from standard Order header we just increase width 
*:               :of CustPo# and decrease Season and Customer Name Only in FRX
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfModalGen,gfPhoneTem,gfTempName,gfMover,gfGetAdr.
*:               : lfwRepWhen,lfvOStatus,lfsrOrder,lfsrAcc,lfsrRep,lfClearRep,
*:               : lfvMulti,lfvReOrd,lfvBulk,lfWorkFile,lfCollTime,lfvSortBy,
*:               : lfShipToAd,lfAdrShift,lfvFormat,lfvEdiOrd.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORDHDG
*:***************************************************************************
*: This Report Program is due to C200480 ...
*:***************************************************************************
*:Modifications :
*:E120013,1 ASH 10/08/2003 Add new format (summary) to add the start date in it.
*:***************************************************************************
*
lcStTime   = TIME()                     && Variable to hold the start Time.
lcLastRep1 = ''
lnCntRep   = 0
lnGrandAcc = 0

STORE "" TO lcHedTitle,lcLineCurr
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot

=lfGetRepVr() && Get Report Variables...

*E301272,4 Use Full Multi currency format capability [End  ]


*-- If user change report critria, Collect report data. 
IF llOGFltCh

  llFilCodes = (lcRpFormat = "S")
  *-- Unrise all Critria variables.
  
  llChSortBy = .F.  && We Fill index field with its corresponding data in data collection.

  *-- Create scan expression from selected filter. [begin]
  lcMultiExp = IIF(llRpMulti,[MULTI = 'Y'],'')
  lcReOrdExp = IIF(llRpReOrd,[CREORDER = 'Y'],'')
  lcBulkExp  = IIF(llRpBulk ,[BULK = 'Y'],'')
  lcStatExp  = IIF(EMPTY(lcRpStatus),'',[STATUS $ lcRpStatus])

  lcHiddExp  = IIF(EMPTY(lcMultiExp),'',[ AND ] + lcMultiExp) +;
               IIF(EMPTY(lcReOrdExp),'',[ AND ] + lcReOrdExp) +;
               IIF(EMPTY(lcBulkExp),'',[ AND ] + lcBulkExp)   +;
               IIF(EMPTY(lcStatExp),'',[ AND ] + lcStatExp)

  lcRpExp    =  IIF('.T.' $ lcRpExp,[CORDTYPE+ORDER = 'O'],;
                                              [CORDTYPE+ORDER = 'O' AND ] + lcRpExp) + ;
                IIF(EMPTY(lcHiddExp),'', lcHiddExp)                              
  
  lcRpExp    = STRTRAN(lcRpExp,'ORDHDR.','')

  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [begin]
  IF !EMPTY(laOGFxFlt[lnRepPos,6])  
    lcRepFile  = laOGFxFlt[lnRepPos,6]
    lnRepStPos = ATC('SEEK(REP1',lcRpExp) 
    lnRepLen   = ATC(lcRepFile,lcRpExp) + 10 - lnRepStPos
    lcRepCond1 = SUBSTR(lcRpExp,lnRepStPos,lnRepLen)
    lcRepCond2 = STRTRAN(lcRepCond1,'REP1','REP2')
    lcRepCond  = '(' + lcRepCond1 + ' OR ' + lcRepCond2 + ')'
    lcRpExp    = STRTRAN(lcRpExp,lcRepCond1,lcRepCond)
  ENDIF
  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [end]
  
  *-- Create scan expression from selected filter. [end]
  
  *-- if you have previous data clear WorkFile then recreate it. [begin]
  IF !USED(WorkFile) OR RECCOUNT(WorkFile) > 0

    IF USED(WorkFile)
      USE IN (WorkFile)
    ENDIF  

    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear WorkFile then recreate it. [end]

  *-- Fill temporary file with report data [begin]
  SELECT ORDHDR
  SET ORDER TO
  SET RELATION TO 'M' + Account INTO CUSTOMER 

  IF !EMPTY(lcRpEdiFlt)
    IF !EMPTY(lcRpExp)
      lcRpExp = lcRpExp + [ AND ]
    ENDIF
    lcRpExp = lcRpExp + lcRpEdiFlt
  ENDIF

  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    =lfFillData()
  ENDSCAN
  SET ORDER TO ORDHDR

  SET RELATION OFF INTO CUSTOMER
  *-- Fill temporary file with report data [end ]
ENDIF  && end If user change report critria, Collect report data. 

IF RECCOUNT(WorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

lcShpTName = ''
lcPhonePic = gfPhoneTem()

SELECT (WorkFile)
*-- if not first run for the same filter and start run is short format then fill codes. [Begin]
IF !llOGFltCh AND llFilCodes AND (lcRpFormat = "L")
  llFilCodes = .F.
  SET ORDER TO
  SCAN
    =lfFillData(.T.)
    IF llChSortBy
      REPLACE cTempKey WITH EVALUATE(lcIndexTg)
    ENDIF
  ENDSCAN
  llChSortBy = .F.
  SET ORDER TO (WorkFile)
ENDIF
*-- if not first run for the same filter and start run is short format then fill codes. [End  ]

IF llChSortBy
  REPLACE ALL cTempKey WITH EVALUATE(lcIndexTg)
  llChSortBy = .F.
ENDIF

*-- Temp File used to Calculate No. of accounts [begin]
*-- in the current sales rep. and in grand totals.
CustFile = gfTempName()
CREATE CURSOR (CustFile) (ACCOUNT C(5) , SALESREP C(3), cCurrCode C(3) ,;
                          cShip1 C(30),cShip2 C(30),cShip3 C(30),cShip4 C(30),cShip5 C(30) ,;
                          STORE C(8))
SELECT (CustFile)
ZAP
INDEX ON ACCOUNT+STORE TAG (CustFile) OF (gcWorkDir+CustFile+'.CDX')

IF lcRpSortBy = 'S'        && WHEN SORTING BY SALESREP

  CUSTFL1 = gfTempName()
  IF llMultCurr
    INDEX ON SALESREP+CCURRCODE+ACCOUNT TAG (CUSTFL1) OF (gcWorkDir+CustFile+'.CDX')
  ELSE
    INDEX ON SALESREP+ACCOUNT TAG (CUSTFL1) OF (gcWorkDir+CustFile+'.CDX')
  ENDIF  
ENDIF
SET ORDER TO TAG (CustFile)
*-- Temp File used to Calculate No. of accounts [end]

SELECT (WorkFile)
GO TOP

*-- Set relation between temp. file and customer file.
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,;
                                   'S' + Account + Store) INTO CUSTOMER  && To customer file.
SET RELATION TO 'B'+Order INTO NOTEPAD ADDITIVE

lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(WorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

DO gfDispRe WITH EVAL('lcRpForm')

USE IN (CustFile)
RETURN
*-- end of report code.

*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

lnRepPos  = lfItmPos('ORDHDR.REP1')
lnOrdPos  = lfItmPos('ORDHDR.ORDER')
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ENDIF  

IF EMPTY(laRpSource)   
   DECLARE laRpSource[5],laRpTarget[5]

  STORE 'Bid'       TO laRpSource[1],laRpTarget[1]
  STORE 'Open'      TO laRpSource[2],laRpTarget[2]
  STORE 'Hold'      TO laRpSource[3],laRpTarget[3]
  STORE 'Canceled'  TO laRpSource[4],laRpTarget[4]
  STORE 'Completed' TO laRpSource[5],laRpTarget[5]
ENDIF
IF !USED(WorkFile)
  DECLARE  laShipTo[5,1],laCodDesc[6,3]
  STORE ''          TO lcRpStatus,laShipTo,laCodDesc
  laCodDesc[1,2] = "SEASON"
  laCodDesc[2,2] = "CDIVISION"
  laCodDesc[3,2] = "CTERMCODE"
  laCodDesc[4,2] = "SHIPVIA"
  laCodDesc[5,2] = "SPCINST"
  laCodDesc[6,2] = "REGION"

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDHDR
  lnFildLen = AFIELDS(laTempStru)

  *-- Add Code fields.
  DIMENSION laTempStru[lnFildLen + 9, 4]
  laTempStru[lnFildLen + 1, 1] = 'cSeaDesc'
  laTempStru[lnFildLen + 1, 2] = 'C'
  laTempStru[lnFildLen + 1, 3] = 37
  laTempStru[lnFildLen + 1, 4] = 0

  laTempStru[lnFildLen + 2, 1] = 'cDivDesc'
  laTempStru[lnFildLen + 2, 2] = 'C'
  laTempStru[lnFildLen + 2, 3] = 37
  laTempStru[lnFildLen + 2, 4] = 0

  laTempStru[lnFildLen + 3, 1] = 'cTrmDesc'
  laTempStru[lnFildLen + 3, 2] = 'C'
  laTempStru[lnFildLen + 3, 3] = 37
  laTempStru[lnFildLen + 3, 4] = 0

  laTempStru[lnFildLen + 4, 1] = 'cViaDesc'
  laTempStru[lnFildLen + 4, 2] = 'C'
  laTempStru[lnFildLen + 4, 3] = 37
  laTempStru[lnFildLen + 4, 4] = 0

  laTempStru[lnFildLen + 5, 1] = 'cSpcDesc'
  laTempStru[lnFildLen + 5, 2] = 'C'
  laTempStru[lnFildLen + 5, 3] = 37
  laTempStru[lnFildLen + 5, 4] = 0

  laTempStru[lnFildLen + 6, 1] = 'cRegDesc'
  laTempStru[lnFildLen + 6, 2] = 'C'
  laTempStru[lnFildLen + 6, 3] = 37
  laTempStru[lnFildLen + 6, 4] = 0

  laTempStru[lnFildLen + 7, 1] = 'cCanDesc'
  laTempStru[lnFildLen + 7, 2] = 'C'
  laTempStru[lnFildLen + 7, 3] = 37
  laTempStru[lnFildLen + 7, 4] = 0

  laTempStru[lnFildLen + 8, 1] = 'cCurDesc'
  laTempStru[lnFildLen + 8, 2] = 'C'
  laTempStru[lnFildLen + 8, 3] = 37
  laTempStru[lnFildLen + 8, 4] = 0

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[lnFildLen + 9, 1] = 'cTempKey'
  laTempStru[lnFildLen + 9, 2] = 'C'
  laTempStru[lnFildLen + 9, 3] = 26
  laTempStru[lnFildLen + 9, 4] = 0
 
  SET ORDER TO ORDHDR   IN ORDHDR
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO SALESREP IN SALESREP

  =lfWorkFile()
  
  =lfvSortBy()
ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ''
IF (ALEN(laRpTarget,1) < 5) AND !(ALEN(laRpTarget,1) = 1 AND EMPTY(laRpTarget))
  DIMENSION laStatus[5]
  laStatus[1] = 'B' 
  laStatus[2] = 'O' 
  laStatus[3] = 'H' 
  laStatus[4] = 'X' 
  laStatus[5] = 'C' 

  PRIVATE lnI

  *-- Loop to make Status expression.
  FOR lnI = 1 TO ALEN(laRpSource,1)
    
    IF ATC('\',laRpSource[lnI]) > 0
      lcRpStatus = lcRpStatus + laStatus[lnI]
    ENDIF  

  ENDFOR  && end Loop to make Status expression.

ENDIF  

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh  = .T.
  llClearOrd = .T.
ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh  = .T.
      llClearOrd = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]

*-- Activate suppress expression [begin]
llCancNow = ('X' $ lcRpStatus) OR EMPTY(lcRpStatus)
llCancPrv = ('X' $ lcOldStat)  OR EMPTY(lcOldStat)
IF (llCancNow AND !llCancPrv) OR (!llCancNow AND llCancPrv)
  CLEAR READ
ENDIF
*-- Activate suppress expression [end]
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
IF lcParm = "S"
  IF !EMPTY(lcRpStatus)
	  SET FILTER TO STATUS$lcRpStatus
  ENDIF
  llClearOrd = .F.
  GO TOP
ELSE
  IF !EMPTY(lcRpStatus)
    SET FILTER TO
  ENDIF
ENDIF  
*-- end of lfsrOrder.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.

*!*************************************************************
*! Name      : lfvMulti
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change Multi Stores only flag.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvMulti()
*!*************************************************************
FUNCTION lfvMulti
*llChMulti = .T.
*-- end of lfvMulti.

*!*************************************************************
*! Name      : lfvReOrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change Reorders only flag.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvReOrd()
*!*************************************************************
FUNCTION lfvReOrd
*llChReOrd = .T.
*-- end of lfvReOrd.

*!*************************************************************
*! Name      : lfvBulk
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change Bulk only flag.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvBulk()
*!*************************************************************
FUNCTION lfvBulk
*llChBulk = .T.
*-- end of lfvBulk.

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Create work cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
FUNCTION lfWorkFile
CREATE CURSOR (WorkFile) ;
   FROM ARRAY laTempStru

SELECT (WorkFile)
*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON cTempKey TAG (WorkFile) OF (gcWorkDir+WorkFile+'.CDX')
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/1998
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
llOGFltCh = .T.

*-- Close temp. opended files, if it used.
*-- Delete temporary work file.
IF USED(WorkFile)
 USE IN (WorkFile)
ENDIF

IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF USED("SYCINT")
    USE IN SYCINT 
  ENDIF

ENDIF
*-- end of lfClearRep.

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
*! Called from : Option Grid
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
*! Name      : lfvSortBy
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
             ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
              (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfShipToAd
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Evaluate ship to address then calculate No. of accounts in range.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfShipToAd()
*!*************************************************************
FUNCTION lfShipToAd
PARAMETERS llShort
PRIVATE lnSeekedRc

*-- if short format.
IF llShort AND llMultCurr
  =lfChCurSm()
ENDIF  

*-- Calculate No. Of accounts [Begin]
SET ORDER TO TAG (CustFile) IN (CustFile)
lnSeekedRc = 0
IF SEEK(ACCOUNT+IIF(llShort,'',STORE) , CustFile)
  IF Alt_ShpTo
    lcShpTName  = ALLTRIM(Customer.STName)
  ELSE
    lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  ENDIF

ELSE

  lnGrandAcc  = lnGrandAcc + 1
  lnCntRep    = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
  lcLastRep1  = REP1+IIF(llMultCurr,CCURRCODE,'')
  m.Account   = ACCOUNT
  m.Store     = STORE
  m.SalesRep  = REP1
  m.cCurrCode = cCurrCode
  INSERT INTO (CustFile) FROM MEMVAR
  =lfUpdAdd()

ENDIF
lnSeekedRc = RECNO(CustFile)

*-- if sort by sales rep.
IF lcRpSortBy = 'S'
  SET ORDER TO TAG (CUSTFL1) IN (CustFile)
  lcSeekVal = IIF(llMultCurr,REP1+cCurrCode+ACCOUNT,REP1+ACCOUNT)
  IF !SEEK(lcSeekVal,CustFile)
    lnCntRep   = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
    lcLastRep1 = REP1+IIF(llMultCurr,CCURRCODE,'')
    m.Account  = ACCOUNT
    m.Store     = STORE
    m.SalesRep = REP1
    m.cCurrCode = cCurrCode
    INSERT INTO (CustFile) FROM MEMVAR
  ENDIF
ENDIF

IF lnSeekedRc <> 0
  GO lnSeekedRc IN (CustFile)
ENDIF

*-- Calculate No. Of accounts [End]
RETURN ''
*-- end of lfShipToAd.

*!*************************************************************
*! Name      : lfUpdAdd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/24/1999
*! Purpose   : Update Customer addresses.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdAdd()
*!*************************************************************
*!E301272,4
FUNCTION lfUpdAdd
PRIVATE lnCurrAls
lnCurrAls = SELECT(0)
laShipTo = ''
*-- IF alternate ship to address
IF Alt_ShpTo
  lcShpTName  = ALLTRIM(Customer.STName)
  IF !llShort
    laShipTo[1] = ALLTRIM(cAddress1)
    laShipTo[2] = ALLTRIM(cAddress2)
    laShipTo[3] = ALLTRIM(cAddress3)
    laShipTo[4] = ALLTRIM(cAddress4)
    laShipTo[5] = ALLTRIM(cAddress5)
  ENDIF  
ELSE    && Else
  lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  IF !llShort
    =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)
  ENDIF  
ENDIF    && End of IF

IF !llShort
  =lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
  FOR lnCnt = 1 TO ALEN(laShipTo)
    laShipTo[lnCnt] = SUBSTR(laShipTo[lnCnt],1,21)
  ENDFOR
  SELECT (CustFile)
  GATHER FROM laShipTo FIELDS cShip1,cShip2,cShip3,cShip4,cShip5
ENDIF  
SELECT (lnCurrAls)
*-- end of lfUpdAdd.


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfvFormat
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Change report format (Long/Short)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvFormat()
*!*************************************************************
FUNCTION lfvFormat
*E120013,1 ASH 10/08/2003 (Begin) Add new format (summary) to add the start date in it.
*lcRpForm = IIF(lcRpFormat = 'L','SOORDHGL','SOORDHGS')
DO CASE
  CASE lcRpFormat='L'
    lcRpForm = 'SOORDHGL'
  CASE lcRpFormat='S'
    lcRpForm = 'SOORDHGS'
  CASE lcRpFormat='M'
    lcRpForm = 'SOORDHGM'
ENDCASE
*E120013,1 ASH 10/08/2003 (End)
= lfRepPltFr(lcRpForm)
*-- end of lfvFormat.

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Get Report Variables
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetRepVr()
*!*************************************************************
*!E301272,4 Use full Multi currency format capability.
FUNCTION lfGetRepVr
*-- ReIndex work file if user change sort By [Begin]
IF llChSortBy
  DO CASE
   CASE lcRpSortBy = 'O'		&& Sort by Order Case
     
     lcSortTitl= "Order"
     IF llCurInGrp
       lcBreak   = 'CCURRCODE'
       lcGrpExpr = [CCURRCODE + ' Totals :']
       
       lcIndexTg = [CCURRCODE+ORDER]
     ELSE
  
       lcBreak   = ''
       lcGrpExpr = ''
  
       lcIndexTg = [ORDER]
     ENDIF
     
   CASE lcRpSortBy = 'A'		&& Sort by Account Case
     IF llCurInGrp
       lcBreak = 'ACCOUNT+CCURRCODE'
       lcGrpExpr = [ACCOUNT + "/" + CCURRCODE + ' Totals :']
       lcIndexTg = [ACCOUNT+CCURRCODE+ORDER]
     ELSE
       lcBreak = 'ACCOUNT'
       lcGrpExpr = [ACCOUNT + ' Totals :']
       lcIndexTg = [ACCOUNT+ORDER]
     ENDIF
     lcSortTitl= "Account"

   CASE lcRpSortBy = 'C'		&& Sort by CustPo Case
     IF llCurInGrp
       lcBreak = 'ACCOUNT+CUSTPO+CCURRCODE'
       lcGrpExpr = [ACCOUNT + '/' + CUSTPO + '/' + CCURRCODE + ' Totals :']
       lcIndexTg = [ACCOUNT+CUSTPO+CCURRCODE+ORDER]
     ELSE
       lcBreak = 'ACCOUNT+CUSTPO'
       lcGrpExpr = [ACCOUNT + '/' + CUSTPO + ' Totals :']
       lcIndexTg = [ACCOUNT+CUSTPO+ORDER]
     ENDIF

     lcSortTitl= "Cust Po"

   CASE lcRpSortBy = 'P'		&& Sort by Priority Case
     IF llCurInGrp
       lcBreak = 'DTOS(COMPLETE)+PRIORITY+CCURRCODE'
       lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + '/' + CCURRCODE + ' Totals :']
       lcIndexTg = [DTOS(COMPLETE)+PRIORITY+CCURRCODE+ACCOUNT+ORDER]
     ELSE
       lcBreak = 'DTOS(COMPLETE)+PRIORITY'
       lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + ' Totals :']
       lcIndexTg = [DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER]
     ENDIF

     lcSortTitl= "Priority"

   CASE lcRpSortBy = 'S'		&& Sort by Primary sales rep. Case
     IF llCurInGrp
       lcBreak   = 'REP1+CCURRCODE' 
       lcGrpExpr = [REP1 + '/' + CCURRCODE + ' Totals :']
       lcIndexTg = [REP1+CCURRCODE+ORDER]
     ELSE
       lcBreak   = 'REP1' 
       lcGrpExpr = [REP1 + ' Totals :']
       lcIndexTg = [REP1+ORDER]
     ENDIF

     lcSortTitl= "Primary Sales Rep."

   CASE lcRpSortBy = 'U'		&& Sort by Currency Case
     lcBreak   = 'CCURRCODE' 
     lcGrpExpr = [lfCurrDesc() + ' Totals :']
     lcIndexTg = [CCURRCODE+ORDER]
     lcSortTitl= "Currency"

  ENDCASE

  IF llCurInGrp AND (lcRpSortBy <> 'U')

    IF lcRpSortBy="O"
      lcSortTitl = "Currency/" + lcSortTitl
    ELSE
      lcSortTitl = lcSortTitl+"/Currency"
    ENDIF  
  ENDIF
ENDIF

lcHedTitle = lcSortTitl 
IF llMultCurr
  lcLineCurr = lfCurrPrnt()
  lcHedTitle = lcHedTitle + ", Printed in : "
  DO CASE
    CASE lcRpCurr = "F"  && Foreign Currency.
      lcHedTitle = lcHedTitle + "Foreign Currency."
    CASE lcRpCurr = "O"  && Original Rates
      lcHedTitle = lcHedTitle + "Equivalent based on the Original transactions exchange rates."
    CASE lcRpCurr = "D"  && Rates on date
      lcHedTitle = lcHedTitle + "Equivalent based on the exchange rates defined on "+ DTOC(ldRpExDate) + "."
    OTHERWISE            && Rates defined now.
      lcHedTitle = lcHedTitle + "Equivalent based on the exchange rates defined now."
  ENDCASE
ENDIF

*-- ReIndex work file if user change sort By [End  ]
*-- end of lfGetRepVr.

*!*************************************************************
*! Name      : lfAsignVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Assign all Report Variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAsignVar()
*!*************************************************************
*!E301272,4 Use full Multi currency format capability.
FUNCTION lfAsignVar

PARAMETERS llFromPrg

IF !llFromPrg
  *-- Make Sort by Arrays and fill it. [Begin]
  DIMENSION laSortDesc[5,1],laSortVal[5,1]
  laSortDesc[1,1] = "Order"
  laSortDesc[2,1] = "Account"
  laSortDesc[3,1] = "Cust PO"
  laSortDesc[4,1] = "Priority"
  laSortDesc[5,1] = "Primary Sales Representative"

  laSortVal[1,1]  = "O"
  laSortVal[2,1]  = "A"
  laSortVal[3,1]  = "C"
  laSortVal[4,1]  = "P"
  laSortVal[5,1]  = "S"
  *-- Make Sort by Arrays and fill it. [Begin]

  llMultCurr = gfGetMemVar('llMulCurr')
ENDIF
  
IF llMultCurr
  
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
  ENDIF
  
  *-- Add Currency item to sort by array. [Begin]
  DIMENSION laSortDesc[ALEN(laSortDesc,1) + 1 , 1] , laSortVal[ALEN(laSortDesc,1) + 1 , 1]
  laSortDesc[ALEN(laSortDesc,1),1] = "Currency"
  laSortVal[ALEN(laSortDesc,1),1]  = "U"
  *-- Add Currency item to sort by array. [End  ]

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]

ENDIF

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
ENDIF  

*-- end of lfAsignVar.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,4
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCurrPrnt()
*!*************************************************************
*!E301272,4
FUNCTION lfCurrPrnt
PRIVATE lcCurrCode
*-- Not Multi Currency Or it is and any Equavelent method.
IF lcRpCurr <> "F"
  lcCurrCode = [gcBaseCurr]
ELSE && Multi Currency and Print forign currency.
  lcCurrCode = [cCurrCode]
ENDIF
RETURN lcCurrCode
*-- end of lfCurrPrnt.

*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChCurSm()
*!*************************************************************
*!E301272,4
FUNCTION lfChCurSm
PRIVATE lcCurrCurr,lnCurrAls
lnCurrAls = SELECT(0)
lcCurrCurr = ALLTRIM(EVALUATE(lcLineCurr))

IF !USED("SYCINT")
  =lfAsignVar(.T.)
ENDIF

SELECT SYCINT
LOCATE FOR cCurrCode = lcCurrCurr
IF FOUND()
  lcCurrRtL = ALLTRIM(cCurrency)
  lcCurrSet = ALLTRIM(cCurrencyI)
  SET CURRENCY TO lcCurrSet
  SET CURRENCY &lcCurrRtL
ENDIF  
SELECT (lnCurrAls)
RETURN ''
*-- end of lfChCurSm.

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************
*!E301272,4
FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cCurrCode)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN PADR(ALLTRIM(lcCurDesc),20)
*-- end of lfCurrDesc.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*E301272,4
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!************************************************************
*! Name      : lfFillData
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/23/1999
*! Purpose   : Evaluate all memvars and insert new line.
*!************************************************************
*! Passed Parameters  : ...
*!************************************************************
*! Returns            : Position
*!************************************************************
*! Example   : =lfFillData()
*!************************************************************
*MAB at 06/23/1999 TO Fasten the report long form.
FUNCTION lfFillData
PARAMETERS llEditOnly

*-- if it is long format
IF lcRpFormat = "L"
  PRIVATE lcCanReson,lcCurrDesc
  STORE "" TO lcCanReson,lcCurrDesc
  laCodDesc[1,1] = SEASON
  laCodDesc[2,1] = CDIVISION
  laCodDesc[3,1] = CTERMCODE
  laCodDesc[4,1] = SHIPVIA
  laCodDesc[5,1] = SPCINST
  laCodDesc[6,1] = CUSTOMER.REGION

  IF EMPTY(ccancreson)
    DIMENSION laCodDesc[6,3]
  ELSE
    DIMENSION laCodDesc[7,3]
    laCodDesc[7,1] = CCANCRESON
    laCodDesc[7,2] = "CCANCRESON"
  ENDIF

  =gfCodDes(@laCodDesc)
  IF !EMPTY(ccancreson)
    lcCanReson = laCodDesc[7,3]
  ENDIF  

  IF llMultCurr
    lcCurrDesc = lfCurrDesc()
  ENDIF  

  m.cSeaDesc = laCodDesc[1,3]
  m.cDivDesc = laCodDesc[2,3]
  m.cTrmDesc = laCodDesc[3,3]
  m.cViaDesc = laCodDesc[4,3]
  m.cSpcDesc = laCodDesc[5,3]
  m.cRegDesc = laCodDesc[6,3]
  m.cCanDesc = lcCanReson
  m.cCurDesc = lcCurrDesc

ENDIF

IF llEditOnly
  REPLACE cSeaDesc WITH laCodDesc[1,3] ,;
          cDivDesc WITH laCodDesc[2,3] ,;
          cTrmDesc WITH laCodDesc[3,3] ,;
          cViaDesc WITH laCodDesc[4,3] ,;      
          cSpcDesc WITH laCodDesc[5,3] ,;
          cRegDesc WITH laCodDesc[6,3] ,;
          cCanDesc WITH lcCanReson     ,;
          cCurDesc WITH lcCurrDesc
ELSE
  
  m.cTempKey = EVALUATE(lcIndexTg)
  INSERT INTO (WorkFile) FROM MEMVAR
ENDIF  
*-- end of lfFillData.

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.
