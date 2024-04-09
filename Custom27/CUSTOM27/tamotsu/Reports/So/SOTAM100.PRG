*:***************************************************************************
*: Program file  : SOTAM100   C102123
*: Program desc. : Custom Order Detail For TamOtsu
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Mohamed_El_Anwar  (AME)
*: Date          : 04/26/2001
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : This program was Ren100 In ARIA 26 with C101366,c101432
*:***************************************************************************
*: Example : DO SOTAM100
*:***************************************************************************
*: Modifications:
*B804478,1 MHM 11/10/2001 Fix the bug of the report doesn't have the option to print by store.     
*B804478,1                Add In list for Account by stores in option grid in case of Select by Account.
*:***************************************************************************

SELECT ORDLINE
SET RELATION  TO STYLE INTO STYLE ADDITIVE
SET RELATION  TO "O"+ORDER INTO ORDHDR ADDITIVE

*B804478,1 MHM 11/10/2001 [Start]
*SET RELATION  TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
*B804478,1 MHM 11/10/2001 [End]

IF llFrTime   &&first time to run report
  lcTempLine = gfTempName()

  DIMENSION laTempStru[1,4]
  laTempStru = ''
  SELECT ORDLINE
  = AFIELDS(laTempStru)
  DIMENSION laTempStru[ALEN(laTempStru,1) + 2, 4]

  laTempStru[ALEN(laTempStru,1) -1  ,1] = 'SPCINST'
  laTempStru[ALEN(laTempStru,1) -1  ,2] = 'C'
  laTempStru[ALEN(laTempStru,1) -1  ,3] = 6
  laTempStru[ALEN(laTempStru,1) -1  ,4] = 0
  laTempStru[ALEN(laTempStru,1)     ,1] = 'XGROUP'
  laTempStru[ALEN(laTempStru,1)     ,2] = 'C'
  laTempStru[ALEN(laTempStru,1)     ,3] = 2
  laTempStru[ALEN(laTempStru,1)     ,4] = 0

  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.  
ENDIF

IF EMPTY(lcTempLine) OR !USED(lcTempLine)
  CREATE TABLE (gcWorkDir+lcTempLine) ;
  FROM ARRAY laTempStru
  INDEX ON ACCOUNT+STORE+SPCINST+XGROUP+ORDER+STYLE+STR(LineNO,6) Tag &lcTempLine
ENDIF

*B804478,1 MHM 11/10/2001 change the way to collect data [start]
IF lcRpSelcBy = 'A' AND !EMPTY(lcDumAcc)
  SELECT CUSTOMER
  SET ORDER TO TAG lcMmajour
  SELECT ORDLINE
  SET RELATION  TO ACCOUNT+STORE INTO CUSTOMER ADDITIVE
  LOCATE
  lcRpExp = lcRpExp + IIF(!EMPTY(lcRpExp), " AND " + lcDumAcc ,lcDumAcc)
  llOGFltCh = .T.  
ELSE
  SET RELATION  TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
ENDIF  
*B804478,1 MHM 11/10/2001 [End]

IF llClearFn OR llOGFltCh
  llClearFn = .F.
  IF ('MA' $ gcCmpModules) AND (lcRpSelcBy = 'F')
    IF AT('FABRIC.FABRIC',lcRpExp) <> 0
      lcRpExp = STRTRAN(lcRpExp,'FABRIC.FABRIC','STYLE.FABRIC')
    ENDIF
  ENDIF

  IF RECCOUNT(lcTempLine) > 0
    SELECT (lcTempLine)
    ZAP
    SET RELATION TO
  ENDIF		&& end If you find any data.
  
  lnOrdPos   = lfItmPos('ORDHDR.ORDER')
  lcTOrdFile = laOGFxFlt[lnOrdPos,6]
  llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
  IF !llWorkDeal
    SELECT ORDLINE
    WAIT WINDOW 'Collecting data.... Please wait' NOWAIT
    SCAN FOR &lcRpExp .AND. ORDHDR.STATUS $ lcRpStatus .AND. ORDHDR.SPCINST = lcRpSInst .AND.  IIF(llRpCorrGp,!EMPTY(GROUP),.T.) 
      WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT
      SCATTER MEMVAR MEMO
      M.SPCINST = ORDHDR.SPCINST
      M.XGROUP  = STYLE.CSTYGROUP
      INSERT INTO (lcTempLine) FROM MEMVAR
    ENDSCAN
  ELSE
    SELECT (lcTOrdFile)
    SCAN
      WAIT WINDOW 'Collecting data Order # '+ Order NOWAIT            
      SELECT ORDLINE
      IF SEEK('O'+&lcTOrdFile..ORDER) AND;
         &lcRpExp  AND;
         ORDHDR.STATUS $ lcRpStatus AND; 
         ORDHDR.SPCINST = lcRpSInst AND ; 
         IIF(llRpCorrGp,!EMPTY(GROUP),.T.)
       
        SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcTOrdFile..Order
          IF &lcRpExp  AND;
             ORDHDR.STATUS $ lcRpStatus AND; 
             ORDHDR.SPCINST = lcRpSInst AND ; 
             IIF(llRpCorrGp,!EMPTY(GROUP),.T.)
            
            
            SCATTER MEMVAR MEMO
            M.SPCINST = ORDHDR.SPCINST
            M.XGROUP  = STYLE.CSTYGROUP
            INSERT INTO (lcTempLine) FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SELECT ORDLINE
SET RELATION TO
SELECT (lcTempLine)
LOCATE

IF EOF()
  *----------There's No record to display------------*
  *----------            <OK>            ------------*
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
DIMENSION XTOTAL(9),laActTot(9),laStorTot(9),laSpInsTot(9),laGrpTot(9)
STORE 0 TO  lnGrndAmt,laStorTot,laActTot , laGrpTot , laSpInsTot , XTOTAL
R_WIDTH  = 'W'
lcStTime   = Time()                     && Time in which we start collect data.
SET DEVICE TO PRINT
PAGENO = 0
ROW    = 00
XTIME  = SUBSTR(TIME(),1,5)
XTOTAL  = 0.00
R_TITLE = " ORDER DETAIL REPORT "
PAGENO = PAGENO +1

DO RPT_HDR WITH 'SOTAM100',''+ lcRpMsg1,R_WIDTH
@ 05,000 SAY 'SI      ORDER  STYLE        DESCRIPTION      COLOR  DESCRIPTION  (1)   (2)   (3)   (4)   (5)   (6)   (7)   PIECES   PRICE      TOTAL.'
@ 06,000 SAY REPLICATE('-',132)
ROW = 7

lcSubGrp    = XGROUP
lcSubSpcIns = SpcInst
lcSubStr    = STORE
lcLoopAcct  = Account 

ROW = ROW + 1
@ ROW ,000 SAY 'ACCT. ' + lcLoopAcct
ROW = ROW + 1
@ ROW ,000 SAY IIF(!EMPTY(STORE),'STORE ' + STORE,'MAIN ACCOUNT')
ROW = ROW + 1
@ ROW ,000 SAY 'GROUP ' + xgroup + ' S.D. '+DTOC(START)+' C.D. '+DTOC(COMPLETE)

SELECT (lcTempLine)
SET RELATION TO STYLE INTO STYLE
WAIT WINDOW 'Printing  data....     <SPACE BAR> to abort' NOWAIT
SCAN WHILE INKEY() <> 32
  IF XGROUP <> lcSubGrp
    =lfPrnSub('laGrpTot')
    STORE 0 TO laGrpTot
  ENDIF
  IF SpcInst <> lcSubSpcIns
    =lfPrnSub('laSpInsTot')
    STORE 0 TO  laSpInsTot
    lcSubSpcIns = SpcInst
  ENDIF
  IF STORE <> lcSubStr
    =lfPrnSub('laStorTot')
    STORE 0 TO  laStorTot
  ENDIF
  IF !(ACCOUNT = lcLoopAcct)
    =lfPrnSub('laActTot')
    STORE 0 TO  laActTot
    *C102113,4 AME Fix the bug of printing the prev header of the account [Start]
    lcSubStr = STORE
    lcLoopAcct = ACCOUNT
    *C102113,4 AME [End]
    ROW = ROW + 1
    @ ROW ,000 SAY 'ACCT. ' + lcLoopAcct
    ROW = ROW + 1
    @ ROW ,000 SAY IIF(!EMPTY(STORE),'STORE ' + STORE,'MAIN ACCOUNT')
    *C102113,4 AME Fix the bug of printing the prev header of the account [Start]
    *lcSubStr = STORE
    *lcLoopAcct = ACCOUNT
    *C102113,4 AME [End]
  ENDIF
  IF  lcSubStr <> STORE
    ROW = ROW + 1
    @ ROW ,000 SAY 'STORE ' + STORE
    lcSubStr = STORE
  ENDIF  
  IF lcSubGrp  <> XGROUP
    lcSubGrp  = XGROUP
    ROW = ROW + 1
    @ ROW ,000 SAY 'GROUP ' + XGROUP + ' S.D. '+DTOC(START)+' C.D. '+DTOC(COMPLETE)
  ENDIF  
  ROW = ROW + 1
  =lfPrnHdr()
  @ ROW , 000 SAY SpcInst
  @ ROW , 008 SAY Order
  @ ROW , 015 SAY STRTRAN(STYLE,SUBSTR(STYLE,lnNonMajSt,LEN(lcNonMajPi)))
  @ ROW , 028 SAY LEFT(Style.Desc,16)
  @ ROW , 045 SAY SUBSTR(STYLE,lnNonMajSt,LEN(lcNonMajPi))
  @ ROW , 052 SAY LEFT(gfCodDes(SUBSTR(STYLE,lnNonMajSt,LEN(lcNonMajPi)), 'COLOR'),9)
  lnPos = 063
  FOR lnIndex = 1 TO 7 
    lcIndex = STR(lnIndex,1)
    @ ROW , lnPos SAY Qty&lcIndex PICTURE '@Z 999999'
    lnPos = lnPos + 6
  ENDFOR
  @ ROW , 107 SAY TotQty PICTURE  '@Z 9999999'
  @ ROW , 117 SAY Price  PICTURE  '9999'
  @ ROW , 123 SAY TotQty*Price PICTURE '999999999'
  *C102123,4 AME Fix the bug of variable lcnotes not found.[Start] 
  *IF llRpOrdLnt .AND. lcNotes = 'Y'
  IF llRpOrdLnt
  *C102123,4 AME [End]
    IF !EMPTY(NOTE_MEM)
      ROW = ROW + 1
      =lfPrnHdr()
      @ ROW , 00 SAY 'ORDER NOTES  :'
      FOR X = 1 TO MEMLINES(NOTE_MEM)
        @ ROW,15 SAY MLINE(NOTE_MEM,X)
         ROW = ROW + 1
         =lfPrnHdr()
      ENDFOR
    ENDIF
  ENDIF
  lnGrndAmt = lnGrndAmt + TotQty*Price
  FOR lnIndex = 1 TO 7 
    lcIndex = STR(lnIndex,1)
    XTOTAL[lnIndex]     = XTOTAL[lnIndex]     + Qty&lcIndex
    laActTot[lnIndex]   = laActTot[lnIndex]   + Qty&lcIndex
    laStorTot[lnIndex]  = laStorTot[lnIndex]  + Qty&lcIndex
    laSpInsTot[lnIndex] = laSpInsTot[lnIndex] + Qty&lcIndex
    laGrpTot[lnIndex]   = laGrpTot[lnIndex]   + Qty&lcIndex
  ENDFOR
  laActTot[8]   = laActTot[8]   + TotQty
  laStorTot[8]  = laStorTot[8]  + TotQty
  laSpInsTot[8] = laSpInsTot[8] + TotQty
  laGrpTot[8]   = laGrpTot[8]   + TotQty
  XTOTAL[8]     = XTOTAL[8]     + TotQty
        
  laActTot[9]   = laActTot[9]   + TotQty*Price
  laStorTot[9]  = laStorTot[9]  + TotQty*Price
  laSpInsTot[9] = laSpInsTot[9] + TotQty*Price
  laGrpTot[9]   = laGrpTot[9]   + TotQty*Price
  XTOTAL[9]     = XTOTAL[9]     + TotQty*Price
ENDSCAN
=lfPrnHdr()

=lfPrnSub('laGrpTot')

=lfPrnHdr()

=lfPrnSub('laSpInsTot')

=lfPrnHdr()

=lfPrnSub('laStorTot')

=lfPrnHdr()

=lfPrnSub('laActTot')

=lfPrnHdr()

=lfPrnSub('XTOTAL')
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT CLEAR
DO ENDREPORT
SET DEVICE TO SCREEN 
RETURN

*!*************************************************************
*! Name      : lfPrnSub.  
*! Developer : AHMED Mohamed ELAnwer. (AME)
*! Date      : 04/30/2001
*! Purpose   : Function to print subtotal.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfPrnSub()
*!************************************************************
FUNCTION lfPrnSub
PARAMETER lcArrType

DO CASE
 CASE lcArrType = 'laGrpTot'
   lcText = 'GROUP  ' + lcSubGrp + '  SUBTOTAL'
 CASE lcArrType = 'laSpInsTot'
   lcText = 'ORDER TYPE  ' + lcSubSpcIns + '  SUBTOTAL'
 CASE lcArrType = 'laStorTot'
   lcText = 'STORE  ' + IIF(!EMPTY(lcSubStr),lcSubStr,'MAIN ACCOUNT') + '  SUBTOTAL'
 CASE lcArrType = 'laActTot'
   lcText = 'ACCT.  ' + lcLoopAcct + '  SUBTOTAL'
 CASE lcArrType = 'XTOTAL'
   lcText = 'GRAND TOTAL  : '
ENDCASE
ROW = ROW + 1
@ ROW,000 SAY REPLICATE('-',132)
ROW = ROW + 1
@ ROW,000 SAY lcText
lnPos = 063
FOR lnIndex = 1 TO 7 
  @ ROW , lnPos SAY &lcArrType[lnIndex] PICTURE  '@Z 999999'
  lnPos = lnPos + 6
ENDFOR
@ ROW , 107 SAY &lcArrType[8] PICTURE  '@Z 9999999'
@ ROW , 123 SAY IIF(lcArrType = 'XTOTAL',lnGrndAmt,&lcArrType[9]) PICTURE '999999999'
lnSubAmt  = 0
ROW = ROW + 1
@ ROW,000 SAY REPLICATE('-',132)


*!*************************************************************
*! Name      : lfPrnHdr.  
*! Developer : AHMED Mohamed ELAnwer. (AME)
*! Date      : 04/26/2001
*! Purpose   : Function to print header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : =lfPrnHdr()
*!************************************************************
FUNCTION lfPrnHdr

IF ROW >=54
  PAGENO = PAGENO +1
  DO RPT_HDR WITH 'SOTAM100',''+ lcRpMsg1,R_WIDTH
  @ 05,000 SAY 'SI      ORDER  STYLE        DESCRIPTION      COLOR  DESCRIPTION  (1)   (2)   (3)   (4)   (5)   (6)   (7)   PIECES   PRICE      TOTAL.'
  @ 06,000 SAY REPLICATE('-',132)
  ROW = 7
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED Mohamed ELAnwer. (AME)
*! Date      : 04/26/2001
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
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'
  lnVarbEnd = 0

  DECLARE laRpSource[5]
  
  IF TYPE('laRpTarget[1]') = 'C'
    IF EMPTY(laRpTarget[1])
      DECLARE laRpTarget[1]
    ELSE
      FOR lnI = 2 TO ALEN(laRpTarget)
        IF TYPE('laRpTarget[lnI]') = 'U'
          laRpTarget[lnI] = ""
        ENDIF
      ENDFOR    
    ENDIF
  ELSE
    DECLARE laRpTarget[1]
  ENDIF
  
  STORE 'Bid'      TO laRpSource[1]  
  STORE 'Open'     TO laRpSource[2]
  STORE 'Hold'     TO laRpSource[3]
  STORE 'Complete' TO laRpSource[4]
  STORE 'Canceled' TO laRpSource[5]
  lcRpStatus = 'BOHCX'



  SET ORDER TO ORDHDR IN ORDHDR      && To use it to validate ORDER   # in option grid.
  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.

  IF 'MA' $ gcCmpModules
    SET ORDER TO FABRIC IN FABRIC      && To use it to validate FABRIC  # in option grid.
  ENDIF  
  
ENDIF
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Ahmed Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfNMajType
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 
*!*************************************************************
*! Name      : lfSROrder
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
DO CASE
  CASE lcParm = 'S'

    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.

ENDCASE
*-- end of lfsChOrder.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF


lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]


*-- end of lfvOStatus.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*
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
*! Name      : lfvSelcBy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : In this function we want to know old value to disable
*!           : last object, and we transfer it to its corressponding 
*!           : character because option grid returns its item number in popup. 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy
lcDummy = "Y"
llChSelect = .T.

llClearAcc = (lcRpSelcBy # 'A')
llClearSty = (lcRpSelcBy # 'S')
llClearFab = (lcRpSelcBy # 'F')

CLEAR READ

*!**************************************************************************
*! Name      : lfwOldSelc
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : To store the Old Select By value
*!**************************************************************************
*! Example   : = lfwOldSelc()
*!**************************************************************************
FUNCTION lfwOldSelc
lcOldSelc = lcRpSelcBy
*-- End of lfwOldSelc.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.
*!*************************************************************
*! Name      : lfArrDumy
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
*! Purpose   : Fill Sort and select arrays
*!*************************************************************
*! Example   : = lfArrDumy()
*!*************************************************************
*B802418,1 Adjust array dimensions
*-- Function to fill select by and sort by arrays.
FUNCTION lfArrDumy
PRIVATE lnSelElms 
lnSelElms = 3

DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1]




*-- Fill Select by array base elements. [Begin]
laSlctDesc[1,1] = lcStyMajor 
laSlctDesc[2,1] = 'Account'
laSlctDesc[3,1] = 'All'

laSlctVal[1,1]  = 'S'
laSlctVal[2,1]  = 'A'
laSlctVal[3,1]  = 'L'
*-- Fill Select by array base elements. [End  ]


IF 'MA' $ gcCmpModules
  lnSelElms = lnSelElms + 1
  DIMENSION laSlctDesc[lnSelElms,1],laSlctVal[lnSelElms,1]

  lnInsFabIn = ASCAN(laSlctDesc,'All',1)
  =AINS(laSlctDesc,lnInsFabIn,1)
  =AINS(laSlctVal,lnInsFabIn,1)
  STORE 'Fabric' TO laSlctDesc[lnInsFabIn,1]
  STORE 'F'      TO laSlctVal[lnInsFabIn,1]
ENDIF


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
llClearFn = .T.
*-- Close temp. opended files, if its used.

IF USED(lcTempLine)
  USE IN (lcTempLine)

  IF FILE(gcWorkDir+lcTempLine+'.DBF') 
    ERASE(gcWorkDir+lcTempLine+'.DBF')
  ENDIF

  IF FILE(gcWorkDir+lcTempLine+'.CDX') 
    ERASE(gcWorkDir+lcTempLine+'.CDX')
  ENDIF

  IF FILE(gcWorkDir+lcTempLine+'.FPT') 
    ERASE(gcWorkDir+lcTempLine+'.FPT')
  ENDIF
ENDIF

*IF lcRpSelcBy = 'A'
*  IF FILE(gcWorkDir+lcTmpIndx+'.CDX') 
*    ERASE(gcWorkDir+lcTmpIndx+'.CDX')
*  ENDIF
*ENDIF
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*! Name      : lfItmPos
*! Developer : Ahmed  Mohamed ElAnwar (AME)
*! Date      : 04/29/2001
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
*
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
*! Name      : LFVStore
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/08/2001
*! Purpose   : Brow the Account inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = LFVStore()
*!*************************************************************
*!*B804478,1 
FUNCTION LFVStore

PRIVATE lcAliasStr

lcAliasStr = ALIAS()
STORE SPACE(0) TO lcExpStr, lcAccStore

lcTempStr = gfTempName()

CREATE CURSOR (lcTempStr) (AccStore C(13) )
ZAP
INDEX ON AccStore TAG (lcTempStr) OF (gcWorkDir+lcTempStr+'.CDX')

lcBrowFlds = [ACCOUNT     :H = 'Acct #'       :10 ,]   + ;
             [STORE       :H = 'store' :10 ,]    + ;
             [BTNAME      :H = 'Name'      :15 ,]    + ;
             [PHONE1      :H = 'Phone'    :15 ,]    + ;
             [NETBAL      :H = 'Balance'    :15 ,]    + ;
             [CADDRESS6   :H = 'Country'       :10 ]

*--THE TEMPORARY INDEX.
lcTmpIndx = gfTempName()
*--temp file for collecting data

SELECT CUSTOMER
INDEX ON  ACCOUNT+STORE TAG lcMmajour OF (gcWorkDir + lcTmpIndx+ '.CDX') 
SET ORDER TO TAG lcMmajour OF (gcWorkDir +lcTmpIndx  + '.CDX')

IF !USED(lcTmpFile)
  =lfCreatTmp()
ENDIF

SELECT (lcTmpFile)
LOCATE

=gfrange(lcBrowFlds,lcTempStr,"AccStore","","","","@! XXXXXXXXXXXXXX")
SELECT (lcTempStr)

DIMENSION laStylFle[1,7]
laStylFle[1,1]= "Account+Store"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempStr
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "Account+Store"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcAccStore = gfGenFlt('laStylFle',.T.,.T.)
lcAccStore = STRTRAN(lcAccStore,'Account+Store','CUSTOMER.ACCOUNT+CUSTOMER.STORE')

lcDumAcc  = lcAccStore 

DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

SELECT (lcAliasStr)
                     *-- End Function LFVSTORE --*


*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/08/2001
*! Purpose   : Brow the style inrange and modify the filter.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatTmp()
*!*************************************************************
*!*B804478,1 
FUNCTION lfCreatTmp


DIMENSION laTempStru[1,4]
laTempStru = ''

SELECT CUSTOMER
lnFildLen = AFIELDS(laTempStru)

  *-- Add Code fields.
DIMENSION laTempStru[lnFildLen + 1, 4]
laTempStru[lnFildLen + 1, 1] = 'AccStore'
laTempStru[lnFildLen + 1, 2] = 'C'
laTempStru[lnFildLen + 1, 3] = 13
laTempStru[lnFildLen + 1, 4] = 0

CREATE CURSOR (lcTmpFile)   FROM ARRAY laTempStru

SELECT (lcTmpFile)
*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON AccStore TAG (lcTmpFile) OF (gcWorkDir+lcTmpFile+'.CDX')

SELECT CUSTOMER
SCAN
  SCATTER MEMVAR MEMO
  m.AccStore = m.Account+m.Store
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN
