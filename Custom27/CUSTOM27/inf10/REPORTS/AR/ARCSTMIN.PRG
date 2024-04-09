*:**************************************************************************
*: Program file  : ARCSTMIN
*: Program desc. : Custom Customer Statement for Infinity Class
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 08/07/2002
*: Reference     : C102649
*:**************************************************************************
*: Calls : 
*:    Procedures : lpPrint
*:    Functions  : lfItmPos , gfGetAdr , lfAdrShift , gfGetMemVar
*:**************************************************************************
*: Example : DO ARCSTMIN
*:**************************************************************************
*

PRIVATE lndHighDate , ldLowDate , lnTranDate
DIMENSION laBtAddres[5,1]
STORE {} TO ldLowDate , ldHighDate

lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
IF lnTranDate > 0
  ldLowDate  = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],1,10))
  ldHighDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12))
ENDIF
ldLowDate  = IIF(EMPTY(ldLowDate),{01/01/01},ldLowDate)
ldHighDate = IIF(EMPTY(ldHighDate),DATE(),ldHighDate)

SET DEVICE TO PRINT
DO lpPrint           && main printing procedure
SET DEVICE TO SCREEN
RETURN
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/08/2002
*! Purpose   : To print all the @ Say
*!**************************************************************************
*! Example   : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
PRIVATE lnTotal, lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnCredit, ;
        lnAmount, llAgeMsg , lcGroupKey , lcAccount , lnPageNo , lnMaxPrgs , ;
        lnX , llGotAR        
lnAmount = 0
STORE '' TO lcGroupKey , laBtAddres
STORE 20 TO lnMaxPrgs , lnPageNo
llNewCust = .T.

DO WHILE INKEY() <> 32
  SELECT (lcTmpAcct)
  IF EOF()
    EXIT
  ENDIF

  IF llNewCust
    llAgeMsg = IIF(ABS(ldHighDate - Customer.AgeDate) <= 10, .T. , .F.)
    STORE 0 TO lnTotal, lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnCredit
 
    SELECT (lcTmpAcct)
    lcGroupKey = cGroupKey
  
    SELECT (lcTmpTrans)
    SEEK lcGroupKey
  
    SCAN REST WHILE cGroupKey = lcGroupKey
      IF lcAgeType = 'T'                    &&Aging by terms
        ldDueDate = IIF(EMPTY(DueDate), TranDate + 30, DueDate)  

        Days = ldHighDate - ldDueDate
        DO CASE               
          CASE Amount < 0             
            lnCredit = lnCredit + Amount  
          CASE Days >= 91             
            lnAge120 = lnAge120 + Amount  
          CASE Days >= 61             
            lnAge90 = lnAge90 + Amount    
          CASE Days >= 31 
            lnAge60 = lnAge60 + Amount    
          CASE Days >= 1              
            lnAge30 = lnAge30 + Amount    
          OTHERWISE                 
            lnCurrent = lnCurrent + Amount 
        ENDCASE                      
      ELSE                                  &&Aging by transaction date
        Days = ldHighDate - TranDate
        DO CASE
          CASE Amount < 0       
            lnCredit = lnCredit + Amount   
          CASE Days >= 120             
            lnAge120 = lnAge120 + Amount   
          CASE Days >= 90              
            lnAge90 = lnAge90 + Amount     
          CASE Days >= 60              
            lnAge60 = lnAge60 + Amount     
          CASE Days >= 30              
            lnAge30 = lnAge30 + Amount     
          OTHERWISE                  
            lnCurrent = lnCurrent + Amount 
        ENDCASE                              
      ENDIF   
    ENDSCAN 
  
    SEEK lcGroupKey
    SUM REST Amount TO XBEGIN WHILE cGroupKey = lcGroupKey AND TranDate < ldLowDate
    *-- See if there are more transactions
    llGotAR = IIF(cGroupKey = lcGroupKey AND TranDate <= ldHighDate, .T., .F.)  
  
    IF XBEGIN = 0 AND !llGotAR       && IF THERE ARE NO MORE TRANSACTIONS
      SELECT (lcTmpAcct)
      SKIP
      LOOP
    ENDIF
  
    lnTotal = XBEGIN
    PAGECT = 0
    SELECT (lcTmpAcct)
    llNewCust = .F.
  ENDIF

  *-- START PRINT
  IF lnPageNo= lnMaxPrgs
    lnHdrRow = 07                   && Start header row.
    lnLinRow = 19                   && Start lines printing row.
    lnBotRow = 36                   && Start footer row.
    MaxRow   = lnBotRow - 3         && Max row number for lines printing.
    lnPageNo = 0
  ELSE
    lnHdrRow = lnBotRow + 13        && Start header row.
    lnLinRow = lnHdrRow + 12        && Start lines printing row.
    lnBotRow = lnLinRow + 17        && Start footer row.
    MaxRow   = lnBotRow - 03        && Max row number for lines printing.
    lnPageNo = lnPageNo + 01
  ENDIF

  SELECT (lcTmpAcct)
  @ lnHdrRow,10 SAY DTOC(gdSysDate)
  @ lnHdrRow,21 SAY Account

  @ lnHdrRow,64 SAY DTOC(gdSysDate)
  @ lnHdrRow,75 SAY Account

  lnHdrRow = lnHdrRow + 03
  
  SELECT (lcTmpAcct)
  =lfGetAddr()
  *L4 = BTADDR2
  *L5 = TRIM(BTCITY)+'  '+BTSTATE+'  '+BTZIP
  *IF L4 = ' '
  *  L4 = L5
  *  L5 = ' '
  *ENDIF

  @ lnHdrRow,06 SAY BTNAME
  lnHdrRow = lnHdrRow + 1
  
  @ lnHdrRow,06 SAY laSoldTo[1]
  lnHdrRow = lnHdrRow + 1

  @ lnHdrRow,06 SAY laSoldTo[2]
  lnHdrRow = lnHdrRow + 1

  @ lnHdrRow,06 SAY laSoldTo[3]
  lnHdrRow = lnHdrRow + 2
  
  *-----------------------------------------------------------------
  * Determine which message to print on customer's statement
  *-----------------------------------------------------------------
  DO CASE
    CASE llAgeMsg AND lnAge120 > 0
      @ lnHdrRow+0,10 SAY M120L1
      @ lnHdrRow+1,10 SAY M120L2
    CASE llAgeMsg AND  lnAge90 > 0
      @ lnHdrRow+0,10 SAY M90L1
      @ lnHdrRow+1,10 SAY M90L2
    CASE llAgeMsg AND  lnAge60 > 0
      @ lnHdrRow+0,10 SAY M60L1
      @ lnHdrRow+1,10 SAY M60L2
    CASE llAgeMsg AND  lnAge30 > 0
      @ lnHdrRow+0,10 SAY M30L1
      @ lnHdrRow+1,10 SAY M30L2
  ENDCASE

  *-- Loop to print Account activity
  DO WHILE .T.
    SELECT (lcTmpTrans)

    DO WHILE &lcTmpTrans..cGroupKey = lcGroupKey AND lnLinRow <= MaxRow
      SELECT (lcTmpTrans)
      
      IF TranDate > ldHighDate
        COUNT REST TO lnX WHILE cGroupKey = lcGroupKey
        EXIT
      ENDIF
      lnTotal  = lnTotal  + Amount
      lnAmount = lnAmount + Amount
      Days = MAX(ldHighDate - TranDate, 0)
      @ lnLinRow,00 SAY TranDate
      @ lnLinRow,11 SAY Tran
      @ lnLinRow,18 SAY SUBSTR(Desc,1,13)
      @ lnLinRow,42 SAY Amount            PICTURE '999999.99'
      @ lnLinRow,52 SAY Amount            PICTURE '999999.99'
      @ lnLinRow,64 SAY Tran
      @ lnLinRow,71 SAY Amount            PICTURE '999999.99'
      SKIP
      lnLinRow = lnLinRow + 1
    ENDDO

    IF !(&lcTmpTrans..cGroupKey == lcGroupKey)
      llNewCust = .T.
    ELSE
      llNewCust = .F.
    ENDIF
    
    EXIT
  ENDDO

  *** do not forget to print the footer.
  IF lcAgeType = 'D'
    @ lnBotRow-1,02 SAY 'CURRENT     OVER30    OVER60    OVER120'
  ELSE
    @ lnBotRow-1,02 SAY 'CURRENT    (+1-30)  (+31-90)  (OVER-91)'
  ENDIF
  @ lnBotRow,01 SAY lnCurrent         PICTURE '999999.99'
  @ lnBotRow,12 SAY lnAge30           PICTURE '999999.99'
  @ lnBotRow,22 SAY lnAge60 + lnAge90 PICTURE '999999.99'
  @ lnBotRow,32 SAY lnAge120          PICTURE '999999.99'  
  @ lnBotRow,50 SAY lnTotal           PICTURE '9999999.99'
  @ lnBotRow,71 SAY lnTotal           PICTURE '9999999.99'
  
  IF llNewCust
    lnAmount = 0
    SELECT (lcTmpAcct)
    SKIP
  ENDIF

ENDDO
*-- End of lpPrint.