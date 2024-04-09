****************************************************************************
*: Program file      : ARCSTMPH.PRG
*: Program desc.     : Customized Customer Statements for Phenix. (C102696)
*: System            : Aria Apparel System (A27).
*: Module            : Account Receivable. (AR) 
*: Developer         : Sameh Saiid Ezzat (SSE)
*: Date              : 09/15/2002
*:**************************************************************************
*: Procedures    : lpPrint
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*B606552,1 SSE 10/20/2002 Fix bug of printing Factor addresses from Option Grid not from customer file.

*-- Declaring the necessary variables
PRIVATE ldHighDate , ldLowDate , lnTranDate , llFactored , lcDuns , ;
        lcFacComp , lnMaxRow , lcTranCode , lcCodesTag , lnCurrUnit ,;
        lnExRate

lnCurrUnit = 1
lnExRate = gfChkRate('lnCurrUnit' , gcBaseCurr , gdSysDate , .T. , gcAct_Comp , .F. , .T.)

lnMaxRow = 50        
lcDuns  = gfGetMemVar('XDUNS')
STORE {} TO ldLowDate , ldHighDate

lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
IF lnTranDate > 0
  ldLowDate  = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],1,10))
  ldHighDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12))
ENDIF
ldLowDate  = IIF(EMPTY(ldLowDate),{01/01/01},ldLowDate)
ldHighDate = IIF(EMPTY(ldHighDate),DATE(),ldHighDate)

lcCodesTag = ORDER('Codes')
SET ORDER TO TAG cCode_No IN Codes
lcTranCode = IIF(SEEK('DTRANCODE','Codes'),Codes.cCode_No,'')
SET ORDER TO TAG (lcCodesTag) IN Codes 

SET DEVICE TO PRINT
DO lpPrint           && main printing procedure
SET DEVICE TO SCREEN
RETURN
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/15/2002
*! Purpose   : To print all the @ Say
*!**************************************************************************
*! Example   : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint

PRIVATE lnTotal , lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
        lnCredit , llAgeMsg , lcGroupKey , lcAccount , lnPageNo , lnX , ;
        llGotAR , lnTotInt , lnTotInts , lnBegin , llDoneAdj        
lcGroupKey = ''
llNewCust = .T.
  
DO WHILE INKEY() <> 32
  SELECT (lcTmpAcct)
  IF EOF()
    EXIT
  ENDIF
  
  IF llNewCust
    llAgeMsg = IIF(ABS(ldHighDate - Customer.AgeDate) <= 10, .T. , .F.)
    STORE 0.00 TO lnTotal, lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnCredit    
    STORE 0.00 TO lnTotInt, lnTotInts
     
    SELECT (lcTmpAcct)
    lcGroupKey = cGroupKey
  
    SELECT (lcTmpTrans)
    SEEK lcGroupKey
    
    SCAN REST WHILE cGroupKey + STR(nGroup,6) + IIF(llMulCurr AND lcRpCurr="F",cCurrCode,"") + DTOS(TranDate) + Tran = lcGroupKey
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
    IF lcRpIntChg = 'Y'
      lnTotInt = lnAge30 + lnAge60 + lnAge90 + lnAge120 
    ENDIF
    
    SEEK lcGroupKey
    SUM REST Amount TO lnBegin WHILE cGroupKey + STR(nGroup,6) + ;
             IIF(llMulCurr AND lcRpCurr="F",cCurrCode,"") + DTOS(TranDate) + ;
             Tran = lcGroupKey AND TranDate < ldLowDate
    
    *-- See if there are more transactions
    llGotAR = IIF(cGroupKey = lcGroupKey AND TranDate <= ldHighDate, .T., .F.)  
  
    IF lnBegin = 0 AND !llGotAR       && IF THERE ARE NO MORE TRANSACTIONS
      SELECT (lcTmpAcct)
      SKIP
      LOOP
    ENDIF

    lnPageNo = 0
    SELECT (lcTmpAcct)

    STORE '' TO lcFacComp , laSoldTo          && Array laSoldTo defined in main program ArCStm
    llFactored = .F.
    
    *B606552,1 print Factor addresses from Option Grid not from customer file. [Begin]
    *IF llRpRemit AND !EMPTY(cFacCode)
    IF llRpRemit AND !EMPTY(lcRpFactor)
    *B606552,1 print Factor addresses from Option Grid not from customer file. [End]
    
      llFactored = .T.

      *B606552,1 print Factor addresses from Option Grid not from customer file. [Begin]
      *IF SEEK(cFacCode,'SycFact')
      IF SEEK(lcRpFactor,'SycFact')
      *B606552,1 print Factor addresses from Option Grid not from customer file. [End]
      
        lcFacComp = SycFact.cFacComp
        = gfGetAdr('SycFact', '' , '' , '' , @laSoldTo , 1)
      ENDIF
    ENDIF
    =lfAdrShift('laSoldTo')        && function in ArCStm        
    llNewCust = .F.
  ENDIF

  lnPageNo = lnPageNo + 1
  @ 01,03 SAY IIF(llPrntComp,lcName,'')
  @ 01,45 SAY IIF(llFactored, "** PLEASE REMIT PAYMENTS TO:", '')
  @ 02,03 SAY IIF(llPrntComp,laCompany[1],'')
  @ 02,45 SAY IIF(llFactored, lcFacComp , '')
  @ 03,03 SAY IIF(llPrntComp,laCompany[2],'')
  @ 03,45 SAY IIF(llFactored, laSoldTo[1], '')
  @ 04,03 SAY IIF(llPrntComp,laCompany[3],'')
  @ 04,45 SAY IIF(llFactored, laSoldTo[2], '')
  @ 05,03 SAY IIF(llPrntComp,TRANSFORM(lcPhone , gfPhoneTem()),'')
  @ 05,45 SAY IIF(llFactored, laSoldTo[3], '')
  @ 06,03 SAY IIF(llPrntComp,lcDuns,'')

  SELECT (lcTmpAcct)
  STORE '' TO laSoldTo
  IF SEEK('M'+Account,'Customer')
    = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , 1)
  ENDIF
  =lfAdrShift('laSoldTo')

  ROW = 13
  @ ROW,10 SAY Account
  @ ROW,50 SAY 'S T A T E M E N T'
  ROW = ROW + 1

  @ ROW,10 SAY BtName
  ROW = ROW+1
  @ ROW,10 SAY laSoldTo[1]
  @ ROW,50 SAY "PAGE: "
  @ ROW,56 SAY lnPageNo PICTURE '999'
  ROW = ROW + 1

  @ ROW,10 SAY laSoldTo[2]
  ROW = ROW + 1

  @ ROW,10 SAY laSoldTo[3]
  @ ROW,50 SAY "ENDING     " + DTOC(ldHighDate)
  ROW = ROW + 1
  @ ROW,50 SAY "PRINTED ON "+DTOC(gdSysDate)

  *-----------------------------------------------------------------
  * Determine which message to print on customer's statement
  *-----------------------------------------------------------------
  ROW = 20
  DO CASE
    CASE llAgeMsg .AND. lnAge120>0
      @ ROW+0,10 SAY M120L1
      @ ROW+1,10 SAY M120L2
      @ ROW+2,10 SAY M120L3
    CASE llAgeMsg .AND. lnAge90>0
      @ ROW+0,10 SAY M90L1
      @ ROW+1,10 SAY M90L2
      @ ROW+2,10 SAY M90L3
    CASE llAgeMsg .AND. lnAge60>0
      @ ROW+0,10 SAY M60L1
      @ ROW+1,10 SAY M60L2
      @ ROW+2,10 SAY M60L3
    CASE llAgeMsg .AND. lnAge30>0
      @ ROW+0,10 SAY M30L1
      @ ROW+1,10 SAY M30L2
      @ ROW+2,10 SAY M30L3
  ENDCASE
  
  @ 24,00 SAY REPLICATE('-',80)
  @ 25,00 SAY 'TRAN#    DATE    .....DESC.....  REFERENCE    STR/CHK      AGE           AMOUNT'
  @ 26,00 SAY REPLICATE('-',80)

  ROW = 27
  IF lnPageNo = 1 AND lnBegin <> 0
    @ ROW,09 SAY 'ACCOUNT SUMMARY THRU ' + DTOC(ldLowDate-1)
    @ ROW,68 SAY lnBegin PICTURE '99999999.99'
    ROW = ROW +1
  ENDIF

  *-- LOOP TO PRINT ACCOUNT ACTIVITY
  DO WHILE .T.
    SELECT (lcTmpTrans)

    DO WHILE &lcTmpTrans..cGroupKey = lcGroupKey AND ROW <= lnMaxRow
      SELECT (lcTmpTrans)
      IF TranDate > ldHighDate
        COUNT REST TO lnX WHILE cGroupKey = lcGroupKey
        EXIT
      ENDIF
      DAYS  = MAX(ldHighDate - TranDate, 0)
      @ ROW,00 SAY Tran
      @ ROW,07 SAY TranDate
      @ ROW,17 SAY SUBSTR(Desc,1,15)
      @ ROW,34 SAY SUBSTR(Reference,1,13)
      @ ROW,47 SAY Store
      @ ROW,59 SAY IIF(Amount>0, Days, 0) PICTURE '@Z 999'
      @ ROW,68 SAY Amount PICTURE '@Z 99999999.99'
      SKIP
      ROW = ROW+1
    ENDDO

    IF &lcTmpTrans..cGroupKey <> lcGroupKey
      llNewCust = .T.
    ENDIF
    EXIT
  ENDDO
  *---------------------- END DISPLAY LOOP -----------------------
  
  IF lcRpIntChg = 'Y'
    lnAge60  = ROUND((lnAge60 * (lnRpIntrst/100)) + lnAge60,2)
    lnAge90  = (lnAge90 * (lnRpIntrst/100)) + lnAge90
    lnAge90  = ROUND((lnAge90 * (lnRpIntrst/100)) + lnAge90,2)
    lnAge120 = (lnAge120 * (lnRpIntrst/100)) + lnAge120
    lnAge120 = (lnAge120 * (lnRpIntrst/100)) + lnAge120
    lnAge120 = ROUND((lnAge120 * (lnRpIntrst/100)) + lnAge120,2)
  ENDIF 

  lnTotal = lnCurrent + lnAge30 + lnAge60 + lnAge90 + lnAge120 + lnCredit
  IF lcRpIntChg = 'Y'
    lnTotInts = lnAge30 + lnAge60 + lnAge90 + lnAge120 - lnTotInt
  ENDIF
  IF llNewCust AND lcRpIntChg = 'Y'
    ROW = ROW + 1
    @ ROW,00 SAY "Total Interest Charges"
    @ ROW,68 SAY lnTotInts  PICTURE '99999999.99'
  ENDIF  

  *-- If user wants to update debit adjustment with this interest charge. 
  IF lcRpAddInt = 'Y' AND llNewCust
    llDoneAdj = .F.
    IF SEEK(&lcTmpAcct..Account,"Debit")
      SELECT Debit
      SCAN REST WHILE Account + Tran + cInstalNo + DTOS(TranDate) = &lcTmpAcct..Account ;
                FOR Trancode = lcTranCode AND TranDate = gdSysDate AND Desc = 'Interest Adjustment'
        llDoneAdj = .T.
        REPLACE Amount WITH lnTotInts 
      ENDSCAN 
    ENDIF
    IF !llDoneAdj AND lnTotInts > 0
      SELECT Debit
      APPEND BLANK
      REPLACE Account WITH &lcTmpAcct..Account,;
              TranType WITH "2"               ,;
              TranCode WITH lcTranCode        ,;
              Tran WITH gfSequence('DEBIT')   ,;
              TranDate WITH gdSysDate         ,;
              Desc WITH 'Interest Adjustment' ,;   
              DueDate WITH gdSysDate          ,;
              Amount WITH lnTotInts           ,;
              Batch WITH gfSequence('BATCH')  ,;
              cCurrCode WITH gcBaseCurr       ,;
              nCurrUnit WITH lnCurrUnit       ,;
              nExRate   WITH lnExRate
              =gfAdd_Info()        
    ENDIF          
  ENDIF
  
  @ IIF(llNewCust,53,51),00 SAY REPLICATE('-',80)
  IF llNewCust
    IF lcAgeType = 'D'
      @ 54,03 SAY 'CURRENT    OVER-30    OVER-60    OVER-90   OVER-120     CREDITS     BALANCE'
    ELSE
      @ 54,03 SAY 'CURRENT    (+1-30)    (+31-60)   (+61-90)  (OVER-91)    CREDITS     BALANCE'
    ENDIF
    @ 55,00 SAY lnCurrent  PICTURE '9999999.99'
    @ 55,11 SAY lnAge30    PICTURE '9999999.99'
    @ 55,22 SAY lnAge60    PICTURE '9999999.99'
    @ 55,33 SAY lnAge90    PICTURE '9999999.99'
    @ 55,44 SAY lnAge120   PICTURE '9999999.99'
    @ 55,56 SAY lnCredit   PICTURE '9999999.99'
    @ 55,67 SAY lnTotal    PICTURE '99999999.99'
  ELSE   
     @ 53,50 SAY 'C O N T I N U E D'
  ENDIF
  @ 56,00 SAY REPLICATE('-',80)
  
  IF llNewCust
    SELECT (lcTmpAcct)
    SKIP
  ENDIF

  @ 57,10 SAY SUBSTR(MADL1,1,60)
  @ 58,10 SAY SUBSTR(MADL2,1,60)
  @ 59,10 SAY SUBSTR(MADL3,1,60)
  @ 60,10 SAY SUBSTR(MADL4,1,60)
  @ 61,10 SAY SUBSTR(MADL5,1,60)
ENDDO
*-- End of lpPrint.
