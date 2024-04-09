*:**************************************************************************
*: Program file  : ARCSTMXE
*: Program desc. : Custom Customer Statement for XES
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 12/10/2000
*: Reference     : C102102
*:**************************************************************************
*: Calls : 
*:    Procedures : lpPrint
*:    Functions  : lfItmPos , gfGetAdr , lfAdrShift , gfGetMemVar
*:**************************************************************************
*: Example : DO ARCSTMXE
*:**************************************************************************
*B606553,1 SSE 10/20/2002 Fix bug of printing Factor addresses from Option Grid not from customer file.

PRIVATE llNewCust , lcGroupKey , lcAccount , lnTotal , ldHighDate , ;
        ldLowDate , ldThruDate , lnTranDate , lnPageNo , lnBegin , ;
        llFactored , lcFacComp , lcDunsNo , lcBillAdr3 , llAgeMsg , ;
        lnX

lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
IF lnTranDate > 0
  ldLowDate  = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],1,10))
  ldHighDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12))
ENDIF
ldLowDate  = IIF(EMPTY(ldLowDate),{01/01/01},ldLowDate)
ldHighDate = IIF(EMPTY(ldHighDate),DATE(),ldHighDate)
ldThruDate = DTOC(ldLowDate - 1)

DO lpPrint           && main printing procedure
RETURN
*-- End of Report.

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/10/2000
*! Purpose   : To print all the @ Say
*!**************************************************************************
*! Example   : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
llNewCust = .T.
lcDunsNo  = gfGetMemVar('XDUNS')

SET DEVICE TO PRINT
DO WHILE INKEY() <> 32
  SELECT (lcTmpAcct)
  IF EOF()
    EXIT
  ENDIF

  *---------------------------------------------------------
  * Get 1st record in financial history file and initialize
  * work totals.
  *---------------------------------------------------------
  IF llNewCust
    lnTotal = 0

    lcGroupKey = &lcTmpAcct..cGroupKey
    lcAccount  = &lcTmpAcct..Account

    =SEEK('M' + lcAccount,'Customer')
    llAgeMsg  = IIF(ABS(ldHighDate - Customer.AgeDate)<=10, .T., .F.)

    SELECT (lcTmpTrans)
    SEEK lcGroupKey
    SUM REST Amount TO lnBegin WHILE cGroupKey = lcGroupKey AND TranDate < ldLowDate

    SELECT (lcTmpAcct)
    IF lnBegin = 0 AND !(cGroupKey = lcGroupKey)
      SKIP
      LOOP
    ENDIF
      
    lnTotal = lnBegin
    lnPageNo = 0
      
    llFactored = .F.
    STORE '' TO lcFacComp, laSoldTo          && Array laSoldTo defined in main program ArCStm
    
    *B606553,1 print Factor addresses from Option Grid not from customer file. [Begin]
    *IF llRpRemit AND !EMPTY(cFacCode)
    IF llRpRemit AND !EMPTY(lcRpFactor)
    *B606553,1 print Factor addresses from Option Grid not from customer file. [End]
    
      llFactored = .T.
      
      *B606553,1 print Factor addresses from Option Grid not from customer file. [Begin]
      *IF SEEK(cFacCode,'SycFact')
      IF SEEK(lcRpFactor,'SycFact')
      *B606553,1 print Factor addresses from Option Grid not from customer file. [End]
      
        lcFacComp = SycFact.cFacComp
        = gfGetAdr('SycFact', '' , '' , '' , @laSoldTo , 1)
      ENDIF
    ENDIF
    =lfAdrShift('laSoldTo')        && function in ArCStm
    llNewCust = .F.
  ENDIF

  lnPageNo = lnPageNo +1
  @ 01,03 SAY IIF(llPrntComp,lcName,'')
  @ 01,45 SAY IIF(llFactored, "** PLEASE REMIT PAYMENTS TO:", '')

  @ 02,03 SAY IIF(llPrntComp,laCompany[1],'')
  @ 02,45 SAY lcFacComp

  @ 03,03 SAY IIF(llPrntComp,laCompany[2],'')
  @ 03,45 SAY laSoldTo[1]

  @ 04,03 SAY IIF(llPrntComp,laCompany[3],'')
  @ 04,45 SAY laSoldTo[2]

  @ 05,03 SAY IIF(llPrntComp,TRANSFORM(lcPhone , gfPhoneTem()),'')   && variable from ArCStm
  @ 05,45 SAY laSoldTo[3]

  @ 06,03 SAY IIF(llPrntComp,lcDunsNo,'')
  lcBillAdr3 = ALLTRIM(SUBSTR(Customer.cAddress32,1,15)) + '  ' + ;
               ALLTRIM(SUBSTR(Customer.cAddress42,1,3)) + '  ' + ;
               ALLTRIM(SUBSTR(Customer.cAddress52,1,10))

  ROW = 13
  @ ROW,10 SAY lcAccount
  IF llPrntComp
    @ ROW,50 SAY 'S T A T E M E N T'
  ENDIF

  ROW = ROW + 1
  @ ROW,10 SAY BtName
  ROW = ROW + 1

  @ ROW,10 SAY Customer.cAddress12
  @ ROW,50 SAY "PAGE: "
  @ ROW,56 SAY lnPageNo PICTURE '999'
  ROW = ROW + 1

  @ ROW,10 SAY Customer.cAddress22
  ROW = ROW + 1

  @ ROW,10 SAY lcBillAdr3
  @ ROW,50 SAY "ENDING "
  @ ROW,59 SAY ldHighDate
  ROW = ROW + 1

  *-----------------------------------------------------------------
  * Determine which message to print on customer's statement
  *-----------------------------------------------------------------
  ROW = 20
  DO CASE
    CASE llAgeMsg AND Age120 > 25
      @ ROW+0,10 SAY M120L1
      @ ROW+1,10 SAY M120L2
      @ ROW+2,10 SAY M120L3
    CASE llAgeMsg AND Age90 > 25
      @ ROW+0,10 SAY M90L1
      @ ROW+1,10 SAY M90L2
      @ ROW+2,10 SAY M90L3
    CASE llAgeMsg AND Age60 > 25
      @ ROW+0,10 SAY M60L1
      @ ROW+1,10 SAY M60L2
      @ ROW+2,10 SAY M60L3
    CASE llAgeMsg AND Age30 > 25
      @ ROW+0,10 SAY M30L1
      @ ROW+1,10 SAY M30L2
      @ ROW+2,10 SAY M30L3
   ENDCASE

   IF llPrntComp
     @ 24,00 SAY REPLICATE('-',80)
     @ 25,00 SAY 'TRAN#  DATE      .....DESC.....  REFERENCE    STORE#           AGE       AMOUNT'
     @ 26,00 SAY REPLICATE('-',80)
   ENDIF

   ROW = 27
   IF lnPageNo = 1 AND lnBegin <>0
     @ ROW,09 SAY 'ACCOUNT SUMMARY THRU &ldThruDate'
     @ ROW,68 SAY lnBegin PICTURE '999999999.99'
     ROW = ROW +1
   ENDIF

   *-- LOOP TO PRINT ACCOUNT ACTIVITY
   SELECT (lcTmpTrans)
   DO WHILE .T.

     DO WHILE cGroupKey = lcGroupKey AND ROW <= 52
       IF TranDate > ldHighDate
         COUNT REST TO lnX WHILE cGroupKey = lcGroupKey
         EXIT
       ENDIF

       lnTotal = lnTotal + AMOUNT

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*TRAN#    DATE   .....DESC.....  REFERENCE     STORE#      CHGBACK      AMOUNT
*123456 MM/DD/YY 123456789012345 1234567890123 12345678 1234567.99  1234567.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

       @ ROW,00 SAY Tran
       @ ROW,07 SAY TranDate               
       @ ROW,17 SAY SUBSTR(Desc,1,9)
       @ ROW,34 SAY SUBSTR(Reference,1,10)
       @ ROW,46 SAY Store
       @ ROW,55 SAY Amount PICTURE '@Z 999999999.99'
       @ ROW,68 SAY Amount PICTURE '@Z 999999999.99'
       SKIP
       ROW = ROW + 1
     ENDDO

     IF !(cGroupKey == lcGroupKey)
       llNewCust = .T.
     ENDIF

     EXIT
   ENDDO
   *---------------------- END DISPLAY LOOP -----------------------

   @ 53,00 SAY REPLICATE('-',80)
   @ 54,02 SAY "PRINTED ON " + DTOC(DATE())

   IF llNewCust
     @ 54,45 SAY 'BALANCE ->'
     @ 54,68 SAY lnTotal PICTURE '99999999.99'
   ELSE
     @ 54,50 SAY 'C O N T I N U E D'
   ENDIF

   @ 55,00 SAY REPLICATE('-',80)

   IF llNewCust
     SELECT (lcTmpAcct)
     SKIP
   ENDIF

   @ 57,10 SAY MADL1
   @ 58,10 SAY MADL2
   @ 59,10 SAY MADL3
   @ 60,10 SAY MADL4
   @ 61,10 SAY MADL5
ENDDO
SET DEVICE TO SCREEN
*-- End of lpPrint