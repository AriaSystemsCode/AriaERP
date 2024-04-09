*:************************************************************************
*: Program file  : ARCSTMY.PRG
*: Program desc. : Customer statment for customer JOVANI FASHIONS.
*:                 A copy from CUS810a.prg with all proper modifications.
*:                 Cust. Prog. # 100572
*:         Module: Aria Apparel Series.
*:      Developer: Mohamed Fahmy Mohamed (MFM)
*:************************************************************************
*: Converted to A27 by Ashraf Sherif Mohammed (ASH) on 03/17/99
*: C#101483.
*:************************************************************************
*:B603780,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new 
*:B603780,1                Customer Statement for Jovani
*:************************************************************************
*
*B603780,1 Commented out [Begin]
*CusTemp    = gfTempName()
*ARTEMP     = gfTempName()
*B603780,1 Commented out [End]

STORE {} TO LDATE,HDATE
lnPos      = ASCAN(laOGVrFlt,'DEBIT.TRANDATE')
IF lnPos <> 0
  lnDatePos  = ASUBSCRIPT(laOGVrFlt,lnPos,1)
  lnPPos     = AT("|",laogvrflt[lnDatePos,6])
  LDATE      = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],1,lnPPos-1))
  HDATE      = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],lnPPos+1))
ENDIF
lDate      = IIF(EMPTY(LDATE),{01/01/01},LDATE)
HDATE      = IIF(EMPTY(HDATE),DATE(),HDATE)
TODAY      = DATE()

*B603780,1 Commented out [Begin]
*-- Selecting the records form lcDummy file that holds all the selected
*-- accounts. And setting the index according to the user selection.
*SELECT * FROM (lcDummy) INTO CURSOR (CusTemp) GROUP BY Account
*lcCurPath = STRTRAN(DBF(CusTemp) , '.TMP' , '.CDX')
*DO CASE
*  CASE lcRPSort = 'A'
*    INDEX ON ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (CusTemp) OF (lcCurPath)
*  CASE lcRPSort = 'C'
*    INDEX ON CADDRESS4+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (CusTemp) OF (lcCurPath)
*  CASE lcRPSort = 'Z'
*    INDEX ON CADDRESS5+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (CusTemp) OF (lcCurPath)
*  CASE lcRPSort = 'R'
*    INDEX ON REGION+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (CusTemp) OF (lcCurPath)
*ENDCASE
*B603780,1 Commented out [End]

*B603780,1 Make Relation with Customer File to get some data [Begin]
SELECT (lcTmpAcct)
SET RELATION TO
SET RELATION TO 'M' + Account INTO Customer ADDITIVE
*B603780,1 Make Relation with Customer File to get some data [End]

*B603780,1 Commented out [Begin]
*-- Selecting the transactions for all the selected accounts.
*SELECT * ;
*  FROM (lcTranFile);
*  WHERE !EMPTY(TranType);
*  INTO CURSOR(ArTemp)  
*lcCurPath = STRTRAN(DBF(ArTemp) , '.TMP' , '.CDX')  
*INDEX ON Account+DTOS(TranDate)+Tran+TranCode TAG (ArTemp) OF (lcCurPath)
*B603780,1 Commented out [End]

SET DEVICE TO PRINT
DO lpPrint

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Ashraf Sherif Mohammed (ASH)
*! Date      : 03/17/1999
*! Purpose   : Print the report
*!**************************************************************************
*! Example   : DO lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint

NEWCUST     = .T.
lnPageNo    = 20
lnMaxPrgs   = 20
lnAmount    = 0

DO WHILE INKEY() <>32
  
  *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
  *SELECT (CusTemp)
  SELECT (lcTmpAcct)
  *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]
  
  IF EOF()
    EXIT
  ENDIF

  *---------------------------------------------------------
  * Get 1st record in financial history file and initialize
  * work totals.
  *---------------------------------------------------------
  IF NEWCUST

    *B603780,1 Add a Customer alias to AgeDate [Begin]
    *AGEMSG  = IIF(ABS(HDATE-AGEDATE)<=10, .T., .F.)
    AGEMSG  = ABS(HDATE-Customer.AgeDate)<=10
    *B603780,1 Add a Customer alias to AgeDate [End]
    
    STORE 0.00 TO XBEGINDB, XBEGINCR, XTOTAL, XCURRENT, XAGE30, XAGE60, XAGE90, XAGE120, XCREDIT    &&DM03/23/92
 
    *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
    *SELECT &CusTemp
    *XACCOUNT = ACCOUNT
    lcGroupKey = &lcTmpAcct..cGroupKey
    *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]
  
    *B603780,1 Change Temp Trans File to match name in New Customer Statement [Begin]
    *SELECT &ARTEMP
    *SEEK XACCOUNT
    SELECT (lcTmpTrans)
    SEEK lcGroupKey
    *B603780,1 Change Temp Trans File to match name in New Customer Statement [End]
    
    *B603780,1 Commented out [Begin]
    *lcAgeType = ALLTRIM(gfGetMemvar('XAGINGTYPE'))
    *B603780,1 Commented out [End]
    
    *B603780,1 Change variable name in Scan loop [Begin]
    *          Scan Transaction table
    *SCAN WHILE ACCOUNT=XACCOUNT
    SCAN WHILE cGroupKey = lcGroupKey
    *B603780,1 Change variable name in Scan loop [End]

      IF lcAgeType = 'T'     &&Aging by terms
        **************************************************************
        XDUEDATE = IIF(EMPTY(DUEDATE), TRANDATE+30, DUEDATE)  
        DAYS=HDATE-XDUEDATE
        DO CASE               
          CASE AMOUNT<0             
            XCREDIT=XCREDIT+AMOUNT  
          CASE DAYS>=91             
            XAGE120=XAGE120+AMOUNT  
          CASE DAYS>=61             
            XAGE90=XAGE90+AMOUNT    
          CASE DAYS>=31 
            XAGE60=XAGE60+AMOUNT    
          CASE DAYS>=1              
            XAGE30=XAGE30+AMOUNT    
          OTHERWISE                 
            XCURRENT=XCURRENT+AMOUNT 
        ENDCASE                      
      
      ELSE                                  &&Aging by transaction date
      
        DAYS = HDATE - TRANDATE
        DO CASE               
          CASE AMOUNT<0       
            XCREDIT=XCREDIT+AMOUNT   
          CASE DAYS>=120             
            XAGE120=XAGE120+AMOUNT   
          CASE DAYS>=90              
            XAGE90=XAGE90+AMOUNT     
          CASE DAYS>=60              
            XAGE60=XAGE60+AMOUNT     
          CASE DAYS>=30              
            XAGE30=XAGE30+AMOUNT     
          OTHERWISE                  
            XCURRENT=XCURRENT+AMOUNT 
        ENDCASE                              
      ENDIF   
    ENDSCAN 
  
    *B603780,1 Change variable to Hold cGroupKey instead of Account [Begin]
    *SEEK XACCOUNT
    *SUM REST AMOUNT TO XBEGIN WHILE ACCOUNT=XACCOUNT .AND.TRANDATE<LDATE
    ****** SEE IF THERE ARE MORE TRANSACTIONS
    *GOTAR = IIF(ACCOUNT=XACCOUNT.AND.TRANDATE<=HDATE, .T., .F.)

    SEEK lcGroupKey
    SUM REST AMOUNT TO XBEGIN WHILE cGroupKey = lcGroupKey .AND.TRANDATE<LDATE
    GOTAR = IIF(cGroupKey = lcGroupKey .AND. TRANDATE<=HDATE, .T., .F.)
    *B603780,1 Change variable to Hold cGroupKey instead of Account [End]
  
    IF XBEGIN=0 .AND. !GOTAR       && IF THERE ARE NO MORE TRANSACTIONS

      *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
      *SELECT (CusTemp)
      SELECT (lcTmpAcct)
      *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]
    
      SKIP
      LOOP
    ENDIF
  
    XTOTAL = XBEGIN
    PAGECT = 0

    *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
    *SELECT &CusTemp
    SELECT (lcTmpAcct)
    *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]
  
    NEWCUST = .F.
  ENDIF

  *-- START PRINT
  IF lnPageNo= lnMaxPrgs
    lnHdrRow = 05                   && Start header row.
    lnLinRow = 23                   && Start lines printing row.
    lnBotRow = 41                   && Start footer row.
    MaxRow   = lnBotRow - 3         && Max row number for lines printing.
    lnPageNo = 0
  ELSE
    lnHdrRow = lnBotRow + 06        && Start header row.
    lnLinRow = lnHdrRow + 19        && Start lines printing row.
    lnBotRow = lnLinRow + 17        && Start footer row.
    MaxRow   = lnBotRow - 03        && Max row number for lines printing.
    lnPageNo = lnPageNo + 01
  ENDIF

  PAGECT = PAGECT +1

  @ lnHdrRow,40 SAY "PAGE: " + ALLTRIM(STR(PAGECT))
  lnHdrRow = lnHdrRow + 6
  
  *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
  *SELECT &CusTemp
  SELECT (lcTmpAcct)
  *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]

  *B603780,1 Change file to Customer File [Begin]
  *lcAddr1 = gfGetAdr(CusTemp , '' , '' , '' , 1)
  *lcAddr2 = gfGetAdr(CusTemp , '' , '' , '' , 2)
  *lcAddr3 = gfGetAdr(CusTemp , '' , '' , '' , 3)
  *lcAddr4 = gfGetAdr(CusTemp , '' , '' , '' , 4)
  *lcAddr5 = gfGetAdr(CusTemp , '' , '' , '' , 5)

  lcAddr1 = gfGetAdr('Customer' , '' , '' , '' , 1)
  lcAddr2 = gfGetAdr('Customer' , '' , '' , '' , 2)
  lcAddr3 = gfGetAdr('Customer' , '' , '' , '' , 3)
  lcAddr4 = gfGetAdr('Customer' , '' , '' , '' , 4)
  lcAddr5 = gfGetAdr('Customer' , '' , '' , '' , 5)
  *B603780,1 Change file needed to Customer File [End]
  
  L4 = lcAddr2
  L5 = TRIM(lcAddr3)+'  '+TRIM(lcAddr4)+'  '+TRIM(lcAddr5)
  IF L4 = ' '
    L4 = L5
    L5 = ' '
  ENDIF

  @ lnHdrRow,10 SAY DTOC(TODAY)
  @ lnHdrRow,22 SAY ACCOUNT

  @ lnHdrRow,64 SAY DTOC(TODAY)
  @ lnHdrRow,76 SAY ACCOUNT
  lnHdrRow = lnHdrRow + 4

  @ lnHdrRow,10 SAY BTNAME
  lnHdrRow = lnHdrRow + 1
  
  @ lnHdrRow,10 SAY lcAddr1
  lnHdrRow = lnHdrRow + 1

  @ lnHdrRow,10 SAY L4
  lnHdrRow = lnHdrRow + 1

  @ lnHdrRow,10 SAY L5
  lnHdrRow = lnHdrRow + 2
  
  *-----------------------------------------------------------------
  * Determine which message to print on customer's statement
  *-----------------------------------------------------------------
  DO CASE
    CASE AGEMSG .AND. xAGE120 > 0
      @ lnHdrRow+0,10 SAY M120L1
      @ lnHdrRow+1,10 SAY M120L2
    CASE AGEMSG .AND.  xAGE90 > 0
      @ lnHdrRow+0,10 SAY M90L1
      @ lnHdrRow+1,10 SAY M90L2
    CASE AGEMSG .AND.  xAGE60 > 0
      @ lnHdrRow+0,10 SAY M60L1
      @ lnHdrRow+1,10 SAY M60L2
    CASE AGEMSG .AND.  xAGE30 > 0
      @ lnHdrRow+0,10 SAY M30L1
      @ lnHdrRow+1,10 SAY M30L2
  ENDCASE

  ******* LOOP TO PRINT ACCOUNT ACTIVITY
  DO WHILE .T.
    *B603780,1 Change Temp Trans File to match name for New Customer Statement [Begin]
    *SELECT &ARTEMP
    SELECT (lcTmpTrans)
    *B603780,1 Change Temp Trans File to match name for New Customer Statement [End]

    *B603780,1 Change Temp Trans File to match name for New Customer Statement [Begin]
    *DO WHILE &ARTEMP..ACCOUNT = XACCOUNT .AND. lnLinRow <= MaxRow
      *SELECT &ARTEMP
    DO WHILE &lcTmpTrans..cGroupKey = lcGroupKey .AND. lnLinRow <= MaxRow
      SELECT (lcTmpTrans)
    *B603780,1 Change Temp Trans File to match name for New Customer Statement [End]

      IF TRANDATE > HDATE
        
        *B603780,1 Change Variable to cGroupKey instead of Account [Begin]
        *COUNT REST TO X WHILE ACCOUNT=XACCOUNT
        COUNT REST TO X WHILE cGroupKey = lcGroupKey
        *B603780,1 Change Variable to cGroupKey instead of Account [End]
        
        EXIT
      ENDIF

      XTOTAL   = XTOTAL   + AMOUNT
      lnAmount = lnAmount + Amount
      DAYS     = MAX(HDATE-TRANDATE, 0)
      PDATE    = TRANDATE
      @ lnLinRow,01 SAY PDATE
      @ lnLinRow,10 SAY TRAN
      @ lnLinRow,19 SAY SUBSTR(DESC,1,20)
      @ lnLinRow,43 SAY AMOUNT            PICTURE '999999.99'
      @ lnLinRow,53 SAY lnAmount          PICTURE '999999.99'
      @ lnLinRow,65 SAY TRAN
      @ lnLinRow,72 SAY lnAmount          PICTURE '999999.99'

      SKIP
      lnLinRow = lnLinRow + 1
    ENDDO

    *B603780,1 Replace if..Endif with one line [Begin]
    *IF &ARTEMP..ACCOUNT<>XACCOUNT
    *  NEWCUST = .T.
    *ELSE
    *  NEWCUST = .F.
    *ENDIF
    NEWCUST = (&lcTmpTrans..cGroupKey <> lcGroupKey)
    *B603780,1 Replace if..Endif with one line [End]
        
    EXIT
  ENDDO

  *** do not forget to print the footer.
  IF lcAgeType = 'D'
    @ lnBotRow-1,02 SAY 'CURRENT     OVER30    OVER60    OVER120'
  ELSE
    @ lnBotRow-1,02 SAY 'CURRENT    (+1-30)  (+31-90)  (OVER-91)'
  ENDIF
  @ lnBotRow,01 SAY XCURRENT          PICTURE '99999.99'
  @ lnBotRow,12 SAY XAGE30            PICTURE '99999.99'
  @ lnBotRow,22 SAY XAGE60 + XAGE90   PICTURE '99999.99'
  @ lnBotRow,32 SAY XAGE120           PICTURE '999999.99'
  @ lnBotRow,52 SAY XTOTAL            PICTURE '9999999.99'
  @ lnBotRow,71 SAY XTOTAL            PICTURE '9999999.99'
  
  IF NEWCUST
    lnAmount = 0

    *B603780,1 Change Temp Customer File to match name for New Customer Statement [Begin]
    *SELECT (CusTemp)
    SELECT (lcTmpAcct)
    *B603780,1 Change Temp Customer File to match name for New Customer Statement [End]

    SKIP
  ENDIF

ENDDO
SET DEVICE TO SCREEN
RETURN