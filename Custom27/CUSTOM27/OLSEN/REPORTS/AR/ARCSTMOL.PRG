**************************************************************************
*: Program file  : ARCSTMOL (Converted from 26 to 27. FOR OLSEN)
*: Program desc. : CUSTOMIZED CUSTOMER STATEMENT.
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*: Date          : 08/10/1999
*: Refer to      : (C101592)
****************************************************************************
*:B603782,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new 
*:B603782,1                Customer Statement for KOBE
*:B604957,1 BWA 09/25/2001 Fix the bug of printing the PDC and move it one line. 
*:B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.
*:**************************************************************************

*B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.[START]
*--Case the Multi currency only.
IF llMulCurr AND !USED(lcPostDc)
  =gfOpenFile(gcDataDir+"PostDChq","PostDChq", 'SH', @lcPostDc, .T.)
ENDIF  
*B804435,1 BWA 09/27/2001.[END]

lcTmpFact = SPACE(5)
llNewCust  = .T.
TODAY  = DATE()
*--Set the variables.
DIMENSION laAddress[1,1]
STORE ' ' TO laAddress,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
STORE ' ' TO lcFName,lcFAddr1,lcFAddr2,lcFAddr3,lcFAddr4,lcFAddr5,lcFAddr6 
STORE {} TO LDATE,HDATE

*B603782,1 Commented out [Begin]
*CusTemp    = gfTempName()
*ARTEMP     = gfTempName()
*B603782,1 Commented out [End]

lnPos     = ASCAN(laOGVrFlt,'DEBIT.TRANDATE')
*-- Get the ldate and hdate
IF lnPos <> 0
  lnDatePos = ASUBSCRIPT(laOGVrFlt,lnPos,1)
  lnPPos    = AT("|",laogvrflt[lnDatePos,6])
  LDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],1,lnPPos-1))
  HDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],lnPPos+1))
ENDIF
lDate  = IIF(EMPTY(LDATE),{01/01/01},LDATE)
HDATE = IIF(HDATE=CTOD('  /  /    '), DATE(), HDATE)
THRUDATE = DTOC(LDATE-1)
END      = HDATE
MAXROW   = 48

*B603782,1 Commented out (These 2 files are already opened in ArCStm [Begin]
*--  Open SyscFact to assure the existing of the factor
*=gfOpenFile(gcSysHome+'SycFact'  , gcSysHome+'cFacCode'  , 'SH')
*=gfOpenFile(gcDataDir+'PostDChq' ,gcDataDir+'PostDChq' ,'SH')

*-- Selecting the records form lcDummy file that holds all the selected
*-- accounts. And setting the index according to the user selection.
*SELECT * FROM (lcDummy) INTO CURSOR (lcCUSTEMP) GROUP BY Account
*DO CASE
*  CASE lcRPSort = 'A'
*    INDEX ON ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcCUSTEMP) OF (gcWorkDir+lcCUSTEMP)
*  CASE lcRPSort = 'C'
*    INDEX ON CADDRESS4+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcCUSTEMP) OF (gcWorkDir+lcCUSTEMP)
*  CASE lcRPSort = 'Z'
*    INDEX ON CADDRESS5+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcCUSTEMP) OF (gcWorkDir+lcCUSTEMP)
*  CASE lcRPSort = 'R'
*    INDEX ON REGION+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcCUSTEMP) OF (gcWorkDir+lcCUSTEMP)
*ENDCASE

*-- Selecting the transactions for all the selected accounts.
*SELECT * ;
*  FROM (lcTranFile);
*  WHERE !EMPTY(TranType);
*  INTO CURSOR(lcArTemp)
*INDEX ON Account+DTOS(TranDate)+Tran+TranCode TAG (lcArTemp)
*B603782,1 Commented out (These 2 files are already opened in ArCStm [End]

*B603782,1 Make Relation with Customer File to get some data [Begin]
SELECT (lcTmpAcct)

*B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.[START]
IF llMulCurr
  =lfPdcAmt()
ENDIF
*B804435,1 BWA 09/27/2001.[END]

SET RELATION TO
SET RELATION TO 'M' + Account INTO Customer ADDITIVE
*B603782,1 Make Relation with Customer File to get some data [End]

*--Start printing
SET DEVICE TO PRINT

*B603782,1 Change Temp Transaction file [Begin]
*SELECT (lcArTemp)
SELECT (lcTmpTrans)
*B603782,1 Change Temp Transaction file [End]

DO WHILE INKEY() <>32
  *B603782,1 Change name of Temp customer file [Begin]
  *SELECT (lcCUSTEMP)
  SELECT (lcTmpAcct)
  *B603782,1 Change name of Temp customer file [End]
  
  IF EOF()
    EXIT
  ENDIF
  *---------------------------------------------------------
  * Get 1st record in financial history file and initialize
  * work totals.
  *---------------------------------------------------------
  IF llNewCust

    *B603782,1 Add a Customer alias to AgeDate [Begin]
    *llAGEMSG  = (ABS(HDATE-AGEDATE)<=10)
    llAGEMSG  = ABS(HDATE-Customer.AGEDATE)<=10
    *B603782,1 Add a Customer alias to AgeDate [End]

    STORE 0.00 TO lnTotal, lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnCredit

    *B603782,1 Change name of Temp Customer file and variable name [Begin]
    *SELECT (lcCUSTEMP)
    *lcAccount = ACCOUNT
    SELECT (lcTmpAcct)
    lcGroupKey = cGroupKey
    *B603782,1 Change name of Temp Customer file and variable name [End]
      
    *B603782,1 Commented out [Begin]
    *lcStore   = STORE
    *B603782,1 Commented out [End]

    *B603782,1 Change name of Temp Transaction file [Begin]
    *SELECT (lcArTemp)
    *=SEEK(lcAccount)
    SELECT (lcTmpTrans)
    =SEEK (lcGroupKey)
    *B603782,1 Change name of Temp Transaction file [End]

    *B603782,1 Change variable name in Scan loop [Begin]
    *SCAN WHILE ACCOUNT=lcAccount
    SCAN WHILE cGroupKey = lcGroupKey
    *B603782,1 Change variable name in Scan loop [End]

      *-- Calculate DAYS
      IF lcAgeType='T'                    &&Aging by terms
        XDUEDATE = IIF(DUEDATE=CTOD('  /  /  '), TRANDATE+30, DUEDATE)  
        DAYS=HDATE-XDUEDATE
        DO CASE               
          CASE AMOUNT<0             
            lnCredit=lnCredit+AMOUNT  
          CASE DAYS>=91             
            lnAge120=lnAge120+AMOUNT  
          CASE DAYS>=61             
            lnAge90=lnAge90+AMOUNT    
          CASE DAYS>=31             
            lnAge60=lnAge60+AMOUNT    
          CASE DAYS>=1              
            lnAge30=lnAge30+AMOUNT    
          OTHERWISE                 
            lnCurrent=lnCurrent+AMOUNT 
        ENDCASE
        *--Aging by transaction date
      ELSE                                  
        DAYS=HDATE-TRANDATE
        DO CASE               
          CASE AMOUNT<0       
            lnCredit=lnCredit+AMOUNT   
          CASE DAYS>=120             
            lnAge120=lnAge120+AMOUNT   
          CASE DAYS>=90              
            lnAge90=lnAge90+AMOUNT     
          CASE DAYS>=60              
            lnAge60=lnAge60+AMOUNT     
          CASE DAYS>=30              
            lnAge30=lnAge30+AMOUNT     
          OTHERWISE                  
            lnCurrent=lnCurrent+AMOUNT 
        ENDCASE                              
      ENDIF   
    ENDSCAN 

    *B603782,1 Change variable to cGroupKey instead of Account [Begin]
    *=SEEK(lcAccount)
    *SUM REST AMOUNT TO lnBEGIN WHILE ACCOUNT=lcAccount .AND.TRANDATE<LDATE
    *-- SEE IF THERE ARE MORE TRANSACTIONS
    *llGOTAR = IIF(ACCOUNT=lcAccount.AND.TRANDATE<=HDATE, .T., .F.)

    SEEK lcGroupKey
    SUM REST AMOUNT TO lnBEGIN WHILE cGroupKey = lcGroupKey .AND.TRANDATE<LDATE
    llGOTAR = IIF(cGroupKey = lcGroupKey .AND. TRANDATE<=HDATE, .T., .F.)
    *B603782,1 Change variable to cGroupKey instead of Account [End]
    
    IF lnBEGIN=0 .AND. !llGOTAR       && IF THERE ARE NO MORE TRANSACTIONS

      *B603782,1 Change name of Temp Customer file [Begin]
      *SELECT (lcCUSTEMP)
      SELECT (lcTmpAcct)
      *B603782,1 Change name of Temp Customer file [End]

      SKIP
      LOOP
    ENDIF
    lnTotal = lnBEGIN
    lnPAGECT = 0
  
    *B603782,1 Change name of Temp Customer file [Begin]
    *SELECT (lcCUSTEMP)
    SELECT (lcTmpAcct)
    *B603782,1 Change name of Temp Customer file [End]
  
    LLFACTORW = (llRPRemit .AND. !EMPTY(CFACCODE))
    IF LLFACTORW .AND. lcTmpFact<>CFACCODE 
      lcTmpFact = CFACCODE
      *-- C101585 Seek the factor in sycFact not in customer file.
      SELECT SycFact
      =SEEK(lcTmpFact,'SycFact')
      STORE '' TO lcFName,lcFAddr1,lcFAddr2,lcFAddr3,lcFAddr4,lcFAddr5,lcFAddr6
      lcFName  = SycFact.cFacComp
      lcFAddr1 = cAddress1
      IF !EMPTY(lcFAddr2)
        lcFAddr2  = cAddress2
        lcFAddr3  = SUBSTR(cAddress3,1,15) +','+SUBSTR(cAddress4,1,3)+','+SUBSTR(cAddress5,1,10)
      ELSE
        lcFAddr2  = SUBSTR(cAddress3,1,15) +','+SUBSTR(cAddress4,1,3)+','+SUBSTR(cAddress5,1,10)
      ENDIF

      *B603782,1 Change name of Temp customer file [Begin]
      *SELECT (lcCUSTEMP)
      SELECT (lcTmpAcct)
      *B603782,1 Change name of Temp customer file [End]

    ENDIF

    *B603782,1 Commented out [Begin]
    *SELECT CUSTOMER
    *=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))    
    *B603782,1 Commented out [End]

    STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
    lcBtName  = CUSTOMER.BTNAME
    =gfGetAdr('Customer','','','',1,'2')
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR
    IF EMPTY(lcBtAdd2)
      lcBtAdd2 = lcBtAdd3
      lcBtAdd3 = lcBtAdd4
      lcBtAdd4 = lcBtAdd5
      lcBtAdd5 = lcBtAdd6
      lcBtAdd6 = ""
    ENDIF
    llNewCust = .F.
  ENDIF
  lnPAGECT = lnPAGECT +1

  *B603782,1 Change name of Temp customer file [Begin]
  *SELECT (lcCUSTEMP)
  SELECT (lcTmpAcct)
  *B603782,1 Change name of Temp customer file [End]

  L4 = IIF(LLFACTORW,lcFAddr2,lcBtadd2)
  L5 = IIF(LLFACTORW,lcFAddr3,lcBtadd3)
  ROW = 11
  @ ROW,10 SAY ACCOUNT
  ROW = ROW+1
  @ ROW,10 SAY IIF(LLFACTORW,lcFName,lcBtName)
  @ ROW,65 SAY lnPAGECT    PICTURE '999'
  ROW = ROW+1
  @ ROW,10 SAY IIF(LLFACTORW,lcFAddr1,lcBtadd1)
  ROW = ROW+1
  @ ROW,10 SAY L4
  @ ROW,65 SAY DTOC(END)
  ROW = ROW+1
  @ ROW,10 SAY L5
  ROW = ROW+1
  @ ROW,65 SAY DTOC(TODAY)
  *-----------------------------------------------------------------
  * Determine which message to print on customer's statement
  *-----------------------------------------------------------------
  ROW = 20
  DO CASE
    CASE llAGEMSG .AND. AGE120 > 25
      @ ROW+0,10 SAY M120L1
      @ ROW+1,10 SAY M120L2
      @ ROW+2,10 SAY M120L3
    CASE llAGEMSG .AND. AGE90>25
      @ ROW+0,10 SAY M90L1
      @ ROW+1,10 SAY M90L2
      @ ROW+2,10 SAY M90L3
    CASE llAGEMSG .AND. AGE60>25
      @ ROW+0,10 SAY M60L1
      @ ROW+1,10 SAY M60L2
      @ ROW+2,10 SAY M60L3
    CASE llAGEMSG .AND. AGE30>25
      @ ROW+0,10 SAY M30L1
      @ ROW+1,10 SAY M30L2
      @ ROW+2,10 SAY M30L3
  ENDCASE
  ROW = 24
  IF lnPAGECT=1 .AND. lnBEGIN<>0
    @ ROW,09 SAY 'ACCOUNT SUMMARY THRU &THRUDATE'
    @ ROW,68 SAY lnBEGIN   PICTURE '99999999.99'
    ROW = ROW +1
  ENDIF
  *-- LOOP TO PRINT ACCOUNT ACTIVITY
  DO WHILE .T.

    *B603782,1 Change name of Temp transaction file [Begin]
    *SELECT (lcArTemp)
    *DO WHILE &lcArTemp..ACCOUNT=lcAccount .AND. ROW<=MAXROW
    SELECT (lcTmpTrans)
    DO WHILE &lcTmpTrans..cGroupKey = lcGroupKey .AND. ROW<=MAXROW    
    *B603782,1 Change name of Temp transaction file [End]


      *B603782,1 Commented out [Begin]
      *SELECT (lcArTemp)
      *B603782,1 Commented out [End]

      IF TRANDATE > HDATE
        
        *B603782,1 Change Variable to cGroupKey instead of Account [Begin]
        *COUNT REST TO X WHILE ACCOUNT = lcAccount
        COUNT REST TO X WHILE cGroupKey = lcGroupKey
        *B603782,1 Change Variable to cGroupKey instead of Account [End]
        
        EXIT
      ENDIF
      lnTotal = lnTotal + AMOUNT
      DAYS  = MAX(HDATE-TRANDATE, 0)
      PDATE = TRANDATE
      *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
      *TRAN#    DATE   .....DESC.....  REFERENCE     STR/CHK     CHGBACK      AMOUNT
      *123456 MM/DD/YY 123456789012345 1234567890123 12345678 1234567.99  1234567.99
      *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
      *   CURRENT    OVER-30    OVER-60    OVER-90   OVER-120     CREDITS     BALANCE
      *   CURRENT   (+31-60)   (+61-90)  (+91-120) (OVER-120)     CREDITS     BALANCE
      *1234567.12 1234567.12 1234567.12 1234567.12 1234567.12  1234567.12  1234567.12
      ROW = ROW + 1
      @ ROW,01 SAY TRAN
      @ ROW,10 SAY PDATE
      @ ROW,19 SAY IIF("PAYMENT-THANK" $ DESC,"PAYMENT-THANKS",SUBSTR(DESC,1,15))
      @ ROW,35 SAY SUBSTR(REFERENCE,1,09)      
      @ ROW,47 SAY STORE
      @ ROW,62 SAY IIF(AMOUNT>0, DAYS, 0)      PICTURE '@Z 999'
      @ ROW,67 SAY AMOUNT                      PICTURE '@Z 99999999.99'
      SKIP
      ROW = ROW+1
    ENDDO

    *B603782,1 Change name of Temp transaction file [Begin]
    *IF &lcArTemp..ACCOUNT<>lcAccount
    IF &lcTmpTrans..cGroupKey <> lcGroupKey
    *B603782,1 Change name of Temp transaction file [End]

      llNewCust = .T.
    ENDIF
    EXIT
  ENDDO
  *---------------------- END DISPLAY LOOP -----------------------
  IF llNewCust

    *B603782,1 Commented out [Begin]
    *lnAlias=SELECT()
    *SELECT PostDChq
    *SEEK lcAccount
    *SUM Amount REST WHILE cGroupKey = lcGroupKey TO lnPstDChq
    *B603782,1 Commented out [End]
    
    *B603782,1 Print Post Dated Cheque in case of multi currency [Begin]

    *B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.[START]
    *IF !llMulCurr
    *B804435,1 BWA 09/27/2001.[END]
    
      *B604957,1 BWA 09/25/2001 Fix the bug of printing the PDC and move it one line.[START]
      *@ 53,01 SAY 'POSTDATED CHEQUES: '

      *B603782,1 Commented out [Begin]
      *@ 53,20 SAY lnPstDChq PICTURE '$9999999.99'
      *SELECT( lnAlias )

      *@ 53,20 SAY &lcTmpAcct..nPostDchq PICTURE '$9999999.99'
      @ 52,01 SAY 'POSTDATED CHEQUES: '
      @ 52,20 SAY &lcTmpAcct..nPostDchq PICTURE '$9999999.99'
      *B604957,1 BWA 09/25/2001.[END]

      *B603782,1 Commented out [End]

    *B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.[START]
    *ENDIF
    *B804435,1 BWA 09/27/2001.[END]

    *B603782,1 Print Post Dated Cheque in case of multi currency [End]
    
    @ 56,00 SAY lnCurrent  PICTURE '9999999.99'  &&@ 55,00 SAY lnCurrent  PICTURE '9999999.99'
    @ 56,11 SAY lnAge30    PICTURE '9999999.99'  &&@ 55,11 SAY lnAge30    PICTURE '9999999.99'
    @ 56,21 SAY lnAge60    PICTURE '9999999.99'  &&@ 55,21 SAY lnAge60    PICTURE '9999999.99'
    @ 56,32 SAY lnAge90    PICTURE '9999999.99'  &&@ 55,32 SAY lnAge90    PICTURE '9999999.99'
    @ 56,42 SAY lnAge120   PICTURE '9999999.99'  &&@ 55,42 SAY lnAge120   PICTURE '9999999.99'
    @ 56,56 SAY lnCredit   PICTURE '9999999.99'  &&@ 55,56 SAY lnCredit   PICTURE '9999999.99'
    @ 56,67 SAY lnTotal    PICTURE '99999999.99'  &&@ 55,67 SAY lnTotal    PICTURE '99999999.99'
  ELSE
    ROW = ROW + 1
    @ ROW,50 SAY 'C O N T I N U E D'
  ENDIF
  IF llNewCust

    *B603782,1 Change name of Temp customer file [Begin]
    *SELECT (lcCUSTEMP)
    SELECT (lcTmpAcct)
    *B603782,1 Change name of Temp customer file [End]

    SKIP
  ENDIF
  @ 57,10 SAY MADL1   &&@ 56,10 SAY MADL1
  @ 58,10 SAY MADL2   &&@ 57,10 SAY MADL2
  @ 59,10 SAY MADL3   &&@ 58,10 SAY MADL3
  @ 60,10 SAY MADL4   &&@ 59,10 SAY MADL4
  @ 61,10 SAY MADL5   &&@ 60,10 SAY MADL5 
ENDDO
SET DEVICE TO SCREEN

*B603782,1 Commented out [Begin]
*IF USED(lcCUSTEMP)
*  USE IN (lcCUSTEMP)
*ENDIF
*B603782,1 Commented out [End]

RETURN
*B804435,1 BWA 09/27/2001 Print the PDC in the Multi currency company.[START]
*!*************************************************************
*! Name      : LFPDCAMT
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/26/2001
*! Purpose   : Collect the amount posted date check for the customers.
*!*************************************************************
*! Called from : ARCSTMOL.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =LFPDCAMT()
*!*************************************************************
FUNCTION LFPDCAMT
PRIVATE lcAlias , lcKeyPdc

LnPostDChq = 0
lcAlias = SELECT(0)
SELECT (lcTmpAcct)
lcKeyPdc = EVAL(KEY())
SCAN
  SELECT (lcPostDc)
  =SEEK(&lcTmpAcct..ACCOUNT)
  SUM Amount REST WHILE Account = &lcTmpAcct..Account TO LnPostDChq
  SELECT (lcTmpAcct)
  REPLACE &lcTmpAcct..nPostDChq WITH LnPostDChq
ENDSCAN

=SEEK(lcKeyPdc , lcTmpAcct)
SELECT(lcAlias)
*--End of LFPDCAMT
*B804435,1 BWA 09/27/2001.[END]