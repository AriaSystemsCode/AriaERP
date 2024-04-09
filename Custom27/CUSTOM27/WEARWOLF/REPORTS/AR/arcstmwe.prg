*!********************************************************************
*: Program file  : ARCSTMS (FOR WEARWOLF) Converted from 26 to 27.
*: Program desc. : CUSTOMIZED CUSTOMER STATEMENT.
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*: Refer to      : (C101500)
*!********************************************************************
*:B603783,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new standard tables
*:B603783,1                Customer Statement for WearWolf
*:B804082,1 BWA 05/21/2001 Modify the address.
*:B607412,1 ABD 07/22/2003 Fix bug that the report prints these accounts although 
*:B607412,1 ABD            You selected print acc. with Zero Bal.= 'No' .
*:**************************************************************************

*B603783,1 Commented out [Begin]
*--Get the age type.
*lcAgeType = ALLTRIM(gfGetMemvar('XAGINGTYPE'))
*B603783,1 Commented out [End]

*--Set the variables.
DIMENSION laAddress[1,1]
STORE ' ' TO lcBtName,laAddress,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
STORE {} TO LDATE,HDATE
llNewCust = .T.
lnInvNo   = 22

*B603783,1 Commented out [Begin]
*lcArTemp  = gfTempName()
*lcCUSTEMP = gfTempName()
*B603783,1 Commented out [End]

lnPos     = ASCAN(laOGVrFlt,'DEBIT.TRANDATE')
*-- Get the ldate and hdate
IF lnPos <> 0
  lnDatePos = ASUBSCRIPT(laOGVrFlt,lnPos,1)
  lnPPos    = AT("|",laogvrflt[lnDatePos,6])
  LDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],1,lnPPos-1))
  HDATE     = CTOD(SUBSTR(laOGVrFlt[lnDatePos,6],lnPPos+1))
ENDIF
lDate  = IIF(EMPTY(LDATE),{01/01/01},LDATE)
HDATE  = IIF(EMPTY(HDATE),DATE(),HDATE)

*B603783,1 Commented out [Begin]
*-- Selecting the records form lcDummy file that holds all the selected
*-- accounts. And setting the index according to the user selection.
*SELECT * FROM (lcDummy) INTO CURSOR (lcCUSTEMP) GROUP BY Account
*DO CASE
*  CASE lcRPSort = 'A'
*    INDEX ON ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcDummy) OF (gcWorkDir+lcDummy)
*  CASE lcRPSort = 'C'
*    INDEX ON CADDRESS4+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcPrtFile) OF (gcWorkDir+lcPrtFile)
*  CASE lcRPSort = 'Z'
*    INDEX ON CADDRESS5+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcPrtFile) OF (gcWorkDir+lcPrtFile)
*  CASE lcRPSort = 'R'
*    INDEX ON REGION+ACCOUNT+IIF(lcRpCurr='F',CCURRCODE,"")+STR(PAGE,1)+CONSOL TAG (lcPrtFile) OF (gcWorkDir+lcPrtFile)
*ENDCASE
*-- Selecting the transactions for all the selected accounts.
*SELECT * ;
*  FROM (lcTranFile);
*  WHERE !EMPTY(TranType);
*  INTO CURSOR(lcArTemp)
*INDEX ON Account+DTOS(TranDate)+Tran+TranCode TAG (lcArTemp)
*B603783,1 Commented out [End]

*B603783,1 Make Relation with Customer File to get some data [Begin]
SELECT (lcTmpAcct)
SET RELATION TO
SET RELATION TO 'M' + Account INTO Customer ADDITIVE
*B603783,1 Make Relation with Customer File to get some data [End]


*--Start printing
SET DEVICE TO PRINT

*B603783,1 Change name of file [Begin]
*SELECT (lcArTemp)
SELECT (lcTmpTrans)
*B603783,1 Change name of file [End]

*B607412,1 ABD - Set relation bettween the Transaction file and account file. [Begin]
SET RELATION TO Account INTO (lcTmpAcct) ADDITIVE
*B607412,1 ABD - [End]

DO WHILE !EOF() AND INKEY() <>32  
  
  *B607412,1 ABD - Check if the cuurent record at transaction file is end of file 
  *B607412,1 ABD - At the account file, so the report must not print this record.  [Begin]
  IF EOF(lcTmpAcct)
    IF EOF()
      EXIT
    ELSE
      SKIP
    ENDIF
    LOOP
  ENDIF
  *B607412,1 ABD - [End]
   
  lcAccount = Account
  IF llNewCust
    STORE 0 TO  lnTotal,lnAge30, lnAge60, lnAge90,lnTotAmnt, lnAge120
    STORE ' ' TO lcBtName,lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6
    IF SEEK ('M'+lcAccount,'Customer')
      lcBtName  = CUSTOMER.BTNAME
      =gfGetAdr('Customer','','','',1,'2')
      *--Get the Bill To adddess except the country.
      FOR lnCount = 1 TO ALEN(laAddress,1)-1
        lcCount = STR(laAddress[lnCount,1],1)

        *B804082,1 BWA 05/21/2001 Modify the address.[START]
        *lcBtAddr&lcCount = lcBtAddr&lcCount + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
        *SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])

        lcBtAddr&lcCount = ALLTRIM(lcBtAddr&lcCount) + IIF(EMPTY(lcBtAddr&lcCount),'',',')+;
        SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
        *B804082,1 [END]

      ENDFOR
    ENDIF
  ENDIF

  *B804082,1 BWA 05/21/2001 Modify the address.[START]
  FOR lnLoop = 1 TO ALEN(laAddress,1)-1
    lcLoop = ALLTRIM(STR(lnLoop))
    IF !EMPTY(lcBtAddr&lcLoop) AND ATC( "," , lcBtAddr&lcLoop , 2) > 0
      lcBtAddr&lcLoop = STRTRAN(lcBtAddr&lcLoop,"," ,' ' , 2 )
    ENDIF
  ENDFOR
  *B804082,1 [END]

  *-- START PRINT
  IF lnInvNo = 22
    lnHdrRow = 10                   && Start header row
    lnLinRow = 23                   && Start lines printing row.
    lnBotRow = 43                   && Start footer row.
    MaxRow   = lnBotRow - 2         && Max row number for lines printing.
    lnInvNo  = 1
  ELSE
    lnHdrRow = lnBotRow + 16        && Start header row.
    lnLinRow = lnHdrRow + 13        && Start lines printing row.
    lnBotRow = lnLinRow + 20        && Start footer row.
    MaxRow   = lnBotRow - 02        && Max row number for lines printing.
    lnInvNo  = lnInvNo  + 01
  ENDIF
  llNewCust   = .T.  
  *-- Print Customer Statement header
  @ lnHdrRow ,14 SAY lcBtName
  lnHdrRow  = lnHdrRow  + 1

  *B603783,1 Change variable name [Begin]
  *@ lnHdrRow ,02 SAY lcAccount
  @ lnHdrRow ,02 SAY Account
  *B603783,1 Change variable name [End]

  @ lnHdrRow ,14 SAY lcBtAddr1
  @ lnHdrRow ,46 SAY DATE()

  *B603783,1 Change variable name [Begin]
  @ lnHdrRow ,59 SAY lcAccount
  *B603783,1 Change variable name [End]

  @ lnHdrRow ,66 SAY DATE()
  lnHdrRow = lnHdrRow + 1
  @ lnHdrRow ,14 SAY lcBtAddr2
  lnHdrRow = lnHdrRow + 1
  @ lnHdrRow ,14 SAY lcBtAddr3
  SCAN WHILE Account = lcAccount
    DO CASE
      CASE EOF()
        llNewCust = .T.
        EXIT
      CASE lnLinRow >= MaxRow
        llNewCust = .F.
        EXIT
    ENDCASE
    IF lcAgeType='T'       &&Aging by terms
      ldDueDate = IIF(EMPTY(DUEDATE), TRANDATE+30, DUEDATE)  
      DAYS = HDate - ldDueDate
      DO CASE               
        CASE DAYS >= 0 AND DAYS < 30
          lnAge30 = lnAge30 + AMOUNT    
        CASE DAYS > 30 AND DAYS < 60 
          lnAge60 = lnAge60 + AMOUNT    
        CASE DAYS > 60 AND DAYS < 90
          lnAge90 = lnAge90 + AMOUNT    
        CASE DAYS >= 90
          lnAge120 = lnAge120 + AMOUNT  
        ENDCASE                      
      ELSE                                  &&Aging by transaction date
        DAYS = HDate - TRANDATE
        DO CASE               
          CASE DAYS >= 0 AND DAYS < 30
            lnAge30 = lnAge30 + AMOUNT     
          CASE DAYS > 30 AND DAYS < 60 
            lnAge60 = lnAge60 + AMOUNT     
          CASE DAYS > 60 AND DAYS < 90
            lnAge90 = lnAge90 + AMOUNT     
          CASE DAYS >= 90
            lnAge120 = lnAge120 + AMOUNT   
        ENDCASE
    ENDIF
    lnTotal = lnAge30 + lnAge60 + lnAge90 + lnAge120
    @ lnLinRow,02 SAY TranDate
    @ lnLinRow,11 SAY LEFT(DESC,11)
    @ lnLinRow,27 SAY Tran    
    @ lnLinRow,42 SAY Amount PICTURE "9999999.99"
    @ lnLinRow,58 SAY Tran
    @ lnLinRow,64 SAY Amount PICTURE "9999999.99"
    lnTotAmnt = lnTotAmnt + Amount
    lnLinRow = lnLinRow + 1
  ENDSCAN
  llNewCust = IIF(Account <> lcAccount, .T., llNewCust)
  IF !llNewCust
    *--Print the Footer
    =lfPrnFootr()
    LOOP
  ENDIF
  =lfPrnFootr()
ENDDO

*B607412,1 ABD - [Begin]
SELECT (lcTmpTrans)
SET RELATION TO 
*B607412,1 ABD - [End]

*- Abdou

SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrnFootr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/18/1999
*! Purpose   : To print the statement footer
*!*************************************************************
*! Example            :  lfPrnFootr()
*!*************************************************************
FUNCTION lfPrnFootr

@ lnBotRow,02 SAY IIF(llNewCust,ALLTRIM(STR(lnAge120,8,2)),'********')
@ lnBotRow,16 SAY IIF(llNewCust,ALLTRIM(STR(lnAge90,8,2)),'********')

lnBotRow = lnBotRow + 2

@ lnBotRow,02 SAY IIF(llNewCust,ALLTRIM(STR(lnAge60,8,2)),'********')
@ lnBotRow,16 SAY IIF(llNewCust,ALLTRIM(STR(lnAge30,8,2)),'********')
@ lnBotRow,42 SAY IIF(llNewCust,ALLTRIM(STR(lnTotal,8,2)),'********')
@ lnBotRow,60 SAY IIF(llNewCust,ALLTRIM(STR(lnTotAmnt,8,2)),'********')
