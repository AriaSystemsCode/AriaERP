*:**************************************************************************
*: Program file  : ARKRA400
*: Program desc. : Print Credit/Debit Memo for (Krazy Kat)
*: Date          : 10/19/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C101633
*:**************************************************************************
*: Calls :  
*:       Procedures : lpCollData, lpPrintHdr, lpPrnNotPd, lpPrintRep
*:       Functions  : lfwRepWhen(), lfCreatFil(), lfvDateRng(), fvpbOk()
*:                    lfChckDate(), lfSRAcc(), lfClearRep()
*:          Global  : gfModalGen()
*:**************************************************************************
*: Modifications     : ........
*:B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*:B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:B603997,1 BWA 10/31/2000 Fix the bug of check for the date.
*:**************************************************************************
*
lcRpExp  = STRTRAN(lcRpExp,"CUSTOMER.","")

*-- IF user changed criteria in OG [Begin]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    = lfCreatFil()
  ENDIF

  *-- Set criteria of Customer file
  SELECT Customer
  SET FILTER TO &lcRpExp

  *-- Collect data for Debit,Credit and ChargeBack according to Transaction
  *-- Type selected in Option Grid
  IF lcRpTrType <> 'C'
    DO lpCollData WITH 'D'   && open debit file and set relation to customer
    IF lcRpTrType = 'A'    
      DO lpCollData WITH 'C'   && open Credit file and set relation to customer
    ENDIF  
  ELSE
    DO lpCollData WITH 'C'   && open Credit file and set relation to customer
  ENDIF
  
  *-- Break relation of Both Bebit/Credit file after collecting data
  SELECT Debit
  SET RELATION TO
  SELECT Credit
  SET RELATION TO  
ENDIF
*-- IF user changed criteria in OG [End]

SELECT (lcWorkFile)
GO TOP
*-- If no records in temp file (Empty)
IF RECCOUNT(lcWorkFile) = 0
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ELSE  
  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT
  DO lpPrintRep      && print the report 
  DO ENDREPORT       
  SET DEVICE TO SCREEN
ENDIF
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************

****************************************************************************
********************* *-- Report Printing Functions --* ********************
****************************************************************************

*!**************************************************************************
*! Name      : lpPrintRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Prints the report
*!**************************************************************************
*! Example   : DO lpPrintRep
*!**************************************************************************
*
PROCEDURE lpPrintRep
PRIVATE lnEndOfPag,lcType
STORE 0 TO lnAmount , PAGENO
R_WIDTH = 'N'
lcRep   = SPACE(3)

SELECT (lcWorkFile)
SET RELATION TO 'M'+Account INTO Customer
*-- Temporary File
*--              |__
*--                 Customer

DO WHILE !EOF() .AND. INKEY() <> 32

  STORE 1 TO PAGENO, ROW         
  lcAccount = IIF(cType<>'K',IIF(cType='D','D','Q')+Account,'K'+Tran)   && New Account Record
  lnTotal = 0

  *-- The function is called here to print the modified header
  DO lpPrintHdr  

  SCAN WHILE cType + cTempKey + DTOS(TranDate) = lcAccount .AND. INKEY() <> 32
    lnEndOfPag = IIF(cType<>'Q',52,51)
    IF ROW > lnEndOfPag
      @ 53,01 SAY REPLICATE('-',80)
      @ 54,33 SAY 'C O N T I N U E'
      @ 55,01 SAY REPLICATE('-',80)
      IF cType <> 'K'
        PAGENO = PAGENO + 1
        ROW = 1
      ENDIF
        
      *-- The function is called here to print the modified header
      DO lpPrintHdr
    ENDIF

    IF cType <> 'K'
      IF cType = 'Q'
        @ ROW,01 SAY '*** Chargebacks'
        ROW = ROW + 1
      ENDIF
      @ ROW,01 SAY Tran
      @ ROW,10 SAY IIF(cType='Q',ChgBk_Date,TranDate)
      @ ROW,21 SAY SUBSTR(Desc,1,16)
      @ ROW,39 SAY SUBSTR(Reference,1,20)
      @ ROW,64 SAY Amount   PICTURE '9999999.99'
      lnTotal = lnTotal + Amount
      IF cType = 'Q'
        ROW = ROW + 1
      ENDIF  
    ELSE 
      @ ROW,09 SAY Tran
      @ ROW,18 SAY TranDate
      @ ROW,29 SAY SUBSTR(Desc,1,16)
      @ ROW,46 SAY SUBSTR(Reference,1,20)
      @ ROW,68 SAY ABS(Amount)   PICTURE '9999999.99'
      lnTotal = lnTotal + ABS(Amount)
    ENDIF
      
    IF SEEK(cType+Tran,'NotePad') .AND. !EMPTY(NotePad.Mnotes)
      ROW = ROW + 1
      DO lpPrnNotPd
    ENDIF         
    ROW = ROW + 1
    lcType = cType
  ENDSCAN

  IF lcType = 'K'
    @ 53,09 SAY REPLICATE('-',71)
    @ 54,47 SAY 'Total         :'
    @ 54,68 SAY lnTotal     PICTURE '9999999.99'
    @ 55,09 SAY 'Salespersons Participation'
    @ 56,11 SAY '$'
    @ 56,14 SAY lnAmount PICTURE '999999.99'
  ELSE
    @ 53,01 SAY REPLICATE('-',80)
    @ 54,04 SAY 'Printed on '+DTOC(DATE())
    @ 54,47 SAY 'Total......'
    @ 54,IIF(cType='Q',63,64) SAY lnTotal     PICTURE '9999999.99'
    @ 55,01 SAY REPLICATE('-',80)
  ENDIF   
ENDDO
*-- End of lpPrintRep.

*!**************************************************************************
*! Name      : lpPrnNotPd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : print Notepad Notes
*!**************************************************************************
*! Example   : DO lpPrnNotPd
*!**************************************************************************
*
PROCEDURE lpPrnNotPd
PRIVATE lnAlias
lnAlias = SELECT()

SELECT NotePad
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
lnNotLine = 1
lnMemLins = MEMLINES(NotePad.Mnotes)
  
IF Type + Key = &lcWorkFile..cType + &lcWorkFile..Tran
  DO WHILE lnNotLine <= lnMemLins
    IF ROW >= 53
      @ 53,IIF(lcRpTrType<>'C',01,09) SAY REPLICATE('-',IIF(lcRpTrType<>'C',80,71))
      @ 54,33 SAY 'C O N T I N U E'
      @ 55,IIF(lcRpTrType<>'C',01,09) SAY REPLICATE('-',IIF(lcRpTrType<>'C',80,71))
      PAGENO = PAGENO + 1
      IF &lcWorkFile..cType = 'Q'
        ROW = 1
        @ ROW,03 SAY gcCom_Name
        ROW = ROW + 1
        FOR lnI = 2 TO 6                        && Print Your Company Name
          lcZ = STR(lnI,1)
          @ ROW,03 SAY HLINE&lcZ
          ROW = ROW + 1
        ENDFOR

        @ 13,10 SAY Customer.Account          && Print Customer Address
        @ 13,47 SAY 'C H A R G E B A C K  M E M O '
        @ 14,10 SAY Customer.BTNAME
        @ 15,10 SAY Customer.cAddress12
        @ 15,50 SAY 'Page: '+STR(PAGENO,3)
        IF EMPTY(Customer.cAddress22)
          @ 16,10 SAY ALLTRIM(Customer.cAddress32)+' '+ALLTRIM(Customer.cAddress42)+'  '+ALLTRIM(Customer.cAddress52)
        ELSE
          @ 16,10 SAY Customer.cAddress22
          @ 17,10 SAY ALLTRIM(Customer.cAddress32)+' '+ALLTRIM(Customer.cAddress42)+'  '+ALLTRIM(Customer.cAddress52)
        ENDIF
        @ 24,01 SAY REPLICATE('-',80)     && Print Title
        @ 25,01 SAY 'Tran#    DATE       ..Description..   .....Reference......          Amount'
        @ 26,01 SAY REPLICATE('-',80)
      ENDIF
      SELECT(lnAlias)
      IF &lcWorkFile..cType <> 'Q'
        DO lpPrintHdr
      ENDIF  
      SELECT NotePad
      ROW = 27
    ENDIF
    @ ROW,IIF(&lcWorkFile..cType<>'K',02,09) SAY MLINE(Mnotes,lnNotLine)
    ROW = ROW + 1
    lnNotLine = lnNotLine + 1
  ENDDO
  ROW = ROW + 1
ENDIF
SET MEMOWIDTH TO lnOldMemW

SELECT(lnAlias)
RETURN
*-- End of lpPrnNotPd.

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : print report header 
*!**************************************************************************
*! Example   : DO lpPrintHdr
*!**************************************************************************
*
PROCEDURE lpPrintHdr
PRIVATE lnAlias , lnColumn
lnAlias = SELECT()   && to save old alias
SELECT (lcWorkFile)

IF cType = 'K'
  @ 01,01 SAY laCreCmpAd[1]
  @ 01,67 SAY 'Page: '+STR(PAGENO,3)
  @ 02,01 SAY laCreCmpAd[2]
  @ 03,01 SAY laCreCmpAd[3]
  @ 04,01 SAY laCreCmpAd[4]
  @ 04,63 SAY 'Cr. Memo: '+Tran
  @ 05,01 SAY laCreCmpAd[5]
  @ 06,01 SAY laCreCmpAd[6]
    
  @ 08,34 SAY 'CREDIT  MEMO'
  @ 13,09 SAY 'Customer Address:'
  @ 13,56 SAY 'Date      : '
  @ 13,69 SAY TranDate
ELSE
  ROW = 1
  lnColumn = IIF(cType='D',1,3)
  @ 01,lnColumn SAY gcCom_Name
  ROW = ROW + 1
  FOR lnI = 2 TO 5           && Print Your Company Name
    lcZ = STR(lnI,1)
    @ ROW,lnColumn SAY laCompAdd[lnI]
    ROW = ROW + 1
  ENDFOR

  IF cType = 'D'
    @ 13,09 SAY Account
    @ 13,50 SAY 'D E B I T   M E M O'
  ELSE
    @ 13,10 SAY Account                     && Print Customer Address
    @ 13,47 SAY 'C H A R G E B A C K  M E M O '    
  ENDIF
ENDIF

IF cType <> 'Q'
  @ 14,09 SAY Customer.BtName
  @ 14,56 SAY IIF(cType='K','Claim #   : '+SUBSTR(Reference,1,11),'')        
  @ 15,09 SAY Customer.cAddress12

  IF cType = 'K'
    SELECT RepComm
    *ahmed remove locate and use seek
    IF SEEK(&lcWorkFile..Tran)
      lcRep    = RepCode
      lnAmount = Amount
    ENDIF
  ENDIF  
  
  @ 15,50 SAY IIF(cType<>'K','Page: '+STR(PAGENO,3),'')
  @ 15,56 SAY IIF(cType<>'K','','Sales Rep : '+lcRep)

ELSE
  @ 14,10 SAY Customer.BtName
  @ 15,10 SAY Customer.cAddress12
  @ 15,50 SAY 'Page: '+STR(PAGENO,3)
ENDIF

lnColumn = IIF(cType<>'Q',9,10)
IF EMPTY(Customer.cAddress22)
  @ 16,lnColumn SAY ALLTRIM(Customer.cAddress32)+' '+ALLTRIM(Customer.cAddress42)+'  '+ALLTRIM(Customer.cAddress52)
  @ 16,56 SAY IIF(cType<>'K','','Type      : '+ ALLTRIM(SUBSTR(Desc,1,16)))
ELSE
  @ 16,lnColumn SAY Customer.cAddress22
  @ 16,56 SAY IIF(cType<>'K','','Type      : '+ ALLTRIM(SUBSTR(Desc,1,16)))
  @ 17,lnColumn SAY ALLTRIM(Customer.cAddress32)+' '+ALLTRIM(Customer.cAddress42)+'  '+ALLTRIM(Customer.cAddress52)
ENDIF

IF cType<>'K'
  @ 24,01 SAY REPLICATE('-',80)
  @ 25,01 SAY 'Tran#    DATE       ..Description..   .....Reference......          Amount'
  @ 26,01 SAY REPLICATE('-',80)
ELSE  
  @ 24,09 SAY REPLICATE('-',71)
  @ 25,09 SAY 'Tran#    DATE       ..Description..  .....Reference......      Amount '
  @ 26,09 SAY REPLICATE('-',71)
ENDIF
ROW = 27
SELECT (lnAlias)  && restore old alias
*-- End of lpPrintHdr.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Select Credit or Debit file and set relation to customer
*!**************************************************************************
*! Parameter : 'C' for credit file / 'D' for Debit file
*!**************************************************************************
*! Example   : DO lpCollData WITH 'C'
*!**************************************************************************
*
PROCEDURE lpCollData
PARAMETER lcFileOpen
PRIVATE lcDebtFltr,lcCrdtFltr

IF lcFileOpen = 'D'
  SELECT Debit
  SET RELATION TO 'M'+Account INTO Customer
  *-- Debit
  *--     |__
  *--        Customer
  
  lcDebtFltr = [!EOF('Customer')]
  IF lcRpTrType <> 'A'
    lcDebtFltr = lcDebtFltr + [ AND ] + [IIF(lcRpTrType<>'H',TranType='2',TranType='3')]
    IF !llNullDate
      lcDebtFltr = lcDebtFltr + [ AND ] + [IIF(lcRpTrType<>'H',;
      BETWEEN(TranDate,ldFrom,ldTo),BETWEEN(ChgBk_Date,ldFrom,ldTo))]
    ENDIF
  ELSE
    IF !llNullDate
      lcDebtFltr = lcDebtFltr + [ AND (BETWEEN(TranDate,ldFrom,ldTo) ;
                   OR BETWEEN(ChgBk_Date,ldFrom,ldTo)) AND TranType $ '23']
    ENDIF
  ENDIF
  
  SCAN FOR &lcDebtFltr
    SCATTER MEMVAR
    m.cType = IIF(TranType='2','D','Q')   && D = Debit / Q = ChargeBack
    m.cTempKey = EVALUATE([Account+Tran])
    INSERT INTO (lcWorkFile) FROM MEMVAR      
  ENDSCAN
ELSE
  SELECT Credit
  SET RELATION TO 'M'+Account INTO Customer
  *-- Credit
  *--      |__
  *--         Customer

  lcCrdtFltr = [!EOF('Customer')]
  lcCrdtFltr = lcCrdtFltr + [ AND TranType='5']
  IF !llNullDate
    lcCrdtFltr = lcCrdtFltr + [ AND BETWEEN(TranDate,ldFrom,ldTo)]
  ENDIF

  SCAN FOR &lcCrdtFltr
    SCATTER MEMVAR
    m.cType = 'K'   && K = Credit
    m.cTempKey = EVALUATE([Tran+Account])
    INSERT INTO (lcWorkFile) FROM MEMVAR      
  ENDSCAN
ENDIF
*-- End of lpCollData.

****************************************************************************
*********************** *-- Option Grid Functions --* **********************
****************************************************************************

*!**************************************************************************
*! Name      : lfWRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Example   : =lfWRepWhen()
*!**************************************************************************
*
FUNCTION lfWRepWhen
DECLARE laCompAdd[3]
laCompAdd = ''
= gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)

DECLARE laCompAdd[5],laCreCmpAd[6]

SELECT SYCCOMP
SEEK gcAct_Comp

laCompAdd[4] = TRANSFORM(cCom_Phon , lcCmp_Phon)
laCompAdd[5] = 'DUNS NO: ' + lcDunsNo

IF LEN(laCompAdd[2]) = 0
  laCompAdd[3] = laCompAdd[2]
  laCompAdd[4] = laCompAdd[3]
  laCompAdd[5] = laCompAdd[4]
  laCompAdd[5] = ' '
ENDIF

laCreCmpAd[1] = SPACE(ROUND((80-LEN(ALLTRIM(gcCom_Name)))/2,0))+ALLTRIM(gcCom_Name)
FOR lnI = 2 TO 6
  laCreCmpAd[lnI] = SPACE(ROUND((80-LEN(ALLTRIM(laCompAdd[lnI-1])))/2,0))+;
                    ALLTRIM(laCompAdd[lnI-1])                     
ENDFOR

SELECT RepComm
INDEX ON Tran TAG (lcTempRep) OF (gcWorkDir+lcTempRep+'.CDX') FOR TranType = '5'

= lfCreatFil()                 && to create the temp file 
*-- End of lfWRepWhen.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Create temporary File structure.
*!**************************************************************************
*! Called from : OG When function. , Main Prog
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
DIMENSION laTempFile[9,4] , laTempStru[1,4]
STORE '' TO laTempFile , laTempStru
PRIVATE lnFileCnt , lnFldRow , lcSetExact

lcSetExact = SET('EXACT')
SET EXACT ON

*-- creating Fields of Temp File [Begin.]
*-- First adding CTYPE field structure --> Debit(D),Credit(K),ChargeBack(Q)
laTempFile[1,1]  = 'CTYPE'
laTempFile[1,2]  = 'C'
laTempFile[1,3]  = 1
laTempFile[1,4]  = 0

*-- second adding some fields from Debit File
SELECT Debit
=AFIELDS(laTempStru)  && copy all File fields and types to this array
laTempFile[2,1]  = 'ACCOUNT'
laTempFile[3,1]  = 'TRAN'
laTempFile[4,1]  = 'TRANDATE'
laTempFile[5,1]  = 'DESC'
laTempFile[6,1]  = 'REFERENCE'
laTempFile[7,1]  = 'AMOUNT'
laTempFile[8,1]  = 'CHGBK_DATE'

*-- Loop to get other dimensions of Debit fields 
FOR lnFileCnt = 2 TO 8
  lnFldRow = ASCAN(laTempStru,laTempFile[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempStru,lnFldRow,1)
    laTempFile[lnFileCnt , 2 ] = laTempStru[lnFldRow , 2 ]
    laTempFile[lnFileCnt , 3 ] = laTempStru[lnFldRow , 3 ]
    laTempFile[lnFileCnt , 4 ] = laTempStru[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Debit fields

*-- Third adding the cTempKey structure to the Temp file
laTempFile[9,1] = 'CTEMPKEY'
laTempFile[9,2] = 'C'
laTempFile[9,3] = 11
laTempFile[9,4] = 0

CREATE TABLE (gcWorkDir+lcWorkFile) FROM ARRAY laTempFile   && create Temp File
RELEASE laTemp*     && not used anymore so there's no need to keep it

*-- Create the Index expression on Temp File
INDEX ON cType+cTempKey+DTOS(TranDate) TAG &lcWorkFile 
*-- creating Fields of Temp File [End.]

*-- Restore SET EXACT settings
SET EXACT &lcSetExact
*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Report clear function
*!**************************************************************************
*! Example   : =lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
SELECT RepComm
CLOSE INDEXES
IF FILE(gcWorkDir+lcTempRep+'.CDX')
  ERASE (gcWorkDir+lcTempRep+'.CDX')
ENDIF
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lfwDateRng
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : When Func. to save date range value
*!**************************************************************************
*! Example   : = lfwDateRng()
*!**************************************************************************
*
FUNCTION lfwDateRng
ldFrom = ldRpFrom
ldTo   = ldRpTo
lcOldDate = DTOC(ldFrom) + DTOC(ldTo)
*-- End of lfvDateRng.

*!**************************************************************************
*! Name      : lfvDateRng
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Shows Date range screen for Transaction Date Range
*!**************************************************************************
*! Calls     : DateRng.spr
*!**************************************************************************
*! Example   : = lfvDateRng()
*!**************************************************************************
*
FUNCTION lfvDateRng
lcTitle = 'Date range'


*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome+'KraDatRg.SPX') && Run The Date Range screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

*-- End of lfvDateRng.

*!**************************************************************************
*! Name      : lfvpbOk
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Validate date range screen's OK button
*!**************************************************************************
*! Example   : =lfvpbOk()
*!**************************************************************************
*
FUNCTION lfvpbOk

*B603997,1 BWA 10/31/2000 Fix the bug of check for the date.[START]
*IF EMPTY(ldFrom) .AND. !EMPTY(ldTo)
IF EMPTY(ldFrom) .AND. !EMPTY(ldTo) OR (ldFrom > ldTo)
*B603997,1 [END]

  *-- Text : Date range From/To error ; cannot proceed  
  =gfModalGen('TRM40090B00000','DIALOG')
  _CUROBJ = OBJNUM(ldFrom)
ELSE 
  = lfChckDate()
  CLEAR READ
ENDIF
ldRpFrom = ldFrom
ldRpTo   = ldTo
*-- End of lfvpbOk.

*!**************************************************************************
*! Name      : lfChckDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : fill the Date range variable
*!**************************************************************************
*! Example   : = lfChckDate()
*!**************************************************************************
*
FUNCTION lfChckDate
*-- Fill the lcDateRang variable according to the grid options.
DO CASE
  CASE !EMPTY(ldFrom) .AND. EMPTY(ldTo)
    llNullDate = .F.
    ldTo = ldFrom
  CASE EMPTY(ldFrom) .AND. EMPTY(ldTo)
    llNullDate = .T.
ENDCASE
IF !(DTOC(ldFrom) + DTOC(ldTo) == (lcOldDate))
  llOGFltCh = .T.
ENDIF
*-- End of lfChckDate.

*!**************************************************************************
*! Name      : lfSRAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/19/1999
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
FUNCTION lfSRAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- End of lfSRAcc.
