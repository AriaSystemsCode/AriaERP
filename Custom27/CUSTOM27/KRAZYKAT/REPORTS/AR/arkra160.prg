*:***************************************************************************
*: Program file  : ARKRA160.PRG
*: Program desc. : Cash & Adjustment journal report
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat Ltd.
*:***************************************************************************
*: C101638,1 KHM 10/20/1999
*:***************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST
*:***************************************************************************
*: Modifications     : ........
*:B603997,1 BWA 10/31/2000 Fix the bug of print ***** in the amount field.
*:***************************************************************************

*-- Option grid variables
*-- ldLwTranDt   The lower date range of the report.
*-- ldHiTranDt   The higer date range of the report.
*-- lcRpBatch    The transaction batch No.
*-- lcRpAcct     The account code.
*-- lcRpCAB      The data (Cash, Adjustment, Both)
*-- lcRpFNB      The account type (Factored, Non factored, Both)
*-- lcRpCNB      The print (Ar cash, Non ar cash, Both)
*-- lcRpFrm      The report (Detail, Summary)
*-- lcRpSort     The sort option (Transaction, Customer, Batch)

*-- To validate the lower date range.
IF ldLwTranDt = {}
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Report date should not be empty. Cannot proceed.')
  RETURN
ELSE
 ldHiTranDt = IIF(ldHiTranDt = {},ldLwTranDt,ldHiTranDt)
ENDIF
lcRpCNB  = IIF(!(EMPTY(lcRpAcct) AND lcRpCAB <> 'A'),SPACE(1),lcRpCNB)
lcRpFNB  = IIF(!EMPTY(lcRpAcct),'B',lcRpFNB)
WorkFile = gfTEMPNAME()
TranFile = gfTEMPNAME()

*-- Creating the temporary file that will hold the credit,debit, and arhist
*-- transaction.
SELECT Credit
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'TranCode'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0
CREATE TABLE (gcWorkDir+TranFile) FROM ARRAY laFileStru

XFILTER = "AMOUNT<>0"
XFILTER1 = XFILTER
DO CASE
  CASE lcRpCAB = 'C'                                 &&-CASH ONLY
    XFILTER = XFILTER + ".AND.TRANTYPE $'4'"
  CASE lcRpCAB = 'A'                                 &&-ADJUSTMENTS ONLY
    XFILTER = XFILTER + ".AND.TRANTYPE $'257'"
  OTHERWISE                                      &&-CASH & ADJUSTMENTS
    lcRpCAB = 'B'
    XFILTER = XFILTER + ".AND.TRANTYPE $'2457'"
ENDCASE

IF !EMPTY(lcRpBatch)
   XFILTER = XFILTER + " .AND. BATCH = lcRpBatch"
   XFILTER1 = XFILTER1 + " .AND. BATCH = lcRpBatch" 
ENDIF

IF !EMPTY(lcRpAcct)
  XFILTER = XFILTER + " .AND. ACCOUNT = lcRpAcct"
  XFILTER1 = XFILTER1 + " .AND. ACCOUNT = lcRpAcct"
ENDIF

LO       = DTOS(ldLwTranDt)
HI       = DTOS(ldhiTranDt)
XFILTER  = XFILTER  + ".AND.DTOS(TRANDATE)>=LO .AND. DTOS(TRANDATE)<=HI"
XFILTER1 = XFILTER1 + ".AND.DTOS(TRANDATE)>=LO .AND. DTOS(TRANDATE)<=HI"
XCRDTFLT = XFILTER   &&Save the filter expresion for Credit file.
                     &&Field lNonAR not found in Arhist and Debit files.

IF lcRpCAB $ 'CB' .AND. !EMPTY(lcRpCNB) .AND. lcRpCNB<>'B'
  DO CASE
    CASE lcRpCNB = 'A'
      XCRDTFLT = XCRDTFLT + ".AND.IIF(TranType='4',!lNonAR,.T.)"
    CASE lcRpCNB = 'N'
      XCRDTFLT = XCRDTFLT + ".AND.IIF(TranType='4',lNonAR,.T.)"
  ENDCASE
ENDIF


*-- Selecting records from the Credit file
SELECT Credit
GOTO TOP

IF lcRpFNB <> 'B'
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

  *COPY ALL TO &gcWorkDir.&TRANFILE FOR &XCRDTFLT .AND. ;
  IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))
  IF !EMPTY(lcRpAcct)
    SEEK lcRpAcct
    lcExp = "REST WHILE Account+Tran+DTOS(TranDate) = lcRpAcct "+;
    "FOR &XCRDTFLT AND IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))"
  ELSE
    lcExp = "FOR &XCRDTFLT AND IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))"
  ENDIF
  SCAN &lcExp
    WAIT WINDOW "Selecting credit transactions: "+Tran NOWAIT
    SCATTER MEMVAR MEMO
    INSERT INTO(TranFile) FROM MEMVAR
    REPLACE &TranFile..TranCode WITH cCreditCod
  ENDSCAN 
ELSE

  *COPY ALL TO &gcWorkDir.&TRANFILE FOR &XCRDTFLT
  SCAN FOR &XCRDTFLT
    WAIT WINDOW "Selecting credit transactions: "+Tran NOWAIT
    SCATTER MEMVAR MEMO
    INSERT INTO(TranFile) FROM MEMVAR
    REPLACE &TranFile..TranCode WITH cCreditCod
  ENDSCAN
ENDIF
WAIT CLEAR
*--------------------------------------------
* SELECT RECORDS FROM ARHIST FILE
*--------------------------------------------
SELECT ArHist
IF !USED('ARHIST_A')
  USE (gcDataDir+'ARHIST')  AGAIN ALIAS ARHIST_A ORDER TAG ARHISTT IN 0
ENDIF
IF lcRpFNB <> 'B'
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
  SET FILTER TO (TRANTYPE<>"8") .AND. (TRANTYPE<>'9') .AND. ;
  IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))
ELSE
  SET FILTER TO (TRANTYPE<>"8") .AND. (TRANTYPE<>'9')
ENDIF

COPY ALL TO &gcWorkDir.&WORKFILE FOR &XFILTER OR IIF(lcRpCAB = "C",.F.,lfChgArh())

SELECT &TRANFILE
APPEND FROM &gcWorkDir.&WORKFILE

*-- Selecting records from the Debit file.
IF lcRpCAB <> 'C'
  SELECT Debit
  IF lcRpFNB <> 'B'
    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

    *COPY ALL TO &gcWorkDir.&WORKFILE FOR (&XFILTER OR IIF(lcRpCAB="C",.F.,lfChgDeb())) .AND. ;
    IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))
    IF !EMPTY(lcRpAcct)
      SEEK lcRpAcct
      lcDbExp = "REST WHILE Account+Tran+cInstalNo+DTOS(TranDate) = lcRpAcct"+;
      " FOR (&XFILTER OR IIF(lcRpCAB='C',.F.,lfChgDeb())) .AND."+ ;
       " IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))"
    ELSE
      lcDbExp = "FOR (&XFILTER OR IIF(lcRpCAB='C',.F.,lfChgDeb())) .AND."+ ;
      "IIF(lcRpFNB='F',!EMPTY(Customer.cFacCode),EMPTY(Customer.cFacCode))"
    ENDIF
    SCAN &lcDbExp
      WAIT WINDOW "Selecting debit transactions: "+Tran NOWAIT
      SCATTER MEMVAR MEMO
      INSERT INTO(TranFile) FROM MEMVAR
    ENDSCAN
  ELSE
    *COPY ALL TO &gcWorkDir.&WORKFILE FOR &XFILTER OR IIF(lcRpCAB="C",.F.,lfChgDeb())
    SCAN FOR &XFILTER OR IIF(lcRpCAB="C",.F.,lfChgDeb())
      WAIT WINDOW "Selecting debit transactions: "+Tran NOWAIT
      SCATTER MEMVAR MEMO
      INSERT INTO(TranFile) FROM MEMVAR
    ENDSCAN
  ENDIF
ENDIF
WAIT CLEAR

SELECT &TRANFILE
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

FROM    = DTOC(ldLwTranDt)
THRU    = DTOC(ldhiTranDt)
XTITLE  = "PERIOD: &FROM - &THRU"

DO CASE
  * CUSTOMER
  CASE lcRpSort = 'C'
    XSORT   = 'ACCOUNT+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'ACCOUNT'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"

  * TRANSACTION NUMBER
  CASE lcRpSort = 'T'
    XSORT   = 'TRANTYPE+TRANCODE+DTOS(TRANDATE)+TRAN'
    HBREAK2 = 'TRANTYPE'
    HBREAK1 = 'TRANCODE'
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"

  * BATCH NUMBER
  CASE lcRpSort = 'B'
    XSORT   = 'BATCH+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'BATCH'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITLE  = XTITLE + " (SORT: BATCH)"
ENDCASE
INDEX ON &XSORT TAG &TRANFILE
SET ORDER TO TAG &TRANFILE

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*TRAN#    DATE   BATCH  CODE .....DESC.....  ACCT# STORE    ...........CUSTOMER........... REFERENCE                 AMOUNT'
*123456 XX/XX/XX XXXXXX  XX  XXXXXXXXXXXXXXX XXXXX XXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX 99999999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

DIMENSION TAMT(3)
TAMT   = 0.00
ROW    = 99
PAGENO = 0
XTIME  = TIME()

SELECT &TRANFILE
REPLACE TranType WITH "2" FOR TranType = "3"
GO TOP

XBREAK1 = &HBREAK1
XBREAK2 = &HBREAK2

DASHLN  = REPLICATE('-',132)
DOTLN   = REPLICATE('.',132)
DASHLN2 = REPLICATE('=',132)

*-------------------------------------------------------
* [REPORT]
*-------------------------------------------------------

R_TITLE = 'CASH & ADJUSTMENTS JOURNAL'

SET DEVICE TO PRINT
DO WHILE INKEY() <>32
   SELECT &TRANFILE

   IF ROW >= 55
      ROW = 0
      PAGENO = PAGENO+1
      DO RPT_HDR WITH 'AR930',XTITLE,R_WIDTH

      *B603997,1 BWA 10/31/2000 Fix the bug of print ***** in the amount field.[START]
      *@ 05,000 SAY 'TRAN#    DATE   BATCH  CODE .....DESC.....  ACCT# STORE    ...........CUSTOMER........... REFERENCE                 AMOUNT'
      @ 05,000 SAY 'TRAN#    DATE   BATCH  CODE .....DESC.....  ACCT# STORE    ...........CUSTOMER........... REFERENCE                      AMOUNT'
      *B603997,1 [END]

      @ 06,000 SAY REPLICATE('-',132)
      ROW = 7
   ENDIF

   LEVEL = ' '
   DO CASE
     CASE EOF()
       LEVEL = '3'
     CASE XBREAK2 <> &HBREAK2
       LEVEL = '2'
     CASE XBREAK1 <> &HBREAK1
       LEVEL = '1'
   ENDCASE

   *------------------------------------------------
   * [SUB-TOTALS]
   *------------------------------------------------
   LCNT = 1
   DO WHILE LCNT <= VAL(LEVEL)
      DO CASE
        CASE LCNT=1
          PDASH = ' '

          *B603997,1 BWA 10/31/2000 Fix the bug of print ***** in the amount field.[START]
          *@ ROW,111 SAY '------------'
          @ ROW,111 SAY '-----------------'
          *B603997,1 [END]

          IF lcRpFrm = 'S'
            PBREAK = '          TOTAL -->     '+ lcTrnCd + '  '+lcTrnDec
          ELSE
            PBREAK = '          TOTAL --> '
          ENDIF
          
        CASE LCNT=2
          PDASH = DASHLN
          @ ROW,00 SAY PDASH
          
          IF lcRpSort='T'
            DO CASE
              CASE XBREAK2='2'
                PBREAK = '          SUBTOTAL => DEBIT ADJUSTMENTS'
              CASE XBREAK2='4'
                PBREAK = '          SUBTOTAL => PAYMENTS'
              CASE XBREAK2='5'
                PBREAK = '          SUBTOTAL => CREDIT ADJUSTMENTS'
              CASE XBREAK2='7'
                PBREAK = '          SUBTOTAL => ALLOWANCES'
            ENDCASE
          ELSE
            PBREAK = '          SUBTOTAL => ' + XBREAK2
          ENDIF

        CASE LCNT=3
          PDASH = DASHLN2
          @ ROW,00 SAY PDASH
          PBREAK = '          GRAND TOTAL'
      ENDCASE
      ROW = ROW+1

      ** PSTAR = REPLICATE('*', LCNT)
      @ ROW,000 SAY PBREAK

      *B603997,1 BWA 10/31/2000 Fix the bug of print ***** in the amount field.[START]
      *@ ROW,111 SAY TAMT(LCNT)   PICTURE '99999999.99'
      @ ROW,111 SAY TAMT(LCNT)   PICTURE '9999999999999.99'
      *B603997,1 [END]

      ** @ ROW,123 SAY PSTAR
      ROW = ROW+1

      @ ROW,00 SAY PDASH
      ROW = ROW + IIF(LCNT=1, 1, 2)

      DO CASE
       CASE LCNT=1
         XBREAK1 = &HBREAK1
       CASE LCNT=2
         XBREAK2 = &HBREAK2
      ENDCASE

      IF LCNT < 3
         TAMT(LCNT+1) = TAMT(LCNT+1) + TAMT(LCNT)
         TAMT(LCNT)   = 0.00
      ENDIF

      LCNT = LCNT+1
   ENDDO

   IF LEVEL = '3'
      EXIT
   ENDIF

   IF LEVEL<>' ' .AND. ROW>50
      LOOP
   ENDIF

   SELECT &TRANFILE
   IF lcRpFrm = 'S'
     TAMT(1) = TAMT(1) + AMOUNT   
     lcTrnCd  = TRANCODE
     lcTrnDec = SUBSTR(DESC,1,15)
     SKIP
     LOOP
   ENDIF

   IF SEEK ('M'+ACCOUNT,'Customer') OR EMPTY(Account)
     SELECT &TRANFILE
     @ ROW,000 SAY TRAN
     @ ROW,007 SAY TRANDATE
     @ ROW,016 SAY BATCH
     @ ROW,024 SAY TRANCODE
     @ ROW,028 SAY SUBSTR(DESC,1,15)
     @ ROW,044 SAY ACCOUNT
     @ ROW,050 SAY STORE
     @ ROW,059 SAY CUSTOMER->BTNAME
     @ ROW,090 SAY SUBSTR(REFERENCE,1,20)

     *B603997,1 BWA 10/31/2000 Fix the bug of print ***** in the amount field.[START]
     *@ ROW,111 SAY AMOUNT PICT '99999999.99'
     @ ROW,111 SAY AMOUNT PICT '9999999999999.99'
     *B603997,1 [END]

     ROW = ROW+1
     PCODESC=SUBSTR(DESC,1,15)
     TAMT(1) = TAMT(1) + AMOUNT
   ENDIF
   SELECT &TRANFILE
   SKIP
ENDDO
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfChgArh
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : Pick the charge backs from ARHIST that were not created as a
*!             result of apply debit/credit .
*:*************************************************************
*! Example     : =lfChgArh()
*!*************************************************************
FUNCTION lfChgArh
IF TranType = "3" AND &XFILTER1
  =Seek(Account+Tran,"ARHIST_A")
  IF ARHIST_A.TranType = "3"
    RETURN 
  ENDIF
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfChgDeb
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : Pick the charge backs from DEBIT file that don't have 
*!			   entries in ARHIST file.	
*:*************************************************************
*! Example     : =lfChgDeb()
*!*************************************************************
FUNCTION lfChgDeb
IF TranType = "3" AND &XFILTER1     &&Tran. type of charge back in DEBIT file.
  IF !SEEK(Account + Tran,"ARHIST_A") 
    RETURN
  ENDIF
ENDIF
RETURN .F.


*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : To assign the transaction date range to its variables.
*:*************************************************************
*! Example     : =lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
lcObjNam = SYS(18)
ldObjVal = EVALUATE(SYS(18))

IF lcObjNam = "LCOGVALUEF" 
  IF ldObjVal = {}
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Report date should not be empty.')
  ENDIF
  ldLwTranDt = ldObjVal
ELSE
  ldHiTranDt = ldObjVal
ENDIF
&lcObjNam = ldObjVal

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : To validate the account code.
*:*************************************************************
*! Example     : =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount

lcObjNam = SYS(18)
lcAccNo   = EVALUATE(SYS(18))

IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !SEEK('M' + lcRpAcct , 'Customer'))
    =CusBrowM(@lcRpAcct , '' , 'M')
ENDIF
&lcObjNam = lcRpAcct
IF ASCAN(laOgObjType,'lcRpFNB') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpFNB'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpAcct)
  = lfOGShowGet('lcRpFNB')
ENDIF
IF ASCAN(laOgObjType,'lcRpCNB') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpCNB'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpAcct) AND lcRpCAB <> 'A'
  = lfOGShowGet('lcRpCNB')
ENDIF
IF !EMPTY(lcRpAcct)
  lcRpFNB = "B"
  = lfOGShowGet('lcRpFNB')
ENDIF
lcRpCNB = 'B'
= lfOGShowGet('lcRpCNB')

*!*************************************************************
*! Name      : lfvData
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : To enable or disable the print option.
*:*************************************************************
*! Example     : = lfvData()
*!*************************************************************
FUNCTION lfvData

IF ASCAN(laOgObjType,'lcRpCNB') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpCNB'),1)
  laOGObjCnt[lnPos] = EMPTY(lcRpAcct) AND lcRpCAB <> 'A'
  = lfOGShowGet('lcRpCNB')
ENDIF
IF !EMPTY(lcRpAcct)
  lcRpFNB = "B"
  = lfOGShowGet('lcRpCNB')
ENDIF


*!*************************************************************
*! Name      : lfOgWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/20/1999
*! Purpose   : Function to be executed in the when of the OG.
*:*************************************************************
*! Example     : = lfOgWhen()
*!*************************************************************
FUNCTION lfOgWhen

R_WIDTH   = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CREDIT.TRANDATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)
ENDIF
ldLwTranDt  = gdSysDate
ldHiTranDt  = gdSysDate