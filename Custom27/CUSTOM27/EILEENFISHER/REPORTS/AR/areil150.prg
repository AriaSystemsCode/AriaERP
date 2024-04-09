*:***************************************************************************
*: Program file  : AREIL150.PRG - (C101458)
*:                  (For Eileen Fisher)
*: Program desc. : CHARGEBACK & CREDIT ON ACCOUNT AND TRANSACTION NUMBER
*: System        : Aria Apparel System. Ver. 2.7
*: Developer     : AHMED SALAH SHALABY - (SSH)
*:***************************************************************************
*---------------------------------------------------------------------------
*  3 = CHARGEBACK         DEBIT
*  6 = OPEN CREDIT        CREDIT
*---------------------------------------------------------------------------

SELECT CUSTOMER
lcSorTcd   = lcRpSortBy
lcDS       = lcRpForm 
lcCOB      = lcRpSelBy
ldHDate    = {}
ldLDate    = {}
ldNulDate  = {}
lcLAccount = SPACE(05)
lcHAccount = SPACE(05)
lcLTran    = SPACE(06)
lcHTran    = SPACE(06)
*--- Function to update filter variable.
=lfUpdFltVar()
IF ldHDate = ldNulDate OR ldLDate = ldNulDate
  WAIT WINDOW "You Must Enter Date Range...!"
  RETURN
ENDIF
lcFilter = "AMOUNT<>0.AND.(TRANTYPE='3'.OR.TRANTYPE='6')"
ldHDate  = IIF(ldHDate=ldNulDate, ldLDate, ldHDate)
lcFilter = lcFilter + ".AND. BETWEEN(TRANDATE,ldLDate,ldHDate)"
*-- Start filter for Account no.
DO CASE
  CASE !EMPTY(lcLAccount) .AND. !EMPTY(lcHAccount)
    lcFilter=lcFilter+".AND.BETWEEN(ACCOUNT,lcLAccount,lcHAccount)"
  CASE EMPTY(lcLAccount) .AND. !EMPTY(lcHAccount)
    lcFilter=lcFilter+".AND. ACCOUNT <= lcHAccount"
  CASE !EMPTY(lcLAccount) .AND. EMPTY(lcHAccount)
    lcFilter=lcFilter+".AND. ACCOUNT >= lcLAccount"
ENDCASE
*-- Start filter for Transaction no.
DO CASE
  CASE !EMPTY(lcLTran) .AND. !EMPTY(lcHTran)
    lcFilter=lcFilter+".AND.BETWEEN(Tran,lcLTran,lcHTran)"
  CASE EMPTY(lcLTran) .AND. !EMPTY(lcHTran)
    lcFilter=lcFilter+".AND. Tran <= lcHTran"
  CASE !EMPTY(lcLTran) .AND. EMPTY(lcHTran)
    lcFilter=lcFilter+".AND. Tran >= lcLTran"
ENDCASE

* --Start copy records matching filter from DEBIT to temp file (lcWorkFile)
SELECT DEBIT
WAIT WINDOW 'SELECTING CHARGE BACKS ...' NOWAIT
lcWorkFile = gfTempName()
SET TALK ON
COPY TO &gcWorkDir.&lcWorkFile FOR &lcFilter
SET TALK OFF
= gfOpenFile (gcWorkDir+ "&lcWorkFile","", "EX")
lcTempFile = gfTempName()
*-- SKIP OPEN CREDITS IF REPORT = CHARGE BACK ONLY
IF lcCOB $ 'OB'
   WAIT WINDOW 'SELECTING OPEN CREDITS ...' NOWAIT
   SET TALK ON
   SELECT CREDIT
   COPY TO &gcWorkDir.&lcTempFile FOR &lcFilter
   SET TALK OFF
   SELECT &lcWorkFile
   APPEND FROM &gcWorkDir.&lcTempFile
ENDIF
SELECT &lcWorkFile
*--Check if the temp file that containing selected record is empty
GOTO TOP
IF EOF()
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING &Z RECORDS ...' NOWAIT

lcFrom  = DTOC(ldLDate)
lcThru  = DTOC(ldHDate)
lcTitle = "PERIOD: &lcFrom - &lcThru"
*--[SORT] WORK FILE TO REPORT SEQUENCE

DO CASE
  CASE lcSorTcd = 'C'
    lcHBreak1 = ACCOUNT
    lcTitle  = lcTitle + " (SORT: CUSTOMER)"
    SET TALK ON
    INDEX ON ACCOUNT+TRAN TO &gcWorkDir.&lcWorkFile
    SET TALK OFF
    *-- TRANSACTION NUMBER
  CASE lcSorTcd = 'T'
    lcHBreak1 = ' '
    lcTitle  = lcTitle + " (SORT: TRANSACTION)"
    SET TALK ON
    INDEX ON TRAN TO &gcWorkDir.&lcWorkFile
    SET TALK OFF
    *-- DATE OF TRANSACTION
  CASE lcSorTcd = 'D'
    lcTitle  = lcTitle + " (SORT: DATE)"
    SET TALK ON
    INDEX ON DTOS(TRANDATE) TO &gcWorkDir.&lcWorkFile
    GOTO TOP
    lcHBreak1 = DTOC(TRANDATE)
    SET TALK OFF
ENDCASE
SET INDEX TO &gcWorkDir.&lcWorkFile
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL340                             123456789012345678901234567890 - CASH & ADJUSTMENTS JOURNAL                      MM/DD/YY
*PAGE: 123                                  PERIOD: MM/DD/YY - MM/DD/YY                                                      HH:MM:SS
*
*TRAN#..DATE     CODE ...DESC....... ACCT# REFERNCE ...........CUSTOMER........... ..ADDL-REF... CHGBCK   AMOUNT(+)   AMOUNT(-)
*                                    12345 12345678 123456789012345678901234567890 1234567890123 123456 12345678.99 12345678.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*123456 XX/XX/XX XXX 123456789012345

*--Declare array to hold Total Amount (laTamt)

DIMENSION laTamt(2,2)
laTamt   = 0.00
lnRow    = 99
PAGENO   = 0
ltTIME   = TIME()
SELECT &lcWorkFile
lcDashLn = REPLICATE('-',132)
DOTLN   = REPLICATE('.',132)
DASHLN2 = REPLICATE('=',132)

*-- Start print [REPORT] 
R_TITLE = 'CHARGE BACK & CREDIT ON ACCOUNT AND TRANS.NO. JOURNAL'
CLEAR TYPEAHEAD
WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

*--Loop for printing report exit when keypreesed = 32 (Space bar)
DO WHILE INKEY() <> 32
   SELECT &lcWorkFile
   IF lnRow >= 55
      lnRow = 0
      PAGENO = PAGENO+1
      DO RPT_HDR WITH 'AREIL150',lcTitle,R_WIDTH
      @ 06,000 SAY 'TRAN#  DATE    CODE ......DESC..... ACCT# STORE    ...........CUSTOMER........... ....REFERENCE....      AMOUNT(+)   AMOUNT(-)'
      lnRow = 7
   ENDIF
   *--[S] DETERMINE WHICH CONTROL BREAK FOR SUB TOTALS
   lcLevel = ' '
   DO CASE
     CASE EOF()
       lcLevel = '2'
       *-- ACCOUNT NO CONTROL BREAK
     CASE lcSorTcd = 'C'
       IF ACCOUNT <> lcHBreak1
          lcLevel = '1'
       ENDIF
       *-- TRANSACTION DATE CONTROL BREAK
     CASE lcSorTcd = 'D'
       IF DTOC(TRANDATE) <> lcHBreak1
          lcLevel = '1'
       ENDIF
   ENDCASE
   *--[SUB-TOTALS]
   LCNT  = 1
   lcPDash = ' '
   lcPStar = REPLICATE('*', LCNT)
   DO WHILE LCNT <= VAL(lcLevel)
      DO CASE
        CASE LCNT=1 .AND. lcHBreak1<>' '
          PBREAK = ' '
          @ lnRow,103 SAY '------------'
          @ lnRow,115 SAY '------------'
          lnRow = lnRow+1
          @ lnRow,000 SAY PBREAK
          @ lnRow,103 SAY laTamt(LCNT,1)   PICTURE '99999999.99'
          @ lnRow,115 SAY laTamt(LCNT,2)   PICTURE '99999999.99'
          @ lnRow,132 SAY lcPStar
          lnRow = lnRow+1
        CASE LCNT=2
          lcPDash  = DASHLN2
          PBREAK = '** GRAND TOTAL'
          @ lnRow,00 SAY lcPDash
          lnRow = lnRow+1
          @ lnRow,000 SAY PBREAK
          @ lnRow,103 SAY laTamt(LCNT,1)   PICTURE '99999999.99'
          @ lnRow,115 SAY laTamt(LCNT,2)   PICTURE '99999999.99'
          lnRow = lnRow+1
      ENDCASE
      lnRow = lnRow+1
      @ lnRow,00 SAY lcPDash
      lnRow = lnRow + IIF(LCNT=1, 1, 2)
      DO CASE
       CASE LCNT=1 .AND. lcSorTcd='C'
         lcHBreak1 = ACCOUNT
       CASE LCNT=1 .AND. lcSorTcd='D'
         lcHBreak1 = DTOC(TRANDATE)
      ENDCASE
      IF LCNT < 2
         laTamt(LCNT+1,1) = laTamt(LCNT+1,1) + laTamt(LCNT,1)
         laTamt(LCNT+1,2) = laTamt(LCNT+1,2) + laTamt(LCNT,2)
         laTamt(LCNT,1)  = 0.00
         laTamt(LCNT,2)  = 0.00
      ENDIF
      LCNT = LCNT+1
   ENDDO
   IF lcLevel = '2'
      EXIT
   ENDIF
   IF lcLevel<>' ' .AND.  lnRow>50
      LOOP
   ENDIF
   SELECT &lcWorkFile
   DO CASE
     CASE AMOUNT >0
       laTamt(1,1) = laTamt(1,1) + AMOUNT
     CASE AMOUNT <0
       laTamt(1,2) = laTamt(1,2) + AMOUNT
   ENDCASE
   SELECT &lcWorkFile
   IF lcDS = 'S'
      SKIP
      LOOP
   ENDIF
   lcAccount = 'M'+ACCOUNT
   SELECT CUSTOMER
   SEEK lcAccount
   SELECT &lcWorkFile
   @ lnRow,000 SAY TRAN
   @ lnRow,007 SAY TRANDATE
   @ lnRow,016 SAY SUBSTR(TRANCODE,1,2)
   @ lnRow,020 SAY SUBSTR(DESC,1,16)
   @ lnRow,036 SAY ACCOUNT
   @ lnRow,042 SAY STORE
   @ lnRow,051 SAY CUSTOMER.BTNAME
   @ lnRow,082 SAY SUBSTR(REFERENCE,1,20)
   DO CASE
     CASE AMOUNT >0
       @ lnRow,103 SAY AMOUNT        PICTURE '99999999.99'
     CASE AMOUNT <0
       @ lnRow,115 SAY AMOUNT        PICTURE '99999999.99'
   ENDCASE
   lnRow = lnRow+1
   SELECT &lcWorkFile
   SKIP
ENDDO
DO ENDREPORT
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF
ERASE &gcWorkDir.&lcWorkFile+'.DBF'
ERASE &gcWorkDir.&lcWorkFile+'.CDX'
ERASE &gcWorkDir.&lcTempFile+'.DBF'
SET DEVICE TO SCREEN
RETURN
*--END AREIL150.PRG (AHMED SALAH SHALABY - SSH)

*!*************************************************************
*! Name      : lfvAcct
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 03/18/99
*! Purpose   : Validate the enterd account
*!*************************************************************
*! Example     : = lfvAcct()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  xAccount = lcObjVal
  DO CUSBROWM WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 03/18/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

R_WIDTH='W'
*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,"DEBIT.TRANDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 03/18/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcLAccount = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,5))
      lcHAccount = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],7,5))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'DEBIT.TRANDATE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ldLDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldHDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,10)))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CREDIT.TRAN' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcLTran = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,6))
      lcHTran = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],8,6))
  ENDCASE
ENDFOR