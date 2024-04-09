*:************************************************************************
*: Program file  : ARCHADJ.Prg
*: Program desc. : CHARGEBACK & CREDIT ON ACCOUNT JOURNAL
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 11/12/1998
*: REF           : *301055,1 
*: Notes         : This program is built on the standard program AR950
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfFlCur()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARCHBCR
*:************************************************************************
*B802254,1 BWA 05/27/1999 Print date period in the report header
*B802764,1 ABD 11/17/1999 Disable the Advanced Bottom in the Option Gried (Done in SYDREPRT).
*B803247,1 NAD 06/04/2000 Fix bug when selecting open-credits the report prints out all the
*B803247,1                transcations both chargebacks and open-credits.
*B803190,1 BWA 05/25/2000 Replacing the reason code description instead of the reason code.
*E301429,1 NAD 07/09/2000 Add record to syrepuvr to ask the user to print
*E301429,1                decimals or not.  
*:************************************************************************
*---------------------------------------------------------------------
*  3 = CHARGEBACK         DEBIT
*  5 = OPEN CREDIT        CREDIT
*---------------------------------------------------------------------
*B802254,1 BWA Adding the date to the report header 
*XTITLE  = ''

*E301429,1 (Start) variable to hold the picture
lcDecPic=IIF (llRpDec,'99999999.99','99999999999')
*E301429,1 (End)


lnDatePos = ASCAN(laOGFxFlt,"CREDIT.TRANDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)

  DO CASE
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        XTITLE   = ''
      
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        Hdata     = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE    = "PERIOD: To &Hdata "
        
    CASE !EMPTY(CTOD(laOGFxFlt[lnDatePos,6]))
        Ldata    = SUBSTR(laOGFxFlt[lnDatePos,6],1,10)
        Hdata    = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE   = "PERIOD FROM: &Ldata To &Hdata "
  ENDCASE
ENDIF
*B802254,1 BWA [END]

QTOPIC  = 'CHARGE BACK AND CREDIT REPORT'
R_TITLE = 'CHARGE BACK & CREDIT ON ACCOUNT JOURNAL'
R_WIDTH = 'W'

*-- Extract the 'Credit' word from the filter, because the same filter will 
*-- be applied on the credit and debit files
lcRpExp = STRTRAN(lcRpExp,'CREDIT.TRANDATE','TRANDATE')
lcRpExp = STRTRAN(lcRpExp,'CREDIT.ACCOUNT','ACCOUNT')
lcRpExp = STRTRAN(lcRpExp,'CREDIT.CCURRCODE','CCURRCODE')

*-- Add a filter to get Charge back and Open credit only.
lcRpExp = '(' +lcRpExp+ ") .AND. AMOUNT <> 0 .AND. INLIST(TRANTYPE, '3' , '6')"


SELECT DEBIT

*B803247,1  (Start) Fill the work file with data only in case of charge back 
WORKFILE = gfTempName()
DO CASE 
  CASE lcRpCOB $ 'CB'
*B803247,1  (End)
   
    WAIT WINDOW 'SELECTING CHARGE BACKS ...' NOWAIT
    
    *B803247,1  (Start) Commented out
    *WORKFILE = gfTempName() 
    *B803247,1  (End) 
    
    SET TALK ON
    COPY TO (gcWorkDir+WORKFILE) FOR &lcRpExp
    SET TALK OFF
    
*B803247,1  (Start) in case of open credits create an empty file    
    *=gfOpenFile(gcWorkDir+WORKFILE,'','EX')
  CASE lcRpCOB= 'O'
    COPY STRUCTURE TO (gcWorkDir+WORKFILE) 
ENDCASE
=gfOpenFile(gcWorkDir+WORKFILE,'','EX')
*B803247,1  (End)

TEMPFILE = gfTempName()

* SKIP OPEN CREDITS IF REPORT = CHARGE BACK ONLY
IF lcRpCOB $ 'OB'
   WAIT WINDOW 'SELECTING OPEN CREDITS ...' NOWAIT
   SELECT CREDIT

   SET TALK ON
   COPY TO (gcWorkDir+TEMPFILE) FOR &lcRpExp
   SET TALK OFF

   =gfOpenFile(gcWorkDir+TEMPFILE,'','EX')

   SCAN
     SCATTER MEMVAR MEMO
     m.TranCode = m.cCreditCod
     INSERT INTO (WORKFILE) FROM MEMVAR
   ENDSCAN

ENDIF

SELECT &WORKFILE
GOTO TOP
IF EOF()
   *-- Message 'There are no records to display...!'
   =gfModalGen('TRM00052B00000','DIALOG')
   RETURN
ENDIF
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING '+Z+' RECORDS ...' NOWAIT

*-- [SORT] WORK FILE TO REPORT SEQUENCE
DO CASE
  *-- CUSTOMER
  CASE lcrpSRTCD = 'C'
    HBREAK1 = ACCOUNT
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"
    SET TALK ON
    INDEX ON ACCOUNT+TRAN TAG &WORKFILE
    SET TALK OFF

  *-- TRANSACTION NUMBER
  CASE lcrpSRTCD = 'T'
    HBREAK1 = ' '
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"
    SET TALK ON
    INDEX ON TRAN TAG &WORKFILE
    SET TALK OFF

  *-- DATE OF TRANSACTION
  CASE lcrpSRTCD = 'D'
    HBREAK1 = DTOC(TRANDATE)
    XTITLE  = XTITLE + " (SORT: DATE)"
    SET TALK ON
    INDEX ON DTOS(TRANDATE) TAG &WORKFILE
    SET TALK OFF
ENDCASE
SET ORDER TO TAG &WORKFILE

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL340                             123456789012345678901234567890 - CASH & ADJUSTMENTS JOURNAL                      MM/DD/YY
*PAGE: 123                                  PERIOD: MM/DD/YY - MM/DD/YY                                                      HH:MM:SS
*
*TRAN#  DATE     CODE   ......DESC.....  ACCT# STORE    ...........CUSTOMER........... .......REFERENCE......   AMOUNT(+)   AMOUNT(-)
*XXXXXX XX/XX/XX 123456 1234567890123456 12345 12345678 123456789012345678901234567890 1234567890123456789012 99999999.99 99999999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*123456 XX/XX/XX XXX 123456789012345

DIMENSION TAMT(2,2)
TAMT   = 0.00
ROW    = 99
PAGENO = 0
XTIME  = TIME()

SELECT &WORKFILE

DASHLN  = REPLICATE('-',132)
DOTLN   = REPLICATE('.',132)
DASHLN2 = REPLICATE('=',132)

*-------------------------------------------------------
* [REPORT]
*-------------------------------------------------------
SET DEVICE TO PRINT

DO WHILE .T.
  SELECT &WORKFILE
  IF ROW >= 55
    ROW = 0
    PAGENO = PAGENO+1
    DO RPT_HDR WITH 'ARCHBCR',XTITLE,R_WIDTH
    
    *B803190,1 BWA 05/25/2000 Replacing the reason code description instead of the reason code.[START]
    * @ 06,000 SAY 'TRAN#  DATE     CODE   ......DESC.....  ACCT# STORE    ...........CUSTOMER........... .......REFERENCE......   AMOUNT(+)   AMOUNT(-)'   
    @ 06,000 SAY 'TRAN#  DATE     CODE           ......DESC.....  ACCT# STORE    .........CUSTOMER......... .....REFERENCE....   AMOUNT(+)   AMOUNT(-)'
    *B803190,1 [END]

    ROW = 7
  ENDIF

  *-----------------------------------------------------
  * [S] DETERMINE WHICH CONTROL BREAK FOR SUB TOTALS
  *-----------------------------------------------------
  LEVEL = ' '
  DO CASE
    CASE EOF()
      LEVEL = '2'

    *-- ACCOUNT NO CONTROL BREAK
    CASE lcrpSRTCD = 'C'
      IF ACCOUNT <> HBREAK1
        LEVEL = '1'
      ENDIF

    *-- TRANSACTION DATE CONTROL BREAK
    CASE lcrpSRTCD = 'D'
      IF DTOC(TRANDATE) <> HBREAK1
        LEVEL = '1'
      ENDIF
  ENDCASE

  *------------------------------------------------
  * [SUB-TOTALS]
  *------------------------------------------------
  LCNT  = 1
  PDASH = ' '
  PSTAR = REPLICATE('*', LCNT)
  DO WHILE LCNT <= VAL(LEVEL)
    DO CASE
      CASE LCNT=1 .AND. HBREAK1<>' '
        PBREAK = ' '
        @ ROW,109 SAY '------------'
        @ ROW,121 SAY '------------'
        ROW = ROW+1
        @ ROW,000 SAY PBREAK
        *E301429,1 (Start) Print the picture from the variable
        *@ ROW,109 SAY TAMT(LCNT,1)   PICTURE '99999999.99'        
        *@ ROW,121 SAY TAMT(LCNT,2)   PICTURE '99999999.99'
        @ ROW,109 SAY TAMT(LCNT,1)   PICTURE lcDecPic
        @ ROW,121 SAY TAMT(LCNT,2)   PICTURE lcDecPic
        *E301429,1 (End)
        
        @ ROW,132 SAY PSTAR
        ROW = ROW+1

      CASE LCNT=2
        PDASH  = DASHLN2
        PBREAK = '** GRAND TOTAL'
        @ ROW,00 SAY PDASH
        ROW = ROW+1
        @ ROW,000 SAY PBREAK
        *E301429,1 (Start) Print the picture from the variable variable        
        *@ ROW,109 SAY TAMT(LCNT,1)   PICTURE '99999999.99'
        *@ ROW,121 SAY TAMT(LCNT,2)   PICTURE '99999999.99'
        @ ROW,109 SAY TAMT(LCNT,1)   PICTURE lcDecPic
        @ ROW,121 SAY TAMT(LCNT,2)   PICTURE lcDecPic
        *E301429,1 (End) 
        ROW = ROW+1
    ENDCASE
    
    ROW = ROW+1
    @ ROW,00 SAY PDASH
    ROW = ROW + IIF(LCNT=1, 1, 2)

    DO CASE
      CASE LCNT=1 .AND. lcrpSRTCD='C'
        HBREAK1 = ACCOUNT

      CASE LCNT=1 .AND. lcrpSRTCD='D'
        HBREAK1 = DTOC(TRANDATE)
    ENDCASE

    IF LCNT < 2
      TAMT(LCNT+1,1) = TAMT(LCNT+1,1) + TAMT(LCNT,1)
      TAMT(LCNT+1,2) = TAMT(LCNT+1,2) + TAMT(LCNT,2)
      TAMT(LCNT,1)  = 0.00
      TAMT(LCNT,2)  = 0.00
    ENDIF

    LCNT = LCNT+1
  ENDDO

  IF LEVEL = '2'
    EXIT
  ENDIF

  IF LEVEL<>' ' .AND. ROW>50
    LOOP
  ENDIF

  SELECT &WORKFILE
  DO CASE
    CASE AMOUNT >0
      TAMT(1,1) = TAMT(1,1) + AMOUNT
    CASE AMOUNT <0
      TAMT(1,2) = TAMT(1,2) + AMOUNT
  ENDCASE

  SELECT &WORKFILE
  IF lcRpDS = 'S'
    SKIP
    LOOP
  ENDIF

  XACCOUNT = 'M'+ACCOUNT
  SELECT CUSTOMER
  SEEK XACCOUNT

  SELECT &WORKFILE
  @ ROW,000 SAY TRAN
  @ ROW,007 SAY TRANDATE
  
  *B803190,1 BWA 05/25/2000 Replacing the reason code description instead of the reason code.[START]
  *@ ROW,016 SAY TRANCODE
  *@ ROW,023 SAY SUBSTR(DESC,1,16)
  *@ ROW,040 SAY ACCOUNT
  *@ ROW,046 SAY STORE
  *@ ROW,055 SAY CUSTOMER->BTNAME
  *@ ROW,086 SAY LEFT(REFERENCE,22)
  
  @ ROW,016 SAY LEFT(IIF(EMPTY(TRANCODE),"",IIF(AMOUNT > 0 ,gfCodDes(&WORKFILE..TRANCODE,'TRANCODE'),gfCodDes(&WORKFILE..TRANCODE,'CCREDITCOD'))),14)
  @ ROW,032 SAY SUBSTR(DESC,1,16)
  @ ROW,048 SAY ACCOUNT
  @ ROW,054 SAY STORE
  @ ROW,063 SAY LEFT(CUSTOMER.BTNAME,26)
  @ ROW,080 SAY LEFT(REFERENCE,18)
  *B803190,1 [END]
  
  DO CASE
   
    CASE AMOUNT >0
      *E301429,1 (Start) Print the picture from the variable 
      *@ ROW,109 SAY AMOUNT        PICT '99999999.99'
      @ ROW,109 SAY AMOUNT        PICT lcDecPic
      *E301429,1 (End)
    CASE AMOUNT <0
      *E301429,1 (Start) Print the picture from the variable  
      *@ ROW,121 SAY AMOUNT        PICT '99999999.99'
      @ ROW,121 SAY AMOUNT        PICT lcDecPic
      *E301429,1 (End)
  ENDCASE

  ROW = ROW+1
  SELECT &WORKFILE
  SKIP
ENDDO

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 11/12/1998
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************

FUNCTION lfwOGWhen
IF llMultCurr
  lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CREDIT.CCURRCODE'),1)
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
ENDIF

*!*************************************************************
*! Name      : lfFlCur
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 11/12/1998
*! Purpose   : Fill the array of currency POPUP
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFlCur()
*!*************************************************************
FUNCTION lfFlCur

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.

IF llMultCurr
  DIMENSION laCurrVal[1,1]
  SELECT DISTINCT CCURRCODE ;
    FROM SYCCURR ;
    ORDER BY CCURRCODE INTO ARRAY laCurrVal
  
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO TAG Ccurrcode
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
