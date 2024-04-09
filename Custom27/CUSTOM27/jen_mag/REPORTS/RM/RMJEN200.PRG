*:********************************************************************
*: Program file  : RMJEN200  (REFERENCE, C102064)
*: Program desc. : Customize R\A Report For (JEN).
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*!********************************************************************
*!********************************************************************
*: Calls         : lfPrnRep()
*:                 lfSortSale()
*:                 lfAddRec()             
*:                                  
*:                 
*:********************************************************************
*: Passed Parameters  :
*:********************************************************************
*: Example            : 
*:********************************************************************
*-- XREPORT  variable of the option grid
XFILTER       = lcRpExp          && GET    THE XFILTER 
XSORT_BY      = lcRpSORT         && SORT   THE TEMPFILE
XTITLE        = lcRpopTi        && SELECT OPTIONAL TITLE
RATEMP        = gfTempName()
*-- select from file according to xfilter which hold criteria
SELECT RETAUTH
SET FILTER TO &XFILTER
GOTO TOP
IF EOF()
  =gfDialog('I','No records have been selected for the report')
ELSE
  =lfPrnRep()
  
  SELECT RETAUTH
  SET FILTER TO
  SET RELATION TO
ENDIF  
IF USED(RATEMP)
  SELECT (RATEMP) 
  USE IN (RATEMP)
 ERASE (gcWorkDir+RATEMP+'.DBF')
 ERASE (gcWorkDir+RATEMP+'.CDX')
ENDIF
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfSortSale
*! Developer : ABD ELRAHIM KOURA-KAM
*! Date      : 12/03/2000
*! Purpose   : To sort the file by the saelsrep
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfSortSale()
*!*************************************************************
FUNCTION lfSortSale
PRIVATE laFileStru

SELECT RETAUTH
=AFIELDS(laFileStru)
lnArrElem = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnArrElem,4]
laFileStru[lnArrElem,1] = 'Rep1'
laFileStru[lnArrElem,2] = 'C'
laFileStru[lnArrElem,3] = 3
laFileStru[lnArrElem,4] = 0
CREATE TABLE (gcWorkDir + RATEMP) FROM ARRAY laFileStru
INDEX ON Rep1+Account+RaNo TAG (RATEMP)

SELECT RETAUTH
SCAN
  SCATTER MEMVAR MEMO
    IF !EMPTY(Invoice) AND SEEK(Invoice, 'INVHDR')
      IF !EMPTY(InvHdr.Rep1) OR !EMPTY(InvHdr.Rep2)
        = lfAddRec()
        REPLACE Rep1 WITH IIF(!EMPTY(InvHdr.Rep1),InvHdr.Rep1,InvHdr.Rep2)
        IF !EMPTY(InvHdr.Rep2) AND !EMPTY(InvHdr.Rep1) AND InvHdr.Rep1 <> InvHdr.Rep2
          =lfAddRec()
          REPLACE Rep1 WITH InvHdr.Rep2
        ENDIF   
      ENDIF         
    ELSE
      IF SEEK ('M'+Account, 'Customer')  AND (!EMPTY(Customer.Salesrep) OR !EMPTY(Customer.Rep2))
        =lfAddRec()
        REPLACE Rep1 WITH IIF(!EMPTY(Customer.Salesrep),Customer.Salesrep,Customer.Rep2)
        IF !EMPTY(Customer.Rep2) AND !EMPTY(Customer.Salesrep) AND Customer.Salesrep <> Customer.Rep2
          =lfAddRec()
          REPLACE Rep1 WITH Customer.Rep2
        ENDIF           
      ENDIF
    ENDIF  
ENDSCAN
SELECT (RATEMP)
RETURN

*!*************************************************************
*! Name      : lfAddRec
*! Developer : ABD ELRAHIM KOURA-KAM
*! Date      : 12/03/2000
*! Purpose   : Adding a new record in the temporary file
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfAddRec()
*!*************************************************************
FUNCTION lfAddRec

SELECT (RATEMP)
APPEND BLANK
GATHER MEMVAR MEMO

*!*************************************************************
*! Name      : lfPrnRep
*! Developer : ABD ELRAHIM KOURA-KAM
*! Date      : 12/03/2000
*! Purpose   : To print the report
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrnRep()
*!*************************************************************
FUNCTION lfPrnRep

DO CASE
  CASE XSORT_BY = 'A'
    SET ORDER TO TAG RETAUTHA
    XBREAK = "ACCOUNT"
  CASE XSORT_BY = 'R'
    SET ORDER TO TAG RETAUTH
    XBREAK = "  "
  CASE XSORT_BY = 'W'
    XBREAK = "CWARECODE"
    RATEMP = gfTempName()
    COPY ALL TO &RATEMP
    USE (RATEMP) IN 0
    Z = STR( RECCOUNT() ,7)
    WAIT 'Sorting &Z transactions by reason code...!' WINDOW NOWAIT
    SET TALK ON
    SELECT (RATEMP)
    INDEX ON CWARECODE+RANO TAG &RATEMP
  CASE XSORT_BY = 'S'
   =lfSortSale()
   
   *C102064,4 KAM 19/02/2001 display message if no records selection[start]
   IF EOF()
     =gfDialog('I','No records have been selected for the report')
     RETURN
   ENDIF  
  *C102064,4 KAM [end]
   XBREAK = "REP1"
  OTHERWISE
    XBREAK = "REASON"
    RATEMP = gfTempName()
    COPY ALL TO &RATEMP
    USE (RATEMP) IN 0
    Z = STR( RECCOUNT() ,7)
    WAIT 'Sorting &Z transactions by reason code...!' WINDOW NOWAIT
    SELECT (RATEMP)
    INDEX ON REASON+RANO TAG &RATEMP
ENDCASE

GO TOP
DO CASE
  CASE XSORT_BY = 'A'
    XBREAK_FLD = ACCOUNT
  CASE XSORT_BY = 'R'
    XBREAK_FLD = "  "
  CASE XSORT_BY = 'W'
    XBREAK_FLD = CWARECODE
  CASE XSORT_BY = 'S'
    XBREAK_FLD = Rep1
  OTHERWISE
    XBREAK_FLD = REASON
ENDCASE

SET RELATION TO IIF( EMPTY(STORE) , 'M'+ACCOUNT , 'S'+ACCOUNT+STORE ) ;
INTO CUSTOMER

lcRep    = SPACE(3)
ROW      =  99
PAGENO   =  0
R_WIDTH  = 'W'
R_TITLE  = 'CUSTOMIZED RETURN AUTHORIZATION REPORT'

DIMENSION XTOT(2,4)
STORE 0 TO XTOT
lnStrtRow = IIF(llMultWare,7,0)

*DO MSG22 WITH 'report printing press <SPACE BAR > to ABORT ..','@'

*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO.... WAREHS AUTH/PCS   AUTH/AMT
*! STATUS STORE... REASON.............. VOID.... ORDER. TRAN......        RETR/PCS   RETR/AMT RETDATE
*!0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*! ±±±±±± ±±±±±    ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±±±±± ±±±±±±  ±±±±±±± ±±±±±±±.±±
*! ±±±±±± ±±±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±/±±/±± ±±±±±± ±±±±±±     ±±±±±±  ±±±±±±± ±±±±±±±.±± ±±/±±/±±

SET DEVICE TO PRINT
SCAN WHILE .T.
  IF ROW > 53
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'JEN200',XTITLE,R_WIDTH
    ROW = 5
    *@ ROW+0,01 SAY 'RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO....'+IIF(llMultWare,' WAREHS','')+' AUTH/PCS  AUTH/AMT'
    *@ ROW+1,01 SAY 'STATUS STORE... REASON.............. VOID.... ORDER. TRAN......'+IIF(llMultWare,SPACE(07),'')+' RETR/PCS  RETR/AMT RETDATE'

    @ ROW+0,01 SAY 'RANO   ACCT#    NAME................ RADATE.. INVOIC CUSTPO....'+IIF(llMultWare,' WAREHS','')+' AUTH/PCS    AUTH/AMT'
    @ ROW+1,01 SAY 'STATUS STORE... REASON.............. VOID.... ORDER. TRAN......'+IIF(llMultWare,SPACE(07),'')+' RETR/PCS    RETR/AMT RETDATE'
 


    ROW = ROW + 3       
  ENDIF
  
  IF !EMPTY(XBREAK)
    IF &XBREAK <> XBREAK_FLD
      @ ROW,00 SAY REPLICATE('-',132)
      ROW = ROW + 1
      @ ROW,01 SAY XBREAK_FLD
      @ ROW,15 SAY 'SUB-TOTAL=>'
      @ ROW,29 SAY 'AUTH PCS/AMT->'
      @ ROW,45 SAY XTOT(1,1) PICTURE '999999'
      @ ROW,55 SAY XTOT(1,2) PICTURE '9999999999.99'
      @ ROW,66 SAY 'RETR PCS/AMT->'
      @ ROW,80 SAY XTOT(1,3) PICTURE '999999'
      @ ROW,90 SAY XTOT(1,4) PICTURE '9999999999.99'
      ROW = ROW + 1
      @ ROW,00 SAY REPLICATE('-',132)
      ROW = ROW + 1
      XBREAK_FLD = &XBREAK
      STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
    ENDIF
  ENDIF

  IF INKEY() = 32 .OR. EOF()
    EXIT
  ENDIF

  IF XSORT_BY $ 'AR'
    SELECT RETAUTH
  ELSE
    SELECT (RATEMP)
  ENDIF
  IF XSORT_BY = 'S'
    IF lcRep <> XBREAK_FLD
      @ ROW,00 SAY "** SALESREP :=>"
      @ ROW,17 SAY XBREAK_FLD
      lcRep = XBREAK_FLD
      ROW = ROW + 1
    ENDIF
  ENDIF  
  @ ROW,01 SAY RANO
  @ ROW,08 SAY ACCOUNT
  @ ROW,17 SAY SUBSTR(CUSTOMER->STNAME,1,20)
  @ ROW,38 SAY RADATE
  @ ROW,47 SAY INVOICE
  @ ROW,54 SAY SUBSTR(CUSTPO,1,10)
  IF llMultWare
    @ ROW,65 SAY cWareCode
  ENDIF
  @ ROW,66+lnStrtRow SAY AUTH     PICTURE '9999999'
  @ ROW,74+lnStrtRow SAY AUTHAMT  PICTURE '99999999.99'

  ROW = ROW + 1
  DO CASE
    CASE STATUS = 'O'
      @ ROW,01 SAY 'OPEN'
    CASE STATUS = 'C'
      @ ROW,01 SAY 'COMPLT'
    CASE STATUS = 'X'
      @ ROW,01 SAY 'CANCLD'
  ENDCASE

  XREASON = REASON
    
  XRETU_DATA = SUBSTR(gfCodDes(XREASON,'REASON'),1,18)

  IF XSORT_BY $ 'RA'
    SELECT RETAUTH
  ELSE
    SELECT (RATEMP)
  ENDIF

  @ ROW,08 SAY STORE
  @ ROW,17 SAY XRETU_DATA
  @ ROW,38 SAY VOID
  @ ROW,47 SAY ORDER
  @ ROW,54 SAY TRAN
  @ ROW,66+lnStrtRow SAY RETURN    PICTURE '9999999'
  @ ROW,74+lnStrtRow SAY RETURNAMT PICTURE '99999999.99'
  @ ROW,86+lnStrtRow SAY RETDATE
  
  ROW = ROW + 1

  IF !EMPTY(CRETNOTE1)
    @ ROW,01 SAY 'NOTES:'
  ENDIF

  FOR X = 1 TO 4
    Z = STR(X,1)
    IF !EMPTY( CRETNOTE&Z )
      @ ROW,07 SAY CRETNOTE&Z
      ROW = ROW + 1
    ENDIF
  ENDFOR

  @ ROW,00 SAY REPLICATE('.',132)
  ROW = ROW + 1

  *** UPDATE THE SUBTOTALS
  XTOT(1,1) = XTOT(1,1) + AUTH
  XTOT(1,2) = XTOT(1,2) + AUTHAMT
  XTOT(1,3) = XTOT(1,3) + RETURN
  XTOT(1,4) = XTOT(1,4) + RETURNAMT

  *** UPDATE THE GRAND TOTALS
  XTOT(2,1) = XTOT(2,1) + AUTH
  XTOT(2,2) = XTOT(2,2) + AUTHAMT
  XTOT(2,3) = XTOT(2,3) + RETURN
  XTOT(2,4) = XTOT(2,4) + RETURNAMT

ENDSCAN
IF !EMPTY(XBREAK)
  IF &XBREAK <> XBREAK_FLD
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    @ ROW,01 SAY XBREAK_FLD
    @ ROW,15 SAY 'SUB-TOTAL=>'
    @ ROW,29 SAY 'AUTH PCS/AMT->'
    @ ROW,45 SAY XTOT(1,1) PICTURE '999999'
    @ ROW,55 SAY XTOT(1,2) PICTURE '9999999999.99'
    @ ROW,66 SAY 'RETR PCS/AMT->'
    @ ROW,80 SAY XTOT(1,3) PICTURE '999999'
    @ ROW,90 SAY XTOT(1,4) PICTURE '9999999999.99'
    ROW = ROW + 1
    @ ROW,00 SAY REPLICATE('-',132)
    ROW = ROW + 1
    XBREAK_FLD = &XBREAK
    STORE 0 TO XTOT(1,1),XTOT(1,2),XTOT(1,3),XTOT(1,4)
  ENDIF
ENDIF

IF XSORT_BY <> 'S'
  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW + 1
  @ ROW,10 SAY 'GRAND TOTAL=>'
  @ ROW,29 SAY 'AUTH PCS/AMT->'
  @ ROW,45 SAY XTOT(2,1) PICTURE '999999'
  @ ROW,55 SAY XTOT(2,2) PICTURE '9999999999.99'
  @ ROW,66 SAY 'RETR PCS/AMT->'
  @ ROW,80 SAY XTOT(2,3) PICTURE '999999'
  @ ROW,90 SAY XTOT(2,4) PICTURE '9999999999.99'
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE('=',132)
ENDIF
DO ENDREPORT

*!*************************************************************
*! Name      : lfSetAcc
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 20/11/2000
*! Purpose   : To set or reset the account browse
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetAcc ()
*!*************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
PRIVATE lcOldSelect

*--depend on the value of parameter we select the order of index
lcOldSelect=SELECT()
DO CASE
   CASE OpGrdParm = 'S'
     GOTO TOP IN CUSTOMER
   CASE OpGrdParm = 'R'
      
ENDCASE
SELECT(lcOldSelect)
***************************************************************