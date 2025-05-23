*:***************************************************************************
*: Program file  : ARFPSSL.prg coped form ARSUMSL.prg
*: Program desc. : CUSTOM SUMMARY SALES REPORT
*: Date          : 04/06/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh (SSE)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpRpPrint
*:    Functions  : lfvAccount() , lfwOgWhen() , lfCreatCur() , lfClearRep() ,
*:                 lfvSortBy()  , lfGetReplc() , lfCollTime() , lfEvalVars() ,
*:                 lfItmPos()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO ARSUMSL
*:***************************************************************************
*: This Program is due to  E301189,1...
*:***************************************************************************
*B603174,1 SSE Inreasing Number of Rows in Page From 55 lines to 70 lines
*E301371,1 ARD Adding Currancy fillter to the option grid
******************************************************************************
*C200137,1 TAK 10/10/2000 TAK Customize the standard summary sales report to be custom for FP
*           Add a new sort and filter by Sales Reps, with a page breake
*           Add a department field in report next the store field. 
******************************************************************************



lcStTime = TIME()
*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.

*E301371,1 Initiat variables
llAllCurr  =.F.
lcCurrency = ''
*E301371,1 [End]


IF llOGFltCh

  *-- if the temp. file is used and it's not empty this means that the report
  *-- will erase the data filled inside it and re-create it again for the new
  *-- filter expression 

  lcRpExp = IIF(lcRpExp = '.T.',[Status <> 'V'],;
                              STRTRAN(lcRpExp,'INVHDR.','') + [ AND Status <> 'V'])
  lcRpExp = IIF('ACCOUNT' $ lcRpExp , STRTRAN(lcRpExp,'ACCOUNT','ACCOUNT+INVOICE'),;
                [ACCOUNT+INVOICE = '' AND ] + lcRpExp )
 
  *E301371,1 Defining new variables [Begin]

  *E301371,1 lcCurrency : Vriable that hold the selected currency.
  *E301371,1 lcRpOldExp : That hold the old lcRpExp.
  *E301371,1 llAllCurr  : that indicate if tha all Currency has been selected or not.
  
  lcRpOldExp = lcRpExp
  lcRpExp = STRTRAN(lcRpExp, 'CCURRCODE = "ALL"','CCURRCODE = ""')
  llAllCurr = IIF(lcRpOldExp <> lcRpExp, .T., .F.)
  IF llAllCurr AND lcRpCurr = 'F'
    *-- This Couldn't Be and Doesn't Acceptable.
    WAIT WINDOW "Can not select foreign currency with all currencies."
    lcRpCurr = 'O'
  ENDIF
  *E301371,1 Defining new variables [End  ]

  IF USED(lcInvhTmp) AND RECCOUNT(lcInvhTmp) > 0
    USE IN (lcInvhTmp)
    =lfCreatCur(lcInvhTmp,'laInvStru','cTempKey')
  ENDIF

  *-- collecting the data into Temp. file [Begin.]

  lcReplExpr = lfGetReplc()  && Get Sort by value to update cTempKey with it.
  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values in if...endif block.
  
  SELECT INVHDR
  SET ORDER TO

  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO

    **C200137,1
    WAIT WINDOW 'Invoice/Account : '+Invoice+'/'+Account NOWAIT
    
    *E301371,1 Calculating the exchang AMOUNT [Begin]
    m.ORIGCARCHG  = m.TOTALCHG
    m.TOTALCHG =  IIF(lcRpCurr='F',m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
    *E301371,1 Calculating the exchang AMOUNT[End  ]

    m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
    INSERT INTO (lcInvhTmp) FROM MEMVAR
  ENDSCAN
  
  *-- collecting the data into Temp. file [End.]
ELSE
  
  *E301371,1 Calculating the exchang AMOUNT [Begin]
  IF lcOldCurr <> lcRpCurr
    SELECT (lcInvhTmp)
    REPLACE ALL TOTALCHG WITH IIF(lcRpCurr='F',ORIGCARCHG,gfAmntDisp(ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
  ENDIF  
  *E301371,1 Calculating the exchang AMOUNT[End  ]

ENDIF
WAIT CLEAR

*E301371,1 Calculating the exchang AMOUNT [Begin]
lcOldCurr = lcRpCurr
*E301371,1 Calculating the exchang AMOUNT [End]

SELECT (lcInvhTmp)
*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]

*-- Reindexing file if user change sort by [Begin.]
IF llChSortBy
  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
  lcReplExpr = lfGetReplc()
  REPLACE ALL cTempKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
ENDIF
*-- Reindexing file if user change sort by [End.]

GO TOP

CLEAR TYPEAHEAD	
SET DEVICE TO PRINT

DO lpRepPrn  && Print the report.

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

DO ENDREPORT
SET DEVICE TO SCREEN
*-- end of report code.

*!*************************************************************
*! Name      : lpRepPrn
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Printing loop for the main report
*!*************************************************************
*! Called from : Main Program (ARSUMSL)
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpRepPrn
*!*************************************************************
*
*-- Begin Print loop
PROCEDURE lpRepPrn
ROW     = 99
PAGENO  = 0

R_TITLE = "Summary Sales Information"

IF llMultCurr 
  lnCurrInAr = ASUBSCRIPT(laCurrVal,ASCAN(laCurrVal,laOGFxFlt[lnCurrPos,6]),1)
  R_TITLE    = R_TITLE + ', Currency ' + ALLTRIM(laCurrVal[lnCurrInAr,1])
ENDIF  

R_WIDTH = 'W'
XREPORT = 'ARSUMSL' 

SELECT (lcInvhTmp)
SET RELATION TO IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+ACCOUNT+STORE,'M'+ACCOUNT) INTO CUSTOMER

IF EMPTY(lcBrkFld)
  XBREAK = ''
ELSE
  XBREAK = &lcBrkFld
ENDIF

**C200137,1
XSAVACCT = ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
XSAVDIV  = CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
XSAVREP  = REP1+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
XBREAKTXT= ''

DIMENSION XTOT(3,2)

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8
*Acct# Store    Name                 City           St     Tot. Pcs  Tot. Amount
*12345 12345678 12345678901234567890 12345678901234 123     1234567 1234567.99
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*
*Acct# Store    Dept. Season Name                 City           St     Tot. Pcs  Tot. Amount
*12345 12345678 12345 123456 12345678901234567890 12345678901234 123     9999999 999999999.99    
*Acct# Store    Dept. Season Name                 City           St     Tot. Pcs    Total Amount
*12345 12345678 12345 123456 12345678901234567890 12345678901234 123   9,999,999 9999,999,999.99 
*1 TOT BY ACCT
*2 SUB TOT
*3 GRD TOT

XTOT = 0

DO WHILE .T.

  **C200137,1
  WAIT WINDOW XBREAK NOWAIT

  *B603174,1 SSE Increasing number of Rows [Begin]
  *IF ROW > 55
  IF ROW > 62
    IF PAGENO > 0
      @ ROW,1 SAY "[Continued...]"
    ENDIF  
  *B603174,1 [End]
  
    PAGENO = PAGENO + 1
    DO LFRPT_HDR WITH XREPORT,'',R_WIDTH
    *@ 05,00  SAY ' Acct# Store    Name                 City           St     Tot. Pcs  Tot. Amount'
    *@ 06,0 SAY REPLICATE('=',80)    
    @ 05,00  SAY ' Acct# Store    Dept. Season Name                 City           St      Tot. Pcs     Total Amount'
    @ 06,0 SAY REPLICATE('=',98)    
    ROW = 07
  ENDIF
 
  *IF !EMPTY(XBREAK)
**C200137,1
* IF (lcRpSort='D' OR lcRpTotal='Y')
  IF (lcRpSort $ 'DR' OR lcRpTotal='Y' OR lcRpDpTot='Y' OR lcRpSzTot='Y')
  
    IF XBREAK <> &lcBrkFld 
      IF lcRpSort='D'
       *XBREAKTXT = IIF(SEEK('D'+XBREAK,'CODES'),CODES.CDATA,'')
        XBREAKTXT = gfCodDes(XBREAK,'CDIVISION')
      ENDIF
**C200137,1
      IF lcRpSort='R'
        XBREAKTXT = IIF(SEEK(XBREAK,'SALESREP'),SALESREP.Name,'')
      ENDIF

      @ ROW,00 SAY REPLICATE('-',98)
      ROW = ROW + 1
      @ ROW,00 SAY '*** SUB-TOTAL: '+XBREAK+' '+XBREAKTXT+' ***'
*      @ ROW,59 SAY XTOT(2,1) PICTURE '9999999'
*      @ ROW,67 SAY XTOT(2,2) PICTURE '9999999999.99'  &&MFM 02/03/94 PICT+2
      @ ROW,72 SAY XTOT(2,1) PICTURE '9,999,999'
      @ ROW,83 SAY XTOT(2,2) PICTURE '9999,999,999.99'  &&MFM 02/03/94 PICT+2
      XTOT(2,1) = 0
      XTOT(2,2) = 0
      ROW       = ROW + 1
      @ ROW,00 SAY REPLICATE('-',98)
      ROW       = ROW + 1
      XBREAK = &lcBrkFld

**C200137,1 Bage preak for sales reps.
      IF lcRpSort='R'
        ROW = 01
        PAGENO = PAGENO + 1
        DO LFRPT_HDR WITH XREPORT,'',R_WIDTH
        @ 05,00  SAY ' Acct# Store    Dept. Season Name                 City           St      Tot. Pcs     Total Amount'
        @ 06,0 SAY REPLICATE('=',98)
        ROW = 07
      ENDIF

    ENDIF
  ENDIF

  IF EOF()
    EXIT
  ENDIF

  @ ROW,01 SAY ACCOUNT
  @ ROW,07 SAY IIF(lcRpTotal='Y',STORE,'')

*  @ ROW,16 SAY SUBSTR(CUSTOMER.STNAME,1,20)
*  @ ROW,37 SAY PADR(CUSTOMER.CADDRESS32,14)
*  @ ROW,52 SAY PADR(CUSTOMER.CADDRESS42,6)

  @ ROW,16 SAY IIF(lcRpDpTot='Y',DEPT,'')
  @ ROW,22 SAY IIF(lcRpSzTot='Y',SEASON,'')

  @ ROW,29 SAY SUBSTR(CUSTOMER.STNAME,1,20)
  @ ROW,50 SAY PADR(CUSTOMER.CADDRESS32,14)
  @ ROW,65 SAY PADR(CUSTOMER.CADDRESS42,6)


  IF lcRpSort='A'
    SUM REST WHILE ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') = XSAVACCT ;
    SHIP,TOTALCHG TO XTOT(1,1),XTOT(1,2)
  ELSE
    **C200137,1
    IF lcRpSort='R'
      SUM REST WHILE REP1+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') = XSAVREP ;
      SHIP,TOTALCHG TO XTOT(1,1),XTOT(1,2)

    ELSE
      SUM REST WHILE CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') = XSAVDIV ;
      SHIP,TOTALCHG TO XTOT(1,1),XTOT(1,2)
    ENDIF
  ENDIF

  *** UPDATE THE GRAND TOTALS
  XTOT(2,1) = XTOT(2,1) + XTOT(1,1)
  XTOT(2,2) = XTOT(2,2) + XTOT(1,2)
  XTOT(3,1) = XTOT(3,1) + XTOT(1,1)
  XTOT(3,2) = XTOT(3,2) + XTOT(1,2)

**C200137,1
* IF lcRpSort='R' .AND. IIF(lcRpTotal='Y',REP1+ACCOUNT+STORE,REP1+ACCOUNT) <> XSAVREP
  IF lcRpSort='R' .AND. REP1+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') <> XSAVREP

*    @ ROW,59 SAY XTOT(1,1) PICTURE '9999999'
*    @ ROW,67 SAY XTOT(1,2) PICTURE '9999999999.99'
    @ ROW,72 SAY XTOT(1,1) PICTURE '9,999,999'
    @ ROW,83 SAY XTOT(1,2) PICTURE '9999,999,999.99'

    XTOT(1,1) = 0
    XTOT(1,2) = 0
    XSAVREP   = REP1+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
    ROW       = ROW + 1

  ELSE 
    IF lcRpSort='D' .AND. CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') <> XSAVDIV
      @ ROW,72 SAY XTOT(1,1) PICTURE '9,999,999'
      @ ROW,83 SAY XTOT(1,2) PICTURE '9999,999,999.99'
      XTOT(1,1) = 0
      XTOT(1,2) = 0
      XSAVDIV   = CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
      ROW       = ROW + 1
    ELSE 
      IF ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'') <> XSAVACCT
        @ ROW,72 SAY XTOT(1,1) PICTURE '9,999,999'
        @ ROW,83 SAY XTOT(1,2) PICTURE '9999,999,999.99' &&MFM 02/03/94 PICT+1
        XTOT(1,1) = 0
        XTOT(1,2) = 0
        XSAVACCT  = ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')
        ROW       = ROW + 1
      ENDIF
    ENDIF
  ENDIF  
ENDDO

ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('=',98)
ROW = ROW + 1
@ ROW,00 SAY '*** Grand Total ***'
@ ROW,72 SAY XTOT(3,1) PICTURE '9,999,999'
@ ROW,83 SAY XTOT(3,2) PICTURE '9999,999,999.99' &&MFM 02/03/94 PICT+3
ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('=',98)
ROW = ROW + 1
*-- End of Print loop



*!*************************************************************
*! Name      : lfGetReplc
*! Developer : Sameh (SSE)
*! Date      : 06/04/99
*! Purpose   : Get Replaced expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lcExpr ---> which means (Sort by expression) 
*!*************************************************************
*! Example   : =lfGetReplc()
*!*************************************************************
*
FUNCTION lfGetReplc
PRIVATE lcExpr
*-- if sort by account
IF lcRpSort='A'
  lcExpr   = [ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')+INVOICE]
  lcBrkFld = IIF(lcRpTotal='Y' OR lcRpDpTot='Y' OR lcRpSzTot='Y',[ACCOUNT],'')
ELSE                        && else sort by CDIVISION
**C200137,1
  IF lcRpSort='R'
    lcExpr   = [REP1+ACCOUNT+IIF(lcRpTotal = 'Y' , STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')+INVOICE]
    lcBrkFld = [REP1]

  ELSE
    lcExpr   = [CDIVISION+ACCOUNT+IIF(lcRpTotal = 'Y' , STORE,'')+IIF(lcRpDpTot='Y',DEPT,'')+IIF(lcRpSzTot='Y',SEASON,'')+INVOICE]
    lcBrkFld = [CDIVISION]
  ENDIF
ENDIF  && end if sort by account
RETURN lcExpr
*-- end of lfGetReplc

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/30/1999
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvAccount()
*!*************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal , lnAlsNo , lcCustOrd , lcObjName
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

lnAlsNo = SELECT(0)
  
SELECT CUSTOMER
lcCustOrd = ORDER()
SET ORDER TO TAG CUSTOMER
  
lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field  
*IF The user want to Browse or if the Account he entered is not in the file

IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , '')
  &lcObjName = lcObjVal
ENDIF    && End of IF
  
SET ORDER TO &lcCustOrd
SELECT(lnAlsNo)
*-- end of lfvAcct.

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Sameh (SSE)
*! Date      : 03/30/1999
*! Purpose   : Load Settings before Report starts (When Func.)
*!*************************************************************
*! Called from : option grid of ARSUMSL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************

FUNCTION lfwOgWhen

**C200137,1 Open salesrep file to gut rep name.
=gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')

DIMENSION laInvStru[1,4]

SELECT INVHDR
=AFIELDS(laInvStru)

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 4]
*-- cTempKey :  field used in all sort by cases as the master key 
laInvStru[ALEN(laInvStru,1)  ,1] = 'cTempKey'
laInvStru[ALEN(laInvStru,1)  ,2] = 'C'
laInvStru[ALEN(laInvStru,1)  ,3] = 25
laInvStru[ALEN(laInvStru,1)  ,4] = 0

*E301371,1 Define new field in the file [Begin]
llAllCurr  =.F.
DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 4]
laInvStru[ALEN(laInvStru,1)  ,1] = 'ORIGCARCHG'
laInvStru[ALEN(laInvStru,1)  ,2] = 'N'
laInvStru[ALEN(laInvStru,1)  ,3] = 13
laInvStru[ALEN(laInvStru,1)  ,4] = 2
*E301371,1 [End]

=lfCreatCur(lcInvhTmp,'laInvStru','cTempKey')

*B602590,1 Adjust currency symbol [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('INVHDR.CCURRCODE')
    
  *-- if Default setting.
  IF lnOGSeting = 1
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr

    *E301371,1 STORE THE NAME OF THE CURRENCY [Begin]
    lcCurrency = laOGFxFlt[lnCurrPos,6]
    *E301371,1 STORE THE NAME OF THE CURRENCY [End  ]

    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF  

ENDIF
*B602590,1 Adjust currency symbol [End  ]

*-- end of lfwOgWhen.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Sameh (SSE)
*! Date      : 03/30/1999
*! Purpose   : Create Cursor file from the temp. file
*!*************************************************************
*! Called from : option grid of ARSUMSL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreatCur()
*!*************************************************************
*
FUNCTION lfCreatCur
PARAMETERS lcCurName,lcCurStrut,lcTagExpr
CREATE CURSOR (lcCurName) FROM ARRAY (lcCurStrut)
INDEX ON &lcTagExpr TAG (lcCurName) OF (gcWorkDir+lcCurName+'.CDX') 
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Sameh (SSE)
*! Date      : 07/20/98
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Sameh (SSE)
*! Date      : 04/05/99
*! Purpose   : change index flag to reindex temp cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.
llOGFltCh  = .T.
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Sameh (SSE)
*! Date      : 03/30/1999
*! Purpose   : Close OG Function.
*!*************************************************************
*! Called from : OG < Close > or < Reset > Buttons.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
IF USED(lcInvhTmp)
  USE IN (lcInvhTmp)
ENDIF
*--end of lfClearRep.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : Sameh (SSE)
*! Date      : 07/04/1999
*! Purpose   : Fill Default values used in both OG and Report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfOpenFile
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
*!
FUNCTION lfEvalVars
*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  PRIVATE llOpenCurr
  llOpenCurr = .F.
  
  DIMENSION laCurrVal[1,1]

  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal

  *E301371,1 Add 'ALL Currency' to the array [Begin]
  DIMENSION laCurrVal[ALEN(laCurrVal,1)+1,1]
  =AINS(laCurrVal,1)
  laCurrVal[1,1] = 'ALL'
  *E301371,1 Add 'ALL Currency' to the array [End  ]
  
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR

  *E301371,1 Add 'ALL Currency' to the array [Begin]
  laCurrDesc[1,1] = 'ALL - ALL CURRENCY'
  *E301371,1 Add 'ALL Currency' to the array [End  ]
  
  IF llOpenCurr
    USE IN SYCCURR
  ENDIF
  
ENDIF
*-- if multi currency evaluate currency arrays [End  ]
*-- end of lfEvalVars.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Ahmed M. Reda (ARD)
*! Date      : 02/09/2000
*! Purpose   : Calculate and change the forign curruncy to base.
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301371,1
*!
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
IF llRpProced AND llAllCurr AND lcRpCurr = 'F'
  *-- This Couldn't Be and Doesn't Acceptable.
  WAIT WINDOW "Can not select foreign currency with all currencies."
  *lcRpCurr = 'O'
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
ENDIF
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfvChkCurr
*! Developer : Ahmed M. Reda (ARD)
*! Date      : 02/09/2000
*! Purpose   : return the currency.
*!*************************************************************
*! Example   : =lfvChkCurr()
*!*************************************************************
*!E301371,1
*!
FUNCTION lfvChkCurr
lcCurrency = laOgFxFlt[lnCurrPos,6]
llAllCurr = IIF(lcCurrency='ALL',.T.,.F.)
IF llAllCurr AND lcRpCurr = 'F'
  *-- This Couldn't Be and Doesn't Acceptable.
  WAIT WINDOW "Can not select foreign currency with all currencies."
  *lcRpCurr = 'O'
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
ENDIF
*-- end of lfvChkCurr





******************************************************************************
* PROG: LFRPT_HDR
* DESC: PRINTS THE HEADER ON THE REPORTS (WIDE PAPER,NARROW PAPER)
* DATE: 05/19/90
* PARA: 1 PROGRAM
*       2 TITLE    ADDITIONAL TITLE (XRPTNAME)
*       3 TYPE     'N' -> NARROW   'W'-> WIDE    'XW'-> XTRA WIDE
* NOTE: THE ACTUAL REPORT CAN START ON LINE 5
*       R_TITLE => THE REPORT TITLE (STANDARD)
*       PAGENO=PAGE NUMBER
******************************************************************************
PROCEDURE LFRPT_HDR
PARAMETER XPROG,XRPTNAME,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
R_TITLE  = TRIM(R_TITLE)
X1 = ((98 - (LEN(TRIM(QCOMPANY))))/2)
X2 = ((98 - (LEN( R_TITLE  )))/2)
X3 = ((98 - (LEN( XRPTNAME )))/2)

*@ 01,000 SAY XPROG
@ 01,X1 SAY QCOMPANY
@ 01,87 SAY DATE()
@ 01,96 SAY '~'   
@ 02,00 SAY TIME()
@ 02,X2 SAY R_TITLE
@ 02,87 SAY 'PAGE#'
@ 02,93 SAY STR(PAGENO,4)
@ 03,X3 SAY XRPTNAME
@ 04,00 SAY REPLICATE('*',98)
RETURN
