*:***************************************************************************
*: Program file  : ARSUMSL
*: Program desc. : SUMMARY SALES REPORT
*: Date          : 06/06/2006
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar  037700
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
*: Modifiction:
*!* B609356,1 SMA 07/26/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[T20111130.0016]
*!* B610742,1 MMT 06/10/2014 Sales Summary report Numeric Overflow error at GMA[T20131219.0020]
*!* B611629,1 HMS 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ]
*:***************************************************************************
*: Example : DO ARSUMSL
*:***************************************************************************

#INCLUDE R:\Aria4xp\reports\ar\arsumsl.H
lcStTime = TIME()
*-- Logical (GLOBAL) Flag used to determine whether the report is run for
*-- the first time or not , if first time llOGFltCh = .T. and report will
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.
llAllCurr  =.F.
lcCurrency = ''
IF loOgScroll.llOGFltCh && OG Filters changed
  *-- if the temp. file is used and it's not empty this means that the report
  *-- will erase the data filled inside it and re-create it again for the new
  *-- filter expression
  IF USED(lcInvhTmp)
    USE IN (lcInvhTmp)
  ENDIF
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Collecting_Data  NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_Data,oAriaApplication.GetHeaderText("Lang_Collecting_Data",AHEADERFILE))  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lfCreateTemp()
  lfCollectDate()

  SELECT (lcInvhTmp)
  IF RECCOUNT() > 0
    lcFilName = lcInvhTmp
   * COPY TO oAriaApplication.WorkDir +  lcFilName + ".DBF"
  ENDIF
ELSE
  IF FILE(oAriaApplication.WorkDir +  lcFilName + ".DBF")
    USE oAriaApplication.WorkDir +  lcFilName + ".DBF" IN 0
  ENDIF
  IF lcOldCurr <> lcRpCurr
    SELECT (lcInvhTmp)
    REPLACE ALL TOTALCHG WITH IIF(lcRpCurr='F',ORIGCARCHG,gfAmntDisp(ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
  ENDIF
ENDIF

lcOldCurr = lcRpCurr

SELECT (lcInvhTmp)
*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Selected   + ALLTRIM(STR(RECCOUNT())) + Lang_Records_in + ALLTRIM(STR(lnInterval,6,2)) + Lang_Seconds NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selected,oAriaApplication.GetHeaderText("Lang_Selected",AHEADERFILE))   + ALLTRIM(STR(RECCOUNT())) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Records_in,oAriaApplication.GetHeaderText("Lang_Records_in",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Seconds,oAriaApplication.GetHeaderText("Lang_Seconds",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
loOgScroll.ExportFromFile = lcExp2Excl
*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]

*-- Asking if no records (Display message) otherwise print report [End.]
loOgScroll.lcOGLastForm = 'ARSUMSL'
loOgScroll.cCRorientation = 'P'

DIMENSION loOgScroll.laCRParams[7,2]
loOgScroll.laCRParams[1,1] =  'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = Lang_Summary_Sales_Information
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Summary_Sales_Information,oAriaApplication.GetHeaderText("Lang_Summary_Sales_Information",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] = 'lcSortBy'
loOgScroll.laCRParams[2,2] = lcRpSort

loOgScroll.laCRParams[3,1] = 'lcRpTotal'
loOgScroll.laCRParams[3,2] = IIF(lcRpTotal = 'Y',1,0)

loOgScroll.laCRParams[4,1] = 'llMultCurr'
loOgScroll.laCRParams[4,2] = IIF(llMultCurr,1,0)

PRIVATE lnTrnDtPos
lnTrnDtPos = lfItmPos('INVHDR.INVDATE')

loOGScroll.laCRParams[5,1] ="Start"
loOGScroll.laCRParams[6,1] ="End"

loOGScroll.laCRParams[5,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnTrnDtPos,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnTrnDtPos,6],1,10))
loOGScroll.laCRParams[6,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnTrnDtPos,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnTrnDtPos,6],12,21))

loOgScroll.laCRParams[7,1] = 'lcRpCurr'
loOgScroll.laCRParams[7,2] = lcRpCurr

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcFilName + ".DBF"
SELECT (lcInvhTmp)
USE

=gfDispRe()
RETURN

*!*  *-- Reindexing file if user change sort by [Begin.]
*!*  IF llChSortBy
*!*    llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
*!*    lcReplExpr = lfGetReplc()
*!*    REPLACE ALL cTempKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
*!*  ENDIF
*!*  *-- Reindexing file if user change sort by [End.]

*!*  GO TOP

*!*  CLEAR TYPEAHEAD	
*!*  SET DEVICE TO PRINT

*!*  *B606563,1 Get the transaction date position. [Begin]
*!*  *-- Variable to get the transaction date position in option grid.
*!*  PRIVATE lnTrnDtPos
*!*  lnTrnDtPos = lfItmPos('INVHDR.INVDATE')
*!*  *B606563,1 Get the transaction date position. [End]

*!*  DO lpRepPrn  && Print the report.

*!*  *-- Calculate spent time in collecting data.
*!*  lcEdTime = TIME()  && Time in which we finish collect data.
*!*  lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*!*  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*!*  DO ENDREPORT
*!*  SET DEVICE TO SCREEN
*!*  *-- end of report code.

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

R_WIDTH = 'N'
XREPORT = 'ARSUMSL'

SELECT (lcInvhTmp)
SET RELATION TO IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+ACCOUNT+STORE,'M'+ACCOUNT) INTO CUSTOMER

IF EMPTY(lcBrkFld)
  XBREAK = ''
ELSE
  XBREAK = &lcBrkFld
ENDIF

XSAVACCT = ACCOUNT+IIF(lcRpTotal='Y',STORE,'')
XSAVDIV  = CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')
XBREAKTXT= ''

DIMENSION XTOT(3,2)

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8
*ACCT# STORE    NAME                 CITY            ST     TOT. PCS TOT. AMOUNT
*12345 12345678 12345678901234567890 123456789012345 123     1234567 1234567.99
*1 TOT BY ACCT
*2 SUB TOT
*3 GRD TOT

XTOT = 0

DO WHILE .T.

  *B603174,1 SSE Increasing number of Rows [Begin]
  *IF ROW > 55
  IF ROW > 62
    IF PAGENO > 0
      @ ROW,1 SAY "[Continued...]"
    ENDIF
  *B603174,1 [End]

    PAGENO = PAGENO + 1

    *B606563,1 Print the period. [Begin]
    *DO RPT_HDR WITH XREPORT,'',R_WIDTH
    DO RPT_HDR WITH XREPORT,'Period : ' + STRTRAN(laOGFxFlt[lnTrnDtPos,6],[|],[ - ]),R_WIDTH
    *B606563,1 Print the period. [End]

    @ 05,00  SAY ' Acct# Store    Name                 City           St     Tot. Pcs  Tot. Amount'
    @ 06,0 SAY REPLICATE('=',80)
    ROW = 07
  ENDIF

  *IF !EMPTY(XBREAK)
  IF (lcRpSort='D' OR lcRpTotal='Y')

    IF XBREAK <> &lcBrkFld
      IF lcRpSort='D'
       *XBREAKTXT = IIF(SEEK('D'+XBREAK,'CODES'),CODES.CDATA,'')
        XBREAKTXT = gfCodDes(XBREAK,'CDIVISION')
      ENDIF
      @ ROW,00 SAY REPLICATE('-',80)
      ROW = ROW + 1
      @ ROW,00 SAY '*** SUB-TOTAL: '+XBREAK+' '+XBREAKTXT+' ***'
      @ ROW,59 SAY XTOT(2,1) PICTURE '9999999'
      @ ROW,67 SAY XTOT(2,2) PICTURE '9999999999.99'  &&MFM 02/03/94 PICT+2
      XTOT(2,1) = 0
      XTOT(2,2) = 0
      ROW       = ROW + 1
      @ ROW,00 SAY REPLICATE('-',80)
      ROW       = ROW + 1
      XBREAK = &lcBrkFld
    ENDIF

  ENDIF

  IF EOF()
    EXIT
  ENDIF

  @ ROW,01 SAY ACCOUNT
  @ ROW,07 SAY IIF(lcRpTotal='Y',STORE,'')
  @ ROW,16 SAY SUBSTR(CUSTOMER.STNAME,1,20)
 *B606819,1 MAN 01/12/2003 Fix printing the billing state and city instead of the
 *B606819,1                shipping state and city (Start)
  *@ ROW,37 SAY PADR(CUSTOMER.CADDRESS32,14)
  *@ ROW,52 SAY PADR(CUSTOMER.CADDRESS42,6)
  @ ROW,37 SAY PADR(CUSTOMER.CADDRESS3,14)
  @ ROW,52 SAY PADR(CUSTOMER.CADDRESS4,6)
 *B606819,1 (End)

  IF lcRpSort='A'
    SUM REST WHILE ACCOUNT+IIF(lcRpTotal='Y',STORE,'') = XSAVACCT ;
    SHIP,TOTALCHG TO XTOT(1,1),XTOT(1,2)
  ELSE
    SUM REST WHILE CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'') = XSAVDIV ;
    SHIP,TOTALCHG TO XTOT(1,1),XTOT(1,2)
  ENDIF

  *** UPDATE THE GRAND TOTALS
  XTOT(2,1) = XTOT(2,1) + XTOT(1,1)
  XTOT(2,2) = XTOT(2,2) + XTOT(1,2)
  XTOT(3,1) = XTOT(3,1) + XTOT(1,1)
  XTOT(3,2) = XTOT(3,2) + XTOT(1,2)

  IF lcRpSort='D' .AND. IIF(lcRpTotal='Y',CDIVISION+ACCOUNT+STORE,CDIVISION+ACCOUNT) <> XSAVDIV
    @ ROW,59 SAY XTOT(1,1) PICTURE '9999999'
    @ ROW,67 SAY XTOT(1,2) PICTURE '9999999999.99'
    XTOT(1,1) = 0
    XTOT(1,2) = 0
    XSAVDIV   = CDIVISION+ACCOUNT+IIF(lcRpTotal='Y',STORE,'')
    ROW       = ROW + 1
  ELSE
    IF ACCOUNT+IIF(lcRpTotal='Y',STORE,'') <> XSAVACCT
      @ ROW,59 SAY XTOT(1,1) PICTURE '9999999'
      @ ROW,67 SAY XTOT(1,2) PICTURE '9999999999.99' &&MFM 02/03/94 PICT+1
      XTOT(1,1) = 0
      XTOT(1,2) = 0
      XSAVACCT  = ACCOUNT+IIF(lcRpTotal='Y',STORE,'')
      ROW       = ROW + 1
    ENDIF
  ENDIF
ENDDO

ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('=',80)
ROW = ROW + 1
@ ROW,00 SAY '*** Grand Total ***'
@ ROW,59 SAY XTOT(3,1) PICTURE '9999999'
@ ROW,67 SAY XTOT(3,2) PICTURE '9999999999.99' &&MFM 02/03/94 PICT+3
ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('=',80)
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
  lcExpr   = [ACCOUNT+IIF(lcRpTotal='Y',STORE,'')+INVOICE]
  lcBrkFld = IIF(lcRpTotal='Y',[ACCOUNT],'')
ELSE                        && else sort by CDIVISION
  lcExpr   = [CDIVISION+ACCOUNT+IIF(lcRpTotal = 'Y' , STORE,'')+INVOICE]
  lcBrkFld = [CDIVISION]
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

loDBFCustomer = CreateObject("RemoteTable","Customer","Customer",'Customer',SET("DATASESSION"))&&,"",.T.)
loDBFInvhdr  = CreateObject("RemoteTable","INVHDR","INVHDR",'INVHDR',SET("DATASESSION"))&&,"",.T.)
loDBFCodes = CreateObject("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
*=lfCreatCur(lcInvhTmp,'laInvStru','cTempKey')

*B602590,1 Adjust currency symbol [Begin]
*!*	IF llMultCurr
*!*	  lnCurrPos  = lfItmPos('INVHDR.CCURRCODE')
*!*	
*!*	  *-- if Default setting.
*!*	  IF lnOGSeting = 1
*!*	    laOGFxFlt[lnCurrPos,6] = gcBaseCurr

*!*	    *E301371,1 STORE THE NAME OF THE CURRENCY [Begin]
*!*	    lcCurrency = laOGFxFlt[lnCurrPos,6]
*!*	    *E301371,1 STORE THE NAME OF THE CURRENCY [End  ]

*!*	    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
*!*	  ENDIF

*!*	ENDIF
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
*B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON &lcTagExpr TAG (lcCurName) OF (gcWorkDir+lcCurName+'.CDX')
INDEX ON &lcTagExpr TAG (lcCurName)
*B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
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
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal

  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR

ENDIF

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Ahmed M. Reda (ARD)
*! Date      : 02/09/2000
*! Purpose   : Calculate and change the forign curruncy to base.
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
IF llRpProced AND llAllCurr AND lcRpCurr = 'F'
  *-- This Couldn't Be and Doesn't Acceptable.
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Can_not_select_foreign_currency_with_all_currencies
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Can_not_select_foreign_currency_with_all_currencies,oAriaApplication.GetHeaderText("Lang_Can_not_select_foreign_currency_with_all_currencies",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
*!
FUNCTION lfvChkCurr
lnCurrPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.CCURRCODE'),1)
lcCurrency = loogscroll.laOgFxFlt[lnCurrPos,6]
*llAllCurr = IIF(lcCurrency='ALL',.T.,.F.) &&llAllCurr AND
IF lcRpCurr = 'F'
  *-- This Couldn't Be and Doesn't Acceptable.
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Can_not_select_foreign_currency_with_all_currencies
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Can_not_select_foreign_currency_with_all_currencies,oAriaApplication.GetHeaderText("Lang_Can_not_select_foreign_currency_with_all_currencies",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *lcRpCurr = 'O'
  laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
ENDIF
*-- end of lfvChkCurr
*!*************************************************************
*! Name      : lfCollectDate
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/06/2006
*! Purpose   : collect data
*!*************************************************************
FUNCTION lfCollectDate

llUseAccount = .F.
lcAccFile  = ""
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.ACCOUNT'),1)
IF lnPosition > 0
  lcAccFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseAccount  = IIF(!EMPTY(lcAccFile) .AND. USED(lcAccFile) .AND. RECCOUNT(lcAccFile)>0,.T.,.F.)
ENDIF

*--Currency filter
IF llMultCurr
  lcCurrFile = ''
  llUseCurr = .F.
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.CCURRCODE'),1)
  IF lnPosition > 0 &&CCurcode
    lcCurrFile  = loOGScroll.gfTempName()	
    lcCurr   = LOOGSCROLL.laOGFxFlt[lnPosition,6]
    llUseCurr = IIF(LEN(lcCurr)>0,.T.,.F.) AND lfConvertToCursor(lcCurr,'CCurcode',lcCurrFile)
  ENDIF
ENDIF

lcSeaFile = ''
llUseSea = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.SEASON'),1)
IF lnPosition > 0 &&CCurcode
  lcSeaFile = loOGScroll.gfTempName()	
  lcSeasons   = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseSea = IIF(LEN(lcSeasons)>0,.T.,.F.) AND lfConvertToCursor(lcSeasons,'CSeason',lcSeaFile)
ENDIF

lcDivFile = ''
llUseDiv = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'INVHDR.CDIVISION'),1)
IF lnPosition > 0 &&CCurcode
  lcDivFile = loOGScroll.gfTempName()	
  lcDivisons = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseDiv = IIF(LEN(lcDivisons)>0,.T.,.F.) AND lfConvertToCursor(lcDivisons,'CDivison',lcDivFile)
ENDIF


lldate = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'INVHDR.INVDATE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
  Ldate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,10))
  Hdate = CTOD(SUBSTR(laOGFxFlt[lnPosition,6],12,21))
  lldate = .T.
ENDIF

lcReplExpr = lfGetReplc()  && Get Sort by value to update cTempKey with it.
IF llUseAccount
  SELECT(lcAccFile)
  SCAN
    IF loDBFInvhdr.Seek(&lcAccFile..Account,"INVHDRA")
      SELECT Invhdr
      SCAN REST WHILE ACCOUNT+INVOICE = &lcAccFile..Account FOR Status <> 'V' AND IIF(llUseDiv,SEEK(invhdr.cdivision,lcDivFile),.T.) AND ;
               IIF(llUseSea,SEEK(invhdr.season,lcSeaFile),.T.) AND ;
               IIF(llMultCurr AND llUseCurr,SEEK(invhdr.ccurrcode,lcCurrFile),.T.) AND ;
               IIF(lldate ,BETWEEN(INVHDR.INVDATE,Ldate,Hdate),.T.)
        SCATTER MEMVAR MEMO
        IF lcRpSort='D'
          m.Div_desc = lfGetDesc(invhdr.cdivision)
        ENDIF
        *m.Div_desc = gfCodDes(invhdr.cdivision,'CDIVISION')
        m.ORIGCARCHG  = m.TOTALCHG
        m.TOTALCHG =  IIF((llMultCurr AND lcRpCurr='F') OR !llMultCurr ,m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
        *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
      **B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
      m.C_STANME = ''
	  m.C_ADD3   = ''
      m.C_ADD4   = ''
      **B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]
        IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+Invhdr.ACCOUNT+Invhdr.STORE,'M'+Invhdr.ACCOUNT))
          m.C_STANME = customer.stname
          m.C_ADD3   = customer.caddress3
          m.C_ADD4   = customer.caddress4
        ENDIF
        INSERT INTO (lcInvhTmp) FROM MEMVAR
        *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
        IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
          INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
          						(m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
        ELSE
          REPLACE SHIP     WITH Ship +m.Ship,;
                  TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
        ENDIF
		*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
  IF llUseDiv
    SELECT(lcDivFile)
    SCAN
      *IF loDBFInvhdr.llNative
        *.SQlrun("Select * from invhdr where ACCOUNT+INVOICE = '' and invhdr.cdivision = '"+&lcDivFile..CDivison +"' AND Status <> 'V' ","INVHD_X",.T.)
        loDBFInvhdr.Seek('')
        SELECT INVHDR
        SCAN FOR invhdr.cdivision = &lcDivFile..CDivison  AND IIF(llUseSea,SEEK(invhdr.season,lcSeaFile),.T.) AND ;
                 IIF(llMultCurr AND llUseCurr,SEEK(invhdr.ccurrcode,lcCurrFile),.T.) AND ;
                 IIF(lldate ,BETWEEN(INVHDR.INVDATE,Ldate,Hdate),.T.) AND  Invhdr.Status <> 'V'
          SCATTER MEMVAR MEMO
          IF lcRpSort='D'
            m.Div_desc = lfGetDesc(invhdr.cdivision)
          ENDIF
*          m.Div_desc = gfCodDes(INVHDR.cdivision,'CDIVISION')
          m.ORIGCARCHG  = m.TOTALCHG
          m.TOTALCHG =  IIF((llMultCurr AND lcRpCurr='F') OR !llMultCurr,m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
          *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
		**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
		m.C_STANME = ''
		m.C_ADD3   = ''
		m.C_ADD4   = ''
		**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]
          IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+INVHDR.ACCOUNT+INVHDR.STORE,'M'+INVHDR.ACCOUNT))
            m.C_STANME = customer.stname
            m.C_ADD3   = customer.caddress3
            m.C_ADD4   = customer.caddress4
          ENDIF
          INSERT INTO (lcInvhTmp) FROM MEMVAR
	        *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
	        IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
	          INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
	          						(m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
	        ELSE
	          REPLACE SHIP     WITH Ship +m.Ship,;
	                  TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
	        ENDIF
		    *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]

        ENDSCAN
      *ENDIF
    ENDSCAN
  ELSE
    IF llUseSea
      SELECT (lcSeaFile)
      SCAN
        *IF loDBFInvhdr.SQlrun("Select * from invhdr where ACCOUNT+INVOICE = '' and invhdr.season = '"+&lcSeaFile..CSeason +"' AND Status <> 'V' ","INVHD_X")
          SELECT INVHDR
          SCAN FOR invhdr.season = &lcSeaFile..CSeason  AND  IIF(llMultCurr AND llUseCurr,SEEK(invhdr.ccurrcode,lcCurrFile),.T.) AND ;
                   IIF(lldate ,BETWEEN(INVHDR.INVDATE,Ldate,Hdate),.T.) AND  Invhdr.Status <> 'V'
            SCATTER MEMVAR MEMO
            IF lcRpSort='D'
              m.Div_desc = lfGetDesc(invhdr.cdivision)
            ENDIF
*            m.Div_desc = gfCodDes(INVHDR.cdivision,'CDIVISION')
            m.ORIGCARCHG  = m.TOTALCHG
            m.TOTALCHG =  IIF((llMultCurr AND lcRpCurr='F') OR !llMultCurr,m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
            *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
			**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
				m.C_STANME = ''
				m.C_ADD3   = ''
				m.C_ADD4   = ''
			**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]
            IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+INVHDR.ACCOUNT+INVHDR.STORE,'M'+INVHDR.ACCOUNT))
              m.C_STANME = customer.stname
              m.C_ADD3   = customer.caddress3
              m.C_ADD4   = customer.caddress4
            ENDIF
            INSERT INTO (lcInvhTmp) FROM MEMVAR
            *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
            IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
              INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
                          (m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
            ELSE
              REPLACE SHIP     WITH Ship +m.Ship,;
                      TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
            ENDIF
            *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
          ENDSCAN
        *ENDIF
      ENDSCAN
    ELSE
      IF lldate
      *AND loDBFInvhdr.SQlrun("Select * from invhdr where ACCOUNT+INVOICE = '' and BETWEEN(INVHDR.INVDATE,CTOD('"+DTOC(Ldate)+"'),CTOD('"+DTOC(Hdate)+"')) AND Status <> 'V' ","INVHD_X")
        SELECT INVHDR
        SCAN FOR IIF(llMultCurr AND llUseCurr,SEEK(invhdr.ccurrcode,lcCurrFile),.T.) AND ;
                 BETWEEN(INVHDR.INVDATE,Ldate,Hdate) AND  Invhdr.Status <> 'V'
            SCATTER MEMVAR MEMO
            IF lcRpSort='D'
              m.Div_desc = lfGetDesc(invhdr.cdivision)
            ENDIF
*            m.Div_desc = gfCodDes(INVHDR.cdivision,'CDIVISION')
            m.ORIGCARCHG  = m.TOTALCHG
            m.TOTALCHG =  IIF((llMultCurr AND lcRpCurr='F') OR !llMultCurr,m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
            *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
			**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
			m.C_STANME = ''
			m.C_ADD3   = ''
			m.C_ADD4   = ''
			**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]
            IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+INVHDR.ACCOUNT+INVHDR.STORE,'M'+INVHDR.ACCOUNT))
              m.C_STANME = customer.stname
              m.C_ADD3   = customer.caddress3
              m.C_ADD4   = customer.caddress4
            ENDIF
            INSERT INTO (lcInvhTmp) FROM MEMVAR
            *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
            IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
              INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
                          (m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
            ELSE
              REPLACE SHIP     WITH Ship +m.Ship,;
                      TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
            ENDIF
            *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
        ENDSCAN
      ELSE
        IF llMultCurr AND llUseCurr
          SELECT(lcCurrFile)
          SCAN
            *IF loDBFInvhdr.SQlrun("Select * from invhdr where ACCOUNT+INVOICE = '' and invhdr.ccurrcode = '"+&lcCurrFile..CCurcode+"' AND Status <> 'V' ","INVHD_X")
              SELECT INVHDR
              SCAN  FOR invhdr.ccurrcode = &lcCurrFile..CCurcode AND Invhdr.Status <> 'V'
                SCATTER MEMVAR MEMO
                m.ORIGCARCHG  = m.TOTALCHG
                IF lcRpSort='D'
                  m.Div_desc = lfGetDesc(invhdr.cdivision)
                ENDIF
                m.TOTALCHG =  IIF(llMultCurr AND lcRpCurr='F',m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
                *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
				**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
				m.C_STANME = ''
				m.C_ADD3   = ''
				m.C_ADD4   = ''
				**B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]

                IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+INVHDR.ACCOUNT+INVHDR.STORE,'M'+INVHDR.ACCOUNT))
                  m.C_STANME = customer.stname
                  m.C_ADD3   = customer.caddress3
                  m.C_ADD4   = customer.caddress4
                ENDIF
                INSERT INTO (lcInvhTmp) FROM MEMVAR
                *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
                IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
                  INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
                              (m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
                ELSE
                  REPLACE SHIP     WITH Ship +m.Ship,;
                          TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
                ENDIF
                *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
              ENDSCAN
            *ENDIF
          ENDSCAN
        ELSE
          *IF loDBFInvhdr.SQlrun("Select * from invhdr where ACCOUNT+INVOICE = '' and Status <> 'V' ","INVHD_X")
            SELECT INVHDR
            SCAN FOR Invhdr.Status <> 'V'
              SCATTER MEMVAR MEMO
              IF lcRpSort='D'
                m.Div_desc = lfGetDesc(invhdr.cdivision)
              ENDIF
*              m.Div_desc = gfCodDes(INVHDR.cdivision,'CDIVISION')
              m.ORIGCARCHG  = m.TOTALCHG
              m.TOTALCHG =  IIF((llMultCurr AND lcRpCurr='F') OR !llMultCurr,m.TOTALCHG,gfAmntDisp(m.ORIGCARCHG,lcRpCurr,ldRpExDate,lcRpTmpNam))
              *m.cTempKey = EVALUATE(lcReplExpr)  && Replace key Field.
				*B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][Begin]
				m.C_STANME = ''
				m.C_ADD3   = ''
				m.C_ADD4   = ''
				*B611629 ,1 HMS, 07/30/2018 ,Aria 5 - error message when trying to open sales r... (La Cera )[T20180725.0015 ][END]
              IF loDBFCustomer.Seek(IIF((!EMPTY(STORE) .AND. lcRpTotal='Y'),'S'+INVHDR.ACCOUNT+INVHDR.STORE,'M'+INVHDR.ACCOUNT))
                m.C_STANME = customer.stname
                m.C_ADD3   = customer.caddress3
                m.C_ADD4   = customer.caddress4
              ENDIF
              INSERT INTO (lcInvhTmp) FROM MEMVAR
              *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
              IF !SEEK(IIF(lcRpSort='D',m.CDIVISION,'')+m.ACCOUNT+IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''),lcExp2Excl)
                INSERT INTO (lcExp2Excl) (ACCOUNT,STORE,NAME,CCITY,CSTATE,DIV_DESC,CDIVISION,Ship,TotalCHG) VALUES ;
                            (m.ACCOUNT,IIF((!EMPTY(m.STORE) .AND. lcRpTotal='Y'),m.STORE,''), m.C_STANME,m.C_ADD3,m.C_ADD4,IIF(lcRpSort='D',m.Div_desc,''),m.CDIVISION,m.Ship,m.TotalCHG)
              ELSE
                REPLACE SHIP     WITH Ship +m.Ship,;
                        TotalCHG WITH TotalCHG +m.TotalCHG IN (lcExp2Excl)
              ENDIF
              *!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
            ENDSCAN
          *ENDIF
        ENDIF
      ENDIF
  	ENDIF
  ENDIF
ENDIF

*
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 05/31/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile

lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE
  CASE  ALLTRIM(lcFieldName) = 'CSeason'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE  ALLTRIM(lcFieldName) = 'CDivison'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0

  CASE   ALLTRIM(lcFieldName) = 'CCurcode'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 3
    laTempacstru[1,4]= 0
ENDCASE


= gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfCreateTemp
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/06/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfCreateTemp
DIMENSION laInvStru[1,18]

SELECT INVHDR
lnOriginal =AFIELDS(laInvStru)

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
*-- cTempKey :  field used in all sort by cases as the master key
laInvStru[ALEN(laInvStru,1)  ,1] = 'cTempKey'
laInvStru[ALEN(laInvStru,1)  ,2] = 'C'
laInvStru[ALEN(laInvStru,1)  ,3] = 25
laInvStru[ALEN(laInvStru,1)  ,4] = 0


llAllCurr  =.F.
DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
laInvStru[ALEN(laInvStru,1)  ,1] = 'ORIGCARCHG'
laInvStru[ALEN(laInvStru,1)  ,2] = 'N'
laInvStru[ALEN(laInvStru,1)  ,3] = 13
laInvStru[ALEN(laInvStru,1)  ,4] = 2

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
laInvStru[ALEN(laInvStru,1)  ,1] = 'C_STANME'
laInvStru[ALEN(laInvStru,1)  ,2] = 'c'
laInvStru[ALEN(laInvStru,1)  ,3] = 20
laInvStru[ALEN(laInvStru,1)  ,4] = 0

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
laInvStru[ALEN(laInvStru,1)  ,1] = 'C_ADD3'
laInvStru[ALEN(laInvStru,1)  ,2] = 'C'
laInvStru[ALEN(laInvStru,1)  ,3] = 14
laInvStru[ALEN(laInvStru,1)  ,4] = 0

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
laInvStru[ALEN(laInvStru,1)  ,1] = 'C_ADD4'
laInvStru[ALEN(laInvStru,1)  ,2] = 'C'
laInvStru[ALEN(laInvStru,1)  ,3] = 6
laInvStru[ALEN(laInvStru,1)  ,4] = 0

DIMENSION laInvStru[ALEN(laInvStru,1) + 1, 18]
laInvStru[ALEN(laInvStru,1)  ,1] = 'Div_desc'
laInvStru[ALEN(laInvStru,1)  ,2] = 'C'
laInvStru[ALEN(laInvStru,1)  ,3] = 30
laInvStru[ALEN(laInvStru,1)  ,4] = 0



FOR lnLoop = 1 TO  6
  STORE ' ' TO  laInvStru[lnOriginal +lnLoop,7],laInvStru[lnOriginal+lnLoop,8],;
                laInvStru[lnOriginal+lnLoop,9],laInvStru[lnOriginal+lnLoop,10],;
                laInvStru[lnOriginal+lnLoop,11],laInvStru[lnOriginal+lnLoop,12],;
                laInvStru[lnOriginal+lnLoop,13],laInvStru[lnOriginal+lnLoop,14],;
                laInvStru[lnOriginal+lnLoop,15],laInvStru[lnOriginal+lnLoop,16]
  STORE 0 TO    laInvStru[lnOriginal+lnLoop,17] ,laInvStru[lnOriginal+lnLoop,18]
ENDFOR

=gfCrtTmp(lcInvhTmp,@laInvStru,"cTempKey",lcInvhTmp,.F.)
*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[Start]
DIMENSION laExpFileStru[9,4]
laExpFileStru[1,1] = 'ACCOUNT'
laExpFileStru[1,2] = 'C'
laExpFileStru[1,3] = 5
laExpFileStru[1,4] = 0

laExpFileStru[2,1] = 'STORE'
laExpFileStru[2,2] = 'C'
laExpFileStru[2,3] = 8
laExpFileStru[2,4] = 0

laExpFileStru[3,1] = 'CDIVISION'
laExpFileStru[3,2] = 'C'
laExpFileStru[3,3] = 6
laExpFileStru[3,4] = 0

laExpFileStru[4,1] = 'DIV_DESC'
laExpFileStru[4,2] = 'C'
laExpFileStru[4,3] = 30
laExpFileStru[4,4] = 0

laExpFileStru[5,1] = 'NAME'
laExpFileStru[5,2] = 'C'
laExpFileStru[5,3] = 30
laExpFileStru[5,4] = 0

laExpFileStru[6,1] = 'CCITY'
laExpFileStru[6,2] = 'C'
laExpFileStru[6,3] = 30
laExpFileStru[6,4] = 0

laExpFileStru[7,1] = 'CSTATE'
laExpFileStru[7,2] = 'C'
laExpFileStru[7,3] = 30
laExpFileStru[7,4] = 0

laExpFileStru[8,1] = 'SHIP'
laExpFileStru[8,2] = 'N'
*!* B610742,1 MMT 06/10/2014 Sales Summary report Numeric Overflow error at GMA[T20131219.0020][Start]
*laExpFileStru[8,3] = 7
laExpFileStru[8,3] = 12
*!* B610742,1 MMT 06/10/2014 Sales Summary report Numeric Overflow error at GMA[T20131219.0020][End]
laExpFileStru[8,4] = 0

laExpFileStru[9,1] = 'TOTALCHG'
laExpFileStru[9,2] = 'N'
laExpFileStru[9,3] = 14
laExpFileStru[9,4] = 2
=gfCrtTmp(lcExp2Excl,@laExpFileStru,IIF(lcRpSort='D',"CDIVISION+",'')+"ACCOUNT+STORE",lcExp2Excl,.F.)
*!* B609798,1 MMT 01/15/2012 Sales Summary report exports details not sumary to Excel[END]
*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfGetDesc
PARAMETERS lcDivision
lcDivLName = ""
*loDBFCodes.Setorder('Codes')
IF loDBFCodes.SEEK('N'+lcDivision+'Y'+'CDIVISION')
 SELECT Codes
 SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcDivision+'Y'+'CDIVISION'  FOR crltd_nam = 'DIVLNAME  '
*!*      IF crltd_nam = 'DIVLNAME  '
   lcDivLName = crltd_vlu
*!*      ENDIF
  ENDSCAN
ENDIF
RETURN lcDivLName
