*:**************************************************************************
*: Program file  : ARCUSCR (037685)
*: Program desc. : CUSTOMER CREDIT REPORT
*: Date          : 05/30/2005
*: System        : Aria 4 XP
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Wael M. Abo-Shawareb (WSH)
*:**************************************************************************
*: Calls :
*:    Procedures : lpRpPrint
*:    Functions  : lfvAcct() , lfCollTime()
*:
*:**************************************************************************
*: Example : DO ARCUSCR
*:**************************************************************************
*: This Program is due to  N132294,1
*:**************************************************************************
#INCLUDE R:\Aria4xp\reports\ar\arcuscr.H
lcStTime = TIME()     && Variable to collect the start time


loOgScroll.lcOGLastForm = 'ARCUSCR'
loOgScroll.cCRorientation = 'P'

DIMENSION loOgScroll.laCRParams[2,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_CUSTOMER_CREDIT
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER_CREDIT,oAriaApplication.GetHeaderText("LANG_CUSTOMER_CREDIT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] = 'lnPrnDec'
loOgScroll.laCRParams[2,2] = IIF(llPrnDec,1,0)


IF loOgScroll.llOGFltCh && OG Filters changed
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Collecting_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Collecting_Data,oAriaApplication.GetHeaderText("Lang_Collecting_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lfCrtTemp()
  lfCollect()
ELSE
  IF FILE(oAriaApplication.WorkDir +  lcTmpData+ ".DBF") AND !USED(lcTmpData)
    USE oAriaApplication.WorkDir +  lcTmpData+ ".DBF" IN 0
  ENDIF
ENDIF


SELECT(lcTmpData)
LOCATE
*-- if no data match Criteria.
IF EOF(lcTmpData)
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

IF USED()
  USE IN (lcTmpData)
ENDIF

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW NOWAIT Lang_Total_time_is +  ALLTRIM(STR(lnInterval,6,2)) + Lang_Seconds
WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Total_time_is,oAriaApplication.GetHeaderText("Lang_Total_time_is",AHEADERFILE)) +  ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Seconds,oAriaApplication.GetHeaderText("Lang_Seconds",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpData + ".DBF"

gfDispRe()

RETURN
*-- End of Report code.

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Collect Data in the Temp file
*!**************************************************************************
*! Example   : =lfCollect
*!**************************************************************************
FUNCTION lfCollect

llUseTerm = .F.
llAcc = .F.
lcTempAcc = ''

lnPOS = ASCAN(loOgScroll.laOGFxFlt,'CUSTOMER.ACCOUNT')
IF lnPos > 0
  lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  lcTempAcc = loOgScroll.laOGFxFlt[lnPOS,6]
ENDIF
llAcc = !EMPTY(lcTempAcc) AND USED(lcTempAcc) AND RECCOUNT(lcTempAcc) > 0

IF llAcc
  SELECT(lcTempAcc)
  LOCATE
  IF EOF()
    llAcc = .F.
  ENDIF
ENDIF

lnTermPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'CUSTOMER.CTERMCODE'),1)
IF lnTermPos  > 0
  lcTermStr = LOOGSCROLL.laOGFXFlt[lnTermPos ,6]
  lcTermFile = loOGScroll.gfTempName()
  llUseTerm = IIF(LEN(lcTermStr)>0,.T.,.F.) AND lfConvertToCursor(lcTermStr,'CTERMCODE',lcTermFile)
ENDIF

loDBFordhdr.Setorder('OrdAcct')
IF llAcc
  SELECT(lcTempAcc)
  SCAN
   *B610552,1 MMT 10/22/2013 Error while Preview report and there is a selected account[Start]
   *WAIT WINDOW NOWAIT Lang_Collecting_Data_For_Account +Account
   WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTING_DATA_FOR_ACCOUNT,oAriaApplication.GetHeaderText("LANG_COLLECTING_DATA_FOR_ACCOUNT",AHEADERFILE)) +Account
   *B610552,1 MMT 10/22/2013 Error while Preview report and there is a selected account[End]

    IF loDBFCustomer.Seek('M'+Account) AND IIF(llUseTerm,SEEK(CUSTOMER.CTERMCODE,lcTermFile),.T.)
	    SELECT Customer
	    SCATTER MEMO MEMVAR
	    XUNSHPM = 0
	    IF loDBFordhdr.SEEK(Customer.Account,'OrdAcct')
	      SELECT ordhdr
	      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = Customer.Account FOR STATUS $ 'OH'
		    IF cCurrCode = oAriaApplication.BaseCurrency
		      XUNSHPM = XUNSHPM + OpenAmt
		    ELSE
		      XUNSHPM = XUNSHPM + gfAmntDisp(OpenAmt,"O",Entered)
		    ENDIF
	      ENDSCAN
	      m.XUNSHPM  = XUNSHPM
	      m.CTRMDESC = SUBSTR(gfCodDes(Customer.CTERMCODE,'CTERMCODE',.T.),1,24)
	      INSERT INTO (lcTmpData) FROM MEMVAR
	    ENDIF
	 ENDIF
  ENDSCAN
ELSE
  loDBFCustomer.Seek('M')
  SELECT Customer
  SCAN REST WHILE TYPE+ACCOUNT+STORE = 'M' FOR IIF(llUseTerm,SEEK(CUSTOMER.CTERMCODE,lcTermFile),.T.)
    SELECT Customer
    SCATTER MEMO MEMVAR
    XUNSHPM = 0
    IF loDBFordhdr.SEEK(Customer.Account,'OrdAcct')
      SELECT ordhdr
      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = Customer.Account FOR STATUS $ 'OH'
	    IF cCurrCode = oAriaApplication.BaseCurrency
	      XUNSHPM = XUNSHPM + OpenAmt
 	    ELSE
	      XUNSHPM = XUNSHPM + gfAmntDisp(OpenAmt,"O",Entered)
	    ENDIF
	  ENDSCAN
	  m.XUNSHPM  = XUNSHPM
	  m.CTRMDESC = SUBSTR(gfCodDes(Customer.CTERMCODE,'CTERMCODE',.T.),1,24)
	  INSERT INTO (lcTmpData) FROM MEMVAR
	ENDIF
  ENDSCAN
ENDIF
*!*	llScanInFilt = !EMPTY(lcActFltr) AND USED(lcActFltr)

*!*	SELECT CUSTOMER

*!*	=gfGoTop()

*!**************************************************************************
*! Name      : lpPrint
*! Developer : Sameh (SSE)
*! Date      : 05/04/1999
*! Purpose   : Printing loop for the main report
*!**************************************************************************
*! Called from : Main Program (ARCUSCR)
*!**************************************************************************
*! Calls       : gfCodDes()
*!**************************************************************************
*! Example     : Do lpPrint
*!**************************************************************************
*
PROCEDURE lpPrint
*--Printing loop

SELECT CUSTOMER
*-- loop filterd accounts
SCAN
  *B610552,1 MMT 10/22/2013 Error while Preview report and there is a selected account[Start]
  *WAIT WINDOW NOWAIT Lang_Collecting_Data_For_Account +Account
  WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLECTING_DATA_FOR_ACCOUNT,oAriaApplication.GetHeaderText("LANG_COLLECTING_DATA_FOR_ACCOUNT",AHEADERFILE)) +Account
  *B610552,1 MMT 10/22/2013 Error while Preview report and there is a selected account[End]
  *-- if page over to next page
  IF ROW >55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ARCUSCR','',R_WIDTH
    *+++NAD
    *@ 05,01 SAY 'Acct# Name.............. City........... State. Zip....... Terms.......................  Credit   Unshipped      Unpaid        Open'
    *@ 06,01 SAY '                                                                                          Limit Merchandise    Invoices      Credit'
     @ 05,01 SAY 'Acct# Name.............. City........... State. Zip....... Terms...................      Credit   Unshipped      Unpaid        Open'
     @ 06,01 SAY '                                                                                          Limit Merchandise    Invoices      Credit'
    *+++nad
    @ 07,00 SAY REPLICATE('=',132)

    ROW = 8
  ENDIF  && end if page over to next page.

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3...
*ACCT# NAME.............. CITY........... STATE. ZIP....... TERMS.....  CREDIT   UNSHIPPED      UNPAID        OPEN
*                                                                       LIMIT MERCHANDISE    INVOICES      CREDIT
*12345 123456789012345678 123456789012345 123456 1234567890 1234567890 1234567  1234567.99  1234567.99  1234567.99
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

  *-- Calculate unshipped amount

  SELECT ORDHDR    && no active order for ORHDHR in OG and that for Rush-More
  *-- Use full index expression with for condition in sum command to activate Rush-More (Partial because of Status)

  *B603175,1 replace SUM command with SCAN..ENDSCAN statement to accumulate
  *B603175,1 the unshipped amount after converting it to Base Currency [Begin]
  *SUM OpenAmt TO XUNSHPM FOR (Account+cOrdType+Order = Customer.Account) AND STATUS $ 'OH'
  =SEEK(Customer.Account)
  *B603700,1 (Start) Reset the variable that holds the unshipped amount.
  XUNSHPM=0
  *B603700,1  (End)

  *B603700,1 (Start) Replace SCAN WHILE With SCAN FOR
  *SCAN WHILE Account+cOrdType+Order = Customer.Account AND STATUS $ 'OH'
  SCAN WHILE Account+cOrdType+Order = Customer.Account FOR STATUS $ 'OH'
  *B603700,1 (End)

    IF cCurrCode = gcBaseCurr
      XUNSHPM = XUNSHPM + OpenAmt
    ELSE
      XUNSHPM = XUNSHPM + gfAmntDisp(OpenAmt,"O",Entered)
    ENDIF
  ENDSCAN
  *B603175,1 replace SUM command with SCAN..ENDSCAN statement [End]

  *-- Calculate open credit line
  SELECT CUSTOMER

  *-- Credit Line - Unshipped Merchandise - Open Invoices (NETBAL)
  XOPENCR=CRLIMIT - XUNSHPM - NETBAL

  *-- Say to printer.
  @ ROW,01  SAY ACCOUNT
  @ ROW,07  SAY SUBSTR(BTNAME,1,18)
  @ ROW,26  SAY PADR(CADDRESS32,15)
  @ ROW,42  SAY PADR(CADDRESS42,6)
  @ ROW,49  SAY PADR(CADDRESS52,10)
  *E301429,1 (Start) Increase the amount field.
  *@ ROW,60  SAY PADR(ALLTRIM(CTERMCODE),6)+' '+PADR(ALLTRIM(gfCodDes(CTERMCODE,'CTERMCODE')),21)
   @ ROW,60  SAY SUBSTR(gfCodDes(CTERMCODE,'CTERMCODE',.T.),1,24)
  *@ ROW,89  SAY CRLIMIT  PICTURE '9999999'
   @ ROW,85  SAY CRLIMIT  PICTURE '99999999999'


  *@ ROW,97  SAY XUNSHPM  PICTURE '99999999.99'
  *@ ROW,109  SAY NETBAL  PICTURE '99999999.99'
  *@ ROW,121 SAY XOPENCR  PICTURE '99999999.99'
  IF llPrnDec
    @ ROW,97  SAY XUNSHPM  PICTURE '99999999.99'
    @ ROW,109  SAY NETBAL  PICTURE '99999999.99'
    @ ROW,121 SAY XOPENCR  PICTURE '99999999.99'
  ElSE
    @ ROW,97  SAY XUNSHPM  PICTURE '99999999999'
    @ ROW,109  SAY NETBAL  PICTURE '99999999999'
    @ ROW,121 SAY XOPENCR  PICTURE '99999999999'
  ENDIF
  *E301429,1  (End)
  ROW = ROW + 1

  *-- Calculate Grand totals.
  XTOT(1) = XTOT(1) + XUNSHPM
  XTOT(2) = XTOT(2) + NETBAL
  XTOT(3) = XTOT(3) + XOPENCR

ENDSCAN  && end loop filterd accounts.

WAIT CLEAR

ROW = ROW + 1
@ ROW,00 SAY REPLICATE ('*',132)
ROW = ROW + 1
@ ROW,01 SAY '*** GRAND TOTAL ***'

*-- Say Grand totals.
*E301429,1 (Start) Increase the amount field.
*@ ROW,97  SAY XTOT(1)     PICTURE '99999999.99'
*@ ROW,109 SAY XTOT(2)     PICTURE '99999999.99'
*@ ROW,121 SAY XTOT(3)     PICTURE '99999999.99'
IF llPrnDec
  @ ROW,97  SAY XTOT(1)     PICTURE '99999999.99'
  @ ROW,109 SAY XTOT(2)     PICTURE '99999999.99'
  @ ROW,121 SAY XTOT(3)     PICTURE '99999999.99'
ELSE
  @ ROW,97  SAY XTOT(1)     PICTURE '99999999999'
  @ ROW,109 SAY XTOT(2)     PICTURE '99999999999'
  @ ROW,121 SAY XTOT(3)     PICTURE '99999999999'
ENDIF
*E301429,1 (End)
ROW = ROW + 1

@ ROW,00 SAY REPLICATE ('*',132)
ROW = ROW + 1

*-- End of lpPrint.

*!**************************************************************************
*! Name      : lfvAcct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/03/1999
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAcct()
*!**************************************************************************
*
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  lcAccount = lcObjVal
  DO CUSBROWM WITH lcAccount
  lcObjVal = lcAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*-- End of lfvAcct.

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/03/1999
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Called from : Report code section.
*!**************************************************************************
*! Passed Parameters : Start collection date,End collection date
*!**************************************************************************
*! Returns           : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*************************************************************
*! Name      : lfGetFilters
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/30/2006
*! Purpose   : Create Filter Cursors
*!*************************************************************
FUNCTION lfGetFilters

*-- Account Filter.
lcCurName = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcActFltr = lcCurName
ELSE
  IF TYPE("lcActFltr") = "C" AND USED(lcActFltr)
    USE IN (lcActFltr)
  ENDIF
  lcActFltr = ''
ENDIF

*-- Terms Filter
lcCond = lfCheckFilter(3, 'CUSTOMER.CTERMCODE')
IF !EMPTY(lcCond)
  lcTrmFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcTrmFltr) (CTERMCODE C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcTrmFltr)
  INDEX ON CTERMCODE TAG (lcTrmFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE CTERMCODE WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcTrmFltr") = "C" AND USED(lcTrmFltr)
    USE IN (lcTrmFltr)
  ENDIF
  lcTrmFltr = ''
ENDIF
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*! Date      : 08/03/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'CTERMCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
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
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/11/2006
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*lfwOGWhen()
FUNCTION lfwOGWhen
loDBFCustomer = CreateObject("RemoteTable","Customer","Customer",'Customer',SET("DATASESSION"))
loDBFordhdr   = CreateObject("RemoteTable","ordhdr","OrdAcct",'ordhdr',SET("DATASESSION"))

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/11/2006
*! Purpose   : temp file creation
*!*************************************************************
FUNCTION lfCrtTemp

DIMENSION laFileStru[1]
SELECT Customer
lnStrulen = AFIELDS(laFileStru)
DIMENSION laFileStru[lnStrulen +2,18]
laFileStru[lnStrulen +1,1] = 'XUNSHPM'
laFileStru[lnStrulen +1,2] = 'N'
laFileStru[lnStrulen +1,3] = 11
laFileStru[lnStrulen +1,4] = 2

laFileStru[lnStrulen +2,1] = 'CTRMDESC'
laFileStru[lnStrulen +2,2] = 'C'
laFileStru[lnStrulen +2,3] = 30
laFileStru[lnStrulen +2,4] = 0



  STORE ' ' TO  laFileStru[lnStrulen +1,7],laFileStru[lnStrulen +1,8],;
                laFileStru[lnStrulen +1,9],laFileStru[lnStrulen +1,10],;
                laFileStru[lnStrulen +1,11],laFileStru[lnStrulen +1,12],;
                laFileStru[lnStrulen +1,13],laFileStru[lnStrulen +1,14],;
                laFileStru[lnStrulen +1,15],laFileStru[lnStrulen +1,16]
  STORE 0 TO    laFileStru[lnStrulen +1,17] ,laFileStru[lnStrulen +1,18]

  STORE ' ' TO  laFileStru[lnStrulen +2,7],laFileStru[lnStrulen +2,8],;
                laFileStru[lnStrulen +2,9],laFileStru[lnStrulen +2,10],;
                laFileStru[lnStrulen +2,11],laFileStru[lnStrulen +2,12],;
                laFileStru[lnStrulen +2,13],laFileStru[lnStrulen +2,14],;
                laFileStru[lnStrulen +2,15],laFileStru[lnStrulen +2,16]
  STORE 0 TO    laFileStru[lnStrulen +2,17] ,laFileStru[lnStrulen +2,18]

= gfCrtTmp(lcTmpData,@laFileStru,.F. ,.F.,.F.)