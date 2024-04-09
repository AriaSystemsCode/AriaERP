*:***************************************************************************
*: Program file  : ARYTDRFM.PRG
*: Program desc. : CUSTOMIZED YTD SALES REPORT FOR FRENCH ROOM.
*: Date          : 10/12/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101978
*:
*:***************************************************************************
*: Calls :
*:    Functions  : lfCreatTmp , lfCollect  , lfPrnHdr   , lfPrnLin  , lfSub
*:               : lfGrand    , lfvAccount , lfsAccount , lfvSalsRp , lfClrRead
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARSCALFG
*:***************************************************************************
*:Modifications
*:***************************************************************************
*:C102030,1 BWA 12/14/2000 Add a new column with header "%sales"
*:                         It will be calculated as follows: = (YTS sales/total YTS sales) * 100
*:***************************************************************************

*-- Initial the vaiables.
*C102030,1 BWA 12/14/2000 Add new variable.[START]
*STORE 0 TO lnSPSales,lnSYTDC,lnSYTDS,lnSMTD,lnSONORDER,;
           lnGPSales,lnGYTDC,lnGYTDS,lnGMTD,lnGONORDER,ROW,PAGENO
STORE 0 TO lnSPSales,lnSYTDC,lnSYTDS,lnSMTD,lnSONORDER,;
           lnGPSales,lnGYTDC,lnGYTDS,lnGMTD,lnGONORDER,ROW,PAGENO,lnTotYtds
STORE SPACE(03) TO lcSubSals
*C102030,1 [END]

R_WIDTH    = 'W'
R_TITLE    = " YTD SALES REPORT "
XTITLE     = ""

IF lcRpSortBy $ 'SA'
  STORE "" TO lcSalesRep
ENDIF

*--- Create and collect data function.
=lfCreatTmp()
=lfCollect()

SELECT(lcActTmp)
GOTO TOP
IF RECCOUNT() = 0
  WAIT CLEAR
  = gfDialog( 'I', 'No records have been selected for the report.' )
  RETURN
ELSE
  SET DEVICE TO PRINT
  =lfPrnHdr()
  =lfPrnLin()
  =lfGrand()
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF
*--- End Main

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to create and fill temp file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatTmp ()
*!*************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (lcActTmp) (Account C(05)  , BTNAME C(30)  , LORDER D(08) ,;
                         SALESREP C(03) , PSALES N(12,3), YTDC N(12,3) ,;
                         YTDS N(12,3)   , MTD N(12,3)   , ONORDER N(12,3))

*--ENd of lfCreatTmp.
*!*************************************************************
*! Name      : lfCollect
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollect ()
*!*************************************************************
FUNCTION lfCollect

lcRpexp = lcRpexp + ' AND ' + IIF(!EMPTY(lcSalesRep), ' CUSTOMER.SALESREP = lcSalesRep ' , '.T.' )
SELECT CUSTOMER
=SEEK('M')

SCAN REST WHILE Type+Account+Store ='M' FOR &lcRpexp
  SCATTER MEMVAR MEMO
  WAIT WINDOW "Selecting Data For Account # " +m.Account NOWAIT
  *--- Collect On Order [OPENAMT]
  WAIT WINDOW "Calculate Sales Order for account# " +m.Account NOWAIT
  M.LOrder = {}
  m.ONORDER = 0.00

  *--- Collect ONORDER From INVHDR File
  SELECT ORDHDR
  IF SEEK(m.Account)
    SCAN REST WHILE  ACCOUNT+ORDER = m.Account FOR STATUS $ 'OH'
      m.ONORDER = m.ONORDER + OPENAMT
    ENDSCAN
    DO WHILE !BOF()
     SKIP -1
     IF STATUS<>'X'
       M.LOrder = ENTERED
       EXIT
     ENDIF
    ENDDO
  ENDIF
  *--- Collect Invoice (PSALES & YTDS)
  ldLastYear  = CTOD('01/01/'+STR(YEAR(DATE())-1,4))
  ldSameDate  = GOMONTH(DATE(),-12)
  ldCurrYear  = CTOD('01/01/'+STR(YEAR(DATE()),4))
  ldCurrMonth = DATE() - DAY(DATE())+1

  *--- Collect PSALES,YTDC,MTD From INVHDR File
  SELECT INVHDR
  =SEEK(m.Account)
  WAIT WINDOW "Calculate Invoices for account# " +m.Account NOWAIT
  m.PSALES = 0
  m.YTDS   = 0
  m.MTD    = 0 
  SCAN REST WHILE Account+Invoice = m.Account FOR STATUS <> 'V'
    IF BETWEEN(INVDATE,ldLastYear,ldSameDate)
      m.PSALES = m.PSALES + INVHDR.SHIPAMT
    ENDIF
    IF BETWEEN(INVDATE,ldCurrYear,DATE())
      m.YTDS = m.YTDS + INVHDR.SHIPAMT
    ENDIF
    IF BETWEEN(INVDATE,ldCurrMonth,DATE())
      m.MTD = m.MTD + INVHDR.SHIPAMT
    ENDIF
  ENDSCAN
  *C102030,1 BWA 12/14/2000 Calculate the total sales ytd.[START]
  lnTotYtds = lnTotYtds + M.YTDS
  *C102030,1 [END]

  *--- Collect YTDC From Credit File
  SELECT CREDIT
  m.YTDC = 0
  =SEEK(m.Account)
  SCAN REST WHILE ACCOUNT = m.Account FOR BETWEEN(TRANDATE,ldCurrYear,DATE())
    m.YTDC = m.YTDC + Amount
  ENDSCAN
  INSERT INTO (lcActTmp) FROM MEMVAR
ENDSCAN

SELECT(lcActTmp)
DO CASE
  CASE  lcRpSortBy = 'S'
    INDEX ON YTDS TAG (lcActTmp) DESCENDING
  CASE  lcRpSortBy = 'R'
    INDEX ON SALESREP+Account TAG (lcActTmp)
  CASE  lcRpSortBy = 'A'
    INDEX ON ACCOUNT TAG (lcActTmp)
ENDCASE
WAIT CLEAR

*--End of lfCollect
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to print header.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnHdr ()
*!*************************************************************
FUNCTION lfPrnHdr
  
PAGENO = PAGENO + 1
DO RPT_HDR WITH 'YTD Sales Report',''+ XTITLE,R_WIDTH
ROW = 5
             *0****0****1****0****2****0****3****0****4****0****5****0****6****0****7****0****8****0****9****0****0****0****1****0****2****0****3
             *Account# Name                           Last Order Sales rep Prior Sales Ytd Credit  Ytd Sales   Mtd Sales    On Order   Sales Trend

*C102030,1 BWA 12/14/2000 Add the new field.[START]
*@ ROW,0  SAY 'Account# Name                           Last Order Sales rep Prior Sales Ytd Credit  Ytd Sales   Mtd Sales    On Order   Sales Trend'
@ ROW,0  SAY 'Account# Name                          Last Order Rep %Sales Prior Sales Ytd Credit  Ytd Sales   Mtd Sales    On Order   Sales Trend'
*C102030,1 [END]

ROW = ROW + 1
@ ROW , 0 SAY REPLICATE('_',132)
ROW = ROW + 1

*-- End of lfPrnHdr.
*!*************************************************************
*! Name      : lfPrnLin
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to print lines.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnLin ()
*!*************************************************************
FUNCTION lfPrnLin

SELECT(lcActTmp)

GOTO TOP
DO WHILE !EOF()
  DO CASE
    CASE lcRpSortBy = 'A'
      lcSubTVar = ACCOUNT
      lcKey     = 'Account'
    CASE lcRpSortBy = 'R'
      lcSubTVar = SALESREP
      lcKey     = 'SalesRep'
    CASE lcRpSortBy = 'S'
      lcSubTVar = YTDS
      lcKey     = 'YTDS'
  ENDCASE
  WAIT WINDOW "Printing data for Account# :" +Account NOWAIT
  SCAN REST WHILE &lcKey = lcSubTVar
    @ ROW,000 SAY Account

    *C102030,1 BWA 12/14/2000 Print the new field and modify postion of another fields.[START]
    *@ ROW,009 SAY BTNAME
    *@ ROW,040 SAY LOrder
    *@ ROW,051 SAY SalesRep

    @ ROW,009 SAY LEFT(BTNAME,29)
    @ ROW,039 SAY LOrder
    @ ROW,050 SAY LEFT(SalesRep,3)
    @ ROW,054 SAY (YTDS/lnTotYtds)*100    PICTURE '99.99%'
    *C102030,1 [END]

    @ ROW,061 SAY PSales     PICTURE '99999999.99'
    @ ROW,072 SAY YTDC       PICTURE '99999999.99'
    @ ROW,083 SAY YTDS       PICTURE '99999999.99'
    @ ROW,095 SAY MTD        PICTURE '99999999.99'
    @ ROW,107 SAY ONORDER    PICTURE '99999999.99'
    *--- Accumulate subtotal Var
    IF lcRpSortBy = 'R'
      lcSubSals  = SalesRep
      lnSPSales  = lnSPSales  + PSales
      lnSYTDC    = lnSYTDC    + YTDC
      lnSYTDS    = lnSYTDS    + YTDS
      lnSMTD     = lnSMTD     + MTD
      lnSONORDER = lnSONORDER + ONORDER
    ENDIF
    lnGPSales  = lnGPSales  + PSales
    lnGYTDC    = lnGYTDC    + YTDC
    lnGYTDS    = lnGYTDS    + YTDS
    lnGMTD     = lnGMTD     + MTD
    lnGONORDER = lnGONORDER + ONORDER
    lnTender = IIF(PSales <> 0,((YTDS - PSales)/PSales)*100,0)
    lnTender = ROUND(lnTender,0)
    DO CASE
      CASE lnTender = 0
        lnTender = STR(lnTender,6) +'%'
      CASE lnTender > 0
        lnTender = "+"+ALLT(STR(lnTender,6))+'%'
      CASE lnTender < 0
         lnTender = STR(lnTender,6)+'%'
    ENDCASE
    @ ROW,124 SAY PADL(lnTender,8)
    ROW = ROW + 1
    IF ROW > 54
      =lfPrnHdr()
    ENDIF
  ENDSCAN
  IF lcRpSortBy = 'R'
    =lfSub()
    IF !EOF()
      =lfPrnHdr()
    ENDIF
  ENDIF
ENDDO

*--End OF lfPrnLin.
*!*************************************************************
*! Name      : lfSub
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to print Subtotal.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSub ()
*!*************************************************************
FUNCTION lfSub

@ ROW , 0 SAY REPLICATE('_',132)
ROW = ROW + 1
@ ROW , 0 SAY "Subtotal for salesrep :  "+lcSubSals +SPACE(03) +IIF(SEEK(lcSubSals,'SALESREP'),SALESREP.NAME,'')
lnSTender = IIF(lnSPSales <> 0,((lnSYTDS - lnSPSales)/lnSPSales)*100,0)
lnSTender = ROUND(lnSTender,0)
DO CASE
  CASE lnSTender = 0
    lnSTender = STR(lnSTender,6) +'%' 
  CASE lnSTender > 0
    lnSTender = "+"+ALLT(STR(lnSTender,6))+'%'
  CASE lnSTender < 0
    lnSTender = STR(lnSTender,6)+'%' 
ENDCASE
@ ROW,061 SAY lnSPSales   PICTURE '99999999.99'
@ ROW,072 SAY lnSYTDC     PICTURE '99999999.99'
@ ROW,083 SAY lnSYTDS     PICTURE '99999999.99'
@ ROW,095 SAY lnSMTD      PICTURE '99999999.99'
@ ROW,107 SAY lnSONORDER  PICTURE '99999999.99'
@ ROW,124 SAY PADL(lnSTender,8)
ROW = ROW + 1
@ ROW , 0 SAY REPLICATE('_',132)
ROW = ROW + 1
lnSPSales  = 0
lnSYTDC    = 0
lnSYTDS    = 0
lnSMTD     = 0
lnSONORDER = 0

*--End OF lfSub.
*!*************************************************************
*! Name      : lfGrand
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Function to print grand total.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : ARYTDRFM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGrand ()
*!*************************************************************
FUNCTION lfGrand

@ ROW , 0 SAY REPLICATE('_',132)
ROW = ROW + 1
@ ROW , 0 SAY "Grand total : "
lnGTender = IIF(lnGPSales <> 0,((lnGYTDS - lnGPSales)/lnGPSales)*100,0)
lnGTender = ROUND(lnGTender,0)
DO CASE
  CASE lnGTender = 0
    lnGTender = STR(lnGTender,3) +'%'
  CASE lnGTender > 0
    lnGTender = "+"+STR(lnGTender,3)+'%'
  CASE lnGTender < 0
    lnGTender = STR(lnGTender,4)+'%'
ENDCASE
@ ROW,061 SAY lnGPSales   PICTURE '99999999.99'
@ ROW,072 SAY lnGYTDC     PICTURE '99999999.99'
@ ROW,083 SAY lnGYTDS     PICTURE '99999999.99'
@ ROW,095 SAY lnGMTD      PICTURE '99999999.99'
@ ROW,107 SAY lnGONORDER  PICTURE '99999999.99'
@ ROW,127 SAY PADR(lnGTender,5)
ROW = ROW + 1
@ ROW , 0 SAY REPLICATE('_',132)
ROW = ROW + 1
WAIT CLEAR

*--End OF lfGrand.
*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 10/12/2000
*! Purpose   : Validation function for the Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvAccount

PRIVATE lcObjName , lcObjVal , llObjRet

laOldVal = SPACE(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF

*-- End of lfvAccount.
*!**************************************************************************
*! Name      : lfsAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 10/12/2000
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsAccount()
*!**************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!**************************************************************************
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'
  SELECT Customer
  GO TOP
ENDIF

*-- End of lfsAccount.
*!*************************************************************
*! Name      : lfvSalsRp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 10/12/2000
*! Purpose   : Validate sales rep.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalsRp()
*!*************************************************************
FUNCTION lfvSalsRp

lcRepCode = VARREAD()

IF !EMPTY(&lcRepCode) AND  !SEEK(&lcRepCode, "SALESREP") 
  XREPCODE = &lcRepCode
  DO REPCHK WITH XREPCODE, .T.
  &lcRepCode = XREPCODE
ENDIF

*-- End of lfvSalsRp.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 10/12/2000
*! Purpose   : Refresh the filters in the option grid.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
FUNCTION lfClrRead

*-- Refresh the filters in the option grid.
STORE "" TO lcSalesRep
CLEAR READ

*-- End of lfClrRead.
