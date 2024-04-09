*:***************************************************************************
*: Program file  : ARPOSTCH
*: Program desc. : Post Dated Checks
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : Ahmed Khalifa  (AKM)
*: Date          : 01/23/2007
*: Tracking Entry: N000582 - T20061226.0025
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO ARPOSTCH
*:***************************************************************************
*:Modifications:
*B608286,1 MMT 09/25/2007 Fix bug or wrong Account Name in report [T20070830.0018]
*:***************************************************************************
*-- lcTmpFile  : Name of temp file used to store the data after collection

#INCLUDE R:\Aria4xp\reports\ar\ARPOSTCH.H
LOCAL lcAcntDesc AS STRING, lcTmpExp as String

loogScroll.cCROrientation = 'P'
llWorkFile   = .F.
lcSalsFile   = ''
lcFilterExp = loogScroll.lcRpExp
*-- if user change filter criteria then you must collect data again
loogScroll.lcOGLastForm = 'ARPOSTCH'
loogScroll.cCROrientation = 'P'

DIMENSION loogScroll.laCRParams[1,2]

loogScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loogScroll.laCRParams[1,2] = Lang_Customer_Postdated_Checks_List
loogScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Customer_Postdated_Checks_List,oAriaApplication.GetHeaderText("Lang_Customer_Postdated_Checks_List",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


lcTmpFile=loogScroll.gfTempName()
=lfCreateWrkCur(lcTmpFile)

SELECT POSTDCHQ
LOCATE

lcTmpExp =STRTRAN(lcFilterExp ,'CUSTOMER.','POSTDCHQ.')
*lcTmpExp =STRTRAN(lcFilterExp ,'POSTDCHQ.',' ')

SCAN for EVALUATE(lcTmpExp)  && Scan and fill the table for Post date Cheques that meet the condition
    SCATTER MEMO MEMVAR
    SELECT (lcTmpFile)
    APPEND BLANK
    REPLACE account WITH m.account, Amount WITH m.Amount, PayDate WITH m.PayDate, ChequeNo WITH m.ChequeNo
ENDSCAN

SELECT Customer
LOCATE

SELECT (lcTmpFile)
LOCATE

SCAN && Get Account Description
    *B608286,1 MMT 09/25/2007 Fix bug or wrong Account Name in report [Start]
    *SELECT Customer
    *=gfSeek('M'+account,'Customer')
    *lcAcntDesc = BtName
    *REPLACE DESCRIPTION WITH lcAcntDesc IN SELECT (lcTmpFile)
    =gfSeek('M'+ &lcTmpFile..account,'Customer')
    lcAcntDesc = Customer.BtName
    REPLACE DESCRIPTION WITH lcAcntDesc IN (lcTmpFile)
    *B608286,1 MMT 09/25/2007 Fix bug or wrong Account Name in report [End]
ENDSCAN

*!* =========================================================
*!* Copy data to Temp file and display report
*!* =========================================================
IF RECCOUNT(lcTmpFile) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
lcTmpFile=ADDBS(oAriaApplication.WorkDir)+lcTmpFile+".DBF"
COPY TO (lcTmpFile)
DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] =  lcTmpFile
gfDispRe ()

*!*************************************************************
*! Name      : lfCreateWrkCur
*! Developer : Ahmed Khalifa
*! Date      : 01/23/2007
*! Purpose   : create temp cursor to work on
*!*************************************************************
*! Called from : Program preview
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCreateWrkCur()
*!*************************************************************

FUNCTION lfCreateWrkCur (lcTmpFileName AS STRING)
    DIMENSION laTempacstru[5,4]
    laTempacstru[1,1]='Account'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]=5
    laTempacstru[1,4]=0

    laTempacstru[2,1]='Description'
    laTempacstru[2,2]='C'
    laTempacstru[2,3]=30
    laTempacstru[2,4]=0

    laTempacstru[3,1]='PayDate'
    laTempacstru[3,2]='D'
    laTempacstru[3,3]=8
    laTempacstru[3,4]=0

    laTempacstru[4,1]='ChequeNo'
    laTempacstru[4,2]='C'
    laTempacstru[4,3]=10
    laTempacstru[4,4]=0

    laTempacstru[5,1]='Amount'
    laTempacstru[5,2]='N'
    laTempacstru[5,3]=14
    laTempacstru[5,4]=2

    CREATE CURSOR (lcTmpFileName) FROM ARRAY  laTempacstru


    *!*************************************************************
    *! Name      : lfwRepWhen
    *! Developer : Ahmed Khalifa
    *! Date      : 01/23/2007
    *! Purpose   : Option Grid When function
    *!*************************************************************
    *! Called from : Option Grid
    *!*************************************************************
    *! Calls       :
    *!*************************************************************
    *! Passed Parameters : None
    *!*************************************************************
    *! Return      : None
    *!*************************************************************
    *! Example     : = lfwRepWhen()
    *!*************************************************************

FUNCTION lfwRepWhen

    =gfOpenTable('Customer','Customer','SH')
    =gfOpenTable('POSTDCHQ','POSTDCHQ','SH')
