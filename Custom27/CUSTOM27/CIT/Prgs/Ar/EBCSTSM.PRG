*!*************************************************************
*! Name      : EBCSTSM
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Send Customers summary Balance to CIT
*!*************************************************************
*! Calls     : None
*!*************************************************************
PARAMETERS lcLgFactor
lcTranType ="A"
DECLARE laTRltFld[4,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnDiscRate'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnDaysDue'
laTRltFld[4,1] = 'NTERDISCD '
laTRltFld[4,2] = 'lnDiscDays'
STORE 0   TO lnDiscRate,lnDaysDue,lnDiscDays
STORE ' ' TO lcTEOM,lcBrowseTl,lcOldValue
STORE .F. TO llBrowse,llInvoice,llCrMemo,llOrders

*SSE 11/12/2003 Add Aging Type variable. [Begin]
lcAgeType = gfGetMemVar('XAGINGTYPE')
*SSE 11/12/2003 Add Aging Type variable. [End]

lcWindTitl = 'Send Customer Summary Balance'

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
*=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
*=gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDRA','SH')
*=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcCustomer = SPACE(5)
lcCustName = SPACE(30)
lcCustFact = SPACE(10)
lnLastTran = 2

*SSE 11/06/2003
ldAgeDate = {}

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcSelBmp   = gcBmpHome + "SEL.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"

STORE '' TO lcClientID,lcClientNo,lcBatchId,lcPassWord,lcLgFactor
STORE  0 TO lnAssignNo

*-- Create transaction temp. file
lcTempTran = gfTempName()
CREATE TABLE (gcWorkDir+lcTempTran) ;
	(Type C(1), Account C(5), TranNum C(6), cSelect C(1))
INDEX ON CsELECT+Type+Account+TranNum TAG 'SELECT'
INDEX ON Type + TranNum TAG RangeSelct

INDEX ON Type+Account+TranNum TAG (lcTempTran)  ADDITIVE
SELECT(lcTempTran)
SET RELATION TO 'M'+Account INTO CUSTOMER

*-- Number of selected transactions
STORE 0 TO lnNoTrans

*B607498,1 SSE 10/14/2003 Some fixes for CIT factor. [Begin]
*lcOutFile = 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
lcOutFile = " "
IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" ADDITIVE
ENDIF  
*B607498,1 SSE 10/14/2003 Some fixes for CIT factor. [End]

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

DO (gcScrDir+gcWinAppl+"\EBCSTSM.SPX")  with "I"

*!*************************************************************
*! Name : lfWrtCST
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*!**************************************************************
*! Synopsis : Write an output Customer Summary Balance to CIT.
*!*************************************************************
*! Called from : 
*!         Procedures : EBCSTSM
*!*************************************************************
*! Calls : 
*!         FUNCTIONS : lfAddSecRec
*!                     lfAddCusRec
*!                     lfAddInvRec
*!                     lfAddRetRec
*!                     lfAddSubRec
*!                     lfAddTrnRec
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWrtCST
PRIVATE lcCustomer

WAIT 'Creating outbound Customer Summary Balance file...' WINDOW NOWAIT

*-- Initialize total number of Name/Address records, Invoice records and
*-- Credit records
STORE 0 TO lnTCusRec, lnTInvRec, lnTRetRec

*-- Initialize total invoice amount, total credit amount and total net amount
STORE 0 TO lnTInvAmt, lnTRetAmt, lnTTotAmt
SELECT (lcTempTran)
SET ORDER TO TAG SELECT

*-- Add Security record
=lfAddSecRec()

*-- First write Invoice records
IF SEEK('»M')
  *-- Initialize subtotal number of Name/Address records, Invoice records
  *-- and Credit records
  STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec
  
  *-- Initialize subtotal invoice amount, subtotal credit amount and 
  *-- subtotal net amount
  STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

  DO WHILE cSelect+Type+Account+TranNum = "»M"
    lcCustomer = Account
    IF SEEK('M'+lcCustomer,'Customer') 
      
      *SSE 11/06/2003
      ldAgeDate = Customer.AgeDate
      
      IF !Empty(Customer.FactAcct)
        lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
      ELSE
        lcCustNum = "9999999"
      ENDIF
    ENDIF  
  
    SCAN REST WHILE cSelect+Type+Account+TranNum = "»M"
      *-- Add Invoice Record Type "A"
         =lfAddCusRec()
      *-- Increament number of Customter records
      lnSCusRec = lnSCusRec + 1
    ENDSCAN
    =lfAddSubRec()
    EXIT
  ENDDO
  *-- Increament total number of records and total amount
  lnTCusRec = lnTCusRec + lnSCusRec
  lnTInvRec = lnTInvRec + lnSInvRec
  lnTRetRec = lnTRetRec + lnSRetRec
  lnTInvAmt = lnTInvAmt + lnSInvAmt
  lnTRetAmt = lnTRetAmt + lnSRetAmt
ENDIF  
SELECT (lcTempTran)
SET RELATION TO

lnTTotAmt = lnTInvAmt - lnTRetAmt
*-- Add Transmission total record Type "T"
=lfAddTrnRec()

*!*************************************************************
*! Name : lfAddSecRec
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*!*************************************************************
*! Synopsis : Write the security record in the output orders text file
*!*************************************************************
*! Called from : 
*!         FUNCTION : lfWrtCST()
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddSecRec
PRIVATE lcSegLine

lcSegLine = '$$ADD'
*-- Client ID
lcSegLine = lcSegLine + SPACE(1) + 'ID=' + 'EP003F'
*-- BATCH ID
lcSegLine = lcSegLine + SPACE(1) + "BID='CN" + SUBSTR(lcBatchId,1,4)+"'"
*-- Password
lcSegLine = lcSegLine + SPACE(1) + "PASSWORD=" + SUBSTR(lcPassWord,1,4)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddCusRec
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*!*************************************************************
*! Synopsis : Write a customer record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddCusRec
PRIVATE lcSegLine

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Record Type
lcSegLine = lcSegLine + "A"
*-- Client Customer Number

*SSE 11/06/2003
*lcSegLine = lcSegLine + PADR(lcCustomer,15)
lcSegLine = lcSegLine + PADR(Customer.Account,15)

*-- Customer Name
lcSegLine = lcSegLine + SUBSTR(Customer.BtName,1,30)
*-- Customer Address Line 1
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress12,1,30)
*-- Customer Address Line 2
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress22,1,30)
*-- Customer Address City
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress32,1,17)
*-- Customer Address State
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress42,1,2)
*-- Zip Code
lcSegLine = lcSegLine + SUBSTR(Customer.cAddress52,1,9)
*-- Country 
lcSegLine = lcSegLine + LEFT(Customer.cAddress62,17) + SPACE(3)
*-- Phone
lcSegLine = lcSegLine + LEFT(Customer.PHONE1,10)
*-- Amount OutStandaing

*SSE 11/06/2003
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.NetBal))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(Customer.NetBal)))),11)

*SSE 11/12/2003 Use Aging Type variable. [Begin]
*-- Amount Current
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.CURRENT))),11)
*-- Past Due Amount 01-09
*lcSegLine = lcSegLine + SPACE(11)
*-- Past Due Amount 10-30
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.AGE30))),11)
*-- Past Due Amount 31-60
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.AGE60))),11)
*-- Past Due Amount 61-90
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.AGE60))),11)
*-- Past Due Amount Over 90
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.AGE120))),11)

*-- Amount Current

*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(IIF(lcAgeType='D',Customer.Current,Customer.TerCurrent)))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(IIF(lcAgeType='D',Customer.Current,Customer.TerCurrent))))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

*-- Past Due Amount 01-09
lcSegLine = lcSegLine + SPACE(11)

*-- Past Due Amount 10-30
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(IIF(lcAgeType='D',Customer.Age30,Customer.TerAge30)))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(IIF(lcAgeType='D',Customer.Age30,Customer.TerAge30))))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

*-- Past Due Amount 31-60
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(IIF(lcAgeType='D',Customer.Age60,Customer.TerAge60)))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(IIF(lcAgeType='D',Customer.Age60,Customer.TerAge60))))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

*-- Past Due Amount 61-90
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(IIF(lcAgeType='D',Customer.Age90,Customer.TerAge90)))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(IIF(lcAgeType='D',Customer.Age90,Customer.TerAge90))))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

*-- Past Due Amount Over 90
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(IIF(lcAgeType='D',Customer.Age120,Customer.TerAge120)))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(IIF(lcAgeType='D',Customer.Age120,Customer.TerAge120))))),11)
*SSE 11/16/2003 Use the absolute amount. [End]
*SSE 11/12/2003 Use Aging Type variable. [End]

*-- Credit Amount
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.OPENCR))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(Customer.OPENCR)))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

*-- Disputed Amount
*SSE 11/16/2003 Use the absolute amount. [Begin]
*lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(Customer.CHGBACK))),11)
lcSegLine = lcSegLine + PADL(ALLTRIM(STR(INT(ABS(Customer.CHGBACK)))),11)
*SSE 11/16/2003 Use the absolute amount. [End]

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddSubRec
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*!*************************************************************
*! Synopsis : Write the subtotal record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtInv
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddSubRec
PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt
*-- Increament the assignment number
lnAssignNo = lnAssignNo + 1
lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Record Type
lcSegLine = lcSegLine + "S"
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
*-- Aging Date

*SSE 11/06/2003
*lcInvDate = PADL(MONTH(CUSTOMER.AGEDATE),2,'0') + PADL(DAY(CUSTOMER.AGEDATE),2,'0') +;
*            RIGHT(STR(YEAR(CUSTOMER.AGEDATE),4),2)
lcInvDate = PADL(MONTH(ldAgeDate),2,'0') + PADL(DAY(ldAgeDate),2,'0') +;
            RIGHT(STR(YEAR(ldAgeDate),4),2)
            
lcSegLine = lcSegLine + lcInvDate
= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name : lfAddTrnRec
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*!*************************************************************
*! Synopsis : Write the transmission total record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddTrnRec
PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

lcSegLine = ''
*-- Filer
lcSegLine = lcSegLine + SPACE(4)
*-- Record Type
lcSegLine = lcSegLine + "T"
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
*-- Transmision Date ??
lcTransDate = PADL(MONTH(Date()),2,'0') + PADL(DAY(Date()),2,'0') +;
              RIGHT(STR(YEAR(Date()),4),2)
lcSegLine = lcSegLine + lcTransDate

= FPUTS(lnOutFile,lcSegLine)
*!*************************************************************
*! Name      : lfvFactor
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Validate Factors
*!*************************************************************
*! Calls     : ARIABROW,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias

lnAlias = SELECT()
IF llBrowse OR (!EMPTY(lcFactor) .AND. !SEEK(lcFactor,'SycFact'))
  lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
                          'cFacCode','laBrowArr'),SycFact.cFacCode,SPACE(6))
ENDIF

IF lcOldValue <> lcFactor
  STORE .T. TO llInvoice , llCrMemo , llOrders
ENDIF

IF EMPTY(lcFactor)
  SHOW GET pbTransact DISABLE
ELSE
  SHOW GET pbTransact ENABLE
ENDIF
=lfRefresh('EBcstsm')
SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Validate Customer
*!*************************************************************
*! Calls     : CUSBROWM,lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvCustomer()
*!*************************************************************
FUNCTION lfvCustomer
PRIVATE xAccount

IF llBrowse .OR. (!EMPTY(lcCustomer) .AND. !SEEK('M'+lcCustomer,'CUSTOMER')) 
  xAccount = lcCustomer
  SELECT CUSTOMER
  DO CUSBROWM WITH xAccount
  lcCustomer = xAccount
ENDIF
IF lcOldValue <> lcCustomer
  STORE .T. TO llInvoice , llCrMemo , llOrders
ENDIF
lcCustFact = IIF(EMPTY(Customer.FactAcct),'**NEW**',Customer.FactAcct)
lcCustName = Customer.BtName
=lfRefresh('EBCSTSM')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvTrans
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Select Transactions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvTrans()
*!*************************************************************
FUNCTION lfvTrans
SELECT Customer

=SEEK('M','CUSTOMER')
SCAN REST WHILE TYPE+Account+Store="M" FOR  cFacCode = lcFactor
  INSERT INTO (lcTempTran) (Type,Account) VALUES ('M',CUSTOMER.Account)  
ENDSCAN 

lcTranBrow = 'Select Cusustomers'
lnTrans    = 0
lcTranMode = "M"
lcTranType ="A"
lcLgFactor=' ' 
DO (gcScrDir+gcWinAppl+"\EBSNDSM.SPX") WITH "A"
SET ORDER TO TAG 'SELECT' IN (lcTempTran)
IF SEEK('»',lcTempTran)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
SET ORDER TO TAG (lcTempTran) IN (lcTempTran)
*!*************************************************************
*! Name      : lfvProceed
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Validate Proceed
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

*-- Client Information
lcClientID = SycFact.cClientID
lcClientNo = SycFact.cClientNo
lcBatchId  = 'CN' + lcClientNo 
lcPassWord = SycFact.cPassWord
lnAssignNo = SycFact.AssignNo
lnBatchNo  = MAX(SycFact.BatchNo,1)

IF EMPTY(lcClientID) .OR. EMPTY(lcClientNo) .OR. EMPTY(lcPassWord)
  =gfModalGen('TRM00000B00000','ALERT','','','Factor information not complete. Cannot proceed.')
  RETURN
ENDIF
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file
lnOutFile = FCREATE(lcOutFile,0)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
*-- Get Transmission date in the format MMDDYY
lcTranDate = PADL(MONTH(gdSysDate),2,'0') + PADL(DAY(gdSysDate),2,'0') +;
             RIGHT(STR(YEAR(gdSysDate),4),2)

=lfWrtCst()
*-- Increament the Assignment number in customer file
SELECT SYCFACT
= RLOCK()
REPLACE AssignNo WITH IIF(lnLastTran=1,0,lnAssignNo) ,;
        BatchNo  WITH IIF(lnBatchNo=99, 1,lnBatchNo+1)
UNLOCK
=FCLOSE(lnOutFile)
CLEAR READ

=gfModalGen('TRM00000B00000','ALERT','','','Output file '+lcOutFile+' has been created.')

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003 
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!             'O' : Switch
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSelect('S')
*!*************************************************************
FUNCTION lfvSelect
PARAMETERS lcType
PRIVATE lcKey

SELECT (lcTempTran)
lcKey = Type+Account+TranNum
DO CASE
  CASE lcType = 'S'
    REPLACE cSelect WITH IIF(cSelect='»',' ','»')
  CASE lcType = 'A'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH '»' WHILE Type+Account+TranNum = lcTranMode
  CASE lcType = 'N'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH ' ' WHILE Type+Account+TranNum = lcTranMode
  CASE lcType = 'V'
    =SEEK(lcTranMode)
    REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type+Account+TranNum = lcTranMode
ENDCASE
=SEEK(lcKey)
=lfwBrowTrn()
*!*************************************************************
*! Name      : lfvSwitch
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Switch selection between Customers
*!*************************************************************
*! Calls     : lfBrowTran
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvSwitch()
*!*************************************************************
FUNCTION lfvSwitch
lcTranMode = "A"
SHOW GET pbTrans,1 ENABLE PROMPT '\<Customers'
=lfBrowTran()
*!*************************************************************
*! Name      : lfReadAct
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Screen Activate function
*!*************************************************************
*! Calls     : lfClearKey,gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDeAct
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Screen Deactivate function
*!*************************************************************
*! Calls     : lpAction, lpTab, lpBackTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

IF WONTOP()=lcTranBrow
  ON KEY LABEL ALT+S DO lpAction WITH 'S'
  ON KEY LABEL ALT+A DO lpAction WITH 'A'
  ON KEY LABEL ALT+N DO lpAction WITH 'N'
  ON KEY LABEL ALT+V DO lpAction WITH 'V'

  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  ON KEY LABEL TAB     DO lpTab     WITH 'EBSNDTR2','pbSelect'
  ON KEY LABEL BACKTAB DO lpBackTab WITH 'EBSNDTR2','pbClose'
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpAction
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Call functions
*!*************************************************************
*! Calls     : lfvSwitch,lfvSelect
*!*************************************************************
*! Parameters: lcType :
*!             'S' : Select/UnSelect
*!             'A' : Select All
*!             'V' : Invert
*!             'N' : Select None
*!             'O' : Switch
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpAction WITH 'S'
*!*************************************************************
PROCEDURE lpAction
PARAMETERS lcType
=lfvSelect(lcType)

*!*************************************************************
*! Name      : lpTab
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'EBSNDTR2','pbSelect'
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Clear key
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+S 
ON KEY LABEL ALT+A 
ON KEY LABEL ALT+N 
ON KEY LABEL ALT+V 
ON KEY LABEL ALT+O
ON KEY LABEL ALT+B

*!*************************************************************
*! Name      : lfBrowTran
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Browse invoices/Credit Memos and Sales Orders
*!*************************************************************
*! Calls     : lfwBrowTrn
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfBrowTran

lcSelect = SELECT()
SELECT (lcTempTran)
GO Top
lnTrans = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnTrans,'>',' '):H=' ':R:1:W=.F.,cSelect :H=' ' :R,"

lcFields = lcFields + " Customer.Account :R :H='Account',Customer.stName :R :H='Name',Customer.caddress1 :R :H='ST Address',Customer.cAddress2 :R :H='ST',"
lcFields = lcFields + "Customer.cAddress3 :R :H='ST City',Customer.caddress4 :R :H='ST State',Customer.cAddress5 :R :H='ST Zip',"
lcFields = lcFields + "Customer.cAddress12 :R :H='BT Address',Customer.caddress22 :R :H='BT',Customer.cAddress32 :R :H='BT City',"
lcFields = lcFields + "Customer.cAddress42 :R :H='BT State',Customer.caddress52 :R :H='BT Zip',Customer.Phone1 :R :H='Phone #...', "
lcFields = lcFields + "Customer.Buyer :R :H='Buyer',Customer.SalesRep :R :H='Rep',Customer.Totage :R :H='Balance'"
lcTranBrow = 'Select Customers'

BROWSE FIELDS &lcFields  ;
       WINDOW EBSNDTR1   ;
       IN WINDOW EBSNDTR ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfwBrowTrn() ;
       TITLE lcTranBrow
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrowTrn
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Show invoices/Credit Memos and Sales Orders
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwBrowTrn()
*!*************************************************************
FUNCTION lfwBrowTrn

SELECT (lcTempTran)
lnTrans = RECNO()
SHOW WINDOW (lcTranBrow) REFRESH SAME
IF EMPTY(cSelect)
  SHOW GET pbSelect,1 ENABLE PROMPT  '\<Select'
ELSE
  SHOW GET pbSelect,1 ENABLE PROMPT  'Un\<Select'
ENDIF

*!*************************************************************
*! Name      : lfvRange
*! Developer : Hassan Ibrahim Ali
*! Date      : 06/25/2003
*! Purpose   : Valid function for push button "Range", the
*!             select range button of the select transactions
*!             screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvRange()
*!*************************************************************
*B804247,1 HS
*!*************************************************************
FUNCTION lfvRange

PRIVATE lcRangeFr , lcRangeTo , lcScope , lcKey , lcOldOrder , lcOldNear
STORE "" TO lcRangeFr , lcRangeTo , lcScope

DO (gcScrDir + gcWinAppl + "\EbArange.SPX")

IF !EMPTY(lcScope)
  SELECT (lcTempTran)
  *-- Save file environment
  lcOldNear  = SET("NEAR")
  lcOldOrder = ORDER()
  lcKey      = Type + TranNum
  
  SET ORDER TO RangeSelct
  SET NEAR ON
  
  SEEK lcTranMode + lcRangeFr
  DO CASE
    CASE lcScope = 'A'
      REPLACE REST cSelect WITH '»' WHILE Type + TranNum <= lcTranMode + lcRangeTo
    CASE lcScope = 'N'
      REPLACE REST cSelect WITH ' ' WHILE Type + TranNum <= lcTranMode + lcRangeTo
    CASE lcScope = 'I'
      REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE Type + TranNum <= lcTranMode + lcRangeTo
  ENDCASE
  
  *-- Restore file environment
  =SEEK(lcKey)
  IF EMPTY(lcOldOrder)
    SET ORDER TO
  ELSE
    SET ORDER TO &lcOldOrder
  ENDIF
  SET NEAR &lcOldNear
  =lfwBrowTrn()
ENDIF

*!*************************************************************
*! Name : lfGetFile
*! Auth : Sameh Saiid Ezzat (SSE)
*! Date : 14/10/2003
*! Ref  : B607498
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of;
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfGetFile
PRIVATE ltOutFile , lcTalk

IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" ADDITIVE
ELSE
  lcOutFile = ""
ENDIF  

ltOutFile = PUTFILE('' , lcOutFile , "NEW" )

IF !EMPTY(ltOutFile)
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
  =lfRefresh('EBFACIN')
ENDIF  
*!*************************************************************

