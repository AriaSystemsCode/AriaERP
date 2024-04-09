*!*************************************************************
*! Name      : EBFACIN
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Send Invoice/Credit Memo and Sales Orders to CIT
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Invoice/Credit Memos
*!                       'O' : Send Sales Orders
*!*************************************************************
*!B804247,1 HS  09/01/2001 add select range of invoices and orders
*!E500439,1 AMM 09/10/2001 some modifications on layout.
*!*************************************************************
PARAMETER lcTranType

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
** lcTranType='I' for invoice
lcWindTitl = IIF(lcTranType='I','Invoices/Credit Memos Transmission','Send Sales Orders Transmission')

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
=gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDRA','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcCustomer = SPACE(5)
lcCustName = SPACE(30)
lcCustFact = SPACE(10)
lnLastTran = 2

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcSelBmp   = gcBmpHome + "SEL.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"

STORE '' TO lcClientID,lcClientNo,lcBatchId,lcPassWord
STORE  0 TO lnAssignNo

IF lcTranType = 'O'
  =gfOpenFile(gcDataDir+'CitTrnLn',gcDataDir+'CitTrnLn','SH')
  *-- Create CIT Send Orders Temp. File
  lcTmpCit = gfTempName()
  CREATE TABLE (gcWorkDir+lcTmpCit) (cFacCode C(6),BatchNo N(2),dDate D,Account c(5),Order C(6))
ENDIF

*-- Create transaction temp. file
lcTempTran = gfTempName()
CREATE TABLE (gcWorkDir+lcTempTran) ;
	(Type C(1), Account C(5), TranNum C(6), cSelect C(1))
INDEX ON CsELECT+Type+Account+TranNum TAG 'SELECT'
*B804247,1 [Begin]
INDEX ON Type + TranNum TAG RangeSelct
*B804247,1 [End]
INDEX ON Type+Account+TranNum TAG (lcTempTran)  ADDITIVE
SET RELATION TO 'M'+Account INTO CUSTOMER
IF lcTranType = 'I'
  SET RELATION TO Account+TranNum INTO INVHDR ADDITIVE
  SET RELATION TO Account+TranNum INTO RETHDR ADDITIVE
ELSE
  SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
ENDIF  
*-- Number of selected transactions
STORE 0 TO lnNoTrans
lcOutFile = 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

DO (gcScrDir+gcWinAppl+"\EBFACIN.SPX") WITH 'I'

IF lcTranType = 'O'
  IF USED(lcTmpCit)
    USE IN (lcTmpCit)
  ENDIF
  SELECT CitTrnLn
  APPEND FROM (gcWorkDir+lcTmpCit)
ENDIF

*!*************************************************************
*! Name : lfWrtInv
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!**************************************************************
*! Synopsis : Write an output Invoices/Credits text file to be send to CIT.
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
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
FUNCTION lfWrtInv
PRIVATE lcCustomer

WAIT 'Creating outbound Invoice/Credit file...' WINDOW NOWAIT

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
IF SEEK('»I')
  *-- Initialize subtotal number of Name/Address records, Invoice records
  *-- and Credit records
  STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec
  
  *-- Initialize subtotal invoice amount, subtotal credit amount and 
  *-- subtotal net amount
  STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

  DO WHILE cSelect+Type+Account+TranNum = "»I"
    lcCustomer = Account
    IF SEEK('M'+lcCustomer,'Customer') 
      IF !Empty(Customer.FactAcct)
        lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
      ELSE
        lcCustNum = "9999999"
      ENDIF
    ENDIF  
      
    *-- Add Name/Address Record Type "A"
    =lfAddCusRec()
    *-- Increament number of Name/Addres records
    lnSCusRec = lnSCusRec + 1
    
    SCAN REST WHILE cSelect+Type+Account+TranNum = "»I"+lcCustomer
      *-- Add Invoice Record Type "D"
      =lfAddInvRec()
      *-- Increament number of invoice records
      lnSInvRec = lnSInvRec + 1
    ENDSCAN
    *B804424 Hassan [Begin]
  *!* ENDDO
  *!* *-- Add Sub Total Record Type "S"
  *!* =lfAddSubRec()
  =lfAddSubRec()
  ENDDO
  *B804424 Hassan [End]
  *-- Increament total number of records and total amount
  lnTCusRec = lnTCusRec + lnSCusRec
  lnTInvRec = lnTInvRec + lnSInvRec
  lnTRetRec = lnTRetRec + lnSRetRec
  lnTInvAmt = lnTInvAmt + lnSInvAmt
  lnTRetAmt = lnTRetAmt + lnSRetAmt
ENDIF  
SELECT (lcTempTran)
SET RELATION TO
*-- Second write Credits records
IF SEEK("»C")
  *-- Initialize subtotal number of Name/Address records, Invoice records 
  *-- and Credit records
  STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec
  
  *-- Initialize subtotal invoice amount, subtotal credit amount and 
  *-- subtotal net amount
  STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

  *-- Set relation to return header file
  SET RELATION TO Account+TranNum INTO RetHdr
  
  *-- Set relation to invoice header file
  SELECT RetHdr
  SET RELATION TO Account+Invoice INTO InvHdr
  SELECT (lcTempTran)
  DO WHILE cSelect+Type+Account+TranNum = "»C"
    lcCustomer = Account
    *-- Get customer factor number
    IF SEEK('M'+lcCustomer,'Customer') 
      IF !Empty(Customer.FactAcct)
        lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
      ELSE
        lcCustNum = "9999999"
      ENDIF
    ENDIF  
    
    *-- Add Name/Address Record Type "A"
    =lfAddCusRec()
    *-- Increament number of Name/Addres records
    lnSCusRec = lnSCusRec + 1

    SCAN REST WHILE cSelect+Type+Account+TranNum = "»C"+lcCustomer
      *-- Add Credit Record Type "C"
      =lfAddRetRec()
      *-- Increament number of invoice records
      lnSRetRec = lnSRetRec + 1
    ENDSCAN
  ENDDO
  *-- Add Sub Total Record Type "S"
  =lfAddSubRec()

  *-- Increament total number of records and total amount
  lnTCusRec = lnTCusRec + lnSCusRec
  lnTInvRec = lnTInvRec + lnSInvRec
  lnTRetRec = lnTRetRec + lnSRetRec
  lnTInvAmt = lnTInvAmt + lnSInvAmt
  lnTRetAmt = lnTRetAmt + lnSRetAmt
ENDIF  
lnTTotAmt = lnTInvAmt - lnTRetAmt
*-- Add Transmission total record Type "T"
=lfAddTrnRec()

*!*************************************************************
*! Name : lfAddSecRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write the security record in the output orders text file
*!*************************************************************
*! Called from : 
*!         FUNCTION : lfWrtOrder(),lfWrtInv()
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddSecRec
PRIVATE lcSegLine

lcSegLine = '$$ADD'
*-- Client ID
*ahmed 
*lcSegLine = lcSegLine + SPACE(1) + 'ID=' + SUBSTR(lcClientID,1,6)
lcSegLine = lcSegLine + SPACE(1) + 'ID=' + 'EP003F'
*ahmed end
*-- BATCH ID
*E500439,1 AMM change the word BatchID to BID
*lcSegLine = lcSegLine + SPACE(1) + "BATCHID='" + SUBSTR(lcBatchId,1,6)+"'"
lcSegLine = lcSegLine + SPACE(1) + "BID='" + SUBSTR(lcBatchId,1,6)+"'"
*E500439,1 AMM end
*-- Password
lcSegLine = lcSegLine + SPACE(1) + "PASSWORD=" + SUBSTR(lcPassWord,1,4)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddCusRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
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
*-- House Date

*E500439,1 AMM coment out
*lcSegLine = lcSegLine + lcTranDate
*-- Factoring Company Number
*lcSegLine = lcSegLine + "4"
*E500439,1 AMM end

*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Line Number
*E500439,1 AMM
*lcSegLine = lcSegLine + IIF(lcTranType='O',PADL(lnBatchNo,2,'0'),"01")
*Hassan
*lcSegLine = lcSegLine + IIF(lcTranType='O',SPACE(2),"01")
lcSegLine = lcSegLine + SPACE(2)
*End Hassan
*E500439,1 AMM end
*-- Record Type
lcSegLine = lcSegLine + "A"
*E500439,1 AMM
*-- Customer Number
*lcSegLine = lcSegLine + lcCustNum
*ahmed 
*lcSegLine = lcSegLine + PADR(lcCustNum,15)
lcSegLine = lcSegLine + PADR(lcCustomer,15)
*ahmed end
*E500439,1 AMM end

*E500439,1 AMM start
*-- Customer Name
*lcSegLine = lcSegLine + SUBSTR(Customer.BtName,1,22)
*-- Customer Address Line 1
*lcSegLine = lcSegLine + SUBSTR(Customer.cAddress12,1,20)
*-- Customer Address Line 2
*lcSegLine = lcSegLine + SUBSTR(Customer.cAddress22,1,20)
*-- Customer Address City
*lcSegLine = lcSegLine + SUBSTR(Customer.cAddress32,1,18)
*-- Customer Address State
*lcSegLine = lcSegLine + SUBSTR(Customer.cAddress42,1,2)
*-- Zip Code
*lcSegLine = lcSegLine + SUBSTR(Customer.cAddress52,1,5)
*-- Filer
*lcSegLine = lcSegLine + SPACE(16)

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
*- phone
lcSegLine = lcSegLine + LEFT(Customer.PHONE1,10)
*-- Filer
*E500439,1 AMM end

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddInvRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write an invoice record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddInvRec
PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM
*B804424 Hassan [Begin]
*!*	lcSegLine = ''
*!*	*-- House Date
*!*	lcSegLine = lcSegLine + lcTranDate
*!*	*-- Factoring Company Number
*!*	lcSegLine = lcSegLine + "4"
*!*	*-- Client Number
*!*	lcSegLine = lcSegLine + lcClientNo
*!*	*-- Line Number
*!*	lcSegLine = lcSegLine + "01"
*!*	*-- Record Type
*!*	lcSegLine = lcSegLine + "D"
*!*	*-- Customer Number
*!*	*ahmed
*!*	*lcSegLine = lcSegLine + lcCustNum
*!*	lcSegLine = lcSegLine + lcCustomer
*!*	*ahmed end
*!*	*-- Filer
*!*	*lcSegLine = lcSegLine + SPACE(7)
*!*	*-- Invoice Number
*!*	lcSegLine = lcSegLine + PADL(&lcTempTran..TranNum,7,'0')
*!*	*-- Entry Code
*!*	lcSegLine = lcSegLine + '12'
*!*	*-- Risk Code
*!*	lcSegLine = lcSegLine + SPACE(1)
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(2)
*!*	*-- Store Number
*!*	lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Store),4,'0')
*!*	*-- Invoice Date
*!*	lcInvDate = PADL(MONTH(InvHdr.InvDate),2,'0') + PADL(DAY(InvHdr.InvDate),2,'0') +;
*!*	            RIGHT(STR(YEAR(InvHdr.InvDate),4),2)
*!*	            
*!*	lcSegLine = lcSegLine + lcInvDate
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + '0'
*!*	*-- Invoice Amount
*!*	lcAmount= STRTRAN(STR(InvHdr.TotalChg,9,2) ,' ','0')
*!*	lcAmount= SUBSTR(lcAmount,1,6) + SUBSTR(lcAmount,8,2)
*!*	lcSegLine = lcSegLine + lcAmount

*!*	*- Increament invoice amount and total net amount
*!*	lnSInvAmt = lnSInvAmt + InvHdr.TotalChg
*!*	lnSTotAmt = lnSTotAmt + InvHdr.TotalChg

*!*	*-- Cust Purchase Order#
*!*	lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.CustPo),15,'0')


*!*	*-- Terms Information
*!*	lcFTermRate = SPACE(4)
*!*	lcFTermDays = SPACE(3)
*!*	lcSTermDays = SPACE(3)
*!*	lcEOM       = SPACE(1)
*!*	lcExtraDays = SPACE(3)

*!*	=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

*!*	IF lnDiscRate <> 0
*!*	  *-- First terms rate
*!*	  lcFTermRate = STRTRAN(STR(lnDiscRate,5,2) ,' ','0')
*!*	  lcFTermRate = SUBSTR(lcFTermRate,1,2) + SUBSTR(lcFTermRate,4,2)
*!*	ENDIF  
*!*	IF lnDiscDays <> 0
*!*	  *-- First Terms days
*!*	  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
*!*	  IF lnDaysDue <> 0
*!*	    lcSTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF
*!*	ELSE
*!*	  IF lnDaysDue <> 0
*!*	    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF    
*!*	ENDIF    
*!*	lcEOM = IIF(lcTEOM='Y', 'E', lcEOM)

*!*	*-- First Terms Rate
*!*	lcSegLine = lcSegLine + lcFTermRate
*!*	*-- First Terms Days
*!*	lcSegLine = lcSegLine + lcFTermDays
*!*	*-- Second Terms Rate
*!*	lcSegLine = lcSegLine + SPACE(4)
*!*	*-- Second Terms Days
*!*	lcSegLine = lcSegLine + lcSTermDays
*!*	*-- End Of Month Term Ind.
*!*	lcSegLine = lcSegLine + lcEOM
*!*	*-- Extra Days
*!*	lcSegLine = lcSegLine + lcExtraDays
*!*	*-- As Of Date
*!*	lcSegLine = lcSegLine + SPACE(6)
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(1)
*!*	*-- Interest Code
*!*	lcSegLine = lcSegLine + IIF(.F.,'Z',SPACE(1))
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(12)
*!*	*-- Net Days
*!*	lcSegLine = lcSegLine + SPACE(3)
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(9)
*!*	= FPUTS(lnOutFile,lcSegLine)

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Trade Style + Space(1) Blank
lcSegLine = lcSegLine + Space(2)
*-- Record Type
lcSegLine = lcSegLine + "D"
*-- Customer Number
lcSegLine = lcSegLine + Padr(lcCustomer,15)
*-- Invoice Number
lcSegLine = lcSegLine + PADL(&lcTempTran..TranNum,8,'0')
*-- Filer
lcSegLine = lcSegLine + SPACE(7)
*-- Invoice Amount
lcAmount= STRTRAN(STR(InvHdr.TotalChg,11,2) ,' ','0')
lcAmount= STRTRAN(lcAmount ,'.','')
lcSegLine = lcSegLine + lcAmount
*-- Invoice Date
lcInvDate = PADL(MONTH(InvHdr.InvDate),2,'0') + PADL(DAY(InvHdr.InvDate),2,'0') +;
            RIGHT(STR(YEAR(InvHdr.InvDate),4),2)
lcSegLine = lcSegLine + lcInvDate
*-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*-- Client Terms Code
lcSegLine = lcSegLine + PADR(Right(Allt(InvHdr.cTermcode),3),3)
*-- Merchandise Code
lcSegLine = lcSegLine + IIF(InvHdr.Discount = 0 , Space(1) , '1')
*-- Merchandise Amount
lcSegLine = lcSegLine + IIF( InvHdr.Discount = 0 ,;
                             Space(10)           ,;
                             RIGHT(STRTRAN(InvHdr.ShipAmt , '.' , '') , 10))
*-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(InvHdr.Store) , 5  , ' ')
*-- Cust Purchase Order#
lcSegLine = lcSegLine + PADr(ALLTRIM(InvHdr.CustPo), 22 , ' ')
*-- Cust Dept {OPtional}
lcSegLine = lcSegLine + Space(6)
*-- Risk Code
lcSegLine = lcSegLine + SPACE(1)
*-- Extra Days
lcSegLine = lcSegLine + SPACE(3)
*-- Extra Days Code
lcSegLine = lcSegLine + SPACE(1)
*-- Discount Type Code
lcSegLine = lcSegLine + SPACE(1)
*-- Discount Amount (10)
lcSegLine = lcSegLine + "0000000000"
*-- Credit Memo INvoice #
lcSegLine = lcSegLine + SPACE(8)
*-- Futur Use
lcSegLine = lcSegLine + SPACE(40)

= FPUTS(lnOutFile,lcSegLine)
*B804424 Hassan [End]
*!*************************************************************
*!*************************************************************
*! Name : lfAddRetRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
*!*************************************************************
*! Synopsis : Write a credit record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddRetRec
PRIVATE lcSegLine, lcTmpDate, lcRetDate, lcFTermRate, lcFTermDays, lcEOM

*B804424 Hassan [Begin]

*!*	lcSegLine = ''
*!*	*-- House Date
*!*	lcSegLine = lcSegLine + lcTranDate
*!*	*-- Factoring Company Number
*!*	lcSegLine = lcSegLine + "4"
*!*	*-- Client Number
*!*	lcSegLine = lcSegLine + lcClientNo
*!*	*-- Line Number
*!*	lcSegLine = lcSegLine + "01"
*!*	*-- Record Type
*!*	lcSegLine = lcSegLine + "C"
*!*	*-- Customer Number
*!*	*ahmed
*!*	*lcSegLine = lcSegLine + lcCustNum
*!*	lcSegLine = lcSegLine + lcCustomer
*!*	*ahmed end
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(7)
*!*	*-- Invoice Number
*!*	lcSegLine = lcSegLine + PADL(RetHdr.Invoice,7,'0')
*!*	*-- Entry Code
*!*	lcSegLine = lcSegLine + '22'
*!*	*-- Risk Code
*!*	lcSegLine = lcSegLine + SPACE(1)
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(2)
*!*	*-- Store Number
*!*	lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.Store),4,'0')
*!*	*-- Credit Date
*!*	lcRetDate = PADL(MONTH(RetHdr.CrDate),2,'0') + PADL(DAY(RetHdr.CrDate),2,'0') +;
*!*	            RIGHT(STR(YEAR(RetHdr.CrDate),4),2)

*!*	lcSegLine = lcSegLine + lcRetDate
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + '0'
*!*	*-- Credit Amount
*!*	lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
*!*	lcAmount= SUBSTR(lcAmount,1,6) + SUBSTR(lcAmount,8,2)
*!*	lcSegLine = lcSegLine + lcAmount

*!*	*- Increament Credit amount and total net amount
*!*	lnSRetAmt = lnSRetAmt + RetHdr.TotCredit
*!*	lnSTotAmt = lnSTotAmt + RetHdr.TotCredit

*!*	*-- Cust Purchase Order#
*!*	lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.CustPo),15,'0')

*!*	*-- Terms Information
*!*	lcFTermRate = SPACE(4)
*!*	lcFTermDays = SPACE(3)
*!*	lcSTermDays = SPACE(3)
*!*	lcEOM       = SPACE(1)
*!*	lcExtraDays = SPACE(3)

*!*	=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')
*!*	IF lnDiscRate <> 0
*!*	  *-- First terms rate
*!*	  lcFTermRate = STRTRAN(STR(lnDiscRate,5,2) ,' ','0')
*!*	  lcFTermRate = SUBSTR(lcFTermRate,1,2) + SUBSTR(lcFTermRate,4,2) 
*!*	ENDIF  
*!*	IF lnDiscDays <> 0
*!*	  *-- First Terms days
*!*	  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
*!*	  IF lnDaysDue <> 0
*!*	    IF .F.
*!*	      lcExtraDays = PADL(INT(lnDaysDue),3,'0')
*!*	    ELSE
*!*	      lcSTermDays = PADL(INT(lnDaysDue),3,'0')
*!*	    ENDIF       
*!*	  ENDIF    
*!*	ELSE
*!*	  IF lnDaysDue <> 0
*!*	    lcFTermDays =  PADL(INT(lnDaysDue),3,'0')
*!*	  ENDIF    
*!*	ENDIF    
*!*	lcEOM = IIF(lcTEOM= 'Y', 'E', lcEOM)

*!*	*-- First Terms Rate
*!*	lcSegLine = lcSegLine + lcFTermRate
*!*	*-- First Terms Days
*!*	lcSegLine = lcSegLine + lcFTermDays
*!*	*-- Second Terms Rate
*!*	lcSegLine = lcSegLine + SPACE(4)
*!*	*-- Second Terms Days
*!*	lcSegLine = lcSegLine + lcSTermDays
*!*	*-- End Of Month Term Ind.
*!*	lcSegLine = lcSegLine + lcEOM
*!*	*-- Extra Days
*!*	lcSegLine = lcSegLine + lcExtraDays
*!*	*-- As Of Date
*!*	lcSegLine = lcSegLine + SPACE(6)
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(1)
*!*	*-- Interest Code
*!*	lcSegLine = lcSegLine + IIF(.F.,'Z',SPACE(1))
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(12)
*!*	*-- Net Days
*!*	lcSegLine = lcSegLine + SPACE(3)
*!*	*-- Filler
*!*	lcSegLine = lcSegLine + SPACE(9)
*!*	= FPUTS(lnOutFile,lcSegLine)

lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Trade Style
lcSegLine = lcSegLine + Space(2)
*-- Record Type
lcSegLine = lcSegLine + "R"
*-- Client Customer Number
lcSegLine = lcSegLine + Padr(lcCustomer,15)
*-- Client Order Number
lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.Order),22,'0')
*-- Credit Amount
lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
lcAmount= STRTRAN(lcAmount,'.','')
lcSegLine = lcSegLine + lcAmount
*-- Bill & Hold Code ?
lcSegLine = lcSegLine + Space(1)
*-- Start Ship Date ?
lcSegLine = lcSegLine + Space(6)




*-- Filer
lcSegLine = lcSegLine + SPACE(7)
*-- Invoice Number
lcSegLine = lcSegLine + PADL(RetHdr.Invoice,7,'0')
*-- Entry Code
lcSegLine = lcSegLine + '22'
*-- Risk Code
lcSegLine = lcSegLine + SPACE(1)
*-- Filer
lcSegLine = lcSegLine + SPACE(2)
*-- Store Number
lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.Store),4,'0')
*-- Credit Date
lcRetDate = PADL(MONTH(RetHdr.CrDate),2,'0') + PADL(DAY(RetHdr.CrDate),2,'0') +;
            RIGHT(STR(YEAR(RetHdr.CrDate),4),2)

lcSegLine = lcSegLine + lcRetDate
*-- Filer
lcSegLine = lcSegLine + '0'
*-- Credit Amount
lcAmount= STRTRAN(STR(RetHdr.TotCredit,9,2) ,' ','0')
lcAmount= SUBSTR(lcAmount,1,6) + SUBSTR(lcAmount,8,2)
lcSegLine = lcSegLine + lcAmount

*- Increament Credit amount and total net amount
lnSRetAmt = lnSRetAmt + RetHdr.TotCredit
lnSTotAmt = lnSTotAmt + RetHdr.TotCredit

*-- Cust Purchase Order#
*ahmed
*lcSegLine = lcSegLine + PADr(ALLTRIM(RetHdr.CustPo),15,'0')
lcSegLine = lcSegLine + PADr(ALLTRIM(RetHdr.CustPo),15,' ')
*ahmed end

*-- Terms Information
lcFTermDays = SPACE(3)
lcSTermDays = SPACE(3)
lcEOM       = SPACE(1)
lcExtraDays = SPACE(3)

=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')
lcEOM = IIF(lcTEOM= 'Y', 'E', lcEOM)
*-- Extra Days
lcSegLine = lcSegLine + lcExtraDays
*-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*-- Filler
lcSegLine = lcSegLine + SPACE(1)
*-- Interest Code
lcSegLine = lcSegLine + IIF(.F.,'Z',SPACE(1))
*-- Filler
lcSegLine = lcSegLine + SPACE(12)
*-- Net Days
lcSegLine = lcSegLine + SPACE(3)
*-- Filler
lcSegLine = lcSegLine + SPACE(9)
= FPUTS(lnOutFile,lcSegLine)

*B804424 Hassan [End]

*!*************************************************************
*! Name : lfAddSubRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
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
*B804424 Hassan [Begin]
*!*	*-- Increament the assignment number
*!*	lnAssignNo = lnAssignNo + 1
*!*	lcSegLine = ''
*!*	*-- House Date
*!*	lcSegLine = lcSegLine + lcTranDate
*!*	*-- Factoring Company Number
*!*	lcSegLine = lcSegLine + "4"
*!*	*-- Client Number
*!*	lcSegLine = lcSegLine + lcClientNo
*!*	*-- Line Number
*!*	lcSegLine = lcSegLine + "01"
*!*	*-- Record Type
*!*	lcSegLine = lcSegLine + "S"
*!*	*-- Total number of Name/Address records
*!*	lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
*!*	*-- Total number of Invoice records
*!*	lcSegLine = lcSegLine + PADL(lnSInvRec,6,'0')
*!*	*-- Total number of Credit records
*!*	lcSegLine = lcSegLine + PADL(lnSRetRec,6,'0')

*!*	*-- Total invoice amount
*!*	lcInvAmt= STRTRAN(STR(lnSInvAmt,10,2) ,' ','0')
*!*	lcInvAmt= SUBSTR(lcInvAmt,1,7) + SUBSTR(lcInvAmt,9,2)
*!*	lcSegLine = lcSegLine + lcInvAmt

*!*	*-- Total credits amount
*!*	lcRetAmt= STRTRAN(STR(lnSRetAmt,10,2) ,' ','0')
*!*	lcRetAmt= SUBSTR(lcRetAmt,1,7) + SUBSTR(lcRetAmt,9,2)
*!*	lcSegLine = lcSegLine + lcRetAmt

*!*	*-- Total net amount
*!*	lcTotAmt= STRTRAN(STR(lnSTotAmt,10,2) ,' ','0')
*!*	lcTotAmt= SUBSTR(lcTotAmt,1,7) + SUBSTR(lcTotAmt,9,2)
*!*	lcSegLine = lcSegLine + lcTotAmt

*!*	*-- Assignment number
*!*	lcSegLine = lcSegLine + PADL(lnAssignNo,3,'0')
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(62)
*!*	= FPUTS(lnOutFile,lcSegLine)


*-- Increament the assignment number
lnAssignNo = lnAssignNo + 1
lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + lcClientNo
*-- Trade Style [Always = "99"]
lcSegLine = lcSegLine + "99"
*-- Record Type
lcSegLine = lcSegLine + "S"
*-- Client Customer Number
lcSegLine = lcSegLine + PADR("",15,"9")
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
*-- Total number of Invoice records
lcSegLine = lcSegLine + PADL(lnSInvRec,6,'0')
*-- Total number of Credit records
lcSegLine = lcSegLine + PADL(lnSRetRec,6,'0')
*-- Total invoice amount
lcInvAmt= STRTRAN(STR(lnSInvAmt,13,2) ,' ','0')
lcInvAmt= STRTRAN(lcInvAmt ,'.','')
lcSegLine = lcSegLine + lcInvAmt
*-- Total credits amount
lcRetAmt= STRTRAN(STR(lnSRetAmt,13,2) ,' ','0')
lcRetAmt= STRTRAN(lcRetAmt,'.','')
lcSegLine = lcSegLine + lcRetAmt
*-- Assignment number
lcSegLine = lcSegLine + PADL(lnAssignNo,4,'0')
*-- Factoring Free Code
lcSegLine = lcSegLine + "0"
*-- Future USe
lcSegLine = lcSegLine + Space(95)

*!*	*-- Total net amount
*!*	lcTotAmt= STRTRAN(STR(lnSTotAmt,10,2) ,' ','0')
*!*	lcTotAmt= SUBSTR(lcTotAmt,1,7) + SUBSTR(lcTotAmt,9,2)
*!*	lcSegLine = lcSegLine + lcTotAmt
*!*	*-- Assignment number
*!*	lcSegLine = lcSegLine + PADL(lnAssignNo,3,'0')
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(62)

= FPUTS(lnOutFile,lcSegLine)
*B804424 Hassan [End]
*!*************************************************************
*! Name : lfAddTrnRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/99
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
*B804424 Hassan [Begin] 
*!*	lcSegLine = ''
*!*	*-- House Date
*!*	lcSegLine = lcSegLine + lcTranDate
*!*	*-- Factoring Company Number
*!*	lcSegLine = lcSegLine + "4"
*!*	*-- Client Number
*!*	lcSegLine = lcSegLine + "9999"
*!*	*-- Line Number
*!*	lcSegLine = lcSegLine + "99"
*!*	*-- Record Type
*!*	lcSegLine = lcSegLine + "T"
*!*	*-- Total number of Name/Address records
*!*	lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
*!*	*-- Total number of Invoice records
*!*	lcSegLine = lcSegLine + PADL(lnTInvRec,6,'0')
*!*	*-- Total number of Credit records
*!*	lcSegLine = lcSegLine + PADL(lnTRetRec,6,'0')

*!*	*-- Total invoice amount
*!*	lcInvAmt= STRTRAN(STR(lnTInvAmt,10,2) ,' ','0')
*!*	lcInvAmt= SUBSTR(lcInvAmt,1,7) + SUBSTR(lcInvAmt,9,2)
*!*	lcSegLine = lcSegLine + lcInvAmt

*!*	*-- Total credits amount
*!*	lcRetAmt= STRTRAN(STR(lnTRetAmt,10,2) ,' ','0')
*!*	lcRetAmt= SUBSTR(lcRetAmt,1,7) + SUBSTR(lcRetAmt,9,2)
*!*	lcSegLine = lcSegLine + lcRetAmt
*!*	*-- Total net amount
*!*	lcTotAmt= STRTRAN(STR(ABS(lnTTotAmt),10,2) ,' ','0')
*!*	lcTotAmt= SUBSTR(lcTotAmt,1,7) + SUBSTR(lcTotAmt,9,2)
*!*	*ahmed
*!*	lcTotAmt= PADL(lcTotAmt,12,'0')
*!*	*ahmed end
*!*	lcSegLine = lcSegLine + lcTotAmt

*!*	*-- Last Transmission Ind.
*!*	lcSegLine = lcSegLine + IIF(lnLastTran=1, '1', ' ')
*!*	*-- Filer
*!*	lcSegLine = lcSegLine + SPACE(64)
*!*	= FPUTS(lnOutFile,lcSegLine)


lcSegLine = ''
*-- Client Number
lcSegLine = lcSegLine + "9999"
*-- Trade Style
lcSegLine = lcSegLine + "99"
*-- Record Type
lcSegLine = lcSegLine + "T"
*-- Client Customer Number
lcSegLine = lcSegLine + PADR("",15,"9")
*-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
*-- Total number of Invoice records
lcSegLine = lcSegLine + PADL(lnTInvRec,6,'0')
*-- Total number of Credit records
lcSegLine = lcSegLine + PADL(lnTRetRec,6,'0')
*-- Total (invoice/Credit Order) amount
lcInvAmt= STRTRAN(STR(lnTInvAmt,13,2) ,' ','0')
lcInvAmt= STRTRAN(lcInvAmt ,'.','')
lcSegLine = lcSegLine + lcInvAmt
*-- Total Credits Amount
lcRetAmt= STRTRAN(STR(lnTRetAmt,13,2) ,' ','0')
lcRetAmt= STRTRAN(lcRetAmt ,'.','')
lcSegLine = lcSegLine + lcRetAmt
*-- Transmision Date ??
*-- Invoice Date
lcTransDate = PADL(MONTH(Date()),2,'0') + PADL(DAY(Date()),2,'0') +;
              RIGHT(STR(YEAR(Date()),4),2)
lcSegLine = lcSegLine + lcTransDate
*-- Future Use
lcSegLine = lcSegLine + SPACE(100)
= FPUTS(lnOutFile,lcSegLine)

*B804424 Hassan [End] 
*!*************************************************************
*! Name      : lfvFactor
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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

*B804247,1 HS If the user didn't change the factor code don't change the flags, instead
*          of storing .F. to them [Begin]
*STORE lcOldValue<>lcFactor TO llInvoice,llCrMemo,llOrders
IF lcOldValue <> lcFactor
  STORE .T. TO llInvoice , llCrMemo , llOrders
ENDIF
*B804247,1 If the user didn't change the factor code don't change the... [End]

IF EMPTY(lcFactor)
  SHOW GET pbTransact DISABLE
ELSE
  SHOW GET pbTransact ENABLE
ENDIF
=lfRefresh('EBFACIN')
SELECT (lnAlias)
llBrowse = .F.

*!*************************************************************
*! Name      : lfvCustomer
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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

*B804247,1 HS If the user didn't change the customer code don't change the flags, instead
*             of storing .F. to them [Begin]
*STORE lcOldValue<>lcCustomer TO llInvoice,llCrMemo,llOrders
IF lcOldValue <> lcCustomer
  STORE .T. TO llInvoice , llCrMemo , llOrders
ENDIF
*B804247,1 If the user didn't change the customer code don't change... [End]

lcCustFact = IIF(EMPTY(Customer.FactAcct),'**NEW**',Customer.FactAcct)
lcCustName = Customer.BtName
=lfRefresh('EBFACIN')
llBrowse = .F.

*!*************************************************************
*! Name      : lfvTrans
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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

IF llInvoice AND lcTranType = 'I'
  SELECT (lcTempTran)
  =SEEK('I')
  DELETE REST WHILE Type = 'I'
  SELECT INVHDR
  =SEEK(ALLTRIM(lcCustomer))
  SCAN REST WHILE Account+Invoice = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND Status <>'V'
    INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('I',INVHDR.Account,INVHDR.Invoice)
  ENDSCAN
  llInvoice = .F.
ENDIF
IF llCrMemo AND lcTranType = 'I'
  SELECT (lcTempTran)
  
  *B804247,1 HS The type for credit memo is "C" not "R" [Begin]
  *=SEEK('R')
  *DELETE REST WHILE Type = 'R'
  =SEEK('C')
  DELETE REST WHILE Type = 'C'
  *B804247,1 HS The type for credit memo is "C" not "R" [End]
  
  SELECT RETHDR
  =SEEK(ALLTRIM(lcCustomer))
  SCAN REST WHILE Account+CrMemo = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND Status <>'V'
    INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('C',RETHDR.Account,RETHDR.CrMemo)
  ENDSCAN
  llCrMemo = .F.
ENDIF
IF llOrders AND lcTranType = 'O'
  SELECT (lcTempTran)
  =SEEK('O')
  DELETE REST WHILE Type = 'O'
  SELECT ORDHDR
  IF EMPTY(lcCustomer)
    SET ORDER TO TAG ORDHDR
    =SEEK('O')
    SCAN REST WHILE cOrdType+Order = 'O' FOR cFacCode = lcFactor AND Status <>'X'
      INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.Order)
    ENDSCAN
  ELSE
    =SEEK(lcCustomer+"O")
    SCAN REST WHILE Account+cOrdType+Order = lcCustomer+"O" FOR cFacCode = lcFactor AND Status <>'X'
      INSERT INTO (lcTempTran) (Type,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.Order)
    ENDSCAN
  ENDIF
  SET ORDER TO TAG ORDACCT
  llOrders = .F.
ENDIF
lcTranBrow = IIF(lcTranType='O','Select Sales','Select Invoices/Credit Memos')
lnTrans    = 0
lcTranMode = IIF(lcTranType='O','O','I')
DO (gcScrDir+gcWinAppl+"\EBSNDTR.SPX") WITH IIF(lcTranType='O','O','I')
SET ORDER TO TAG 'SELECT' IN (lcTempTran)
IF SEEK('»',lcTempTran)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
SET ORDER TO TAG (lcTempTran) IN (lcTempTran)

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*E500439,1 AMM
*lcBatchId  = 'HI' + lcClientNo 
*ahmed
*lcBatchId  = 'CI' + lcClientNo 
lcBatchId  = 'C' +IIF(lcTranType='I','I','O') +lcClientNo 
*E500439,1 AMM end
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

=IIF(lcTranType='O',lfWrtOrder(),lfWrtInv())
*-- Increament the Assignment number in customer file
SELECT SYCFACT
= RLOCK()
REPLACE AssignNo WITH IIF(lnLastTran=1,0,lnAssignNo) ,;
        BatchNo  WITH IIF(lnBatchNo=99, 1,lnBatchNo+1)
UNLOCK
=FCLOSE(lnOutFile)
CLEAR READ
*E300817,1 Message : 00370
*E300817,1 Output file has been created
*E300817,1 Button : 00000
*E300817,1 Ok
=gfModalGen('TRM00000B00000','ALERT','','','Output file '+lcOutFile+' has been created.')

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
*! Purpose   : Switch selection between Invoices/Credit Memos and Sales Orders
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
lcTranMode = IIF(lcTranMode='I','C','I')
IF lcTranMode='I'
  SHOW GET pbTrans,1 ENABLE PROMPT 'Credit Mem\<os'
ELSE
  SHOW GET pbTrans,1 ENABLE PROMPT 'Inv\<oices'
ENDIF
=lfBrowTran()

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
IF lcType = 'O'
  =lfvSwitch()
ELSE
  =lfvSelect(lcType)
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*! Developer : Wael Aly Mohamed
*! Date      : 12/31/1999
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
*! Developer : WAM
*! Date      : 12/30/1999
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
*! Developer : WAM
*! Date      : 12/30/1999
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
lnTrans = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnTrans,'>',' '):H=' ':R:1:W=.F.,cSelect :H=' ' :R,"
DO CASE
  CASE lcTranMode = 'I'
    lcFields = lcFields + "InvHdr.Invoice :R,InvHdr.InvDate :R :H='Date',InvHdr.Account :R,InvHdr.Store :R,InvHdr.Order :R,;
    cFactNo=IIF(EMPTY(Customer.FactAcct),'**NEW**',Customer.FactAcct) :R :H='Fact. Accnt.',InvHdr.Ship :R,InvHdr.TotalChg :R :H='Total Chg.'"
    lcTranBrow = 'Select Invoices'
    =SEEK('I')
  CASE lcTranMode = 'C'
    lcFields = lcFields + "RETHDR.CrMemo :R :H='Memo#',RETHDR.CrDate :R :H='Date',RETHDR.Account :R,RETHDR.RANO :R :H='RA#',;
    cFactNo=IIF(EMPTY(Customer.FactAcct),'**NEW**',Customer.FactAcct) :R :H='Fact. Accnt.',RETHDR.Reference :R :H='Reference',"
    lcFields = lcFields + "RETHDR.TotCredit :R :H='Total Credit' "
    lcTranBrow = 'Select Credit Memos'
    =SEEK('C')
  CASE lcTranMode = 'O'
    lcFields = lcFields + "OrdHdr.Order  :R,lcSes=gfCodDes(OrdHdr.Season,'SEASON') :R:H='Season' :P='XXXXXXXXXXXXXXXXXXXX',lcDiv=gfCodDes(OrdHdr.cDivision,'CDIVISION') :R:H='Division' :P='XXXXXXXXXXXXXXXXXXXX',"
    lcFields = lcFields + "OrdHdr.Status :R :H='ST',OrdHdr.Account :R ,cAccName=SUBSTR(Customer.StName,1,20) :R:H='Name',cStore=IIF(OrdHdr.Multi='Y','*MULTI*',OrdHdr.Store) :R :H='Store',"
    lcFields = lcFields + "OrdHdr.Start  :R,OrdHdr.Complete :R,OrdHdr.OpenAmt :R:P='9999999.99':H='Open Amnt.',OrdHdr.ApprAmt :R:P='9999999.99':H='App.Amnt.'"
    lcTranBrow = 'Select Sales Orders'
    =SEEK('O')
ENDCASE
BROWSE FIELDS &lcFields  ;
       WINDOW EBSNDTR1   ;
       IN WINDOW EBSNDTR ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       KEY lcTranMode    ;
       WHEN lfwBrowTrn() ;
       TITLE lcTranBrow
SELECT (lcSelect)

*!*************************************************************
*! Name      : lfwBrowTrn
*! Developer : WAM
*! Date      : 12/30/1999
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
*! Name      : lfWrtOrder
*! Developer : WAM
*! Date      : 12/30/1999
*! Purpose   : Write outbound Order
*!*************************************************************
*! Calls     : lfAddSecRec(),lfAddCusRec(),lfAddOrdRec(),lfAddOrdSub(),lfAddOrdTot()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowTran()
*!*************************************************************
FUNCTION lfWrtOrder
PRIVATE lcCustomer

WAIT'Creating outbound Orders file...' WINDOW NOWAIT
*-- Add Security record
=lfAddSecRec()

*-- Initialize subtotal number of Name/Address records, Order records
STORE 0 TO lnSCusRec, lnSOrdRec, lnSOrdAmt
STORE 0 TO lnTCusRec, lnTOrdRec, lnTOrdAmt
STORE 0 TO lnNewCust

SELECT (lcTempTran)
SET ORDER TO TAG SELECT
=SEEK('»O')
DO WHILE cSelect+Type+Account+TranNum = "»O"
  lcCustomer = Account
  *-- Get customer factor number
  IF SEEK('M'+lcCustomer,'Customer') 
    IF Empty(Customer.FactAcct)
      lnNewCust = lnNewCust + 1
      lcCustNum = "999" + PADL(lnNewCust,4,'0')
    ELSE
      lcCustNum = PADL(ALLTRIM(SUBSTR(Customer.FactAcct,1,7)),7,'0')
    ENDIF
  ENDIF  
  *-- Add Name/Address Record Type "A"
  =lfAddCusRec()
  *-- Increament number of Name/Addres records
  lnSCusRec = lnSCusRec + 1
  
  *-- Add Order Record for this customer Type "D"    
  SCAN WHILE cSelect+Type+Account+TranNum = "»O"+lcCustomer
    =lfAddOrdRec()
    
    *-- Update the CitTrnLn Temp. File
    INSERT INTO (lcTmpCit) (cFacCode,BatchNo,dDate,Account,Order) VALUES ;
    (lcFactor,lnBatchNo,gdSysDate,lcCustomer,OrdHdr.Order)

    *-- Increament number of order records
    lnSOrdRec = lnSOrdRec + 1
  ENDSCAN
ENDDO
*-- Add Sub Total Record Type "S"
=lfAddOrdSub()
  
*-- Increament total number of records and total amount
lnTCusRec = lnTCusRec + lnSCusRec
lnTOrdRec = lnTOrdRec + lnSOrdRec
lnTOrdAmt = lnTOrdAmt + lnSOrdAmt

*-- Add Transmission total record Type "T"
=lfAddOrdTot()
WAIT CLEAR

*!*************************************************************
*! Name : lfAddOrdRec
*! Auth : Wael Aly MOhamed
*! Date : 12/30/1999
*!*************************************************************
*! Synopsis : Write the order record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION _fAddOrdRec
PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermRate, lcFTermDays, lcEOM

lcSegLine = ''
*01-- Transmission Date
lcSegLine = lcSegLine + lcTranDate
*02-- Factoring Company Number
lcSegLine = lcSegLine + "4"
*03-- Client Number
lcSegLine = lcSegLine + lcClientNo
*04-- Batch Number
lcSegLine = lcSegLine + PADL(lnBatchNo,2,'0')
*05-- Record Type
lcSegLine = lcSegLine + "D"
*06-- Customer Number
lcSegLine = lcSegLine + lcCustNum
*07-- Filler
lcSegLine = lcSegLine + SPACE(6)
*08-- Client Control Key
lcSegLine = lcSegLine + &lcTempTran..TranNum + SPACE(8)
*09-- Filler
lcSegLine = lcSegLine + SPACE(9)
*10-- Reserved
lcSegLine = lcSegLine + '0'
*11-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),7,'0')
lnSOrdAmt = lnSOrdAmt + ROUND(OrdHdr.BookAmt,0)
*12-- Filler
lcSegLine = lcSegLine + SPACE(1)
*13-- Order Bill And Hold Indicator
lcSegLine = lcSegLine + SPACE(1)
*14-- Order Start Date
lcOrdDate = PADL(MONTH(OrdHdr.Start),2,'0') + PADL(DAY(OrdHdr.Start),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Start),4),2)
lcSegLine = lcSegLine + lcOrdDate
*15-- Order Completion Date
lcOrdDate = PADL(MONTH(OrdHdr.Complete),2,'0') + PADL(DAY(OrdHdr.Complete),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Complete),4),2)
lcSegLine = lcSegLine + lcOrdDate
*16-- Filler
lcSegLine = lcSegLine + SPACE(2)
*-- Terms Information
lcFTermRate = '0000'
lcFTermDays = '000'
lcSTermDays = '000'
lcEOM       = SPACE(1)
lcExtraDays = '000'
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
IF lnDiscRate <> 0
  *-- First terms rate
  lcFTermRate = STRTRAN(STR(lnDiscRate,5,2) ,' ','0')
  lcFTermRate = SUBSTR(lcFTermRate,1,2) + SUBSTR(lcFTermRate,4,2)
ENDIF  
IF lnDiscDays <> 0
  *-- First Terms days
  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
  IF lnDaysDue <> 0
    IF .F.
      lcExtraDays = PADL(INT(lnDaysDue),3,'0')
    ELSE
      lcSTermDays = PADL(INT(lnDaysDue),3,'0')
    ENDIF       
  ENDIF    
ELSE
  IF lnDaysDue <> 0
    lcFTermDays =  PADL(INT(lnDaysDue),3,'0')
  ENDIF    
ENDIF    
lcEOM = IIF(lcTEOM='Y','E', lcEOM)

*17-- First Terms Rate
lcSegLine = lcSegLine + lcFTermRate
*18-- First Terms Days
lcSegLine = lcSegLine + lcFTermDays
*19-- Second Terms Rate
lcSegLine = lcSegLine + '0000'
*20-- Second Terms Days
lcSegLine = lcSegLine + lcSTermDays
*21-- End Of Month Term Ind.
lcSegLine = lcSegLine + lcEOM
*22-- Extra Days
lcSegLine = lcSegLine + lcExtraDays
*23-- As Of Date
lcSegLine = lcSegLine + SPACE(6)
*24-- Comments
lcSegLine = lcSegLine + SUBSTR(OrdHdr.Note1,1,26)
= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddOrdSub
*! Auth : Wael Aly MOhamed
*! Date : 12/30/1999
*!*************************************************************
*! Synopsis : Write the subtotal record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdSub
PRIVATE lcSegLine

lcSegLine = ''

*E500439,1 AMM start
*01-- Transmission Date
*lcSegLine = lcSegLine + lcTranDate
*02-- Factoring Company Number
*lcSegLine = lcSegLine + "4"
*03-- Client Number
*lcSegLine = lcSegLine + lcClientNo
*04-- Batch Number
*lcSegLine = lcSegLine + PADL(lnBatchNo,2,'0')
*05-- Record Type
*lcSegLine = lcSegLine + "S"
*06-- Total number of Name/Address records
*lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
*07-- Total number of Order records
*lcSegLine = lcSegLine + PADL(lnSOrdRec,6,'0')
*08-- Filler
*lcSegLine = lcSegLine + SPACE(6)
*09-- Total Order amount
*lcSegLine = lcSegLine + PADL(lnSOrdAmt,9,'0')
*10-- Filer
*lcSegLine = lcSegLine + SPACE(83)

*01-- Client Number
lcSegLine = lcSegLine + lcClientNo

*02-- Trade Style
lcSegLine = lcSegLine + '99'

*03-- Record Type
lcSegLine = lcSegLine + "S"

*04-- Customer Number
lcSegLine = lcSegLine + REPLICATE('9',15)

*05-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')

*06-- Total number of Order records
lcSegLine = lcSegLine + PADL(lnSOrdRec,6,'0')

*07-- Filler
lcSegLine = lcSegLine + SPACE(6)

*08-- Total Order amount
lcSegLine = lcSegLine + PADL(lnSOrdAmt,12,'0')

*09-- Filer
lcSegLine = lcSegLine + SPACE(118)
*E500439,1 AMM end

= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name : lfAddOrdTot
*! Auth : Wael Aly MOhamed
*! Date : 12/30/1999
*!*************************************************************
*! Synopsis : Write the transmission total record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : EBFACIN
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdTot
PRIVATE lcSegLine

lcSegLine = ''
*E500439,1 AMM start
*01-- Transmission Date
*lcSegLine = lcSegLine + lcTranDate
*02-- Factoring Company Number
*lcSegLine = lcSegLine + "4"
*03-- Client Number
*lcSegLine = lcSegLine + lcClientNo
*04-- Batch Number
*lcSegLine = lcSegLine + PADL(lnBatchNo,2,'0')
*05-- Record Type
*lcSegLine = lcSegLine + "T"
*06-- Total number of Name/Address records
*lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
*07-- Total number of Order records
*lcSegLine = lcSegLine + PADL(lnTOrdRec,6,'0')
*08-- Filler
*lcSegLine = lcSegLine + SPACE(6)
*09-- Total Order amount
*lcSegLine = lcSegLine + PADL(lnTOrdAmt,9,'0')
*10-- Client Security Code
*lcSegLine = lcSegLine + PADR(lcBatchId,7,' ')
*11-- Filer
*lcSegLine = lcSegLine + SPACE(76)

*03-- Client Number
*ahmed
*lcSegLine = lcSegLine + lcClientNo
lcSegLine = lcSegLine + '9999'
*ahmed end

*-- Trade style
lcSegLine = lcSegLine + '99'

*05-- Record Type
lcSegLine = lcSegLine + "T"

*04-- Customer Number
lcSegLine = lcSegLine + REPLICATE('9',15)

*06-- Total number of Name/Address records
lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')

*07-- Total number of Order records
lcSegLine = lcSegLine + PADL(lnTOrdRec,6,'0')

*08-- Filler
lcSegLine = lcSegLine + SPACE(6)

*09-- Total Order amount
*ahmed
*lcSegLine = lcSegLine + PADL(lnTOrdAmt,9,'0')
lcSegLine = lcSegLine + PADL(lnTOrdAmt,12,'0')
*ahmed end

*11-- Filer
lcSegLine = lcSegLine + SPACE(12)

*-- Transmission date
lcOrdDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +;
             RIGHT(STR(YEAR(DATE()),4),2)
lcSegLine = lcSegLine + lcOrdDate

*-- Filler
lcSegLine = lcSegLine + SPACE(100)
*E500439,1 AMM end


= FPUTS(lnOutFile,lcSegLine)

*!*************************************************************
*! Name      : lfvRange
*! Developer : Haytham El-Sheltawi
*! Date      : 8/23/2001
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

DO (gcScrDir + gcWinAppl + "\Ebarange.SPX")

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
*! Name : lfAddOrdRec
*! Auth : Ahmed Mohamed Ibrahim
*! Date : 09/09/2001
*! Ref  : E500439,1 AMM
*!*************************************************************
*! Synopsis : Write the order record in the output orders text file
*!*************************************************************
*! Called from : 
*!         Procedures : lfWrtOrder
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfAddOrdRec
PRIVATE lcSegLine, lcTmpDate, lcOrdDate, lcFTermRate, lcFTermDays, lcEOM

lcSegLine = ''

*01-- Client Number
lcSegLine = lcSegLine + lcClientNo

*03-- Trade Style
lcSegLine = lcSegLine +SPACE(1)
lcSegLine = lcSegLine +SPACE(1)
*04-- Record Type
lcSegLine = lcSegLine +"R"

*05-- Customer Number
*ahmed
*lcSegLine = lcSegLine + PADR(lcCustNum,15)
lcSegLine = lcSegLine + PADR(lcCustomer,15)
*AHMED END

*06-- Client Control Key
lcSegLine = lcSegLine + PADR(&lcTempTran..TranNum,22)

*07-- Order Amount
lcSegLine = lcSegLine + PADL(ROUND(OrdHdr.BookAmt,0),8,'0')

lnSOrdAmt = lnSOrdAmt + ROUND(OrdHdr.BookAmt,0)

*08-- Order Bill And Hold Indicator
lcSegLine = lcSegLine + SPACE(1)

*09-- Order Start Date
lcOrdDate = PADL(MONTH(OrdHdr.Start),2,'0') + PADL(DAY(OrdHdr.Start),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Start),4),2)
lcSegLine = lcSegLine + lcOrdDate

*-- Terms Information
lcFTermDays = SPACE(3)

=gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE')

IF lnDiscDays <> 0
  *-- First Terms days
  lcFTermDays = PADL(INT(lnDiscDays),3,'0')
ELSE
  IF lnDaysDue <> 0
    lcFTermDays = PADL(INT(lnDaysDue),3,'0')
  ENDIF    
ENDIF    

*10 -- First Terms Days
lcSegLine = lcSegLine + lcFTermDays

*11-- Order Completion Date
lcOrdDate = PADL(MONTH(OrdHdr.Complete),2,'0') + PADL(DAY(OrdHdr.Complete),2,'0') +;
             RIGHT(STR(YEAR(OrdHdr.Complete),4),2)
lcSegLine = lcSegLine + lcOrdDate

lcSegLine = lcSegLine + SPACE(102)
= FPUTS(lnOutFile,lcSegLine)


