*!*************************************************************
*! Name      : EBFACID
*! Developer : Wael Ali Mohamed
*! Date      : 30/12/1999
*! Purpose   : Send Detail Invoice/Credit Memo and Sales Orders to CIT
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcTranType
*!                       'I' : Send Detail Invoice/Credit Memos
*!                       'O' : Send Sales Orders
*!*************************************************************
*!B804247,1 HS  09/01/2001 add select range of invoices and orders
*!E500439,1 AMM 09/10/2001 some modifications on layout.
*!B605078,1 Hassan 10/30/2001 add the ability to choose output file.g
*!C200350,1 Hassan 05/21/2002 Add the ability to select Login Factor.
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

lcWindTitl = IIF(lcTranType='I','Detail Invoices Transmission','Send Sales Orders Transmission')

=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH')
=gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDRA','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINE','SH')
=gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYLEUPC','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCKLINS','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')

*-- Initialize variables
lcFactor   = SPACE(6)
lcCustomer = SPACE(5)
lcCustName = SPACE(30)
lcCustFact = SPACE(10)
lnLastTran = 2

*C200350,1 Add the ability to select Login Factor Hassan 05/21/2002 [Begin]
lcLgFactor  = SPACE(6)
lclgBtchId  = ''
lclgPssWrd  = ''
*C200350,1 Add the ability to select Login Factor Hassan 05/21/2002 [End]

lcKeyBmp   = gcBmpHome + "ExtKey.BMP"
lcSelBmp   = gcBmpHome + "SEL.BMP"
lcProceed  = gcBmpHome + "proceed.bmp"
lcClose    = gcBmpHome + "Close2.bmp"

STORE '' TO lcClientID,lcClientNo,lcBatchId,lcPassWord
*C127027,1 fixed data of client No. AAH 04/26/2005 [Begin]
store '' TO lcClientCust 
*C127027,1 fixed data of client No. AAH 04/26/2005 [End]
STORE  0 TO lnAssignNo

IF lcTranType = 'O'
  =gfOpenFile(gcDataDir+'CitTrnLn',gcDataDir+'CitTrnLn','SH')
  *-- Create CIT Send Orders Temp. File
  lcTmpCit = gfTempName()
  CREATE TABLE (gcWorkDir+lcTmpCit) (cFacCode C(6),BatchNo N(2),dDate D,Account C(5),ORDER C(6))
ENDIF

*-- Create transaction temp. file
lcTempTran = gfTempName()
CREATE TABLE (gcWorkDir+lcTempTran) ;
  (TYPE C(1), Account C(5), TranNum C(6), cSelect C(1))
INDEX ON cSelect+TYPE+Account+TranNum TAG 'SELECT'
*B804247,1 [Begin]
INDEX ON TYPE + TranNum TAG RangeSelct
*B804247,1 [End]
INDEX ON TYPE+Account+TranNum TAG (lcTempTran)  ADDITIVE
SET RELATION TO 'M'+Account INTO CUSTOMER
IF lcTranType = 'I'
  SET RELATION TO Account+TranNum INTO INVHDR ADDITIVE
  SET RELATION TO Account+TranNum INTO RETHDR ADDITIVE
  *  SET RELATION TO TranNum INTO INVline ADDITIVE
ELSE
  SET RELATION TO Account+'O'+TranNum INTO ORDHDR ADDITIVE
ENDIF
*-- Number of selected transactions
STORE 0 TO lnNoTrans
*B605078 Hassan 10/29/2001 [Begin]
lcOutFile = gcDataDir + 'CIT'+PADL(MONTH(gdSysDate),2,'0')+PADL(DAY(gdSysDate),2,'0')+'.NEW'
IF FILE(gcDataDir+"MEMO.MEM")
  RESTORE FROM gcDataDir+"MEMO" ADDITIVE
ENDIF
*B605078 Hassan 10/29/2001 [End]

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
DO (gcScrDir+gcWinAppl+"\EBFACID.SPX") WITH 'I'

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
*!         Procedures : EBFACID
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
  WAIT 'Creating outbound Detail Invoice/Credit file...' WINDOW NOWAIT
  *-- Initialize total number of Name/Address records, Invoice records and
  *-- Credit records
  STORE 0 TO lnTCusRec, lnTInvRec, lnTRetRec
  ** Initiate Color Variables for the Styles
  lnMajorLen = LEN(gfItemMask("PM"))
  DECLARE laItemSeg[1]
  llColor = .F.
  STORE 0 TO lnClrLen,lnClrPos
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      llColor = .T.
      lnClrLen = LEN(laItemSeg[lnCount,3])
      lnClrPos = laItemSeg[lnCount,4]
      EXIT
    ENDIF
  ENDFOR
  **
  *-- Initialize total invoice amount, total credit amount and total net amount
  STORE 0 TO lnTInvAmt, lnTRetAmt, lnTTotAmt
  SELECT (lcTempTran)
  SET ORDER TO TAG SELECT
  *-- Add Security record
  IF llFTP = 0
    = lfAddSecRec()
  ENDIF
  *-- First write Invoice records
  IF SEEK('»I')
    *-- Initialize subtotal number of Name/Address records, Invoice records
    *-- and Credit records
    STORE 0 TO lnSCusRec, lnSInvRec, lnSRetRec ,lnSInvSum

    *-- Initialize subtotal invoice amount, subtotal credit amount and
    *-- subtotal net amount
    STORE 0 TO lnSInvAmt, lnSRetAmt, lnSTotAmt

    DO WHILE cSelect+TYPE+Account+TranNum = "»I"
      lcCustomer = Account
      IF SEEK('M'+lcCustomer,'Customer')
        IF !EMPTY(CUSTOMER.FactAcct)
          lcCustNum = PADL(ALLTRIM(SUBSTR(CUSTOMER.FactAcct,1,7)),7,'0')
        ELSE
          lcCustNum = "9999999"
        ENDIF
      ENDIF

      SCAN REST WHILE cSelect+TYPE+Account+TranNum = "»I"+lcCustomer
        *-- Add Name/Address Record Type "N"
        =lfAddCusRec()
        *-- Increament number of Name/Addres records
        lnSCusRec = lnSCusRec + 1

        STORE 0 TO lnSInvRec
        SELECT InvLine
        =SEEK(&lcTempTran..TranNum,'InvLine')
        SCAN REST WHILE INVOICE+STR(LINENO,6) = &lcTempTran..TranNum
          STORE 0 TO lnCount
          FOR lnCount=1 TO 8
            STORE '' TO lcUpc,lcSize,lcColor, lcSKU, lcClrDesc
            lcStr=PADL(lnCount,1,' ')
            IF Qty&lcStr. != 0
              IF SEEK(STYLE+lcStr,'StyleUpc')
                lcUpc = ALLTRIM(StyleUpc.cupcnum1 + StyleUpc.cupcnum2 + StyleUpc.cupcnum3)
              ENDIF
              lcColor = IIF(llColor,SUBSTR(STYLE,lnClrPos,lnClrLen),'')
              IF !EMPTY(lcColor)
                =SEEK('N' + PADR(lcColor,6) + 'N' + PADR('COLOR',10) , 'CODES')
                lcClrDesc = ALLTRIM(CODES.cDiscRep)
              ENDIF  
              IF SEEK('S'+SCALE,'SCALE')
                lcSize = SCALE.SZ&lcStr.
              ENDIF
              SELECT Spck_Lin 
              =SEEK("S"+InvLine.Account+InvLine.STYLE)
              LOCATE REST WHILE type+account+style+pack_id = "S"+InvLine.Account+InvLine.STYLE FOR Qty&lcStr. = 1
              lcSKU =IIF(FOUND(),SPCK_LIN.pack_id," ")
              SELECT InvLine
              *-- Increament number of invoice records
              lnSInvRec = lnSInvRec + 1
              *-- Add Invoice Line Item (detail)Record Type "I"
              =lfAddInvRec()
            ENDIF
          ENDFOR
        ENDSCAN
        SELECT (lcTempTran)
        *-- Increament number of invoice Summary records
         lnSInvSum = lnSInvSum + 1
        *-- Add Invoice Summary Record Type "D" or "C"
        =lfAddInvSum()  
        lnSInvAmt = lnSInvAmt + INVHDR.TotalChg       
        *lnSRetAmt = lnSRetAmt + INVHDR.TotalChg
      ENDSCAN
    ENDDO
    *-- Add Sub Total Record Type "S"
    *-- Increament total number of records and total amount
    lnTCusRec = lnTCusRec + lnSCusRec
    lnTInvRec = lnTInvRec + lnSInvSum
    lnTRetRec = lnTRetRec + lnSRetRec
    lnTInvAmt = lnTInvAmt + lnSInvAmt
    lnTRetAmt = lnTRetAmt + lnSRetAmt
    =lfAddSubRec()    
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
    SET RELATION TO Account+TranNum INTO RETHDR

    *-- Set relation to invoice header file
    SELECT RETHDR
    SET RELATION TO Account+INVOICE INTO INVHDR
    SELECT (lcTempTran)
    DO WHILE cSelect+TYPE+Account+TranNum = "»C"
      lcCustomer = Account
      *-- Get customer factor number
      IF SEEK('M'+lcCustomer,'Customer')
        IF !EMPTY(CUSTOMER.FactAcct)
          lcCustNum = PADL(ALLTRIM(SUBSTR(CUSTOMER.FactAcct,1,7)),7,'0')
        ELSE
          lcCustNum = "9999999"
        ENDIF
      ENDIF

      SCAN REST WHILE cSelect+TYPE+Account+TranNum = "»C"+lcCustomer
        *-- Add Name/Address Record Type "N"
        =lfAddCusRec()
        *-- Increament number of Name/Addres records
        lnSCusRec = lnSCusRec + 1

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
  lcSegLine = lcSegLine + SPACE(1) + 'ID=' + 'EP003F'

  *-- BATCH ID
  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [Begin]
  lcSegLine = lcSegLine + SPACE(1) + "BID='DI" + ;
    SUBSTR(IIF(EMPTY(lclgBtchId),lcBatchId,lclgBtchId),1,4)+"'"
  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [End]

  *-- Password
  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [Begin]
  lcSegLine = lcSegLine + SPACE(1) + "PASSWORD=" + SUBSTR(IIF(EMPTY(lclgPssWrd),lcPassWord,lclgPssWrd),1,4)
  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [End]

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddCusRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write a customer record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddCusRec
  PRIVATE lcSegLine

  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + lcClientNo
  *-- Trade Style
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "N"
  *-- Client Customer Number
  *C127027,1 fixed data of client No. AAH 04/26/2005 [Begin]
  lcSegLine = lcSegLine +lcClientCust 
  *C127027,1 fixed data of client No. AAH 04/26/2005 [End]
  *lcSegLine = lcSegLine +PADR(lcCustomer,15," ")
  *-- Invoice Number
  lcSegLine = lcSegLine + Padr(&lcTempTran..TranNum,8,' ')
  *-- Filler
  lcSegLine = lcSegLine + SPACE(7)
  *-- Customer "Bill To" Name
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.BtName,1,30),30)
  *-- Customer Address Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress12,1,30),30,' ')
  *-- Customer Address Line 2
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress22,1,30),30,' ')
  *-- Customer Address City
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress32,1,17),17,' ')
  *-- Customer Address State
  lcSegLine = lcSegLine + padr(SUBSTR(CUSTOMER.cAddress42,1,2),2,' ')
  *-- Zip Code
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress52,10)
  *-- Country
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress62,17)
  *- phone
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.PHONE1,1,15),15,' ')
  *- Fax
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Fax,1,15),15,' ')
  *- Duns
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Duns,1,9),9,' ')
  *- EMail
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.Usr_Dfnd1,1,40),40,' ')
  *-- Customer "Ship To" Name
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.StName,1,30),30,' ')
  *-- Customer Address Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress1,1,30),30,' ')
  *-- Customer Address Line 2
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress2,1,30),30,' ')
  *-- Customer Address City
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress3,17)
  *-- Customer Address State
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress4,2)
  *-- Zip Code
  lcSegLine = lcSegLine + LEFT(CUSTOMER.cAddress5,10)
  *-- Country
  lcSegLine = lcSegLine + PADR(SUBSTR(CUSTOMER.cAddress6,1,17),17,' ')

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddInvRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write an invoice record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddInvRec
  PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM

  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + lcClientNo
  *-- Trade Style
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "I"
  *-- Client Customer Number
  *lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
  lcSegLine = lcSegLine + lcClientCust
  *-- Invoice Number
  lcSegLine = lcSegLine + PADR(&lcTempTran..TranNum,8,' ')
  *--  Filler
  lcSegLine = lcSegLine + SPACE(7)
  *-- Sequential Line No.
  lcSegLine = lcSegLine + PADL(lnSInvRec,5,'0')
  *-- Quantity Invoiced
*  lcSegLine = lcSegLine + PADL(InvLine.TotQty,10,'0')
  lcSegLine = lcSegLine + PADL(InvLine.Qty&lcStr,10,'0')
  *-- Unit or Basis for Measurement Code
  lcSegLine = lcSegLine + 'EA'
  *-- Unit Price
  lcSegLine = lcSegLine + PADL(InvLine.Price,17,'0')
  *-- Basis of Unit Price Code
  lcSegLine = lcSegLine + "PE"
  *-- UPC Number
  lcSegLine = lcSegLine + PADR(lcUpc,20,' ')
  *-- Buyer's Catalog Number
  lcSegLine = lcSegLine + PADR(SUBSTR(lcSKU,1,20),20,' ')
  *-- Vendor Style Number
  lcSegLine = lcSegLine + PADR(SUBSTR(InvLine.STYLE,1,lnMajorLen),20,' ')
  *-- European Article Number
  lcSegLine = lcSegLine + IIF(LEN(lcUpc)=12,SPACE(20),PADR(lcUpc,20,' '))
  *-- Item Discription Line 1
  lcSegLine = lcSegLine + PADR(SUBSTR(InvLine.Desc1,1,30),30,' ')
  *-- Item Discription Line 2
  lcSegLine = lcSegLine + SPACE(30)
  *-- Color Description
  lcSegLine = lcSegLine + PADR(lcClrDesc,20,' ')
  *-- Size Description
  lcSegLine = lcSegLine + PADR(ALLTRIM(lcSize),20,' ')

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddRetRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write a credit record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddRetRec
  PRIVATE lcSegLine, lcTmpDate, lcRetDate, lcFTermRate, lcFTermDays, lcEOM

  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + lcClientNo
  *-- Trade Style
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "R"
  *-- Client Customer Number
  *lcSegLine = lcSegLine + PADR(lcCustomer,15)
  lcSegLine = lcSegLine + lcClientCust
  *-- Client Order Number
  lcSegLine = lcSegLine + PADL(ALLTRIM(RETHDR.ORDER),22,'0')
  *-- Credit Amount
  lcAmount= STRTRAN(STR(RETHDR.TotCredit,9,2) ,' ','0')
  lcAmount= STRTRAN(lcAmount,'.','')
  lcSegLine = lcSegLine + lcAmount
  *-- Bill & Hold Code ?
  lcSegLine = lcSegLine + SPACE(1)
  *-- Start Ship Date ?
  lcSegLine = lcSegLine + SPACE(6)
  *-- Filer
  lcSegLine = lcSegLine + SPACE(7)
  *-- Invoice Number
  lcSegLine = lcSegLine + PADL(RETHDR.INVOICE,7,'0')
  *-- Entry Code
  lcSegLine = lcSegLine + '22'
  *-- Risk Code
  lcSegLine = lcSegLine + SPACE(1)
  *-- Filer
  lcSegLine = lcSegLine + SPACE(2)
  *-- Store Number
  lcSegLine = lcSegLine + PADL(ALLTRIM(RETHDR.STORE),4,'0')
  *-- Credit Date
  lcRetDate = PADL(MONTH(RETHDR.CrDate),2,'0') + PADL(DAY(RETHDR.CrDate),2,'0') +;
    RIGHT(STR(YEAR(RETHDR.CrDate),4),2)

  lcSegLine = lcSegLine + lcRetDate
  *-- Filer
  lcSegLine = lcSegLine + '0'
  *-- Credit Amount
  lcAmount= STRTRAN(STR(RETHDR.TotCredit,9,2) ,' ','0')
  lcAmount= SUBSTR(lcAmount,1,6) + SUBSTR(lcAmount,8,2)
  lcSegLine = lcSegLine + lcAmount

  *- Increament Credit amount and total net amount
  lnSRetAmt = lnSRetAmt + RETHDR.TotCredit
  lnSTotAmt = lnSTotAmt + RETHDR.TotCredit

  *-- Cust Purchase Order#
  *ahmed
  *lcSegLine = lcSegLine + PADL(ALLTRIM(RetHdr.CustPo),15,'0')
  lcSegLine = lcSegLine + PADR(ALLTRIM(RETHDR.CustPo),15,' ')
  *ahmed end

  *-- Terms Information
  lcFTermDays = SPACE(3)
  lcSTermDays = SPACE(3)
  lcEOM       = SPACE(1)
  lcExtraDays = SPACE(3)

  =gfRltFld(INVHDR.cTermCode,@laTRltFld,'CTERMCODE')
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
*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!***************************************************************
  *! Name : lfAddInvSum
  *! Auth : Waleed Hamed
  *! Date : 01/13/2002
  *!*************************************************************
  *! Synopsis : Write an invoice summary in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddInvSum
  PRIVATE lcSegLine, lcTmpDate, lcInvDate, lcFTermRate, lcFTermDays, lcEOM

  lcSegLine = ''
  *-- Client Number 1 - 4
  lcSegLine = lcSegLine + lcClientNo
  *-- Trade Style 5 - 5
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type 6 - 6
  lcSegLine = lcSegLine + IIF(lcTranType='I','D','C')
  *-- Client Customer Number 7 - 21
  *C127027,1 fixed data of client No. AAH 04/26/2005 [Begin]
  *lcSegLine = lcSegLine + PADR(lcCustomer,15," ")
  lcSegLine = lcSegLine + lcClientCust
  *C127027,1 fixed data of client No. AAH 04/26/2005 [End]
  *-- Invoice Number 22 - 29
  lcSegLine = lcSegLine + PADR(LEFT(&lcTempTran..TranNum,8),8,' ')
  *-- Filler 30 - 36
  lcSegLine = lcSegLine + SPACE(7)
  *-- Invoice Item Count 37 - 41
  lcSegLine = lcSegLine + PADL(lnSInvRec,5,'0')
  *-- Invoice Amount 42 - 51
  lcSegLine = lcSegLine + STRTRAN(STR(INVHDR.TotalChg*100,10),' ','0')
  *-- Invoice Date 52 - 57
  lcInvDate = PADL(MONTH(INVHDR.InvDate),2,'0')+PADL(DAY(INVHDR.InvDate),2,'0')+RIGHT(STR(YEAR(INVHDR.InvDate),4),2)
  lcSegLine = lcSegLine + lcInvDate
  *-- As Of Date 58 - 63
  lcSegLine = lcSegLine + SPACE(6)
  *-- Date Shipped  64 - 69
  lcDateShp = PADL(MONTH(INVHDR.ShipDate),2,'0')+PADL(DAY(INVHDR.ShipDate),2,'0')+RIGHT(STR(YEAR(INVHDR.ShipDate),4),2)
  lcSegLine = lcSegLine + lcDateShp
  *-- Reserved 70 - 75
  lcSegLine = lcSegLine + SPACE(6)
  IF lcTranType='I'
    *-- Client Terms Code 76 - 78
    lcSegLine = lcSegLine + PADR(RIGHT(ALLTRIM(INVHDR.cTermCode) , 3) , 3 , " ") 
    *-- Terms Information 79 - 108
    =SEEK('N' + INVHDR.cTermCode + 'N' + 'CTERMCODE' , 'CODES')
    lcSegLine = lcSegLine + PADR(ALLTRIM(CODES.cDiscRep),30 , ' ' )
  ELSE
    *-- Client Terms Code 76 - 78
    lcSegLine = lcSegLine + '000'
    *-- Terms Information 79 - 108
    lcSegLine = lcSegLine + SPACE(30)
  ENDIF
  
  *-- Merchandise Amount 109 - 118
  lcSegLine = lcSegLine + PADL(INT(INVHDR.ShipAmt*100),10,'0')

  *-- Store Number 119 - 123
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.STORE) , 5  , ' ')
  *-- Cust Purchase Order# 124 - 145
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.CustPo), 22 , ' ')
  *-- Cust Purchase Order Date 146 - 151
  =SEEK(lcCustomer+'O'+InvHdr.Order,'ORDHDR')
  lcEnter = PADL(MONTH(OrdHdr.Entered),2,'0')+PADL(DAY(OrdHdr.Entered),2,'0')+RIGHT(STR(YEAR(OrdHdr.Entered),4),2)
  lcSegLine = lcSegLine + lcEnter
  *-- Cust department# 152 - 157
  lcSegLine = lcSegLine + PADR(ALLTRIM(INVHDR.Dept), 6 , ' ')
  *-- Risk Code 158 - 158
  lcSegLine = lcSegLine + SPACE(1)
  *-- Discount Type Code 159 - 159
  lcSegLine = lcSegLine + IIF(ABS(INVHDR.DISCOUNT)>0,'1',SPACE(1))
  *-- Discount Amount 160 - 169
  lcSegLine = lcSegLine + PADL(INT(ABS(INVHDR.DISCOUNT)*100),10,'0')
  *-- Credit Memo Invoice # 170 - 177
  lcSegLine = lcSegLine + IIF(lcTranType='I',SPACE(8),LEFT(&lcTempTran..TranNum,8))
  *-- Filler 178 - 184
  lcSegLine = lcSegLine + SPACE(7)
  *-- Freight Amount 185 - 194
  lcSegLine = lcSegLine + PADL(INT(INVHDR.FREIGHT*100),10,'0')
  *-- Sales Tax Amount 195 - 204
  lcSegLine = lcSegLine + PADL(INT((INVHDR.Tax_Amt+INVHDR.nPstAmt)*100),10,'0')
  *-- Other Charge Amount 205 - 214
  lcSegLine = lcSegLine + PADL(INT((INVHDR.COD+INVHDR.INSUR+INVHDR.nCharges)*100),10,'0') 
  *-- Allowance Amount 215 - 224
  lcSegLine = lcSegLine + SPACE(10)
  *-- Vendor ID 225 - 239
  lcSegLine = lcSegLine + PADR(ALLTRIM(CUSTOMER.ccusvend), 15 , ' ')
  *-- Freight Carrier 240 - 269
  =SEEK('N'+INVHDR.ShipVia+'N'+'SHIPVIA','CODES')
  lcSegLine = lcSegLine + PADR(ALLTRIM(CODES.cDiscRep),30,' ')
  *-- Shipment Payment Code 270 - 271
  lcSegLine = lcSegLine + "PP"
  *-- Number of Cartons 272 - 277
  lcSegLine = lcSegLine + PADL(INVHDR.cartons,6,'0')

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

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
  *-- Increament the assignment number
  lnAssignNo = lnAssignNo + 1
  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + lcClientNo
  *-- Filler
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "S"
  *-- Client Customer Number
  *lcSegLine = lcSegLine + PADR("",15,"9")
  *-- Total number of Name/Address records
  lcSegLine = lcSegLine + PADL(lnSCusRec,6,'0')
  *-- Total number of Invoice records
  lcSegLine = lcSegLine + PADL(lnSInvSum,6,'0')
  *-- Total number of Credit records
  lcSegLine = lcSegLine + PADL(lnSRetRec,6,'0')
  *-- Total invoice amount
  lcSegLine = lcSegLine + PADL(INT(lnSInvAmt*100),12,'0')
  *-- Total credits amount
  lcSegLine = lcSegLine + PADL(INT(lnSRetAmt*100),12,'0') 
  *-- Assignment number
  lcSegLine = lcSegLine + PADL(ALLTRIM(STR(lnAssignNo)),4,'0')
  *-- Assignment Date
  lcAssignDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') + RIGHT(STR(YEAR(DATE()),4),2)
  lcSegLine = lcSegLine + lcAssignDate
  *-- Factoring Free Code
  lcSegLine = lcSegLine + "0"
*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddTrnRec
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/99
  *!*************************************************************
  *! Synopsis : Write the transmission total record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddTrnRec
  PRIVATE lcSegLine, lcInvAmt, lcRetAmt, lcTotAmt

  lcSegLine = ''
  *-- Client Number
  lcSegLine = lcSegLine + "9999"
  *-- Filler
  lcSegLine = lcSegLine + SPACE(1)
  *-- Record Type
  lcSegLine = lcSegLine + "T"
  *-- Total number of Name/Address records
  lcSegLine = lcSegLine + PADL(lnTCusRec,6,'0')
  *-- Total number of Invoice records
  lcSegLine = lcSegLine + PADL(lnTInvRec,6,'0')
  *-- Total number of Credit records
  lcSegLine = lcSegLine + PADL(lnTRetRec,6,'0')
  *-- Total (invoice/Credit Order) amount
  lcSegLine = lcSegLine + PADL(INT(lnTInvAmt*100),12,'0') 
  *-- Total Credits Amount
  lcSegLine = lcSegLine + PADL(INT(lnTRetAmt*100),12,'0')  
  *-- Transmission Date
  lcTransDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +RIGHT(STR(YEAR(DATE()),4),2)
  lcSegLine = lcSegLine + lcTransDate
*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

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
  =lfRefresh('EBFACID')
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

  lcCustFact = IIF(EMPTY(CUSTOMER.FactAcct),'**NEW**',CUSTOMER.FactAcct)
  lcCustName = CUSTOMER.BtName
  =lfRefresh('EBFACID')
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
    DELETE REST WHILE TYPE = 'I'
    SELECT INVHDR
    =SEEK(ALLTRIM(lcCustomer))
    SCAN REST WHILE Account+INVOICE = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
      INSERT INTO (lcTempTran) (TYPE,Account,TranNum) VALUES ('I',INVHDR.Account,INVHDR.INVOICE)
    ENDSCAN
    llInvoice = .F.
  ENDIF
  IF llCrMemo AND lcTranType = 'I'
    SELECT (lcTempTran)

    *B804247,1 HS The type for credit memo is "C" not "R" [Begin]
    *=SEEK('R')
    *DELETE REST WHILE Type = 'R'
    =SEEK('C')
    DELETE REST WHILE TYPE = 'C'
    *B804247,1 HS The type for credit memo is "C" not "R" [End]

    SELECT RETHDR
    =SEEK(ALLTRIM(lcCustomer))
    SCAN REST WHILE Account+CrMemo = ALLTRIM(lcCustomer) FOR cFacCode = lcFactor AND STATUS <>'V'
      INSERT INTO (lcTempTran) (TYPE,Account,TranNum) VALUES ('C',RETHDR.Account,RETHDR.CrMemo)
    ENDSCAN
    llCrMemo = .F.
  ENDIF
  IF llOrders AND lcTranType = 'O'
    SELECT (lcTempTran)
    =SEEK('O')
    DELETE REST WHILE TYPE = 'O'
    SELECT ORDHDR
    IF EMPTY(lcCustomer)
      SET ORDER TO TAG ORDHDR
      =SEEK('O')
      SCAN REST WHILE cOrdType+ORDER = 'O' FOR cFacCode = lcFactor AND STATUS <>'X'
        INSERT INTO (lcTempTran) (TYPE,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.ORDER)
      ENDSCAN
    ELSE
      =SEEK(lcCustomer+"O")
      SCAN REST WHILE Account+cOrdType+ORDER = lcCustomer+"O" FOR cFacCode = lcFactor AND STATUS <>'X'
        INSERT INTO (lcTempTran) (TYPE,Account,TranNum) VALUES ('O',ORDHDR.Account,ORDHDR.ORDER)
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

  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [Begin]
  = SEEK(lcFactor,'SycFact')
  *C200350 ,1 Add the ability to select Login Factor Hassan 05/21/2002 [End]
  *-- Client Information
  lcClientID = SycFact.cClientID
  *C127027,1 fixed data of client No. AAH 04/26/2005 [Begin]
  *lcClientNo = SycFact.cClientNo
  lcClientNo  = '1723'
  lcClientCust = PADR(IIF(INLIST(gcAct_comp , '04', '08'), '406X',IIF(INLIST(gcAct_comp , '01', '05'),"1724","1723")),15," ")
  *C127027,1 fixed data of client No. AAH 04/26/2005 [Begin]
  lcBatchId  = 'D' +IIF(lcTranType='I','I','O') +lcClientNo
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

  =IIF(lcTranType='I',lfWrtInv(),lfWrtOrder())
  *-- Increament the Assignment number in customer file
  SELECT SycFact
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
  lcKey = TYPE+Account+TranNum
  DO CASE
    CASE lcType = 'S'
      REPLACE cSelect WITH IIF(cSelect='»',' ','»')
    CASE lcType = 'A'
      =SEEK(lcTranMode)
      REPLACE REST cSelect WITH '»' WHILE TYPE+Account+TranNum = lcTranMode
    CASE lcType = 'N'
      =SEEK(lcTranMode)
      REPLACE REST cSelect WITH ' ' WHILE TYPE+Account+TranNum = lcTranMode
    CASE lcType = 'V'
      =SEEK(lcTranMode)
      REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE TYPE+Account+TranNum = lcTranMode
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
  ACTIVATE WINDOW (lcWindName)
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
  ACTIVATE WINDOW (lcWindName)
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
  DO WHILE cSelect+TYPE+Account+TranNum = "»O"
    lcCustomer = Account
    *-- Get customer factor number
    IF SEEK('M'+lcCustomer,'Customer')
      IF EMPTY(CUSTOMER.FactAcct)
        lnNewCust = lnNewCust + 1
        lcCustNum = "999" + PADL(lnNewCust,4,'0')
      ELSE
        lcCustNum = PADL(ALLTRIM(SUBSTR(CUSTOMER.FactAcct,1,7)),7,'0')
      ENDIF
    ENDIF
    *-- Add Name/Address Record Type "A"
    =lfAddCusRec()
    *-- Increament number of Name/Addres records
    lnSCusRec = lnSCusRec + 1

    *-- Add Order Record for this customer Type "D"
    SCAN WHILE cSelect+TYPE+Account+TranNum = "»O"+lcCustomer
      =lfAddOrdRec()

      *-- Update the CitTrnLn Temp. File
      INSERT INTO (lcTmpCit) (cFacCode,BatchNo,dDate,Account,ORDER) VALUES ;
        (lcFactor,lnBatchNo,gdSysDate,lcCustomer,ORDHDR.ORDER)

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
  lcSegLine = lcSegLine + PADL(ROUND(ORDHDR.BookAmt,0),7,'0')
  lnSOrdAmt = lnSOrdAmt + ROUND(ORDHDR.BookAmt,0)
  *12-- Filler
  lcSegLine = lcSegLine + SPACE(1)
  *13-- Order Bill And Hold Indicator
  lcSegLine = lcSegLine + SPACE(1)
  *14-- Order Start Date
  lcOrdDate = PADL(MONTH(ORDHDR.START),2,'0') + PADL(DAY(ORDHDR.START),2,'0') +;
    RIGHT(STR(YEAR(ORDHDR.START),4),2)
  lcSegLine = lcSegLine + lcOrdDate
  *15-- Order Completion Date
  lcOrdDate = PADL(MONTH(ORDHDR.COMPLETE),2,'0') + PADL(DAY(ORDHDR.COMPLETE),2,'0') +;
    RIGHT(STR(YEAR(ORDHDR.COMPLETE),4),2)
  lcSegLine = lcSegLine + lcOrdDate
  *16-- Filler
  lcSegLine = lcSegLine + SPACE(2)
  *-- Terms Information
  lcFTermRate = '0000'
  lcFTermDays = '000'
  lcSTermDays = '000'
  lcEOM       = SPACE(1)
  lcExtraDays = '000'
  =gfRltFld(ORDHDR.cTermCode,@laTRltFld,'CTERMCODE')
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
  lcSegLine = lcSegLine + SUBSTR(ORDHDR.Note1,1,26)
*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

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
  lcSegLine = lcSegLine + SPACE(1)

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

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfAddOrdTot
  *! Auth : Wael Aly MOhamed
  *! Date : 12/30/1999
  *!*************************************************************
  *! Synopsis : Write the transmission total record in the output orders text file
  *!*************************************************************
  *! Called from :
  *!         Procedures : EBFACID
  *!*************************************************************
  *! Modifications : None.
  *!*************************************************************
FUNCTION lfAddOrdTot
  PRIVATE lcSegLine

  lcSegLine = ''
  *-- Format Type
  lcSegLine = lcSegLine + "CF"
  *-- Group Client Number
  lcSegLine = lcSegLine + SPACE(7)
  *-- Client Number
  lcSegLine = lcSegLine + PADR(lcClientNo,4," ")
  *-- Record Type
  lcSegLine = lcSegLine + "80"
  *-- Customer Number
  lcSegLine = lcSegLine + PADR(lcCustNum,7," ")
  *06-- Total number of Order records
  lcSegLine = lcSegLine + PADL(lnSOrdRec,6,'0')
  *08-- Total Order amount
  lcSegLine = lcSegLine + PADL(lnSOrdAmt,12,'0')
  *-- Future Use
  lcSegLine = lcSegLine + SPACE(7)
  *03-- Client Number
  lcSegLine = lcSegLine + '9999'
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
  lcSegLine = lcSegLine + PADL(lnTOrdAmt,12,'0')
  *11-- Filer
  lcSegLine = lcSegLine + SPACE(12)
  *-- Transmission date
  lcOrdDate = PADL(MONTH(DATE()),2,'0') + PADL(DAY(DATE()),2,'0') +;
    RIGHT(STR(YEAR(DATE()),4),2)
  lcSegLine = lcSegLine + lcOrdDate
  *-- Filler
  lcSegLine = lcSegLine + SPACE(100)

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

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
    lcKey      = TYPE + TranNum

    SET ORDER TO RangeSelct
    SET NEAR ON

    SEEK lcTranMode + lcRangeFr
    DO CASE
      CASE lcScope = 'A'
        REPLACE REST cSelect WITH '»' WHILE TYPE + TranNum <= lcTranMode + lcRangeTo
      CASE lcScope = 'N'
        REPLACE REST cSelect WITH ' ' WHILE TYPE + TranNum <= lcTranMode + lcRangeTo
      CASE lcScope = 'I'
        REPLACE REST cSelect WITH IIF(cSelect='»',' ','»') WHILE TYPE + TranNum <= lcTranMode + lcRangeTo
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

  *-- Customer Number
  lcSegLine = lcSegLine + PADR(lcCusttomer,15," ")
  *-- Customer Name
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.StName,1,30)
  *-- Customer Address Line 1
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.cAddress1,1,30)
  *-- Customer Address Line 2
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.cAddress2,1,30)
  *-- Customer Address City
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.cAddress3,1,17)
  *-- Customer Address State
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.cAddress4,1,2)
  *-- Zip Code
  lcSegLine = lcSegLine + SUBSTR(CUSTOMER.cAddress5,1,5)
  *-- Order#
  lcSegLine = lcSegLine + PADL(ALLTRIM(ORDHDR.ORDER),22,' ')
  *-- Order Amount
  lcSegLine = lcSegLine + ROUND(ORDHDR.BookAmt,0)
  *-- Order Start Date
  lcOrdDate = PADL(MONTH(ORDHDR.START),2,'0') + PADL(DAY(ORDHDR.START),2,'0') +;
    RIGHT(STR(YEAR(ORDHDR.START),4),2)
  lcSegLine = lcSegLine + lcOrdDate
  *-- Order Completion Date
  lcOrdDate = PADL(MONTH(ORDHDR.COMPLETE),2,'0') + PADL(DAY(ORDHDR.COMPLETE),2,'0') +;
    RIGHT(STR(YEAR(ORDHDR.COMPLETE),4),2)
  lcSegLine = lcSegLine + lcOrdDate
  *-- Client Terms Code
  lcSegLine = lcSegLine + PADR(RIGHT(ALLTRIM(INVHDR.cTermCode) , 3) , 3 , " ")
  *-- Terms Information
  =SEEK('N' + INVHDR.cTermCode + 'N' + 'CTERMCODE' , 'CODES')
  lcSegLine = lcSegLine + PADR(ALLTRIM(CODES.cDiscRep),30 , ' ' )

*  = FPUTS(lnOutFile,lcSegLine)
  = FWRITE(lnOutFile,lcSegLine)
  = FPUTS(lnOutFile,'')

  *!*************************************************************
  *! Name : lfGetFile
  *! Auth : Hassan ALi
  *! Date : 10/29/2001
  *! Ref  : B605078,1
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

  ltOutFile = PUTFILE('',lcOutFile)

  IF EMPTY(ltOutFile) = .F.
    lcOutFile = ltOutFile
    lcTalk = SET('TALK')
    SET TALK OFF
    SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
    SET TALK &lcTalk.
    =lfRefresh('EBFACID')
  ENDIF
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
  =lfRefresh('EBFACID')
  SELECT (lnAlias)
  llBrowse = .F.
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
  =lfRefresh('EBFACID')
  SELECT (lnAlias)
  llBrowse = .F.
  *!*************************************************************
  *! Name      : lfvFactor
  *! Developer : Hassan Ali
  *! Date      : 05/21/2002
  *! Purpose   : Validate Login Factor
  *!*************************************************************
  *! Calls     : ARIABROW,lfRefresh
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  *! Example   :  =lfvLgFctr()
  *!*************************************************************
FUNCTION lfvLgFctr
  PRIVATE lnAlias

  lnAlias = SELECT()
  IF llBrowse OR (!EMPTY(lcLgFactor) .AND. !SEEK(lcLgFactor,'SycFact'))
    lcBrFields = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
    SELECT SycFact
    lcLgFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,'','',;
      'cFacCode','laBrowArr'),SycFact.cFacCode,SPACE(6))

    *-- Client Information
    lclgBtchId  = 'D' +IIF(lcTranType='I','I','O') +SycFact.cClientNo
    lclgPssWrd  = SycFact.cPassWord

  ENDIF
  = SEEK(lcFactor,'SycFact')

  =lfRefresh('EBFACID')
  SELECT (lnAlias)
  llBrowse = .F.
  *!*************************************************************
