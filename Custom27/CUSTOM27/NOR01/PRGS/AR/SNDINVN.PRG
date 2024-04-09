*!***************************************************************************
*! Program file  : SNDINVNO.PRG
*! Program desc. : Send Invoices to Miberg for North Bay (NOR01)
*! Date          : 12/13/2004
*! System        : Aria Advantage Series.
*! Module        : Accounts Receivable (AR)
*! Developer     : NADER NABIL (NNA)
*! Tracking Job Number: C124052
*!***************************************************************************
*! Calls :
*!    Procedures : ....
*!***************************************************************************
*! Passed Parameters  : None
*!***************************************************************************
*! Notes   : ....
*!***************************************************************************
*! Example : DO sndinvno
*!***************************************************************************
*! Modifications :
*!***************************************************************************
PRIVATE lcExpr , lcTmpFile , lcOutFile,lnOutFile
STORE '' TO lcExpr , lcTmpFile , lcOutFile
STORE 0 TO lnOutFile

IF !USED('INVHDR')
  = gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDRA','SH') 
ENDIF
IF !USED('CUSTOMER')
  = gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH') 
ENDIF
IF !USED('CODES')
  = gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH') 
ENDIF
SELECT INVHDR
LOCATE FOR !EMPTY(cFacCode)
IF !FOUND()
  =gfModalGen("INM00000B00000","DIALOG",'','','There is no factored invoice')
  RETURN
ENDIF
lcEscapeKy = ON("KEY","ESC")
lcExpr     = gfOpGrid('SNDINVN' , .T.)
IF EMPTY(LCEXPR)
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ENDIF
IF EMPTY(lcOutFile)
  =gfModalGen("INM00000B00000","DIALOG",'','','There is no Output file selected')
  RETURN
ENDIF
ON KEY LABEL ESCAPE &lcEscapeKy
lcTmpFile  = gfTempName()

=lfCreatTmp()
=lfColData()
IF RECCOUNT(lcTmpFile) > 0
  =lfTextRec()
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
  =FFLUSH(lcOutFile)
  =FCLOSE(lnOutFile)
  RETURN
ENDIF
=FFLUSH(lcOutFile)
=FCLOSE(lnOutFile)

*E300817,1 Message : 00370
*E300817,1 Output file has been created
*E300817,1 Button : 00000
*E300817,1 Ok
=gfModalGen('TRM00000B00000','ALERT','','','Output file '+lcOutFile+' has been created.')

*!***************************************************************************
*! Name      : lfCreatTmp
*! Developer : NADER NABIL (NNA)
*! Date      : 12/13/2004
*! Purpose   : Create the Temp files that will use to print the Text File
*!***************************************************************************
*! Passed Parameters  : ............
*!***************************************************************************
*! Calls              : from the Prg.
*!***************************************************************************
*! Example   : =lfCreatTmp()
*!***************************************************************************
FUNCTION lfCreatTmp
CREATE TABLE (gcWorkDir+lcTmpFile) (Account C(5) , Name C(30) , Address1 C(30) ,;
              Address2 C(30) , City C(17) , State C(2) , Zip C(5) , Store C(8) ,; 
              Invoice C(6) , InvDate D(8) , InvAmt N(10,2) , CTerms C(6) , ;
              TermDesc C(30) , CustPo C(15))
INDEX ON Invoice TAG (lcTmpFile)  OF (lcTmpFile)

*--End of Function lfCreatTmp.
*!*************************************************************
*! Name      : lfColData
*! Developer : NADER NABIL (NNA)
*! Date      : 12/13/2004
*! Purpose   : Collecting Data.
*!*************************************************************
*! Calls     : SNDINVNO.PRG
*!*************************************************************
*! Example   :  =lfColData()
*!*************************************************************
FUNCTION lfColData
*-- Check if the output file allready exists.
IF FILE(lcOutFile)
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output file

lnOutFile = FCREATE(lcOutFile)
IF lnOutFile < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
=lfWriteInv()

*--End of Function lfColData.
*!*************************************************************
*! Name      : lfWriteInv
*! Developer : NADER NABIL (NNA)
*! Date      : 12/13/2004
*! Purpose   : Write an output Invoices text file to be send to Rosenthal.
*!*************************************************************
*! Called from : SNDINVNO.PRG
*!*************************************************************
*! Calls : FUNCTIONS : lfAddInvRec
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfWriteInv
PRIVATE lcCustomer , lcTermDesc
STORE '' TO lcCustomer , lcTermDesc
WAIT 'Creating outbound Invoice file...' WINDOW NOWAIT
*-- First write Invoice records
SELECT INVHDR
SET RELATION TO
SET RELATION TO 'M' + INVHDR.ACCOUNT INTO CUSTOMER ADDITIVE
SCAN FOR &lcExpr AND !EMPTY(cFacCode)
  lcTermDesc = IIF(SEEK('N' + INVHDR.CTermCode + 'N' + 'CTERMCODE ','CODES'),CODES.Cdiscrep,'')
  lcCustomer = Account
  IF SEEK('M'+lcCustomer,'Customer') 
    SELECT (lcTmpFile)
    IF !SEEK(INVHDR.Invoice)
      APPEND BLANK
      REPLACE Account  WITH INVHDR.Account				 ,;
              Name     WITH CUSTOMER.bTname         	 ,;
              Address1 WITH CUSTOMER.cAddress12			 ,;
              Address2 WITH CUSTOMER.cAddress22			 ,;
              City     WITH CUSTOMER.cAddress32			 ,;
              State    WITH CUSTOMER.cAddress42			 ,;
              Zip      WITH CUSTOMER.cAddress52			 ,;
              Store    WITH INVHDR.STORE            	 ,;
              Invoice  WITH INVHDR.INVOICE          	 ,;
              InvDate  WITH INVHDR.INVDATE          	 ,;
			  InvAmt   WITH INVHDR.TOTALCHG              ,;
              CTerms   WITH INVHDR.CTermCode             ,;
              TermDesc WITH ALLTRIM(lcTermDesc)          ,;
              CustPo   WITH INVHDR.CUSTPO
    ENDIF
  ENDIF
ENDSCAN
*--End of Function lfWriteInv.
*!*************************************************************
*! Name      : lfTextRec
*! Developer : NADER NABIL (NNA)
*! Date      : 12/13/2004
*! Purpose   : Write an invoice records in the output orders text file
*!*************************************************************
*! Called from : SNDINVNO
*!*************************************************************
*! Modifications : None.
*!*************************************************************
FUNCTION lfTextRec
PRIVATE lcSegLine, lcTmpDate, lcInvDate , lnInvAmt , lcEndTxt
STORE 0 TO lnInvAmt
SELECT (lcTmpFile)
SCAN
  lnInvAmt = lnInvAmt + INVAMT
  lcSegLine = ''
  *-- Customer Number (Bill To customer number)
  lcSegLine = lcSegLine + PADR(ALLTRIM(Account),15,' ')
  *lcSegLine = lcSegLine + Account
  *-- Customer Name (Bill To customer  name)
  lcSegLine = lcSegLine + Name
  *-- Customer Address12 (Bill To address 1)
  lcSegLine = lcSegLine +Address1
  *-- Customer Address22 (Bill To address 2)
  lcSegLine = lcSegLine +Address2
  *-- Customer Address32 (Bill To city)
  lcSegLine = lcSegLine +City
  *-- Customer Address42 (Bill To state)
  lcSegLine = lcSegLine +State
  *-- Customer Address52  (Bill To zip code)
  lcSegLine = lcSegLine +ALLTRIM(Zip)
  *-- Store Number (Bill To store number)
  lcSegLine = lcSegLine +PADL(ALLTRIM(Store),8,' ')
  *-- Invoice Number	
  lcSegLine = lcSegLine +Invoice
  *-- Invoice Date
  lcInvDate = PADL(MONTH(InvDate),2,'0') + PADL(DAY(InvDate),2,'0') + RIGHT(STR(YEAR(InvDate),4),2)
  lcSegLine = lcSegLine +lcInvDate
  *-- Invoice Amount
  lcSegLine = lcSegLine +PADL(ALLTRIM(STR(InvAmt,8,2)),10,' ')
  *--Invoice Terms Code
  lcSegLine = lcSegLine +' '+ RIGHT(CTerms,3)
  *--Invoice Terms Description
  lcSegLine = lcSegLine +TermDesc
  *--Customer PO No.
  lcSegLine = lcSegLine +CustPo
  = FPUTS(lnOutFile,lcSegLine)
ENDSCAN
lcEndTxt = 'Total Number of Invoices = ' + ALLTRIM(STR(RECCOUNT(lcTmpFile))) +;
           SPACE (10) + 'Total Amount of Invoices = ' + ALLTRIM(STR(lnInvAmt,10,2))
= FPUTS(lnOutFile,'')
= FPUTS(lnOutFile,lcEndTxt)
*--End of Function lfTextRec.
*!*************************************************************
*! Name : lfGetFile
*! Auth : Hassan Ibrahim ALi
*! Date : 10/29/2001
*! Ref  : C102494,1
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
lcOutFile = ""

ltOutFile = PUTFILE('Select the output file' , lcOutFile , "TXT" )

IF !EMPTY(ltOutFile)
  lcOutFile = ltOutFile 
  lcTalk = SET('TALK')
  SET TALK OFF
  SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile
  SET TALK &lcTalk.
ENDIF  

