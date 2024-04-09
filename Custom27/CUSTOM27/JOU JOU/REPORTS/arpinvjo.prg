*:***************************************************************************
*: Program file  : ARPINVJO
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR JOU JOU
*! Date          : 01/12/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVJO
*:***************************************************************************
*: This Report Program is due to C101742 ...
*:B803103,1 SHA 03/09/2000 Some changes done in the FRX
*:B803159,1 BWA 04/10/2000 Fix the bug of When printing any invoice, the "Printed" 
*:B803159,1                column in the screen and the field in INVHDR file that
*:B803159,1                shows the list of the invoices to be printed doesn't get updated.
*:B803159,1                the fix in the FRX
*:B803364,1 SHA 06/19/2000 Fixed the problem of not printing the shipped 
*:B803364,1 SHA            amount in the first line of the form footer (Done in FRX)
*:B803551,1 BWA 08/03/2000 Add the second user defined field (in the customer screen) 
*:B803551,1                to be printed next to the last line of the sold to address.[FRX]
*:***************************************************************************

PRIVATE lcAlias
lcAlias = ALIAS()

*-- lcTypeJO  && Variable that is hold the name of the message memo file
*-- lcTypeJO = 'ARPINVJO'
IF FILE(gcDataDir+lcTypeJO+'.MEM')
  RESTORE FROM gcDataDir+lcTypeJO+'.MEM' ADDITIVE
ENDIF

*-- Creat table hold in it the notepad we will print it.
CREATE TABLE (gcWorkDir+lcInvNote) (InvNote M(10))


*-- Open the Credit file to get from it Invoice TotCr. which is the Payment .

IF !USED(lcCredit)
  =gfOpenFile(gcDataDir+"Credit","Credit", 'SH', @lcCredit, .T.)
ENDIF  

*-- Open the Ordline file to get from it the OrdLine TotQty. which is the back order quantity.

IF !USED(lcOrdLine)  
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

IF !USED("NEWORD")
  SELECT 0
  USE (gcDataDir+"ORDLINE") AGAIN ALIAS NEWORD ORDER ORDLINE
ENDIF

SELECT InvLine
SET RELATION TO 'O'+Order+Store+Style+STR(lineno,6) INTO &lcOrdLine ADDITIVE

_PADVANCE = 'LINEFEED'

SELECT (lcAlias)
*!*************************************************************
*! Name        : lfInvCred
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 01/12/2000
*! Purpose     : To seek with the invoice in the credit.BDF
*!*************************************************************
*! Called from : ARPINVJO.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfInvCred()
*!*************************************************************

FUNCTION lfInvCred
PARAMETERS lcDumdy
PRIVATE lcAlias

lcAlias   = ALIAS()
lnMisc    = 0

SELECT (lcCredit)
SCAN FOR account+tran+DTOS(trandate) = ""
  IF LEFT(&lcCredit..Reference,6) = InvHdr.ORDER
    lnMisc = lnMisc + &lcCredit..Amount
  ENDIF
ENDSCAN 

SELECT (lcAlias)
RETURN lcDumdy

*!*************************************************************
*! Name      : lfvJoMsg
*! Developer : Bassem Rafaat (BWA)
*! Date      : 01/12/2000
*! Purpose   : Function to get Optional Message from the User
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvJoMsg()
*!*************************************************************
FUNCTION lfvJoMsg
PARAMETER lcReturn
PRIVATE laJOMsg

DECLARE laJOMsg[4,2]       && Array to hold the name and length of the variables to be used in the message screen

laJOMsg[1,1] = 'lcJOMes1'        && 1st.   line Variable
laJOMsg[2,1] = 'lcJOMes2'        && 2nd.   line Variable
laJOMsg[3,1] = 'lcJOMes3'        && 3rd.   line Variable
laJOMsg[4,1] = 'lcJOMes4'        && Forth. line Variable
laJOMsg[1,2] = 60                && Line length

IF FILE(gcDataDir+lcTypeJO+'.MEM')
  RESTORE FROM gcDataDir+lcTypeJO+'.MEM' ADDITIVE
ENDIF

lcReturn= gfOptMsg('laJoMsg','Optional message')    && Call Function to write message
   
SET MEMOWIDTH TO 75
SAVE TO gcDataDir+lcTypeJo+'.MEM' ALL LIKE lcJoMes*
    
RETURN lcReturn

*!*************************************************************
*! Name        : lfInvNote
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 01/12/2000
*! Purpose     : To seek with the invoice in the notepad.
*!*************************************************************
*! Called from : ARPINVJO.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfInvNote()
*!*************************************************************

FUNCTION lfInvNote
PARAMETERS lcDumdy
PRIVATE lcAlias , lnOldMemW
lcAlias = ALIAS()
STORE '' TO  lcInvNotJ1 , lcInvNotJ2 , lcInvNotJ3 , lcInvNotJ4


SELECT INVHDR
IF SEEK('C' + InvHdr.Invoice,'Notepad')
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 20
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)

  IF lcInvoice # INVHDR.invoice
    lnCountJo  = 0
  ENDIF

  IF EVAL(lcTmpDbt+'.cfile_num') = '2'
    SELECT(lcInvNote)
    APPEND BLANK
    FOR  lnI = lnCountJo TO lnMemLins
      REPLACE InvNote WITH ALLTRIM(MLINE(NOTEPAD.MNOTES, lnI )) + ' '  ADDITIVE
    ENDFOR
    lcInvNotJ1 = ALLTRIM(EVAL(lcInvNote+'.InvNote'))
  ELSE
    lnCountJo = lnCountJo + 1
    lcInvNotJ1 = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnCountJo))
    lnCountJo = lnCountJo + 1

    lcInvNotJ2 = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnCountJo))
    lnCountJo = lnCountJo + 1
  ENDIF

  IF !EOF('SPCK_LIN') 
    lcInvNotJ3 = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnCountJo))
    lnCountJo = lnCountJo + 1

    lcInvNotJ4 = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnCountJo))
    lnCountJo = lnCountJo + 1
  ENDIF  

  lcInvoice = INVHDR.invoice
  SET MEMOWIDTH TO lnOldMemW
ENDIF 


SELECT (lcAlias)
RETURN ALLTRIM(lcInvNotJ1)+ALLTRIM(lcInvNotJ2)+ALLTRIM(lcInvNotJ3)+ALLTRIM(lcInvNotJ4)

*!*************************************************************
*! Name        : lfCOUNT
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 01/12/2000
*! Purpose     : TO GET THE VARIABLE lnCountJo INIATIAL VALUE = 0
*!*************************************************************
*! Called from : ARPINVJO.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCOUNT()
*!*************************************************************

FUNCTION lfCOUNT
PARAMETERS lcDumdy

lnCountJo = 0

IF lcInvoice # INVHDR.invoice
  LCMDATE    = SUBSTR(DTOS(EVAL(lcOrdLine+'.START')),5,8)+SUBSTR(DTOS(EVAL(lcOrdLine+'.COMPLETE')),5,8)    
ENDIF
lcInvoice = INVHDR.invoice

RETURN lcDumdy

*!*************************************************************
*! Name      : lfSolShp
*! Developer : Bassem Rafaat (BWA)
*! Date      : 01/12/2000
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : ARPINVJO.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lnCusRecNo

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]

laSoldTo        = ''              && Array to hold the Sold To address
laShipTo        = ''              && Array to hold the Ship To address
lcDivLName      = ''              && Variable to hold the Division long name
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'


lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfAdrShift('laSoldTo')


IF ORDHDR.Alt_ShpTo

  lcShpTName  = ORDHDR.STName  
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5

ELSE 

  lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)  
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF 
=lfAdrShift('laShipTo')

lcTerms    = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
lcShipVia  = gfCodDes(INVHDR.ShipVia   , 'SHIPVIA')
= gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')

RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/12/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfGetDate
*! Developer : Sherif Ishak
*! Date      : 03/09/2000
*! Purpose   : Return the latest completion date. 
*!*************************************************************
*! Called from : ARPINV.PRG 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*:Refer to B803103,1 SHA 03/09/2000
*!*************************************************************
FUNCTION lfGetDate
PARAMETERS ldComp
PRIVATE ldDate

STORE {} TO ldComp
lcOldDbf = ALIAS()
SELECT INVLINE
lcOldRec = Invoice + STR(LineNo,6)
SCAN WHILE INVOICE + STR(LineNo,6) = INVHDR.INVOICE
  IF SEEK("O"+Order+STR(LineNo,6),"NEWORD") AND NEWORD.Complete > ldComp
    ldComp =  NEWORD.Complete
  ENDIF
ENDSCAN
SEEK lcOldRec
SELECT &lcOldDbf
RETURN ldComp


*******************************END OF PROGRAM**************************
