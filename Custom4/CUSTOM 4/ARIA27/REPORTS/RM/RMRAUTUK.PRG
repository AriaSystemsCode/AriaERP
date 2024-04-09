*:***************************************************************************
*: Program file  : RMRAUTUK.PRG
*: Program desc. : Customized Return Authorization Form For UK.
*: Date          : 11/09/2005
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Nader Nabil Abd Elmonam (NNA)
*: Tracking Job Number : E130126
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Example : DO RMRAUTUK
*:**************************************************************************
*:Modification:
*:**************************************************************************

*-- lfHeader : this function called from RMRAUTUK.frx to display the Bill to address (As Standard Forms)
*--            or ship to address as UK's Customers want.
*!***************************************************************************
*!* Name        : lfNHeader
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 11/09/2005
*!* Purpose     : Function to fill the approparate data for report header.
*!***************************************************************************
*!* Called from : RMRAUTUK.FRX [Header Band]
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfNHeader()
*!***************************************************************************
FUNCTION lfNHeader
PARAMETER lcReturn
PRIVATE lnPrevAl
= gfRltFld(RETAUTH.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lcReason = gfCodDes(Retauth.reason,'REASON')
* fill Sold To address array
lnPrevAl = SELECT(0)
SELECT CUSTOMER
IF !EOF()
  GOTO RECNO()
ENDIF
SELECT(lnPrevAl)
laSoldTo[1] = CUSTOMER.STName
IF lcRpAddres = 'B'
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
  laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))
  laSoldTo[7] = TRANSFORM(Customer.Phone1,lcPhonPict)
ELSE
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  laSoldTo[7] = TRANSFORM(Customer.Phone1,lcPhonPict)
ENDIF
laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laWareAdd[6]= TRANSFORM(WareHous.cphone , lcPhonPict)

= lfAdrShift('laWareAdd')
= lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.

IF llTocca
  lcVendCD = Customer.CCUSVEND
  lcDept = IIF(!EMPTY(retauth.INVOICE) AND SEEK(retauth.INVOICE,'INVHDR'),INVHDR.DEPT,'')
ENDIF
RETURN .T.
*-- End of Function lfNHeader.
*!***************************************************************************
*!* Name        : lfAdrShift
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 11/09/2005
*!* Purpose     : Function to Shift the Address array if there is any
*!                empty lines in the address
*!***************************************************************************
*!* Called from : Rmrautuk.Prg
*!***************************************************************************
*!* Parameters  : Address Array name
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdrShift()
*!***************************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    = ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

*-- FOR Loop to loop through the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *-- IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End of Function lfAdrShift.
