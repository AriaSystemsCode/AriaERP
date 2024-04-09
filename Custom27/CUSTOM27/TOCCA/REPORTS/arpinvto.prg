*:***************************************************************************
*: Program file  : ARPINVTO
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR Tocca
*! Date          : 01/17/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Nader Anis 
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVFP
*:***************************************************************************
*: This Report Program is due to C101749
*:***************************************************************************
*B603482,1 NAD 02/27/2000 Always print the vendor # of the account

* -------- inserts the Tocca logo in a temp file
PRIVATE lcAlias
lcAlias = ALIAS()

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'TOCALOGO.BMP')
SELECT (lcAlias)

 
*!***************************************************************
*! Name      : lfGetVend
*! Developer : Nader Anis 
*! Date      : 02/27/2000
*! Purpose   : Get the Vendor no from the master custmer account  
*!***************************************************************
*! Parameters: None									*B603482,1
*!***************************************************************
*! Returns   :  None
*!***************************************************************
*! Example   :  =lfGetVend()
*!***************************************************************
Function  lfGetVend
PARAMETERS lcDumy

*B603482,1 Save the customer record pointer
lnCusRecNo = RECNO('CUSTOMER')
*B603482,1 Get the vendor # of the current account
lcVendor = LOOKUP(CUSTOMER.cCusVend,'M'+CUSTOMER.ACCOUNT,CUSTOMER.ACCOUNT, 'CUSTOMER')
*B603482,1 Restore the customer record pointer if it is not end of file
IF BETWEEN(lnCusRecNo, 1, RECCOUNT('CUSTOMER'))
  GO lnCusRecNo IN CUSTOMER
ENDIF  

RETURN ""

*--End of Function lfGetVend
