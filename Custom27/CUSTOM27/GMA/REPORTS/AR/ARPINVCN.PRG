*:***************************************************************************
*: Program file  : ARPINVCE.PRG
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR GMA ACCOSORIES CANADA
*! Date          : 11/14/2007
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mohamed Shokry (MHM)			
*: Ticket Job Number: T20071019.0011
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVCN
*:**************************************************************************
*:Modifications :
*:***************************************************************************


=gfOpenFile(gcDataDir+"OrdLine","OrdLine",'SH')
SELECT INVHDR
SET RELATION TO REP1 INTO Salesrep ADDITIVE

SELECT InvLine
SET RELATION TO "O"+ Order + STR(lineno,6) INTO Ordline ADDITIVE


SELECT INVHDR
LOCATE
*-- End of Section of open files --*

*!*************************************************************
*! Name        : lfGetHST
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 11/18/2007
*! Purpose     : To get the HST from Codes file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : 
*!*************************************************************
FUNCTION lfGetHST
PARA lctemmp
lcAlias = ALIAS()

SELECT CODES
lcOldOrd = ORDER()
SET ORDER TO Codes
llRpHSTTax = .F.
IF SEEK("N"+ LEFT(Customer.cAddress4,6)+"YSTATE")
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = "N"+ LEFT(Customer.cAddress4,6)+"YSTATE"
    IF crltd_nam = "LHSTTAX   "
       llRpHSTTax = IIF(ALLTRIM(crltd_vlu) = "T",.T.,.F.)
       EXIT
    ENDIF
  ENDSCAN  
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrd 

SELECT (lcAlias)

RETURN