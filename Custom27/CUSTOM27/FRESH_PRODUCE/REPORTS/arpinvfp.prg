*:***************************************************************************
*: Program file  : ARPINVFP
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR FRESH PREDUCE
*! Date          : 09/29/1999
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
*: Example : DO ARPINVFP
*:***************************************************************************
*: This Report Program is due to C101654 ...
*:***************************************************************************
*:Modifications  :
*:B603281,1 BWA 11/11/1999  1) Creat new option in the option grid and fix the bug 
*:                             of change the flag on the invoice when printing 
*:                          2) Fix the bug of not printing terms in the first copy.
*:B603310,1 BWA 29/11/1999  Fix the bug of the Account Country for both the Bill To
*:                          and the Ship To are not appearing on the Invoice.
*:                          and fix the data of the customer name in the third 
*:                          copy.
*:***************************************************************************

PRIVATE lcAlias
lcAlias = ALIAS()

*-- Open the Ordline , BomVar files to get from them the OrdLine Qty. and InvLine Qty.

*--Declare an arrays to hold the sales orders to print them in the <FRX> 
DECLARE laSalOrd[3,1]

IF !USED(lcConsinvh)
  =gfOpenFile(gcDataDir+"Consinvh","Consinvh",'SH', @lcConsinvh, .T.)
ENDIF

IF !USED(lcConsinvl)
  =gfOpenFile(gcDataDir+"Consinvl","Consinvl",'SH', @lcConsinvl, .T.)
ENDIF

IF !USED(lcOrdLine)  
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

IF !USED(lcBomVar)
  =gfOpenFile(gcDataDir+"BomVar","Bomvar", 'SH', @lcBomVar, .T.)
ENDIF  


CREAT TABLE (gcWorkDir+lcInvFresh) ( Invoice C(6), LineCnt n(10))
INDEX ON Invoice TAG Invoice of (gcWorkDir+lcInvFresh)

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'FRLOGO.BMP')

CREATE CURSOR (lclines) (Invoice C(6) , Account C(5) , ALtStyle C(19), Style C(19) ,;
                         Qty1 N(6)    , Qty2 N(6)    , Qty3 N(6)    , Qty4 N(6)    ,;
                         Qty5 N(6)    , Qty6 N(6)    , Qty7 N(6)    , Qty8 N(6)    ,;
                         Totqty N(7)  , Price N(9,2) , Order C(6)   , Store C(8)   ,;
                         LineNo N(6)  , LineCnt n(10))

INDEX ON Invoice+Order+STR(LineNo,6) TAG Invoice of (gcWorkDir+lclines)

PRIVATE lcInvoice, lcScanExp, lnLineCnt , LineCnt

SELECT InvHdr
SET SKIP TO
lcScanExp = IIF(UPPER(lcRpExp)==".T.", "", "FOR " + lcRpExp)

SCAN &lcScanExp
  lcInvoice = INVHDR.invoice
  IF InvHdr.consol = 'Y'
    SELECT (lcConsinvl)
  ELSE
    SELECT InvLine  
  ENDIF
  lnLineCnt = 0
  LineCnt   = 0
  =SEEK(lcInvoice)   
  SCAN  WHILE invoice = lcInvoice
  
    SCATTER FIELDS LIKE Invoice , Account  , ALtStyle , Style , Qty1 , Qty2   , Qty3  ,;
                         Qty4   , Qty5     , Qty6     , Qty7  , Qty8 , Totqty , Price ,;
                         Order  , Store   , LineNo   MEMVAR
    lnLineCnt = lnLineCnt + 2
    INSERT INTO (lcLines) ;
                         (Invoice , Account  , ALtStyle , Style , Qty1 , Qty2   , Qty3  , Qty4            ,;
                          Qty5    , Qty6     , Qty7  , Qty8 , Totqty , Price , Order    , Store           ,;
                          LineNo  , LineCnt  )                                                             ;      
                   VALUES                                                                                  ;
                         (M.Invoice ,M.Account  ,M.ALtStyle ,M.Style ,M.Qty1   ,M.Qty2  ,M.Qty3  ,M.Qty4  ,;
                          M.Qty5    ,M.Qty6     ,M.Qty7     ,M.Qty8  ,M.Totqty ,M.Price ,M.Order ,M.Store ,;
                          M.LineNo  , lnLineCnt  )
    IF !SEEK(InvHdr.Invoice,lcInvFresh)
      INSERT INTO (lcInvFresh) (INVOICE) VALUES (M.Invoice)
    ENDIF                          
    SELECT (lcInvFresh)
    REPLACE LineCnt WITH LineCnt + 2    
  ENDSCAN
  SELECT INVHDR
ENDSCAN

SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

CREATE CURSOR (lcCopisTmp) (Copy C(1))
SELECT (lcCopisTmp)

INSERT INTO (lcCopisTmp) (Copy) VALUES ('A')
INSERT INTO (lcCopisTmp) (Copy) VALUES ('B')
INSERT INTO (lcCopisTmp) (Copy) VALUES ('C')
INDEX ON Copy TAG (lcCopisTmp) OF (lcCopisTmp)


SELECT InvHdr
SET RELATION TO '' INTO (lcCopisTmp) ADDITIVE
SELECT (lcCopisTmp)        
SET RELATION TO INVHDR.Invoice INTO &lcLines ADDITIVE     
SELECT InvHdr
SET RELATION TO Invoice INTO (lcInvFresh) ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + Invhdr.order INTO Ordhdr ADDITIVE

SELECT (lcLines)
SET RELATION TO IIF(!EMPTY(EVAL(lcLines+'.ALTSTYLE')) , EVAL(lcLines+'.ALTSTYLE') ,EVAL(lcLines+'.STYLE')) INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lcLines+'.Account') + EVAL(lcLines+'.STYLE') INTO SPCK_LIN ADDITIVE
SET RELATION TO 'O'+order+store+style+STR(lineno,6) INTO &lcOrdLine ADDITIVE
SET RELATION TO "SO"+order+STR(lineno,6)            INTO &lcBomVar  ADDITIVE

SELECT InvHdr
SET SKIP TO (lcCopisTmp),(lcLines)

SELECT (lcAlias)

*!*************************************************************
*! Name        : lfTOtQty
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/29/1999
*! Purpose     : To sum the number of the total pieces of InvLine Qty.
*!*************************************************************
*! Called from : ARPINVFP.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfTOtQty()
*!*************************************************************

FUNCTION lfTOtQty
PARAMETERS lcDumy

lcOldInv = INVHDR.INVOICE + EVAL(lcCopisTmp+'.Copy') 
IF lcOldInv = InvHdr.Invoice  + EVAL(lcCopisTmp+'.Copy') 
  LNTOTQTY = LNTOTQTY + EVAL(lcLines+'.TotQty')
ENDIF

*!*************************************************************
*! Name        : lfInvNote
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/29/1999
*! Purpose     : To seek with the invoice in the notepad
*!*************************************************************
*! Called from : ARPINVFP.FRX
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
PARAMETERS lcDumy

=SEEK('C' + InvHdr.Invoice,'Notepad') 

*--This variables hold the first tow lines of the header notepad to 
*--put it in the comment of the invoice.
    lcInvNote1 = MLINE(NOTEPAD.mNotes , 1) 
    lcInvNote2 = MLINE(NOTEPAD.mNotes , 2) 

RETURN !EMPTY(lcInvNote1) .AND. !EMPTY(lcInvNote2)

*!*************************************************************
*! Name        : lfSalOrd
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/29/1999
*! Purpose     : To seek with the invoice in the Consinvh to get 
*!             : the sales orders for one invoice.
*!*************************************************************
*! Called from : ARPINVFP.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSalOrd()
*!*************************************************************

FUNCTION lfSalOrd
PARAMETERS lcDumy
PRIVATE lncount 

lcAlias = ALIAS()
SELECT (lcConsinvh)
=SEEK(InvHdr.Invoice, (lcConsinvh) )
lncount = 0

SCAN WHILE &lcConsinvh..Invoice = InvHdr.Invoice  .AND. lncount < 4
  IF ASCAN(laSalOrd,&lcConsinvh..ORDER) = 0
    lncount = lncount + 1
    laSalOrd[lncount,1]  = &lcConsinvh..ORDER
  ENDIF 
ENDSCAN

SELECT (lcAlias)
RETURN ''

*!*************************************************************
*! Name        : lflcOldInv
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 09/29/1999
*! Purpose     : To get the variable lcOldInv its value
*!*************************************************************
*! Called from : ARPINVFP.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lflcOldInv()
*!*************************************************************

FUNCTION lflcOldInv
PARAMETERS lcDumy

lcOldInv = InvHdr.invoice

RETURN ''

*!*************************************************************
*! Name      : lfSolShp
*! Developer : Bassem Rafaat (BWA)
*! Date      : 09/29/1999
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
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

DECLARE laSoldTo[5,1] , laShipTo[5,1] 

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

lcSolTName = CUSTOMER.BTName

*:B603310,1 BWA 29/11/1999  Fix the bug of the Account Country for both the Bill To
*:B603310,1 BWA 29/11/1999  and the Ship To are not appearing on the Invoice. [START]
*laSoldTo[1] = CUSTOMER.cAddress12
*laSoldTo[2] = CUSTOMER.cAddress22
*laSoldTo[3] = CUSTOMER.cAddress32
*laSoldTo[4] = CUSTOMER.cAddress42
*laSoldTo[5] = CUSTOMER.cAddress52

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
*:B603310,1 BWA 29/11/1999 [END]

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
  
  *:B603310,1 BWA 29/11/1999  Fix the bug of the Account Country for both the Bill To
  *:B603310,1 BWA 29/11/1999  and the Ship To are not appearing on the Invoice. [START]
  *laShipTo[1] = CUSTOMER.cAddress1
  *laShipTo[2] = CUSTOMER.cAddress2
  *laShipTo[3] = CUSTOMER.cAddress3
  *laShipTo[4] = CUSTOMER.cAddress4
  *laShipTo[5] = CUSTOMER.cAddress5

  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
  *:B603310,1 BWA 29/11/1999 [END]  
ENDIF 
=lfAdrShift('laShipTo')

lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lnCusRecNo = RECNO('CUSTOMER')
lcCustVend = LOOKUP(CUSTOMER.cCusVend, 'M'+ORDHDR.Account,CUSTOMER.Account, 'CUSTOMER')
IF BETWEEN(lnCusRecNo, 1, RECCOUNT('CUSTOMER'))
  GO lnCusRecNo IN CUSTOMER
ENDIF  

RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 11/02/1997
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
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*B603310,1 BWA 29/11/1999  Fix the data of the customer name in the third copy.[START]
*!*************************************************************
*! Name      : lfPrint
*! Developer : Bassem Rafaat (BWA)
*! Date      : 09/29/1999
*! Purpose   : Function to convert the field PRTFLAG from N to Y
*!*************************************************************
*! Called from : ARPINVFP.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrint
PARAMETERS lcDumy

IF gcDevice = 'PRINT'
  SELECT InvHdr
  =SEEK(IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store), 'Customer')
    IF PrtFlag <> 'P'
      = RLOCK()                 && lock current record to update PrtFlag field in invoice file
      REPLACE PrtFlag WITH 'P'
      UNLOCK
    ENDIF
ENDIF

RETURN ''
*B603310,1 BWA 29/11/1999 [END]