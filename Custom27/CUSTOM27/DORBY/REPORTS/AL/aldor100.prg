*!********************************************************************
*: Program file  : ALDOR100.PRG (REFERE TO C#101555)
*: Program desc. : CUSTOMIZED BILL OF LADING FORM FOR Dorby Forcks.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: AL
*:      Developer: Ahmed Salah Shalaby -(SSH)
*!********************************************************************
*: Calls         : FUNCTIONS  : 
*!********************************************************************
*: Passed Parameters  : NONE
*!********************************************************************

*-- Restoring from the memory file to check if the system is a 
*-- multi-warehous or not.
llWareHous = (gfGetMemVar('M_WAREHOUS',gcAct_Comp) = 'Y')

*-- To get the company's warehous in case of single warehous compnay.

lcWhName  = SPACE(05)
lcWhAddr1 = SPACE(05)
lcWhAddr2 = SPACE(05)
lcWhAddr3 = SPACE(05)

IF !llWareHous
  SELECT WareHous
  GOTO TOP
  lcWhName  = cDesc
  lcWhAddr1 = gfGetAdr('WareHous' , '' , '' , '' , 1)
  lcWhAddr2 = gfGetAdr('WareHous' , '' , '' , '' , 2)
  lcWhAddr3 = gfGetAdr('WareHous' , '' , '' , '' , 3)
ENDIF
*-- Function to Set the necessary Relations
=lfSetFiles()

*-- Initializing the necessary variables.

STORE 0 TO lnSubTCart,lnSubTWght,lnNoOfLine
FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
   IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'BOL_HDR.BOL_NO'
     tmpBolFile = laOgFxFlt[lnInd,6]
   ENDIF
ENDFOR
IF !USED(tmpBolFile)
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
  SET DEVICE TO SCREEN
ELSE
  SELECT (tmpBolFile)
  GOTO TOP
  INDEX ON Bol_No TAG (tmpBolFile)
  IF EOF()
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
    SET DEVICE TO SCREEN
  ENDIF
ENDIF

SET DEVICE TO SCREEN
WAIT WINDOW  'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT

*-- Starts printing the report
SELECT (tmpBolFile)
GO TOP
SET RELATION TO Bol_No INTO Bol_Hdr ADDITIVE

SCAN WHILE !EOF() AND INKEY() <> 32  
  STORE 0 TO lnSubTCart,lnSubTWght,lnNoOfLine  
  IF !EOF('Bol_Lin')
    SELECT Bol_Lin    
    *-- To print the header's information of the BOL form.
    =lfPrntHdr()
    SCAN REST WHILE Bol_No+Order+Pack_No = &tmpBolFile..Bol_No
      =SEEK(Pack_No,'Pack_Hdr')
      lnNoOfLine = lnNoOfLine + 1
      *-- This check is done because the maximum number of lines are 9.
      *-- When the lines exceed 9 for one BOL then we will print the
      *-- subtotal for the current page
      IF lnNoOfLine > 9
        @ 56,02 SAY "SUBTOTAL"
        @ 56,48 SAY "SUBTOTAL"
        @ 57,01 SAY lnSubTCart PICTURE "99999999"
        @ 57,18 SAY "CONTINUED ON NEXT PAGE"
        @ 57,46 SAY lnSubTWght PICTURE "9999999.99"
        STORE 0 TO lnSubTCart,lnSubTWght
        *-- To print the header's information of the BOL form.
        =lfPrntHdr()
        lnNoOfLine = 1
      ENDIF
      DO CASE
        CASE lnNoOfLine = 1
          *-- To print the information of the pack.
          =lfPrnLine(43)
        CASE lnNoOfLine = 2
          =lfPrnLine(45)
        CASE lnNoOfLine = 3
          =lfPrnLine(46)
        CASE lnNoOfLine = 4
          =lfPrnLine(48)
        CASE lnNoOfLine = 5
          =lfPrnLine(49)
        CASE lnNoOfLine = 6
          =lfPrnLine(51)
        CASE lnNoOfLine = 7
          =lfPrnLine(52)
        CASE lnNoOfLine = 8
          =lfPrnLine(54)
        CASE lnNoOfLine = 9
          =lfPrnLine(55)
      ENDCASE
      *-- Accumolating the Subtotal in order to print it for one
      *-- page of BOL
      lnSubTCart = lnSubTCart + Pack_Hdr.Tot_Cart
      lnSubTWght = lnSubTWght + Pack_Hdr.Tot_Wght
    ENDSCAN
    *-- Grand total for the printed BOL.
    @ 56,04 SAY "TOTAL"
    @ 56,51 SAY "TOTAL"
    @ 57,01 SAY Bol_Hdr.Tot_Cart PICTURE "99999999"
    @ 57,46 SAY Bol_Hdr.Tot_Wght PICTURE "9999999.99"    
  ENDIF  
ENDSCAN  
DO ENDREPORT
*--- Start Clear Relations.
SELECT (tmpBolFile)
SET RELATION TO
SELECT Pack_Hdr
SET RELATION TO
SELECT Bol_Lin
SET RELATION TO
SELECT Bol_Hdr
SET RELATION TO
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfSetFiles  
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Set the necessary relations.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetFiles()
*!*************************************************************
FUNCTION lfSetFiles

SELECT Pack_Hdr
SET RELATION TO 'O'+Order INTO OrdHdr ADDITIVE

SELECT Bol_Lin
SET RELATION TO Pack_No INTO Pack_Hdr ADDITIVE

SELECT Bol_Hdr
SET RELATION TO IIF(EMPTY(Store),'M'+Account,'S'+Account+Store) INTO Customer ADDITIVE
SET RELATION TO Bol_No INTO BOL_Lin ADDITIVE
SET RELATION TO W_Code INTO WareHous ADDITIVE


*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : To print the header's information.
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrntHdr()
*!*************************************************************
FUNCTION lfPrntHdr

@ 12,10 SAY Pack_Hdr.ShipVia + "/" + ALLTRIM(Bol_Hdr.Carrier)
@ 12,66 SAY &tmpBolFile..Bol_No
IF llWareHous
  lcWhName  = WareHous.cDesc
  lcWhAddr1 = gfGetAdr('WareHous' , '' , '' , '' , 1)
  lcWhAddr2 = gfGetAdr('WareHous' , '' , '' , '' , 2)
  lcWhAddr3 = gfGetAdr('WareHous' , '' , '' , '' , 3)
ENDIF
@ 16,05 SAY lcWhName
@ 17,05 SAY lcWhAddr1
@ 18,05 SAY IIF(EMPTY(lcWhAddr2),lcWhAddr3,lcWhAddr2)
@ 19,05 SAY IIF(EMPTY(lcWhAddr2),'',lcWhAddr3)

lcName  = Customer.StName
lcAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
lcAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
lcAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)

@ 24,05 SAY lcName
@ 25,05 SAY lcAddr1
@ 26,05 SAY IIF(!EMPTY(lcAddr2),lcAddr2,ALLTRIM(lcAddr3))
@ 27,05 SAY IIF(!EMPTY(lcAddr2),ALLTRIM(lcAddr3),'')

*!*************************************************************
*! Name      : lfPrnLine
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : To print the information of the BOL lines.
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnLine()
*!*************************************************************
FUNCTION lfPrnLine
PARAMETERS lnRow

@ lnRow,01 SAY Pack_Hdr.Tot_Cart
lcCustPo = OrdHdr.CustPo
IF OrdHdr.MultiPO
  =SEEK('O'+Pack_Hdr.Order+Pack_Hdr.Store,'OrdLine')
  lcCustPo = OrdLine.CustPo
ENDIF
@ lnRow,15 SAY IIF(!EMPTY(lcCustPO),"PO#:"+ALLTRIM(lcCustPO),'')+" "+;
               IIF(!EMPTY(OrdHdr.Dept),"DEPT#:"+OrdHdr.Dept,'')
@ lnRow,46 SAY Pack_Hdr.Tot_Wght PICTURE "9999999.99"

*!*************************************************************
*! Name      : lfWhen
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Option Grid When Function.
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfWhen()
*!*************************************************************
FUNCTION lfWhen

R_WIDTH   = 'N'    
