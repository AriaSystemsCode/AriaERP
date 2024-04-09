*:************************************************************************
*: Program file  : SOSRORD.Prg
*: Program desc. : STYLE REORDER REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 12/29/98
*: Reference     : *E301107,1 AMM
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfsrvSty(), lfSRVFab(), lfFabSum(), lfwOGWhen(), 
*:                 lfPrtHdr(), lfPrtCTot(), lfPrtSTot()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOSRORD
*:************************************************************************
*: Modifications:
*B602679,1 AMM 03/18/99 Don't run the report if item code structure not found
*B802380,1 AMM 07/14/99 Fix bugs in the style IN RANGE function in the option grid
*E500271,4 SSE   07/14/1999 Add popup in OG to filter for the flag field named
*E500271,4                  LEDIORDER which indicate whether this Order is coming
*E500271,4                  from EDI or not 
*B803355,1 WMA 06/19/2000 Fix the bug of printing a wrong account name when printing the invoice lines 
*:B603713,11  MHM 10/17/2000 increase the length of the fields (Gros_Price ,Price, 
*:B603713,11                 PriceA , PriceB , PriceC , Amount) With 2 digit we add 
*:B603713,11                 record in SYREPUVR to get print dicemal yes or no 
*:B129787,1 NNA 10/30/2005 Fix bug that report doesn't get the correct Desc. for the first record.
*:************************************************************************

*-- XCLRTOT      Array to hold nonmajor totals
*-- XSTYTOT      Array to hold Major totals
*-- XGRDTOT      Array to hold grand totals

DIMENSION XCLRTOT(10), XSTYTOT(10), XGRDTOT(10)
STORE 0 TO XCLRTOT,XSTYTOT,XGRDTOT,PAGENO
XTITLE    = SPACE(30)
STORE SPACE(0) TO XSTYLE, XCOLOR
ROW       = 99
R_TITLE   = 'STYLE REORDER REPORT'

lcOrdHdr = gfTempName()
USE (gcDataDir+'ORDHDR') AGAIN ALIAS (lcOrdHdr) IN 0 ORDER TAG ORDHDR
*B802380,1 AMM Comment out, define it in the option grid
*lcMaj      = gfItemMask('PM')             && Get the major of the style
*B802380,1 AMM end

lnMajSize  = LEN(lcMaj)                   && Length of the major
lcStyTitle = gfItemMask('HI')             && Style title

SELECT INVLINE
SET RELATION TO 'O'+ORDER INTO (lcOrdHdr) ADDITIVE

SELECT ORDLINE
SET RELATION TO cOrdType+Order INTO ORDHDR ADDITIVE

SELECT STYLE
SET RELATION TO STYLE INTO ORDLINE 
SET RELATION TO STYLE INTO INVLINE ADDITIVE
SET SKIP TO ORDLINE,INVLINE

XDESC  = DESC
XPRICE = PRICEA

*B602679,1 AMM Add condition to the filter not to display an empty report.
*LOCATE ALL FOR &lcRpExp

*E500271,4 add Field LEDIORDER to the filter expression [Begin.]
IF !EMPTY(lcRpEdiFlt)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcRpEdiFlt
ENDIF
*E500271,4 add Field LEDIORDER to the filter expression [End.]
*B129787,1 NNA 10/30/2005 (Start) comment the next lines because it makes report so slowly and scan for only 
*B129787,1 NNA            one record matches with this criteria
*LOCATE ALL FOR &lcRpExp .AND. ((ORDHDR.STATUS # 'X' .AND. ORDHDR.CREORDER = 'Y') .OR. ;
                        (&lcOrdHdr..STATUS # 'X' .AND. &lcOrdHdr..CREORDER = 'Y'))
*B602679,1 AMM end
SET SKIP TO
*IF !FOUND()
*  =gfModalGen('TRM00052B00000','DIALOG')
*  SET DEVICE TO SCREEN
*  RETURN
*ENDIF
llFound = .F.
SCAN FOR &lcRpExp
  IF SEEK(STYLE.STYLE,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE STYLE = STYLE.STYLE FOR ((ORDHDR.STATUS # 'X' .AND. ORDHDR.CREORDER = 'Y') .OR. (&lcOrdHdr..STATUS # 'X' .AND. &lcOrdHdr..CREORDER = 'Y'))
      llFOUND = .T.
      EXIT
    ENDSCAN
  ENDIF
  IF llFOUND = .T.
    EXIT
  ENDIF
ENDSCAN
IF !llFOUND
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
FRSTREC = 1
*B129787,1 NNA (End)

SET DEVICE TO PRINT
SCAN FOR &lcRpExp

  *** PRINT THE HEADER
  IF ROW>55
    =lfPrtHdr()
  ENDIF
  
  *B129787,1 NNA 10/30/2005 (Begin) if we are in the first record and Xstyle is Empty then get Style.Desc
  IF EMPTY(XSTYLE) .AND. FRSTREC = 1
    =lfPrtSTot()
    XDESC  = DESC
    XPRICE = PRICEA
    FRSTREC = FRSTREC + 1
  ENDIF
  *B129787,1 NNA (End)
  
  *-- If the nonMajor changes, print its subtotals
  IF !EMPTY(XSTYLE) .AND. (STYLE <> XSTYLE)
    =lfPrtCTot()
    XDESC  = DESC
    XPRICE = PRICEA
  ENDIF

  *-- If the Major changes, print its subtotals
  IF !EMPTY(xStyle) .and. SUBSTR(STYLE,1,lnMajSize) # SUBSTR(XSTYLE,1,lnMajSize)
    =lfPrtSTot()
    XDESC  = DESC
    XPRICE = PRICEA
  ENDIF
  
  XSTYLE  = STYLE
  xColor  = SUBSTR(STYLE,lnMajSize+2)

  WAIT WINDOW 'Printing Style '+STYLE NOWAIT
  *-- If this style has no orders or no invoices, get next one.
  IF EOF('ORDLINE') .AND. EOF('INVLINE')
    LOOP
  ENDIF

*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3.
* SUMMARY FORMAT
* STYLE        COLOR  DESC.         SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY    PRICE   TOTAL $$
* 123456789012 123456 1234567890 123456 123456 123456 123456 123456 123456 123456 123456 1234567  1234.00 1234567.00
*
* DETAIL FORMAT
* STYLE        COLOR  ACCT# NAME            ORDER# ENTERED     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY   PRICE   TOTAL $$
* 123456789012 123456 12345 123456789012345 123456 99/99/99 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234.00 1234567.00

  *-- PRINT THE ORDER LINES FOR THE DETAIL FORMAT
  IF !EOF('ORDLINE')
    SELE ORDLINE
    SCAN WHILE STYLE = XSTYLE
      IF ORDHDR.STATUS='X' .OR. ORDHDR.CREORDER # 'Y'
        LOOP
      ENDIF

      *** PRINT THE HEADER
      IF ROW>55
        =lfPrtHdr()
      ENDIF
      IF lcRPT_TYPE ='D'
        @ ROW,000 SAY 'O'
        @ ROW,002 SAY STYLE
        @ ROW,022 SAY ACCOUNT
        IF SEEK(IIF(EMPTY(ORDHDR.STORE),'M'+ORDHDR.ACCOUNT, ;
                             'S'+ORDHDR.ACCOUNT+ORDHDR.STORE),'CUSTOMER')
        
          @ ROW,028 SAY SUBSTR(CUSTOMER.STNAME,1,7)
        ENDIF
        @ ROW,036 SAY ORDER
        @ ROW,043 SAY ORDHDR->ENTERED
        @ ROW,052 SAY QTY1   PICTURE '@Z 999999'
        @ ROW,059 SAY QTY2   PICTURE '@Z 999999'
        @ ROW,066 SAY QTY3   PICTURE '@Z 999999'
        @ ROW,073 SAY QTY4   PICTURE '@Z 999999'
        @ ROW,080 SAY QTY5   PICTURE '@Z 999999'
        @ ROW,087 SAY QTY6   PICTURE '@Z 999999'
        @ ROW,094 SAY QTY7   PICTURE '@Z 999999'
        @ ROW,101 SAY QTY8   PICTURE '@Z 999999'
        @ ROW,108 SAY TOTQTY PICTURE '@Z 9999999'
        *:B603713,11  MHM 10/17/2000 [start]
        *@ ROW,116 SAY PRICE  PICTURE '9999.99'
        *@ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999.99'
        IF lcRpDeciml = "Y"
          @ ROW,116 SAY PRICE  PICTURE '9999.99'
          @ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999.99'
        ELSE  
          @ ROW,116 SAY PRICE  PICTURE '9999999'
          @ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999999'
        ENDIF
        *:B603713,11  MHM 10/17/2000 [end]
        ROW = ROW + 1
      ENDIF
      *-- Add quantities to the nonMajor totals
      XCLRTOT(1) = XCLRTOT(1) + QTY1
      XCLRTOT(2) = XCLRTOT(2) + QTY2
      XCLRTOT(3) = XCLRTOT(3) + QTY3
      XCLRTOT(4) = XCLRTOT(4) + QTY4
      XCLRTOT(5) = XCLRTOT(5) + QTY5
      XCLRTOT(6) = XCLRTOT(6) + QTY6
      XCLRTOT(7) = XCLRTOT(7) + QTY7
      XCLRTOT(8) = XCLRTOT(8) + QTY8
      XCLRTOT(9) = XCLRTOT(9) + TOTQTY
      XCLRTOT(10)= XCLRTOT(10)+ TOTQTY*PRICE
    ENDSCAN
  ENDIF
  *** PRINT THE INVOICE LINES FOR THE DETAIL FORMAT
  IF !EOF('INVLINE')
    SELECT INVLINE
    SCAN WHILE STYLE = XSTYLE 
      IF &lcOrdHdr..CREORDER # 'Y'
        LOOP
      ENDIF
      *** PRINT THE HEADER
      IF ROW>55
        =lfPrtHdr()
      ENDIF
      SELE INVLINE
      IF lcRPT_TYPE = 'D'
        @ ROW,000 SAY 'I'
        @ ROW,002 SAY STYLE
        @ ROW,022 SAY ACCOUNT
        *B803355,1 WMA [Begin] Seek of the account exist in the invline table not ordhdr.
        *IF SEEK (IIF(EMPTY(ORDHDR.STORE),'M'+ORDHDR.ACCOUNT, ;
                               'S'+ORDHDR.ACCOUNT+ORDHDR.STORE),'CUSTOMER')
       
        IF SEEK (IIF(EMPTY(INVLINE.STORE),'M'+INVLINE.ACCOUNT, ;
                              'S'+INVLINE.ACCOUNT+INVLINE.STORE),'CUSTOMER')       
        *B803355,1 WMA [End]
          
          @ ROW,028 SAY SUBSTR(CUSTOMER.STNAME,1,7)
        ENDIF
        @ ROW,036 SAY ORDER
        @ ROW,043 SAY INVDATE
        @ ROW,052 SAY QTY1   PICTURE '@Z 999999'
        @ ROW,059 SAY QTY2   PICTURE '@Z 999999'
        @ ROW,066 SAY QTY3   PICTURE '@Z 999999'
        @ ROW,073 SAY QTY4   PICTURE '@Z 999999'
        @ ROW,080 SAY QTY5   PICTURE '@Z 999999'
        @ ROW,087 SAY QTY6   PICTURE '@Z 999999'
        @ ROW,094 SAY QTY7   PICTURE '@Z 999999'
        @ ROW,101 SAY QTY8   PICTURE '@Z 999999'
        @ ROW,108 SAY TOTQTY PICTURE '@Z 9999999'
        *:B603713,11  MHM 10/17/2000 [start]
        *@ ROW,116 SAY PRICE  PICTURE '9999.99'
        *@ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999.99'
        IF lcRpDeciml = "Y"
          @ ROW,116 SAY PRICE  PICTURE '9999.99'
          @ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999.99'
        ELSE
          @ ROW,116 SAY PRICE  PICTURE '9999999'
          @ ROW,124 SAY TOTQTY*PRICE PICTURE '@Z 9999999999'
        ENDIF  
        *:B603713,11  MHM 10/17/2000 [end]
        ROW = ROW + 1
      ENDIF
      *-- Add quantities to nonMajor totals
      XCLRTOT(1) = XCLRTOT(1) + QTY1
      XCLRTOT(2) = XCLRTOT(2) + QTY2
      XCLRTOT(3) = XCLRTOT(3) + QTY3
      XCLRTOT(4) = XCLRTOT(4) + QTY4
      XCLRTOT(5) = XCLRTOT(5) + QTY5
      XCLRTOT(6) = XCLRTOT(6) + QTY6
      XCLRTOT(7) = XCLRTOT(7) + QTY7
      XCLRTOT(8) = XCLRTOT(8) + QTY8
      XCLRTOT(9) = XCLRTOT(9) + TOTQTY
      XCLRTOT(10)= XCLRTOT(10)+ TOTQTY*PRICE
    ENDSCAN
  ENDIF
ENDSCAN
*-- Print NonMajor totals
=lfPrtCTot()
*-- Print Major totals
=lfPrtSTot()

*-- Print Grand totals
IF lcRPT_TYPE = 'S'
  @ ROW,00 SAY '**GRAND TOTAL***'
  @ ROW,033 SAY XGRDTOT(1)  PICTURE '@Z 999999'
  @ ROW,040 SAY XGRDTOT(2)  PICTURE '@Z 999999'
  @ ROW,047 SAY XGRDTOT(3)  PICTURE '@Z 999999'
  @ ROW,054 SAY XGRDTOT(4)  PICTURE '@Z 999999'
  @ ROW,061 SAY XGRDTOT(5)  PICTURE '@Z 999999'
  @ ROW,068 SAY XGRDTOT(6)  PICTURE '@Z 999999'
  @ ROW,075 SAY XGRDTOT(7)  PICTURE '@Z 999999'
  @ ROW,082 SAY XGRDTOT(8)  PICTURE '@Z 999999'
  @ ROW,089 SAY XGRDTOT(9)  PICTURE '9999999'
  *:B603713,11  MHM 10/17/2000 [start]
  *@ ROW,106 SAY XGRDTOT(10) PICTURE '9999999.99'
  IF lcRpDeciml = "Y"
    @ ROW,106 SAY XGRDTOT(10) PICTURE '9999999.99'
  ELSE
    @ ROW,106 SAY XGRDTOT(10) PICTURE '9999999999'
  ENDIF  
  *:B603713,11  MHM 10/17/2000 [end]
ELSE
  @ ROW,000 SAY '**GRAND TOTAL***'
  @ ROW,052 SAY XGRDTOT(1)  PICTURE '@Z 999999'
  @ ROW,059 SAY XGRDTOT(2)  PICTURE '@Z 999999'
  @ ROW,066 SAY XGRDTOT(3)  PICTURE '@Z 999999'
  @ ROW,073 SAY XGRDTOT(4)  PICTURE '@Z 999999'
  @ ROW,080 SAY XGRDTOT(5)  PICTURE '@Z 999999'
  @ ROW,087 SAY XGRDTOT(6)  PICTURE '@Z 999999'
  @ ROW,094 SAY XGRDTOT(7)  PICTURE '@Z 999999'
  @ ROW,101 SAY XGRDTOT(8)  PICTURE '@Z 999999'
  @ ROW,108 SAY XGRDTOT(9)  PICTURE '9999999'
  *:B603713,11  MHM 10/17/2000 [start]
  *@ ROW,124 SAY XGRDTOT(10) PICTURE '9999999.99'
  IF lcRpDeciml = "Y"
    @ ROW,124 SAY XGRDTOT(10) PICTURE '9999999.99'
  ELSE
    @ ROW,124 SAY XGRDTOT(10) PICTURE '9999999999'
  ENDIF  
  *:B603713,11  MHM 10/17/2000 [end]
ENDIF

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN

IF USED(lcOrdHdr)
  USE IN (lcOrdHdr)
ENDIF

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/29/98
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/29/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

SET ORDER TO SEGVAL IN ICSEGVAL
SET ORDER TO SCALE IN SCALE  

*!*************************************************************
*! Name      : lfPrtHdr
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Print the header of the report
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtHdr()
*!*************************************************************
FUNCTION lfPrtHdr

*** PRINT THE HEADER
PAGENO = PAGENO + 1
*ahmed 10/06/1999
*DO RPT_HDR WITH 'ORD960',XTITLE,R_WIDTH
DO RPT_HDR WITH 'SOSRORD',XTITLE,R_WIDTH
*ahmed end
IF lcRPT_TYPE = 'S'
  @ 05,00 SAY SPACE(2) + LEFT(lcStyTitle,19) + ' DESC.         SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY     PRICE  TOTAL $$'
ELSE
  @ 05,00 SAY SPACE(2) + LEFT(lcStyTitle,19)  +' ACCT# NAME    ORDER# ENTERED     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8  TOTQTY   PRICE   TOTAL $$'
ENDIF

@ 06,00 SAY REPLICATE('*',134)
ROW = 7


*!*************************************************************
*! Name      : lfPrtCTot
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Print the NonMajor subtotal
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtCTot()
*!*************************************************************
FUNCTION lfPrtCTot
IF lcRPT_TYPE = 'S'
  IF !llRpROrd .OR. XCLRTOT(9) <> 0
    @ ROW,002 SAY XSTYLE
    @ ROW,022 SAY SUBSTR( XDESC,1,10)
    @ ROW,033 SAY XCLRTOT(1)  PICTURE '@Z 999999'
    @ ROW,040 SAY XCLRTOT(2)  PICTURE '@Z 999999'
    @ ROW,047 SAY XCLRTOT(3)  PICTURE '@Z 999999'
    @ ROW,054 SAY XCLRTOT(4)  PICTURE '@Z 999999'
    @ ROW,061 SAY XCLRTOT(5)  PICTURE '@Z 999999'
    @ ROW,068 SAY XCLRTOT(6)  PICTURE '@Z 999999'
    @ ROW,075 SAY XCLRTOT(7)  PICTURE '@Z 999999'
    @ ROW,082 SAY XCLRTOT(8)  PICTURE '@Z 999999'
    @ ROW,089 SAY XCLRTOT(9)  PICTURE '9999999'
    *:B603713,11  MHM 10/17/2000 [start]
    *@ ROW,099 SAY XPRICE PICTURE '9999.99'
    *@ ROW,106 SAY XCLRTOT(10) PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,099 SAY XPRICE PICTURE '9999.99'
      @ ROW,106 SAY XCLRTOT(10) PICTURE '9999999.99'
    ELSE
      @ ROW,099 SAY XPRICE PICTURE '9999999'
      @ ROW,106 SAY XCLRTOT(10) PICTURE '9999999999'
    ENDIF
    *:B603713,11  MHM 10/17/2000 [end]
    ROW=ROW+1
  ENDIF
ELSE
  IF !llRpROrd .OR. XCLRTOT(9) <> 0
    @ ROW,000 SAY '*** SUB-TOTAL*** ' + XSTYLE
    @ ROW,052 SAY XCLRTOT(1)  PICTURE '@Z 999999'
    @ ROW,059 SAY XCLRTOT(2)  PICTURE '@Z 999999'
    @ ROW,066 SAY XCLRTOT(3)  PICTURE '@Z 999999'
    @ ROW,073 SAY XCLRTOT(4)  PICTURE '@Z 999999'
    @ ROW,080 SAY XCLRTOT(5)  PICTURE '@Z 999999'
    @ ROW,087 SAY XCLRTOT(6)  PICTURE '@Z 999999'
    @ ROW,094 SAY XCLRTOT(7)  PICTURE '@Z 999999'
    @ ROW,101 SAY XCLRTOT(8)  PICTURE '@Z 999999'
    @ ROW,108 SAY XCLRTOT(9)  PICTURE '9999999'
    *:B603713,11  MHM 10/17/2000 [start]
    *@ ROW,124 SAY XCLRTOT(10) PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,124 SAY XCLRTOT(10) PICTURE '9999999.99'
    ELSE
      @ ROW,124 SAY XCLRTOT(10) PICTURE '9999999999'
    ENDIF  
    *:B603713,11  MHM 10/17/2000 [start]
    ROW = ROW + 2
  ENDIF
ENDIF
XSTYTOT(1) = XSTYTOT(1) + XCLRTOT(1)
XSTYTOT(2) = XSTYTOT(2) + XCLRTOT(2)
XSTYTOT(3) = XSTYTOT(3) + XCLRTOT(3)
XSTYTOT(4) = XSTYTOT(4) + XCLRTOT(4)
XSTYTOT(5) = XSTYTOT(5) + XCLRTOT(5)
XSTYTOT(6) = XSTYTOT(6) + XCLRTOT(6)
XSTYTOT(7) = XSTYTOT(7) + XCLRTOT(7)
XSTYTOT(8) = XSTYTOT(8) + XCLRTOT(8)
XSTYTOT(9) = XSTYTOT(9) + XCLRTOT(9)
XSTYTOT(10)= XSTYTOT(10)+ XCLRTOT(10)
STORE 0 TO XCLRTOT



*!*************************************************************
*! Name      : lfPrtSTot
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Print the Major subtotal
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrtSTot()
*!*************************************************************
FUNCTION lfPrtSTot
IF lcRPT_TYPE = 'S'
  IF !llRpROrd .OR. XSTYTOT(9) <> 0
    @ ROW,00 SAY '*** TOTAL*** ' + SUBSTR(XSTYLE,1,lnMajSize)
    @ ROW,033 SAY XSTYTOT(1)  PICTURE '@Z 999999'
    @ ROW,040 SAY XSTYTOT(2)  PICTURE '@Z 999999'
    @ ROW,047 SAY XSTYTOT(3)  PICTURE '@Z 999999'
    @ ROW,054 SAY XSTYTOT(4)  PICTURE '@Z 999999'
    @ ROW,061 SAY XSTYTOT(5)  PICTURE '@Z 999999'
    @ ROW,068 SAY XSTYTOT(6)  PICTURE '@Z 999999'
    @ ROW,075 SAY XSTYTOT(7)  PICTURE '@Z 999999'
    @ ROW,082 SAY XSTYTOT(8)  PICTURE '@Z 999999'
    @ ROW,089 SAY XSTYTOT(9)  PICTURE '9999999'
    *:B603713,11  MHM 10/17/2000 [start]
    *@ ROW,106 SAY XSTYTOT(10) PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,106 SAY XSTYTOT(10) PICTURE '9999999.99'
    ELSE
      @ ROW,106 SAY XSTYTOT(10) PICTURE '9999999999'
    ENDIF  
    *:B603713,11  MHM 10/17/2000 [end]
    ROW = ROW + 2
   ENDIF
ELSE
  IF !llRpROrd .OR. XSTYTOT(9) <> 0
    @ ROW,000 SAY '*** TOTAL*** ' + SUBSTR(XSTYLE,1,lnMajSize)
    @ ROW,052 SAY XSTYTOT(1)  PICTURE '@Z 999999'
    @ ROW,059 SAY XSTYTOT(2)  PICTURE '@Z 999999'
    @ ROW,066 SAY XSTYTOT(3)  PICTURE '@Z 999999'
    @ ROW,073 SAY XSTYTOT(4)  PICTURE '@Z 999999'
    @ ROW,080 SAY XSTYTOT(5)  PICTURE '@Z 999999'
    @ ROW,087 SAY XSTYTOT(6)  PICTURE '@Z 999999'
    @ ROW,094 SAY XSTYTOT(7)  PICTURE '@Z 999999'
    @ ROW,101 SAY XSTYTOT(8)  PICTURE '@Z 999999'
    @ ROW,108 SAY XSTYTOT(9)  PICTURE '9999999'
    *:B603713,11  MHM 10/17/2000 [start]
    *@ ROW,124 SAY XSTYTOT(10) PICTURE '9999999.99'
    IF lcRpDeciml = "Y"
      @ ROW,124 SAY XSTYTOT(10) PICTURE '9999999.99'
    ELSE
      @ ROW,124 SAY XSTYTOT(10) PICTURE '9999999999'
    ENDIF
    *:B603713,11  MHM 10/17/2000 [end]
    ROW = ROW + 2
  ENDIF
ENDIF
XGRDTOT(1) = XGRDTOT(1) + XSTYTOT(1)
XGRDTOT(2) = XGRDTOT(2) + XSTYTOT(2)
XGRDTOT(3) = XGRDTOT(3) + XSTYTOT(3)
XGRDTOT(4) = XGRDTOT(4) + XSTYTOT(4)
XGRDTOT(5) = XGRDTOT(5) + XSTYTOT(5)
XGRDTOT(6) = XGRDTOT(6) + XSTYTOT(6)
XGRDTOT(7) = XGRDTOT(7) + XSTYTOT(7)
XGRDTOT(8) = XGRDTOT(8) + XSTYTOT(8)
XGRDTOT(9) = XGRDTOT(9) + XSTYTOT(9)
XGRDTOT(10)= XGRDTOT(10)+ XSTYTOT(10)
STORE 0 TO XSTYTOT


*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'D'
      lnDPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Division'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Z'
      lnZPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Season'
      ENDIF
    CASE laMajSeg[lnC,1] = 'G'
      lnGPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style Group'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
    CASE laMajSeg[lnC,1] = 'S'
      lnSPos = lnC
       IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Scale'
      ENDIF
  ENDCASE

ENDFOR


*!*************************************************************
*! Name      : lfPreGrd
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/18/99
*! Purpose   : Don't allow running the report if Item code structure 
*!             not found.
*! Reference : *B602679,1
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPreGrd()
*!*************************************************************
FUNCTION lfPreGrd
llOpStr = gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U','ICISTRU')
  IF USED('ICISTRU') .AND. llOpStr
    USE IN ICISTRU
  ENDIF
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  RETURN .F.
ENDIF
IF USED('ICISTRU') .AND. llOpStr
  USE IN ICISTRU
ENDIF


*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*!E500271,4
FUNCTION lfvEdiOrd
lcRpEdiFlt = ""
IF 'EB' $ gcCmpModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.
