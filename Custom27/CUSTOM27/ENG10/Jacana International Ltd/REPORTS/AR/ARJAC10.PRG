*:**************************************************************************
*: Program file  : ARJAC10
*: Program desc. : Custom Invoice Price Comparison for Jacana.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 01/22/2001
*: Reference     : C200262,1
*:**************************************************************************
*: Procedures    : lpCreatFil , lpGenExpr , lpCollData , lpInsrtRec
*: Functions     : lfwRepWhen , lfItmPos , lfsrAcc , lfSRVSty , lfStySum , lfSRInv
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*B605540 Problems in the invoice comparison report 

*-- IF filter change collect data again.
IF llOGFltCh
  DO lpCreatFil
  
  SELECT InvLine
  SET RELATION TO InvLine.Invoice INTO InvHdr ADDITIVE
  SET RELATION TO 'M' + Account INTO Customer ADDITIVE
  SET RELATION TO Style INTO Style ADDITIVE  
  
  DO lpCollData            && Collect the data for report.
  
  SELECT InvLine
  SET RELATION TO  
ENDIF
*-- Endif of Filter change.  

SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
  DO CASE
    CASE lcRpSortBy = "A"
      SET ORDER TO TAG Account
    CASE lcRpSortBy = "S"
      SET ORDER TO TAG Style
    CASE lcRpSortBy = "D"    
      SET ORDER TO TAG cDivision
  ENDCASE
  
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lnEqvPrice , lcPriceLvl , lcPriceCat , lnCurrUnit , lnExRate , lcExRSin

SELECT (lcWorkFile)
SET ORDER TO TAG InvStyle

SELECT InvLine
SCAN FOR &lcRpExp AND InvHdr.Status <> "V"
  lcPriceLvl = IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  lcPriceCat = IIF(Customer.PriceLvl='Q',;
               IIF(Style.nAtQtyC>0 AND TotQty >= Style.nAtQtyC,'C',;
               IIF(Style.nAtQtyB>0 AND TotQty >= Style.nAtQtyB,'B','A')),;
               IIF(INLIST(lcPriceLvl,'A','B','C'),lcPriceLvl,'A'))
  IF InvHdr.cCurrCode = gcBaseCurr
    IF ABS(Style.Price&lcPriceCat - Price) > 0.02
      lnEqvPrice = Price
      DO lpInsrtRec 
    ENDIF
  ELSE

    STORE 1 TO lnCurrUnit , lnExRate
    lcExRSin = "/"
    lcUntSin = SPACE(1)
    lnEqvPrice = 0
    lcExRSin = gfGetExSin(@lcUntSin, InvHdr.cCurrCode)
    *B605540,1 (Start)
    *lnExRate  = gfChkRate('lnCurrUnit' , InvHdr.cCurrCode , gdSysDate , .T. , gcAct_Comp , .F. , .T.)
    lnExRate   = INVHDR.nexRate
    lnCurrUnit = INVHDR.nCurrunit
    *B605540,1 (End)
    lnEqvPrice = ROUND(Price &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)    
    *B605540,1 (Start)
    *IF ABS(Style.Price&lcPriceCat - lnEqvPrice) > 0.02
    *IF ABS(IIF(SEEK(Style+InvHdr.cCurrCode,'StyPrice') AND StyPrice.Price&lcPriceCat <> 0,StyPrice.Price&lcPriceCat,Style.Price&lcPriceCat) - lnEqvPrice) > 0.02
    IF ABS(IIF(SEEK(Style+InvHdr.cCurrCode,'StyPrice') AND StyPrice.Price&lcPriceCat <> 0,(StyPrice.Price&lcPriceCat &lcExRSin lnExRate &lcUntSin lnCurrUnit),Style.Price&lcPriceCat) - lnEqvPrice) > 0.02
    *B605540,1 (End)
    
      DO lpInsrtRec
    ENDIF
  ENDIF
ENDSCAN
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpInsrtRec
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To insert a new record or update an old one in temp file.
*!**************************************************************************
*! Example   : DO lpInsrtRec
*!**************************************************************************
*
PROCEDURE lpInsrtRec

*B605540,1 (Start)
*IF SEEK(Invoice + Style,(lcWorkFile))
IF SEEK(Invoice + SUBSTR(Style,1,16),(lcWorkFile))
*B605540,1 (End)
  SELECT (lcWorkFile)
  REPLACE TotQty WITH TotQty + InvLine.TotQty  
ELSE
  m.Account = Account
  m.cBtName = Customer.BtName
  m.Invoice = Invoice
  m.dPostDate = InvHdr.dPostDate
  m.Order     = InvHdr.Order
  m.Style     = Style
  m.cStyDesc = gfCodDes(Style.cStyGroup , 'CSTYGROUP')
  m.TotQty    = TotQty
  *B605540,1 (Start)
  *m.nStdPrice = Style.Price&lcPriceCat
  *m.nStdPrice = IIF(SEEK(InvLine.Style+InvHdr.cCurrCode,'StyPrice') AND StyPrice.Price&lcPriceCat <> 0 ;
  *              AND InvHdr.cCurrCode <> gcBaseCurr,ROUND(StyPrice.Price&lcPriceCat &lcExRSin lnExRate &lcUntSin lnCurrUnit,2),Style.Price&lcPriceCat)
  IF (SEEK(InvLine.Style+InvHdr.cCurrCode,'StyPrice') AND StyPrice.Price&lcPriceCat <> 0) AND InvHdr.cCurrCode <> gcBaseCurr
    m.nStdPrice = ROUND(StyPrice.Price&lcPriceCat &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
  ELSE
    m.nStdPrice = Style.Price&lcPriceCat
  ENDIF
  *B605540,1 (End)
  
  m.nActPrice = ROUND((InvLine.nEqvAmnt/InvLine.TotQty),2)
  m.cDivision = InvHdr.cDivision
  
  *B605540,1 (Start)
  INSERT INTO (lcWorkFile) FROM MEMVAR
  *B605540,1 (End)
ENDIF  
*B605540,1 (Start)
*INSERT INTO (lcWorkFile) FROM MEMVAR
*B605540,1 (End)
*-- End of lpInsrtRec.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnPDatePos  = lfItmPos('INVHDR.DPOSTDATE') && get Post Date position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
PRIVATE lcPerFrom , lcPerTo , lcLoop

*-- Create a temporary table to hold data which will be displayed in report.
CREATE TABLE (gcWorkDir+lcWorkFile) (Account C(5) , cBtName C(30) , Invoice C(6) , ;
              dPostDate D(8) , Order C(6) , Style C(16) , cStyDesc C(15) , ;
              TotQty N(13) , nStdPrice N(15,2) , nActPrice N(15,2) , cDivision C(6))
*DO CASE
*  CASE lcRpSortBy = "A"
*    INDEX ON Account TAG Account
*  CASE lcRpSortBy = "S"
*    INDEX ON Style TAG Style
*  CASE lcRpSortBy = "D"    
*    INDEX ON cDivision TAG cDivision
*ENDCASE

INDEX ON Account+Invoice TAG Account
INDEX ON Style TAG Style
INDEX ON cDivision TAG cDivision
INDEX ON Invoice + Style TAG InvStyle
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
*
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT Style
    SET ORDER TO TAG Cstyle
    SET RELATION TO Style.Style INTO STYLE_X
    GO TOP IN Style
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT Style
    SET ORDER TO TAG Style
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!***************************************************************************
*! Name      : lfSRInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/22/2002
*! Purpose   : control browse Invoices for InvHdr File
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRInv()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRInv
PARAMETERS lcParm
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT InvHdr

DO CASE
  CASE lcParm = 'S'  && Set code
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SELECT (lnAlias)
ENDCASE
*-- End of lfSRInv.
