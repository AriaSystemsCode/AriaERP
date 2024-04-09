*:***************************************************************************
*: Program file  : ICINVICI.PRG
*: Program desc. : INVENTORY REPORT FOR INF10
*: For Report    : (ICINVICI.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mohamed Shokry Mohamed (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,lfSeTSRep,lfGetMem,lfRankFile
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICINVICI
*:***************************************************************************
*: This Report Program is due to C120869 ...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
lcStTime = TIME()

STORE '' To m.Style , m.Desc ,m.SCALE
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.price,m.Amount

IF USED(TMPINVFILE)
  USE IN (TMPINVFILE)
ENDIF
=lfCrtTmp()

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR
SET RELATION TO STYLE INTO STYLE ADDI
SELECT STYLE
SET RELATION TO 'S' + SCALE INTO SCALE
SELECT INVLINE

*-- Scan all invoices in selected critria.
SCAN FOR &lcRpExp
  =lfGetMem()  && Fill Memory variables with its corresponding values.

  *-- if you find this style in temp. file
  IF SEEK(INVLINE.STYLE,TMPINVFILE)

    SELECT (TMPINVFILE)
    REPLACE Amount    WITH Amount    + m.Amount ,;
            Qty1      WITH QTY1      + m.QTY1,;
            Qty2      WITH QTY2      + m.QTY2,;
            Qty3      WITH QTY3      + m.QTY3,;
            Qty4      WITH QTY4      + m.QTY4,;
            Qty5      WITH QTY5      + m.QTY5,;
            Qty6      WITH QTY6      + m.QTY6,;
            Qty7      WITH QTY7      + m.QTY7,;
            Qty8      WITH QTY8      + m.QTY8,;
            TotQty    WITH TotQTY    + m.TotQTY,;
            SCALE     WITH m.Scale  
  ELSE  && Add new style record.
    IF !EMPTY(m.Style)
      INSERT INTO (TMPINVFILE) FROM MEMVAR
    ENDIF
  ENDIF
  
ENDSCAN

SELECT (TMPINVFILE)

*-- Calculate spent time in collecting data.
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
*
FUNCTION lfCrtTmp

CREATE CURSOR (TMPINVFILE) (STYLE C(19),DESC C(20), QTY1 N(6), QTY2 N(6), QTY3 N(6);
                           , QTY4 N(6), QTY5 N(6), QTY6 N(6), QTY7 N(6), QTY8 N(6);
                           , TotQTY N(6), Price N(12,2), Amount N(14,2), SCALE C(3))   

INDEX ON STYLE TAG (TMPINVFILE) OF (TMPINVFILE)

*-- end of lfColData.
*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Fill RANK_FILE memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
*
FUNCTION lfGetMem

SCATT MEMVAR MEMO
m.Desc=Style.Desc
m.price= STYLE.PRICEA
m.Amount = m.neqvamnt
*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : validate style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfStyBrw()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvStyle()
*!**************************************************************************
* 
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.
*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
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
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
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
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
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


*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : get the Scale sizes
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
PARAMETERS lcScale
PRIVATE lcOldAlias,lnX,lcString,lcScale,lcZ
lcOldAlias = ALIAS()

SELECT SCALE
SEEK 'S'+lcScale
lnX      = 1
lcString = ''
IF FOUND() 
  DO WHILE lnX <= CNT
    lcZ = STR(lnX,1)
    lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','  ')
    lnX= lnX + 1
  ENDDO
ELSE
  lcString = '* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(lcOldAlias)) > 0
  SELECT (lcOldAlias)
ENDIF
RETURN(lcString)
*-- End of lfGetScale.
