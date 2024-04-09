*:**************************************************************************
*: Program file  : ICDSNVST.PRG
*: Program desc. : Copy from the Standard DATE SENSITIVE INVENTORY REPORT (FOR STU10)
*: Date          : 12/06/2003
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Walid A. Wahab (WAB)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:             
*:         Functions  : lfwRepWhen()
*:                    : lfItmPos()
*:                    : lfNoRecord()
*:                    : gfModalGen()
*:                    : gfOpenFile()
*:                    : gfItemMask()
*:                    : gfBroWWare()
*:                    : gfStyBrw()
*:                    : FaBrow()
*:                    : lfCreatFil() 
*:                    : lfNonMaj()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:                    : lfFillAray() 
*:                    : lfvWareHos() 
*:                    : lfwOldWare() 
*:                    : lfvSortBy()
*:                    : lfGetScale()
*:                    : lfvPrnWhD()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*: Example : DO ICDATSEN
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR STU10 (120734) 
*:**************************************************************************
lcStTime = TIME()       && To store the current time
lcRpExp = STRTRAN(lcRpExp,"STYLE.","")

*-- check first of Sort by WareHouse and Empty of Warehouse Code

*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.

* - Save the old warehouse to assign it after collect 
* - The date in case print more than one warehouse.  
lcOldWare = lcWareCode


*-- If User changed the Filter in OG [Begin]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcStyleTmp) AND RECCOUNT(lcStyleTmp) > 0
    = lfCreatFil()
  ENDIF
  
  *-- make the Style File match the criteria selected
  SELECT Style
  SET FILTER TO &lcRpExp
  *-- If print Warehouse Details is set to YES [Begin]
  IF llRpPrnWhD
    SELECT StyDye
    *-- If Multi Bins is true [Begin]
    IF llMultiBin
      SET RELATION TO Style+cWareCode INTO WhsLoc ADDITIVE
    ENDIF
    *-- If Multi Bins is true [End]
    SET RELATION TO Style INTO Style ADDITIVE

    *Empty the variable when sorting by Style.
    lcWareCode = IIF(lcRpSortBy = 'W',lcWareCode,'')


	IF !EMPTY(lcWareCode)
      SET FILTER TO CWARECODE = lcWareCode AND !EOF('Style')
    ENDIF  

    *--  StyDye 
    *--       |___
    *--       |   WhsLoc     (If Multi Bins)     
    *--       |
    *--       |___
    *--           Style       
    
  ELSE         && Else print Warehouse Details is set to NO
    SELECT Style
    IF llMultiBin
      SET RELATION TO Style INTO WhsLoc ADDITIVE
    ENDIF  

    *--  Style 
    *--       |___
    *--           WhsLoc     (If Multi Bins)     

  ENDIF
  *-- Endif print Warehouse Details is Yes [Begin]
  
  *-- If no records match criteria

  IF lfNoRecord()
    RETURN
  ELSE
    STORE 0 TO lnRpTVlCst,lnRpTSalVl
    DO lpCollData   && collect the data to the temporary file
  ENDIF  
  *-- EndIf of no records match criteria  
ENDIF
*-- If User changed the Filter in OG [End]

*-- if end of file (no records match criteria) [Begin]
SELECT (lcStyleTmp)
IF lfNoRecord()
  *- Assign the old warehouse to global warehouse variable after collect 
  *- The date in case print more than one warehouse.  
  lcWareCode = lcOldWare
  RETURN
ELSE
  lcExpr = 'ABS(nStk1)+ABS(nStk2)+ABS(nStk3)+ABS(nStk4)+ABS(nStk5)+ABS(nStk6)+ABS(nStk7)+ABS(nStk8) <> 0'
  DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcExpr

  *Assign the old warehouse to global warehouse variable after collect 
  *The date in case print more than one warehouse.  
  lcWareCode = lcOldWare
ENDIF
*-- Endif of (no records match criteria) [End]
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lfNoRecord
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Dectect if no records match criteria
*!**************************************************************************
*! Called from : PRG
*!**************************************************************************
*! Calls       : gfModalGen()
*!**************************************************************************
*! Example     : = lfNoRecord()
*!**************************************************************************
*
FUNCTION lfNoRecord
GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*-- End of lfNoRecord.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfCreatFil()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
SELECT StyInvJl
SET ORDER TO StyInvJl DESCENDING

lnStatPos = lfItmPos('STYLE.STATUS')    && store the Style Status position
laOGFxFlt[lnStatPos,6] = 'A'            && make status target defaulted to "A"

= lfCreatFil()       && to create the temp file 
*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns            : Position
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
*! Name      : lfCreatFil
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
*-- If change Sort By from Style to Warehouse or vise versa close the temp file first 
IF USED(lcStyleTmp)
  USE IN (lcStyleTmp)
ENDIF
*-- Var lcFileName to hold the Master File whether it's STYLE or STYDYE
lcFileName = IIF(lcRpSortBy = 'S','STYLE','STYDYE')

SELECT StyInvJl
=AFIELDS(laFileStru) 
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'PRICEA'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 12
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'SCALE'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 3
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'TOTCOST'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'DESC'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 20
laFileStru[lnNewFld,4] = 0
  
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Ave_Cost'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1) + 1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'llWare'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

CREATE DBF (gcWorkDir+lcStyleTmp) FROM ARRAY laFileStru     && create the temp file

=gfOpenFile(gcWorkDir+lcStyleTmp,'','EX')   && to open the Temp File Exclusively

IF lcRpSortBy = 'W' AND EMPTY(lcWareCode)
  INDEX ON cWareCode +Style TAG (lcStyleTmp) OF (lcStyleTmp)
ELSE
  INDEX ON Style+cWareCode TAG (lcStyleTmp) OF (lcStyleTmp)
ENDIF

*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
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
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
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
*! Name      : lfvFabric
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
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
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
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
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
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
*! Name      : lfFillAray
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*! Example   : = lfArrDummy()
*!**************************************************************************
*
FUNCTION lfFillAray
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
laSortDesc[1,1] = PROPER(lcMajTtl)
laSortDesc[2,1] = 'Location'
laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'W'
*-- End of lfFillAray.

*!**************************************************************************
*! Name      : lfvWareHos
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Validate Warehouse Code
*!**************************************************************************
*! Example   : = lfvWareHos()
*!**************************************************************************
*
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag

lcWareHous = VARREAD()
lcTag      = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

*B607419,1 ABD - Valid Ony if the Field is not Empty. [Begin]
IF !EMPTY(&lcWareHous) 
  *B607419,1 ABD - [End]
  IF LASTKEY() = 13 AND !MDOWN()
    IF SEEK(&lcWareHous.,'WAREHOUS') 
      &lcWareHous = WAREHOUS.cWareCode
    ELSE
      &lcWareHous = gfBroWWare(.T.)
    ENDIF
  ELSE
    &lcWareHous = lcOldWare
  ENDIF
  *B607419,1 ABD - End IF for IF Statment. [Begin]
ENDIF
*B607419,1 ABD - [End]

SET ORDER TO &lcTag IN WAREHOUS
*-- End of lfvWareHos.


*!**************************************************************************
*! Name      : lfwOldWare
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*! Example   : = lfwOldWare()
*!**************************************************************************
*
FUNCTION lfwOldWare
lcOldWare = EVALUATE(SYS(18))
*-- End of lfwOldWare.


*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : valid sort by function
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
*
FUNCTION lfvSortBy
llRpPrnWhD = IIF(lcRpSortBy='W',.T.,llOldPrWhD)
CLEAR READ
*-- End of lfvSortBy.

*!**************************************************************************
*! Name      : lfvPrnWhD
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : valid print warehouse details
*!**************************************************************************
*! Example   : =lfvPrnWhD()
*!**************************************************************************
*
FUNCTION lfvPrnWhD
llOldPrWhD = llRpPrnWhD   && Save current print ware house details in another 
*-- End of lfvPrnWhD.

*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
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
    *B607273,1 ALB Fix the header alliment [Begin]
    *lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','  ')
    lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','   ')
    *B607273,1 ALB Fix the header alliment [end]
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

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Walid A. Wahab (WAB)
*! Date      : 12/06/2003
*! Purpose   : Collect the data from StyInvJl file.
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lpCollData()
*!**************************************************************************
*!**************************************************************************
PROCEDURE lpCollData
lcWareCode = IIF(lcRpSortBy = 'W',lcWareCode,'')

SELECT StyInvJl
lcOldTag = SET("ORDER")

*change to the new index STYLE+CWARECODE+DTOS(dTrDate)+cSession+cIRType
SET ORDER TO TAG Stydate

SELECT (lcFileName)


SCAN

  *Assign the current warehouse to global warehouse variable while collect 
  *The date in case print more than one warehouse.  
  IF lcRpSortBy = 'W'
    lcWareCode = cWarecode
  ENDIF

  WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style NOWAIT
  lcStyle   = IIF(lcFileName = 'STYDYE',Style.Style,Style)

  lcDyelot  = IIF(lcFileName = 'STYDYE',STYDYE.Dyelot,SPACE(10))

  SELECT StyInvJl
  IF SEEK(lcStyle+lcWareCode)

    SCAN REST WHILE Style+cWareCode+DTOS(dTrDate)+cSession+cIrType= lcStyle;
              FOR dTrDate <= ldRpDate AND ;
              IIF(!EMPTY(lcWareCode),cWareCode = lcWareCode,.T.) ;
              AND IIF(lcFileName = 'STYDYE',cDyelot = lcDyelot,.T.)

      WAIT WINDOW 'Collecting Data for ' + lcMajTtl + ' ' + Style NOWAIT
      SCATTER MEMVAR
      =SEEK(lcStyle,'Style')

      SELECT(lcStyleTmp)
      IF !SEEK(lcStyle+lcWareCode)
        INSERT INTO (lcStyleTmp) FROM MEMVAR
        REPLACE Scale   WITH Style.Scale       ,;
                TotCost WITH Style.TotCost     ,;
                PriceA  WITH Style.PriceA      ,;
                Desc    WITH Style.Desc        ,;
                nStkVal WITH nTotStk * TotCost ,;
                Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)

       IF llRpPrnWhD AND lcRpSortBy = 'S'
         REPLACE cWareCode WITH SPACE(0)
       ENDIF
    
      ELSE
        FOR lnStkCnt = 1 TO 8
          lcStkCnt = STR(lnStkCnt,1)
          REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
        ENDFOR
        REPLACE nTotStk WITH nTotStk + m.nTotStk,;
                nStkVal WITH nTotStk * TotCost,;
                Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
      ENDIF

      IF lcRpSortBy = 'S'
        IF llRpPrnWhD
          SELECT(lcStyleTmp)
          IF !SEEK(lcStyle + StyInvJl.cWareCode)
            INSERT INTO (lcStyleTmp) FROM MEMVAR
            REPLACE Scale     WITH Style.Scale        ,;
                    TotCost   WITH Style.TotCost      ,;
                    PriceA    WITH Style.PriceA       ,;
                    Desc      WITH Style.Desc         ,;
                    cWareCode WITH StyInvJl.cWareCode ,;
                    nStkVal   WITH nTotStk * TotCost  ,;
                    llWare    WITH .T.                ,;
                    Ave_Cost  WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
          ELSE
            FOR lnStkCnt = 1 TO 8
              lcStkCnt = STR(lnStkCnt,1)
              REPLACE nStk&lcStkCnt WITH nStk&lcStkCnt + m.nStk&lcStkCnt
            ENDFOR
            REPLACE nTotStk  WITH nTotStk + m.nTotStk ,;
                    nStkVal  WITH nTotStk * TotCost ,;
                    Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF  
ENDSCAN
SET ORDER TO &lcOldTag IN StyInvJl



