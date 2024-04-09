*:***************************************************************************
*: Program file  : MaProjc
*: Program desc. : Material Projection Report
*: For Report    : @x,y Say
*: System        : Aria Advantage Series.
*: Module        : Material (MA)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrnCode,PRNTLOOP,PAGEHDR,GROPHDR,DETAIL,GROPFOT,GRANDTOT
*:    Functions  : ARIABROW,gfItemMask,gfModalGen,lfMakeExpr,lfCollTime,lfWRunGrid,lfTempFile,
*:               : lfSRSty,lfStySum,lfSRFab,lfFabSum,lfWOldVal,lfEvalVars,lfvRange,
*:               : lfvChngFlt,lfClearRep,lfEditProj,lfvEdtPrj,lfUpdStyle.
*:***************************************************************************
*: Example : DO MaProjc
*:***************************************************************************
*: This Program is due to E301097,1 ...
*:Modifications :
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes.
*B607281,1 ABD 06/30/2003 Fix bug operator/operand type mismatch While
*B607281,1 ABD            Select styles and hit the range option
*B607281,1 ABD            Also change valid function on one record at syrepuvr.
*B038431,1 NNA 09/09/2004 Fix bug that if you didn't define item code structure you'll get error
*B038431,1 NNA            [Variable 'laSeg' Not Found ] when you run this report 
*:***************************************************************************
*:
lcStTime = TIME()
*-- if user change criteria.

*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [Begin
IF llClearFn OR llOGFltCh
*E301170,1 MAB 03/15/1999 Use variables detect OG filter changes. [End

  PRIVATE lcFltExpr,lcStyleExp,lcStatus,lcOrderExp

  llUpdPlan = .F.  && Need to update plan in style file.
  
  *-- Define variables to avoid recreate data again [Begin
  STORE .F. TO llClearFn,llFiltChng,llChStyle,llChFabric
  lcLastExpr = lcRpExp
  *-- Define variables to avoid recreate data again [End

  *-- Recreate Temp. File if it have previous data. Begin
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    = lfTempFile()  && Create temporary used in collecting data.
  ENDIF
  *-- Recreate Temp. File if it have previous data. End 

  lcStyleExp = [STYLE = '' AND STATUS = 'A']
  lcStatus   = IIF(llRpHold,'','H')  + 'CX'
  
  lcOrderExp = [!(ORDHDR.STATUS $ lcStatus) AND TOTQTY > 0 ]

  IF llRpExPOrd
    lcOrderExp = lcOrderExp + ' AND DTOS(ORDHDR.COMPLETE) >= DTOS(gdSysDate)'
  ENDIF

  *-- loop fixed filter array to make optimized expressions, For Both Style and OrdLine.
  FOR lnI = 1 TO ALEN(laOGFxFlt,1)
    *-- if User enter filtered data [Begin]
    IF !EMPTY(laOGFxFlt[lnI,6]) AND IIF(laOGFxFlt[lnI,7]='V', .T. , ;
       USED(laOGFxFlt[lnI,6]) AND RECCOUNT(laOGFxFlt[lnI,6]) > 0)
       
       lcFltExpr = lfMakeExpr(lnI)  && Make Expression

       *-- if you have a filter expression.
       IF !EMPTY(lcFltExpr)
         IF 'STYLE.' $ lcFltExpr
           lcStyleExp = lcStyleExp + ' AND ' + lcFltExpr
         ELSE
           lcOrderExp = lcOrderExp + ' AND ' + lcFltExpr
         ENDIF
       ENDIF  
    
    ENDIF
    *-- if User enter filtered data [Begin]
    
  ENDFOR
  *-- end loop fixed filter array to make optimized expressions.
  
  lcStyleExp = STRTRAN(lcStyleExp,'STYLE.','')
  lcOrderExp = STRTRAN(lcOrderExp,'ORDLINE.','')

  SELECT ORDLINE
  SET ORDER TO TAG ORDLINES
  SET RELATION TO cOrdType+Order INTO ORDHDR
  
  SELECT STYLE
  SET ORDER TO  && To use Rush-More Tech.

  *-- Scan Style file for style expression. 
  SCAN FOR &lcStyleExp
    
    WAIT WINDOW 'Calculate Projection for ' + lcStyTitle + ' : ' + Style NOWAIT

    *-- if you find this style in ordline file. 
    IF SEEK(STYLE,'ORDLINE')

      STORE 0 TO m.SUnits,m.SAmount,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8
      m.Color = IIF(lcFree_Clr = 'C',SUBSTR(STYLE,lnNonMajSt,lnColorLen),SPACE(6))

      SELECT ORDLINE

      *-- Scan ordline file for current style.
      SCAN REST WHILE STYLE+DTOS(Complete)+cOrdType+Order+Store+STR(lineno,6) = STYLE.STYLE ;
           FOR &lcOrderExp
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          m.Qty&lcI = m.Qty&lcI + Qty&lcI
        ENDFOR
        m.SUnits  = m.SUnits  + TotQty
        m.SAmount = m.SAmount + TotQty * Price
      
      ENDSCAN  && end Scan ordline file for current style.

      SELECT STYLE
      
      *-- if you loop around lines and have data
      IF m.SUnits > 0

        m.YIELD    = 0
        m.ONORDER  = 0
        lcLoopAray = IIF(MAKE,'laFNumber','laINumber')

        *-- Loop Fabric cost items
        FOR lnCount = 1 TO ALEN(&lcLoopAray,1)

          IF SEEK(cStyMajor,'BOM')
            SELECT BOM 
            LOCATE REST WHILE cItmMajor=STYLE.cStyMajor ;
                    FOR LIKE(STRTRAN(cItmMask,'*','?'),STYLE.STYLE) AND TYP=&lcLoopAray[lnCount] 
          ENDIF           

          m.YIELD = m.YIELD + IIF (FOUND(),BOM.nBomTotQty,0)
          m.ICLR  = IIF (FOUND(),IIF(BOM.ICLR='******',m.Color,BOM.ICLR),m.Color)
          
          SELECT STYLE
          *-- SALES UNITS & AMOUNT CALCULATION
          IF SEEK(Fabric,'FABRIC')
            m.OnOrder = m.OnOrder + Fabric.OnOrder
            m.IDesc   = Fabric.Desc
          ENDIF
        ENDFOR  && end Loop Fabric cost items.
        
        *-- Calculate Rest of line memvar to insert into temp. file [Begin]
        m.TotProj = 0
        *-- Assign Projection memvars [Begin]
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          m.Proj&lcI = IIF(lcRpBase='S',m.Qty&lcI+INT(m.Qty&lcI*lnRpProjct/100),;
                                        Plan&lcI +INT(Plan&lcI *lnRpProjct/100))
          m.TotProj =  m.TotProj + m.Proj&lcI
        ENDFOR
        *-- Assign Projection memvars [End  ]

        m.PAmount   = m.TotProj * PriceA
        m.PYarDge   = m.TotProj * m.Yield
        m.Style     = PADR(cStyMajor,lnMajorLen)
        m.Scale     = SCALE                                     && To use in Edit projection validation.
        m.cNonMajor = SUBSTR(Style,lnMajorLen+LEN(lcSepRate)+1) && To use in Printing
        m.cFullSty  = STYLE                                     && To use in edit projection Validation. 
        m.Desc      = Desc
        m.PriceA    = PriceA
        m.Fabric    = Fabric
        m.SYarDge   = m.SUnits * m.Yield                

        m.BUnits    = m.TotProj - m.SUnits
        m.BAmount   = m.PAmount - m.SAmount
        *-- Calculate Rest of line memvar to insert into temp. file [End  ]

        INSERT INTO (lcWorkFile) FROM MEMVAR                     && Add new line to Temp. File

      ENDIF  && if you loop around lines and have data.

    ENDIF    && end if you find this style in ordline file.

  ENDSCAN    && end Scan Style file for style expression. 

  WAIT CLEAR
  
  SELECT ORDLINE
  SET RELATION TO

  RELEASE lcFltExpr,lcStyleExp,lcStatus,lcOrderExp  && Free Memory.

ENDIF        && end if user change criteria.

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (lcWorkFile)

*-- if user want to edit projection.
IF llRpEdit
  =lfEditProj()  && Edit Projection.
ENDIF

*-- Start Print
XREPORT = 'MAPROJC'
ROW     = 0
SET DEVICE TO PRINT
CLEAR TYPEAHEAD

DO lpPrnCode  && Do Print code.

DO ENDREPORT
SET DEVICE TO SCREEN
*-- End   Print

WAIT CLEAR

*--Update plan in style file.
*-- Do you wish to update the projection to the style master file.
*--                        [ Yes / No ] 
IF llUpdPlan AND gfModalGen('QRM36112B36001','DIALOG') = 1
  =lfUpdStyle()
ENDIF
*-- end of report code...

*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfMakeExpr
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Make an expression for one item in fixed filter array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Item row in fixed filter array
*!*************************************************************
*! Returns            : line expression
*!*************************************************************
*! Example   : =lfMakeExpr()
*!*************************************************************
*!
FUNCTION lfMakeExpr
PARAMETERS lnFxFltItm
PRIVATE lcItmExpr

lcItmExpr = ''
*-- if operator is like
IF 'LIKE' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
  lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' = "' +  laOGFxFlt[lnFxFltItm,6] + '"'

ELSE  && operator is either Between or InList

  *-- if operator is between.
  IF 'BETWEEN' $ UPPER(ALLTRIM(laOGFxFlt[lnFxFltItm,5]))
    *-- if Type is Date
    PRIVATE lcFrom,lcTo
    lcFrom = SUBSTR(laOGFxFlt[lnFxFltItm,6],1,ATC('|',laOGFxFlt[lnFxFltItm,6]) - 1)
    lcTo   = SUBSTR(laOGFxFlt[lnFxFltItm,6],ATC('|',laOGFxFlt[lnFxFltItm,6]) + 1)

    IF laOGFxFlt[lnFxFltItm,3] = 'D'
      
      lcFrom = CTOD(lcFrom)
      lcTo   = CTOD(lcTo)

      IF !EMPTY(lcFrom)
        lcFrom = DTOS(lcFrom)
        lcTo   = DTOS(lcTo)
        lcItmExpr = 'BETWEEN( DTOS(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ') , "' + ;
                                 lcFrom + '" , "' + lcTo + '")'
      ENDIF  

    
    ELSE  && Type is not date (this case not occured in this report)
    
    ENDIF

  ELSE  && else operator is in list

    *-- if in range case.
    IF laOGFxFlt[lnFxFltItm,7] = 'R'
      lcItmExpr = 'SEEK(' + ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ', "' +;
                            ALLTRIM(laOGFxFlt[lnFxFltItm,6]) + '")'
    ELSE  && default in list case
      lcItmExpr = ALLTRIM(laOGFxFlt[lnFxFltItm,1]) + ' $ "' + laOGFxFlt[lnFxFltItm,6] + '"'
    ENDIF                           
    
  ENDIF

ENDIF
RETURN IIF(EMPTY(lcItmExpr),lcItmExpr,'(' + lcItmExpr + ')')
*-- end of lfMakeExpr.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
*!
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : When run OG function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfWRunGrid
*-- First time run or user press < Reset > .
IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)
  R_WIDTH    = 'W'

  SET ORDER TO FABRIC IN FABRIC
  SET ORDER TO STYLE IN STYLE

  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDLINE.START'),1)
  IF lnDatePos > 0 AND EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)
  ENDIF

  = lfTempFile()  && Create temporary used in collecting data.

  *-- get the number of the right type from the control file for manif. styles
  DECLARE laFNumber[1]
  laFNumber  = ''
  lnAryCount = 0
  FOR lnCheck = 1 TO 5
    lcVar  = "lcMType" + ALLTRIM(STR(lnCheck,1))
    IF EVAL(lcVar) = 'F'
      lnAryCount = lnAryCount + 1
      IF !EMPTY(laFNumber[1])
        DECLARE laFNumber[lnAryCount]
      ENDIF
      laFNumber[lnAryCount] = ALLTRIM(STR(lnCheck,1))
    ENDIF
  ENDFOR

  *-- get the number of the right type from the control file for imported. styles
  DECLARE laINumber[1]
  lnCheck = 1
  laINumber  = ''
  lnAryCount = 0
  FOR lnCheck = 1 TO 5
    lcVar  = "lcIType" + ALLTRIM(STR(lnCheck,1))
    IF EVAL(lcVar) = 'F'
      lnAryCount = lnAryCount + 1
      IF !EMPTY(laINumber[1])
        DECLARE laINumber[lnAryCount]
      ENDIF
      laINumber[lnAryCount] = ALLTRIM(STR(lnCheck,1))
    ENDIF
  ENDFOR

ENDIF
*-- end of lfWRunGrid.

*!*************************************************************
*! Name      : lfTempFile
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
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
*! Example     : = lfTempFile()
*!*************************************************************
*
FUNCTION lfTempFile
CREATE CURSOR (lcWorkFile) ;
    (STYLE C(lnMajorLen), cNonMajor C(19-lnMajorLen-LEN(lcSepRate))   ,;
     Scale C(3) , Color C(6),DESC C(20),PRICEA N(7,2) , cFullSty C(19),;
     FABRIC C(7), IDESC C(20), ICLR C(6), ONORDER N(8,0)              ,;
     YIELD N(7,3), SUNITS N(6,0), SAMOUNT N(9,2) , SYARDGE N(10,3)    ,;
                   PUNITS N(6,0), PAMOUNT N(9,2) , PYARDGE N(10,3)    ,;
                   BUNITS N(6,0), BAMOUNT N(9,2)                      ,;
           QTY1 N(6,0) , QTY2 N(6,0) , QTY3 N(6,0) , QTY4 N(6,0)      ,;
           QTY5 N(6,0) , QTY6 N(6,0) , QTY7 N(6,0) , QTY8 N(6,0)      ,;
           PROJ1 N(6,0), PROJ2 N(6,0), PROJ3 N(6,0), PROJ4 N(6,0)     ,;
           PROJ5 N(6,0), PROJ6 N(6,0), PROJ7 N(6,0), PROJ8 N(6,0)     ,;
           TOTPROJ N(7,0),Planned L(1))

INDEX ON STYLE       TAG &lcWorkFile
INDEX ON FABRIC+ICLR TAG &lcIndxTag
INDEX ON Planned     TAG Planned

SET ORDER TO TAG (lcWorkFile)
*-- end of lfRankFile.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRSty
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
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
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
FUNCTION lfSRFab
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
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
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
*! Name      : lfWOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Get any get field old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid, any Get Field.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWOldVal()
*!*************************************************************
FUNCTION lfWOldVal
laOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars

*B038431,1 NNA 09/09/2004 (Begin) Check if there is item code structure or not
IF lfSegInfo()
*B038431,1 NNA (End)

  lnMajSeg   = gfItemMask('SM')  && No. of major segments.
  lcMajPict  = gfItemMask("PM")

  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  lcSepRate = laMajSegs[lnMajSeg,6]
  
  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]
      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
  ENDFOR    && end Loop Around Non Major elements.

  lnMajorLen = LEN(lcMajPict)
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'

  lcStyGrp = lcStyMajor + ' Group' 
  *-- Compute Free/Color Items in Style code Structure. [End]
  
*B038431,1 NNA (Begin)
ENDIF
*B038431,1 NNA (End)

RETURN ''
*-- end of lfEvalVars.

*!*************************************************************
*! Name      : lfvRange
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Projection increase Percentage validation (00 to 100)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvRange()
*!*************************************************************
*!
*B607281,1 ABD -  Rename this function, that because we have a 
*B607281,1 ABD -  Global funtion with the same name, also change the record
*B607281,1 ABD -  at syrepuvr. [Begin]
*FUNCTION lfvRange
FUNCTION lfvProRang
*B607281,1 ABD [End]

IF lnRpProjct <> laOldVal
  IF BETWEEN(lnRpProjct,000.00,100.00)
    =lfvChngFlt()
  ELSE
    WAIT WINDOW 'Valid Range between 00.00, 100.00' TIMEOUT 2
    lnRpProjct = laOldVal
    =lfOGShowGet('lnRpProjct')
  ENDIF
ENDIF  
*-- end of lfvRange.

*!*************************************************************
*! Name      : lfvChngFlt
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Change Collect data due to change to variables that affect filter.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvChngFlt()
*!*************************************************************
*!
FUNCTION lfvChngFlt
llFiltChng = .T.
*-- end of lfvChngFlt.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.  && Rise clear done flag.
*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfEditProj
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Edit Projection Function
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Code.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfEditProj()
*!*************************************************************
*!
FUNCTION lfEditProj

lcBrFields = "cFullSty    :R :H=lcStyTitle    :30,"+;
	         "lcDesc = STYLE.Desc1  :R :H='Description' :41,"+;
	         "nOpnHld = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 :R :H= IIF(llRpHold,'Open+Hold','Open') :12,"+;
	         "TotProj  :R :H='Projected' :12 "

SELECT (lcWorkFile)
GO TOP
=ARIABROW('','Edit projection',gnbrfsrow1,gnbrfscol1,gnbrfsrow2,gnbrfscol2,'lfvEdtPrj',;
             'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;\<Edit;\<Ok' )

RETURN
*-- end of lfEditProj.

*!*************************************************************
*! Name      : lfvEdtPrj
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Edit Projection Valid Function
*!*************************************************************
*! Calls     : 
*!             Procedures : MAEDTPR.SPX
*!             Functions  : ....
*!*************************************************************
*! Called from : lfEditProj
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvEdtPrj()
*!*************************************************************
*!
FUNCTION lfvEdtPrj
llOk = .F.
SCATTER FIELDS Proj1,Proj2,Proj3,Proj4,Proj5,Proj6,Proj7,Proj8,TotProj TO laPrjn

= SEEK('S'+PADR(ALLTRIM(SCALE),3),'SCALE')
SCATTER FIELDS SCALE.SZ1,SCALE.SZ2,SCALE.SZ3,SCALE.SZ4,SCALE.SZ5,SCALE.SZ6,SCALE.SZ7,SCALE.SZ8 TO laSizes

*-- do edit projection screen.
DO (gcRepHome+"MA\MAEDTPR.SPX")

IF llOk
  GATHER FROM laPrjn FIELDS Proj1,Proj2,Proj3,Proj4,Proj5,Proj6,Proj7,Proj8,TotProj
  REPLACE PAMOUNT WITH TOTPROJ*PRICEA   ,;
          PYARDGE WITH TOTPROJ*YIELD    ,;
          BUNITS  WITH TOTPROJ - SUNITS ,;
          BAMOUNT WITH PAMOUNT - SAMOUNT,;
          Planned WITH .T.
  llUpdPlan = .T.
ENDIF
*-- end of lfvEdtPrj.

*!*************************************************************
*! Name      : lfUpdStyle
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Update style master file with Edited Projections.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Code.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfUpdStyle()
*!*************************************************************
*!
FUNCTION lfUpdStyle
PRIVATE lcAlias,lcOrder,lcStyOrder
lcAlias = SELECT(0)

WAIT WINDOW 'Updating projections to the style master file ...' NOWAIT
SELECT (lcWorkFile)
lcOrder    = ORDER()
lcStyOrder = ORDER('STYLE')
SET ORDER TO TAG Planned
SET ORDER TO TAG STYLE IN STYLE

SCAN FOR Planned
  IF SEEK(cFullSty,'STYLE')
    REPLACE Planned WITH .F.
    SELECT STYLE
    =RLOCK()
    REPLACE PLAN1   WITH &lcWorkFile..Proj1,;
            PLAN2   WITH &lcWorkFile..Proj2,;
            PLAN3   WITH &lcWorkFile..Proj3,;
            PLAN4   WITH &lcWorkFile..Proj4,;
            PLAN5   WITH &lcWorkFile..Proj5,;
            PLAN6   WITH &lcWorkFile..Proj6,;
            PLAN7   WITH &lcWorkFile..Proj7,;
            PLAN8   WITH &lcWorkFile..Proj8,;
            TOTPLAN WITH &lcWorkFile..TotProj
    UNLOCK
  ENDIF  
ENDSCAN
llUpdPlan = .F.

SELECT (lcWorkFile)
SET ORDER TO TAG &lcOrder

SELECT STYLE
SET ORDER TO &lcStyOrder

SELECT (lcAlias)
WAIT CLEAR
RETURN
*-- end of lfUpdStyle.

*---------------------- PRINTING SECTION ----------------------
*--------------------------------------------------------------
*!*************************************************************
*! Name      : lpPrnCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/22/1998
*! Purpose   : Print Procedure, Adjust variables to print.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Code.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPrnCode
*!*************************************************************
*!
PROCEDURE lpPrnCode
PAGENO  = 0

R_TITLE = 'Fabric Projection Report'
HBREAK  = 'STYLE'
DO PRNTLOOP                          && Main report printing

R_TITLE = 'Fabric Projection Recap'
HBREAK  = 'FABRIC'
DO PRNTLOOP                          && Recap printing
*-- end of lpPrnCode.


****************************************************************************
*                                                                          *
*                          Procedure : PRNTLOOP                            *                              *
*                                                                          *
****************************************************************************
PROCEDURE PRNTLOOP

*
* The main printing loop for report and recap
*
GO TOP
*** INITIALIZ ALL SUBTOTALS AND GRAN TOTALS
STORE 0 TO XSTOT1,XSTOT2,XSTOT3,XSTOT4,XSTOT5,XSTOT6,XSTOT7,XSTOT8
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT4,XGTOT5,XGTOT6,XGTOT7,XGTOT8
BREAK = &HBREAK

*-- YMA 06/14/94 The old layout.
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                                                      ...........SALES............   ........PROJECTION.........   ..BAL. TO SELL..
* STYLE           CLR DESCRIPTIONS    PRICE    YIELD   UNITS     AMOUNT     YARDAGE   UNITS     AMOUNT    YARDAGE   UNITS     AMOUNT
* ±±±±±±±±±±±±±±± ±±± ±±±±±±±±±±±±± ±±±±.±±  ±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±± ±±±±±± ±±±±±±±.±±
* 123456789*12345 123 123456789*123 1234.12  123.123  123456  123456.12  123456.123  123456  123456.12  123456.123 123456 1234567.12
* ±±±±±±±±±±±±±±± ±±± ±±±±±±±±±±±±± ±±±±.±±  ±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±± ±±±±±± ±±±±±±±.±±

*-- YMA 06/14/94 The new layout.
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                                                      ...........SALES............   ........PROJECTION..........  ..BAL. TO SELL..
* STYLE        COLOR  DESCRIPTIONS    PRICE    YIELD   UNITS     AMOUNT     YARDAGE   UNITS     AMOUNT     YARDAGE  UNITS     AMOUNT
* ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±± ±±±±.±±  ±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±± ±±±±±± ±±±±±±±.±±
* 123456789*12 123456 123456789*123 1234.12  123.123  123456  123456.12  123456.123  123456  123456.12  123456.123 123456 1234567.12
* ±±±±±±±±±±±± ±±±±±± ±±±±±±±±±±±±± ±±±±.±±  ±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±±  ±±±±±±  ±±±±±±.±±  ±±±±±±.±±± ±±±±±± ±±±±±±±.±±

*-- YMA 06/14/94 The old layout.
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* FABRIC  CLR  DESCRIPTIONS         ONORDER   YARD SALES   YARDS CUT  YARDS BAL TO SELL
* ±±±±±±± ±±±  ±±±±±±±±±±±±±±±±±±±± ±±±±±±±   ±±±±±±.±±±  ±±±±±±.±±±         ±±±±±±.±±±
* 1234567 123  123456789*123456789* 1234567   123456.123  123456.123         123456.123
* ±±±±±±± ±±±  ±±±±±±±±±±±±±±±±±±±± ±±±±±±±   ±±±±±±.±±±  ±±±±±±.±±±         ±±±±±±.±±±
* SUBTOTAL FOR FABRIC ±±±±±±±=>    ±±±±±±±±  ±±±±±±±.±±± ±±±±±±±.±±±        ±±±±±±±.±±±


*-- YMA 06/14/94 The new layout.
* 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* FABRIC  COLOR  DESCRIPTIONS         ONORDER  YARD SALES   YARDS CUT  YARDS BAL TO SELL
* ±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±  ±±±±±±.±±±  ±±±±±±.±±±         ±±±±±±.±±±
* 1234567 123456 123456789*123456789* 1234567  123456.123  123456.123         123456.123
* ±±±±±±± ±±±±±± ±±±±±±±±±±±±±±±±±±±± ±±±±±±±  ±±±±±±.±±±  ±±±±±±.±±±         ±±±±±±.±±±
* SUBTOTAL FOR FABRIC ±±±±±±±=>      ±±±±±±±± ±±±±±±±.±±± ±±±±±±±.±±±        ±±±±±±±.±±±


DO PAGEHDR                            && Print 1st page  header
DO GROPHDR                            && Print 1st group header

DO WHILE .NOT. EOF()
  IF ROW > 51
    DO PAGEHDR                        && Page break if end of page
  ENDIF
  IF BREAK <> &HBREAK
    DO GROPFOT                        && Print group subtotal
    DO GROPHDR                        && Print new group header
    BREAK = &HBREAK                   && Update break flag
  ENDIF
  DO DETAIL                           && Print group detail of report or
    ROW=ROW+1                         && recap according to hbreak flag
  IF HBREAK='STYLE'                   && In case of recap printing there is
    SKIP                              && SUM statment moving record pointer
  ENDIF
ENDDO

DO GROPFOT                            && Print last group subtotal
DO GRANDTOT                           && Print Grand total of all groups
RETURN




****************************************************************************
*                                                                          *
*                            Procedure: PAGEHDR                            *
*                                                                          *
****************************************************************************
PROCEDURE PAGEHDR
*
*Print page header at the begining of all pages
*
PAGENO = PAGENO+1

DO RPT_HDR WITH XREPORT,lcRpOpTitl,R_WIDTH
DO CASE
  CASE HBREAK = 'STYLE'
    @ 005,000 SAY '                                                     ...........SALES............   ........PROJECTION..........  ..BAL. TO SELL..'   && YMA 06/14/94
    @ 006,000 SAY lcStyTitle + ' DESCRIPTIONS    PRICE    YIELD   UNITS     AMOUNT     YARDAGE   UNITS     AMOUNT     YARDAGE  UNITS     AMOUNT'   && YMA 06/14/94
  CASE HBREAK = 'FABRIC'
    @ 005,000 SAY '*'
    @ 006,000 SAY 'FABRIC  COLOR  DESCRIPTIONS         ONORDER  YARD SALES   YARDS CUT  YARDS BAL TO SELL'   && YMA 06/14/94
ENDCASE
@ 007,000 SAY REPLICATE('=',132)
ROW = 8

RETURN



****************************************************************************
*                                                                          *
*                            Procedure: GROPHDR                            *
*                                                                          *
****************************************************************************
PROCEDURE GROPHDR

*
*Print group header
*
IF .NOT.EOF()
  @ ROW,000 SAY &HBREAK
ENDIF
RETURN

****************************************************************************
*                                                                          *
*                              Procedure: DETAIL                           *
*                                                                          *
****************************************************************************
PROCEDURE DETAIL
*
*Print all detail lines to both report and recap
*

lcNnMjPic = gfItemMask('PN')

DO CASE
  CASE HBREAK = 'STYLE'

    @ ROW,lnMajorLen+LEN(lcSepRate) SAY cNonMajor PICTURE lcNnMjPic

    @ ROW,020 SAY DESC      PICTURE 'XXXXXXXXXXXXX'   && TAK 03/01/94
    @ ROW,034 SAY PRICEA    PICTURE '9999.99'
    @ ROW,043 SAY YIELD     PICTURE '999.999'
    @ ROW,052 SAY SUNITS    PICTURE '999999'
    @ ROW,060 SAY SAMOUNT   PICTURE '999999.99'
    @ ROW,071 SAY SYARDGE   PICTURE '999999.999'
    @ ROW,083 SAY TOTPROJ   PICTURE '999999'
    @ ROW,091 SAY PAMOUNT   PICTURE '999999.99'
    @ ROW,102 SAY PYARDGE   PICTURE '999999.999'
    @ ROW,113 SAY BUNITS    PICTURE '999999'
    @ ROW,120 SAY BAMOUNT   PICTURE '9999999.99'
    *** ADD THE SUBTOTALS AND GRAND TOTALS ***
    XSTOT1 = XSTOT1 + SUNITS
    XGTOT1 = XGTOT1 + SUNITS
    XSTOT2 = XSTOT2 + SAMOUNT
    XGTOT2 = XGTOT2 + SAMOUNT
    XSTOT3 = XSTOT3 + SYARDGE
    XGTOT3 = XGTOT3 + SYARDGE
    XSTOT4 = XSTOT4 + TOTPROJ
    XGTOT4 = XGTOT4 + TOTPROJ
    XSTOT5 = XSTOT5 + PAMOUNT
    XGTOT5 = XGTOT5 + PAMOUNT
    XSTOT6 = XSTOT6 + PYARDGE
    XGTOT6 = XGTOT6 + PYARDGE
    XSTOT7 = XSTOT7 + BUNITS
    XGTOT7 = XGTOT7 + BUNITS
    XSTOT8 = XSTOT8 + BAMOUNT
    XGTOT8 = XGTOT8 + BAMOUNT

  CASE HBREAK = 'FABRIC'
    *-- YMA 06/14/94 Modified the coordenates.
    @ ROW,008  SAY ICLR      PICTURE 'XXXXXX'
    @ ROW,015  SAY IDESC     PICTURE 'XXXXXXXXXXXXXXXXXXXX'
    @ ROW,036  SAY ONORDER   PICTURE '9999999'
    XSTOT1=XSTOT1+ONORDER
    XGTOT1=XGTOT1+ONORDER

    KEY = FABRIC + ICLR
    SUM REST  SYARDGE , PYARDGE    ;
        WHILE (FABRIC+ICLR = KEY ) ;
        TO    XYSALES , XYCUT
    XYB2SALE = XYCUT - XYSALES

    @ ROW,045  SAY XYSALES   PICTURE '999999.999'
    @ ROW,057  SAY XYCUT     PICTURE '999999.999'
    @ ROW,076  SAY XYB2SALE  PICTURE '999999.999'
    *** ADD THE SUBTOTALS AND GRAND TOTALS ***
    XSTOT2=XSTOT2+XYSALES
    XGTOT2=XGTOT2+XYSALES
    XSTOT3=XSTOT3+XYCUT
    XGTOT3=XGTOT3+XYCUT
    XSTOT4=XSTOT4+XYB2SALE
    XGTOT4=XGTOT4+XYB2SALE

ENDCASE
RETURN




****************************************************************************
*                                                                          *
*                         Procedure: GRPFOOT                               *
*                                                                          *
****************************************************************************
PROCEDURE GROPFOT
*
*Print group subtotal for both report and recap
*
ROW = ROW+1
@ ROW,00 SAY 'SUBTOTAL FOR &HBREAK '+BREAK+'=>'
DO CASE
  CASE HBREAK = 'STYLE'
    @ ROW,051  SAY XSTOT1  PICTURE '9999999'
    @ ROW,059  SAY XSTOT2  PICTURE '9999999.99'
    @ ROW,070  SAY XSTOT3  PICTURE '9999999.999'
    @ ROW,082  SAY XSTOT4  PICTURE '9999999'
    @ ROW,090  SAY XSTOT5  PICTURE '9999999.99'
    @ ROW,101  SAY XSTOT6  PICTURE '9999999.999'
    @ ROW,112  SAY XSTOT7  PICTURE '9999999'
    @ ROW,119  SAY XSTOT8  PICTURE '99999999.99'
  CASE HBREAK = 'FABRIC'
    *-- YMA 06/14/94 Modified the coordenates.
    @ ROW,035  SAY XSTOT1  PICTURE '99999999'
    @ ROW,044  SAY XSTOT2  PICTURE '9999999.999'
    @ ROW,056  SAY XSTOT3  PICTURE '9999999.999'
    @ ROW,075  SAY XSTOT4  PICTURE '9999999.999'
ENDCASE
STORE 0 TO XSTOT1,XSTOT2,XSTOT3,XSTOT4,XSTOT5,XSTOT6,XSTOT7,XSTOT8

IF .NOT. EOF()
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('=',132)
  ROW = ROW+1
ENDIF

RETURN




****************************************************************************
*                                                                          *
*                           Procedure: GRANDTOT                            *
*                                                                          *
****************************************************************************
PROCEDURE GRANDTOT
*
*Print Grand total of all groups for both report and recap
*
ROW=ROW+1
@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW+1
@ ROW,00 SAY '* GRAND TOTAL *'
DO CASE
  CASE HBREAK = 'STYLE'
    @ ROW,051  SAY XGTOT1  PICTURE '9999999'
    @ ROW,059  SAY XGTOT2  PICTURE '9999999.99'
    @ ROW,070  SAY XGTOT3  PICTURE '9999999.999'
    @ ROW,082  SAY XGTOT4  PICTURE '9999999'
    @ ROW,090  SAY XGTOT5  PICTURE '9999999.99'
    @ ROW,101  SAY XGTOT6  PICTURE '9999999.999'
    @ ROW,112  SAY XGTOT7  PICTURE '9999999'
    @ ROW,119  SAY XGTOT8  PICTURE '99999999.99'
  CASE HBREAK = 'FABRIC'
    *-- YMA 06/14/94 Modified the coordenates.
    @ ROW,035  SAY XGTOT1  PICTURE '99999999'
    @ ROW,044  SAY XGTOT2  PICTURE '9999999.999'
    @ ROW,056  SAY XGTOT3  PICTURE '9999999.999'
    @ ROW,075  SAY XGTOT4  PICTURE '9999999.999'
ENDCASE
ROW = ROW+1
@ ROW,00 SAY REPLICATE('*',132)
RETURN

*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!038431 (Bld41)
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  CLEAR READ  
  RETURN .F.
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.