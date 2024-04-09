*:***************************************************************************
*: Program file  : ICINVAG.PRG
*: Program desc. : INVENTORY AGAING REPORT.
*: Date          : 06/06/2006
*: System        : Aria 4 XP
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Wael M. ABo-Shawareb (WSH)
*: Tracking Job #: E039570
*:***************************************************************************
*: Calls :
*:   Procedures :
*:   Functions  :
*:***************************************************************************
*: Example : DO ICINVAG
*: T20060809.0030,1 MMT Convert layout to Graphics
*: B608209,1 SSH Add season and devision filter
*: B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [T20180619.0001]
*:***************************************************************************

DIMENSION laAging[5]
STORE ''  TO lcColorTltm, lcSepart
STORE ''  TO lcStyFltr, lcGrpFltr
*: B608209,1 SSH Add season and devision filter
STORE ''  TO lcSesFltr, lcDivFltr
*: B608209,1 SSH Add season and devision filter
STORE .F. TO llMScale
STORE 0   TO laAging, lnTotSty, lnTotVal

lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
lcStkVal   = "IIF(STYINVJL.NTOTSTK >= lnStkQty, lnStkQty, STYINVJL.NTOTSTK) * STYLE.Ave_Cost"
llMScale   = gfGetMemVar('M_USEEXSSC')

IF loOgScroll.llOGFltCh
  lcStTime = TIME()
  
  *--Empty Temp File
  IF USED(lcWrkTmp) AND RECCOUNT(lcWrkTmp) > 0
    SELECT (lcWrkTmp)
    ZAP
  ENDIF
  
  *--Prepare filter
  =lfGetFilters()
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp), '', ' AND ') + 'STYLE.TOTSTK <> 0'
  
  *--Collect data
  =lpCollect()
  
  lcEdTime   = TIME()
  lnInterval = lfCollTime(lcStTime, lcEdTime)
  
  WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcWrkTmp))) + " Records in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
ENDIF

SELECT (lcWrkTmp)
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

loogScroll.cCROrientation = 'L'


*T20060809.0030,1 MMT Convert layout to Graphics[Start]
*lcRPFormNa = IIF(lcRpFormat = 'D', 'ICINVGD', 'ICINVGS')
*T20060809.0030,1 MMT Convert layout to Graphics[End]

DO gfDispRe WITH EVALUATE('lcRPFormNa')
RETURN
*-- End of the Program

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
*! Purpose   : Raise change style flag, in range browse screen.
*!*************************************************************
*! Called from : ICINVAG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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
    llChStyle = .T.

  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*--End of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
*! Purpose   : Sum a specific field for the current style in style file.
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar

PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
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
*--End of lfStySum.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Rerport When function.
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [Start]

*=gfOpenTable(oAriaApplication.DataDir + 'STYINVJL', 'STYINVJL', 'SH')
=gfOpenTable(oAriaApplication.DataDir + 'STYINVJL', 'STYDATE', 'SH')
*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [End]

=gfOpenTable(oAriaApplication.DataDir + 'STYLE', 'CSTYLE', 'SH')
=gfOpenTable(oAriaApplication.DataDir + 'CODES', 'CODES', 'SH')

SELECT Codes
SET ORDER TO CODES

IF !USED(lcWrkTmp)
  CREATE TABLE (gcWorkDir+lcWrkTmp) (cStyGroup C(6)      , STY C(19)          , nRecv N(7)      ,;
                                     Style C(lnMajorLen) , StyShrtD C(15)     , StyLongD C(30)  ,;
                                     COLOR C(lnColorLen) , ColorDsc C(15)     , Total N(7)      ,;
                                     UnitCost N(7,2)     , CostVal N(12,2)    , nAge90 N(12,2)  ,;
                                     nAge180 N(12,2)     , nAge270 N(12,2)    , nAge360 N(12,2) ,;
                                     nAgeAbv N(12,2)     , Adjustmnts N(12,2) , Scale C(3)      ,;
                                     TOTSTK  N(7)        , AVE_COST N(7,2))
  INDEX ON CSTYGROUP + STYLE + COLOR + Scale TAG (lcWrkTmp)
ENDIF
*--End of lfwRepWhen.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
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
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

*-- Major length
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''

*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnClrPo = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'

*-- Compute Free/Color Items in Style Structure. [End]
lcIMjrPt   = gfItemMask('PI')
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)
lcSepart   = SUBSTR(lcIMjrPt,lnstylewid+1,1)

RETURN ''
*--End of lfEvalSegs.

*!*************************************************************
*! Name      : lpCollect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Example     : =lpCollect()
*!*************************************************************
FUNCTION lpCollect

LOCAL lnOldOrd
lnOldOrd = ORDER("STYLE")

SELECT STYLE
=gfSetOrder("STYLE")

SELECT STYINVJL

*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [Start]

*=gfSetOrder("STYINVJL")
 =gfSetOrder("STYDATE")
*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [End]


*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [Start]
*SET ORDER TO TAG STYINVJL DESCENDING
 SET ORDER TO TAG STYDATE DESCENDING

*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [End]

WAIT 'Collecting data... Please wait' WINDOW NOWAIT

DIMENSION laAging[5]
STORE 0 TO laAging, lnTotSty, lnTotVal

*-- If user select Styles from OG ...
IF !EMPTY(lcStyFltr) AND USED(lcStyFltr)
  SELECT (lcStyFltr)
  SCAN
    SELECT STYLE
    =gfSeek(SUBSTR(EVALUATE(lcStyFltr + '.CSTYMAJOR'), 1, lnMajorLen))
    SCAN REST WHILE STYLE = SUBSTR(EVALUATE(lcStyFltr + '.CSTYMAJOR'), 1, lnMajorLen) FOR &lcRpExp
      WAIT WINDOW  lcStyMajor + ' Group\'+lcStyMajor + ': ' + cstygroup + '\' + Style.cStyMajor NOWAIT
      
      IF !EMPTY(lcGrpFltr) AND !SEEK(STYLE.CSTYGROUP, lcGrpFltr)
        LOOP
      ENDIF
*: B608209,1 SSH Add season and devision filter
      IF !EMPTY(lcSesFltr) AND !SEEK(STYLE.SEASON, lcSesFltr)
        LOOP
      ENDIF

      IF !EMPTY(lcDivFltr) AND !SEEK(STYLE.CDIVISION, lcDivFltr)
        LOOP
      ENDIF
*: B608209,1 SSH Add season and devision filter
      
            
      =lfStyCollect()
    ENDSCAN
  ENDSCAN
ELSE
  SELECT STYLE
  llExitLoop = !gfGoTop()
  DO WHILE !llExitLoop
    WAIT WINDOW  lcStyMajor + ' Group\' + lcStyMajor + ': ' + cstygroup + '\' + Style.cStyMajor NOWAIT
    
    llLoop = !(&lcRpExp.)
    llLoop = llLoop OR (!EMPTY(lcGrpFltr) AND !SEEK(STYLE.CSTYGROUP, lcGrpFltr))
*: B608209,1 SSH Add season and devision filter
    llLoop = llLoop OR (!EMPTY(lcSesFltr) AND !SEEK(STYLE.SEASON, lcSesFltr))
    llLoop = llLoop OR (!EMPTY(lcDivFltr) AND !SEEK(STYLE.CDIVISION, lcDivFltr))
*: B608209,1 SSH Add season and devision filter
    IF llLoop
      SELECT STYLE
      llExitLoop = !gfGoNext()
      LOOP
    ENDIF  
    
    =lfStyCollect()
    
    SELECT STYLE
    llExitLoop = !gfGoNext()
  ENDDO
ENDIF

SELECT (lcWrkTmp)
SCAN
  *--if the total quantities recived is less than the stock
  IF nRecv < TOTSTK
    REPLACE Adjustmnts WITH (TOTSTK - nRecv) * AVE_COST
  ENDIF
ENDSCAN

SELECT STYLE
=gfSetOrder(lnOldOrd)

SELECT (lcWrkTmp)
LOCATE

RETURN
*--End of lpCollect.

*!*************************************************************
*! Name      : lfStyCollect
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Function to get a style data.
*!*************************************************************
*! Example   : =lfStyCollect()
*!*************************************************************
FUNCTION lfStyCollect

SELECT STYINVJL
IF STYLE.TOTSTK > 0 AND lfRecv()
  WAIT WINDOW lcStyMajor + ' group\' + lcStyMajor+'-'+lcNonMajTl+ ': ' +style.cstygroup+'\'+ style NOWAIT
  
  lcKey    = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
  lnStkQty = STYLE.TOTSTK
 
  
*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [Start]
*SCAN REST WHILE STYLE + CWARECODE + CSESSION + DTOS(DTRDATE) + CTRCODE + STR(LINENO,6) = Style.style ;
            FOR CIRTYPE = 'R' AND CTRTYPE $ '56'
       
            SCAN REST WHILE STYLE + CWARECODE + DTOS(DTRDATE) + CSESSION +cirtype = Style.style ;
            FOR CIRTYPE = 'R' AND CTRTYPE $ '56'
            
*B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [End]     
                    
            
    ldDate = STYINVJL.DTRDATE
    
    SELECT (lcWrkTmp)
    IF !llMScale
      lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
    ELSE
      lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen) + STYLE.SCALE
    ENDIF
    
    IF !SEEK(lcKey)
      APPEND BLANK
      REPLACE cStyGroup WITH Style.cStyGroup                        ,;
              Style     WITH SUBSTR(Style.style,1,lnMajorLen)       ,;
              StyShrtD  WITH Style.Desc                             ,;
              StyLongD  WITH Style.Desc1                            ,;
              Color     WITH SUBSTR(Style.style,lnClrPo,lnColorLen) ,;
              ColorDsc  WITH SUBSTR(lfCodeDesc(SUBSTR(Style.style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
              Total     WITH Style.TotStk                           ,;
              UnitCost  WITH Style.Ave_Cost                         ,;
              CostVal   WITH Style.nStkVal                          ,;
              STY       WITH STYLE.STYLE                            ,;
              AVE_COST  WITH STYLE.AVE_COST                         ,;
              TOTSTK    WITH STYLE.TOTSTK
      IF llMScale
         REPLACE Scale WITH Style.Scale
      ENDIF
      
      lnTotSty = lnTotSty+Style.TotStk
      lnTotVal = lnTotVal+Style.nStkVal
    ENDIF
    
    DO CASE
      CASE BETWEEN(DATE()-ldDate,0,90)
        REPLACE nAge90  WITH nAge90  + EVALUATE(lcStkVal)
        laAging[1] = laAging[1] + EVALUATE(lcStkVal)
      CASE BETWEEN(DATE()-ldDate,91,180)
        REPLACE nAge180 WITH nAge180 + EVALUATE(lcStkVal)
        laAging[2] = laAging[2] + EVALUATE(lcStkVal)
      CASE BETWEEN(DATE()-ldDate,181,270)
        REPLACE nAge270 WITH nAge270 + EVALUATE(lcStkVal)
        laAging[3] = laAging[3] + EVALUATE(lcStkVal)
      CASE BETWEEN(DATE()-ldDate,271,360)
        REPLACE nAge360 WITH nAge360 + EVALUATE(lcStkVal)
        laAging[4] = laAging[4] + EVALUATE(lcStkVal)
      CASE DATE()-ldDate > 360
        REPLACE nAgeAbv WITH nAgeAbv + EVALUATE(lcStkVal)
        laAging[5] = laAging[5] + EVALUATE(lcStkVal)
    ENDCASE
    lnStkQty = lnStkQty - STYINVJL.NTOTSTK
    
    *--Increase nRecv field with the value recived.
    REPLACE nRecv WITH nRecv + STYINVJL.nTotStk
    
    *--If the stock quantity is less than the total recivings no need to complete the loop.
    IF lnStkQty<=0
      EXIT
    ENDIF
  ENDSCAN
ELSE
  SELECT (lcWrkTmp)
  
  lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
  IF llMScale
    lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen) + STYLE.SCALE
  ENDIF  
  
  IF !SEEK(lcKey)
    APPEND BLANK
    REPLACE cStyGroup  WITH Style.cStyGroup                        ,;
            Style      WITH SUBSTR(Style.style,1,lnMajorLen)       ,;
            StyShrtD   WITH Style.Desc                             ,;
            StyLongD   WITH Style.Desc1                            ,;
            Color      WITH SUBSTR(Style.style,lnClrPo,lnColorLen) ,;
            ColorDsc   WITH SUBSTR(lfCodeDesc(SUBSTR(Style.style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
            Total      WITH Style.TotStk                           ,;
            UnitCost   WITH Style.Ave_Cost                         ,;
            CostVal    WITH Style.nStkVal                          ,;
            Adjustmnts WITH Style.nStkVal
    IF llMScale
      REPLACE Scale WITH Style.Scale
    ENDIF
    
    lnTotSty = lnTotSty+Style.TotStk
    lnTotVal = lnTotVal+Style.nStkVal
  ENDIF
ENDIF
*--End of lpCollect.

*!*************************************************************
*! Name      : lfRecv
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Check that there is at least one receiving for the current style
*!*************************************************************
*! Example     : =lfRecv()
*!*************************************************************
FUNCTION lfRecv
PRIVATE lcSlct, llFound

lcSlct  = SELECT(0)
llFound = .F.

IF gfSEEK(Style.Style, 'STYINVJL')
  SELECT STYINVJL
  
            
               *B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [Start]
            *SCAN REST WHILE STYLE + CWARECODE + CSESSION + DTOS(DTRDATE) + CTRCODE + STR(LINENO,6) = STYLE.STYLE ;
            FOR CIRTYPE = 'R' AND CTRTYPE $ '56'
           SCAN REST WHILE STYLE + CWARECODE + DTOS(DTRDATE) + CSESSION +cirtype = STYLE.STYLE ;
            FOR CIRTYPE = 'R' AND CTRTYPE $ '56'
  *B611609,1 Es 07/29/2018 Modify this issue ->"The Inventory Aging report displays the style aged qty in wrong period at DCC" [End]
 
    llFound = .T.
    EXIT
  ENDSCAN
ENDIF

SELECT (lcSlct)
RETURN llFound
*--End of lfRecv.

*************************************************************
*! Name      : lfGetFilters
*! Developer : Wael M. Abo-Shaweareb (WSH)
*! Date      : 06/06/2006
*! Purpose   : Get Optiongrid Filter Cursors
*!*************************************************************
FUNCTION lfGetFilters

LOCAL lcStatus, lnAlias, lcCurName, llFound, lcCond
lnAlias = SELECT(0)
lcRpExp = ".T."

*-- Status Filter
lcStatus = lfCheckFilter(1, "STYLE.STATUS")
lcRpExp  = IIF(EMPTY(lcStatus), "", "STYLE.STATUS $ '" + lcStatus + "'")

*-- Style Filter
lcCurName = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcStyFltr = lcCurName
  SELECT (lcStyFltr)
  INDEX ON CSTYMAJOR TAG (lcStyFltr)
ELSE
  IF TYPE("lcStyFltr") = "C" AND USED(lcStyFltr)
    USE IN (lcStyFltr)
  ENDIF
  lcStyFltr = ''
ENDIF

*-- Group Filter
lcCond = lfCheckFilter(1, 'STYLE.CSTYGROUP')
IF !EMPTY(lcCond)
  lcGrpFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcGrpFltr) (CSTYGROUP C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcGrpFltr)
  INDEX ON CSTYGROUP TAG (lcGrpFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE CSTYGROUP WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcGrpFltr") = "C" AND USED(lcGrpFltr)
    USE IN (lcGrpFltr)
  ENDIF
  lcGrpFltr = ''
ENDIF


*-- Season Filter
lcCond = lfCheckFilter(1, 'STYLE.SEASON')
IF !EMPTY(lcCond)
  lcSesFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcSesFltr) (SEASON C(6))
  DIMENSION laValues[1] 
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcSesFltr)
  INDEX ON SEASON TAG (lcSesFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE Season WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcSesFltr") = "C" AND USED(lcSesFltr)
    USE IN (lcSesFltr)
  ENDIF
  lcSesFltr = ''
ENDIF

*-- DIVISION Filter
lcCond = lfCheckFilter(1, 'STYLE.CDIVISION')
IF !EMPTY(lcCond)
  lcDivFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcDivFltr) (CDIVISION C(6))
  DIMENSION laValues[1] 
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcDivFltr)
  INDEX ON CDIVISION TAG (lcDivFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE CDIVISION WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcDivFltr") = "C" AND USED(lcDivFltr)
    USE IN (lcDivFltr)
  ENDIF
  lcDivFltr = ''
ENDIF
  

SELECT (lnAlias)
RETURN

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS   
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*!
FUNCTION lfCollTime
LPARAMETERS lcStart, lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))

RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Get code description
*!*************************************************************
*! Example   : =lfCodeDesc(Reason, "REASON")
*!*************************************************************
FUNCTION lfCodeDesc
LPARAMETERS lcCodeVal, lcCodeFld

LOCAL lnAlias, lcRetVal
lnAlias  = SELECT(0)
lcRetVal = ''

SELECT Codes
IF gfSeek("N" + lcCodeVal + "N" + lcCodeFld)
  lcRetVal = cDiscrep
ENDIF

SELECT (lnAlias)
RETURN lcRetVal
*!*************************************************************
*! Name      : lfVldLayout
*! Developer : Mariam Mazhar [MMt]
*! Date      : 09/20/2006
*! Purpose   : refresh layout
*: T20060809.0030,1 MMT Convert layout to Graphics
*!*************************************************************
FUNCTION lfVldLayout
lcRPFormNa = IIF(lcRpFormat = 'D', 'ICINVGD', 'ICINVGS')
= lfRepPltFr(lcRPFormNa)