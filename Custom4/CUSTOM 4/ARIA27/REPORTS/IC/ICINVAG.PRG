*:***************************************************************************
*: Program file  : ICINVAG.PRG
*: Program desc. : INVENTORY AGAING REPORT.
*: Date          : 02/24/2002
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: E301845               Refer to: (C#102256 & 200210)
*:
*:***************************************************************************
*: Calls :
*:   Procedures :
*:   Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICINVAG
*:***************************************************************************
*: Modifications :
*:B120302,1 MMM 10/26/2003 Fix Bug printing the column headings on page 1 only  when The  
*:                          Total is printed at first line of a second page. [Fix in FRX]. 
*:B128627,1 EIH 08/17/2005 Fix Bug in case of extended size scale QTY's and values are invalid [Fix in FRX]. 
*****************************************************************************

*-- lnMajorLen     && Holds the major length.
*-- Major length

lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
STORE '' TO lcColorTltm,lcSepart
DIMENSION laAging[5]
STORE 0 TO laAging,lnTotSty,lnTotVal
lcStkVal = "IIF(STYINVJL.NTOTSTK>=lnStkQty,lnStkQty,STYINVJL.NTOTSTK)*STYLE.Ave_Cost"

*B128627,1 EIH 08/17/2005 Define llMScale to now if extended size scale or not [Begin]. 
STORE .F. TO llMScale
*--Use extended size scale.  
llMScale   = gfGetMemVar('M_USEEXSSC')
*B128627,1 EIH 08/17/2005 [End]. 


IF llOgFltCh
  *--Empty Temp File
  IF USED(lcWrkTmp) AND RECCOUNT(lcWrkTmp) >0
    SELECT (lcWrkTmp)
    ZAP
  ENDIF
  *--Prepare filter
  lcRpExp  = lcRpExp + ' AND TOTSTK <>0'
  *--Check for styles needed
  WAIT 'Collecting data... Please wait' WINDOW NOWAIT
  SELECt STYLE
  *--Clear filter as it may be filtered from a previous run.
  SET FILTER TO
  LOCATE FOR &lcRpExp
  IF EOF()
    *-- Message : There are no records to display...!
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
  *--Collect data
  =lpCollect()
ENDIF

SELECT (lcWrkTmp)
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
lcRPFormNa = IIF(lcRpFormat = 'D','ICINVGD','ICINVGS')
DO gfDispRe WITH EVAL('lcRPFormNa')

                       *-- End of the Program --*
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
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
*! Purpose   : Rerport When function.
*!*************************************************************
*! Called from : SYDREPRT.DBF
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
  SET ORDER TO TAG STYINVJL DESCENDING
ENDIF
IF !USED(lcWrkTmp)
  *B128627,1 EIH 08/17/2005 Add scale field in temperary file [Begin]. 
  *CREATE TABLE (gcWorkDir+lcWrkTmp) (cStyGroup C(6)      , STY C(19)          , nRecv N(7)      ,;
                                     Style C(lnMajorLen) , StyShrtD C(15)     , StyLongD C(30)  ,;
                                     COLOR C(lnColorLen) , ColorDsc C(15)     , Total N(7)      ,;
                                     UnitCost N(7,2)     , CostVal N(12,2)    , nAge90 N(12,2)  ,;
                                     nAge180 N(12,2)     , nAge270 N(12,2)    , nAge360 N(12,2) ,;
                                     nAgeAbv N(12,2)     , Adjustmnts N(12,2))
  
  *INDEX ON CSTYGROUP + STYLE + COLOR TAG (lcWrkTmp)
  
  CREATE TABLE (gcWorkDir+lcWrkTmp) (cStyGroup C(6)      , STY C(19)          , nRecv N(7)      ,;
                                     Style C(lnMajorLen) , StyShrtD C(15)     , StyLongD C(30)  ,;
                                     COLOR C(lnColorLen) , ColorDsc C(15)     , Total N(7)      ,;
                                     UnitCost N(7,2)     , CostVal N(12,2)    , nAge90 N(12,2)  ,;
                                     nAge180 N(12,2)     , nAge270 N(12,2)    , nAge360 N(12,2) ,;
                                     nAgeAbv N(12,2)     , Adjustmnts N(12,2) , Scale C(3) )
  INDEX ON CSTYGROUP + STYLE + COLOR + Scale TAG (lcWrkTmp)
  *B128627,1 EIH 08/17/2005 [End]. 
                                       
  
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
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
lcIMjrPt  = gfItemMask('PI')
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

RETURN ''

*--End of lfEvalSegs.
*!*************************************************************
*! Name      : lpCollect
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
*! Purpose   : Function to Collect the data.
*!*************************************************************
*! Called from : ICINVAG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpCollect()
*!*************************************************************
FUNCTION lpCollect

DIMENSION laAging[5]
STORE 0 TO laAging,lnTotSty,lnTotVal
*--
SELECT STYLE
SCAN FOR &lcRpExp
  WAIT WINDOW  lcStyMajor + ' group\'+lcStyMajor + ': ' + cstygroup + '\' + Style.cStyMajor NOWAIT
  SELECT STYINVJL
  IF STYLE.TOTSTK > 0 AND lfRecv()
    WAIT WINDOW lcStyMajor + ' group\' + lcStyMajor+'-'+lcColorTlt+ ': ' +style.cstygroup+'\'+ style NOWAIT
    lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
    lnStkQty = STYLE.TOTSTK
                    
    SCAN REST WHILE STYLE + CWARECODE + CSESSION + DTOS(DTRDATE) + CTRCODE + STR(LINENO,6) = Style.style ;
              FOR CIRTYPE = 'R' AND CTRTYPE $ '56'
      ldDate = STYINVJL.DTRDATE
      SELECT (lcWrkTmp)
      *B128627,1 EIH 08/17/2005 If not extended size scale [Begin]. 
      *lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
      IF !llMScale
        lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen)
      ELSE
        lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen) + STYLE.SCALE
      ENDIF
      *B128627,1 EIH 08/17/2005 [End]. 
      
      
      IF !SEEK(lcKey)
        APPEND BLANK
        REPLACE cStyGroup WITH Style.cStyGroup                        ,;
                Style     WITH SUBSTR(Style.style,1,lnMajorLen)       ,;
                StyShrtD  WITH Style.Desc                             ,;
                StyLongD  WITH Style.Desc1                            ,;
                Color     WITH SUBSTR(Style.style,lnClrPo,lnColorLen) ,;
                ColorDsc  WITH SUBSTR(gfCodDes(SUBSTR(Style.style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                Total     WITH Style.TotStk                           ,;
                UnitCost  WITH Style.Ave_Cost                         ,;
                CostVal   WITH Style.nStkVal                          ,;
                STY       WITH STYLE.STYLE  
                
        
        *B128627,1 EIH 08/17/2005 If not extended size scale [Begin]. 
        IF llMScale
           REPLACE  Scale      WITH Style.Scale                                     
        ENDIF
        *B128627,1 EIH 08/17/2005 [End]. 
        
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

    *B128627,1 EIH 08/17/2005 If extended size scale [Begin]. 
    IF llMScale
      lcKey = Style.cstyGroup + SUBSTR(Style.cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(Style.style,lnClrPo,lnColorLen) + STYLE.SCALE
    ENDIF  
    *B128627,1 EIH 08/17/2005 [End].       

    IF !SEEK(lcKey)
      APPEND BLANK
      REPLACE cStyGroup  WITH Style.cStyGroup                        ,;
              Style      WITH SUBSTR(Style.style,1,lnMajorLen)       ,;
              StyShrtD   WITH Style.Desc                             ,;
              StyLongD   WITH Style.Desc1                            ,;
              Color      WITH SUBSTR(Style.style,lnClrPo,lnColorLen) ,;
              ColorDsc   WITH SUBSTR(gfCodDes(SUBSTR(Style.style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
              Total      WITH Style.TotStk                           ,;
              UnitCost   WITH Style.Ave_Cost                         ,;
              CostVal    WITH Style.nStkVal                          ,;
              Adjustmnts WITH Style.nStkVal

      lnTotSty = lnTotSty+Style.TotStk
      lnTotVal = lnTotVal+Style.nStkVal

      *B128627,1 EIH 08/17/2005 If not extended size scale [Begin]. 
      IF llMScale
        REPLACE Scale      WITH Style.Scale
      ENDIF  
      *B128627,1 EIH 08/17/2005 [End].       
    ENDIF
  ENDIF
ENDSCAN

SELECT (lcWrkTmp)
SCAN
  =SEEK(&lcWrkTmp..STY,'STYLE')
  *--if the total quantities recived is less than the stock
  IF &lcWrkTmp..nRecv < STYLE.TOTSTK
    REPLACE Adjustmnts WITH (STYLE.TOTSTK-nRecv)*STYLE.AVE_COST
  ENDIF
ENDSCAN

*--End of lpCollect.
*!*************************************************************
*! Name      : lfRecv
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 02/24/2002
*! Purpose   : Check that there is at least one receiving for the current style
*!*************************************************************
*! Called from : ICINVAG.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfRecv()
*!*************************************************************
FUNCTION lfRecv
PRIVATE lcSlct,llFound

lcSlct  = SELECT(0)
llFound = .F.
IF SEEK(Style.Style,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE STYLE + CWARECODE + CSESSION + DTOS(DTRDATE) + CTRCODE + STR(LINENO,6) = STYLE.STYLE ;
            FOR CIRTYPE = 'R' AND CTRTYPE $'56'
    llFound = .T.
    EXIT
  ENDSCAN
ENDIF
SELECT (lcSlct)
RETURN llFound

*--End of lfRecv.