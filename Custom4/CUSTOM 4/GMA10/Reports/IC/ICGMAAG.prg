***************************************************************************************
*: Procedure file: ICGMAAG.PRG (Custom Inventory Report for GMA)
*:         System: ARIA4xp 
*:         Module: Inventory Control
*:         Author: Mariam mazhar [MMT]
*:         Date  : 10/03/2004
*:      Refer to : (C#038610)
*:***************************************************************************************

lnMajorLen = LEN(ALLTRIM(gfItemMask('PM'))) && Holds the major length.
STORE '' TO lcExlExp,lcColorTlt,lcSepart
DIMENSION laAging[5]
STORE 0 TO laAging,lnTotSty,lnTotVal
*lcStkVal = "IIF(&lcStyInvTmp..NTOTSTK>=lnStkQty,lnStkQty,&lcStyInvTmp..NTOTSTK)*&lcStyFnl..Ave_Cost"
lcStkVal = "IIF(STYINVJL.NTOTSTK>=lnStkQty,lnStkQty,STYINVJL.NTOTSTK)*&lcStyFnl..Ave_Cost"
*IF lluse_config AND llRpUseConfg = 'Y'
IF llOgFltCh
  *--Empty Temp File
  IF USED(lcWrkTmp) AND RECCOUNT(lcWrkTmp) >0
    SELECT (lcWrkTmp)
    ZAP
  ENDIF
  WAIT 'Collecting data... Please wait' WINDOW NOWAIT
  lcStyleSelctExp = 'TOTSTK <> 0 '
  *--check the user checked status
  =lfStatusCheck()

  *-- fields will be selected from the style file
  lcSelFldsStyle = "Style.Dept,Style.cStyMajor,Style.TotStk,Style.cStyGroup,Style.Style,Style.Desc ,Style.Desc1,;
                    Style.Ave_Cost,Style.nStkVal "

  *--the files will select from 
  lcTableStyle = " Style "

  *--replacing the field name with the whole file index
  lcStyleSelctExp = STRTRAN(lcStyleSelctExp,"STYLE.STATUS","STYLE.STATUS+STYLE.CSTYGROUP")

  *--checking the if user has select style groups
  =lfGroupCheck()

  *--checking the if user has select style majors
  =lfMajorCheck()
 *--Selecting records which satisfy the selection critria  from the style file 
 SELECT &lcSelFldsStyle FROM &lcTableStyle WHERE &lcStyleSelctExp INTO CURSOR &lcStyTmp
 
*!*     lnBuffering = CURSORGETPROP("Buffering",lcStyTmp)
*!*      =CURSORSETPROP("Buffering",3,lcStyTmp)
*!*     *-- creating index on the temp style file 
*!*     INDEX ON  Style TAG Style 
*!*     SET ORDER TO TAG STYLE 

 SELECT(lcStyTmp)
 SET RELATION TO 
 LOCATE 

 *--Prepare filter
 lcExlExp = lfPrepFil()
  *--Checking the user departments selection.
 =lfDeptCheck()

  *SELECT &lcStyTmp..* from &lcStyTmp,&lcDepDtTmp WHERE &lcStyTmp..cStyMajor = SUBSTR(&lcDepDtTmp..style,1,lnMajorLen) INTO CURSOR &lcStyFnl
  SELECT &lcStyTmp..* from &lcStyTmp INTO CURSOR &lcStyFnl
  lnBuffering = CURSORGETPROP("Buffering",lcStyFnl)
  =CURSORSETPROP("Buffering",3,lcStyFnl)
 *-- creating index on the temp style file 
  INDEX ON  Style TAG Style 
  SET ORDER TO TAG STYLE 

   *--Check for styles needed
  WAIT 'Collecting data... Please wait' WINDOW NOWAIT

  SELECT(lcStyFnl)
  SET FILTER TO
  LOCATE 

  IF EOF()
    *-- Message : There are no records to display...!
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
  *--Collect data
*  IF !(lluse_config AND llRpUseConfg = 'Y')
    DO lpCollect  
*!*      ELSE
*!*        =gfOpenFile(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
*!*        DO lpCollectDye
*!*      ENDIF 
ENDIF

SELECT (lcWrkTmp)
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*-- determining if user wants to print detials or summary
lcRPFormNa = IIF(lcRpFormat = 'D','ICGMAGD','ICGMAGS')

DO gfDispRe WITH EVAL('lcRPFormNa')

*!*************************************************************
*! Name      : lfsrvSty
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
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
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
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
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
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
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SET ORDER TO style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF 
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


*!***************************************************************************************
*! Name      : lfwRepWhen
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
*! Purpose   : Rerport When function
*!***************************************************************************************
FUNCTION lfwRepWhen

DECLARE laRpSource[3],laRpTarget[1]

lnStatus = lcRpStatus
*--Active|Hold|Canceled~A|H|X
STORE 'Active'      TO laRpSource[1]
STORE 'Hold'        TO laRpSource[2]
STORE 'Canceled'    TO laRpSource[3]

lcRpStatus = ''

IF !USED(lcWrkTmp)
  DIMENSION laTempacstru[20,4]
  laTempacstru[1,1]='Type'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 1
  laTempacstru[1,4]= 0

  laTempacstru[2,1]='Dept'
  laTempacstru[2,2]='C'
  laTempacstru[2,3]= 5
  laTempacstru[2,4]= 0
  
  laTempacstru[3,1]='cStyGroup'
  laTempacstru[3,2]='C' 
  laTempacstru[3,3]= 6
  laTempacstru[3,4]= 0
  
  laTempacstru[4,1]='Style'
  laTempacstru[4,2]='C' 
  laTempacstru[4,3]= lnMajorLen
  laTempacstru[4,4]= 0
  
  laTempacstru[5,1]='StyShrtD'
  laTempacstru[5,2]='C' 
  laTempacstru[5,3]= 15
  laTempacstru[5,4]= 0

  laTempacstru[6,1]='StyLongD'
  laTempacstru[6,2]='C' 
  laTempacstru[6,3]= 30
  laTempacstru[6,4]= 0
  
  laTempacstru[7,1]='COLOR'
  laTempacstru[7,2]='C' 
  laTempacstru[7,3]= lnColorLen
  laTempacstru[7,4]= 0
  
  laTempacstru[8,1]='ColorDsc'
  laTempacstru[8,2]='C' 
  laTempacstru[8,3]= 15
  laTempacstru[8,4]= 0
  
  laTempacstru[9,1]='Total'
  laTempacstru[9,2]='N' 
  laTempacstru[9,3]= 7
  laTempacstru[9,4]= 0
  
  laTempacstru[10,1]='UnitCost'
  laTempacstru[10,2]='N' 
  laTempacstru[10,3]= 7
  laTempacstru[10,4]= 2

  laTempacstru[11,1]='CostVal'
  laTempacstru[11,2]='N' 
  laTempacstru[11,3]= 12
  laTempacstru[11,4]= 2

  laTempacstru[12,1]='nAge90'
  laTempacstru[12,2]='N' 
  laTempacstru[12,3]= 12
  laTempacstru[12,4]= 2
  
  laTempacstru[13,1]='nAge180'
  laTempacstru[13,2]='N' 
  laTempacstru[13,3]= 12
  laTempacstru[13,4]= 2
  
  laTempacstru[14,1]='nAge270'
  laTempacstru[14,2]='N' 
  laTempacstru[14,3]= 12
  laTempacstru[14,4]= 2
  
  
  laTempacstru[15,1]='nAge360'
  laTempacstru[15,2]='N' 
  laTempacstru[15,3]= 12
  laTempacstru[15,4]= 2
  
  laTempacstru[16,1]='nAgeAbv'
  laTempacstru[16,2]='N' 
  laTempacstru[16,3]= 12
  laTempacstru[16,4]= 2

  laTempacstru[17,1]='Adjustmnts'
  laTempacstru[17,2]='N' 
  laTempacstru[17,3]= 12
  laTempacstru[17,4]= 2
  
  laTempacstru[18,1]='nRecv'
  laTempacstru[18,2]='N' 
  laTempacstru[18,3]= 7
  laTempacstru[18,4]= 0
  
  laTempacstru[19,1]='STY'
  laTempacstru[19,2]='C' 
  laTempacstru[19,3]= 19
  laTempacstru[19,4]= 0
  
  laTempacstru[20,1]='DYELOT'
  laTempacstru[20,2]='C' 
  laTempacstru[20,3]= 10
  laTempacstru[20,4]= 0
  

  
  gfCrtTmp(lcWrkTmp,@laTempacstru,"Dept+cStyGroup+Style+Color+dyelot",lcWrkTmp,.T.)
  
ENDIF

*!***************************************************************************************
*! Name      : lfPrepFil
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
*! Purpose   : Prepare filter.
*!***************************************************************************************
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
FUNCTION lfPrepFil

lcRpExp  = lcRpExp + ' AND TOTSTK <>0' 
*--Cut the Department code from the filter as the filter will be on Style file.
lnDepPos = AT("ICDEPTHD.DEPT",lcRpExp)
IF lnDepPos > 0
  lnDepPosA = ASCAN(laOGFxFlt,"ICDEPTHD.DEPT")
  lnDepPosA = ASUBSCRIPT(laOGFxFlt,lnDepPosA,1)
  *--Take the length of 'INLIST(' into consederation.
  lnDepPos  = lnDepPos -7
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnDepPos))
  lcExlExp  = SUBSTR(lcRpExp,lnDepPos,lnExpEnd)
  lcRpExp   = STRTRAN(lcRpExp,lcExlExp,'.T.')
ENDIF
lcExlExp = STRTRAN(lcExlExp,'ICDEPTHD.','ICDEPTDT.')
RETURN lcExlExp


*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/03/2004
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
lcIMjrPt  = gfItemMask('PI')
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lpCollect
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
*! Purpose   : Collect data
*!*************************************************************
PROCEDURE lpCollect

*--Set needed relations
*!*    IF USED(lcDepdtTmp)
*!*      SELECT(lcDepdtTmp)
*!*      SET RELATION TO dept+cstygroup INTO (lcDepHdTmp)
*!*    ENDIF 

DIMENSION laAging[5]
STORE 0 TO laAging,lnTotSty,lnTotVal
IF !USED('STYINVJL')
*C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [BEGIN]
  *=gfOpenFile(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
  =gfOpenTable(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
  *C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [END]
  
ENDIF
*lcInvJlFlds = "Distinct STYINVJL.STYLE,STYINVJL.CSESSION,STYINVJL.DTRDATE,STYINVJL.CTRCODE,STYINVJL.LINENO AS LINENO ,CIRTYPE,CTRTYPE,NTOTSTK,CWARECODE,CDYELOT" 
lcInvJlFlds = "STYINVJL.STYLE,STYINVJL.CSESSION,STYINVJL.DTRDATE,STYINVJL.CTRCODE,STYINVJL.LINENO AS LINENO ,CIRTYPE,CTRTYPE,NTOTSTK,CWARECODE,CDYELOT" 
*--sql
*lcInvJlFlds = "ITEMJRNL.STYLE,ITEMJRNL.CSESSION,ITEMJRNL.DTRDATE,ITEMJRNL.CTRCODE,[LINENO],CIRTYPE,CTRTYPE,NTOTSTK,CWARECODE" 
*lcSelCond =" CIRTYPE='R' AND CTRTYPE IN ('5','6') AND CINVTYPE = "+lcStyInvType
*lcSeleTable = "ITEMJRNL(INDEX = STYINVJL)" 
*--lcSeleTable ="STYINVJL INNER JOIN " + lcStyTmp +" ON  STYINVJL.style+STYINVJL.cwarecode+STYINVJL.csession+DTOS(STYINVJL.dtrdate)+STYINVJL.ctrcode+STR(STYINVJL.lineno,6) = "+lcStyTmp +".STYLE"
lcSeleTable ="STYINVJL ," + lcStyFnl  
*lcSelCond ="STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineno,6) = "+lcStyFnl +".STYLE"+" AND CIRTYPE='R' AND CTRTYPE $'56' "
lcSelCond ="STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineno,6) = "+lcStyFnl +".STYLE"

SELECT(lcStyFnl)
*--sql
*!*    IF !EOF()
*!*      lcCurName = lcStyTmp
*!*      IF !EMPTY(lcCurName)
*!*        SELECT &lcCurName    
*!*        IF (RECCOUNT() > 0) 
*!*          lcSQLStyle = loOgScroll.gfSQLTempName('','STYLE C(19)',lcCurName,'STYLE')
*!*          IF EMPTY(lcSQLStyle)
*!*            *-- SQL connection error. can't open the report
*!*            =gfModalGen('TRM00416B40011','ALERT')
*!*            RETURN .F.
*!*          ENDIF
*!*          lcSeleTable = lcSeleTable + "," + lcSQLStyle
*!*          lcSelCond = lcSelCond + " AND  ITEMJRNL.STYLE =  " + lcSQLStyle+".STYLE"
*!*        ENDIF
*!*      ENDIF 
*!*    ENDIF     
*!*    IF lfOpenSql(lcInvJlFlds , lcSeleTable , lcStyInvTmp ,lcSelCond  )  
*!*      SELECT(lcStyInvTmp)
*!*      LOCATE 
*!*    ENDIF 
*SELECT  &lcInvJlFlds FROM &lcSeleTable WHERE &lcSelCond INTO CURSOR &lcStyInvTmp
*lnBuffering = CURSORGETPROP("Buffering",lcStyInvTmp)
*=CURSORSETPROP("Buffering",3,lcStyInvTmp) 
*INDEX ON STYLE TAG  STYINVJL 
*---
llDEPSEL = .F.
lnPosDep = ASCAN(loOgScroll.laOgFXFlt,"ICDEPTHD.DEPT")
  IF lnPosDep > 0
    lnPosDep = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDep,1)  
    lcSlctDept = loOgScroll.laOgFxFlt[lnPosDep , 6]
    IF !EMPTY(lcSlctDept) AND USED(lcSlctDept)
       llDEPSEL =  .T.
    ENDIF 
  ENDIF 
*---

lcStyInvTmp = 'STYINVJL'
SELECT (lcStyInvTmp)

  SET ORDER TO STYINVJL desc

SELECT(lcStyFnl)
SCAN 
  IF llDEPSEL &&AND 
*   IF SEEK(SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen),lcDepdtTmp)
   IF SEEK(&lcStyFnl..Style+&lcStyFnl..DepT,lcDepdtTmp)
    WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor + ': ' +&lcStyFnl..dept+'\'+cstygroup+'\'+&lcStyFnl..cStyMajor NOWAIT
    SELECT(lcStyInvTmp)
    IF &lcStyFnl..TOTSTK>0 AND lfRecv()
      WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor+'-'+lcColorTlt+ ': ' +&lcStyFnl..dept+'\'+&lcStyFnl..cstygroup+'\'+style NOWAIT  
      *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lnStkQty = &lcStyFnl..TOTSTK
      SCAN REST WHILE Style = &lcStyFnl..style FOR CIRTYPE='R' AND CTRTYPE $'56'
     *      ;FOR CIRTYPE='R' AND CTRTYPE $ '56'  
        ldDate = &lcStyInvTmp..DTRDATE
        SELECT (lcWrkTmp)
        *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
        lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
        IF !SEEK(lcKey)
          APPEND BLANK
          REPLACE Type      WITH 'D'                                 ,;
                  Dept      WITH &lcStyFnl..dept                       ,;
                  cStyGroup WITH &lcStyFnl..cStyGroup                     ,;
                  Style     WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                  StyShrtD  WITH &lcStyFnl..Desc                          ,;
                  StyLongD  WITH &lcStyFnl..Desc1                         ,;
                  Color     WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                  ColorDsc  WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                  Total     WITH &lcStyFnl..TotStk                        ,;
                  UnitCost  WITH &lcStyFnl..Ave_Cost                      ,;
                 CostVal   WITH &lcStyFnl..nStkVal                       
          REPLACE STY WITH &lcStyFnl..STYLE
          lnTotSty = lnTotSty+&lcStyFnl..TotStk
          lnTotVal = lnTotVal+&lcStyFnl..nStkVal
        ENDIF
        *WAIT WINDOW &lcStyFnl..STYLE+'*'+PADR(&lcStyInvTmp..DTRDATE,10)+'*'+PADR(DATE()-ldDate,10)
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
        lnStkQty = lnStkQty - &lcStyInvTmp..NTOTSTK

        *--Increase nRecv field with the value recived.
        REPLACE nRecv WITH nRecv+&lcStyInvTmp..nTotStk
      
        *--If the stock quantity is less than the total recivings no need to complete the loop.
        IF lnStkQty<=0
          EXIT
        ENDIF      
      ENDSCAN
    ELSE
      SELECT (lcWrkTmp)
      *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      IF !SEEK(lcKey)
        APPEND BLANK
        REPLACE Type       WITH 'D'                                 ,;
                Dept       WITH &lcStyFnl..dept                       ,;
                cStyGroup  WITH &lcStyFnl..cStyGroup                     ,;
                Style      WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                StyShrtD   WITH &lcStyFnl..Desc                          ,;
                StyLongD   WITH &lcStyFnl..Desc1                         ,;
                Color      WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                ColorDsc   WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                Total      WITH &lcStyFnl..TotStk                        ,;
                UnitCost   WITH &lcStyFnl..Ave_Cost                      ,;
                CostVal    WITH &lcStyFnl..nStkVal                       ,;
                Adjustmnts WITH &lcStyFnl..nStkVal
        lnTotSty = lnTotSty+&lcStyFnl..TotStk
        lnTotVal = lnTotVal+&lcStyFnl..nStkVal
      ENDIF
    ENDIF
   ENDIF  
  ELSE
    WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor + ': ' +&lcStyFnl..dept+'\'+cstygroup+'\'+&lcStyFnl..cStyMajor NOWAIT
    SELECT(lcStyInvTmp)
    IF &lcStyFnl..TOTSTK>0 AND lfRecv()
      WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor+'-'+lcColorTlt+ ': ' +&lcStyFnl..dept+'\'+&lcStyFnl..cstygroup+'\'+style NOWAIT  
      *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lnStkQty = &lcStyFnl..TOTSTK
      SCAN REST WHILE Style = &lcStyFnl..style FOR CIRTYPE='R' AND CTRTYPE $'56'
     *      ;FOR CIRTYPE='R' AND CTRTYPE $ '56'  
        ldDate = &lcStyInvTmp..DTRDATE
        SELECT (lcWrkTmp)
        *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
        lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
        IF !SEEK(lcKey)
          APPEND BLANK
          REPLACE Type      WITH 'D'                                 ,;
                  Dept      WITH &lcStyFnl..dept                       ,;
                  cStyGroup WITH &lcStyFnl..cStyGroup                     ,;
                  Style     WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                  StyShrtD  WITH &lcStyFnl..Desc                          ,;
                  StyLongD  WITH &lcStyFnl..Desc1                         ,;
                  Color     WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                  ColorDsc  WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                  Total     WITH &lcStyFnl..TotStk                        ,;
                  UnitCost  WITH &lcStyFnl..Ave_Cost                      ,;
                 CostVal   WITH &lcStyFnl..nStkVal                       
          REPLACE STY WITH &lcStyFnl..STYLE
          lnTotSty = lnTotSty+&lcStyFnl..TotStk
          lnTotVal = lnTotVal+&lcStyFnl..nStkVal
        ENDIF
        *WAIT WINDOW &lcStyFnl..STYLE+'*'+PADR(&lcStyInvTmp..DTRDATE,10)+'*'+PADR(DATE()-ldDate,10)
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
        lnStkQty = lnStkQty - &lcStyInvTmp..NTOTSTK

        *--Increase nRecv field with the value recived.
        REPLACE nRecv WITH nRecv+&lcStyInvTmp..nTotStk
      
        *--If the stock quantity is less than the total recivings no need to complete the loop.
        IF lnStkQty<=0
          EXIT
        ENDIF      
      ENDSCAN
    ELSE
      SELECT (lcWrkTmp)
      *lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      lcKey = &lcStyFnl..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)
      IF !SEEK(lcKey)
        APPEND BLANK
        REPLACE Type       WITH 'D'                                 ,;
                Dept       WITH &lcStyFnl..dept                       ,;
                cStyGroup  WITH &lcStyFnl..cStyGroup                     ,;
                Style      WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                StyShrtD   WITH &lcStyFnl..Desc                          ,;
                StyLongD   WITH &lcStyFnl..Desc1                         ,;
                Color      WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                ColorDsc   WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                Total      WITH &lcStyFnl..TotStk                        ,;
                UnitCost   WITH &lcStyFnl..Ave_Cost                      ,;
                CostVal    WITH &lcStyFnl..nStkVal                       ,;
                Adjustmnts WITH &lcStyFnl..nStkVal
        lnTotSty = lnTotSty+&lcStyFnl..TotStk
        lnTotVal = lnTotVal+&lcStyFnl..nStkVal
      ENDIF
    ENDIF
  ENDIF  
ENDSCAN

SELECT (lcWrkTmp) 
SCAN 
  =SEEK(&lcWrkTmp..STY,lcStyFnl)
  *--if the total quantities recived is less than the stock 
  IF &lcWrkTmp..nRecv < &lcStyFnl..TOTSTK
    REPLACE Adjustmnts WITH (&lcStyFnl..TOTSTK-nRecv) * &lcStyFnl..AVE_COST            
  ENDIF
ENDSCAN
*:**************************************************************************
*:* Name        : lfRecv
*:* Developer   : Mariam Mazhar (MMT)
*:* Date        : 10/03/2004
*:* Purpose     : Check that there is at least one reciving for the currnet style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRecv()
*:***************************************************************************
FUNCTION lfRecv
PRIVATE lcSlct,llFound
lcSlct = SELECT(0)
llFound = .F.
IF SEEK(&lcStyFnl..Style,lcStyInvTmp)
  SELECT(lcStyInvTmp)
  SCAN REST WHILE STYLE = &lcStyFnl..STYLE
    IF CIRTYPE='R' AND CTRTYPE $'56'
      llFound = .T.
      EXIT 
    ENDIF
  ENDSCAN
ENDIF 
SELECT (lcSlct)
RETURN llFound
*-- end of lfRecv.
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE

  CASE UPPER(lcTable) = lcStyTmp
   
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'Style'
    laIndex[1,2] = 'Style'
    laIndex[2,1] = 'cstymajor'
    laIndex[2,2] = 'cStyle'
    
  CASE UPPER(lcTable) = lcDepHdTmp 
    
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'DEPT+CSTYGROUP'
    laIndex[1,2] = 'DEPTHD'
    laIndex[2,1] = 'CSTYGROUP'
    laIndex[2,2] = 'CGROUP'

  CASE UPPER(lcTable) = lcDepDtTmp 
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'DEPT+CSTYGROUP+STYLE'
    laIndex[1,2] = 'DEPTDT'
    laIndex[2,1] = 'STYLE+DEPT'
    laIndex[2,2] = 'DEPTDTS'


  CASE UPPER(lcTable) = lcStyInvTmp
    DIMENSION laIndex[1,2]
    laIndex[1,1] = ' STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)'
    laIndex[1,2] = 'STYINVJL'
                                                                                       
ENDCASE
*!*************************************************************
*! Name      : lfConvInList
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function to convert cursor contents to inlist
*!*************************************************************
*! Parameters: cursor name,field name 
*!*************************************************************
*! Returns   : inlist expression
*!*************************************************************

FUNCTION lfConvInList
PARAMETERS lcCursor,lcField
lcRetrnExpression = ""
lcRetrnExp = ""
SELECT(lcCursor)
LOCATE 
SCAN
  lcRetrnExp = lcRetrnExp + ",'"+&lcField+"'"
ENDSCAN 
lcRetrnExpression = lcRetrnExpression + "INLIST("+lcField+lcRetrnExp+")"
RETURN lcRetrnExpression

*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenFox

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,;
                                             oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                            'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfConvToInList
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function TO convert cursor to inlist
*!*************************************************************
*! Parameters: cursor name , field name
*!*************************************************************
*! Returns   : the inlist expression
*!*************************************************************

FUNCTION lfConvToInList
PARAMETERS lcCursor,lcField
lcRetrnExpression = ""
lcRetrnExp = ""
llFirst = .T.
SELECT(lcCursor)
LOCATE 
lnCount = 1
SCAN
  IF lnCount < 24
    lcRetrnExp = lcRetrnExp + ",'"+ALLTRIM(&lcField)+"'"
    lnCount = lnCount + 1
  ELSE 
    lcRetrnExpression = lcRetrnExpression + IIF(llFirst, "INLIST("," OR INLIST(")+lcField+lcRetrnExp+")"
    llFirst = .F.    
    lnCount = 1
    lcRetrnExp = ""
  ENDIF 
ENDSCAN 
RETURN lcRetrnExpression

*!*************************************************************
*! Name      : lfStatusCheck
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function TO check if user select status
*!*************************************************************
*! Parameters: cursor name , field name
*!*************************************************************
*! Returns   : the inlist expression
*!*************************************************************
FUNCTION lfStatusCheck
*!*      lnPosStatus = ASCAN(loOgScroll.laOgFXFlt,"STYLE.STATUS")
*!*      IF lnPosStatus > 0
*!*        lnPosStatus = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStatus,1)  
lcStyleSelctExp = lcStyleSelctExp +" AND STYLE.STATUS IN"+lfCreateStatusExp()&&the select condition
*--+IIF(EMPTY(lcRpStatus),"AHX",lcRpStatus)+"'"
*!*        IIF(!EMPTY(loOgScroll.laOgFxFlt[lnPosStatus,8])," AND "+loOgScroll.laOgFxFlt[lnPosStatus,8]," AND STYLE.STATUS $ 'AHX'")
*!*      ENDIF  
  
*!*************************************************************
*! Name      : lfGroupCheck
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function TO check if user select style groups
*!*************************************************************
*! Parameters: cursor name , field name
*!*************************************************************
*! Returns   : the inlist expression
*!*************************************************************
FUNCTION lfGroupCheck
 lnPosStyGrp = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYGROUP")
 IF lnPosStyGrp > 0 &&check if user selected groups
   lnPosStyGrp = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyGrp,1)  
   IF OCCURS('|',loOgScroll.laOgFxFlt[lnPosStyGrp,6]) < 24 AND !EMPTY(loOgScroll.laOgFxFlt[lnPosStyGrp,6])
     *-- if user selected less than 24 group
     lcStyleSelctExp = lcStyleSelctExp +"  AND  "+loOgScroll.laOgFxFlt[lnPosStyGrp,8]
   ELSE &&if user selected more than 24 group
     lcGroups= loOgScroll.laOgFxFlt[lnPosStyGrp,6]
     IF !EMPTY(lcGroups) 
     *--converting the selected groups into cursor
       lcGrpCursor = loOgScroll.gfTempName()
       *-- temp file to put the selected group into it
       DIMENSION laTempacstru[1,4]
       laTempacstru[1,1]='CSTYGROUP'
       laTempacstru[1,2]='C'
       laTempacstru[1,3]= 6
       laTempacstru[1,4]= 0
       gfCrtTmp(lcGrpCursor,@laTempacstru,"CSTYGROUP",lcGrpCursor,.T.)
       lnStart=1
       lnEnd=AT('|',lcGroups)
       DO WHILE lnEnd <> 0
         SELECT(lcGrpCursor) 
         APPEND BLANK 
         REPLACE CSTYGROUP WITH SUBSTR(lcGroups,lnStart,lnEnd-1)
         lcGroups = STUFF(lcGroups ,lnStart,lnEnd,"") 
         lnEnd=AT('|',lcGroups)
       ENDDO 
       IF lnEnd = 0
         SELECT(lcGrpCursor) 
          APPEND BLANK 
          REPLACE CSTYGROUP WITH lcGroups
       ENDIF 
       SELECT(lcGrpCursor)
       LOCATE 
       IF !EOF()
         lcTableStyle =lcTableStyle + " INNER JOIN "+lcGrpCursor+" ON STYLE.CSTYGROUP = "+lcGrpCursor+".CSTYGROUP"
       ENDIF     
     ENDIF 
   ENDIF   
 ENDIF  

*!*************************************************************
*! Name      : lfMajorCheck
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function TO check if user select majors
*!*************************************************************
*! Parameters: cursor name , field name
*!*************************************************************
*! Returns   : the inlist expression
*!*************************************************************
FUNCTION lfMajorCheck
 lcStyleListExp = ""
 lcStyleListExp = loogscroll.lcrpfoxexp 
 lnPosStyle = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
 IF  lnPosStyle > 0 
   lnPosStyle = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyle,1)
   lcSelctdStyle= loOgScroll.laOgFxFlt[lnPosStyle,6]
   IF !EMPTY(lcSelctdStyle)
     SELECT(lcSelctdStyle)
     LOCATE
     IF !EOF()
       SELECT &lcSelFldsStyle FROM &lcTableStyle WHERE &lcStyleSelctExp INTO CURSOR &lcStyleFinal 
       lcTableStyle =lcStyleFinal + " , "+lcSelctdStyle 
       lcSelFldsStyle ="&lcStyleFinal..*"
       lcStyleSelctExp =" &lcStyleFinal..CSTYMAJOR= "+lcSelctdStyle+".CSTYMAJOR"
     ENDIF 
   ENDIF   
 ENDIF
*!*************************************************************
*! Name      : lfDeptCheck
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function TO check if user selected style majors
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
FUNCTION lfDeptCheck
 lcTableDepDt ="ICDEPTDT "
 lcDeptFilter = ""
 lcDeptHdrFld = "ICDEPTHD.DEPT , ICDEPTHD.CSTYGROUP"
 lcDeptDTFld = " DISTINCT ICDEPTDT.DEPT , ICDEPTDT.CSTYGROUP, ICDEPTDT.STYLE"
 lcTableDepHdr ="ICDEPTHD"
 lcDeptDtFilter = ""
 IF !EMPTY(lcExlExp)&& that is mean that user make condition on the department filter
   lcDeptFilter = lcExlExp 
   lnPosDep = ASCAN(loOgScroll.laOgFXFlt,"ICDEPTHD.DEPT")
   IF lnPosDep > 0
     lnPosDep = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDep,1)  
     lcSlctDept = loOgScroll.laOgFxFlt[lnPosDep , 6]
     IF !EMPTY(lcSlctDept)
       SELECT(lcSlctDept)
       LOCATE 
       lcTableDepHdr = lcTableDepHdr + " INNER JOIN " + lcSlctDept + " ON  ICDEPTHD.DEPT+ICDEPTHD.CSTYGROUP = " + lcSlctDept + ".DEPT"
*!*           IF !EOF()
*!*             IF RECCOUNT()<=24
*!*               lcDepFlt = lfConvInList(lcSlctDept,'DEPT')              
*!*               lcDeptFilter = lcDepFlt
*!*             ELSE 
*!*               lcDepFlt = lfConvToInList(lcSlctDept,'DEPT')              
*!*               lcDeptFilter = lcDepFlt
*!*             ENDIF 
*!*             lcDeptDtFilter =STRTRAN(lcDepFlt,'DEPT','ICDEPTDT.DEPT')
*!*           ENDIF 
     ENDIF 
   ENDIF 
 ENDIF 
*!*     IF lfOpenFox(lcDeptHdrFld , lcTableDepHdr , lcDepHdTmp ,lcDeptFilter )  
*!*       SELECT(lcDepHdTmp)
*!*       LOCATE 
*!*       lcDeptDtFilter = lcDeptDtFilter + IIF(EMPTY(lcDeptDtFilter ),""," AND ")+"ICDEPTDT.DEPT = ICDEPTHD.DEPT AND ICDEPTDT.CSTYGROUP = ICDEPTHD.CSTYGROUP AND ICDEPTDT.STYLE+ICDEPTDT.DEPT = &lcStyTmp..Style"
*!*       lcTableDepDt =lcTableDepDt +","+lcStyTmp
*!*       SELECT &lcDeptDtFld FROM &lcTableDepDt  WHERE &lcDeptDtFilter  INTO CURSOR &lcDepDtTmp 
*!*       lnBuffering = CURSORGETPROP("Buffering",lcDepDtTmp)
*!*        =CURSORSETPROP("Buffering",3,lcDepDtTmp)
*!*       SELECT(lcDepDtTmp)
*!*       INDEX ON STYLE+DEPT TAG DEPTDTS
*!*       SET ORDER TO tag DEPTDTS
*!*       LOCATE 
*!*     ENDIF 
 **WHERE &lcDeptFilter 
 SELECT &lcDeptHdrFld  FROM &lcTableDepHdr INTO CURSOR &lcDepHdTmp
 SELECT(lcDepHdTmp)
 lnBuffering = CURSORGETPROP("Buffering",lcDepHdTmp)
 =CURSORSETPROP("Buffering",3,lcDepHdTmp)
 INDEX ON DEPT+CSTYGROUP TAG DEPTHD
 SET ORDER TO tag DEPTHD
 LOCATE 
 lcTableDepDt = lcTableDepDt + " INNER JOIN "+ lcDepHdTmp +" on ICDEPTDT.dept+ICDEPTDT.cstygroup+ICDEPTDT.style = "+lcDepHdTmp+".DEPT+"+lcDepHdTmp+".CSTYGROUP"+" INNER JOIN " +lcStyTmp+" ON ICDEPTDT.style+ICDEPTDT.Dept = "+lcStyTmp+".Style"
 SELECT &lcDeptDTFld FROM &lcTableDepDt INTO CURSOR &lcDepDtTmp
 lnBuffering = CURSORGETPROP("Buffering",lcDepDtTmp)
 =CURSORSETPROP("Buffering",3,lcDepDtTmp)
 INDEX ON STYLE+DEPT TAG DEPTDTS
 SET ORDER TO tag DEPTDTS
 LOCATE 
 *!*************************************************************
*! Name      : lpCollectDye
*! Auth      : Mariam Mazhar (MMT)
*! DATE      : 10/03/2004
*! Purpose   : Collect data
*!*************************************************************
PROCEDURE lpCollectDye

DIMENSION laAging[5]
STORE 0 TO laAging,lnTotSty,lnTotVal
IF !USED('STYINVJL')
*C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [BEGIN]
  *=gfOpenFile(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
  =gfOpenTable(oAriaApplication.DataDir+'STYINVJL',oAriaApplication.DataDir+'STYINVJL','SH')
  *C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [END]

ENDIF
*C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [BEGIN]
SELECT STYINVJL
=gfSeek('')
*C202177,1 SAH 05/21/2018 CONVERT STYINVJL TO SQL [End]
lcInvJlFlds = "Distinct STYINVJL.STYLE,STYINVJL.CSESSION,STYINVJL.DTRDATE,STYINVJL.CTRCODE,STYINVJL.LINENO AS LINENO ,CIRTYPE,CTRTYPE,NTOTSTK,CWARECODE,CDYELOT" 
lcSeleTable ="STYINVJL ," + lcStyFnl  
lcSelCond ="STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineno,6) = "+lcStyFnl +".STYLE"+" AND CIRTYPE='R' AND CTRTYPE $'56' "
lcStkValDye = "IIF(&lcStyInvTmp..NTOTSTK>=lnStkQty,lnStkQty,&lcStyInvTmp..NTOTSTK)*&lcAveCost..Ave_Cost"
SELECT(lcStyFnl)

SELECT  &lcInvJlFlds FROM &lcSeleTable WHERE &lcSelCond INTO CURSOR &lcStyInvTmp
lnBuffering = CURSORGETPROP("Buffering",lcStyInvTmp)
=CURSORSETPROP("Buffering",3,lcStyInvTmp) 
INDEX ON STYLE+Cdyelot TAG  STYINVJL 

  SET ORDER TO tag  STYINVJL
  

SELECT(lcStyFnl)

SCAN 
  lcSlctStyle = &lcStyFnl..style
  SELECT style,DYELOT,SUM(TOTSTK) as totstk,SUM(TOTWIP) as totwip,SUM(TOTORD) as totord FROM STYDYE WHERE style+cwarecode+dyelot = lcSlctStyle  AND !EMPTY(DYELOT) GROUP BY DYELOT,STYLE INTO CURSOR &lcDyelots
  SELECT ave_cost ,dyelot,nStkVal FROM stydye WHERE style+cwarecode+dyelot = lcSlctStyle INTO CURSOR &lcAveCost
  lnBuffering = CURSORGETPROP("Buffering",lcAveCost)
  =CURSORSETPROP("Buffering",3,lcAveCost)
  *-- creating index on the temp style file 
  INDEX ON  dyelot TAG dyelot
  SET ORDER TO TAG dyelot 
  SELECT(lcDyelots)  
  SCAN 
    IF SEEK(SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen),lcDepdtTmp)
      WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor + ': ' +&lcDepdtTmp..dept+'\'+&lcDepdtTmp..cstygroup+'\'+&lcStyFnl..cStyMajor NOWAIT
      =SEEK(&lcDyelots..dyelot,lcAveCost)
      SELECT(lcStyInvTmp)
      IF &lcDyelots..TOTSTK >0 AND lfRecvDye()
      WAIT WINDOW  'Department\'+lcStyMajor + ' group\'+lcStyMajor+'-'+lcColorTlt+ ': ' +&lcDepdtTmp..dept+'\'+&lcStyFnl..cstygroup+'\'+style NOWAIT  
      lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)+&lcDyelots..Dyelot
      lnStkQty = &lcDyelots..TOTSTK
      SCAN REST WHILE Style+Cdyelot = &lcStyFnl..style +&lcDyelots..dyelot
        ldDate = &lcStyInvTmp..DTRDATE
        SELECT (lcWrkTmp)
        lcKey = &lcDepdtTmp..dept + &lcStyFnl..cstyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen)+&lcStyInvTmp..cDyelot
        IF !SEEK(lcKey)
          APPEND BLANK
          REPLACE Type      WITH 'D'                                 ,;
                  Dept      WITH &lcDepdtTmp..dept                       ,;
                  cStyGroup WITH &lcStyFnl..cStyGroup                     ,;
                  Style     WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                  StyShrtD  WITH &lcStyFnl..Desc                          ,;
                  StyLongD  WITH &lcStyFnl..Desc1                         ,;
                  Color     WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                  ColorDsc  WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                  Total     WITH &lcDyelots..TotStk                        ,;
                  UnitCost  WITH &lcStyFnl..Ave_Cost                      ,;
                  CostVal   WITH &lcStyFnl..nStkVal                       ,;
                  dyelot    WITH &lcStyInvTmp..Cdyelot
          REPLACE STY WITH &lcStyFnl..STYLE
          lnTotSty = lnTotSty+&lcDyelots....TotStk
          lnTotVal = lnTotVal+&lcAveCost....nStkVal
        ENDIF
        DO CASE        
          CASE BETWEEN(DATE()-ldDate,0,90) 
            REPLACE nAge90  WITH nAge90  + EVALUATE(lcStkValDye)
            laAging[1] = laAging[1] + EVALUATE(lcStkValDye)
          CASE BETWEEN(DATE()-ldDate,91,180)
            REPLACE nAge180 WITH nAge180 + EVALUATE(lcStkValDye)
            laAging[2] = laAging[2] + EVALUATE(lcStkValDye)
          CASE BETWEEN(DATE()-ldDate,181,270)
            REPLACE nAge270 WITH nAge270 + EVALUATE(lcStkValDye)
            laAging[3] = laAging[3] + EVALUATE(lcStkValDye)
          CASE BETWEEN(DATE()-ldDate,271,360)
            REPLACE nAge360 WITH nAge360 + EVALUATE(lcStkValDye)
            laAging[4] = laAging[4] + EVALUATE(lcStkValDye)
          CASE DATE()-ldDate > 360
            REPLACE nAgeAbv WITH nAgeAbv + EVALUATE(lcStkValDye)
            laAging[5] = laAging[5] + EVALUATE(lcStkValDye)   
        ENDCASE
        lnStkQty = lnStkQty - &lcStyInvTmp..NTOTSTK

        *--Increase nRecv field with the value recived.
        REPLACE nRecv WITH nRecv+&lcStyInvTmp..nTotStk
      
        *--If the stock quantity is less than the total recivings no need to complete the loop.
        IF lnStkQty<=0
          EXIT
        ENDIF      
      ENDSCAN
    ELSE
      SELECT (lcWrkTmp)
      lcKey = &lcDepdtTmp..dept + &lcStyFnl..cStyGroup + SUBSTR(&lcStyFnl..cStyMajor,1,lnMajorLen) + lcSepart + SUBSTR(&lcStyFnl..Style,lnClrPo,lnColorLen)+&lcDyelots..Dyelot
      IF !SEEK(lcKey)
        APPEND BLANK
        REPLACE Type       WITH 'D'                                 ,;
                Dept       WITH &lcDepdtTmp..dept                       ,;
                cStyGroup  WITH &lcStyFnl..cStyGroup                     ,;
                Style      WITH SUBSTR(&lcStyFnl..style,1,lnMajorLen),;
                StyShrtD   WITH &lcStyFnl..Desc                          ,;
                StyLongD   WITH &lcStyFnl..Desc1                         ,;
                Color      WITH SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen),;
                ColorDsc   WITH SUBSTR(gfCodDes(SUBSTR(&lcStyFnl..style,lnClrPo,lnColorLen), 'COLOR'),1,15),;
                Total      WITH &lcDyelots..TotStk                        ,;
                UnitCost   WITH &lcStyFnl..Ave_Cost                      ,;
                CostVal    WITH &lcStyFnl..nStkVal                       ,;
                Adjustmnts WITH &lcAveCost..nStkVal ,;
                dyelot     WITH &lcDyelots..Dyelot
        lnTotSty = lnTotSty+&lcDyelots..TotStk
        lnTotVal = lnTotVal+&lcAveCost..nStkVal
      ENDIF
    ENDIF
  ENDIF  
  ENDSCAN 
ENDSCAN
  SELECT Style,DYELOT,SUM(TOTSTK) as totstk,SUM(TOTWIP) as totwip,SUM(TOTORD) as totord FROM STYDYE WHERE style+cwarecode+dyelot =&lcStyFnl..style  AND !EMPTY(DYELOT) GROUP BY DYELOT,Style INTO CURSOR &lcDyelots
  lnBuffering = CURSORGETPROP("Buffering",lcDyelots)
  =CURSORSETPROP("Buffering",3,lcDyelots)
  *-- creating index on the temp style file 
  INDEX ON  Style+dyelot TAG stydyelot
  SET ORDER TO TAG stydyelot

*  SELECT Style,ave_cost ,dyelot,nStkVal FROM stydye WHERE style+cwarecode+dyelot = &lcStyFnl..style INTO CURSOR &lcAveCost
*!*      lnBuffering = CURSORGETPROP("Buffering",lcAveCost)
*!*      =CURSORSETPROP("Buffering",3,lcAveCost)
*!*      *-- creating index on the temp style file 
*!*      INDEX ON  style+dyelot TAG StyDye
*!*      SET ORDER TO TAG StyDye

SELECT (lcWrkTmp) 
SCAN 
  =SEEK(&lcWrkTmp..STY+dyelot,lcDyelots)
  *--if the total quantities recived is less than the stock 
  IF &lcWrkTmp..nRecv < &lcDyelots..TOTSTK
   =SEEK(&lcWrkTmp..STY ,lcStyFnl)
    REPLACE Adjustmnts WITH (&lcDyelots..TOTSTK-nRecv) * &lcStyFnl..AVE_COST            
  ENDIF
ENDSCAN
*:**************************************************************************
*:* Name        : lfRecvDye
*:* Developer   : Mariam Mazhar (MMT)
*:* Date        : 10/03/2004
*:* Purpose     : Check that there is at least one reciving for the currnet style
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRecv()
*:***************************************************************************
FUNCTION lfRecvDye
PRIVATE lcSlct,llFound
lcSlct = SELECT(0)
llFound = .F.
IF SEEK(&lcStyFnl..Style,lcStyInvTmp)
  SELECT(lcStyInvTmp)
  SCAN REST WHILE STYLE+cdyelot = &lcStyFnl..STYLE + &lcDyelots..Dyelot
    llFound = .T.
    EXIT 
  ENDSCAN
ENDIF 
SELECT (lcSlct)
RETURN llFound
*-- end of lfRecv.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/09/2004
*! Purpose   : Valid function for STSTUS Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
*--Active|Hold|Canceled~A|H|X
*= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,'Select Style Status',.T.,'')  && call mover function.
lcRpStatus = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Active','A',;
      IIF(laRpTarget[lnI] = 'Hold','H',;
      IIF(laRpTarget[lnI] = 'Canceled','X','')))

  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpStatus)
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*!*************************************************************
*! Name      : lfCreateStatusExp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCreateStatusExp

IF !EMPTY(lcRpStatus)
  IF LEN(lcRpStatus) = 1 
    lcStatusVal ="('"+lcRpStatus+"')"
  ELSE 
    IF LEN(lcRpStatus) = 2
      lcStatusVal ="('"+SUBSTR(lcRpStatus,1,1)+"','"+SUBSTR(lcRpStatus,2,1)+"')"
    ELSE 
      lcStatusVal ="('"+SUBSTR(lcRpStatus,1,1)+"','"+SUBSTR(lcRpStatus,2,1)+"','"+SUBSTR(lcRpStatus,3,1)+"')"
    ENDIF 
  ENDIF 
ELSE   
  lcStatusVal ="('A','H','X')"
  lcRpStatus = 'AHX'
ENDIF
RETURN lcStatusVal
