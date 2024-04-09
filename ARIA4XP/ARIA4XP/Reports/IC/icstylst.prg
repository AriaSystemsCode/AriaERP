*!***********************************************************************
*: Program file       : ICSTYLST
*: Program description: Style master list
*: Module             : Inventory Control (IC)
*: Developer          : Tarek Noaman (TNA)
*: Tracking Job Number: N000529
*: Date               : 3/27/2006
*:**********************************************************************
*: Calls: 
*:         Programs   : 
*:         Screens    : 
*: Global Function    :gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:                     gfRltFld,gfCodDes,gfOptMsg
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters  : 
*:***************************************************************************
*: Example : DO ICSTYLST
*:***************************************************************************
*:                   Option grid Filter contains 
*:1-Title                          is                          lcRPTitle 
*:2-Domestic/Imported              Domestic/Imported/Both      lcRPDomImp
*:3-Print Style Picture            YES/NO                      llRpPrnPic
*:4-Style                          In List                     STYLE.CSTYMAJOR 
*:5-Primary Fabric                 In List                     STYLE.FABRIC
*:6-Season                         In List                     STYLE.SEASON
*:7-Division                       In List                     STYLE.CDIVISION 
*:8-Style Group                    In List                     STYLE.CSTYGROUP 
*:9-Status                         In List                     STYLE.STATUS
*:10-Only This Color               In List                     SUBSTR(STYLE.Style,lnClrPo,lnColorLen)
*:***************************************************************************
*:                         Tables Used
*:                        _______________
*:01- Style
*:02- Scale
*:03- Objlink
*:04- Objects
*:***************************************************************************
*Modifications:
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[T20060908.0003]
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[T20121023.0008]
*:***************************************************************************

lcStTime   = TIME()
llDontPrnt = .F.
lcTime     = gfGetTime()
lnMajLen   = LEN(SUBSTR(lcMajPic,4))
llDummy    = loOgScroll.llOGFltCh AND lfCollData()
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[Start]
*!*	IF llRpPrnPic = 'Y' AND UPPER(ALLTRIM(lcRepMode)) <> "TEXT"
*!*	  SELECT FStyle
*!*	  SET RELATION TO 
*!*	  SET RELATION TO 'S'+SUBSTR(Style,1,lnMajLen) INTO OBJLINK ADDITIVE
*!*	  SET SKIP TO OBJLINK
*!*	  SELECT OBJLINK
*!*	  SET RELATION TO
*!*	  SET RELATION TO cobject_id INTO OBJECTS  ADDITIVE
*!*	ENDIF
*B610134,1 MMT 10/31/2012 Report prints repeated lines if print picture is set to Yes[End]
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT FStyle
GO TOP
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcRepNam')

*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Noaman	(TNA)
*! Date      : 3/27/2006
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData
CREATE CURSOR FStyle(style C(19),cstymajor C(19),desc C(20),status C(1),season C(6),cdivision C(6),cstygroup C(6),pattern C(10),fabric C(7),cColor M,start D,;
                     soldout D,location C(8),nstyweight N(5,2),qty_ctn N(3),pricea N(12,2),priceb N(12,2),pricec N(12,2),commission L,scale C(3),prepak C(1))

IF lcRpDomImp <>  'B'
  lcRpExp = lcRpExp + 'AND' + IIF(lcRpDomImp = 'I' , ' !Make' , ' Make' )
ENDIF
lcStyMaj  = SPACE(0)
SELECT STYLE
SET ORDER TO STYLE
SCAN FOR EVALUATE(lcRpExp)
  IF (lcStyMaj # STYLE.cStyMajor)
    m.cColor = SPACE(0)
  ENDIF
  m.cColor = m.cColor + SUBSTR(STYLE.Style,lnClrPo,lnColorLen) + " "
  IF (lcStyMaj # STYLE.cStyMajor)
    SCATTER MEMVAR MEMO
    INSERT INTO FStyle FROM MEMVAR
  ELSE
    lcAlias = SELECT(0)
    SELECT FStyle
    REPLACE cColor WITH m.cColor
    SELECT (lcAlias)
  ENDIF
  lcStyMaj = STYLE.cStyMajor
ENDSCAN

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*-- Check the cost access

lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDCASE
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfAdjPrnArr
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Prepare the arrays that hold the print by values 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjPrnArr()
*!*************************************************************
FUNCTION lfAdjPrnArr
DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]

laRPPrnDsp[1,1]=lcMajTtl
laRPPrnDsp[2,1]=lcNonMajT

laRPPrnRet[1,1]='S'
laRPPrnRet[2,1]='C'

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic
lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
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


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*B802264,1
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
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*B802264,1
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
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvBins      *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Function for Bins
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
FUNCTION lfvBins
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*= gfMover(@laRpSource,@laRpTarget,'Style Bins',.T.,'')
= lfOGMover(@laRpSource,@laRpTarget,'Style Bin',.T.,'')  && call mover function.
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

*!*************************************************************
*! Name      : lfFillBin     *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Function to fill bins arrays
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfFillBin
llUSdBy =gfOpenFile(gcDataDir+'WHSLOC' ,'WHSLOC','SH')
DIME laRpSource[1,1]
DIME laRpTarget[1,1]
SELECT WHSLOC
SELECT DISTINCT CLOCATION FROM WHSLOC WHERE !EMPTY(CLOCATION)INTO ARRAY laRpSource
USE IN IIF(llUSdBy,'WHSLOC',0)

*!*************************************************************
*! Name      : lfClearRed *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to clear read
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfClearRed()
*!*************************************************************
FUNCTION lfClearRed
CLEAR READ

*****************************************
FUNCTION lfGetStr

PRIVATE lcTmpAlias
lcTmpAlias  = SELECT(0)

lcSizStr    = ''
lcPrePakStr = ''

SELECT SCALE
IF SEEK('S'+FStyle.Scale)
  lcSizStr = Sz1 + ' ' + Sz2 + ' ' + Sz3 + ' ' + Sz4 + ' ' + Sz5 + ' ';
             + Sz6 + ' ' + Sz7 + ' ' + Sz8
ENDIF

IF SEEK('P'+FStyle.Scale+FStyle.PrePak)
  lcPrePakStr = STR(Pp1) + STR(Pp2) + STR(Pp3) + STR(Pp4) + STR(Pp5) + ;
                STR(Pp6) + STR(Pp7) + STR(Pp8)
ENDIF
SELECT (lcTmpAlias)
RETURN lcPrePakStr

*!************************************************************** 
* Name      : lfModeVld
* Developer : Ramy Mabrouk
* Date      : 11/24/99
* Purpose   : Report Mode Validation 
* Job No.   : B# 803531,1
*!*************************************************************
* Called from : Option Grid
*!*************************************************************
* Passed Parameters  : None
*!*************************************************************
* Returns            : None
*!*************************************************************
* Example   : =lfModeVld()
*!*************************************************************
*-B803531,1 RAMY [start]
FUNCTION lfModeVld
CLEAR READ
*-- end of lfModeVld.

*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[Start]
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
FUNCTION lfCreatExp
  IF TYPE('llCallFromScr') = 'L'
    =ACOPY(loOGScroll.laOGFxFlt , laFxExpr)
    =ACOPY(loOGScroll.laOGVrFlt , laVrExpr)
  ENDIF 
  
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 
*N000548,1 MMT 08/20/2007 convert locking screen to Aria4xp[End]

