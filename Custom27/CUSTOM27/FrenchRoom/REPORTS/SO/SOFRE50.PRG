*:**************************************************************************
*: Program file  : SOFRE50
*: Program desc. : Custom Order Detail Report for Le Mystere
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 10/09/2000
*: Reference     : C101981 (Also refer to C101561 A26) 
*:**************************************************************************
*: Calls : 
*:    Procedures : lfDispRe
*:    Functions  : lfsrvSty , lfStySum , lfsrRep , lfEvalSegs , lfNMajType , 
*:               : lfwRepWhen , lfItmPos , lfCollTime , lfClearRep
*:**************************************************************************
*: Example : DO SOFRE50
*:**************************************************************************
*

lcStTime = TIME()                     && Variable to hold the Time

*-- If user changed criteria collect data again [Begin]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF
 
  PRIVATE lcNewRpExp 
  lcNewRpExp = ""
  DO lpGenExpr
  lcNewRpExp = STRTRAN(lcNewRpExp,"SEEK(ORDLINE.STYLE","SEEK(SUBSTR(ORDLINE.STYLE,1,lnMajorLen)")
  lcNewRpExp = IIF(EMPTY(lcNewRpExp),".T.",lcNewRpExp)
  
  SELECT OrdLine
  SET RELATION TO "O" + Order INTO OrdHdr ADDITIVE

  *-- Scan Loop around Order Line file 
  SCAN FOR &lcNewRpExp
    WAIT WINDOW 'Collecting data from Order # ' + Order NOWAIT   
    DO lpInsRecds
  ENDSCAN
  *-- EndScan around Order Line file 
  
  SELECT OrdLine
  SET RELATION OFF INTO OrdHdr
ENDIF
*-- EndIf of user changed criteria [End]

lnInterval = lfCollTime(lcStTime,TIME())
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

SELECT (lcWorkFile)
DO gfDispRe WITH EVAL('lcRpForm')
*-- End of Report.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
lnStylePos = lfItmPos('ORDLINE.STYLE')  && Style Position
lnColorPos = lfItmPos('SUBSTR(ORDLINE.STYLE,lnNonMajSt,lnColorLen)')  && Color Position 
lnRepPos   = lfItmPos('ORDHDR.REP1')    && Sales Rep Position

*-- Generate the new expression which will be used in scanning.
CREATE TABLE (gcWorkDir+lcTmpStyle) (cStyMajor C(lnMajorLen))
INDEX ON cStyMajor TAG (lcTmpStyle)

PRIVATE llOpenSInt , llOpenSCmp
llOpenSInt = gfOpenFile(gcSysHome+'SycInt','cContCode','SH')
llOpenSCmp = gfOpenFile(gcSysHome+'SycComp','cComp_ID','SH')

*-- since the Company is single currency so it use only one currency symbol 
*-- and also a fixed position of symbol (Left or Right of amount)
IF SEEK(gcAct_Comp,'SycComp') AND SEEK(SycComp.cCont_Code,'SycInt')
  lcCurSymbl = ALLTRIM(SycInt.cCurrencyI)
  llRightPos = IIF(SycInt.cCurrency=="RIGHT",.T.,.F.)
ENDIF

IF llOpenSInt
  USE IN SycInt
ENDIF

IF llOpenSCmp
  USE IN SycComp
ENDIF

DO lpCreatFil
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : To get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
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

*!***************************************************************************
*! Name      : lpInsRecds
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : To insert a record in the Temp Work File.
*!***************************************************************************
*! Example   : DO lpInsRecds
*!***************************************************************************
*
PROCEDURE lpInsRecds
IF SEEK(SUBSTR(Style,1,lnMajorLen) + OrdHdr.Rep1 + Account,(lcWorkFile))
  SELECT (lcWorkFile)
  REPLACE Qty    WITH Qty + OrdLine.TotQty ;
          Amount WITH Amount + (OrdLine.TotQty * OrdLine.Price)   
ELSE
  INSERT INTO (lcWorkFile) ;
              (Style , Account , Rep , Qty , Amount) ;
       VALUES (SUBSTR(OrdLine.Style,1,lnMajorLen) , OrdLine.Account , OrdHdr.Rep1 , ;
               OrdLine.TotQty , OrdLine.TotQty * OrdLine.Price)
ENDIF               
*-- End of lpInsRecds.    

*!***************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Create work File.
*!***************************************************************************
*! Example   : DO lpCreatFil
*!***************************************************************************
*
PROCEDURE lpCreatFil
CREATE TABLE (gcWorkDir+lcWorkFile) ;
             (Style C(19) , Account C(5) , Rep C(3) , Qty N(10) , Amount N(13,2))
INDEX ON SUBSTR(Style,1,lnMajorLen) + Rep + Account TAG (lcWorkFile)
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Generate Expression
*!***************************************************************************
*! Example   : DO lpGenExpr
*!***************************************************************************
*
PROCEDURE lpGenExpr
PRIVATE lcAlias , lcStyleExp
lcAlias = ALIAS()

*-- If user selected Style from Style InList [Begin]
IF USED(laOGFxFlt[1,6]) AND RECCOUNT(laOGFxFlt[1,6]) > 0 
    
  *-- Copy all laOGFxFlt to another array to save the old value.
  DIMENSION laTempExpr[1]
  =ACOPY(laOGFxFlt,laTempExpr)

  *-- Define new Fixed filter array to hold one expression only.
  DIMENSION laOGFxFlt[1,7]
  STORE "" TO laOGFxFlt , lcStyleExp
  
  PRIVATE lcBrowPos    

  *-- Copy all Browse Filter array which is used in case of INLIST 
  *-- to another temp array to save the old value.
  DIMENSION laBrTmpFlt[1]
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  *-- resize Browse filter array to hold one browse expression only.
  DIMENSION laBrFldFlt[1,5]
  laBrFldFlt = ""

  *-- Copy the Account expression to laOGFxFlt
  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
  *-- Convert numeric position to string.
  lcBrowPos = ALLTRIM(STR(lnStylePos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
      EXIT
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = 'laOGFxFlt:1' 
  
  *-- Generate expression for Style.
  lcStyleExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- Zap the Temp. Style file
  SELECT (lcTmpStyle)  
  ZAP
  
  *-- Generate the new expression which will be used in scanning.
  SELECT(laOGFxFlt[1,6])
  SCAN
    SCATTER MEMVAR MEMO
    m.cStyMajor = SUBSTR(m.cStyMajor,1,lnMajorLen)
    INSERT INTO (gcWorkDir+lcTmpStyle) FROM MEMVAR
  ENDSCAN
  laOGFxFlt[1,6] = lcTmpStyle
      
  *-- Generate expression for Style.
  lcNewRpExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
  *-- Replace Style Expr in lcRpExp with the new Style Expr
  lcNewRpExp = STRTRAN(lcRpExp,lcStyleExp,lcNewRpExp)

  *-- Restore original laBrFldFlt
  DIMENSION laBrFldFlt[1]
  =ACOPY(laBrTmpFlt,laBrFldFlt)  
  
    *-- Restore original laOGFxFlt
  DIMENSION laOGFxFlt[1]
  =ACOPY(laTempExpr,laOGFxFlt)
ENDIF
*-- If user selected Style from Style InList [End]

SELECT (lcAlias)
*-- End of lpGenExpr.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
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
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
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

*!**************************************************************************
*! Name      : lfsrRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsrRep()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChRep = .T.
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- End of lfsrRep.

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfNMajType
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Mask NonMajor segments .
*!**************************************************************************
*! Called from : lfEvalSegs.
*!**************************************************************************
*! Example     : = lfNMajType()
*!**************************************************************************
*
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- End of lfNMajType. 

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 10/09/2000
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep

*-- Close & Delete temporary work file.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
ERASE(gcWorkDir+lcWorkFile+'.DBF')
*-- End of lfClearRep.

