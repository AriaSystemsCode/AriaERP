*:************************************************************************
*: Program file  : ICOTSEX.PRG
*: Program desc. : Custom OTS report export to Excel from Aria 2.7 directly  FOR FALCON BAY
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: TMI - TAREK MOHAMMED IBRAHIM
*:     Tracking #: C200834,1 , ticket #T20070719.0007
*:     Date      : 08/20/2007
*:************************************************************************

*- Initialize mask variable
STORE '' TO lcExpr, lcRpMask
DIME laFxFlt[5,7]

*--Calling the option grid
lcOgExpr = gfOpGrid('ICOTSEX' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcOgExpr)
  RETURN
ENDIF

*-Get lnColorLen,lcClrPo variables values
STORE 0 TO lnColorLen,lnClrPo,lnStyPos
=lfGetClr()

*- Check expression
PRIVATE lcTmpSty
lcTmpSty = gfTempName()
lnMajLen = LEN(gfItemMask("PM"))

*- Update expression got from laOgFxFlt
lcFlt = lfUpdExpr()

*- Open files
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')

*- Create the needed temp files
SELECT STYLE
=AFIELDS(laStru)
CREATE TABLE (gcWorkDir+lcTmpSty) FROM ARRAY laStru
INDEX ON STYLE TAG STYLE

*- Collecting the data as per the selected criteria from the STYLE file
llSelSty = .F.
IF !EMPTY(laFxFlt[lnStyPos,6]) .AND. USED(laFxFlt[lnStyPos,6])
  SELECT (laFxFlt[lnStyPos,6])
  lnCnt = 0
  COUNT TO lnCnt
  llSelSty = lnCnt>0
ENDIF
IF llSelSty
  SELECT (laFxFlt[lnStyPos,6])
  LOCATE
  SCAN
    lcSeekSty = SUBSTR(CSTYMAJOR,1,lnMajLen)
    =SEEK(lcSeekSty,'STYLE')
    SELECT STYLE
    SCAN REST WHILE STYLE = lcSeekSty &lcFlt
      WAIT WINDOW NOWAIT STYLE
      SCATTER MEMVAR   
      INSERT INTO &lcTmpSty FROM MEMVAR
    ENDSCAN
  ENDSCAN
ELSE
  SELECT STYLE
  LOCATE
  SCAN &lcFlt
    WAIT WINDOW NOWAIT STYLE
    SCATTER MEMVAR   
    INSERT INTO &lcTmpSty FROM MEMVAR
  ENDSCAN
ENDIF

SELECT &lcTmpSty 
LOCATE
IF EOF(lcTmpSty)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display.')
  RETURN
ENDIF

WAIT WINDOW NOWAIT 'Opening the Excel application ... '
lcTempMemo = gfTempName()
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') 
lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')
USE IN &lcTmpSty

lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcAppHome+'IC\'+[ICOTS.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
WAIT CLEAR

*:**************************************************************************
*:* Name        : lfwMask
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : When function for the lcRpMask variable in OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfwMask

*-- end of lfwMask.
*:**************************************************************************
*:* Name        : lfvMask
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : Valid function for the lcRpMask variable in OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvMask
lcRpMask = STRTRAN(lcRpMask,' ','*')
*-- end of lfvMask.

*:**************************************************************************
*:* Name        : lfMajTtGet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : To get the style major segement title
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfMajTtGet
RETURN gfItemMask("HM")

*:**************************************************************************
*:* Name        : lfMajPic
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : To get major segment Picture
*:***************************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*:**************************************************************************
*:* Name        : lfSRSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : Set and Rest functions for style filter.
*:***************************************************************************
*!* Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:***************************************************************************
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- end of lfsrvSty.

*:**************************************************************************
*:* Name        : lfStySum
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : sum a specific field for the current style in style file
*:***************************************************************************
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
*-- end of lfStySum.

*:**************************************************************************
*:* Name        : lfvRunRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : obtain some variables from the OG
*:***************************************************************************
FUNCTION lfvRunRep
DIMENSION laFxFlt[ALEN(laOgFxFlt,1),ALEN(laOgFxFlt,2)]
=ACOPY(laOgFxFlt,laFxFlt)
*-- end of lfvRunRep.

*:**************************************************************************
*:* Name        : lfGetClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2007
*:* Purpose     : *-Get lnColorLen,lcClrPo variables values
*:***************************************************************************
FUNCTION lfGetClr
PRIVATE laMajSegs,i
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
FOR i = 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[i,1] = 'C'
    lnClrPo = laMajSegs[i,4]
    lnColorLen = LEN(laMajSegs[i,3])
  ENDIF
ENDFOR
*-- end of lfGetClr.


*:**************************************************************************
*:* Name        : lfUpdExpr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2007
*:* Purpose     : Update expression got from laOgFxFlt
*:***************************************************************************
FUNCTION lfUpdExpr
PRIVATE lcRet
lnStyPos = lfGetPos('STYLE.CSTYMAJOR')

*C200834,3 TMI [Start] be sure to take only styles with colors included in cstymajor
*lcExpr = ''
lcExpr = 'LEN(ALLTRIM(SUBSTR(style,14)))=2 AND '+;
         'INLIST(SUBSTR(STYLE,14,2), SUBSTR(CSTYMAJOR,LEN(ALLT(CSTYMAJOR))-2 , 2 ),'+;
                                    'SUBSTR(CSTYMAJOR,LEN(ALLT(CSTYMAJOR))-3 , 2 )) AND '
*C200834,3 TMI [End  ] 
=lfGetExpr('STYLE.CDIVISION')
=lfGetExpr('STYLE.SEASON')
=lfGetExpr('STYLE.CSTYGROUP')
=lfGetExpr('SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)')
lcExpr = IIF(!EMPTY(lcExpr),LEFT(lcExpr,LEN(lcExpr)-5),'')

*- check if there is style mask
IF LEN(STRTRAN(lcRpMask,'*','')) > 0
  lcMsk = STRTRAN(lcRpMask,'*','?')
  lcRet = lcExpr + IIF(!EMPTY(lcExpr),'.AND.','') + "LIKE('"+lcMsk+"',SUBSTR(STYLE,1,lnMajLen))"
ELSE
  lcRet = lcExpr
ENDIF
lcRet = IIF(!EMPTY(lcRet),'FOR ','')+lcRet 
RETURN lcRet
*-- end of lfUpdExpr.

*:**************************************************************************
*:* Name        : lfGetExpr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2007
*:* Purpose     : Get code criteria expression 
*:***************************************************************************
FUNCTION lfGetExpr
PARAMETER lcOgFlt
PRIVAT lnPos,lcP,lcExp
lcExp = ''
lnPos = lfGetPos(lcOgFlt)
IF lnPos>0
  lcP = ALLTRIM(STR(lnPos))
  IF !EMPTY(laFxFlt[lnPos,6])
    lcExp = lcOgFlt + ' $ laFxFlt[&lcP,6]'
    lcExpr = lcExpr + lcExp + '.AND.'
  ENDIF
ENDIF
*-- end of lfGetExpr.

*:**************************************************************************
*:* Name        : lfGetPos
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/20/2007
*:* Purpose     : Get posistion from laOgFxFlt
*:***************************************************************************
FUNCTION lfGetPos
PARAMETER lcFlt
PRIVAT lnPos
lnPos = ASCAN(laFxFlt,lcFlt)
IF lnPos>0
  lnPos = ASUBSCRIPT(laFxFlt,lnPos,1)
ENDIF
RETURN lnPos
*-- end of lfGetPos.
