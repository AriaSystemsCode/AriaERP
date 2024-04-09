*:***************************************************************************
*: Program file   : APDIVIS.PRG
*: Program desc.  : Divisions rerport 
*: System         : ARIA 4XP
*: Module         : Accounts Payable(AP)
*: Developer      : TAREK MOHAMED IBRAHIM 
*: Tracking#, Date: E302935,1 TMI 07/18/2011
*:***************************************************************************
* Modifications
*E302935,3 Division TMI 10/16/2011 [Start] remove the Divison from the lcRpExp and change its format
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011 
*:************************************************************************

SELECT APDIV
SET ORDER TO TAG DIVISION  

*E302935,3 Division TMI 10/16/2011 [Start] remove the Divison from the lcRpExp and change its format
LOCAL lnDivPos
lnDivPos = lfGetPos("APDIV.CDIVISION","laOgFxFlt")
lcDiv = loOgScroll.laOgFxFlt[lnDivPos,6]
IF lcRpExp<>'.T.' AND !EMPTY(lcDiv)
  lcRpExp = '.T. AND '+lcRpExp   && add this just to employ the function "lfPolishExp"
  =lfPolishExp(@lcRpExp,"APDIV.CDIVISION")
  lcRpExp = lcRpExp + " AND APDIV.CDIVISION $ lcDiv"
ENDIF
*E302935,3 Division TMI 10/16/2011 [End  ] 
DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp


*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Gets the position of element from array 
*! Developer     : TAREK MOHAMED IBRAHIM
*! Date          : 10/16/2011
*!**************************************************************************
*E302935,3 
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(loOGScroll.&lcArray,lcOpt)
lnPos = ASUBSCRIPT(loOGScroll.&lcArray,lnPos,1)
RETURN lnPos
*--End of function

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
*E302935,3 
FUNCTION lfPolishExp
PARAMETERS lcExp,lcRmv
LOCAL lnPos,lcRight
lcRight = ")"
lnPos = AT(lcRmv,lcExp)
DO WHILE lnPos>0
  lnAndPos = RAT(' AND ',SUBSTR(lcExp,1,lnPos))
  lcLeftStr = LEFT(lcExp,lnAndPos-1)
  lnPranth = AT(lcRight,SUBSTR(lcExp,lnAndPos))
  lcRightStr = SUBSTR(lcExp,lnAndPos+lnPranth+LEN(lcRight)-1)
  lcExp = lcLeftStr+lcRightStr
  lnPos = AT(lcRmv,lcExp)
ENDDO
