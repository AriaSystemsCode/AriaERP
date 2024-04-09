*:***************************************************************************
*: Program file  : ICCTSLHS.PRG
*: Program desc. : CUSTOMIZED STYLE CUT & SOLD FOR Henry Segal.
*! Date          : 09/04/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102703
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCTSLHS
*:***************************************************************************
*: Modifications :
*: B607330,1 KHM 06/09/2003 Put zero in the planned wip figure in case of its -ve.
*: B607330,1                and add a new option plan WIP only.
*:***************************************************************************

*-- lcRepNmTtl hold the header of the non major segment for the frx
*-- lcSortTtl  hold the sort type
*-- lcGroupExp hold the expression of the report first group
*-- lcSortExp  hold the expression of the report second group
*-- lcMajExp   hold the expression of the major seg. expression
*-- lcNMajExp  hold the expression of the nonmajor seg. expression
*-- lcDescExp  hold the expression of the description expression
*-- lcSortFld  hold the field name which the sort will be upon

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()

*-- lcTrnsAgin   Variable to hold temp. name to be used to open the temp.
*--              file lcTrns again with another alias.
*-- lnMajLen     Variable to hold the style major length
*-- lcSizeHed    Variable to hold the sizes header
*-- lcRecapHed   Variable to hold the recap header

lcTrnsAgin = ''
lnMajLen   = 0
lcSizeHed  = ''
lcRecapHed = PADR(lcRepNMTtl , 12) + SPACE(1) + PADR('Description' , 20) +;
             SPACE(1) + PADR('Div.' , 6) + SPACE(1) + PADR('Sea.' , 6) +;
             SPACE(1) + PADR('Pattern' , 10) + SPACE(1) + PADL('WIP' , 8) +;
             SPACE(1) + PADL('Stock' , 8) + SPACE(1) + PADL('Avail.' , 8) +;
             SPACE(1) + PADL('Orders' , 8) + SPACE(1) + PADL('O-T-S' , 8) +;
             SPACE(1) + PADL('Shipped' , 8) + SPACE(1) + PADL('Plan' , 8) +;
             SPACE(1) + PADL('Booked' , 8)

STORE SPACE(0) TO lcNonMCode,lcNonMDesc,lcDiv,lcSea,lcPat
STORE 0 To lnWIP,lnStk,lnAva,lnOrd,lnOTS,lnShp,lnBok

*-- This is to create and declare the variables that hold the totals 
*-- in the Group footer

= lfCrTmp()  
= lfDatCollect()

IF llRPBySize
  lcGrp2Exp = [EVAL(lcTrns+".cStyMajor")+EVAL(lcTrns+".StyCode")]
ELSE  
  lcGrp2Exp = [EVAL(lcTrns+'.cStyMajor')]
ENDIF  

SELECT(lcTrns)
LOCATE
DO gfDispRe WITH EVAL('lcRPFormNa')
WAIT CLEAR
*-- end of Report code.

IF USED(lcTrnsAgin)
  USE IN (lcTrnsAgin)
ENDIF  

IF USED(lcTrns)
  SELECT(lcTrns)
  SET RELATION TO
  USE IN (lcTrns)
ENDIF  
IF USED(lcDummy)
  SELECT(lcDummy)
  SET RELATION TO
  USE IN (lcDummy)
ENDIF  

ERASE (gcWorkDir+lcTrns+".DBF")
ERASE (gcWorkDir+lcTrns+".CDX")
ERASE (gcWorkDir+lcDummy+".DBF")
ERASE (gcWorkDir+lcDummy+".CDX")

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
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

*--End of lfMajTtGet.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : To get the style nonmajor segement structure.
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

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- Check the cost access

DIMENSION laRPPrnItm[14]
llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')
lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  OTHERWISE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
ENDCASE

*-- Disable/enable Only This colors, Free Segment. [end]

= lfvBySize()

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfvBySize
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Validate print by size option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvBySize()
*!*************************************************************
FUNCTION lfvBySize

lnRecapPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'llRPRecap'),1)
llRPRecap = IIF(llRPBySize,llRPRecap,.F.)
laOGObjCnt[lnRecapPo] = llRPBySize
= lfOGShowGet('llRPRecap')

lnRPOTSMin = IIF(llRPBySize,lnRPOTSMin,0)
lnOTSMinPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRPOTSMin'),1)
laOGObjCnt[lnOTSMinPo] = llRPBySize
= lfOGShowGet('lnRPOTSMin')

lcRPFormNa = IIF(llRPBySize,'ICCTSLHS','ICCTSLHO')
=lfRepPltFr(lcRPFormNa)

*--End of lfvBySize.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Get major Segment picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Validate Style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvStyle()
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Validate Fabric.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvFabric()
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

*--End of lfvFabric.
*!*************************************************************
*! Name      : lfCrTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Create temp. files
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCrTmp()
*!*************************************************************
FUNCTION lfCrTmp

lcTrns   = gfTempName()

*-- if the user is going to print the recap 
IF llRPRecap
  lcTrnsAgin = gfTempName()
ENDIF    && End of IF llRPRecap

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyCode'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyDesc'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStyMajor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Status'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'WIP8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWIP'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Stk8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotStk'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Plan8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotPlan'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTS8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PosOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NegOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NetOTS'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Ord8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Bok8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotBok'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Shp8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotShp'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Season'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Division'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'FGroup'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SGroup'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Pattern'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Last'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*B607330,1 KHM 06/09/2003 (Begin) Adding the following fileds to be displayed in the planned WIP
FOR lnCntr = 1 TO 8
  lcCntr = STR(lnCntr,1)
  lnI = ALEN(laFileStru,1)+1
  DIMENSION laFileStru[lnI,4]
  laFileStru[lnI,1] = 'nPlanWIP'+lcCntr
  laFileStru[lnI,2] = 'N'
  laFileStru[lnI,3] = 8
  laFileStru[lnI,4] = 0
ENDFOR
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nPlanWIP'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0
*B607330,1 KHM 06/09/2003 (End)

DIMENSION laIndx[1,2]
laIndx[1,1] = "StyCode+Last"
laIndx[1,2] = "StySort"

=gfCrtTmp(lcTrns,@laFileStru,@laIndx)

*--End of lfCrTmp.
*!*************************************************************
*! Name      : lfDatCollect
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Collecting data in temp. file.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDatCollect()
*!*************************************************************
FUNCTION lfDatCollect

PRIVATE lnOTSQty

SELECT STYLE
SET ORDER TO STYLE DESCENDING
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE

SELECT STYLE
GO TOP
lcMajor = SPACE(19)
SCAN FOR &lcRpExp
  WAIT WINDOW 'Selecting Records For The Report ...' + STYLE.STYLE NOWAIT
  lnOTSQty =  STYLE.TotStk+IIF(lcRPBased='W',STYLE.TotWIP,STYLE.TotPlan)-STYLE.TotOrd
  IF (lnRPOTSMin >= 0 AND lnOTSQty >=lnRPOTSMin) OR ;
     (lnRPOTSMin <= 0 AND lnOTSQty <=lnRPOTSMin)

    *B607330,1 KHM 06/09/2003 (Begin) If the new option (Plan WIP only =Yes) the planned
    *B607330,1                WIP should be greater than zero
    *IF llRpPlnWIP AND ;
    *  STYLE.TotPlan - ((STYLE.TotSTK+IIF(lcRPBased='W',STYLE.TotWIP,STYLE.TotPlan))-STYLE.TotOrd) <=0
    *  LOOP
    *ENDIF
    IF llRpPlnWIP
      llLoop = .T.
      FOR lnCntr = 1 TO 8
        lcCntr = STR(lnCntr,1)
        IF Style.Plan&lcCntr - ((STYLE.STK&lcCntr+IIF(lcRPBased='W',STYLE.WIP&lcCntr,STYLE.Plan&lcCntr))-STYLE.Ord&lcCntr) > 0
          llLoop = .F.
          EXIT
        ENDIF
      ENDFOR
      IF llLoop
        LOOP
      ENDIF  
    ENDIF
    *B607330,1 KHM 06/09/2003 (End)

    IF llRPRecap .AND. lcMajor # STYLE.cStyMajor
      DO lpInsInTmp WITH 'Y'
    ENDIF
    DO lpInsInTmp WITH 'N'
    lcMajor = STYLE.cStyMajor
  ENDIF
ENDSCAN

SELECT STYLE
SET ORDER TO STYLE ASCENDING
SET RELATION TO

SELECT (lcTrns)
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE
lnMajLen = LEN(lcMajPic) - 3        && Variable to hold the style major length

*-- if the user is going to print the recap 
IF llRPRecap
  SET RELATION TO IIF(&lcTrns..Last = 'Y' ,;
                      PADR(ALLTRIM(&lcTrns..cStyMajor) , lnMajLen) ,;
                      SPACE(19)) INTO STYLE ADDITIVE
  SET SKIP TO STYLE
ENDIF    && End of IF llRPRecap

*-- if the user is going to print the recap 
IF llRPRecap
  USE (gcWorkDir + lcTrns) AGAIN ALIAS (lcTrnsAgin) IN 0
  SET ORDER TO TAG StySort IN (lcTrnsAgin)
  SELECT STYLE
  SET RELATION TO Style + 'N' INTO (lcTrnsAgin) ADDITIVE
ENDIF    && End of IF llRPRecap

*--End of lfDatCollect.
*!*************************************************************
*! Name      : lpInsInTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Insert record in the temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpInsInTmp()
*!*************************************************************
PROCEDURE lpInsInTmp

PARAMETERS lcIsLast
PRIVATE lnCurAlias 

lnCurAlias = SELECT(0)

SELECT(lcTrns)

APPEND BLANK
REPLACE StyCode   WITH STYLE.Style,;
        StyDesc   WITH STYLE.Desc1,;
        cStyMajor WITH STYLE.cStyMajor,;
        Status    WITH STYLE.Status,;
        Scale     WITH STYLE.Scale,;
        Season    WITH STYLE.Season,;
        Division  WITH STYLE.cDivision,;
        FGroup    WITH STYLE.Fabric,;
        SGroup    WITH STYLE.cStyGroup,;
        Pattern   WITH STYLE.Pattern,;
        Last      WITH lcIsLast

REPLACE WIP1      WITH STYLE.WIP1,;
        WIP2      WITH STYLE.WIP2,;
        WIP3      WITH STYLE.WIP3,;
        WIP4      WITH STYLE.WIP4,;
        WIP5      WITH STYLE.WIP5,;
        WIP6      WITH STYLE.WIP6,;
        WIP7      WITH STYLE.WIP7,;
        WIP8      WITH STYLE.WIP8,;
        TOTWIP    WITH STYLE.TOTWIP,;
        Stk1      WITH STYLE.STK1,;
        Stk2      WITH STYLE.STK2,;
        Stk3      WITH STYLE.STK3,;
        Stk4      WITH STYLE.STK4,;
        Stk5      WITH STYLE.STK5,;
        Stk6      WITH STYLE.STK6,;
        Stk7      WITH STYLE.STK7,;
        Stk8      WITH STYLE.STK8,;
        TOTStk    WITH STYLE.TOTSTK

REPLACE Plan1     WITH STYLE.Plan1,;
        Plan2     WITH STYLE.Plan2,;
        Plan3     WITH STYLE.Plan3,;
        Plan4     WITH STYLE.Plan4,;
        Plan5     WITH STYLE.Plan5,;
        Plan6     WITH STYLE.Plan6,;
        Plan7     WITH STYLE.Plan7,;
        Plan8     WITH STYLE.Plan8,;
        TOTPlan   WITH STYLE.TotPlan
          
REPLACE OTS1      WITH lfOTSCalc('1'),;
        OTS2      WITH lfOTSCalc('2'),;
        OTS3      WITH lfOTSCalc('3'),;
        OTS4      WITH lfOTSCalc('4'),;
        OTS5      WITH lfOTSCalc('5'),;
        OTS6      WITH lfOTSCalc('6'),;
        OTS7      WITH lfOTSCalc('7'),;
        OTS8      WITH lfOTSCalc('8'),;
        TOTOTS    WITH OTS1+OTS2+OTS3+OTS4+OTS5+OTS6+OTS7+OTS8

REPLACE ORD1      WITH STYLE.ORD1,;
        ORD2      WITH STYLE.ORD2,;
        ORD3      WITH STYLE.ORD3,;
        ORD4      WITH STYLE.ORD4,;
        ORD5      WITH STYLE.ORD5,;
        ORD6      WITH STYLE.ORD6,;
        ORD7      WITH STYLE.ORD7,;
        ORD8      WITH STYLE.ORD8,;
        TOTORD    WITH STYLE.TOTORD
        
REPLACE BOK1      WITH STYLE.SHP1+STYLE.ORD1,;
        BOK2      WITH STYLE.SHP2+STYLE.ORD2,;
        BOK3      WITH STYLE.SHP3+STYLE.ORD3,;
        BOK4      WITH STYLE.SHP4+STYLE.ORD4,;
        BOK5      WITH STYLE.SHP5+STYLE.ORD5,;
        BOK6      WITH STYLE.SHP6+STYLE.ORD6,;
        BOK7      WITH STYLE.SHP7+STYLE.ORD7,;
        BOK8      WITH STYLE.SHP8+STYLE.ORD8,;
        TOTBOK    WITH STYLE.TOTSHP+STYLE.TOTORD,;
        SHP1      WITH STYLE.SHP1,;
        SHP2      WITH STYLE.SHP2,;
        SHP3      WITH STYLE.SHP3,;
        SHP4      WITH STYLE.SHP4,;
        SHP5      WITH STYLE.SHP5,;
        SHP6      WITH STYLE.SHP6,;
        SHP7      WITH STYLE.SHP7,;
        SHP8      WITH STYLE.SHP8,;
        TOTSHP    WITH STYLE.TOTSHP

*B607330,1 KHM 06/09/2003 (Begin) Replacing the planned WIP figures.
FOR lnCounter = 1 TO 8
  lcCounter = STR(lnCounter,1)
  REPLACE nPlanWip&lcCounter WITH MAX(Plan&lcCounter - OTS&lcCounter,0)
ENDFOR
REPLACE nPlanWip WITH nPlanWip1+nPlanWip2+nPlanWip3+nPlanWip4+nPlanWip5+nPlanWip6+nPlanWip7+nPlanWip8
*B607330,1 KHM 06/09/2003 (End)

SELECT(lnCurAlias)

*--End of lpInsInTmp.
*!*************************************************************
*! Name      : lfOTSCalc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Calculate OTS.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOTSCalc()
*!*************************************************************
FUNCTION lfOTSCalc

PARAMETER lcSz
PRIVATE lnRet
lnRet = 0

lnRet = EVAL('STYLE.STK'+lcSz)+;
        IIF(lcRPBased='W',EVAL('STYLE.WIP'+lcSz),EVAL('STYLE.Plan'+lcSz))-;
        EVAL('STYLE.Ord'+lcSz)


lcFld = IIF(lnRet>0,'PosOTS','NegOTS')
REPLACE  &lcFld WITH &lcFld + lnRet,;
         NetOTS WITH NetOTS + lnRet

RETURN lnRet

*--End of lfOTSCalc.
*!*************************************************************
*! Name      : lfMajExp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Reconstruct the major seg. expression.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfMajExp()
*!*************************************************************
FUNCTION lfMajExp

RETURN lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Warehouse')-LEN(lcMajTtl),0)) + ALLTRIM(EVAL(lcTrns+".cStyMajor"))

*--End of lfMajExp.
*!*************************************************************
*! Name      : lfNMajExp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Reconstruct the nonmajor seg. expression.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNMajExp()
*!*************************************************************
FUNCTION lfNMajExp

lcRet = ALLTRIM(lcRepNMTtl)+SPACE(1)+;
        PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0)) +;
        ALLTRIM(SUBSTR(EVAL(lcTrns+".StyCode"),lnNonMajPo)) + ;
        '          Scale : '+EVAL(lcTrns+'.Scale')+'          SEA/DIV : '+;
        EVAL(lcTrns+'.Season')+'/'+EVAL(lcTrns+'.Division')+;
        '       Pattern : '+EVAL(lcTrns+'.Pattern')

RETURN lcRet 

*--End of lfNMajExp.
*!*************************************************************
*! Name      : lfDescExp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Reconstruct the description expression.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDescExp()
*!*************************************************************
FUNCTION lfDescExp

lcSizeHed = SPACE(18) +;
            PADL(ALLTRIM(Scale.Sz1) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz2) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz3) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz4) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz5) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz6) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz7) , 5) + SPACE(4) +;
            PADL(ALLTRIM(Scale.Sz8) , 5) + SPACE(4) +;
            'Total' + SPACE(4) + 'Neg-OTS' + SPACE(4) +;
            'Pos-OTS' + SPACE(4) + 'Net-OTS'

RETURN 'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))+EVAL(lcTrns+".StyDesc")

*--End of lfDescExp.
*!*************************************************************
*! Name      : lfAdjHead
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Printing the recap. head if it is style last record.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAdjHead()
*!*************************************************************
FUNCTION lfAdjHead

IF &lcTrns..Last='Y' 
  llPrinted = .T.

  lnTrnRec = RECNO(lcTrns)
  lnStyRec = RECNO('STYLE')
  IF SEEK(STYLE.Style,lcTrns)
    lcNonMCode = SUBSTR(&lcTrns..StyCode,lnNonMajPo)
    lcNonMDesc = &lcTrns..StyDesc
    lcDiv      = &lcTrns..Division
    lcSea      = &lcTrns..Season
    lcPat      = &lcTrns..Pattern
    lnWIP      = &lcTrns..TotWIP
    lnStk      = &lcTrns..TotStk
    lnAva      = IIF(lcRPBased = 'W',&lcTrns..TotWIP,&lcTrns..TotPlan)+&lcTrns..TotStk
    lnOrd      = &lcTrns..TotOrd
    lnOTS      = &lcTrns..TotOTS
    lnShp      = &lcTrns..TotShp
    lnBok      = &lcTrns..TotBok

    lnSTotWIP      = lnSTotWIP + lnWIP
    lnSTotStk      = lnSTotStk + lnStk
    lnSTotAva      = lnSTotAva + lnAva
    lnSTotOrd      = lnSTotOrd + lnOrd
    lnSTotOTS      = lnSTotOTS + lnOTS
    lnSTotShp      = lnSTotShp + lnShp
    lnSTotBok      = lnSTotBok + lnBok

    lnTotWIP      = lnTotWIP + lnWIP
    lnTotStk      = lnTotStk + lnStk
    lnTotAva      = lnTotAva + lnAva
    lnTotOrd      = lnTotOrd + lnOrd
    lnTotOTS      = lnTotOTS + lnOTS
    lnTotShp      = lnTotShp + lnShp
    lnTotBok      = lnTotBok + lnBok

  ENDIF
  IF RECCOUNT(lcTrns) >= lnTrnRec
    GO lnTrnRec IN (lcTrns)
  ENDIF
  IF RECCOUNT('STYLE') >= lnStyRec
    GO lnStyRec IN ('STYLE')
  ENDIF

ENDIF  

RETURN ''

*--End of lfAdjHead.
*!*************************************************************
*! Name      : lfSetSTY
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/04/2002
*! Purpose   : Go top in the style IN RANGE.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSetSTY()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.