*:***************************************************************************
*: Program file  : ICCTSAPP.PRG
*: Program desc. : Cut & Sold
*: For Report    : ICCUSO.FRX,ICCUSOAP.FRX
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : TMI tarek mohammed ibrahim  
*: Modified for APPRESS BY : This is a copy of the cut& sold report modified by TMI tarek mohammed ibrahim - 08/14/2001
*: C102397
*:***************************************************************************
*:
*: Calls : Procedures : lpInsInTmp()
*:
*: Calls : FUNCTIONS  : lfMajTtGet()
*:                    : lfNonMaj()
*:                    : lfwRepWhen()
*:                    : lfvBySize()
*:                    : lfMajPic()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:                    : lfCrTmp()
*:                    : lfArrUpd()
*:                    : lfDatCollect()
*:                    : lfOTSCalc()
*:                    : lfMajExp()
*:                    : lfNMajExp()
*:                    : lfDescExp()
*:                    : lfAdjHead()
*:                    : lfSetSty()
*:                    : lfInitVars()
*:                    : lfCollect()
*:                    : lfAddRec()
*:                    : lfZap()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCUTSOL
*:***************************************************************************
*:Modifications
*:B804419,1 TMI 09/19/2001 Print only 4 styles in each page to prevent a style from printed in 2 separate pages
*:B607963,1 TMI 02/07/2007 Fix a bug that the report crashes when pages are more than 1000 page
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

lnMajLen   = 0
lcSizeHed  = ''
lcRecapHed = PADR(lcRepNMTtl , 19) + SPACE(1) + PADR('Description' , 20) +;
             SPACE(1) + PADR('Div.' , 6) + SPACE(1) + PADR('Sea.' , 6) +;
             SPACE(1) + PADR('Pattern' , 10) + SPACE(1) + PADL('WIP' , 8) +;
             SPACE(1) + PADL('Stock' , 8) + SPACE(1) + PADL('Avail.' , 8) +;
             SPACE(1) + PADL('Orders' , 8) + SPACE(1) + PADL('O-T-S' , 8) +;
             SPACE(1) + PADL('Shipped' , 8) + SPACE(1) + PADL('Booked' , 8)             

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

GO TOP
DO gfDispRe WITH EVAL('lcRPFormNa')
*-- end of Report code.

IF USED(lcTrnsAgin)
  USE IN (lcTrnsAgin)
ENDIF  
IF USED(lcStyAlias)
  USE IN (lcStyAlias)
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

*-- Function section

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
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
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
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
*! Developer : TMI tarek mohammed ibrahim
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

DIMENSION laRPPrnItm[14]
*MAN
*llCostAccs = gfUserPriv('IC','ICSTYLE')
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

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvBySize
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Validate print by size option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvBySize()
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

lcRPFormNa = IIF(llRPBySize,'ICCUSOAP','ICCUSO')
=lfRepPltFr(lcRPFormNa)

*!*************************************************************
*! Name      : lfMajPic
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Get major seg. picture
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

*!*************************************************************
*! Name      : lfvStyle
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfCrTmp
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
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
*! Example     : = lfCrTmp()
*!*************************************************************

FUNCTION lfCrTmp

*--Empty lcTrns file
=lfZap(lcTrns)

*-- if the user is going to print the recap 
IF llRPRecap
  =lfZap(lcTrnsAgin)
  IF !lfZap(lcStyAlias)
    CREATE CURSOR (lcStyAlias) (STYLE C(19))
    INDEX ON STYLE TAG STYLE
  ENDIF
ENDIF    && End of IF llRPRecap

DIMENSION laFileStru[1,4]
=lfArrUpd('StyCode','C',19,0)
=lfArrUpd('StyDesc','C',60,0)
=lfArrUpd('cStyMajor','C',60,0)
=lfArrUpd('Status','C',1,0)
=lfArrUpd('Scale','C',3,0)
*--WIP
=lfArrUpd('WIP1','N',8,0)
=lfArrUpd('WIP2','N',8,0)
=lfArrUpd('WIP3','N',8,0)
=lfArrUpd('WIP4','N',8,0)
=lfArrUpd('WIP5','N',8,0)
=lfArrUpd('WIP6','N',8,0)
=lfArrUpd('WIP7','N',8,0)
=lfArrUpd('WIP8','N',8,0)
=lfArrUpd('WIP9','N',8,0)
=lfArrUpd('WIP10','N',8,0)
=lfArrUpd('WIP11','N',8,0)
=lfArrUpd('TotWIP','N',8,0)
*--STK
=lfArrUpd('Stk1','N',8,0)
=lfArrUpd('Stk2','N',8,0)
=lfArrUpd('Stk3','N',8,0)
=lfArrUpd('Stk4','N',8,0)
=lfArrUpd('Stk5','N',8,0)
=lfArrUpd('Stk6','N',8,0)
=lfArrUpd('Stk7','N',8,0)
=lfArrUpd('Stk8','N',8,0)
=lfArrUpd('Stk9','N',8,0)
=lfArrUpd('Stk10','N',8,0)
=lfArrUpd('Stk11','N',8,0)
=lfArrUpd('TotStk','N',8,0)
*--PLAN
=lfArrUpd('Plan1','N',8,0)
=lfArrUpd('Plan2','N',8,0)
=lfArrUpd('Plan3','N',8,0)
=lfArrUpd('Plan4','N',8,0)
=lfArrUpd('Plan5','N',8,0)
=lfArrUpd('Plan6','N',8,0)
=lfArrUpd('Plan7','N',8,0)
=lfArrUpd('Plan8','N',8,0)
=lfArrUpd('Plan9','N',8,0)
=lfArrUpd('Plan10','N',8,0)
=lfArrUpd('Plan11','N',8,0)
=lfArrUpd('TotPlan','N',8,0)
*--OTS
=lfArrUpd('OTS1','N',8,0)
=lfArrUpd('OTS2','N',8,0)
=lfArrUpd('OTS3','N',8,0)
=lfArrUpd('OTS4','N',8,0)
=lfArrUpd('OTS5','N',8,0)
=lfArrUpd('OTS6','N',8,0)
=lfArrUpd('OTS7','N',8,0)
=lfArrUpd('OTS8','N',8,0)
=lfArrUpd('OTS9','N',8,0)
=lfArrUpd('OTS10','N',8,0)
=lfArrUpd('OTS11','N',8,0)
=lfArrUpd('TotOTS','N',8,0)

=lfArrUpd('PosOTS','N',8,0)
=lfArrUpd('NegOTS','N',8,0)
=lfArrUpd('NetOTS','N',8,0)
*--ORD
=lfArrUpd('Ord1','N',8,0)
=lfArrUpd('Ord2','N',8,0)
=lfArrUpd('Ord3','N',8,0)
=lfArrUpd('Ord4','N',8,0)
=lfArrUpd('Ord5','N',8,0)
=lfArrUpd('Ord6','N',8,0)
=lfArrUpd('Ord7','N',8,0)
=lfArrUpd('Ord8','N',8,0)
=lfArrUpd('Ord9','N',8,0)
=lfArrUpd('Ord10','N',8,0)
=lfArrUpd('Ord11','N',8,0)
=lfArrUpd('TotOrd','N',8,0)
*--BOK
=lfArrUpd('Bok1','N',8,0)
=lfArrUpd('Bok2','N',8,0)
=lfArrUpd('Bok3','N',8,0)
=lfArrUpd('Bok4','N',8,0)
=lfArrUpd('Bok5','N',8,0)
=lfArrUpd('Bok6','N',8,0)
=lfArrUpd('Bok7','N',8,0)
=lfArrUpd('Bok8','N',8,0)
=lfArrUpd('Bok9','N',8,0)
=lfArrUpd('Bok10','N',8,0)
=lfArrUpd('Bok11','N',8,0)
=lfArrUpd('TotBok','N',8,0)
*--SHP
=lfArrUpd('Shp1','N',8,0)
=lfArrUpd('Shp2','N',8,0)
=lfArrUpd('Shp3','N',8,0)
=lfArrUpd('Shp4','N',8,0)
=lfArrUpd('Shp5','N',8,0)
=lfArrUpd('Shp6','N',8,0)
=lfArrUpd('Shp7','N',8,0)
=lfArrUpd('Shp8','N',8,0)
=lfArrUpd('Shp9','N',8,0)
=lfArrUpd('Shp10','N',8,0)
=lfArrUpd('Shp11','N',8,0)
=lfArrUpd('TotShp','N',8,0)
*--Mesic
=lfArrUpd('Season','C',6,0)
=lfArrUpd('Division','C',6,0)
=lfArrUpd('FGroup','C',7,0)
=lfArrUpd('SGroup','C',6,0)
=lfArrUpd('Pattern','C',10,0)
*--Sizes
=lfArrUpd('SZ1','C',5,0)
=lfArrUpd('SZ2','C',5,0)
=lfArrUpd('SZ3','C',5,0)
=lfArrUpd('SZ4','C',5,0)
=lfArrUpd('SZ5','C',5,0)
=lfArrUpd('SZ6','C',5,0)
=lfArrUpd('SZ7','C',5,0)
=lfArrUpd('SZ8','C',5,0)
=lfArrUpd('SZ9','C',5,0)
=lfArrUpd('SZ10','C',5,0)
=lfArrUpd('SZ11','C',5,0)
*--LAST
=lfArrUpd('Last','C',1,0)
*B804419,1 TMI [Start] Add nPage field to save page# , and print only 4 styles in each page
*B607963,1 TMI [Start] allow to print more than 1000 pages
*=lfArrUpd('nPage','N',3,0)
=lfArrUpd('nPage','N',6,0)
*B607963,1 TMI [End  ] 
*B804419,1 TMI [End  ]

*--INDECES
DIMENSION laIndx[1,2]
laIndx[1,1] = "StyCode+Last"
laIndx[1,2] = "StySort"

=gfCrtTmp(lcTrns,@laFileStru,@laIndx)

*:**************************************************************************
*:* Name        : lfArrUpd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/01
*:* Purpose     : Update the array laFileStru
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfArrUpd()
*:***************************************************************************
FUNCTION lfArrUpd
PARAMETERS lcFldName,lcType,lnLen,lnDec
PRIVATE lnI
IF TYPE('laFileStru') = 'L'
  lnI = 1
ELSE
  lnI = ALEN(laFileStru,1)+1  
ENDIF
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = lcFldName
laFileStru[lnI,2] = lcType
laFileStru[lnI,3] = lnLen
laFileStru[lnI,4] = lnDec

*-- end of lfArrUpd.

*!*************************************************************
*! Name      : lfDatCollect
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Modified by : TMI 08/14/2001
*! Purpose   : Collecting data in temp. file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDatCollect()
*!*************************************************************

FUNCTION lfDatCollect

*--Initliaize Variables
SELECT (lcTrns)
SCATTER MEMV BLANK
SELECT STYLE 
SCATTER MEMV

*-- lnScaleCnt --> Holds scale sizes count
STORE 0 TO lnScaleCnt,lnPosOTS,lnNegOTS,lnNetOTS

SELECT STYLE
SET ORDER TO STYLE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE

SELECT STYLE
LOCATE
lcMajor = SPACE(19)

lnStyLen = LEN(lcMajPic)+lnColorLen-2
lcStyleClr = SPACE(lnStyLen)

SCAN FOR &lcRpExp
  llNewStyle = (SUBSTR(STYLE.STYLE,1,lnStyLen) # lcStyleClr)
  
  *B607963,1 TMI [Start] notification wait window
  WAIT WINDOW NOWAIT  STYLE
  *B607963,1 TMI [End  ]   
  

  IF llNewStyle                && new style color
    
    IF !EMPTY(lcStyleClr)
      *--Print last style data
      IF lfAddRec()
        IF llRPRecap AND STYLE.cStyMajor # lcMajor
          DO lpInsInTmp WITH 'Y'
        ENDIF
        DO lpInsInTmp WITH 'N'
      ENDIF
    ENDIF
    lcStyleClr = SUBSTR(STYLE.STYLE,1,lnStyLen)
    lcMajor = STYLE.cStyMajor
     
    *--Initialize variables   
    =lfInitVars()
    
    *--Collect data for sizes of the 1st scale.
    lnScaleCnt = SCALE.CNT
    
    *--For each style collect data for sizes for this scale
    =lfCollect(1,lnScaleCnt)

  ELSE
      
    =lfCollect(lnScaleCnt+1,lnScaleCnt + SCALE.CNT)
    lnScaleCnt = lnScaleCnt + SCALE.CNT
    
  ENDIF    
  
ENDSCAN

*B607963,1 TMI [Start] clear the window
WAIT CLEAR
*B607963,1 TMI [End  ] 

IF !EMPTY(lcStyleClr) AND lfAddRec()
  IF llRPRecap AND STYLE.cStyMajor # lcMajor
    DO lpInsInTmp WITH 'Y'
  ENDIF
  DO lpInsInTmp WITH 'N'
ENDIF

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
                      SPACE(19)) INTO (lcStyAlias) ADDITIVE
  SET SKIP TO (lcStyAlias)

  USE (gcWorkDir + lcTrns) AGAIN ALIAS (lcTrnsAgin) IN 0
  SET ORDER TO TAG StySort IN (lcTrnsAgin)
  SELECT (lcStyAlias)
  SET RELATION TO SUBSTR(Style,1,LEN(lcMajPic)+lnColorLen-2)+SPACE(4) + 'N' INTO (lcTrnsAgin) ADDITIVE
ENDIF    && End of IF llRPRecap

*B804419,1 TMI [Start] Update page# field to print only 4 styles in each page
SELECT (lcTrns)
LOCATE
lnPage = 1
ln4 = 0
SCAN
  REPLACE nPage WITH lnPage
  ln4 = ln4 + 1  
  IF ln4 = 4
    ln4 = 0
    lnPage = lnPage + 1
  ENDIF 
ENDSCAN
LOCATE
*B804419,1 TMI [End  ]


*!*************************************************************
*! Name      : lpInsInTmp
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Modified by : TMI 08/14/2001
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
*! Example     : DO lpInsInTmp
*!*************************************************************

PROCEDURE lpInsInTmp
PARAMETERS M.Last

*--Insert a line in lcTrns
INSERT INTO (lcTrns) FROM MEMVAR

*--Insert a line in lcStyAlias
IF llRPRecap AND M.Last='N'
  IF !EMPTY(M.StyCode) AND !SEEK(M.StyCode,(lcStyAlias))
    INSERT INTO (lcStyAlias) VALUES (M.StyCode)
  ENDIF
ENDIF
*-- end of lpInsInTmp.

*!*************************************************************
*! Name      : lfOTSCalc
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Modified by : TMI 08/14/2001
*! Purpose   : Calculate OTS
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOTSCalc()
*!*************************************************************
FUNCTION lfOTSCalc
PARAMETER lcSz
PRIVATE lnRet
lnRet = 0

lnRet = EVAL('STYLE.STK'+lcSz)+;
        IIF(lcRPBased='W',EVAL('STYLE.WIP'+lcSz),EVAL('STYLE.Plan'+lcSz))-;
        EVAL('STYLE.Ord'+lcSz)

lcOTSVar = IIF(lnRet>0,'lnPosOTS','lnNegOTS')
&lcOTSVar = &lcOTSVar + lnRet
lnNetOTS  = lnNetOTS  + lnRet

RETURN lnRet
*-- end of lfOTSCalc.

*!*************************************************************
*! Name      : lfMajExp
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Reconstruct the major seg. expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajExp()
*!*************************************************************

FUNCTION lfMajExp

RETURN lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Warehouse')-LEN(lcMajTtl),0)) + ALLTRIM(EVAL(lcTrns+".cStyMajor"))

*!*************************************************************
*! Name      : lfNMajExp
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Reconstruct the nonmajor seg. expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajExp()
*!*************************************************************

FUNCTION lfNMajExp

lcRet = ALLTRIM(lcRepNMTtl) + SPACE(1) +;
        PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0)) +;
        ALLTRIM(SUBSTR(EVAL(lcTrns+".StyCode"),lnNonMajPo)) + ;
        '          Scale : '+LEFT(EVAL(lcTrns+'.Scale'),1)+'            SEA/DIV : '+;
        EVAL(lcTrns+'.Season')+'/'+EVAL(lcTrns+'.Division')+;
        '       Pattern : '+EVAL(lcTrns+'.Pattern')

RETURN lcRet 

*!*************************************************************
*! Name      : lfDescExp
*! Developer : TMI tarek mohammed ibrahim
*! Modified by : TMI 08/14/2001
*! Date      : 08/14/2001
*! Purpose   : Reconstruct the description expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDescExp()
*!*************************************************************
FUNCTION lfDescExp
lcSizeHed = SPACE(16) +;
            PADL(ALLTRIM(Sz1 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz2 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz3 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz4 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz5 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz6 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz7 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz8 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz9 ) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz10) , 5) + SPACE(3) +;
            PADL(ALLTRIM(Sz11) , 5) + SPACE(3) +;
            'Total' + SPACE(2) + '-OTS' + SPACE(4)+;
            '+OTS' + SPACE(2) + 'Net-OTS'            

RETURN 'Description'+SPACE(1)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))+EVAL(lcTrns+".StyDesc") 

*!*************************************************************
*! Name      : lfAdjHead
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Printing the recap. head if it is style last record
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjHead()
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

*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : TMI tarek mohammed ibrahim
*! Date      : 08/14/2001
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty  
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE


*:**************************************************************************
*:* Name        : lfInitVars
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/12/2001
*:* Purpose     : Initialize variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfInitVars()
*:***************************************************************************
FUNCTION lfInitVars

*--Blank needed variables
SELECT (lcTrns)
SCATTER MEMVAR BLANK

*--Get values from style file
SELECT STYLE
SCATTER MEMVAR 

*--Assure that some variables are zeros to calc their values later
STORE 0 TO M.WIP1,M.WIP2,M.WIP3,M.WIP4,M.WIP5,M.WIP6,M.WIP7,M.WIP8,M.WIP9,M.WIP10,M.WIP11,;
           M.STK1,M.STK2,M.STK3,M.STK4,M.STK5,M.STK6,M.STK7,M.STK8,M.STK9,M.STK10,M.STK11,;
           M.PLAN1,M.PLAN2,M.PLAN3,M.PLAN4,M.PLAN5,M.PLAN6,M.PLAN7,M.PLAN8,M.PLAN9,M.PLAN10,M.PLAN11,;
           M.OTS1,M.OTS2,M.OTS3,M.OTS4,M.OTS5,M.OTS6,M.OTS7,M.OTS8,M.OTS9,M.OTS10,M.OTS11,;
           M.ORD1,M.ORD2,M.ORD3,M.ORD4,M.ORD5,M.ORD6,M.ORD7,M.ORD8,M.ORD9,M.ORD10,M.ORD11,;
           M.SHP1,M.SHP2,M.SHP3,M.SHP4,M.SHP5,M.SHP6,M.SHP7,M.SHP8,M.SHP9,M.SHP10,M.SHP11,;
           M.OTS1,M.OTS2,M.OTS3,M.OTS4,M.OTS5,M.OTS6,M.OTS7,M.OTS8,M.OTS9,M.OTS10,M.OTS11,;
           lnPosOTS,lnNegOTS,lnNetOTS

*-- end of lfInitVars.



*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/12/2001
*:* Purpose     : Collect data for sizes from lnSzA to lnSzB
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCollect()
*:***************************************************************************
FUNCTION lfCollect
PARAMETERS lnSzA,lnSzB
PRIVATE lnCnt,lcCnt,lnStyCnt
lcStyCnt = '1'

FOR lnCnt = lnSzA TO lnSzB
  lcCnt = ALLT(STR(lnCnt))

  M.WIP&lcCnt  = EVAL('STYLE.WIP'+lcStyCnt)
  M.STK&lcCnt  = EVAL('STYLE.STK'+lcStyCnt)
  M.PLAN&lcCnt = EVAL('STYLE.PLAN'+lcStyCnt)
  M.ORD&lcCnt  = EVAL('STYLE.ORD'+lcStyCnt)
  M.SHP&lcCnt  = EVAL('STYLE.SHP'+lcStyCnt)
  M.OTS&lcCnt  = lfOTSCalc(lcStyCnt)

  M.BOK&lcCnt  =  M.SHP&lcCnt+M.ORD&lcCnt
  M.SZ&lcCnt   = EVAL('SCALE.SZ'+lcStyCnt)

  lcStyCnt = STR(VAL(lcStyCnt)+1,1)
ENDFOR
*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfAddRec
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/12/2001
*:* Purpose     : Can add this record depending on OTS qty.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddRec()
*:***************************************************************************
FUNCTION lfAddRec
PRIVATE llAddRec
*--is this record will be added depending on OTS qty
M.TOTOTS  = M.OTS1+M.OTS2+M.OTS3+M.OTS4+M.OTS5+M.OTS6+M.OTS7+M.OTS8+M.OTS9+M.OTS10+M.OTS11

llAddRec = (lnRPOTSMin >= 0 AND M.TOTOTS >=lnRPOTSMin) OR ;
           (lnRPOTSMin <= 0 AND M.TOTOTS <=lnRPOTSMin)

*--Update values of other variables befor the insert into command
IF llAddRec

  M.TOTWIP  = M.WIP1+M.WIP2+M.WIP3+M.WIP4+M.WIP5+M.WIP6+M.WIP7+M.WIP8+M.WIP9+M.WIP10+M.WIP11
  M.TOTSTK  = M.STK1+M.STK2+M.STK3+M.STK4+M.STK5+M.STK6+M.STK7+M.STK8+M.STK9+M.STK10+M.STK11
  M.TOTPLAN = M.PLAN1+M.PLAN2+M.PLAN3+M.PLAN4+M.PLAN5+M.PLAN6+M.PLAN7+M.PLAN8+M.PLAN9+M.PLAN10+M.PLAN11
  M.TOTORD  = M.ORD1+M.ORD2+M.ORD3+M.ORD4+M.ORD5+M.ORD6+M.ORD7+M.ORD8+M.ORD9+M.ORD10+M.ORD11
  M.TOTSHP  = M.SHP1+M.SHP2+M.SHP3+M.SHP4+M.SHP5+M.SHP6+M.SHP7+M.SHP8+M.SHP9+M.SHP10+M.SHP11
  M.TOTBOK  = M.BOK1+M.BOK2+M.BOK3+M.BOK4+M.BOK5+M.BOK6+M.BOK7+M.BOK8+M.BOK9+M.BOK10+M.BOK11
  
  M.PosOTS = lnPosOTS
  M.NegOTS = lnNegOTS
  M.NetOTS = lnNetOTS
  
  M.StyCode  =  PADR(SUBSTR(M.Style,1,lnStyLen),19)
  M.StyDesc  =  M.Desc1
  M.Division =  M.cDivision
  M.FGroup   =  M.Fabric
  M.SGroup   =  M.cStyGroup

ENDIF

RETURN llAddRec
*-- end of lfAddRec.

*:**************************************************************************
*:* Name        : lfZap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/14/2001
*:* Purpose     : If a file is open Zap it
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfZap()
*:***************************************************************************
FUNCTION lfZap
PARAMETERS lcFile
PRIVATE lcSlct,llZapped
llZapped = .F.
lcSlct = SELECT(0)

IF USED(lcFile)
  SELECT (lcFile)
  zap
  llZapped = .T.
ENDIF

SELECT (lcSlct)
RETURN llZapped
*-- end of lfZap.