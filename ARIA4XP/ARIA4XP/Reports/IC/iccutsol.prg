*:***************************************************************************
*! Program file        : ICCUTSOL.PRG
*! Program description : Style Cut & Sold
*! For System          : Aria Advantage Series - Version oldAriaSql
*! For Module          : Inventory Control (IC)
*! Developer Name      : BASSEM RAAFAT ERNEST(BWA)
*! Tracking Job Number : N037557
*! Date                : 04/18/2006
*!--------------------------------------------------------------------------
*! Calls               : ICCUSO.FRX , ICCUSOS.FRX
*!--------------------------------------------------------------------------
*! Called From         : System Menu
*!--------------------------------------------------------------------------
*! Passed Parameters   : None.
*!--------------------------------------------------------------------------
*! Example             : DO ICCUTSOL
*!--------------------------------------------------------------------------
*! Modifications       :
*! N000536,1 AYM 12/12/2006:CONVERT CUT & SOLD REPORT TO GRAPHICS FOR TICKET NO : T20060809.0029 
*!---------------------------------------------------------------------------------------------------

*-- lcRepNmTtl hold the header of the non major segment for the frx
*-- lcSortTtl  hold the sort type
*-- lcGroupExp hold the expression of the report first group
*-- lcSortExp  hold the expression of the report second group
*-- lcMajExp   hold the expression of the major seg. expression
*-- lcNMajExp  hold the expression of the nonmajor seg. expression
*-- lcDescExp  hold the expression of the description expression
*-- lcSortFld  hold the field name which the sort will be upon
*-- lcTrnsAgin   Variable to hold temp. name to be used to open the temp.
*--              file lcTrns again with another alias.
*-- lnMajLen     Variable to hold the style major length
*-- lcSizeHed    Variable to hold the sizes header
*-- lcRecapHed   Variable to hold the recap header

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lcTrnsAgin = ''
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

=lfCrTmp()  
=lfDatCollect()

IF llRPBySize
  lcGrp2Exp = [EVAL(lcTrns+".cStyMajor")+EVAL(lcTrns+".StyCode")]
ELSE  
  lcGrp2Exp = [EVAL(lcTrns+'.cStyMajor')]
ENDIF  

SELECT(lcTrns)
LOCATE
*!N000536,1 12/12/2006 AYM:CONVERT CUT &SOLD REPORT TO GRAPHICS .. BEGIN
loogScroll.cCROrientation = 'P'
*!N000536,1 12/12/2006 AYM:CONVERT CUT &SOLD REPORT TO GRAPHICS .. END

DO gfDispRe WITH EVAL('lcRPFormNa')


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
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--Get the mask of Major Segment & Color Segment
lnMajorlen=LEN(gfItemMask("PM","",lcInvType))
lnColorLen=LEN(gfItemMask("PN","",lcInvType))

*--Make Temp. File From Item Location File
IF !llFirstTim
  lcSelFld1= "SELECT ITEMLOC.STYLE ,ITEMLOC.CWARECODE,ITEMLOC.NTOTHUSAGE AS USAGE ,ITEMLOC.TOTSTK AS ONHAND ,;
              ITEMLOC.TOTWIP AS ONORDER ,ITEMLOC.NSTKVAL ,ITEMLOC.DYELOT FROM ITEMLOC " 
  lcSelFld1= lcSelFld1 + "  WHERE ITEMLOC.CINVTYPE= '" +lcInvType+"'"
  lnResult1 = loOGScroll.orda.SqlRun (lcSelFld1,lcFabDye,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  llFirstTim = .T.
ENDIF 

IF lnResult1 >=1
  lnBuffering = CURSORGETPROP("Buffering",lcFabDye)
  =CURSORSETPROP("Buffering",3,lcFabDye)
  SELECT (lcFabDye)
  INDEX ON STYLE+CWARECODE+DYELOT TAG lcFabDye
ENDIF 


*--Item File
IF TYPE('loDBFItem') <> 'O'
  loDBFItem = CreateObject("RemoteTable","ITEM",,"CursorITEM",SET("DATASESSION"))
ENDIF

*-- Check the cost access
DIMENSION laRPPrnItm[14]
llCostAccs = gfUserPriv('IC','ICSTYLE','COSTING')
lnClrSgPo = ASUBSCRIPT(laOGFxFlt,;
            ASCAN(laOGFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = ASUBSCRIPT(laOGFxFlt,;
            ASCAN(laOGFxFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnFreSgPo] = .T.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  OTHERWISE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
ENDCASE

*-- Disable/enable Only This colors, Free Segment. [end]

= lfvBySize()

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfvBySize
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

lnRecapPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LLRPRECAP'),1)
llRPRecap = IIF(llRPBySize,llRPRecap,.F.)
laOGObjCnt[lnRecapPo] = llRPBySize
= lfOGShowGet('llRPRecap')

lnRPOTSMin = IIF(llRPBySize,lnRPOTSMin,0)
lnOTSMinPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'LNRPOTSMIN'),1)
laOGObjCnt[lnOTSMinPo] = llRPBySize
= lfOGShowGet('lnRPOTSMin')

lcRPFormNa = IIF(llRPBySize,'ICCUSOS','ICCUSO')
=lfRepPltFr(lcRPFormNa)

*--End of lfvBySize. 
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--End of lfMajPic.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--End of lfvFabric.
*!*************************************************************
*! Name      : lfCrTmp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

lcTrns   = gfTempName()

*-- Add this line to get a temp. name to be used to open the temp.
*-- file lcTrns again with another alias. [Begin]
*-- if the user is going to print the recap 
IF llRPRecap
  lcTrnsAgin = gfTempName()
ENDIF    && End of IF llRPRecap

DIMENSION laFileStru[1,18]
  
=lfAddField("laFileStru", "StyCode" ,"C",19,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "StyDesc" ,"C",60,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "cStyMajor" ,"C",60,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Status" ,"C",1,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Scale" ,"C",3,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "WIP8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotWIP" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Stk8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotStk" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Plan8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotPlan" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "OTS8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotOTS" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "PosOTS" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "NegOTS" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "NetOTS" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Ord8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotOrd" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Bok8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotBok" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp1" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp2" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp3" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp4" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp5" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp6" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp7" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Shp8" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "TotShp" ,"N",8,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Season" ,"C",6,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Division" ,"C",6,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "FGroup" ,"C",7,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "SGroup" ,"C",6,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Pattern" ,"C",10,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "Last" ,"C",1,0,.T.,.F.,"","","","","","","","","","",0,0)

DIMENSION laIndx[1,2]
laIndx[1,1] = "StyCode+Last"
laIndx[1,2] = "StySort"

=gfCrtTmp(lcTrns,@laFileStru,@laIndx)

*--End of lfCrTmp.
*!*************************************************************
*! Name      : lfDatCollect
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

PRIVATE lnOTSQty

SELECT STYLE
SET ORDER TO STYLE DESCENDING
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE

SELECT STYLE
GO TOP
lcMajor = SPACE(19)
SCAN FOR &lcRpExp
  lnOTSQty =  STYLE.TotStk+IIF(lcRPBased='W',STYLE.TotWIP,STYLE.TotPlan)-STYLE.TotOrd
  IF (lnRPOTSMin >= 0 AND lnOTSQty >=lnRPOTSMin) OR ;
     (lnRPOTSMin <= 0 AND lnOTSQty <=lnRPOTSMin)

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

*-- Add this line to add to get the style major length.
lnMajLen = LEN(lcMajPic) - 3

*-- if the user is going to print the recap 
IF llRPRecap
  SET RELATION TO IIF(&lcTrns..Last = 'Y' ,;
                      PADR(ALLTRIM(&lcTrns..cStyMajor) , lnMajLen) ,;
                      SPACE(19)) INTO STYLE ADDITIVE
  
  SET SKIP TO STYLE
ENDIF

*-- Add these lines to use the other alias of the temp. file
*   lcTrns (lcTrnsAgin) to print the recap part [Begin]
*-- if the user is going to print the recap 
IF llRPRecap
  USE (gcWorkDir + lcTrns) AGAIN ALIAS (lcTrnsAgin) IN 0
  SET ORDER TO TAG StySort IN (lcTrnsAgin)
  SELECT STYLE
  SET RELATION TO Style + 'N' INTO (lcTrnsAgin) ADDITIVE
ENDIF

*--End of lfDatCollect.
*!*************************************************************
*! Name      : lpInsInTmp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

SELECT(lnCurAlias)

*--End of lpInsInTmp.
*!*************************************************************
*! Name      : lfOTSCalc
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

lcFld = IIF(lnRet>0,'PosOTS','NegOTS')
REPLACE  &lcFld WITH &lcFld + lnRet,;
         NetOTS WITH NetOTS + lnRet

RETURN lnRet

*--End of lfOTSCalc.
*!*************************************************************
*! Name      : lfMajExp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

RETURN lcMajTtl + SPACE(9) + PADL(': ',2+MAX(LEN('Warehouse')-LEN(lcMajTtl),0)) + ALLTRIM(EVAL(lcTrns+".cStyMajor"))

*--End of lfMajExp.
*!*************************************************************
*! Name      : lfNMajExp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

lcRet = ALLTRIM(lcRepNMTtl)+SPACE(6)+;
        PADL(": ",2+MAX(LEN('Description')-LEN(ALLTRIM(lcRepNMTtl)),0)) +;
        ALLTRIM(SUBSTR(EVAL(lcTrns+".StyCode"),lnNonMajPo)) + ;
        '          Scale : '+EVAL(lcTrns+'.Scale')+'          SEA/DIV : '+;
        EVAL(lcTrns+'.Season')+'/'+EVAL(lcTrns+'.Division')+;
        '       Pattern : '+EVAL(lcTrns+'.Pattern')

RETURN lcRet 

*--End of lfNMajExp.
*!*************************************************************
*! Name      : lfDescExp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

RETURN 'Description'+SPACE(2)+PADL(': ',2+MAX(LEN(ALLTRIM(lcRepNMTtl))-LEN('Description'),0))+EVAL(lcTrns+".StyDesc")

*--End of lfDescExp.
*!*************************************************************
*! Name      : lfAdjHead
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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

*--End of lfAdjHead.
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
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
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!**************************************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
*! Purpose   : Add fields to the array of file structure.
*!**************************************************************************************
*! Called from :
*!**************************************************************************************
*! Passed Parameters : 1) Array name.
*!  				 : 2) lcFldName -- Field Name
*!                   : 3) lcFldType -- Field Type
*!                   : 		                       1) C = Characters
*!                   : 							   2) Y = Currency
*!                   : 							   3) D = Date
*!                   : 							   4) T = DateTime
*!                   : 							   5) B = Double
*!                   : 							   6) F = Float
*!                   : 							   7) G = General
*!                   : 							   8) I = Integer
*!                   : 							   9) L = Logical
*!                   : 						      10) M = Memo
*!                   : 						      11) N = Numeric
*!                   : 						      12) Q = Varbinary
*!                   : 						      13) V = Varchar and Varchar (Binary)
*!                   : 						      14) W = Blob
*!                   : 4) lnFldLen  -- Field width                        >> Numeric
*!                   : 5) lnFldDec  -- Field Decimal places               >> Numeric
*!                   : 6) ln5       -- Null values allowed                >> Logical
*!                   : 7) ln6       -- Code page translation not allowed  >> Logical
*!                   : 8) ln7       -- Field validation expression        >> Character
*!                   : 9) ln8       -- Field validation text
*!                   :10) ln9       -- Field default value
*!                   :11) ln10      -- Table validation expression
*!                   :12) ln11      -- Table validation text
*!                   :13) ln12      -- Long table name
*!                   :14) ln13      -- Insert trigger expression
*!                   :15) ln14      -- Update trigger expression
*!                   :16) ln15      -- Delete trigger expression
*!                   :17) ln16      -- Table comment
*!                   :18) ln17      -- NextValue for autoincrementing     >> Numeric
*!                   :19) ln18      -- Step for autoincrementing          >> Numeric
*!**************************************************************************************
*! Return      : None
*!**************************************************************************************
*! Example     : 
*! =lfAddField("laFileStru", "Status" ,"C",3,0,.F.,.F.,"","","","","","","","","","",0,0)
*!
*! FOR lnCrtTmp = 1 TO 8
*!   lcNumQty = ALLTRIM(STR(lnCrtTmp+8))
*!   =lfAddField("laFileStru", "Qty"  + lcNumQty ,"N",6,0,.F.,.F.,"","","","","","","","","","",0,0)
*! ENDFOR
*!**************************************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec ,ln5,ln6,ln7,ln8,ln9,ln10,ln11,ln12,ln13,ln14,ln15,ln16,ln17,ln18

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

FOR lnCount = 5 TO 18
  lcParam = 'ln' + ALLTRIM(STR(lnCount))
  IF lnCount = 5 OR lnCount = 6
    &lcStruArry[lnFldPos, lnCount] = &lcParam
  ENDIF
  IF BETWEEN(lnCount , 7 , 16)
    &lcStruArry[lnFldPos ,lnCount] = &lcParam
  ENDIF
  IF lnCount = 17 OR lnCount = 18
    &lcStruArry[lnFldPos , lnCount] = &lcParam
  ENDIF
ENDFOR 

*--End of lfAddField.
*!*************************************************************
*! Name      : lfSumFab1
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 04/18/2006
*! Purpose   : Sum a specific field for the current fabric 
*!           : in fabric file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : (Item.STYLE,Item.Calculated Field)
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfSumFab1()
*!*************************************************************
FUNCTION lfSumFab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
LOCAL lnAlias

lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcFabDye)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcFabDye)
  LOCATE 
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE SUBSTR(STYLE,1,lnMajorLen)= SUBSTR(lcFab,1,lnMajorLen) AND EMPTY(DYELOT)
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
SELECT(lnAlias)
RETURN INT(lnTotcomp)

*--End of lfSumFab1.
