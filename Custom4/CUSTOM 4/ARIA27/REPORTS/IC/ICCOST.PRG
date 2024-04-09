*:***************************************************************************
*: Program file  : ICCOST
*: Program desc. : Costing
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCOST
*:***************************************************************************
*:B802009,1 MAN 03/21/1999 Fix wrong positioning and display for some fields
*:B802009,1 MAN            Fix wrong behaviour for some of the OG objects
*:B802009,1 MAN            Added selection in the option grid to either base 
*:B802009,1 MAN            the markup on the standard or the average cost 
*:B802401,1 WAB - 07/25/99 - style filter needs to be changed to style browse 
*:B802401,1                  to enable select range
*B603348,1  ABD 02/07/2000 Increase The Picture Of The purchase price per item [UnTCost]
*B603348,1                 To 9999999.999 & the Picture Of The TotCost [ ToTCost ] 
*B603348,1                 To 999999999999.99 Instead of the fields.
*:B603734,1 AMH 07/09/2000 To Add Currency code
*:***************************************************************************
*-- lcRepNmTtl hold the header of the non major segment for the frx
*-- llDontPrn  if there is no records matched the criteria it is .F.
*-- lcTime     hold time
*-- lnMajLen   hold major segment lenght

llDontPrn = .F.

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))

DO lpPrepPrnt

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


*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

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

lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')

*B802009,1 MAN Disable the Item Cost PopUp
laOGObjCnt[4] = .F.
= lfOGShowGet('lcRPPrnItm')

*-- end of lfwRepWhen.



*!*************************************************************
*! Name      : lfvDomImp
*! Developer : MAN
*! Date      : 03/22/1999
*! Purpose   : validate Domestic/Import Style Selection
*!             As fix for B802009,1 MAN Created the fucntion
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDomImp()
*!*************************************************************

FUNCTION lfvDomImp
PARAMETER lcLoadOG
*-- Use To Display OG PopUp Get Cost No.     Get Setup Values
DIMENSION  laRPCstDsp[7,1], laRPCstRet[7,1], laCstSetUp[5,2]

*-- Get Cost Labels From Setup for either Manufatured or Imported Styles
lcImpOrMfg = IIF(TYPE("lcRPDomImp") # "C" .OR. lcRPDomImp = "M","M","I")
FOR lnNo = 1 TO 5
  laCstSetUp[lnNo,1]  = 'M_C'+ lcImpOrMfg +'SLBL'+STR(lnNo,1)
ENDFOR
= gfGetMemVar(@laCstSetUp)

*-- Fill Popup and Equivelant Cost No. arrays
FOR lnNo = 1 TO 5
  laRPCstDsp[lnNo,1] = laCstSetUp[lnNo,2]
  laRPCstRet[lnNo,1] = STR(lnNo,1)
ENDFOR

*-- Add None and All to the arrays
laRPCstDsp[6,1] = "None"
laRPCstDsp[7,1] = "All"
laRPCstRet[6,1] = 'N'
laRPCstRet[7,1] = 'A'

*-- If this variable is defined that means that the OG is active
*-- and we have to refresh the Print Item PopUp
IF TYPE("lcLoadOG") = "C"  .AND. lcLoadOG # "Y"
  = lfOGShowGet('lcRPPrnItm')
ENDIF  
*!*************************************************************
*! Name      : lfvPrintBy
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate print by option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrintBy()
*!*************************************************************

FUNCTION lfvPrintBy

lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

IF lcRPPrntBy = 'C'
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
  ENDCASE
  *B802009,1 MAN Enable the Item Cost PopUp
  laOGObjCnt[4] = .T.
  = lfOGShowGet('lcRPPrnItm')
ELSE
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  
  *B802009,1 MAN Disable the Item Cost PopUp
  laOGObjCnt[4] = .F.
  = lfOGShowGet('lcRPPrnItm')
ENDIF

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

*B802009,1 MAN Fill The array for the cost Items Popup with corresponding
*B802009,1 MAN Cost Items {MFG or Imp}
=lfvDomImp("Y")
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
*! Name      : lpPrepPrnt
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : restoring memory variables for costing items 
*!             (preparing for printing)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrepPrnt()
*!*************************************************************

PROCEDURE lpPrepPrnt

*-- RESTORE Memory variables (Start)
DIMENSION laSetUps[22,2]
laSetUps[01,1]  = 'M_CMTYPE1'
laSetUps[02,1]  = 'M_CMTYPE2'
laSetUps[03,1]  = 'M_CMTYPE3'
laSetUps[04,1]  = 'M_CMTYPE4'
laSetUps[05,1]  = 'M_CMTYPE5'
laSetUps[06,1]  = 'M_CITYPE1'
laSetUps[07,1]  = 'M_CITYPE2'
laSetUps[08,1]  = 'M_CITYPE3'
laSetUps[09,1]  = 'M_CITYPE4'
laSetUps[10,1]  = 'M_CITYPE5'
laSetUps[11,1]  = 'M_CISLBL1'
laSetUps[12,1]  = 'M_CISLBL2'
laSetUps[13,1]  = 'M_CISLBL3'
laSetUps[14,1]  = 'M_CISLBL4'
laSetUps[15,1]  = 'M_CISLBL5'
laSetUps[16,1]  = 'M_CMSLBL1'
laSetUps[17,1]  = 'M_CMSLBL2'
laSetUps[18,1]  = 'M_CMSLBL3'
laSetUps[19,1]  = 'M_CMSLBL4'
laSetUps[20,1]  = 'M_CMSLBL5'
laSetUps[21,1]  = 'M_STYMARK'
laSetUps[22,1]  = 'M_COST_MET'
= gfGetMemVar(@laSetups)
lcIcType1 = laSetUps[01,2]
lcIcType2 = laSetUps[02,2]
lcIcType3 = laSetUps[03,2]
lcIcType4 = laSetUps[04,2]
lcIcType5 = laSetUps[05,2]
lcMcType1 = laSetUps[06,2]
lcMcType2 = laSetUps[07,2]
lcMcType3 = laSetUps[08,2]
lcMcType4 = laSetUps[09,2]
lcMcType5 = laSetUps[10,2]
lcCISLBL1 = LEFT(laSetUps[11,2],9)
lcCISLBL2 = LEFT(laSetUps[12,2],9)
lcCISLBL3 = LEFT(laSetUps[13,2],9)
lcCISLBL4 = LEFT(laSetUps[14,2],9)
lcCISLBL5 = LEFT(laSetUps[15,2],9)
lcCMSLBL1 = LEFT(laSetUps[16,2],9)
lcCMSLBL2 = LEFT(laSetUps[17,2],9)
lcCMSLBL3 = LEFT(laSetUps[18,2],9)
lcCMSLBL4 = LEFT(laSetUps[19,2],9)
lcCMSLBL5 = LEFT(laSetUps[20,2],9)
lcStyMark = laSetUps[21,2]
lcCostMth = laSetUps[22,2]
*-- RESTORE Memory variables (End)

XTITLE    = lcRPTitle
XBOMPRT   = lcRPPrnItm
XMAKE     = lcRPDomImp
XSORT     = lcRPPrntBy
XAVG_COST = lcCostMth<>'S'
*B802009,1 MAN Added The Doemstic or Importing Flag to the filter
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',' .AND. ')  
lcRpExp = lcRpExp + IIF(lcRPDomImp="M","","!") + "MAKE"

STORE '0' TO lcFType,lcTType,lcMType,lcMType2,lcSType,lcPType,lcDType

IF xMake='M'
  FOR I=1 TO 5
    Z=STR(I,1)
    DO CASE
     CASE lcMcType&Z = 'F'
       lcFType = Z
     CASE lcMcType&Z = 'T'
       lcTType = Z
     CASE lcMcType&Z = 'M' .AND. lcMType='0'
       lcMType = Z
     CASE lcMcType&Z = 'S'
       lcSType = Z
     CASE lcMcType&Z = 'M' .AND. lcMType<>'0'
       lcMType2 = Z
    ENDCASE 
  ENDFOR
ELSE
  FOR I=1 TO 5
    Z=STR(I,1)
    DO CASE
     CASE lcIcType&Z = 'P'
       lcPType = Z
     CASE lcIcType&Z = 'S'
       lcSType = Z
     CASE lcIcType&Z = 'D'
       lcDType = Z
     CASE lcIcType&Z = 'F'
       lcFType = Z
     CASE lcIcType&Z = 'M'
       lcMType  = Z
       lcMType2 = Z
    ENDCASE 
  ENDFOR
ENDIF
*B802009,1 MAN Fix Filltering on the Cost Item Type, The commented case
*B802009,1 MAN structure works fine only for Imported Styles 
*B802009,1 MAN Start
*DO CASE
*  CASE XBOMPRT=lcFType
*    lcItemType="TYP=lcFType"
*  CASE XBOMPRT=lcTType
*    lcItemType="TYP=lcTType"
*  CASE XBOMPRT=lcMType
*    lcItemType="TYP=lcMType"
*  CASE XBOMPRT=lcMType2
*    lcItemType="TYP=lcMType2"
*  CASE XBOMPRT=lcSType
*    lcItemType="TYP=lcSType"
*  CASE XBOMPRT=lcPType
*    lcItemType="TYP=lcPType"
*  CASE XBOMPRT=lcDType
*    lcItemType="TYP=lcDType"
*  OTHE
*    lcItemType=IIF(XBOMPRT='A','.T.','.F.')
*ENDCASE

IF BETWEEN(XBOMPRT,"1","5")
 lcItemType="TYP=XBOMPRT"
ELSE
 lcItemType=IIF(XBOMPRT='A','.T.','.F.')
ENDIF

*B802009,1 MAN End Correct the filter on the cost types

R_TITLE = IIF(xMake='B','Costing for purchased styles','Costing for manufactured styles')
R_WIDTH = 'W'

ROW     = 99
PAGENO  = 0

lcBomFile='BOM'

WAIT WIND 'Report printing press <SPACE BAR>  to abort ...' NOWAIT

SET DEVICE TO PRINT
DO lpPrint

IF !llDontPrn
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF  

*!*************************************************************
*! Name      : lpPrint
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Print report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrint()
*!*************************************************************

PROCEDUR lpPrint

SELECT STYLE
SET FILTER TO &lcRPExp
GO TOP

IF EOF()
  Wait wind 'NO RECORDS SELECTED!!!'
  llDontPrn = .T.
  SET DEVICE TO SCREEN
ENDIF  

*B603348,1 ABD Get the Picture Of Non major segment & Get The Weight of the NonMajor
*B603348,1     To Print The header Upon the Weight Of The non Major If Print Color Only . [ Begin ]
IF XSORT='C'
  lcNonjPo = gfItemMask('PN')
  lnNonjPo = Len (lcNonjPo)
EndIF 
*B603348,1 ABD [ End ]


DO WHILE .T.
  IF EOF()
    EXIT
  ENDIF

  XSTYLE = SUBSTR(STYLE,1,lnMajLen)
  lcStyle = STYLE.Style
  
  IF ROW > 55
    PAGENO = PAGENO +1
    DO RPT_HDR WITH 'ICCOST',XTITLE,R_WIDTH
    @ 005,000 SAY PADR(lcMajTtl,19) + ' PATTERN    SEASON  DIVISION  GROUP   SIZE---------------------------------SCALE---------------------------------'
    @ 006,000 SAY REPLICATE('=',132)
    ROW = 7
  ENDIF

  SELE CODES
  XSEAS_DATA = gfCodDes(STYLE.SEASON,'SEASON')
  XSEAS_DATA = IIF(STYLE->SEASON='Y ','YEAR ROUND',XSEAS_DATA)
  XDIVI_DATA = gfCodDes(STYLE.cDivision,'cDivision')
  XGROU_DATA = gfCodDes(STYLE.cStyGroup,'CSTYGROUP')
  SELECT STYLE

  @ ROW,00 SAY SUBSTR(STYLE,1,lnMajLen)
  *B802009,1 MAN Fix Printing the Pattern 
  *lcpattern = 'XXXXXXXXXX'
  *@ ROW,20 SAY lcPATTERN
  @ ROW,20 SAY PATTERN
  @ ROW,31 SAY SEASON
  @ ROW,39 SAY cDIVISION
  @ ROW,49 SAY cStyGROUP

  XSCALE = GETSCALE(SCALE,SPACE(1))
  @ ROW,57 SAY STYLE->SCALE
  @ ROW,62 SAY XSCALE
  IF XSORT='C'
    ROW=ROW+1
    @ ROW,000 SAY REPLICATE('-',132)
  ENDIF
  ROW=ROW+1
  lcSp=SPACE(1)
  IF XSORT='C'
    *B603348,1 ABD Remark The next Line & Move Them To The Left & Right to Indicate The Layout. [Begin]
    *lcHd1 = PADR(lcNonMajT,lnNonMajPo) + ' ' + 'Desc.' + SPACE(26-LEN(PADR(lcNonMajT,lnNonMajPo) + ' ' + 'Desc.'))
    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *lcHd1 = PADR(lcNonMajT,lnNonjPo)  +'   '+'Desc.'+ SPACE(8)
    lcHd1 = PADR(lcNonMajT,lnNonjPo)  +'   '+'Desc.'+ SPACE(2)
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)
    *B603348,1 ABD [ End ]
  ELSE
    *B603348,1 ABD Remark The next Line to Decrease The Weight Of DESCRIPTION 
    *B603348,1     to Indicate The Layout. [Begin]
    *lcHd1 = 'DESCRIPTION             '
    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *lcHd1 = 'DESCRIPTION           '    
    lcHd1 = 'DESCRIPTION     '
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)
    *B603348,1 ABD [ End ]
  ENDIF
  IF XMAKE='B'
    *B603348,1 ABD Remark The next Line to Decrease & Increase The Weight Of 
    *B603348,1     Some filed In The headre Indicate The Layout. [Begin]
    *lcHd2 = PADL(ALLTRIM(lcCISLBL1),9)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL2),9)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL3),9)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL4),9)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL5),9)
    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *lcHd2 = PADL(ALLTRIM(lcCISLBL1),9)+'     '+;
    *        PADL(ALLTRIM(lcCISLBL2),9)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL3),8)+lcSp+' '+;
    *        PADL(ALLTRIM(lcCISLBL4),8)+lcSp+;
    *        PADL(ALLTRIM(lcCISLBL5),8)
    lcHd2 = PADL(ALLTRIM(lcCISLBL1),9)+'     '+;
            PADL(ALLTRIM(lcCISLBL2),9)+lcSp+'   '+;
            PADL(ALLTRIM(lcCISLBL3),8)+lcSp+'    '+;
            PADL(ALLTRIM(lcCISLBL4),8)+lcSp+'   '+;
            PADL(ALLTRIM(lcCISLBL5),8)+'  '
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)
  *B603348,1 ABD [ End ]
  
  ELSE
    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *lcHd2 = PADL(ALLTRIM(lcCMSLBL1),9)+lcSp+;
    *        PADL(ALLTRIM(lcCMSLBL2),9)+lcSp+;
    *        PADL(ALLTRIM(lcCMSLBL3),9)+lcSp+;
    *        PADL(ALLTRIM(lcCMSLBL4),9)+lcSp+;
    *        PADL(ALLTRIM(lcCMSLBL5),9)
    lcHd2 = PADL(ALLTRIM(lcCMSLBL1),9)+'     '+;
            PADL(ALLTRIM(lcCMSLBL2),9)+lcSp+'   '+;
            PADL(ALLTRIM(lcCMSLBL3),8)+lcSp+'    '+;
            PADL(ALLTRIM(lcCMSLBL4),8)+lcSp+'   '+;
            PADL(ALLTRIM(lcCMSLBL5),8)+'   '
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)
  ENDIF
  *B603348,1 ABD Remark The next Line to Move TotCost to left 
  *B603348,1     to Indicate The Layout of The headre . [Begin]
  *lchd3 = "  TOTCOST PRICE A  MARKUP PRICE B  MARKUP PRICE C  MARKUP"
  *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
  *lchd3  = "     TOTCOST PRICE A MARKUP PRICE B  MARKUP PRICE C  MARKUP"
  lchd3  = "     TOTCOST PRICEA MARKUP PRICEB MARKUP PRICEC MARKUP"
  *B603734,1 AMH 07/09/2000 To Add Currency code (End)
  *B603348,1 ABD [ End ]
    
  @ ROW,00 SAY lcHd1+lcHd2+lchd3
  ROW=ROW+1

  ***** SCAN FOR ALL STYLE COLORS *****
  SELECT STYLE
  DO WHILE SUBSTR(STYLE,1,lnMajLen)=XSTYLE
    XCOLOR=SUBSTR(STYLE.Style,lnNonMajPo)
    *-------------CALCULATE THE MARKUP --------*
    *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG                
    *IF PRICEA<>0 .AND. (IIF(XAVG_COST,AVE_COST<>0,TOTCOST<>0))
    IF PRICEA<>0 .AND. (IIF(lcRPMarkUp="A",AVE_COST<>0,TOTCOST<>0))    
      IF lcStyMark='T'
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG
        *XMARKA=((PRICEA-IIF(XAVG_COST,AVE_COST,TOTCOST));
                /(IIF(XAVG_COST,AVE_COST,TOTCOST)))*100
        XMARKA=((PRICEA-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST));
                /(IIF(XAVG_COST,AVE_COST,TOTCOST)))*100
        
      ELSE
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG
        *XMARKA=((PRICEA-IIF(XAVG_COST,AVE_COST,TOTCOST))/PRICEA) * 100
        XMARKA=((PRICEA-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST))/PRICEA) * 100
      ENDIF
    ELSE
      XMARKA   = 0
    ENDIF
    *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG                
    *IF PRICEB<>0 .AND. (IIF(XAVG_COST,AVE_COST<>0,TOTCOST<>0))
    IF PRICEB<>0 .AND. (IIF(lcRPMarkUp="A",AVE_COST<>0,TOTCOST<>0))    
      IF lcStyMark='T'
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG
        *XMARKB=((PRICEB-IIF(XAVG_COST,AVE_COST,TOTCOST));
                /(IIF(XAVG_COST,AVE_COST,TOTCOST)))*100
        XMARKB=((PRICEB-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST));
                /(IIF(XAVG_COST,AVE_COST,TOTCOST)))*100
                
      ELSE
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG
        *XMARKB=((PRICEB-IIF(XAVG_COST,AVE_COST,TOTCOST))/PRICEB)*100
        XMARKB=((PRICEB-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST))/PRICEB)*100
        
      ENDIF
    ELSE
      XMARKB   = 0
    ENDIF
    *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG                
    *IF PRICEC<>0 .AND. (IIF(XAVG_COST,AVE_COST<>0,TOTCOST<>0))
    IF PRICEC<>0 .AND. (IIF(lcRPMarkUp="A",AVE_COST<>0,TOTCOST<>0))    
      IF lcStyMark='T'
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG            
        *XMARKC=((PRICEC-IIF(XAVG_COST,AVE_COST,TOTCOST));
                /(IIF(XAVG_COST,AVE_COST,TOTCOST)))*100
        XMARKC=((PRICEC-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST));
                /(IIF(lcRPMarkUp="A",AVE_COST,TOTCOST)))*100         
      ELSE
        *B802009,1 MAN Base Markup on either Std or Avg Cost based on the OG      
        *XMARKC=((PRICEC-IIF(XAVG_COST,AVE_COST,TOTCOST))/PRICEC)*100
        XMARKC=((PRICEC-IIF(lcRPMarkUp="A",AVE_COST,TOTCOST))/PRICEC)*100
      ENDIF
    ELSE
      XMARKC   = 0
    ENDIF
    *-------------------------------------------*
    IF XSORT='C'
      *B603348,1 ABD Remark The next Line to Decrease The Weight 
      *B603348,1     Of Color and Print 12 Char from The Desc. [ Begin ] 
      *lcNonMStr = PADR(XCOLOR,lnNonMajPo)+' '+LEFT(DESC,24-lnNonMajPo)
      lcNonMStr = PADR(XCOLOR,lnNonjPo)+' '+LEFT(DESC,18-lnNonjPo)
      *B603348,1 ABD [ End ]
      *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
      *@ ROW,000 SAY lcNonMStr
      @ ROW,000 SAY SUBSTR(lcNonMStr,1,11)
      *B603734,1 AMH 07/09/2000 To Add Currency code (End)
    ELSE
      *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
      *@ ROW,000 SAY SUBSTR(DESC,1,19) 
      @ ROW,000 SAY SUBSTR(DESC,1,11)
      *B603734,1 AMH 07/09/2000 To Add Currency code (End)
    ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
*lcNonbMajT--------- Desc. ±±±±±±±±± ±±±±±±±±± ±±±±±±±±± ±±±±±±±±± ±±±±±±±±±  TOTCOST PRICE A  MARKUP PRICE B  MARKUP PRICE C  MARKUP
*DESCRIPTION               999999.99 999999.99 999999.99 999999.99 999999.99 99999.99 9999.99 999.99% 9999.99 999.99% 9999.99 999.99%
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.

    *B603348,1 ABD Remark The next Line to Increase The Picture Of The NiCost1 &
    *B603348,1 NiCost1 to '99999999.99' Upon Increase the UntCost Filed
    *B603348,1  & Move Them To Left & Right To Indicate The Printing . [ Begin ]
    *@ ROW,026 SAY IIF(XMAKE='M',NMCOST1,NICOST1) PICTURE '999999.99'
    *@ ROW,036 SAY IIF(XMAKE='M',NMCOST2,NICOST2) PICTURE '999999.99'    
    *@ ROW,046 SAY IIF(XMAKE='M',NMCOST3,NICOST3) PICTURE '999999.99'
    *@ ROW,056 SAY IIF(XMAKE='M',NMCOST4,NICOST4) PICTURE '999999.99'
    *@ ROW,066 SAY IIF(XMAKE='M',NMCOST5,NICOST5) PICTURE '999999.99'
    *@ ROW,076 SAY TOTCOST PICTURE '99999.99'

    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *@ ROW,020 SAY IIF(XMAKE='M',NMCOST1,NICOST1) PICTURE '99999999.99'
    *@ ROW,034 SAY IIF(XMAKE='M',NMCOST2,NICOST2) PICTURE '99999999.99'
    *@ ROW,044 SAY IIF(XMAKE='M',NMCOST3,NICOST3) PICTURE '999999.99'
    *@ ROW,055 SAY IIF(XMAKE='M',NMCOST4,NICOST4) PICTURE '9999999.99'
    *@ ROW,064 SAY IIF(XMAKE='M',NMCOST5,NICOST5) PICTURE '999999.99'
    *@ ROW,074 SAY TOTCOST PICTURE '99999999.99'
    @ ROW,011 SAY IIF(XMAKE='M',NMCOST1,NICOST1) PICTURE '99999999.99'
    @ ROW,022 SAY CPRICECUR
    @ ROW,025 SAY IIF(XMAKE='M',NMCOST2,NICOST2) PICTURE '99999999.99'
    @ ROW,036 SAY CDUTYCUR
    @ ROW,039 SAY IIF(XMAKE='M',NMCOST3,NICOST3) PICTURE '999999.99'
    @ ROW,048 SAY gcBaseCurr
    @ ROW,051 SAY IIF(XMAKE='M',NMCOST4,NICOST4) PICTURE '9999999.99'
    @ ROW,061 SAY CDUTYCUR
    @ ROW,064 SAY IIF(XMAKE='M',NMCOST5,NICOST5) PICTURE '999999.99'
    @ ROW,073 SAY gcBaseCurr
    @ ROW,076 SAY TOTCOST PICTURE '99999999.99'
    @ ROW,087 SAY gcBaseCurr
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)
    *B603348,1 ABD [ End ] 
    *B603734,1 AMH 07/09/2000 To Add Currency code (Start)
    *@ ROW,085 SAY PRICEA PICTURE '9999.99'
    *@ ROW,093 SAY XMARKA PICTURE '999.99%'
    *@ ROW,101 SAY PRICEB PICTURE '9999.99'
    *@ ROW,109 SAY XMARKB PICTURE '999.99%'
    *@ ROW,117 SAY PRICEC PICTURE '9999.99'
    *@ ROW,125 SAY XMARKC PICTURE '999.99%'
    @ ROW,090 SAY PRICEA PICTURE '9999.99'
    @ ROW,097 SAY XMARKA PICTURE '999.99%'
    @ ROW,104 SAY PRICEB PICTURE '9999.99'
    @ ROW,111 SAY XMARKB PICTURE '999.99%'
    @ ROW,118 SAY PRICEC PICTURE '9999.99'
    @ ROW,125 SAY XMARKC PICTURE '999.99%'
    *B603734,1 AMH 07/09/2000 To Add Currency code (End)

    XROW_FLG=.F.
    *B802009,1 MAN The manufactured style could also have No detail costing
    *IF XSORT='C' .AND. ( lDetCost .OR. xMake='M' )
    IF XSORT='C' .AND. lDetCost 
      SELECT &lcBomFile
      IF !SEEK( XSTYLE )
        ROW=ROW+1
      ENDIF
      XROW_FLG=.F.              && FLAG TO INCREMENT ROW FOR THOSE COLORS THAT

                              && DO NOT HAVE ANY FABRIC
        
      llFrst=.T.
      DO WHILE citmmajor = XSTYLE
        IF &lcItemType .AND. ;
        ( LIKE(STRTRAN(citmmask,'*','?'),lcStyle) .OR. ( TYP $ lcMType+lcMType2) )
          IF llFrst
             ROW=ROW+1
             *B802009,1 MAN Correct Positioning
             *@ ROW,000 SAY 'ITEM         ' + PADR(lcNonMajT,19) + ' QTY.    COST    AMOUNT'
             *B603348,1  ABD Increase The Picture Of The purchase price per item [UnTCost]
             *B603348,1  To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
             *B603348,1  Instead of the fields that Need To Move Cost And Amount To The Right. [ Begin ]
             *@ ROW,000 SAY 'ITEM         ' + " COLOR " + '     QTY.    COST    AMOUNT'
             @ ROW,000 SAY 'ITEM         ' + " COLOR " + '     QTY.        COST        AMOUNT'
             *B603348,1  ABD [ End ]
             ROW=ROW+1
            llFrst=.F.
          ENDIF
          XROW_FLG=.T.

          IF !EMPTY(mfgCode)
            @ ROW,000 SAY DESC
          ELSE
            *B802009,1 MAN As the displayed item could be either a style component
            *B802009,1 MAN or Fabric, the IF ELSE distuingusih the print out
            *B802009,1 MAN Start fixing printing Item
            *@ ROW,000 SAY ITEM
            *@ ROW,015 SAY IIF(ICLR='******',XCOLOR,ICLR)  
            IF cCatgTyp = "S"
              @ ROW,000 SAY ITEM
            ELSE
              @ ROW,000 SAY LEFT(ITEM,12)
              @ ROW,014 SAY IIF(ICLR='******',XCOLOR,ICLR)  
            ENDIF  
            *B802009,1 MAN End fixing printing Item
          ENDIF 
          *B802009,1 MAN Start fixing Positions
          *@ ROW,039 SAY nBOMTOTQTY  PICTURE '999.999'
          *@ ROW,047 SAY UNTCOST PICTURE '999.999'
          *@ ROW,055 SAY TOTCOST PICTURE '999999.99'  
          @ ROW,022 SAY nBOMTOTQTY  PICTURE '999.999'
          
          *B603348,1  ABD Increase The Picture Of The purchase price per item [UnTCost]
          *B603348,1  To 9999999.999 & the Picture Of The TotCost [ ToTCost ] To 999999999999.99 
          *B603348,1  Instead of the fields. [ Begin ]
          *@ ROW,030 SAY UNTCOST PICTURE '999.999'
          *@ ROW,038 SAY TOTCOST PICTURE '999999.99'
          @ ROW,030 SAY UNTCOST PICTURE '9999999.999'
          @ ROW,042 SAY TOTCOST PICTURE '9999999999.99'
         *B603348,1  ABD [ End ]
        
          *B802009,1 MAN End fixing Positions

          ROW=ROW+1
        ENDIF

        IF ROW>55         && CHECK PAGE WHILE PRINTING ALL FABRICS
          SKIP                && ONLY TO CHECK IF MORE RECORDS TO PRINT SO PAGING IS DONE
          IF citmmajor=XSTYLE
            PAGENO = PAGENO +1
            DO RPT_HDR WITH 'ICCOST',XTITLE,R_WIDTH
                    @ 005,000 SAY PADR(lcMajTtl,19) + ' PATTERN    SEASON  DIVISION  GROUP   SIZE---------------------------------SCALE---------------------------------'
            @ 006,000 SAY REPLICATE('=',132)
            ROW = 7
            llFrst=.T.
          ENDIF
          SKIP -1
        ENDIF

        IF !EOF()
          SKIP
        ENDIF
      ENDDO
      IF XROW_FLG
        ROW=ROW+1
      ENDIF
    ELSE
      ROW=ROW+1         && FOR Detail costing no.
    ENDIF

    llContrl =.T.
    SELE STYLE
    IF ROW > 55
      SKIP
      IF .NOT. EOF()
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'ICCOST',XTITLE,R_WIDTH
                @ 005,000 SAY PADR(lcMajTtl,19) + ' PATTERN    SEASON  DIVISION  GROUP   SIZE---------------------------------SCALE---------------------------------'
        @ 006,000 SAY REPLICATE('=',132)
        ROW = 7
        XROW_FLG=.F.
        llContrl =.F.
      ENDIF
      SKIP -1
    ENDIF

    IF XSORT='C'
      IF !EOF()
        SKIP
      ENDIF
    ELSE
      SCAN WHILE SUBSTR(STYLE,1,lnMajLen)=XSTYLE
      ENDSCAN
    ENDIF
    *B802009,1 MAN Increment Row Number othwerise all colors will print on
    *B802009,1 MAN the same Row
    ROW=ROW+1
    
  ENDDO
  IF .NOT. XROW_FLG
    ROW=ROW+IIF(XSORT='C',1,0)
  ENDIF
  IF llContrl 
    @ ROW,000 SAY REPLICATE('=',132)
    ROW=ROW+1
  ENDIF    
  SELE STYLE
ENDDO
*B802401,1 - WAB - Add function for order the table when press button in range 
*B802401,1 		   at option  grad
*B802401,1 - WAB - Start
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 07/25/1999
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
*B802401,1 - WAB - END
