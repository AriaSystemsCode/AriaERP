*!***********************************************************************
*: Program file       : ICSTYLST
*: Program description: Custom Inventory Report for ACA10
*: Module             : Inventory Control (IC)
*: Developer          : Mariam Mazhar (MMT)
*: Tracking Job Number: C201575.122,C201576.exe[T20130128.0032]
*: Date               : 04/29/2013
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
#include r:\aria4xp\reports\ic\ICINVACA.h
lcStTime   = TIME()
llDontPrnt = .F.
lcTime     = gfGetTime()
llDummy    = loOgScroll.llOGFltCh AND lfCollData()
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.*--
SELECT (lcStyles)
GO TOP
IF EOF()
  = gfModalGen('TRM00052B40011','ALERT')
  RETURN 
ENDIF
WAIT WINDOW LANG_SELECTED +' ' + ALLTRIM(STR(RECCOUNT())) +;
            ' '+LANG_RECIN+' ' + ALLTRIM(STR(lnInterval,6,2)) + ' '+;
            LANG_SECO NOWAIT
*USE IN (lcStyles)            
loogScroll.cCROrientation = 'P'
*lfAdjCrParameters()
DO gfDispRe WITH EVAL('lcRepNam')

*!*************************************************************
*! Name      : lfCollData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 04/29/2013
*! Purpose   : Collection of Data
*!*************************************************************
*! Called from : This Program
*!*************************************************************
FUNCTION lfCollData

*Create Cursor to hold Data
lfCreateInvTemp()

*Style Major
llSelectStyle = .F. 
lcCursorStyle= ''
lnPosStyle = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CSTYMAJOR")
IF lnPosStyle > 0 
  lnPosStyle = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosStyle,1)
  lcCursorStyle= loOgScroll.laOgVRFlt[lnPosStyle,6]
  IF !EMPTY(lcCursorStyle)
    SELECT(lcCursorStyle)
    LOCATE
    IF !EOF()
      llSelectStyle = .T. 
    ENDIF   
  ENDIF 
ENDIF 



*SEASON
llUseSeason  = .F.
lnSeaPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.SEASON")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnSeaPos,6]),loOgScroll.laOgVRFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF   
ENDIF   

*DIVISION
llUseDiv  = .F.
lnDivPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CDIVISION")
IF lnDivPos > 0 
  lnDivPos = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnDivPos,1)
  lcDivSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnDivPos,6]),loOgScroll.laOgVRFlt[lnDivPos,6],'')
  IF !EMPTY(lcDivSel) 
    lcDivFile = loOGScroll.gfTempName()
    llUseDiv = IIF(LEN(lcDivSel)>0,.T.,.F.) AND lfConvertToCursor(lcDivSel,'CDIVISION',lcDivFile)
  ENDIF   
ENDIF   


*Style Group
llUseGrp  = .F.
lnGrpPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.CSTYGROUP")
IF lnGrpPos  > 0 
  lnGrpPos  = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnGrpPos ,1)
  lcGrpSel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnGrpPos ,6]),loOgScroll.laOgVRFlt[lnGrpPos ,6],'')
  IF !EMPTY(lcGrpSel) 
    lcGrpFile = loOGScroll.gfTempName()
    llUseGrp = IIF(LEN(lcGrpSel)>0,.T.,.F.) AND lfConvertToCursor(lcGrpSel,'CSTYGRP',lcGrpFile)
  ENDIF   
ENDIF   

*Color
llUseClr1  = .F.
lnClr1Pos = ASCAN(loOgScroll.laOgVRFlt,"SUBSTR(STYLE.Style,lnClrPo,lnColorLen)")
IF lnClr1Pos > 0 
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnClr1Pos ,6]),loOgScroll.laOgVRFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel ) 
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File )
  ENDIF   
ENDIF   

*Status
llUseStatus = .F.
lcStatusValue = ''
lnStatusPos = ASCAN(loOgScroll.laOgVRFlt,"STYLE.STATUS")
IF lnStatusPos > 0 
  lnStatusPos = ASUBSCRIPT(loOgScroll.laOgVRFlt,lnStatusPos,1)
  lcStatusValue =IIF(!EMPTY(loOgScroll.laOgVRFlt[lnStatusPos,6]),loOgScroll.laOgVRFlt[lnStatusPos,6],'')
  IF !EMPTY(ALLTRIM(lcStatusValue)) 
    llUseStatus = .T.
  ENDIF   
ENDIF   

*******************Collecting Data from style table***********************************************
SELECT STYLE
SET ORDER TO STYLE
IF llSelectStyle && if user selected style major from the option grid
  SELECT(lcCursorStyle)
  LOCATE
  SCAN
    lcStyToScan = SUBSTR(&lcCursorStyle..cStyMajor,1,lnMajLen)
    SELECT STYLE 
    IF SEEK(lcStyToScan)
      lcStyleMaj = SPACE(19)
      SCAN REST WHILE STYLE = lcStyToScan FOR IIF(lcRpDomImp <> 'B',IIF(lcRpDomImp = 'I' ,  !Make , Make ),.T.) AND;
      				IIF(llUseSeason,SEEK(Season,lcSeaFile),.T.) AND ;
      				IIF(llUseDiv,SEEK(CDIVISION,lcDivFile),.T.) AND ;
      				IIF(llUseGrp,SEEK(CSTYGROUP,lcGrpFile),.T.) AND ;
      				IIF(llUseStatus,Status $ lcStatusValue,.T.) AND ;
      				IIF(llUseClr1,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),	lcClr1File),.T.)
	      				
 	WAIT WINDOW LANG_COLLDATA +Style.Style NOWAIT   	
     				
         m.Style = Style.Style
         m.Desc = Style.DESC
         m.cStyMajor= Style.cStyMajor
         m.cClrDesc = ALLTRIM(gfCodDes(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), 'COLOR     '))
         m.cimagpath = ALLTRIM(gfGetStyleImageInfo("S",Style.Style, .T.))
         =SEEK('S'+STYLE.Scale,'Scale','Scale')
         STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totQty
         STORE '' TO m.SZ1,m.SZ2,m.SZ3,m.SZ4,m.SZ5,m.SZ6,m.SZ7,m.SZ8
         FOR lnCntS=1 TO Scale.cnt
           lcCntS= STR(lnCntS,1)
           IF lcRpQtTy  ='U'
             m.Qty&lcCntS. = MAX(STYLE.STK&lcCntS. - STYLE.ALO&lcCntS. ,0)
           ELSE
             m.Qty&lcCntS. = STYLE.STK&lcCntS. + STYLE.WIP&lcCntS. - STYLE.ORD&lcCntS. 
           ENDIF 
           m.SZ&lcCntS. = Scale.SZ&lcCntS.
         ENDFOR
         m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
         INSERT INTO (lcStyles) FROM MEMVAR
      ENDSCAN 
    ENDIF 
  ENDSCAN
ELSE && if user did not select style major from the option grid
  SELECT STYLE 
  lcStyleMaj = SPACE(19)
  SCAN FOR		IIF(llUseClr1,SEEK(SUBSTR(STYLE.Style,lnClrPo,lnColorLen),	lcClr1File),.T.) AND ;
                  IIF(lcRpDomImp <> 'B',IIF(lcRpDomImp = 'I' ,  !Make , Make ),.T.) AND;
  				IIF(llUseSeason,SEEK(Season,lcSeaFile),.T.) AND ;
  				IIF(llUseDiv,SEEK(CDIVISION,lcDivFile),.T.) AND ;
  				IIF(llUseGrp,SEEK(CSTYGROUP,lcGrpFile),.T.) AND ;
  				IIF(llUseStatus,Status $ lcStatusValue,.T.) 
 	WAIT WINDOW LANG_COLLDATA+Style.Style NOWAIT   	
     m.Style = Style.Style
     m.Desc = Style.DESC
     m.cStyMajor= Style.cStyMajor
     m.cClrDesc = ALLTRIM(gfCodDes(SUBSTR(STYLE.Style,lnClrPo,lnColorLen), 'COLOR     '))
     m.cimagpath= ALLTRIM(gfGetStyleImageInfo("S",Style.Style, .T.))
     =SEEK('S'+STYLE.Scale,'Scale','Scale')
     STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.totQty
     STORE '' TO m.SZ1,m.SZ2,m.SZ3,m.SZ4,m.SZ5,m.SZ6,m.SZ7,m.SZ8
     FOR lnCntS=1 TO Scale.cnt
       lcCntS= STR(lnCntS,1)
       IF lcRpQtTy  ='U'
         m.Qty&lcCntS. = MAX(STYLE.STK&lcCntS. - STYLE.ALO&lcCntS. ,0)
       ELSE
         m.Qty&lcCntS. = STYLE.STK&lcCntS. + STYLE.WIP&lcCntS. - STYLE.ORD&lcCntS. 
       ENDIF 
       m.SZ&lcCntS. = Scale.SZ&lcCntS.
     ENDFOR
     m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
     INSERT INTO (lcStyles) FROM MEMVAR
   ENDSCAN 
ENDIF 
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
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
lcColorTt = LANG_ONLYTHIS +' ' + ALLTRIM(lcNonMajT)
*N000682,1 MMT 02/11/2013 Globalization changes[End]
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
= lfOGMover(@laRpSource,@laRpTarget,LANG_STYLEBIN,.T.,'')  && call mover function.

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
*! Date      : 04/29/2013
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
*! Date      : 04/29/2013
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


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mariam Mazhar (MMT)
*: Date      : 04/29/2013
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*!*************************************************************
*! Name      : lfCreateInvTemp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 04/29/2013
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
FUNCTION lfCreateInvTemp
IF USED(lcStyles)
  USE IN (lcStyles)
ENDIF 
DIMENSION laStyStructure[22,4]

laStyStructure[1,1] = 'Style'
laStyStructure[1,2] = 'C'
laStyStructure[1,3] = 19
laStyStructure[1,4] = 0

laStyStructure[2,1] = 'cstymajor'
laStyStructure[2,2] = 'C'
laStyStructure[2,3] = 19
laStyStructure[2,4] = 0

laStyStructure[3,1] = 'cClrDesc'
laStyStructure[3,2] = 'C'
laStyStructure[3,3] = 30
laStyStructure[3,4] = 0

laStyStructure[4,1] = "cimagpath"
laStyStructure[4,2] = 'C'
laStyStructure[4,3] = 254
laStyStructure[4,4] = 0

laStyStructure[5,1] = 'Qty1'
laStyStructure[5,2] = 'N'
laStyStructure[5,3] = 7
laStyStructure[5,4] = 0

laStyStructure[6,1] = 'Qty2'
laStyStructure[6,2] = 'N'
laStyStructure[6,3] = 7
laStyStructure[6,4] = 0

laStyStructure[7,1] = 'Qty3'
laStyStructure[7,2] = 'N'
laStyStructure[7,3] = 7
laStyStructure[7,4] = 0

laStyStructure[8,1] = 'Qty4'
laStyStructure[8,2] = 'N'
laStyStructure[8,3] = 7
laStyStructure[8,4] = 0

laStyStructure[9,1] = 'Qty5'
laStyStructure[9,2] = 'N'
laStyStructure[9,3] = 7
laStyStructure[9,4] = 0

laStyStructure[10,1] = 'Qty6'
laStyStructure[10,2] = 'N'
laStyStructure[10,3] = 7
laStyStructure[10,4] = 0

laStyStructure[11,1] = 'Qty7'
laStyStructure[11,2] = 'N'
laStyStructure[11,3] = 7
laStyStructure[11,4] = 0

laStyStructure[12,1] = 'Qty8'
laStyStructure[12,2] = 'N'
laStyStructure[12,3] = 7
laStyStructure[12,4] = 0

laStyStructure[13,1] = 'TotQty'
laStyStructure[13,2] = 'N'
laStyStructure[13,3] = 8
laStyStructure[13,4] = 0

laStyStructure[14,1] = 'SZ1'
laStyStructure[14,2] = 'C'
laStyStructure[14,3] = 5
laStyStructure[14,4] = 0

laStyStructure[15,1] = 'SZ2'
laStyStructure[15,2] = 'C'
laStyStructure[15,3] = 5
laStyStructure[15,4] = 0

laStyStructure[16,1] = 'SZ3'
laStyStructure[16,2] = 'C'
laStyStructure[16,3] = 5
laStyStructure[16,4] = 0

laStyStructure[17,1] = 'SZ4'
laStyStructure[17,2] = 'C'
laStyStructure[17,3] = 5
laStyStructure[17,4] = 0

laStyStructure[18,1] = 'SZ5'
laStyStructure[18,2] = 'C'
laStyStructure[18,3] = 5
laStyStructure[18,4] = 0

laStyStructure[19,1] = 'SZ6'
laStyStructure[19,2] = 'C'
laStyStructure[19,3] = 5
laStyStructure[19,4] = 0

laStyStructure[20,1] = 'SZ7'
laStyStructure[20,2] = 'C'
laStyStructure[20,3] = 5
laStyStructure[20,4] = 0

laStyStructure[21,1] = 'SZ8'
laStyStructure[21,2] = 'C'
laStyStructure[21,3] = 5
laStyStructure[21,4] = 0

laStyStructure[22,1] = 'Desc'
laStyStructure[22,2] = 'C'
laStyStructure[22,3] = 20
laStyStructure[22,4] = 0

= gfCrtTmp(lcStyles,@laStyStructure,"cstymajor+cClrDesc",lcStyles,.T.)
 
*!*************************************************************
*! Name      : lfAdjCrParameters
*: Developer : Mariam Mazhar (MMT)
*: Date      : 04/29/2013
*! Purpose   : Adjust the Arrays of the Crystal report
*!*************************************************************
FUNCTION lfAdjCrParameters 

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcStyles+'.DBF' 

DIMENSION loOGScroll.laCRParams[4,2]
loOGScroll.laCRParams[1,1] = 'SystemDate'
loOGScroll.laCRParams[1,2] = oAriaApplication.SystemDate

loOGScroll.laCRParams[2,1] = 'OptionalTitle'
loOGScroll.laCRParams[2,2] = lcRPTitle 

loOGScroll.laCRParams[3,1] = 'ReportName'
loOGScroll.laCRParams[3,2] = 'Custom Inventory Report'

loOGScroll.laCRParams[4,1] = 'QtyBy' 
loOGScroll.laCRParams[4,2] = lcRpQtTy
 