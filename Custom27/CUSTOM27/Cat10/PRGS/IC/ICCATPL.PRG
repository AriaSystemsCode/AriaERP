*!**********************************************************************************************
*: Program file  : ICCATPLN.PRG
*: Program desc. : Compute the plan quantity size ration based on sales order size ration.
*:         Module: Aria Apparel Series.
*:         Date  : 12/02/2001
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*!**********************************************************************************************
*! Refer to (C102507)
*!**********************************************************************************************
*! Modifications:
*!B605271,1 ADEL  12/23/01   Fix the bug of changing plan quantities.
*!B605876,1 ASH   04/17/2002 TotPlan not equal total plan sizes.
*!**********************************************************************************************
*...

*-- Intialize needed variables
STORE 0   TO lnColorLen,lnClrPo, lnSelRec , lnDelRec , lnUnCmRec , lnUnSelRec , lnShavePer
STORE .F. TO llCallScop
STORE ''  TO lcCanCh1,lcExpr,lcBrowTtl
laDefProc[9]= .F.     && Save procedure(lpSavScr)
lnBrRecNo   = 1                     && Varible to hold the browse record number
lcRpDomImp  = 'B'
DIMENSION laPanelObj(1,3)    && Array to add new buttons to the control panel
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = 'VALID lfvScope()'
lcEscSt = ON("KEY","ESCAPE")
STORE '' TO lcCaCh0,lcCaCh1,lcTmpFil
lcWindTitle = 'Styles'

IF !gfSetup()
  RETURN
ENDIF

= gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
= gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
llNoShow = .F.         && Flag to make the screen call the PROCEDURE lpShow evry time it runs
llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
IF !WEXIST(gcBaseWind)
  lcCaCh0     = gfTempName()
  lcCaCh1     = gfTempName()
  lcTmpFil    = gfTempName()
  lcWindTitle = 'Styles'
  llCallScop  = .T.
  llNthing    = lfCrtTmp()      && Create the Temp File Based on the ORDLINE File
ENDIF
*-- Preparing Option Grid Variables
lcStyTitle  = gfItemMask('HI')
lnMajSeg    = gfItemMask('SM')  && No. of major segments.

*--------- Run the screen
DO (gcScrDir+gcWinAppl+"\ICCATPL.SPX")

IF glQuitting
  IF USED(lcTmpFil)
    USE IN (lcTmpFil)
  ENDIF
  ERASE &gcWorkdir.&lcTmpFil..DBF  
  ERASE &gcWorkdir.&lcTmpFil..CDX  
  ERASE &gcWorkdir.&lcTmpFil..FPT  
ENDIF
RELEASE WINDOW (lcBrowTtl)


*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/02/2001
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/02/2001
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
*! Name      : lfMajPic
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/02/2001
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
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
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

RETURN
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

*!*************************************************************
*! Name      : lfsrSty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/28/1999
*! Purpose   : Set and Rest functions for style filter.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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


*!*************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 06/28/1999
*! Purpose   : sum a specific field for the current style in style file
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


*!*************************************************************
*! Name      : lfvWareHo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/27/1998
*! Purpose   : Validate warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWareHo()
*!*************************************************************

FUNCTION lfvWareHo

lcWareHo = VARREAD()

lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
  &lcWareHo = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF

SET ORDER TO WAREHOUS IN WAREHOUS



*!*************************************************************
*! Name      : lfPvRun
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/26/2000
*! Purpose   : change color code in filter array
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************

FUNCTION lfPvRun
*--lcsty1 var to get exp
*--lcsty2 var to get colors and concatenate it to lcsty1
PRIVATE lcSty1,lcSty2 
STORE 0 TO lcSty1,lcSty2

*-- get color length
DECLARE laItemSeg[1]
STORE 0 TO lncolorLen
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lncolorLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*-- get color from array and change it
*-- get color position
lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)
*-- Get first color 
lcsty1 = SUBSTR(laOgVrFlt[lnClrSgPo,6],1,lnColorLen)

*-- loop for No. of Occurance of Separator "|" in Color exp. and add 1 to last color 
FOR lnCounter = 1 TO OCCUR("|",laOgVrFlt[lnClrSgPo,6])+1

  *--get from second color to rest color 
  IF lnCounter > 1
    *-- get  position of "|"
    lnFirstPos  = ATC('|',laOgVrFlt[lnClrSgPo,6],lnCounter-1)
    *-- we add one to positon to substr after "|"
    lcSty2      = SUBSTR(laOgVrFlt[lnClrSgPo,6],lnFirstPos+1,lnColorLen)
  ENDIF

  IF !EMPTY(lcSty2)
    *--Concatenate expression
    lcSty1 = lcsty1 + '|' + lcSty2
  ELSE

    *--for chose first color only
    lcSty1 = lcsty1
  ENDIF
ENDFOR    

laOgVrFlt[lnClrSgPo,6] = lcSty1

*-- end of lfPvRun


FUNCTION lfOpenFile

= gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
= gfOpenFile(gcDataDir + 'CODES' ,'','SH')
= gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')
= gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH')
SET RELATION TO STYLE INTO STYDYE


*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Function To Create the temp file according to Cancellation Type
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfCrtTmp

SELECT STYLE
= AFIELDS(laFileStru)
*-- Add Some Fields To The Structure
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+17,4]
laFileStru[lnFileStru+1,1] = 'pPerc1'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 2

laFileStru[lnFileStru+2,1] = 'pPerc2'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 4
laFileStru[lnFileStru+2,4] = 2

laFileStru[lnFileStru+3,1] = 'pPerc3'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 4
laFileStru[lnFileStru+3,4] = 2

laFileStru[lnFileStru+4,1] = 'pPerc4'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 4
laFileStru[lnFileStru+4,4] = 2

laFileStru[lnFileStru+5,1] = 'pPerc5'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 4
laFileStru[lnFileStru+5,4] = 2

laFileStru[lnFileStru+6,1] = 'pPerc6'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 4
laFileStru[lnFileStru+6,4] = 2

laFileStru[lnFileStru+7,1] = 'pPerc7'
laFileStru[lnFileStru+7,2] = 'N'
laFileStru[lnFileStru+7,3] = 4
laFileStru[lnFileStru+7,4] = 2

laFileStru[lnFileStru+8,1] = 'pPerc8'
laFileStru[lnFileStru+8,2] = 'N'
laFileStru[lnFileStru+8,3] = 4
laFileStru[lnFileStru+8,4] = 2

laFileStru[lnFileStru+9,1] = 'oPerc1'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 4
laFileStru[lnFileStru+9,4] = 2

laFileStru[lnFileStru+10,1] = 'oPerc2'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 4
laFileStru[lnFileStru+10,4] = 2

laFileStru[lnFileStru+11,1] = 'oPerc3'
laFileStru[lnFileStru+11,2] = 'N'
laFileStru[lnFileStru+11,3] = 4
laFileStru[lnFileStru+11,4] = 2

laFileStru[lnFileStru+12,1] = 'oPerc4'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 4
laFileStru[lnFileStru+12,4] = 2

laFileStru[lnFileStru+13,1] = 'oPerc5'
laFileStru[lnFileStru+13,2] = 'N'
laFileStru[lnFileStru+13,3] = 4
laFileStru[lnFileStru+13,4] = 2

laFileStru[lnFileStru+14,1] = 'oPerc6'
laFileStru[lnFileStru+14,2] = 'N'
laFileStru[lnFileStru+14,3] = 4
laFileStru[lnFileStru+14,4] = 2

laFileStru[lnFileStru+15,1] = 'oPerc7'
laFileStru[lnFileStru+15,2] = 'N'
laFileStru[lnFileStru+15,3] = 4
laFileStru[lnFileStru+15,4] = 2

laFileStru[lnFileStru+16,1] = 'oPerc8'
laFileStru[lnFileStru+16,2] = 'N'
laFileStru[lnFileStru+16,3] = 4
laFileStru[lnFileStru+16,4] = 2

laFileStru[lnFileStru+17,4] = 0
laFileStru[lnFileStru+17,1] = 'LLSEL'
laFileStru[lnFileStru+17,2] = 'L'
laFileStru[lnFileStru+17,3] = 1
laFileStru[lnFileStru+17,4] = 0
CREATE TABLE &gcWorkDir.&lcTmpFil FROM ARRAY laFileStru


*!*************************************************************
*! Name      : lfNMajType
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 04/08/1998
*! Purpose   : Function For calculating major and non-major segments
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lcFree_Clr IF Not Empty
*!*************************************************************

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
*-- end of lfNMajType. 

*!*****************************************************************
*! Function: lfReadAct()
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Read activate function
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB


*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 26/07/1998
*! Purpose   : Browses the TEMP File Records
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   : =lfDispBrow()
*!*************************************************************

FUNCTION lfDispBrow

IF EOF()
  lnBrRecNo = RECCOUNT()
ELSE
  lnBrRecNo = RECNO()
ENDIF  
lcBrowTtl = 'Styles'
lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
            "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
            "STYLE   :R :H= lcStyTitle   :19 ,"+;
            "Desc    :R :H='Description' :20," + ;
            "TotOrd  :R :H='Tot. Open Orders'," + ;
            "TotPlan :R :H='Tot. Plan',"  + ;
            "oPerc1 :R :H='%Ord1' :5 :P = '.99',"  + ;
            "oPerc2 :R :H='%Ord2' :5 :P = '.99',"  + ;
            "oPerc3 :R :H='%Ord3' :5 :P = '.99',"  + ;
            "oPerc4 :R :H='%Ord4' :5 :P = '.99',"  + ;
            "oPerc5 :R :H='%Ord5' :5 :P = '.99',"  + ;
            "oPerc6 :R :H='%Ord6' :5 :P = '.99',"  + ;
            "oPerc7 :R :H='%Ord7' :5 :P = '.99',"  + ;
            "oPerc8 :R :H='%Ord8' :5 :P = '.99',"   + ;
            "pPerc1 :R :H='%PLan1' :6 :P = '.99',"  + ;
            "pPerc2 :R :H='%PLan2' :6 :P = '.99',"  + ;
            "pPerc3 :R :H='%PLan3' :6 :P = '.99',"  + ;
            "pPerc4 :R :H='%PLan4' :6 :P = '.99',"  + ;
            "pPerc5 :R :H='%PLan5' :6 :P = '.99',"  + ;
            "pPerc6 :R :H='%PLan6' :6 :P = '.99',"  + ;
            "pPerc7 :R :H='%PLan7' :6 :P = '.99',"  + ;
            "pPerc8 :R :H='%PLan8' :6 :P = '.99'" 
SELECT (lcTmpFil)
GOTO  TOP
BROWSE FIELDS &lcBrowStr               ;
       WINDOW (lcCaCh0)             ;
       WHEN lfwBrows() .AND. lfvpbSel();
       VALID :F lfvBrows()             ;  
       IN WINDOW (gcBaseWind)          ;
       LOCK 0                          ;
       NOAPPEND                        ;
       NOCLEAR                         ;
       NODELETE                        ;
       NOWAIT                          ;
       NOEDIT                          ;
       NOMENU                          ;
       SAVE                            ;
       TITLE lcBrowTtl
ON KEY LABEL CTRL+ENTER DO lpvBrow
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)


*!**************************************************************************
*! Name      : lfwBrows
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : When Function The Browse
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfwBrows

lnBrRecNo  = RECNO(lcTmpFil)
SHOW WINDOW (lcBrowTtl) REFRESH
   
llEOF = .F.
llBOF = .F.

IF !EOF() .AND. !BOF()
  SKIP 1
  IF EOF()
    llEOF = .T.
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ELSE
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ENDIF
ELSE
  llEOF = .T.
  llBOF = .T.
ENDIF
SHOW MENU _mSysMenu
ON KEY LABE ESCAPE &lcEscSt

*!**************************************************************************
*! Name      : lfvpbSel
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Function for Switching 'Select And Unselect'
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvpbSel

*--IF The record is selected
IF LLSEL
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ENDIF    && End of IF

RETURN .T.


*!*************************************************************
*! Name      : lpShow
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Handle The special case for screen modes
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpShow
*!*************************************************************

PROCEDURE lpShow

SHOW GET pbEdt     DISABLE
SHOW GET pbBrws    DISABLE
SHOW GET pbcpPrint DISABLE
SHOW GET pbSlct    DISABLE
SHOW GET pbDlt     DISABLE
SHOW GET pbBtm     DISABLE
SHOW GET pbNxt     DISABLE
SHOW GET pbPrvs    DISABLE
SHOW GET pbTop     DISABLE
laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "DISABLE"                && Browse button
SHOW MENU _mSysMenu
*--IF Statment to check if we are going to call the option grid for the user
*--IF these is the first time for these session of the program
IF llCallScop
  SHOW GET pbSav DISABLE
  llNthing   = lfvScope()
  llCallScop = .F.
ENDIF  && End of IF
llFrstTime = .F.

*--Disable\enabel Buttons according to temp File State
IF USED(lcTmpFil)
  IF EOF(lcTmpFil)
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
  ELSE
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    IF lnSelRec <> 0
      SHOW GET pbSelNon ENABLE
    ENDIF  
    SHOW GET pbInvert ENABLE
  ENDIF
ENDIF
lnBrRecNo  = RECNO(lcTmpFil)
IF TYPE('lcExpr') <> 'L'
  SHOW WINDOW (lcBrowTtl) REFRESH
ENDIF

*!*****************************************************************
*! Function: lfClrTrap
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
FUNCTION lfClrTrap
IF WONTOP() = (lcBrowTtl)
  ON KEY LABEL TAB     DO lpFld4Tab WITH (lcCaCh1),pbSelect
  ON KEY LABEL BACKTAB DO lpBackTab
ENDIF


*!*************************************************************
*! Name      : lfvScope
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/13/1997
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrT() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvScope

ON KEY LABEL CTRL+ENTER
SELECT STYLE
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
lcOldChld = lcCaCh0
lcExpr = gfOpGrid('ICCATPL' , .T.)  && Run selection grid.
IF llCallScop
  lcOldExpr = lcExpr      && Fill the old Expr With Expr.
ENDIF
*--IF The user Selected to cancel [Close] From the Option Grid
IF TYPE('lcExpr') = 'L'
  IF TYPE('lcOldExpr') = 'U'
    lcOldExpr = " "
  ENDIF
  lcExpr = lcOldExpr             && Restore the old Expr.
ELSE    && Else
  llNthing = lfRemTmp()        && Dlete the old Temp File
  llNthing = lfCrtTmp()        && Re-Create the Temp File
  llNthing = lfFillField()   && AnyWay Start filling the Tmp File
  SELECT (lcTmpFil)
  REPLACE ALL LLSEL WITH .F.
  IF RECNO() = 0
    *--Disable Buttons
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbSelNon DISABLE
  ELSE
    *--Enable Buttons
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbSelNon DISABLE
  ENDIF
  lnBrRecNo = RECNO()
  llNthing   = lfDispBrow()    && Browse What U Got
  llCallScop = .F.             && Screen Already Initialized
  *--IF The temp file [lcTmpFil] is not empty
  IF !EOF()
    laScrMode[2] = .T.
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbSelNon DISABLE
  ELSE    && Else
    laScrMode[1] = .T.
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbSelNon DISABLE
  ENDIF    && End of IF
  SHOW GETS ONLY
  SHOW GET pbTop DISABLE
  SHOW GET pbPrvs DISABLE
  laCtrStat[1]  = "DISABLE"               && First button 
  laCtrStat[4]  = "DISABLE"               && Priveus button
  SHOW MENU _mSysMenu
ENDIF    && End of IF
=lfOpenFile()

*!**************************************************************************
*! Name      : lfRemTmp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Function To Delete the tepm file
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfRemTmp

IF USED(lcTmpFil)  && We are going to close the Temp Pick ticket & Erase it.
  SELECT(lcTmpFil)
  USE IN &lcTmpFil
ENDIF   && End of IF

ERASE &gcWorkdir.&lcTmpFil..DBF  && Erase the Temp file.
ERASE &gcWorkdir.&lcTmpFil..CDX  && Erase the Temp index.

*!**************************************************************************
*! Name      : lfFillField
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Function To Fill The Temp Table according to user selection
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfFillField

SELECT STYLE
SCAN FOR &lcExpr
  SCATTER MEMVAR MEMO
  WAIT WINDOW 'Collecting data...' NOWAIT
  INSERT INTO (lcTmpFil) FROM MEMVAR
  REPLACE &lcTmpFil..oPerc1 WITH IIF(TotOrd  <> 0 ,Ord1/TotOrd,0),;
          &lcTmpFil..oPerc2 WITH IIF(TotOrd  <> 0 ,Ord2/TotOrd,0),;
          &lcTmpFil..oPerc3 WITH IIF(TotOrd  <> 0 ,Ord3/TotOrd,0),;
          &lcTmpFil..oPerc4 WITH IIF(TotOrd  <> 0 ,Ord4/TotOrd,0),;
          &lcTmpFil..oPerc5 WITH IIF(TotOrd  <> 0 ,Ord5/TotOrd,0),;
          &lcTmpFil..oPerc6 WITH IIF(TotOrd  <> 0 ,Ord6/TotOrd,0),;
          &lcTmpFil..oPerc7 WITH IIF(TotOrd  <> 0 ,Ord7/TotOrd,0),;
          &lcTmpFil..oPerc8 WITH IIF(TotOrd  <> 0 ,Ord8/TotOrd,0),;
          &lcTmpFil..pPerc1 WITH IIF(TotPlan <> 0 ,Plan1/TotPlan,0),;
          &lcTmpFil..pPerc2 WITH IIF(TotPlan <> 0 ,Plan2/TotPlan,0),;
          &lcTmpFil..pPerc3 WITH IIF(TotPlan <> 0 ,Plan3/TotPlan,0),;
          &lcTmpFil..pPerc4 WITH IIF(TotPlan <> 0 ,Plan4/TotPlan,0),;
          &lcTmpFil..pPerc5 WITH IIF(TotPlan <> 0 ,Plan5/TotPlan,0),;
          &lcTmpFil..pPerc6 WITH IIF(TotPlan <> 0 ,Plan6/TotPlan,0),;
          &lcTmpFil..pPerc7 WITH IIF(TotPlan <> 0 ,Plan7/TotPlan,0),;
          &lcTmpFil..pPerc8 WITH IIF(TotPlan <> 0 ,Plan8/TotPlan,0)
  IF (&lcTmpFil..pPerc1 + &lcTmpFil..pPerc2+&lcTmpFil..pPerc3+&lcTmpFil..pPerc4 +;
      &lcTmpFil..pPerc5+&lcTmpFil..pPerc6+&lcTmpFil..pPerc7+&lcTmpFil..pPerc8) <100
    REPLACE &lcTmpFil..pPerc1 WITH &lcTmpFil..oPerc1,;
            &lcTmpFil..pPerc2 WITH &lcTmpFil..oPerc2,;
            &lcTmpFil..pPerc3 WITH &lcTmpFil..oPerc3,;
            &lcTmpFil..pPerc4 WITH &lcTmpFil..oPerc4,;
            &lcTmpFil..pPerc5 WITH &lcTmpFil..oPerc5,;
            &lcTmpFil..pPerc6 WITH &lcTmpFil..oPerc6,;
            &lcTmpFil..pPerc7 WITH &lcTmpFil..oPerc7,;
            &lcTmpFil..pPerc8 WITH &lcTmpFil..oPerc8
  ENDIF    
ENDSCAN
SELECT (lcTmpFil)
IF EOF()
  WAIT WINDOW 'No Records Were Selected'
ENDIF


*!*****************************************************************
*! Function: lfvBrows
*!*****************************************************************
*! Author : Hesham
*!*****************************************************************
*! Purpose : Stop browse case activating another screen
*!*****************************************************************

FUNCTION lfvBrows
IF WONTOP(lcBrowTtl)
  glFromBrow = .T.
ELSE
  = gfStopBrow()
  glFromBrow = .F.  
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
    CLEAR READ
    RETURN TO SMFRMCD.SPR
  ENDIF  
ENDIF  

*!**************************************************************************
*! Name      : lfvSelect
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelect
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvSelect

SELECT(lcTmpFil)
REPLACE LLSEL WITH !LLSEL
=lfvpbSel()
lnSelRec   = IIF(llSel , lnSelRec + 1 , lnSelRec - 1)
lnUnSelRec = IIF(llSel , lnUnSelRec - 1 , lnUnSelRec + 1)
SHOW WINDOW (lcBrowTtl) REFRESH

*--IF No records was selected
IF lnSelRec = 0
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSav DISABLE
  SHOW GET pbSelAll ENABLE
ELSE    && Else
  SHOW GET pbSelNon ENABLE
  SHOW GET pbSav ENABLE
  
  *--IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ELSE    && Else
    SHOW GET pbSelAll ENABLE
  ENDIF    && End of IF
ENDIF    && End of IF
*-- End

*!**************************************************************************
*! Name      : lfvSelAll
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelall
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvSelAll

SELECT(lcTmpFil)
IF !EOF()
  REPLACE ALL LLSEL WITH .T.
  lnSelRec   = RECCOUNT() - lnDelRec
  lnUnSelRec = 0
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbSav ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbSelAll DISABLE
ENDIF

*!**************************************************************************
*! Name      : lfvSelNon
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbSelnon
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
FUNCTION lfvSelNon

SELECT(lcTmpFil)
IF !EOF()
  REPLACE ALL LLSEL WITH .F.
  lnSelRec   = 0
  lnUnSelRec = RECCOUNT() - lnDelRec
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbSav DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ENDIF
*-- End

*!**************************************************************************
*! Name      : lfvInvert
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 1/09/1998
*! Purpose   : Valid Function for pbInv
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************

FUNCTION lfvInvert

SELECT(lcTmpFil)
REPLACE ALL LLSEL WITH !LLSEL
GO lnBrRecNo
=lfvpbSel()
lnUnSelRec = lnSelRec
lnSelRec   = RECCOUNT() - lnDelRec - lnSelRec
SHOW WINDOW (lcBrowTtl) REFRESH

*--IF there is no selected records
IF lnSelRec = 0
  SHOW GET pbSav DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ELSE    && Else
  SHOW GET pbSav ENABLE
  SHOW GET pbSelNon ENABLE
  *--IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ENDIF    && End of IF
ENDIF    && End of IF
*--End

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/01/1996
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

SELECT (lcTmpFil)
lnRecNo =RECNO()
SCAN FOR llSel
  IF SEEK(STYLE,'STYLE') AND TOTORD <> 0
    SELECT STYLE
    FOR lnPlnNo = 1 TO 8
      lcPlanNo = STR(lnPlnNo,1)
      IF &lcTmpFil..Ord&lcPlanNo <> 0
        *B605271,1 (Begin) Fix the bug of changing plan quantities.
        *REPLACE Plan&lcPlanNo WITH &lcTmpFil..oPerc&lcPlanNo * TOTPLAN ,;
                &lcTmpFil..Plan&lcPlanNo WITH &lcTmpFil..oPerc&lcPlanNo * TOTPLAN,;
                &lcTmpFil..pPerc&lcPlanNo WITH IIF(&lcTmpFil..TotPlan <> 0 ,&lcTmpFil..Plan&lcPlanNo/&lcTmpFil..TotPlan,0)
        *REPLACE TOTPLAN WITH PLAN1+PLAN2+PLAN3+PLAN4+PLAN5+PLAN6+PLAN7+PLAN8,;
               &lcTmpFil..TotPlan  WITH TOTPLAN

        REPLACE Plan&lcPlanNo WITH (&lcTmpFil..Ord&lcPlanNo/&lcTmpFil..TotOrd) * TOTPLAN ,;
                &lcTmpFil..Plan&lcPlanNo WITH (&lcTmpFil..Ord&lcPlanNo/&lcTmpFil..TotOrd)  * TOTPLAN,;
                &lcTmpFil..pPerc&lcPlanNo WITH IIF(&lcTmpFil..TotPlan <> 0 ,&lcTmpFil..Plan&lcPlanNo/&lcTmpFil..TotPlan,0)
       *B605271,1 (End)
      ENDIF          
    ENDFOR 
    *B605876,1 ASH 04/17/2002 (Begin) Fix the bug of Totplan is not equal the plan per quantities
    *B605876,1 ASH                  
    lnDiff = Plan1+Plan2+Plan3+Plan4+Plan5+Plan6+Plan7+Plan8 - TotPlan
    IF lnDiff <> 0
      DIME laPlan[8,3]    && Array to hold the plan has 3 columns, 1=INT of the result of dividing, 2= the decimals of this result, 3 the size number
      laPlan = 0
      IF lnDiff > 0
        lcSign = '-'
        lcSign1 = '>'
      ELSE
        lcSign = '+'
        lcSign1 = '<'
      ENDIF
      
      FOR I = 1 TO 8
        lcPlanNo = STR (I,1)
        lnPlanQty = (&lcTmpFil..Ord&lcPlanNo/&lcTmpFil..TotOrd) * TOTPLAN      
        IF lnPlanQty - INT(lnPlanQty) &lcSign1 0.5
          laPlan[I,1] = INT((&lcTmpFil..Ord&lcPlanNo/&lcTmpFil..TotOrd) * TOTPLAN )
          laPlan[I,2] = (&lcTmpFil..Ord&lcPlanNo/&lcTmpFil..TotOrd) * TOTPLAN - laPlan[I,1]
          laPlan[I,3] = I
        ENDIF
      ENDFOR
      =ASORT(laPlan,2,8,0)
      FOR K = 1 TO ABS(lnDiff)
        FOR I = 1 to 8
          IF laPlan[I,2] = 0
            LOOP
          ENDIF
          lcPlanNo = STR(laPlan[I,3],1)
          REPLACE Plan&lcPlanNo WITH Plan&lcPlanNo &lcSign 1
          laPlan[I,2] = 0
          EXIT
        ENDFOR
      ENDFOR
    ENDIF
    *B605876,1 ASH 04/17/2002 (End)
    *B605271,1 (Begin) Fix the bug of changing plan quantities.                    
    REPLACE &lcTmpFil..TotPlan  WITH TOTPLAN
    *B605271,1 (End)            
  
    IF (&lcTmpFil..pPerc1 + &lcTmpFil..pPerc2+&lcTmpFil..pPerc3+&lcTmpFil..pPerc4 +;
        &lcTmpFil..pPerc5+&lcTmpFil..pPerc6+&lcTmpFil..pPerc7+&lcTmpFil..pPerc8) <100
      REPLACE &lcTmpFil..pPerc1 WITH &lcTmpFil..oPerc1,;
              &lcTmpFil..pPerc2 WITH &lcTmpFil..oPerc2,;
              &lcTmpFil..pPerc3 WITH &lcTmpFil..oPerc3,;
              &lcTmpFil..pPerc4 WITH &lcTmpFil..oPerc4,;
              &lcTmpFil..pPerc5 WITH &lcTmpFil..oPerc5,;
              &lcTmpFil..pPerc6 WITH &lcTmpFil..oPerc6,;
              &lcTmpFil..pPerc7 WITH &lcTmpFil..oPerc7,;
              &lcTmpFil..pPerc8 WITH &lcTmpFil..oPerc8
    ENDIF
    *--Check prepak
    IF SEEK('P','SCALE')
      SELECT SCALE
      llPrPkFnd = .F.
      SCAN  REST WHILE type+scale+prepak = 'P' FOR SCALE = STYLE.SCALE
        IF (&lcTmpFil..Ord1/&lcTmpFil..TotOrd = pp1/pptot) AND (&lcTmpFil..Ord2/&lcTmpFil..TotOrd = pp2/pptot) AND (&lcTmpFil..Ord3/&lcTmpFil..TotOrd = pp3/pptot) ;
           (&lcTmpFil..Ord4/&lcTmpFil..TotOrd = pp4/pptot) AND (&lcTmpFil..Ord5/&lcTmpFil..TotOrd = pp5/pptot) AND (&lcTmpFil..Ord6/&lcTmpFil..TotOrd = pp6/pptot) ;
           (&lcTmpFil..Ord7/&lcTmpFil..TotOrd = pp7/pptot) AND (&lcTmpFil..Ord7/&lcTmpFil..TotOrd = pp8/pptot)
           llPrPkFnd = .T.
           EXIT
        ENDIF  
      ENDSCAN
      REPLACE STYLE.PREPAK WITH IIF(llPrPkFnd,SCALE.PREPAK,'')
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcTmpFil)
REPLACE ALL llSel WITH .F.
SHOW WINDOW (lcBrowTtl) REFRESH
SHOW GET pbSelect,1 PROMPT 'Se\<lect'
SHOW GET pbSelNon DISABLE
SHOW GET pbSelAll ENABLE
lnSelRec = 0
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,1)

*!*****************************************************************
*! Function: lpFld4Tab
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
*!B603785,1 
*!*****************************************************************
PROCEDURE lpFld4Tab
PARAMETERS lcWindToAct,lcObject

ACTIVATE WINDOW (lcWindToAct)
_CUROBJ=OBJNUM(lcObject)


*!*****************************************************************
*! Function: lpFld4Tab
*!*****************************************************************
*! Author  : Adel Mohammed El Gazzar (ADEL)
*!*****************************************************************
*! Purpose : Trap Browse window
*!*****************************************************************
*! Date    : 08/08/2000 
*!*****************************************************************
PROCEDURE lpBackTab

IF WONTOP() = "Styles"
  lcWind = "gwccontrl1"
  lcObj   = 'pbCls'
  ACTIVATE WINDOW (lcWind)
  _CUROBJ=OBJNUM(lcObj)
ENDIF  
