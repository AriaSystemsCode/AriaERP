*:***************************************************************************
*: Program file       : SOEXSTY
*: Program description: Export Style Info to CSV files
*: Module             : Sales Order (SO)
*: Developer          : Saber A.Razek (SAB)
*: Tracking Job Number: C201461.EXE
*: Date               : 02/08/2012
*:***************************************************************************
*Modifications:
*:***************************************************************************
*lcRpContNum  ===> Variable to hold selected Contract Number
*lcRpOutNam   ===> Variable to hold selected Output file path
*lcPriceLvl   ===> Variable to hold selected Price Level
PRIVATE lcStyleTmp
lcStyleTmp = gfTempName()

PRIVATE lcRpContNum, lcRpOutNam, lcPriceLvl, lcMajPic, lnClrPo, lnColorLen, lnMajSeg, lnNonMajPo, lnFreeLen
STORE '' TO lcRpContNum, lcRpOutNam, lcPriceLvl, lcMajPic
STORE 0 TO lnClrPo, lnColorLen, lnMajSeg, lnNonMajPo, lnFreeLen

LOCAL lcExp
lcExp = gfOpGrid('SOEXSTY' , .T.,.F.,.F.,.T.,.T.)

IF EMPTY(lcExp) .OR. (lcExp == ".F.")
  RETURN .F.
ENDIF

=lfCreateTemp()
IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE','SH')    && STYLE
ENDIF

=lfNonMaj()

*WAIT WINDOW 'Exporting style info ...' NOWAIT
=lfFillStyTmp(lcExp)

=lfExportCSV()
WAIT WINDOW 'File exported successfully !' TIMEOUT 3


*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/08/2012
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen
*- Check the cost access

lnClrSgPo = ASUBSCRIPT(laOgFxFlt,;
            ASCAN(laOgFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOgFxFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOgFxFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDCASE
ENDFUNC

*!*************************************************************
*! Name      : lfOgValid
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Validate option grid befor processing
*!*************************************************************
FUNCTION lfOgValid

IF EMPTY(lcRpOutNam)
  gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select an output file!')
  RETURN .F.
ENDIF

ENDFUNC

*!*************************************************************
*! Name      : lfvOutPth
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/08/2012
*! Purpose   : Validate the OUT File/Path
*!*************************************************************
FUNCTION lfvOutPth

LOCAL lcExt, lcDirect
lcExt    = UPPER(ALLTRIM(SUBSTR(lcRpOutNam,AT('.',lcRpOutNam))))
lcDirect = SUBSTR(lcRpOutNam,1,AT('\',lcRpOutNam,OCCURS('\',lcRpOutNam)))

IF '?' $ lcRpOutNam OR (!EMPTY(lcRpOutNam) AND ((lcExt <> '.CSV') .OR. !DIRECTORY(lcDirect)))
  lcRpOutNam = GETFILE('CSV', '','Save')
ENDIF

ENDFUNC

*!*************************************************************
*! Name      : lfvContNum
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/08/2012
*! Purpose   : Validate the Contract number
*!*************************************************************
FUNCTION lfvContNum

IF !USED('ORDHDR')
  =gfOpenFile(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')    && CORDTYPE+ORDER
ENDIF
IF '?' $ lcRpContNum OR (!EMPTY(lcRpContNum) AND !SEEK('C'+PADR(lcRpContNum, 6), 'ORDHDR'))
  llObjRet = OrdBrowO(@lcRpContNum , '' , 'C')
ENDIF
ENDFUNC

*!*************************************************************
*! Name      : lfMajPic
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/08/2012
*! Purpose   : get major segment picture
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic
ENDFUNC

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Rise change style flag, in range browse screen.
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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
ENDFUNC

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
=gfItemMask(@laMajSeg)

llStopConc = .F.

*- Loop Around Non Major elements.
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

RETURN ''
ENDFUNC

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp
LOCAL lnItemPos
lnItemPos = 0
DIMENSION laStruArr[7, 4]

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'cStyMajor'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 19
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Style'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 19
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Color'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 19
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Desc'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 20
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Size1'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 10
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Size2'
laStruArr[lnItemPos, 2] = 'C'
laStruArr[lnItemPos, 3] = 10
laStruArr[lnItemPos, 4] = 0

lnItemPos = lnItemPos + 1
laStruArr[lnItemPos, 1] = 'Price'
laStruArr[lnItemPos, 2] = 'N'
laStruArr[lnItemPos, 3] = 6
laStruArr[lnItemPos, 4] = 2

CREATE CURSOR (lcStyleTmp) FROM ARRAY laStruArr

ENDPROC

*!*************************************************************
*! Name      : lfFillStyTmp
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Fill Style temp file with info needed to be exported
*!*************************************************************
FUNCTION lfFillStyTmp
LPARAMETERS lcFltExp

LOCAL llIsExtend, lnPrice
llIsExtend = gfGetMemVar('M_USEEXSSC')
IF !USED('ORDLINE')
  =gfOpenTable('ORDLINE','ORDBLKST','SH')    && CORDTYPE+ORDER+STYLE+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STORE
ENDIF
IF !USED('SCALE')
  =gfOpenTable('SCALE','SCALE','SH')    && TYPE+SCALE+PREPAK
ENDIF

SELECT STYLE
SCAN FOR &lcFltExp.
  *- Get from Contract record if exsists or from Style Price A, B or C according to selected price level
  lnPrice = IIF(SEEK('C'+lcRpContNum+STYLE.Style, 'ORDLINE'), ORDLINE.Price, STYLE.Price&lcPriceLvl.)
  
  LOCAL lcSz
  SELECT SCALE
  IF SEEK('S'+ALLTRIM(STYLE.Scale), 'SCALE')
    *- Add Wait Window
    WAIT WINDOW 'Collecting data for style ' + STYLE.cStyMajor NOWAIT
    FOR lnSz = 1 TO SCALE.Cnt
      lcSz = ALLTRIM(STR(lnSz))
    
      SELECT (lcStyleTmp)
      APPEND BLANK
      REPLACE &lcStyleTmp..cStyMajor WITH STYLE.cStyMajor,;
              &lcStyleTmp..Style     WITH STYLE.Style,;          
              &lcStyleTmp..Color     WITH SUBSTR(STYLE.Style, lnClrPo, lnColorLen),;
              &lcStyleTmp..Desc      WITH STYLE.Desc,;
              &lcStyleTmp..Size1     WITH SCALE.cDim1,;
              &lcStyleTmp..Size2     WITH SCALE.Sz&lcSz.,;
              &lcStyleTmp..Price     WITH lnPrice      
    ENDFOR
  ENDIF
ENDSCAN

ENDFUNC

*!*************************************************************
*! Name      : lfExportCSV
*! Developer : Saber A.Razek (SAB)
*! Date      : 02/08/2012
*! Purpose   : Fill Style temp file with info needed to be exported
*!*************************************************************
FUNCTION lfExportCSV

LOCAL lcLineStr
lcLineStr = 'Style Major,Color,Short Description,Price,Size-1,Size2'+CHR(13)+CHR(10)
STRTOFILE(lcLineStr, lcRpOutNam, .F.)

SELECT (lcStyleTmp)
SCAN 
  *- Add Wait Window
  WAIT WINDOW 'Exporting style ' + &lcStyleTmp..cStyMajor NOWAIT
  lcLineStr = &lcStyleTmp..cStyMajor+','+;
              &lcStyleTmp..Color+','+;
              &lcStyleTmp..Desc+','+;
              STR(&lcStyleTmp..Price, 6, 2)+','+;
              &lcStyleTmp..Size1+','+;
              &lcStyleTmp..Size2+CHR(13)+CHR(10)
  STRTOFILE(lcLineStr, lcRpOutNam, .T.)
ENDSCAN
ENDFUNC
