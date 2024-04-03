*:***************************************************************************
*: Program file  : ICPHINV.PRG
*: Program desc. : Physical Inventory WorkSheet Report
*: For Report    : (ICPHINV.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar (MMT)
*: Issue      : (N037565)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfItemMask.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfSRVSty,lfStySum,lfvFabric,FaBrow,
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICPHINV
*:***************************************************************************
*: This Report Program is due to E301080 ...
*:B607906,1 25/12/2006 Fix Report Orientation (T20061124.0003)
*:***************************************************************************
*: Modifications:
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:***************************************************************************

lcStTime   = TIME()
llRpByBins = llMultBin AND llRpByBins
*:B607906,1 25/12/2006 Fix Report Orientation
*loOgScroll.cCRorientation = 'P'
loOgScroll.cCRorientation = 'L'
*:B607906,1 25/12/2006 Fix Report Orientation
IF loOgScroll.llOGFltCh && OG Filters changed
  lfCrtTemp()
  lfCollect()
ENDIF 

*-- Assign Report Variables [begin]
*-- if style have color segment.
IF lcFree_Clr = 'C'
  lcHeader = [lcStyTitle + ' ' + lcNonMajTl + ' Description             ' + lcStyMajor + ' Desc.']
  lcDetail = [STYLE + ' ' + gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen) , 'COLOR') + ' ' + DESC ]

ELSE  && else style does not have color segment

  lcHeader = [lcStyTitle  + ' ' + 'Description']
  lcDetail = [STYLE + ' ' + PADR(DESC1,52)]
ENDIF
*-- Assign Report Variables [end]

SELECT(lcTmpFile)
DO gfDispRe WITH EVAL('lcRpForm') 
*, 'FOR ' + lcRpExp
lcLastExpr = lcRpExp
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF !llFrstTime
  loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  loDBFScale    = CreateObject("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
  loDBFWareHous = CreateObject("RemoteTable","WAREHOUS","WAREHOUS",'WAREHOUS',SET("DATASESSION"))&&,"",.T.)
  loDBFCodes   = CreateObject("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
  loDBFWHSLOC  =  CreateObject("RemoteTable","WHSLOC","WHSLOCST",'WHSLOC',SET("DATASESSION"))&&,"",.T.)
    
  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF 

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)
  *:B603856,1 MHM 08/29/2000[Begin]
  IF ('MA' $ oAriaApplication.CompanyInstalledModules)
*    SET ORDER TO FABRIC IN FABRIC
  ENDIF 
  *:B603856,1 MHM 08/29/2000 [End]
 * SET ORDER TO STYLE IN STYLE
ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

  *-- Loop Around Non Major elements.
  FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
    IF laMajSegs[lnI,1] $ 'CF'
      lcFree_Clr = laMajSegs[lnI,1]

      lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
       
      lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                   laMajSegs[lnI,3],;
                   lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

      lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                   PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                   lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    ENDIF                     

    *-- If you Find Color Type or Find Free Type and current type not Free.
    IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
      EXIT
    ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

  ENDFOR    && end Loop Around Non Major elements.

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcStyBinTl = 'Print ' + lcStyMajor + '(s) by Bins' 
  lcStyStat  = lcStyMajor + ' Status'
  lcStyMake  = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
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
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
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
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*--End of lfvFabric.

*C125875,1 BWA 12/25/2004 Add new FRX to print page for every Bin.[START]
*!*************************************************************
*! Name      : lfChngForm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/25/2004
*! Purpose   : Change printed form.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm

IF llRpByBins
  lcRPForm = "ICPHINVB"
ELSE
  lcRPForm = "ICPHINV"
ENDIF

=lfRepPltFr(lcRPForm)

*--End of lfChngForm.
*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/11/2006
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : MAriam Mazhar[MMT]
*! Date      : 06/11/2006
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
LPARAMETERS lcFab, lcComp
LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0

  IF SEEK(ALLTRIM(lcFab), lcTmpFab)
    SUM &lcTmpFab..&lcCOMP. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
  ENDIF

RETURN lnTotcomp

FUNCTION lfCrtTemp
DIMENSION laTempStru[1,18]
laTempStru = ''
IF llRpByBins
  SELECT WHSLOC
ELSE
  SELECT STYLE
ENDIF 
lnAllFlds = AFIELDS(laTempStru)
DIMENSION laTempStru[lnAllFlds + IIF(llRpByBins,11,9), 18]

laTempStru[lnAllFlds+1,1] = 'sz1'
laTempStru[lnAllFlds+1,2] = 'C'
laTempStru[lnAllFlds+1,3] = 6
laTempStru[lnAllFlds+1,4] = 0

laTempStru[lnAllFlds+2,1] = 'sz2'
laTempStru[lnAllFlds+2,2] = 'C'
laTempStru[lnAllFlds+2,3] = 6
laTempStru[lnAllFlds+2,4] = 0

laTempStru[lnAllFlds+3,1] = 'sz3'
laTempStru[lnAllFlds+3,2] = 'C'
laTempStru[lnAllFlds+3,3] = 6
laTempStru[lnAllFlds+3,4] = 0

laTempStru[lnAllFlds+4,1] = 'sz4'
laTempStru[lnAllFlds+4,2] = 'C'
laTempStru[lnAllFlds+4,3] = 6
laTempStru[lnAllFlds+4,4] = 0

laTempStru[lnAllFlds+5,1] = 'sz5'
laTempStru[lnAllFlds+5,2] = 'C'
laTempStru[lnAllFlds+5,3] = 6
laTempStru[lnAllFlds+5,4] = 0

laTempStru[lnAllFlds+6,1] = 'sz6'
laTempStru[lnAllFlds+6,2] = 'C'
laTempStru[lnAllFlds+6,3] = 6
laTempStru[lnAllFlds+6,4] = 0

laTempStru[lnAllFlds+7,1] = 'sz7'
laTempStru[lnAllFlds+7,2] = 'C'
laTempStru[lnAllFlds+7,3] = 6
laTempStru[lnAllFlds+7,4] = 0

laTempStru[lnAllFlds+8,1] = 'sz8'
laTempStru[lnAllFlds+8,2] = 'C'
laTempStru[lnAllFlds+8,3] = 6
laTempStru[lnAllFlds+8,4] = 0

laTempStru[lnAllFlds+9,1] = 'cDesc'
laTempStru[lnAllFlds+9,2] = 'C'
laTempStru[lnAllFlds+9,3] = 35
laTempStru[lnAllFlds+9,4] = 0


IF llRpByBins
  laTempStru[lnAllFlds+10,1] = 'Desc'
  laTempStru[lnAllFlds+10,2] = 'C'
  laTempStru[lnAllFlds+10,3] = 20
  laTempStru[lnAllFlds+10,4] = 0

  laTempStru[lnAllFlds+11,1] = 'DESC1'
  laTempStru[lnAllFlds+11,2] = 'C'
  laTempStru[lnAllFlds+11,3] = 60
  laTempStru[lnAllFlds+11,4] = 0

ENDIF 

  FOR lnLoop = 1 TO  IIF(llRpByBins,11,9)
  STORE ' ' TO  laTempStru[lnAllFlds+lnLoop,7],laTempStru[lnAllFlds+lnLoop,8],;
                laTempStru[lnAllFlds+lnLoop,9],laTempStru[lnAllFlds+lnLoop,10],;
                laTempStru[lnAllFlds+lnLoop,11],laTempStru[lnAllFlds+lnLoop,12],;
                laTempStru[lnAllFlds+lnLoop,13],laTempStru[lnAllFlds+lnLoop,14],;
                laTempStru[lnAllFlds+lnLoop,15],laTempStru[lnAllFlds+lnLoop,16]
  STORE 0 TO    laTempStru[lnAllFlds+lnLoop,17] ,laTempStru[lnAllFlds+lnLoop,18]
ENDFOR   

 = gfCrtTmp(lcTmpFile,@laTempStru,IIF(llRpByBins,"cwarecode+clocation+style+color" ,"Style" ),lcTmpFile,.T.)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
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

CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
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
*! Name      : lfGetDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfGetDesc
PARAMETERS lcMFCode
lcMfName = ""
loDBFCodes.Setorder('Codes')
IF loDBFCodes.SEEK('N'+lcMFCode+'N'+'COLOR     ')
  lcMfName = codes.cdiscrep
ENDIF
RETURN lcMfName
  
FUNCTION lfCollect

llUseStyle = .F.
llUseDiv   = .F.
llUseSea   = .F.
llUseGrp   = .F.
llUseFab   = .F.
llUseClr   = .F.

*--Style major
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 

*---Division Filter
lnDivPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.CDIVISION'),1)
IF lnDivPos  > 0
  lcDivStr = LOOGSCROLL.laOGVRFlt[lnDivPos,6]
  lcDivFile = loOGScroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGVRFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*--Style Group
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGVRFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF

*--Style Fabric
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  llUseFab = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF
IF llUseFab 
  SELECT(lcFabFile)
  LOCATE 
  IF EOF()
    llUseFab = .F.
  ENDIF 
ENDIF 


*--Style Color
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGFXFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

*--Pattern
lcPattStr = ""
lnPattPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.PATTERN'),1)
IF lnPattPos > 0
  lcPattStr = LOOGSCROLL.laOGVRFlt[lnPattPos,6]
ENDIF

lcStatStr  = ""
IF lcRpStatus <>'B'
  lcStatStr  = IIF(lcRpStatus='B','',"Style.Status='" + lcRpStatus+"'")
ENDIF
lcFileName = IIF(llRpByBins,"WHSLOC","STYLE")

IF llUseStyle
  SELECT(lcStylFile)
  SCAN 
    IF !llRpByBins
      =loDBFStyle.Seek(&lcStylFile..CSTYMAJOR,'CSTYLE')
      SELECT STYLE
      SCAN REST WHILE CSTYMAJOR = &lcStylFile..CSTYMAJOR FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.)
           SCATTER MEMO MEMVAR
           IF loDBFScale.SEEK('S'+ m.SCALE) 
             m.SZ1  = scale.SZ1
             m.SZ2  = scale.SZ2
             m.SZ3  = scale.SZ3 
             m.SZ4  = scale.SZ4
             m.SZ5  = scale.SZ5
             m.SZ6  = scale.SZ6
             m.SZ7  = scale.SZ7
             m.SZ8  = scale.SZ8
           ENDIF   
      * cWareCode          
           INSERT INTO (lcTmpFile) FROM MEMVAR 
      ENDSCAN               
     ELSE
       SELECT WHSLOC
       loDBFWHSLOC.Setorder("WHSLOCST")
       =loDBFWHSLOC.Seek(left(&lcStylFile..CSTYMAJOR,lnMajorLen),"WHSLOCST")
       SCAN REST WHILE STYLE+COLOR+CWARECODE+CLOCATION = left(&lcStylFile..CSTYMAJOR,lnMajorLen) FOR loDBFStyle.Seek(WHSLOC.STYLE,'STYLE') ;
                     AND IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.)
             SCATTER MEMO MEMVAR
             m.Desc1 = Style.Desc1
             m.Desc = Style.Desc
             IF loDBFScale.SEEK('S'+ Style.SCALE) 
               m.SZ1  = scale.SZ1
               m.SZ2  = scale.SZ2
               m.SZ3  = scale.SZ3 
               m.SZ4  = scale.SZ4
               m.SZ5  = scale.SZ5
               m.SZ6  = scale.SZ6
               m.SZ7  = scale.SZ7
               m.SZ8  = scale.SZ8
             ENDIF   
             IF loDBFWAREHOUS.Seek(CWARECODE)
               m.cdesc = WAREHOUS.cdesc
             ENDIF   
             INSERT INTO (lcTmpFile) FROM MEMVAR 
     ENDSCAN    
   ENDIF  
  ENDSCAN  
ELSE
  IF !llRpByBins
    SELECT STYLE
    SCAN FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
             AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
             AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
             AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
             AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
             AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
             AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.)
      SCATTER MEMO MEMVAR
      IF loDBFScale.SEEK('S'+ Style.SCALE) 
        m.SZ1  = scale.SZ1
        m.SZ2  = scale.SZ2
        m.SZ3  = scale.SZ3 
        m.SZ4  = scale.SZ4
        m.SZ5  = scale.SZ5
        m.SZ6  = scale.SZ6
        m.SZ7  = scale.SZ7
        m.SZ8  = scale.SZ8
      ENDIF   
      INSERT INTO (lcTmpFile) FROM MEMVAR 
    ENDSCAN
  ELSE    
    SELECT WHSLOC
    SCAN  FOR loDBFStyle.Seek(WHSLOC.STYLE,'STYLE') ;
                    AND IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.)
             SCATTER MEMO MEMVAR
             m.Desc = Style.Desc
             m.Desc1 = Style.Desc1
             IF loDBFScale.SEEK('S'+ Style.SCALE) 
               m.SZ1  = scale.SZ1
               m.SZ2  = scale.SZ2
               m.SZ3  = scale.SZ3 
               m.SZ4  = scale.SZ4
               m.SZ5  = scale.SZ5
               m.SZ6  = scale.SZ6
               m.SZ7  = scale.SZ7
               m.SZ8  = scale.SZ8
             ENDIF   
             IF loDBFWAREHOUS.Seek(CWARECODE)
               m.cdesc = WAREHOUS.cdesc
             ENDIF   
             INSERT INTO (lcTmpFile) FROM MEMVAR 
    ENDSCAN 
  ENDIF
ENDIF 

 