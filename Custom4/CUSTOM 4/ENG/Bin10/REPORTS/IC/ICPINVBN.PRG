*!***************************************************************************
*! Program file      : ICPINVBN.PRG
*! Program desc.     : Custom Physical Inventory WorkSheet Report By Bin Locations
*! For Report        : (ICPINVBN.FRX)
*! Date              : 07/17/2008
*! System            : Aria Advantage Series.
*! Module        	 : Inventory Control (IC)
*! Developer         : TAREK MOHAMED IBRAHIM
*!***************************************************************************
*! Calls :Functions  : gfDispRe,gfItemMask,lfwRepWhen,lfwOldVal,lfClearRep,
*!                   : lfEvalSegs,lfSRVSty,lfStySum,lfvFabric,FaBrow,
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Example           : DO ICPINVBN
*!***************************************************************************
*! This Report Program is due to *T20071102.0018,10/C200876 
*!***************************************************************************
lcStTime   = TIME()

llRpByBins = llMultBin AND llRpByBins
loOgScroll.cCRorientation = 'L'

IF loOgScroll.llOGFltCh && OG Filters changed
  lfOpenFls()
  lfCrtTemp()
  lfCollect()
ENDIF 

IF lcFree_Clr = 'C'
  lcHeader = [lcStyTitle + ' ' + lcNonMajTl + ' Description             ' + lcStyMajor + ' Desc.']
  lcDetail = [STYLE.STYLE + ' ' + gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) , 'COLOR') + ' ' + STYLE.DESC ]
ELSE  && else style does not have color segment
  lcHeader = [lcStyTitle  + ' ' + 'Description']
  lcDetail = [STYLE.STYLE + ' ' + PADR(STYLE.DESC1,52)]
ENDIF

SELECT(lcTempFile)
SET RELATION TO STYLE INTO STYLE,;
                cWareCode INTO WAREHOUS
IF llRpSectn
  SET ORDER TO TAG SECLOCSTY
ELSE
  SET ORDER TO TAG (lcTempFile)
ENDIF
LOCATE

DO gfDispRe WITH EVAL('lcRpForm')
lcLastExpr = lcRpExp
*-- End of Report code.

*-- Start of function and procedure section.
*!*************************************************************
*! Name        : lfwRepWhen
*! Developer   : TAREK MOHAMED IBRAHIM
*! Date        : 07/17/2008
*! Purpose     : When function of OG
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
FUNCTION lfwRepWhen

IF TYPE('loOGScroll.llFrstTime')='U'
  loOGScroll.AddProperty('llFrstTime')
  loOGScroll.llFrstTime = .F.
ENDIF

IF !loOGScroll.llFrstTime
  loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)  
  loDBFScale    = CreateObject("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
  loDBFWareHous = CreateObject("RemoteTable","WAREHOUS","WAREHOUS",'WAREHOUS',SET("DATASESSION"))&&,"",.T.)
  loDBFCodes   = CreateObject("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
  loDBFWHSLOC  =  CreateObject("RemoteTable","WHSLOC","WHSLOCST",'WHSLOC',SET("DATASESSION"))&&,"",.T.)
    
  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  loOGScroll.llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF 

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)
  IF 'MA' $ oAriaApplication.CompanySetupModules
    SET ORDER TO FABRIC IN FABRIC
  ENDIF 
  SET ORDER TO STYLE IN STYLE
ENDIF
*-- End of Function lfwRepWhen.
*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/17/2008
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
  INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.

*!*************************************************************
*! Name        : lfwOldVal
*! Developer   : TAREK MOHAMED IBRAHIM
*! Date        : 07/17/2008
*! Purpose     : When function to get the Old value
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

*-- End of Function lfwOldVal.
*!*************************************************************
*! Name        : lfEvalSegs
*! Developer   : TAREK MOHAMED IBRAHIM
*! Date        : 07/17/2008
*! Purpose     : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  *-- Compute Free/Color Items in Style code Structure. 
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

  *-- Compute Variables titles.
  lcStyBinTl = 'Print ' + lcStyMajor + '(s) by Bins' 
  lcStyStat  = lcStyMajor + ' Status'
  lcStyMake  = lcStyMajor + ' (Make/Buy)'

RETURN ''
*-- End of Function lfEvalSegs.
*!*************************************************************
*! Name         : lfsrvSty
*! Developer    : TAREK MOHAMED IBRAHIM
*! Date         : 07/17/2008
*! Purpose      : Rise change style flag, in range browse screen.
*!*************************************************************
*! Called from  : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns      : None
*!*************************************************************
*! Example      : =lfsrvSty()
*!*************************************************************
*! Note         : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
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
*-- End of Function lfsrvSty.
*!*************************************************************
*! Name        : lfStySum
*! Developer   : TAREK MOHAMED IBRAHIM
*! Date        : 07/17/2008
*! Purpose     : sum a specific field for the current style in style file
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : Calculated field value.
*!*************************************************************
*! Example     : =lfStySum()
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
*-- End of Function lfStySum.

*!*************************************************************
*! Name        : lfsrLoc
*! Developer   : TAREK MOHAMED IBRAHIM
*! Date        : 07/17/2008
*! Purpose     : Rise change Location flag, in range browse screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example     : =lfsrLoc()
*!*************************************************************
*! Note        : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrLoc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN WAREHOUS
  CASE lcParm = 'R'
ENDCASE
*-- end of lfsrLoc.
*!******************************************************************************************************
*! Name      : lfItmPos
*! Developer : TAREK MOHAMED IBRAHIM
*! Date      : 07/17/2008
*! Purpose   : Evaluate fixed filter position within array.
*!******************************************************************************************************
*! Called from : Report code
*!******************************************************************************************************
*! Passed Parameters  : ...
*!******************************************************************************************************
*! Returns            : Position
*!******************************************************************************************************
*! Example   : = lfItmPos()
*!******************************************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of Function lfItmPos.

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/17/2008
*:* Purpose     : Open files
*:***************************************************************************
FUNCTION lfOpenFls

IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHBINLST','SH')     && STYLE+CWARECODE+CLOCATION
ENDIF
SELECT WHBINLOC
=gfSetOrder('WHBINLST')

*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfCrtTemp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/17/2008
*:* Purpose     : Create needed temp file for the report
*:***************************************************************************
FUNCTION lfCrtTemp
*--Create Temp file.
CREATE TABLE (oAriaApplication.WorkDir + lcTempFile)(STYLE C(19),CWARECODE C(6),CLOCATION C(10),CSECTION C(2))
INDEX ON (STYLE+CWARECODE+CLOCATION+CSECTION) TAG (lcTempFile) OF (lcTempFile)
INDEX ON (CSECTION+CWARECODE+CLOCATION+STYLE) TAG SECLOCSTY OF (lcTempFile)
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

*-- end of lfCrtTemp.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/17/2008
*:* Purpose     : loop to collect the data
*:***************************************************************************
FUNCTION lfCollect

llUseStyle = .F.
llUseDiv   = .F.
llUseSea   = .F.
llUseGrp   = .F.
llUseFab   = .F.
llUseClr   = .F.
llUseWhs   = .F.

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
*!*	lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
*!*	IF lnClrPos > 0
*!*	  lcClrStr = LOOGSCROLL.laOGFXFlt[lnClrPos,6]
*!*	  lcClrFile = loOGScroll.gfTempName()
*!*	  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
*!*	ENDIF

*--Pattern
lcPattStr = ""
lnPattPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.PATTERN'),1)
IF lnPattPos > 0
  lcPattStr = LOOGSCROLL.laOGVRFlt[lnPattPos,6]
ENDIF

*- Style status
lcStatStr  = ""
IF lcRpStatus <>'B'
  lcStatStr = IIF(lcRpStatus='B','',"Style.Status='" + lcRpStatus+"'")
ENDIF

*- Make status
lcMakeOp = IIF(lcRpMake='B','', IIF(lcRpMake='M','','!')+[STYLE.Make])

lnWhPos    = IIF('WAREHOUS.CWARECODE' $ lcRpExp,lfItmPos('WAREHOUS.CWARECODE'),0)
lcWhTitle=''
IF lnWhPos>0 AND !EMPTY(laOgFxFlt[lnWhPos,6]) 
  lcWhFile = laOgFxFlt[lnWhPos,6]
  SELECT &lcWhFile
  COUNT TO lnWhCnt
  IF lnWhCnt>0
    llUseWhs = .T.    
    lcWhTitle = IIF(lnWhCnt>1,'Multi WareHouses',WAREHOUS.CWARECODE + ' - ' + WAREHOUS.CDESC)
  ENDIF
ENDIF

llRpByBins = llMultBin AND llRpByBins

*- Setting the needed relations
*!*	IF llRpByBins
*!*	  SELECT WHBINLOC
*!*	  SET RELATION TO cWareCode+cLocation+Space(19) INTO WHSLOC    
*!*	ENDIF

loDBFWHSLOC.Setorder("WHSLOCST")

IF llUseStyle
  SELECT(lcStylFile)
  SCAN 
    IF !llRpByBins
      *loDBFWHSLOC.Setorder("WHSLOCST")
      =loDBFStyle.Seek(&lcStylFile..CSTYMAJOR,'CSTYLE')
      SELECT STYLE
      SCAN REST WHILE CSTYMAJOR = &lcStylFile..CSTYMAJOR ;
                  FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
                  AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                  AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                  AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                  AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                  AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                  AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.);
                  AND IIF(!EMPTY(lcMakeOp),Eval(lcMakeOp),.T.)
           SCATTER MEMO MEMVAR
           loDBFWHSLOC.SEEK(STYLE.STYLE+'      '+STYLE.CDEFWARE,'WHSLOCST')
           SELECT WHSLOC
           SCAN REST WHILE STYLE+COLOR+CWARECODE+CLOCATION = STYLE.STYLE+'      '+STYLE.CDEFWARE
             m.cSection = WHSLOC.cSection
             WAIT WINDOW NOWAIT M.STYLE
             INSERT INTO (lcTempFile) FROM MEMVAR
             IF !llRpSectn
               EXIT
             ENDIF
           ENDSCAN
      ENDSCAN               
    ELSE
      *loDBFWHSLOC.Setorder("WHSLOC")
      SELECT WAREHOUS
      SCAN FOR IIF(llUseWhs,SEEK(WAREHOUS.CWARECODE,lcWhFile),.T.)
        =loDBFStyle.Seek(&lcStylFile..CSTYMAJOR,'CSTYLE')
        SELECT STYLE
        SCAN REST WHILE CSTYMAJOR = &lcStylFile..CSTYMAJOR ;
                    FOR loDBFWHSLOC.Seek(STYLE.STYLE+'      '+WAREHOUS.CWARECODE,'WHSLOCST') ;
                    AND IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.);
                    AND IIF(!EMPTY(lcMakeOp),Eval(lcMakeOp),.T.)
          
          SELECT WHBINLOC        
          =gfSeek(STYLE.STYLE+WAREHOUS.CWARECODE,'WHBINLOC')
          SCAN
            SCATTER MEMO MEMVAR
            m.cSection = WHSLOC.cSection
            WAIT WINDOW NOWAIT M.STYLE
            INSERT INTO (lcTempFile) FROM MEMVAR 
          ENDSCAN
          
        ENDSCAN        
      ENDSCAN      
    ENDIF  
  ENDSCAN  
ELSE
  IF !llRpByBins
    *loDBFWHSLOC.Setorder("WHSLOCST")
    SELECT STYLE
    SCAN FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
             AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
             AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
             AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
             AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
             AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
             AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.);
             AND IIF(!EMPTY(lcMakeOp),Eval(lcMakeOp),.T.)
      SCATTER MEMO MEMVAR
      loDBFWHSLOC.SEEK(STYLE.STYLE+'      '+STYLE.CDEFWARE,'WHSLOCST')
      m.cSection = WHSLOC.cSection
      SELECT WHSLOC
      SCAN REST WHILE STYLE+COLOR+CWARECODE+CLOCATION = STYLE.STYLE+'      '+STYLE.CDEFWARE
        WAIT WINDOW NOWAIT M.STYLE
        INSERT INTO (lcTempFile) FROM MEMVAR 
        IF !llRpSectn
          EXIT
        ENDIF
      ENDSCAN
    ENDSCAN
  ELSE    
    *loDBFWHSLOC.Setorder("WHSLOCST")
    SELECT WAREHOUS
    SCAN FOR IIF(llUseWhs,SEEK(WAREHOUS.CWARECODE,lcWhFile),.T.)
      SELECT STYLE
      SCAN FOR loDBFWHSLOC.Seek(STYLE.STYLE+'      '+WAREHOUS.CWARECODE,'WHSLOCST') ;
           AND IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen),lcClrFile),.T.);
           AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
           AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
           AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
           AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
           AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
           AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.);
           AND IIF(!EMPTY(lcMakeOp),Eval(lcMakeOp),.T.)

        SELECT WHBINLOC
        =gfSeek(STYLE.STYLE+WAREHOUS.CWARECODE,'WHBINLOC')
        SCAN
          SCATTER MEMO MEMVAR
          m.cSection = WHSLOC.cSection
          WAIT WINDOW NOWAIT M.STYLE
          INSERT INTO (lcTempFile) FROM MEMVAR 
        ENDSCAN
        
      ENDSCAN 
    ENDSCAN
  ENDIF
ENDIF 
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/17/2008
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
*! Name      : lfFabSum
*! Developer : MAriam Mazhar[MMT]
*! Date      : 07/17/2008
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