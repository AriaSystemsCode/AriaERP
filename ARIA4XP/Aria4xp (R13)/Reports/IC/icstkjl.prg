*:***************************************************************************
*: Program file  : ICSTKJL
*: Program desc. : Stock Adjustment Journal Report
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 03/22/2005
*! Entry No.     : N000473 - Invenetory Adjustment Journal
*!***************************************************************************
*: Modifications:
*: B131829,1 WSH 04/19/2006 Adjust the report to be called from the Inventory Screens
*: B040321,1 MMT 08/07/2006 fix bug of error after preview report
*: B607853,1 MMT 12/03/2006 Error while printing report after saving phyiscal inventory (T20061203.0002)
*: B608354,1 NNA 11/16/2007 Fix a problem about Error message appears when responding Yes to print Inventory Adjustment report
*: B608354,1 NNA            with invenory transfere , phisical or adjustment
*: B608471,1 WAM 03/04/2008 Add another record with opposite sign only if called from inventory transfer screen T20080228.0006
*: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[T20081124.0010]
*: B609838,1 MMT 02/22/2012 Stock Adjustment report hangs when user select style group[T20120125.0010]
*! B610304,1 HIA 04/15/2013 T20130305.0043 - Stock Adjustments Report - Export to Excel is different from printed
*! B610685,1 TMI 03/03/2014 Add else to skip in lcStyInvJl [T20140207.0003]
*! B610919,1 MMT 12/11/2014 Stock adjustment report deos not filter by date correctly if location is selected[T20141204.0022]
*! B611297,1 AHH 02/04/2017 NEGATIVE INVENTORY [T20170328.0031]
*! B611361,1 HIA 07/13/2013 Error while select location without selecting filtering by date [T20170713.0012]
*! B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022]
*! B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037]
*! B611716,1 Es 2/10/2019 Modify issue "When user does not select Style in Inventory adjustment report, report does not show data." [T20190204.0016]
*! B611946,1 MMT 02/20/2025  La Cera - Stock Adjustment Journal - Print Issue[T-ERP-20250107.002]
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start] 
#include  r:\aria4xp\reports\ic\icstkjl.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*B131829,1 WSH 04/19/2006 Check if it is called from the Screen. [Start]
PRIVATE llFromScreen
llFromScreen = TYPE("lcScreenTemp") = "C" AND FILE(oAriaApplication.WorkDir + lcScreenTemp + ".DBF")
*B131829,1 WSH 04/19/2006 [End]

DIMENSION laAdjQty[8],laTotQty[8]
STORE 0 TO laAdjQty,laTotQty
*--Array hold Parameters will be sent to Crystal report
loOGScroll.cCRPaperSize = 'LETTER'
IF lcRpFormat='S'
  DIMENSION loOGScroll.laCRParams[12,2]
ELSE
  DIMENSION loOGScroll.laCRParams[14,2]
  loOGScroll.laCRParams[13,1] = 'ShowSub'
  IF lcRpRepF <> 'A'
    loOGScroll.laCRParams[13,2] = 1
  ELSE
    loOGScroll.laCRParams[13,2] = 0
  ENDIF
  loOGScroll.laCRParams[14,1] = 'Type'
  loOGScroll.laCRParams[14,2] = IIF(!EMPTY(lcRpInvExp),lcRpInvExp,'LTPA')
ENDIF

*-- Cost priviliage
loOGScroll.laCRParams[11,1] = 'CostPrv'
IF qCostPrv
  loOGScroll.laCRParams[11,2] = 1
ELSE
  loOGScroll.laCRParams[11,2] = 0
ENDIF

*-- Average cost
loOGScroll.laCRParams[12,1] = 'AVG_COST'
IF XAVG_COST
  loOGScroll.laCRParams[12,2] = 1
ELSE
  loOGScroll.laCRParams[12,2] = 0
ENDIF


*--print decimals or not
loOGScroll.laCRParams[10,1] = 'PrnDec'

IF llRpDec
  loOGScroll.laCRParams[10,2] = 1
ELSE
  loOGScroll.laCRParams[10,2] = 0
ENDIF

loOGScroll.cCROrientation = 'L'
*--The Layout Parmaeter
loOGScroll.laCRParams[1,1] = 'Layout'
IF lcRpFormat = "S"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOGScroll.laCRParams[1,2] = Lang_Short
  loOGScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Short,oAriaApplication.GetHeaderText("Lang_Short",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loOGScroll.lcOGLastForm = "ICSTKJLS"
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOGScroll.laCRParams[1,2] = Lang_Long
  loOGScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Long,oAriaApplication.GetHeaderText("Lang_Long",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loOGScroll.lcOGLastForm  = "ICSTKJLL"
ENDIF

*--Show Details or not
loOGScroll.laCRParams[2,1] = 'ShowDetails'
IF llRpShow
  loOGScroll.laCRParams[2,2] = 1
ELSE
  loOGScroll.laCRParams[2,2] = 0
ENDIF

*--Style Major Length
loOGScroll.laCRParams[3,1] = "StyLen"
loOGScroll.laCRParams[3,2] = lnMajorLen

*--Report Name
loOGScroll.laCRParams[4,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOGScroll.laCRParams[4,2] = Lang_Stock_Adjustments_Journal
loOGScroll.laCRParams[4,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Stock_Adjustments_Journal,oAriaApplication.GetHeaderText("Lang_Stock_Adjustments_Journal",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*--Style Title
loOGScroll.laCRParams[5,1] = "Style_Title"
loOGScroll.laCRParams[5,2] = lcStyTitle

*--use configuration or not
loOGScroll.laCRParams[6,1] = 'USECONF'
IF (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y') AND (ALLTRIM(gfGetMemVar("M_STYCNFG")) = "Y")
  loOGScroll.laCRParams[6,2]= 1
ELSE
  loOGScroll.laCRParams[6,2]= 0
ENDIF

*--Multi warehous or not
loOGScroll.laCRParams[7,1] = 'llMultiWH '
IF llMultiWH
  loOGScroll.laCRParams[7,2]= 1
ELSE
  loOGScroll.laCRParams[7,2]= 0
ENDIF

*B131829,1 WSH 04/19/2006 If called from the Screen, don't update Date here. [Start]
*loOGScroll.laCRParams[8,1] = "Start"
*loOGScroll.laCRParams[8,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnDatePos,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
*
*loOGScroll.laCRParams[9,1] = "End"
*loOGScroll.laCRParams[9,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
LOCAL lcDevice
lcDevice = oAriaApplication.gcDevice

loOGScroll.laCRParams[8,1] = "Start"
loOGScroll.laCRParams[9,1] = "End"
IF !llFromScreen
  loOGScroll.laCRParams[8,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnDatePos,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
  loOGScroll.laCRParams[9,2] = IIF(EMPTY(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
ELSE
  oAriaApplication.gcDevice = 'SCREEN'
ENDIF
*B131829,1 WSH 04/19/2006 [End]

IF llOGFltCh &&OR llClearFn
  *--lcStyleFnl
  IF lcInvtAdj <> lcTempInvtAdj
    IF USED(lcInvtAdj)
      USE IN (lcInvtAdj)
    ENDIF
  ELSE
    lcInvtAdj = loOGScroll.gfTempName()
  ENDIF
  IF lcStyleFnl = lcTempStyle && OR lcStyleFnl = lcTempStyle_x
    lcStyleFnl = loOGScroll.gfTempName()
    SELECT * FROM &lcTempStyle  WHERE .F. INTO CURSOR &lcStyleFnl READWRITE
    =lfMakeIndex(lcStyleFnl)

  ENDIF
  IF lcStyleFile <> lcTempStyle
    IF USED(lcStyleFile)
      USE IN (lcStyleFile)
      SELECT * FROM &lcTempStyle  WHERE STYLE = '*****' INTO CURSOR &lcStyleFile READWRITE
      =lfMakeIndex(lcStyleFile)
      SELECT * FROM &lcTempStyle  WHERE STYLE = '*****' INTO CURSOR &lcStyleFnl READWRITE
      =lfMakeIndex(lcStyleFnl)
    ENDIF
  ELSE
    lcStyleFile = loOGScroll.gfTempName()
    SELECT * FROM &lcTempStyle  WHERE STYLE = '*****' INTO CURSOR &lcStyleFile READWRITE
    =lfMakeIndex(lcStyleFile)
  ENDIF
  IF lcStyInvJl <> lcTempStyInvJl
    IF USED(lcStyInvJl)
      USE IN (lcStyInvJl)
      *      SELECT * FROM &lcTempStyInvJl WHERE ;
      STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = '*****' ;
      INTO CURSOR &lcStyInvJl READWRITE
      SELECT * FROM &lcTempStyInvJl WHERE .F. INTO CURSOR &lcStyInvJl READWRITE
      =lfMakeIndex(lcStyInvJl)
    ENDIF
  ELSE
    lcStyInvJl = loOGScroll.gfTempName()
    *SELECT * FROM &lcTempStyInvJl WHERE ;
    STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = '*****' ;
    INTO CURSOR &lcStyInvJl READWRITE
    SELECT * FROM &lcTempStyInvJl WHERE .F. INTO CURSOR &lcStyInvJl READWRITE
    =lfMakeIndex(lcStyInvJl)
  ENDIF

  *B131829,1 WSH 04/19/2006 [Start]
  IF llFromScreen
    IF !USED(lcScreenTemp )
      USE oAriaApplication.WorkDir + lcScreenTemp + ".DBF" IN 0 ALIAS (lcScreenTemp)
    ENDIF
    *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[Start]
    IF USED(lcInvTmp)
      *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[End]
      SELECT (lcInvTmp)
      DELETE ALL
      *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[Start]
    ENDIF
    *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[End]

    SELECT (lcScreenTemp)
    LOCATE

    loOGScroll.laCRParams[8,2] = DTOC(EVALUATE(lcScreenTemp + '.Date'))
    loOGScroll.laCRParams[9,2] = DTOC(EVALUATE(lcScreenTemp + '.Date'))
  ENDIF
  *B131829,1 WSH 04/19/2006 [End]

  =lfCollectData()

  SELECT (lcInvTmp)
  lcDispFile = lcInvTmp
  *  lcDispFile = lfGetInvtAdj(lcInvTmp,lcDispFile)
ENDIF
*/-- the temp file will preview from it

*! B610304,1 HIA 04/15/2013 T20130305.0043 - Stock Adjustments Report - Export to Excel is different from printed
SELECT(lcDispFile)
IF (lcRpFormat ='S') AND  (loOGScroll.cTextRepType == "EXCEL" OR loOGScroll.cTextRepType == "EXCELX") AND (oAriaApplication.gcDevice = "FILE" )
  SCAN
    IF TYPE = "P" OR TYPE = "L"
      REPLACE adj1 WITH adj1 - OldQty1
      REPLACE adj2 WITH adj2 - OldQty2
      REPLACE adj3 WITH adj3 - OldQty3
      REPLACE adj4 WITH adj4 - OldQty4
      REPLACE adj5 WITH adj5 - OldQty5
      REPLACE adj6 WITH adj6 - OldQty6
      REPLACE adj7 WITH adj7 - OldQty7
      REPLACE adj8 WITH adj8 - OldQty8

      REPLACE TOTADJ WITH ( adj1 + adj2 + adj3 + adj4 + adj5 + adj6 + adj7 + adj8 )

    ENDIF
  ENDSCAN
  llOGFltCh = .T.
  SELECT(lcDispFile)
  LOCATE
ENDIF
*! B610304,1 HIA 04/15/2013 T20130305.0043 - Stock Adjustments Report - Export to Excel is different from printed

SELECT(lcDispFile)
IF !EMPTY(lcRpInvExp)
  SET FILTER TO TYPE $ lcRpInvExp
ENDIF
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok >
  = gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
ELSE
  lcRepFile  = loOGScroll.gfTempName()
  lcScaleRep = loOGScroll.gfTempName()

  SELECT(lcScale)
  COPY TO oAriaApplication.WorkDir+lcScaleRep+'.DBF'

  SELECT(lcDispFile)
  COPY TO oAriaApplication.WorkDir+lcRepFile+'.DBF'

  DIMENSION loOGScroll.lacrTABLES[2]  && array For Temp Table & pathes
  loOGScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF'
  loOGScroll.lacrTABLES[2]= oAriaApplication.WorkDir+lcScaleRep+'.DBF'
*! B611946,1 MMT 02/20/2025  La Cera - Stock Adjustment Journal - Print Issue[T-ERP-20250107.002][Start]
IF llFromScreen
 oAriaApplication.gcDevice = lcDevice
ENDIF
*! B611946,1 MMT 02/20/2025  La Cera - Stock Adjustment Journal - Print Issue[T-ERP-20250107.002][End]
  *B131829,1 WSH 04/19/2006 [Start]
  *= gfDispRe("ICSTKJL")
  = gfDispRe(IIF(llFromScreen, "ICSTKJL", .F.))
  *B131829,1 WSH 04/19/2006 [End]

ENDIF

*B131829,1 WSH 04/19/2006 [Start]
*B040321,1 MMT 08/07/2006 fix bug of error after preview report[Start]
*IF !llFromScreen
IF llFromScreen
  *B040321,1 MMT 08/07/2006 fix bug of error after preview report[End]
  IF USED(lcScreenTemp)
    USE IN (lcScreenTemp)
  ENDIF
  oAriaApplication.gcDevice = lcDevice
ENDIF
*B131829,1 WSH 04/19/2006 [End]

*-- end of report code...
*----------------------- Report Code End -----------------------------


*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************

*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mariam Mazhar(MMT)
*! Date      : 10/19/98
*! Purpose   : When run OG function.
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
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfWRunGrid
  *-- First time run or user press < Reset > .
  *--Scale
  *--lcScaleTemp lcScale
  IF TYPE('loDBFScale') <> 'O'

    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[Start]
    *!*	  loDBFScale = CreateObject("RemoteTable","Scale","Scale",lcScaleTemp,SET("DATASESSION"))
    *!*	  SELECT * FROM &lcScaleTemp WHERE .F. INTO CURSOR &lcScale READWRITE
    loDBFScale = CREATEOBJECT("RemoteTable","Scale","Scale","Scale",SET("DATASESSION"))
    SELECT * FROM  "Scale" WHERE .F. INTO CURSOR &lcScale READWRITE
    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[End]

    =lfMakeIndex(lcScale)
  ENDIF

  *--Style file
  IF TYPE('loDBFStyle') <> 'O'
    loDBFStyle = CREATEOBJECT("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION"))
    SELECT * FROM &lcTempStyle  WHERE .F. INTO CURSOR &lcStyleFile READWRITE
    =lfMakeIndex(lcStyleFile)
  ENDIF

  *B131829,1 WSH 04/19/2006 Open the StyDye file. [Start]
  IF TYPE('loDBFStyDye') <> 'O'
    loDBFStyDye = CREATEOBJECT("RemoteTable","StyDye","StyDye",lcTempStyDye,SET("DATASESSION"))
  ENDIF
  *B131829,1 WSH 04/19/2006 [End]

  *--InvtAdj file
  IF TYPE('loDBFInvtAdj') <> 'O'
    loDBFInvtAdj = CREATEOBJECT("RemoteTable","InvtAdj","InvtAdj",lcTempInvtAdj,SET("DATASESSION"))
    SELECT * FROM &lcTempInvtAdj WHERE .F. INTO CURSOR &lcInvtAdj READWRITE
    =lfMakeIndex(lcInvtAdj)
  ENDIF

  *--WareHous file
  IF TYPE('loDBFWareHous') <> 'O'
    loDBFWareHous   = CREATEOBJECT("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION"))
  ENDIF

  *--StyInvJl file
  IF TYPE('loDBFStyInvJl') <> 'O'
    loDBFStyInvJl  = CREATEOBJECT("RemoteTable","StyInvJl","StyInvJl",lcTempStyInvJl,SET("DATASESSION"))
    SELECT * FROM &lcTempStyInvJl WHERE .F. INTO CURSOR &lcStyInvJl READWRITE
    =lfMakeIndex(lcStyInvJl)
  ENDIF

  IF EMPTY(lcLastExpr)
    lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
    IF lnDatePos > 0
      lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    ENDIF
    lnLocPos = ASCAN(laOGFxFlt,'INVTADJ.CFROMWARE')
    IF lnLocPos > 0
      lnLocPos = ASUBSCRIPT(laOGFxFlt,lnLocPos,1)
    ENDIF
    R_WIDTH    = 'W'
  ENDIF


  *!*************************************************************
  *! Name      : lfsrvSty
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 10/19/98
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
      USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
      SELECT STYLE
      SET ORDER TO TAG Cstyle
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      GO TOP IN STYLE
      llChStyle = .T.
    CASE lcParm = 'R'  && Reset code
      USE IN STYLE_X
      SELECT STYLE
      SET ORDER TO TAG STYLE
  ENDCASE
  *-- end of lfsrvSty.

  *!*************************************************************
  *! Name      : lfStySum
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 10/19/98
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
  SELECT STYLE_X
  SET ORDER TO STYLE
  IF SEEK(ALLTRIM(lcSty))
    SUM &lccomp TO lnTotcomp WHILE cStyMajor = lcSty
  ENDIF

  SELECT STYLE
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
  *! Name      : lfWOldVal
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 10/19/98
  *! Purpose   : Get any get field old value.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : Option Grid, any Get Field.
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : =lfWOldVal()
  *!*************************************************************
FUNCTION lfWOldVal
  *-- Converted 11/22/02 12:48:40 AM, Get Active object reference.[BEGIN
  laOldVal = EVALUATE(OGSYS18())
  *-- Converted 11/22/02 12:48:40 AM, Get Active object reference.[END..

  *!*************************************************************
  *! Name      : lfEvalSegs
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 10/19/98
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

  lnMajSeg   = gfItemMask('SM')  && No. of major segments.
  lcMajPict  = gfItemMask("PM")

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

  lnMajorLen = LEN(lcMajPict)
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcColorTlt = Lang_ONLYTHESE+' ' + ALLTRIM(lcNonMajTl) + 's'
  lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_ONLYTHESE,oAriaApplication.GetHeaderText("Lang_ONLYTHESE",AHEADERFILE))+' ' + ALLTRIM(lcNonMajTl) + 's'
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[END]
  *-- Compute Free/Color Items in Style code Structure. [End]

  lcStyGrp = lcStyMajor + ' Group'

  *-- Define Mover for Inventory adjustments arrays. [begin]
  lnArrayLen = IIF(llMultiWH,4,3)
  DECLARE laRpSorInv[lnArrayLen,1],laRpTarInv[lnArrayLen,1]
  STORE 'Adjustments' TO laRpSorInv[1],laRpTarInv[1]
  STORE 'Physical'    TO laRpSorInv[2],laRpTarInv[2]
  STORE 'Locking'     TO laRpSorInv[3],laRpTarInv[3]
  IF lnArrayLen = 4
    STORE 'Transfer' TO laRpSorInv[4],laRpTarInv[4]
  ENDIF
  *-- Define Mover for Inventory adjustments arrays. [end  ]
  RETURN ''
  *-- end of lfEvalSegs.

  *!*************************************************************
  *! Name      : lfVRepF
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 03/24/2005
  *! Purpose   : If user change report form valid function.
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
  *! Example   : =lfVRepF()
  *!*************************************************************
FUNCTION lfVRepF
  ClearRead()
  *-- end of lfVRepF.

  *!*************************************************************
  *! Name      : lfvInvTran
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 10/19/98
  *! Purpose   : If user Press Inventort transaction button.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : gfMover
  *!*************************************************************
  *! Called from : Option Grid
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!***************************************************************************
  *! Modifications:
  *! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
  *!***************************************************************************
  *! Example   : =lfvInvTran()
  *!*************************************************************
  * N000862 ,1 Thabet Handle globalization issues [Start]

FUNCTION lfvInvTran
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *= lfOGMover(@laRpSorInv,@laRpTarInv,Lang_Select_Inventory_Transaction_type,.T.,'')  && call mover function.
  = lfOGMover(@laRpSorInv,@laRpTarInv,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Select_Inventory_Transaction_type,oAriaApplication.GetHeaderText("Lang_Select_Inventory_Transaction_type",AHEADERFILE)),.T.,'')  && call mover function.
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  = lfInvExpr()
  *-- end of lfvInvTran.
  *FUNCTION lfvInvTran
  *= lfOGMover(@laRpSorInv,@laRpTarInv,'Select Inventory Transaction type',.T.,'')  && call mover function.
  *= lfInvExpr()
  *-- end of lfvInvTran.

  * N000862 ,1 Thabet Handle globalization issues [Start]

  *!*************************************************************
  *! Name      : lfInvExpr
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 11/21/1998
  *! Purpose   : - Evaluate Company expression.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Called from : lfvInvTran
  *!*************************************************************
  *! Passed Parameters  : ....
  *!*************************************************************
  *! Returns            : ....
  *!*************************************************************
  *! Example   : = lfInvExpr()
  *!*************************************************************
FUNCTION lfInvExpr
  PRIVATE laTarget

  lcRpInvExp = ''

  *-- Copy Used array.
  IF EMPTY(laRpTarInv)
    *= ACOPY(laRpSorInv,laTarget)
    RETURN .T.
  ELSE
    = ACOPY(laRpTarInv,laTarget)
  ENDIF

  = ASORT(laTarget)  && Sort array.

  *-- loop to get inventory transaction type expression.
  FOR lnI = 1 TO ALEN(laTarget,1)
    lcRpInvExp = IIF(EMPTY(lcRpInvExp),PADR(laTarget[lnI],1),;
      lcRpInvExp + ','+PADR(laTarget[lnI],1))
  ENDFOR
  *-- end of lfInvExpr.
  *!*************************************************************
  *! Name      : lfClearRep
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 05/27/1998
  *! Purpose   : Function that we call when Close the option grid.
  *!*************************************************************
  *! Called from : [Option Grid] < Close > button.
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfClearRep()
  *!*************************************************************
FUNCTION _lfClearRep
  llClearFn = .T.  && Rise clear done flag.
  IF USED('WAREHOUS') .AND. llWHare
    USE IN WAREHOUS
  ENDIF
  IF USED('CODES')
    USE IN CODES
  ENDIF
  *-- end of lfClearRep.

  *!*************************************************************
  *! Name      : lfLockApen
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 24/08/2000
  *! Purpose   : Function that Append Locking Transaction to tmp file
  *!*************************************************************
  *! Called from : ....
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfLockApen()
  *!*************************************************************
  *!B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
  *!
FUNCTION lfLockApen
  *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][Start]
  lccurrentsty = EVALUATE(lcStyInvJl+'.STYLE')
  *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][END]
  SELECT (lcInvTmp)
  APPEND BLANK
  REPLACE STYLE     WITH EVALUATE(lcStyInvJl+'.STYLE')                                             ,;
    DATE      WITH EVALUATE(lcStyInvJl+'.DTRDATE')                                            ,;
    TYPE      WITH 'L'                                                         ,;
    CSESSION  WITH EVALUATE(lcStyInvJl+'.CSESSION')                                         ,;
    OLD_COST  WITH IIF(EVALUATE(lcStyInvJl+'.NTOTSTK')=0,0,EVALUATE(lcStyInvJl+'.NSTKVAL')/EVALUATE(lcStyInvJl+'.NTOTSTK')) ,;
    TOTOLD    WITH -EVALUATE(lcStyInvJl+'.NTOTSTK')                                           ,;
    CFROMWARE WITH EVALUATE(lcStyInvJl+'.CWARECODE')
  FOR I = 1 TO 8
    Z = STR(I,1)
    REPLACE OLDQTY&Z. WITH -&lcStyInvJl..NSTK&Z
  ENDFOR
  SELECT(lcStyInvJl)
  IF !EOF(lcStyInvJl)
    *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][Start]
    *!* B611297,1 AHH 02/04/2017 NEGATIVE INVENTORY [Begin][T20170328.0031]
    *!*	  *Skip
    *!*	  lcSrtyleOld = EVALUATE(lcStyInvJl+'.STYLE')
    SKIP

    *!*	  IF !(lcSrtyleOld == EVALUATE(lcStyInvJl+'.STYLE'))
    *!*	    SKIP -1
    *!*	  ENDIF
    *!* B611297,1 AHH 02/04/2017 NEGATIVE INVENTORY [End][T20170328.0031]
    *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][END]
  ENDIF
  *!* B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037]
  *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][Start]
  *!*	 IF !EOF(lcStyInvJl) AND  lccurrentsty = EVALUATE(lcStyInvJl+'.STYLE')
  *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][END]
  IF !EOF(lcStyInvJl)
    IF lccurrentsty = EVALUATE(lcStyInvJl+'.STYLE')
      *!* B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037][End]
      SELECT (lcInvTmp)
      REPLACE UNT_COST WITH IIF(EVALUATE(lcStyInvJl+'.NTOTSTK')=0,0,EVALUATE(lcStyInvJl+'.NSTKVAL')/EVALUATE(lcStyInvJl+'.NTOTSTK')) ,;
        TOTADJ   WITH EVALUATE(lcStyInvJl+'.NTOTSTK')
      FOR I = 1 TO 8
        Z = STR(I,1)
        REPLACE ADJ&Z. WITH &lcStyInvJl..NSTK&Z
      ENDFOR
      *!* B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037]
    ELSE
      SKIP -1
    ENDIF
    *!* B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037][End]
    *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][Start]
  ENDIF
  *!* B611393,1 AHH 24/08/2017 Testing the Inventory Locking functionality[T20170817.0022][END]
  loDBFStyle.SEEK(EVALUATE(lcInvTmp+'.Style'))
  SELECT (lcInvTmp)
  REPLACE StotCost  WITH EVALUATE(lcTempStyle+'.TotCost'),;
    Save_Cost WITH EVALUATE(lcTempStyle+'.Ave_Cost'),;
    Cdivision WITH EVALUATE(lcTempStyle+'.Cdivision') ,;
    SCALE     WITH EVALUATE(lcTempStyle+'.Scale')
  loDBFScale.SEEK("S"+EVALUATE(lcTempStyle+'.Scale'))
  IF !SEEK('S'+EVALUATE(lcTempStyle+'.Scale'),lcScale)

    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[Start]
    *SELECT(lcScaleTemp)
    SELECT "Scale"
    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[End]

    SCATTER MEMO MEMVAR
    SELECT(lcScale)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDIF
  *---
  SELECT(lcStyInvJl)
  IF !EOF(lcStyInvJl)
    SKIP
  ENDIF
  *-- end of lfLockApen.

  *!*************************************************************
  *! Name      : lfAPTAppen
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 24/08/2000
  *! Purpose   : Function that Append Not Locking Transaction to tmp file
  *!*************************************************************
  *! Called from : ....
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAPTAppen()
  *!*************************************************************
  *!
FUNCTION lfAPTAppen

  SCATTER MEMVAR
  SELECT (lcInvTmp)
  APPEND BLANK
  GATHER MEMVAR
  IF TYPE = 'T'
    loDBFStyle.SEEK(EVALUATE(lcInvTmp+'.Style'))
    SELECT (lcInvTmp)
    REPLACE StotCost  WITH EVALUATE(lcTempStyle+'.TotCost'),;
      Save_Cost WITH EVALUATE(lcTempStyle+'.Ave_Cost'),;
      Cdivision WITH EVALUATE(lcTempStyle+'.Cdivision') ,;
      SCALE     WITH EVALUATE(lcTempStyle+'.Scale')
  ENDIF
  SELECT (lcInvTmp)

  *B608354,1 NNA 11/16/2007 (Begin) Check if this Report calls from a screen or option grid
  *B608471,1 WAM 03/04/2008 (Add another record with opposite sign only if called from inventory transfer screen
  *IF llFromScreen THEN
  IF llFromScreen AND TYPE = 'T'
    *B608471,1 WAM 03/04/2008 (End)

    APPEND BLANK
    GATHER MEMVAR
    REPLACE adj1   WITH  -1 * adj1  ,;
      adj2   WITH  -1 * adj2  ,;
      adj3   WITH  -1 * adj3  ,;
      adj4   WITH  -1 * adj4  ,;
      adj5   WITH  -1 * adj5  ,;
      adj6   WITH  -1 * adj6  ,;
      adj7   WITH  -1 * adj7  ,;
      adj8   WITH  -1 * adj8  ,;
      TOTADJ WITH  -1 * TOTADJ
  ELSE
    *B608354,1 NNA (End)

    IF TYPE = 'T' AND EMPTY(laOGFxFlt[lnLocPos,6])
      APPEND BLANK
      GATHER MEMVAR
      REPLACE adj1   WITH  -1 * adj1  ,;
        adj2   WITH  -1 * adj2  ,;
        adj3   WITH  -1 * adj3  ,;
        adj4   WITH  -1 * adj4  ,;
        adj5   WITH  -1 * adj5  ,;
        adj6   WITH  -1 * adj6  ,;
        adj7   WITH  -1 * adj7  ,;
        adj8   WITH  -1 * adj8  ,;
        TOTADJ WITH  -1 * TOTADJ
    ELSE
      IF TYPE = 'T' AND !EMPTY(laOGFxFlt[lnLocPos,6])
        SELECT(laOGFxFlt[lnLocPos,6])
        IF SEEK(CFROMWARE)
          SELECT (lcInvTmp)
          *--  AND ( laOGFxFlt[lnLocPos,6] == cFromWare)
          REPLACE adj1   WITH  -1 * adj1  ,;
            adj2   WITH  -1 * adj2  ,;
            adj3   WITH  -1 * adj3  ,;
            adj4   WITH  -1 * adj4  ,;
            adj5   WITH  -1 * adj5  ,;
            adj6   WITH  -1 * adj6  ,;
            adj7   WITH  -1 * adj7  ,;
            adj8   WITH  -1 * adj8  ,;
            TOTADJ WITH  -1 * TOTADJ
        ENDIF
      ENDIF
    ENDIF

    *B608354,1 NNA (Start)
  ENDIF
  *B608354,1 NNA (End)

  loDBFStyle.SEEK(EVALUATE(lcInvTmp+'.Style'))
  SELECT (lcInvTmp)
  REPLACE StotCost  WITH EVALUATE(lcTempStyle+'.TotCost'),;
    Save_Cost WITH EVALUATE(lcTempStyle+'.Ave_Cost'),;
    Cdivision WITH EVALUATE(lcTempStyle+'.Cdivision') ,;
    SCALE     WITH EVALUATE(lcTempStyle+'.Scale')
  loDBFScale.SEEK("S"+EVALUATE(lcTempStyle+'.Scale'))
  IF !SEEK('S'+EVALUATE(lcTempStyle+'.Scale'),lcScale)

    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[Start]
    *SELECT(lcScaleTemp)
    SELECT "Scale"
    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[End]

    SCATTER MEMO MEMVAR
    SELECT(lcScale)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDIF

  SELECT(lcFnlInvtAdj)
  IF !EOF(lcFnlInvtAdj)
    SKIP
  ENDIF
  *-- end of lfAPTAppen.

  *!*************************************************************
  *! Name      : lfUpdtSTot
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 24/08/2000
  *! Purpose   : Function that Update SubTotals in Long B Format
  *!*************************************************************
  *! Called from : ....
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfUpdtSTot()
  *!*************************************************************
  *!B803556,1 AMH 23/08/2000 Fix the bug of Dont Printing Locking Adjustment
  *!
FUNCTION lfUpdtSTot
  lnCol = IIF(TYPE='A',1,IIF(TYPE='T',2,IIF(TYPE='L',3,4)))
  FOR lnC = 1 TO 8
    lcC = STR(lnC,1)
    laAllQty[lnC,lnCol] = laAllQty[lnC,lnCol] + EVAL('Adj'+lcC) - IIF(lnCol>2,EVAL('OldQty'+lcC),0)
    laAlTotQty[lnC,lnCol] = laAlTotQty[lnC,lnCol] + EVAL('Adj'+lcC) - IIF(lnCol>2,EVAL('OldQty'+lcC),0)
  ENDFOR
  XSTOTAlPCS[lnCol]  = XSTOTAlPCS[lnCol] + TOTADJ - IIF(lnCol>2,TOTOLD,0)
  XSTOTAlAMT[lnCol]  = XSTOTAlAMT[lnCol] + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) ;
    - IIF(lnCol>2,(TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST)),0)
  XGTOTAlPCS[lnCol]  = XGTOTAlPCS[lnCol] + TOTADJ - IIF(lnCol>2,TOTOLD,0)
  XGTOTAlAMT[lnCol]  = XGTOTAlAMT[lnCol] + (TOTADJ * IIF(XAVG_COST,UNT_COST,STYLE.TOTCOST)) ;
    - IIF(lnCol>2,(TOTOLD*IIF(XAVG_COST,OLD_COST,STYLE.TOTCOST)),0)
  *-- end of lfUpdtSTot.

  *!*****************************************************************************************
  *! Name      : RefreshTransaction
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 11/24/2002 08:24:23 PM
  *! Purpose   : Refresh the transactions readable area.
  *! Entry no. : N000473 - Invenetory Adjustment Journal
  *!*****************************************************************************************
  *!
FUNCTION RefreshTransaction
  LOCAL lcTransactionsStr, lnTarget
  lcTransactionsStr = ""
  IF !EMPTY(laRpTarInv)
    FOR lnTarget = 1 TO ALEN(laRpTarInv,1)
      lcTransactionsStr = lcTransactionsStr + ", " + laRpTarInv[lnTarget]
    ENDFOR
    lcTransactionsStr = SUBSTR(lcTransactionsStr,3)
  ENDIF
  RETURN lcTransactionsStr
ENDFUNC
*-- end of RefreshTransaction.
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 24/08/2000
*! Purpose   : Function that Append Locking Transaction to tmp file
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfLockApen()
*!*************************************************************
FUNCTION lfCreateTemp

  SELECT(lcTempInvtAdj)
  DIMENSION laInvTmp[46,18]

  laInvTmp[1,1]   = "Style"
  laInvTmp[1, 2]  = 'C'
  laInvTmp[1 , 3] = 19
  laInvTmp[1 , 4] = 0

  laInvTmp[2,1]   = "Dyelot"
  laInvTmp[2, 2]  = 'C'
  laInvTmp[2 , 3] = 10
  laInvTmp[2 , 4] = 0

  laInvTmp[3,1]   = "cReason"
  laInvTmp[3, 2]  = 'C'
  laInvTmp[3 , 3] = 25
  laInvTmp[3 , 4] = 0

  laInvTmp[4,1]   = "Date"
  laInvTmp[4, 2]  = 'D'
  laInvTmp[4 , 3] = 8
  laInvTmp[4 , 4] = 0

  laInvTmp[5,1]   = "dPostDate"
  laInvTmp[5, 2]  = 'D'
  laInvTmp[5 , 3] = 8
  laInvTmp[5 , 4] = 0

  laInvTmp[6,1]   = "Type"
  laInvTmp[6, 2]  = 'C'
  laInvTmp[6 , 3] = 1
  laInvTmp[6 , 4] = 0

  laInvTmp[7,1]   = "cSession"
  laInvTmp[7, 2]  = 'C'
  laInvTmp[7 , 3] = 6
  laInvTmp[7 , 4] = 0

  laInvTmp[8,1]   = "Unt_Cost"
  laInvTmp[8, 2]  = 'N'
  laInvTmp[8 , 3] = 12
  laInvTmp[8 , 4] = 2

  laInvTmp[9,1]   = "Old_Cost"
  laInvTmp[9, 2]  = 'N'
  laInvTmp[9 , 3] = 12
  laInvTmp[9 , 4] = 2

  laInvTmp[10,1]   = "Adj1"
  laInvTmp[10, 2]  = 'N'
  laInvTmp[10 , 3] = 8
  laInvTmp[10, 4] = 0

  laInvTmp[11,1]   = "Adj2"
  laInvTmp[11, 2]  = 'N'
  laInvTmp[11 , 3] = 8
  laInvTmp[11, 4] = 0

  laInvTmp[12,1]   = "Adj3"
  laInvTmp[12, 2]  = 'N'
  laInvTmp[12 , 3] = 8
  laInvTmp[12, 4] = 0

  laInvTmp[13,1]   = "Adj4"
  laInvTmp[13, 2]  = 'N'
  laInvTmp[13 , 3] = 8
  laInvTmp[13, 4] = 0

  laInvTmp[14,1]   = "Adj5"
  laInvTmp[14, 2]  = 'N'
  laInvTmp[14 , 3] = 8
  laInvTmp[14, 4] = 0

  laInvTmp[15,1]   = "Adj6"
  laInvTmp[15, 2]  = 'N'
  laInvTmp[15 , 3] = 8
  laInvTmp[15, 4] = 0

  laInvTmp[16,1]   = "Adj7"
  laInvTmp[16, 2]  = 'N'
  laInvTmp[16 , 3] = 8
  laInvTmp[16, 4] = 0

  laInvTmp[17,1]   = "Adj8"
  laInvTmp[17, 2]  = 'N'
  laInvTmp[17 , 3] = 8
  laInvTmp[17, 4] = 0

  laInvTmp[18,1]   = "TotAdj"
  laInvTmp[18, 2]  = 'N'
  laInvTmp[18 , 3] = 10
  laInvTmp[18 , 4] = 0

  laInvTmp[19,1]   = "OldQty1"
  laInvTmp[19, 2]  = 'N'
  laInvTmp[19 , 3] = 8
  laInvTmp[19, 4] = 0

  laInvTmp[20,1]   = "OldQty2"
  laInvTmp[20, 2]  = 'N'
  laInvTmp[20 , 3] = 8
  laInvTmp[20, 4] = 0

  laInvTmp[21,1]   = "OldQty3"
  laInvTmp[21, 2]  = 'N'
  laInvTmp[21 , 3] = 8
  laInvTmp[21, 4] = 0

  laInvTmp[22,1]   = "OldQty4"
  laInvTmp[22, 2]  = 'N'
  laInvTmp[22 , 3] = 8
  laInvTmp[22, 4] = 0

  laInvTmp[23,1]   = "OldQty5"
  laInvTmp[23, 2]  = 'N'
  laInvTmp[23 , 3] = 8
  laInvTmp[23, 4] = 0

  laInvTmp[24,1]   = "OldQty6"
  laInvTmp[24, 2]  = 'N'
  laInvTmp[24 , 3] = 8
  laInvTmp[24, 4] = 0

  laInvTmp[25,1]   = "OldQty7"
  laInvTmp[25, 2]  = 'N'
  laInvTmp[25 , 3] = 8
  laInvTmp[25, 4] = 0

  laInvTmp[26,1]   = "OldQty8"
  laInvTmp[26, 2]  = 'N'
  laInvTmp[26 , 3] = 8
  laInvTmp[26, 4] = 0

  laInvTmp[27,1]   = "TotOld"
  laInvTmp[27, 2]  = 'N'
  laInvTmp[27 , 3] = 10
  laInvTmp[27 , 4] = 0

  laInvTmp[28,1]   = "nOldTo1"
  laInvTmp[28, 2]  = 'N'
  laInvTmp[28 , 3] = 8
  laInvTmp[28, 4] = 0

  laInvTmp[29,1]   = "nOldTo2"
  laInvTmp[29, 2]  = 'N'
  laInvTmp[29 , 3] = 8
  laInvTmp[29, 4] = 0

  laInvTmp[30,1]   = "nOldTo3"
  laInvTmp[30, 2]  = 'N'
  laInvTmp[30 , 3] = 8
  laInvTmp[30, 4] = 0

  laInvTmp[31,1]   = "nOldTo4"
  laInvTmp[31, 2]  = 'N'
  laInvTmp[31 , 3] = 8
  laInvTmp[31, 4] = 0

  laInvTmp[32,1]   = "nOldTo5"
  laInvTmp[32, 2]  = 'N'
  laInvTmp[32 , 3] = 8
  laInvTmp[32, 4] = 0

  laInvTmp[33,1]   = "nOldTo6"
  laInvTmp[33, 2]  = 'N'
  laInvTmp[33 , 3] = 8
  laInvTmp[33, 4] = 0

  laInvTmp[34,1]   = "nOldTo7"
  laInvTmp[34, 2]  = 'N'
  laInvTmp[34 , 3] = 8
  laInvTmp[34, 4] = 0

  laInvTmp[35,1]   = "nOldTo8"
  laInvTmp[35, 2]  = 'N'
  laInvTmp[35 , 3] = 8
  laInvTmp[35, 4] = 0

  laInvTmp[36,1]   = "nTotOldTo"
  laInvTmp[36, 2]  = 'N'
  laInvTmp[36 , 3] = 10
  laInvTmp[36 , 4] = 0

  laInvTmp[37,1]   = "Link_Code"
  laInvTmp[37, 2]  = 'C'
  laInvTmp[37 , 3] = 6
  laInvTmp[37, 4] = 0

  laInvTmp[38,1]   = "glFYear"
  laInvTmp[38, 2]  = 'C'
  laInvTmp[38 , 3] = 4
  laInvTmp[38 , 4] = 0

  laInvTmp[39,1]   = "glPeriod"
  laInvTmp[39, 2]  = 'C'
  laInvTmp[39 , 3] = 2
  laInvTmp[39 , 4] = 0

  laInvTmp[40,1]   = "cFromWare"
  laInvTmp[40, 2]  = 'C'
  laInvTmp[40 , 3] = 6
  laInvTmp[40 , 4] = 0

  laInvTmp[41,1]   = "cToWare"
  laInvTmp[41, 2]  = 'C'
  laInvTmp[41 , 3] = 10
  laInvTmp[41 , 4] = 0

  laInvTmp[42,1]   = "cOwner"
  laInvTmp[42, 2]  = 'C'
  laInvTmp[42 , 3] = 16
  laInvTmp[42 , 4] = 0

  laInvTmp[43,1]   = "Cdivision"
  laInvTmp[43,2]  = 'C'
  laInvTmp[43,3] = 6
  laInvTmp[43,4] = 0

  laInvTmp[44,1]   = "Stotcost"
  laInvTmp[44,2]   = 'N'
  laInvTmp[44,3]   = 13
  laInvTmp[44,4]   = 2

  laInvTmp[45,1]   = "Save_cost"
  laInvTmp[45,2]   = 'N'
  laInvTmp[45,3]   = 13
  laInvTmp[45,4]   = 2

  laInvTmp[46,1]   = "Scale"
  laInvTmp[46,2]  = 'C'
  laInvTmp[46,3] = 3
  laInvTmp[46,4] = 0

  FOR lnLoop = 1 TO  46
    STORE ' ' TO  laInvTmp[lnLoop,7],laInvTmp[lnLoop,8],;
      laInvTmp[lnLoop,9],laInvTmp[lnLoop,10],;
      laInvTmp[lnLoop,11],laInvTmp[lnLoop,12],;
      laInvTmp[lnLoop,13],laInvTmp[lnLoop,14],;
      laInvTmp[lnLoop,15],laInvTmp[lnLoop,16]
    STORE 0 TO    laInvTmp[lnLoop,17] ,laInvTmp[lnLoop,18]
  ENDFOR
  *--Create temp. file
  =gfCrtTmp(lcInvTmp,@laInvTmp,"STYLE",lcInvTmp,.T.)
  *--lcInvtAdj
  =gfCrtTmp(lcInvtAdj,@laInvTmp,"STYLE",lcInvtAdj,.T.)
  *!*************************************************************
  *! Name      : lfMakeIndex
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 01/03/05
  *! Purpose   : function to make index on a temp. file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMakeIndex
  PARAMETERS lcTempName
  PRIVATE laIndex
  DIMENSION laIndex[1,2]

  lcCursor = lcTempName
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

  *!*************************************************************
  *! Name      : lfCrtindex
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 12/27/04
  *! Purpose   : function to Set the index for the SQL files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfCrtindex

  LPARAMETERS lcTable
  DO CASE
      *--scale
    CASE UPPER(lcTable) =  lcScale
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+Scale'
      laIndex[1,2] = lcScale


      *--temp. Customer File
    CASE UPPER(lcTable) = lcStyleFile
      DIMENSION laIndex[4,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

      laIndex[4,1] = 'Season'
      laIndex[4,2] = 'Season'

      laIndex[2,1] = 'CDivision'
      laIndex[2,2] = 'Division'

      laIndex[3,1] = 'cStyGroup'
      laIndex[3,2] = 'cStyGroup'

    CASE UPPER(lcTable) =  lcInvtAdj
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'STYLE'
      laIndex[1,2] = lcInvtAdj

    CASE UPPER(lcTable) =  lcStyjl
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'STYLE'
      laIndex[1,2] = lcStyjl

    CASE UPPER(lcTable) =  lcStyInvJl
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'STYLE'
      laIndex[1,2] = lcStyInvJl

    CASE UPPER(lcTable) =  lcStyleFnl
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'STYLE'
      laIndex[1,2] = lcStyleFnl

    CASE UPPER(lcTable) =  lcFnlInvtAdj
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'STYLE'
      laIndex[1,2] = lcFnlInvtAdj

  ENDCASE
  *!*************************************************************
  *! Name      : lfCollectData
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 12/27/04
  *! Purpose   : function to Set the index for the SQL files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfCollectData

  *--CHECK first for selected styles and its properties [Start]
  *--style major
  llSelectStyle = .F.
  =lfCreateTemp()

  *B131829,1 WSH 04/19/2006 [Start]
  IF llFromScreen
    LOCAL lnTotStk
    lnTotStk = 0

    SELECT (lcScreenTemp)
    SCAN
      SCATTER MEMO MEMVAR
      SELECT(lcInvtAdj)
      APPEND BLANK
      GATHER MEMO MEMVAR

      IF EMPTY(CFROMWARE) AND EMPTY(Dyelot)
        loDBFStyle.SEEK(EVALUATE(lcInvTmp+'.Style'))
        SELECT (lcTempStyle)
      ELSE

        *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[Start]
        IF TYPE('loDBFStyDye') <> 'O'
          loDBFStyDye = CREATEOBJECT("RemoteTable","StyDye","StyDye",lcTempStyDye,SET("DATASESSION"))
        ENDIF
        *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[End]

        loDBFStyDye.SEEK(EVALUATE(lcInvTmp+'.Style')+EVALUATE(lcInvTmp+'.cFromWare')+EVALUATE(lcInvTmp+'.Dyelot'))
        SELECT (lcTempStyDye)
      ENDIF

      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
      lnTotStk = TotStk

      *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[Start]
      *SELECT (lcTmpAdj)
      SELECT (lcInvtAdj)
      *B607853,1 MMT 12/03/2006	Error while printing report after saving phyiscal inventory[End]

      GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4,OldQty5,OldQty6,OldQty7,OldQty8
      REPLACE TOTOLD WITH lnTotStk
    ENDSCAN

    lcFnlInvtAdj = lcInvtAdj

    SELECT(lcFnlInvtAdj)
    LOCATE
    DO WHILE !EOF()
      =lfAPTAppen()
    ENDDO

    RETURN
  ENDIF
  *B131829,1 WSH 04/19/2006 [End]

  lnPosStyle = ASCAN(loOGScroll.laOGFxFlt,"STYLE.CSTYMAJOR")
  IF lnPosStyle > 0
    lnPosStyle = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosStyle,1)
    lcCursorStyle= loOGScroll.laOGFxFlt[lnPosStyle,6]
    IF !EMPTY(lcCursorStyle)
      SELECT(lcCursorStyle)
      LOCATE
      IF !EOF()
        llSelectStyle = .T.
        SCAN
          loDBFStyle.SEEK(TRIM(&lcCursorStyle..cStyMajor))
          *-- IN CASE OF SQL
          *-- loDBFStyle.Seek(lcInvType + TRIM(&lcCursorStyle..cStyMajor))
          SELECT(lcTempStyle)
          SCAN REST WHILE STYLE = TRIM(&lcCursorStyle..cStyMajor)
            SELECT(lcTempStyle)
            SCATTER MEMVAR MEMO
            IF !SEEK(m.style,lcStyleFile)
              *!*	            SELECT(lcTempStyle)
              *!*	            SCATTER MEMO MEMVAR
              SELECT(lcStyleFile)
              APPEND BLANK
              GATHER MEMO MEMVAR
            ENDIF
          ENDSCAN
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
  IF !llSelectStyle
    IF loDBFStyle.llnative
      *--    lcSelcFldsStyle = lcTempStyle+".*"
      lcSelcFldsStyle = "Distinct "+lcTempStyle+".STYLE"&&,"+lcTempStyle+".Season,"+lcTempStyle+".cDivision,"+lcTempStyle+".cStyGroup"
      lcSelcFilsStyle = lcTempStyle
      lcSelcWherStyle = ''
    ELSE
      lcSelcFldsStyle = "ITEM.CstyMajor,ITEM.TotCost,ITEM.Ave_Cost,ITEM.cStyGroup,ITEM.Season,ITEM.Cdivision"
      lcSelcFilsStyle = "ITEM"
      lcSelcWherStyle = "cInvType = "+lcInvType
      *--when style File convreted to sql
    ENDIF
  ELSE
    */  lcSelcFldsStyle = lcStyleFile+".Style"
    lcSelcFldsStyle = lcStyleFile+".STYLE,"+lcStyleFile+".Season,"+lcStyleFile+".cDivision,"+lcStyleFile+".cStyGroup"
    lcSelcFilsStyle = lcStyleFile
    lcSelcWherStyle = ""
  ENDIF
  lcScanExp = ".T."
  *-----------
  *--Style Color
  lnPosStyColor = ASCAN(loOGScroll.laOGFxFlt,"SUBSTR(INVTADJ.STYLE,lnNonMajSt,lnColorLen)")
  IF lnPosStyColor > 0
    lnPosStyColor = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosStyColor,1)
    lcStyleColor = loOGScroll.laOGFxFlt[lnPosStyColor,6]
    IF !EMPTY(lcStyleColor)
      lcStyColorCursor = loOGScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='cStyColor'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= lnColorLen
      laTempacstru[1,4]= 0
      =gfCrtTmp(lcStyColorCursor,@laTempacstru,"cStyColor",lcStyColorCursor,.T.)
      lnStart=1
      lnEnd=AT('|',lcStyleColor)
      DO WHILE lnEnd <> 0
        SELECT(lcStyColorCursor)
        APPEND BLANK
        REPLACE cStyColor WITH SUBSTR(lcStyleColor,lnStart,lnEnd-1)
        lcStyleColor = STUFF(lcStyleColor ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcStyleColor)
      ENDDO
      IF lnEnd = 0
        SELECT(lcStyColorCursor)
        APPEND BLANK
        REPLACE cStyColor WITH lcStyleColor
      ENDIF
      IF !llSelectStyle
        IF loDBFStyle.llnative
          lcSelcFilsStyle = lcSelcFilsStyle + "," + lcStyColorCursor
          lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+"SUBSTR(&lcTempStyle..STYLE,"+ALLTRIM(STR(lnNonMajSt))+","+ALLTRIM(STR(lnColorLen))+") = "+lcStyColorCursor+".cStyColor"
          * lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+"SUBSTR(&lcTempStyle..STYLE,"+ALLTRIM(STR(1))+","+ALLTRIM(STR(lnMajorLen+1))+")+"+lcStyColorCursor+".cStyColor = &lcTempStyle..STYLE"
          lcScanExp = "SEEK(SUBSTR(&lcTempStyle..STYLE,"+ALLTRIM(STR(lnNonMajSt))+"),lcStyColorCursor)"
        ELSE
          SELECT(lcStyColorCursor)
          IF !EOF()&&IF user select styles from the style file
            lcCurName = lcStyColorCursor
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                lcSQLStyleColor = loOGScroll.gfSQLTempName('','cStyColor C(&lnColorLen)',lcCurName,'cStyColor')
                IF EMPTY(lcSQLStyleColor)
                  *-- SQL connection error. can't open the report
                  =gfModalGen('TRM00416B40011','ALERT')
                  RETURN .F.
                ELSE
                  lcSelcFilsStyle = lcSelcFilsStyle + "," + lcSQLStyleColor
                  lcSelcWherStyle = IIF(EMPTY(lcSelcWherStyle),""," AND ")+" SubString(ITEM.Style,lnNonMajSt,lnColorLen) = "+lcSQLStyleColor+".cStyColor"
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        lcSelcFilsStyle = lcSelcFilsStyle + "," + lcStyColorCursor
        lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+"SUBSTR(&lcStyleFile..STYLE,"+ALLTRIM(STR(lnNonMajSt))+","+ALLTRIM(STR(lnColorLen))+")="+lcStyColorCursor+".cStyColor"
      ENDIF
    ENDIF
  ENDIF


  *--Season
  lnPosSeason = ASCAN(loOGScroll.laOGFxFlt,"STYLE.SEASON")
  IF lnPosSeason > 0
    lnPosSeason = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosSeason,1)
    lcSeasons= loOGScroll.laOGFxFlt[lnPosSeason,6]
    IF !EMPTY(lcSeasons)
      lcSeasonCursor = loOGScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='Season'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      =gfCrtTmp(lcSeasonCursor,@laTempacstru,"SEASON",lcSeasonCursor,.T.)
      lnStart=1
      lnEnd=AT('|',lcSeasons)
      DO WHILE lnEnd <> 0
        SELECT(lcSeasonCursor)
        APPEND BLANK
        REPLACE Season WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
        lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcSeasons)
      ENDDO
      IF lnEnd = 0
        SELECT(lcSeasonCursor)
        APPEND BLANK
        REPLACE Season WITH lcSeasons
      ENDIF
      IF !llSelectStyle
        IF loDBFStyle.llnative
          lcSelcFilsStyle = lcSelcFilsStyle + "," + lcSeasonCursor
          lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+lcTempStyle+".SEASON = "+lcSeasonCursor+".SEASON"
          lcScanExp = lcScanExp + " AND " + "SEEK(&lcTempStyle..SEASON,lcSeasonCursor)"
        ELSE
          SELECT(lcSeasonCursor)
          IF !EOF()&&IF user select styles from the style file
            lcCurName = lcSeasonCursor
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                lcSQLStyleSeason = loOGScroll.gfSQLTempName('','Season C(6)',lcCurName,'Season')
                IF EMPTY(lcSQLStyleSeason)
                  *-- SQL connection error. can't open the report
                  =gfModalGen('TRM00416B40011','ALERT')
                  RETURN .F.
                ELSE
                  lcSelcFilsStyle = lcSelcFilsStyle + "," + lcSQLStyleSeason
                  lcSelcWherStyle = IIF(EMPTY(lcSelcWherStyle),""," AND ")+" ITEM.Season = "+lcSQLStyleSeason+".Season"
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        lcSelcFilsStyle = lcSelcFilsStyle + "," + lcSeasonCursor
        lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+lcStyleFile+".SEASON = "+lcSeasonCursor+".SEASON"
      ENDIF
    ENDIF
  ENDIF

  *--Division
  lnPosDivision = ASCAN(loOGScroll.laOGFxFlt,"STYLE.CDIVISION")
  IF lnPosDivision > 0
    lnPosDivision = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosDivision,1)
    lcDivisions = loOGScroll.laOGFxFlt[lnPosDivision,6]
    IF !EMPTY(lcDivisions)
      lcDivCursor = loOGScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='CDIVISION'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      =gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
      lnStart=1
      lnEnd=AT('|',lcDivisions)
      DO WHILE lnEnd <> 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE Cdivision WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
        lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcDivisions)
      ENDDO
      IF lnEnd = 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE Cdivision WITH lcDivisions
      ENDIF
      IF !llSelectStyle
        IF loDBFStyle.llnative
          lcSelcFilsStyle = lcSelcFilsStyle + "," + lcDivCursor
          lcSelcWherStyle =IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+ lcTempStyle+".CDIVISION = "+lcDivCursor+".CDIVISION"
          lcScanExp = lcScanExp + " AND " + "SEEK(&lcTempStyle..CDIVISION,lcDivCursor)"

        ELSE
          SELECT(lcDivCursor)
          IF !EOF()&&IF user select styles from the style file
            lcCurName = lcDivCursor
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                lcSQLStyleDiv = loOGScroll.gfSQLTempName('','Cdivision C(6)',lcCurName,'Cdivision')
                IF EMPTY(lcSQLStyleDiv)
                  *-- SQL connection error. can't open the report
                  =gfModalGen('TRM00416B40011','ALERT')
                  RETURN .F.
                ELSE
                  lcSelcFilsStyle = lcSelcFilsStyle + "," + lcSQLStyleDiv
                  lcSelcWherStyle = IIF(EMPTY(lcSelcWherStyle),""," AND ")+" ITEM.cDivision = "+lcSQLStyleDiv+".cDivision"
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
        lcSelcFilsStyle = lcSelcFilsStyle + "," + lcDivCursor
        lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+lcStyleFile+".CDIVISION= "+lcDivCursor+".CDIVISION"
      ENDIF
    ENDIF
  ENDIF

  *--Style Group
  lnPosStyGrp = ASCAN(loOGScroll.laOGFxFlt,"STYLE.CSTYGROUP")
  IF lnPosStyGrp > 0
    lnPosStyGrp = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosStyGrp,1)
    lcStyGrpExp = loOGScroll.laOGFxFlt[lnPosStyGrp,6]
    IF !EMPTY(lcStyGrpExp)
      lcStyGrpCursor = loOGScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='cStyGroup'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      = gfCrtTmp(lcStyGrpCursor ,@laTempacstru,"cStyGroup",lcStyGrpCursor ,.T.)
      lnStart=1
      lnEnd=AT('|',lcStyGrpExp)
      DO WHILE lnEnd <> 0
        SELECT(lcStyGrpCursor)
        APPEND BLANK
        REPLACE cStyGroup WITH SUBSTR(lcStyGrpExp ,lnStart,lnEnd-1)
        *: B609838,1 MMT 02/22/2012 Stock Adjustment report hangs when user select style group[Start]
        *lcStyGrp = STUFF(lcStyGrpExp,lnStart,lnEnd,"")
        lcStyGrpExp = STUFF(lcStyGrpExp,lnStart,lnEnd,"")
        *: B609838,1 MMT 02/22/2012 Stock Adjustment report hangs when user select style group[END]
        lnEnd=AT('|',lcStyGrpExp)
      ENDDO
      IF lnEnd = 0
        SELECT(lcStyGrpCursor)
        APPEND BLANK
        REPLACE cStyGroup WITH lcStyGrpExp
      ENDIF
      IF !llSelectStyle
        IF loDBFStyle.llnative
          lcSelcFilsStyle = lcSelcFilsStyle + "," + lcStyGrpCursor
          lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle),lcSelcWherStyle +" AND "," ")+lcTempStyle+".cStyGroup = "+lcStyGrpCursor+".cStyGroup"
          lcScanExp = lcScanExp + " AND " + "SEEK(&lcTempStyle..cStyGroup,lcStyGrpCursor)"
        ELSE
          SELECT(lcStyGrpCursor)
          IF !EOF()&&IF user select styles from the style file
            lcCurName = lcStyGrpCursor
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                lcSQLStyleGrp = loOGScroll.gfSQLTempName('','cStyGroup C(6)',lcCurName,'cStyGroup')
                IF EMPTY( lcSQLStyleGrp)
                  *-- SQL connection error. can't open the report
                  =gfModalGen('TRM00416B40011','ALERT')
                  RETURN .F.
                ELSE
                  lcSelcFilsStyle = lcSelcFilsStyle + "," +  lcSQLStyleGrp
                  lcSelcWherStyle = IIF(EMPTY(lcSelcWherStyle),""," AND ")+" ITEM.cStyGroup = "+ lcSQLStyleGrp+".cStyGroup"
                ENDIF
              ENDIF
            ENDIF
          ENDIF

        ENDIF
      ELSE
        lcSelcFilsStyle = lcSelcFilsStyle + "," + lcStyGrpCursor
        lcSelcWherStyle = IIF(!EMPTY(lcSelcWherStyle), lcSelcWherStyle + " AND "," ")+lcStyleFile+".cStyGroup = "+ lcStyGrpCursor +".cStyGroup"
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcSelcWherStyle)
    IF loDBFStyle.llnative
      *! B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037] [Start]
      *!*	      lcSelcWherStyle  =  lcTempStyle+".style = '' AND  " + lcSelcWherStyle
      IF !llSelectStyle
        lcSelcWherStyle  =  lcTempStyle+".style = '' AND  " + lcSelcWherStyle
        SELECT(lcTempStyle)
      ELSE
        lcSelcWherStyle  =  lcStyleFile+".style = '' AND  " + lcSelcWherStyle
        SELECT(lcStyleFile)
      ENDIF
      LOCATE
      *! B611490,1 AHH 11/12/2017 Stock adjustment report doesn't display some styles[T20171204.0037] [End]
      SELECT  &lcSelcFldsStyle FROM &lcSelcFilsStyle WHERE &lcSelcWherStyle INTO CURSOR &lcStyleFnl
    ELSE
      = lfOpenSql(lcSelcFldsStyle,lcSelcFilsStyle ,lcStyleFnl,lcSelcWherStyle )
    ENDIF
  ELSE
    lcStyleFnl = lcSelcFilsStyle
  ENDIF

  lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  llWareHous = .F.
  llDateSelect  = .F.
  lcSeleWare = ""
  lnPosWare = ASCAN(loOGScroll.laOGFxFlt,"INVTADJ.CFROMWARE")
  llDateSelect = !EMPTY(laOGFxFlt[lnDatePos,6])
  ldStartDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))
  ldEndDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21))
  IF lnPosWare > 0
    lnPosWare = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosWare,1)
    lcSeleWare = loOGScroll.laOGFxFlt[lnPosWare,6]
    IF !EMPTY(lcSeleWare)
      llWareHous = .T.
      SELECT(lcSeleWare)
      INDEX ON CWARECODE TAG CWARECODE
    ENDIF
  ENDIF
  *----------------------------
  *-- if user select a selection style critria
  IF lcStyleFnl <> lcTempStyle
    *AND !llNonKeySelected
    *--- LOCKING is not selected
    IF !EMPTY(lcRpInvExp) AND !('L' $ lcRpInvExp)
      SELECT(lcStyleFnl)
      LOCATE
      IF !EOF()
        SCAN
          loDBFInvtAdj.SEEK(&lcStyleFnl..STYLE)
          SELECT(lcTempInvtAdj)
          SCAN REST WHILE STYLE = &lcStyleFnl..STYLE FOR IIF(!EMPTY(lcRpInvExp),TYPE $ lcRpInvExp,.T.)
            SCATTER MEMO MEMVAR
            SELECT(lcInvtAdj)
            APPEND BLANK
            GATHER MEMO MEMVAR
          ENDSCAN
        ENDSCAN
        lcFnlInvtAdj = lfGetInvtAdj(lcInvtAdj,lcFnlInvtAdj)
        SELECT(lcFnlInvtAdj)
        LOCATE
        DO WHILE !EOF()
          =lfAPTAppen()
        ENDDO
      ELSE
        RETURN
        *-- NO MATCHED styles for selected style criteria
        *-- so no record to display
      ENDIF
    ELSE && if locking selected
      SELECT(lcStyleFnl)
      LOCATE
      IF !EOF()
        SCAN
          loDBFInvtAdj.SEEK(&lcStyleFnl..STYLE)
          SELECT(lcTempInvtAdj)
          SCAN REST WHILE STYLE = &lcStyleFnl..STYLE FOR IIF(!EMPTY(lcRpInvExp),TYPE $ lcRpInvExp,.T.)
            SCATTER MEMO MEMVAR
            SELECT(lcInvtAdj)
            APPEND BLANK
            GATHER MEMO MEMVAR
          ENDSCAN
          loDBFStyInvJl.SEEK(&lcStyleFnl..STYLE)
          SELECT(lcTempStyInvJl)
          SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = &lcStyleFnl..STYLE;
              FOR CTRTYPE = '9'  AND IIF(llDateSelect ,BETWEEN(DTRDATE,ldStartDate,ldEndDate),.T.);
              AND IIF(llWareHous,SEEK(CWARECODE,lcSeleWare),.T.)
            SCATTER MEMO MEMVAR
            SELECT(lcStyInvJl)
            APPEND BLANK
            GATHER MEMO MEMVAR
          ENDSCAN
        ENDSCAN
        lcFnlInvtAdj =lfGetInvtAdj(lcInvtAdj,lcFnlInvtAdj)
        SELECT(lcFnlInvtAdj)
        LOCATE
        SELECT(lcStyInvJl)
        SET FILTER TO CTRTYPE = '9'
        LOCATE
        DO WHILE !EOF(lcStyInvJl)
          SELECT(lcFnlInvtAdj)
          IF !EOF()
            IF DATE < &lcStyInvJl..DTRDATE
              =lfAPTAppen()
            ELSE
              =lfLockApen()
            ENDIF
          ELSE
            =lfLockApen()
          ENDIF
        ENDDO
        SELECT(lcFnlInvtAdj)
        DO WHILE !EOF()
          =lfAPTAppen()
        ENDDO
      ELSE
        *-- No records matched the style selection criteria
        RETURN
      ENDIF
    ENDIF
  ELSE
    IF loDBFInvtAdj.llnative
      lcInvtAdj = lcTempInvtAdj

      lcFnlInvtAdj = lfGetInvtAdj(lcInvtAdj,lcFnlInvtAdj)

      IF !EMPTY(lcRpInvExp) AND !('L' $ lcRpInvExp)
        SELECT(lcFnlInvtAdj)
        LOCATE
        DO WHILE !EOF()
          =lfAPTAppen()
        ENDDO
      ELSE
  *B611716,1 Es 2/10/2019 Modify issue "When user does not select Style in Inventory adjustment report, report does not show data." [Start]
        *IF loDBFStyInvJl.llnative
         IF loDBFStyInvJl.SEEK('')
 *B611716,1 Es 2/10/2019 Modify issue "When user does not select Style in Inventory adjustment report, report does not show data." [End]

          lcStyInvJl = lcTempStyInvJl
          SELECT(lcFnlInvtAdj)
          LOCATE
          SELECT(lcStyInvJl)
          LOCATE
          SET FILTER TO CTRTYPE = '9'
          * AND IIF(llWareHous,SEEK(CWARECODE,lcSeleWare),.T.) ;
          * AND IIF(!EMPTY(laOGFxFlt[lnDatePos,6]),BETWEEN(DTRDATE,ldStartDate,ldEndDate),.T.)
          LOCATE
          DO WHILE !EOF(lcStyInvJl)
            IF !EOF(lcFnlInvtAdj)
              SELECT (lcStyInvJl)
              *!*	            IF IIF(llDateSelect ,BETWEEN(&lcStyInvJl..DTRDATE,ldStartDate,ldEndDate),.T.);
              *!*	              AND IIF(llWareHous,SEEK(&lcStyInvJl..CWARECODE,lcSeleWare),.T.)
              IF IIF(llDateSelect ,BETWEEN(DTRDATE,ldStartDate,ldEndDate),.T.);
                  AND IIF(llWareHous,SEEK(CWARECODE,lcSeleWare),.T.)
                SELECT(lcFnlInvtAdj)
                IF DATE < EVALUATE(lcStyInvJl+'.DTRDATE')
                  =lfAPTAppen()
                ELSE
                  =lfLockApen()
                ENDIF
              ELSE
                SELECT(lcStyInvJl)
                SKIP
                LOOP
              ENDIF
            ELSE
              SELECT (lcStyInvJl)
              IF IIF(llDateSelect ,BETWEEN(DTRDATE,ldStartDate,ldEndDate),.T.);
                  AND  IIF(llWareHous,SEEK(CWARECODE,lcSeleWare),.T.)
                SELECT(lcFnlInvtAdj)
                =lfLockApen()
                *B610685,1 TMI 03/03/2014 22:05 [Start] Add else to skip in lcStyInvJl
              ELSE
                SELECT (lcStyInvJl)
                SKIP
                LOOP
                *B610685,1 TMI 03/03/2014 22:05 [End  ]
              ENDIF
            ENDI
          ENDDO
          SELECT(lcFnlInvtAdj)
          DO WHILE !EOF()
            =lfAPTAppen()
          ENDDO
        ENDIF
      ENDIF
    ELSE
      lcSelInvAdj  = "*"
      lcSelCondInv = "ITEMADJ.cInvType" = lcInvType
      lcSelTableInv = "ITEMADJ"
      lnPosWare = ASCAN(loOGScroll.laOGFxFlt,"INVTADJ.CFROMWARE")
      IF lnPosWare > 0
        lnPosWare = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosWare,1)
        lcSeleWare = loOGScroll.laOGFxFlt[lnPosWare,6]
        IF !EMPTY(lcSeleWare)
          *!*          lcFilsInvtAdj =  lcFilsInvtAdj+","+lcSeleWare
          *!*          lcWHRInvtAdj  = lcInvtAdj+".CFROMWARE = "+lcSeleWare+".CFROMWARE"
          SELECT(lcSeleWare)
          LOCATE
          IF !EOF()&&IF user select styles from the style file
            lcCurName = lcSeleWare
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                lcSQLStyleWare = loOGScroll.gfSQLTempName('','CFROMWARE C(6)',lcCurName,'CWARECODE')
                IF EMPTY(lcSQLStyleWare)
                  *-- SQL connection error. can't open the report
                  =gfModalGen('TRM00416B40011','ALERT')
                  RETURN .F.
                ELSE
                  lcSelTableInv =lcSelTableInv + "," +  lcSQLStyleWare
                  lcSelCondInv = IIF(EMPTY(lcSelCondInv ),""," AND ")+" ITEMADJ.CFROMWARE= "+lcSQLStyleWare+".CFROMWARE"
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      *--date filter
      lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
      IF lnDatePos > 0
        lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
        IF !EMPTY(laOGFxFlt[lnDatePos,6])
          lcSelCondInv = IIF(EMPTY(lcSelCondInv ),""," AND ")+"BETWEEN  ITEMADJ.DATE  "+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+" and "+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        ENDIF
      ENDIF
      IF lfOpenSql(lcSelInvAdj ,lcSelcFilsStyle ,lcFnlInvtAdj,lcSelCondInv )
        IF !EMPTY(lcRpInvExp) AND !('L' $ lcRpInvExp)
          SELECT(lcFnlInvtAdj)
          LOCATE
          DO WHILE !EOF()
            =lfAPTAppen()
          ENDDO
        ELSE
          lcSeleStyinvJl ="ITEMINVJL.*"
          lcSelStyInvJFiles = "ITEMINVJL"
          lcseleCondJl = "cInvType ="+lcInvType
          IF lfOpenSql( lcSeleStyinvJl ,lcSelStyInvJFiles ,lcStyInvJl,lcseleCondJl)
            SELECT(lcStyInvJl)
            SET FILTER TO CTRTYPE = '9'
            LOCATE
            DO WHILE !EOF(lcStyInvJl)
              SELECT(lcFnlInvtAdj)
              IF !EOF()
                IF DATE < &lcStyInvJl..DTRDATE
                  =lfAPTAppen()
                ELSE
                  =lfLockApen()
                ENDIF
              ELSE
                =lfLockApen()
              ENDIF
            ENDDO
            SELECT(lcFnlInvtAdj)
            DO WHILE !EOF()
              =lfAPTAppen()
            ENDDO
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  SELECT (lcInvTmp)

  *!*************************************************************
  *! Name      : lfOpenSql
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 09/08/2004
  *! Purpose   : function to open SQL tables
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfOpenSql

  LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
  LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
  PRIVATE laIndex
  DIMENSION laIndex[1,2]

  lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
    'BROWSE',SET("DATASESSION"))

  IF lnConnectionHandlar = 1
    lnBuffering = CURSORGETPROP("Buffering",lcCursor)
    =CURSORSETPROP("Buffering",3,lcCursor)
    *-- To initialize the indecis that will be created for each file
    =lfCrtindex(lcCursor)
    SELECT (lcCursor)
    FOR lnI = 1 TO ALEN(laIndex,1)
      lcIndex = laIndex[lnI,1]
      lcTag   = laIndex[lnI,2]
      INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
    ENDFOR
    lcTag = laIndex[1,2]
    SET ORDER TO TAG (lcTag)

  ELSE
    =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    RETURN .F.
  ENDIF
  *-- end of lfOpenSql.

  *!*************************************************************
  *! Name      : lfLockApen
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 24/08/2000
  *! Purpose   : Function that Append Locking Transaction to tmp file
  *!*************************************************************
  *! Called from : ....
  *!*************************************************************
  *! Calls       : ....
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfLockApen()
  *!*************************************************************
FUNCTION lfLockApenSql

  SELECT (lcInvTmp)
  APPEND BLANK
  REPLACE STYLE     WITH &lcStyjl..STYLE                                              ,;
    DATE      WITH &lcStyjl..DTRDATE                                            ,;
    TYPE      WITH 'L'                                                         ,;
    CSESSION  WITH &lcStyjl..CSESSION                                           ,;
    OLD_COST  WITH IIF(&lcStyjl..NTOTSTK=0,0,&lcStyjl..NSTKVAL/&lcStyjl..NTOTSTK) ,;
    TOTOLD    WITH -&lcStyjl..NTOTSTK                                           ,;
    CFROMWARE WITH &lcStyjl..CWARECODE
  FOR I = 1 TO 8
    Z = STR(I,1)
    REPLACE OLDQTY&Z. WITH -&lcStyjl..NSTK&Z
  ENDFOR
  SELECT(lcStyjl)
  IF !EOF(lcStyjl)
    SKIP
  ENDIF
  SELECT (lcInvTmp)
  REPLACE UNT_COST WITH IIF(&lcStyjl..NTOTSTK=0,0,&lcStyjl..NSTKVAL/&lcStyjl..NTOTSTK) ,;
    TOTADJ   WITH &lcStyjl..NTOTSTK
  FOR I = 1 TO 8
    Z = STR(I,1)
    REPLACE ADJ&Z. WITH &lcStyjl..NSTK&Z
  ENDFOR
  loDBFStyle.SEEK(&lcInvTmp..STYLE)
  SELECT (lcInvTmp)
  REPLACE StotCost  WITH &lcTempStyle..TOTCOST,;
    Save_Cost WITH &lcTempStyle..Ave_Cost,;
    Cdivision WITH &lcTempStyle..Cdivision ,;
    SCALE     WITH &lcTempStyle..SCALE
  loDBFScale.SEEK("S"+&lcTempStyle..SCALE)
  IF !SEEK('S'+&lcTempStyle..SCALE,lcScale)

    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[Start]
    *SELECT(lcScaleTemp)
    SELECT "Scale"
    *: B608744,1 MMT 11/27/2008 Fix bugs of error in style browse in extended size scale[Start]

    SCATTER MEMO MEMVAR
    SELECT(lcScale)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDIF

  SELECT(lcStyjl)
  IF !EOF(lcStyjl)
    SKIP
  ENDIF
  *-- end of lfLockApen.
  *!**************************************************************************
  *! Name      : lfCollTime
  *! Developer : Mariam Mazhar - (MMT)
  *! Date      : 12/14/2004
  *! Purpose   : Calcualte spent time in data collection.
  *!**************************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!**************************************************************************
  *! Returns            : Spent time.
  *!**************************************************************************
  *! Example   : =lfCollTime()
  *!**************************************************************************
FUNCTION lfCollTime
  PARAMETERS lcStart,lcEnd
  lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
  lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
  lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
  lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
  RETURN (lnEnd - lnStart)
  *-- END OF lfCollTime.

  *!**************************************************************************
  *! Name      : lfGetInvtAdj
  *! Developer : Mariam Mazhar - (MMT)
  *! Date      : 12/14/2004
  *! Purpose   : Calcualte spent time in data collection.
  *!**************************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!**************************************************************************
  *! Returns            : Spent time.
  *!**************************************************************************
  *! Example   : =lfCollTime()
  *!**************************************************************************
FUNCTION lfGetInvtAdj
  PARAMETERS lcFileToFilter,lcReturnFile
  lcSelInvtAdj  = " DISTINCT "+lcFileToFilter+".*"
  lcFilsInvtAdj = lcFileToFilter
  lcWHRInvtAdj  = ""

  *--date filter
  lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
  IF lnDatePos > 0
    lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    IF !EMPTY(laOGFxFlt[lnDatePos,6])
      lcWHRInvtAdj  =IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND ","")+"BETWEEN("+lcFileToFilter+".DATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))"
      */ lcWHRInvtAdjFROM  =IIF(!EMPTY(lcWHRInvtAdjFROM),lcWHRInvtAdjFROM+" AND ","")+"BETWEEN(DATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))"
      */lcWHRInvtAdjTO  =IIF(!EMPTY(lcWHRInvtAdjTO),lcWHRInvtAdjTO+" AND "++"BETWEEN(DATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))","")
    ENDIF
  ENDIF

  *--lcfnlInvtAdj INVTADJ.CFROMWARE
  lnPosWare = ASCAN(loOGScroll.laOGFxFlt,"INVTADJ.CFROMWARE")
  IF lnPosWare > 0
    lnPosWare = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosWare,1)
    lcSeleWare = loOGScroll.laOGFxFlt[lnPosWare,6]
    IF !EMPTY(lcSeleWare)
      lcFilsInvtAdj = lcFilsInvtAdj+","+lcSeleWare
      *! B611361,1 HIA 07/13/2013 Error while select location without selecting filtering by date [T20170713.0012][Start]

      *! B610919,1 MMT 12/11/2014 Stock adjustment report deos not filter by date correctly if location is selected[T20141204.0022][Start]
      *lcWHRInvtAdj  = IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND ","")+lcFileToFilter+".CFROMWARE = "+lcSeleWare+".CWARECODE OR "+lcFileToFilter+".cToWare = " + lcSeleWare+".CWARECODE "

      *lcWHRInvtAdj  = IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND (","")+lcFileToFilter+".CFROMWARE = "+lcSeleWare+".CWARECODE OR "+lcFileToFilter+".cToWare = " + lcSeleWare+".CWARECODE )"

      *! B610919,1 MMT 12/11/2014 Stock adjustment report deos not filter by date correctly if location is selected[T20141204.0022][End]
      */    lcWHRInvtAdjTO  = lcFileToFilter+".cToWare = " + +lcSeleWare+".CWARECODE "&& OR cToWare = " + +lcSeleWare+".CWARECODE "
      lcWHRInvtAdj  = IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND (","")+lcFileToFilter+".CFROMWARE = "+lcSeleWare+".CWARECODE OR "+lcFileToFilter+".cToWare = " + lcSeleWare+".CWARECODE "  + IIF(!EMPTY(lcWHRInvtAdj)," ) ","")
      *! B611361,1 HIA 07/13/2013 Error while select location without selecting filtering by date [T20170713.0012][End]


    ENDIF
  ENDIF


  IF !EMPTY(lcWHRInvtAdj)
    IF lcFileToFilter = lcReturnFile
      lcReturnFile = loOGScroll.gfTempName()
    ENDIF
    SELECT  &lcSelInvtAdj FROM &lcFilsInvtAdj WHERE &lcWHRInvtAdj  ORDER BY DATE INTO CURSOR &lcReturnFile
  ELSE
    lcReturnFile = lcFileToFilter
  ENDIF
  RETURN (lcReturnFile)
  *!**************************************************************************
  *! Name      : lfGetStyInvtJl
  *! Developer : Mariam Mazhar - (MMT)
  *! Date      : 12/14/2004
  *! Purpose   : Calcualte spent time in data collection.
  *!**************************************************************************
  *! Passed Parameters  : Start collection date,End collection date
  *!**************************************************************************
  *! Returns            : Spent time.
  *!**************************************************************************
  *! Example   : =lfCollTime()
  *!**************************************************************************
FUNCTION lfGetStyInvtJl
  PARAMETERS lcFileToFilter,lcReturnFile
  lcSelInvtAdj  = " DISTINCT "+lcFileToFilter+".*"
  lcFilsInvtAdj = lcFileToFilter
  lcWHRInvtAdj  = ""

  *--date filter
  lnDatePos = ASCAN(laOGFxFlt,'INVTADJ.DATE')
  IF lnDatePos > 0
    lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
    IF !EMPTY(laOGFxFlt[lnDatePos,6])
      lcWHRInvtAdj  =IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND ","")+"BETWEEN("+lcFileToFilter+".DTRDATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))"
      */ lcWHRInvtAdjFROM  =IIF(!EMPTY(lcWHRInvtAdjFROM),lcWHRInvtAdjFROM+" AND ","")+"BETWEEN(DATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))"
      */lcWHRInvtAdjTO  =IIF(!EMPTY(lcWHRInvtAdjTO),lcWHRInvtAdjTO+" AND "++"BETWEEN(DATE,CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],1,10)+"'),CTOD('"+SUBSTR(laOGFxFlt[lnDatePos,6],12,21)+"'))","")
    ENDIF
  ENDIF

  *--lcfnlInvtAdj INVTADJ.CFROMWARE
  lnPosWare = ASCAN(loOGScroll.laOGFxFlt,"INVTADJ.CFROMWARE")
  IF lnPosWare > 0
    lnPosWare = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPosWare,1)
    lcSeleWare = loOGScroll.laOGFxFlt[lnPosWare,6]
    IF !EMPTY(lcSeleWare)
      lcFilsInvtAdj = lcFilsInvtAdj+","+lcSeleWare
      lcWHRInvtAdj  = IIF(!EMPTY(lcWHRInvtAdj),lcWHRInvtAdj+" AND ","")+lcFileToFilter+".CWARECODE = "+lcSeleWare+".CWARECODE"&& OR "+lcFileToFilter+".cToWare = " + lcSeleWare+".CWARECODE "
      */    lcWHRInvtAdjTO  = lcFileToFilter+".cToWare = " + +lcSeleWare+".CWARECODE "&& OR cToWare = " + +lcSeleWare+".CWARECODE "
    ENDIF
  ENDIF


  IF !EMPTY(lcWHRInvtAdj)
    IF lcFileToFilter = lcReturnFile
      lcReturnFile = loOGScroll.gfTempName()
    ENDIF
    SELECT  &lcSelInvtAdj FROM &lcFilsInvtAdj WHERE &lcWHRInvtAdj  ORDER BY DTRDATE INTO CURSOR &lcReturnFile
  ELSE
    lcReturnFile = lcFileToFilter
  ENDIF
  RETURN (lcReturnFile)
