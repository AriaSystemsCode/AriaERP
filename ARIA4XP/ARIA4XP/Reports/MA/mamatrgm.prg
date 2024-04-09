*!************************************************************************
*! Program file      : MAMATRGM.PRG
*! Program desc.     : Material Requirements Report For GMA.
*! Module            : MATERIALS (MA).
*! Developer         : HFK - Heba Fathi
*! Tracking          : C039146
*! Date              : 03/28/2005         
*!************************************************************************
*! Modifications :
*!*********************************************************************************************************
*!
#INCLUDE 'R:\Aria4XP\Reports\MA\MAMATRGM.h' 

IF loOGScroll.llOGFltCh  
  *-- Defina Variables
  STORE 0 TO lnCountPO
  STORE 0 TO lnOTotRem , lnHTotRem , lnPTotRem , lnOIssued , lnHIssued , lnPIssued
  STORE '' TO lcFDesc , lcClrDsc
  *--Print the fabric openqty by yards for Mexx
  lnfUOM = 1
  *--lcRpExp1  Expresion on Fabric file.
  lnRpExp1 = ASCAN(loOGScroll.laFltExp,'ITEM')
  IF  lnRpExp1 > 0
    lcRpExp1 = ASUBSCRIPT(loOGScroll.laFltExp,lnRpExp1,1)
    lcRpExp1 = IIF(lcRpExp1>0,loOGScroll.laFltExp[lcRpExp1,2],"")
  ELSE
   lcRpExp1 = ""
  ENDIF 
  *--Read the Style major part length and item title.
  lnMajorLn = LEN(lcMjrPct)
  lnNMjrLn  = LEN(lCnMJPct)

  *--Flag for fabric by location or no (Location was entered in grid).
  llByFabLoc= ('lcFabLoc' $ loOGSCroll.lcRpExp)

  *--Create MR temp file with structure.
  =lfCreateFl()

  *- Call function that gets the main data from Sql files
  =lfCollectData()
  SELECT (lcMatReq)
  IF lfNoRecord()
    RETURN
  ENDIF 

  IF llByFabLoc
    =lfDataByFb()
  ENDIF  
  lcRpExp1 = ''
ENDIF

SELECT (lcMatReq)
SET ORDER TO TAG MatReq

IF !lfNoRecord()
  IF !llRpZeroRq 
    SET FILTER TO ReqTotQt<>0 OR IssTotQt<>0
  ENDIF

  IF  lcRpRName ="S"
    DO gfDispRe WITH EVAL('lcRpName')
  ELSE
    =lfPrtToEXl()  
  ENDIF

  RETURN
ENDIF 
*!
*!*************************************************************
*! Name        : lfwRepWhen
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Optional Grid When Function.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfwRepWhen

*--Set needed orders in grid. 
IF !llFrstTime 
  lcTmpFab = loOGScroll.gfTempName()
  lcSelected = " SELECT ITEMLOC.TOTWIP,ITEMLOC.TOTSTK,ITEM.CSTYMAJOR AS FABRIC FROM ITEM (INDEX = CSTYLE)INNER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEM.CDEFWARE = ITEMLOC.CWARECODE "
  lcWhereCondition = " ITEMLOC.DYELOT = '   ' AND ITEM.CINVTYPE = 0002 AND ITEMLOC.CINVTYPE = 0002 " 
  lcSqlStatement = lcSelected + " WHERE " + lcWhereCondition
  lnResult1 = loOGScroll.orda.SqlRun (lcSqlStatement,lcTmpFab,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  IF lnResult1 >= 1
    lnBuffering = CURSORGETPROP("Buffering",lcTmpFab)
    =CURSORSETPROP("Buffering",3,lcTmpFab)
    SELECT (lcTmpFab)
    INDEX ON Fabric TAG &lcTmpFab
    SET ORDER TO TAG &lcTmpFab
  ENDIF 
ENDIF 
*--Temp files names.
lcMatReq   = loOGScroll.gfTempName()
lcTmpItems = loOGScroll.gfTempName()
lcOldValue = ' '
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
=lfGetColor()

RETURN
*!
*!*************************************************************
*! Name        : lfGetColor
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    EXIT
  ENDIF
ENDFOR
RETURN
*!
*!**************************************************************************
*! PROG      : lfDataByFb.PRG   (C#102584)
*! DESC      : Function to get Fabrics Qty which not related to any Cost 
*!           : sheet if has OnOrder or OnHand Or Req.
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfDataByFb

PRIVATE lcOrdMat

SELECT (lcMatreq)
lcOrdMat = SET('ORDER')
SET ORDER TO TAG Matreq

SELECT &lcItemLocS
SCAN 
  *-- if fabric/color is entred befor , ignor it
  IF SEEK(Style,lcMatreq) AND &lcMatreq..lUpdonce
    LOOP
  ENDIF
  IF OnHand = 0 AND OnOrder=0
    LOOP
  ENDIF  
  lnOnHand  = OnHand
  lnOnOrder = OnOrder
  lcItem = SUBSTR(Style,1,lnMajorLn)
  lcIclr = SUBSTR(Style,lnMajorLn+1)
  SELECT (lcMatreq)
  APPEND BLANK  
  REPLACE Item      WITH lcItem,;
          IClr      WITH lcIclr ,;
          Desc      WITH &lcItemLocS..Desc  ,;
          Uom       WITH &lcItemLocS..UomBuy,;
          cWareCode WITH &lcItemLocS..CWARECODE,;
          STKTOTQY  WITH lnOnHand,;
          ONORDQTY  WITH lnOnOrder
ENDSCAN

*!
*!**************************************************************************
*! PROG      : lfUpdStk   (C#102584)
*! DESC      : Function to Update StockQty And OnOrder Qty to 0 becuase it is not repeated
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfUpdStk
PRIVATE lcFbric,lcColor
STORE '' TO lcFbric,lcColor

SELECT(lcMatReq)
SET ORDER TO TAG Matreq
SCAN
  IF (lcFbric <> Item) OR (lcColor<>iclr)
    lcFbric = Item
    lcColor = iclr
    lcPo = PO
    SKIP    
    SCAN REST WHILE (lcFbric = Item) AND (lcColor=iclr)
      REPLACE &lcMatReq..STKTOTQY  WITH 0 ,;
              &lcMatReq..ONORDQTY  WITH 0
      IF lcPo = PO
        REPLACE &lcMatReq..ISSTOTQT  WITH 0 
      ELSE
        lcPo = PO
      ENDIF
    ENDSCAN     
    SKIP-1     
  ENDIF  
ENDSCAN
*!
*!**************************************************************************
*! Name      : lfNoRecord
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Dectect if no records match criteria
*!**************************************************************************
* 
FUNCTION lfNoRecord
GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*!
*!**************************************************************************
*! Name      : lfCollectData
*: Developer : Heba Fathi (HFK)
*! Date      : 03/30/2005
*! Purpose   : Main function to get data
*!**************************************************************************
*
FUNCTION lfCollectData

*--to know location of the fabric
lcFabLoc = ''
lnSelLoc  = 0
lnPosition = ASUBSCRIPT(loOGScroll.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'lcFabLoc'),1)
IF lnPosition > 0
  lcFabLoc = loOGScroll.laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcFabLoc) AND USED(lcFabLoc)
    SELECT (lcFabLoc)
    LOCATE
    lnSelLoc= RECCOUNT()
    LOCATE
  ENDIF  
ENDIF
IF lnSelLoc =  1
  lcWareHos = &lcFabLoc..CWARECODE
ELSE
  lcWareHos = LANG_MaMaTrGm_All
ENDIF

*-- Get data from Sql Files:Poshdr,posln based on conditions
lcSqlSel = " Select PosHdr.PO,PosHdr.Status,PosHdr.Vendor,PosLn.Style,PosLn.Vendor,PosLn.cWareCode,PosLn.TotQty,PosLn.Scale,STR(PosLn.[LineNo],6) As [LineNo]" 
lcSqlSel = lcSqlSel + " From PosHdr (Index = PosHdr) "
lcSqlSel = lcSqlSel + " Inner Join PosLn (Index = PosLn) On PosHdr.cBusDocu+PosHdr.cstytype+PosHdr.po=PosLn.cBusDocu+PosLn.cStyType+PosLn.Po "
lcSqlSel = lcSqlSel + " Where PosLn.TranCd = '1' And PosHdr.Status Not In ('C','X','S') "

lnRow = ASCAN(loOGScroll.laFltExp,'POSHDR')
IF lnRow > 0
  lnRow = ASUBSCRIPT(loOGScroll.laFltExp,lnRow,1)
  lcPoExp = loOGScroll.laFltExp[lnRow,2]
  IF !EMPTY(lcPoExp)
  lcSqlSel = lcSqlSel + " AND " + lcPoExp
  ENDIF 
ENDIF 

lcPoSql = loOGScroll.gfTempName()
lnPOResult = loOGScroll.oRDA.SqlRun(lcSqlSel,lcPoSql,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
IF lnPOResult > 0
  SELECT &lcPoSql
  IF RecCount() = 0
    RETURN .F.
  ELSE
    lnBuffering = CURSORGETPROP("Buffering","&lcPoSql")
    =CURSORSETPROP("Buffering",3,"&lcPoSql")
    INDEX ON Style TAG &lcPoSql
    lcTmpPos = loOGScroll.gfTempName()
    SELECT DISTINCT Po FROM &lcPoSql INTO CURSOR &lcTmpPos
    lcSqlPOs = loOgScroll.gfSQLTempName('','PO C(6)',lcTmpPos,'PO')
    lcCtktBomSel = " SELECT cTktBom.Item,cTktBom.CutTkt,cTktBom.Issue_Qty FROM cTktBom (Index = cTktBom) INNER Join " + lcSqlPOs + " TmpPO on cTKtBom.CutTkt = TmpPo.PO "
    lcCtktBomSel = lcCtktBomSel + " Where cTktBom.cImTyp = 'I' And Typ = '5' "
    lccTKtBom = loOGScroll.gfTempName()
    lnCtktResult = loOGScroll.orda.SqlRun (lcCtktBomSel,lcCtktBom,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 

    IF lnCtktResult> 0
      SELECT &lcCtktBom
      lnBuffering = CURSORGETPROP("Buffering","&lcCtktBom")
      =CURSORSETPROP("Buffering",3,"&lcCtktBom")
      INDEX ON CutTkt+Item TAG &lcCtktBom
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("SqlRun",lnCtktResult,.T.)
      RETURN .F.
    ENDIF 
  ENDIF 
ELSE 
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnPOResult,.T.)
  RETURN .F.
ENDIF 

lcSqlSel = " Select Item.Style,Item.cStyMajor,Item.[Desc],Uom.cUom_B  " 
lcSqlSel = lcSqlSel + " From Item (Index = Style) Inner Join Uom (INDEX = UOM) ON ITEM.CCONVBUY = UOM.CUOMCODE "
lcSqlSel = lcSqlSel + " Where Item.cInvType = 0002 "

*-get color length
lnColorLen=LEN(gfItemMask("PN","",'0002'))
*- add colors selected to where condition
lcColors = " "
IF !EMPTY(loOgScroll.laOgFxFlt[11,6])
  lcColors = "'"+STRTRAN(loOgScroll.laOgFxFlt[11,6],"|","','")+"'"
ENDIF 

IF !EMPTY(lcColors) 
  lcSqlSel = lcSqlSel + " AND RIGHT(ITEM.STYLE," +ALLTRIM(STR(lnColorLen)) +" ) IN (" + lcColors +")"
ENDIF 

lnRow = ASCAN(loOGScroll.laFltExp,'ITEM')
IF lnRow > 0
  lnRow = ASUBSCRIPT(loOGScroll.laFltExp,lnRow,1)
  lcItemExp = loOGScroll.laFltExp[lnRow,2]
  IF !EMPTY(lcItemExp)
  lcSqlSel = lcSqlSel + " AND " + lcItemExp
  ENDIF 
ENDIF 
lcItemSql = loOGScroll.gfTempName()
lnItemResult = loOGScroll.oRDA.SqlRun(lcSqlSel,lcItemSql,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
IF lnItemResult > 0
  SELECT &lcItemSql
  IF RecCount() = 0
    RETURN .F.
  ELSE
    lnBuffering = CURSORGETPROP("Buffering","&lcItemSql")
    =CURSORSETPROP("Buffering",3,"&lcItemSql")
    INDEX ON Style TAG &lcItemSql
  ENDIF 
ELSE 
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnItemResult,.T.)
  RETURN .F.
ENDIF 

*-- call function to get BOM file selected data
=lfGetBomData()

*-- call function to get BOMLINE file selected data
=lfGetBomLineData()

*- if there are no records in Bom and BomLine then we will not continue
IF llNoBom .AND. llNoBomLn
  SET DEVICE TO SCREEN
  RETURN .T.
ENDIF 

=lfGetScales()
*-*-HFK
IF !llByFabLoc                       && No WH selected , get all stkqty and onorder qty from fabric file
  IF EMPTY(lcItemLocM)
    =lfGetItemLoc('M')
  ENDIF   
ELSE    
  IF EMPTY(lcItemLocS)
    =lfGetItemLoc('S')    
  ENDIF 
ENDIF 
*-*-HFK
lcSelStyles = ""
lnLocCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'lcStyLoc'),1)
lcLocFile = loOGScroll.laOgFxFlt[lnLocCondition,6]
IF !EMPTY(lcLocFile)
  loStyDye =CreateObject("RemoteTable","STYDYE","STYDYE","STYDYER",SET("Datasession"))
  lcSelStyles = loOGScroll.gfTempName()
  SELECT STYLE FROM STYDYER INNER JOIN &lcLocFile ON STYDYER.cWareCode = &lcLocFile..cWareCode INTO DBF oAriaApplication.WorkDir+lcSelStyles+".DBF"
  SELECT &lcSelStyles
  INDEX ON Style TAG &lcSelStyles
ENDIF 

loStyle =CreateObject("RemoteTable","STYLE","STYLE","STYLER",SET("Datasession"))
lnStyleExp = ASCAN(loOGScroll.laFltExp,'STYLE')
lcStyleExp = loOGScroll.laFltExp[ASUBSCRIPT(loOGScroll.laFltExp,lnStyleExp,1),2]

*-check for style group expression.
llGroup = ATC('STYLE.CSTYGROUP',lcStyleExp)>0
IF llGroup
  lnGroupCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYGROUP'),1)
  lcGroupCondition = loOGScroll.laOgFxFlt[lnGroupCondition,6]
  IF !EMPTY(lcGroupCondition)
    lcGroupFile = loOGScroll.gfTempName()
    gfCrtTmp(lcGroupFile,"(cStyGroup C(6))",,"",.F.)
    SELECT &lcGroupFile
    lnSepOccur1 = OCCURS("|",lcGroupCondition)
    IF lnSepOccur1 = 0
      lcGroup = lcGroupCondition
      INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
    ELSE
      FOR lnGroups = 1 TO lnSepOccur1+1
        lcGroup = IIF(lnGroups=1,SUBSTR(lcGroupCondition,1,6),SUBSTR(lcGroupCondition,ATC('|',lcGroupCondition,lnGroups-1)+1,6))
        INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
      ENDFOR
    ENDIF 
    SELECT &lcGroupFile
    INDEX ON cStyGroup TAG &lcGroupFile
  ENDIF 
ENDIF 

*-- Check for season expression.
llSeason = ATC('STYLE.SEASON',lcStyleExp)> 0 
IF llSeason
  lnSeasonCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.SEASON')
  lnSeasonCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnSeasonCondition,1)
  lcSeasonCondition = loOGScroll.laOGFxFlt[lnSeasonCondition,6]
  IF !EMPTY(lcSeasonCondition)
    lcSeasonFile = loOGScroll.gfTempName()
    gfCrtTmp(lcSeasonFile,"(Season C(6))",,"",.F.)
    lnSepOccur1 = OCCURS("|",lcSeasonCondition)
    IF lnSepOccur1 = 0
      lcSeason = lcSeasonCondition
      INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
    ELSE
      FOR lnSeasons = 1 TO lnSepOccur1+1
        lcSeason = IIF(lnSeasons=1,SUBSTR(lcSeasonCondition,1,6),SUBSTR(lcSeasonCondition,ATC('|',lcSeasonCondition,lnSeasons-1)+1,6))
        INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
      ENDFOR
    ENDIF 
    SELECT &lcSeasonFile
    INDEX ON Season TAG &lcSeasonFile
  ENDIF 
ENDIF 

*-- Check for division condition
llDivision = ATC('STYLE.CDIVISION',lcStyleExp)> 0
IF llDivision
  lnDivisionCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.CDIVISION')
  lnDivisionCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDivisionCondition,1)
  lcDivisionCondition = loOGScroll.laOGFxFlt[lnDivisionCondition,6]
  IF !EMPTY(lcDivisionCondition)
    lcDivisionFile = loOGScroll.gfTempName()
    gfCrtTmp(lcDivisionFile,"(Division C(6))",,"",.F.)
    lnSepOccur1 = OCCURS("|",lcDivisionCondition)
    IF lnSepOccur1 = 0
      lcDivision = lcDivisionCondition
      INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
    ELSE
      FOR lnDivisions = 1 TO lnSepOccur1+1
        lcDivision = IIF(lnDivisions=1,SUBSTR(lcDivisionCondition,1,6),SUBSTR(lcDivisionCondition,ATC('|',lcDivisionCondition,lnDivisions-1)+1,6))
        INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
      ENDFOR
    ENDIF 
    SELECT &lcDivisionFile
    INDEX ON Division TAG &lcDivisionFile
  ENDIF 
ENDIF 

*-- Check for Style Major
llStyMajor = ATC('STYLE.CSTYMAJOR',lcStyleExp)> 0
IF llStyMajor
  lnStyMajorCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYMAJOR'),1)
  lcStyMajorFile = loOGScroll.laOgFxFlt[lnStyMajorCnd,6]
  IF !EMPTY(lcStyMajorFile)
    SELECT &lcStyMajorFile
    INDEX ON cStyMajor TAG &lcStyMajorFile
  ENDIF 
ENDIF 

*-- Check for style primary fabric
llPrmFab = ATC('STYLE.FABRIC',lcStyleExp)> 0
IF llPrmFab
  lnPrmFabCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.FABRIC'),1)
  lcPrmFabFile = loOGScroll.laOgFxFlt[lnPrmFabCnd,6]
  IF !EMPTY(lcPrmFabFile)
    SELECT &lcPrmFabFile
    INDEX ON cStyMajor TAG &lcPrmFabFile
  ENDIF 
ENDIF 

*-- Check for style department.
llDept = ATC('STYLE.DEPT',lcStyleExp)> 0
IF llDept
  lnDeptCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.DEPT'),1)
  lcDeptFile = loOGScroll.laOgFxFlt[lnDeptCnd,6]
  IF !EMPTY(lcDeptFile) 
    SELECT &lcDeptFile
    INDEX ON Dept TAG &lcDeptFile
  ENDIF 
ENDIF 

*-- Check for style colors
llColor = ATC('SUBSTR(STYLE.STYLE',lcStyleExp)> 0
IF llColor
  lnColorCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'SUBSTR(STYLE.STYLE'),1)
  lcColorCondition = loOGScroll.laOgFxFlt[lnColorCondition,6]
  IF !EMPTY(lcColorCondition)
    lcColorFile = loOGScroll.gfTempName()
    gfCrtTmp(lcColorFile,"(Color C(6))",,"",.F.)
    SELECT &lcColorFile
    lnSepOccur1 = OCCURS("|",lcColorCondition)
    IF lnSepOccur1 = 0
      lcColor = lcColorCondition
      INSERT INTO &lcColorFile (Color) VALUES (lcColor)
    ELSE
      FOR lnColors = 1 TO lnSepOccur1+1
        lcColor = IIF(lnColors=1,SUBSTR(lcColorCondition,1,6),SUBSTR(lcColorCondition,ATC('|',lcColorCondition,lnColors-1)+1,6))
        INSERT INTO &lcColorFile (Color) VALUES (lcColor)
      ENDFOR
    ENDIF 
    SELECT &lcColorFile
    INDEX ON Color TAG &lcColorFile
  ENDIF 
ENDIF 
lcPatCond = loOGScroll.laOgFxFlt[ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.PATTERN'),1),6]

SELECT StyleR
loStyle.GoTop()
DO WHILE .T.
  lcCStyle = StyleR.Style
  IF !EMPTY(lcSelStyles)
    IF !SEEK(StyleR.Style,lcSelStyles)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 

  IF !EMPTY(lcPatCond)
    IF StyleR.Pattern <> lcPatCond
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 

  IF StyleR.Make = .T. .OR. !(StyleR.Status $ "HA") 
    IF !loStyle.GoNext()
      Exit
    ELSE
      LOOP 
    ENDIF 
  ENDIF 
  IF llGroup
    IF !SEEK(StyleR.cStyGroup,lcGroupFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llSeason
    IF !SEEK(StyleR.Season,lcSeasonFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llDivision
    IF !SEEK(StyleR.cDivision,lcDivisionFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llStyMajor
    IF !SEEK(StyleR.cStyMajor,lcStyMajorFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llPrmFab
    IF !SEEK(StyleR.Fabric,lcPrmFabFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llDept
    IF !SEEK(StyleR.Dept,lcDeptFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF llColor
    IF !SEEK(SUBSTR(StyleR.Style,lnClrSrt,lnclrEnd),lcColorFile)  
      IF !loStyle.GoNext()
        Exit
      ELSE
        LOOP 
      ENDIF 
    ENDIF 
  ENDIF 
  IF SEEK(StyleR.Style,'&lcPoSql')
    SELECT &lcPoSql
    SCAN FOR Style = lcCStyle
      lnCnt    = IIF(SEEK("S"+&lcPoSql..Scale,'&lcScales'), &lcScales..Cnt , 8 )
      IF Status = 'H' .AND. !llNoBom
        lcStyMajor = PADR(SUBSTR(lcCStyle,1,lnMajorLn),19)
        SELECT &lcBomFile
        IF SEEK(lcStyMajor)
          SCAN REST WHILE cItmMajor = lcStyMajor;
                 FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle)
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
            IF llByFabLoc          
              IF !SEEK(lcItem+"-"+lcIClr,lcItemLocS)
                LOOP
              ENDIF 
            ENDIF           
            IF !SEEK(lcItem+"-"+lcIClr,lcItemSql)
              LOOP 
            ENDIF 
            =lfUpdReq()
          ENDSCAN
        ENDIF 
      ELSE
        IF !llNoBomLn
          SELECT &lcBomLine
          SCAN FOR cTktNo+LineNo+Style = &lcPoSql..Po+&lcPoSql..LineNo+lcCStyle
            STORE '' TO lcItem,lcIClr
            IF !lfReadItem()
              LOOP
            ENDIF 
          IF llByFabLoc          
            IF !SEEK(lcItem+"-"+lcIClr,lcItemLocS)
              LOOP
            ENDIF 
          ENDIF           
            
            IF !SEEK(lcItem+"-"+lcIClr,lcItemSql)
              LOOP 
            ENDIF 
            =lfUpdReq()
          ENDSCAN 
        ENDIF 
      ENDIF 
    ENDSCAN 
  ENDIF 
  IF !loStyle.GoNext()
    Exit
  ELSE
    LOOP 
  ENDIF 
ENDDO 
*!
*!**************************************************************************
*! Name      : lfGetScales
*: Developer : Heba Fathi (HFK)
*! Date      : 03/30/2005
*! Purpose   : Get Scales form scale file
*!**************************************************************************
*
FUNCTION lfGetScales
*--Open report needed files.
*- create object from data access class
IF TYPE('loSqlConnection') <> 'O'
  loSqlConnection = CREATEOBJECT('RemoteDataAccess')
ENDIF 

*- Get data from style file.
lcScaleStat = " Select Type,Scale,Cnt From Scale "
lnConnectionHandlar = loSqlConnection.SqlRun(lcScaleStat,lcScales,,oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DATASESSION"))
IF lnConnectionHandlar = 1
  SELECT &lcScales
  IF RECCOUNT() = 0 
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN .F.
  ENDIF 
  lnBuffering = CURSORGETPROP("Buffering","&lcScales")
  =CURSORSETPROP("Buffering",3,"&lcScales")
  *-- To initialize the indices that will be created for each file
  SELECT ("&lcScales")
  INDEX ON TYPE+SCALE TAG &lcScales
  SET ORDER TO TAG &lcScales
  =CURSORSETPROP("Buffering",5,"&lcScales")
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*! 
*!*************************************************************
*! Name        : lfReadItem
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 02/08/2002
*! Purpose     : To read item data
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfReadItem

IF &lcPoSql..Status = "H"
  lcItem = SUBSTR(&lcBomFile..Item,1,lnMajorLn)
  lcIClr = IIF('*' $ &lcBomFile..IClr,SUBSTR(lcCStyle,lnClrSrt,lnClrEnd),&lcBomFile..IClr)
ELSE
  lcItem = SUBSTR(&lcBomLine..Item,1,lnMajorLn)
  lcIClr = &lcBomLine..IClr
ENDIF  
RETURN .T.
*!**************************************************************************
*! Name        : lfUpdReq                                        
*! Developer   : TMI - TAREK MOHAMED IBRAHIM
*! Date        : 03/03/2003
*! Purpose     : Update material requirement file.
*!***************************************************************************
*! Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*!
FUNCTION lfUpdReq

*--get data from cTktBom File.

PRIVATE lcPos
SELECT (lcMatReq)
lnOrder = ORDER()
SET ORDER TO TAG Matreq
lcExp = PADR(lcItem,19)+PADR(lcIClr,6)+&lcPoSql..PO
IF !SEEK('&lcExp','&lcMatReq')
  WAIT WINDOW LANG_MaMaTrGm_MsgClct + &lcPoSql..STYLE NOWAIT
  INSERT INTO &lcMatReq (Style,Typ,Item,IClr);
              VALUES    (&lcPoSql..STYLE,&lcBomLine..cBomTyp,PADR(lcItem,19),PADR(lcIClr,6))
  REPLACE Cnt       WITH IIF(lnCnt=0,8,lnCnt),;
          PO        WITH &lcPoSql..PO,;
          Status    WITH &lcPoSql..Status,;
          cVenCOde  WITH &lcPoSql..vendor,;
          cWareCode WITH &lcPoSql..CWareCode,;
          ISSTOTQT  WITH IIF(SEEK(&lcPoSql..PO+lcItem+'-'+lcIClr,'&lcCtktBom'),&lcCtktBom..Issue_Qty,0),;
          REQTOTQT  WITH REQTOTQT - IIF(&lcPoSql..STATUS='H',&lcBomFile..nBOMTotQty*&lcPoSql..TotQty,&lcBomLine..ITEMQTY)
ELSE
  REPLACE REQTOTQT  WITH REQTOTQT - IIF(&lcPoSql..STATUS='H',&lcBomFile..nBOMTotQty*&lcPoSql..TotQty,&lcBomLine..ITEMQTY)            
ENDIF 

*--The fields "STKTOTQY" , "ONORDQTY" needs to updated once only ( per WH ), so the field lUpdonce is added for this porpous
lcSeekExp = PADR(lcItem,19)+PADR(lcIClr,6)
lcStyleID = PADR(lcItem,lnMajorLn)+'-'+lcIClr
IF SEEK('&lcSeekExp','&lcMatReq') AND !&lcMatReq..lUpdonce
  IF !llByFabLoc                       && No WH selected , get all stkqty and onorder qty from fabric file
    =SEEK(lcStyleID,'&lcItemLocM')
    REPLACE STKTOTQY  WITH &lcItemLocM..OnHand,;
            ONORDQTY  WITH &lcItemLocM..OnOrder,;
            lUpdonce  WITH .T.
  ELSE    
    *-- If some WH are selected , sum the stock and onorder only of the selected WH's only
    SELECT &lcItemLocS
    =SEEK(lcStyleID)
    SCAN REST WHILE Style = lcStyleID
      SELECT (lcMatReq)
      REPLACE STKTOTQY  WITH STKTOTQY + &lcItemLocS..OnHand ,;
              ONORDQTY  WITH ONORDQTY + &lcItemLocS..ONORDER
    ENDSCAN
    SELECT (lcMatReq)
    REPLACE lUpdonce  WITH .T.
  ENDIF
ENDIF          
SELECT (lcMatReq)
SET ORDER TO TAG &lnOrder
*!
*!**********************************************************************
*! Name      : lfGetBomData
*! Developer : Heba Fathi (HFK)
*! Date      : 03/29/2005
*! Purpose   : Get Data From Bom File
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns   : File Contains Bom File data
*!**********************************************************************
*!
FUNCTION lfGetBomData
lcBomSel = " Select Bom.Item,Bom.nBomTotQty,Bom.cItmMajor,Bom.cItmMask,RIGHT(Bom.Item,"+ALLTRIM(STR(lnFabNMLen))+") as IClr" 
lcBomSel = lcBomSel + " From Bom (Index = MULTIBOM ) "
lcBomSel = lcBomSel + " Where Bom.Typ = '5' "
lcBomFile = loOGScroll.gfTempName()
lnResult = loOGScroll.oRDA.SqlRun(lcBomSel,lcBomFile,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
IF lnResult > 0
  SELECT &lcBomFile
  IF RECCOUNT()=0
    llNoBom = .T.
  ELSE
    lnBuffering = CURSORGETPROP("Buffering","&lcBomFile")
    =CURSORSETPROP("Buffering",3,"&lcBomFile")
    INDEX ON cItmMajor TAG &lcBomFile
  ENDIF 
ELSE 
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
  RETURN .F.
ENDIF 
*!
*!**********************************************************************
*! Name      : lfGetBomLineData
*! Developer : Heba Fathi (HFK)
*! Date      : 03/29/2005
*! Purpose   : Get BomLine File Data
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns   : File contains bomline file data
*!**********************************************************************
*!
FUNCTION lfGetBomLineData

lcBomLineSel = " Select BomLine.Item,BomLine.ItemQty,BomLine.cBomTyp,RIGHT(BomLine.Item,"+ALLTRIM(STR(lnFabNMLen))+") As IClr,BomLine.cTktNo," 
lcBomLineSel = lcBomLineSel + " STR(BomLine.[LineNo],6) As [LineNo],BomLine.Style  From BomLine (Index = BomLine) "
lcBomLineSel = lcBomLineSel + " Where BomLine.cImTyp = 'I' And BomLine.cType = '1' And BomLine.cBomTyp = '5'  "
*-And BomLine.cCatgTyp = 'F'
lcBomLine = loOGScroll.gfTempName()
lnResult = loOGScroll.oRDA.SqlRun(lcBomLineSel,lcBomLine,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
IF lnResult > 0
  SELECT &lcBomLine
  IF RECCOUNT()=0
    llNoBomLn = .T.
  ELSE
    lnBuffering = CURSORGETPROP("Buffering","&lcBomLine")
    =CURSORSETPROP("Buffering",3,"&lcBomLine")
    INDEX ON Item TAG &lcBomLine
  ENDIF 
ELSE 
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
  RETURN .F.
ENDIF 
*!
*!**********************************************************************
*! Name      : lfGetItemLoc
*! Developer : Heba Fathi (HFK)
*! Date      : 03/29/2005
*! Purpose   : Get data from Itemloc file
*!**********************************************************************
*! Passed Parameters  : 'M' - No specific warehouses selected,collect for all
*!                      'S' - some warehouses selected, collect only for them
*!**********************************************************************
*! Returns   : File contains ItemLoc file data
*!**********************************************************************
*!
FUNCTION lfGetItemLoc
PARAMETERS lcGet
lnAlias = SELECT()

IF lcGet = 'M'
  lcItemLocM = loOGScroll.gfTempName()
  lcItemLocSel = " Select ItemLoc.Style,SUM(ItemLoc.TotStk) As OnHand,SUM(ItemLoc.TotWip) As OnOrder  From ItemLoc (Index = StyDye) "
  IF !EMPTY(lcRpExp1) .OR. !EMPTY(lcColors)
    lcItemLocSel = lcItemLocSel + " Inner Join Item (Index = Style) On Item.Style = ItemLoc.Style "
  ENDIF 
 
  lcItemLocSel = lcItemLocSel + " Where ItemLoc.cInvType = '0002' And ItemLoc.DyeLot = ' ' "
  lcColors = " "
  IF !EMPTY(loOgScroll.laOgFxFlt[11,6])
    lcColors = "'"+STRTRAN(loOgScroll.laOgFxFlt[11,6],"|","','")+"'"
  ENDIF 
  IF !EMPTY(lcColors) 
    lcItemLocSel = lcItemLocSel + " AND RIGHT(ITEM.STYLE," +ALLTRIM(STR(lnColorLen)) +" ) IN (" + lcColors +")"
  ENDIF   
  lcItemLocSel = lcItemLocSel + " Group By ItemLoc.Style Order By ItemLoc.Style "
  lnItemLocResult = loOGScroll.oRDA.SqlRun(lcItemLocSel,lcItemLocM,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnItemLocResult > 0
    SELECT &lcItemLocM
    IF lfNoRecord()
      RETURN .F.
    ELSE
      lnBuffering = CURSORGETPROP("Buffering","&lcItemLocM")
      =CURSORSETPROP("Buffering",3,"&lcItemLocM")
      INDEX ON Style TAG &lcItemLocM
    ENDIF 
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("SqlRun",lnItemLocResult,.T.)
    RETURN .F.
  ENDIF 
ELSE
  lcItemLocS = loOGScroll.gfTempName()
  lcLocations = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'lcFabLoc'),1)
  lcLocFile = loOGScroll.laOgFxFlt[lcLocations,6]
  lcSqlLocs = loOgScroll.gfSQLTempName('','WareCode C(6)',lcLocFile,'CWARECODE')
  lcItemLocSel = " Select ItemLoc.Style,ItemLoc.cWareCode,ItemLoc.TotStk As OnHand,ItemLoc.TotWip As OnOrder, "
  lcItemLocSel = lcItemLocSel + "Item.[Desc],Uom.cUom_B As UomBuy From ItemLoc (Index = StyDye)"
  lcItemLocSel = lcItemLocSel + " Inner Join Item (Index = Style) On Item.Style = ItemLoc.Style "
  lcItemLocSel = lcItemLocSel + " Inner Join Uom (INDEX = UOM) ON ITEM.CCONVBUY = UOM.CUOMCODE "
  lcItemLocSel = lcItemLocSel + " INNER Join " + lcSqlLocs + " TmpLocations On ItemLoc.cWareCode = TmpLocations.WareCode "  
  lcItemLocSel = lcItemLocSel + " Where ItemLoc.cInvType = '0002' And ItemLoc.DyeLot = ' ' And Item.cInvType = '0002'  "
  IF !EMPTY(lcRpExp1)
    lcItemLocSel = lcItemLocSel + " And " + lcRpExp1
  ENDIF 

  lcColors = " "
  IF !EMPTY(loOgScroll.laOgFxFlt[11,6])
    lcColors = "'"+STRTRAN(loOgScroll.laOgFxFlt[11,6],"|","','")+"'"
  ENDIF 

  IF !EMPTY(lcColors) 
    lcItemLocSel = lcItemLocSel + " AND RIGHT(ITEM.STYLE," +ALLTRIM(STR(lnColorLen)) +" ) IN (" + lcColors +")"
  ENDIF 
  lcItemLocSel = lcItemLocSel + "  Order By ItemLoc.Style "
  lnItemLocResult = loOGScroll.oRDA.SqlRun(lcItemLocSel,lcItemLocS,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnItemLocResult > 0
    SELECT &lcItemLocS
    IF RecCount() = 0
      RETURN .F.
    ELSE
      lnBuffering = CURSORGETPROP("Buffering","&lcItemLocS")
      =CURSORSETPROP("Buffering",3,"&lcItemLocS")
      INDEX ON Style+cWareCode TAG &lcItemLocS
    ENDIF 
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("SqlRun",lnItemLocResult,.T.)
    RETURN .F.
  ENDIF 
ENDIF 
SELECT (lnAlias)
*-------------------* 2- Functions For Printing to Excel *-------------------*
*!
*!**************************************************************************
*! PROG      : lfPrtToEXl.PRG   (C#102584)
*! DESC      : Function to print detail to Excel sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfPrtToEXl

IF loOGScroll.llOGFltCh
  *-- Create temp file to get All PO's , vendors and status
  gfCrtTmp(lcPoTmpFil,"( PO C(6), Vendor C(8),Status C(1) , TotIssu N(14,2),TotReq N(14,2))","PO",lcPoTmpFil,.F.)
  lnCountPO = 0
  lcPoNo = '      '
  
  SELECT(lcMatReq)
  SET ORDER TO TAg MrPOSty
  SCAN
    *--Check for not exceed 256 Column 
    IF lnCountPO >123
        =gfModalGen("INM00000B42001","Dialog",.F.,.F.,LANG_MaMaTrGm_Notify)
      EXIT
    ENDIF    
    IF (PO <> lcPoNo) AND !SEEK(PO,lcPoTmpFil)
      lnCountPO = lnCountPO+1
      lcPoNo = PO

      SELECT(lcPoTmpFil)
      APPEND BLANK
      REPLACE PO      WITH lcPoNo,;
              Vendor  WITH &lcMatReq..cVenCode,;
              Status  WITH &lcMatReq..status
    ENDIF  
  ENDSCAN

  *--function to prep. temp file to print from
  =lfPrepTemp()
  =lfUpdatQty()
ENDIF
=lfPrint()
*!
*!**************************************************************************
*! PROG      : lfUpdatQty.PRG   (C#102584)
*! DESC      : Function to Update qty in new tempfile for detail option
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfUpdatQty
PRIVATE lcFbric,lcColor
PRIVATE lnTotStok,lnTotBalan,lnTotOnOrd,lnTotAval
STORE '' TO lcFbric,lcColor
STORE 0 TO lnTotStok,lnTotBalan,lnTotOnOrd,lnTotAval

SELECT(lcMatReq)
SET ORDER TO TAG Matreq
SCAN
  IF (lcFbric <> Item) OR (lcColor<>Iclr)    && this is a new fabric/color
    lcFbric = Item
    lcColor = Iclr
    lnCount = 1
    lcCount = ALLTRIM(STR(lnCount,5))
    lcPo = '      '
    lnTotReq&lcCount = 0
    lnTotIss&lcCount = 0

    SELECT(lcTmpDtl)
    APPEND BLANK
    *-- nStock and nOnOrder will be updated incrementally in the loop below
    REPLACE FabrColr  WITH ALLTRIM(&lcMatReq..ITEM)+'  '+ALLTRIM(&lcMatReq..ICLR)           
    lnTotStok = lnTotStok +&lcTmpDtl..NSTOCK
    lnTotOnOrd = lnTotOnOrd + &lcTmpDtl..NONORDER
    
    lnCount = 0
    SELECT(lcMatReq)
    SCAN REST WHILE (lcFbric = Item) AND (lcColor=iclr)
      IF lcPo <> PO
        lnCount = lnCount  +1
        lcCount = ALLTRIM(STR(lnCount,5))

        IF !SEEK(PO,lcPoTmpFil)
          LOOP
        ENDIF
        lcPo = PO
      ENDIF
      IF lnCountPO <> 0
      *-hfk, 05/04/2005, modify equation to calculate balance and available for B127641
*!*          REPLACE &lcTmpDtl..NBALANCE WITH &lcTmpDtl..NBALANCE +&lcTmpDtl..NSTOCK + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
*!*                  &lcTmpDtl..NAVALABL WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.ONORDQTY')+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT') 
*!*          REPLACE &lcTmpDtl..NBALANCE WITH &lcTmpDtl..NBALANCE +&lcTmpDtl..NSTOCK + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
*!*                  &lcTmpDtl..NAVALABL WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.ONORDQTY')+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT') 
*!*          REPLACE &lcTmpDtl..NBALANCE WITH &lcTmpDtl..NBALANCE+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT'),;
*!*                  &lcTmpDtl..NAVALABL WITH &lcTmpDtl..NAVALABL+EVAL(lcMatReq+'.STKTOTQY')+EVAL(lcMatReq+'.REQTOTQT')+EVAL(lcMatReq+'.ISSTOTQT')+EVAL(lcMatReq+'.ONORDQTY') 

*!*          *-- update nStock and nOnOrder
*!*          REPLACE &lcTmpDtl..nStock   WITH &lcTmpDtl..nStock   + &lcMatReq..STKTOTQY,;
*!*                  &lcTmpDtl..NONORDER WITH &lcTmpDtl..NONORDER + &lcMatReq..ONORDQTY
        *-- update nStock and nOnOrder

        REPLACE &lcTmpDtl..NBALANCE WITH &lcTmpDtl..NBALANCE +&lcMatReq..STKTOTQY + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT,;
                &lcTmpDtl..NAVALABL WITH &lcTmpDtl..NAVALABL +&lcMatReq..ONORDQTY+&lcMatReq..STKTOTQY + &lcMatReq..REQTOTQT + &lcMatReq..ISSTOTQT

        REPLACE &lcTmpDtl..nStock   WITH &lcTmpDtl..nStock   + &lcMatReq..STKTOTQY,;
                &lcTmpDtl..NONORDER WITH &lcTmpDtl..NONORDER + &lcMatReq..ONORDQTY

        *-hfk

        
        IF !EMPTY(ALLTRIM(lcPO))        
          REPLACE &lcTmpDtl..NREQ&lcPO WITH &lcMatReq..REQTOTQT + &lcTmpDtl..NREQ&lcPo ,;
                  &lcTmpDtl..NISS&lcPO WITH &lcMatReq..ISSTOTQT +&lcTmpDtl..NISS&lcPo 

          REPLACE &lcPoTmpFil..TotReq  WITH &lcPoTmpFil..TotReq  + &lcMatReq..REQTOTQT
          REPLACE &lcPoTmpFil..TotIssu WITH &lcPoTmpFil..TotIssu  + &lcMatReq..ISSTOTQT
        ENDIF  
      ENDIF  
    ENDSCAN     
    lnTotBalan = lnTotBalan + &lcTmpDtl..NBALANCE
    lnTotAval  = lnTotAval  + &lcTmpDtl..NAVALABL
    SKIP-1     
  ENDIF  
  ENDSCAN
SELECT (lcTmpDtl)
APPEND BLANK
REPLACE FabrColr  WITH LANG_MaMaTrGm_Total,;
        NSTOCK    WITH lnTotStok,;
        NONORDER  WITH lnTotOnOrd ,;
        NBALANCE  WITH lnTotBalan,;
        NAVALABL  WITH lnTotAval  
SELECT(lcPoTmpFil)        
lnCount = 0
SCAN
  lcPo = &lcPoTmpFil..PO
  lnCount = 1 +lnCount
  lcCount = ALLTRIM(STR(lnCount,5))
  
  IF !EMPTY(ALLTRIM(lcPO))        
    REPLACE &lcTmpDtl..NREQ&lcPo   WITH &lcPoTmpFil..TotReq+&lcTmpDtl..NREQ&lcPo ,;
            &lcTmpDtl..NISS&lcPo   WITH &lcPoTmpFil..TotIssu+&lcTmpDtl..NISS&lcPo
  ENDIF
ENDSCAN
*!**************************************************************************
*! PROG      : lfPrint.PRG   (C#102584)
*! DESC      : Function to print to Excel sheet in detail option
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfPrint

lcRpDtDir  = gcDataDir  && Save The Data Directory.
lcRpWrDir  = gcWorkDir  && Save The Work Directory.
lcRpSysDir = gcSysHome  && Save The system  Directory.
lcRpRpDir  = gcRepHome  && Save The report Directory.
lcRpComp   = gcAct_Comp && Save The Active Company.
lcRpDefPth = oAriaApplication.DefaultPath  && Save The DefPath.
lcRpTmpFl1 = lcTmpDtl   && Save The TempName for the first  printed file.
lcRpTmpFl2 = lcPoTmpFil && Save The TempName for the second printed file.
lcRpPath= oAriaApplication.OutputHome
lcRpDefPth = ALLTRIM(lcRpDefPth)
lcSorCop = ALLTRIM(lcRpDefPth) +'DETAIL.XLS'
lcRpFilNam = 'Template'
lcRpFilNam = lcRpFilNam + ".XLS"
lcTarCop = lcRpPath+lcRpFilNam
lcRpTitle  = lcRpOptTtl && Printed Custom Titel
lcRpWareHs = lcWareHos  && Save The warehouse 

*-- When user press preview twice with no time interval between the two presses 
*-- an error occures while creating the target Excel file,this loop to resolve this situation
PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  COPY FILE &lcSorCop TO  &lcTarCop  
  *-- if error occurs while copying , ask to continue or exit loop
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr

lnLen = LEN(lcRpFilNam)
lcRpFilNam = LEFT(lcRpFilNam,lnLen-4)
SAVE TO (oAriaApplication.WorkDir+lcTempMemo+'.MEM') ALL LIKE l?RP* 
lcCommLine = (oAriaApplication.WorkDir+lcTempMemo+'.MEM')

=lfWriteExl(lcCommLine)

RETURN
*!
*!**************************************************************************
*! PROG      : lfWriteExl
*! DESC      : Function to print to Excel sheet in detail option
*! Developer : Heba Fathi (HFK)
*! Date      : 03/31/2005
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfWriteExl
LPARAMETERS lcMemoFile

IF TYPE('lcMemoFile')#'C'
  RETURN
ENDIF

RESTORE FROM (lcMemoFile) ADDI
ON ERROR DO lfError
LOCAL xlsheet,XLApp,tmpsheet

*-- Open Temp File.
IF !USED(lcRpTmpFl1)
  USE (ALLTRIM(oAriaApplication.WorkDir)+lcRpTmpFl1) ORDER 1 IN 0
ENDIF

IF !USED(lcRpTmpFl2)
  USE (ALLTRIM(oAriaApplication.WorkDir)+lcRpTmpFl2) ORDER 1 IN 0
ENDIF

TmpSheet = GetObject(ALLTRIM(lcRpPath)+lcRpFilNam,'excel.sheet')
XLApp = TmpSheet.application
XLApp.WorkBooks.Add(ALLTRIM(lcRpPath)+lcRpFilNam)
XLApp.Sheets("Detail").Select

*-- First, Write the Headers in the Excel sheet
XLApp.Sheets("Detail").Cells(1,4).Value          = "Material Req. By Vendor"
XLApp.Sheets("Detail").Cells(1,4).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(1,4).Font.Size      = 13
XLApp.Sheets("Detail").Cells(1,4).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(2,5).Value          = "Detail Format"
XLApp.Sheets("Detail").Cells(2,5).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(2,5).Font.Size      = 13
XLApp.Sheets("Detail").Cells(2,5).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(5,1).Value          = "WareHouse"
XLApp.Sheets("Detail").Cells(5,1).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(5,1).Font.Size      = 13
XLApp.Sheets("Detail").Cells(5,1).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(5,2).Value          = lcRpWareHs
XLApp.Sheets("Detail").Cells(5,2).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(5,2).Font.Size      = 13
XLApp.Sheets("Detail").Cells(5,2).Font.FontStyle = "Regular"

XLApp.Sheets("Detail").Cells(3,4).Value          = lcRpTitle
XLApp.Sheets("Detail").Cells(3,4).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(3,4).Font.Size      = 11
XLApp.Sheets("Detail").Cells(3,4).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(6,1).Value          = "Fabric/Color"
XLApp.Sheets("Detail").Cells(6,1).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(6,1).Font.Size      = 11
XLApp.Sheets("Detail").Cells(6,1).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(6,2).Value          = "Stock"
XLApp.Sheets("Detail").Cells(6,2).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(6,2).Font.Size      = 11
XLApp.Sheets("Detail").Cells(6,2).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(7,1).Value          = "PO# ->"
XLApp.Sheets("Detail").Cells(7,1).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(7,1).Font.Size      = 11
XLApp.Sheets("Detail").Cells(7,1).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(8,1).Value          = "Vendor"
XLApp.Sheets("Detail").Cells(8,1).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(8,1).Font.Size      = 11
XLApp.Sheets("Detail").Cells(8,1).Font.FontStyle = "Bold"

XLApp.Sheets("Detail").Cells(9,1).Value          = "Status"
XLApp.Sheets("Detail").Cells(9,1).Font.Name      = "Arial"
XLApp.Sheets("Detail").Cells(9,1).Font.Size      = 11
XLApp.Sheets("Detail").Cells(9,1).Font.FontStyle = "Bold"

lnCol = 3
SELECT(lcRpTmpFl2)
LOCATE 
SCAN
  XLApp.Sheets("Detail").Cells(6,lnCol).Value = "Req."
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.FontStyle = "Bold"
  
  XLApp.Sheets("Detail").Cells(7,lnCol).Value = &lcRpTmpFl2..PO
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.FontStyle = "Regular"
  
  XLApp.Sheets("Detail").Cells(8,lnCol).Value = &lcRpTmpFl2..Vendor
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.FontStyle = "Regular"

  XLApp.Sheets("Detail").Cells(9,lnCol).Value = &lcRpTmpFl2..Status
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.FontStyle = "Regular"
  
  lnCol = lnCol+1

  XLApp.Sheets("Detail").Cells(6,lnCol).Value = "Issued."
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(6,lnCol).Font.FontStyle = "Bold"
  
  XLApp.Sheets("Detail").Cells(7,lnCol).Value = &lcRpTmpFl2..PO
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(7,lnCol).Font.FontStyle = "Regular"
  
  XLApp.Sheets("Detail").Cells(8,lnCol).Value = &lcRpTmpFl2..Vendor
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(8,lnCol).Font.FontStyle = "Regular"
  
  XLApp.Sheets("Detail").Cells(9,lnCol).Value = &lcRpTmpFl2..Status
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.Name  = "Arial"
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.Size  = 11
  XLApp.Sheets("Detail").Cells(9,lnCol).Font.FontStyle = "Regular"
  
  lnCol = lnCol+1

ENDSCAN

XLApp.Sheets("Detail").Cells(6,lnCol).Value = "Balance"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Name  = "Arial"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Size  = 11
XLApp.Sheets("Detail").Cells(6,lnCol).Font.FontStyle = "Bold"

lnCol = lnCol+1
XLApp.Sheets("Detail").Cells(6,lnCol).Value = "On Order"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Name  = "Arial"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Size  = 11
XLApp.Sheets("Detail").Cells(6,lnCol).Font.FontStyle = "Bold"

lnCol = lnCol+1
XLApp.Sheets("Detail").Cells(6,lnCol).Value = "Avlbl"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Name  = "Arial"
XLApp.Sheets("Detail").Cells(6,lnCol).Font.Size  = 11
XLApp.Sheets("Detail").Cells(6,lnCol).Font.FontStyle = "Bold"

SELECT (lcRpTmpFl1)
LOCATE
lnRow = 11
SCAN
  XLApp.Sheets("Detail").ROWS(lnRow).INSERT
  XLApp.Sheets("Detail").Cells(lnRow,1).Value = FabrColr
  XLApp.Sheets("Detail").Cells(lnRow,2).Value = NSTOCK
  lnColCont = 2

  SELECT(lcRpTmpFl2)
  lnCount = 1
  SCAN
    LcPo = Po
    SELECT (lcRpTmpFl1)  
    XLApp.Sheets("Detail").Cells(lnRow,lnColCont +(lnCount*2)-1).Value = NREQ&LcPo
    XLApp.Sheets("Detail").Cells(lnRow,lnColCont +lnCount*2 ).Value    = NISS&LcPo
    lnCount = 1 + lnCount
  ENDSCAN
  SELECT (lcRpTmpFl1)    
  
  XLApp.Sheets("Detail").Cells(lnRow,lnCount*2+1).Value         = NBALANCE
  XLApp.Sheets("Detail").Cells(lnRow,(lnCount*2) +2).Value    = NONORDER
  XLApp.Sheets("Detail").Cells(lnRow,(lnCount*2)+3).Value     = NAVALABL

  lnRow = lnRow + 1
ENDSCAN
XLApp.Visible = .T.
*!
*----------------------* Function For OG Browses *----------------------*

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*!
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
*!
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*!
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
LOCAL lnAlias
lnAlias = SELECT()
lnTotcomp = 0

SELECT(lcTmpFab)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcTmpFab)
  LOCATE 
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE Fabric = lcFab
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
SELECT(lnAlias)
RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*!
*!*************************************************************
*! Name        : lfsrvTrans
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/12/2002
*! Purpose     : To set relation on or off when running the in range function 
*!                 in the option grid.
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
    SET FILTER TO STATUS $"OH"
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
    SET FILTER TO 
ENDCASE
*-- end of lfsrvTrans.
*!
*!**************************************************************************
*! Name      : lfStDepart
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*!
FUNCTION lfStDepart
PARAMETERS OpGrdParm
PRIVATE lnSlct
lnSlct = SELECT()
SELECT ICDEPTHD   
DO CASE
  CASE  OpGrdParm='S'           
    lcTmpIndex =loOGScroll.gfTempName()
    SELECT  DISTINCT DEPT,cDeptDesc FROM ICDEPTHD INTO DBF (oAriaApplication.WorkDir+ lcTmpIndex)
    USE IN ICDEPTHD
    use in (lcTmpIndex)
    USE (oAriaApplication.WorkDir+ lcTmpIndex) IN 0 ALIAS ICDEPTHD
    SELECT ICDEPTHD
    INDEX ON DEPT TAG Depthd
    GO TOP
  CASE  OpGrdParm='R'
    USE IN ICDEPTHD
    =gfOpenFile(oAriaApplication.DataDir+'ICDEPTHD',oAriaApplication.DataDir+'DEPTHD ','SH')
ENDCASE
SELECT (lnSlct)
*-- End of lfStDepart.
*!
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*!
FUNCTION lfSRVSty
PARAMETERS lcParm
*-- Save the old alias
PRIVATE lnSlct
lnSlct = SELECT()
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
*-- restore the old work aria
SELECT (lnSlct)
*-- end of lfsrvSty.
*!
*--------------------* Functions Create Temp Files *--------------------*

*!*************************************************************
*! Name        : lfCreateFl
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/12/2002
*! Purpose     : Create the report temp file.
*!*************************************************************
*!
FUNCTION lfCreateFl

DIMENSION laFlSruc[20,4]

laFlSruc[1,1] = "STYLE"
laFlSruc[2,1] = "ITEM"
laFlSruc[3,1] = "ICLR"
laFlSruc[4,1] = "DESC"
laFlSruc[5,1] = "CWARECODE"
laFlSruc[6,1] = "TYP"
laFlSruc[7,1] = "CCATGTYP"
laFlSruc[8,1] = "CNT"
laFlSruc[9,1] = "UOM"
laFlSruc[10,1]= "STKTOTQY"
laFlSruc[11,1]= "REQTOTQT"
laFlSruc[12,1]= "ISSTOTQT"
laFlSruc[13,1]= "ONORDQTY"
laFlSruc[14,1]= "NYTOWIP"
laFlSruc[15,1]= "NUSEDREQ"
laFlSruc[16,1]= "NNETREQ"
laFlSruc[17,1]= "LSTYMAKE"
laFlSruc[18,1] = "PO"
laFlSruc[19,1] = "cVenCode"
laFlSruc[20,1] = "Status"

*--Second array element [Type].
STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2],;
             laFlSruc[5,2] ,laFlSruc[6,2] ,laFlSruc[7,2] ,laFlSruc[9,2],;
             laFlSruc[18,2] ,laFlSruc[19,2] ,laFlSruc[20,2]
             
STORE "N" TO laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],laFlSruc[13,2],;
             laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],laFlSruc[8,2]

STORE "L" TO laFlSruc[17,2]

*--Third array element [Length].
STORE  1  TO laFlSruc[6,3] ,laFlSruc[7,3] ,laFlSruc[8,3],laFlSruc[16,3],laFlSruc[20,3],laFlSruc[17,3]
STORE  3  TO laFlSruc[9,3]
STORE  6  TO laFlSruc[3,3] ,laFlSruc[5,3] ,laFlSruc[18,3]
STORE  13  TO laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],laFlSruc[13,3]
STORE  8  TO laFlSruc[19,3]
STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
STORE 12  TO laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3]
STORE 20  TO laFlSruc[4,3]

*--Forth array element [Decimal].
STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4], ;
             laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4] ,laFlSruc[8,4], ;
             laFlSruc[9,4] ,laFlSruc[17,4],laFlSruc[18,4],laFlSruc[19,4],;
             laFlSruc[20,4]
             
STORE  3  TO laFlSruc[14,4],laFlSruc[15,4],laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],;
             laFlSruc[13,4],laFlSruc[16,4] 

*-- Add the field "lUpdonce" so that the stk and OnOrder is updated once for each fabric/color
PRIVATE lnDim
lnDim = ALEN(laFlSruc,1) + 1
DIMENSION laFlSruc[lnDim,4]
laFlSruc[lnDim,1] = 'lUpdOnce'
laFlSruc[lnDim,2] = 'L'
laFlSruc[lnDim,3] = 1
laFlSruc[lnDim,4] = 0

gfCrtTmp(lcMatReq,@laFlSruc,,"",.F.)

SELECT &lcMatReq
INDEX ON Item+IClr+PO TAG Matreq OF (lcMatReq)
INDEX ON PO+STYLE TAG MrPOSty OF (lcMatReq)
RETURN
*!
*!**************************************************************************
*! PROG      : lfPrepTemp.PRG   (C#102584)
*! DESC      : Function to print detail to Excel sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2002
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ...
*!**************************************************************************
*!
FUNCTION lfPrepTemp

DIMENSION laTempStru[1,4]
laTempStru = ''

DIMENSION laTempStru[ALEN(laTempStru,1) , 4]
laTempStru[ALEN(laTempStru,1),1] = 'FabrColr'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 17
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NSTOCK'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

SELECT(lcPOTmpFil)
SCAN
  lcVarReq="NREQ"+PO
  lcVarIss="NISS"+PO

  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  laTempStru[ALEN(laTempStru,1),1] = lcVarReq
  laTempStru[ALEN(laTempStru,1),2] = 'N'
  laTempStru[ALEN(laTempStru,1),3] = 13
  laTempStru[ALEN(laTempStru,1),4] = 0

  DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
  laTempStru[ALEN(laTempStru,1),1] = lcVarIss
  laTempStru[ALEN(laTempStru,1),2] = 'N'
  laTempStru[ALEN(laTempStru,1),3] = 13
  laTempStru[ALEN(laTempStru,1),4] = 0
ENDSCAN

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NBALANCE'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NONORDER'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NAVALABL'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 13
laTempStru[ALEN(laTempStru,1),4] = 0

gfCrtTmp(lcTmpDtl,@laTempStru,"FabrColr",lcTmpDtl,.F.)
*!

