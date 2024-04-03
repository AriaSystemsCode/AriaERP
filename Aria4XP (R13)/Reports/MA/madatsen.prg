*:**************************************************************************
*: Program file        : MADATSEN.PRG
*: Program desc.       : Date Sensitive Material Report
*: Module              : MATERIAL (MA)
*: Developer           : Heba Fathi (HFK)
*: Tracking Job Number : #037631
*: Date                : 09/19/2004
*:**************************************************************************
*: Calls :
*: Global Function:
*:                     : gfModalGen()
*:                     : gfBroWWare()
*:**************************************************************************
*: Passed Parameters   : None
*:**************************************************************************
*: Example             : DO MADATSEN
*:**************************************************************************
*: Modifications       : Add option to specify base date (Transaction date/Posting date)
*: E302309,1 AYM 9/1/2006 CONVERT TO GRAPHICS  (T20060809.0032)
*: B607854,1 MMT 12/04/2006 Bug when select fabric type
*: B607868,1 12/07/2006 MMT Fix bug of wrong data when Domestic/manfactured option changed. (T20061013.0017)
*: B609350,1 MMT 07/18/2010 Material date senstive inv. report does not get data when user select Fabrics range[T20100629.0017]
*! B609356,1 SMA 07/25/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*: B609350,2 MMT 07/18/2010 Material date senstive inv. report does not Print Fabrics that have Zero Stock value but have stock[T20100629.0017]
*:**************************************************************************
*!
#INCLUDE R:\ARIA4XP\REPORTS\MA\MADATSEN.H



lcStTime = TIME()       && To store the current time
LOCAL lcWhereCnd
LOCAL lcItem
LOCAL llItem
*-Check filter changes
IF loOGScroll.llOGFltCh
  STORE "" TO lcItem,lcWhereCnd
  STORE .F. TO llItem

  *- Call function to build where condition
  lcWhereCnd = lfBldWhereCond('lcWhereCnd')
  *- Convert Item.Cstymajor Filter to a cursor
  lcItem =lfBldSqlCur('ITEM.CSTYMAJOR','llItem','lcItem','ItemMajor C(19)','CSTYMAJOR')

  *- Specify selected fields
  lcSelFields = "ITEM.STYLE as Fabric,ITEMJRNL.CWARECODE,ITEM.[DESC],ITEMJRNL.NTOTSTK,ITEMJRNL.NSTKVAL,ITEMJRNL.CIRTYPE,ITEMJRNL.CICACNT AS GLACC "
  lcStatmnt = " ITEM (INDEX = STYLE) LEFT OUTER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE "
  lcStatmnt = lcStatmnt + " INNER JOIN ITEMJRNL (INDEX = STYDATE) ON ITEMLOC.STYLE = ITEMJRNL.STYLE AND "
  lcStatmnt = lcStatmnt + " ITEMLOC.CWARECODE = ITEMJRNL.CWARECODE "

  *-If items selected in the in list add it to Sql query
  IF !EMPTY(lcItem)  && Items selected in the in list browse
    lcStatmnt = lcStatmnt + " INNER JOIN " + lcItem + " TmpItem ON TmpItem.ItemMajor = ITEM.CSTYMAJOR "
  ENDIF

  lcSqlStat = "Select " + lcSelFields + " FROM " + lcStatmnt + " WHERE " + lcWhereCnd
  lnResult = loOGScroll.oRDA.SqlRun (lcSqlStat,lcGetData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnResult > 0  && got data from Sql
    SELECT &lcGetData
    lnBuffering = CURSORGETPROP("Buffering","&lcGetData")
    =CURSORSETPROP("Buffering",3,"&lcGetData")
    SELECT &lcGetData
    *- create index on sql data file
    INDEX ON Fabric+CWARECODE TAG &lcGetData
    SET ORDER TO TAG &lcGetData
    = lfCreatFil()

    *-- If print Warehouse Details is set to YES [Begin]
    IF llRpPrnWhD
      *-- If Multi Bins is true [Begin]
      IF llMultiBin
        SET RELATION TO Fabric+cWareCode INTO WhsLoc ADDITIVE
      ENDIF
    ELSE         && Else print Warehouse Details is set to NO
      IF llMultiBin
        SET RELATION TO Fabric INTO WhsLoc ADDITIVE
      ENDIF
    ENDIF
    *-- Endif print Warehouse Details is Yes [Begin]

    *-- If no records match criteria
    SELECT &lcGetData
    IF lfNoRecord()
      RETURN
    ELSE
      STORE 0 TO lnRpTVlCst,lnRpTSalVl
      *-  If sort by location or sort by fabric and print location details
      IF lcRpSortBy='W' .OR. (lcRpSortBy = 'S' .AND. llRpPrnWhD)
        =lfSortByLc()
      ELSE
        DO lpCollData   && collect the data to the temporary file
      ENDIF
    ENDIF   && no records match criteria
  ENDIF
ENDIF
IF USED(lcFabTmp)
  SELECT (lcFabTmp)
  SET FILTER TO !(nReceived = nIssued)
  IF lfNoRecord()
    RETURN
  ELSE
    *: B609350,2 MMT 07/18/2010 Material date senstive inv. report does not Print Fabrics that have Zero Stock value but have stock[Start]
    *lcExpr = 'ABS(nStkVal)<> 0'
    lcExpr = 'ABS(nStkVal)<> 0 OR ABS(nTotStk)<> 0'
    *: B609350,2 MMT 07/18/2010 Material date senstive inv. report does not Print Fabrics that have Zero Stock value but have stock[End]
    *: E302309,1 AYM 9/1/2006 CONVERT TO GRAPHICS --BEGIN
    loogScroll.cCROrientation = 'P'
    *: E302309,1 AYM 9/1/2006 CONVERT TO GRAPHICS --END
    DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcExpr
  ENDIF
ELSE
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*-- Endif of (no records match criteria) [End]
****************************************************************************
*************************** *-- End of Report--* ***************************
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
*-- End of lfNoRecord.
*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Option Grid When function
*!**************************************************************************
*
FUNCTION lfwRepWhen


LOCAL lnResult1
IF !llFrstTime
  lcTmpFab = loOGScroll.gfTempName()
  lcSelected = " SELECT ITEMLOC.TOTWIP,ITEMLOC.TOTSTK,ITEM.CSTYMAJOR AS FABRIC FROM ITEM (INDEX = CSTYLE)INNER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEM.CDEFWARE = ITEMLOC.CWARECODE "
  lcWhereCondition = " ITEMLOC.DYELOT = '   ' AND ITEM.CINVTYPE = 0002 AND ITEMLOC.CINVTYPE = 0002 "
  lcSqlStatement = lcSelected + " WHERE " + lcWhereCondition
  lnResult1 = loOGScroll.orda.SqlRun (lcSqlStatement,lcTmpFab,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  llFrstTime = .T.
  IF lnResult1 >= 1
    lnBuffering = CURSORGETPROP("Buffering",lcTmpFab)
    =CURSORSETPROP("Buffering",3,lcTmpFab)
    SELECT (lcTmpFab)
    INDEX ON Fabric TAG &lcTmpFab
    SET ORDER TO TAG &lcTmpFab
  ENDIF
ENDIF

*-- end of lfwRepWhen.
*!**************************************************************************
*! Name      : lfCreatFil
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*
FUNCTION lfCreatFil

DIMENSION laFileStru[1,1]
SELECT &lcGetData
= AFIELDS(laFileStru)
lnNoField = ALEN(laFileStru , 1)
DIMENSION laFileStru[lnNoField + 6 , 18]

laFileStru[lnNoField + 1,1] = 'Ave_Cost'
laFileStru[lnNoField + 1,2] = 'N'
laFileStru[lnNoField + 1,3] = 10
laFileStru[lnNoField + 1,4] = 3

laFileStru[lnNoField + 2,1] = 'llWare'
laFileStru[lnNoField + 2,2] = 'L'
laFileStru[lnNoField + 2,3] = 1
laFileStru[lnNoField + 2,4] = 0

laFileStru[lnNoField + 3,1] = 'nReceived'
laFileStru[lnNoField + 3,2] = 'N'
laFileStru[lnNoField + 3,3] = 13
laFileStru[lnNoField + 3,4] = 3

laFileStru[lnNoField + 4,1] = 'nIssued'
laFileStru[lnNoField + 4,2] = 'N'
laFileStru[lnNoField + 4,3] = 13
laFileStru[lnNoField + 4,4] = 3

IF lnMajorLen = 0
  lnMajorLen = 12
ENDIF

laFileStru[lnNoField + 5,1] = 'cFabric'
laFileStru[lnNoField + 5,2] = 'C'
laFileStru[lnNoField + 5,3] = lnMajorLen
laFileStru[lnNoField + 5,4] = 0

lnClrLen = 18 - lnMajorLen

laFileStru[lnNoField + 6,1] = 'cColor'
laFileStru[lnNoField + 6,2] = 'C'
laFileStru[lnNoField + 6,3] = lnClrLen
laFileStru[lnNoField + 6,4] = 0

FOR  lnLen = 7 TO 18
  FOR lnCount = 1 TO 6
    STORE SPACE(1) TO laFileStru[lnNoField + lnCount,lnLen]
  ENDFOR
ENDFOR
lcFabTmp = loOGScroll.gfTempName()
gfCrtTmp(lcFabTmp,@laFileStru)
SELECT (lcFabTmp)
*- If sort by location and not selection a location code index on the cwarecode


IF lcRpSortBy='W'
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON cWareCode+Fabric TAG (lcFabTmp) OF (lcFabTmp)
  INDEX ON cWareCode+Fabric TAG (lcFabTmp)
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ELSE
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON cWareCode+Fabric TAG (lcFabTmp) OF (lcFabTmp)
  INDEX ON Fabric + cWareCode TAG (lcFabTmp)
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDIF

*-- End of lfCreatFil.
*!
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*!
FUNCTION lfFabSum
PARAMETERS lcFab,lcComp
PRIVATE lnItemRec
LOCAL lnAlias
lnAlias = SELECT()
lnTotcomp = 0
SELECT(lcTmpFab)
IF RECCOUNT() != 0
  lnItemRec = RECNO('ITEM')
  SELECT(lcTmpFab)
  LOCATE
  IF SEEK(SUBSTR(lcFab,1,lnMajorLen))
    SUM &lcCOMP TO lnTotcomp WHILE Fabric = lcFab
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnItemRec,1,RECCOUNT())
    GO lnItemRec
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN INT(lnTotcomp)
*!
*!**************************************************************************
*! Name      : lfFillAray
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*!
FUNCTION lfFillAray
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1,1] = LANG_MADATSEN_Fabric
laSortDesc[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MADATSEN_Fabric,oAriaApplication.GetHeaderText("LANG_MADATSEN_Fabric",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2,1] = LANG_MADATSEN_Location
laSortDesc[2,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MADATSEN_Location,oAriaApplication.GetHeaderText("LANG_MADATSEN_Location",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'W'
*-- End of lfFillAray.
*!
*!**************************************************************************
*! Name      : lfvWareHos
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Validate Warehouse Code (Only this location Option)
*!**************************************************************************
*
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag
lcWareHous = OGSYS18(.T.)
lcTag      = ORDER('WAREHOUS')
IF !EMPTY(lcWareCode)
  IF SEEK(&lcWareHous.,'WAREHOUS')
    &lcWareHous = WAREHOUS.cWareCode
  ELSE
    &lcWareHous = gfBroWWare(.T.)
  ENDIF
ENDIF
SET ORDER TO &lcTag IN WAREHOUS

*-- End of lfvWareHos.
*!**************************************************************************
*! Name      : lfwOldWare
*: Developer : Heba Fathi (HFK)
*! Date      : 09/25/2004
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*!
FUNCTION lfwOldWare
lcOldWare = EVALUATE(OGSYS18(.T.))
*-- End of lfwOldWare.
*!
*!**************************************************************************
*! Name      : lfvSortBy
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : valid sort by function
*!**************************************************************************
*!
FUNCTION lfvSortBy
llRpPrnWhD = IIF(lcRpSortBy='W',.T.,.F.)
ClearRead()
*-- End of lfvSortBy.
*!
*!**************************************************************************
*! Name      : lfvPrnWhD
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : valid print warehouse details
*!**************************************************************************
*
FUNCTION lfvPrnWhD
llOldPrWhD = llRpPrnWhD   && Save current print ware house details in another
*-- End of lfvPrnWhD.
*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Collect the data from MatInvJl file.
*!**************************************************************************
*!
PROCEDURE lpCollData
LOCAL lcColors
STORE '' TO lcColors
*- Get Colors selected into a variable
lnFltPos = ASCAN(loOGScroll.laOGVrFlt,'lcRpColor')
IF lnFltPos > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnFltPos,1)
  lcColors = loOGScroll.laOGVrFlt[lnRow,6]
ENDIF

SELECT (lcGetData)
SCAN
  lcFabClr = SUBSTR(Fabric,lnMajorLen+2)
  lcClrExp = IIF(EMPTY(lcColors)," .T. ","lcFabClr $ lcColors")
  IF &lcClrExp
    lcFabID = Fabric
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MADATSEN_MsgColData + Fabric NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MADATSEN_MsgColData,oAriaApplication.GetHeaderText("LANG_MADATSEN_MsgColData",AHEADERFILE)) + Fabric NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    SCATTER MEMVAR
    m.cFabric = SUBSTR(Fabric,1,lnMajorLen)
    m.cColor = lcFabClr
    m.nReceived = IIF(cIRType = 'R',nTotStk,0)
    m.nIssued = IIF(cIRType = 'I',nTotStk * -1,0)
    SELECT(lcFabTmp)
    IF !SEEK(lcFabID)
      INSERT INTO (lcFabTmp) FROM MEMVAR
      REPLACE Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)
      *-- Replace nStkVal with if Ave_Cost = 0 or nTotStk
      IF nTotStk = 0 OR Ave_Cost = 0
        REPLACE nStkVal WITH 0
      ENDIF
    ELSE
      REPLACE  nTotStk    WITH m.nTotStk + nTotStk,;
               nreceived  WITH nReceived + m.nReceived ,;
               nissued    WITH nIssued + m.nIssued ,;
               nStkVal    WITH nStkVal + m.nStkVal,;
               Ave_Cost   WITH IIF(nTotStk<>0,nStkVal /nTotStk,0)
      IF nTotStk = 0 OR Ave_Cost = 0
        REPLACE nStkVal WITH 0
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
*!
*!**************************************************************************
*! Name      : lfSortByLc
*: Developer : Heba Fathi (HFK)
*! Date      : 09/25/2004
*! Purpose   : Called if sort by location.
*!**************************************************************************
FUNCTION lfSortByLc
PRIVATE lnAlias
LOCAL lcColors
STORE '' TO lcColors
*- Get Colors selected into a variable
lnFltPos = ASCAN(loOGScroll.laOGVrFlt,'lcRpColor')
IF lnFltPos > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnFltPos,1)
  lcColors = loOGScroll.laOGVrFlt[lnRow,6]
ENDIF
lcClrExp = IIF(EMPTY(lcColors)," .T. ","lcFabClr $ lcColors")
lnAlias = SELECT(0)
SELECT &lcGetData
SCAN
  lcFabID     = Fabric
  lcWareHouse = cWareCode
  lcFabClr = SUBSTR(Fabric,lnMajorLen+2)
  IF &lcClrExp
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_MADATSEN_MsgColData + Fabric NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MADATSEN_MsgColData,oAriaApplication.GetHeaderText("LANG_MADATSEN_MsgColData",AHEADERFILE)) + Fabric NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    SCATTER MEMVAR
    m.llWare    = IIF(lcRpSortBy = 'S' .AND. llRpPrnWhD,.T.,.F.)
    m.cFabric   = SUBSTR(Fabric,1,lnMajorLen)
    m.cColor    = lcFabClr
    m.nReceived = IIF(cIRType = 'R',nTotStk,0)
    m.nIssued   = IIF(cIRType = 'I',nTotStk * -1,0)
    SELECT(lcFabTmp)
    IF lcRpSortBy = 'W'
      llSeekExp = !SEEK(lcWareHouse + lcFabID)
    ELSE
      llSeekExp = !SEEK(lcFabID + lcWareHouse)
    ENDIF
    IF llSeekExp
      INSERT INTO (lcFabTmp) FROM MEMVAR
      REPLACE Ave_Cost WITH IIF(nTotStk<>0,nStkVal/nTotStk,0)

      IF nTotStk = 0 OR Ave_Cost = 0
        REPLACE nStkVal WITH 0
      ENDIF

    ELSE
      REPLACE  nTotStk    WITH m.nTotStk + nTotStk,;
               nreceived  WITH nReceived + m.nReceived ,;
               nissued    WITH nIssued + m.nIssued ,;
               nStkVal    WITH nStkVal + m.nStkVal,;
               Ave_Cost   WITH IIF(nTotStk<>0,nStkVal /nTotStk,0)

      IF nTotStk = 0 OR Ave_Cost = 0
        REPLACE nStkVal WITH 0
      ENDIF
    ENDIF
  ENDIF
ENDSCAN  && Scan in Sql file
SELECT(lnAlias)
*!
*!*************************************************************
*! Name      : lfBldSqlCur
*! Developer : Heba Fathi (HFK)
*! Date      : 09/01/2004
*! Purpose   : Build Sql Cursors Needed For Getting Data
*!*************************************************************
*! Example   : = lfBldSqlCur()
*!*************************************************************
*!
FUNCTION lfBldSqlCur
PARAMETERS lcFilter,lcFound,lcCursor,lcFldName,lcSntFld
LOCAL  lnFltPos,lnRow,lcExpression
STORE 0 TO lnPosition,lnFltPos,lnRow
STORE '' TO lcExpression
lnFltPos = ASCAN(loOGScroll.laOGFxFlt,lcFilter)
IF lnFltPos > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnFltPos,1)
  lcTmpCur = loOGScroll.laOGFxFlt[lnRow,6]
  IF !EMPTY(lcTmpCur)  &&user selected some styles.
    SELECT &lcTmpCur
    &lcFound = ( RECCOUNT() > 0)
    IF &lcFound
      &lcCursor = loOgScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
      IF EMPTY(&lcCursor)
        *-- SQL connection Error. Can't open The Report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ELSE
    &lcCursor = ""
  ENDIF && Empty(lcTmpCur)
ENDIF && Position > 0
RETURN &lcCursor
*!
*!*************************************************************
*! Name      : lfBldWhereCond
*! Developer : Heba Fathi (HFK)
*! Date      : 09/27/2004
*! Purpose   : Build Where Condition for SQL.
*!*************************************************************
*! Example   : = lfBldWhereCond()
*!*************************************************************
*!
FUNCTION lfBldWhereCond
PARAMETERS lcWhereCondition
*- Add inventory type filter to the Sql condition
&lcWhereCondition = ' ITEM.CINVTYPE = ' + lcInvType + ' AND ITEMJRNL.CINVTYPE = ' + lcInvType + ' AND ITEMLOC.CINVTYPE = ' + lcInvType
*-ADD SQLEXP TO WHERE COND
IF LEN(loOGScroll.lcRpSqlExp) > 1
  lcExp = loOGScroll.lcRpSqlExp
  *: B609350,1 MMT 07/18/2010 Material date senstive inv. report does not get data when user select Fabrics range[Start]
*!*    lnOccur = OCCURS(' AND',lcExp)
*!*    IF lnOccur > 0
*!*      FOR lnCount = 1 TO lnOccur + 1
*!*        lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 4)
*!*        lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC(' AND',lcExp,lnCount))
*!*        lnLength = IIF(lnCount = lnOccur + 1,lnEnd - lnStart+1,lnEnd - lnStart)
*!*        lcTake = SUBSTR(lcExp,lnStart,lnLength)
*!*        IF !ATC('ITEM.CSTYMAJOR',lcTake) > 0
*!*          &lcWhereCondition = &lcWhereCondition + " AND " + lcTake
*!*        ENDIF
*!*      ENDFOR
*!*    ELSE
*!*      IF !ATC('ITEM.CSTYMAJOR',lcExp) > 0
*!*        &lcWhereCondition = &lcWhereCondition + " AND " + lcExp
*!*      ENDIF
*!*    ENDIF
  &lcWhereCondition = &lcWhereCondition + " AND " + lcExp
  *: B609350,1 MMT 07/18/2010 Material date senstive inv. report does not get data when user select Fabrics range[End]
*!*B607854,1 MMT 12/04/2006 Bug when select fabric type [Start]
ELSE
  IF ASCAN(loogscroll.lafltexp,'ITEM')# 0 AND !EMPTY(loogscroll.lafltexp[ASUBSCRIPT(loogscroll.lafltexp,ASCAN(loogscroll.lafltexp,'ITEM') ,1),2])
    lcItemCond = loogscroll.lafltexp[ASUBSCRIPT(loogscroll.lafltexp,ASCAN(loogscroll.lafltexp,'ITEM') ,1),2]
    lcItemCond = STRTRAN(lcItemCond ,'.AND.',' AND ')
    &lcWhereCondition = &lcWhereCondition + lcItemCond
  ENDIF
*!*B607854,1 MMT 12/04/2006 Bug when select fabric type [End]
ENDIF

*- Add manufactured/purchased filter to the Sql condition
IF lcRPDomImp <> 'B'
  IF lcRPDomImp = 'D'  && MANUFACTURED

    *B607868,1 12/07/2006 MMT Fix bug of wrong data when Domestic/manfactured option changed[Start]
    *&lcWhereCondition = &lcWhereCondition + " AND ITEMJRNL.CBUSDOCU = 'P' AND ITEMJRNL.CSTYTYPE = 'U' "
    &lcWhereCondition = &lcWhereCondition + " AND Item.make = '1'"
    *B607868,1 12/07/2006 MMT Fix bug of wrong data when Domestic/manfactured option changed[End]

  ELSE  && IMPORTED

   *B607868,1 12/07/2006 MMT Fix bug of wrong data when Domestic/manfactured option changed[Start]
   * &lcWhereCondition = &lcWhereCondition + " AND ITEMJRNL.CBUSDOCU = 'P' AND ITEMJRNL.CSTYTYPE = 'P' "
   &lcWhereCondition = &lcWhereCondition + " AND Item.make = '0'"
   *B607868,1 12/07/2006 MMT Fix bug of wrong data when Domestic/manfactured option changed[End]

  ENDIF
ENDIF

*- Add selected warehous(if any) to the Sql Condition.
IF !EMPTY(lcWareCode)
  &lcWhereCondition = &lcWhereCondition + " AND ITEMLOC.CWARECODE = '" + lcWareCode + "'"
ENDIF

*- Add fabric has dyelot filter to the Sql condition
IF lcHasDylot <> 'A'
  &lcWhereCondition = &lcWhereCondition + IIF(lcHasDylot = 'Y'," AND ITEM.CDYE_FLG = 'Y' "," AND ITEM.CDYE_FLG = 'N' ")
ENDIF

*- Add selected G/L inventory account(if any) to the where condition.
IF !EMPTY(lcRpGlAcc)
  &lcWhereCondition = &lcWhereCondition + " AND ItemJrnl.CICACNT = '" + ALLTRIM(lcRpGlAcc) + "'"
ENDIF

*- Add date specified to the Sql condition
ldTranDate = DTOC(ldRpDate)
IF lcRpBaseDt = 'T'  && Variable to declare base date
  &lcWhereCondition = &lcWhereCondition + " AND ITEMJRNL.DTRDATE <= '" + ldTranDate + "'"
ELSE
  &lcWhereCondition = &lcWhereCondition + " AND ITEMJRNL.DPOSTDATE <= '" + ldTranDate + "'"
ENDIF
RETURN &lcWhereCondition
