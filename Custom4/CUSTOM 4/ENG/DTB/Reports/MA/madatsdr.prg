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
*! B609350,2 MMT 07/18/2010 Material date senstive inv. report does not Print Fabrics that have Zero Stock value but have stock[T20100629.0017]
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [T20120625.0018]
*! C201536,1 SAB 11/05/2012 Create adjustment for styles that has no movement at any warehouse [T20121031.0001]
*:**************************************************************************
*!
#INCLUDE R:\ARIA4XP\REPORTS\MA\MADATSDR.H


lcStTime = TIME()       && To store the current time
LOCAL lcWhereCnd
LOCAL lcItem
LOCAL llItem


*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
llGlLink = IIF(gfGetMemVar('M_LINK_GL')='Y',.T.,.F.)
lnPos = lfItmPos('CADJREASON')
IF llRpAutoAdj AND EMPTY(laOgfxFlt[lnPos, 6])
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'adjustment reason')  
  RETURN .F.  
ENDIF
IF llRpAutoAdj AND EMPTY(ldRpAdjDate)
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'adjustment date')  
  RETURN .F.  
ENDIF
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

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
        *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
        IF llRpAutoAdj AND (lcRpSortBy = 'W' OR llRpPrnWhD)
          SELECT (lcFabLocTmp)
          APPEND FROM (oAriaApplication.WorkDir+lcFabTmp) FOR !EMPTY(cWareCode)
        ENDIF
        *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]        
      ELSE
        *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
        IF llRpAutoAdj AND lcRpSortBy = 'S' AND !llRpPrnWhD
          llRpPrnWhD = .T.
          =lfSortByLc()
          SELECT (lcFabLocTmp)
          APPEND FROM (oAriaApplication.WorkDir+lcFabTmp) FOR !EMPTY(cWareCode)
          SELECT (lcFabTmp)
          ZAP
          llRpPrnWhD = .F.
        ENDIF
        *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
        DO lpCollData   && collect the data to the temporary file    
      ENDIF
    ENDIF   && no records match criteria  
  ENDIF 
ENDIF 

*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
lnPos = lfItmPos('CADJREASON')
STORE laOgFxFlt[lnPos, 6] TO lcAdjReason
IF llRpAutoAdj  
  =lfCreateAdjust(lcFabLocTmp)
ENDIF
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

IF USED(lcFabTmp)
  SELECT (lcFabTmp)
  SET FILTER TO !(nReceived = nIssued)
  IF lfNoRecord()
    RETURN
  ELSE
    *: B609350,2 MMT 07/18/2010 Material date senstive inv. report does not Print Fabrics that have Zero Stock value but have stock[Start]
    *lcExpr = 'ABS(nStkVal)<> 0'
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
    *lcExpr = 'ABS(nStkVal)<> 0 OR ABS(nTotStk)<> 0'
    IF llRpPrZero
      lcExpr = 'ABS(nStkVal)<> 0 AND ABS(nTotStk)<> 0'
    ELSE
      lcExpr = 'ABS(nStkVal)<> 0 OR ABS(nTotStk)<> 0'      
    ENDIF
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
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

*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
=gfOpenTable(oAriaApplication.DataDir+'Codes',oAriaApplication.DataDir+'CCODE_NO','SH')   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
=gfOpenTable('ItemJrnl','STYDATE','SH')   && CINVTYPE+STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE+CTRCODE+STR(LINENO,6)+CTRTYPE+STR(NLINENO,4)
=gfOpenTable('ITEM','STYLE','SH')   && CINVTYPE+STYLE
=gfOpenTable('ITEMADJ','INVTADJ','SH')   && CINVTYPE+STYLE
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

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

*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
gfCrtTmp(lcFabLocTmp,@laFileStru)
SELECT (lcFabLocTmp)
IF lcRpSortBy='W'
  INDEX ON cWareCode+Fabric TAG (lcFabLocTmp)
ELSE  
  INDEX ON Fabric + cWareCode TAG (lcFabLocTmp)
ENDIF
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

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
laSortDesc[1,1] = LANG_MADATSEN_Fabric
laSortDesc[2,1] = LANG_MADATSEN_Location
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
    WAIT WINDOW LANG_MADATSEN_MsgColData + Fabric NOWAIT
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
    WAIT WINDOW LANG_MADATSEN_MsgColData + Fabric NOWAIT
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


*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/14/2012
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

ENDFUNC
*-- End of lfItmPos.

*!*************************************************************
*! Name      : lfCreateAdjust
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/14/2012
*! Purpose   : Create Style Adjustment
*!*************************************************************
FUNCTION lfCreateAdjust
LPARAMETERS lcFabLocTmp


STORE '' TO lcGLFYear, lcGLPeriod

*-- Open a temp file to be used in calling gl distributer proc.
lcWorkDir=oAriaApplication.Workdir
IF llGlLink
  =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO (lcWorkDir+lcTmpGlDt)
  USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF  

*!*	*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
*!*	lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
*!*	IF TYPE('lcTranCode') = 'N'
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
*!*	  RETURN .F.
*!*	ENDIF
*!*	*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

SELECT (lcFabLocTmp)
SCAN 
  *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
  *IF nTotStk > 0
  IF nTotStk > 0 .AND. nStkVal > 0
  *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
    llContinue = .T.
    SELECT ItemJrnl
    *SET ORDER TO STYDATE   && CINVTYPE+STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE+CTRCODE+STR(LINENO,6)+CTRTYPE+STR(NLINENO,4)
    
    *! C201536,1 SAB 11/05/2012 Create adjustment for styles that has no movement at any warehouse [Start]
    *=gfSeek("0002"+&lcFabLocTmp..Fabric+&lcFabLocTmp..cWareCode, 'ItemJrnl', 'STYDATE')
    =gfSeek("0002"+&lcFabLocTmp..Fabric, 'ItemJrnl', 'STYDATE')
    *! C201536,1 SAB 11/05/2012 Create adjustment for styles that has no movement at any warehouse [End]
    IF FOUND()      
      *! C201536,1 SAB 11/05/2012 Create adjustment for styles that has no movement at any warehouse [Start]
      *SCAN REST WHILE CINVTYPE+STYLE+CWARECODE = "0002"+&lcFabLocTmp..Fabric+&lcFabLocTmp..cWareCode FOR dTrDate > ldRpAdjDate
      SCAN REST WHILE CINVTYPE+STYLE+CWARECODE = "0002"+&lcFabLocTmp..Fabric FOR dTrDate > ldRpAdjDate
      *! C201536,1 SAB 11/05/2012 Create adjustment for styles that has no movement at any warehouse [End]
        llContinue = .F.
        EXIT
      ENDSCAN
    ENDIF    

    IF !llContinue
      LOOP
    ENDIF
    
    *- Create an inventry adjustment
    WAIT WINDOW "Create a Physical Inventory for item " + &lcFabLocTmp..Fabric+ " in warehouse " + &lcFabLocTmp..cWareCode NOWAIT
   
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
    *=gfSeek(&lcFabLocTmp..Fabric, 'ITEM')
    =gfSeek("0002"+&lcFabLocTmp..Fabric, 'ITEM')
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
    lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(ITEM.Link_Code), ITEM.Link_Code, 'DEFDEF'), "")
    PRIVATE laOtherPar
    DIMENSION laOtherPar[2,2]
    laOtherPar[1,1] = 'lcRelCode'
    laOtherPar[1,2] = ITEM.cConvBuy
    laOtherPar[2,1] = 'llDontUpdate'
    laOtherPar[2,2] = .T.

    SELECT (lcFabLocTmp)
    *--Gl adjustment account.
    lcAdjAcct = ' '
    IF llGlLink
      DECLARE laTrmRltFd[1,2]
      laTrmRltFd[1,1] = 'GLACCOUNT'
      laTrmRltFd[1,2] = 'lcAdjAcct'
      =gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
    ELSE
      *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
      *lcAdjReason = ' '
      *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
    ENDIF
  
    lcGlSess = gfSequence('GLSession')
    
    SELECT (lcFabLocTmp)
    lcFab    = Fabric 
    lcWare   = cWareCode
    lcDye    = ''
    ldPost   = ldRpDate
    lcWareto = ''
    *--G/L Array difinition and initialization.
    IF llGlLink
      =CHECKPRD(ldRpDate, 'lcGLFYear', 'lcGLPeriod', 'IA', .T.)
      DECLARE laGLDistAr[2,13]      
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkCode
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '016'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'MP'       TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldRpDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcTmpGlDt  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = lcAdjAcct
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF    

    DIMENSION laAdjust[9]
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      laAdjust[lnI] = 0
    ENDFOR
    laAdjust[1] = &lcFabLocTmp..nTotStk
    laAdjust[9] = &lcFabLocTmp..nTotStk    
    lnACost = 0 &&&lcFabTmp..Ave_Cost * -1

    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
    **- Fill Roll temp file here
    *lcCostMth = gfGetMemVar('M_MatCstMt')
    *IF (lcCostMth  $ 'LFI')
    *  STORE '' TO lcTmpJour, lcFullRoll, lcTmpRoll
    *  lcTmpJour  = gfTempName()
    *  lcFullRoll = gfTempName()
    *  lcTmpRoll  = gfTempName()
    *  
    *  *DO (oAriaApplication.ProgramHome+'MAJrRoData.fxp') WITH '2', '0002', lcFab, lcDye, lcWare, '', laAdjust, lcTmpJour, lcFullRoll, lcTmpRoll, ldRpDate, ldRpDate      
    *  IF !USED('ROLLS')
    *    =gfOpenTable(oAriaApplication.DataDir+'ROLLS',oAriaApplication.DataDir+'ROLLS','SH')
    *  ENDIF 
    *
    *  =lfCreateRollsTables()
    *  
    *ENDIF
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
    
    *- Fill Roll temp file here
    lcCostMth = gfGetMemVar('M_MatCstMt')
    IF (lcCostMth  $ 'LFI')
      STORE '' TO lcTmpJour, lcFullRoll, lcTmpRoll
      lcTmpJour  = gfTempName()
      lcFullRoll = gfTempName()
      lcTmpRoll  = gfTempName()      
      
      DO (oAriaApplication.ProgramHome+'MAJRRODATA.FXP') WITH '2', '0002', lcFab, lcDye, lcWare, '', laAdjust, lcTmpJour, lcFullRoll, lcTmpRoll, ldRpDate, ldRpDate, .T.
      
      
    ENDIF
    
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
    *- Call the gfStyCrl function to do Inventory adjustment  
    *lnRet = gfItemCrl('2', '0002', lcFab, lcWare, lcDye, ldRpDate, ldRpDate, SPACE(6), @laAdjust,;
                   lnACost, 'Custom Adjustment', lcAdjReason, gfCodDes(lcAdjReason, 'CADJREASON'), @laGLDistAr, lcGlSess, '', '',;
                   '', '', '', '', .F., '', .F., @laOtherPar)
    lnRet = gfItemCrl('2', '0002', lcFab, lcWare, lcDye, ldRpDate, ldRpDate, SPACE(6), @laAdjust,;
                   lnACost, gfCodDes(lcAdjReason, 'CADJREASON'), lcAdjReason, gfCodDes(lcAdjReason, 'CADJREASON'), @laGLDistAr, lcGlSess, '', '',;
                   '', '', '', '', .F., '', .F., @laOtherPar)
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]
    
    
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]    
    IF lnRet = 1
      =lfUpdateTables()
    ENDIF
    *! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

  ENDIF
ENDSCAN

*!*	*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
*!*	*- Update ITEM Table
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('item',lcTranCode,SET("Datasession"),;
*!*	                      'CINVTYPE,STYLE','ITEM','STYLE')
*!*	IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	  =TABLEREVERT(.T.)
*!*	  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*!*	  RETURN .F.
*!*	ELSE
*!*	  =TABLEUPDATE(.T.,.T.)
*!*	ENDIF

*!*	*- Update ITEMLOC Table
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemloc',lcTranCode,SET("Datasession"),;
*!*	                      'CINVTYPE,STYLE,CWARECODE,DYELOT','ITEMLOC','STYDYE')
*!*	IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	  =TABLEREVERT(.T.)
*!*	  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*!*	  RETURN .F.
*!*	ELSE
*!*	  =TABLEUPDATE(.T.,.T.)
*!*	ENDIF

*!*	*- Update ITEMJRNL Table
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemjrnl',lcTranCode,SET("Datasession"),;
*!*	                      'CINVTYPE,STYLE,CWARECODE,DTRDATE,CSESSION,CIRTYPE,CTRCODE,LINENO,CTRTYPE,NLINENO','ITEMJRNL','STYDATE')  
*!*	IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	  =TABLEREVERT(.T.)
*!*	  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*!*	  RETURN .F.
*!*	ELSE
*!*	  =TABLEUPDATE(.T.,.T.)
*!*	ENDIF

*!*	*- Update ITEMADJ Table
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemadj',lcTranCode,SET("Datasession"),;
*!*	                                                                   'CSESSION,CINVTYPE,STYLE,CFROMWARE,DYELOT,LINENO','ITEMADJ','ITEMADJ')
*!*	IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	  =TABLEREVERT(.T.)
*!*	  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*!*	  RETURN .F.
*!*	ELSE
*!*	  =TABLEUPDATE(.T.,.T.)
*!*	ENDIF
*!*	  
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
*!*	IF lnConnectionHandlar # 1
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
*!*	  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
*!*	  llReturn = .F.
*!*	ENDIF
*!*	*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]

ENDFUNC 
*-End of lfCreateAdjust


*!*************************************************************
*! Name      : lfvAutAdj
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/08/2012
*! Purpose   : Validate Auto Adjust Stock to Zero
*!*************************************************************
FUNCTION lfvAutAdj

ClearRead()

IF llRpAutoAdj
  ldRpAdjDate = oAriaApplication.SystemDate
  lnPos = lfItmPos('CADJREASON')
  STORE IIF(gfSeek('D'+'CADJREASON', 'CODES', 'CCODE_NO'), CODES.CCODE_NO, '') TO loOgScroll.laOgFXFlt[lnPos, 6], lcAdjReason
ENDIF

ENDFUNC 
*-End of lfvAutAdj


*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [Start]
*!*************************************************************
*! Name      : lfCreateRollsTables
*! Developer : Saber A.Razek (SAB)
*! Date      : 09/11/2012
*! Purpose   : Create Rolls temp files
*!*************************************************************
FUNCTION lfCreateRollsTables


IF !USED(lcTmpRoll)
  DIMENSION laTags[3,3]
  laTags[1,1]='cRollID+Style+cWareCode+cDyelot+cRsession'
  laTags[1,2]=lcTmpRoll

  laTags[2,1]='Style+cWareCode+cDyelot+STR(LineNo,6)+cMorder'
  laTags[2,2]='lcTmpRoll2'
  
  laTags[3,1]='cRsession+Style+cWareCode+cDyelot' 
  laTags[3,2]='lcTmpRoll3'

  DIMENSION laFileStru[26,4]
  
  laFileStru[1 ,1] = 'cRollId'
  laFileStru[1 ,2] = 'C'
  laFileStru[1 ,3] = 20
  laFileStru[1 ,4] = 0

  laFileStru[2 ,1] = 'cTrn_Seq'
  laFileStru[2 ,2] = 'C'
  laFileStru[2 ,3] = 6
  laFileStru[2 ,4] = 0

  laFileStru[3 ,1] = 'cFabric'
  laFileStru[3 ,2] = 'C'
  laFileStru[3 ,3] = 7
  laFileStru[3 ,4] = 0

  laFileStru[4 ,1] = 'cColor'
  laFileStru[4 ,2] = 'C'
  laFileStru[4 ,3] = 6
  laFileStru[4 ,4] = 0

  laFileStru[5 ,1] = 'cWareCode'
  laFileStru[5 ,2] = 'C'
  laFileStru[5 ,3] = 6
  laFileStru[5 ,4] = 0

  laFileStru[6 ,1] = 'cDyelot'
  laFileStru[6 ,2] = 'C'
  laFileStru[6 ,3] = 10
  laFileStru[6 ,4] = 0

  laFileStru[7 ,1] = 'cRSession'
  laFileStru[7 ,2] = 'C'
  laFileStru[7 ,3] = 6
  laFileStru[7 ,4] = 0

  laFileStru[8 ,1] = 'cISession'
  laFileStru[8 ,2] = 'C'
  laFileStru[8 ,3] = 6
  laFileStru[8 ,4] = 0

  laFileStru[9 ,1] = 'cTran'
  laFileStru[9 ,2] = 'C'
  laFileStru[9 ,3] = 6
  laFileStru[9 ,4] = 0

  laFileStru[10,1] = 'cTranType'
  laFileStru[10,2] = 'C'
  laFileStru[10,3] = 1
  laFileStru[10,4] = 0

  laFileStru[11,1] = 'dTranDate'
  laFileStru[11,2] = 'D'
  laFileStru[11,3] = 8
  laFileStru[11,4] = 0

  laFileStru[12,1] = 'dPostDate'
  laFileStru[12,2] = 'D'
  laFileStru[12,3] = 8
  laFileStru[12,4] = 0

  laFileStru[13,1] = 'nCost'
  laFileStru[13,2] = 'N'
  laFileStru[13,3] = 9
  laFileStru[13,4] = 3

  laFileStru[14,1] = 'nUntCstBuy'
  laFileStru[14,2] = 'N'
  laFileStru[14,3] = 9
  laFileStru[14,4] = 3

  laFileStru[15,1] = 'nBalance'
  laFileStru[15,2] = 'N'
  laFileStru[15,3] = 12
  laFileStru[15,4] = 3

  laFileStru[16,1] = 'nReceived'
  laFileStru[16,2] = 'N'
  laFileStru[16,3] = 12
  laFileStru[16,4] = 3

  laFileStru[17,1] = 'nIssued'
  laFileStru[17,2] = 'N'
  laFileStru[17,3] = 12
  laFileStru[17,4] = 3

  laFileStru[18,1] = 'nApply'
  laFileStru[18,2] = 'N'
  laFileStru[18,3] = 12
  laFileStru[18,4] = 3

  laFileStru[19,1] = 'cMarker'
  laFileStru[19,2] = 'C'
  laFileStru[19,3] = 1
  laFileStru[19,4] = 0

  laFileStru[20,1] = 'lStatus'
  laFileStru[20,2] = 'C'
  laFileStru[20,3] = 1
  laFileStru[20,4] = 0

  laFileStru[21,1] = 'lNeeded'
  laFileStru[21,2] = 'L'
  laFileStru[21,3] = 0
  laFileStru[21,4] = 0

  laFileStru[22,1] = 'RolTranCd'
  laFileStru[22,2] = 'C'
  laFileStru[22,3] = 1
  laFileStru[22,4] = 0

  laFileStru[23,1] = 'LineNo'
  laFileStru[23,2] = 'N'
  laFileStru[23,3] = 6
  laFileStru[23,4] = 0

  laFileStru[24,1] = 'Style'
  laFileStru[24,2] = 'C'
  laFileStru[24,3] = 19
  laFileStru[24,4] = 0

  laFileStru[25,1] = 'nIssue'
  laFileStru[25,2] = 'N'
  laFileStru[25,3] = 6
  laFileStru[25,4] = 3

  laFileStru[26,1] = 'cMorder'
  laFileStru[26,2] = 'C'
  laFileStru[26,3] = 6
  laFileStru[26,4] = 0

  =gfCrtTmp(lcTmpRoll ,@laFileStru,@laTags)
ENDIF


IF !USED(lcFullRoll)
  DIMENSION laTagArr[2,2]
  laTagArr[1,1] = 'cRollID+Style+cWareCode+cDyelot+cRsession'
  laTagArr[1,2] = lcFullRoll
  
  laTagArr[2,1] = 'Style+cWareCode+cDyelot+STR(LineNo,6)'
  laTagArr[2,2] = 'lcFullRol2'

  =gfCrtTmp(lcFullRoll ,@laFileStru,@laTagArr)
ENDIF

IF !USED(lcTmpJour)
  lnConv = 1  
  LOCAL lnEngBehav
  lnEngBehav = SET("EngineBehavior")
  SET ENGINEBEHAVIOR 70
  SELECT cSession,cInvType,Style,cWareCode,cDyelot,cRSession,cISession,;
         cTrCode,cTrType,dTrDate,dPostDate,nCost As 'nCost',nUntCstBuy,;
         SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
         SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',;
         SUM(nStk5) AS 'nBal5',SUM(nStk6) AS 'nBal6',;
         SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8',;
         SUM(nTotStk) AS 'nBalance',;
         SUM(IIF(CIRTYPE='R',nTotStk,0))/lnConv AS 'nReceived',;
         SUM(IIF(CIRTYPE='I',ABS(nTotStk),0))/lnConv AS 'nIssued',;
         00000000.000 As 'nApply1',00000000.000 As 'nApply2',;
         00000000.000 As 'nApply3',00000000.000 As 'nApply4',;
         00000000.000 As 'nApply5',00000000.000 As 'nApply6',;
         00000000.000 As 'nApply7',00000000.000 As 'nApply8',;
         00000000.000 As 'nApply','' AS RolTranCd,00000000.000 As 'niSsue',;
         "S" AS lStatus ,.F. AS 'lNeeded' ,LineNo ,nPrvSQty,nPrvSVal  ;
  FROM   itemjrnl WHERE  cDyelot = '' GROUP BY cDyelot,cRSession HAVING nBalance <> 0;
  ORDER BY cDyelot,cRSession INTO DBF (oAriaApplication.workdir+lcTmpJour)
  
  SET ENGINEBEHAVIOR (lnEngBehav)
        
  SELECT (lcTmpJour)
  INDEX ON cInvType+Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour

ENDIF


ENDFUNC



*!*************************************************************
*! Name      : lfUpdateTables
*! Developer : Saber A.Razek (SAB)
*! Date      : 09/11/2012
*! Purpose   : Update Tables
*!*************************************************************
FUNCTION lfUpdateTables
*- Update ITEM Table

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('item',lcTranCode,SET("Datasession"),;
                      'CINVTYPE,STYLE','ITEM','STYLE')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  =TABLEREVERT(.T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*- Update ITEMLOC Table
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemloc',lcTranCode,SET("Datasession"),;
                      'CINVTYPE,STYLE,CWARECODE,DYELOT','ITEMLOC','STYDYE')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  =TABLEREVERT(.T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*- Update ITEMJRNL Table
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemjrnl',lcTranCode,SET("Datasession"),;
                      'CINVTYPE,STYLE,CWARECODE,DTRDATE,CSESSION,CIRTYPE,CTRCODE,LINENO,CTRTYPE,NLINENO','ITEMJRNL','STYDATE')  
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  =TABLEREVERT(.T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*- Update ITEMADJ Table
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate('itemadj',lcTranCode,SET("Datasession"),;
                                                                   'CSESSION,CINVTYPE,STYLE,CFROMWARE,DYELOT,LINENO','ITEMADJ','ITEMADJ')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  =TABLEREVERT(.T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF
  
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  llReturn = .F.
ENDIF
*! C201512,1 SAB 08/14/2012 Custom Auto Material Inventory Adjustment [End]