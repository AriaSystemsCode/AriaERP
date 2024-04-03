*:**********************************************************************
*: Program file       : POESTLND.PRG
*: Program description: ESTIMATED VS LANDED COSTS REPORT FOR GMA
*: Module             : Style Purchase Order (PO)
*: Developer          : Heba Fathi Khalaf Allah (HFK)
*: Tracking Job Number: N#038746
*: Date               : 11/01/2004
*:**********************************************************************
*: Calls:
*:         Programs       :
*:         Screens        :
*:         Global Function:
*:**********************************************************************
*: Called From:
*:**********************************************************************
*: Modification:
*: B128727,1 HFK 06/28/2005 Solve bug of not showing complete POs
*: B129291,1 HFK 08/03/2005 Solve bug of duplicated records in some shipments
*:**********************************************************************
*!

#INCLUDE R:\ARIA4XP\REPORTS\PO\POESTLND.H
loogScroll.cCROrientation = 'L'

lcSetting = SET('CENTURY')
SET CENTURY OFF

IF loOGScroll.llOGFltCh   &&If Filter Changed
  lcTmpFile  = loOGScroll.gfTempName()
  lcTmpFile1 = loOGScroll.gfTempName()
  lcTmpFile2 = loOGScroll.gfTempName()
  lcTmpFile3 = loOGScroll.gfTempName()
  lcTmpFile4 = loOGScroll.gfTempName()
  *-call function to create temp file.
  =lfCreateTmpFile()
  *- TAKE THE SQL EXPREESION AND GET FIELDS FROM PosHdr,POSLN,SHPMTHDR
  lcMainCursor = loOGScroll.gfTempName()

  IF EMPTY(lcBom)
    lcBom = loOGScroll.gfTempName()
    lcBomStatment = " Select TotCost,cItmMajor,Typ,cItmMask,MfgCode From Bom (INDEX=MULTIBOM) Where cInvType = 0001 "
		lnBomResult = loOGScroll.oRDA.SqlRun(lcBomStatment,lcBom,,oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("DATASESSION"))
    IF lnBomResult = 1
      SELECT &lcBom
      lnBuffering = CURSORGETPROP("Buffering","&lcBom")
      =CURSORSETPROP("Buffering",3,"&lcBom")
      *-- To initialize the indices that will be created for each file
      SELECT ("&lcBom")
      INDEX ON cItmMajor+Typ TAG &lcBom
      SET ORDER TO TAG &lcBom
      =CURSORSETPROP("Buffering",5,"&lcBom")
    ELSE
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnBomResult ,.T.)
      RETURN .F.
    ENDIF
  ENDIF


  lcCostCond = ""
  *- B128727,1 HFK  [Start]
  *!*    IF lcRpCost # '8'
  *!*      DO CASE
  *!*        CASE lcRpCost = '1'
  *!*          lcCostCond = " POSLN.NFCOST1 <> 0 "
  *!*        CASE lcRpCost = '2'
  *!*          lcCostCond = " POSLN.NFCOST2 <> 0 "
  *!*        CASE lcRpCost = '3'
  *!*          lcCostCond = " POSLN.NFCOST3 <> 0 "
  *!*        CASE lcRpCost = '4'
  *!*          lcCostCond = " POSLN.NFCOST4 <> 0 "
  *!*        CASE lcRpCost = '5'
  *!*          lcCostCond = " POSLN.NFCOST5 <> 0 "
  *!*        CASE lcRpCost = '6'
  *!*          lcCostCond = " POSLN.NFCOST6 <> 0 "
  *!*        OTHERWISE
  *!*          lcCostCond = " POSLN.NFCOST7 <> 0 "
  *!*      ENDCASE
  *!*    ENDIF
  *- B128727,1 HFK  [End]

  lcSelectStat = "SELECT PosHdr.PO,PosHdr.cFob,PosLn.cWareCode,PosLn.Vendor,PosLn.Style,Convert(Varchar(8),POSLN.DATE,1) AS DATE,PosLn.TotQty,PosLn.cRsession,PosLn.[LineNo],"
  lcSelectStat = lcSelectStat + "PosLn.NFLANCOST1,PosLn.NFLANCOST2,PosLn.NFLANCOST3,PosLn.NFLANCOST4,PosLn.NFLANCOST5,PosLn.NFLANCOST6,PosLn.NFLANCOST7,ShpmtHdr.ShipNo,ShpmtHdr.ShipVia FROM PosHdr "
  lcSelectStat = lcSelectStat + " (INDEX = POSHDR) INNER JOIN POSLN (INDEX = POSLNSH) ON PosHdr.CBUSDOCU+PosHdr.CSTYTYPE+PosHdr.PO = POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO "
  lcSelectStat = lcSelectStat + " LEFT OUTER JOIN SHPMTHDR (INDEX = SHPMTHDR) ON POSLN.SHIPNO = SHPMTHDR.SHIPNO "
  *-- B129291,1 HFK  [Start]
  *-- lcWhereCond  = " PosHdr.cStyType = 'P' AND PosHdr.CBUSDOCU = 'P' AND POSLN.TranCD = '2' AND SHPMTHDR.SHIPNO <> '  ' "
  lcWhereCond  = " PosHdr.cStyType = 'P' AND PosHdr.CBUSDOCU = 'P' AND POSLN.TranCD = '2' AND SHPMTHDR.SHIPNO <> '  ' And ShpmtHdr.cShpType = 'P' "
  *-- B129291,1 HFK  [Start]

  *- B128727,1 HFK [Start]
  *!*    IF !EMPTY(lcCostCond)
  *!*      lcWhereCond = lcWhereCond + " AND " + lcCostCond
  *!*    ENDIF
  *- B128727,1 HFK [End]


  *- add the status to the select statement condition
  StatCount =  LEN(lcRpStatus)
  IF StatCount <> 3
    lcStatusFilter = " POSHDR.STATUS IN ("
    lnCount = 1
    FOR I = 1 TO StatCount
      lcStatusFilter = IIF(lnCount = 1,lcStatusFilter + " '" + SUBSTR(lcRpStatus,I,1) + "' ",lcStatusFilter+ ",'" + SUBSTR(lcRpStatus,I,1) + "' ")
      lnCount = 2
    ENDFOR
    lcStatusFilter = lcStatusFilter + ")"
    lcWhereCond = lcWhereCond + " AND " + lcStatusFilter
  ENDIF

  *- add the sql expression to the select statement condition
  IF !EMPTY(loOGScroll.lcRpSqlExp)
    lcWhereCond = lcWhereCond + " AND " + loOGScroll.lcRpSqlExp
  ENDIF

  lcMainStatement = lcSelectStat + " WHERE " + lcWhereCond

  *- get data from Sql
  lnMainResult = loOGScroll.oRDA.SqlRun(lcMainStatement,lcMainCursor,,oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("DATASESSION"))
  IF lnMainResult = 1
    SELECT &lcMainCursor
    IF RECCOUNT() <> 0
      SELECT &lcMainCursor
      lnBuffering = CURSORGETPROP("Buffering","&lcMainCursor")
      =CURSORSETPROP("Buffering",3,"&lcMainCursor")
      *-- To initialize the indices that will be created for each file
      INDEX ON Style TAG &lcMainCursor
      SET ORDER TO TAG &lcMainCursor
      =CURSORSETPROP("Buffering",5,"&lcMainCursor")
      =lfCollectData()
    ENDIF
  ELSE  && Sql result is < 0
    =loOGScroll.oRDA.CheckRetResult("sqlrun",lnMainResult,.T.)
    RETURN .F.
  ENDIF
ENDIF

*-  Restore the century setting [Start]
SET CENTURY &lcSetting.
SELECT (lcTmpFile)
GO BOTTOM
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ELSE
  lcRecCount = LTRIM(STR(RECCOUNT(),7))
  *-  Change the century setting
  DO CASE
    CASE lcRpType = 'H'  && Shipment Summary report.
      SELECT (lcTmpFile)

     * N000682 ,1 Thabet Handle globalization issues [Start]
     *WAIT WINDOW 'Sorting &lcRecCount Records For Report' NOWAIT
	 WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Sorting,oAriaApplication.GetHeaderText("LANG_Sorting",AHEADERFILE))+' &lcRecCount '+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECORDS,oAriaApplication.GetHeaderText("LANG_RECORDS",AHEADERFILE))  NOWAIT
     * N000682 ,1 Thabet Handle globalization issues [END]
      SET ORDER TO (lcTmpFile1)
    CASE lcRpType = 'S'  && Style Detail report.
      SELECT (lcTmpFile)

  	* N000682 ,1 Thabet Handle globalization issues [Start]
  	*      WAIT WINDOW 'Sorting &lcRecCount Records For Report' NOWAIT
	  WAIT WINDOW  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Sorting,oAriaApplication.GetHeaderText("LANG_Sorting",AHEADERFILE))+' &lcRecCount '+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECORDS,oAriaApplication.GetHeaderText("LANG_RECORDS",AHEADERFILE))  NOWAIT
  	* N000682 ,1 Thabet Handle globalization issues [END]
      DO CASE
        CASE lcRpSort = 'V'
          SET ORDER TO (lcTmpFile2)
        CASE lcRpSort = 'P'
          SET ORDER TO (lcTmpFile3)
        CASE lcRpSort = 'F'
          SET ORDER TO (lcTmpFile4)
      ENDCASE

    OTHERWISE

	  * N000682 ,1 Thabet Handle globalization issues [Start]
	  *WAIT WINDOW 'Sorting &lcRecCount Records For Report' TIMEOUT 1
	  WAIT WINDOW  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Sorting,oAriaApplication.GetHeaderText("LANG_Sorting",AHEADERFILE))+' &lcRecCount '+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECORDS,oAriaApplication.GetHeaderText("LANG_RECORDS",AHEADERFILE)) TIMEOUT 1
  	* N000682 ,1 Thabet Handle globalization issues [END]

  ENDCASE

ENDIF
LOCATE

*--- Start printing report
DO gfDispRe WITH EVAL('lcRpForm')
SET DEVICE TO SCREEN
*--end of Main Programe.
*!
*!*************************************************************
*! Name      : lfCollectData
*! Developer : AHMED MAHER
*! Date      : 09/23/2001
*! Purpose   : Scan the Posln file
*!*************************************************************
*! Passed Parameters  : lcWhileExp = the while expration part in the scan loop
*!                      lcForExp   = the for expration part in the scan loop
*!                      llShipLoop = loop for not selected shipvia of shipment
*!                      llPosHLoop = loop for not selected status of po
*!                      llStylLoop = loop for not selected style group of style
*!*************************************************************
*! Returns            : none
*!*************************************************************
*!
FUNCTION lfCollectData

loStyle =CreateObject("RemoteTable","STYLE","STYLE","STYLER",SET("Datasession"))
loDeptHd = CreateObject("RemoteTable","ICDEPTHD","DEPTHD","ICDEPTHDR",SET("Datasession"))
loDeptDt = CreateObject("RemoteTable","ICDEPTDT","DEPTDTS","ICDEPTDTR",SET("Datasession"))

lnStyleExp = ASCAN(loOGScroll.laFltExp,'STYLE')
IF lnStyleExp > 0
  lcStyleExp = loOGScroll.laFltExp[ASUBSCRIPT(loOGScroll.laFltExp,lnStyleExp,1),2]
ELSE
  lcStyleExp = ""
ENDIF
llGroup = ATC('STYLE.CSTYGROUP',lcStyleExp)>0
*-- Check for style groups selected in style expression
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

llStyMajor = ATC('STYLE.CSTYMAJOR',lcStyleExp)> 0
IF llStyMajor
  lnStyMajorCnd = ASUBSCRIPT(loOGScroll.laOgFxFlt,ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYMAJOR'),1)
  lcStyMajorFile = loOGScroll.laOgFxFlt[lnStyMajorCnd,6]
  IF !EMPTY(lcStyMajorFile)
    SELECT &lcStyMajorFile
    INDEX ON cStyMajor TAG &lcStyMajorFile
  ENDIF
ENDIF

SELECT &lcMainCursor
SCAN
  lcStyleID = Style
  IF loStyle.Seek(lcStyleID)
    IF llGroup
      IF !SEEK(StyleR.cStyGroup,lcGroupFile)
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
    DO lpAddNew
    SELECT &lcMainCursor
    IF lcRpCost = '4' .AND. lcRpMfgo # 'TOTALZ'
      IF EMPTY(lcBomLine)
        lcBomLine     = loOGScroll.gfTempName()
        lcBomLineFlds = "Select cType,cTktNo,[LineNo],Style,CRSESSION From BomLine (INDEX = BOMLINE)"
        lcWhereCond   = " cImTyp = 'I' AND cType IN ('1','2') AND cBomTyp = '4' AND cInvType = '0001' AND MfgCode = '" + lcRpMfgo + "'"
        lcBomLineStat = lcBomLineFlds + " WHERE " + lcWhereCond
        lnBomLineResult = loOGScroll.oRDA.SqlRun(lcBomLineStat,lcBomLine,,oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("DATASESSION"))
        IF lnBomLineResult = 1
          SELECT &lcBomLine
          lnBuffering = CURSORGETPROP("Buffering","&lcBomLine")
          =CURSORSETPROP("Buffering",3,"&lcBomLine")
          *-- To initialize the indices that will be created for each file
          SELECT ("&lcBomLine")
          INDEX ON cType+cTktNo+STR(LINENO,6)+STYLE TAG &lcBomLine
          SET ORDER TO TAG &lcBomLine
          =CURSORSETPROP("Buffering",5,"&lcBomLine")
        ELSE
          =loOGScroll.oRDA.CheckRetResult("sqlrun",lnBomLineResult,.T.)
          RETURN .F.
        ENDIF
      ENDIF
      SELECT &lcMainCursor
      IF SEEK('1'+&lcMainCursor..PO+STR(&lcMainCursor..LINENO,6)+&lcMainCursor..STYLE,'&lcBomLine')
        SELECT &lcBomLine
        SCAN REST WHILE cType+cTktNo+STR(LineNo,6)+Style =;
                        '1'+&lcMainCursor.PO+STR(&lcMainCursor..LINENO,6)+&lcMainCursor..STYLE
          SELECT (lcTmpFile)
          IF lFirst
            REPLACE &lcTmpFile..NLNDCOST WITH 0,;
                    &lcTmpFile..LFIRST   WITH .F.
          ENDIF
          REPLACE &lcTmpFile..NLNDCOST WITH &lcTmpFile..NLNDCOST + (&lcBomLine..UNITCOST*IIF(lcRpType='H',&lcMainCursor..TOTQTY,1))
        ENDSCAN
        SELECT (lcTmpFile)
        IF lFirst .AND. lcRpType # 'H'
          DELETE
          LOOP
        ENDIF
        SELECT &lcBomLine
        IF SEEK('2'+&lcMainCursor..PO+STR(&lcMainCursor..LINENO,6)+&lcMainCursor.STYLE)
          SCAN REST WHILE cType+cTktNo+STR(LineNo,6)+Style =;
                          '2'+&lcMainCursor..PO+STR(&lcMainCursor..LINENO,6)+POSLN.STYLE;
                    FOR CRSESSION = &lcMainCursor..CRSESSION
            SELECT (lcTmpFile)
            IF lFirstLnd
              REPLACE NLNDCOST  WITH 0,;
                      LFIRSTLND WITH .F.
            ENDIF
            REPLACE NLNDCOST WITH NLNDCOST + (BOMLINE.UNITCOST*IIF(lcRpType='H',POSLN.TOTQTY,1))
          ENDSCAN
        ENDIF
        IF lcRpType = 'H'
          SELECT (lcTmpFile)
          REPLACE LFIRSTLND WITH .F.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
*--end of lfCollectData.
*!
*!************************************************************
*! Name      : lpAddNew
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/19/2001
*! Purpose   : Add new record to the timp table.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*!
PROCEDURE lpAddNew

lcShipNo = IIF(ISNULL(&lcMainCursor..ShipNo),' ',&lcMainCursor..ShipNo)
IF loDeptDt.Seek(StyleR.Style)
  loDeptHd.Seek(ICDEPTDTR.Dept)
  lcDepartment = ICDEPTDTR.Dept
  lcDeptDesc = IcDeptHdR.cDeptDesc
ELSE
  lcDepartment = ""
  lcDeptDesc = ""
ENDIF
lcShipVia = IIF(ISNULL(&lcMainCursor..SHIPVIA),' ',&lcMainCursor..SHIPVIA)

IF llFrstTime = .F.
  lnDateFilterPos = ASCAN(loOGScroll.laOGFxFlt,"POSLN.DATE")
  *-if filter exists
  IF lnDateFilterPos > 0
    lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDateFilterPos,1)
    lcDateExpression = loOGScroll.laOGFxFlt[lnRow,6]
    IF !EMPTY(lcDateExpression)
      lnPosition = ATC("|",lcDateExpression)
      lnLength   = LEN(lcDateExpression)-lnPosition
      lcFromDate = SUBSTR(lcDateExpression,1,lnPosition-1)
      lcToDate   = SUBSTR(lcDateExpression,lnPosition+1,lnLength)
    ELSE
      lcFromDate = " / / "
      lcToDate   = " / / "
    ENDIF
  ENDIF
  llFrstTime = .T.
ENDIF

SELECT (lcTmpFile)
*-- if report format is shipment summary and this shipment number is found;
*-- do nothing, else append blank.
IF !(lcRpType = 'H' .AND. SEEK(lcShipNo))  &&SHIPMENT SUMMARY
  APPEND BLANK
  REPLACE LFIRST    WITH .T.,;
          LFIRSTLND WITH .T.
ENDIF
REPLACE &lcTmpFile..cShipNo   WITH lcShipNo,;
        &lcTmpFile..CWARECODE WITH &lcMainCursor..CWARECODE,;
        &lcTmpFile..CFOB      WITH &lcMainCursor..CFOB,;
        &lcTmpFile..CVENDOR   WITH &lcMainCursor..VENDOR,;
        &lcTmpFile..CDESC     WITH IIF(lcRpType = 'S',StyleR.DESC+' ','') + StyleR.DESC1,;
        &lcTmpFile..CDEPT     WITH lcDepartment + IIF(lcRpType = 'S',' '+lcDeptDesc,''),;
        &lcTmpFile..CSTYGROUP WITH StyleR.CSTYGROUP + IIF(lcRpType = 'S',' '+gfCodDes(StyleR.CSTYGROUP,'CSTYGROUP'),''),;
        &lcTmpFile..CPO       WITH &lcMainCursor..PO,;
        &lcTmpFile..CSTYLE    WITH StyleR.CSTYMAJOR
REPLACE &lcTmpFile..cColor    WITH ALLTRIM(SUBSTR(&lcMainCursor..Style,lnMajorLen+2)),;
        &lcTmpFile..dReceived WITH CTOD(&lcMainCursor..Date),;
        &lctmpFile..cShipVia  WITH lcShipVia,;
        &lcTmpFile..dLastRecv WITH CTOD(&lcMainCursor..Date),;
        &lcTmpFile..cDateFrom WITH lcFromDate,;
        &lcTmpFile..cDateTo   WITH lcToDate
REPLACE &lcTmpFile..NTOTQTY   WITH IIF(lcRpType='H',NTOTQTY,0)+&lcMainCursor..TOTQTY,;
        &lcTmpFile..NSTYCOST  WITH IIF(lcRpType='H',NSTYCOST,0)+lfGtStyCst(lcRpCost),;
        &lcTmpFile..NLNDCOST  WITH IIF(lcRpType='H',NLNDCOST,0)+lfGetCost('NFLANCOST',lcRpCost)

*--end of lpAddNew.
*!
*!*************************************************************
*! Name      : lfGtStyCst
*! Developer : AHMED MAHER
*! Date      : 09/19/2001
*! Purpose   : Get the estimated cost
*!*************************************************************
*! Passed Parameters  : lcType  = cost element type
*!*************************************************************
*! Returns            : cost value
*!*************************************************************
*!
FUNCTION lfGtStyCst
PARAMETERS lcType
PRIVATE lnCostVal, lnAlias

lnCostVal = 0
lnAlias = SELECT(0)
lcType = IIF(lcType='8','',lcType)

IF SEEK(StyleR.CSTYMAJOR+lcType,'&lcBom')
  SELECT &lcBom
  SUM TOTCOST REST WHILE cItmMajor+Typ = StyleR.CSTYMAJOR+lcType;
              FOR (CITMMASK = &lcMainCursor..STYLE .OR. ALLTRIM(SUBSTR(CITMMASK,lnMajorLen+2)) = '******');
                  .AND. IIF(lcType = '4' .AND. lcRpMfgo # 'TOTALZ',MFGCODE=lcRpMfgo,.T.);
                  TO lnCostVal
ENDIF

SELECT (lnAlias)
lnCostVal = lnCostVal*IIF(lcRpType='H',&lcMainCursor..TOTQTY,1)
RETURN lnCostVal
*-- End of lfGtStyCst.
*!
*!*************************************************************
*! Name      : lfGetCost
*! Developer : AHMED MAHER
*! Date      : 09/19/2001
*! Purpose   : Get the estimated/landed cost
*!*************************************************************
*! Passed Parameters  : lcField = estimated/landed
*!                      lcType  = cost element type
*!*************************************************************
*! Returns            : cost value
*!*************************************************************
*!
FUNCTION lfGetCost
PARAMETERS lcField, lcType
PRIVATE lnCostVal
lnCostVal = 0
IF lcType = '8'
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    lnCostVal = lnCostVal + EVALUATE('&lcMainCursor..'+lcField+lcI)
  ENDFOR
ELSE
  lnCostVal = EVALUATE('&lcMainCursor..'+lcField+lcType)
ENDIF
lnCostVal = lnCostVal*IIF(lcRpType='H',&lcMainCursor..TOTQTY,1)
RETURN lnCostVal
*-- End of lfGetCost.
*!
*!*************************************************************
*! Name      : lfvRepType
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/24/2001
*! Purpose   : Validate lcRpCost option
*!*************************************************************
*!
FUNCTION lfvRepType
DO CASE
  CASE lcRpType = 'D'  && Shipment Detail report.
    lcRpForm = 'POESTLND'
  CASE lcRpType = 'H'  && Shipment Summary report.
    lcRpForm = 'POESTLN1'
  CASE lcRpType = 'S'  && Style Detail report.
    lcRpForm = 'POESTLN2'
ENDCASE
ClearRead()
*-- end lfvRepType.
*!
*!************************************************************
*! Name      : RefreshStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : function to Set the index for the SQL files
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
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
*!
*!
*!*************************************************************
*! Name      : lfFillCost
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Fill the two arrays of cost items.
*!*************************************************************
*!
FUNCTION lfFillCost

DECLARE laCostDesc[8,1], laCostVal[8,1]
DECLARE laMainSetp[15,2]

FOR lnCount = 1 TO 7
  lcCount = STR(lnCount,1)
  laMainSetp[lnCount,1] = 'M_CISLBL'+lcCount
ENDFOR

=gfGetMemVar(@laMainSetp,oAriaApplication.ActiveCompanyID)

FOR lnCount = 1 TO 7
  lcCount = STR(lnCount,1)
  laCostDesc[lnCount,1] = laMainSetp[lnCount,2]
  laCostVal[lnCount,1] = lcCount
ENDFOR
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCostDesc[8,1] = LANG_POEstLnd_Total
laCostDesc[8,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Total,oAriaApplication.GetHeaderText("LANG_POEstLnd_Total",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laCostVal[8,1]  = '8'
*-- end lfFillCost.
*!
*!*************************************************************
*! Name      : lfFillMfgo
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Fill the two arrays of Mfgo.
*!*************************************************************
*!
FUNCTION lfFillMfgo

lnNo = 1
DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laMfgoDesc[lnNo,1] = LANG_POEstLnd_TotalMisc
laMfgoDesc[lnNo,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_TotalMisc,oAriaApplication.GetHeaderText("LANG_POEstLnd_TotalMisc",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laMfgoCode[lnNo,1] = 'TOTALZ'
SELECT CODES
IF SEEK('DMFGCODE')
  lnNo = lnNo + 1
  DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
  laMfgoDesc[lnNo,1] = cCode_No + ' - ' + cDiscrep
  laMfgoCode[lnNo,1] = cCode_No
  IF SEEK('NMFGCODE')
    SELECT CODES
    SCAN REST WHILE cDefCode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'NMFGCODE';
              FOR CRLTFIELD = 'N' .AND. CCODE_NO <> laMfgoCode[2,1]
      lnNo = lnNo + 1
      DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
      laMfgoDesc[lnNo,1] = cCode_No + ' - ' + cDiscrep
      laMfgoCode[lnNo,1] = cCode_No
    ENDSCAN
  ENDIF
ENDIF
*-- end lfFillMfgo.
*!
*!*************************************************************
*! Name      : lfvCost
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Validate lcRpCost option
*!*************************************************************
*!
FUNCTION lfvCost
ClearRead()
*-- end lfvCost.
*!
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MAHER
*! Date      : 09/18/2001
*! Purpose   : When function of the option grid.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwOGWhen
DECLARE laRpSource[3] , laRpTarget[1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_POEstLnd_Open     TO laRpSource[1]
*STORE LANG_POEstLnd_Complete TO laRpSource[2]
*STORE LANG_POEstLnd_Closed   TO laRpSource[3]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Open,oAriaApplication.GetHeaderText("LANG_POEstLnd_Open",AHEADERFILE))     TO laRpSource[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Complete,oAriaApplication.GetHeaderText("LANG_POEstLnd_Complete",AHEADERFILE)) TO laRpSource[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Closed,oAriaApplication.GetHeaderText("LANG_POEstLnd_Closed",AHEADERFILE))   TO laRpSource[3]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*--end lfwOGWhen.
*!
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : AHMED MAHER
*! Date      : 09/18/2001
*! Purpose   : Evaluate Status expression.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*!
FUNCTION lfvOStatus
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,LANG_POEstLnd_MoverMsg,.T.,'')
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_MoverMsg,oAriaApplication.GetHeaderText("LANG_POEstLnd_MoverMsg",AHEADERFILE)),.T.,'')
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcRpStatus = ' '

*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open'        ,'O',;
*!*	                              IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
*!*	                              IIF(laRpTarget[lnI] = 'Complete','C','')))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Open,oAriaApplication.GetHeaderText("LANG_POEstLnd_Open",AHEADERFILE))        ,'O',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Closed,oAriaApplication.GetHeaderText("LANG_POEstLnd_Closed",AHEADERFILE))  ,'S',;
							  IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POEstLnd_Complete,oAriaApplication.GetHeaderText("LANG_POEstLnd_Complete",AHEADERFILE)),'C','')))
    * N000682 ,1 Thabet Handle globalization issues [End]
  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
lcRpStatus = IIF(EMPTY(lcRpStatus),'OSC',ALLTRIM(lcRpStatus))
*-- End of lfvOStatus.

*!
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MAHER
*! Date      : 09/19/2001, C#102413
*! Purpose   : Rise change style flag, in range browse screen..
*!*************************************************************
*! Passed Parameters  : lcParm
*!*************************************************************
*! Returns            :
*!*************************************************************
*!
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
ENDCASE
*-- end of lfsrvSty.
*!
*!*************************************************************
*! Name      : lfStySum
*! Developer : AHMED MAHER
*! Date      : 09/19/2001, C#102413
*! Purpose   : sum a specific field for the current style in style file.
*!*************************************************************
*! Passed Parameters  : lcSty, lcComp, lnAddToVar
*!*************************************************************
*! Returns            :
*!*************************************************************
*!
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
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
*! Name      : lfsrvSty
*! Developer : AHMED MAHER
*! Date      : 09/19/2001, C#102413
*! Purpose   : get cursor from style to make join in sql and to
*!             get style information.
*!*************************************************************
*! Passed Parameters  : lcParm
*!*************************************************************
*! Returns            :
*!*************************************************************
*!
FUNCTION lfCreateTmpFile

DIMENSION laTmpFile[20,4]

laTmpFile[1,1]= 'CSHIPNO'
laTmpFile[1,2]= 'C'
laTmpFile[1,3]= 6
laTmpFile[1,4]= 0

laTmpFile[2,1]= 'CWARECODE'
laTmpFile[2,2]= 'C'
laTmpFile[2,3]= 6
laTmpFile[2,4]= 0

laTmpFile[3,1]= 'dReceived'
laTmpFile[3,2]= 'D'
laTmpFile[3,3]= 0
laTmpFile[3,4]= 0

laTmpFile[4,1]= 'CFOB'
laTmpFile[4,2]= 'C'
laTmpFile[4,3]= 18
laTmpFile[4,4]= 0

laTmpFile[5,1]= 'CVENDOR'
laTmpFile[5,2]= 'C'
laTmpFile[5,3]= 8
laTmpFile[5,4]= 0

laTmpFile[6,1]= 'CDESC'
laTmpFile[6,2]= 'C'
laTmpFile[6,3]= 81
laTmpFile[6,4]= 0

laTmpFile[7,1]= 'CDEPT'
laTmpFile[7,2]= 'C'
laTmpFile[7,3]= 36
laTmpFile[7,4]= 0

laTmpFile[8,1]= 'CSTYGROUP'
laTmpFile[8,2]= 'C'
laTmpFile[8,3]= 37
laTmpFile[8,4]= 0

laTmpFile[9,1]= 'CPO'
laTmpFile[9,2]= 'C'
laTmpFile[9,3]= 6
laTmpFile[9,4]= 0

laTmpFile[10,1]= 'CSTYLE'
laTmpFile[10,2]= 'C'
laTmpFile[10,3]= 19
laTmpFile[10,4]= 0

laTmpFile[11,1]= 'CCOLOR'
laTmpFile[11,2]= 'C'
laTmpFile[11,3]= 6
laTmpFile[11,4]= 0

laTmpFile[12,1]= 'NTOTQTY'
laTmpFile[12,2]= 'N'
laTmpFile[12,3]= 7
laTmpFile[12,4]= 0

laTmpFile[13,1]= 'NSTYCOST'
laTmpFile[13,2]= 'N'
laTmpFile[13,3]= 13
laTmpFile[13,4]= 3

laTmpFile[14,1]= 'NLNDCOST'
laTmpFile[14,2]= 'N'
laTmpFile[14,3]= 13
laTmpFile[14,4]= 3

laTmpFile[15,1]= 'CSHIPVIA'
laTmpFile[15,2]= 'C'
laTmpFile[15,3]= 6
laTmpFile[15,4]= 0

laTmpFile[16,1]= 'dLastRecv'
laTmpFile[16,2]= 'D'
laTmpFile[16,3]= 0
laTmpFile[16,4]= 0

laTmpFile[17,1]= 'CDATEFROM'
laTmpFile[17,2]= 'C'
laTmpFile[17,3]= 10
laTmpFile[17,4]= 0

laTmpFile[18,1]= 'CDATETO'
laTmpFile[18,2]= 'C'
laTmpFile[18,3]= 10
laTmpFile[18,4]= 0

laTmpFile[19,1]= 'LFIRST'
laTmpFile[19,2]= 'L'
laTmpFile[19,3]= 0
laTmpFile[19,4]= 0

laTmpFile[20,1]= 'LFIRSTLND'
laTmpFile[20,2]= 'L'
laTmpFile[20,3]= 0
laTmpFile[20,4]= 0

gfCrtTmp(lcTmpFile,@laTmpFile,"CSHIPNO+CSTYLE+CCOLOR",lcTmpFile)

SELECT &lcTmpFile
*-CREATE 5 DIFFERENT INDEX NAMES.
*-INDEX ON CSHIPNO+CSTYLE+CCOLOR TAG (lcTmpFile)  OF (lcTmpFile)
INDEX ON CSHIPVIA+CSHIPNO      TAG (lcTmpFile1) OF (lcTmpFile)
INDEX ON CSTYLE+CVENDOR        TAG (lcTmpFile2) OF (lcTmpFile)
INDEX ON CSTYLE+CPO            TAG (lcTmpFile3) OF (lcTmpFile)
INDEX ON CSTYLE+CFOB           TAG (lcTmpFile4) OF (lcTmpFile)
SELECT (lcTmpFile)
SET ORDER TO (lcTmpFile)

