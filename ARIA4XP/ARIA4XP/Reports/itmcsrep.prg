*:**********************************************************************
*: Program file        : ITMCSREP
*: Program description : Style Cost Sheet Report
*: Module              : Style Purchase Order (PO, MF AND MA)
*: Developer           : Heba Fathi (HFK)
*: Tracking Job Number : #037710
*: Date                : 07/21/2004
*:**********************************************************************
*: Calls 
*:      Programs       : None
*:      Screens        : 
*:      Global Function: gfModalGen,gfCodDes,gfGetMemVar,gfUserPriv,gfItemMask
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*: Example: DO ITMCSREP
*:**********************************************************************
*: Modifications : modify the cost sheet browse and merge it with style browse 
*: HFK, B038982, 02/01/2005 : Fix Bug of selecting more than 24 items in 
*: Season and style groups
*: B999999,1 MMT 04/10/2005,Fix problem of repeating materials And change Fabric filter
*: E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002]
*: E303189,1 MMT 07/03/2012 Add option to Style cost sheet report to show non-costing or not[T20120425.0002]   
*: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002]   
*: B610710,1 TMI 04/09/2014 fix problem that Style Cost Sheet print only prints the first cost sheet [T20140326.0002 ] 
*:**********************************************************************
*-- Define Shared Variables --*
*-
#INCLUDE R:\ARIA4XP\REPORTS\ITMCSREP.H

LOCAL lcWhereCnd
LOCAL lnResult
STORE 0 TO lnResult 

*-Define Variables depending on the module
IF loOGScroll.llOGFltCh   &&If Filter Changed
  STORE .F. TO llClearFn
  STORE "" TO lcWhereCnd

  *- Call function to build where condition
  lcWhereCnd = lfBldWhereCond('lcWhereCnd')

  DO CASE
    CASE oAriaApplication.ActiveModuleID = 'MA'  
      lcStyTitle = " Item - Color "

    CASE oAriaApplication.ActiveModuleID = 'PO'  
      llChStyClr = .F.  && change color/style or color selection flag
      llChFabric = .F.  && change fabric filter flag.
      llChStyle  = .F.  && change style filter flag.
      *-- if first time you (preview or run ) or user change critria.
      STORE SPACE(0) TO lcCurCod
      STORE 0 TO lnETotCost,lnLinTot
      **-- open needed files --**
      =gfOpenFile(oAriaApplication.SysPath+'SYCCURR','Ccurrcode','SH')
      lcRpFormat = lcRpStyClr

    CASE oAriaApplication.ActiveModuleID = 'MF'
      llChStyClr = .F.  && change color/style or color selection flag
      llChFabric = .F.  && change fabric filter flag.
      llChStyle  = .F.  && change style filter flag.
      *-- if first time you (preview or run ) or user change critria.
      lcRpFormat = lcRpStyClr
  ENDCASE

  *-- start defining variables for Sql statement 
  lcGetData = loOGScroll.gfTempName()
  lcSelFields = " BOMHEADR.CITMMAJOR , BOMHEADR.CCSTSHT_ID , BOMHEADR.CCSTSHTDSC "
  lcSelFields = lcSelFields + " ,BOMHEADR.LDEFCSTSHT , BOMHEADR.CSTATUS , BOMHEADR.LBASONSIZ "
  lcSelFields = lcSelFields + " ,BOMHEADR.CCSTSHTTYP , BOMHEADR.CINVTYPE , BOMHEADR.NCOST1"
  lcSelFields = lcSelFields + " ,BOMHEADR.NCOST2 , BOMHEADR.NCOST3 , BOMHEADR.NCOST4 "
  lcSelFields = lcSelFields + " ,BOMHEADR.NCOST5 , BOMHEADR.NCOST6 , BOMHEADR.NCOST7"
  lcSelFields = lcSelFields + " ,BOM.CITMMASK , BOM.MSIZES , BOM.MSZCROSREF,BOM.[DESC] AS DESCRPTION"
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][BEGIN]
  *lcSelFields = lcSelFields + " ,BOM.NBOMTOTQTY , BOM.NLINENO, BOM.TRIM_INVT, BOM.TYP,BOM.MFGCODE , BOM.TOTCOST "
   lcSelFields = lcSelFields + " ,BOM.NBOMTOTQTY , BOM.NLINENO, BOM.TRIM_INVT, BOM.TYP,BOM.MFGCODE , BOM.TOTCOST, BOM.CCURRCODE , BOM.NEXRATE " 
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][END]
  *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][Start]
  lcSelFields = lcSelFields + " ,lhidenln"
  *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][END]
  lcSelFields = lcSelFields + " ,BOM.UNTCOST,BOM.cCatgTyp , UOM.CUOM_V AS UOM ,BOM.ITEM , BOM.LMATERIAL, ITEM.MAKE"
  lcSelFields = lcSelFields + " ,ITEM.VENDOR ,ITEM.CINVTYPE , ITEM.STYLE ,ITEM.CSTYMAJOR AS FABRIC "
  lcSelFields = lcSelFields + " ,ITEM.[DESC] ,ITEM.ITEM_TYPE , ITEM.PATTERN " 

  lcStatmnt = " BOMHEADR (INDEX  = BOMHEADR) INNER JOIN BOM (INDEX = MULTIBOM) ON BOMHEADR.CITMMAJOR+BOMHEADR.CCSTSHT_ID+BOMHEADR.CCSTSHTTYP = BOM.CITMMAJOR+BOM.CCSTSHT_ID+BOM.CCSTSHTTYP "
  lcStatmnt = lcStatmnt + " LEFT OUTER JOIN ITEM (INDEX = CSTYLE) ON BOM.CITMMAJOR = ITEM.CSTYMAJOR LEFT OUTER JOIN UOM (INDEX = UOM ) ON BOM.CUOMCODE = UOM.CUOMCODE " 

  IF !EMPTY(lcSqlCost)
  *--MMT
 *   lcStatmnt = lcStatmnt + " INNER JOIN " + lcSqlCost + " TmpCost ON BomHeadr.cItmMajor+'-'+BomHeadr.cCstSht_id = TmpCost.cCostSht "
    lcStatmnt = lcStatmnt + " INNER JOIN " + lcSqlCost + " TmpCost ON BomHeadr.cItmMajor = TmpCost.cCostSht "
    *--MMT
  ENDIF 

  lcSqlStat = "Select " + lcSelFields + " FROM " + lcStatmnt + " WHERE " + lcWhereCnd 
  lnResult = loOGScroll.oRDA.SqlRun(lcSqlStat,lcGetData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnResult > 0
    SELECT &lcGetData
    IF oAriaApplication.ActiveModuleID <> 'MA'  
      *- Call function to get style file fields needed.
      =lfGetStyleCursor()
      lnAlias = SELECT(0)
      SELECT Style_A
      INDEX ON style TAG TmpStyle
      IF RECCOUNT ()= 0
        =gfModalGen('TRM00052B00000','DIALOG')
        llNoRec = .T.
        USE IN Style_A
        RETURN .F.
      ENDIF 
      SELECT (lnAlias)
    ENDIF 
    SELECT &lcGetData
    =lfCrtTmp('F')
    lnBuffering = CURSORGETPROP("Buffering","&lcGetData")
    =CURSORSETPROP("Buffering",3,"&lcGetData")
    SELECT &lcGetData
    INDEX ON CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM TAG &lcGetData
    SET ORDER TO TAG &lcGetData
    SELECT (WORKFILE)
    INDEX ON cItmMajor + cSeekStyle + Typ + cItmMask + MfgCode + FABRIC + IClr TAG &WORKFILE
    IF oAriaApplication.ActiveModuleID <> 'MA' 
      IF lcRpStyClr='C'
        DO lpStyClr           && PRINT BY COLOR
      ELSE
        DO lpStyOnly          && PRINT FOR ALL COLORS
      ENDIF           
    ELSE
      *-- Function to build the report's work file.
      =lfBuilForm(lcRpForm)
      FOR lnCount = 5 TO 7
        lcCount  = STR(lnCount,1)
        lcSlbl&lcCount = " "
      ENDFOR 
      
    ENDIF 
    
    IF RECCOUNT(WORKFILE) = 0 
      IF llNoRec = .F.
        =gfModalGen('TRM00052B00000','DIALOG')
      ENDIF 
      RETURN .F.
    ELSE 
      DO CASE
        CASE oAriaApplication.ActiveModuleID = 'MA'
          lcRptName = LANG_ITMCSREP_FabCstSht
        OTHERWISE
          lcRptName = LANG_ITMCSREP_StyCstSht
      ENDCASE
      IF lcRpFormat='C' 
        lcRpForm  = 'ITMCSREPA'

        DECLARE loOGScroll.laCRTables[1]
        loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  WorkFile  + ".DBF"
        DECLARE loOGScroll.laCRParams[12,2]
        loOGScroll.laCRParams[1,1] = 'ReportName'
        loOGScroll.laCRParams[1,2] = lcRptName
        loOGScroll.laCRParams[2,1] = 'LayOut'
        loOGScroll.laCRParams[2,2] = IIF(oAriaApplication.ActiveModuleID = 'MA',LANG_ITMCSREP_FabClr,LANG_ITMCSREP_StyClr)
        loOGScroll.laCRParams[3,1] = 'StyTitle'
        loOGScroll.laCRParams[3,2] = lcStyTitle
        loOGScroll.laCRParams[4,1] = 'Label1'
        loOGScroll.laCRParams[4,2] = lcSlbl1
        loOGScroll.laCRParams[5,1] = 'Label2'
        loOGScroll.laCRParams[5,2] = lcSlbl2
        loOGScroll.laCRParams[6,1] = 'Label3'
        loOGScroll.laCRParams[6,2] = lcSlbl3
        loOGScroll.laCRParams[7,1] = 'Label4'
        loOGScroll.laCRParams[7,2] = lcSlbl4
        loOGScroll.laCRParams[8,1] = 'Label5'
        loOGScroll.laCRParams[8,2] = lcSlbl5
        loOGScroll.laCRParams[9,1] = 'Label6'
        loOGScroll.laCRParams[9,2] = lcSlbl6
        loOGScroll.laCRParams[10,1] = 'Label7'
        loOGScroll.laCRParams[10,2] = lcSlbl7

        loOGScroll.laCRParams[11,1] = 'MultCur'
        loOGScroll.laCRParams[11,2] = IIF(llMulCurr,'T','F')
        loOGScroll.laCRParams[12,1] = 'Module'
        loOGScroll.laCRParams[12,2] = oAriaApplication.ActiveModuleID
        
        

        lnSelect = SELECT()
        SELECT &WorkFile
        LOCATE 
        SELECT (lnSelect)
        USE IN &WorkFile
      ELSE
        lcRpForm  = 'ITMCSREPB'
        DECLARE loOGScroll.laCRTables[2]
        loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcBomHdr  + ".DBF"
        loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  WorkFile  + ".DBF"

        DECLARE loOGScroll.laCRParams[11,2]
        loOGScroll.laCRParams[1,1] = 'ReportName'
        loOGScroll.laCRParams[1,2] = lcRptName
        loOGScroll.laCRParams[2,1] = 'LayOut'
        loOGScroll.laCRParams[2,2] = IIF(oAriaApplication.ActiveModuleID = 'MA',LANG_ITMCSREP_FabOnly,LANG_ITMCSREP_StyOnly)
        loOGScroll.laCRParams[3,1] = 'Label1'
        loOGScroll.laCRParams[3,2] = lcSlbl1
        loOGScroll.laCRParams[4,1] = 'Label2'
        loOGScroll.laCRParams[4,2] = lcSlbl2
        loOGScroll.laCRParams[5,1] = 'Label3'
        loOGScroll.laCRParams[5,2] = lcSlbl3
        loOGScroll.laCRParams[6,1] = 'Label4'
        loOGScroll.laCRParams[6,2] = lcSlbl4
        loOGScroll.laCRParams[7,1] = 'Label5'
        loOGScroll.laCRParams[7,2] = lcSlbl5
        loOGScroll.laCRParams[8,1] = 'Label6'
        loOGScroll.laCRParams[8,2] = lcSlbl6
        loOGScroll.laCRParams[9,1] = 'Label7'
        loOGScroll.laCRParams[9,2] = lcSlbl7

        loOGScroll.laCRParams[10,1] = 'Module'
        loOGScroll.laCRParams[10,2] = oAriaApplication.ActiveModuleID
        loOGScroll.laCRParams[11,1] = 'MultCur'
        loOGScroll.laCRParams[11,2] = IIF(llMulCurr,'T','F')
        
        loOGScroll.cCRPaperSize = 'A4'
        lnSelect = SELECT()
        SELECT &lcBomHdr
        SET ORDER TO TAG &lcBomHdr
        LOCATE 
        SELECT &WorkFile
        SET RELATION TO 
        LOCATE 
        SET RELATION TO &WORKFILE..CITMMAJOR INTO &lcBomHdr
        SELECT (lnSelect)
        USE IN &WorkFile
        USE IN &lcBomHdr
      ENDIF 
      WAIT CLEAR
      loOgScroll.lcOGLastForm = lcRpForm
      llClearFn = .T.
      loogScroll.cCROrientation = 'P'
      =gfDispRe()
    ENDIF 
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
    RETURN .F.
  ENDIF 
ELSE  && Filter Did Not Change
  IF llClearFn = .T. && Run Preview Twice
    lcRpFormat = lcRpStyClr
    lcPath = oAriaApplication.WorkDir
    IF lcRpFormat='C' 
      USE &lcPath.&WorkFile IN 0 
      lnSelect = SELECT()
      SELECT &WorkFile
      IF RECCOUNT(WORKFILE) = 0 
        =gfModalGen('TRM00052B00000','DIALOG')
        RETURN .F.
      ELSE 
        LOCATE 
        SELECT (lnSelect)
        USE IN &WorkFile
      ENDIF 
    ELSE
      loOGScroll.cCRPaperSize = 'A4'

      USE &lcPath.&WorkFile IN 0 
      USE &lcPath.&lcBomHdr IN 0 
      lnSelect = SELECT()
      SELECT &lcBomHdr
      SET ORDER TO TAG &lcBomHdr
      LOCATE 
      SELECT &WorkFile
      SET RELATION TO 
      LOCATE 
      SET RELATION TO &WORKFILE..CITMMAJOR INTO &lcBomHdr
      SELECT (lnSelect)
      IF RECCOUNT(WORKFILE) = 0 
        =gfModalGen('TRM00052B00000','DIALOG')
        RETURN .F.
      ELSE 
        USE IN &WorkFile
        USE IN &lcBomHdr
      ENDIF 
    ENDIF 
    WAIT CLEAR
    loOgScroll.lcOGLastForm = lcRpForm
    llClearFn = .T.
    loogScroll.cCROrientation = 'P'
    =gfDispRe()
  ELSE 
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN .F.
  ENDIF 
ENDIF 
*-- end of report code.
*---------------Procedures and Functions section ------------*
*!************************************************************
*! Name      : lpStyClr
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Collect report data in style/color case
*!************************************************************
*! Passed Parameters : None
*!************************************************************
*! Return      : None
*!************************************************************
*!
PROCEDURE lpStyClr
BREAK  = 'CITMMAJOR + SCLR'
*- Variable to calculate cost by size.
lnCstBySiz = 0
*- Variable hold the totl count of the current scale.
lnSCnt    = 8
*- Variable hold the sizes selected for the current cost item.
lnBomSzs  = 8
*- Variable hold the current unit cost.
lnUntCost = 0
*- Variable hold the current total cost.
lnTotCost = 0
IF oAriaApplication.ActiveModuleID <> 'MA'
  IF oAriaApplication.ActiveModuleID = "PO"
    lcCatg = "PMD"
  ELSE 
    lcCatg = "M"
  ENDIF 
ENDIF 
SELECT STYLE_A
SCAN 
  XSTYLE = cStyMajor
  XSTYCLR = SUBSTR(STYLE_A.STYLE,lnMajorLen+2)
  SELECT (lcGetData)
  = SEEK(XSTYLE)
  *-- scan style in Cursor Got from SQL Query file
  *: E303189,1 MMT 07/03/2012 Add option to Style cost sheet report to show non-costing or not[T20120425.0002][Start]
  *SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = XSTYLE FOR LIKE(STRTRAN(cItmMask,'*','?') , STYLE_A.Style)  
  *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][Start]
  *SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = XSTYLE FOR LIKE(STRTRAN(cItmMask,'*','?') , STYLE_A.Style) AND IIF(!llRpPrnNC,TYP <> '8',.T.)
  SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = XSTYLE FOR LIKE(STRTRAN(cItmMask,'*','?') , STYLE_A.Style) AND;
                  IIF(!llRpPrnNC,!(TYP = '8' AND lhidenln),.T.)
  *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][End]  
  *: E303189,1 MMT 07/03/2012 Add option to Style cost sheet report to show non-costing or not[T20120425.0002][END]
      DO CASE
        CASE cCatgTyp $ lcCatg  
          SCATTER MEMVAR MEMO 
          m.SClr = XSTYCLR
          m.cSeekStyle  = STYLE_A.STYLE 
          m.lnECost =0 
          m.cCURRENCY  = ""
          m.ITEM_ID    = ""
          m.lnCstBySiz = 0
          m.ICLR       = ""
          m.StyDesc = STYLE_A.Desc1
          m.PriceA = STYLE_A.PriceA
          INSERT INTO (WORKFILE) FROM MEMVAR

        OTHERWISE
          SCATTER MEMVAR MEMO
          IF cCatgTyp = "F" .OR. (cCatgTyp = "T" .AND. Trim_Invt)
            m.ICLR  = SUBSTR(ITEM,lnNonMajSt,lnColorLen)
          ENDIF
          IF oAriaApplication.ActiveModuleID = 'MF' .AND. cCatgTyp = "TS" 
            m.Sclr = XSTYCLR
          ENDIF 
          m.cSeekStyle = STYLE_A.STYLE
          m.SClr       = XSTYCLR
          m.StyDesc    = STYLE_A.Desc1
          INSERT INTO (WORKFILE) FROM MEMVAR
      ENDCASE
  ENDSCAN  && end scan style in BOM file
ENDSCAN    && end Scan all selected styles.
SELECT (WORKFILE)
LOCATE 

XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP
HBREAK=SPACE(1)
lcSayStyle = cSeekStyle
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
DO WHILE .T.
  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    *- If user can access cost.
    lnCstBySiz = 0
    lnSCnt   = 8
    lnBomSzs = 8
    lnUntCost = 0
    lnTotCost = 0
    XNEW_STY=.T.
    HBREAK = &BREAK
    lcSayStyle = cSeekStyle
    EXIT
  ENDDO
  IF EOF()
    EXIT
  ENDIF

  IF XNEW_STY
    XNEW_STY=.F.
    WAIT WINDOW LANG_ITMCSREP_WaitSty + cSeekStyle NOWAIT
    IF SEEK(cSeekStyle,'STYLE_A')
      XSTYDESC = STYLE_A.DESC1
      XPRICEA  = STYLE_A.PRICEA
    ELSE
      XSTYDESC=SPACE(0)
      XPRICEA =0
    ENDIF
  ENDIF
  IF llMulCurr 
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = STYLE_A.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = oAriaApplication.BaseCurrency
      OTHERWISE     
        lcCurCod = STYLE_A.CDutyCur      
    ENDCASE 
  ENDIF
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][BEGIN]
  lcCurCod = CCURRCODE 
  lnExRate = NEXRATE
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][END]
  REPLACE &WORKFILE..ITEM_ID WITH IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))
  Z = Typ
  =lfCostBySz()
  *- if user can access cost
  IF qCostPrv         && COSTING ACCESS
      REPLACE &WORKFILE..UnitCost WITH lnUntCost
      REPLACE &WORKFILE..TotalCost WITH lnTotCost
    IF llMulCurr 
      REPLACE &WORKFILE..cCURRENCY WITH IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
      lnEqvCost = 0.00
      *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
      *lnEqvCost = lfAmntDisp(lnTotCost,oAriaApplication.SystemDate,lcCurCod)
      lnEqvCost = lfAmntDisp(lnTotCost,oAriaApplication.SystemDate,lcCurCod,lnExRate)
      *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]
      REPLACE &WORKFILE..lnECost WITH lnEqvCost
      lnETotCost = lnETotCost + lnEqvCost
      lnLinTot = lnLinTot + lnEqvCost
    ENDIF
  ENDIF
  Z = Typ
  SELE &WORKFILE
  SKIP
ENDDO
*!
*!*************************************************************
*! Name      : lpStyOnly
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Collect report data in style only case
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
PROCEDURE lpStyOnly
IF oAriaApplication.ActiveModuleID = 'PO'
  lcScanCond = " .T. "
  lcIfCond = " (lcOldMajor # STYLE_A.cStyMajor) OR (cCatgTyp $ 'FS' AND '******' $ cItmMask)"
ELSE 
  lcScanCond = " !lMaterial "
  lcIfCond = " (cCatgTyp # 'M' AND '******' $ cItmMask)"
ENDIF
lcOldMajor = ""
SELECT STYLE_A
*-- Scan all selected styles.
SCAN 
  XSTYCLR = SUBSTR(STYLE_A.STYLE,lnMajorLen+2)
  SELECT &lcGetData
  IF SEEK(STYLE_A.cStyMajor)
    *-scan styles in Cursor Got from SQL Query file
    *: E303189,1 MMT 07/03/2012 Add option to Style cost sheet report to show non-costing or not[T20120425.0002][Start]
    *SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = STYLE_A.cStyMajor .AND. &lcScanCond
    *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][Start]
    *SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = STYLE_A.cStyMajor .AND. &lcScanCond AND IIF(!llRpPrnNC,TYP <> '8',.T.)
    *B610710,1 TMI 04/09/2014 15:17 [Start] replace first AND with FOR
    *SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = STYLE_A.cStyMajor .AND. &lcScanCond AND ;
                    IIF(!llRpPrnNC,!(TYP ='8' AND lhidenln),.T.)
    SCAN REST WHILE CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+ITEM = STYLE_A.cStyMajor FOR &lcScanCond AND ;
                    IIF(!llRpPrnNC,!(TYP ='8' AND lhidenln),.T.)
    *B610710,1 TMI 04/09/2014 15:17 [End  ] 
    *: E303189,2 MMT 07/16/2012 Moidify option 'Show non-costing elements' to be 'Show Hidden non-costing elements'[T20120425.0002][END]    
    *: E303189,1 MMT 07/03/2012 Add option to Style cost sheet report to show non-costing or not[T20120425.0002][END]
      IF &lcIfCond
        SCATTER MEMVAR MEMO
        m.cSeekStyle = IIF('******' $ cItmMask , STYLE_A.STYLE , cItmMask)
        m.SClr       = SUBSTR(m.cSeekStyle,lnMajorLen+1) 
        m.IClr       = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
        IF EMPTY(m.ICLR) AND (cCatgTyp = 'S')
          m.IClr = SUBSTR(m.ITEM,lnNonMajSt,lnColorLen)
        ENDIF
        m.lnECost = 0
        m.cCURRENCY = ""
        m.ITEM_ID = ""
        m.lnCstBySiz = 0
        m.StyDesc = STYLE_A.Desc1
        INSERT INTO (WORKFILE) FROM MEMVAR
      ENDIF
    ENDSCAN  && end scan style in BOM file
  ENDIF 
  lcOldMajor = STYLE_A.cStyMajor
ENDSCAN  && end Scan all selected styles.

BREAK  = 'CITMMAJOR'
*- Variable to calculate cost by size.
lnCstBySiz = 0
*- Variable hold the totl count of the current scale.
lnSCnt   = 8
*- Variable hold the sizes selected for the current cost item.
lnBomSzs = 8
*- Variable hold the current unit cost.
lnUntCost = 0
*- Variable hold the current total cost.
lnTotCost = 0


=lfCrtTmp('C')

SELECT (WORKFILE)
LOCATE 

XNEW_STY=.T.            && FLAG THAT TELLS IF NEW STYLE OR NOT
XPRV_TYP = TYP
HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
DO WHILE .T.
  DO WHILE .NOT. EMPTY(BREAK)
    IF &BREAK = HBREAK
      EXIT
    ENDIF
    *- Variable to calculate cost by size.
    lnCstBySiz = 0
    *- Variable hold the totl count of the current scale.
    lnSCnt   = 8
    *- Variable hold the sizes selected for the current cost item.
    lnBomSzs = 8
    *- Variable hold the current unit cost.
    lnUntCost = 0
    *- Variable hold the current total cost.
    lnTotCost = 0
    XNEW_STY=.T.
    HBREAK = &BREAK
    EXIT
  ENDDO
  IF EOF()
    EXIT
  ENDIF
  IF XNEW_STY
    XNEW_STY=.F.
    XSTYLE = ALLTRIM(&WORKFILE..cItmMajor)
    WAIT WINDOW LANG_ITMCSREP_WaitSty + cSeekStyle NOWAIT
    INSERT INTO &lcBomHdr (ItemMajor) VALUES (&WORKFILE..CITMMAJOR)
    = SEEK (cSeekStyle,'STYLE_A')
    *------- NOW DISPLAY ALL STYLE INFO ----*
    XSEAS_DATA = IIF(STYLE_A.SEASON = 'Y     ','YEAR ROUND',gfCodDes(STYLE_A.SEASON , 'SEASON'))    && Get Season from codes file. 
    XDIVI_DATA = gfCodDes(STYLE_A.CDIVISION , 'CDIVISION')    && Get Division from codes file.  
    XGROU_DATA = gfCodDes(STYLE_A.CSTYGROUP , 'CSTYGROUP')    && Get Style group from codes file.  
    REPLACE &lcBomHdr..SeasData WITH PADR(XSEAS_DATA,17)
    REPLACE &lcBomHdr..DivData WITH PADR(XDIVI_DATA,17)
    REPLACE &lcBomHdr..GroupData WITH SUBSTR(XGROU_DATA,1,14)
    REPLACE &lcBomHdr..PriceA WITH STYLE_A.PRICEA
    REPLACE &lcBomHdr..PriceB WITH STYLE_A.PRICEB
    REPLACE &lcBomHdr..PriceC WITH STYLE_A.PRICEC
    REPLACE &lcBomHdr..Scale WITH STYLE_A.SCALE
    = SEEK('S' + STYLE_A.SCALE,'SCALE')    

    FOR lnI = 1 TO 8 
      lcSc = STR(lnI,1)
      REPLACE &lcBomHdr..ScaleSize&lcSc WITH PADL(ALLTRIM(SCALE.SZ&lcSc),5)
    ENDFOR

    REPLACE &lcBomHdr..Pattern WITH ALLTRIM(STYLE_A.PATTERN)
    REPLACE &lcBomHdr..PrePack WITH STYLE_A.PREPAK

    IF !EMPTY(STYLE_A.PREPAK)
      = SEEK('P' + STYLE_A.SCALE + STYLE_A.PREPAK ,'SCALE')
      STORE 0 TO XPREPAK
      X   = 1
      DO WHILE X<=8
        Y=STR(X,1)
        REPLACE &lcBomHdr..PPQty&Y WITH SCALE.PP&Y
        X = X+1
      ENDDO
    ENDIF
    lcColors = ''
    DO WHILE ALLTRIM(STYLE_A.CSTYMAJOR) == XSTYLE
      IF EOF()
        EXIT
      ENDIF
      IF EMPTY(lcColors)
        lcColors = lcColors + IIF(lcFree_Clr = 'C',SUBSTR(STYLE_A.STYLE,lnNonMajSt,lnColorLen),'******')
      ELSE
        lcColors = lcColors + " - " + IIF(lcFree_Clr = 'C',SUBSTR(STYLE_A.STYLE,lnNonMajSt,lnColorLen),'******')
      ENDIF 
      SKIP IN STYLE_A
    ENDDO
    REPLACE &lcBomHdr..MColors WITH lcColors
  ENDIF         && END OF NEW STYLE
  
  *- if use multi currency get currency code [Begin]
  IF llMulCurr 
    = SEEK(cSeekStyle,'STYLE_A')
    DO CASE
      CASE cCatgTyp = "P"
        lcCurCod = STYLE_A.CPriceCur
      CASE cCatgTyp $ "FTS"
        lcCurCod = oAriaApplication.BaseCurrency
      OTHERWISE     
       lcCurCod = STYLE_A.CDutyCur      
    ENDCASE 
  ENDIF
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][BEGIN]
  lcCurCod = CCURRCODE 
  lnExRate = NEXRATE
  *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][END]
  REPLACE &lcBomHdr..ItemID WITH IIF(CCATGTYP = 'F',PADR(ITEM,7),PADR(ITEM,lnMajorLen))

  IF SCLR = '******'
    TmpSclr = IIF(ICLR # '******',ICLR,IIF(lcFree_Clr = 'C',SUBSTR(STYLE_A.STYLE,lnNonMajSt,lnColorLen),'******'))
    REPLACE &WORKFILE..ICLR WITH TmpSclr
  ENDIF
  *- Call local function to calculate cost.
  Z = Typ
  =lfCostBySz()
  IF qCostPrv             && COSTING ACCESS
    REPLACE &WORKFILE..UnitCost WITH lnUntCost
    REPLACE &WORKFILE..TotalCost WITH lnTotCost
    IF llMulCurr 
      REPLACE &WORKFILE..cCURRENCY WITH IIF(SEEK(lcCurCod,'SYCCURR'),SYCCURR.cCurrSmbl,'')
      lnEqvCost = 0.00
      *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
      *lnEqvCost = lfAmntDisp(lnTotCost,oAriaApplication.SystemDate,lcCurCod)
      lnEqvCost = lfAmntDisp(lnTotCost,oAriaApplication.SystemDate,lcCurCod,lnExRate )
      *: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]
      REPLACE &WORKFILE..lnECost WITH lnEqvCost
    ENDIF
  ENDIF
  Z = Typ
  *---- NOW DISPLAY THE FABRIC AND STYLE COLOR ASSIGNEMENTS ----*

  IF .NOT. EOF() .AND. (XPRV_TYP=TYP)
    SKIP IN (WORKFILE)
  ENDIF
  IF XPRV_TYP <> TYP
    XPRV_TYP = TYP
  ENDIF   
ENDDO

**>> 
*!
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/13/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwRepWhen
LOCAL lnResult1

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
  lcTmpCost = loOGScroll.gfTempName()
  IF oAriaApplication.ActiveModuleID = 'MA'
    lcBomHdrSelect = " SELECT BOMHEADR.CITMMAJOR,BOMHEADR.CCSTSHT_ID,BOMHEADR.CCSTSHTTYP,ITEM.[Desc],ITEM.item_Type,ITEM.LOCATION ,ITEM.Vendor,ITEM.Pattern  FROM ITEM INNER JOIN BOMHEADR ON ITEM.CSTYMAJOR = BOMHEADR.CITMMAJOR  "
  ELSE
    lcBomHdrSelect = " SELECT cItmMajor,cCstSht_ID,CCSTSHTTYP From BomHeadr (INDEX = BomHeadr) Where BomHeadr.cInvType =" + lcInvTyp
  ENDIF 
  lnResult2 = loOGScroll.orda.SqlRun (lcBomHdrSelect,lcTmpCost,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" )) 
  llFrstTime = .T.
  IF lnResult2 >= 1
    lnBuffering = CURSORGETPROP("Buffering",lcTmpCost)
    =CURSORSETPROP("Buffering",3,lcTmpCost)
    SELECT (lcTmpCost)
    *--B999999,1 MMT 04/10/2005,Fix problem of repeating materials[Start]
    INDEX ON cItmMajor TAG &lcTmpCost UNIQUE 
    *--B999999,1 MMT 04/10/2005,Fix problem of repeating materials[End]
    SET ORDER TO TAG &lcTmpCost
  ENDIF 
ENDIF 
IF oAriaApplication.ActiveModuleID <> 'MA'
  IF EMPTY(lcSlbl1)
    IF lcFree_Clr # 'C'
      laOGObjCnt[1] = .F.    && Disble if there is no color segment.
      =lfOGShowGet('LCRPSTYCLR')  && Called to show object get.
    ENDIF
  ENDIF 
ENDIF 

XAVG_COST = (gfGetMemVar('M_COST_MET') = 'A')
qCostPrv = gfUserPriv('IC','ICSTYLE','COSTING')

SET ORDER TO CODES IN CODES        && To use it to validate STYLE   # in option grid.
IF oAriaApplication.ActiveModuleID <> 'MA'
  SET ORDER TO STYLE IN STYLE        && To use it to validate STYLE   # in option grid.
  SET ORDER TO SCALE IN SCALE        && To use it to validate STYLE   # in option grid.
ENDIF 

IF oAriaApplication.ActiveModuleID <> 'MA'
  FOR lnCount = 1 TO 7
    lcCount  = STR(lnCount,1)
    IF oAriaApplication.ActiveModuleID = 'MF'
      lcSlbl&lcCount = gfGetMemVar('M_CMSLBL'+lcCount)
    ELSE 
      lcSlbl&lcCount = gfGetMemVar('M_CISLBL'+lcCount)
    ENDIF 
  ENDFOR 
ELSE
  *--B999999,1 MMT 04/10/2005 , Fix problem of browse material[Start]
  lcbrowflds = LCTMPCOST+".CITMMAJOR :R :H= 'Item' ,"+;
             LCTMPCOST+".CCSTSHT_ID :R :H='Cost Sheet', Color='******' ,"+;
             LCTMPCOST+".Desc :R :H= 'Description' , "+;
             "lcItmType=gfCodDes("+LCTMPCOST+".item_Type,'ITEM_TYPE') :R :H= 'Type' ,"+;
             LCTMPCOST+".LOCATION  :R :H= 'Location',"+;
             LCTMPCOST+".Vendor:R :H= 'Vendor',"+;
             LCTMPCOST+".Pattern :R :H= 'Pattern'"
  *--B999999,1 MMT 04/10/2005 , Fix problem of browse material[End]

  FOR lnCount = 1 TO 4
    lcCount  = STR(lnCount,1)
    lcSlbl&lcCount = gfGetMemVar('M_CTSLBL'+lcCount)
  ENDFOR 
ENDIF


*-- end of lfwRepWhen.
*!
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

lcColorTlt = LANG_ITMCSREP_OnlyThs + ALLTRIM(lcNonMajTl) + 's'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.
*!
*!*************************************************************
*! Name      : lfNMajType
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/27/1998
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
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
*!
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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

IF oAriaApplication.ActiveModuleID = 'MA'
  DO CASE
    CASE lcParm = 'S'  && Set code
      *-- open this file in another alias to set order to Style Major 
      *-- unique index.
      SELECT &lcTmpCost
      SET RELATION TO cSTYMajor INTO ITEM 
      GO TOP IN &lcTmpCost
      llChStyle = .T.
  ENDCASE
ELSE
  DO CASE
    CASE lcParm = 'S'  && Set code
      *-- open this file in another alias to set order to Style Major 
      *-- unique index.
      USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
      SELECT STYLE
      SET ORDER TO TAG cStyle
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      SELECT (lcTmpCost)
      SET RELATION TO cItmMajor INTO Style 
      GO TOP IN &lcTmpCost
      llChStyle = .T.
    CASE lcParm = 'R'  && Reset code
      USE IN STYLE_X
      SELECT STYLE
      SET ORDER TO TAG STYLE
  ENDCASE
ENDIF 
*-- end of lfsrvSty.
*!
*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
SET ORDER TO Style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF 

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
*!
*!*************************************************************
*! Name      : lfChanged
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : rise flag tell us that user change report format.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfChanged
llChStyClr = .T.
*- END OF lfChanged.
*!
*!*************************************************************
*! Name      : lfFillForm
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Fill report format arrays called from dummy variable in rep. gen.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfFillForm
IF lcFree_Clr = 'C'
  DIMENSION laFormDesc[2,1] , laFormVal[2,1]
  laFormDesc[1,1] = lcStyMajor + ' / ' + lcNonMajTl
  laFormDesc[2,1] = lcStyMajor + LANG_ITMCSREP_Only

  laFormVal[1,1]  = 'C'
  laFormVal[2,1]  = 'S'
ELSE
  DIMENSION laFormDesc[1,1] , laFormVal[1,1]
  laFormDesc[1,1] = lcStyMajor + LANG_ITMCSREP_Only
  laFormVal[1,1]  = 'S'
ENDIF
*-- end of lfFillForm.
*!
*!*************************************************************
*! Name      : lfFormDefa
*! Developer : MAB
*! Date      : 8/18/1998
*! Purpose   : Set default type for form format. 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*!
FUNCTION lfFormDefa
RETURN IIF(lcFree_Clr = 'C','C','S')
*-- end of lfFormDefa.
*!
*!*************************************************************
*! Name      : lfCostBySz
*! Developer : Reham Al-Allamy
*! Date      : 05/10/1999
*! Purpose   : Calculate cost for each cost element y size.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfCostBySz
PRIVATE lcCurAlias , lnStyRec , lnSclRec
 
*- Save current alias...
lcCurAlias = ALIAS()
*- Save the record pointer in the style file.
lnStyRec   = RECNO("STYLE")
*- Save the record pointer in the scale file.
lnSclRec   = RECNO("SCALE")
*- Variable hold the scale count.
lnSCnt   = 8
lnBomSzs = 8

SELECT (WORKFILE)
*- If the sizes field have sizes.
IF !EMPTY(MSizes)
  *- Seek with the current style from BOM file in style file.
  IF SEEK(ALLTRIM(cItmMajor) , 'STYLE_A') 
    SELECT STYLE_A
    LOCATE REST FOR Style.cStyMajor = &WORKFILE..cItmMajor .AND. ;
                    LIKE(STRTRAN(&WORKFILE..cItmMask , "*" , "?") , STYLE_A.Style)
    *- If found the style in the style file.
    IF FOUND()
      *- Seek with the style scale in the scale file.
      IF SEEK('S'+STYLE.Scale,'SCALE')
        *- Get the scale count for the current style.
        lnSCnt=SCALE.Cnt
      ENDIF
    ENDIF
  ENDIF
  
  *- If the system extended size scale & current category type is style
  *- and the cross reference between the parent style & its child has sizes.
  SELECT (WORKFILE)
  IF llMScale AND cCatgTyp='S' AND !EMPTY(MSzCrosref)
    *- Variable hold the selected sizes that the current cost applied to it.
    lnBomSzs=0
    *- Loop in the lines of the memo field that hold the cross reference
    *- sizes between the parent style & its child.
    FOR lnMln=1 TO MEMLINES(MSzCrosref)
      *- If the style scale is the same as the current scale in the current line 
      *- of the memo field that hold the cross reference sizes between the 
      *- parent style & its child.
      IF STYLE.Scale=SUBSTR( MLINE(MSzCrosref,lnMln) ,1 , 3)
        *- Calculate the selected sizes from this scale.
        lnBomSzs=lnBomSzs+1
      ENDIF
    ENDFOR
  ELSE
    *- If the system is not extended size scale & the current category 
    *- type is not style, use the sizes memo field.
    
    *- Default the selected sizes to the scale count.
    lnBomSzs=lnSCnt
    *- Loop in the lines of the sizes memo field.
    FOR lnMln=1 TO MEMLINES(MSizes)
      *- If the style scale exist in the current line of the sizes memo field.
      IF OCCURS(STYLE_A.Scale,MLINE(MSizes,lnMln)) <> 0
        *- Get the occurence of this scale, means the sizes selected from 
        *- this sizes memo field.
        lnBomSzs=OCCURS(',',MLINE(MSizes,lnMln))+1
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF 
*- Multiple tot cost by the selected sizes only & divide it by total sizes of the scale.
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
*lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*TotCost,2)
lnCstBySiz = lnCstBySiz + ROUND((lnBomSzs/lnSCnt)*TotCost,3)
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]
*- Variable hold the current unit cost.
lnUntCost = UntCost * (lnBomSzs/lnSCnt)
*- Variable hold the current total cost.
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
*lnTotCost = ROUND(TotCost * (lnBomSzs/lnSCnt),2)
lnTotCost = ROUND(TotCost * (lnBomSzs/lnSCnt),3)
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]
*- Restore the record pointer in the style file.
IF lnStyRec > 0 .AND. lnStyRec <= RECCOUNT("STYLE_A")
  GOTO lnStyRec IN STYLE_A
ENDIF
*- Restore the record pointer in the scale file.
IF lnSclRec > 0 .AND. lnSclRec <= RECCOUNT("SCALE")
  GOTO lnSclRec IN SCALE
ENDIF
*- Restore the original alias...
SELECT (lcCurAlias)
*!
*!********************************************************************
*! Name      : lfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcCurrCode   && Currency Code
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*!
FUNCTION lfAmntDisp
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
*PARAMETER lnAmount,ldExRateDt,lcCurrCode
PARAMETER lnAmount,ldExRateDt,lcCurrCode, pnEXRate
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]
PRIVATE lnAmount,ldExRateDt,lcExSin1,lcExSin2,lnSavAlias
lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

lnExRate   = 0
lnUnit     = 0
    
IF lcCurrCode = oAriaApplication.BaseCurrency
  lnExRate = 1
  lnUnit   = 1
ELSE
  lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
ENDIF
lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)

*WSH 10/19/2005 [Start]
*lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
*WSH 10/19/2005 [End]
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
IF TYPE('pnEXRate')='N' AND pnEXRate <> 0
  lnEXRate = pnEXRate
ENDIF   
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]

lcExSin2   = ' '
lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][Begin]
*lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,3)
*: B609969,1 HIA 06/20/2012 Fixing Media issues[T20120425.0002][End]

SELECT (lnSavAlias)
RETURN lnAmount
*!
*!************************************************************
*! Name      : lfFillArray
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : Fill the order status array
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION lfFillArray

DIMENSION laRpSource[4],laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget

laRpSource[1] = LANG_ITMCSREP_Active
laRpSource[2] = LANG_ITMCSREP_Hold
laRpSource[3] = LANG_ITMCSREP_Cncld
laRpSource[4] = LANG_ITMCSREP_InWork
*!
*!************************************************************
*! Name      : lfvStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : For the order status mover
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION lfvStatus
= lfOGMover(@laRpSource,@laRpTarget,LANG_ITMCSREP_CstStat,.T.,'')
STORE .T. TO llogfltch           && Rise this flag to recollect the data.
STORE SPACE(0) TO lcRpStatus
STORE .F. TO llFlgMovr

IF ASCAN(laRpTarget,LANG_ITMCSREP_Active) > 0       
  lcRpStatus = "A"
  llFlgMovr = .T.
ENDIF

IF ASCAN(laRpTarget,LANG_ITMCSREP_Hold) > 0         
  lcRpStatus = lcRpStatus + "H"
  llFlgMovr = .T.
ENDIF

IF ASCAN(laRpTarget,LANG_ITMCSREP_Cncld) > 0    
  lcRpStatus = lcRpStatus + "X"
  llFlgMovr = .T.
ENDIF

IF ASCAN(laRpTarget,LANG_ITMCSREP_InWork) > 0    
  lcRpStatus = lcRpStatus + "W"
  llFlgMovr = .T.
ENDIF

IF !llFlgMovr
  lcRpStatus = "AHXW"
ENDIF
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
*!************************************************************
*! Name      : LFVCOST
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : disable and enable cost sheet ID browse
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION LFVCOST
llDefCst = llDefOnly
CLEARREAD()
*!
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Heba Fathi
*! Date      : 09/01/2004
*! Purpose   : Create Temp. Files Structure.
*!*************************************************************
*!
FUNCTION lfCrtTmp
PARAMETERS lcType

lnAlias = SELECT(0)
IF lcType = 'C'
  DIMENSION laTmpBomHr[28,4]
  laTmpBomHr[1,1] = 'ItemMajor' 
  laTmpBomHr[1,2] = 'C'
  laTmpBomHr[1,3] = 19
  laTmpBomHr[1,4] = 0

  laTmpBomHr[2,1] = 'PRICEA' 
  laTmpBomHr[2,2] = 'N'
  laTmpBomHr[2,3] = 12
  laTmpBomHr[2,4] = 2

  laTmpBomHr[3,1] = 'PRICEB' 
  laTmpBomHr[3,2] = 'N'
  laTmpBomHr[3,3] = 12
  laTmpBomHr[3,4] = 2

  laTmpBomHr[4,1] = 'PRICEC' 
  laTmpBomHr[4,2] = 'N'
  laTmpBomHr[4,3] = 12
  laTmpBomHr[4,4] = 2

  laTmpBomHr[5,1] = 'SeasData' 
  laTmpBomHr[5,2] = 'C'
  laTmpBomHr[5,3] = 17
  laTmpBomHr[5,4] = 0

  laTmpBomHr[6,1] = 'DivData' 
  laTmpBomHr[6,2] = 'C'
  laTmpBomHr[6,3] = 17
  laTmpBomHr[6,4] = 0

  laTmpBomHr[7,1] = 'GroupData' 
  laTmpBomHr[7,2] = 'C'
  laTmpBomHr[7,3] = 17
  laTmpBomHr[7,4] = 0

  laTmpBomHr[8,1] = 'Scale' 
  laTmpBomHr[8,2] = 'C'
  laTmpBomHr[8,3] = 3
  laTmpBomHr[8,4] = 0

  laTmpBomHr[9,1] = 'ScaleSize1' 
  laTmpBomHr[9,2] = 'C'
  laTmpBomHr[9,3] = 5
  laTmpBomHr[9,4] = 0

  laTmpBomHr[10,1] = 'ScaleSize2' 
  laTmpBomHr[10,2] = 'C'
  laTmpBomHr[10,3] = 5
  laTmpBomHr[10,4] = 0

  laTmpBomHr[11,1] = 'ScaleSize3' 
  laTmpBomHr[11,2] = 'C'
  laTmpBomHr[11,3] = 5
  laTmpBomHr[11,4] = 0

  laTmpBomHr[12,1] = 'ScaleSize4' 
  laTmpBomHr[12,2] = 'C'
  laTmpBomHr[12,3] = 5
  laTmpBomHr[12,4] = 0

  laTmpBomHr[13,1] = 'ScaleSize5' 
  laTmpBomHr[13,2] = 'C'
  laTmpBomHr[13,3] = 5
  laTmpBomHr[13,4] = 0

  laTmpBomHr[14,1] = 'ScaleSize6' 
  laTmpBomHr[14,2] = 'C'
  laTmpBomHr[14,3] = 5
  laTmpBomHr[14,4] = 0

  laTmpBomHr[15,1] = 'ScaleSize7' 
  laTmpBomHr[15,2] = 'C'
  laTmpBomHr[15,3] = 5
  laTmpBomHr[15,4] = 0

  laTmpBomHr[16,1] = 'ScaleSize8' 
  laTmpBomHr[16,2] = 'C'
  laTmpBomHr[16,3] = 5
  laTmpBomHr[16,4] = 0

  laTmpBomHr[17,1] = 'Pattern' 
  laTmpBomHr[17,2] = 'C'
  laTmpBomHr[17,3] = 10
  laTmpBomHr[17,4] = 0

  laTmpBomHr[18,1] = 'PrePack' 
  laTmpBomHr[18,2] = 'C'
  laTmpBomHr[18,3] = 1
  laTmpBomHr[18,4] = 0

  laTmpBomHr[19,1] = 'PPQty1' 
  laTmpBomHr[19,2] = 'N'
  laTmpBomHr[19,3] = 3
  laTmpBomHr[19,4] = 0

  laTmpBomHr[20,1] = 'PPQty2' 
  laTmpBomHr[20,2] = 'N'
  laTmpBomHr[20,3] = 3
  laTmpBomHr[20,4] = 0

  laTmpBomHr[21,1] = 'PPQty3' 
  laTmpBomHr[21,2] = 'N'
  laTmpBomHr[21,3] = 3
  laTmpBomHr[21,4] = 0

  laTmpBomHr[22,1] = 'PPQty4' 
  laTmpBomHr[22,2] = 'N'
  laTmpBomHr[22,3] = 3
  laTmpBomHr[22,4] = 0

  laTmpBomHr[23,1] = 'PPQty5' 
  laTmpBomHr[23,2] = 'N'
  laTmpBomHr[23,3] = 3
  laTmpBomHr[23,4] = 0

  laTmpBomHr[24,1] = 'PPQty6' 
  laTmpBomHr[24,2] = 'N'
  laTmpBomHr[24,3] = 3
  laTmpBomHr[24,4] = 0

  laTmpBomHr[25,1] = 'PPQty7' 
  laTmpBomHr[25,2] = 'N'
  laTmpBomHr[25,3] = 3
  laTmpBomHr[25,4] = 0

  laTmpBomHr[26,1] = 'PPQty8' 
  laTmpBomHr[26,2] = 'N'
  laTmpBomHr[26,3] = 3
  laTmpBomHr[26,4] = 0

  laTmpBomHr[27,1] = 'MColors' 
  laTmpBomHr[27,2] = 'M'
  laTmpBomHr[27,3] = 0
  laTmpBomHr[27,4] = 0

  laTmpBomHr[28,1] = 'ItemID' 
  laTmpBomHr[28,2] = 'C'
  laTmpBomHr[28,3] = 10
  laTmpBomHr[28,4] = 0
  gfCrtTmp(lcBomHdr,@laTmpBomHr,"ItemMajor",lcBomHdr,.F.)

ELSE 
  DIMENSION laTempStru [1,1]
  = AFIELDS(laTempStru)  
  lnNoField = ALEN(laTempStru , 1)
  DIMENSION laTempStru[lnNoField + 11 , 18]

  *-- cSeekStyle     :  field save style to seek it in style file.
  laTempStru[lnNoField + 1 ,1] = 'cSeekStyle'
  laTempStru[lnNoField + 1 ,2] = 'C'
  laTempStru[lnNoField + 1 ,3] = 19
  laTempStru[lnNoField + 1 ,4] = 0

  *-- SCLR     :  field save style color
  laTempStru[lnNoField + 2 ,1] = 'SCLR'
  laTempStru[lnNoField + 2 ,2] = 'C'
  laTempStru[lnNoField + 2 ,3] = 19
  laTempStru[lnNoField + 2 ,4] = 0

  laTempStru[lnNoField + 3 ,1] = 'lnECost'
  laTempStru[lnNoField + 3 ,2] = 'N'
  laTempStru[lnNoField + 3 ,3] = 13
  laTempStru[lnNoField + 3 ,4] = 3

  laTempStru[lnNoField + 4 ,1] = 'cCURRENCY'
  laTempStru[lnNoField + 4 ,2] = 'C'
  laTempStru[lnNoField + 4 ,3] = 3
  laTempStru[lnNoField + 4 ,4] = 0

  laTempStru[lnNoField + 5 ,1] = 'ITEM_ID'
  laTempStru[lnNoField + 5 ,2] = 'C'
  laTempStru[lnNoField + 5 ,3] = 8
  laTempStru[lnNoField + 5 ,4] = 0

  *-Variable To Hold Cost By Size
  laTempStru[lnNoField + 6 ,1] = 'lnCstBySiz'
  laTempStru[lnNoField + 6 ,2] = 'N'
  laTempStru[lnNoField + 6 ,3] = 9
  laTempStru[lnNoField + 6 ,4] = 2

  laTempStru[lnNoField + 7 ,1] = 'ICLR'
  laTempStru[lnNoField + 7 ,2] = 'C'
  laTempStru[lnNoField + 7 ,3] = 19
  laTempStru[lnNoField + 7 ,4] = 0

  laTempStru[lnNoField + 8 ,1] = 'UnitCost'
  laTempStru[lnNoField + 8 ,2] = 'N'
  laTempStru[lnNoField + 8 ,3] = 11
  laTempStru[lnNoField + 8 ,4] = 3

  laTempStru[lnNoField + 9 ,1] = 'TotalCost'
  laTempStru[lnNoField + 9 ,2] = 'N'
  laTempStru[lnNoField + 9 ,3] = 13
  laTempStru[lnNoField + 9 ,4] = 3

  laTempStru[lnNoField + 10 ,1] = 'StyDesc'
  laTempStru[lnNoField + 10 ,2] = 'C'
  laTempStru[lnNoField + 10 ,3] = 41
  laTempStru[lnNoField + 10 ,4] = 0

  laTempStru[lnNoField + 11 ,1] = 'PriceA'
  laTempStru[lnNoField + 11 ,2] = 'N'
  laTempStru[lnNoField + 11 ,3] = 9
  laTempStru[lnNoField + 11 ,4] = 2

  FOR  lnLen = 7 TO 18
    FOR lnCount = 1 TO 11
      STORE SPACE(1) TO laTempStru[lnNoField + lnCount,lnLen]
    ENDFOR 
  ENDFOR
  gfCrtTmp(WORKFILE,@laTempStru)
ENDIF 
SELECT (lnAlias)
*!
*!*************************************************************
*! Name      : lfBuilForm
*! Developer : KHALID MOHI EL-DIN MOHAMED
*! Date      : 05/23/1999
*! Purpose   : Building the temporary file according to the form.
*!*************************************************************
*! Passed Parameters  :  lcForm
*!*************************************************************
*!
FUNCTION lfBuilForm
PARAMETER lcForm
=lfCrtTmp('C')
lnSrch1 = ASCAN(loOGScroll.laOGFxFlt,"lcRpColors")
IF lnSrch1 > 0
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnSrch1,1)
  IF !EMPTY(loOGScroll.laOGFxFlt[lnRow,6])
    lcAllClr = loOGScroll.laOGFxFlt[lnRow,6]
  ELSE
    lcAllClr = ""
  ENDIF 
ENDIF

*-- Build the work file.
SELECT &WorkFile
*-- Indexing the temporary BOM file according to the selected form.
IF RIGHT(lcRpForm,1) = 'A'
  INDEX ON SUBSTR(cItmMajor,1,7)+cItmMask+Typ+IIF(cCatgTyp <> "M",Item,MFGCode)+IClr+STR(RECNO(),7) TAG &WorkFile
ELSE
  INDEX ON SUBSTR(cItmMajor,1,7)+Typ+IIF(cCatgTyp <> "M",Item,MFGCode)+cItmMask+IClr+STR(RECNO(),7) TAG &WorkFile
ENDIF  
SET ORDER TO TAG &WorkFile

SELECT &lcGetData
SCAN 
  lcFabClr = ALLTRIM(SUBSTR(STYLE,lnMajorLen+2))
  lcClrExp = IIF(EMPTY(lcAllClr)," .T. ","lcFabClr $ lcAllClr")
  IF &lcClrExp
    WAIT WINDOW LANG_ITMCSREP_WaitFab + Fabric NOWAIT
    lcFabric = Fabric

    lnFabRec = RECNO()
    INSERT INTO &lcBomHdr (ItemMajor) VALUES (&lcGetData..CITMMAJOR)
    lcColors = ''
    SCAN REST WHILE ALLTRIM(SUBSTR(cItmMajor,1,7)) = ALLTRIM(lcFabric)
      lnUntCost = 0
      IF EMPTY(lcColors)
        lcColors = lcColors + ALLTRIM(SUBSTR(STYLE,lnMajorLen+2))
      ELSE
        lcColors = lcColors + " - " + ALLTRIM(SUBSTR(STYLE,lnMajorLen+2))
      ENDIF 
      IF cCatgTyp = "M"
        SCATTER MEMVAR
        m.cSeekStyle = Style
        M.ICLR = ALLTRIM(SUBSTR(STYLE,lnMajorLen+2))
        SELECT &WorkFile
        APPEND BLANK
        GATHER MEMVAR
        REPLACE cItmMask WITH lcFabClr
      ELSE
        IF ('******' $ cItmMask ) .OR. (lcFabClr $ cItmMask )
          IF cCatgTyp = "F"
            ICLR  = ALLTRIM(SUBSTR(ITEM,lnMajorLen+2))
            lcKey = SUBSTR(Item,1,7)+IIF(IClr='******', lcFabClr, IClr)
            SELECT &lcGetData
            SCATTER MEMVAR
            m.cSeekStyle = Style
            SELECT &WorkFile
            APPEND BLANK
            GATHER MEMVAR
            REPLACE cItmMask WITH lcFabClr,;
                    IClr     WITH IIF(IClr <> '******', IClr, lcFabClr),;
                    UntCost  WITH lnUntCost
            REPLACE TotCost WITH ROUND(UntCost*nBomTotQty,2)
            SELECT &lcGetData
          ENDIF
          IF cCatgTyp = "T"
            SELECT &lcGetData
            SCATTER MEMVAR
            m.cSeekStyle = Style
            SELECT &WorkFile
            APPEND BLANK
            GATHER MEMVAR
            REPLACE cItmMask WITH lcFabClr,;
                    IClr     WITH IIF(IClr <> '******', IClr, lcFabClr)
            SELECT &lcGetData
          ENDIF
        ENDIF
      ENDIF
      REPLACE &lcBomHdr..MColors WITH lcColors
      SELECT &lcGetData
    ENDSCAN 
  ENDIF 
ENDSCAN
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
PARAMETERS lcFilter,lcCursor,lcFldName,lcSntFld
LOCAL   lnPosition,lnFltPos,lnRow,lcExpression
STORE 0 TO lnPosition,lnFltPos,lnRow
STORE '' TO lcExpression
lnFltPos = ASCAN(loOGScroll.laOGFxFlt,lcFilter)
*-if filter exists
IF lnFltPos > 0  
  lnRow = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnFltPos,1)
  lcTmpCur = loOGScroll.laOGFxFlt[lnRow,6]
  IF !EMPTY(lcTmpCur)  &&user selected range of selections
    SELECT &lcTmpCur
    IF ( RECCOUNT() > 0) 
      &lcCursor = loOgScroll.gfSQLTempName('',lcFldName,lcTmpCur,lcSntFld) && SQL Temp File
      IF EMPTY(&lcCursor)
        *-- SQL connection Error. Can't open The Report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ELSE  && EMPTY CURSOR AND IT IS NOT STYLE FILE
      &lcCursor = ""
    ENDIF 
  ELSE
    &lcCursor = ""
  ENDIF 
ENDIF 
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
&lcWhereCondition = ""

DO CASE
  CASE oAriaApplication.ActiveModuleID = 'MA'  
  *- LcStyTyp (Material Type) = 'B' -> Both, 'D'-> Domestic, 'I'->Imported
  IF LcStyTyp <>'B'
    IF LcStyTyp = 'D'
      lcFlt1 = " Item.Make = 1 "
    ELSE
      lcFlt1 = " Item.Make = 0 "
    ENDIF 
    *-- add material type filter to where condition
    &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt1,&lcWhereCondition + " AND " + lcFlt1)
  ENDIF 

  *-- Add inventory type control value
  lcFlt2 = " Bom.cInvType = " + lcInvTyp + " AND Bom.lMaterial = 1 "
  &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt2,&lcWhereCondition + " AND " + lcFlt2)

  CASE oAriaApplication.ActiveModuleID = 'PO'  
    lcFlt2 = " Bom.cInvType = " + lcInvTyp
    &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt2,&lcWhereCondition + " AND " + lcFlt2)

  CASE oAriaApplication.ActiveModuleID = 'MF'
    lcFlt2 = " Bom.cInvType = " + lcInvTyp
    &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt2,&lcWhereCondition + " AND " + lcFlt2)
ENDCASE

*-ADD SQLEXP TO WHERE COND
IF LEN(loOGScroll.lcRpSqlExp) > 1
  lcExp = loOGScroll.lcRpSqlExp
  llPrevious = .F.
  lnOccur = OCCURS(' AND',lcExp)
  IF (lnOccur = 1 .AND. ATC('BETWEEN',lcExp) > 0) .OR. (lnOccur = 0)
    IF ATC('ITEM.CSTYMAJOR',lcExp)= 0 .AND. ATC('BOMHEADR.CCSTSHT_ID',lcExp)= 0
      &lcWhereCondition = &lcWhereCondition + " AND " + lcExp
    ENDIF 
  ELSE
    FOR lnCount = 1 TO lnOccur + 1
      lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 5)
      lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp)+1,ATC(' AND',lcExp,lnCount))
      lnLength = lnEnd - lnStart
      lcTake = SUBSTR(lcExp,lnStart,lnLength)
      IF ATC('BETWEEN',lcTake) > 0
        lcBetExpr = lcTake
        llPrevious = .T.
      ELSE
        IF llPrevious
          lcFullExpr = lcBetExpr + " AND " + lcTake  
      &lcWhereCondition = &lcWhereCondition + " AND " + lcFullExpr
      llPrevious = .F.
        ELSE
          IF ATC('ITEM.CSTYMAJOR',lcTake)= 0 .AND. ATC('BOMHEADR.CCSTSHT_ID',lcTake)= 0
            &lcWhereCondition = &lcWhereCondition + " AND " + lcTake
          ENDIF 
        ENDIF 
      ENDIF 
    ENDFOR 
  ENDIF
ENDIF 

IF oAriaApplication.ActiveModuleID = 'MA'
  lcStyleRow = ASCAN(loOGScroll.laOgFxFlt,'ITEM.CSTYMAJOR')
  &lcWhereCondition = &lcWhereCondition + " AND BomHeadr.cInvType = '0002' "
ELSE
  lcStyleRow = ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYMAJOR')
  &lcWhereCondition = &lcWhereCondition + " AND BomHeadr.cInvType = '0001' "
ENDIF 

lcStyleRow = ASUBSCRIPT(loOGScroll.laOgFxFlt,lcStyleRow,1)
lcStyleCursor = loOGScroll.laOgFxFlt[lcStyleRow,6]
IF !EMPTY(lcStyleCursor)
  lcSqlCost = loOgScroll.gfSQLTempName('','cCostSht C(19)',lcStyleCursor,'CSTYMAJOR')
ENDIF 

*-append the condition of the default cost sheet onl(Yes/No) to the where condition
IF llDefCst = .T.
  lcFlt4 = " Bomheadr.LDefCstSht = 1 "
  &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt4,&lcWhereCondition+ " AND " + lcFlt4)
ENDIF 

*-append the cost sheet status to the where condition 
StatCount =  LEN(lcRpStatus)
IF StatCount <> 4
  lcFlt5 = " BOMHEADR.CSTATUS IN ("
  lnCnt = 1
  FOR I = 1 TO StatCount
    lcFlt5 = IIF(lnCnt = 1,lcFlt5 + " '" + SUBSTR(lcRpStatus,I,1) + "' ",lcFlt5 + ",'" + SUBSTR(lcRpStatus,I,1) + "' ")
    lnCnt = 2
  ENDFOR 
  lcFlt5 = lcFlt5 + ")"
  &lcWhereCondition = IIF(EMPTY(&lcWhereCondition),lcFlt5,&lcWhereCondition+ " AND " + lcFlt5)
ENDIF 
RETURN &lcWhereCondition
*!
*!*************************************************************
*! Name      : lfGetStyleCursor
*! Developer : Heba Fathi
*! Date      : 11/10/2004
*! Purpose   : get cursor from style to make join in sql and to 
*!             get style information.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*!
FUNCTION lfGetStyleCursor
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment, loSqlConnection

*-- 1-First collect the style file expression
LOCAL lcFoxFlt,llFabricCursor
lcFoxFlt = ""
llFabricCursor = .F.

*-- add condition of style procurement type(I -> Imported, D-> Domestic, B-> Both)
lcFoxFlt = IIF(lcStyTyp = 'D'," STYLE.MAKE = .T. ",IIF(lcStyTyp = 'I'," STYLE.MAKE = .F. "," .T. "))

*-- HFK, B038982, 02/01/2005 [Start]
lcSeasonStat = ""
lcGroupStat  = ""
lcColorStat = ""
*-- HFK, B038982, 02/01/2005 [End]

    
*-ADD Fox Expression TO WHERE COND
IF LEN(loOGScroll.lcRpFoxExp) > 1
  lcExp = loOGScroll.lcRpFoxExp
  lnOccur = OCCURS('.AND.',lcExp)
  IF lnOccur > 0
    FOR lnCount = 1 TO lnOccur + 1
      lnStart = IIF(lnCount = 1 , 1 , ATC('.AND.',lcExp,lnCount-1) + 5)
      lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC('.AND.',lcExp,lnCount))
      lnLength = lnEnd - lnStart +IIF(lnCount = lnOccur + 1,1,0)
      lcTake = SUBSTR(lcExp,lnStart,lnLength)
      IF ATC('STYLE.CSTYMAJOR',lcTake)= 0 .AND. ATC('STYLE.SEASON',lcTake)= 0 .AND. ATC('STYLE.CSTYGROUP',lcTake)= 0  .AND. ;
      ATC('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)',lcTake)= 0 .AND. ATC('STYLE.FABRIC',lcTake)= 0  
        lcFoxFlt = lcFoxFlt + " AND " + lcTake
      ELSE
      *-- HFK, B038982, 02/01/2005 [Start]
      DO CASE
      *------------ Case Season Filter ------------*
        CASE  ATC('STYLE.SEASON',lcTake) > 0
            lnSeasonCondition = ASCAN(loOGScroll.laOgVrFlt,'STYLE.SEASON')
            lnSeasonCondition = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnSeasonCondition,1)
            lcSeasonCondition = loOGScroll.laOgVrFlt[lnSeasonCondition,6]
            IF !EMPTY(lcSeasonCondition)
              lcSeasonFile = loOGScroll.gfTempName()
              gfCrtTmp(lcSeasonFile,"(Season C(6))",,"",.F.)
             lnSepOccur = OCCURS("|",lcSeasonCondition)
        IF lnSepOccur = 0
          lcseason = lcSeasonCondition
          INSERT INTO &lcSeasonFile (Season) VALUES (lcseason)
        ELSE
          FOR lnSeasons = 1 TO lnSepOccur+1
            lcSeason = IIF(lnSeasons=1,SUBSTR(lcSeasonCondition,1,6),SUBSTR(lcSeasonCondition,ATC('|',lcSeasonCondition,lnSeasons-1)+1,6))
            INSERT INTO &lcSeasonFile (Season) VALUES (lcseason)
          ENDFOR
        ENDIF 
             lcSeasonStat = " INNER JOIN " + lcSeasonFile + " TmpSeason On Style.Season = TmpSeason.Season "      
        ENDIF 
      
      *------------ Case Group Filter ------------*      
      CASE  ATC('STYLE.CSTYGROUP',lcTake)> 0
      lnGroupCondition = ASCAN(loOGScroll.laOgVrFlt,'STYLE.CSTYGROUP')
      lnGroupCondition = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnGroupCondition,1)
      lcGroupCondition = loOGScroll.laOgVrFlt[lnGroupCondition,6]
      IF !EMPTY(lcGroupCondition)
              lcGroupFile = loOGScroll.gfTempName()
              gfCrtTmp(lcGroupFile,"(cStyGroup C(6))",,"",.F.)
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
          lcGroupStat = " INNER JOIN " + lcGroupFile + " TmpGroup On Style.CSTYGROUP = TmpGroup.CSTYGROUP"                
        ENDIF 
      *------------ Case Color Filter ------------*
      CASE  ATC('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)',lcTake)> 0
        lnColorCondition = ASCAN(loOGScroll.laOgFxFlt,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)')
        lnColorCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnColorCondition,1)
        lcColorCondition = loOGScroll.laOgFxFlt[lnColorCondition,6]
        IF !EMPTY(lcColorCondition)
          lcColorFile = loOGScroll.gfTempName()
          gfCrtTmp(lcColorFile,"(Color C(6))",,"",.F.)
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
          lcColorStat = " INNER JOIN " + lcColorFile + " TmpColor On SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) = TmpColor.Color"
        ENDIF 
      ENDCASE
      *-- HFK, B038982, 02/01/2005 [End]      
      ENDIF 
    ENDFOR 
  ENDIF 
ENDIF 

*- Add Primary fabrics
lnPrFabCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.FABRIC')
lnPrFabCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPrFabCondition,1)
lcPrFabFile = loOGScroll.laOGFxFlt[lnPrFabCondition,6]
IF !EMPTY(lcPrFabFile) .AND. RECCOUNT('&lcPrFabFile')<> 0 
  SELECT (lcPrFabFile)
  lcTmpPrFab = loOGSCroll.gfTempName()
  COPY TO oAriaApplication.WorkDir+lcTmpPrFab+".dbf"
  IF !USED('&lcTmpPrFab')
    USE oAriaApplication.WorkDir + lcTmpPrFab + ".DBF" IN 0
  ENDIF 
  llFabricCursor = .T.
ENDIF 


*!*  2- prepare Temp. files
lcSelectedStyles = loOGScroll.gfTempName()
lcTmpSelected = loOGScroll.gfTempName()
SELECT DISTINCT cItmMajor FROM &lcGetData INTO CURSOR &lcTmpSelected
SELECT &lcTmpSelected
COPY TO oAriaApplication.DataDir + lcSelectedStyles + ".DBF"
USE oAriaApplication.DataDir + lcSelectedStyles + ".DBF" IN 0 

*-- 3-identify calling select statement expression.
lcStatement = " STYLE.CSTYMAJOR,STYLE.STYLE,STYLE.DESC1,STYLE.PRICEA,STYLE.CDUTYCUR,STYLE.CPRICECUR"
lcStatement = lcStatement + ",STYLE.SEASON,STYLE.CDIVISION,STYLE.CSTYGROUP,STYLE.PRICEB,STYLE.PRICEC"
lcStatement = lcStatement + ",STYLE.SCALE,STYLE.PATTERN,STYLE.PREPAK FROM STYLE "
lcStatement = lcStatement + " INNER JOIN " + lcSelectedStyles + " TMPCURSOR ON STYLE.cStyMajor = TMPCURSOR.cItmMajor "
IF llFabricCursor
  lcStatement = lcStatement + " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpPrFab+".dbf' ON STYLE.FABRIC = "+lcTmpPrFab+".cStyMajor"
ENDIF 
*-- HFK, B038982, 02/01/2005 [Start]
IF !EMPTY(lcSeasonStat)
  lcStatement = lcStatement + lcSeasonStat
ENDIF 
IF !EMPTY(lcGroupStat)
  lcStatement = lcStatement + lcGroupStat
ENDIF 
*-- HFK, B038982, 02/01/2005 [End]
IF !EMPTY(lcColorStat)
  lcStatement = lcStatement + lcColorStat
ENDIF 

lcStatement = lcStatement + ' WHERE ' + lcFoxFlt
lcTmpStyFile = loOGScroll.gfTempName()
SELECT &lcStatement INTO DBF (oAriaApplication.WorkDir+ lcTmpStyFile)
USE IN (lcTmpStyFile)
IF USED ('STYLE_A')
  USE IN ('STYLE_A')
ENDIF 
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
*USE (oAriaApplication.WorkDir+ lcTmpStyFile) IN 0 ALIAS Style_A
USE (oAriaApplication.WorkDir+ lcTmpStyFile) IN 0 ALIAS Style_A EXCLUSIVE 
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]
