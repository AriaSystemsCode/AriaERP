*:**********************************************************************
*: Program file       : CSTSHTRPT.PRG
*: Program description: Cost Sheet Report Program
*: Module             : PO, MF
*: Developer          : Heba Fathi (HFK)
*: Tracking Job Number: #037708
*: Date               : 10/06/2004
*:**********************************************************************
*: Calls: 
*:            Programs: 
*:             Screens: 
*:     Global Function: gfGetMemVar()
*:**********************************************************************
*: Called From        : POPOCS, MFCTCS
*:**********************************************************************
*: Passed Parameters  : NONE
*:**********************************************************************
*: Modifications : 
*: B038984,1 HFK 01/27/2005 fixing bug of emptying the Cuttint ticket brows title
*: B038984,2 HFK 01/30/2005 fixing bug of msg "alias not found"
*: B127278,1 MMT 04/09/2005 displaying no of records
*: B131721,1 WSH 04/02/2006 Problem in the C/T cost sheet printing.
*: B607925,1 WAM 01/10/2007 Fix calculating of cost when select to print Actual cost
*: B608123,1 MMT 06/18/2007 fix bug of wrong totals in report layout MF 
*: B608221,1 WAM 08/15/2007 Change the technique used to create the temporary files
*: B608609,1 WAM 07/09/2008 Fix bug in collecting data [T20080501.0007 ]
*:**********************************************************************
*:
#INCLUDE R:\ARIA4XP\REPORTS\CSTSHTRP.H
loOGScroll.cCRPaperSize = 'A4'
loogScroll.cCROrientation = 'P'

LOCAL lcWhereCnd
IF lcModule = 'PO'
  IF !llImpCost
    * Message  'System has not been setup to use detailed costing
    *           therefore P/O cost sheet report is not available.'
    =gfModalGen('TRM34123B00000','DIALOG')
    RETURN .F.
  ENDIF
ENDIF 
STORE "" TO lcWhereCnd
*-Check filter changes
IF loOGScroll.llOGFltCh
  STORE "" TO lcWhereCnd
  IF lcModule = "PO"
    *- get CtktBom FIELDS
    lcCtktFld = "PosHdr.Po as CutTkt,cTktBom.UntCost,cTktBom.MfgCode,cTktBom.Item,cTktBom.cCatGTyp,"
    lcCtktFld = lcCtktFld + "cTktBom.Typ,cTktBom.Req_Qty,cTktBom.DyeLot,cTktBom.[Desc],cTktBom.cImTyp"
    lcSelTables = "POSHDR (INDEX = POSHDR) LEFT OUTER JOIN CTKTBOM (INDEX = CTKTBOM) ON  POSHDR.PO = CTKTBOM.CUTTKT "      
    lcWhereCnd  = " POSHDR.NSTYORDER <> 0 "
    *- Add sql expression to the sql condition
    IF !EMPTY(loOGScroll.lcRpSqlExp)
      lcWhereCnd = lcWhereCnd + " AND " + loOGScroll.lcRpSqlExp
    ENDIF 

    *- Add cost sheet type to sql condition (Style PO/InterLocation PO)
    IF lcCSType = 'S'
      lcWhereCnd = lcWhereCnd +  " AND POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE = 'P' "
    ELSE 
      lcWhereCnd = lcWhereCnd +  " AND POSHDR.CBUSDOCU = 'N' AND POSHDR.CSTYTYPE= 'N' "
    ENDIF 
    lcPOCondition = lcWhereCnd    

    *- Add status to the sql condition ( if not selecting All only)
    IF lcRpStatus <> 'L'
        lcWhereCnd = lcWhereCnd +  " AND POSHDR.STATUS = '" + lcRpStatus + "'"      
    ELSE
        lcWhereCnd = lcWhereCnd +  " AND((POSHDR.STATUS IN ('A','O','C','S') AND ( CTKTBOM.CIMTYP='I') )  OR POSHDR.STATUS = 'H') "
    ENDIF 
    

    lcCtktData = loOGSCroll.gfTempName()
    lcCTktStatment = "Select " + lcCtktFld + " FROM " + lcSelTables + " WHERE " + lcWhereCnd
    lnCtktResult = loOGScroll.oRDA.SqlRun (lcCTktStatment,lcCtktData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))

    IF lnCtktResult > 0  && got data from Sql
      SELECT &lcCtktData
      *B608221,1 WAM 08/15/2007 Change the technique used to create the temporary files
      SELECT * , 00000000000.000 AS RequireQty, 000000000000000.000 AS EstExCost, 00000000000.000 AS Used_Qty, 0000000000000.000 AS Act_ExCost FROM (lcCtktData) INTO DBF (oAriaApplication.WOrkDir+lcDetail)
*!*	      COPY TO oAriaApplication.WorkDir + lcDetail + ".DBF" WITH CDX  
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN RequireQty  N(15,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN EstExCost   N(19,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN Used_Qty    N(15,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN Act_ExCost  N(17,3)
      *B608221,1 WAM 08/15/2007 (End)
      SELECT &lcDetail

      IF lfNoRecord()  && no records found
        RETURN .F.
      ELSE  
        lnBuffering = CURSORGETPROP("Buffering","&lcCtktData")
        =CURSORSETPROP("Buffering",3,"&lcCtktData")
        INDEX ON CutTkt+Typ TAG &lcDetail
      ENDIF 

      *- get distinct PO# into Temp. file
      lcTmpPO = loOGScroll.gfTempName()
      SELECT DIST (CutTkt) AS PO FROM &lcCtktData INTO DBF (oAriaApplication.WorkDir+ lcTmpPO)
      *- check if user selected some of Target Location POs, if Yes then we get this locations and make join with the 
      *- selected Pos and get information from PosLn, Else we transform selected POs directly to a Sql file and join
      lnWarePos = ASCAN(loOGScroll.laogFxFlt,'lcRpWare')
      IF lnWarePos <> 0 
        lnWarePos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnWarePos,1)
        lnWarePos = loOGScroll.laogFxFlt[lnWarePos,6]
        IF !EMPTY(lnWarePos)
          SELECT (lcTmpPO)
          lcPoSel = ""
          lcPoSel = loOGScroll.gfSqlTempName('','PO C(6)',lcTmpPO,'PO')
          lcWareHouse = ""
          lcWareHouse = loOgScroll.gfSQLTempName('','cWareCode C(6)',lnWarePos,'cWareCode')
          IF EMPTY(lcPoSel) .AND. EMPTY(lcWareHouse)
            *-- SQL connection Error. Can't Proceed
            =gfModalGen('TRM00416B40011','ALERT')
            RETURN .F.
          ELSE 
            lcSelect = " Select PosLn.PO,PosLn.cWareCode From Posln (INDEX = POSLNW) INNER JOIN "+ lcPoSel +  " TmpPo ON TmpPo.PO = POSLN.PO "
            lcSelect = lcSelect + " INNER JOIN "+ lcWareHouse +  " TmpWare ON TmpWare.cWareCode = POSLN.cWareCode "
            lcSelect = lcSelect + " Where PosLn.cBusDocu ='N' and PosLn.cStyType = 'N' "
            lcFilter = loOGScroll.gfTempName()
            lnPOResult = loOGScroll.oRDA.SqlRun (lcSelect,lcFilter,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
            IF lnPOResult > 0
              SELECT &lcFilter
              lcSelected = loOGScroll.gfTempName()
              COPY TO oAriaApplication.WorkDir + lcSelected + ".DBF"
              USE oAriaApplication.WorkDir + lcSelected + ".DBF" IN 0 
              lcSelPO = ""
              *-convert Temp. file to Sql Temp file
              lcSelPO = loOgScroll.gfSQLTempName('','PO C(6)',lcSelected,'PO')
              IF EMPTY(lcSelPO)
                *-- SQL connection Error. Can't Proceed
                =gfModalGen('TRM00416B40011','ALERT')
                RETURN .F.
              ENDIF
            ENDIF 
          ENDIF
        ELSE 
          lcSelPO = ""
          *-convert Temp. file to Sql Temp file
          lcSelPO = loOgScroll.gfSQLTempName('','PO C(6)',lcTmpPO,'PO')
          IF EMPTY(lcSelPO)
            *-- SQL connection Error. Can't Proceed
            =gfModalGen('TRM00416B40011','ALERT')
            RETURN .F.
          ENDIF
        ENDIF 
      ENDIF  

      *- get fields from PO file.
      IF lcRpStatus <> 'L'
        lcPOCondition = lcPOCondition +  " AND POSHDR.STATUS = '" + lcRpStatus + "'"      
      ELSE
        lcPOCondition = lcPOCondition +  " AND POSHDR.STATUS <> 'X' "      
      ENDIF 
  	  lcPOData = loOGScroll.gfTempName()
      lcPoFld = "POSHDR.Po,POSHDR.Available,POSHDR.Vendor,POSHDR.cWareCode,POSHDR.cPriceCur,POSHDR.cDutyCur,POSHDR.nPriceRat,"
      lcPoFld = lcPoFld + "POSHDR.nLan_Cost1,POSHDR.nLan_Cost2,POSHDR.nLan_Cost3,POSHDR.nLan_Cost4,POSHDR.nLan_Cost5,POSHDR.nLan_Cost6,"
      lcPoFld = lcPoFld + "POSHDR.nLan_Cost7,POSHDR.nICost1,POSHDR.nICost2,POSHDR.nICost3,POSHDR.nICost4,POSHDR.nICost5,POSHDR.nICost6,"
      lcPoFld = lcPoFld + "POSHDR.nICost7,POSHDR.nAct_Cost1,POSHDR.nAct_Cost2,POSHDR.nAct_Cost3,POSHDR.nAct_Cost4,POSHDR.nAct_Cost5,"
      lcPoFld = lcPoFld + "POSHDR.nAct_Cost6,POSHDR.nAct_Cost7,POSHDR.nStyOrder,POSHDR.Cancel,POSHDR.Receive,POSHDR.[Open],POSHDR.Damage,"
      lcPoFld = lcPoFld + "POSHDR.nCurrUnit,POSHDR.Complete,POSHDR.Status,POSHDR.cDivision,POSHDR.Link_Code,SPACE(6) As Account,"
      lcPoFld = lcPoFld + "SPACE(25) AS WIPAcnt,SPACE(30) AS Vend_Name,SPACE(30) AS Division,POSHDR.nDCurUnit,POSHDR.nDutyRat"
      lcSelTable1 = "POSHDR (INDEX = POSHDR) INNER JOIN "+ lcSelPO +  " TmpPO ON TmpPO.PO = POSHDR.PO "
      lcPOStatment = "Select " + lcPoFld + " FROM " + lcSelTable1 + " WHERE " + lcPOCondition
      lnPOResult = loOGScroll.oRDA.SqlRun (lcPOStatment,lcPOData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
      IF lnPOResult > 0 
        SELECT &lcPOData
        IF lfNoRecord()  && no records found
          RETURN .F.
        ELSE  
          lnBuffering = CURSORGETPROP("Buffering","&lcPOData")
          =CURSORSETPROP("Buffering",3,"&lcPOData")
          INDEX ON PO TAG &lcHeader
          DO lpCollectPoData
          *-
          *- Start Defining Variables for Crystal
          lcReportName = LANG_CSTSHTRP_PoRepName
          IF llDyelot  && Dyelot used
            IF llConfg  && configuration used 
              lcDyeLabel = LANG_CSTSHTRP_Config
            ELSE
              lcDyeLabel = LANG_CSTSHTRP_Dylot
            ENDIF 
          ELSE 
            lcDyeLabel = '   '
          ENDIF 

          *- Declare parameters sent to crystal
          DECLARE loOGScroll.laCRParams[18,2]
          loOGScroll.laCRParams[1,1] = 'ReportName'
          loOGScroll.laCRParams[1,2] = lcReportName
          loOGScroll.laCRParams[2,1] = 'OpTitle'
          loOGScroll.laCRParams[2,2] = lcRpTITLE
          loOGScroll.laCRParams[3,1] = 'DyeLabel'
          loOGScroll.laCRParams[3,2] = lcDyeLabel
          loOGScroll.laCRParams[4,1] = 'Short1'
          loOGScroll.laCRParams[4,2] = lcShrt1
          loOGScroll.laCRParams[5,1] = 'Short2'
          loOGScroll.laCRParams[5,2] = lcShrt2
          loOGScroll.laCRParams[6,1] = 'Short3'
          loOGScroll.laCRParams[6,2] = lcShrt3
          loOGScroll.laCRParams[7,1] = 'Short4'
          loOGScroll.laCRParams[7,2] = lcShrt4
          loOGScroll.laCRParams[8,1] = 'Short5'
          loOGScroll.laCRParams[8,2] = lcShrt5
          loOGScroll.laCRParams[9,1] = 'Short6'
          loOGScroll.laCRParams[9,2] = lcShrt6
          loOGScroll.laCRParams[10,1] = 'Short7'
          loOGScroll.laCRParams[10,2] = lcShrt7
          loOGScroll.laCRParams[11,1] = 'long1'
          loOGScroll.laCRParams[11,2] = lcLong1
          loOGScroll.laCRParams[12,1] = 'long2'
          loOGScroll.laCRParams[12,2] = lcLong2
          loOGScroll.laCRParams[13,1] = 'long3'
          loOGScroll.laCRParams[13,2] = lcLong3
          loOGScroll.laCRParams[14,1] = 'long4'
          loOGScroll.laCRParams[14,2] = lcLong4
          loOGScroll.laCRParams[15,1] = 'long5'
          loOGScroll.laCRParams[15,2] = lcLong5
          loOGScroll.laCRParams[16,1] = 'long6'
          loOGScroll.laCRParams[16,2] = lcLong6
          loOGScroll.laCRParams[17,1] = 'long7'
          loOGScroll.laCRParams[17,2] = lcLong7
          loOGScroll.laCRParams[18,1] = 'PoType'
          loOGScroll.laCRParams[18,2] = lcCSType
            
          lnSelect = SELECT()
          SELECT &lcDetail
          SET ORDER TO TAG &lcDetail
          LOCATE
          SELECT &lcPOData
          COPY TO oAriaApplication.WorkDir + lcHeader + ".DBF" WITH CDX 
		  *-B038984,2 HFK 01/30/2005 [Start]
          *-USE &lcPath.&lcHeader          
		  USE oAriaApplication.WorkDir + lcHeader + ".DBF" IN 0           
		  *-B038984,2 HFK 01/30/2005 [End]
          SELECT &lcHeader
          *--B127278,1 mmt 04/09/2005 displaying no of records[Start]
          lcRecCount = LTRIM(STR(RECCOUNT(),7))
          WAIT WINDOW 'SORTING &lcRecCount RECORDS FOR P/O COST SHEET REPORT ...' TIMEOUT 1 &&NOWAIT 
          *--B127278,1 mmt 04/09/2005 displaying no of records[End]
          SET RELATION TO 
          SET ORDER TO TAG &lcHeader
          LOCATE 
          SET RELATION TO &lcHeader..PO INTO &lcDetail
          *-declare tables sent to crytal report
          DECLARE loOGScroll.laCRTables[2]
          loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcDetail + ".DBF"
          loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcHeader + ".DBF"
          *-close tables and preview
          SELECT (lnSelect)
          USE IN &lcDetail
          USE IN &lcHeader
          WAIT CLEAR 
          loOgScroll.lcOGLastForm = lcRpForm
          llClearFn = .T.
          loogScroll.cCROrientation = 'P'        
          =gfDispRe()
        ENDIF 
      ELSE   && PO Data from sql didn't succeed
        =loOGScroll.oRDA.CheckRetResult("sqlrun",lnPOResult,.T.)
        RETURN .F.
      ENDIF 
    ELSE && sql return result from Ctktbom data is < 0
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnCtktResult,.T.)
      RETURN .F.
    ENDIF && lnResult > 0
*-----------------------------  MF Module Part  ---------------------------------------*
  ELSE  && MF module
    lcWherCnd = " POSHDR.NSTYORDER <> 0 AND POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE = 'U' "
    IF lcStatus  <> 'L'
      lcWherCnd = lcWherCnd +  " AND POSHDR.STATUS = '" + lcStatus + "'"
    ENDIF 
    IF !EMPTY(loOGScroll.lcRpSqlExp)
      lcWherCnd = lcWherCnd + " AND " + loOGScroll.lcRpSqlExp
    ENDIF 
    lcCtktCond = lcWherCnd + " AND CtktBom.CIMTYP = 'M' "
    *-GET DATA FROM CTKTBOM file
    lcCtktFld = "cTktBom.UntCost,cTktBom.Item,cTktBom.CutTkt,cTktBom.Typ,cTktBom.Req_Qty,CTKTBOM.MFGCODE,"
    lcCtktFld = lcCtktFld + "cTktBom.DyeLot,cTktBom.[Desc],cTktBom.cCatgtyp,SPACE(6) AS ItemColor"
    lcCtktFile = " CtktBom (INDEX = CTKTYP) INNER JOIN POSHDR (INDEX = POSHDR) ON  POSHDR.PO = CTKTBOM.CUTTKT " 
    lcCTktStatment = "Select " + lcCtktFld + " FROM " + lcCtktFile + " WHERE " + lcCtktCond
    lcCtktData = loOGScroll.gfTempName()
    lnCtktResult = loOGScroll.oRDA.SqlRun (lcCTktStatment,lcCtktData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnCtktResult > 0
      SELECT &lcCtktData
      *B608221,1 WAM 08/15/2007 Change the technique used to create the temporary files
      SELECT * ,0000000.000 AS UnitCost,0000000.000 AS YARDGE,0000000.000 AS Extd_Cost ,0000000.000 AS Avg_Yield ,0000000.000 AS Avg_Piece FROM (lcCtktData) INTO DBF (oAriaApplication.WOrkDir+lcDetail)
*!*	      COPY TO oAriaApplication.WorkDir + lcDetail + ".DBF" WITH CDX
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN UnitCost  N(11,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN YARDGE    N(11,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN Extd_Cost N(11,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN Avg_Yield N(11,3)
*!*	      ALTER TABLE oAriaApplication.WorkDir + lcDetail + ".DBF" ADD COLUMN Avg_Piece N(11,3)
      *B608221,1 WAM 08/15/2007 (End)
      SELECT &lcDetail
      IF lfNoRecord()  && no records found
        RETURN .F.
      ELSE  
        lnBuffering = CURSORGETPROP("Buffering","&lcCtktData")
        =CURSORSETPROP("Buffering",3,"&lcCtktData")
        INDEX ON CutTkt + Typ TAG &lcDetail
        INDEX ON CutTkt + MfgCode TAG MfgCode
      ENDIF 
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
      RETURN .F.
    ENDIF 

    *- get fields from bomline file
    lcBomLineSelect = " SELECT cTktNo,STYLE,MFGCODE,cBomTyp,ITEM,cCatgTyp FROM BOMLINE (INDEX = BOMLINE) WHERE BOMLINE.CIMTYP = 'M' AND BOMLINE.CTYPE = '1' AND BOMLINE.CINVTYPE = 0001"
    BomLine = loOGScroll.gfTempName()
    lnBomLineResult = loOGScroll.oRDA.SqlRun (lcBomLineSelect,BomLine,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnBomLineResult > 0
      lnBuffering = CURSORGETPROP("Buffering","&BomLine")
      =CURSORSETPROP("Buffering",3,"&BomLine")
      INDEX ON cTktNo TAG &BomLine
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
      RETURN .F.
    ENDIF

    *-get fields from Posln File
    lcPoSelect = " SELECT PO,Style,DyeLot,TotQty,Trancd FROM POSLN (INDEX = POSLN) WHERE POSLN.CBUSDOCU = 'P' AND POSLN.CSTYTYPE = 'U'"
    Posln = loOGScroll.gfTempName()
    lnPoslnResult = loOGScroll.oRDA.SqlRun (lcPoSelect,Posln,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))    
    IF lnPoslnResult > 0
      lnBuffering = CURSORGETPROP("Buffering","&Posln")
      =CURSORSETPROP("Buffering",3,"&Posln")
      INDEX ON PO+Style TAG &Posln
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
      RETURN .F.
    ENDIF

    *- select statement for mfgoprhd:
    lcMFOprSelect = "SELECT MFGOprHd.cTktNo,MFGOprHd.cOprCode,MFGOprHd.cContCode,MFGOprHd.COPERSEQ FROM MFGOprHd (INDEX = MFGOPRHD) INNER JOIN POSHDR (INDEX = POSHDR) ON POSHDR.PO = MFGOPRHD.CTKTNO "
    lcMfgCnd = " MFGOPRHD.lInHouse = 0 AND MFGOPRHD.CIMTYP = 'M'"
    lcMfgStatment = lcMFOprSelect + " WHERE " + lcMfgCnd
    lnMfgResult = loOGScroll.oRDA.SqlRun (lcMfgStatment,lcMfgData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnMfgResult>0
      lnBuffering = CURSORGETPROP("Buffering","&lcMfgData")
      =CURSORSETPROP("Buffering",3,"&lcMfgData")
      INDEX ON CtktNo TAG &lcMfgData
      SELECT &lcMfgData
      IF lfNoRecord()  && no records found
        RETURN .F.
      ENDIF 
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
      RETURN .F.
    ENDIF 

    SELECT &lcCtktData
    lcTmpPO = loOGScroll.gfTempName()
    SELECT DIST (CutTkt) AS PO FROM &lcCtktData INTO DBF (oAriaApplication.WorkDir+ lcTmpPO + '.DBF')
    lcSelPO = ""
    *- get distinct Cut Tickets into Sql Temp File
    lcSelPO = loOgScroll.gfSQLTempName('','PO C(6)',lcTmpPO,'PO')
    IF EMPTY(lcSelPO)
      *-- SQL connection Error. Can't open The Report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ELSE 
      *- get fields from POSHDR file
      lcPoFld = "POSHDR.Po,POSHDR.Season,POSHDR.cDivision,POSHDR.Style,POSHDR.nStyOrder,POSHDR.Cancel,POSHDR.Status,POSHDR.DEL_DATE,CutPick.[Order],"
      lcPoFld = lcPoFld + "POSHDR.Receive,POSHDR.[Open],POSHDR.Pcs_Act,POSHDR.Entered,POSHDR.Pattern,POSHDR.cTktType,POSHDR.Damage,POSHDR.Complete,"
      lcPoFld = lcPoFld + "SPACE(6) AS MFG_OPR1,SPACE(6) AS MFG_OPR2,SPACE(6) AS MFG_OPR3,SPACE(8) AS CONTR1,SPACE(8) AS CONTR2,SPACE(8) As Account,"
      lcPoFld = lcPoFld + "SPACE(8) AS CONTR3,SPACE(30) AS VEND_NAME1,SPACE(30) AS VEND_NAME2,SPACE(30) AS VEND_NAME3,SPACE(30) AS MFG_DESC1,"
      lcPoFld = lcPoFld + "SPACE(30) AS MFG_DESC2,SPACE(30) AS MFG_DESC3,SPACE(17) AS Season_Des,SPACE(30) AS Div_Desc,SPACE(3) As ScaleID,"
      lcPoFld = lcPoFld + "SPACE(30) As ScaleDesc,SPACE(8) As PrimVend,SPACE(15) As StyleDesc,CutPick.TranCd As CPTranCd"
      lcSelTable1 = "POSHDR (INDEX = POSHDR) INNER JOIN "+ lcSelPO +  " TmpPO ON TmpPO.PO = POSHDR.PO "
      lcSelTable1 = lcSelTable1 + " LEFT OUTER JOIN CutPick (INDEX = CUTPKORD) ON  POSHDR.PO = CUTPICK.CTKTNO "
      lcPOStatment = "Select " + lcPoFld + " FROM " + lcSelTable1 + " WHERE " + lcWherCnd
      lcPOData = loOGScroll.gfTempName()
      lnPOResult = loOGScroll.oRDA.SqlRun (lcPOStatment,lcPOData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
      IF lnPOResult > 0 
        SELECT &lcPOData
        *B608221,1 WAM 08/15/2007 Change the technique used to create the temporary files
        SELECT * ,00000000000.000 AS Rate1,00000000000.000 AS Rate2,00000000000.000 AS Rate3,000000.00 AS SellPrice ,0000000000000.000 AS Pieces,000000000.000 AS ActualQty ,.F. AS llVendor  FROM (lcPOData) INTO DBF (oAriaApplication.WOrkDir+lcHeader)
*!*	        COPY TO oAriaApplication.WorkDir + lcHeader + ".DBF" WITH CDX 
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN Rate1     N(15,3)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN Rate2     N(15,3)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN Rate3     N(15,3)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN SellPrice N(9,2)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN Pieces    N(17,3)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN ActualQty N(13,3)
*!*	        ALTER TABLE oAriaApplication.WorkDir + lcHeader + ".DBF" ADD COLUMN llVendor  L
        *B608221,1 WAM 08/15/2007 (End)
        SELECT &lcHeader
        IF lfNoRecord()  && no records found
          RETURN .F.
        ELSE 
          lnBuffering = CURSORGETPROP("Buffering","&lcPOData")
          =CURSORSETPROP("Buffering",3,"&lcPOData")
          INDEX ON PO TAG &lcHeader
          =lfCheckVendor()
          IF lfNoRecord()  && no records found
            RETURN .F.
          ENDIF 
        ENDIF 
      ELSE 
        =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
        RETURN .F.
      ENDIF
    ENDIF 

    *- SELECT BOMCOST FIELDS
    *B607925,1 WAM 01/10/2007 Get quantity issues and actual cost for each cost element
*!*	    lcBomCostStat = " SELECT BOMCOST.CTKTNO,SUM(BomCost.nTotQty) AS TotQty,SUM(BomCost.nUnitACst * BomCost.nTotQty)AS UnitActCst FROM BOMCOST "
*!*	    lcBomCostStat = lcBomCostStat + "(INDEX = POBOMCLS) INNER JOIN CTKTBOM (INDEX = CTKTYP)ON BOMCOST.cBomType"
*!*	    lcBomCostStat = lcBomCostStat + "+BOMCOST.cTktNo+BOMCOST.ITEM+BOMCOST.MFGCODE+BOMCOST.cDyeLot = CTKTBOM.Typ+"
*!*	    lcBomCostStat = lcBomCostStat + "CTKTBOM.CUTTKT+CTKTBOM.ITEM+CTKTBOM.MFGCODE+CTKTBOM.DYELOT INNER JOIN " + lcSelPO + ""
*!*	    lcBomCostStat = lcBomCostStat + " TmpPO ON TmpPO.PO= CTKTBOM.CUTTKT WHERE BOMCOST.cIMtyp = 'M' GROUP BY BOMCOST.CTKTNO"

    lcBomCostStat = " SELECT BOMCOST.CTKTNO,BOMCOST.cBomType,BOMCOST.ITEM,BOMCOST.MFGCODE,BOMCOST.cDyeLot,SUM(BomCost.nTotQty) AS TotQty,SUM(BomCost.nUnitACst * BomCost.nTotQty)AS UnitActCst FROM BOMCOST "
    lcBomCostStat = lcBomCostStat + "(INDEX = POBOMCLS) INNER JOIN CTKTBOM (INDEX = CTKTYP)ON BOMCOST.cBomType"
    lcBomCostStat = lcBomCostStat + "+BOMCOST.cTktNo+BOMCOST.ITEM+BOMCOST.MFGCODE+BOMCOST.cDyeLot = CTKTBOM.Typ+"
    lcBomCostStat = lcBomCostStat + "CTKTBOM.CUTTKT+CTKTBOM.ITEM+CTKTBOM.MFGCODE+CTKTBOM.DYELOT INNER JOIN " + lcSelPO + ""
    lcBomCostStat = lcBomCostStat + " TmpPO ON TmpPO.PO= CTKTBOM.CUTTKT WHERE BOMCOST.cIMtyp = 'M' GROUP BY BOMCOST.CTKTNO,BOMCOST.cBomType,BOMCOST.ITEM,BOMCOST.MFGCODE,BOMCOST.cDyeLot"
    *B607925,1 WAM 01/10/2007 (End)

    lcBomCostData = loOGScroll.gfTempName()
    lnBomCstResult = loOGScroll.oRDA.SqlRun (lcBomCostStat,lcBomCostData,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnBomCstResult > 0
      lnBuffering = CURSORGETPROP("Buffering","&lcBomCostData")
      =CURSORSETPROP("Buffering",3,"&lcBomCostData")
      *B607925,1 WAM 01/10/2007 Get quantity issues and actual cost for each cost element
      *INDEX ON CtktNo TAG &lcBomCostData
      INDEX ON CtktNo+cBomType+ITEM+MFGCODE+cDyeLot TAG &lcBomCostData
      *B607925,1 WAM 01/10/2007 (End)
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("sqlrun",lnResult,.T.)
      RETURN .F.
    ENDIF 
    *-call procedure to collect data  
    DO lpCollectMFData
    *-define variables needed for crystal
    lcReportName = LANG_CSTSHTRP_MfRepName
    IF XDYELOT_S
      IF llConfg
        lcDyeLabel = LANG_CSTSHTRP_Config
      ELSE
        lcDyeLabel = LANG_CSTSHTRP_Dylot
      ENDIF 
    ELSE 
      lcDyeLabel = '   '
    ENDIF 
    *-declare crystal parameters 
    
    *: B608123,1 MMT 06/18/2007 fix bug of wrong totals in report layout MF [Start]
    *DECLARE loOGScroll.laCRParams[11,2]
    DECLARE loOGScroll.laCRParams[12,2]
    *: B608123,1 MMT 06/18/2007 fix bug of wrong totals in report layout MF [End]
    
    loOGScroll.laCRParams[1,1] = 'ReportName'
    loOGScroll.laCRParams[1,2] = lcReportName
    loOGScroll.laCRParams[2,1] = 'OpTitle'
    loOGScroll.laCRParams[2,2] = XTITLE
    loOGScroll.laCRParams[3,1] = 'DyeLabel'
    loOGScroll.laCRParams[3,2] = lcDyeLabel
    loOGScroll.laCRParams[4,1] = 'StyleMarkUp'
    loOGScroll.laCRParams[4,2] = M_STYMARK
    loOGScroll.laCRParams[5,1] = 'long1'
    loOGScroll.laCRParams[5,2] = lcLong1
    loOGScroll.laCRParams[6,1] = 'long2'
    loOGScroll.laCRParams[6,2] = lcLong2
    loOGScroll.laCRParams[7,1] = 'long3'
    loOGScroll.laCRParams[7,2] = lcLong3
    loOGScroll.laCRParams[8,1] = 'long4'
    loOGScroll.laCRParams[8,2] = lcLong4
    loOGScroll.laCRParams[9,1] = 'long5'
    loOGScroll.laCRParams[9,2] = lcLong5
    loOGScroll.laCRParams[10,1] = 'long6'
    loOGScroll.laCRParams[10,2] = lcLong6
    loOGScroll.laCRParams[11,1] = 'long7'
    loOGScroll.laCRParams[11,2] = lcLong7
    
    *: B608123,1 MMT 06/18/2007 fix bug of wrong totals in report layout MF [Start]
    loOGScroll.laCRParams[12,1] = 'lcCostM'
    loOGScroll.laCRParams[12,2] = lcCostM
	*: B608123,1 MMT 06/18/2007 fix bug of wrong totals in report layout MF [End]
  
    lnSelect = SELECT()
    SELECT &lcDetail
    SET ORDER TO TAG &lcDetail
    LOCATE
    SELECT &lcHeader
  
*!*	    *--B127278,1 mmt 04/09/2005 displaying no of records[Start]
*!*	    lcRecCount = LTRIM(STR(RECCOUNT(),7))
*!*	    WAIT WINDOW 'SORTING &lcRecCount RECORDS FOR P/O COST SHEET REPORT ...' TIMEOUT 1 &&NOWAIT 
*!*	    *--B127278,1 mmt 04/09/2005 displaying no of records[End]

    SET RELATION TO 
    SET ORDER TO TAG &lcHeader
    LOCATE 
    SET RELATION TO &lcHeader..PO INTO &lcDetail
    SELECT (lnSelect)
    DECLARE loOGScroll.laCRTables[2]
    loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcDetail + ".DBF"
    loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcHeader + ".DBF"
    WAIT CLEAR 
    USE IN &lcHeader
    USE IN &lcDetail
     *--B127278,1 mmt 04/09/2005 displaying no of records[Start]
    lcRecCount = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'SORTING &lcRecCount RECORDS FOR P/O COST SHEET REPORT ...' TIMEOUT 1 &&NOWAIT 
    *--B127278,1 mmt 04/09/2005 displaying no of records[End]
    
    loOgScroll.lcOGLastForm = lcRpForm
    llClearFn = .T.
    loogScroll.cCROrientation = 'P'
    =gfDispRe()
  ENDIF && lcModule
ELSE && filter didn't change
*-------------------------- in Case click preview twice without changing filter ---------*
  IF llClearFn = .T. && Run Preview Twice
    IF lcModule = 'PO' 
	  *-B038984,2 HFK 01/30/2005 [Start]    
	  *- USE &lcPath.&lcHeader IN 0 
      *- USE &lcPath.&lcDetail IN 0 
  	  USE oAriaApplication.WorkDir + lcHeader + ".DBF" IN 0 
      USE oAriaApplication.WorkDir + lcDetail + ".DBF" IN 0 
	  *-B038984,2 HFK 01/30/2005 [End]      
      lnSelect = SELECT()
      IF RECCOUNT(lcHeader) = 0 
        =gfModalGen('TRM00052B00000','DIALOG')
        RETURN .F.
      ELSE 
        SELECT &lcDetail
        SET ORDER TO TAG &lcDetail
        LOCATE
        SELECT &lcHeader
        SET RELATION TO 
        SET ORDER TO TAG &lcHeader
        LOCATE 
        SET RELATION TO &lcHeader..PO INTO &lcDetail
        SELECT (lnSelect)
        USE IN &lcDetail
        USE IN &lcHeader
      ENDIF 
    ELSE    && MF module
	  *-B038984,2 HFK 01/30/2005 [Start]    
      *- USE &lcPath.&lcHeader IN 0 
      *- USE &lcPath.&lcDetail IN 0 
	  USE oAriaApplication.WorkDir + lcHeader + ".DBF" IN 0 
      USE oAriaApplication.WorkDir + lcDetail + ".DBF" IN 0 
	  *-B038984,2 HFK 01/30/2005 [End]    
      lnSelect = SELECT()
      IF RECCOUNT(lcHeader) = 0 
        =gfModalGen('TRM00052B00000','DIALOG')
        RETURN .F.
      ELSE 
        lnSelect = SELECT()
        SELECT &lcDetail
        SET ORDER TO TAG &lcDetail
        LOCATE
        SELECT &lcHeader
        SET RELATION TO 
        SET ORDER TO TAG &lcHeader
        LOCATE 
        SET RELATION TO &lcHeader..PO INTO &lcDetail
        SELECT (lnSelect)
        USE IN &lcHeader
        USE IN &lcDetail

      ENDIF 
    ENDIF && module ID
    WAIT CLEAR
    loOgScroll.lcOGLastForm = lcRpForm
    llClearFn = .T.
    loogScroll.cCROrientation = 'P'
     *--B127278,1 mmt 04/09/2005 displaying no of records[Start]
    lcRecCount = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'SORTING &lcRecCount RECORDS FOR P/O COST SHEET REPORT ...' TIMEOUT 1 &&NOWAIT 
    *--B127278,1 mmt 04/09/2005 displaying no of records[End]
    
    =gfDispRe()
  ELSE 
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN .F.
  ENDIF 
ENDIF && llogfltch
*!
*!*************************************************************
*! Name      : lfvVend
*! Developer : Heba Fathi (HFK)
*! Date      : 10/05/2004
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvVend
PRIVATE lcVar, lcObj
lcVar = OGSYS18(.T.)
lcObj = EVALUATE(OGSYS18(.T.))
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj     && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*!
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Fathi (HFK)
*! Date      : 05/10/2004
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18(.T.))
*!
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwOGWhen
*- get cost elements label

DIMENSION laCost[14,2]
laCost[1,1]  = IIF(lcModule = 'PO','M_CITYPE1 ','M_CMTYPE1 ')
laCost[2,1]  = IIF(lcModule = 'PO','M_CITYPE2 ','M_CMTYPE2 ')
laCost[3,1]  = IIF(lcModule = 'PO','M_CITYPE3 ','M_CMTYPE3 ')
laCost[4,1]  = IIF(lcModule = 'PO','M_CITYPE4 ','M_CMTYPE4 ')
laCost[5,1]  = IIF(lcModule = 'PO','M_CITYPE5 ','M_CMTYPE5 ')
laCost[6,1]  = IIF(lcModule = 'PO','M_CITYPE6 ','M_CMTYPE6 ')
laCost[7,1]  = IIF(lcModule = 'PO','M_CITYPE7 ','M_CMTYPE7 ')

laCost[8,1]  = IIF(lcModule = 'PO','M_CISLBL1 ','M_CMSLBL1 ')
laCost[9,1]  = IIF(lcModule = 'PO','M_CISLBL2 ','M_CMSLBL2 ')
laCost[10,1] = IIF(lcModule = 'PO','M_CISLBL3 ','M_CMSLBL3 ')
laCost[11,1] = IIF(lcModule = 'PO','M_CISLBL4 ','M_CMSLBL4 ')
laCost[12,1] = IIF(lcModule = 'PO','M_CISLBL5 ','M_CMSLBL5 ')
laCost[13,1] = IIF(lcModule = 'PO','M_CISLBL6 ','M_CMSLBL6 ')
laCost[14,1] = IIF(lcModule = 'PO','M_CISLBL7 ','M_CMSLBL7 ')

=gfGetMemvar(@laCost,oAriaApplication.ActiveCompanyID)

FOR lnI = 1 TO 7
  lcI = ALLTRIM(STR(lnI))
  lcShrt&lcI = laCost[lnI+7,2]
  lcLong&lcI = laCost[lnI+7,2]
ENDFOR
*!
*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/98
*! Purpose   : To set relation on or off when running the in range function 
*!             of selecting PO in the option grid.
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
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
*!
*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Get title to be displayed 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! 
FUNCTION lfGetTit
PRIVATE lcTit
lcTit  = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit + '#')
RETURN lcTit
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
  IF USED('&lcDetail')
    USE IN &lcDetail
  ENDIF 
  IF USED('&lcHeader')
    USE IN &lcHeader
  ENDIF 
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*!
*!*************************************************************
*! Name      : lpCollectPoData
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : Print Purchase orders Bill of Matirial.
*!*************************************************************
PROCEDURE lpCollectPoData
*- Get informaton required from BomCost File
SELECT &lcPOData
SCAN 
  WAIT WINDOW LANG_CSTSHTRP_MsgCollectPo + PO + " ...."  NOWAIT 
  *-- Get vendor name, division description, WIP gl account for header
  lcVenName = IIF(SEEK(&lcPOData..Vendor,'APVENDOR'),APVENDOR.cVenComp,'')
  lcDivision = gfCodDes(&lcPOData..CDivision, 'CDIVISION')
  lcWIPAcnt = &lcPOData->Link_Code + '-'
  IF(SEEK(&lcPOData->Link_Code+'013','GL_Link'))
    lcGlAccount = SUBSTR(GL_Link.GLACNT,1,18)
    lcWIPAcnt = lcWIPAcnt + lcGlAccount
  ENDIF 
  REPLACE &lcPOData..WIPAcnt   WITH lcWIPAcnt,;
          &lcPOData..Vend_Name WITH lcVenName,;
          &lcPOData..Division  WITH lcDivision
ENDSCAN
=lfCalculate()
*!
*!*************************************************************
*! Name      : lpCollectMFData
*! Developer : Heba Fathi
*! Date      : 10/20/2004
*! Purpose   : collect data for the Mf module
*!*************************************************************
*!
PROCEDURE lpCollectMFData

LOCAL lcVendor
lcVendor = ""

*B131721,1 WSH 04/02/2006 Delete repeated header lines. [Start]
LOCAL lcPrevOrd
lcPrevOrd = SPACE(6)
*B131721,1 WSH 04/02/2006 [End]

SELECT &lcHeader &&POSHDR DATA
LOCATE 
SET RELATION TO STYLE INTO STYLE
SET RELATION TO PO INTO &lcDetail
*-First,second and third are logic variables to indicate that the first,second and third
*-Contractor and mfg.operation are filled correctly
SCAN 
  WAIT WINDOW LANG_CSTSHTRP_MsgCollectMf + PO + " ...."  NOWAIT 
  
  *B131721,1 WSH 04/02/2006 Delete repeated header lines. [Start]
  IF PO == lcPrevOrd
    DELETE
    LOOP
  ENDIF
  lcPrevOrd = PO
  *B131721,1 WSH 04/02/2006 [End]
  
  IF !EMPTY(&lcHeader..Season)
    REPLACE &lcHeader..Season_Des WITH SUBSTR(gfCodDes(&lcHeader..Season,'SEASON'),1,17)
  ENDIF 
  IF !EMPTY(&lcHeader..cDivision)
    REPLACE &lcHeader..Div_Desc WITH SUBSTR(gfCodDes(&lcHeader..cDivision,'CDIVISION'),1,17)  
  ENDIF 
  IF SEEK(ALLTRIM(&lcHeader..Style),'STYLE')
    REPLACE &lcHeader..ScaleID   WITH STYLE.SCALE
    REPLACE &lcHeader..StyleDesc WITH LEFT(STYLE.DESC1,15)
    lcSizes = ""
    lcSizes = GetScale(STYLE.SCALE,SPACE(1))
    REPLACE &lcHeader..ScaleDesc WITH lcSizes
    XPRIM_FAB = ALLTRIM(PADR(STYLE.FABRIC,19))
    IF 'MA' $ oAriaApplication.CompanyInstalledModules .AND. !EMPTY(XPRIM_FAB)
      lcVendor = loOGScroll.gfTempName()
      lcVendorStatement = " SELECT VENDOR As VendorID FROM ITEM (INDEX = CSTYLE) WHERE CSTYMAJOR = '" + XPRIM_FAB + "'"
      lnVendorResult = loOGScroll.oRDA.SqlRun (lcVendorStatement,lcVendor,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
      IF lnVendorResult > 0
        REPLACE &lcHeader..PrimVend WITH VendorID
      ENDIF 
    ENDIF 
    SELECT &lcHeader
    DO CASE
      CASE XPRICE_LVL = 'B'
        XSELLPRICE = STYLE->PRICEB
        REPLACE &lcHeader..SellPrice WITH XSELLPRICE
      CASE XPRICE_LVL = 'C'    
        XSELLPRICE = STYLE->PRICEC
        REPLACE &lcHeader..SellPrice WITH XSELLPRICE
      OTHERWISE
        XSELLPRICE = STYLE->PRICEA
        REPLACE &lcHeader..SellPrice WITH XSELLPRICE
    ENDCASE
  ENDIF
  lcPieces = 0
  xAct_Qty = 0
  DO CASE
    CASE &lcHeader..Status='O'
      xAct_Qty = &lcHeader..nStyOrder - (&lcHeader..Cancel + &lcHeader..Receive + &lcHeader..Damage )
      lcPieces = &lcHeader..nStyOrder - &lcHeader..Cancel
    CASE &lcHeader..Status='C'
      xAct_Qty = &lcHeader..Receive
      lcPieces = &lcHeader..Receive
    CASE &lcHeader..Status='S'
      xAct_Qty = &lcHeader..Receive
      lcPieces = &lcHeader..Receive
    CASE &lcHeader..Status='A'
      xAct_Qty = &lcHeader..Pcs_Act
      lcPieces = &lcHeader..Pcs_Act
    CASE &lcHeader..Status='H'
      xAct_Qty = &lcHeader..Open
      lcPieces = &lcHeader..Open
  ENDCASE     
  REPLACE &lcHeader..ActualQty WITH xAct_Qty
  REPLACE &lcHeader..Pieces    WITH lcPieces

  lcOrder = " "
  IF &lcHeader..CPTranCd = '1'
    lcOrder = &lcHeader..Order
  ENDIF 
  IF !("NULL" $ lcOrder) .AND. !EMPTY(lcOrder).AND. 'SO' $ oAriaApplication.CompanyInstalledModules
    IF SEEK('O'+lcOrder,'ORDHDR')
      REPLACE &lcHeader..Account WITH ORDHDR.ACCOUNT
    ENDIF 
  ELSE 
      REPLACE &lcHeader..Account WITH LANG_CSTSHTRP_ToStock
  ENDIF 
  XCUTTKT = &lcHeader..Po
  ******* CUTTKT BOM FOUND SO PRINT IT FOR THIS CUTTING TICKET *******
  SELECT &lcDetail
  SET ORDER TO 
  SET ORDER TO TAG &lcDetail
  XPRV_TYP   = SPACE(1)
  OldTyp     = SPACE(1)
  lcOldCtg   = cCatgtyp
  *B608609,1 WAM 07/09/2008 Fix bug in collecting data
  *DO WHILE .T.
  *B608609,1 WAM 07/09/2008 (End)
    SEEK XCUTTKT
    SCAN REST WHILE (CutTkt = XCUTTKT)
      IF IIF(lcOldCtg='M',OldTyp <> TYP,XPRV_TYP <> cCatgTyp)
        XPRV_TYP  = cCatgTyp         &&TYP
        lcOldCtg  = cCatgTyp
        lcNoToPrn = TYP
        OldTyp    = TYP
      ENDIF
      IF lcCostM  = 'A'
        STORE 0 TO lnQty,lnUntCst,lnFound
        *B607925,1 WAM 01/10/2007 Get quantity issues and actual cost for each cost element
        *IF SEEK(&lcHeader..Po,'&lcBomCostData')
        IF SEEK(&lcHeader..Po+&lcDetail..Typ+&lcDetail..ITEM+&lcDetail..MFGCODE+&lcDetail..DyeLot,'&lcBomCostData')
        *B607925,1 WAM 01/10/2007 (End)
          lnUntCst = &lcBomCostData..UnitActCst
          lnQty    = &lcBomCostData..TotQty
          lnFound  = lnFound + 1
        ENDIF            
        IF lnFound = 0
          lnUntCst  = &lcDetail..UntCost
        ELSE
          lnUntCst = IIF(lnQty <> 0 , lnUntCst / lnQty,0)
        ENDIF
        SELECT &lcDetail
      ELSE
        lnUntCst  = &lcDetail..UntCost
        lnQty     = &lcDetail..Req_Qty
      ENDIF
      xExt_Cst = ROUND(lnUntCst*lnQty,2)
      *--intialise variables :-
      *--lnBudjet-----> budjet qty  when TranCd = '1'
      *--lnReciev-----> Recived qty when TranCd = '2' 
      *--lnDamje -----> Damaje qty  when TranCd = '3'
      *--lnCancel-----> Cancel Qty  when TranCd = '4'
      STORE 0 TO lnBudjet,lnReciev,lnDamje,lnCancel
      SELECT &BOMLINE
      IF SEEK(&lcDetail..CutTkt)
        IF XPRV_TYP = 'M'
          lcExpr = [REST WHILE cTktNo = &lcDetail..CutTkt FOR mfgCode = &lcDetail..mfgCode .AND. cBomTyp = &lcDetail..Typ]
        ELSE
          lcExpr = [REST WHILE cTktNo = &lcDetail..CutTkt FOR ITEM = &lcDetail..ITEM .AND. ]+;
                   [ cCatgTyp = &lcDetail..cCatgTyp .AND. cBomTyp = &lcDetail..Typ]
        ENDIF
        SCAN &lcExpr
          SELECT &PosLn
          lcCtkt = &lcDetail..CutTkt
          lcStyle = &BOMLINE..STYLE
          =SEEK(lcCtkt+lcStyle)
          SCAN REST WHILE &PosLn..PO + &PosLn..Style = &lcDetail..CutTkt + &BomLine..STYLE
            lnBudjet = lnBudjet + IIF(TranCd = '1',TotQty,0)
            lnReciev = lnReciev + IIF(TranCd = '2',TotQty,0)
            lnDamje  = lnDamje  + IIF(TranCd = '3',TotQty,0)
            lnCancel = lnCancel + IIF(TranCd = '4',TotQty,0)
          ENDSCAN
        ENDSCAN
      ENDIF
      *--determin the devided qty depend on the cuttkt status
      lnPieces=0
      DO CASE
        CASE &lcHeader..Status ='O'    && the cuttkt status is 'OPEN'
          lnPieces = lnBudjet - ( lnCancel + lnReciev + lnDamje)
        CASE &lcHeader..Status $ 'CS'  && the cuttkt status is 'Complete','CLOSE','ACTUALISE'
          lnPieces = lnReciev
        CASE &lcHeader..Status = 'A'   && the cuttkt status is 'ON HOLD'
          lnPieces = lnBudjet 
      ENDCASE          
      lnPieces = IIF(lcCostM = 'A',lnPieces,lnBudjet)
      xAvg_Pcs = IIF(lnPieces>0,ROUND(xExt_Cst/lnPieces,2),0)
      xAvg_Yld = IIF(lnPieces<>0,lnQty/lnPieces,0)
      SELECT &lcDetail
      lcItem = ITEM
      IF !EMPTY(lcItem)
        lcItemClr = SUBSTR(lcItem,lnMajorLen+2,19-lnMajorLen)
        REPLACE &lcDetail..ItemColor WITH lcItemClr
      ENDIF 
      REPLACE &lcDetail..UnitCost  WITH lnUntCst
      REPLACE &lcDetail..Yardge    WITH lnQty
      REPLACE &lcDetail..Extd_Cost WITH XEXT_CST
      REPLACE &lcDetail..Avg_Yield WITH ABS(XAVG_YLD)
      REPLACE &lcDetail..Avg_Piece WITH ABS(XAVG_PCS)
    ENDSCAN 
    *B608609,1 WAM 07/09/2008 Fix bug in collecting data
  *  IF EOF() .OR. &lcCtktData..CutTkt <> XCUTTKT
  *    EXIT
  *  ENDIF
  *ENDDO
  *B608609,1 WAM 07/09/2008 (End)
ENDSCAN 
*!
****************************************************************************
*! Name      : lfCalculate()
*! Developer : Heba Fathi 
*! Date      : 10/20/2004
*! Purpose   : Function calculates Req_Qty, Used_Qty, Est.Extd.Cost, Act.Extd.Cost
****************************************************************************
*! 
FUNCTION lfCalculate
lnAlias = SELECT(0)

BOMLINE = loOGScroll.gfTempName()
lcSql1 = "SELECT BOMLINE.CTKTNO,BOMLINE.ITEMQTY,BOMLINE.UNITCOST,BOMLINE.cCatGTyp,BOMLINE.ITEM,BOMLINE.MFGCODE,BOMLINE.cBomTyp FROM BOMLINE (INDEX = BOMLINE) "
lcSql1  = lcSql1 + "INNER JOIN " + lcSelPO + " TmpPO ON TmpPO.PO = BOMLINE.CTKTNO WHERE "
lcSql1  = lcSql1 + "BOMLINE.CIMTYP = 'I' AND BOMLINE.CTYPE = '1' AND BOMLINE.CINVTYPE = 0001 "
lnResult1 = loOGScroll.oRDA.SqlRun (lcSql1,BOMLINE,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
IF lnResult1 > 0
  lnBuffering = CURSORGETPROP("Buffering","&BomLine")
  =CURSORSETPROP("Buffering",3,"&BomLine")
  SELECT &BomLine
  INDEX ON cTKTNO TAG BomLine
ENDIF 
BomCost = loOGScroll.gfTempName()
lcSql = " SELECT BomCost.nTotQty,BomCost.nTotCst,BomCost.cBomType,BomCost.cTktNo,BomCost.Item,BomCost.MfgCode FROM BOMCOST "
lcSql  = lcSql + "(INDEX = BOMCSTKT) INNER JOIN " + lcSelPO + " TmpPO ON TmpPO.PO= "
lcSql  = lcSql + "BOMCOST.cTktNo WHERE BOMCOST.cIMtyp = 'I' "
lnResult = loOGScroll.oRDA.SqlRun (lcSql,BomCost,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
IF lnResult > 0  
  lnBuffering = CURSORGETPROP("Buffering","&BomCost")
  =CURSORSETPROP("Buffering",3,"&BomCost")
  SELECT &BomCost
  INDEX ON CBOMTYPE+CTKTNO+ITEM+MFGCODE TAG BomCost
ENDIF 
SELECT &lcDetail
SET ORDER TO 
SET ORDER TO TAG &lcDetail
SCAN 
  lcCPo = Cuttkt
  STORE 0 TO lnReqQty,lnUsedQty,lnActExCst,lnEstExCst
  SELECT &BOMLINE
  SEEK lcCPo
  lcForKey=IIF(EMPTY(&lcDetail..Item),&lcDetail..MfgCode,&lcDetail..Item)
  SUM (ItemQty),(ItemQty*UnitCost) REST ;
           WHILE CTKTNO = lcCPo ;
           FOR  IIF(!(cCatGTyp$'MDP'),ITEM,MFGCODE) = lcForKey .AND. cBomTyp=&lcDetail..Typ ;
           TO   lnReqQty,lnEstExCst
  STORE '/' TO lcPExSign,lcDExSign,lcPUntSin,lcDUntSin
  IF SEEK(lcCPo,'&lcPoData')
    lcPExSign = gfGetExSin(@lcPUntSin,&lcPoData..cPriceCur)
    lcDExSign = gfGetExSin(@lcDUntSin,&lcPoData..cDutyCur)
    DO CASE
      CASE &lcDetail..cCatgTyp='P'
        IF lcPUntSin = "/"
          lcCurUnit = IIF(&lcPoData..nCurrUnit = 0,1,&lcPoData..nCurrUnit)  
        ELSE 
          lcCurUnit = &lcPoData..nCurrUnit
        ENDIF 
        IF lcPExSign = "/"
          lcRate = IIF(&lcPoData..nPriceRat = 0,1,&lcPoData..nPriceRat)  
        ELSE 
          lcRate = &lcPoData..nPriceRat
        ENDIF 
        lnEstExCst = lnEstExCst &lcPExSign lcRate &lcPUntSin lcCurUnit
      CASE !INLIST(&lcDetail..cCatgTyp,'S','F','T')
        IF lcDUntSin = "/"
          lcCurUnit = IIF(&lcPoData..nDCurUnit = 0,1,&lcPoData..nDCurUnit)
        ELSE 
          lcCurUnit = &lcPoData..nDCurUnit
        ENDIF 
        IF lcDExSign = "/"
          lcRate = IIF(&lcPoData..nDutyRat = 0,1,&lcPoData..nDutyRat)  
        ELSE 
          lcRate = &lcPoData..nDutyRat
        ENDIF 
        lnEstExCst = lnEstExCst &lcDExSign lcRate &lcDUntSin lcCurUnit
    ENDCASE
  ENDIF

  SELECT &lcDetail
  REPLACE &lcDetail..RequireQty  WITH lnReqQty
  REPLACE &lcDetail..EstExCost   WITH lnEstExCst
  
  SELECT &BOMCOST
  =SEEK(&lcDetail..Typ+lcCPo+&lcDetail..Item+&lcDetail..MfgCode)
  SUM nTotQty,nTotCst REST WHILE cBomType+cTktNo+Item+MfgCode = ;
                      &lcDetail..Typ+lcCPo+&lcDetail..Item+&lcDetail..MfgCode ;
                      TO lnUsedQty,lnActExCst 

  SELECT &lcDetail
  REPLACE &lcDetail..Used_Qty   WITH lnUsedQty
  REPLACE &lcDetail..Act_ExCost WITH lnActExCst
ENDSCAN 
SELECT (lnAlias)
RETURN
*!
****************************************************************************
*! Name      : lfCheckVendor()
*! Developer : Heba Fathi 
*! Date      : 10/20/2004
*! Purpose   : Function check contractor validity
****************************************************************************
*! 
FUNCTION lfCheckVendor
SELECT &lcHeader
SCAN 
  llVend  = .F.
  llFirst = .F.
  llSecond = .F.
  llThird = .F.
  *-check that there are no records to display
  SELECT &lcMfgData
  IF SEEK(&lcHeader..PO)
    SCAN REST WHILE CtktNo = (&lcHeader..PO)
      llVend  = IIF(!llVend .AND. !EMPTY(lcRpVend),lcRpVend = cContCode,llVend)
      IF llFirst = .F.
        REPLACE &lcHeader..Contr1 WITH &lcMfgData..cContCode
        IF SEEK(&lcHeader..Contr1,'APVENDOR')
          REPLACE &lcHeader..VEND_NAME1 WITH APVENDOR.cVenComp
        ENDIF 
        lcMfgOper1 = &lcMfgData..COprCode
        IF SEEK(CtktNo + lcMfgOper1,'&lcDetail')
          REPLACE &lcHeader..Rate1 WITH &lcDetail..UntCost
        ENDIF 
        REPLACE &lcHeader..Mfg_Opr1 WITH lcMfgOper1
        REPLACE &lcHeader..MFG_DESC1 WITH gfCodDes(&lcHeader..Mfg_Opr1,'MFGCODE')
        llFirst = .T.
      ELSE 
        IF llSecond = .F.
          REPLACE &lcHeader..Contr2 WITH &lcMfgData..cContCode
          IF SEEK(&lcHeader..Contr2,'APVENDOR')
            REPLACE &lcHeader..VEND_NAME2 WITH APVENDOR.cVenComp
          ENDIF 
          lcMfgOper2 = &lcMfgData..COprCode
          IF SEEK(CtktNo + lcMfgOper2,'&lcDetail')
            REPLACE &lcHeader..Rate2 WITH &lcDetail..UntCost
          ENDIF 
          REPLACE &lcHeader..Mfg_Opr2 WITH &lcMfgData..COprCode
          REPLACE &lcHeader..MFG_DESC2 WITH gfCodDes(&lcHeader..Mfg_Opr2,'MFGCODE')
          llSecond = .T.
        ELSE 
          IF llThird = .F.
            REPLACE &lcHeader..Contr3 WITH &lcMfgData..cContCode
            IF SEEK(&lcHeader..Contr3,'APVENDOR')
              REPLACE &lcHeader..VEND_NAME3 WITH APVENDOR.cVenComp
            ENDIF 
            lcMfgOper3 = &lcMfgData..COprCode
            IF SEEK(CtktNo + lcMfgOper3,'&lcDetail')
              REPLACE &lcHeader..Rate3 WITH &lcDetail..UntCost
            ENDIF 

            REPLACE &lcHeader..Mfg_Opr3 WITH &lcMfgData..COprCode
            REPLACE &lcHeader..MFG_DESC3 WITH gfCodDes(&lcHeader..Mfg_Opr3,'MFGCODE')
            llThird = .T.
          ENDIF 
        ENDIF 
      ENDIF   
    ENDSCAN 
  REPLACE &lcHeader..llVendor WITH llVend
  ENDIF 
ENDSCAN 
IF !EMPTY(lcRpVend)
  SET FILTER TO llVendor
ENDIF 
*!
****************************************************************************
*! Name      : GetScale()
*! Developer : Heba Fathi (HFK)
*! Date      : 10/20/2004
*! Purpose   : Function that will return a string with the size scale
****************************************************************************
*! Parameters: 1 =>  Size scale to be displayed
*!             2 =>  Spacing between each size
****************************************************************************
PROCEDURE GetScale
PARAMETERS XSC,XSPACE
PRIVATE XOLDALIAS,X,XSTRING,XSC,XSPACE,Z
XOLDALIAS= ALIAS()
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
SELE SCALE
SEEK 'S'+XSC
X       = 1
XSTRING = ''
DO WHILE FOUND() .AND. X<=CNT
  Z = STR(X,1)
  XSTRING = XSTRING + SZ&Z + IIF(X=CNT,'',XSPACE)
  X= X + 1
ENDDO
IF .NOT. FOUND()
  XSTRING ='* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(XOLDALIAS)) > 0
  SELE &XOLDALIAS
ENDIF

RETURN(XSTRING)
*!
****************************************************************************
*! Name      : lfvCost()
*! Developer : Heba Fathi (HFK)
*! Date      : 10/20/2004
*! Purpose   : Function that change the header of PO#
****************************************************************************
*! Parameters: NONE
****************************************************************************
FUNCTION lfvCost

lnPos = ASCAN(loOGScroll.laogFxFlt,'POSHDR.PO')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxFlt,lnPos,1)
 lcPos = loOGScroll.laogFxFlt[lnPos,6]
 IF !EMPTY(lcPos)
   SELECT(lcPos)
   ZAP 
 ENDIF 
ENDIF  

ClearRead()
llClrPO = .T.
lcStatusStr = ""
DO CASE
  CASE lcCSType = 'I'
    lcPoHdr   = 'InterLocation PO#'
  	lcVnHdr   = 'Source Location' 
    lcKeyExpr = 'NN'
  	lcVendFld  = "cWareCode :R :H= 'Location' , cDesc :R :H= 'Description'"
   	lcFile = 'WAREHOUS'
  	lcSelFld  = 'cWareCode'
  OTHERWISE
    lcPoHdr   = 'Purchase Order #'
    lcVnHdr   = 'Vendor'        
    lcKeyExpr = 'PP'
  	lcVendFld  = "cVendCode :R :H='Vendor':20, cVenComp  :R :H='Name':45,cPhoneNo  :R :P= gfPhoneTem() :H='Phone':28,         cVenOurAc :R :H='Our Account':28"
  	lcFile = 'APVendor'
  	lcSelFld  = 'CVENDCODE'
ENDCASE
*!
*!**************************************************************************
*! Name      : lfvWareHos
*: Developer : Heba Fathi (HFK)
*! Date      : 11/23/2004
*! Purpose   : Validate Warehouse Code .
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
*!
*!**************************************************************************
*! Name      : lfwOldWare
*: Developer : Heba Fathi (HFK)
*! Date      : 09/25/2004
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*!
FUNCTION lfwOldWare
lcOldWare = EVALUATE(OGSYS18(.T.))
*-- End of lfvWareHos.
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
LOCAL lcStatusStr
  lcStatusStr = ""
  RETURN lcStatusStr
ENDFUNC 
*! -B038984,1 HFK 01/27/2005 [Start]
*!************************************************************
*! Name      : lfSetTit
*: Developer : Heba Fathi (HFK)
*: Date      : 01/27/2005
*! Purpose   : defining the cut ticket browse title
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
FUNCTION lfSetTit
PARAMETERS lcparameters
IF lcparameters = 'R'
  lcTitle = lfGetTit()
ENDIF 
*! -B038984,1 HFK 01/27/2005 [End]