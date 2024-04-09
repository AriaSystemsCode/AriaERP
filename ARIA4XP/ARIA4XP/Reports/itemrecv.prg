*:***************************************************************************
*: Program File  : POSTYREC(N03721)
*: Program Desc. : Item Order Receipts Log
*: System        : Aria 4 XP.
*: Module Calls  : 1) Style PO Receipt Log (PO)
*:                 2) Material PO Receipt Log (MA)
*:                 3) Material Manufacturing Order Receipt (MA)
*:                 4) Dye Order Receipt (MF)
*:                 5) Cutting Ticket Receipt (MF)
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Date          : 10/10/2004
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Modifications:
*: B127309,1 MMT,04/11/2005 Fix bug of wrong landed cost printing
*: B128055,1 WSH 05/20/2005 Handle special chacaters in Select Statement for Vendors.
*: B129242,1 WSH 10/03/2005 Fix PO browse problems.
*: B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen T20070404.0012
*: B608206,1 SSH 08/05/2007 add subtotal by colour in the crystal.
*: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[T20071001.0016]
*: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [T20080603.0010]
*: B609163,1 MMT 03/09/2010 Fix error while printing receipt log report from rec. screen  [T20100219.0007]
*: B609809,1 MMT 01/30/2012 PO Receipt Log report exports wrong report format to PDF[T20120111.0002]
*: B610515,1 HIA 09/15/13 T20130906.0007 - company designation in subject on email version the Style Purchase Order Receipt Log.
*: B610810,1 MMT 08/19/2014 PO receipt log report filters by style group incorrectly[T20140815.0002]
*:***************************************************************************
*B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[Start]
*LPARAMETERS lcTmpPosLn, lcRecShipNo, llPrint
LPARAMETERS lcTmpPosLn, lcRecShipNo, llPrint,ldReDate
*B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[End]


#INCLUDE R:\ARIA4XP\REPORTS\ITEMRECV.H

PRIVATE llCalledFromRec  && If Report Called from Receiving Program
LOCAL lcSQLStmt    && Holds the SQL statement
LOCAL lcFields     && Holds the selection fields to get from SQL
LOCAL lcWhereCon   && Holds the SQL where condition
LOCAL lcJoin       && Holds the SQL join expression
LOCAL lnResult     && Holds the result of the remote data access
LOCAL lcCon        && Used to hold Inlist values returned from Option Grid
LOCAL lnRetCurs    && Return result for Build SQl Cursor for Filters

*--Temp SQL Cursors used to Join with POSLN file to filter results
LOCAL lcSQLStyle, lcSQLFabric, lcSQLPO, lcSQLVendor, lcSQLLocation, lcSQLSLoc, lcSQLDefLoc, lcSQLComp, lcSQLContr

LOCAL lnDataSess, lcDevice
lnDataSess = SET("Datasession")
lcDevice   = oAriaApplication.gcDevice

*-- Check if passed Cursor name exist or not
llCalledFromRec = .F.
IF TYPE('lcTmpPosLn') = 'C' AND !EMPTY(lcTmpPosLn)
  IF !FILE(oAriaApplication.WorkDir + lcTmpPosLn + '.DBF')
    *--There is no record to display
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF
  llCalledFromRec = .T.
  oAriaApplication.gcDevice = IIF(llPrint, 'PRINTER', 'SCREEN')
ENDIF

IF llCalledFromRec OR loOgScroll.llOGFltCh

  *--Adjust lcRPPoType variable to use in filtering POSLN.cStyType "Order Types":
  *--                        'R' - Return PO       --> 'P' with Style, and 'M' with Fabric
  *--                        'P' - Purchase Order  --> As it is
  *--                        'N' - Inter Location  --> As it is
  *--                        'A' - All             --> Will be Adjusted After Building SQL Cursors
  lcRPPoType = IIF(lcRPPoType = 'R', IIF(lcInvType = '0001', 'P', 'M'), lcRPPoType)

  *********************************************************************
  *--Statement selection fields
  *********************************************************************
  *
  *: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [Start]
  *lcFields = "POSLN.PO,POSLN.cSTYTYPE,POSLN.SHIPNO,POSHDR.Complete,POSHDR.Entered,POSHDR.LINK_CODE,POSLN.[LineNo],"+;
  "posln.[Date],POSLN.trancd,POSLN.Scale,POSLN.Style,POSLN.Reference,POSHDR.Season,POSHDR.cDivision,"+;
  "POSLN.Vendor,POSHDR.cPriceCur,POSHDR.STATUS,POSLN.cWareCode,POSLN.DYELOT,POSLN.nflanCost1,POSLN.cInvType,"+;
  "POSLN.Qty1,POSLN.Qty2,POSLN.Qty3,POSLN.Qty4,POSLN.Qty5,POSLN.Qty6,POSLN.Qty7,POSLN.Qty8,POSLN.TotQty,POSLN.cBusDocu,POSLN.Width AS STYWIDTH,"+;
  "POSLN.niCost1,POSLN.niCost2,POSLN.niCost3,POSLN.niCost4,POSLN.niCost5,POSLN.niCost6,POSLN.niCost7,POSLN.Pattern,"+;
  "POSLN.nlan_cost1,POSLN.nlan_cost2,POSLN.nlan_cost3,POSLN.nlan_cost4,POSLN.nlan_cost5,POSLN.nlan_cost6,POSLN.nlan_cost7,"+;
  "POSLN.nact_cost1,POSLN.nact_cost2,POSLN.nact_cost3,POSLN.nact_cost4,POSLN.nact_cost5,POSLN.nact_cost6,POSLN.nact_cost7,"+;
  "POSLN.cUOMCODE,UOM.cUOM_B AS UOMBUY,UOM.cUOM_V AS UOMUSE,UOM.nConf AS nConv,'" + SPACE(35) + "' AS cCurDesc,'" + SPACE(35) + "' AS cWareDesc" + ;
  IIF(lcInvType = '0002', ",ITEM.[Desc] AS STYDESC", ",'" + SPACE(20) + "' AS STYDESC") +;
  IIF(lcInvType = '0002',  ",Item.nSugRetPri", ",000000000.00") + " AS nSugRetPri" +;
  ",'" + SPACE(30) + "' AS VENDESC, '" + SPACE(30) + "' AS DIVDESC, '" + SPACE(20) + "' AS SEASDESC"+;
  ",'" + SPACE(30) + "' AS cWipAcct,'" + SPACE(10) + "' AS Cont1, '" + SPACE(10) + "' AS Cont2, " +;
  "ShpmtHdr.airwayb, ShpmtHdr.Entered AS ShipEnt, ShpmtHdr.Cartons, ShpmtHdr.Eta "

  lcFields = "POSLN.PO,POSLN.cSTYTYPE,POSLN.SHIPNO,POSHDR.Complete,POSHDR.Entered,POSHDR.LINK_CODE,POSLN.[LineNo],"+;
    "posln.[Date],POSLN.trancd,POSLN.Scale,POSLN.Style,POSLN.Reference,POSHDR.Season,POSHDR.cDivision,"+;
    "POSLN.Vendor,POSHDR.cPriceCur,POSHDR.STATUS,POSLN.cWareCode,POSLN.DYELOT,POSLN.nflanCost1,POSLN.cInvType,"+;
    "POSLN.Qty1,POSLN.Qty2,POSLN.Qty3,POSLN.Qty4,POSLN.Qty5,POSLN.Qty6,POSLN.Qty7,POSLN.Qty8,POSLN.TotQty,POSLN.cBusDocu,POSLN.Width AS STYWIDTH,"+;
    "POSLN.niCost1,POSLN.niCost2,POSLN.niCost3,POSLN.niCost4,POSLN.niCost5,POSLN.niCost6,POSLN.niCost7,POSLN.Pattern,"+;
    "POSLN.nlan_cost1,POSLN.nlan_cost2,POSLN.nlan_cost3,POSLN.nlan_cost4,POSLN.nlan_cost5,POSLN.nlan_cost6,POSLN.nlan_cost7,"+;
    "POSLN.nact_cost1,POSLN.nact_cost2,POSLN.nact_cost3,POSLN.nact_cost4,POSLN.nact_cost5,POSLN.nact_cost6,POSLN.nact_cost7,"+;
    "POSLN.cUOMCODE,UOM.cUOM_B AS UOMBUY,UOM.cUOM_V AS UOMUSE,UOM.nConf AS nConv,'" + SPACE(35) + "' AS cCurDesc,'" + SPACE(35) + "' AS cWareDesc" + ;
    IIF(lcInvType = '0002', ",ITEM.[Desc] AS STYDESC", ",'" + SPACE(20) + "' AS STYDESC") +;
    IIF(lcInvType = '0002',  ",Item.nSugRetPri", ",000000000.00") + " AS nSugRetPri" +;
    ",'" + SPACE(30) + "' AS VENDESC, '" + SPACE(30) + "' AS DIVDESC, '" + SPACE(20) + "' AS SEASDESC"+;
    ",'" + SPACE(30) + "' AS cWipAcct,'" + SPACE(10) + "' AS Cont1, '" + SPACE(10) + "' AS Cont2, " +;
    "ShpmtHdr.airwayb, ShpmtHdr.Entered AS ShipEnt, ShpmtHdr.Cartons, ShpmtHdr.Eta "+;
    ", POSHDR.nAct_Cost1 As POActCst1"+;
    ", POSHDR.nAct_Cost2 As POActCst2, POSHDR.nAct_Cost3 As POActCst3"+;
    ", POSHDR.nAct_Cost4 As POActCst4, POSHDR.nAct_Cost5 As POActCst5"+;
    ", POSHDR.nAct_Cost6 As POActCst6, POSHDR.nAct_Cost7 As POActCst7"

  *: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [End]
  *--Create Statement Inner Join lines
  lcJoin = " POSLN (INDEX=POSLN) INNER JOIN POSHDR (INDEX = POSHDR) "+;
    "   ON POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND "+;
    "      POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND "+;
    "      POSLN.PO = POSHDR.PO " +;
    " LEFT OUTER JOIN SHPMTHDR (INDEX = SHPMTHDR) " +;
    "   ON POSLN.cBusDocu = SHPMTHDR.cBusDocu AND " +;
    "      POSLN.cStyType = SHPMTHDR.cShpType AND " +;
    "      POSLN.SHIPNO = SHPMTHDR.SHIPNO "

  *--Condition for Order Type and Business Document Type
  IF lcRPPoType = 'A' && ALL Order Types Selected "In case of Style or Material POs only"
    lcWhereCon = "POSLN.CBUSDOCU IN ('P', 'R', 'N') AND "
    IF lcInvType = '0001'  && Only Style Order Types
      lcWhereCon = lcWhereCon + "POSLN.CSTYTYPE IN ('P', 'N') AND "
    ELSE  && Only Material Order Types
      lcWhereCon = lcWhereCon + "POSLN.CSTYTYPE IN ('M', 'L') AND "
    ENDIF
  ELSE  && Cuttkts, MFGs, Dye Ords
    lcWhereCon = "POSLN.CBUSDOCU = '" + lcBusDocu + "' AND POSLN.CSTYTYPE = '" + lcRPPoType + "' AND "
  ENDIF

  *--Condition for Item Inventory Type
  lcWhereCon = lcWhereCon + "POSLN.cInvType = '"  + lcInvType + "' AND " +;
    "POSLN.TranCD = '2'"

  *----------------------------------------------------------*
  *------------ If Called from Receiving Program ------------*
  *----------------------------------------------------------*
  IF llCalledFromRec
    PRIVATE loOgScroll, lcShipNo
    lcShipNo = lcRecShipNo

    *-- Create a Dummy Object of the Option Grid...
    *-- Create a Dummy Object of the Option Grid...
    LOCAL lcClassDir, oOptionGrid, lUsePDF
    lcClassDir  = ADDBS(oAriaApplication.ClassDir)
    oOptionGrid = NEWOBJECT("optiongrid",lcClassDir+"optiongrid.vcx")
    *oOptionGrid = CREATEOBJECT("OptionGrid")
    loOgScroll  = oOptionGrid.OptionGrid.oHost
    lUsePDF     = gfGetMemVar('LLUSEPDF', oAriaApplication.ActiveCompanyID)

    loOgScroll.lcOGLastForm  = IIF(lcRpSortBy = 'T', 'ITMRVS', 'ITMRVB')
    loOgScroll.llCrystal     = .T.
    loOgScroll.lcOGWinTitl   = lcRpTitle
    loOgScroll.lUsePDFViewer = IIF(TYPE('lUsePDF') = 'L', lUsePDF, .T.)

    SET DATASESSION TO loOgScroll.PARENT.DATASESSIONID

    *-- Create needed variables to run the Report outside Option Grid
    lcSQLPOS   = loOgScroll.gfTempName()
    lcRPHdrTmp = loOgScroll.gfTempName()
    lcRpSclTmp = loOgScroll.gfTempName()
    =lfFillVars()

    IF lcInvType = '0001' AND !USED('STYLE')
      *-- For now 'In Phase 1', it is faster than opening the Style file using RDAC
      USE (oAriaApplication.DataDir+'Style') ORDER STYLE IN 0
    ENDIF

    *--Join with Item file to get Fabric information
    IF lcInvType = '0002'
      lcJoin = lcJoin + " INNER JOIN ITEM ON POSLN.cInvType = Item.cInvType AND POSLN.Style = Item.Style "
    ENDIF

    *--Join with UOM file to get UOM information
    lcJoin = lcJoin + " LEFT OUTER JOIN UOM (INDEX = UOMCODE) ON POSLN.cUOMCode = UOM.cUOMCode "

    lcWhereCon = '(1 = 2)' && Get Structure only

    *----------------------------------------------------------*
    *--------------- If Called from Option Grid ---------------*
    *----------------------------------------------------------*
  ELSE
    *********************************************************************
    *--Create SQl Temp Cursors to JOIN with POSLN to create SQL Statement
    *********************************************************************
    *
    *--Selected Orders JOIN

    *B129242,1 WSH 10/03/2005 Use the complete key for the POSHDR file. [Start]
    *lnRetCurs = lfCreateSQLCursor(1, IIF(lcRPPoType = 'D', 'POSHDR.PO2', 'POSHDR.PO'), @lcSQLPO, 'PO C(6)', 'PO')
    lnRetCurs = lfCreateSQLCursor(1, IIF(lcRPPoType = 'D', 'POSHDR.PO2', 'POSHDR.PO'), @lcSQLPO, 'PO C(8)', 'KEYEXP')
    *B129242,1 WSH 10/03/2005 [End]

    IF lnRetCurs <> -1 && No Error Happened

      *B129242,1 WSH 10/03/2005 Use the complete key for the POSHDR file. [Start]
      *lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLPO + " TmpPO ON TmpPO.PO = POSLN.PO ", "")
      lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLPO + " TmpPO ON SUBSTRING(TmpPO.PO,1,1) = POSLN.cBusDocu AND SUBSTRING(TmpPO.PO,2,1) = POSLN.cStyType AND SUBSTRING(TmpPO.PO,3,6) = POSLN.PO ", "")
      *B129242,1 WSH 10/03/2005 [End]

    ELSE
      RETURN .F.
    ENDIF

    IF lcInvType = '0001' && Order Types related to Styles only
      *--Check if there is a filter on Style or Style Groups
      lnRetCurs = lfBuildStyleCursor(@lcSQLStyle)
      IF lnRetCurs <> -1 && No Error Happened
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN "+ lcSQLStyle +" TmpSty ON TmpSty.Style = POSLN.Style ", "")
      ELSE
        RETURN .F.
      ENDIF
    ELSE  && Fabric Order Types only
      *--Join with Item file as we need Fabric Information from it
      lcJoin = lcJoin + " INNER JOIN Item ON POSLN.cInvType = Item.cInvType AND POSLN.Style = Item.Style "

      *--Check if there is a filter on Fabric
      lnRetCurs = lfCreateSQLCursor(1, 'ITEM.CSTYMAJOR', @lcSQLFabric, 'Fabric C(19)', 'cStyMajor')
      IF lnRetCurs <> -1
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLFabric + " TmpFab ON TmpFab.Fabric = Item.cStyMajor", "")
      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    *--Check if there is a filter on Location
    lnRetCurs = lfCreateSQLCursor(1, 'POSLN.CWARECODE', @lcSQLLocation, 'Location C(6)', 'cWareCode')
    IF lnRetCurs <> -1
      lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLLocation + " TmpLoc ON TmpLoc.Location = POSLN.cWareCode ", "")
    ELSE
      RETURN .F.
    ENDIF

    *--Check if there is a filter on Vendor
    IF lcRPPoType $ 'PDAM'  && If Order is Style PO, Material PO, or Dye Order
      lnRetCurs = lfCreateSQLCursor(1, 'POSHDR.VENDOR', @lcSQLVendor, 'Vendor C(8)', 'cVendCode')
      IF lnRetCurs <> -1
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLVendor + " TmpVend ON TmpVend.Vendor = POSHDR.Vendor ", "")
      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    *--Check if there is a filter on Source Location for InterLocation
    IF lcRPPoType = 'N'  && If Order is Style Inter Location PO
      lnRetCurs = lfCreateSQLCursor(1, 'POSLN.VENDOR', @lcSQLSLoc, 'SourceLoc C(8)', 'cWareCode')
      IF lnRetCurs <> -1
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLSLoc + " TmpSLoc ON TmpSLoc.SourceLoc = POSLN.Vendor ", "")
      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    IF lcRPPoType = 'F'  && If Order is Manufacturing Orders
      *--Check if there is a filter on Default Receiving Location
      lnRetCurs = lfCreateSQLCursor(1, 'POSHDR.CWARECODE', @lcSQLDefLoc, 'DefLoc C(6)', 'cWareCode')
      IF lnRetCurs <> -1
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN " + lcSQLDefLoc + " TmpDefLoc ON TmpDefLoc.DefLoc = POSHDR.cWareCode ", "")
      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    IF lcRPPoType = 'U'  && If Order is Cutting Ticket
      *--Check if there is Contractors Selected
      lnRetCurs = lfCreateSQLCursor(1, 'MFGOPRHD.CCONTCODE', @lcSQLContr, 'CCONTCODE C(8)', 'CVENDCODE')
      IF lnRetCurs <> -1
        lcJoin = lcJoin + IIF(lnRetCurs = 1, " INNER JOIN MFGOPRHD ON POSLN.PO = MFGOPRHD.cTktNo AND MFGOPRHD.cIMTyp = 'M' INNER JOIN " + lcSQLContr + " TmpContr ON TmpContr.cContCode = MFGOPRHD.cContCode", "")
      ELSE
        RETURN .F.
      ENDIF
    ENDIF

    *--Join with UOM file to get UOM information
    lcJoin = lcJoin + " LEFT OUTER JOIN UOM (INDEX = UOMCODE) ON POSLN.cUOMCode = UOM.cUOMCode "

    *********************************************************************
    *--Create SQl Statement Selection Condition Formula
    *********************************************************************
    *
    *--Condition for Item Non Majors, If it is free Segment
    IF lcFreeClr == 'F'
      *--Get Item Non Majors Filter
      lcCon = lfInListString(1, 'SUBSTR(' + IIF(lcInvType = '0002', 'ITEM', 'STYLE') + '.STYLE,lnNonMajPo,lnFreeLen)')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND SUBSTRING(POSLN.STYLE," + STR(lnNonMajPo) + "," + STR(lnFreeLen) + ") IN (" + lcCon + ")"
      ENDIF
    ENDIF

    *--Condition for Item Color, if Color Segment found.
    IF lcFreeClr == 'C'
      *--Get Item Color Filter
      lcCon = lfInListString(1, 'SUBSTR(' + IIF(lcInvType = '0002', 'ITEM', 'STYLE') + '.STYLE,lnClrPo,lnColorLen)')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND SUBSTRING(POSLN.STYLE," + STR(lnClrPo) + "," + STR(lnColorLen) + ") IN (" + lcCon + ")"
      ENDIF
    ENDIF

    *--Get Received Date Filter
    lcCon = lfCheckFilter(1, 'POSLN.[DATE]')
    IF !EMPTY(lcCon)
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[Start]
      lcCon = STRTRAN(lcCon, "|", "' AND '")
      *lcStartDate = ""
      *lcEndDate = ""
      *lnSepPos = ATC("|",lcCon)
      *IF lnSepPos > 0
      *  lcStartDate =  DTOS(CTOD(SUBSTR(lcCon,1,lnSepPos-1)))
      *  lcEndDate   =  DTOS(CTOD(SUBSTR(lcCon,lnSepPos+1)))
      *  lcCon = lcStartDate+"' AND  '"+ lcEndDate
      *ENDIF
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[End]
      lcWhereCon = lcWhereCon + " AND POSLN.[Date] BETWEEN '" + lcCon + "'"
    ENDIF

    IF lcRPPoType $ 'PDAM'  && For Style PO, Material PO, and Dye-Order Order Types only
      *--Get Price Currency Filter
      lcCon = lfInListString(1, 'POSHDR.CPRICECUR')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND POSHDR.CPRICECUR IN (" + lcCon + ")"
      ENDIF

      *--Get Duty Currency Filter
      lcCon = lfInListString(1, 'POSHDR.CDUTYCUR')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND POSHDR.cDutyCur IN (" + lcCon + ")"
      ENDIF
    ENDIF

    *--Get Status Filter
    lcCon = lfInListString(1, 'POSHDR.STATUS')
    IF !EMPTY(lcCon)
      lcWhereCon = lcWhereCon + " AND POSHDR.STATUS IN (" + lcCon + ")"
    ENDIF

    IF lcRPPoType # 'F' && For Manufaturing Order Types only
      *--Get Division Filter
      lcCon = lfInListString(1, 'POSHDR.CDIVISION')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND POSHDR.CDIVISION IN (" + lcCon + ")"
      ENDIF
    ENDIF

    IF lcRPPoType = 'U' && For Cutting Tickets Order Types only
      *--Get Season Filter
      lcCon = lfInListString(1, 'POSHDR.SEASON')
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND POSHDR.SEASON IN (" + lcCon + ")"
      ENDIF
    ENDIF

    *--Get Entered Filter
    lcCon = lfCheckFilter(1, 'POSHDR.ENTERED')
    IF !EMPTY(lcCon)
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[Start]
      lcCon = STRTRAN(lcCon, "|", "' AND '")
      *lcStartDate = ""
      *lcEndDate = ""
      *lnSepPos = ATC("|",lcCon)
      *IF lnSepPos > 0
      *  lcStartDate =  DTOS(CTOD(SUBSTR(lcCon,1,lnSepPos-1)))
      *  lcEndDate   =  DTOS(CTOD(SUBSTR(lcCon,lnSepPos+1)))
      *  lcCon = lcStartDate+"' AND  '"+ lcEndDate
      *ENDIF
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[End]
      lcWhereCon = lcWhereCon + " AND POSHDR.ENTERED BETWEEN '" + lcCon + "'"
    ENDIF


    *--Get Completed Filter
    lcCon = lfCheckFilter(1, 'POSHDR.COMPLETE')
    IF !EMPTY(lcCon)
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[Start]
      lcCon = STRTRAN(lcCon,"|","' AND '")
      *lcStartDate = ""
      *lcEndDate = ""
      *lnSepPos = ATC("|",lcCon)
      *IF lnSepPos > 0
      *  lcStartDate =  DTOS(CTOD(SUBSTR(lcCon,1,lnSepPos-1)))
      *  lcEndDate   =  DTOS(CTOD(SUBSTR(lcCon,lnSepPos+1)))
      *  lcCon = lcStartDate+"' AND  '"+ lcEndDate
      *ENDIF
      *: B608358,1 MMT 11/21/2007 Fix bug of Error when Company is UK and User Select Date Range[End]
      lcWhereCon = lcWhereCon + " AND POSHDR.COMPLETE BETWEEN '" + lcCon + "'"
    ENDIF

    IF lcInvType = '0002' && Material Order types
      *--Get Item Group Filter
      *B99999,1 MMT 02/16/2005 Fix bug of error when select item group[Start]
      *lcCon = lfCheckFilter(1, 'ITEM.CSTYGROUP')
      lcCon = lfInListString(1, 'ITEM.CSTYGROUP')
      *B99999,1 MMT 02/16/2005 Fix bug of error when select item group[Start]
      IF !EMPTY(lcCon)
        lcWhereCon = lcWhereCon + " AND ITEM.CSTYGROUP IN (" + lcCon + ")"
      ENDIF
    ENDIF
  ENDIF  && Called from Receiving

  *********************************************************************
  *--Combining The SQl Statement Parts
  *********************************************************************
  *
  *--Combine the Statement Parts
  lcSQLStmt = "SELECT " + lcFields + " FROM " + lcJoin + " WHERE " + lcWhereCon

  *********************************************************************
  *--Run The SQl Statement and Adjust remaining field values
  *********************************************************************
  *

  WAIT WINDOW "Collecting data for Report... " NOWAIT

  *--Run the SQL Statement
  lnResult = loOgScroll.oRDA.SqlRun(lcSQLStmt, lcSQLPOS, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE",)

  IF lnResult = 1
    *--Make a copy of the working file to the working directory to be used by crystal
    IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF")
      ERASE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF")
    ENDIF

    SELECT (lcSQLPOS)
    COPY TO oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF" WITH CDX

    *--Make a copy of Scale file to Work Directory as it is needed by Crystal and cannot be handled from its default location
    IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRpSclTmp + ".DBF")
      ERASE (oAriaApplication.WorkDir + lcRpSclTmp + ".DBF")
    ENDIF

    lnRemResult = loOgScroll.oRDA.SqlRun("SELECT * FROM SCALE", "SCALE_TMP", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")
    IF lnRemResult <> 1
      =gfModalGen('TRM00416B40011','ALERT')
      IF llCalledFromRec
        oOptionGrid = .NULL.
        SET DATASESSION TO (lnDataSess)
      ENDIF
      oAriaApplication.gcDevice = lcDevice
      RETURN .F.
    ENDIF

    SELECT SCALE_TMP
    COPY TO oAriaApplication.WorkDir + lcRpSclTmp + ".DBF" WITH CDX
    USE IN SCALE_TMP

    *--Open the working file from its new location to handle other needed field values
    USE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF") IN 0 ALIAS (lcRPHdrTmp)

    IF llCalledFromRec
      SELECT (lcRPHdrTmp)
      APPEND FROM (oAriaApplication.WorkDir + lcTmpPosLn + '.DBF') FOR IIF(llCalledFromRec AND lcRpSortBy = 'T', .T., TranCd = '2')

      *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[Start]
      SELECT (lcRPHdrTmp)
      SCAN
        lnSeleResult = loOgScroll.oRDA.SqlRun("SELECT POSHDR.COMPLETE,POSHDR.ENTERED FROM POSHDR(INDEX = POSHDR) where Cstytype = '"+&lcRPHdrTmp..Cstytype+"' and cbusdocu = '"+&lcRPHdrTmp..cbusdocu +"' and PO = '"+&lcRPHdrTmp..PO+"'", "POSHDR_TMP", , oAriaApplication.ActiveCompanyConStr, 3, 3, "BROWSE")
        IF lnSeleResult >0
          SELECT (lcRPHdrTmp)
          REPLACE ENTERED  WITH POSHDR_TMP.ENTERED,;
            COMPLETE WITH POSHDR_TMP.COMPLETE
          USE IN POSHDR_TMP
        ENDIF
        SELECT (lcRPHdrTmp)
        REPLACE DATE WITH ldReDate
      ENDSCAN
      *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[End]

    ENDIF

    IF !lfAdjustFields()
      USE IN (lcRPHdrTmp)
      IF llCalledFromRec
        oOptionGrid = .NULL.
        SET DATASESSION TO (lnDataSess)
      ENDIF
      oAriaApplication.gcDevice = lcDevice
      RETURN .F.
    ENDIF
  ELSE  && An error happened while running the query
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    IF llCalledFromRec
      oOptionGrid = .NULL.
      SET DATASESSION TO (lnDataSess)
    ENDIF
    oAriaApplication.gcDevice = lcDevice
    RETURN .F.
  ENDIF
ENDIF

WAIT CLEAR

*********************************************************************
*-- Prepare and Run The Report
*********************************************************************
*
*--Create report parameters and cursors arrays
=lfAdjustCRSettings()

IF !USED(lcRPHdrTmp)
  USE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" IN 0
ENDIF

SELECT (lcRPHdrTmp)
IF RECCOUNT() = 0
  *--There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  USE IN (lcRPHdrTmp)
  IF llCalledFromRec
    oOptionGrid = .NULL.
    SET DATASESSION TO (lnDataSess)
  ENDIF
  oAriaApplication.gcDevice = lcDevice
  RETURN .F.
ENDIF

IF llCalledFromRec
  SELECT (lcRPHdrTmp)
  LOCATE
ELSE
  USE IN (lcRPHdrTmp)
ENDIF

*--Run the Report
SET DATASESSION TO (lnDataSess)
gfDispRe(IIF(llCalledFromRec, IIF(lcInvType = '0001', 'POSTYREC', 'MAPOREC'), .F.))

IF llCalledFromRec
  oOptionGrid = .NULL.
ENDIF

RETURN

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter()
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS

DO CASE
CASE lnArrayType = 1              && Fixed Filter
  lnPOS = ASCAN(loOgScroll.laOGFxFlt, lcFilter)
  IF lnPOS > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt, lnPOS, 1)
    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
CASE lnArrayType = 2             && Hidden Filter
  lnPOS = ASCAN(loOgScroll.laOGHDFlt, lcFilter)
  IF lnPOS > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt, lnPOS, 1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
CASE lnArrayType = 3           && Variable Filter
  lnPOS = ASCAN(loOgScroll.laOGvrFlt, lcFilter)
  IF lnPOS > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt, lnPOS, 1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
  ELSE
    lcReturn = ""
  ENDIF
OTHERWISE :
  lcReturn = ""
ENDCASE

RETURN lcReturn

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]
DIMENSION loOgScroll.laCRParams[13,2]

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRpSclTmp + ".DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF"
loOgScroll.laCRParams[1,1] = 'Format'

IF llRPCostDt
  loOgScroll.laCRParams[1,2] = 'Detailed'
ELSE
  loOgScroll.laCRParams[1,2] = 'Summary'
ENDIF

loOgScroll.laCRParams[2,1] = 'OpTitle'
loOgScroll.laCRParams[2,2] = lcRpTitle
loOgScroll.laCRParams[3,1] = 'GroupName'
loOgScroll.laCRParams[3,2] = lcRpSortBy
loOgScroll.laCRParams[4,1] = 'OrderGroup'
loOgScroll.laCRParams[5,1] = 'ReportName'
DO CASE
CASE lcRPPoType $ 'PANR' AND lcInvType = '0001'
  loOgScroll.laCRParams[4,2] = 'SPO'
  loOgScroll.laCRParams[5,2] = 'Style Purchase Order Receipt Log'
CASE lcRPPoType $ 'MALR' AND lcInvType = '0002'
  loOgScroll.laCRParams[4,2] = 'MPO'
  loOgScroll.laCRParams[5,2] = 'Material Purchase Order Receipt Log'
CASE lcRPPoType $ 'U'
  loOgScroll.laCRParams[4,2] = 'CTO'
  loOgScroll.laCRParams[5,2] = 'Cutting Ticket Receipt Log'
CASE lcRPPoType $ 'F'
  loOgScroll.laCRParams[4,2] = 'MFO'
  loOgScroll.laCRParams[5,2] = 'Material Manufacturing Order Receipt Log'
OTHERWISE
  loOgScroll.laCRParams[4,2] = 'DYO'
  loOgScroll.laCRParams[5,2] = 'Dye Order Receipt Log'
ENDCASE

loOgScroll.laCRParams[6,1] = 'REPEATREPORTHEADER'
loOgScroll.laCRParams[6,2] = 1
loOgScroll.laCRParams[7,1] = 'SortBy'
DO CASE
CASE lcRpSortBy = 'V'
  loOgScroll.laCRParams[7,2] = 'Vendor'
CASE lcRpSortBy = 'S'
  loOgScroll.laCRParams[7,2] = 'Style'
CASE lcRpSortBy = 'M'
  loOgScroll.laCRParams[7,2] = 'Complete Date'
CASE lcRpSortBy = 'R'
  loOgScroll.laCRParams[7,2] = 'Receive Date'
CASE lcRpSortBy = 'T'
  loOgScroll.laCRParams[7,2] = 'Shipment'
OTHERWISE
  loOgScroll.laCRParams[7,2] = 'PO Number'
ENDCASE

loOgScroll.laCRParams[8,1] = 'cDyeConfig'

IF lcInvType = '0001'
  loOgScroll.laCRParams[8,2] = IIF(gfGetMemVar('M_DYELOT') = 'Y', IIF(gfGetMemVar('M_STYCNFG') = 'Y', 'C', 'D'), 'R')
ELSE
  loOgScroll.laCRParams[8,2] = IIF(gfGetMemVar('M_MATDYE') = 'Y', 'D', 'R')
ENDIF

loOgScroll.laCRParams[9,1] = 'StyMask'
loOgScroll.laCRParams[9,2] = LEN(gfItemMask('PM', '', lcInvType))
loOgScroll.laCRParams[10,1] = 'cMajorTitle'
loOgScroll.laCRParams[10,2] = ALLTRIM(lcMajTtl)
loOgScroll.laCRParams[11,1] = 'lMultiCurr'
loOgScroll.laCRParams[11,2] = IIF(TYPE('lcRpCurr') = 'C' AND lcRpCurr = 'F' AND llMultCurr AND !(lcRPPoType $ 'UFD'), 1, 0)
loOgScroll.laCRParams[12,1] = 'lMultiWare'
loOgScroll.laCRParams[12,2] = IIF(lfChkMulWH(), 1, 0)
loOgScroll.laCRParams[13,1] = 'cBaseCurr'
loOgScroll.laCRParams[13,2] = oAriaApplication.BaseCurrency

IF llRPCostDt
  LOCAL lnI, laSetUps[7,2], laCost[7]
  STORE '' TO laSetUps, laCost

  DIMENSION loOgScroll.laCRParams[20,2]

  IF lcInvType = '0002' AND lcRPPoType # 'F'
    FOR lnI = 1 TO 7
      loOgScroll.laCRParams[13+lnI,1] = 'cCostLabel' + STR(lnI,1)
    ENDFOR
    loOgScroll.laCRParams[14,2] = LANG_MAT_ICOSTLLBL1
    loOgScroll.laCRParams[15,2] = LANG_MAT_ICOSTLLBL2
    loOgScroll.laCRParams[16,2] = LANG_MAT_ICOSTLLBL3
    loOgScroll.laCRParams[17,2] = LANG_MAT_ICOSTLLBL4
    loOgScroll.laCRParams[18,2] = LANG_MAT_ICOSTLLBL5
    loOgScroll.laCRParams[19,2] = LANG_MAT_ICOSTLLBL6
    loOgScroll.laCRParams[20,2] = LANG_MAT_ICOSTLLBL7
  ELSE
    FOR lnI = 1 TO IIF(lcInvType = '0001', 7, 4)
      laSetUps[lnI,1] = 'M_C' + IIF(lcInvType = '0002', 'T', IIF(gcAct_Appl = "MF", 'M', 'I')) + 'SLBL' + STR(lnI,1)
    ENDFOR

    = gfGetMemVar(@laSetUps)

    FOR lnI = 1 TO 7
      loOgScroll.laCRParams[13+lnI,1] = 'cCostLabel' + STR(lnI,1)
      loOgScroll.laCRParams[13+lnI,2] = LEFT(laSetUps[lnI,2],9)
    ENDFOR
  ENDIF
ENDIF
loOgScroll.cCROrientation = 'L'

*************************************************************
*! Name      : lfMajInfGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the title and picture of style major segement
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajInfGet()
*!*************************************************************

FUNCTION lfMajGet

lcMajPic = gfItemMask('PM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))
lnMajPic = LEN(lcMajPic)
lcMajPic = "@! " + lcMajPic
lcMajTtl = gfItemMask('HM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF (lcInvType = '0002' OR lcRPPoType = 'U') AND !llFrstTime
  lcSqlStat1 = "SELECT ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOgScroll.oRDA.SqlRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|", "lcFabDye|")
  ENDIF
ENDIF

llMultCurr = lfChkMulCu()

IF llMultCurr
  =lfvPrice()
ELSE
  lcRpCurr = "O"
ENDIF

=lfArrDumy(loOgScroll.lcOGRepID)

IF loOgScroll.lcOGRepID = 'POSTYREC'
  IF oAriaApplication.ActiveModuleID == 'PO'
    loOgScroll.lcOGWinTitl = "Style Purchase Order Receipt Log"
  ELSE
    loOgScroll.lcOGWinTitl = "Dye Order Receipt Log"
  ENDIF

  *! B610515,1 HIA 09/15/13 T20130906.0007 - company designation in subject on email version the Style Purchase Order Receipt Log [Begin]
  loOgScroll.lcOGWinTitl = '(' + ALLTRIM(oAriaApplication.ActiveCompanyID) + ') ' + ;
                 IIF(EMPTY(oAriaApplication.ActiveCompanyName), ;
                     '', ;
                     '- ' + ALLTRIM(oAriaApplication.ActiveCompanyName) + ' - ') + ;
                loOgScroll.lcOGWinTitl
                
  *! B610515,1 HIA 09/15/13 T20130906.0007 - company designation in subject on email version the Style Purchase Order Receipt Log [End]
ENDIF

*--end of lfwRepWhen.

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/1/2004
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
FUNCTION lfFillVars

IF lfChkMulCu()
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    lnResult = loOgScroll.oRDA.SqlRun("SELECT * FROM SYCCURR", "SYCCURR", , oAriaApplication.SystemConnectionString, 3, "BROWSE",)

    IF lnResult <> 1
      *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF

    =lfCreateIndecies("SYCCURR", "CCURRCODE|", "CCURRCODE|")
    SELECT SYCCURR
    SET ORDER TO CCURRCODE
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  *-- if multi currency evaluate currency arrays [Begin]
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- end of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/1/2004
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--Erase working files from working folder after closing the option grgid
IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF")
  ERASE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF")
ENDIF

IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRpSclTmp + ".DBF")
  ERASE (oAriaApplication.WorkDir + lcRpSclTmp + ".DBF")
ENDIF

*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/14/2004
*! Purpose   : Enable / Disable Currency display if Retail price.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChCurSm()
*!*************************************************************
FUNCTION lfvPrice

IF ASCAN(laOGObjType, 'LLRPCOSTDT') # 0
  lnCostDtPo = ASUBSCRIPT(laOGObjType, ASCAN(laOGObjType, 'LLRPCOSTDT'),1)
  laOGObjCnt[lnCostDtPo] = IIF(ASCAN(laOGObjType, 'LCRPPRICE') # 0, lcRPPrice = 'C', .T.)
  IF ASCAN(laOGObjType, 'LCRPPRICE') # 0 AND lcRPPrice = 'R'
    llRPCostDt = .F.
  ENDIF
  = lfOGShowGet('llRPCostDt')
  = lfvPrnForn()
ENDIF
*-- end of lfvPrice.

*!*************************************************************
*! Name      : lfPrnFrnSt
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   :
*!*************************************************************
*! Example   : =lfPrnFrnSt()
*!*************************************************************
FUNCTION lfPrnFrnSt

IF ASCAN(loOgScroll.laOGObjType, 'llRPFrnCur') # 0
  lnFnrCurPo = ASUBSCRIPT(loOgScroll.laOGObjType, ASCAN(loOgScroll.laOGObjType, 'llRPFrnCur'), 1)
  IF llMultCur AND !llRPCostDt AND (ASCAN(laOGObjType, 'LCRPPRICE') = 0 OR lcRPPrice = 'C')
    loOgScroll.laOGObjCnt[lnFnrCurPo] = .T.
  ELSE
    loOgScroll.laOGObjCnt[lnFnrCurPo] = .F.
    llRPFrnCur = .F.
  ENDIF
  = loOgScroll.lfOGShowGet('llRPFrnCur')
  = lfvPrnForn()
ENDIF

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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

FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major
  *-- unique index.

  *lnRemResult = loOgScroll.oRDA.SqlRun("SELECT * FROM STYLE", "STYLE_X", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")
  *=lfCreateIndecies("STYLE_X", "Style|", "Style|")
  USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0

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
*! Name      : lfSRVPo
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 03/09/1999
*! Purpose   : control browsing primary fabric and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty
DO CASE
CASE lcParm = 'S'  && Set code
  SET ORDER TO Vencode IN APVENDOR
  SELECT POSHDR
  SET ORDER TO TAG POSHDR
  SET RELATION TO VENDOR INTO APVENDOR ADDITIVE
  GO TOP IN POSHDR

CASE lcParm = 'R'  && Reset code
  SELECT POSHDR
  SET RELATION TO
ENDCASE
*-- end of lfSRVPo

*!*************************************************************
*! Name      : lfGetVendor
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Get Vendor Name...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetVendor()
*!*************************************************************
FUNCTION lfGetVendor
LPARAMETERS lcVenCode, lcStyType

IF lcStyType = 'N'
  =SEEK(RTRIM(lcVenCode), 'WAREHOUS', 'WAREHOUS')
  RETURN WAREHOUS.cDesc
ELSE
  =SEEK(lcVenCode, 'APVENDOR', 'VENCODE')
  RETURN APVENDOR.cVenComp
ENDIF

*!*************************************************************
*! Name      : lfCalAmnts
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Calculte amounts in base currency to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCalAmnts()
*!*************************************************************
FUNCTION lfCalAmnts
LPARAMETERS lcName

LOCAL lnRetrnVal
lnRetrnVal = 0

*--If Print in forign currency.
IF lcRpCurr = "F"
  lnRetrnVal = EVALUATE(lcName + ".nflanCost1")
ELSE
  *--If Print in original base currency.
  IF lcRpCurr = "O"
    lnRetrnVal = IIF(ASCAN(laOGObjType, 'LCRPPRICE') # 0 AND lcRPPrice = 'R', lfPrnPrice(), EVALUATE(lcName + ".nlan_Cost1"))
  ELSE  && print By Date or calculated now.
    SELECT SYCCURR
    IF (CCURRCODE = gcBaseCurr) OR (EVALUATE(lcName+".nlan_Cost1") = 0)
      lnRetrnVal = EVALUATE(lcName + ".nlan_Cost1")
    ELSE
      lnRetrnVal = gfAmntDisp(EVALUATE(lcName + ".nflanCost1"), lcRpCurr, ldRpExDate, lcRpTmpNam)
    ENDIF
  ENDIF
ENDIF

RETURN lnRetrnVal

*!*************************************************************
*! Name      : lfPrnPrice
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : print price
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfPrnPrice()
*!*************************************************************
FUNCTION lfPrnPrice

LOCAL lnPrice, ldStart, ldEnd, lnDscPrcnt

IF lcRPPoType = 'N'
  IF EMPTY(STYLE.cDiscCode)
    lnPrice = &lcRPHdrTmp..nSugRetPri
  ELSE
    STORE {} TO ldStart, ldEnd
    STORE 0 TO lnDscPrcnt

    DIMENSION laDiscRng[3,2]
    laDiscRng[1,1] = "START"
    laDiscRng[1,2] = "ldStart"
    laDiscRng[2,1] = "DENDATE"
    laDiscRng[2,2] = "ldEnd"
    laDiscRng[3,1] = "DISCPCNT"
    laDiscRng[3,2] = "lnDscPrcnt"

    *-- Fill the related GL information from the codes file.
    =gfRltFld(STYLE.cDiscCode, @laDiscRng, "CDISCCODE")

    IF BETWEEN(POSHDR.ENTERED, ldStart, ldEnd)
      lnPrice = &lcRPHdrTmp..nSugRetPri - (&lcRPHdrTmp..nSugRetPri * lnDscPrcnt / 100)
    ELSE
      lnPrice = &lcRPHdrTmp..nSugRetPri
    ENDIF
  ENDIF
ELSE
  IF oAriaApplication.ActiveModuleID # 'MA'
    lnPrice = &lcRPHdrTmp..nSugRetPri
  ELSE
    SELECT (lcRPHdrTmp)
    lnPrice = nSugRetPri
  ENDIF
ENDIF

RETURN lnPrice

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Wael M. Ab-Shawareb (WSH)
*! Date      : 10/11/2004
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

LOCAL llStopConc

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg = gfItemMask('SM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))  && No. of major segments.

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg, '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(EMPTY(lnNonMajPo), laMajSeg[lnI,4], lnNonMajPo)

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

lcColorTt = 'Only these ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfGetPoTyp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/10/2004
*! Purpose   : Get the initial value of lcRpPoType variable.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :
*!*************************************************************
FUNCTION lfGetPoTyp

LOCAL lcRet

IF oAriaApplication.ActiveModuleID = "MF"
  lcRet = 'D'
ELSE
  lcRet = 'A'
ENDIF

RETURN lcRet
*--end of lfGetPoTyp.

*!*************************************************************
*! Name      : lfvPOType
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 11/10/2004
*! Purpose   : Valid function for Order type Combo
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfvPOType()
*!*************************************************************
FUNCTION lfvPOType

DO CASE
CASE lcRPPoType $ 'PDMF'
  lcBusDocu = 'P'
CASE lcRPPoType = 'A' && ALL
  lcBusDocu = 'A'     && ALL
CASE lcRPPoType = 'R'
  lcBusDocu = 'R'
CASE lcRPPoType $ 'NL'
  lcBusDocu = 'N'
ENDCASE

IF lcRPPoType $ 'NL'
  lcLocationTtl   = "Target Location"
  laSortDesc[5,1] = "Source Location"
ELSE
  lcLocationTtl   = "Location"

  IF lcRPPoType $ 'APRM'
    laSortDesc[5,1] = "Vendor"
  ENDIF
ENDIF

ClearRead()

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask('HM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask('PM', '', IIF(oAriaApplication.ActiveModuleID == 'MA', '0002', '0001'))

RETURN lcMajPic

*!*************************************************************
*! Name      : lfChkMulCu
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : check system using for multi currency
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulCu()
*!*************************************************************
FUNCTION lfChkMulCu

RETURN gfGetMemVar('llMulCurr')

*!*************************************************************
*! Name      : lfChkMulWH
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : check system using for multi warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkMulWH()
*!*************************************************************
FUNCTION lfChkMulWH

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS')))  = 'Y')

*!*************************************************************
*! Name      : lfChkDye
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check if the system uses dyelot
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfChkDye()
*!*************************************************************
FUNCTION lfChkDye

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfArrDumy
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Check if the system uses dyelot
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfArrDumy()
*!*************************************************************
FUNCTION lfArrDumy
PARAMETERS lcRepID

DIMENSION laSortDesc[4,1] , laSortVal[4,1]

laSortDesc[2,1] = ALLTRIM(gfItemMask("HM", "", IIF(oAriaApplication.ActiveModuleID = 'MA', "0002", "0001")))
laSortDesc[3,1] = "Complete Date"
laSortDesc[4,1] = "Receive Date"

laSortVal[1,1]  = "P"
laSortVal[2,1]  = "S"
laSortVal[3,1]  = "M"
laSortVal[4,1]  = "R"

DO CASE
CASE lcRepID = 'POSTYREC' AND oAriaApplication.ActiveModuleID = 'PO'
  DIMENSION laSortDesc[6,1] , laSortVal[6,1]

  laSortDesc[1,1] = "Style PO #"
  laSortDesc[5,1] = "Vendor"
  laSortDesc[6,1] = "Shipment"

  laSortVal[5,1] = "V"
  laSortVal[6,1] = "T"

CASE lcRepID = 'MAPOREC'
  DIMENSION laSortDesc[5,1] , laSortVal[5,1]

  laSortDesc[1,1] = "Material PO #"
  laSortDesc[5,1] = "Vendor"

  laSortVal[5,1] = "V"

CASE lcRepID = 'MFCTREC'
  DIMENSION laSortDesc[3,1], laSortVal[3,1]

  laSortDesc[1,1] = "Ticket #"
  laSortDesc[3,1] = 'Date'
  laSortVal[3,1]  = "R"

CASE lcRepID = 'MAMFREC'
  laSortDesc[1,1] = "MFG Order #"

CASE lcRepID = 'POSTYREC' AND oAriaApplication.ActiveModuleID = 'MF'
  DIMENSION laSortDesc[5,1] , laSortVal[5,1]

  laSortDesc[1,1] = "Dye Order #"
  laSortDesc[5,1] = "Vendor"

  laSortVal[5,1] = "V"
ENDCASE

=loOgScroll.lfOGShowGet('LCRPSORTBY')

*!*************************************************************
*! Name      : lfStySum
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/13/2004
*! Purpose   : Sum Quantities from Style file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStySum()
*!*************************************************************
FUNCTION lfStySum
LPARAMETERS lcSty, lccomp, lnAddToVar

PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)

lnTotcomp = 0

SELECT STYLE_X
SET ORDER TO STYLE

IF SEEK(ALLTRIM(lcSty))
  SUM &lccomp TO lnTotcomp WHILE STYLE = ALLTRIM(lcSty)
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

*!*************************************************************
*! Name      : lfvCostDtl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/13/2004
*! Purpose   :
*!*************************************************************
FUNCTION lfvCostDtl

=lfvPrnForn()

*!*************************************************************
*! Name      : lfvPrnForn
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/14/2004
*! Purpose   : validate Print Costing Details option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnForn()
*!*************************************************************
FUNCTION lfvPrnForn

LOCAL lnVarCnt, lncCurDisp, lncDuCurPO, lncPrCurPO

IF !(lcRPPoType $ 'UDF')  && Not Cutting Ticket, Dye Order, or MFG Ord
  *-- Adjust Enabling stauts of currency fields (Start)
  lncCurDisp = ASCAN(laOGObjType,'LNREPCURR', 1, ALEN(laOGObjType), 0, 9)

  *!*    lncPrCurPOFlt = ASCAN(laOGFxFlt,'POSHDR.CPRICECUR', 1, ALEN(laOGFxFlt), 0, 9)
  *!*    lncDuCurPOFlt = ASCAN(laOGFxFlt,'POSHDR.CDUTYCUR', 1, ALEN(laOGFxFlt), 0, 9)

  *!*    lncPrCurPO = ASCAN(laOGObjType, 'laOGFxFlt[' + ALLTRIM(STR(lncPrCurPOFlt)) + ',', 1, ALEN(laOGObjType), 0, 9)
  *!*    lncDuCurPO = ASCAN(laOGObjType, 'laOGFxFlt[' + ALLTRIM(STR(lncDuCurPOFlt)) + ',', 1, ALEN(laOGObjType), 0, 9)

  laOGObjCnt[lncCurDisp] = !llRPCostDt AND (ASCAN(laOGObjType, 'LCRPPRICE') = 0 OR lcRPPrice = 'C')
  = lfOGShowGet(lncCurDisp)

  *!*    laOGObjCnt[lncPrCurPO] = !llRPCostDt AND (ASCAN(laOGObjType, 'LCRPPRICE') = 0 OR lcRPPrice = 'C')
  *!*    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncPrCurPoFlt)) + ',6]')

  *!*    laOGObjCnt[lncDuCurPO] = !llRPCostDt AND (ASCAN(laOGObjType, 'LCRPPRICE') = 0 OR lcRPPrice = 'C')
  *!*    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lncDuCurPoFlt)) + ',6]')
ENDIF

IF llRPCostDt
  IF !(lcRPPoType $ 'UDF')
    lcRpCurr = 'F'
    *!*      loOgScroll.laOGFxFlt[lncPrCurPo-lnVarCnt,6] = gcBaseCurr
    *!*      loOgScroll.laOGFxFlt[lncDuCurPo-lnVarCnt,6] = gcBaseCurr
  ENDIF
  loOgScroll.lcOGLastForm = 'ITMREC'
  *B609809,1 MMT 01/30/2012 PO Receipt Log report exports wrong report format to PDF[Start]
  LCRPFORMAT = 'ITMREC'
  *B609809,1 MMT 01/30/2012 PO Receipt Log report exports wrong report format to PDF[END]
  IF ASCAN(laOGObjType, 'LCRPFORMAT') # 0
    lnRepFormat = ASUBSCRIPT(laOGObjType, ASCAN(laOGObjType, 'LCRPFORMAT'),1)
    laOGObjCnt[lnRepFormat] = .F.
    = lfOGShowGet('LCRPFORMAT')
  ENDIF
ELSE
  *B609809,1 MMT 01/30/2012 PO Receipt Log report exports wrong report format to PDF[Start]
  LCRPFORMAT = 'ITMRVB'
  *B609809,1 MMT 01/30/2012 PO Receipt Log report exports wrong report format to PDF[END]
  loOgScroll.lcOGLastForm = LCRPFORMAT

  IF ASCAN(laOGObjType, 'LCRPFORMAT') # 0
    lnRepFormat = ASUBSCRIPT(laOGObjType, ASCAN(laOGObjType, 'LCRPFORMAT'),1)
    laOGObjCnt[lnRepFormat] = .T.
    = lfOGShowGet('LCRPFORMAT')
  ENDIF
ENDIF

*-- Adjust Enabling stauts of currency fields (End)

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr, @ldRpExDate, lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfSumFab
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfSumFab
LPARAMETERS lcFab, lccomp

LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0
*lnAlias   = ALIAS(SELECT())

*!*  SELECT ITEM
*!*  lnItemRec = IIF(BETWEEN(RECNO(lcTmpFab),1,RECCOUNT(lcTmpFab)),RECNO(lcTmpFab),1)

*!*  IF lnItemRec > 0
IF SEEK(ALLTRIM(lcFab), lcTmpFab)
  SUM &lcTmpFab..&lccomp. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
ENDIF

*!*    SELECT ITEM
*!*    GO lnItemRec
*!*  ENDIF

*SELECT (lnAlias)
RETURN lnTotcomp

*!*************************************************************
*! Name      : lfInListString
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : Build In List Value
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfInListString
LPARAMETERS lnFiltType, lcFiltExp

LOCAL lcILString
lcILString = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcILString)
  lcILString = "'" + STRTRAN(lcILString, "|", "','") + "'"
ENDIF

RETURN lcILString

*!*************************************************************
*! Name      : lfCreateSQLCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : Build SQL Cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfCreateSQLCursor
LPARAMETERS lnFiltType, lcFiltExp, lcSQLCurs, lcStruct, lcRetFld

LOCAL lcCurName, lcSQLRet, llFound, lnRet
llFound = .F.
lnRet   = 0  && Filter not found

lcCurName = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcCurName)
  SELECT (lcCurName)
  llFound = (RECCOUNT() > 0)
  IF llFound
    lcSQLCurs = loOgScroll.gfSQLTempName('', lcStruct, lcCurName, lcRetFld) && SQL Temp File
    IF EMPTY(lcSQLCurs)
      *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      lnRet = -1  && Error while creating cursor
    ELSE
      lnRet = 1   && Cursor Created
    ENDIF
  ENDIF
ENDIF

RETURN lnRet

*!*************************************************************
*! Name      : lfBuildStyleCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : Build SQL Style Cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfBuildStyleCursor
LPARAMETERS lcSQLStyle

LOCAL llStyle, llStyGr, llComp, lcOptStyle, lcGroups, lcStyStat, lcTempSty, lcSQLComp, lnRet
lcTempSty = loOgScroll.gfTempName()
llStyle   = .F.
lnRet     = 0  && Filter not found

*--Check if there is a filter on Style
lcOptStyle = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
IF !EMPTY(lcOptStyle)
  SELECT (lcOptStyle)
  llStyle = (RECCOUNT() > 0)
ENDIF

IF lcRPPoType = 'U'  && If Order is Cutting Ticket
  *--Check if there is a Primary Component
  lcSQLComp = lfCheckFilter(1, 'STYLE.FABRIC')
  IF !EMPTY(lcSQLComp)
    SELECT (lcSQLComp)
    llComp = (RECCOUNT() > 0)
  ENDIF
ENDIF

*--Check if there is a filter on Style Groups
lcGroups = lfCheckFilter(1, 'STYLE.CSTYGROUP')
*: B610810,1 MMT 08/19/2014 PO receipt log report filters by style group incorrectly[T20140815.0002][Start]
*lcGroups = STRTRAN(lcGroups, "|", ",")
lcGroups = STRTRAN(lcGroups, "|", "','")
*: B610810,1 MMT 08/19/2014 PO receipt log report filters by style group incorrectly[T20140815.0002][End]
llStyGr  = !EMPTY(lcGroups)

*--Craete the Select Statement that filter Styles frfom Style file and Run it.
IF llStyle OR llStyGr OR llComp
  lcStyStat = "SELECT A.Style FROM STYLE A " +;
    IIF(llStyle, "INNER JOIN " + lcOptStyle + " B ON A.cStyMajor == B.cStyMajor ", "") +;
    IIF(llComp, "INNER JOIN " + lcSQLComp + " C ON A.Fabric == C.cStyMajor ", "") +;
    IIF(llStyGr, "WHERE A.cStyGroup IN ('" + lcGroups + "') ", "") +;
    "INTO CURSOR " + lcTempSty

  *--Run the Statement
  &lcStyStat

  *--Craete the SQl Style Temp Cursor
  lcSQLStyle = loOgScroll.gfSQLTempName('','STYLE C(19)', lcTempSty, 'Style') && SQL Temp File
  IF EMPTY(lcSQLStyle)
    *--SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    lnRet = -1  && Error while Creating Cursor
  ELSE
    lnRet = 1  && Cursor Successfully created
  ENDIF
ENDIF

IF TYPE("lcOptStyle") = 'C' AND USED(lcOptStyle)
  USE IN (lcOptStyle)
ENDIF

IF USED(lcTempSty)
  USE IN (lcTempSty)
ENDIF

RETURN lnRet

*!*************************************************************
*! Name      : lfAdjustFields
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
*! Purpose   : Adjust needed fields in the working file
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfAdjustFields

*--Fill the Contractors 1,2 Fields in the Result Cursor from MFGOPRHD file
IF lcRPPoType $ 'U'  && In case of Cutting Tickets only
  LOCAL lcSQLTickets

  *--Create Temp SQL Cursor for the Resulted Ticket Numbers
  lcSQLTickets = loOgScroll.gfSQLTempName('', 'CutTkt C(6)', lcRPHdrTmp, 'PO') && SQL Temp File
  IF EMPTY(lcSQLTickets)
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF

  *--Create Statement to get Contractors for the Resulted Tickets Only
  lcSQLStmt = "SELECT cTktNo, cContCode FROM MFGOPRHD (INDEX = MFGOPRHD) INNER JOIN " +;
    lcSQLTickets + " TKTS ON TKTS.CutTkt = MFGOPRHD.cTktNo WHERE MFGOPRHD.cIMTyp = 'M' AND lInHouse = 0"
  lnResult = loOgScroll.oRDA.SqlRun(lcSQLStmt, lcSQLCONTS, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE",)

  IF lnResult <> 1
    *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF

  =lfCreateIndecies(lcSQLCONTS, "cTktNo|", "cTktNo|")
  SELECT (lcSQLCONTS)
  SET ORDER TO cTktNo
ENDIF

*--Open Currency file to hande Multi Currency
IF !(lcRPPoType $ 'UFD') AND llMultCurr
  SELECT SYCCURR
  SET ORDER TO CCURRCODE
  SELECT (lcRPHdrTmp)
ENDIF

*--Set Relation to Style to Get needed field values from style file
IF lcInvType = '0001'
  SELECT STYLE
  SET ORDER TO STYLE
ENDIF

LOCAL llMultiWare, lnRemResult
llMultiWare = lfChkMulWH()

IF llMultiWare
  *-- Open Warehouse file to get warehouse descriptions Account
  lnRemResult = loOgScroll.oRDA.SqlRun("SELECT cWareCode, cDesc FROM warehous", "WARE_TMP", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")

  =lfCreateIndecies("WARE_TMP", "cWareCode|", "WARE_TMP|")
  SELECT WARE_TMP
  SET ORDER TO WARE_TMP
ENDIF

IF llRPCostDt && Case of Detail Costing
  *-- Open GL_LINK file to get WIP Account
  lnRemResult = loOgScroll.oRDA.SqlRun("SELECT LINK_CODE, CatgKey, GLACNT FROM GL_LINK", "LINK_TMP", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")

  =lfCreateIndecies("LINK_TMP", "Link_Code+CatgKey|", "LINK_TMP|")
  SELECT LINK_TMP
  SET ORDER TO LINK_TMP

  *--Create Relation with APVendor file to get Vendor Description field
  SELECT APVENDOR
  SET ORDER TO Vencode

  *--Open CODES file to get Codes Fields' Description Account
  ****
  **** I don't use the "gfCodDes" Global function as it makes performance very pad "TOO SLOW"...
  *--Open Temp CODES Cursor for Division Description
  lnRemResult = loOgScroll.oRDA.SqlRun("SELECT cCode_No, cDiscrep FROM CODES WHERE cFld_Name = 'CDIVISION '", "CODES_DIV", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")

  =lfCreateIndecies("CODES_DIV", "cCode_No|", "CODES_DIV|")
  SELECT CODES_DIV
  SET ORDER TO CODES_DIV

  *--Open Temp CODES Cursor for Season Description
  lnRemResult = loOgScroll.oRDA.SqlRun("SELECT cCode_No, cDiscrep FROM CODES WHERE cFld_Name = 'SEASON    '", "CODES_SES", , oAriaApplication.cAriaNativeDataFilesConStr, 3, 3, "BROWSE")

  =lfCreateIndecies("CODES_SES", "cCode_No|", "CODES_SES|")
  SELECT CODES_SES
  SET ORDER TO CODES_SES
ENDIF

LOCAL lcWIPAccount, lcUOM_B, lcUOM_V, lnConv, lcStat, lcCurrGroup, lcShpmtHdr, lcVendor, lcVenCode
lcCurrGroup = ''
lcVenCode   = ''
lcShpmtHdr  = loOgScroll.gfTempName()
lcVendor    = loOgScroll.gfTempName()

*: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [Start]
lcLastPO = ''
*: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [End]


*--Scan in working file to handle other needed fields
SELECT (lcRPHdrTmp)
SCAN

  *: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [Start]
  IF lcLastPO = &lcRPHdrTmp..PO
    REPLACE POActCst1 WITH 0,;
      POActCst2 WITH 0,;
      POActCst3 WITH 0,;
      POActCst4 WITH 0,;
      POActCst5 WITH 0,;
      POActCst6 WITH 0,;
      POActCst7 WITH 0
  ENDIF
  lcLastPO = &lcRPHdrTmp..PO
  *: B608628,1 MMT 07/22/2008 Fix Bug of Wrong Actual cost in cut ticket receipt log report [End]


  IF lcInvType = '0001'
    =SEEK(STYLE, 'Style')
    REPLACE StyDesc    WITH STYLE.Desc1,;
      nSugRetPri WITH STYLE.nSugRetPri,;
      SCALE      WITH STYLE.SCALE
  ENDIF

  *--Adjust Cost accorging to Currwncy Display Type
  *--B127309,MMT,04/11/2005 Fix bug of wrong landed cost printing[Start]
  IF llMultCurr AND !(lcRPPoType $ 'UFD')
    SELECT (lcRPHdrTmp)
    =SEEK(cPriceCur, "SYCCURR")
    REPLACE cCurDesc WITH cPriceCur + ' - ' + SYCCURR.CCURRDESC
    IF !llRPCostDt AND !llCalledFromRec
      REPLACE nlan_Cost1 WITH lfCalAmnts(lcRPHdrTmp)
    ENDIF
    *--B127309,MMT,04/11/2005 Fix bug of wrong landed cost printing[End]
  ENDIF

  IF llCalledFromRec
    IF lcCurrGroup <> cbusdocu + Cstytype + PO + ShipNo
      lcCurrGroup = cbusdocu + Cstytype + PO + ShipNo
      lcStat = "SELECT SHPMTHDR.AirWayB, SHPMTHDR.Entered, SHPMTHDR.Cartons, SHPMTHDR.ETA, UOM.cUOM_B, UOM.cUOM_V, UOM.nConf " +;
        "  FROM POSLN (INDEX = POSLN)" +;
        "  LEFT OUTER JOIN SHPMTHDR (INDEX = SHPMTHDR)" +;
        "    ON SHPMTHDR.cBusDocu = POSLN.cBusDocu AND " +;
        "       SHPMTHDR.cShpType = POSLN.cStyType AND " +;
        "       SHPMTHDR.ShipNo = " + IIF(lcRpSortBy = 'T', "'" + lcShipNo + "'", "POSLN.ShipNo") +;
        "  LEFT OUTER JOIN UOM (INDEX = UOMCODE)" +;
        "    ON POSLN.cUomCode = UOM.cUomCode" +;
        " WHERE POSLN.cBusDocu = '" + EVALUATE(lcRPHdrTmp + '.cBusDocu') + "' AND" +;
        "       POSLN.cStyType = '" + EVALUATE(lcRPHdrTmp + '.cStyType') + "' AND" +;
        "       POSLN.PO = '" + EVALUATE(lcRPHdrTmp + '.PO') + "' AND" +;
        "       POSLN.Trancd = '1' AND" +;
        "       POSLN.[LineNo] = '" + STR(EVALUATE(lcRPHdrTmp + '.LineNo')) + "'"
      lnRemResult = loOgScroll.oRDA.SqlRun(lcStat, lcShpmtHdr, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE",)
      IF lnRemResult <> 1
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      SELECT (lcShpmtHdr)
      LOCATE
    ENDIF

    SELECT (lcRPHdrTmp)
    IF USED(lcShpmtHdr)
      REPLACE UOMBUY  WITH EVALUATE(lcShpmtHdr + '.cUOM_B')  ,;
        UOMUSE  WITH EVALUATE(lcShpmtHdr + '.cUOM_V')  ,;
        nConv   WITH EVALUATE(lcShpmtHdr + '.nConf')   ,;
        AirWayB WITH EVALUATE(lcShpmtHdr + '.AirWayB') ,;
        Cartons WITH EVALUATE(lcShpmtHdr + '.Cartons') ,;
        Eta     WITH EVALUATE(lcShpmtHdr + '.Eta')     ,;
        ShipEnt WITH EVALUATE(lcShpmtHdr + '.Entered')
    ENDIF

    IF lcVenCode <> VENDOR

      *B128055,1 WSH 05/20/2005 Handle special chacaters in Select Statement. [Start]
      *lcStat = "SELECT cVenComp FROM APVENDOR WHERE CVENDCODE = '" + Vendor + "'"
      m.VENDOR = VENDOR
      lcStat   = "SELECT cVenComp FROM APVENDOR WHERE CVENDCODE = ?m.Vendor"
      *B128055,1 WSH 05/20/2005 [End]

      lnRemResult = loOgScroll.oRDA.SqlRun(lcStat, lcVendor, , oAriaApplication.cAriaNativeDataFilesConStr, 3, "BROWSE",)
      IF lnRemResult <> 1
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      SELECT (lcVendor)
      LOCATE
    ENDIF

    SELECT (lcRPHdrTmp)
    IF USED(lcVendor)
      REPLACE VenDesc WITH EVALUATE(lcVendor + '.cVenComp')
    ENDIF
  ENDIF

  *--Solve Bug of bad calculations if nConv field is Null "When UOM Code not found in UOM file(Bad Data)"
  SELECT (lcRPHdrTmp)
  IF ISNULL(nConv)
    REPLACE nConv WITH 1
  ENDIF

  *--Fill values for Division, Season, Vendor, and WIP Account Descriptions
  IF llRPCostDt
    =SEEK(LINK_CODE + '013', "LINK_TMP")
    =SEEK(cDivision, "CODES_DIV")
    =SEEK(Season, "CODES_SES")
    =SEEK(VENDOR, "APVENDOR")

    *--Get WIP Account
    SELECT LINK_TMP
    lcWIPAccount = LINK_CODE + '-' + GLACNT

    SELECT (lcRPHdrTmp)
    REPLACE DivDesc   WITH CODES_DIV.cDiscrep,;
      SeasDesc  WITH CODES_SES.cDiscrep,;
      VenDesc   WITH APVENDOR.cVenComp,;
      cWipAcct  WITH lcWIPAccount
  ENDIF

  IF llMultiWare
    =SEEK(cWareCode, 'WARE_TMP')
    REPLACE cWareDesc WITH WARE_TMP.cDesc
  ENDIF

  *--Fill values for Contractor1 and Contractor2 fields
  IF lcRPPoType $ 'U'
    SELECT (lcSQLCONTS)
    =SEEK(&lcRPHdrTmp..PO)
    IF !EOF()
      REPLACE &lcRPHdrTmp..Cont1 WITH &lcSQLCONTS..cContCode

      SELECT (lcSQLCONTS)
      SKIP
      IF !EOF()
        REPLACE &lcRPHdrTmp..Cont2 WITH &lcSQLCONTS..cContCode
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

*--Close Temporary Openneed files
IF USED('LINK_TMP')
  USE IN LINK_TMP
ENDIF
IF USED('WARE_TMP')
  USE IN WARE_TMP
ENDIF
IF USED('CODES_DIV')
  USE IN CODES_DIV
ENDIF
IF USED('CODES_SES')
  USE IN CODES_SES
ENDIF

*--Clear Relations
SELECT (lcRPHdrTmp)
SET RELATION TO

RETURN .T.
*--End of lfAdjustFields()

*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Wael M. Abo-Shawareb
*! Date      : 10/18/2004
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
  *: B609163,1 MMT 03/09/2010 Fix error while printing receipt log report from rec. screen  [Start]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages) &&OF (lcCursor)
  *: B609163,1 MMT 03/09/2010 Fix error while printing receipt log report from rec. screen  [End]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.
*--End of lfCreateIndecies()

*************************************************************
*! Name      : lfvFormat
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/30/2004
*! Purpose   : To set the report format
*!*************************************************************
FUNCTION lfvFormat()

loOgScroll.lcOGLastForm = LCRPFORMAT
