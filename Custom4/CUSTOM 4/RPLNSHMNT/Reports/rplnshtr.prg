*:****************************************************************
*: Program file  : RPLNSHTR
*: Program desc. : Generate Replenishment Report.
*: System        : Aria Apparel System - Version 40.
*: Module        : (IC)
*: Developer     : SABER A.Razek -  (SAB)
*: Date          : 09/29/2011
*: Tracking#     : C201407
*:****************************************************************
*: Parameters    : lcRequestID    ==> 
*:                 lcXMLFileName  ==> 
*:                 ClientId       ==> 
*:****************************************************************
*:Modifications  :
*! B609739,1 SAB 11/22/2011 Fix Execlusive connection problem [T20110621.0044]
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [T20110621.0044]
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044]
*! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [T20110621.0044]
*! C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [T20110621.0044]
*! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044]
*! E303108,1 SAB 04/10/2012 Develop issue 24 and 35 in Replenishment Project [T20110621.0044]
*! B609913,1 SAB 05/09/2012 Fix Problem in CCONCSSNGRD Field [T20110621.0044]
*! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017]
*! B610017,1 SAB 07/19/2012 Some records not appear in Report[T20120715.0004]
*! C201533,1 SAB 10/31/2012 Change Replenishment Calculation to Include WIP [T20121016.0001]
*! E303305,1 SAB 11/27/2012 Add % Uplift to Replenishment Repport [T20121113.0052]
*! B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005]
*! B610586,2 MMT 11/19/2013 Replinshment Report calculates avialable QTY incorrectly in Destination store[T20131107.0005]
*! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001]
*! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001]
*! B610981,1 MMT 04/19/2015 Fix bug of duplicating records in report[T20150402.0010]
*:****************************************************************
PARAMETERS lcRequestID, lcXMLFileName, ClientId

STORE '' TO lnMajPos, lnMajLen, lnClrPos, lnClrLen, lnSclPos, lnSclLen
PRIVATE lcRplnshmntFgr, lcRplnshmntTmp, lcStrucTmp, lcStyDyeTmp, lcBinLoc

IF TYPE('lcXMLFileName') = 'C'
  *- If Report is called from Request Builder
  TRY  
    *-------------------------Part 1
    PRIVATE loAgent
    loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  
    PRIVATE loProgress
    loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
    loProgress.Percent = 0
    loProgress.DESCRIPTION = "Opening Data Files..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  
    LOCAL loEnvironment
    loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
    loEnvironment.ClientId = ClientId
    LOCAL lcCurrentProcedure
    lcCurrentProcedure = loEnvironment.Aria40SharedPath
    loEnvironment.ConnectionsRefresh()
  
    LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
    lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
    lcClientRoot = STRTRAN(ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath)), UPPER("SQLDictionary\"), "", -1, 1, 1)
    lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
    
    DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
    oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  
    oAriaEnvironment.REPORT.gcAct_Appl = 'IC'
  
    IF LEFT(gcDevice, 7) = "PRINTER"
      oAriaEnvironment.gcDevice = "PRINTER"
    ELSE
      oAriaEnvironment.gcDevice = "FILE"
    ENDIF
    oAriaEnvironment.Report.cCROrientation = 'L'
    
    *-------------------------Part 2
    =lfGetSegmentInfo()
    
    oAriaEnvironment.gcDevice = 'FILE'
    oAriaEnvironment.ActiveModuleID = 'IC'
  
    lcRplnshmntFgr = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp file to hold replenishment figure with 8 sizes per line
    lcRplnshmntTmp = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp file to hold replenishment figure with 14 sizes per line
    lcStrucTmp     = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp fiel to hold style major structure 
    lcTmpIssue     = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp file to hold replenishment figure that will be issued
    lcStyDyeTmp    = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp file to hold StyDye
    lcBinLoc       = oAriaEnvironment.CURSORS.GetCursorTempName() && Temp file to hold Bin Location Stock
    
    =lfCreateTemp()
    IF !lfCollectData()
      RETURN 
    ENDIF
    
    *-------------------------Part 3
    loProgress.Percent = 0.9
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  
    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    IF loProxy.GetRequest(lcRequestID, ClientId).STATUS = 3    
      oAriaEnvironment.REPORT.OGLastForm = 'RPLNSHTR'&&lcRpForm
      oAriaEnvironment.REPORT.lcLogoPath = ""
      lfAdjustCRSettings()
      
      *-Create and Issue Inter Location PO
      IF llCreatePO
        lfCreateInterLocationPO()
      ENDIF
      
      IF USED(lcRplnshmntFgr)
        USE IN (lcRplnshmntFgr)
      ENDIF
      IF USED(lcRplnshmntTmp)
        USE IN (lcRplnshmntTmp)
      ENDIF 

      *-Printing Report
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)    
  
      loProgress.Percent = 1.0
      loProgress.DESCRIPTION = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
    ENDIF
  CATCH TO ex
    IF TYPE('lcXMLFileName') = 'C'
      loAgent.UpdateRequestStatus(lcRequestID, 6, ex.Message, lcClientID)
    ENDIF  
  ENDTRY
ELSE   
  *- If Report is called from Aria4xp 
  *-------------------------Part 1
  loogscroll.cCROrientation = 'L'
    
  *-------------------------Part 2
  =lfGetSegmentInfo()
  lcRplnshmntFgr = gfTempName() && Temp file to hold replenishment figure with 8 sizes per line
  lcRplnshmntTmp = gfTempName() && Temp file to hold replenishment figure with 14 sizes per line
  lcStrucTmp     = gfTempName() && Temp file to hold style major structure 
  lcTmpIssue     = gfTempName() && Temp file to hold replenishment figure that will be issued
  lcStyDyeTmp    = gfTempName() && Temp file to hold StyDye
  lcBinLoc       = gfTempName() && Temp file to hold Bin Location Stock
    
  =lfCreateTemp()
  IF !lfCollectData()
    RETURN 
  ENDIF
  
  *-------------------------Part 3
  loogscroll.lcOGLastForm = 'RPLNSHTR'&&lcRpForm
  loogscroll.lcLogoPath = ""
  =lfAdjustCRSettings()
  
  *-Create and Issue Inter Location PO  
  IF llCreatePO
    lfCreateInterLocationPO()
  ENDIF
  
  IF USED(lcRplnshmntFgr)
    USE IN (lcRplnshmntFgr)
  ENDIF
  IF USED(lcRplnshmntTmp)
    USE IN (lcRplnshmntTmp)
  ENDIF 
  
  DO gfdispre WITH EVALUATE('lcRpForm')
ENDIF
*:***************************************************************************************************************************
*														 END of Code.
*:***************************************************************************************************************************

*!**************************************************************************
*! Name      : lfvWareHos
*! Developer : Saber A.Razek (SAB)
*! Date      : 09/29/2011
*! Purpose   : Validate Warehouse Code (Only this location Option)
*!**************************************************************************
FUNCTION lfvWareHos
PRIVATE lcWareCode , lcTag
lcWareCode = "lcRpSrcWareHouse"
lcTag      = ORDER('WAREHOUS')
IF !EMPTY(&lcWareCode)
  IF SEEK(&lcWareCode.,'WAREHOUS')
    &lcWareCode = WAREHOUS.cWareCode
  ELSE
    &lcWareCode = gfBrowWare(.T.)
  ENDIF
ENDIF  
SET ORDER TO &lcTag IN WAREHOUS
ENDFUNC

*!**************************************************************************
*! Name       : lfwOldWare
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
FUNCTION lfwOldWare
lcOldWare = EVALUATE("lcRpSrcWareHouse")
ENDFUNC

*! Name       : lfGetSegmentInfo
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Procedure to Get Style Segment Information
*! Parameters : 
*!*************************************************************
PROCEDURE lfGetSegmentInfo

DIMENSION laMajSegs[1,1]
*! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
*= gfItemMask(@laMajSegs)
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laMajSegs)
ELSE
  =gfItemMask(@laMajSegs)
ENDIF
*! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
lnMajLen = gfItemMask('SM')  && No. of major segments.
*-- Loop Around Non Major elements.
STORE '' TO lcNonMajPi 
FOR lnI =  1 TO ALEN(laMajSegs,1)
  DO CASE
    CASE laMajSegs[lnI ,1] = 'F'
      lnMajPos = laMajSegs[lnI ,4]
      lnMajLen = LEN(laMajSegs[lnI ,3])
    CASE laMajSegs[lnI ,1] = 'C'
      lnClrPos = laMajSegs[lnI ,4]
      lnClrLen = LEN(laMajSegs[lnI ,3])
    CASE laMajSegs[lnI ,1] = 'S'
      lnSclPos = laMajSegs[lnI ,4]
      lnSclLen = LEN(laMajSegs[lnI ,3])
  ENDCASE
ENDFOR

ENDPROC

*!*************************************************************
*! Name       : lfGetRplnshmntFgr
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Procedure to Get Actual Replenishment Figure
*! Parameters : 
*!*************************************************************
PROCEDURE lfGetRplnshmntFgr

LOCAL lcSQLStatement


lnPosStore = ASCAN(laOgFXFlt,"STORE")
lcCursorStore = ''
llSelectStore = .F.
IF lnPosStore > 0
  lnPosStore = ASUBSCRIPT(laOgFxFlt,lnPosStore,1)
  lcCursorStore= laOgFxFlt[lnPosStore,6]
  IF !EMPTY(lcCursorStore)
    SELECT(lcCursorStore)
    LOCATE
    IF !EOF()
      llSelectStore = .T.
    ENDIF      
  ENDIF  
ENDIF

*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start]
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]                 
*!*	lcSQLStatement = "SELECT h.ostkhkey, h.account, h.store, h.cstymajor, h.ccncssngrd, c.Concession_Warehouse cwarecode, c.Concession_Optimum_Stock optstkhold, "+;
*!*	                 "       d.style, d.qty1, d.qty2, d.qty3, d.qty4, d.qty5, d.qty6, d.qty7, d.qty8 "+;
*!*	                 "FROM   OptStckH h, OptStckD d, CONCESSION_SETUP_T c "+;
*!*	                 "WHERE  h.ostkhkey = d.ostkhkey AND h.account = c.Account AND h.store = c.Store "+;
*!*	                 "AND h.cstymajor IN(SELECT cItmMajor FROM BOMHEADR WHERE cInvType ='0001' AND cCstShtTyp='I') "+;
*!*	                 " AND c.Concession_Warehouse <> '' AND c.Concession_Warehouse <> '" + lcRpSrcWareHouse + "'"
IF llSelectStore
   lcSQLStatement = "SELECT h.ostkhkey, h.account, h.store, h.cstymajor, h.ccncssngrd, c.Concession_Warehouse cwarecode, c.Concession_Optimum_Stock optstkhold, "+;
                   "       d.style, d.qty1, d.qty2, d.qty3, d.qty4, d.qty5, d.qty6, d.qty7, d.qty8 ,d.ostkdkey "+;
                   "FROM   OptStckH h, OptStckD d, CONCESSION_SETUP_T c "+;
                   "WHERE  h.ostkhkey = d.ostkhkey AND h.account = c.Account AND h.store = c.Store "+;
                   "AND h.cstymajor IN(SELECT cItmMajor FROM BOMHEADR WHERE cInvType ='0001' AND cCstShtTyp='I') "+;
                   " AND c.Concession_Warehouse <> '' AND c.Concession_Warehouse <> '" + lcRpSrcWareHouse + "'"
ELSE
   lcSQLStatement = "SELECT h.ostkhkey, h.account, h.store, h.cstymajor, h.ccncssngrd, c.Concession_Warehouse cwarecode, c.Concession_Optimum_Stock optstkhold, "+;
                   "       d.style, d.qty1, d.qty2, d.qty3, d.qty4, d.qty5, d.qty6, d.qty7, d.qty8 ,d.ostkdkey "+;
                   "FROM   OptStckH h, OptStckD d, CONCESSION_SETUP_T c "+;
                   "WHERE  h.ostkhkey = d.ostkhkey AND h.account = c.Account AND h.ccncssngrd = c.Concession_Grade "+;
                   "AND h.cstymajor IN(SELECT cItmMajor FROM BOMHEADR WHERE cInvType ='0001' AND cCstShtTyp='I') "+;
                   " AND c.Concession_Warehouse <> '' AND c.Concession_Warehouse <> '" + lcRpSrcWareHouse + "' AND  h.store = ''"
ENDIF                 
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]                 
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]
LOCAL lnConnHandler
*! B609739,1 SAB 11/22/2011 Fix Execlusive connection problem [Start]
*lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSQLStatement,'','RplnshmntFgr','',;
                                                      oAriaApplication.ActiveCompanyConStr,3,"",SET("Datasession"), .T., @lnConnHandler)
lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSQLStatement,'','RplnshmntFgr','',;
                                                      oAriaApplication.ActiveCompanyConStr,3,"",SET("Datasession"), .F., @lnConnHandler)
*! B609739,1 SAB 11/22/2011 Fix Execlusive connection problem [End]
IF lnResult > 0
  IF !llSelectStore
    lcSQLStatement = "SELECT DISTINCT h.account, h.store "+;
                    "FROM   OptStckH h, CONCESSION_SETUP_T c "+;
                     "WHERE  h.account = c.Account AND h.ccncssngrd = c.Concession_Grade "+;
                     "AND h.cstymajor IN(SELECT cItmMajor FROM BOMHEADR WHERE cInvType ='0001' AND cCstShtTyp='I') "+;
                    " AND c.Concession_Warehouse <> '' AND c.Concession_Warehouse <> '" + lcRpSrcWareHouse + "' AND  h.store <> ''"
    lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSQLStatement,'','AccountStores','',;
                                                      oAriaApplication.ActiveCompanyConStr,3,"",SET("Datasession"), .F., @lnConnHandler)
    IF lnResult > 0
      SELECT AccountStores
      LOCATE
      SCAN 
        SELECT * From RplnshmntFgr WHERE  account = AccountStores.account INTO CURSOR 'TempRecord' READWRITE
        SELECT 'TempRecord'
        REPLACE ALL STORE WITH AccountStores.Store 
        LOCATE 
        SCAN 
          SCATTER MEMO MEMVAR 
          INSERT INTO RplnshmntFgr FROM MEMVAR 
        ENDSCAN
        SELECT AccountStores
      ENDSCAN 
      SELECT RplnshmntFgr 
      LOCATE
      DELETE ALL FOR EMPTY(Store)
      
    ENDIF                
  ENDIF

  SELECT RplnshmntFgr  
  SET FILTER TO &lcRpExp.
  SCAN
    SCATTER MEMVAR memo
    *! B610981,1 MMT 04/19/2015 Fix bug of duplicating records in report[T20150402.0010][Start]
    IF SEEK(m.ACCOUNT+m.STORE+m.STYLE,lcRplnshmntFgr,'RPLNSHMNT')
      LOOP 
    ENDIF
    *! B610981,1 MMT 04/19/2015 Fix bug of duplicating records in report[T20150402.0010][End]
    SELECT (lcRplnshmntFgr)
    APPEND BLANK
    
    *- Get Division and Purchase Group
    LOCAL lnAlias
    lnAlias = SELECT()
    IF USED('StyleInfo')
      USE IN StyleInfo
    ENDIF
      
    USE oAriaApplication.DataDir+'Style' IN 0 SHARED AGAIN ALIAS 'StyleInfo'
    SELECT StyleInfo
    SET ORDER TO STYLE   && STYLE    
        
    IF SEEK(ALLTRIM(m.Style), 'StyleInfo')
      m.cdivision = StyleInfo.cdivision
      m.cpurcode  = StyleInfo.cpurcode
    ELSE
      m.cdivision = ''
      m.cpurcode  = ''
    ENDIF
        
    USE IN StyleInfo
    SELECT (lnAlias)
    
    GATHER MEMVAR MEMO
  ENDSCAN
  SET FILTER TO
ELSE
  RETURN
ENDIF

IF RECCOUNT(lcRplnshmntFgr) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
  *=gfModalGen('TRM00052B40011','ALERT')
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('TRM00052B40011','ALERT')
  ENDIF
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
  RETURN .F.
ENDIF

IF !USED('StyDye')
 =gfOpenTable('StyDye','STYDYEW','SH')&&, 'WHStck')
ENDIF

SELECT * FROM StyDye WHERE Style in (SELECT Style FROM RplnshmntFgr) AND cwarecode == lcRpSrcWareHouse INTO CURSOR (lcStyDyeTmp) READWRITE 
SELECT (lcStyDyeTmp)
INDEX on CWARECODE+STYLE+DYELOT TAG STYDYEW
SET ORDER TO STYDYEW

*! E303305,1 SAB 11/27/2012 Add % Uplift to Replenishment Repport [Start]
LOCAL lnIncrsFctr
lnIncrsFctr = IIF(lnRpUplift = 0, 1, 1 + lnRpUplift / 100)
*! E303305,1 SAB 11/27/2012 Add % Uplift to Replenishment Repport [End]

lfFillBinLoc()
LOCAL lcStyle, lnOstkHKey
SELECT (lcRplnshmntFgr)
SET ORDER TO OSTKHKEY
LOCATE
SCAN
  lnOstkHKey        = &lcRplnshmntFgr..ostkhkey
  lcStyle           = &lcRplnshmntFgr..style
  lcCncssnWarehouse = &lcRplnshmntFgr..cwarecode
  IF gfSeek(PADR(lcCncssnWarehouse, 6)+PADR(lcStyle, 19), "STYDYE", "STYDYEW")
    *! E303305,1 SAB 11/27/2012 Add % Uplift to Replenishment Repport [Start]
    *- Add the % Uplift to the replenishment quantity
    REPLACE qty1 WITH CEILING(&lcRplnshmntFgr..qty1 * lnIncrsFctr),;
            qty2 WITH CEILING(&lcRplnshmntFgr..qty2 * lnIncrsFctr),;
            qty3 WITH CEILING(&lcRplnshmntFgr..qty3 * lnIncrsFctr),;
            qty4 WITH CEILING(&lcRplnshmntFgr..qty4 * lnIncrsFctr),;
            qty5 WITH CEILING(&lcRplnshmntFgr..qty5 * lnIncrsFctr),;
            qty6 WITH CEILING(&lcRplnshmntFgr..qty6 * lnIncrsFctr),;
            qty7 WITH CEILING(&lcRplnshmntFgr..qty7 * lnIncrsFctr),;
            qty8 WITH CEILING(&lcRplnshmntFgr..qty8 * lnIncrsFctr) IN (lcRplnshmntFgr)
    *! E303305,1 SAB 11/27/2012 Add % Uplift to Replenishment Repport [End]
    
    *- Get the primary replenishment figure
    *! C201533,1 SAB 10/31/2012 Change Replenishment Calculation to Include WIP [Start]
    *REPLACE qty1 WITH MAX(&lcRplnshmntFgr..qty1 - STYDYE.stk1, 0),;
            qty2 WITH MAX(&lcRplnshmntFgr..qty2 - STYDYE.stk2, 0),;
            qty3 WITH MAX(&lcRplnshmntFgr..qty3 - STYDYE.stk3, 0),;
            qty4 WITH MAX(&lcRplnshmntFgr..qty4 - STYDYE.stk4, 0),;
            qty5 WITH MAX(&lcRplnshmntFgr..qty5 - STYDYE.stk5, 0),;
            qty6 WITH MAX(&lcRplnshmntFgr..qty6 - STYDYE.stk6, 0),;
            qty7 WITH MAX(&lcRplnshmntFgr..qty7 - STYDYE.stk7, 0),;
            qty8 WITH MAX(&lcRplnshmntFgr..qty8 - STYDYE.stk8, 0) IN (lcRplnshmntFgr)
    *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][Start]       
*!*	    REPLACE qty1 WITH MAX(&lcRplnshmntFgr..qty1 - (STYDYE.stk1 + STYDYE.WIP1), 0),;
*!*	            qty2 WITH MAX(&lcRplnshmntFgr..qty2 - (STYDYE.stk2 + STYDYE.WIP2), 0),;
*!*	            qty3 WITH MAX(&lcRplnshmntFgr..qty3 - (STYDYE.stk3 + STYDYE.WIP3), 0),;
*!*	            qty4 WITH MAX(&lcRplnshmntFgr..qty4 - (STYDYE.stk4 + STYDYE.WIP4), 0),;
*!*	            qty5 WITH MAX(&lcRplnshmntFgr..qty5 - (STYDYE.stk5 + STYDYE.WIP5), 0),;
*!*	            qty6 WITH MAX(&lcRplnshmntFgr..qty6 - (STYDYE.stk6 + STYDYE.WIP6), 0),;
*!*	            qty7 WITH MAX(&lcRplnshmntFgr..qty7 - (STYDYE.stk7 + STYDYE.WIP7), 0),;
*!*	            qty8 WITH MAX(&lcRplnshmntFgr..qty8 - (STYDYE.stk8 + STYDYE.WIP8), 0) IN (lcRplnshmntFgr)
    *! B610586,2 MMT 11/19/2013 Replinshment Report calculates avialable QTY incorrectly in Destination store[Start]
*!*	    REPLACE qty1 WITH MAX(&lcRplnshmntFgr..qty1 - STYDYE.stk1, 0),;
*!*	            qty2 WITH MAX(&lcRplnshmntFgr..qty2 - STYDYE.stk2, 0),;
*!*	            qty3 WITH MAX(&lcRplnshmntFgr..qty3 - STYDYE.stk3, 0),;
*!*	            qty4 WITH MAX(&lcRplnshmntFgr..qty4 - STYDYE.stk4, 0),;
*!*	            qty5 WITH MAX(&lcRplnshmntFgr..qty5 - STYDYE.stk5, 0),;
*!*	            qty6 WITH MAX(&lcRplnshmntFgr..qty6 - STYDYE.stk6, 0),;
*!*	            qty7 WITH MAX(&lcRplnshmntFgr..qty7 - STYDYE.stk7, 0),;
*!*	            qty8 WITH MAX(&lcRplnshmntFgr..qty8 - STYDYE.stk8, 0) IN (lcRplnshmntFgr)
    REPLACE qty1 WITH MAX(&lcRplnshmntFgr..qty1 - (STYDYE.stk1 + STYDYE.WIP1), 0),;
            qty2 WITH MAX(&lcRplnshmntFgr..qty2 - (STYDYE.stk2 + STYDYE.WIP2), 0),;
            qty3 WITH MAX(&lcRplnshmntFgr..qty3 - (STYDYE.stk3 + STYDYE.WIP3), 0),;
            qty4 WITH MAX(&lcRplnshmntFgr..qty4 - (STYDYE.stk4 + STYDYE.WIP4), 0),;
            qty5 WITH MAX(&lcRplnshmntFgr..qty5 - (STYDYE.stk5 + STYDYE.WIP5), 0),;
            qty6 WITH MAX(&lcRplnshmntFgr..qty6 - (STYDYE.stk6 + STYDYE.WIP6), 0),;
            qty7 WITH MAX(&lcRplnshmntFgr..qty7 - (STYDYE.stk7 + STYDYE.WIP7), 0),;
            qty8 WITH MAX(&lcRplnshmntFgr..qty8 - (STYDYE.stk8 + STYDYE.WIP8), 0) IN (lcRplnshmntFgr)
        *! B610586,2 MMT 11/19/2013 Replinshment Report calculates avialable QTY incorrectly in Destination store[END]        
    *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][End]            
    *! C201533,1 SAB 10/31/2012 Change Replenishment Calculation to Include WIP [End]    
  ENDIF
  IF Seek(PADR(lcRpSrcWareHouse, 6)+PADR(lcStyle, 19), lcStyDyeTmp, "STYDYEW") &&"STYDYE", "STYDYEW")
    *- Get the final replenishment figure
    *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][Start]       
*!*	    REPLACE qty1 WITH MAX(MIN(&lcRplnshmntFgr..qty1, &lcStyDyeTmp..stk1),0),;
*!*	            qty2 WITH MAX(MIN(&lcRplnshmntFgr..qty2, &lcStyDyeTmp..stk2),0),;
*!*	            qty3 WITH MAX(MIN(&lcRplnshmntFgr..qty3, &lcStyDyeTmp..stk3),0),;
*!*	            qty4 WITH MAX(MIN(&lcRplnshmntFgr..qty4, &lcStyDyeTmp..stk4),0),;
*!*	            qty5 WITH MAX(MIN(&lcRplnshmntFgr..qty5, &lcStyDyeTmp..stk5),0),;
*!*	            qty6 WITH MAX(MIN(&lcRplnshmntFgr..qty6, &lcStyDyeTmp..stk6),0),;
*!*	            qty7 WITH MAX(MIN(&lcRplnshmntFgr..qty7, &lcStyDyeTmp..stk7),0),;
*!*	            qty8 WITH MAX(MIN(&lcRplnshmntFgr..qty8, &lcStyDyeTmp..stk8),0) IN (lcRplnshmntFgr)
    REPLACE qty1 WITH MAX(MIN(&lcRplnshmntFgr..qty1, &lcStyDyeTmp..stk1 - &lcStyDyeTmp..Ord1),0),;
            qty2 WITH MAX(MIN(&lcRplnshmntFgr..qty2, &lcStyDyeTmp..stk2 - &lcStyDyeTmp..Ord2),0),;
            qty3 WITH MAX(MIN(&lcRplnshmntFgr..qty3, &lcStyDyeTmp..stk3 - &lcStyDyeTmp..Ord3),0),;
            qty4 WITH MAX(MIN(&lcRplnshmntFgr..qty4, &lcStyDyeTmp..stk4 - &lcStyDyeTmp..Ord4),0),;
            qty5 WITH MAX(MIN(&lcRplnshmntFgr..qty5, &lcStyDyeTmp..stk5 - &lcStyDyeTmp..Ord5),0),;
            qty6 WITH MAX(MIN(&lcRplnshmntFgr..qty6, &lcStyDyeTmp..stk6 - &lcStyDyeTmp..Ord6),0),;
            qty7 WITH MAX(MIN(&lcRplnshmntFgr..qty7, &lcStyDyeTmp..stk7 - &lcStyDyeTmp..Ord7),0),;
            qty8 WITH MAX(MIN(&lcRplnshmntFgr..qty8, &lcStyDyeTmp..stk8 - &lcStyDyeTmp..Ord8),0) IN (lcRplnshmntFgr)
    *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][End]               
    REPLACE stk1 WITH &lcStyDyeTmp..stk1 - &lcRplnshmntFgr..qty1,;
            stk2 WITH &lcStyDyeTmp..stk2 - &lcRplnshmntFgr..qty2,;
            stk3 WITH &lcStyDyeTmp..stk3 - &lcRplnshmntFgr..qty3,;
            stk4 WITH &lcStyDyeTmp..stk4 - &lcRplnshmntFgr..qty4,;
            stk5 WITH &lcStyDyeTmp..stk5 - &lcRplnshmntFgr..qty5,;
            stk6 WITH &lcStyDyeTmp..stk6 - &lcRplnshmntFgr..qty6,;
            stk7 WITH &lcStyDyeTmp..stk7 - &lcRplnshmntFgr..qty7,;
            stk8 WITH &lcStyDyeTmp..stk8 - &lcRplnshmntFgr..qty8 IN (lcStyDyeTmp)
            
  ELSE 
    REPLACE qty1 WITH 0,;
            qty2 WITH 0,;
            qty3 WITH 0,;
            qty4 WITH 0,;
            qty5 WITH 0,;
            qty6 WITH 0,;
            qty7 WITH 0,;
            qty8 WITH 0 IN (lcRplnshmntFgr)
  ENDIF
  
  IF Seek(PADR(lcStyle, 19), lcBinLoc, "STYLE")
    REPLACE qty1 WITH MAX(MIN(&lcRplnshmntFgr..qty1, &lcBinLoc..qty1),0),;
            qty2 WITH MAX(MIN(&lcRplnshmntFgr..qty2, &lcBinLoc..qty2),0),;
            qty3 WITH MAX(MIN(&lcRplnshmntFgr..qty3, &lcBinLoc..qty3),0),;
            qty4 WITH MAX(MIN(&lcRplnshmntFgr..qty4, &lcBinLoc..qty4),0),;
            qty5 WITH MAX(MIN(&lcRplnshmntFgr..qty5, &lcBinLoc..qty5),0),;
            qty6 WITH MAX(MIN(&lcRplnshmntFgr..qty6, &lcBinLoc..qty6),0),;
            qty7 WITH MAX(MIN(&lcRplnshmntFgr..qty7, &lcBinLoc..qty7),0),;
            qty8 WITH MAX(MIN(&lcRplnshmntFgr..qty8, &lcBinLoc..qty8),0) IN (lcRplnshmntFgr)
    REPLACE qty1 WITH &lcBinLoc..qty1 - &lcRplnshmntFgr..qty1,;
            qty2 WITH &lcBinLoc..qty2 - &lcRplnshmntFgr..qty2,;
            qty3 WITH &lcBinLoc..qty3 - &lcRplnshmntFgr..qty3,;
            qty4 WITH &lcBinLoc..qty4 - &lcRplnshmntFgr..qty4,;
            qty5 WITH &lcBinLoc..qty5 - &lcRplnshmntFgr..qty5,;
            qty6 WITH &lcBinLoc..qty6 - &lcRplnshmntFgr..qty6,;
            qty7 WITH &lcBinLoc..qty7 - &lcRplnshmntFgr..qty7,;
            qty8 WITH &lcBinLoc..qty8 - &lcRplnshmntFgr..qty8 IN (lcBinLoc)
  ELSE 
    REPLACE qty1 WITH 0,;
            qty2 WITH 0,;
            qty3 WITH 0,;
            qty4 WITH 0,;
            qty5 WITH 0,;
            qty6 WITH 0,;
            qty7 WITH 0,;
            qty8 WITH 0 IN (lcRplnshmntFgr)
  ENDIF   
ENDSCAN

*! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [Start]
*- These lines commented and the comment should be removed if the user asked to hide zero lines

IF llRpSubZero
  SELECT (lcRplnshmntFgr)
  SUM qty1 + qty2 + qty3 + qty4 + qty5 + qty6 + qty7 + qty8 TO lnSumQty
  IF lnSumQty <= 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
    *=gfModalGen('TRM00052B40011','ALERT')
    IF TYPE('lcXMLFileName') <> 'C'
      =gfModalGen('TRM00052B40011','ALERT')
    ENDIF
    *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
    RETURN .F.
  ENDIF
ENDIF
*! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [End]



ENDPROC

*!*************************************************************
*! Name       : lfCollectData
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Procedure to make replenishment table have 14 size per line
*! Parameters : 
*!*************************************************************
PROCEDURE lfCollectData
*- Call Function to get Data from database (8 sizes per line)
IF !lfGetRplnshmntFgr()
  RETURN .F.
ENDIF

*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
IF TYPE('lcXMLFileName') <> 'C'
  loProssBar = CREATEOBJECT('ariaprogressbar')
  loProssBar.lblFirstLabel.Caption = "Collecting Data..."
  loProssBar.TotalProgress = RECCOUNT(lcRplnshmntFgr)
  loProssBar.AutoCenter = .T.
  loProssBar.Show()
  lnCurrRecNm = 0 
  LOOGscroLL.PARENT.VISIBLE = .f.
ENDIF  
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]

*- Reformate the data to be 14 size per line
LOCAL lcStyle, lnOstkHKey, lnRecNo
LOCAL llExtSz
llExtSz = gfGetMemVar('M_USEEXSSC')
SELECT (lcRplnshmntFgr)
*! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][Start]
lcPartStyClr = SPACE(19)
*! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][End]
*! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][Start]
lcStoreVar = SPACE(8)
lcStoreVal = SPACE(8)
*! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][End]
*! B610017,1 SAB 07/19/2012 Some records not appear in Report[Start]
lcOStkHKey = ''
*! B610017,1 SAB 07/19/2012 Some records not appear in Report[End]
SCAN   
  *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][Start]
  *! B610017,1 SAB 07/19/2012 Some records not appear in Report[Start]
  *IF lcPartStyClr = SUBSTR(Style ,1,lnClrPos+lnClrLen-1)
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][Start]
  *IF lcPartStyClr = SUBSTR(Style ,1,lnClrPos+lnClrLen-1) AND lcOStkHKey = oStkHKey  
  IF lcPartStyClr = SUBSTR(Style ,1,lnClrPos+lnClrLen-1) AND lcOStkHKey = oStkHKey AND lcStoreVar  = Store 
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][End]
  *! B610017,1 SAB 07/19/2012 Some records not appear in Report[End]
    LOOP
  ENDIF
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][Start]
  lcStoreVar  = Store 
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][End]  
  lcPartStyClr = SUBSTR(Style ,1,lnClrPos+lnClrLen-1)
  *! B610017,1 SAB 07/19/2012 Some records not appear in Report[Start]
  lcOStkHKey = oStkHKey
  *! B610017,1 SAB 07/19/2012 Some records not appear in Report[End]
  FOR lnCz = 1 TO 14
    lcCz = ALLTRIM(STR(lnCz))
    STORE '' TO m.size&lcCz.
    STORE 0 TO m.qty&lcCz.
  ENDFOR   
  llRecInsrt = .T.
  *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][End]
  
  lnRecNo = RECNO(lcRplnshmntFgr)
  
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
  IF TYPE('lcXMLFileName') <> 'C'
    lnCurrRecNm = lnCurrRecNm + 1
    loProssBar.CurrentProgress(lnCurrRecNm)
  ENDIF  
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]
  
  SCATTER MEMVAR memo                &&ostkhkey, account, store, cstymajor, ccncssngrd, cwarecode, style
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
  m.CSTYMAJOR  = SUBSTR(m.style,1,lnMajLen)
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][Start]
  *IF EMPTY(lnOstkHKey) OR !(m.ostkhkey == lnOstkHKey AND m.Style == lcStyle)  
  IF EMPTY(lnOstkHKey) OR !(m.ostkhkey == lnOstkHKey AND m.Style == lcStyle AND lcStoreVal == m.Store)
    lcStoreVal = m.Store
  *! B610920,1 MMT 12/17/2014 Replinshment report does not print some styles when stores selected[T20141212.0001][End]
    lnOstkHKey = m.ostkhkey
    lcStyle = m.style
    *-accname, storename, styledesc, colordesc, optstkhold, crnstkhold, Color
    m.accname    = lfGetAccountName(m.Account)
    m.storename  = lfGetAccountName(m.Account, m.Store, .T.)
    m.styledesc  = lfGetStyleDesc(m.cstymajor)
    m.color      = SUBSTR(m.style, lnClrPos, lnClrLen)
    m.colordesc  = gfCodDes(m.color, 'COLOR')
    *! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start]
    *m.optstkhold = lfGetOptStkHold(m.ostkhkey)
    *! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]
    m.crnstkhold = lfGetCrntStkHold(PADR(m.cstymajor, lnMajLen), m.cwarecode)        
    =lfGetMajorStruc(m.cstymajor)
    
    SELECT (lcStrucTmp)
    SET ORDER TO NUM
    SCAN
      *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][Start]
      IF !llRecInsrt
        FOR lnCz = 1 TO 14
  	      lcCz = ALLTRIM(STR(lnCz))
   	      STORE '' TO m.size&lcCz.
	      STORE 0 TO m.qty&lcCz.
  	    ENDFOR          
      ENDIF
      llRecInsrt = .T.
      *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][End]
      
      lcScale = &lcStrucTmp..Scale
      lcSize  = ALLTRIM(STR(&lcStrucTmp..Size))
      lnSzNum = IIF(&lcStrucTmp..Num%14 = 0, 14, &lcStrucTmp..Num%14)
      lcSzNum = ALLTRIM(STR(lnSzNum))
      
      m.size&lcSzNum. = &lcStrucTmp..Caption
      IF llExtSz
        lcStyle = STUFF(lcStyle, lnSclPos, lnSclLen, lcScale)
      ENDIF
      SELECT (lcRplnshmntFgr)      
      LOCATE FOR &lcRplnshmntFgr..ostkhkey == m.ostkhkey AND &lcRplnshmntFgr..style == lcStyle
      IF FOUND()
        m.qty&lcSzNum.  = &lcRplnshmntFgr..qty&lcSize.
      ELSE
        m.qty&lcSzNum.  = 0
      ENDIF
            
      IF INLIST(lnSzNum, 14, RECCOUNT(lcStrucTmp))
        SELECT (lcRplnshmntTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO        
        *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][Start]
        llRecInsrt = .F.
        *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][End] 
      ENDIF      
      
    ENDSCAN
    *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][Start]
    IF llRecInsrt
      SELECT (lcRplnshmntTmp)
      APPEND BLANK
      GATHER MEMVAR MEMO  
    ENDIF
    *! B609964,1 SAB 06/18/2012 Fix Style Sizes and number of records problem [T20120523.0017][End]
    
    SELECT (lcRplnshmntFgr)    
    GOTO lnRecNo
  ENDIF
ENDSCAN
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
IF TYPE('lcXMLFileName') <> 'C'
  LOOGscroLL.parent.VISIBLE = .t.
ENDIF
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]
ENDPROC

*************************************************************
*! Name      : lfGetMajorStruc
*! Developer : Saber Saber(SAB)
*! Date      : 09/29/2011
*! Purpose   : To Get Style Major Structure
*! Parameters: lcMajor ==> Style Major
*!*************************************************************
PROCEDURE lfGetMajorStruc
LPARAMETERS lcMajor

IF EMPTY(lcMajor)
  RETURN
ENDIF

LOCAL lnAlias, lcScale, lnDimLen
lnAlias = SELECT()

IF USED(lcStrucTmp)
  USE IN (lcStrucTmp)
ENDIF

DIMENSION laStruArr[4, 4]

laStruArr[1  ,1] = 'Num'
laStruArr[1  ,2] = 'N'
laStruArr[1  ,3] = 3
laStruArr[1  ,4] = 0

laStruArr[2  ,1] = 'Scale'
laStruArr[2  ,2] = 'C'
laStruArr[2  ,3] = 3
laStruArr[2  ,4] = 0

laStruArr[3  ,1] = 'Size'
laStruArr[3  ,2] = 'N'
laStruArr[3  ,3] = 2
laStruArr[3  ,4] = 0

laStruArr[4  ,1] = 'Caption'
laStruArr[4  ,2] = 'C'
laStruArr[4  ,3] = 10
laStruArr[4  ,4] = 0

CREATE CURSOR (lcStrucTmp) FROM ARRAY laStruArr
INDEX on Scale+ALLTRIM(STR(Size)) TAG SCALE
INDEX on Num TAG NUM

IF !USED('Style')
  =gfOpenTable('Style','STYLE','SH')      && STYLE
ENDIF
IF !USED('Scale')
  =gfOpenTable('Scale','SCALE','SH')      && TYPE+SCALE+PREPAK
ENDIF

lcMajor = PADR(lcMajor,lnMajlen)
SELECT Style
IF SEEK(lcMajor)
  lcScale = LEFT(Style.Scale, 2)  

  STORE 0 TO lnSize, lnCnt, lnCntAll
  SELECT Scale
  IF SEEK('S'+lcScale)
    SCAN REST WHILE Type+LEFT(Scale, 2) == 'S'+lcScale      
      lnCnt = Scale.cnt
      FOR i = 1 TO lnCnt
        lnSize = lnCntAll + i
        SELECT (lcStrucTmp)
        APPEND BLANK 
        REPLACE Num WITH lnSize, Scale WITH Scale.Scale, Size WITH i, Caption WITH EVALUATE("Scale.Sz"+ALLTRIM(STR(i)))
      ENDFOR
      lnCntAll = lnCntAll + lnCnt
    ENDSCAN
  ENDIF
ELSE
  RETURN
ENDIF
SELECT(lnAlias)

ENDPROC

*!*************************************************************
*! Name       : lfGetAccountName
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Account/Store Name
*! Parameters : lcAccount ==> Account Code
*!              lcStore   ==> Store Code
*!              llIsStore ==> If need to get name of store(.T. in Store case)
*!*************************************************************
FUNCTION lfGetAccountName
LPARAMETERS lcAccount, lcStore, llIsStore
LOCAL lcAccountName, lnAlias

IF EMPTY(lcAccount)
  lcAccountName = ''
ELSE  
  lnAlias = SELECT()
  IF USED('CustomerInfo')
    USE IN CustomerInfo
  ENDIF

  *-12-05
  USE oAriaApplication.DataDir+'Customer' IN 0 SHARED AGAIN ALIAS 'CustomerInfo'
  SELECT CustomerInfo
  SET ORDER TO CUSTOMER   && TYPE+ACCOUNT+STORE
  
  IF SEEK(IIF(llIsStore, 'S', 'M')+IIF(EMPTY(lcStore), lcAccount, PADR(lcAccount, 5)+PADR(lcStore, 8)), 'CustomerInfo')
    lcAccountName = CustomerInfo.STName
  ELSE
    lcAccountName = ''
  ENDIF
  
  USE IN CustomerInfo
  SELECT (lnAlias)
ENDIF

RETURN lcAccountName

ENDFUNC

*!*************************************************************
*! Name       : lfGetStyleDesc
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Style Description
*! Parameters : lcMajor ==> Style Major
*!*************************************************************
FUNCTION lfGetStyleDesc
LPARAMETERS lcMajor
LOCAL lcStyleDesc, lnAlias

IF EMPTY(lcMajor)
  lcStyleDesc = ''
ELSE  
  lnAlias = SELECT()
  IF USED('StyleInfo')
    USE IN StyleInfo
  ENDIF
  
  USE oAriaApplication.DataDir+'Style' IN 0 SHARED AGAIN ALIAS 'StyleInfo'
  SELECT StyleInfo
  SET ORDER TO STYLE   && STYLE

  lcMajor = ALLTRIM(lcMajor)
  IF SEEK(lcMajor, 'StyleInfo')
    lcStyleDesc = StyleInfo.Desc
  ELSE
    lcStyleDesc = ''
  ENDIF
  
  USE IN StyleInfo
  SELECT (lnAlias)
ENDIF

RETURN lcStyleDesc

ENDFUNC

*!*************************************************************
*! Name       : lfGetCrntStkHold
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Current Stock Holding
*! Parameters : lcStyMajor  ==> Style Major
*!              lcWarehouse ==> Concession Warehouse
*!              lnMajLen    ==> Major Length
*!*************************************************************
FUNCTION lfGetCrntStkHold
LPARAMETERS lcStyMajor, lcWarehouse&&, lnMajLen

LOCAL lnAlias, lnCurStckHold
lnAlias = SELECT()

IF !USED('StyDye')
  =gfOpenTable('StyDye','STYDYE','SH')   && STYLE+CWARECODE+DYELOT
ENDIF

SELECT StyDye
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start]
*SUM TotStk FOR PADR(Style, lnMajLen) == lcStyMajor AND PADR(cWareCode, 6) = lcWarehouse TO lnCurStckHold
SUM TotStk FOR PADR(cWareCode, 6) = lcWarehouse TO lnCurStckHold
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]
SELECT (lnAlias)
RETURN lnCurStckHold

ENDFUNC

*!*************************************************************
*! Name       : lfGetOptStkHold
*! Developer  : Saber A.Razek (SAB)
*! Date       : 09/29/2011
*! Purpose    : Function to Get Optimum Stock Holding
*! Parameters : lcOptStckHKey ==> Optimum Stock Header Key
*!*************************************************************
FUNCTION lfGetOptStkHold
LPARAMETERS lnOptStckHKey

LOCAL lnOptStckHold, lcSQLStatement
lcSQLStatement = "SELECT SUM(ISNULL(QTY1,0) + ISNULL(QTY2,0) + ISNULL(QTY3,0) + ISNULL(QTY4,0)+"+;
                            "ISNULL(QTY5,0) + ISNULL(QTY6,0) + ISNULL(QTY7,0) + ISNULL(QTY8,0)) AS TotStck "+;
                            "FROM optStckD WHERE ostkhkey = " + ALLTRIM(STR(lnOptStckHKey))
LOCAL lnConnHandler
*! B609739,1 SAB 11/22/2011 Fix Execlusive connection problem [Start]
*lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSQLStatement,'','OptStckHold','',;
                                                      oAriaApplication.ActiveCompanyConStr,3,"",SET("Datasession"), .T., @lnConnHandler)
lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSQLStatement,'','OptStckHold','',;
                                                      oAriaApplication.ActiveCompanyConStr,3,"",SET("Datasession"), .F., @lnConnHandler)
*! B609739,1 SAB 11/22/2011 Fix Execlusive connection problem [End]
IF lnResult > 0
  SELECT OptStckHold
  LOCATE
  lnOptStckHold = OptStckHold.TotStck
  USE IN OptStckHold
ELSE 
  RETURN 0
ENDIF

RETURN lnOptStckHold 
ENDFUNC 

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saber Saber(SAB)
*! Date      : 09/29/2011
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

IF TYPE('lcXMLFileName') = 'C'
  DIMENSION oAriaEnvironment.REPORT.laCRTables[1]
  DIMENSION oAriaEnvironment.REPORT.laCRParams[6,2]

  oAriaEnvironment.REPORT.cCROrientation='L'

  oAriaEnvironment.REPORT.laCRTables[1] = oAriaApplication.WorkDir +  lcRplnshmntTmp + ".DBF"

  LOCAL lnI 
  lnI  = 0
  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'ReportName'
  oAriaEnvironment.REPORT.laCRParams[lnI ,2] = ""&&'Replenishment Report'

  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'Layout'
  oAriaEnvironment.REPORT.laCRParams[lnI, 2] = ""&&"Account\Store"
  
  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'SortBy'
  oAriaEnvironment.REPORT.laCRParams[lnI, 2] = "Account \ Store"
  
  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'OpTitle'
  oAriaEnvironment.REPORT.laCRParams[lnI, 2] = ""

  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'SysDate'
  oAriaEnvironment.REPORT.laCRParams[lnI, 2] = oAriaApplication.SYSTEMDATE
  
  *! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [Start]
  lnI = lnI + 1
  oAriaEnvironment.REPORT.laCRParams[lnI, 1] = 'SubZero'
  oAriaEnvironment.REPORT.laCRParams[lnI, 2] = llRpSubZero
  *! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [End]
ELSE
  DIMENSION loOgScroll.laCRTables[1]
  DIMENSION loOgScroll.laCRParams[6,2]
  
  loOGScroll.cCROrientation='L'
  
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRplnshmntTmp + ".DBF"
  
  LOCAL lnI 
  lnI  = 0
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'ReportName'
  loOgScroll.laCRParams[lnI ,2] = ""&&'Replenishment Report'
   
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'Layout'
  loOgScroll.laCRParams[lnI, 2] = ""&&"Account\Store"
  
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'SortBy'
  loOgScroll.laCRParams[lnI, 2] = "Account \ Store"
  
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
  loOgScroll.laCRParams[lnI, 2] = ""
  
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'SysDate'
  loOgScroll.laCRParams[lnI, 2] = oAriaApplication.SYSTEMDATE
  
  *! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [Start]
  lnI = lnI + 1
  loOgScroll.laCRParams[lnI, 1] = 'SubZero'
  loOgScroll.laCRParams[lnI, 2] = llRpSubZero
  *! E303094,1 SAB 03/26/2012 Suppress zero quantities in Replenishment report [T20110621.0044] [End]
ENDIF

ENDPROC

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Saber A.Razek (SAB)
*! Date      : 09/29/2011
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
PROCEDURE lfCreateTemp
DIMENSION laStruArr[18, 4]

laStruArr[1  ,1] = 'ostkhkey'
laStruArr[1  ,2] = 'N'
laStruArr[1  ,3] = 7
laStruArr[1  ,4] = 0

laStruArr[2  ,1] = 'account'
laStruArr[2  ,2] = 'C'
laStruArr[2  ,3] = 5
laStruArr[2  ,4] = 0

laStruArr[3  ,1] = 'store'
laStruArr[3  ,2] = 'C'
laStruArr[3  ,3] = 8
laStruArr[3  ,4] = 0

laStruArr[4  ,1] = 'cstymajor'
laStruArr[4  ,2] = 'C'
laStruArr[4  ,3] = 19
laStruArr[4  ,4] = 0

laStruArr[5  ,1] = 'ccncssngrd'
laStruArr[5  ,2] = 'C'
laStruArr[5  ,3] = 6
laStruArr[5  ,4] = 0

laStruArr[6  ,1] = 'cwarecode'
laStruArr[6  ,2] = 'C'
laStruArr[6  ,3] = 6
laStruArr[6  ,4] = 0

laStruArr[7  ,1] = 'style'
laStruArr[7  ,2] = 'C'
laStruArr[7  ,3] = 19
laStruArr[7  ,4] = 0

FOR i = 1 TO 8
  lcI  = ALLTRIM(STR(i))
  lnSz = 7 + i
  laStruArr[lnSz ,1] = 'qty' + lcI
  laStruArr[lnSz ,2] = 'N'
  laStruArr[lnSz ,3] = 6
  laStruArr[lnSz ,4] = 0
ENDFOR

laStruArr[16  ,1] = 'cdivision'
laStruArr[16  ,2] = 'C'
laStruArr[16  ,3] = 6
laStruArr[16  ,4] = 0

laStruArr[17  ,1] = 'cpurcode'
laStruArr[17  ,2] = 'C'
laStruArr[17  ,3] = 6
laStruArr[17  ,4] = 0

*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start]
laStruArr[18  ,1] = 'optstkhold'
laStruArr[18  ,2] = 'N'
laStruArr[18  ,3] = 7
laStruArr[18  ,4] = 0
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]

CREATE CURSOR (lcRplnshmntFgr) FROM ARRAY laStruArr
INDEX on OSTKHKEY TAG OSTKHKEY
INDEX on ACCOUNT+STORE+STYLE TAG RPLNSHMNT

DIMENSION laStruArr2[42, 4]

laStruArr2[1  ,1] = 'ostkhkey'
laStruArr2[1  ,2] = 'N'
laStruArr2[1  ,3] = 7
laStruArr2[1  ,4] = 0

laStruArr2[2  ,1] = 'account'
laStruArr2[2  ,2] = 'C'
laStruArr2[2  ,3] = 5
laStruArr2[2  ,4] = 0

laStruArr2[3  ,1] = 'accname'
laStruArr2[3  ,2] = 'C'
laStruArr2[3  ,3] = 30
laStruArr2[3  ,4] = 0

laStruArr2[4  ,1] = 'store'
laStruArr2[4  ,2] = 'C'
laStruArr2[4  ,3] = 8
laStruArr2[4  ,4] = 0

laStruArr2[5  ,1] = 'storename'
laStruArr2[5  ,2] = 'C'
laStruArr2[5  ,3] = 30
laStruArr2[5  ,4] = 0

laStruArr2[6  ,1] = 'cstymajor'
laStruArr2[6  ,2] = 'C'
laStruArr2[6  ,3] = 19
laStruArr2[6  ,4] = 0

laStruArr2[7  ,1] = 'styledesc'
laStruArr2[7  ,2] = 'C'
laStruArr2[7  ,3] = 20
laStruArr2[7  ,4] = 0

laStruArr2[8  ,1] = 'colordesc'
laStruArr2[8  ,2] = 'C'
laStruArr2[8  ,3] = 30
laStruArr2[8  ,4] = 0

laStruArr2[9  ,1] = 'optstkhold'
laStruArr2[9  ,2] = 'N'
laStruArr2[9  ,3] = 7
laStruArr2[9  ,4] = 0

laStruArr2[10 ,1] = 'crnstkhold'
laStruArr2[10 ,2] = 'N'
laStruArr2[10 ,3] = 7
laStruArr2[10 ,4] = 0

laStruArr2[11 ,1] = 'ccncssngrd'
laStruArr2[11 ,2] = 'C'
laStruArr2[11 ,3] = 6
laStruArr2[11 ,4] = 0

laStruArr2[12 ,1] = 'cwarecode'
laStruArr2[12 ,2] = 'C'
laStruArr2[12 ,3] = 6
laStruArr2[12 ,4] = 0

laStruArr2[13 ,1] = 'style'
laStruArr2[13 ,2] = 'C'
laStruArr2[13 ,3] = 19
laStruArr2[13 ,4] = 0

laStruArr2[14 ,1] = 'Color'
laStruArr2[14 ,2] = 'C'
laStruArr2[14 ,3] = 30
laStruArr2[14 ,4] = 0

FOR i = 1 TO 14
  lcI  = ALLTRIM(STR(i))
  lnSz = 14 + ((i-1)*2)+1
  laStruArr2[lnSz ,1] = 'size' + lcI
  laStruArr2[lnSz ,2] = 'C'
  laStruArr2[lnSz ,3] = 5
  laStruArr2[lnSz ,4] = 0
  
  lnSz = lnSz + 1 
  laStruArr2[lnSz ,1] = 'qty' + lcI
  laStruArr2[lnSz ,2] = 'N'
  laStruArr2[lnSz ,3] = 6
  laStruArr2[lnSz ,4] = 0
ENDFOR

lcWorkFile = lcRplnshmntTmp
=gfCrtTmp(lcWorkFile,@laStruArr2,'ACCOUNT+STORE+STYLE','RPLNSHMNT',.F.)

ENDPROC

*!*************************************************************
*! Name      : lfCreateInterLocationPO
*! Developer : Saber A.Razek (SAB)
*! Date      : 11/28/2011
*! Purpose   : Procedure to Create and Issue Inter Location PO for each Account-Store-Style 
*!*************************************************************
PROCEDURE lfCreateInterLocationPO
LOCAL lnAlias
lnAlias = SELECT()
*! E303108,1 SAB 04/10/2012 Develop issue 24 and 35 in Replenishment Project [T20110621.0044][Start]
*SELECT DISTINCT cWareCode, cDivision, cPurCode, '000000' AS PO FROM (lcRplnshmntFgr) INTO CURSOR RplnshmntGrp READWRITE
*! B609913,1 SAB 05/09/2012 Fix Problem in CCONCSSNGRD Field [T20110621.0044][Start]
*SELECT DISTINCT cWareCode, cDivision, cPurCode, '000000' AS PO FROM (lcRplnshmntFgr) ORDER BY cConcssnGrd INTO CURSOR RplnshmntGrp READWRITE
SELECT DISTINCT cCncssnGrd, cWareCode, cDivision, cPurCode, '000000' AS PO FROM (lcRplnshmntFgr) ORDER BY cCncssnGrd INTO CURSOR RplnshmntGrp READWRITE
*! B609913,1 SAB 05/09/2012 Fix Problem in CCONCSSNGRD Field [T20110621.0044][End]
*! E303108,1 SAB 04/10/2012 Develop issue 24 and 35 in Replenishment Project [T20110621.0044][End]

*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start]
*- Create a progress bar to show current process
LOCAL oPross, lnCurrRec, lnRecCount
oPross = CREATEOBJECT('ariaprogressbar')
SELECT RplnshmntGrp
COUNT TO lnRecCount
LOCATE
lnCurrRec = 0

oPross.lblFirstLabel.Caption = "Creating Inter Location POs"
oPross.TotalProgress = lnRecCount
oPross.AutoCenter = .T.
oPross.Show()
*! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [Start]
*loogscroll.Visible = .F.
loogscroll.Parent.Visible = .F.
*! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [End]
*! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]

*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
IF llRpRmv
  lfRemoveOldStyles(PADR(lcRpSrcWareHouse, 6),lcRplnshmntFgr)
ENDIF
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]


SELECT RplnshmntGrp
SCAN    
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]                                 
*!*	  SELECT Style, SUM(Qty1) Qty1, SUM(Qty2) Qty2, SUM(Qty3) Qty3, SUM(Qty4) Qty4, SUM(Qty5) Qty5, SUM(Qty6) Qty6, ;
*!*	         SUM(Qty7) Qty7, SUM(Qty8) Qty8  ;
*!*	         FROM (lcRplnshmntFgr) ;
*!*	         WHERE cWareCode = RplnshmntGrp.cWareCode AND cDivision = RplnshmntGrp.cDivision ;
*!*	           AND cPurCode = RplnshmntGrp.cPurCode ;
*!*	         GROUP BY Style INTO CURSOR (lcTmpIssue) 
  SELECT Style, SUM(Qty1) Qty1, SUM(Qty2) Qty2, SUM(Qty3) Qty3, SUM(Qty4) Qty4, SUM(Qty5) Qty5, SUM(Qty6) Qty6, ;
         SUM(Qty7) Qty7, SUM(Qty8) Qty8  ;
         FROM (lcRplnshmntFgr) ;
         WHERE cWareCode = RplnshmntGrp.cWareCode AND cDivision = RplnshmntGrp.cDivision ;
           AND cPurCode = RplnshmntGrp.cPurCode ;
         GROUP BY Style INTO CURSOR (lcTmpIssue) READWRITE 
  *! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]                                 
  objReplenishment = CREATEOBJECT('REPLENISHMENT')
  IF objReplenishment.SaveInterLocationPO(PADR(lcRpSrcWareHouse, 6), RplnshmntGrp.cWareCode, lcTmpIssue)        
    *! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [Start]
    *objReplenishment.IssueInterLocationPO()    
    IF llAutoIssue
      objReplenishment.IssueInterLocationPO()
    ENDIF
    *! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [End]
    REPLACE PO WITH objReplenishment.cSavedPO IN RplnshmntGrp
  ENDIF  
  *objReplenishment.RemoveObject('REPLENISHMENT')  
  
  *! C201454,1 SAB 01/15/2015 Fix issue 17 problems [Start] 
  *- Updating progress bar status
  lnCurrRec = lnCurrRec + 1
  oPross.CurrentProgress(lnCurrRec)
  *! C201454,1 SAB 01/15/2015 Fix issue 17 problems [End]

ENDSCAN



*! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [Start]
*loogscroll.Visible = .T.
loogscroll.Parent.Visible = .T.
*! B609831,1 SAB 02/14/2012 Fix Problems in issue# 21 in Project [End]
LOCAL lcFirstPO, lcLastPO
SELECT RplnshmntGrp
LOCATE
lcFirstPO = RplnshmntGrp.PO
IF RECCOUNT() == RECNO()
  *! C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [Start]
  *WAIT WINDOW 'Purchase order: '+lcFirstPO
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Purchase order: '+lcFirstPO)  
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Purchase order: '+lcFirstPO)  
  ENDIF
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
  *! C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [End]
ELSE  
  GOTO BOTTOM 
  lcLastPO = RplnshmntGrp.PO  
  *! C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [Start]
  *WAIT WINDOW 'Purchase order: '+lcFirstPO+' through '+lcLastPO
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Purchase order: '+lcFirstPO+' through '+lcLastPO)
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Purchase order: '+lcFirstPO+' through '+lcLastPO)
  ENDIF
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
  *! C201407,1 SAB 11/22/2011 Fix Issues, 25, 28, 30 and 32 and Regenrate Fix [End]
ENDIF 


SELECT (lnAlias)

ENDPROC

*!**************************************************************************
*! Name      : lfChkWH
*! Developer : Saber A.Razek (SAB)
*! Date      : 09/29/2011
*! Purpose   : Validate choosing a Warehouse
*!**************************************************************************
FUNCTION lfChkWH
LOCAL llValidWH
llValidWH = .F.
IF EMPTY(lcRpSrcWareHouse)
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][Start]
  *gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select a warehouse!')
  IF TYPE('lcXMLFileName') <> 'C'
    gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select a warehouse!')
  ENDIF
  *! C201609,1 SAB 03/30/2014 Modify the Replenishemnt Report to Run from RB [T20140304.0001][End]
  llValidWH = .F.
ELSE
  llValidWH = .T.
ENDIF

RETURN llValidWH
ENDFUNC

*:*************************************************************
*: Name      : lfGetDfltWH
*: Developer : Saber A.Razek (SAB)
*: Date      : 09/29/2011
*: Purpose   : Get default warehouse code
*:*************************************************************
FUNCTION lfGetDfltWH

LOCAL lcAlias, lcDefltWareCode
lcAlias = SELECT()
lcDefltWareCode = ''

IF !USED('WareHous')
 =gfOpenTable('WareHous','WAREHOUS','SH')  && CWARECODE
ENDIF

SELECT WareHous
LOCATE FOR lDefWare = .T. AND !DELETED()
IF FOUND()
  lcDefltWareCode = WareHous.cWareCode
ENDIF

SELECT (lcAlias)
RETURN lcDefltWareCode 

ENDFUNC

*:*************************************************************
*: Name      : lfFillBinLoc
*: Developer : Saber A.Razek (SAB)
*: Date      : 12/22/2011
*: Purpose   : Procedure to Fill Bin Location Stock
*:*************************************************************
PROCEDURE lfFillBinLoc
LOCAL llBULKPICK
llBULKPICK = gfGetMemVar('M_BULKPICK')

IF !USED('Style')
  =gfOpenTable('Style','STYLE','SH')      && STYLE
ENDIF
IF !USED('WHBINLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHBINLOC','WHSTYBIN','SH')
ENDIF
IF !USED('WHSLOC')
  =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOC','SH')
ENDIF

DIMENSION laStruArr[9, 4]
laStruArr[1  ,1] = 'Style'
laStruArr[1  ,2] = 'C'
laStruArr[1  ,3] = 19
laStruArr[1  ,4] = 0
FOR lnI = 1 TO 8
  lcI = ALLTRIM(STR(lnI))
  laStruArr[1+lnI ,1] = 'Qty'+lcI
  laStruArr[1+lnI ,2] = 'N'
  laStruArr[1+lnI ,3] = 7
  laStruArr[1+lnI ,4] = 0
ENDFOR
IF USED(lcBinLoc)
  USE IN (lcBinLoc)
ENDIF
CREATE CURSOR (lcBinLoc) FROM ARRAY laStruArr
INDEX on STYLE TAG STYLE

IF gfSEEK(lcRpSrcWareHouse,'WHBINLOC')
  SELECT WHBINLOC
  SET RELATION TO
  SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
  
  IF llBULKPICK
    SELECT Whbinloc.Style,;
       SUM(Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1, SUM(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
       SUM(Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3, SUM(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
       SUM(Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5, SUM(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
       SUM(Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7, SUM(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8;       
	FROM  Whbinloc, Whsloc;
	WHERE Whsloc.cLocation = Whbinloc.cLocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.Style=SPACE(19);
	GROUP BY Whbinloc.Style;
	INTO CURSOR (lcBinLoc) READWRITE
	INDEX on STYLE TAG STYLE
  ELSE	
	SELECT DISTINCT Style FROM (lcRplnshmntFgr) INTO CURSOR OptStyles
	LOCAL lcSz, lcFlathang, lcPrmClss, lcSecClss, lcRemClss, lnSumQty
	SELECT OptStyles
	SCAN       
      m.Style = OptStyles.Style
      IF gfSeek(m.Style, 'Style', 'STYLE')
        lcFlathang = STYLE.cFlatHang
        FOR lnSz = 1 TO 8
          lcSz = ALLTRIM(STR(lnSz))          
          lcPrmClss  = SUBSTR(STYLE.CPRIMCLSS,lnSz ,1)
          lcSecClss  = SUBSTR(STYLE.CSECCLSS ,lnSz ,1)
          lcRemClss  = SUBSTR(STYLE.CREMCLSS ,lnSz ,1)
          SELECT SUM(Whbinloc.Qty&lcSz. - Whbinloc.Alo&lcSz.) AS QTY&lcSz. FROM  Whbinloc, Whsloc;
	      WHERE Whsloc.cLocation = Whbinloc.cLocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style=SPACE(19);
	        AND Whbinloc.Style = m.Style AND WhsLoc.cFlatHang = lcFlathang AND WhsLoc.cBinClass IN (lcPrmClss, lcSecClss, lcRemClss);
	        INTO ARRAY laResult
	      m.Qty&lcSz. = IIF(ISNULL(laResult[1]),0,laResult[1])
        ENDFOR
      ENDIF
      SELECT (lcBinLoc)
      APPEND BLANK
      GATHER MEMVAR MEMO
	ENDSCAN

  ENDIF
ENDIF

ENDPROC

*:*************************************************************
*: Name      : lfRemoveOldStyles
*: Developer : Saber A.Razek (SAB)
*: Date      : 01/22/2012
*: Purpose   : Procedure to remove optimum stock record that has no stock
*:*************************************************************
PROCEDURE lfRemoveOldStyles
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]
LPARAMETERS lcWareSrcHouse,lcTempCurs
IF !USED('STYDYE_A')
  =gfOpenTable('STYDYE','STYDYEW','SH','STYDYE_A')
ENDIF 
IF !USED('OPTSTCKD_A')
  =gfOpenTable('OPTSTCKD','OPTSTCKD','SH','OPTSTCKD_A')
ENDIF 

SELECT(lcTempCurs)
SCAN FOR !DELETED()
  lcStyleVal= &lcTempCurs..STYLE
  SELECT 'STYDYE_A'
  IF gfSeek(PADR(lcWareSrcHouse, 6) + lcStyleVal,'STYDYE_A','STYDYEW')
    LOCAL llZeroStck
    llZeroStck = .T.
    FOR lnSzNo = 1 TO 8      
      lcSzNo = ALLTRIM(STR(lnSzNo))
      *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][Start]
      *IF (STYDYE_A.stk&lcSzNo. > 0) OR (STYDYE_A.wip&lcSzNo. > 0)
      IF (STYDYE_A.stk&lcSzNo. > 0)
      *B610586,1 MMT 11/13/2013 Replinshment Report calculates avialable QTY incorrectly[T20131107.0005][End]
        llZeroStck = .F.
      ENDIF
    ENDFOR
    IF llZeroStck
      SELECT 'OPTSTCKD_A'
      IF gfSeek(ALLTRIM(STR(&lcTempCurs..ostkhkey)))
        LOCATE FOR STYLE = lcStyleVal
        IF FOUND()
          GFDELETE ()
        ENDIF  
      ENDIF
    ENDIF
  ELSE 
    SELECT 'OPTSTCKD_A'
    IF gfSeek(ALLTRIM(STR(&lcTempCurs..ostkhkey)))
      LOCATE FOR STYLE = lcStyleVal
      IF FOUND()
        GFDELETE ()
      ENDIF  
    ENDIF
  ENDIF
ENDSCAN 
SELECT 'OPTSTCKD_A'
=gfTableUpdate()
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]

ENDPROC

*:*************************************************************
*: Name      : lfProceed
*: Developer : Saber A.Razek (SAB)
*: Date      : 09/29/2011
*: Purpose   : Procedure contain report main code
*:*************************************************************
PROCEDURE lfProceed

ENDPROC

*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Saber A.Razek (SAB)
*: Date      : 09/29/2011
*: Purpose   : Report When
*:*************************************************************
FUNCTION lfwRepWhen
*- Select Default Warehouse in Option Grid
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][Start]                 
*lcRpSrcWareHouse = lfGetDfltWH()
*! B609805,1 MMT 01/26/2012 Fix issue 18 in Project [T20110621.0044][END]                 
ENDFUNC
