***********************************************
* Modifications : 
*C201619,1 TMI 05/22/14 14:38 [Start] 
*tmi* T20140716.0001 [START] I found that calling the gftableupdate at this point makes the program losses the lnAria connection to sqlserver
*******************************************************************************************

PARAMETERS lcRequestID, lcXMLFileName, ClientId

IF TYPE('lcXMLFileName') = 'C'

gcRequestID = lcRequestID
gcClientId = ClientId

  PRIVATE loAgent
  IF TYPE('goRemoteCall') = 'U' && R12
    loAgent = CreateObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  ELSE  
    loAgent =  goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  Endif 

 
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
 
  LOCAL loEnvironment
if type('goRemoteCall')='U'
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
else
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
endif 
  loEnvironment.ClientId = ClientId
 
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
 
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)

  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  lcActiveMod = 'SO'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'SO'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.Report.cCROrientation = 'P' 
ELSE
  loOGScroll.cCROrientation = 'P'
ENDIF
*C201619,1 TMI 05/22/14 14:38 [End  ] 

*C201619,1 TMI 05/22/14 15:30 [Start] 
*!*	IF TYPE('lcXMLFileName') = 'C'
*!*	  lcRpPath
*!*	  lcRpHist
*!*	  lcRPChg
*!*	ENDIF 
*C201619,1 TMI 05/22/14 15:30 [End  ] 

*!*	2012-12-19 -- 
*!*	a)       update Dadd_date and cadd_user fields in new store records 
*!*	b)       if the store is vatable then update the customer vat rate with the system rate from the setups file 
	SET DATE BRITISH
	SET CENTURY ON
	*C201619,1 TMI 05/22/14 20:38 [Start] 
	IF TYPE('lcXMLFileName') <> 'C'
	*C201619,1 TMI 05/22/14 20:38 [End  ] 
	IF  .NOT. llogfltch
		RETURN
	ENDIF
	*C201619,1 TMI 05/22/14 20:39 [Start] 
	ENDIF 
	*C201619,1 TMI 05/22/14 20:39 [End  ] 
	llWhSelect = .F.
	lcWhSel = ''
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	lnWhPos = ASCAN(loogscroll.laogfxflt, "WAREHOUS.CWARECODE")
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
	lnWhPos = ASCAN(laogfxflt, "WAREHOUS.CWARECODE")
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	IF lnWhPos>0
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		lnWhPos = ASUBSCRIPT(loogscroll.laogfxflt, lnWhPos, 1)
		lcWhSel = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnWhPos, 6)), loogscroll.laogfxflt(lnWhPos, 6), '')
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
		lnWhPos = ASUBSCRIPT(laogfxflt, lnWhPos, 1)
		lcWhSel = IIF( .NOT. EMPTY(laogfxflt(lnWhPos, 6)), laogfxflt(lnWhPos, 6), '')
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		IF  .NOT. EMPTY(lcWhSel) .AND. USED(lcWhSel)
			llWhSelect = .T.
		ENDIF
	ENDIF
	IF !llWhSelect
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please Select a Warehouse')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    RETURN
	ENDIF
	llCustSelect = .F.
	lcCustSel = ''
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	lnCustPos = ASCAN(loogscroll.laogfxflt, "CUSTOMER.ACCOUNT")
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
	lnCustPos = ASCAN(laogfxflt, "CUSTOMER.ACCOUNT")
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	IF lnCustPos>0
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		lnCustPos = ASUBSCRIPT(loogscroll.laogfxflt, lnCustPos, 1)
		lcCustSel = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnCustPos, 6)), loogscroll.laogfxflt(lnCustPos, 6), '')
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
		lnCustPos = ASUBSCRIPT(laogfxflt, lnCustPos, 1)
		lcCustSel = IIF( .NOT. EMPTY(laogfxflt(lnCustPos, 6)), laogfxflt(lnCustPos, 6), '')
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		IF  .NOT. EMPTY(lcCustSel) .AND. USED(lcCustSel)
			llCustSelect = .T.
		ENDIF
	ENDIF
	IF !llCustSelect
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please Select a Web Order Account')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    RETURN
	ENDIF
	llPotSelect = .F.
	lcPotSel = ''
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	lnPotPos = ASCAN(loogscroll.laogfxflt, "POTCUST.ACCOUNT")
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
	lnPotPos = ASCAN(laogfxflt, "POTCUST.ACCOUNT")
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	IF lnPotPos>0
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		lnPotPos = ASUBSCRIPT(loogscroll.laogfxflt, lnPotPos, 1)
		lcPotSel = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnPotPos, 6)), loogscroll.laogfxflt(lnPotPos, 6), '')
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ELSE
		lnPotPos = ASUBSCRIPT(laogfxflt, lnPotPos, 1)
		lcPotSel = IIF( .NOT. EMPTY(laogfxflt(lnPotPos, 6)), laogfxflt(lnPotPos, 6), '')
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
		IF  .NOT. EMPTY(lcPotSel) .AND. USED(lcPotSel)
			llPotSelect = .T.
		ENDIF
	ENDIF
	IF !llPotSelect
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please Select a Potential Web Account')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    RETURN
	ENDIF
	SELECT (lcWhSel)
    COUNT FOR  .NOT. DELETED() TO lnwarcnt
    IF lnwarcnt>1
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Warehouse')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       RETURN
    ENDIF
	GO TOP
	SELECT (lcCustSel)
    COUNT FOR  .NOT. DELETED() TO lncustcnt
    IF lncustcnt>1
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Web Order Account')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       RETURN
    ENDIF
	GO TOP
	SELECT (lcPotSel)
    COUNT FOR  .NOT. DELETED() TO lnPottcnt
    IF lnPottcnt>1
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Potential Customer Account')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
       RETURN
    ENDIF
	GO TOP
	IF EMPTY(lcrppath) .OR. ( .NOT. DIRECTORY(lcrppath))
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Invalid Import Directory")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN
	ENDIF
	lcrppath = ADDBS(ALLTRIM(lcrppath))
	IF EMPTY(lcrpHist) .OR. ( .NOT. DIRECTORY(lcrpHist))
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Invalid History Directory")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN
	ENDIF
	lcrpHist = ADDBS(ALLTRIM(lcrpHist))
	if lcrpHist = lcrppath
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "You must select different Directories")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN
	ENDIF
	IF EMPTY(lcRpChg)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Please Select an Order Charge Code")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN
	ENDIF
	lctemphead = gftempname()
	lctempline = gftempname()
	lctempCust = gftempname()
	lcCustfile = gftempname()
	lclogfile = gftempname()
	lcOrdChg = gftempname()
	lfcreattemp()
	lntax_rate = gfgetmemvar('M_TAX_RATE', gcact_comp)
	*lodbfOrdChg = CREATEOBJECT("RemoteTable", 'ordchg', '', 'ordchg', SET("DATASESSION"))
	lodbfOrdChg = CREATEOBJECT("RemoteTable", 'ORDCHG', 'ORDCHG', 'ORDCHG', SET("DATASESSION"))
	lcSQL="SELECT cordchg, nordchg, [order] FROM ordchg WHERE [order]='xxx'"
	IF  NOT lodbfOrdChg.sqlrun(lcSQL)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "ordchg Array Update Failed")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		FCLOSE(lnFile)
		RETURN .F.
	ENDIF
	SELECT ordchg
	lnAria = CURSORGETPROP('ConnectHandle')
	IF SQLSETPROP(lnAria, 'Transactions', 2)<0  && manual
		= SQLDISCONNECT(lnAria)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., 'Failed to open Aria SQL')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN .F.
	ENDIF
	= AFIELDS(latempstru)
	= gfcrttmp(lcOrdChg, @latempstru, , "", .T.)
	=gfopentable('StyleUpc', 'STYUPCN')
    = gfopentable('SYCINT', 'CCONTCODE', 'SH', 'SYCINT')
			*C201619,1 TMI 05/22/14 14:43 [Start] gfopentable instead of gfopenfile
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	=gfOpenFile(oariaapplication.datadir+'stydye', oariaapplication.datadir+'stydye', 'SH')
	=gfOpenFile(oariaapplication.datadir+'ORDHDR', oariaapplication.datadir+'ORDHDR', 'SH')
	=gfOpenFile(oariaapplication.datadir+'ORDLINE', oariaapplication.datadir+'ORDLINE', 'SH')
			*C201619,1 TMI 05/22/14 14:43 [Start] gfopentable instead of gfopenfile
			ELSE
	=gfOpenTable(oariaapplication.datadir+'STYDYE', oariaapplication.datadir+'STYDYE', 'SH')
	=gfOpenTable(oariaapplication.datadir+'ORDHDR', oariaapplication.datadir+'ORDHDR', 'SH')
	=gfOpenTable(oariaapplication.datadir+'ORDLINE', oariaapplication.datadir+'ORDLINE', 'SH')
	=gfOpenTable(oariaapplication.datadir+'CUSTOMER', oariaapplication.datadir+'CUSTOMER', 'SH')
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	= gfopentable('STYLE', 'STYLE', 'SH', 'STYLE')
	STORE 0 TO lnIp
	DIMENSION laProcessed[1]
	lcSkeleton=lcrppath+'*.csv'
	lnFnumber = ADIR(laFiles, lcSkeleton)  && Create array
	IF lnFnumber=0
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		= gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		RETURN
	ENDIF
	ASORT(laFiles)
	oRE = CreateObject("VBScript.RegExp")
	FOR nCount = 1 TO lnFnumber
		oRE.Pattern = "^orders-[0-9]{10}.csv$" && dragons Peter Werth
		IF oRE.test(LOWER(laFiles(nCount,1)))
			lcFile=lcrppath+laFiles(nCount,1)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			WAIT WINDOW NOWAIT lcFile
			*C201619,1 TMI 05/22/14 14:44 [Start] 
			ENDIF
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			SELECT (lctemphead)
			APPEND FROM (lcFile) FOR worder<>'order_id' AND qty>0 DELIMITED
			lnIp=lnIp+1
			DIMENSION laProcessed[lnIp]
			laProcessed[lnIp]=lcFile
			LOOP
		ENDIF
		oRE.Pattern = "^customers.csv$"
		IF oRE.test(LOWER(laFiles(nCount,1)))
			lcFile=lcrppath+laFiles(nCount,1)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			WAIT WINDOW nowait lcFile
			*C201619,1 TMI 05/22/14 14:44 [Start] 
			ENDIF
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			SELECT (lctempCust)
			APPEND FROM (lcFile) for cust_id<>'customer_id' DELIMITED
			lnIp=lnIp+1
			DIMENSION laProcessed[lnIp]
			laProcessed[lnIp]=lcFile
			LOOP
		ENDIF
	ENDFOR
	RELEASE oRE
	IF (.NOT. EMPTY(lctemphead) .AND. USED(lctemphead))
		SELECT (lctemphead)
		SCAN
			IF !EMPTY(&lctemphead..worder)
				IF !EMPTY(&lctemphead..store) && Mocks Allow for empty store
					IF !SEEK(&lcCustSel..account+&lctemphead..store,lcCustFile)
						SELECT (lcCustFile)
						APPEND BLANK
						REPL account WITH &lcCustSel..account in (lcCustFile)
						REPL store WITH &lctemphead..store in (lcCustFile)
					ENDIF
					SELECT (lcCustFile)
					REPL btname WITH &lctemphead..btname, cemail WITH &lctemphead..email, caddress1 WITH &lctemphead..b_addr_1, caddress2 WITH &lctemphead..b_addr_2  in (lcCustFile)
					REPL caddress3 WITH &lctemphead..b_addr_3, caddress4 WITH &lctemphead..b_addr_4, cpostcode WITH &lctemphead..b_postcode in (lcCustFile)
					REPL ccountry WITH UPPER(ALLTRIM(&lctemphead..b_country)), ccurrency WITH &lctemphead..cCurrcode, cphone WITH &lctemphead..b_phone in (lcCustFile)
					REPL stname WITH &lctemphead..s_name, saddress1 WITH &lctemphead..s_addr_1, saddress2 WITH &lctemphead..s_addr_2  in (lcCustFile)
					REPL saddress3 WITH &lctemphead..s_addr_3, saddress4 WITH &lctemphead..s_addr_4, spostcode WITH &lctemphead..s_postcode in (lcCustFile)
					lcOrder=&lctemphead..worder
					lcCountry=UPPER(ALLTRIM(&lctemphead..b_country))
				ENDIF
				lcOrder=&lctemphead..worder
				lcCountry=UPPER(ALLTRIM(&lctemphead..b_country))
			ENDIF
			IF EMPTY(&lctemphead..sku)
				lffilllog("UPC not found "+alltrim(&lctemphead..sku),lcOrder,.T.)
				LOOP
			ENDIF
			IF gfseek(LEFT(&lctemphead..sku,6)+SUBSTR(&lctemphead..sku,7,5)+SUBSTR(&lctemphead..sku,12,1), 'STYLEUPC')
				IF !SEEK(padr(lcOrder,12,' ')+StyleUPC.Style,lcTempLine)
					SELECT (lcTempLine)
					APPEND BLANK
					REPL wOrder WITH lcOrder, style WITH styleupc.style IN (lcTempLine)
				ENDIF
				=gfSeek(styleupc.style, 'style')
				SELECT (lcTempLine)
				lcSize = ALLTRIM(StyleUPC.Size)
				&& dragons
				store .f. to llVat
				if between(val(lcSize), style.ntaxbreak, 8)
					if lcCountry='GB'
						store .t. to llVat
					else
						if lfEurope(lcCountry)
							store .t. to llVat
						endif
					endif
				endif
				&& dragons
*!*					lnPrice=IIF(lcCountry='GB',ROUND((&lctemphead..price*100)/((100+lntax_rate)*&lctemphead..Qty),2),round(&lctemphead..price/&lctemphead..Qty,2))
				lnPrice=IIF(llVat,ROUND((&lctemphead..price*100)/((100+lntax_rate)*&lctemphead..Qty),2),round(&lctemphead..price/&lctemphead..Qty,2)) && dragons
				REPL qty&lcsize WITH &lctemphead..Qty, price WITH lnPrice IN (lcTempLine)
			ELSE
				lffilllog("UPC not found "+alltrim(&lctemphead..sku),lcOrder,.T.)
			ENDIF
		ENDSCAN
	ENDIF
	IF (.NOT. EMPTY(lctempCust) .AND. USED(lctempCust))
		SELECT (lctempCust)
		SCAN
			IF SEEK(&lcCustSel..account+&lctempCust..cust_id,lcCustFile) && already have record for Customer WITH Order
				LOOP
			ENDIF
			IF !SEEK(&lcPotSel..account+&lctempCust..cust_id,lcCustFile)
				SELECT (lcCustFile)
				APPEND BLANK
				REPL account WITH &lcPotSel..account in (lcCustFile)
				REPL store WITH &lctempCust..cust_id in (lcCustFile)
			ENDIF
			SELECT (lcCustFile)
			REPL btname WITH &lctempCust..cust_name, cemail WITH &lctempCust..cust_email, caddress1 WITH &lctempCust..cust_addr1, caddress2 WITH &lctempCust..cust_addr2  in (lcCustFile)
			REPL caddress3 WITH &lctempCust..cust_addr3, caddress4 WITH &lctempCust..cust_addr4, cpostcode WITH &lctempCust..post_code in (lcCustFile)
			REPL ccountry WITH &lctempCust..country, cphone WITH &lctempCust..phone_no in (lcCustFile)
		ENDSCAN
	ENDIF

	SELECT (lcTempHead)
	DELETE FROM (lcTempHead) WHERE worder IN (SELECT DISTINCT worder FROM (lclogfile))

	SELECT customer
	SET ORDER TO 1
	=gfSeek('M'+&lcCustSel..ACCOUNT, 'customer') && Get Main Account
	lcSalesRep=customer.SalesRep
	lccblkpck=customer.cblkpck

	IF (.NOT. EMPTY(lcCustfile) .AND. USED(lcCustfile))
		SELECT (lcCustfile)
		SET ORDER TO
		SCAN FOR ACCOUNT=&lcCustSel..ACCOUNT && Has Order
			IF !gfSeek('S'+&lcCustFile..ACCOUNT+&lcCustFile..STORE, 'customer') && Doesn't Exist, create new
				SELECT customer
				APPEND BLANK
				REPL account WITH &lcCustFile..ACCOUNT, store WITH &lcCustFile..STORE, billto WITH 'A', STATUS WITH 'A', link_code WITH 'DEFDEF', consol WITH 'N', type WITH 'S', cblkpck with lccblkpck
				= gfadd_info('CUSTOMER') && dragons 2012-12-19
			ENDIF
			REPL btname WITH &lcCustFile..btname, phone1 WITH &lcCustFile..cphone IN customer
			REPL caddress12 WITH &lcCustFile..caddress1, caddress22 WITH &lcCustFile..caddress2 IN customer
			REPL caddress32 WITH &lcCustFile..caddress3, caddress42 WITH &lcCustFile..caddress4 IN customer
			REPL caddress52 WITH &lcCustFile..cpostcode, caddress62 WITH &lcCustFile..ccountry, ccont_code WITH &lcCustFile..ccountry IN customer
			REPL lvatexem WITH IIF(UPPER(ALLTRIM(&lcCustFile..ccountry))='GB',.F.,!lfEurope(UPPER(ALLTRIM(&lcCustFile..ccountry)))) IN customer && dragons
			REPL nTaxRate WITH IIF(UPPER(ALLTRIM(&lcCustFile..ccountry))='GB',lntax_rate,iif(lfEurope(UPPER(ALLTRIM(&lcCustFile..ccountry))),lntax_rate,0)) IN customer && dragons 2012-12-19
			REPL cCurrCode WITH &lcCustFile..ccurrency, SalesRep WITH lcSalesRep, cblkpck with lccblkpck IN customer
			REPL stname WITH IIF(EMPTY(&lcCustFile..stname), &lcCustFile..btname, &lcCustFile..stname) IN customer
			REPL caddress1 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress1, &lcCustFile..saddress1) IN customer
			REPL caddress2 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress2, &lcCustFile..saddress2) IN customer
			REPL caddress3 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress3, &lcCustFile..saddress3) IN customer
			REPL caddress4 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress4, &lcCustFile..saddress4) IN customer
			REPL caddress5 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..cpostcode, &lcCustFile..spostcode) IN customer
			REPL caddress6 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..ccountry, &lcCustFile..scountry) IN customer
			REPL ccont_cod2 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..ccountry, &lcCustFile..scountry) IN customer
			IF gfSeek('S'+&lcPotSel..ACCOUNT+&lcCustFile..STORE, 'customer') && Potential Exists, change status
				REPL STATUS WITH 'X' in customer
			ENDIF
		ENDSCAN
		SCAN FOR ACCOUNT=&lcPotSel..ACCOUNT && Potential Customer
			IF gfSeek('S'+&lcCustSel..ACCOUNT+&lcCustFile..STORE, 'customer') && Customer already exists WITH Order
				LOOP
			ENDIF
			IF !gfSeek('S'+&lcPotSel..ACCOUNT+&lcCustFile..STORE, 'customer') && Doesn't Exist, create new
				SELECT customer
				APPEND BLANK
				REPL account WITH &lcCustFile..ACCOUNT, store WITH &lcCustFile..STORE, billto WITH 'A', STATUS WITH 'P', link_code WITH 'DEFDEF', consol WITH 'N', type WITH 'S'
			ENDIF
			REPL btname WITH &lcCustFile..btname, phone1 WITH &lcCustFile..cphone IN customer
			REPL caddress12 WITH &lcCustFile..caddress1, caddress22 WITH &lcCustFile..caddress2 IN customer
			REPL caddress32 WITH &lcCustFile..caddress3, caddress42 WITH &lcCustFile..caddress4 IN customer
			REPL caddress52 WITH &lcCustFile..cpostcode, caddress62 WITH &lcCustFile..ccountry, ccont_code WITH &lcCustFile..ccountry IN customer
			REPL cCurrCode WITH &lcCustFile..ccurrency, SalesRep WITH lcSalesRep IN customer
			REPL stname WITH IIF(EMPTY(&lcCustFile..stname), &lcCustFile..btname, &lcCustFile..stname) IN customer
			REPL caddress1 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress1, &lcCustFile..saddress1) IN customer
			REPL caddress2 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress2, &lcCustFile..saddress2) IN customer
			REPL caddress3 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress3, &lcCustFile..saddress3) IN customer
			REPL caddress4 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..caddress4, &lcCustFile..saddress4) IN customer
			REPL caddress5 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..cpostcode, &lcCustFile..spostcode) IN customer
			REPL caddress6 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..ccountry, &lcCustFile..scountry) IN customer
			REPL ccont_cod2 WITH IIF(EMPTY(&lcCustFile..saddress1), &lcCustFile..ccountry, &lcCustFile..scountry) IN customer
		ENDSCAN
		SELECT customer
		*tmi* T20140716.0001 [START] I found that calling the gftableupdate at this point makes the program losses the lnAria connection to sqlserver
		*gftableupdate()
		TABLEUPDATE(.T.,.T.)
		*tmi* T20140716.0001
	ENDIF

	store space(0) to lcStart, m.Order
	lcResult=chr(13)+'No Orders Created'

	IF ( .NOT. EMPTY(lctempline) .AND. USED(lctempline))
		= gfopentable('ORDLINE', 'ORDLINE', 'SH')
		= gfopentable('ORDHDR', 'ORDHDR', 'SH')
		= gfopentable('NOTEPAD', 'NOTEPAD', 'SH')
		= gfseek('M'+&lcCustSel..account, 'CUSTOMER', 'CUSTOMER')
		m.rep1 = customer.salesrep
		m.comm1 = customer.comm
		m.comm2 = customer.comm2
		m.rep2 = customer.rep2
		m.ctermcode = customer.ctermcode
		m.gl_sales = customer.cslsgllink
		m.link_code = customer.link_code
		m.priority = customer.priority
		m.shipvia = customer.shipvia
		m.spcinst = customer.spcinst
		m.buyer = customer.buyer
		SELECT (lctemphead)
		SCAN for !empty(wOrder)
			llvalidqty = .T.
			llvalidstyle = .T.
			llvalidacc = .T.
			llvalidstore = .T.
			llvalidcnt = .T.
			llvalidprice = .T.
			llvalidcurr = .T.
			llvalidrate = .T.
			IF cCurrcode<>oariaapplication.basecurrency
			*C201619,1 TMI 05/22/14 14:43 [Start] gfopentable instead of gfopenfile
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
				= gfopenfile(oariaapplication.syspath+'SYCCURR', 'CCURRCODE', 'SH')
			*C201619,1 TMI 05/22/14 14:43 [Start] gfopentable instead of gfopenfile
			ELSE 
				= gfopenTable(oariaapplication.syspath+'SYCCURR', 'CCURRCODE', 'SH')
			ENDIF			
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
				SELECT (lctemphead)
				IF  .NOT. SEEK(cCurrcode, 'SYCCURR')
					llvalidcurr = .F.
					lffilllog("Invalid Currency Code "+cCurrcode,&lctemphead..worder,.T.)
				ENDIF
				lnunit = 0
				lnnexrate = 0
				lnnexrate = gfchkrate('lnUnit', cCurrcode, ctot(strtran(&lctemphead..order_date,' ','T')), .F.)
				SELECT (lctemphead)
				IF lnnexrate=0
					llvalidrate = .F.
					lffilllog("Invalid Currency Exchange Rate "+cCurrcode,&lctemphead..worder,.T.)
				ENDIF
			ENDIF
			=SEEK(&lctemphead..worder,lctempline)
			SELECT (lctempline)
			SCAN REST WHILE worder+STYLE = &lctemphead..worder
				llvalidstyle = llvalidstyle .AND. gfseek(style, 'Style', 'Style')
				IF  .NOT. llvalidstyle
					lffilllog("Invalid Style No."+STYLE,&lctemphead..worder,.F.)
				ENDIF
*!*					IF price<0.01
*!*						lffilllog("Price is less than 0.01",&lctemphead..worder,.F.)
*!*						llvalidprice = .F.
*!*					ENDIF
			ENDSCAN
			IF  .NOT. llvalidqty .OR.  .NOT. llvalidstyle .OR.  .NOT. llvalidacc .OR.  .NOT. llvalidstore .OR.  .NOT. llvalidcnt .OR.  .NOT. llvalidprice .OR.  .NOT. llvalidcurr .OR.  .NOT. llvalidrate
				SELECT (lctemphead)
				DELETE
				LOOP
			ELSE
				STORE '' TO m.cclass, m.ccurrcode, m.cdivision, m.cordtype, m.ctermcode, m.custpo, m.order, m.cwarecode, m.gl_sales, m.link_code, m.multi, m.note1, m.priority, m.season, m.shipvia, m.spcinst, m.status, m.stname, lcorder
				STORE .F. TO m.alt_shpto, m.lhasnotes, m.multipo, m.lfromweb
				STORE '' TO m.bulk, m.creorder, m.phone
				STORE 0 TO m.comm1, m.comm2, m.nexrate, m.disc, m.appramt
				STORE 0 TO m.book, m.bookamt, lnlastlno, m.lastline, m.nexrate, m.ncurrunit, m.openamt, m.open, m.totamnt
				SELECT (lctemphead)
				SCATTER MEMO MEMVAR
				m.entered=ctot(strtran(&lctemphead..order_date,' ','T'))
				m.start=ctot(strtran(&lctemphead..order_date,' ','T'))
				m.complete=ctot(strtran(&lctemphead..order_date,' ','T'))
				IF m.ccurrcode<>oariaapplication.basecurrency
					lnunit = 0
					m.nexrate = gfchkrate('lnUnit', m.ccurrcode, m.entered, .F.)
					m.ncurrunit = lnunit
				ELSE
					m.ncurrunit = 1
					m.nexrate = 1
				ENDIF
				m.lfromweb = .T.
				m.disc = 0.00
				m.appramt = 0
				=SEEK(&lctemphead..worder,lctempline)
				=gfseek(&lctempline..STYLE,'Style','Style')
				m.cdivision = style.cdivision
				m.order = 'B'+RIGHT(gfsequence('ORDER'),5)
				lcStart=iif(empty(lcStart),m.Order,lcStart)
				m.season = style.season
				SELECT (lctempline)
				COUNT REST WHILE worder+STYLE = &lctemphead..worder TO lnlastlno
				=SEEK(&lctemphead..worder,lctempline)
				SELECT (lctempline)
				SUM qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, (qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8)* price  REST WHILE worder+STYLE = &lctemphead..worder TO m.qty1,m.qty2, m.qty3,m.qty4 ,m.qty5 ,m.qty6 ,m.qty7 ,m.qty8 ,m.totamnt
				SELECT (lctemphead)
				m.ngiftvalue=&lctemphead..gift_value
				m.ncacctbal=&lctemphead..acc_bal
				m.lgifttag=IIF(&lctemphead..gift_tag=1,.T.,.F.)
				m.c3rdstatus=&lctemphead..td_status
				m.CCV21=&lctemphead..avs_cv2
				m.CCV22=&lctemphead..avs_addr
				m.CCV23=&lctemphead..avs_postc
				m.CCV24=&lctemphead..avs_result
				m.Cpromo=&lctemphead..promo_code
				m.c3rdman=&lctemphead..td_score
				m.csagepay=&lctemphead..sage_ref
				m.multipo = .F.
				m.bulk = 'N'
				m.creorder = 'N'
				m.cordtype = 'O'
				m.custpo = m.worder
				m.cwarecode = &lcWhSel..cwarecode
				m.multi = 'N'
				lcNotes=alltrim(m.gnotes)
				do case
					case len(lcNotes)=0
						store space(0) to m.note1, m.note2
					case len(lcNotes)<31
						m.note1=lcNotes
						m.note1=''
					otherwise
						m.note1=left(lcNotes,30)
						m.note2=substr(lcNotes,31)
				endcase
				m.status = 'O'
				m.stname = &lctemphead..s_name
				m.caddress1=&lctemphead..s_addr_1
				m.caddress2=&lctemphead..s_addr_2
				m.caddress3=&lctemphead..s_addr_3
				m.caddress4=&lctemphead..s_addr_4
				m.caddress5=&lctemphead..s_postcode
				m.phone = &lctemphead..b_phone
				m.alt_shpto = .T.
				m.mnotes=m.tag_text
				m.lhasnotes = IIF( .NOT. EMPTY(m.mnotes), .T., .F.)
				m.book = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
				m.bookamt = m.totamnt
				m.lastline = lnlastlno
				m.flag = ''
				m.open = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
				m.openamt = m.totamnt
				m.account = &lcCustSel..account
				m.cblkpck=customer.cblkpck
				SELECT ordhdr
				gfappend('ORDHDR', .T.)
				= gfadd_info('ORDHDR')
				IF &lctemphead..nOrdChg>0
					m.cOrdChg=lcRpChg
					SELECT (lcOrdChg)
					gfappend(lcOrdChg, .T.)
				ENDIF
				IF  .NOT. EMPTY(m.mnotes)
					m.type = 'B'
					m.key = m.order
					m.cdesc = "Notes For Order Number : "+m.order
					SELECT notepad
					gfappend('NOTEPAD', .T.)
					= gfadd_info('NOTEPAD')
				ENDIF
				SELECT (lctempline)
				=SEEK(&lctemphead..worder,lctempline)
				lnline = 1
				SCAN REST WHILE worder+STYLE = &lctemphead..worder
					STORE 0 TO m.book1, m.book2, m.book3, m.book4, m.book5, m.book6, m.book7, m.book8
					STORE 0 TO m.comm1, m.comm2
					SCATTER MEMO MEMVAR
					m.cwarecode = &lcWhSel..cwarecode
					m.cordtype = 'O'
					m.custpo = m.worder
					=gfseek(&lctempline..STYLE,'Style','Style')
					m.gros_price = m.price
					m.cost = style.totcost
					m.desc1 = style.desc1
					m.flag = ''
					m.gl_cost = style.link_code
					m.gl_sales = customer.cslsgllink+style.cslsgllink
					m.scale = style.scale
					m.season = style.season
					m.book1 = m.qty1
					m.book2 = m.qty2
					m.book3 = m.qty3
					m.book4 = m.qty4
					m.book5 = m.qty5
					m.book6 = m.qty6
					m.book7 = m.qty7
					m.book8 = m.qty8
					m.totbook = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
					m.totqty = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
					m.lineno = lnline
					lnline = lnline+1
					SELECT ordline
					gfappend('ORDLINE', .T.)
					= gfadd_info('ORDLINE')
					SELECT STYLE 
					SET ORDER TO 1
					IF SEEK(&lctempline..STYLE)
						REPL ORD1 WITH ORD1+M.BOOK1
						REPL ORD2 WITH ORD2+M.BOOK2
						REPL ORD3 WITH ORD3+M.BOOK3
						REPL ORD4 WITH ORD4+M.BOOK4
						REPL ORD5 WITH ORD5+M.BOOK5
						REPL ORD6 WITH ORD6+M.BOOK6
						REPL ORD7 WITH ORD7+M.BOOK7
						REPL ORD8 WITH ORD8+M.BOOK8
						REPL TOTORD WITH TOTORD+M.TOTBOOK
					ENDIF
					SELECT STYDYE
					SET ORDER TO 2
					IF SEEK(M.CWARECODE+&lctempline..STYLE+'          ')
						REPL ORD1 WITH ORD1+M.BOOK1
						REPL ORD2 WITH ORD2+M.BOOK2
						REPL ORD3 WITH ORD3+M.BOOK3
						REPL ORD4 WITH ORD4+M.BOOK4
						REPL ORD5 WITH ORD5+M.BOOK5
						REPL ORD6 WITH ORD6+M.BOOK6
						REPL ORD7 WITH ORD7+M.BOOK7
						REPL ORD8 WITH ORD8+M.BOOK8
						REPL TOTORD WITH TOTORD+M.TOTBOOK
					ENDIF
				ENDSCAN
			ENDIF
		ENDSCAN
		lcAddTime=''
		lcAddDate=''
		SELECT (lcOrdChg)
		SCAN
			lcSql="INSERT INTO [ordchg] ([cadd_time] ,[cadd_user] ,[cadd_ver] ,[cedit_time],[cedit_user],[cedt_ver],[cordchg] ,[dadd_date],[dedit_date] ,[invoice] ,[invoiced] ,[nordchg] ,[order])"
			lcSql=lcSql+" VALUES (CONVERT(CHAR(11),GETDATE(),108),'"+ALLTRIM(oariaapplication.user_id)+"','A40','','','','"+ALLTRIM(&lcOrdChg..cordchg)+"',GETDATE(),'1900-01-01','',0,"+ALLTRIM(STR(&lcOrdChg..nordchg,9,2))+",'"+&lcOrdChg..order+"')"
			lnRetVal = 0
			DO while .T.
				lnRetVal = SQLEXEC(lnAria, lcSql)
				IF lnRetVal<>0
					EXIT
				ENDIF
				LOOP
			ENDDO
			IF lnRetVal < 0 && Error
				= AERROR(laError)
				SQLROLLBACK(lnAria)
				SQLDISCONNECT(lnAria)
				STORE 0 TO lnAria
				lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpweb.sql')
				FPUTS(lnFileHandle, lcSQL)
				FOR n = 1 TO 7  && Display all elements of the array
					IF VARTYPE(laError(n))$'N,C'
						FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
					ENDIF
				ENDFOR
				FCLOSE(lnFileHandle)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
				= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable add OrdChg')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
				RETURN .F.
			ENDIF
		ENDSCAN
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		WAIT WINDOW NOWAIT 'Updating SQL'
			*C201619,1 TMI 05/22/14 14:44 [Start] 
			ENDIF
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
		IF SQLCOMMIT(lnAria)<>1
			= AERROR(laError)
			SQLROLLBACK(lnAria)
			SQLDISCONNECT(lnAria)
			STORE 0 TO lnAria
			lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpweb.sql')
			FPUTS(lnFileHandle, lcSQL)
			FOR n = 1 TO 7  && Display all elements of the array
				IF VARTYPE(laError(n))$'N,C'
					FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
				ENDIF
			ENDFOR
			FCLOSE(lnFileHandle)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to Update SQL')
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
			RETURN .F.
		ENDIF
		SELECT ordhdr
		gftableupdate()
		SELECT ordline
		gftableupdate()
		SELECT notepad
		gftableupdate()
		select style
		gftableupdate()
		select stydye
		gftableupdate()
		= gfclosetable('ORDLINE')
		= gfclosetable('ORDHDR')
		= gfclosetable('STYLE')
		= gfclosetable('NOTEPAD')
		= gfclosetable('CUSTOMER')
		= gfclosetable('WAREHOUS')
		if !empty(lcStart)
			lcResult=chr(13)+chr(13)+'Orders '+lcStart+' to '+m.Order+' Created'
		ENDIF
	ENDIF
	lnFnumber = ALEN(laProcessed, 1)  && Count Files
	FOR nCount = 1 TO lnFnumber
		IF !EMPTY(laProcessed(nCount))
			lcName=ADDBS(lcRpHist)+JUSTFNAME(laProcessed(nCount))+'x'
			lcfile=laProcessed(nCount)
			COPY FILE (laProcessed(nCount)) TO (lcName)
			ERASE (laProcessed(nCount))
		ENDIF
	ENDFOR

	IF RECCOUNT(lclogfile)>0
		SELECT (lclogfile)
		lcname=ADDBS(lcRpHist)+'WebErrors_'+TTOC(DATETIME(),1)+'.csv'
		COPY TO (lcname) DELIMITED
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Errors found, please check Error File'+chr(13)+lcname)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
	ENDIF

			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			IF TYPE('lcXMLFileName') <> 'C'
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Finished Processing'+lcResult)
			*C201619,1 TMI 05/22/14 14:43 [Start] Suppress any message calling in the request processing
			ENDIF 
			*C201619,1 TMI 05/22/14 14:44 [End  ] 
ENDFUNC
**
PROCEDURE lfvPath
	IF ALLTRIM(lcrppath)="?"
		lcrppath = GETDIR()
	ENDIF
ENDPROC
**
**
PROCEDURE lfvHist
	IF ALLTRIM(lcrpHist)="?"
		lcrpHist = GETDIR()
	ENDIF
ENDPROC
**
PROCEDURE lfwOgWhen
		*C201619,1 TMI 05/22/14 14:51 [Start] used only in aria4xp
		IF TYPE('lcXMLFileName') <> 'C'
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	loogscroll.parent.ogtoolbar.cntexternal.cmdemail.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdprint.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdexport.enabled = .F.
		*C201619,1 TMI 05/22/14 14:51 [Start] don't us e loogscroll.laogfxflt in RB case, us e laogfxflt instead
		ENDIF 
		*C201619,1 TMI 05/22/14 14:51 [End  ] 
	IF EMPTY(lcrppath)
		lcrppath = 'c:\temp'
		lcrpHist = 'c:\temp'
	ENDIF
ENDPROC
**
**
**
**
**
PROCEDURE lfChkStrct
	DIMENSION laitemseg[1]
	*C201612,1 TMI 04/23/14 16:26 [Start] 
	IF TYPE('lcXMLFileName') <> 'C'
  	*C201612,1 TMI 04/23/14 16:26 [End  ] 
	= gfitemmask(@laitemseg)
	*C201612,1 TMI 04/23/14 16:26 [Start] 
	ELSE 
	  ItemMask = CREATEOBJECT("GetItemMask")
      =ItemMask.Do(@laItemSeg)
	ENDIF 
	*C201612,1 TMI 04/23/14 16:26 [End  ] 
	FOR lncount = 1 TO ALEN(laitemseg, 1)
		DO CASE
			CASE laitemseg(lncount, 1)='C'
				lnclrlngl = LEN(laitemseg(lncount, 3))
				lnclrposgl = laitemseg(lncount, 4)
			CASE laitemseg(lncount, 1)='F'
				lnstylngl = LEN(laitemseg(lncount, 3))
				lnstyposgl = laitemseg(lncount, 4)
			CASE laitemseg(lncount, 1)='S'
				lnscalngl = LEN(laitemseg(lncount, 3))
				lnscaposgl = laitemseg(lncount, 4)
		ENDCASE
	ENDFOR
ENDPROC
****
****
PROCEDURE lfCreatTemp
	DIMENSION latempstru[11, 4]
	STORE SPACE(0) TO latempstru
	latempstru[1, 1] = 'WORDER'
	latempstru[1, 2] = "C"
	latempstru[1, 3] = 12
	latempstru[1, 4] = 0
	latempstru[2, 1] = 'Style'
	latempstru[2, 2] = "C"
	latempstru[2, 3] = 19
	latempstru[2, 4] = 0
	latempstru[3, 1] = 'Qty1'
	latempstru[3, 2] = "N"
	latempstru[3, 3] = 6
	latempstru[3, 4] = 0
	latempstru[4, 1] = 'Qty2'
	latempstru[4, 2] = "N"
	latempstru[4, 3] = 6
	latempstru[4, 4] = 0
	latempstru[5, 1] = 'Qty3'
	latempstru[5, 2] = "N"
	latempstru[5, 3] = 6
	latempstru[5, 4] = 0
	latempstru[6, 1] = 'Qty4'
	latempstru[6, 2] = "N"
	latempstru[6, 3] = 6
	latempstru[6, 4] = 0
	latempstru[7, 1] = 'Qty5'
	latempstru[7, 2] = "N"
	latempstru[7, 3] = 6
	latempstru[7, 4] = 0
	latempstru[8, 1] = 'Qty6'
	latempstru[8, 2] = "N"
	latempstru[8, 3] = 6
	latempstru[8, 4] = 0
	latempstru[9, 1] = 'Qty7'
	latempstru[9, 2] = "N"
	latempstru[9, 3] = 6
	latempstru[9, 4] = 0
	latempstru[10, 1] = 'Qty8'
	latempstru[10, 2] = "N"
	latempstru[10, 3] = 6
	latempstru[10, 4] = 0
	latempstru[11, 1] = 'Price'
	latempstru[11, 2] = "N"
	latempstru[11, 3] = 12
	latempstru[11, 4] = 2
	= gfcrttmp(lctempline, @latempstru, "WORDER+Style", lctempline, .T.)
	DIMENSION latempstru[9, 4]
	STORE SPACE(0) TO latempstru
	latempstru[1, 1] = "Worder"
	latempstru[1, 2] = "C"
	latempstru[1, 3] = 12
	latempstru[1, 4] = 0
	latempstru[2, 1] = "Entered"
	latempstru[2, 2] = "D"
	latempstru[2, 3] = 8
	latempstru[2, 4] = 0
	latempstru[3, 1] = "Account"
	latempstru[3, 2] = "C"
	latempstru[3, 3] = 5
	latempstru[3, 4] = 0
	latempstru[4, 1] = "Store"
	latempstru[4, 2] = "C"
	latempstru[4, 3] = 8
	latempstru[4, 4] = 0
	latempstru[5, 1] = "Style"
	latempstru[5, 2] = "C"
	latempstru[5, 3] = 19
	latempstru[5, 4] = 0
	latempstru[6, 1] = "szcnt"
	latempstru[6, 2] = "N"
	latempstru[6, 3] = 1
	latempstru[6, 4] = 0
	latempstru[7, 1] = "TOTQty"
	latempstru[7, 2] = "N"
	latempstru[7, 3] = 10
	latempstru[7, 4] = 0
	latempstru[8, 1] = "PRICE"
	latempstru[8, 2] = "N"
	latempstru[8, 3] = 7
	latempstru[8, 4] = 2
	latempstru[9, 1] = "DESC"
	latempstru[9, 2] = "M"
	latempstru[9, 3] = 10
	latempstru[9, 4] = 0
	= gfcrttmp(lclogfile, @latempstru, "WORDER", lclogfile, .F.)
	DIMENSION latempstru[21, 4]
	STORE SPACE(0) TO latempstru
	latempstru[1, 1] = "Account"
	latempstru[1, 2] = "C"
	latempstru[1, 3] = 5
	latempstru[1, 4] = 0
	latempstru[2, 1] = "Store"
	latempstru[2, 2] = "C"
	latempstru[2, 3] = 8
	latempstru[2, 4] = 0
	latempstru[3, 1] = 'btname'
	latempstru[3, 2] = "C"
	latempstru[3, 3] = 30
	latempstru[3, 4] = 0
	latempstru[4, 1] = 'cemail'
	latempstru[4, 2] = "C"
	latempstru[4, 3] = 128
	latempstru[4, 4] = 0
	latempstru[5, 1] = 'caddress1'
	latempstru[5, 2] = "C"
	latempstru[5, 3] = 30
	latempstru[5, 4] = 0
	latempstru[6, 1] = 'caddress2'
	latempstru[6, 2] = "C"
	latempstru[6, 3] = 30
	latempstru[6, 4] = 0
	latempstru[7, 1] = 'caddress3'
	latempstru[7, 2] = "C"
	latempstru[7, 3] = 30
	latempstru[7, 4] = 0
	latempstru[8, 1] = 'caddress4'
	latempstru[8, 2] = "C"
	latempstru[8, 3] = 30
	latempstru[8, 4] = 0
	latempstru[9, 1] = 'cpostcode'
	latempstru[9, 2] = "C"
	latempstru[9, 3] = 10
	latempstru[9, 4] = 0
	latempstru[10, 1] = 'ccountry'
	latempstru[10, 2] = "C"
	latempstru[10, 3] = 2
	latempstru[10, 4] = 0
	latempstru[11, 1] = 'ccurrency'
	latempstru[11, 2] = "C"
	latempstru[11, 3] = 3
	latempstru[11, 4] = 0
	latempstru[12, 1] = 'cphone'
	latempstru[12, 2] = "C"
	latempstru[12, 3] = 12
	latempstru[12, 4] = 0
	latempstru[13, 1] = 'stname'
	latempstru[13, 2] = "C"
	latempstru[13, 3] = 30
	latempstru[13, 4] = 0
	latempstru[14, 1] = 'saddress1'
	latempstru[14, 2] = "C"
	latempstru[14, 3] = 30
	latempstru[14, 4] = 0
	latempstru[15, 1] = 'saddress2'
	latempstru[15, 2] = "C"
	latempstru[15, 3] = 30
	latempstru[15, 4] = 0
	latempstru[16, 1] = 'saddress3'
	latempstru[16, 2] = "C"
	latempstru[16, 3] = 30
	latempstru[16, 4] = 0
	latempstru[17, 1] = 'saddress4'
	latempstru[17, 2] = "C"
	latempstru[17, 3] = 30
	latempstru[17, 4] = 0
	latempstru[18, 1] = 'spostcode'
	latempstru[18, 2] = "C"
	latempstru[18, 3] = 10
	latempstru[18, 4] = 0
	latempstru[19, 1] = 'scountry'
	latempstru[19, 2] = "C"
	latempstru[19, 3] = 2
	latempstru[19, 4] = 0
	latempstru[20, 1] = 'scurrency'
	latempstru[20, 2] = "C"
	latempstru[20, 3] = 3
	latempstru[20, 4] = 0
	latempstru[21, 1] = 'sphone'
	latempstru[21, 2] = "C"
	latempstru[21, 3] = 12
	latempstru[21, 4] = 0
	= gfcrttmp(lcCustfile, @latempstru, "ACCOUNT+STORE", lcCustfile, .T.)
	DIMENSION latempstru[41, 18]
	STORE '' TO latempstru
	STORE 0 TO lnindex
	lnindex=lnindex+1
	latempstru[lnindex, 1]="worder"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="order_date"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=19
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="store"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=8
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="btname"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_addr_1"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_addr_2"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_addr_3"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_addr_4"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_postcode"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=10
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_country"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=3
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="cCurrcode"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=3
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="b_phone"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="email"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=128
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_name"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_addr_1"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_addr_2"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_addr_3"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_addr_4"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_postcode"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=10
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_country"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=3
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="s_phone"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="gnotes"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=60
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="promo_code"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=6
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="acc_bal"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=11
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="gift_value"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=9
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="gift_tag"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=1
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="tag_text"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=250
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="td_status"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="avs_addr"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="avs_postc"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="avs_cv2"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="avs_result"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=24
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="td_score"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=8
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="sage_ref"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=30
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="nordchg"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=9
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="sku"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=13
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="style_code"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="colour"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=12
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="size"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=5
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="price"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=9
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="qty"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=6
	latempstru[lnindex, 4]=0
	= gfcrttmp(lctemphead, @latempstru, , "", .T.)
	DIMENSION lalogstr[9, 4]
	lalogstr[1, 1] = "Worder"
	lalogstr[1, 2] = "C"
	lalogstr[1, 3] = 12
	lalogstr[1, 4] = 0
	lalogstr[2, 1] = "Entered"
	lalogstr[2, 2] = "D"
	lalogstr[2, 3] = 8
	lalogstr[2, 4] = 0
	lalogstr[3, 1] = "Account"
	lalogstr[3, 2] = "C"
	lalogstr[3, 3] = 5
	lalogstr[3, 4] = 0
	lalogstr[4, 1] = "Store"
	lalogstr[4, 2] = "C"
	lalogstr[4, 3] = 8
	lalogstr[4, 4] = 0
	lalogstr[5, 1] = "Style"
	lalogstr[5, 2] = "C"
	lalogstr[5, 3] = 19
	lalogstr[5, 4] = 0
	lalogstr[6, 1] = "szcnt"
	lalogstr[6, 2] = "N"
	lalogstr[6, 3] = 1
	lalogstr[6, 4] = 0
	lalogstr[7, 1] = "TOTQty"
	lalogstr[7, 2] = "N"
	lalogstr[7, 3] = 10
	lalogstr[7, 4] = 0
	lalogstr[8, 1] = "PRICE"
	lalogstr[8, 2] = "N"
	lalogstr[8, 3] = 7
	lalogstr[8, 4] = 2
	lalogstr[9, 1] = "DESC"
	lalogstr[9, 2] = "C"
	lalogstr[9, 3] = 50
	lalogstr[9, 4] = 0
	= gfcrttmp(lclogfile, @lalogstr, "WORDER", lclogfile, .F.)
	DIMENSION latempstru[14, 4]
	latempstru[1, 1] = "cust_id"
	latempstru[1, 2] = "C"
	latempstru[1, 3] = 8
	latempstru[1, 4] = 0
	latempstru[2, 1] = "cust_group"
	latempstru[2, 2] = "C"
	latempstru[2, 3] = 20
	latempstru[2, 4] = 0
	latempstru[3, 1] = "cust_name"
	latempstru[3, 2] = "C"
	latempstru[3, 3] = 60
	latempstru[3, 4] = 0
	latempstru[4, 1] = "cust_email"
	latempstru[4, 2] = "C"
	latempstru[4, 3] = 128
	latempstru[4, 4] = 0
	latempstru[5, 1] = "cust_dob"
	latempstru[5, 2] = "D"
	latempstru[5, 3] = 10
	latempstru[5, 4] = 0
	latempstru[6, 1] = "gender"
	latempstru[6, 2] = "C"
	latempstru[6, 3] = 6
	latempstru[6, 4] = 0
	latempstru[7, 1] = "news_letter"
	latempstru[7, 2] = "N"
	latempstru[7, 3] = 1
	latempstru[7, 4] = 0
	latempstru[8, 1] = "cust_addr1"
	latempstru[8, 2] = "C"
	latempstru[8, 3] = 30
	latempstru[8, 4] = 0
	latempstru[9, 1] = "cust_addr2"
	latempstru[9, 2] = "C"
	latempstru[9, 3] = 30
	latempstru[9, 4] = 0
	latempstru[10, 1] = "cust_addr3"
	latempstru[10, 2] = "C"
	latempstru[10, 3] = 30
	latempstru[10, 4] = 0
	latempstru[11, 1] = "cust_addr4"
	latempstru[11, 2] = "C"
	latempstru[11, 3] = 30
	latempstru[11, 4] = 0
	latempstru[12, 1] = "post_code"
	latempstru[12, 2] = "C"
	latempstru[12, 3] = 10
	latempstru[12, 4] = 0
	latempstru[13, 1] = "country"
	latempstru[13, 2] = "C"
	latempstru[13, 3] = 2
	latempstru[13, 4] = 0
	latempstru[14, 1] = "phone_no"
	latempstru[14, 2] = "C"
	latempstru[14, 3] = 12
	latempstru[14, 4] = 0
	= gfcrttmp(lctempCust, @latempstru, , "", .T.)
ENDPROC
**
PROCEDURE lfFillLog
	PARAMETER lcmessage, lcordernum, llheader
	lcoldalias = SELECT()
	select (lclogfile)
	APPEND BLANK
	REPLACE worder WITH lcordernum, account WITH &lcCustSel..account, STORE WITH &lctemphead..STORE, entered WITH ctot(strtran(&lctemphead..order_date,' ','T'))
	REPLACE desc WITH lcmessage
	SELECT (lcoldalias)
ENDPROC
***
PROCEDURE lfEurope
	PARAMETER lcCountCode
	private lfReturn
	lfReturn=.f.
	lcoldalias = SELECT()
	select sycint
	if gfSeek(lcCountCode,'sycint')
		lfreturn=SYCINT.LEUROPCOM
	endif
	SELECT (lcoldalias)
	return lfReturn
ENDPROC
***






