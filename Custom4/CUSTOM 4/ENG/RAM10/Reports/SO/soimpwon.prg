
*-SAB ----- []


*-SAB ----- [Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID

SET STEP ON	

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId  
  
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  	
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath 
   
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  
  PUBLIC gcAct_Appl
  gcAct_Appl  = 'SO'
  oAriaEnvironment.REPORT.gcAct_Appl = 'SO'
  oAriaEnvironment.ActiveModuleID = 'SO'
  oAriaEnvironment.RequestID = lcRequestID
  
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  
  
  llWhSelect = .F.
  lcWhSel = ''
  lnWhPos = ASCAN(laogfxflt, "WAREHOUS.CWARECODE")
  IF lnWhPos>0
  	lnWhPos = ASUBSCRIPT(laogfxflt, lnWhPos, 1)
  	lcWhSel = IIF( .NOT. EMPTY(laogfxflt(lnWhPos, 6)), laogfxflt(lnWhPos, 6), '')
  	IF  .NOT. EMPTY(lcWhSel) .AND. USED(lcWhSel)
  		llWhSelect = .T.
  	ENDIF
  ENDIF
	llCustSelect = .F.
	lcCustSel = ''
	lnCustPos = ASCAN(laogfxflt, "CUSTOMER.ACCOUNT")
	IF lnCustPos>0
		lnCustPos = ASUBSCRIPT(laogfxflt, lnCustPos, 1)
		lcCustSel = IIF( .NOT. EMPTY(laogfxflt(lnCustPos, 6)), laogfxflt(lnCustPos, 6), '')
		IF  .NOT. EMPTY(lcCustSel) .AND. USED(lcCustSel)
			llCustSelect = .T.
		ENDIF
	ENDIF
	llPotSelect = .F.
	lcPotSel = ''
	lnPotPos = ASCAN(laogfxflt, "POTCUST.ACCOUNT")
	IF lnPotPos>0
		lnPotPos = ASUBSCRIPT(laogfxflt, lnPotPos, 1)
		lcPotSel = IIF( .NOT. EMPTY(laogfxflt(lnPotPos, 6)), laogfxflt(lnPotPos, 6), '')
		IF  .NOT. EMPTY(lcPotSel) .AND. USED(lcPotSel)
			llPotSelect = .T.
		ENDIF
	ENDIF
	lcrppath = ADDBS(ALLTRIM(lcrppath))
	lcrpHist = ADDBS(ALLTRIM(lcrpHist))
  lcrppath = loEnvironment.ResolveMappedDrive(lcrppath)
  lcrpHist = loEnvironment.ResolveMappedDrive(lcrpHist) 

	=gfOPenTable('customer','customer','SH')
ELSE  
*-SAB ----- [End]

	SET DATE BRITISH
	SET CENTURY ON
	IF  .NOT. llogfltch
		RETURN
	ENDIF
	llWhSelect = .F.
	lcWhSel = ''
	lnWhPos = ASCAN(loogscroll.laogfxflt, "WAREHOUS.CWARECODE")
	IF lnWhPos>0
		lnWhPos = ASUBSCRIPT(loogscroll.laogfxflt, lnWhPos, 1)
		lcWhSel = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnWhPos, 6)), loogscroll.laogfxflt(lnWhPos, 6), '')
		IF  .NOT. EMPTY(lcWhSel) .AND. USED(lcWhSel)
			llWhSelect = .T.
		ENDIF
	ENDIF
	IF !llWhSelect
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please Select a Warehouse')
	    RETURN
	ENDIF
	llCustSelect = .F.
	lcCustSel = ''
	lnCustPos = ASCAN(loogscroll.laogfxflt, "CUSTOMER.ACCOUNT")
	IF lnCustPos>0
		lnCustPos = ASUBSCRIPT(loogscroll.laogfxflt, lnCustPos, 1)
		lcCustSel = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnCustPos, 6)), loogscroll.laogfxflt(lnCustPos, 6), '')
		IF  .NOT. EMPTY(lcCustSel) .AND. USED(lcCustSel)
			llCustSelect = .T.
		ENDIF
	ENDIF
	IF !llCustSelect
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please Select a Web Order Account')
	    RETURN
	ENDIF
	SELECT (lcWhSel)
	*-SAB ----- [Start]
	*IF RECCOUNT()>1
	LOCATE 
	COUNT FOR !DELETED() TO lnWarCnt
	IF lnWarCnt >1
	**-SAB ----- [End]
		= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Warehouse')
    	RETURN
	ENDIF
	GO TOP
	SELECT (lcCustSel)
	*-SAB ----- [Start]
	*IF RECCOUNT()>1
	LOCATE 
	COUNT FOR !DELETED() TO lnCustCnt
	IF lnCustCnt > 1
	*-SAB ----- [End]
		= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Web Order Account')
    	RETURN
	ENDIF
	IF EMPTY(lcrppath) .OR. ( .NOT. DIRECTORY(lcrppath))
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Invalid Import Directory")
		RETURN
	ENDIF
	lcrppath = ADDBS(ALLTRIM(lcrppath))
	IF EMPTY(lcrpHist) .OR. ( .NOT. DIRECTORY(lcrpHist))
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Invalid History Directory")
		RETURN
	ENDIF
	lcrpHist = ADDBS(ALLTRIM(lcrpHist))
	if lcrpHist == lcrppath
		= gfmodalgen('INM00000B00000', .F., .F., .F., "You must select different Directories")
		RETURN
	ENDIF
	IF empty(lcRpChg)
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Please Select Order Charge")
		RETURN
	ENDIF	
*-SAB ----- [Start]
ENDIF
*-SAB ----- [End]

	lctemphead = gftempname()
	lctempline = gftempname()
	lctempOrd = gftempname()
	lclogfile = gftempname()
	lcOrdChg = gftempname()
	lfcreattemp()
	lntax_rate = gfgetmemvar('M_TAX_RATE', gcact_comp)
&& dragons
	SET STEP ON
	lodbfOrdChg = CREATEOBJECT("RemoteTable", 'ordchg', '', 'ordchg', SET("DATASESSION"))
	lcSQL="SELECT cordchg, nordchg, [order] FROM ordchg WHERE [order]='xxx'"
	IF  NOT lodbfOrdChg.sqlrun(lcSQL)
		*-SAB ----- [Start]
		*= gfmodalgen('INM00000B00000', .F., .F., .F., "ordchg Array Update Failed")
		IF TYPE('lcXMLFileName') <> 'C'
		  = gfmodalgen('INM00000B00000', .F., .F., .F., "ordchg Array Update Failed")
		ENDIF
		*-SAB ----- [End]
		FCLOSE(lnFile)
		RETURN .F.
	ENDIF
	SELECT ordchg
	lnAria = CURSORGETPROP('ConnectHandle')
	lfSQLExec( lnAria, 'BEGIN TRANSACTION' )
	IF SQLSETPROP(lnAria, 'Transactions', 2)<0  && manual
		= SQLDISCONNECT(lnAria)
		*-SAB ----- [Start]
		*= gfmodalgen('INM00000B00000', .F., .F., .F., 'Failed to Set Transaction')
		IF TYPE('lcXMLFileName') <> 'C'
		  = gfmodalgen('INM00000B00000', .F., .F., .F., 'Failed to Set Transaction')
		ENDIF
		*-SAB ----- [End]
		RETURN .F.
	ENDIF
	= AFIELDS(latempstru)
	= gfcrttmp(lcOrdChg, @latempstru, , "", .T.)
&& dragons
	=gfopentable('StyleUpc', 'STYUPCN')
	=gfOpenFile(oariaapplication.datadir+'stydye', oariaapplication.datadir+'stydye', 'SH')
	=gfOpenFile(oariaapplication.datadir+'ORDHDR', oariaapplication.datadir+'ORDHDR', 'SH')
	=gfOpenFile(oariaapplication.datadir+'ORDLINE', oariaapplication.datadir+'ORDLINE', 'SH')
	STORE 0 TO lnIp
	DIMENSION laProcessed[1]
	lcSkeleton=lcrppath+'*.csv'
	lnFnumber = ADIR(laFiles, lcSkeleton)  && Create array
	IF lnFnumber=0
	    *-SAB ----- [Start]
            *= gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
            IF TYPE('lcXMLFileName') <>'C'
		= gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
	    ELSE
	      loAgent.UpdateRequestStatus(lcRequestID, 6, "No Files to Process", ClientID)
	    ENDIF
  	    *-SAB ----- [End]
	    RETURN
	ENDIF
	ASORT(laFiles)
	oRE = CreateObject("VBScript.RegExp")
	FOR nCount = 1 TO lnFnumber
		oRE.Pattern = "^sale\.[0-9]{8}\.aria\.[0-9]+\.csv$"
		IF oRE.test(LOWER(laFiles(nCount,1)))
			lcFile=lcrppath+laFiles(nCount,1)
		    *-SAB ----- [Start]
		    *WAIT WINDOW NOWAIT lcFile
		    IF TYPE('lcXMLFileName') <>'C'
			WAIT WINDOW NOWAIT lcFile
	        ELSE
	          loProgress.Percent = (nCount /lnFnumber)
	          loProgress.DESCRIPTION =  lcFile
	          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
	        ENDIF
	        *-SAB ----- [End]
			
			SELECT (lctemphead)
			APPEND FROM (lcFile) FOR worder<>'order_id' AND qty>0 DELIMITED
			lnIp=lnIp+1
			DIMENSION laProcessed[lnIp]
			laProcessed[lnIp]=lcFile
			LOOP
		ENDIF
	ENDFOR
	RELEASE oRE
	*-SAB ----- [Start]
	*WAIT CLEAR
	IF TYPE('lcXMLFileName') <> 'C'
	WAIT CLEAR
	ENDIF
	*-SAB ----- [End]
	IF (.NOT. EMPTY(lctemphead) .AND. USED(lctemphead))
		SELECT (lctemphead)
		SCAN
			IF EMPTY(&lctemphead..order_date)
				REPL order_date WITH DTOC(DATE()) IN (lcTempHead)
			ENDIF
*!*				REPL store WITH '' IN (lcTempHead)
			IF EMPTY(&lctemphead..sku)
				lffilllog("UPC not found "+alltrim(&lctemphead..sku),&lctemphead..worder)
				LOOP
			ENDIF
			IF gfseek(LEFT(&lctemphead..sku,6)+SUBSTR(&lctemphead..sku,7,5)+SUBSTR(&lctemphead..sku,12,1), 'STYLEUPC')
				IF !SEEK(padr(&lctemphead..worder,12,' ')+StyleUPC.Style,lcTempLine)
					SELECT (lcTempLine)
					APPEND BLANK
					REPL wOrder WITH &lctemphead..worder, style WITH styleupc.style IN (lcTempLine)
				ENDIF
				SELECT (lcTempLine)
				lcSize = ALLTRIM(StyleUPC.Size)
				lnPrice=IIF(UPPER(ALLTRIM(&lctemphead..b_country))='GB',ROUND((&lctemphead..price*100)/(100+lntax_rate),2),&lctemphead..price)
				REPL qty&lcsize WITH &lctemphead..Qty, price WITH lnPrice IN (lcTempLine)
			ELSE
				lffilllog("UPC not found "+alltrim(&lctemphead..sku),&lctemphead..worder)
			ENDIF
		ENDSCAN
	ENDIF

*!*		SELECT (lcTempline)
*!*		copy to z:\ordersin\lctempline && dragons
	SELECT (lcTempHead)
	DELETE FROM (lcTempHead) WHERE worder IN (SELECT DISTINCT worder FROM (lclogfile) WHERE !EMPTY(worder))

	SELECT (lcCustSel)
	GO TOP
	SELECT customer
	SET ORDER TO customer
	=gfSeek('M'+&lcCustSel..ACCOUNT, 'customer') && Get Main Account
*!*		lcSalesRep=customer.SalesRep

	store space(0) to lcStart, m.Order
	lcResult=chr(13)+'No Orders Created'

	IF ( .NOT. EMPTY(lctempline) .AND. USED(lctempline))
		= gfopentable('ORDLINE', 'ORDLINE', 'SH')
		= gfopentable('ORDHDR', 'ORDHDR', 'SH')
		= gfopentable('STYLE', 'STYLE', 'SH', 'STYLE')
		= gfopentable('NOTEPAD', 'NOTEPAD', 'SH')
*!*			= gfseek('M'+&lcCustSel..account, 'CUSTOMER', 'CUSTOMER')
*!*			m.rep1 = customer.salesrep
*!*			m.comm1 = customer.comm
*!*			m.comm2 = customer.comm2
*!*			m.rep2 = customer.rep2
*!*			m.ctermcode = customer.ctermcode
*!*			m.gl_sales = customer.cslsgllink
*!*			m.link_code = customer.link_code
*!*			m.priority = customer.priority
*!*			m.shipvia = customer.shipvia
*!*			m.spcinst = customer.spcinst
*!*			m.buyer = customer.buyer
		SELECT (lctemphead)
		SCAN
			IF !SEEK(&lctemphead..wOrder, lcTempOrd)
				llvalidqty = .T.
				llvalidstyle = .T.
				llvalidacc = .T.
				llvalidstore = .T.
				llvalidcnt = .T.
				llvalidprice = .T.
				llvalidcurr = .T.
				llvalidrate = .T.
				llAccount=.F.
				if gfSeek('M'+upper(alltrim(&lctemphead..store)), 'customer') && If account in file exists as customer use it, else use entered Account
					llAccount=.T.
				else
					=gfSeek('M'+&lcCustSel..ACCOUNT, 'customer') && Get Main Account
				endif
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
				SELECT (lcTempOrd)
				APPEND BLANK
				REPL wOrder WITH &lctemphead..wOrder IN (lcTempOrd)
				SELECT (lctemphead)
				IF cCurrcode<>oariaapplication.basecurrency
					= gfopenfile(oariaapplication.syspath+'SYCCURR', 'CCURRCODE', 'SH')
					SELECT (lctemphead)
					IF  .NOT. SEEK(cCurrcode, 'SYCCURR')
						llvalidcurr = .F.
						lffilllog("Invalid Currency Code "+cCurrcode,&lctemphead..worder)
					ENDIF
					lnunit = 0
					lnnexrate = 0
					lnnexrate = gfchkrate('lnUnit', cCurrcode, ttod(ctot(strtran(&lctemphead..order_date,' ','T'))), .F.)
					SELECT (lctemphead)
					IF lnnexrate=0
						llvalidrate = .F.
						lffilllog("Invalid Currency Exchange Rate "+cCurrcode,&lctemphead..worder)
					ENDIF
				ENDIF
				=SEEK(&lctemphead..worder,lctempline)
				SELECT (lctempline)
				SCAN REST WHILE worder+STYLE = &lctemphead..worder
					llvalidstyle = llvalidstyle .AND. gfseek(style, 'Style', 'Style')
					IF  .NOT. llvalidstyle
						lffilllog("Invalid Style No."+STYLE,&lctemphead..worder)
					ENDIF
				ENDSCAN
				IF  .NOT. llvalidqty .OR.  .NOT. llvalidstyle .OR.  .NOT. llvalidacc .OR.  .NOT. llvalidstore .OR.  .NOT. llvalidcnt .OR.  .NOT. llvalidprice .OR.  .NOT. llvalidcurr .OR.  .NOT. llvalidrate
					SELECT (lctemphead)
					DELETE
					LOOP
				ELSE
					STORE '' TO m.cclass, m.ccurrcode, m.cdivision, m.cordtype, m.ctermcode, m.custpo, m.order, m.cwarecode, m.gl_sales, m.link_code, m.multi, m.note1, m.priority, m.season, m.shipvia, m.spcinst, m.status, m.stname
					STORE .F. TO m.alt_shpto, m.lhasnotes, m.multipo, m.lfromweb
					STORE '' TO m.bulk, m.creorder, m.phone
					STORE 0 TO m.comm1, m.comm2, m.nexrate, m.disc, m.appramt
					STORE 0 TO m.book, m.bookamt, lnlastlno, m.lastline, m.nexrate, m.ncurrunit, m.openamt, m.open, m.totamnt
					SELECT (lctemphead)
					SCATTER MEMO MEMVAR
					if llAccount
						m.store=''
					endif
					m.entered=ttod(ctot(strtran(&lctemphead..order_date,' ','T')))
					m.start=m.entered
					m.complete=m.entered
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
					m.order = 'W'+RIGHT(gfsequence('ORDER'),5)
					lcStart=iif(empty(lcStart),m.Order,lcStart)
					m.season = style.season
					SELECT (lctempline)
					COUNT REST WHILE worder+STYLE = &lctemphead..worder TO lnlastlno
					=SEEK(&lctemphead..worder,lctempline)
					SELECT (lctempline)
					SUM qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, (qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8)* price  REST WHILE worder+STYLE = &lctemphead..worder TO m.qty1,m.qty2, m.qty3,m.qty4 ,m.qty5 ,m.qty6 ,m.qty7 ,m.qty8 ,m.totamnt
					SELECT (lctemphead)
*!*						m.ngiftvalue=&lctemphead..gift_value
*!*						m.ncacctbal=&lctemphead..acc_bal
*!*						m.lgifttag=IIF(&lctemphead..gift_tag=1,.T.,.F.)
*!*						m.c3rdstatus=&lctemphead..td_status
*!*						m.CCV21=&lctemphead..avs_cv2
*!*						m.CCV22=&lctemphead..avs_addr
*!*						m.CCV23=&lctemphead..avs_postc
*!*						m.CCV24=&lctemphead..avs_result
*!*						m.Cpromo=&lctemphead..promo_code
*!*						m.c3rdman=&lctemphead..td_score
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
					m.note1=&lctemphead..b_phone
					m.status = 'O'
					m.stname = &lctemphead..s_name
					m.caddress1=&lctemphead..s_addr_1
					m.caddress2=&lctemphead..s_addr_2
					m.caddress3=&lctemphead..s_addr_3
					m.caddress4=&lctemphead..s_addr_4
					m.caddress5=&lctemphead..s_postcode
					m.phone = &lctemphead..b_phone
					m.alt_shpto = .T.
					m.mnotes=m.gnotes
					m.lhasnotes = IIF( .NOT. EMPTY(m.mnotes), .T., .F.)
					m.book = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
					m.bookamt = m.totamnt
					m.lastline = lnlastlno
					m.flag = ''
					m.open = m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
					m.openamt = m.totamnt
					m.account = customer.account
					SELECT ordhdr
					gfappend('ORDHDR', .T.)
					= gfadd_info('ORDHDR')
					IF &lctemphead..ship_cost>0
						m.nOrdChg=&lctemphead..ship_cost
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
			ENDIF
		ENDSCAN
		SELECT (lcOrdChg)
		SCAN
			lcSql="INSERT INTO [ordchg] ([cadd_time] ,[cadd_user] ,[cadd_ver] ,[cedit_time],[cedit_user],[cedt_ver],[cordchg] ,[dadd_date],[dedit_date] ,[invoice] ,[invoiced] ,[nordchg] ,[order])"
			lcSql=lcSql+" VALUES (CONVERT(CHAR(11),GETDATE(),108),'"+ALLTRIM(oariaapplication.user_id)+"','A40','','','','"+ALLTRIM(&lcOrdChg..cordchg)+"',GETDATE(),'1900-01-01','',0,"+ALLTRIM(STR(&lcOrdChg..nordchg,9,2))+",'"+&lcOrdChg..order+"')"
			IF !lfSQLExec(lnAria, lcSql)
				STORE 0 TO lnAria
			    *-SAB ----- [Start]
			    *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to add ordChg')
			    IF TYPE('lcXMLFileName') <> 'C'
			    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to add ordChg')
			    ENDIF
			    *-SAB ----- [End]
				RETURN .F.
			ENDIF
		ENDSCAN
&& dragons
                *-SAB ----- [Start]
		*WAIT WINDOW NOWAIT 'Updating SQL'
		IF TYPE('lcXMLFileName') <> 'C'
                  WAIT WINDOW NOWAIT 'Updating SQL'
                ENDIF 
                *-SAB ----- [Start]
		IF SQLCOMMIT(lnAria)<>1
			= AERROR(laError)
			SQLROLLBACK(lnAria)
			SQLDISCONNECT(lnAria)
			STORE 0 TO lnAria
			lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpwrm.sql')
			FPUTS(lnFileHandle, lcSQL)
			FOR n = 1 TO 7  && Display all elements of the array
				IF VARTYPE(laError(n))$'N,C'
					FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
				ENDIF
			ENDFOR
			FCLOSE(lnFileHandle)
		*-SAB ----- [Start]
		*= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to Update SQL')
		IF TYPE('lcXMLFileName') <> 'C'
			= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to Update SQL')
		ENDIF
		*-SAB ----- [End]
			RETURN .F.
		ENDIF
&& dragons
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
		= gfclosetable('CUSTOMER')
		= gfclosetable('WAREHOUS')
		if !empty(lcStart)
			lcResult=chr(13)+chr(13)+'Orders '+lcStart+' to '+m.Order+' Created'
		ENDIF
	ENDIF
&& dragons
*!*		IF SQLCOMMIT(lnAria)<>1
*!*			= AERROR(laError)
*!*			SQLROLLBACK(lnAria)
*!*			SQLDISCONNECT(lnAria)
*!*			STORE 0 TO lnAria
*!*			lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpwrm.sql')
*!*			FPUTS(lnFileHandle, lcSQL)
*!*			FOR n = 1 TO 7  && Display all elements of the array
*!*				IF VARTYPE(laError(n))$'N,C'
*!*					FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
*!*				ENDIF
*!*			ENDFOR
*!*			FCLOSE(lnFileHandle)
*!*			= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Unable to Update SQL')
*!*			RETURN .F.
*!*		ENDIF
&& dragons
	*-SAB ----- [Start]
	IF TYPE('laProcessed[1]') <> 'L'
    *-SAB ----- [End]
	lnFnumber = ALEN(laProcessed, 1)  && Count Files
	FOR nCount = 1 TO lnFnumber
		IF !EMPTY(laProcessed(nCount))
			lcName=ADDBS(lcRpHist)+JUSTFNAME(laProcessed(nCount))+'x'
			lcfile=laProcessed(nCount)
			COPY FILE (laProcessed(nCount)) TO (lcName)
			ERASE (laProcessed(nCount))
		ENDIF
	ENDFOR
	*-SAB ----- [Start]
	ENDIF
    *-SAB ----- [End]

	SET STEP ON
	IF RECCOUNT(lclogfile)>0
		SELECT (lclogfile)
		lcname=ADDBS(lcRpHist)+'WebErrors_'+TTOC(DATETIME(),1)+'.csv'
		COPY TO (lcname) DELIMITED
	    *-SAB ----- [Start]
        *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Errors found, please check Error File'+chr(13)+lcname)
        IF TYPE('lcXMLFileName') <>'C'
	    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Errors found, please check Error File'+chr(13)+lcname)
	    ELSE
	      *loAgent.UpdateRequestStatus(lcRequestID, 6, 'Errors found, please check Error File'+chr(13)+lcname, ClientID)      	      
		  *- Attache the Log file
    	  IF FILE(lcName)&&FILE(oAriaEnvironment.gcoutfile)
            loRequestObj= loAgent.GetRequest(lcRequestID, ClientID)
            IF !loRequestObj.CompleteNotification.Attachment.ContainsKey(lcName)  &&(oAriaEnvironment.gcoutfile)
              lcOldAttch = loRequestObj.CompleteNotification.ConvertAttachmentToString()
              lcOldAttch = lcOldAttch + IIF(EMPTY(lcOldAttch),'',',')+ lcName &&oAriaEnvironment.gcoutfile
              *loRequestObj.CompleteNotification.ConvertAttachmentFromString(lcOldAttch)
              loRequestObj.CompleteNotification.ConvertAttachmentFromString(lcName)
              loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
              loProxy.AddRequest(loRequestObj,ClientID)
            ENDIF
          ENDIF	
	      RETURN
        ENDIF
	    *-SAB ----- [End]
	    
	ENDIF

    *-SAB ----- [Start]
    *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Finished Processing'+lcResult)
    IF TYPE('lcXMLFileName') <>'C'
      = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Finished Processing'+lcResult)
    ELSE
      loProgress.Percent = 1
      loProgress.DESCRIPTION = "Finished Processing" + lcResult
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      
      loRequestObj= loAgent.GetRequest(lcRequestID, ClientID)      
      loRequestObj.CompleteNotification.ConvertAttachmentFromString("")
      loRequestObj.CompleteNotification.Body = "Finished Processing" + lcResult
      loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
      loProxy.AddRequest(loRequestObj,ClientID)
      
    ENDIF
    *-SAB ----- [End]
ENDFUNC




PROCEDURE lfvPath
	IF ALLTRIM(lcrppath)="?"
		lcrppath = GETDIR()
	ENDIF
ENDPROC


PROCEDURE lfvHist
	IF ALLTRIM(lcrpHist)="?"
		lcrpHist = GETDIR()
	ENDIF
ENDPROC



PROCEDURE lfwOgWhen
	loogscroll.parent.ogtoolbar.cntexternal.cmdemail.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdprint.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdexport.enabled = .F.
	IF EMPTY(lcrppath)
		lcrppath = oAriaApplication.defaultpath+'Download'
		lcrpHist = oAriaApplication.defaultpath+'History'
	ENDIF
ENDPROC



PROCEDURE lfChkStrct
	DIMENSION laitemseg[1]
	*-SAB ----- [Start]
	*= gfitemmask(@laitemseg)
	IF TYPE('lcXMLFileName') <> 'C'
	= gfitemmask(@laitemseg)
	ELSE
      ItemMask = CREATEOBJECT("GetItemMask")
      =ItemMask.Do(@laitemseg)
    ENDIF  
	*-SAB ----- [End]
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
	DIMENSION latempstru[31, 18]
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
	latempstru[lnindex, 3]=12
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
	latempstru[lnindex, 1]="sage_ref"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=30
	latempstru[lnindex, 4]=0
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
	latempstru[lnindex, 1]="price"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=9
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="qty"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=6
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="ship_meth"
	latempstru[lnindex, 2]='C'
	latempstru[lnindex, 3]=30
	latempstru[lnindex, 4]=0
	lnindex=lnindex+1
	latempstru[lnindex, 1]="ship_cost"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=9
	latempstru[lnindex, 4]=2
	lnindex=lnindex+1
	latempstru[lnindex, 1]="ship_taxr"
	latempstru[lnindex, 2]='N'
	latempstru[lnindex, 3]=6
	latempstru[lnindex, 4]=2
	= gfcrttmp(lcTemphead, @latempstru, , "", .T.)
	DIMENSION lalogstr[9, 4]
	lalogstr[1, 1] = "Worder"
	lalogstr[1, 2] = "C"
	lalogstr[1, 3] = 12
	lalogstr[1, 4] = 0
	lalogstr[2, 1] = "ret_no"
	lalogstr[2, 2] = "C"
	lalogstr[2, 3] = 12
	lalogstr[2, 4] = 0
	lalogstr[3, 1] = "Entered"
	lalogstr[3, 2] = "D"
	lalogstr[3, 3] = 8
	lalogstr[3, 4] = 0
	lalogstr[4, 1] = "Account"
	lalogstr[4, 2] = "C"
	lalogstr[4, 3] = 5
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
	= gfcrttmp(lclogfile, @lalogstr, "WORDER+ret_no", lclogfile, .F.)
	DIMENSION latempstru[1, 4]
	latempstru[1, 1] = "wOrder"
	latempstru[1, 2] = "C"
	latempstru[1, 3] = 12
	latempstru[1, 4] = 0
	= gfcrttmp(lctempord, @latempstru, "WORDER", lctempord, .T.)
ENDPROC
**
PROCEDURE lfFillLog
	PARAMETER lcmessage, lcordernum
	lcoldalias = SELECT()
	select (lclogfile)
	APPEND BLANK
	REPLACE worder WITH lcordernum, account WITH &lcCustSel..account, entered WITH ttod(ctot(strtran(&lctemphead..order_date,' ','T')))
	REPLACE desc WITH lcmessage
	SELECT (lcoldalias)
ENDPROC
*
*****
*****
FUNCTION lfSQLExec
	PARAMETER lnDB, lcCode
	lnRetVal = 0
	DO while .T.
		lnRetVal = SQLEXEC(lnDB, lcCode)
		IF lnRetVal<>0
			EXIT
		ENDIF
		LOOP
	ENDDO
	IF lnRetVal < 0 && Error
		= AERROR(laError)
		SQLROLLBACK(lnDB)
		SQLDISCONNECT(lnDB)
		lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpwrm.sql')
		FPUTS(lnFileHandle, lcCode)
		FOR n = 1 TO 7  && Display all elements of the array
			IF VARTYPE(laError(n))$'N,C'
				FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
			ENDIF
		ENDFOR
		FCLOSE(lnFileHandle)
		RETURN .F.
	ENDIF
	RETURN .T.
ENDFUNC
*****
*****
