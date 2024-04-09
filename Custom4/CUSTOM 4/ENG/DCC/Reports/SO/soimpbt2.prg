*!*		= gfopentable('contact', 'email', 'SH')
*!*		SELECT contact
*!*		SET ORDER TO email
	PARAMETER lcrequestid, lcxmlfilename, clientid
	gcrequestid = lcrequestid
	gcclientid = clientid
	IF TYPE('lcXMLFileName')='C'
		PRIVATE loagent
		loagent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
		PRIVATE loprogress
		loprogress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
		loprogress.percent = 0
		loprogress.description = "Opening Data Files..."
		loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
		LOCAL loenvironment
		loenvironment = goremotecall.getremoteobject("Aria.Environment.AriaEnviromentVariables")
		loenvironment.clientid = clientid
		LOCAL lccurrentprocedure
		lccurrentprocedure = loenvironment.aria40sharedpath
		loenvironment.connectionsrefresh()
		LOCAL lcrequestcompany, lcclientroot, lcenvoutput
		lcrequestcompany = loagent.getrequestcompany(lcrequestid, clientid)
		lcclientroot = loenvironment.aria40sharedpath
		lcenvoutput = loenvironment.getaria27companydataconnectionstring(lcrequestcompany)
		DO (lccurrentprocedure+"SRVPRGS\SY\ariamain.fxp") WITH lcrequestcompany, clientid, lccurrentprocedure, loenvironment
		oariaenvironment.xml.restorefromxml(FILETOSTR(lcxmlfilename), .T.)
		lcactivemod = 'SO'
		oariaenvironment.report.gcact_appl = lcactivemod
		oariaenvironment.activemoduleid = 'SO'
		oariaenvironment.requestid = lcrequestid
		PUBLIC gcact_appl
		gcact_appl = lcactivemod
		IF LEFT(gcdevice, 7)="PRINTER"
			oariaenvironment.gcdevice = "PRINTER"
		ELSE
			oariaenvironment.gcdevice = "FILE"
		ENDIF
		oariaenvironment.report.ccrorientation = 'P'
	ELSE
		loogscroll.ccrorientation = 'P'
	ENDIF
	local lni
	= gfopentable('styleupc')
	select styleupc
	set order to styupcn

	lcTmpOut = gftempname()
	lcTmpLines= gftempname()
	= lfbuildtmp()
	*C201612,3 TMI 04/30/2014 14:14 [Start] 
	IF TYPE('lcXMLFileName')<>'C'
	*C201612,3 TMI 04/30/2014 14:14 [End  ] 
	lcrppath=oAriaApplication.DefaultPath+'BT\Import\'
	lcrphist=oAriaApplication.DefaultPath+'BT\History\'
	*C201612,3 TMI 04/30/2014 14:15 [Start] 
	ELSE
   lcrppath='z:\btest\Import\'	
   lcrphist='z:\btest\History\'	
	ENDIF 
	*C201612,3 TMI 04/30/2014 14:15 [End  ] 
	STORE 0 TO lnip
	DIMENSION laprocessed[1]
	lcskeleton = lcrppath+'??????????.xml' && 10 characters
	lnfnumber = ADIR(lafiles, lcskeleton)
	IF lnfnumber=0
		IF TYPE('lcXMLFileName')<>'C'
			= gfmodalgen('INM00000B00000', .F., .F., .F., "No Files to Process")
		ENDIF
		RETURN
	ENDIF
	lcXMLOne='<?xml version = "1.0" encoding="Windows-1252" standalone="yes"?>'
	ASORT(lafiles)
	ore = CREATEOBJECT("VBScript.RegExp")
	FOR ncount = 1 TO lnfnumber
		ore.pattern = "[0-9]{10}\.xml$" && 10 digits
		IF ore.test(LOWER(lafiles(ncount, 1)))
			lcfile = lcrppath+lafiles(ncount, 1)
			lcXML=filetostr(lcFile)
			lnTotLines=alines(lcArray, lcXML)
			store space(0) to lcPOID, lcEmail, lcName, lcAddr, lcitemx, lcPartID, lcPartSite
			store .T. to llEAN
			for lnI=1 to lnTotLines

				if atc('<POID>', lcArray(lnI))>0
					lnstart=atc('>', lcArray(lnI))+1
					lnEnd=atc('</POID>', lcArray(lnI))
					if lnEnd>0
						lcPOID=alltrim(substr(lcArray(lnI), lnstart, lnend-lnstart))
					endif
					loop
				endif

				if atc('<PARTY_SITE_ID>', lcArray(lnI))>0
					lnstart=atc('>', lcArray(lnI))+1
					lnEnd=atc('</PARTY_SITE_ID>', lcArray(lnI))
					if lnEnd>0
						lcPartSite=alltrim(substr(lcArray(lnI), lnstart, lnend-lnstart))
					endif
					loop
				endif

				if upper(alltrim(lcArray(lnI)))='<PARTNER>'
					lnPart=lnI
					store space(0) to lcPartner
					loop
				endif
				if upper(alltrim(lcArray(lnI)))='</PARTNER>'
					for lnj=lnPart to lnI
						lcPartner=lcPartner+chr(10)+lcArray(lnJ)
					endfor
					if atc('Supplier', lcPartner)>0
						lcPartner=lcXMLOne+chr(10)+'<process_area>'+lcPartner+chr(10)+'</process_area>'
						lnLines=xmltocursor(lcPartner,"lcCursor",0)
						if lnLines>0
							select lcCursor
							locate
							lcPartID=lcCursor.partnrid
						endif
					endif
					loop
				endif

				if upper(alltrim(lcArray(lnI)))='<POORDERLIN>'
					lnPoStart=lnI
					store space(0) to lcPOLine, lcLine
					loop
				endif
				if upper(alltrim(lcArray(lnI)))='</POORDERLIN>'
					for lnj=lnPoStart to lnI
						if atc('<USERAREA>', lcArray(lnJ))=0
							lcPOLine=lcPOLine+chr(10)+lcArray(lnJ)
						else
							if atc('"PROMSHIP"', lcArray(lnj))>0
								store 0 to lnStart, lnEnd
								lnStart=atc('<DISTPROJECT>', lcArray(lnj))
								lnEnd=atc('</DISTPROJECT>', lcArray(lnj))
								lcLine=strtran(alltrim(substr(lcArray(lnJ),lnStart,lnEnd-lnstart+14)),'><','>'+chr(10)+'<')
								lcLine=lcXMLOne+chr(10)+'<process>'+chr(10)+lcLine+'</process>'
								lnLines=xmltocursor(lcLine,"lcCursor",0)
								if lnLines>0
									select lcCursor
									locate
									lcEmail=lcCursor.email
									lcName=lcCursor.requestor
								endif
							endif
						endif
					endfor
					lcPOLine=lcXMLOne+chr(10)+'<process>'+chr(10)+lcPOLine+'</process>'
					lnlines=xmltocursor(lcPOLine,"lcCursor",0)
					if lnlines>0
						select lcCursor
						locate
						if empty(lcCursor.itemx)
							IF TYPE('lcXMLFileName')<>'C'
								=gfmodalgen('INM00000B00000', .F., .F., .F., "No Ean, PO id:"+lcPoid+str(lni)+str(lnTotlines))
							endif
							loop
						endif
						if vartype(lcCursor.itemx)='N'
							lcEAN=str(lcCursor.itemx,13)
						else
							lcEAN=alltrim(lcCursor.itemx)
						endif
						if gfseek(lcEAN,'styleupc')
							select (lcTmpLines)
							set order to lcTmpLines
							IF !SEEK(PADR(lcpoid,16,' ')+styleupc.STYLE,lcTmpLines)
								SELECT (lcTmpLines)
								APPEND BLANK
								repl poid with lcPoid, style with styleupc.STYLE
							ENDIF
							SELECT (lcTmpLines)
							lcsize = ALLTRIM(styleupc.size)
							REPL qty&lcsize WITH qty&lcsize+val(lcCursor.quantity) IN (lcTmpLines)
						else
							IF TYPE('lcXMLFileName')<>'C'
								=gfmodalgen('INM00000B00000', .F., .F., .F., "Ean not found:"+lcEAN+chr(13)+"PO id:"+lcPoid+str(lni)+str(lnTotlines))
							endif
						endif
						lcAddr=lcCursor.notes
					endif
					loop
				endif

			endfor

			select (lcTmpOut)
			append blank
			repl File with justfname(lcfile), POID with lcPOID, partnerid with alltrim(str(lcPartID)), email with lcEmail, stname with lcname siteid with lcPartSite in (lcTmpOut)
			lcAddr=strtran(lcAddr,'NoteToSupplier=','')
			lnLines=alines(laAddr,lcAddr,1,',')
			if lnLines>0
				repl cAddress5 with laAddr(lnLines) phone with laAddr(1) in (lcTmpOut)
				for lni=2 to lnLines
					do case
						case lni=2
							repl cAddress1 with laAddr(2) in (lcTmpOut)
						case lni=3
							repl cAddress2 with laAddr(3) in (lcTmpOut)
						case lni=4
							repl cAddress3 with laAddr(4) in (lcTmpOut)
						case lni=5
							repl cAddress4 with laAddr(5) in (lcTmpOut)
					endcase
				endfor
			endif
			do case
				case empty(lcPOID)
					repl Process with 'POID not in file', passed with .F. in (lcTmpOut)
				case empty(lcPartSite)
					repl Process with 'No Partner Site ID in file', passed with .F. in (lcTmpOut)
				case empty(lcPartID)
					repl Process with 'No Partner ID in file', passed with .F. in (lcTmpOut)
				otherwise
					repl passed with .T. in (lcTmpOut)
			endcase
			lnip = lnip+1
			DIMENSION laprocessed[lnip]
			laprocessed[lnip] = lcfile
			
		ENDIF
	ENDFOR
	
*!*		select (lcTmpOut)
*!*		copy to "C:\Users\administrator.DCC\Documents\lcTmpOut"
*!*		select (lcTmpLines)
*!*		copy to "C:\Users\administrator.DCC\Documents\lcTmpLines"


	STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl
	= lfchkstrct()

	= gfopentable('ordhdr')
	= gfopentable('ordline')
	= gfopentable('stydye')
	= gfopentable('style')
	= gfopentable('piktkt')
	= gfopentable('customer')
	select style
	SET ORDER TO style	
	select stydye
	SET ORDER TO stydye
	select ordline
	set order to ordline
	select account, btdunsno from customer where !empty(btdunsno) into cursor lcbtAccts
	select customer
	set order to customer
	
	= gfopentable('syccomp', 'ccomp_id', 'SH')
	IF TYPE('lcXMLFileName')<>'C'
		SELECT syccomp
		SET ORDER TO ccomp_id
		=gfSeek(oariaapplication.activecompanyid,'syccomp')
		lcConnect="Driver={SQL Server};Server="+ALLTRIM(syccomp.cConServer)+";uid="+ALLTRIM(syccomp.cConUserID)+";pwd="+ALLTRIM(syccomp.cConPasWrd)+";database="+ALLTRIM(syccomp.cConDBName)
	ELSE
		lcconnect = oariaenvironment.activecompanyconstr
	ENDIF
	STORE SQLSTRINGCONNECT(lcConnect) TO lnAria
	IF lnAria < 1 && Error
		IF TYPE('lcXMLFileName')<>'C'
			= gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to open Aria SQL")
		endif
		RETURN .F.
	ENDIF

	lcPikProc='#'+gfTempName()
	lcCreate='CREATE PROCEDURE '+lcPikProc+' @piktkt char(6), @account char(5), @order char(6) as'
	lcSQL=" INSERT INTO [pickpack] ([piktkt] ,[pikdate] ,[cpstatus] ,[order] ,[cadd_user] ,[cadd_time] ,[dadd_date] ,[llok_stat] ,[clok_user] ,[dlok_date] ,[clok_time] ,[cedit_user]"
	lcSQL=lcSQL+" ,[cadd_ver] ,[dedit_date] ,[cedt_ver] ,[cedit_time] ,[account] ,[store] ,[cpickby] ,[dpickby] ,[cpicktime] ,[lcomppick] ,[chkedby] ,[dchkby] ,[cchkbytime] ,[cdespby] ,[ddespby]"
	lcSQL=lcSQL+" ,[cdesptime] ,[consgment] ,[action] ,[actiondate] ,[actiontime] ,[actionto] ,[acttomail] ,[notes] ,[noofcarton])"
	lcSQL=lcSQL+" VALUES (@piktkt ,GETDATE() ,'Not Issued' ,@Order ,'"+ALLTRIM(oariaapplication.user_id)+"' ,'' ,GETDATE() ,0 ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,''"
	lcSQL=lcSQL+" ,@account ,'' ,'' ,'1901-01-01' ,'' ,0 ,'' ,'1901-01-01' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'1901-01-01' ,'' ,'' ,'' ,'' ,0)"
	IF !lfSQLExec(lnAria, lcCreate+lcSQL)
		IF TYPE('lcXMLFileName')<>'C'
			=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to create PickPack proc")
		endif
		RETURN .F.
	ENDIF
	lcPriProc='#'+gfTempName()
	lcCreate='CREATE PROCEDURE '+lcPriProc+' @cCurrcod char(3), @pricCode char(6), @stydv char(19) as'
	lcSQL=" select [pricedv] from [cstprice] where cCurrcod=@cCurrcod and pricCode=@pricCode and stydv=@stydv"
	IF !lfSQLExec(lnAria, lcCreate+lcSQL)
		IF TYPE('lcXMLFileName')<>'C'
			=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to create cstPrice proc")
		endif
		RETURN .F.
	ENDIF

	select (lcTmpOut)
	scan for passed
		store space(0) to lcAccount
		select lcbtAccts
		scan for btdunsno=&lcTmpOut..siteid
			lcAccount=lcbtAccts.account
		endscan
		if empty(lcAccount)
			repl Process with 'Partner Site ID not found', passed with .F. in (lcTmpOut)
			loop
		endif
		=gfSeek('M'+lcAccount,'customer')
		if gfseek(&lcTmpOut..poid,lcTmpLines)
			STORE '' TO m.cclass, m.ccurrcode, m.cdivision, m.cordtype, m.custpo, m.order, m.cwarecode, m.multi, m.note1, m.season, m.status, m.stname
			STORE .F. TO m.alt_shpto, m.lhasnotes, m.multipo, m.lfromweb, llpicked
			STORE '' TO m.bulk, m.creorder, m.phone, m.store, m.cemail_ad1
			STORE 0 TO m.comm1, m.comm2, m.nexrate, m.disc, m.appramt
			STORE 0 TO m.book, m.bookamt, lnlastlno, m.lastline, m.nexrate, m.ncurrunit, m.openamt, m.open, m.totamnt
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
			m.entered=oariaapplication.systemdate
			m.start = m.entered
			m.complete = m.entered
			m.ccurrcode=oariaapplication.basecurrency
			m.ncurrunit = 1
			m.nexrate = 1
			m.lfromweb = .T.
			m.disc_pcnt = 0.00 
			m.ppQty = 0.00 
			m.appramt = 0
			m.shipments = 0
			=SEEK(&lcTmpOut..poid,lcTmpLines)
			=gfseek(&lcTmpLines..STYLE,'Style','Style')
			m.cdivision = style.cdivision
			m.order = gfsequence('ORDER')
			m.season = style.season
			SELECT (lcTmpLines)
			COUNT REST WHILE poid+STYLE = &lcTmpOut..poid TO lnlastlno
			=SEEK(&lcTmpOut..poid,lcTmpLines)
			SELECT (lcTmpLines)
			SUM qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8 REST WHILE poid+STYLE = &lcTmpOut..poid TO m.qty1,m.qty2, m.qty3,m.qty4 ,m.qty5 ,m.qty6 ,m.qty7 ,m.qty8
			SELECT (lcTmpOut)
			m.multipo = .F.
			m.bulk = 'N'
			m.creorder = 'N'
			m.cordtype = 'O'
			m.custpo = &lcTmpOut..poid
			m.cContRef = &lcTmpOut..POID
			m.cemail_ad1=&lcTmpOut..eMail
			m.cwarecode = 'DCC'
			m.multi = 'N'
			m.note1=&lcTmpOut..phone
			m.status = IIF(customer.status=='H', 'H', 'O')
			m.stname = &lcTmpOut..stname
			m.caddress1=&lcTmpOut..caddress1
			m.caddress2=&lcTmpOut..caddress2
			m.caddress3=&lcTmpOut..caddress3
			m.caddress4=&lcTmpOut..caddress4
			m.caddress5=&lcTmpOut..caddress5
			m.phone = &lcTmpOut..phone
			m.alt_shpto = .T.
			m.mnotes = ''
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
			store .T. to llPicked
			SELECT (lcTmpLines)
			=SEEK(&lcTmpOut..poid,lcTmpLines)
			lnline = 1
			SCAN REST WHILE poid+STYLE = &lcTmpOut..poid
				STORE 0 TO m.book1, m.book2, m.book3, m.book4, m.book5, m.book6, m.book7, m.book8
				STORE 0 TO m.comm1, m.comm2, lnRecs, m.gros_price, m.price
				SCATTER MEMO MEMVAR
				m.piktkt = ''
				m.cwarecode = 'DCC'
				m.cordtype = 'O'
				m.custpo = &lcTmpOut..poid
				=gfseek(&lcTmpLines..STYLE,'Style','Style')
				lcSQL="EXEC "+lcPriProc+" '"+customer.cCurrcode+"','"+customer.pricCode+"','"+&lcTmpLines..STYLE+"'"
				IF !lfSQLExec(lnAria, lcSQL, 'lcCstPrice')
					IF TYPE('lcXMLFileName')<>'C'
						=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to get cstPrice")
					endif
					RETURN .F.
				ENDIF
				if lnRecs>0
					select lcCstPrice
					locate
					store lcCstPrice.pricedv to m.gros_price, m.price
				endif
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
				repl ordhdr.bookamt with ordhdr.bookamt+(m.totbook*m.price) ordhdr.openamt with ordhdr.openamt+(m.totbook*m.price)
				m.lineno = lnline
				lnline = lnline+1
				m.totpik = 0
				m.picked=.F.
				m.pikdate={}
				FOR lni = 1 TO 8
					lci = STR(lni, 1)
					m.pik&lci=0
				endfor
				SELECT style
				IF SEEK(&lcTmpLines..STYLE)
					REPLACE ord1 WITH ord1+m.book1
					REPLACE ord2 WITH ord2+m.book2
					REPLACE ord3 WITH ord3+m.book3
					REPLACE ord4 WITH ord4+m.book4
					REPLACE ord5 WITH ord5+m.book5
					REPLACE ord6 WITH ord6+m.book6
					REPLACE ord7 WITH ord7+m.book7
					REPLACE ord8 WITH ord8+m.book8
					REPLACE totord WITH totord+m.totbook
				ENDIF
				SELECT stydye
				IF SEEK(&lcTmpLines..STYLE+'DCC')
					REPLACE ord1 WITH ord1+m.book1
					REPLACE ord2 WITH ord2+m.book2
					REPLACE ord3 WITH ord3+m.book3
					REPLACE ord4 WITH ord4+m.book4
					REPLACE ord5 WITH ord5+m.book5
					REPLACE ord6 WITH ord6+m.book6
					REPLACE ord7 WITH ord7+m.book7
					REPLACE ord8 WITH ord8+m.book8
					REPLACE totord WITH totord+m.totbook
					FOR lni = 1 TO 8
						lci = STR(lni, 1)
						lnqty=stydye.stk&lci-stydye.alo&lci
						IF m.book&lci>0
							IF lnQty<m.book&lci
								store .F. to llPicked
							endif
						endif
					endfor
				else
					IF TYPE('lcXMLFileName')<>'C'
						wait window "not Found"
					endif
				ENDIF
				SELECT ordline
				gfappend('ORDLINE', .T.)
				= gfadd_info('ORDLINE')
			ENDSCAN
			IF llpicked
				lcPiktkt = gfsequence('PIKTKT')
				=gfSeek('O'+ordhdr.order, 'Ordline')
				select ordline
				scan rest for order=ordhdr.order
					repl ordline.piktkt with lcpiktkt, ordline.picked with .T., ordline.pikdate with date()
					=gfSeek(ordline.style,'style')
					=gfSeek(ordline.style+ordline.cWareCode,'stydye')
					FOR lni = 1 TO 8
						lci = STR(lni, 1)
						IF ordline.book&lci>0
							REPL stydye.alo&lci WITH stydye.alo&lci+ordline.book&lci, stydye.totalo WITH stydye.totalo+ordline.book&lci
							REPL STYLE.alo&lci WITH STYLE.alo&lci+ordline.book&lci, STYLE.totalo WITH STYLE.totalo+ordline.book&lci
							repl ordline.pik&lci with ordline.book&lci, ordline.totpik with ordline.totpik+ordline.book&lci
						endif
					endfor
				endscan
				repl Process with 'Order Allocated', orderNo with ordhdr.order, poPik with lcPiktkt in (lcTmpOut)
				lcSQL="EXEC "+lcPikProc+" '"+lcpiktkt+"','"+ordhdr.account+"','"+ordhdr.order+"'"
				IF !lfSQLExec(lnAria, lcSQL)
					IF TYPE('lcXMLFileName')<>'C'
						=gfmodalgen('INM00000B00000', .F., .F., .F., "Failed to Insert PickPack")
					endif
					RETURN .F.
				ENDIF

				select ordhdr
				scatter memo memvar
				m.piktkt = lcpiktkt
				m.date = date()
				SELECT piktkt
				gfappend('PIKTKT', .T.)
				= gfadd_info('PIKTKT')
			else
				repl Process with 'Order Created', orderNo with ordhdr.order in (lcTmpOut)
			ENDIF
	&& end	

			SELECT ordhdr
			gftableupdate()
			SELECT ordline
			gftableupdate()
			SELECT style
			gftableupdate()
			SELECT stydye
			gftableupdate()
			SELECT piktkt
			gftableupdate()
		else
			repl Process with 'No valid details' in (lcTmpOut)
		ENDIF
	endscan


*!*		=sqldisconnect(lnaria)
*!*		store 0 to lnaria

	select (lcTmpOut)
	*C201612,3 TMI 04/30/2014 14:15 [Start] 
	IF TYPE('lcXMLFileName')<>'C'
	*C201612,3 TMI 04/30/2014 14:15 [End  ] 
	copy to (oAriaApplication.DefaultPath+'BT\History\BT_'+ttoc(datetime(),1)+'.csv') delimited
	*C201612,3 TMI 04/30/2014 14:15 [Start] 
	ELSE
	copy to ('z:\btest\History\BT_'+ttoc(datetime(),1)+'.csv') delimited
	ENDIF 
	*C201612,3 TMI 04/30/2014 14:15 [End  ] 
	IF TYPE('lcXMLFileName')<>'C'
		DO gfdispre WITH EVALUATE('lcFormName')
	else
		oariaenvironment.report.ccrorientation = 'P'
		oariaenvironment.report.oglastform = lcformname
		loprogress.description = "Printing Report..."
		loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
		PRIVATE loproxy
		loproxy = goremotecall.getremoteobject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
		oariaenvironment.report.print(oariaenvironment.report.oglastform)
	ENDIF

	store 0 to lnfnumber
	IF vartype(laProcessed[1])<>'L'
		lnfnumber = ALEN(laprocessed, 1)
		FOR ncount = 1 TO lnfnumber
			IF  .NOT. EMPTY(laprocessed(ncount))
				lcname = ADDBS(lcrphist)+JUSTFNAME(laprocessed(ncount))+'x'
				lcfile = laprocessed(ncount)
				COPY FILE (laprocessed(ncount)) TO (lcname)
				ERASE (laprocessed(ncount))
			ENDIF
		ENDFOR
	ENDIF
	
	IF TYPE('lcXMLFileName')<>'C'
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Number of Files Processed:"+str(lnfnumber))
	endif
	
****
****
PROCEDURE lfBuildTmp
	DIMENSION latempstru[16, 18]
	STORE '' TO latempstru
	STORE 0 TO lnindex
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'File'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 50
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'SiteID'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 25
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'POID'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 16
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'orderno'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'POpik'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'partnerid'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 10
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'stname'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'phone'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 16
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'cAddress1'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'cAddress2'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'cAddress3'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'cAddress4'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'cAddress5'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 30
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'eMail'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 100
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'passed'
	latempstru[lnindex, 2] = 'L'
	latempstru[lnindex, 3] = 1
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'Process'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 100
	latempstru[lnindex, 4] = 0
* --------------------------------------------------------
	= gfcrttmp(lcTmpOut, @latempstru, 'poid', lcTmpOut, .F.)
* --------------------------------------------------------
	DIMENSION latempstru[10, 18]
	STORE '' TO latempstru
	STORE 0 TO lnindex
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'POID'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 16
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'style'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 19
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty1'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty2'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty3'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty4'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty5'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty6'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty7'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'qty8'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
* --------------------------------------------------------
	= gfcrttmp(lcTmpLines, @latempstru, 'poid+style', lcTmpLines, .F.)
* --------------------------------------------------------
ENDPROC
**
function lfwRepWhen
endfunc
**
function lfgetCrt
endfunc
**
FUNCTION lfSQLExec
	PARAMETER lnDB, lcCode, lcResult
	if PCOUNT()<3
		lcResult=''
	endif
	lnRetVal = 0
	DO while .T.
		lnRetVal = SQLEXEC(lnDB, lcCode,lcResult,laCount)
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
		select (lcTmpOut)
		*C201612,3 TMI 04/30/2014 14:15 [Start] 
		IF TYPE('lcXMLFileName')<>'C'
		*C201612,3 TMI 04/30/2014 14:15 [End  ] 
		copy to (oAriaApplication.DefaultPath+'BT\History\BT_'+ttoc(datetime(),1)+'.csv') delimited
		*C201612,3 TMI 04/30/2014 14:15 [Start] 
		ELSE
		copy to ('z:\btest\History\BT_'+ttoc(datetime(),1)+'.csv') delimited
		ENDIF 
		*C201612,3 TMI 04/30/2014 14:15 [End  ] 
		lnFileHandle=FCREATE(ADDBS(oariaapplication.workdir)+'soimpbt2.sql')
		FPUTS(lnFileHandle, lcCode)
		FOR n = 1 TO ALEN(laError,1)  && Display all elements of the array
			IF VARTYPE(laError(n))$'N,C'
				FPUTS(lnFileHandle, IIF(VARTYPE(laError(n))='N',STR(laError(n)),laError(n)))
			ENDIF
		ENDFOR
		FCLOSE(lnFileHandle)
		RETURN .F.
	ENDIF
	lnRecs=laCount[1,2]
	return .T.
ENDFUNC
****
PROCEDURE lfChkStrct
	DIMENSION laitemseg[1]
	IF TYPE('lcXMLFileName')<>'C'
		= gfitemmask(@laitemseg)
	ELSE
		itemmask = CREATEOBJECT("GetItemMask")
		= itemmask.do(@laitemseg)
	ENDIF
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
*****
