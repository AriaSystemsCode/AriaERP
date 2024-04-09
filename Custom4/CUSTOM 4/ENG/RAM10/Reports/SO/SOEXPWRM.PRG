*! B610289,1 HIA 04/04/2013 Aria XP - covnert in house reports to run on the Request Builder, T20121129.0015
***********************************************************************************************************


*-SAB ----- []

*-SAB ----- [Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID
LOCAL lnhandle
*-SAB ----- [End]

SET DATE BRITISH
SET CENTURY ON

*-SAB ----- [Start]
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

  IF lcRpType<>'O'
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
  ENDIF
  lcrppath = loEnvironment.ResolveMappedDrive(lcrppath)
  
  *! B610289,1 HIA 04/04/2013 Aria XP - covnert in house reports to run on the Request Builder, T20121129.0015 [Start]
  lcrppath = ADDBS(lcrppath )
  *! B610289,1 HIA 04/04/2013 Aria XP - covnert in house reports to run on the Request Builder, T20121129.0015 [End]

ELSE
*-SAB ----- [End]
	
	
	IF  .NOT. llogfltch
		RETURN
	ENDIF
	IF EMPTY(lcrppath) .OR. ( .NOT. DIRECTORY(lcrppath))
		= gfmodalgen('INM00000B00000', .F., .F., .F., "Invalid Directory")
		RETURN
	ENDIF
	lcrppath = ADDBS(ALLTRIM(lcrppath))
	gfopentable('StyleUpc', 'StyleUpc')
	IF lcRpType<>'O'
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
	ENDIF
*-SAB ----- [Start]
ENDIF
*-SAB ----- [End]
    lcTempFile=oAriaApplication.workdir+gftempname()
    lcStyles=gftempname()
	DIMENSION latempstru[1, 18]
	latempstru = ''
	latempstru[1, 1] = 'cStyle'
	latempstru[1, 2] = 'C'
	latempstru[1, 3] = 19
	latempstru[1, 4] = 0
	= gfcrttmp(lcStyles, @latempstru, , "", .F.)
    lcUPC=gftempname()
	DIMENSION latempstru[2, 18]
	latempstru = ''
	latempstru[1, 1] = 'cStyle'
	latempstru[1, 2] = 'C'
	latempstru[1, 3] = 19
	latempstru[1, 4] = 0
	latempstru[2, 1] = 'cSize'
	latempstru[2, 2] = 'C'
	latempstru[2, 3] = 5
	latempstru[2, 4] = 0
	= gfcrttmp(lcUPC, @latempstru, , "", .F.)
	
	IF lcRpType=='O'
		=gfopenFile(oariaapplication.datadir+'ORDHDR', ,'SH')
		SELECT order, custpo as order_id, ctrackref as number, gfcoddes(shipvia, 'SHIPVIA') AS carrier FROM ordHdr WHERE !EMPTY(custpo) AND !EMPTY(ctrackref) AND !lsenttoweb INTO CURSOR lcOrders
		SELECT lcOrders
		if reccount()>0
			lnhandle = FCREATE(lcTempFile)
			IF lnhandle<0
			    *-SAB ----- [Start]
			    *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Cannot open output file. Cannot proceed.')
			    IF TYPE('lcXMLFileName') <>'C'
			      = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Cannot open output file. Cannot proceed.')
  				ELSE
				  loAgent.UpdateRequestStatus(lcRequestID, 6, 'Cannot open output file. Cannot proceed.', ClientID)
				ENDIF
				*-SAB ----- [End]
				RETURN .F.
			ENDIF
			lcExpr='order_id,carrier,number'
			FPUTS(lnhandle, lcexpr)
			SELECT lcOrders
			scan
				lcExpr=lfProcField(order_id)+','+lfProcField(carrier)+','+lfProcField(number)
				FPUTS(lnhandle, lcexpr)
			endscan
		else
            *-SAB ----- [Start]
			*= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'No Records Exported')
			IF TYPE('lcXMLFileName') <>'C'
			  = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'No Records Exported')
			ELSE
  			  loAgent.UpdateRequestStatus(lcRequestID, 6, 'No Records Exported', ClientID)
			ENDIF
			*-SAB ----- [End]
			return .F.
		endif
		FCLOSE(lnhandle)
	ELSE
		IF llWhSelect
			SELECT (lcWhSel)
			*-SAB ----- [Start]
			*IF RECCOUNT()>1
			  *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Warehouse')
    	    LOCATE 
		    COUNT FOR !DELETED() TO lnWCnt
		    IF lnWCnt > 1
			  IF TYPE('lcXMLFileName') <>'C'
			    = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Please select One Warehouse')
		  	  ELSE
			    loAgent.UpdateRequestStatus(lcRequestID, 6, 'Please select One Warehouse', ClientID)
			  ENDIF
   			*-SAB ----- [End]
		      RETURN
			ENDIF
			STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl
			= lfchkstrct()
			=gfopentable('notepad', 'notepad')
			SELECT notepad
			SET ORDER TO notepad
			=gfopentable('StyleUpc', 'StyleUpc')
			SELECT StyleUpc
			SET ORDER TO StyleUpc
			=gfopentable('scale', 'scale')
			SELECT Scale
			SET ORDER TO Scale
			=gfopentable('STYDYE', 'STYDYE')
			SELECT Stydye
			SET ORDER TO STYDYE
			=gfopentable('STYPRICE', 'STYPRICE')
			SELECT Styprice
			SET ORDER TO Styprice
			=gfopentable('Style', 'Style')
			SELECT Style
			=afields(laStyFile,'style') && DS
			SET RELATION TO 'S'+scale INTO scale
			SET RELATION TO 'F'+cstymajor INTO notepad ADDITIVE
			DO CASE
				CASE lcRpType=='A'
					lcFile=lcrppath+'Stock_Update.csv'
					lcExpr='sku,price,quantity,deleted'
				CASE lcRpType=='P'
					lcFile=lcrppath+'Stock.csv'
					lcExpr='sku,style_code,name,size,description_1,might_like_1,might_like_2,might_like_3,might_like_4,price'
				OTHERWISE
	    			*-SAB ----- [Start]
	    			*= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Invalid Option: '+lcRpType)
				    IF TYPE('lcXMLFileName') <>'C'
		  		      = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Invalid Option: '+lcRpType)
					ELSE
  					 loAgent.UpdateRequestStatus(lcRequestID, 6,  'Invalid Option: '+lcRpType, ClientID)
					ENDIF
					*-SAB ----- [End]
					RETURN .F.
			ENDCASE
			lnhandle = FCREATE(lcTempFile)
			FPUTS(lnhandle, lcexpr)
			IF lnhandle<0
	       		*-SAB ----- [Start]
			    *= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Cannot open output file. Cannot proceed.')
			    IF TYPE('lcXMLFileName') <>'C'
	   			  = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Cannot open output file. Cannot proceed.')
				ELSE
  				  loAgent.UpdateRequestStatus(lcRequestID, 6, 'Cannot open output file. Cannot proceed.', ClientID)
				ENDIF
				*-SAB ----- [End]
				RETURN .F.
			ENDIF
		    SELECT (lcWhSel)
		    GO TOP
	    	SELECT STYLE
	    	SCAN FOR lpublish
				= gfseek('S'+style.scale, 'Scale')
				= gfseek('F'+style.cstyMajor, 'Notepad')
				lnRows=ALINES(laStyNotes, notepad.mnotes)
				store space(0) to lcDesc
				for lnI=1 to lnRows
					lcDesc=lcDesc+' '+alltrim(laStyNotes[lnI])
				ENDFor
				if len(lcDesc)>320
					lcDesc=left(lcDesc,320)
				endif
				store space(0) to lcPrices
				store 0 to lnPI
				if gfSeek(style.style,'styPrice')
					select styPrice
					scan rest for styPrice.style=style.style
						lnPI=lnPI+1
						dime laPrices[lnPI,2]
						laPrices[lnPI,1]=alltrim(styprice.cCurrCode)
						laPrices[lnPI,2]=alltrim(str(styprice.nsugretpri))
					endscan
					for lnPI=1 to alen(laPrices,1)
						lcPrices=lcPrices+","+laPrices[lnPI,1]+","+laPrices[lnPI,2]
					endfor
				endif
	    		IF gfSeek(style.style+&lcWhSel..cWareCode, 'stydye')
	    			*-SAB ----- [Start]
	    			*WAIT WINDOW NOWAIT &lcWhSel..cWareCode+'/'+style.style
	    		    IF TYPE('lcXMLFileName') <>'C'
					  WAIT WINDOW NOWAIT &lcWhSel..cWareCode+'/'+style.style
					ELSE
					   loProgress.Percent = (RECNO() /RECCOUNT())
					   loProgress.DESCRIPTION = &lcWhSel..cWareCode+'/'+style.style
					   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
	    			ENDIF
					*-SAB ----- [End]
					FOR lni = 1 TO scale.cnt
						lci = STR(lni, 1)
						lcstyleupc = ''
						IF gfseek(style.style+lci, 'STYLEUPC') && AND stydye.Stk&lci-stydye.Alo&lci>0
							lcstyleupc = PADR(styleupc.cupcnum1+styleupc.cupcnum2+styleupc.cupcnum3, 13)
							DO CASE
								CASE lcRpType=='A'
									lcExpr = lfProcField(lcstyleupc)+','+ALLTRIM(STR(style.nwebprice,10,2))+','+IIF(stydye.Stk&lci-stydye.ord&lci>0,ALLTRIM(STR(stydye.Stk&lci-stydye.ord&lci)),'0')+','+IIF(style.lDelete,'1','0')+lcPrices
									IF style.lDelete
										INSERT INTO (lcStyles) VALUES (style.style)
									ENDIF
								CASE lcRpType=='P'
									IF !style.lDelete
									*-DS ----- [Start]
*!*											lcExpr=lcstyleupc+','+lfProcField(style.style)+','+lfProcField(style.desc1)+','+lfProcField(scale.sz&lci)+','+IIF(!EMPTY(lcDesc),lfProcField(lcDesc),'')
*!*											lcExpr=lcExpr+','+lfProcField(style.cmstyle1)+','+lfProcField(style.cmstyle2)+','+ALLTRIM(style.cmstyle3)+','+ALLTRIM(style.cmstyle4)+','+ALLTRIM(STR(style.nwebprice,10,2))+lcPrices
										lcExpr=lcstyleupc+','+lfProcField(style.style)+','+lfProcField(style.desc1)+','+lfProcField(scale.sz&lci)+','+IIF(!EMPTY(lcDesc),lfProcField(lcDesc),'')
										lcExpr=lcExpr+','+alltrim(gfCoddes(substr(style.style,lnclrposgl,lnclrlngl),'COLOR'))
										lcExpr=lcExpr+','+iif(ascan(laStyFile,'CMSTYLE1')>0,lfProcField(style.cmstyle1),'')+','+iif(ascan(laStyFile,'CMSTYLE2')>0,lfProcField(style.cmstyle2),'')
										lcExpr=lcExpr+','+iif(ascan(laStyFile,'CMSTYLE3')>0,lfProcField(style.cmstyle3),'')+','+iif(ascan(laStyFile,'CMSTYLE4')>0,lfProcField(style.cmstyle4),'')
										lcExpr=lcExpr+','+ALLTRIM(STR(style.nsugretpri,10,2))+lcPrices
									*-DS ----- [End]
									ENDIF
							ENDCASE
							FPUTS(lnhandle, lcexpr)
						ELSE
							SELECT (lcUPC)
							APPEND BLANK
							REPL cStyle WITH style.style, cSize WITH scale.sz&lcI
						ENDIF
					ENDFOR
				ENDIF
			ENDSCAN
			FCLOSE(lnhandle)
		ENDIF
	ENDIF
	DO CASE
		CASE lcRpType=='O'
			lcFile=lcrppath+'Tracking.csv'
		CASE lcRpType=='A'
			lcFile=lcrppath+'Stock_Update.csv'
		CASE lcRpType=='P'
			lcFile=lcrppath+'Stock.csv'
	ENDCASE
*!*		lcseq = ALLTRIM(gfsequence('CEXPWBSQ'))
*!*		lcseq = PADL(ALLTRIM(STR(VAL(lcseq))),6,'0')
*!*		lcfilename = 'OPSO'+oariaapplication.activecompanyid+lcseq
	if FILE(lcTempFile) 
		COPY FILE (lcTempFile) TO (lcFile)
		ERASE (lcTempFile)
	endif
	IF lcRpType=='O' and used('lcorders') and reccount('lcorders')>0
		UPDATE ordHdr SET lsenttoweb=.T. WHERE order IN (SELECT order FROM lcOrders)
	endif
	SET STEP ON
	IF lcRpType=='P'
	    UPDATE style SET lPublish=.F., lDelete=.F. WHERE style IN (SELECT cstyle FROM (lcStyles) GROUP BY cstyle)
	    IF RECCOUNT(lcUPC)>0
	    	lcFile=lcrppath+'UPC_'+ttoc(datetime(),1)+'.csv'
			IF FILE(lcFile)
				ERASE (lcFile)
			ENDIF
			SELECT (lcUPC)
			copy to (lcFile) delimited
			*-SAB ----- [Start]
			*= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Styles without UPCs'+CHR(13)+CHR(13)+'Please Check '+lcFile)
   		    IF TYPE('lcXMLFileName') <>'C'
			  = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'Styles without UPCs'+CHR(13)+CHR(13)+'Please Check '+lcFile)
			ELSE
			  *- Attache the Log file
    	      IF FILE(lcFile)&&FILE(oAriaEnvironment.gcoutfile)
                loRequestObj= loAgent.GetRequest(lcRequestID, ClientID)
                IF !loRequestObj.CompleteNotification.Attachment.ContainsKey(lcFile)  &&(oAriaEnvironment.gcoutfile)
                  lcOldAttch = loRequestObj.CompleteNotification.ConvertAttachmentToString()
                  lcOldAttch = lcOldAttch + IIF(EMPTY(lcOldAttch),'',',')+ lcFile &&oAriaEnvironment.gcoutfile
                  *loRequestObj.CompleteNotification.ConvertAttachmentFromString(lcOldAttch)
                  loRequestObj.CompleteNotification.ConvertAttachmentFromString(lcFile)
                  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
                  loProxy.AddRequest(loRequestObj,ClientID)
                ENDIF
              ENDIF	   
			ENDIF
			*-SAB ----- [End]
	    ENDIF
    ENDIF
	*-SAB ----- [Start]
	*WAIT CLEAR
	*= gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'End of Processing')
   	IF TYPE('lcXMLFileName') <>'C'
	  WAIT CLEAR
	  = gfmodalgen('TRM00000B00000', 'ALERT', '', '', 'End of Processing')
	ELSE
	  RETURN 
	ENDIF
	*-SAB ----- [End]
*CATCH TO ExpVar
*   loAgent.UpdateRequestStatus(lcRequestID, 6, ExpVar.MessAge, ClientID)
*ENDTRY 
ENDFUNC
**
PROCEDURE lfvExpTyp
	CLEARREAD()
ENDPROC
**
PROCEDURE lfvPath
	IF ALLTRIM(lcrppath)="?"
		lcrppath = GETDIR()
	ENDIF
ENDPROC
**
PROCEDURE lfwOgWhen
	loogscroll.parent.ogtoolbar.cntexternal.cmdemail.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdprint.enabled = .F.
	loogscroll.parent.ogtoolbar.cntprint.cmdexport.enabled = .F.
	IF EMPTY(lcrppath)
		lcrppath = 'c:\temp'
	ENDIF
ENDPROC
**
**
PROCEDURE GetArrayRef(tcArrayName)
	RETURN @&tcArrayName
ENDPROC
**
**
PROCEDURE lfsrLoc
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S'
       llchloc = .T.
       GOTO TOP IN warehous
    CASE lcparm='R'
       llclearloc = .F.
 ENDCASE
ENDPROC
**
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
**
FUNCTION lfProcField
	PARAMETER lcField
	lcField=ALLTRIM(lcField)
	if lcField=='N/A'
		RETURN ''
	endif
	DO CASE
		CASE EMPTY(lcField)
			RETURN ''
		CASE ATC('"',lcField)>0
			lcField=STRTRAN(lcField,'"','""')
			lcField='"'+lcField+'"'
		CASE ATC(',',lcField)>0
			lcField='"'+lcField+'"'
		CASE ATC(' ',lcField)>0
			lcField='"'+lcField+'"'
		OTHERWISE
			RETURN lcField
	ENDCASE
	RETURN lcField
ENDFUNC
**