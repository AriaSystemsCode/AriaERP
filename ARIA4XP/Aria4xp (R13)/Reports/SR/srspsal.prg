*:***************************************************************************
*: Program file  : SRSPSAL
*: Program desc. : SHIPMENTS BY SALESREP
*! Date          : 11/9/2006
*: System        : Aria Advantage Series.
*: Module        : SALES REPRESENTATIVES (SR)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : N000558 - T20060908.0029
*E303439,1 TMI 01/12/2014 Add a grand total to the sales rep shipment report [T20140103.0015 ] 
*B611564,1 AHH 05/07/2018 Error with Exporting report [T20180424.0009] 
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003]
*B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002]
*:***************************************************************************
*!*	_screen.Visible = .T.
*!*	ACTIVATE WINDOW trace
*!*	SUSPEND
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][ENd]
#Include r:\aria4xp\reports\SR\srspsal.h
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientID
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()
  PRIVATE loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)

  LOCAL lcCurrentProcedure
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  SET DEFAULT TO &lcCurrentProcedure.
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment

  SET DEFAULT TO &lcCurrentProcedure.

  =gfOpenFile('INVHDR','INVHDR')
  =gfOpenFile('INVLINE','INVLINE')
  =gfOpenFile('STYLE','STYLE')
  =gfOpenFile('SALESREP','SALESREP')
  =gfOpenFile('Customer','Customer')
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  oAriaEnvironment.REPORT.gcAct_Appl = 'SR'

  PUBLIC gcAct_Appl
  gcAct_Appl = 'SR'
  oAriaEnvironment.activeModuleID = 'SR'

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.REPORT.cCROrientation = 'P'

ENDIF
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]

*E303439,1 TMI 01/12/2014 18:00 [Start] set lcRpCurr to empty string in case of single currency
lcRpCurr = IIF(!llMultCurr,' ',lcRpCurr)
*E303439,1 TMI 01/12/2014 18:00 [End  ] 
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
*IF llOgFltCh
IF loogscroll.llOgFltCh
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	*-- llStyCom   variable that hold the salesrep commention
	llStyCom  = gfGetMemVar('M_STY_COM') = 'Y'
	*-- lcWorkfile   Variables that hold the temporary files name
	lcWorkfile   = loogscroll.gfTempName()
	llDonprnt=.F.
	lcCent = SET('CENT')
	SET CENT ON
	llAccFltr   = .F.
	lcMainFile = ''
	lcDatFltr  = ''
	lcCustFile = ''
	lcStylFile = ''
	lnMajorLen = LEN(gfItemMask("PM"))    && Define major len variable
	lcSeekHdr=" INVHDR.STATUS <> 'V' "
	*Customer Filter
	lcCustFile = ''
	lcCustFile = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
	llAccFltr   = !EMPTY(lcCustFile ) AND USED(lcCustFile ) AND RECCOUNT(lcCustFile ) > 0
	IF llAccFltr
	  SELECT (lcCustFile )
	  INDEX ON ACCOUNT  TAG (lcCustFile )
	ELSE
	  IF TYPE("lcCustFile ") = "C" AND USED(lcCustFile )
	    USE IN (lcCustFile )
	  ENDIF
	  lcCustFile = ''
	ENDIF

	*Invhdr Expression BEGIN
	* SalesRep Filter
	lcRepFltr= ''
	lcRepFltr= lfCheckFilter(1, 'SALESREP.REPCODE')
	llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
	IF llRepFltr
	  SELECT (lcRepFltr)
	  INDEX ON REPCODE TAG (lcRepFltr)
	  lcSeekHdr=lcSeekHdr+" AND (SEEK( INVHDR.REP1,'"+lcRepFltr+"') .OR. SEEK( INVHDR.REP2,'"+lcRepFltr+"'))"
	ELSE
	  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
	    USE IN (lcRepFltr)
	  ENDIF
	  lcRepFltr= ''
	ENDIF

	* Check if there is a filter on DIVISION
	lcCurName = lfCheckFilter(1, 'INVHDR.CDIVISION')
	lcDiv   = loOgScroll.gfTempName()
	llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
	IF llDiv
	  SELECT (lcDiv)
	  INDEX on CDivision TAG (lcDiv)
	  lcSeekHdr=lcSeekHdr+" AND SEEK(INVHDR.CDIVISION,'"+lcDiv+"')"
	ENDIF
	*Invhdr Expression END

	*INVLINE Expression BEGIN
	lcSeekLn=' .T. '
	LCSTYSEEK=''
	*-- Style Filter
	lcStylFile = lfCheckFilter(1, 'STYLE.CSTYMAJOR')
	llStyFltr   = !EMPTY(lcStylFile ) AND USED(lcStylFile ) AND RECCOUNT(lcStylFile ) > 0
	IF llStyFltr
	  SELECT (lcStylFile )
	  INDEX ON cstymajor TAG (lcStylFile )
	  LCSTYSEEK=" AND SEEK(SUBSTR(STYLE,1,lnMajorLen),'"+lcStylFile +"') "

	ELSE
	  IF TYPE("lcStylFile ") = "C" AND USED(lcStylFile )
	    USE IN (lcStylFile )
	  ENDIF
	  lcStylFile = ''
	ENDIF

	lcMainFile = IIF(llAccFltr   ,lcCustFile,IIF(!EMPTY(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,lcStylFile,'INVHDR'))
	IF lcMainFile <>lcStylFile
	   lcSeekLn=lcSeekLn+LCSTYSEEK
	ENDIF

	* Check if there is a filter on Style SEASON
	lcCurName = lfCheckFilter(1, 'INVHDR.SEASON')
	lcSea  = loOgScroll.gfTempName()
	llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
	IF llSea
	  SELECT (lcSea  )
	  INDEX on SEASON TAG (lcSea  )
	  lcSeekLn=lcSeekLn+" AND SEEK(SEASON,'"+lcSea+"') "
	ENDIF

	* Check if there is a filter on Style COLOR
	lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)')
	lcCol  = loOgScroll.gfTempName()
	llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
	IF llCol
	  SELECT (lcCol  )
	  INDEX on Color TAG (lcCol  )
	  lcSeekLn=lcSeekLn+" AND SEEK(SUBSTR(STYLE,lnClrPo,lnColorLen),'"+lcCol  +"')"
	ENDIF
	*INVLINE Expression END

	*Date Expression
	lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
	LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
	HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)
	IF !EMPTY(LDATE)
	  lcDatFltr=" BETWEEN(INVDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"'))"
	ELSE
	  IF  !EMPTY(HDATE)
	    lcDatFltr="  INVDATE<CTOD('"+HDATE+"')"
	  ENDIF
	ENDIF
	*Collecting the data
	= lfBuildTmp()
	DO lpCollData
	SET CENT &lcCent
	
	=lfAdjustCRSettings()
	SELECT (lcWorkfile )
	
	IF RECCOUNT(lcWorkfile )=0
	    llDonprnt=.T.
 	    *-- Message : There are no records to display...!
	    *--                < Ok >
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
	    IF TYPE('lcXMLFileName') <> 'C'
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	      =gfModalGen('TRM00052B40011','ALERT')
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
	    ENDIF
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	    RETURN

	ENDIF
  	IF USED(lcWorkfile )
	    USE IN (lcWorkfile )
  	ENDIF
  	*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
	IF TYPE('lcXMLFileName') <> 'C'
	*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
      =gfDispRe()
    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
    ELSE
      loProgress.Percent = 0.9
      loProgress.DESCRIPTION = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
      PRIVATE loProxy
      loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
      loogScroll   = oAriaEnvironment.REPORT
      oAriaEnvironment.REPORT.OGLastForm = lcRpName  
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.DESCRIPTION = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF
	*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	ELSE
	  IF llDonprnt
	    *-- Message : There are no records to display...!
	    *--                < Ok >
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
	    IF TYPE('lcXMLFileName') <> 'C'
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]

	    =gfModalGen('TRM00052B40011','ALERT')
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
	    ENDIF
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	    RETURN
	  ELSE
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
    	IF TYPE('lcXMLFileName') <> 'C'
	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
 	      =gfDispRe()
 	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
 	    ELSE
	      loProgress.Percent = 0.9
	      loProgress.DESCRIPTION = "Printing Report..."
	      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
	      PRIVATE loProxy
	      loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
	      loogScroll   = oAriaEnvironment.REPORT
	      oAriaEnvironment.REPORT.OGLastForm = lcRpName  
	      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
	      loProgress.Percent = 1.0
	      loProgress.DESCRIPTION = "Printing Report..."
	      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
 	    ENDIF
 	    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]  
	  ENDIF
ENDIF  && FILTER CHANGE


*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT
*! Date      : 04/29/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollData
*!*************************************************************
PROCEDURE lpCollData
SELECT INVHDR
LOCATE FOR Invoice = '' AND &lcSeekHdr
IF EOF()
  RETURN
ELSE

  SELECT INVHDR
  lcOrder = ORDER()
  IF llAccFltr
    SET ORDER TO TAG INVHDRA
  ENDIF
  SELECT INVLINE
  SET ORDER TO TAG IIF(llAccFltr    OR lcMainFile = 'INVHDR','INVLINE','INVLINES')
  SELECT (lcMainFile)
  IF llAccFltr
    *--if the user select specific Customer
    lcSeekHdr= lcSeekHdr+ IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr
    SCAN
      lcAccount=Account
        SELECT INVHDR
        =GFSEEK(lcAccount)
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
        IF TYPE('lcXMLFileName') = 'C'
          loProgress.Percent = 0.1
        ENDIF
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
        SCAN REST WHILE account+invoice = lcAccount FOR &lcSeekHdr
            lcInvoice=INVOICE
            SELECT INVLINE
            =GFSEEK(lcInvoice)
            SCAN REST WHILE invoice+STR(lineno,6) = lcInvoice FOR &lcSeekLn
             * N000862 ,1 Thabet Handle globalization issues [Start]
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT Lang_Selecting_records_for_report_Account +PADR(lcAccount,6) + Lang_for_Invoice +lcInvoice       WINDOW NOWAIT
		     *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
		     IF TYPE('lcXMLFileName') <> 'C'
		     *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
	            WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selecting_records_for_report_Account,oAriaApplication.GetHeaderText("Lang_Selecting_records_for_report_Account",AHEADERFILE)) +PADR(lcAccount,6) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_for_Invoice,oAriaApplication.GetHeaderText("Lang_for_Invoice",AHEADERFILE)) +lcInvoice       WINDOW NOWAIT
			 *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
			 ELSE
			   
               loProgress.DESCRIPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selecting_records_for_report_Account,oAriaApplication.GetHeaderText("Lang_Selecting_records_for_report_Account",AHEADERFILE)) +PADR(lcAccount,6) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_for_Invoice,oAriaApplication.GetHeaderText("Lang_for_Invoice",AHEADERFILE)) +lcInvoice    
               loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  			 ENDIF
 			 *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
* N000682,1 11/20/2012 MMT Globlization changes[End]

             * N000862 ,1 Thabet Handle globalization issues [END]
                =lfGetData()
            ENDSCAN
        ENDSCAN
    ENDSCAN
    
    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
    IF TYPE('lcXMLFileName') = 'C'
      loProgress.Percent = 0.7
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    ENDIF
    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]

  ELSE
    IF lcMainFile = 'INVHDR'
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
        IF TYPE('lcXMLFileName') = 'C'
          loProgress.Percent = 0.1
            loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        ENDIF
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
      *--if the user didn't select any Style(s) or  Customer(s) .
      lcSeekHdr= lcSeekHdr+ IIF(!EMPTY(lcDatFltr)," .AND. ","") + lcDatFltr
      SCAN FOR Invoice = '' AND &lcSeekHdr
        lcInvoice=INVOICE
        SELECT INVLINE
        =GFSEEK(lcInvoice)
        SCAN REST WHILE invoice+STR(lineno,6) = lcInvoice FOR &lcSeekLn
        * N000862 ,1 Thabet Handle globalization issues [Start]
           *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT Lang_Selecting_records_for_report_Invoice +lcInvoice WINDOW NOWAIT
		     *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
		     IF TYPE('lcXMLFileName') <> 'C'
		     *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
               WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selecting_records_for_report_Invoice,oAriaApplication.GetHeaderText("Lang_Selecting_records_for_report_Invoice",AHEADERFILE)) +lcInvoice WINDOW NOWAIT
             *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
			 ELSE
			   
               loProgress.DESCRIPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selecting_records_for_report_Invoice,oAriaApplication.GetHeaderText("Lang_Selecting_records_for_report_Invoice",AHEADERFILE)) +lcInvoice 
               loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
              
 			 ENDIF
 			 *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

           * N000862 ,1 Thabet Handle globalization issues [END]
            =lfGetData()
        ENDSCAN
      ENDSCAN
   *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
    IF TYPE('lcXMLFileName') = 'C'
      loProgress.Percent = 0.7
    ENDIF
   *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
      
    ELSE
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
        IF TYPE('lcXMLFileName') = 'C'
          loProgress.Percent = 0.1
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        ENDIF
        *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][ENd]
      *--if the user select specific Style(s)
      lcSeekLn= lcSeekLn+ IIF(!EMPTY(lcDatFltr) ," .AND. ","") + lcDatFltr
      SELECT (lcMainFile)
      SCAN
        lcStyle = PADR(ALLTRIM(cStyMajor),lnMajorLen)
        SELECT INVLINE
        =GFSEEK(lcStyle)
        SCAN REST WHILE style+invoice+STR(lineno,6) = lcStyle FOR &lcSeekLn
        * N000862 ,1 Thabet Handle globalization issues [Start]
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT Lang_Selecting_records_for_report_Style +lcStyle + Lang_for_Invoice +invoice WINDOW NOWAIT
         *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
		 IF TYPE('lcXMLFileName') <> 'C'
		 *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
           WAIT Lang_Selecting_records_for_report_Style +lcStyle + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_for_Invoice,oAriaApplication.GetHeaderText("Lang_for_Invoice",AHEADERFILE)) +invoice WINDOW NOWAIT
         *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
		ELSE
			   
               loProgress.DESCRIPTION =Lang_Selecting_records_for_report_Style +lcStyle + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_for_Invoice,oAriaApplication.GetHeaderText("Lang_for_Invoice",AHEADERFILE)) +invoice
               loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)           

         ENDIF
         *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

          * N000862 ,1 Thabet Handle globalization issues [END]
          IF GFSEEK(INVLINE.INVOICE,'INVHDR') AND &lcSeekHdr
            =lfGetData()
          ENDIF
        ENDSCAN
     ENDSCAN
    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
    IF TYPE('lcXMLFileName') = 'C'
      loProgress.Percent = 0.7
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    ENDIF
    *E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]

  ENDIF
ENDIF

ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : WAB - Walid A. Wahab
*! Date      : 03/05/2003
*! Purpose   : creat a record to the temp file
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetData()
*!*************************************************************
FUNCTION lfGetData

M.ACCOUNT=INVLINE.ACCOUNT
M.INVOICE=INVLINE.INVOICE
M.ORDER=INVLINE.ORDER
M.TOTQTY=INVLINE.TOTQTY
M.NAME=IIF( gfSEEK(IIF(EMPTY(INVHDR.STORE),'M'+ACCOUNT,'S'+INVHDR.ACCOUNT+INVHDR.STORE ),'CUSTOMER') , LEFT(CUSTOMER.STNAME,25) , '' )
M.CUSTPO=INVHDR.CUSTPO
M.REP11=INVHDR.REP1
M.REP22=INVHDR.REP2
M.COMM1=INVLINE.COMM1
M.COMM2=INVLINE.COMM2
M.STYLE=SUBSTR(INVLINE.STYLE,1,lnClrPo-2)
M.COLOR=SUBSTR(INVLINE.STYLE,lnClrPo,lnColorLen)
M.PRICE=INVLINE.PRICE
*E303439,1 TMI 01/12/2014 21:49 [Start] apply the gfAmntDisp function on the price field
IF llMultCurr
  LOCAL lnSlct
  lnSlct = SELECT(0)
  SELECT INVHDR
  M.PRICE = gfAmntDisp(INVLINE.PRICE,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
  SELECT (lnSlct)
ENDIF 
*E303439,1 TMI 01/12/2014 21:49 [End  ] 
M.INVDATE=INVHDR.INVDATE
M.DiscPcnt=INVHDR.DiscPcnt
M.PIECE=INVLINE.TOTQTY
M.TRDDISC=InvHdr.Trde_Disc
*E303439,1 TMI 01/12/2014 21:52 [Start] populate new field varaible
m.CCURRCODE = INVHDR.CCURRCODE
m.NCURRUNIT = INVHDR.NCURRUNIT 
m.NEXRATE = INVHDR.NEXRATE 
*E303439,1 TMI 01/12/2014 21:52 [End  ] 
*B611564,1 AHH 05/07/2018 Error with Exporting report [T20180424.0009][Start] 
*({@EXT}-({abc.discpcnt}*{@EXT}/100))* ( 1-{abc.trddisc}/ 100 )
m.NExtAmnt= INVLINE.TOTQTY*INVLINE.PRICE
m.NComm1Amnt=(((INVLINE.TOTQTY*INVLINE.PRICE)-(m.discpcnt*(INVLINE.TOTQTY*INVLINE.PRICE)/100))* ( 1-m.trddisc/ 100))*INVLINE.COMM1/100
m.NComm2Amnt=(((INVLINE.TOTQTY*INVLINE.PRICE)-(m.discpcnt*(INVLINE.TOTQTY*INVLINE.PRICE)/100))* ( 1-m.trddisc/ 100))*INVLINE.COMM2/100
*B611564,1 AHH 05/07/2018 Error with Exporting report [T20180424.0009][End]
DO CASE
 CASE EMPTY(INVHDR.REP1) AND EMPTY(INVHDR.REP2)
   M.REPCODE = ''
   INSERT INTO (lcWorkfile ) FROM MEMVAR

 CASE !EMPTY(INVHDR.REP1) AND !EMPTY(INVHDR.REP2)
   IF llRepFltr
     IF SEEK(INVHDR.REP1,lCRepFltr   )
       M.REPCODE = INVHDR.REP1
	     IF !EMPTY(M.REPCODE)
	       M.REPNAME = IIF( gfSEEK(M.REPCODE,'SALESREP') , SALESREP.NAME , '' )
	     ELSE
	       M.REPNAME=''
	     ENDIF
	     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][Start]
         IF SEEK(INVHDR.REP2,lCRepFltr   )
           M.REP22=""
           M.COMM2= 0 
           m.NComm2Amnt= 0
         ENDIF  
	     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][End]

       INSERT INTO (lcWorkfile ) FROM MEMVAR
     ENDIF
     IF SEEK(INVHDR.REP2,lCRepFltr   )
       M.REPCODE = INVHDR.REP2
	     IF !EMPTY(M.REPCODE)
	       M.REPNAME = IIF( gfSEEK(M.REPCODE,'SALESREP') , SALESREP.NAME , '' )
	     ELSE
	       M.REPNAME=''
	     ENDIF
	     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][Start]
         IF SEEK(INVHDR.REP1,lCRepFltr   )
           M.REP11=""
           M.COMM1= 0 
           m.NComm1Amnt= 0
         ENDIF  
 		 M.REP22=INVHDR.REP2
 		 M.COMM2=INVLINE.COMM2
     	 m.NComm2Amnt=(((INVLINE.TOTQTY*INVLINE.PRICE)-(m.discpcnt*(INVLINE.TOTQTY*INVLINE.PRICE)/100))* ( 1-m.trddisc/ 100))*INVLINE.COMM2/100
	     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][End]
       INSERT INTO (lcWorkfile ) FROM MEMVAR
     ENDIF
   ELSE
     M.REPCODE = INVHDR.REP1
     IF !EMPTY(M.REPCODE)
       M.REPNAME = IIF( gfSEEK(M.REPCODE,'SALESREP') , SALESREP.NAME , '' )
     ELSE
       M.REPNAME=''
     ENDIF
     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][Start]
     M.REP22=""
     M.COMM2= 0 
     m.NComm2Amnt= 0
     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][End]

     INSERT INTO (lcWorkfile ) FROM MEMVAR

     M.REPCODE = INVHDR.REP2
     IF !EMPTY(M.REPCODE)
       M.REPNAME = IIF( gfSEEK(M.REPCODE,'SALESREP') , SALESREP.NAME , '' )
     ELSE
       M.REPNAME=''
     ENDIF
     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][Start]
     M.REP11=""
     M.COMM1= 0 
     m.NComm1Amnt= 0
     M.REP22=INVHDR.REP2
 	 M.COMM2=INVLINE.COMM2
 	 m.NComm2Amnt=(((INVLINE.TOTQTY*INVLINE.PRICE)-(m.discpcnt*(INVLINE.TOTQTY*INVLINE.PRICE)/100))* ( 1-m.trddisc/ 100))*INVLINE.COMM2/100     
     *B612647,1 MMT 12/19/2022 SHIPMENTS BY SALESREP displays incorrect invoice Sales rep. commission if the same invoice has 2 sales reps.[T20221130.0002][End]

     INSERT INTO (lcWorkfile ) FROM MEMVAR
   ENDIF

 CASE !EMPTY(INVHDR.REP1) AND EMPTY(INVHDR.REP2)
   M.REPCODE = INVHDR.REP1
     IF !EMPTY(M.REPCODE)
       M.REPNAME = IIF( gfSEEK(M.REPCODE,'SALESREP') , SALESREP.NAME , '' )
     ELSE
       M.REPNAME=''
     ENDIF
   INSERT INTO (lcWorkfile ) FROM MEMVAR

*! Not a Logic case
*! I do not remeber if Invoice screen accept to handle rep1 and give value for rep2
 CASE EMPTY(INVHDR.REP1) AND !EMPTY(INVHDR.REP2)
   M.REPCODE = INVHDR.REP2
   INSERT INTO (lcWorkfile ) FROM MEMVAR
ENDCASE



*!*************************************************************
*! Name      : lfsrvSty
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : To set relation on or off when running the in
*!           : range function in the option grid.
*!*************************************************************
*! Called from : SRSPSAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE  IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE laItemSeg,lnCount
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnColorLen = LEN(laItemSeg[lnCount,3])
    lnClrPo    = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*- Update the color title variable
=lfNonMaj()



*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Mohamed Mohamed  (AMM)
*! Date      : 08/27/1998
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
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj
PRIVATE lnClrPo
llCodeF = .F.
lnClrPo = 0
lnNonMajPo = 0
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.

* Array to collect data about all segments in the style code structure
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' .AND. !llCodeF

    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
    lnFreeLen = LEN(lcNonMajPi)
  ENDIF

  *-- If you Find Color Type or Find previous Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      *-- Color position
      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      *-- Picture
      lcNonMajPi = laMajSeg[lnI,3]
      *-- NonMajor title
      lcColorTt = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(lcNonMajPi)
      EXIT
    ELSE
      llCodeF = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

*-- Compute Free/Color Items in Style Structure. [End]

* get the style major segement title
lcMajTtl =gfItemMask("HM")

lcMajPic = "@! " + gfItemMask("PM")

RETURN ''

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
*E303439,1 TMI 01/12/2014 21:28 [Start] add one more parameter
*DIMENSION loOgScroll.laCRParams[5,2]
DIMENSION loOgScroll.laCRParams[6,2]
*E303439,1 TMI 01/12/2014 21:28 [End  ] 

loOgScroll.lcOGLastForm ='SRSPSAL'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"
loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2]= Lang_SHIPMENTS_BY_SALESREP
loOgScroll.laCRParams[1,2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_SHIPMENTS_BY_SALESREP,oAriaApplication.GetHeaderText("Lang_SHIPMENTS_BY_SALESREP",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] = 'LCDEC'
loOgScroll.laCRParams[2,2]= lcRpDeciml

loOgScroll.laCRParams[3,1] = 'LAYOUT'
loOgScroll.laCRParams[3,2]= lcRpPrint

loOgScroll.laCRParams[4,1] = 'NWPAGE'
loOgScroll.laCRParams[4,2]= lcRpStart

loOgScroll.laCRParams[5,1] = 'llStyCom'
loOgScroll.laCRParams[5,2]= llStyCom

*E303439,1 TMI 01/12/2014 20:58 [Start] add a new parameter for the lcRpCurr variable
loOgScroll.laCRParams[6,1] = 'lcRpCurr'
loOgScroll.laCRParams[6,2]= lcRpCurr
*E303439,1 TMI 01/12/2014 20:58 [End  ] 


*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp
*B611564,1 AHH 05/08/2018 [Start] add a new fields to the laTempStru array
*E303439,1 TMI 01/12/2014 21:22 [Start] add a new fields to the laTempStru array
*DIMENSION laTempStru[20,18] ,laTempCOM[1,18]
*!*	DIMENSION laTempStru[23,18] ,laTempCOM[1,18]
*E303439,1 TMI 01/12/2014 21:22 [End  ] 
DIMENSION laTempStru[26,18] ,laTempCOM[1,18]
*B611564,1 AHH 05/08/2018 [Start] add a new fields to the laTempStru array
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
SELECT INVLINE
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][Start]
*=OGAFIELDS(@laTempCOM)
=AFIELDS(laTempCOM)
*E612545,1 MMT 04/28/2022 Automatic emailing of Shipments by Sales Rep. Report [T20220407.0003][End]
laTempStru[1,1]  = 'INVOICE'
laTempStru[2,1]  = 'ACCOUNT'
laTempStru[3,1]  = 'INVDATE'
laTempStru[4,1]  = 'STYLE'
laTempStru[5,1]  = 'PRICE '
laTempStru[6,1]  = 'ORDER'
laTempStru[7,1]  = 'COMM1'
laTempStru[8,1]  = 'COMM2'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 1 TO 9
  lnFldRow = ASCAN(laTempCOM,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCOM,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCOM[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCOM[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCOM[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)
laTempStru[9,1] = 'CUSTPO'
laTempStru[9,2] = 'C'
laTempStru[9,3] = 16
laTempStru[9,4] = 0

laTempStru[10,1] = 'REP11'
laTempStru[10,2] = 'C'
laTempStru[10,3] = 3
laTempStru[10,4] = 0

laTempStru[11,1] = 'REP22'
laTempStru[11,2] = 'C'
laTempStru[11,3] = 3
laTempStru[11,4] = 0

laTempStru[12,1] = 'REPCODE'
laTempStru[12,2] = 'C'
laTempStru[12,3] = 3
laTempStru[12,4] = 0

laTempStru[13,1] = 'NAME'
laTempStru[13,2] = 'C'
laTempStru[13,3] = 50
laTempStru[13,4] = 0

laTempStru[14,1] = 'STYNET1'
laTempStru[14,2] = 'N'
laTempStru[14,3] = 10
laTempStru[14,4] = 2

laTempStru[15,1] = 'STYNET2'
laTempStru[15,2] = 'N'
laTempStru[15,3] = 10
laTempStru[15,4] = 2

laTempStru[16,1] = 'DISCPCNT'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 10
laTempStru[16,4] = 2

laTempStru[17,1] = 'PIECE'
laTempStru[17,2] = 'N'
laTempStru[17,3] = 10
laTempStru[17,4] = 2

laTempStru[18,1] = 'COLOR'
laTempStru[18,2] = 'C'
laTempStru[18,3] = 10
laTempStru[18,4] = 0

laTempStru[19,1] = 'REPNAME'
laTempStru[19,2] = 'C'
laTempStru[19,3] = 30
laTempStru[19,4] = 0

laTempStru[20,1] = 'TRDDISC'
laTempStru[20,2] = 'N'
laTempStru[20,3] = 10
laTempStru[20,4] = 2

*E303439,1 TMI 01/12/2014 21:23 [Start] 
i = 20
i = i+1
laTempStru[i,1] = 'CCURRCODE'
laTempStru[i,2] = 'C'
laTempStru[i,3] = 3
laTempStru[i,4] =0

i = i+1
laTempStru[i,1] = 'NCURRUNIT'
laTempStru[i,2] = 'N'
laTempStru[i,3] = 4
laTempStru[i,4] =0

i = i+1
laTempStru[i,1] = 'NEXRATE'
laTempStru[i,2] = 'N'
laTempStru[i,3] = 9
laTempStru[i,4] =4
*E303439,1 TMI 01/12/2014 21:23 [End  ] 

*B611564,1 AHH 05/07/2018 Error with Exporting report [T20180424.0009][start]
i = i+1
laTempStru[i,1] = 'NExtAmnt'
laTempStru[i,2] = 'N'
laTempStru[i,3] = 13
laTempStru[i,4] =2

i = i+1
laTempStru[i,1] = 'NComm1Amnt'
laTempStru[i,2] = 'N'
laTempStru[i,3] = 13
laTempStru[i,4] =2

i = i+1
laTempStru[i,1] = 'NComm2Amnt'
laTempStru[i,2] = 'N'
laTempStru[i,3] = 13
laTempStru[i,4] =2

*B611564,1 AHH 05/07/2018 Error with Exporting report [T20180424.0009][End]

=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString


*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/12/2014
*! Purpose   : call the function that based on it we select the currecy option to show the amounts
*!*************************************************************
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*- End of lfvCurDisp.