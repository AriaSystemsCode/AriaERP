*:***************************************************************************
*: Program file  : SMCCODE
*: Program desc. : Codes report
*: For Report    : (SMCCODE.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : System Manager (SM)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: N000622,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[T20090805.0001]
*: N000622,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[T20100512.0026]
*: E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*! E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004]
*! E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media]
*! B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms
*! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13]
*! B611098,1 MMT 12/21/2015 Codes report prints incorrect related fields values[T20150925.0002 - Issue#63]
*:***************************************************************************

*: N000622,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[Start]
*!*	loogscroll.cCROrientation = 'L'
*!*	*N000622,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[Start]
*!*	IF oAriaApplication.MULTIINST
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\SM\SMCCODE.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\SM\SMCCODE.FXP
*!*	ELSE
*!*	  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
*!*	  DO lcSrvRpt+"SM\SMCCODE.FXP" WITH .F.,.F.
*!*	ENDIF
*!*	*N000622,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[End]
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10

*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS LCREQUESTID, LCXMLFILENAME, CLIENTID
*T20100512.0026 Hassan 2010 05 23 [END]


*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
IF TYPE('lcRequestID') = 'C' .AND. 'TEMP.TXT' $ UPPER(LCREQUESTID)
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][Start]
  *STRTOFILE("2.0.0.1", lcRequestID, .F.)
  STRTOFILE("3.0.0.0", lcRequestID, .F.)
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][End]
  RETURN
ENDIF
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC GCREQUESTID, GCCLIENTID
  GCREQUESTID = LCREQUESTID
  GCCLIENTID = CLIENTID

  PRIVATE LOAGENT
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  LOAGENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  PRIVATE LOPROGRESS
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  LOPROGRESS = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *loProgress = goRemoteCall.GetRemoteObject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  LOPROGRESS.PERCENT = 0
  LOPROGRESS.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL LOENVIRONMENT

  *E3028015 HIA Consider the client pathes after assign the code pathes [Begin]

  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *loEnvironment.ClientId = ClientId
  *loEnvironment.ConnectionsRefresh()
  *loEnvironment.GetAria27CompanyDataConnectionString(loAgent.GetRequestCompany(lcRequestID, ClientId))

  *T20100512.0026 Hassan 2010 05 23 [END]
  *LOCAL lcCurrentProcedure
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)

  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  LOENVIRONMENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  LOENVIRONMENT.CLIENTID = CLIENTID

  LOCAL LCCURRENTPROCEDURE
  *!* T20110801.0008 MAH 8/2/2011
  *--lcCurrentProcedure = STRTRAN(ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath)), UPPER("SQLDictionary\"), "", -1, 1, 1)
  LCCURRENTPROCEDURE =    LOENVIRONMENT.ARIA40SHAREDPATH
  *!* T20110801.0008 MAH 8/2/2011 End

  LOENVIRONMENT.CONNECTIONSREFRESH()

  *BADRAN
  LOCAL LCREQUESTCOMPANY, LCCLIENTROOT, LCENVOUTPUT
  LCREQUESTCOMPANY = LOAGENT.GETREQUESTCOMPANY(LCREQUESTID, CLIENTID)

  *!* T20110801.0008 MAH 8/2/2011
  *--lcClientRoot = STRTRAN(ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath)), UPPER("SQLDictionary\"), "", -1, 1, 1)
  LCCLIENTROOT = LOENVIRONMENT.ARIA40SHAREDPATH
  *!* T20110801.0008 MAH 8/2/2011 End

  *BADRAN loEnvironment.GetAria27CompanyDataConnectionString(loAgent.GetRequestCompany(lcRequestID, ClientId))
  LCENVOUTPUT = LOENVIRONMENT.GETARIA27COMPANYDATACONNECTIONSTRING(LCREQUESTCOMPANY)
  *E3028015 HIA Consider the client pathes after assign the code pathes [End]


  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
  *BADRAN DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientId), ClientId

  DO (LCCURRENTPROCEDURE + "SRVPRGS\SY\ariamain.fxp") WITH LCREQUESTCOMPANY , CLIENTID, LCCURRENTPROCEDURE, LOENVIRONMENT
  *T20100512.0026 Hassan 2010 05 23 [END]

  OARIAENVIRONMENT.XML.RESTOREFROMXML(FILETOSTR(LCXMLFILENAME),.T.)
  OARIAENVIRONMENT.REPORT.GCACT_APPL = LCACTIVEMOD
  OARIAENVIRONMENT.ACTIVEMODULEID = 'SM'
  OARIAENVIRONMENT.REQUESTID = LCREQUESTID

  PUBLIC GCACT_APPL
  GCACT_APPL = LCACTIVEMOD

  IF LEFT(GCDEVICE, 7) = "PRINTER"
    OARIAENVIRONMENT.GCDEVICE = "PRINTER"
  ELSE
    OARIAENVIRONMENT.GCDEVICE = "FILE"
  ENDIF
  OARIAENVIRONMENT.REPORT.CCRORIENTATION = 'L'
ELSE
  LOOGSCROLL.CCRORIENTATION = 'L'
ENDIF

IF USED('SYCCOMP')
  USE IN SYCCOMP
  GFCLOSETABLE('SYCCOMP')
ENDIF
IF USED('SYDFIELD')
  USE IN SYDFIELD
  GFCLOSETABLE('SYDFIELD')
ENDIF

IF TYPE('lcXMLFileName') = 'C'
  OARIAENVIRONMENT.GCDEVICE = 'FILE'

  OARIAENVIRONMENT.REMOTETABLEACCESS.OPENTABLE('SydField' ,'CFLD_NAME','SH',.F.,.F.)
  OARIAENVIRONMENT.REMOTETABLEACCESS.OPENTABLE('SYCCOMP' ,'CCOMP_ID','SH',.F.,.F.)

  LCPRINTHDR = OARIAENVIRONMENT.CURSORS.GETCURSORTEMPNAME()        && Temp Cursor For the Header of codes and Related fields to be print
  LCHEADTEMP = OARIAENVIRONMENT.CURSORS.GETCURSORTEMPNAME()        && Temp Cursor For the Header of codes and Related fields
  LCDETALTMP = OARIAENVIRONMENT.CURSORS.GETCURSORTEMPNAME()        && Temp Cursor For the Details of codes and Related fields to be print
  LCHDRINDEX = OARIAENVIRONMENT.CURSORS.GETCURSORTEMPNAME()        && Temp Name for Header index used in print only.
ELSE
  =GFOPENTABLE('SYDFIELD','CFLD_NAME','SH')
  =GFOPENTABLE('SYCCOMP','CCOMP_ID','SH')
  LCPRINTHDR = GFTEMPNAME()        && Temp Cursor For the Header of codes and Related fields to be print
  LCHEADTEMP = GFTEMPNAME()        && Temp Cursor For the Header of codes and Related fields
  LCDETALTMP = GFTEMPNAME()        && Temp Cursor For the Details of codes and Related fields to be print
  LCHDRINDEX = GFTEMPNAME()        && Temp Name for Header index used in print only.
ENDIF

LCSTTIME = TIME()
LFFILLCODE()

IF TYPE("laRltFdInf[1,1]")$"UL"
  =LFFILLRELT()
ENDIF

*-- if user change filter expression.(We Collect the Data of the report again)
*-- lnSepWidth : Variable to hold the separator between the Headers of the realated fields.
PRIVATE LNSEPWIDTH
LNSEPWIDTH = 3

=LFCOLLECT()  && Collect report data.


*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(LCPRINTHDR) = 0
  *---Text : 'No Record Selected for the report..!'
  IF TYPE('lcXMLFileName') <> 'C'
    =GFMODALGEN('TRM00052B00000','DIALOG')
  ENDIF
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]


PRIVATE LCCURRFLD , LNCOMPPOS , LCCOMPNAME
*-- lcCurrFld : This Variable will be used in .FRX to detect if we are at start
*--             of new code group or not (To print headers).
LCCURRFLD = SPACE(10)

*-- Get the ID and the Name of the selected company to be print
LNCOMPPOS = ASCAN(LACOMPDESC,LCRPCOMP)  && Scan the array laCompDesc which hold Description of
&& company (ID , Name) by the selected company
IF LNCOMPPOS > 0
  LNCOMPPOS  = ASUBSCRIPT(LACOMPDESC,LNCOMPPOS,1)
  LCCOMPNAME = LACOMPDESC[lnCompPos]          && Get the Id & the name of selected company
ENDIF

*-- Set relation to header print table
SELECT (LCPRINTHDR)
SET ORDER TO (LCHDRINDEX)
SET RELATION TO CFLD_NAME INTO (LCDETALTMP)
SET SKIP TO (LCDETALTMP)


LLNEWPAGE=.T.   && Detect if it new page

IF TYPE('lcXMLFileName') = 'C'
  OARIAENVIRONMENT.REPORT.OGLASTFORM = LCRPNAME

  LOPROGRESS.PERCENT = 0.9
  LOPROGRESS.DESCRIPTION = "Printing Report..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  PRIVATE LOPROXY
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  LOPROXY = GOREMOTECALL.GETREMOTEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *IF loProxy.GetRequest(lcRequestID).Status = 3
  *IF loProxy.GetRequest(lcRequestID, ClientId).STATUS = 3

  IF (OARIAENVIRONMENT.GCDEVICE = "FILE" .AND. OARIAENVIRONMENT.REPORT.CTEXTREPTYPE = "EXCEL")
    SELECT &LCPRINTHDR..CFLD_NAME, &LCPRINTHDR..CFLD_HEAD ,;
      &LCDETALTMP..CCODE_NO  ,;
      &LCDETALTMP..CFLD_DESC ,;
      &LCPRINTHDR..MREL_FLD ,SPACE(200) AS CRTL_FLDS, ;
      &LCDETALTMP..CREL_VLU  FROM  (LCPRINTHDR)INNE JOIN (LCDETALTMP) ON &LCDETALTMP..CFLD_NAME =&LCPRINTHDR..CFLD_NAME INTO CURSOR 'CURSXLS' READWRITE
    SELECT 'CURSXLS'
    REPLACE ALL CRTL_FLDS WITH MREL_FLD
    LOCATE

  ENDIF



  *T20100512.0026 Hassan 2010 05 23 [END]
  OARIAENVIRONMENT.REPORT.PRINT(OARIAENVIRONMENT.REPORT.OGLASTFORM)

  LOPROGRESS.PERCENT = 1.0
  LOPROGRESS.DESCRIPTION = "Printing Report..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
  *T20100512.0026 Hassan 2010 05 23 [END]
  *ENDIF
ELSE
  IF (OARIAAPPLICATION.GCDEVICE = "FILE" .AND. LOOGSCROLL.CTEXTREPTYPE = "EXCEL")
    SELECT &LCPRINTHDR..CFLD_NAME, &LCPRINTHDR..CFLD_HEAD ,;
      &LCDETALTMP..CCODE_NO  ,;
      &LCDETALTMP..CFLD_DESC ,;
      &LCPRINTHDR..MREL_FLD ,SPACE(200) AS CRTL_FLDS ,;
      &LCDETALTMP..CREL_VLU FROM  (LCPRINTHDR)INNE JOIN (LCDETALTMP) ON &LCDETALTMP..CFLD_NAME =&LCPRINTHDR..CFLD_NAME INTO CURSOR 'CURSXLS' READWRITE
    SELECT 'CURSXLS'
    REPLACE ALL CRTL_FLDS WITH MREL_FLD
    LOCATE
  ENDIF
  *WAIT WINDOW EVALUATE('lcRpName') &&SABER
  DO GFDISPRE WITH EVALUATE('lcRpName')
ENDIF
*-- Remove relation
SET RELATION TO
*: N000622,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[End]
*!*************************************************************
*! Name      : lfFillRelt
*! Developer : Mariam MAzhar
*! Date      : 09/28/2008
*! Purpose   : Fill Related fields array
*!*************************************************************
FUNCTION LFFILLRELT

*-- Array to Hold headers of releated fields
DIMENSION LARLTFDINF[45,3]
LARLTFDINF[1,1]="CADJACCT"
LARLTFDINF[1,2]="Adj. Account"
LARLTFDINF[1,3]=24
LARLTFDINF[2,1]="CLRLNAME"
LARLTFDINF[2,2]="Long Name"
LARLTFDINF[2,3]=30
LARLTFDINF[3,1]="CNRFCODE"
LARLTFDINF[3,2]="NRF Code"
LARLTFDINF[3,3]=5
LARLTFDINF[4,1]="ALLOW_TYPE"
LARLTFDINF[4,2]="Allowance Type"
LARLTFDINF[4,3]=1
LARLTFDINF[5,1]="CBNKCODE"
LARLTFDINF[5,2]="Bank Code"
LARLTFDINF[5,3]=8
LARLTFDINF[6,1]="CCHKACCT"
LARLTFDINF[6,2]="Bank Checking Acc."
LARLTFDINF[6,3]=12
LARLTFDINF[7,1]="DISCPCNT"
LARLTFDINF[7,2]="Discount Type"
LARLTFDINF[7,3]=6
LARLTFDINF[8,1]="START"
LARLTFDINF[8,2]="Start Date"
LARLTFDINF[8,3]=8
LARLTFDINF[9,1]="DENDATE"
LARLTFDINF[9,2]="End Date"
LARLTFDINF[9,3]=8
LARLTFDINF[10,1]="DIVLNAME"
LARLTFDINF[10,2]="Division Long Name"
LARLTFDINF[10,3]=30
LARLTFDINF[11,1]="LINK_CODE"
LARLTFDINF[11,2]="GL Link Code"
LARLTFDINF[11,3]=6
LARLTFDINF[12,1]="CSLSGLLINK"
LARLTFDINF[12,2]="GL Sales Link Code"
LARLTFDINF[12,3]=3
LARLTFDINF[13,1]="DIVGROUP"
LARLTFDINF[13,2]="Division Group"
LARLTFDINF[13,3]=3
LARLTFDINF[14,1]="CUPCMAN"
LARLTFDINF[14,2]="U.C.C. Manufacture ID"
LARLTFDINF[14,3]=6
LARLTFDINF[15,1]="CUPCGENTYP"
LARLTFDINF[15,2]="Eropune UPC"
LARLTFDINF[15,3]=1
LARLTFDINF[16,1]="GLACCOUNT"
LARLTFDINF[16,2]="GL Account"
LARLTFDINF[16,3]=24
LARLTFDINF[17,1]="COPERSEQ"
LARLTFDINF[17,2]="Operation Seq."
LARLTFDINF[17,3]=2
LARLTFDINF[18,1]="LINHOUSE"
LARLTFDINF[18,2]="In House (Y/N)"
LARLTFDINF[18,3]=1
LARLTFDINF[19,1]="CCONTCODE"
LARLTFDINF[19,2]="Contractore/Department"
LARLTFDINF[19,3]=8
LARLTFDINF[20,1]="CCONTNAME"
LARLTFDINF[20,2]="Contractore Name"
LARLTFDINF[20,3]=30
LARLTFDINF[21,1]="LMFGOPR"
LARLTFDINF[21,2]="Consider As Operation(Y/N)"
LARLTFDINF[21,3]=1
LARLTFDINF[22,1]="LEADTIME"
LARLTFDINF[22,2]="Lead Time"
LARLTFDINF[22,3]=3
LARLTFDINF[23,1]="CFRGTACNT"
LARLTFDINF[23,2]="GL Freight Account"
LARLTFDINF[23,3]=24
LARLTFDINF[24,1]="CTAXCODE"
LARLTFDINF[24,2]="Tax Code"
LARLTFDINF[24,3]=6
LARLTFDINF[25,1]="CARGLACC"
LARLTFDINF[25,2]="AR/Non AR Account"
LARLTFDINF[25,3]=24
LARLTFDINF[26,1]="NTERDUED"
LARLTFDINF[26,2]="Net Due Days"
LARLTFDINF[26,3]=3
LARLTFDINF[27,1]="NTERDISCD"
LARLTFDINF[27,2]="Discount Days"
LARLTFDINF[27,3]=3
LARLTFDINF[28,1]="NTERDISCR"
LARLTFDINF[28,2]="Discount Percent"
LARLTFDINF[28,3]=6
LARLTFDINF[29,1]="EOM"
LARLTFDINF[29,2]="E.O.M (Y/N)"
LARLTFDINF[29,3]=1
LARLTFDINF[30,1]="EOMDAY"
LARLTFDINF[30,2]="End Of Month Day"
LARLTFDINF[30,3]=2
LARLTFDINF[31,1]="CODYN"
LARLTFDINF[31,2]="C.O.D (Y/N)"
LARLTFDINF[31,3]=1
LARLTFDINF[32,1]="LINSTALLM"
LARLTFDINF[32,2]="Use Installments (Y/N)"
LARLTFDINF[32,3]=1
LARLTFDINF[33,1]="LLCASH"
LARLTFDINF[33,2]="Cash Payment (Y/N)"
LARLTFDINF[33,3]=1
LARLTFDINF[34,1]="NRYLRATE"
LARLTFDINF[34,2]="Royalty Rate"
LARLTFDINF[34,3]=6
LARLTFDINF[35,1]="CARRIERCOD"
LARLTFDINF[35,2]="Carrier Code"
LARLTFDINF[35,3]=4
LARLTFDINF[36,1]="CUPC"
LARLTFDINF[36,2]="UPC Type"
LARLTFDINF[36,3]=13
LARLTFDINF[37,1]="NCODCHARGE"
LARLTFDINF[37,2]="COD Charge"
LARLTFDINF[37,3]=5
LARLTFDINF[38,1]="NFXDPRCNT"
LARLTFDINF[38,2]="Merchandise Charge"
LARLTFDINF[38,3]=5
LARLTFDINF[39,1]="NINSCHARGE"
LARLTFDINF[39,2]="Insurance Charge/100$"
LARLTFDINF[39,3]=5
LARLTFDINF[40,1]="NTAXRATE"
LARLTFDINF[40,2]="Tax Rate"
LARLTFDINF[40,3]=6
LARLTFDINF[41,1]="CTAXRULE"
LARLTFDINF[41,2]="Tax Rule"
LARLTFDINF[41,3]=2
LARLTFDINF[42,1]="CGLINPACCT"
LARLTFDINF[42,2]="GL Input Account"
LARLTFDINF[42,3]=24
LARLTFDINF[43,1]="CGLOUTACCT"
LARLTFDINF[43,2]="GL Output Account"
LARLTFDINF[43,3]=24
LARLTFDINF[44,1]="C1099CODE"
LARLTFDINF[44,2]="1099 Code(Rent,Royalties)"
LARLTFDINF[44,3]=2

*B605226,1 RAE [START]
LARLTFDINF[45,1]="LLOBSOLETE"
LARLTFDINF[45,2]="Obsolete"
LARLTFDINF[45,3]=10
*B605226,1 RAE [END]
*-- end of lfFillRelt.


FUNCTION LFREPWHEN
GFOPENTABLE('CODES','CODES','SH','CODES')
*gfopenTable('SYSCCOMP','CCOMP_ID','SH','SYSCCOMP')
GFOPENTABLE('Sydfield','CFLD_NAME','SH','Sydfield')
*!************************************************************************
*! Name      : lfFillCode
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill The arrays of companys information & code information
*!************************************************************************
*N000120,1
FUNCTION LFFILLCODE
PRIVATE LNI

*-- Fill Company Array
SELECT SYCCOMP
LNI = 1
*-- scaning "SYCCOMP" to get the companys information
SCAN
  DIMENSION LACOMPDESC[lnI,1] , LACOMPVAL[lnI,1]
  LACOMPDESC[lnI,1] = CCOMP_ID + "-" + CCOM_NAME    && array to hold the companys information
  && (ID & Name)

  LACOMPVAL[lnI,1]  = CCOMP_ID                      && array to hold the companys ID
  LNI = LNI + 1
ENDSCAN   && end scaning "SYCCOMP" to get the companys information

*-- Fill codes arrays.
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  LCTEMPCODE = OARIAENVIRONMENT.CURSORS.GETCURSORTEMPNAME()  && temp cursor to hold codes
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  LCTEMPCODE = GFTEMPNAME()
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
*-- select the needed information for codes from "Sydfield" and save it in the temp cuesor
*-- lcTempCode
IF USED('SydField_A')
  =GFCLOSETABLE('SydField_A')
ENDIF
=GFOPENTABLE('Sydfield','CFLD_NAME','SH','SydField_A')

*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	SELECT  cFld_name , cfld_head ,mrltFields ,lrltFields ,mcodeinfo;
*!*	  FROM SydField_A;
*!*	  WHERE lvldentry ;
*!*	  ORDER BY cfld_head ;
*!*	  INTO CURSOR (lcTempCode)
SELECT  CFLD_NAME , CFLD_HEAD ,MRLTFIELDS ,LRLTFIELDS ,MCODEINFO;
  FROM SYDFIELD_A;
  WHERE LVLDENTRY AND (CVER='A27' OR EMPTY(CVER));
  ORDER BY CFLD_HEAD ;
  INTO CURSOR (LCTEMPCODE)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*-- if there is no codes found and saved in (lcTempCode)
*-- terminate the Option Grid , else fill codes array
IF _TALLY = 0
  WAIT WINDOW "No Codes found"
  LLOGTRMNAT = .T.
  RETURN .F.
ELSE
  DIMENSION LACODEDESC[_TALLY + 1,1] , LACODERET[_TALLY + 1,1]  && array to hold codes information
  LACODEDESC[1] = "All"
  LACODERET[1]  = ""

  SELECT (LCTEMPCODE)
  LNI = 2
  *-- scan temp cursor to fill the codes array
  SCAN
    LACODEDESC[lnI,1] = ALLTRIM(CFLD_HEAD)
    LACODERET [lnI,1] = CFLD_NAME
    LNI = LNI + 1
  ENDSCAN  && scan temp cursor to fill the codes array

ENDIF  && if there is no codes found and saved in (lcTempCode)

SELECT (LCTEMPCODE)
INDEX ON CFLD_NAME TAG (LCTEMPCODE) &&OF (goAriaEnvironment.WorkDir+lcTempCode+".CDX")


*-- end of lfFillCode.
*!*************************************************************
*!*************************************************************
*! Name      : lfCollect
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Collect the report Data
*!*************************************************************
*N000120,1
FUNCTION LFCOLLECT
PRIVATE LNLINES,LNI,LNWIDTH ,LNFOUND, LCDESC , LCCURRCODE , LCCODES , LNCNT
STORE "" TO LCDESC , LCCURRCODE

=LFNEWSESON()      && Open Codes for selected company and creates the needed temp


&& cursors (lcPrintHdr, lcHeadTemp, lcDetalTmp)

SELECT (LCTEMPCODE)   && Select the temp cursor which is a copy of "SydField"
COUNT FOR CFLD_NAME = LCRPCODE AND !DELETED() TO LNCODECNT

SELECT (LCTEMPCODE)   && Select the temp cursor which is a copy of "SydField"
LOCATE
*-- Scan the temp cursor which holds all the codes for the selected code(s).
SCAN FOR CFLD_NAME = LCRPCODE

  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    LNPERCENT = RECNO()/LNCODECNT
    IF MOD(RECNO(),CEILING(LNCODECNT / 10)) = 0
      LOPROGRESS.PERCENT = LNPERCENT * 0.9
      LOPROGRESS.DESCRIPTION = "Collecting Data..."
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF

    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]

  =LFFILLMAST()  && Fill Header and details for master code records.

  *-- if there is a related fields for this code
  IF &LCTEMPCODE..LRLTFIELDS

    =LFHDRRELAT()  && Fill related Memo/Records in Header File
    =LFDETRELAT()  && Fill related Memo/Records in Detail File

  ENDIF  && end if there is a related fields for this code

ENDSCAN   && end Scan the temp cursor for the selected code or codes
*-- end of lfCollect.
*!*************************************************************
*! Name      : lfNewSeson
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : open and creats files to be used in the report
*!*************************************************************

FUNCTION LFNEWSESON

PRIVATE LCDATADIR

*-- Close Codes file if opened from another company
IF USED("CODES")
  IF TYPE('lcXMLFileName') = 'C'
    OARIAENVIRONMENT.REMOTETABLEACCESS.CLOSETABLE("CODES")
  ELSE
    GFCLOSETABLE("CODES")
  ENDIF
ENDIF


IF TYPE('lcXMLFileName') = 'C'
  =OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD(LCRPCOMP,"SYCCOMP",'CCOMP_ID')
ELSE
  GFSEEK(LCRPCOMP,"SYCCOMP",'CCOMP_ID')
ENDIF
IF TYPE('lcXMLFileName') = 'C'
  LCDATADIR = ALLTRIM(OARIAENVIRONMENT.GETDATADIRECTORY(ALLTRIM(SYCCOMP.CCOM_DDIR)))
  LCDATADIR = LCDATADIR + IIF(RIGHT(LCDATADIR,1)="\","","\")
ELSE
  LCDATADIR = OARIAAPPLICATION.DATADIR
ENDIF   && end if the selected company found in "SYCCOMP" file get the directory of it

IF TYPE('lcXMLFileName') = 'C'
  LCOLDCOMP = OARIAENVIRONMENT.ACTIVECOMPANYID
  OARIAENVIRONMENT.GETCOMPANYINFORMATION(LCRPCOMP)
  OARIAENVIRONMENT.REMOTETABLEACCESS.OPENTABLE(OARIAENVIRONMENT.DATADIR + 'CODES' ,'Ccode_no','SH',.F.,.F.,LCRPCOMP)
ELSE
  LCOLDCOMP = OARIAAPPLICATION.ACTIVECOMPANYID
  OARIAAPPLICATION.GETCOMPANYINFORMATION(LCRPCOMP)
  GFOPENTABLE(OARIAAPPLICATION.DATADIR + 'CODES' ,'Ccode_no','SH',.F.,.F.,LCRPCOMP)
ENDIF



SELECT CODES
IF TYPE('lcXMLFileName') = 'C'
  OARIAENVIRONMENT.REMOTETABLEACCESS.SETORDERTO('Ccode_no')
  OARIAENVIRONMENT.ACTIVECOMPANYID = LCOLDCOMP
  OARIAENVIRONMENT.GETCOMPANYINFORMATION(LCOLDCOMP)
ELSE
  GFSETORDER('Ccode_no')
  OARIAAPPLICATION.ACTIVECOMPANYID = LCOLDCOMP
  OARIAAPPLICATION.GETCOMPANYINFORMATION(LCOLDCOMP)
ENDIF



=GFOPENFILE(OARIAAPPLICATION.DATADIR+'CODES','Ccode_no','SH')  && Reopen Codes file from selected company

*-- Create temp. tables used by this report to print layout [Begin]
*-- Create Temp. Header Cursor
IF USED(LCHEADTEMP)
  USE IN (LCHEADTEMP)
ENDIF
CREATE CURSOR (LCHEADTEMP) (CFLD_NAME C(10), CFLD_HEAD C(30), MRLT_CODE M(10), MREL_FLD M(10))
ZAP
INDEX ON CFLD_NAME TAG (LCHEADTEMP)&& OF (goAriaEnvironment.WorkDir+lcHeadTemp+".CDX")

*-- Create Temp. Header Print File
IF USED(LCPRINTHDR)
  USE IN (LCPRINTHDR)
ENDIF
CREATE CURSOR (LCPRINTHDR) (CFLD_NAME C(10), CFLD_HEAD C(30), MREL_FLD M(10))
ZAP
INDEX ON CFLD_HEAD TAG (LCHDRINDEX) &&OF (goAriaEnvironment.WorkDir+lcPrintHdr+".CDX")
INDEX ON CFLD_NAME TAG (LCPRINTHDR) &&OF (goAriaEnvironment.WorkDir+lcPrintHdr+".CDX")
SET ORDER TO (LCPRINTHDR)

*-- Create Temp. Detail File
IF USED(LCDETALTMP)
  USE IN (LCDETALTMP)
ENDIF
CREATE CURSOR (LCDETALTMP) (CFLD_NAME C(10),CCODE_NO C(10) ,CFLD_DESC C(41), CREL_VLU C(90))
ZAP
INDEX ON CFLD_NAME + CCODE_NO TAG (LCDETALTMP) &&OF (goAriaEnvironment.WorkDir+lcDetalTmp+".CDX")
*-- Create temp. tables used by this report to print layout [End  ]

*-- end of lfNewSeson
*!*************************************************************
*! Name      : lfFillMast
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill The Header file and the Detail file for
*!           : master records (which is the codes itself)
*!*************************************************************
*: Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes
*!*************************************************************

FUNCTION LFFILLMAST

*-- Note : Selected Alias is Temp. SydField which is (lcTempCode)
*-- Fill Header for master records [Begin]
SCATTER MEMVAR
INSERT INTO (LCHEADTEMP) FROM MEMVAR     && Fill Header for master records (which is the codes itself)
INSERT INTO (LCPRINTHDR) FROM MEMVAR     && Fill Header for master records to be Print (which is the codes itself)
*-- Fill Header for master records [End  ]

SELECT CODES
*-- Fill Detail for master records(which is the value of the code itself) [Begin]
IF TYPE('lcXMLFileName') = 'C'
  OARIAENVIRONMENT.REMOTETABLEACCESS.SETORDERTO('Idrltfname')
ELSE
  GFSETORDER('Idrltfname')
ENDIF

*-- if Seek in codes file for not defaulted records
*-- and it is the header record for active code
IF TYPE('lcXMLFileName') = 'C'

  * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [Begin]
  *IF oAriaEnvironment.remotetableaccess.SEEKRECORD("NN"+&lcTempCode..cFld_name  ,"CODES")
  IF OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD("NN"+SUBSTR(&LCTEMPCODE..CFLD_NAME,1,10)  ,"CODES")
    * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [End]

    SELECT CODES

    *-- Scan codes table for not defaulted records and it is the header record for active code
    *-- 1St N : Stands for : not defaulted records
    *-- 2Nd N : Stands for : header record (not a releated record)
    * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [Begin]
    *SCAN WHILE cdefcode+crltfield+cFld_name = "NN" + &lcTempCode..cFld_name
    SCAN WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN" + SUBSTR(&LCTEMPCODE..CFLD_NAME    ,1,10)
      * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [End]
      *-- If this code is editable the header of it will be "code # - discrption"
      *-- if not editable the header will be "discrption"
      LCDESC = IIF("EDITABLE" $ &LCTEMPCODE..MCODEINFO, CCODE_NO+"-" , "") + CDISCREP

      INSERT INTO (LCDETALTMP) (CFLD_NAME ,CCODE_NO, CFLD_DESC) VALUES;
        (CODES.CFLD_NAME ,CODES.CCODE_NO, LCDESC)

    ENDSCAN  && end Scan codes table for not defaulted records and it is the header
    && record for active code

    LCCURRCODE = &LCDETALTMP..CFLD_NAME   && save the active code in the global variavle "lcCurrcode"
  ENDIF
ELSE
  * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [Begin]
  *IF GFSEEK("NN"+&LCTEMPCODE..CFLD_NAME  ,"CODES")
  IF GFSEEK("NN"+ SUBSTR(&LCTEMPCODE..CFLD_NAME,1,10)  ,"CODES")
  * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [End]
  
    SELECT CODES
    *-- Scan codes table for not defaulted records and it is the header record for active code
    *-- 1St N : Stands for : not defaulted records
    *-- 2Nd N : Stands for : header record (not a releated record)
    
    * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [Begin]
    *SCAN WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN" + &LCTEMPCODE..CFLD_NAME
    SCAN WHILE CDEFCODE+CRLTFIELD+CFLD_NAME = "NN" + SUBSTR(&LCTEMPCODE..CFLD_NAME,1 ,10)    
    * B610480,1 HIA 08/22/13 T20130812.0026 - Cannot run a Codes report for Payment terms [End]
    
      *-- If this code is editable the header of it will be "code # - discrption"
      *-- if not editable the header will be "discrption"
      LCDESC = IIF("EDITABLE" $ &LCTEMPCODE..MCODEINFO, CCODE_NO+"-" , "") + CDISCREP

      INSERT INTO (LCDETALTMP) (CFLD_NAME ,CCODE_NO, CFLD_DESC) VALUES;
        (CODES.CFLD_NAME ,CODES.CCODE_NO, LCDESC)

    ENDSCAN  && end Scan codes table for not defaulted records and it is the header
    && record for active code

    LCCURRCODE = &LCDETALTMP..CFLD_NAME   && save the active code in the global variavle "lcCurrcode"
  ENDIF
ENDIF  && end if Seek in codes file for not defaulted records
&& and it is the header record for active code
*-- Fill Detail for master records [End  ]

*-- end of lfFillMast.

*!*************************************************************
*! Name      : lfHdrRelat
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill related Memo/Records in Header File
*!*************************************************************
*! Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes if there is
*!             : releated fields
*!*************************************************************

FUNCTION LFHDRRELAT


SELECT (LCTEMPCODE)   && Select the temp cursor which holds all codes

DIMENSION LARLTFLD[1]     && array to hold the relaeted fields for the active code
LCRELATED = STRTRAN(ALLTRIM(MRLTFIELDS),"$","")
LCRELATED = STRTRAN(ALLTRIM(LCRELATED),"~","")
=GFSUBSTR(LCRELATED,@LARLTFLD,"|")

LNWIDTH = 90     && the width for the header of the releated fields in the report

FOR LNI=1 TO ALEN(LARLTFLD)
  LNFOUND = ASCAN(LARLTFDINF,ALLTRIM(LARLTFLD[lnI]))   && scan the array which holds all the releated fields for all codes
  && for the releated fields for the active code
  *-- if the releated fields for the active found in "laRltFdInf"
  IF LNFOUND > 0
    LNFOUND = ASUBSCRIPT(LARLTFDINF,LNFOUND,1)
    *-- call function lfFillMemo() to fill the memo field of the header file by the valus found
    *-- in "laRltFdInf" for the active code
    =LFFILLMEMO(LARLTFDINF[lnFound,1],LARLTFDINF[lnFound,2],;
      LARLTFDINF[lnFound,3])
  ELSE
    *-- if this field is found in sydfield
    IF TYPE('lcXMLFileName') = 'C'

      *E303030,1 BEGIN
      *=oAriaEnvironment.remotetableaccess.SEEKRECORD(PADR(laRLtFLd[lnI],10),"SydField")
      *! E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][Start]
      *=oAriaEnvironment.remotetableaccess.SEEKRECORD(PADR(laRLtFLd[lnI],oAriaEnvironment.FieldW),"SydField")
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
      *=oAriaEnvironment.remotetableaccess.SEEKRECORD(PADR(laRLtFLd[lnI],10),"SydField")
      =OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD(PADR(LARLTFLD[lnI],OARIAAPPLICATION.FIELDW),"SydField")
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
      *! E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][ENd]
      *E303030,1 END

    ELSE

      *E303030,1 BEGIN
      *=gfseek(PADR(laRLtFLd[lnI],10),"SydField")
      *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
      * =gfseek(PADR(laRLtFLd[lnI],oAriaEnvironment.FieldW),"SydField")
      *! E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][Start]
      *=gfseek(PADR(laRLtFLd[lnI],oAriaApplication.FieldW),"SydField")
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
      =GFSEEK(PADR(LARLTFLD[lnI],OARIAAPPLICATION.FIELDW),"SydField")
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
      *! E303079,1 MMT 03/11/2012 Fixing Media issues[T20120304.0004][END]
      *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
      *E303030,1 END

    ENDIF
    *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
    SELECT "SydField"
    LOCATE REST WHILE CFLD_NAME = PADR(LARLTFLD[lnI],OARIAAPPLICATION.FIELDW) FOR (CVER='A27' OR EMPTY(CVER))
    IF FOUND()
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
      *-- Add new Items to Array laRltFdInf
      DIMENSION LARLTFDINF[ALEN(laRltFdInf,1) + 1 , ALEN(laRltFdInf,2)]
      LARLTFDINF[ALEN(laRltFdInf,1),1] = SYDFIELD.CFLD_NAME
      LARLTFDINF[ALEN(laRltFdInf,1),2] = ALLTRIM(SYDFIELD.CFLD_HEAD)
      LARLTFDINF[ALEN(laRltFdInf,1),3] = SYDFIELD.NFLD_WDTH
      *-- call function lfFillMemo() to fill the memo field of the header file by the valus found
      *-- in "SydField"  for the active code
      =LFFILLMEMO(SYDFIELD.CFLD_NAME,SYDFIELD.CFLD_HEAD,;
        SYDFIELD.NFLD_WDTH)
      *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
    ENDIF
    *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
  ENDIF  && if this field is found in sydfield
ENDFOR
*-- end of lfHdrRelat.

*!*************************************************************
*! Name      : lfDetRelat
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill related Memo/Records in Detail File
*!*************************************************************
*! Called From : Function (lfCollect) , within scaning the temp
*!             : cursor which holds all the codes if there is
*!             : releated fields
*!*************************************************************

FUNCTION LFDETRELAT
PRIVATE LNNOOFREC , LCDESC

*-- if the active code found in the detail file we get it
IF SEEK(LCCURRCODE,LCDETALTMP)
  SELECT (LCDETALTMP)          && Select the detail file

  *-- scan the detail file for the active code
  SCAN FOR CFLD_NAME = LCCURRCODE
    SCATTER MEMVAR
    *-- If the crosponding record to the active code found in the Header file we get it
    IF SEEK (CFLD_NAME,LCHEADTEMP)
      LNNOOFREC = 1       && variable to hold # of records for the active code in the
      && Header file

      SELECT (LCHEADTEMP) && Select the Header file
      *-- scan the Header file for the active code
      SCAN FOR CFLD_NAME = &LCDETALTMP..CFLD_NAME
        *-- if The # of records for the active code is more than one
        *-- we select the detail file and add another record for the same active code
        IF LNNOOFREC > 1
          SELECT (LCDETALTMP)
          APPEND BLANK
          REPLACE CFLD_NAME WITH m.CFLD_NAME ,;
            CCODE_NO  WITH m.CCODE_NO
          =LFFLDETMEM()   && Call function to fill the memo of the detail file for the active code
        ELSE
          *-- Update Detail memo field
          =LFFLDETMEM()  && Call function to fill the memo of the detail file for the active code
          LNNOOFREC = LNNOOFREC + 1  && incrementing the # of records

        ENDIF && if The # of records for the active code is more than one

      ENDSCAN  && scan the Header file for the active code
    ENDIF && If the crosponding record to the active code found in the Header file we get it

  ENDSCAN  && scan the detail file for the active code

ENDIF  && if the active code found in the detail file
*-- end of lfDetRelat.
*!*************************************************************
*! Name      : lfFlDetMem
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill memo field of Detal Temp files
*!*************************************************************
*! Called From : Function (lfDetRelat)
*!**************************************************************

FUNCTION LFFLDETMEM
PRIVATE LNI , LNPOS , LNRLTPOS , LCRLTD_NAME , LCMEMOSTR , LNSPACELEN
LCMEMOSTR = ""     && String variable to hold the values of the related
&& fiels and the separator between them

DIMENSION LARLTCODE[1]    && array to hold the codes of the related fiels from
&& memo field which hold them in the header file
STORE "" TO LARLTCODE
SELECT CODES
IF TYPE('lcXMLFileName') = 'C'
  OARIAENVIRONMENT.REMOTETABLEACCESS.SETORDERTO('CODES')
  OARIAENVIRONMENT.STRINGS.GETSUBSTRING(&LCHEADTEMP..MRLT_CODE,@LARLTCODE,"|")
ELSE
  GFSETORDER('CODES')
  GFSUBSTR(&LCHEADTEMP..MRLT_CODE,@LARLTCODE,"|")
ENDIF
*N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]


DIMENSION LATEMP[ALEN(laRltCode,1),3]  && temp array to hold the values of the related fiels
&& and the start position to be print from in the report

LARLTCODE[1]  = SPACE(2) + LARLTCODE[1]

SELECT CODES
*-- if Seek in codes file for not defaulted records
*-- and it is the related field for active code and active code # get it
*cdefcode+ccode_no+crltfield+cfld_name
IF TYPE('lcXMLFileName') = 'C'
  = OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD("N" + PADR(&LCDETALTMP..CCODE_NO,6) + "Y" + PADR(&LCDETALTMP..CFLD_NAME,10))
ELSE
  GFSEEK("N" + PADR(&LCDETALTMP..CCODE_NO,6) + "Y" + PADR(&LCDETALTMP..CFLD_NAME,10))
ENDIF
*-- Scan codes table for not defaulted records and it is he related field for active code
*-- and active code #
*-- N : Stands for : not defaulted records
*-- Y : Stands for : releated record

SCAN REST WHILE  CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME = "N" + PADR(&LCDETALTMP..CCODE_NO,6) +;
    "Y" + PADR(&LCDETALTMP..CFLD_NAME,10)
  LCRLTD_NAME = SPACE(LNSEPWIDTH-1) + ALLTRIM(CODES.CRLTD_NAM)   && variable to hold the code
  && of the related field
  LNPOS = ASCAN(LARLTCODE,LCRLTD_NAME)             && scan the array which hold the codes of
  && the related fiels by lcRltd_Name
  *-- if found calculate the start position of it ,to be print in the report
  IF LNPOS > 0
    LNPOS = ASUBSCRIPT(LARLTCODE,LNPOS,1)
    LNRLTPOS = 0
    *-- if the start position not (1) calculate it
    IF LNPOS > 1
      FOR LNI=1 TO LNPOS-1
        LNRLTPOS = LNRLTPOS + IIF(LNI=1 , LEN(LARLTCODE[lnI]), LEN(LARLTCODE[lnI])-(LNSEPWIDTH-1));
          + LNSEPWIDTH
      ENDFOR
    ELSE
      LNRLTPOS = 1
    ENDIF    && end if the start position not (1) calculate it

    *-- fill the temp array with the value of the related field and its position
    LATEMP[lnPos,1] = ALLTRIM(CODES.CRLTD_VLU)
    LATEMP[lnPos,2] = LNRLTPOS

    *B605226,1 RAE [START]
    LATEMP[lnPos,3] = ALLTRIM(CODES.CRLTD_NAM)
    *B605226,1 RAE [END]

  ENDIF && end if found calculate the start position of it ,to be print in the report

ENDSCAN  && end Scan codes table for not defaulted records and it is he related field for active code
&& and active code #

*we have add the next lines of code to insert the Obseleted value to the report instead of creating a fix program
*to add the related field obeselete to the color code.
IF ASCAN(LATEMP,'LLOBSOLETE') = 0
  IF TYPE('lcXMLFileName') = 'C'
    =OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD('LLOBSOLETE','SYDFIELD')
  ELSE
    GFSEEK('LLOBSOLETE','SYDFIELD')
  ENDIF
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
  SELECT 'SYDFIELD'
  LOCATE REST WHILE CFLD_NAME = 'LLOBSOLETE'  AND (CVER='A27' OR EMPTY(CVER))
  *N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
  LCRLTD_NAME = SPACE(LNSEPWIDTH-1) + ALLTRIM('LLOBSOLETE')
  LNPOS = ASCAN(LARLTCODE,LCRLTD_NAME)             && scan the array which hold the codes of
  && the related fiels by lcRltd_Name
  *-- if found calculate the start position of it ,to be print in the report
  IF LNPOS > 0
    LNPOS = ASUBSCRIPT(LARLTCODE,LNPOS,1)
    LNRLTPOS = 0
    *-- if the start position not (1) calculate it
    IF LNPOS > 1
      FOR LNI=1 TO LNPOS-1
        LNRLTPOS = LNRLTPOS + IIF(LNI=1 , LEN(LARLTCODE[lnI]), LEN(LARLTCODE[lnI])-(LNSEPWIDTH-1));
          + LNSEPWIDTH
      ENDFOR
    ELSE
      LNRLTPOS = 1
    ENDIF    && end if the start position not (1) calculate it

    *-- fill the temp array with the value of the related field and its position
    LATEMP[lnPos,1] = 'N'
    LATEMP[lnPos,2] = LNRLTPOS
    LATEMP[lnPos,3] = 'LLOBSOLETE'
  ENDIF
ENDIF
*-- fill the string which holds the values of related fields and order them
*-- by their positions
FOR LNI = 1 TO ALEN(LATEMP,1)
  *-- if this element in the array not define store null to the value and 0 to the position
  IF TYPE("laTemp[lnI,2]") $ "UL"
    LATEMP[lnI,1] = ""
    LATEMP[lnI,2] = 0
  ENDIF     && end if this element in the array not define store
  && null to the value and 0 to the position

  *-- if this element is logical store "yes" insted of "T" and "No" insted of "F" to the value

  IF TYPE('laTemp[lnI,3]') = "C"
    IF TYPE('lcXMLFileName') = 'C'
      =OARIAENVIRONMENT.REMOTETABLEACCESS.SEEKRECORD(ALLTRIM(LATEMP[lnI,3]),'SYDFIELD') .AND. SYDFIELD.CDATA_TYP = 'L'
    ELSE
      =GFSEEK(ALLTRIM(LATEMP[lnI,3]),'SYDFIELD') .AND. SYDFIELD.CDATA_TYP = 'L'
    ENDIF
    *! B611098,1 MMT 12/21/2015 Codes report prints incorrect related fields values[T20150925.0002 - Issue#63][Start]
    IF SYDFIELD.CDATA_TYP = 'L'
    *! B611098,1 MMT 12/21/2015 Codes report prints incorrect related fields values[T20150925.0002 - Issue#63][End]    
      LATEMP[lnI,1] = IIF(LATEMP[lnI,1] = 'T' ,"Yes","No")
    *! B611098,1 MMT 12/21/2015 Codes report prints incorrect related fields values[T20150925.0002 - Issue#63][Start]
    ENDIF
    *! B611098,1 MMT 12/21/2015 Codes report prints incorrect related fields values[T20150925.0002 - Issue#63][End]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  LNSPACELEN = IIF(LNI=1 OR (LATEMP[lnI,2]=0),0, LATEMP[lnI,2] - LEN(LCMEMOSTR) - 2)
  LCMEMOSTR = LCMEMOSTR + SPACE(LNSPACELEN) + LATEMP[lnI,1]
ENDFOR   && end fill the string which holds the values of related fields and order them
&& by their positions
*-- Fill the memo field
REPLACE &LCDETALTMP..CREL_VLU WITH LCMEMOSTR

&& and it is the related field for active code and active code #

*-- end of lfFlDetMem
*!****************************************************************************
*! Name      : lfFillMemo
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill memo field of Header Temp files for the
*!           : active code
*!****************************************************************************
*! Called From : Function (lfHdrRelat)
*!****************************************************************************
*! Parameters  : lcFldcode : hold the code of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!             : lcFldHead : hold the Header of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!             : lnFldWidth: hold the width of the related field
*!             :           : from either the array (laRltFdInf) or (Sydfield)
*!****************************************************************************
FUNCTION LFFILLMEMO
PARAMETERS LCFLDCODE,LCFLDHEAD, LNFLDWIDTH
PRIVATE LCFLDCODE,LCFLDHEAD, LNFLDWIDTH , LCSEPARTOR

LCSEPARTOR= "|" + SPACE(LNSEPWIDTH-1)   && variable to hold the separator between
&& related fields in the memo

LCFLDHEAD = ALLTRIM(LCFLDHEAD)
LNFLDWIDTH = MAX(LNFLDWIDTH,LEN(LCFLDHEAD))  && Get the maxmam width of the related fields
&& the width of it from ("laRltFdInf" or "Sydfield")
&& or the lenth of the header

*-- if the lenth of the header less than the width for the header of the
*-- releated fields in the report save it in the memo field , otherwise
*-- insert new record in the detail file for the same code
IF LEN(LCFLDHEAD) <= LNWIDTH
  *-- Accomulate Related Fields for Temp. Header File
  SELECT (LCHEADTEMP)
  REPLACE MRLT_CODE WITH MRLT_CODE + IIF(EMPTY(MRLT_CODE),"",LCSEPARTOR) +;
    PADR(LCFLDCODE,LNFLDWIDTH),;
    MREL_FLD WITH MREL_FLD + IIF(EMPTY(MREL_FLD),"",SPACE(LNSEPWIDTH)) +;
    PADR(LCFLDHEAD,LNFLDWIDTH)

  *-- Accomulate Related Fields for Temp. Print Header File
  SELECT (LCPRINTHDR)
  REPLACE MREL_FLD WITH MREL_FLD + IIF(EMPTY(MREL_FLD),"",SPACE(LNSEPWIDTH)) +;
    PADR(LCFLDHEAD,LNFLDWIDTH)

ELSE

  *-- Insert New Record into Temp. Related Header
  INSERT INTO (LCHEADTEMP) (CFLD_NAME , MRLT_CODE, MREL_FLD );
    VALUES                  (&LCTEMPCODE..CFLD_NAME, PADR(LCFLDCODE,LNFLDWIDTH),;
    PADR(LCFLDHEAD,LNFLDWIDTH))

  *-- Accomulate Related Fields for Temp. Print Header File
  SELECT (LCPRINTHDR)
  REPLACE MREL_FLD WITH MREL_FLD + IIF(EMPTY(MREL_FLD),"",CHR(13)) +;
    PADR(LCFLDHEAD,LNFLDWIDTH)

  LNWIDTH = 90
ENDIF  && if the lenth of the header less than the width for the header of the
&& releated fields in the report

LNWIDTH = LNWIDTH - ( LNFLDWIDTH + LNSEPWIDTH)  && decrement the width for the header of the
&& releated fields in the report by the width
&& of the header of the related field
*-- end of lfFillMemo

*!*************************************************************
*! Name      : lfCurrFld
*! Developer : Hend Ghanem
*! Date      : 05/25/2000
*! Purpose   : Control printing headers of codes and related
*!           : fields in the .frx
*!*************************************************************
*! Called From :  .FRX detail band
*!*************************************************************
FUNCTION LFCURRFLD
LCCURRFLD = &LCPRINTHDR..CFLD_NAME
LLNEWPAGE= .F.
RETURN ""
*!*************************************************************
*! Name      : lfDummy
*! Developer : Hend Ghanem
*! Date      : 05/25/2000
*! Purpose   : Control printing headers if it new page
*!           : in the .frx
*!*************************************************************
*! Called From :  .FRX detail band
*!*************************************************************
FUNCTION LFDUMMY
LLNEWPAGE= .T.
