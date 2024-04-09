*:***************************************************************************
*: Program file  : ALSHPCR
*: Program desc. : Shipping Confirmation
*: Tracking#     : E612306
*:        System : Aria 4 XP
*:        Module : AL
*:     Developer : Mariam Mazhar (MMT)
*:***************************************************************************
Parameters lcRequestID, lcXMLFileName, ClientId

* look, to br removed [Begin]
*!*	lcRequestID = "21E6D395-56B2-450F-9F2A-719A2907CF25"
*!*	ClientId = "GMA10"
*!*	lcXMLFileName = 'X:\ARIA4XP\OUTPUT\XL0KUII5.xml'
*!*	Set Classlib To "D:\SHARED\ARIA4XP\SRVCLSS\SY\requesthandler.vcx" AddI
*!*	goRemoteCall = Createobject("RemoteObject")
*!*	goRemoteCall.cInstanceName = ""
*!*	goRemoteCall.cServerName   = "ARIATESTING"
*!*	goRemoteCall.nPort         = 1500
* look, to br removed [End]


If Type('lcXMLFileName') <> 'C'
  lcExpr = gfOpGrid('ALSHPCR' , .T.)&&,.F.,.F.,.T.,.T.)
Else
  Private loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  Private loProgress
  loProgress = Createobject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.Description = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  Local loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientId = ClientId
  Local lcCurrentProcedure
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  Local lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  Do (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") With lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  oAriaEnvironment.XML.RestoreFromXML(Filetostr(lcXMLFileName),.T.)
  lcActiveMod = 'EB'
  oAriaEnvironment.Report.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'EB'
  oAriaEnvironment.RequestID = lcRequestID
  Public gcAct_Appl
  gcAct_Appl = lcActiveMod
  **************************************************************************************************************************
  *Read Cursor of accounr - Treansaction 846 number
  lcAccounts =""
  lnOrdPos   = Ascan(laOgFXflt,"CUSTOMER.ACCOUNT")
  If lnOrdPos> 0
    lnOrdPos= Asubscript(laOgFXflt,lnOrdPos,1)
    lcAccounts = laOgFXflt[lnOrdPos,6]
    If !Empty(lcAccounts) And Used(lcAccounts) And Reccount(lcAccounts) > 0
      Select(lcAccounts)
      Locate
      If Eof()
        lcAccounts =""
      Endif
    Endif
  Endif

  Select (lcAccounts)
  Locate
  iCounter = 0
  Dimension laPartners[1]
  Scan
    If !Isnull(keyexp) And Type('keyexp') == 'C' And !Empty(Alltrim(keyexp))
      iCounter = iCounter + 1
      Dimension laPartners[iCounter]
      lcKeyExp = Alltrim(keyexp)
      laPartners[iCounter] = lcKeyExp
    Endif
  Endscan

  *prepare EDI [Begin]******************************************************************************************************
  lcoldAppHome = oAriaApplication.ApplicationHome
  lcoldAppRep  = oAriaApplication.ReportHome
  lcoldAppBitMapHome = oAriaApplication.BitMapHome
  lcoldAppCls = oAriaApplication.ClassDir
  lcoldAppScx = oAriaApplication.ScreenHome
  loRemoteCompanyData = oAriaApplication.RemoteCompanyData
  *lcEdiPath = oAriaApplication.ediinstallationpath
  If Type('oAriaApplication.ediinspath') = 'U'
    oAriaApplication.AddProperty("ediinspath",.T.) && DUE TO ISSUES IN ariamain of request builder
    oAriaApplication.ediinspath = Strtran(Upper(oAriaApplication.AriaApplicationPath),"ARIA4XP","ARIA3EDI")
  Endif
  lcEdiPath = Alltrim(oAriaApplication.ediinspath)
  oAriaApplication.ApplicationHome = lcEdiPath +  'PRGS\'
  oAriaApplication.ReportHome = lcEdiPath +  'REPORTS\'
  oAriaApplication.BitMapHome = lcEdiPath +  'BMPs\'
  oAriaApplication.ClassDir   = lcEdiPath +  'CLASSES\'
  oAriaApplication.ScreenHome = lcEdiPath +  'SCREENS\'
  oAriaApplication.ClientId = ClientId
  oAriaApplication.syspath = oAriaApplication.systemfilespath
  lcSys = Strtran(Upper(oAriaApplication.syspath), "ARIA4XP\SYSFILES\", "Aria3Edi\")

  If Type('oAriaApplication.isRemoteComp') = 'U'
    oAriaApplication.AddProperty("isRemoteComp",.T.) && DUE TO ISSUES IN ariamain of request builder
  Endif
  If Type('oAriaApplication.lcAria4Class') = 'U'
    oAriaApplication.AddProperty("lcAria4Class", oAriaApplication.AriaApplicationPath+"CLASSES\")
  Endif
  If Type('oAriaApplication.clientprogramhome') = 'U'
    oAriaApplication.AddProperty("clientprogramhome", lcSys + "PRGS\")
  Endif
  If Type('oAriaApplication.EDIMAPPINGCONNECTION') = 'U'
    oAriaApplication.AddProperty("EDIMAPPINGCONNECTION", "")
  Endif
  If Type('oAriaApplication.Aria5SystemManagerConnection') = 'U'
    oAriaApplication.AddProperty("Aria5SystemManagerConnection", "")
  Endif
  If Type('oAriaApplication.Aria4SysFilesConnection') = 'U'
    oAriaApplication.AddProperty("Aria4SysFilesConnection", "")
  Endif
  If Type('oAriaApplication.clientxmlmaphome') = 'U'
    oAriaApplication.AddProperty("clientxmlmaphome", "")
  Endif
  If Type('oAriaApplication.laevnttrig') = 'U'
    oAriaApplication.AddProperty("laevnttrig(1,1)", .F.)
  Endif

  oAriaApplication.clientxmlmaphome = lcSys + "XMLMAPPINGS\"
  oAriaApplication.EDIPath = lcSys + "EDI\"

  If !Empty(oAriaApplication.sqlsysfilesconnectionstring)
    If !Empty(ClientId)
      Local lnConnHandle, lnRemResult
      lnConnHandle = Sqlstringconnect(oAriaApplication.sqlsysfilesconnectionstring)
      If lnConnHandle < 1
        Return
      Else
        lcsqlsysfilesconnectionstring = oAriaApplication.sqlsysfilesconnectionstring
        lnRemResult = SQLExec(lnConnHandle,"Select * from Clients where CCLIENTID='" + ClientId+ "'","Clients")
        oAriaApplication.Aria5SystemManagerConnection = "Driver={SQL Server};server=" + Alltrim(CCONSERVER) + ";DATABASE=" + Alltrim(CCONDBNAME) + ";uid=" + Alltrim(CCONUSERID) + ";pwd=" + Alltrim(CCONPASWRD)
        oAriaApplication.EDIMAPPINGCONNECTION= "Driver={SQL Server};server=" + Alltrim(CCONSERVER) + ";DATABASE=EDIMappings;uid=" + Alltrim(CCONUSERID) + ";pwd=" + Alltrim(CCONPASWRD)
        oAriaApplication.Aria4SysFilesConnection = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB=" + Alltrim(ARIA40SYS) + ";SourceType=DBF;Exclusive=No;BackgroundFetch=No;Collate=Machine;Null=No;Deleted=Yes;"
      Endif
    Else
      Return
    Endif
  Endif

  If (oAriaApplication.ClassDir+'EDI.VCX' $ Set('classlib'))
    Release Classlib (oAriaApplication.ClassDir+'EDI.VCX')
  Endif
  Set Classlib To (oAriaApplication.ClassDir +'EDI_DLL.VCX') AddIt
  *ObjLogger = Createobject("Logger", oAriaApplication.EDIPath, loAgent,lcRequestID, ClientId)
  ObjLogger = Createobject("Logger", oAriaApplication.EDIPath, loAgent,lcRequestID, ClientId)
  ObjLogger.Log("RequestID:" + lcRequestID + "ClientID" + ClientId + "Prepaing Environment Variables")
  lcTime = Time() && Variable to hold the Time
  lcDate = Dtos(Date())
  ObjLogger.Log("Date: " + lcDate)
  ObjLogger.Log("Time: " + lcTime)
  *prepare EDI [End]******************************************************************************************************
  *call EDI Pipeline [Begin]
  Try
    If Type('laPartners[1]') = 'C'
      Set Step On
      oEDI_DLL = Createobject('MAIN')
      lcType = "S"
      lcPartnerType = "A"
      lcEdiTrnTyp = "856"
      lcProc = Strtran(Upper(oAriaApplication.ClassDir),"CLASSES","PRGS")+"EDIGLOBL.fxp"
      Set Procedure To (lcProc ) Additive

      lcProc = Strtran(Upper(oAriaApplication.clientprogramhome),"PRGS\")+"gmavmain.fxp"
      Set Procedure To  (lcProc)Additive

      *oEDI_DLL.SendingPipeline(loAgent,lcRequestID, ClientId, lcType, lcEdiTrnTyp, lcPartnerType, @laPartners, @laTransactions,.t., .t.)
      oEDI_DLL.SendingPipeline(loAgent, lcRequestID, ClientId, lcType, lcEdiTrnTyp, lcPartnerType, @laPartners,.F., .T., .T., 'SHPSH')

    Else
      ObjLogger.Log("No Accounts Passed")
      ObjLogger.Log("==========================================================")
    Endif
    loProgress.Percent = 100
    loProgress.Description = "Complete Sending Transactions"
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
    ObjLogger.Log("Request Runing Passed")
    ObjLogger.Log("==========================================================")
  Catch To ErrorVar
    ObjLogger.Log("Error Happen While Request Runing:" + Message())
    ObjLogger.Log("==========================================================")
    loProgress.Percent = 10
    loProgress.Description = "Stopped Sending Transactions - Error happen"
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  Endtry
  *call EDI Pipeline [End]


Endif
*!***************************************************************************
*! Name      : lfOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/2021
*! Purpose   : Opion grid when function
*!***************************************************************************
Function lfOGWhen
