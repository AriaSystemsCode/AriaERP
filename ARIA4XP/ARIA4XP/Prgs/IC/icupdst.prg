*:***************************************************************************
*: Program file  : ICUPDST
*: Program desc. : Update Stock Availability
*: Tracking#     : E612305
*:        System : Aria 4 XP
*:        Module : IC
*:     Developer : Mariam Mazhar (MMT)
*:     Developer : Hassan Ibrahim (HIA)
*:***************************************************************************
Parameters lcRequestID, lcXMLFileName, ClientId
Set Step On

*!*	look, use when run wihtout request builder [Begin]
*!*	lcRequestID = "c54b2e28-b7f6-4f13-b6a8-1b65ae0c08d1"
*!*	ClientId = "GMA10"
*!*	lcXMLFileName = 'X:\ARIA4XP\OUTPUT\XH0SUCH1.xml'
*!*	Set Classlib To "D:\SHARED\ARIA4XP\SRVCLSS\SY\requesthandler.vcx" AddI
*!*	goRemoteCall = Createobject("RemoteObject")
*!*	goRemoteCall.cInstanceName = ""
*!*	goRemoteCall.cServerName   = "ARIATESTING"
*!*	goRemoteCall.nPort         = 1500
*!*	look, use when run wihtout request builder [End]

If Type('lcXMLFileName') <> 'C'
  lcExpr = gfOpGrid('ICUPDST' , .T.)&&,.F.,.F.,.T.,.T.)
Else
  *** EDI code
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

  lcOrders =""
  lnOrdPos   = Ascan(laOgFXflt,"INVADVHD.CINVADVCNO")
  If lnOrdPos> 0
    lnOrdPos= Asubscript(laOgFXflt,lnOrdPos,1)
    lcOrders = laOgFXflt[lnOrdPos,6]
    If !Empty(lcOrders) And Used(lcOrders) And Reccount(lcOrders) > 0
      Select(lcOrders)
      Locate
      If Eof()
        lcOrders =""
      Endif
    Endif
  Endif
  Select (lcOrders)
  Locate
  iCounter = 0
  iCounterR = 0
  Dimension laPartners[1]
  Dimension laPartnersR[1]
  Dimension laTransactions[1]

  Scan
    If !Isnull(keyexp) And Type('keyexp') == 'C' And !Empty(Alltrim(keyexp))
      iCounter = iCounter + 1
      Dimension laPartners[iCounter]
      Dimension laTransactions[iCounter]
      lcKeyExp = Alltrim(keyexp)
      laPartners[iCounter] = Substr(lcKeyExp,1,At('-',lcKeyExp)-1)
      laTransactions[iCounter] = Substr(lcKeyExp,At('-',lcKeyExp)+1, Len(lcKeyExp))
      If Ascan(laPartnersR, laPartners[iCounter]) < 1
        iCounterR = iCounterR + 1
        Dimension laPartnersR[iCounterR]
        laPartnersR[iCounterR] = Substr(lcKeyExp,1,At('-',lcKeyExp)-1)
      Endif
    Endif
  Endscan

  *prepare EDI [Begin]******************************************************************************************************
  lcoldAppHome = oAriaApplication.ApplicationHome
  lcoldAppRep  = oAriaApplication.ReportHome
  lcoldAppBitMapHome = oAriaApplication.BitMapHome
  lcoldAppCls = oAriaApplication.ClassDir
  lcoldAppScx = oAriaApplication.ScreenHome
  loRemoteCompanyData = oAriaApplication.RemoteCompanyData

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

      oEDI_DLL = Createobject('MAIN')
      lcType = "R"
      lcPartnerType = "A" && AS comes from XML cursor are customers.
      lcEdiTrnTyp = "850"
      lcTransactionsCursor = ""
      lcProc = Strtran(Upper(oAriaApplication.ClassDir),"CLASSES","PRGS")+"EDIGLOBL.fxp"
      Set Procedure To (lcProc ) Additive

      lcProc = Strtran(Upper(oAriaApplication.clientprogramhome),"PRGS\")+"gmavmain.fxp"
      Set Procedure To  (lcProc)Additive
      oEDI_DLL.ReceivingPipeline(loAgent,lcRequestID, ClientId, lcType, lcEdiTrnTyp, lcPartnerType, @laPartnersR,.F.,'SHPPO')

      lcType = "S"
      lcPartnerType = "A"
      lcEdiTrnTyp = "846"
      oEDI_DLL.SendingPipeline(loAgent, lcRequestID, ClientId, lcType, lcEdiTrnTyp, lcPartnerType, @laPartners, @laTransactions, .F.,.F., 'SHPPD')

      * Call Carrier Integration*********************************************************************************
      * set classes to Aria4xp then call the class 'carrierintegration'

      oAriaApplication.ApplicationHome = Strtran(Upper(oAriaApplication.ApplicationHome ) , "ARIA3EDI", "ARIA4XP")
      oAriaApplication.ReportHome = Strtran(Upper(oAriaApplication.ReportHome) , "ARIA3EDI", "ARIA4XP")
      oAriaApplication.BitMapHome = Strtran(Upper(oAriaApplication.BitMapHome ), "ARIA3EDI", "ARIA4XP")
      oAriaApplication.ClassDir = Strtran(Upper(oAriaApplication.ClassDir), "ARIA3EDI", "ARIA4XP")
      oAriaApplication.ScreenHome = Strtran(Upper(oAriaApplication.ScreenHome ), "ARIA3EDI", "ARIA4XP")

      ObjLogger.Log("Call Carrier Integration")
      ObjLogger.Log("Time: " + lcTime)
      *        on error x=1
      oAriaApplication.ClassDir = Upper(oAriaApplication.ClassDir)
      oAriaApplication.ClassDir = Strtran(oAriaApplication.ClassDir, "ARIA3EDI", "ARIA4XP")
      Set Classlib To (oAriaApplication.ClassDir +'process_carrier_shipment.VCX') Additive
      *XX MMT
      Set Classlib To (oAriaApplication.ClassDir +'MAIN.VCX') Additive
      *XX MMT
      If Type('oariaapplication.systemmasterconnectionstring') = 'U'
        oAriaApplication.AddProperty("systemmasterconnectionstring", oAriaApplication.sqlsysfilesconnectionstring)
      Endif
      *X MMT
      If Type('oAriaApplication.oActiveLang.lIs_RTL') = 'U'
        oAriaApplication.oActiveLang.AddProperty("lIs_RTL", .F.)
      Endif

      If Type('oAriaApplication.lcseparator') = 'U'
        oAriaApplication.AddProperty("lcseparator", "")
      Endif
      *X MMT

      If Type('oAriaApplication.ProgramModuleID') = 'U'
        oAriaApplication.AddProperty("ProgramModuleID", "AL")
      Endif
      If Type('oAriaApplication.CLIENTAPPLICATIONHOME') = 'U'
        oAriaApplication.AddProperty("CLIENTAPPLICATIONHOME", "x:\aria4xp\")
      Endif
      If Type('oAriaApplication.VFPCHR0') = 'U'
        oAriaApplication.AddProperty("VFPCHR0", "")

      Endif
      If Type('oAriaApplication.ACTIVEPROGRAMHELPCNTX') = 'U'
        oAriaApplication.AddProperty("ACTIVEPROGRAMHELPCNTX", "")

      Endif
      Public LANG_OGTOOLBAR_DEFAULT
      LANG_OGTOOLBAR_DEFAULT = "default"

      Use (oAriaApplication.DataDir+"SETUPS.DBF") Shared In 0 Again
      oCarrierIntegration = Createobject('CarrierIntegration')

      oCarrierIntegration.STealth = llRpHdPS
      oCarrierIntegration.Labeltype = LCRPLABELTYPE
      oCarrierIntegration.Test = llRpTstL
      oCarrierIntegration.VAlidateaddress = Iif(llRPPRVERADD ="FALSE",.F.,.T.)
      oCarrierIntegration.lGenerateBol = llRpGenB
      oCarrierIntegration.transaction_type = 'O'

      *to fir Aria4xp issues [begin]
      If !Used("piktkt")
        Use Shared (oAriaApplication.DataDir+"piktkt") Order piktkt In 0
      Endif
      Set Proc To oAriaApplication.ApplicationHome+"SY\gfopgrid.fxp" AddI
      Set Proc To oAriaApplication.ApplicationHome+"SY\ariaglb.fxp" AddI
      *to fir Aria4xp issues [end]
      oCarrierIntegration.processlistoftransactions(lcTransactionsCursor,loAgent,lcRequestID,ClientId)

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

*!***************************************************************************
*! Name      : lfSRVINREP
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/03/2021
*! Purpose   : Set/Reset Transaction#
*!***************************************************************************
Function lfSRVINREP
Parameters OpGrdParm

Do Case
Case  OpGrdParm='S'
  lcTempFile = ''
  Select INVADVHD
  Set Filter To
  lnAcct = Ascan(loOgScroll.laOgFXflt,"CUSTOMER.ACCOUNT")
  lnAcct = Asubscript(loOgScroll.laOgFXflt,lnAcct,1)
  lcAccFlt= loOgScroll.laOgFXflt[lnAcct ,6]
  If !Empty(lcAccFlt) And Used(lcAccFlt) And Reccount(lcAccFlt)> 0
    Select(lcAccFlt)
    Locate
    If !Eof()
      lcTempFile =lcAccFlt
    Endif
  Endif
Case  OpGrdParm='R'
  Select INVADVHD
  Set Filter To
Endcase
