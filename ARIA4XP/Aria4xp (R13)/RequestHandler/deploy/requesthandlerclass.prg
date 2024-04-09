************************************************************************
*PROCEDURE RequestHandlerClass
****************************
***  Function: Processes incoming Web Requests for RequestHandlerClass
***            requests. This function is called from the wwServer 
***            process.
***      Pass: loServer -   wwServer object reference
*************************************************************************
LPARAMETER loServer
LOCAL loProcess
PRIVATE Request, Response, Server, Session, Process
STORE NULL TO Request, Response, Server, Session, Process

#INCLUDE WCONNECT.H

loProcess = CREATEOBJECT("RequestHandlerClass", loServer)
loProcess.lShowRequestData = loServer.lShowRequestData

IF VARTYPE(loProcess)#"O"
   *** All we can do is return...
   RETURN .F.
ENDIF

*** Call the Process Method that handles the request
loProcess.Process()

*** Explicitly force process class to release
loProcess.Dispose()

RETURN

*************************************************************
DEFINE CLASS RequestHandlerClass AS WWC_RESTPROCESS
*************************************************************

*** Response class used - override as needed
cResponseClass = [WWC_PAGERESPONSE]

*** Default for page script processing if no method exists
*** 1 - MVC Template (ExpandTemplate()) 
*** 2 - Web Control Framework Pages
*** 3 - MVC Script (ExpandScript())
nPageScriptMode = 3

*!* cAuthenticationMode = "UserSecurity"  && `Basic` is default


*** ADD PROCESS CLASS EXTENSIONS ABOVE - DO NOT MOVE THIS LINE ***


#IF .F.
* Intellisense for THIS
LOCAL THIS as RequestHandlerClass OF RequestHandlerClass.prg
#ENDIF
 
*********************************************************************
* Function RequestHandlerClass :: OnProcessInit
************************************
*** If you need to hook up generic functionality that occurs on
*** every hit against this process class , implement this method.
*********************************************************************
FUNCTION OnProcessInit

*!* LOCAL lcScriptName, llForceLogin
*!*	THIS.InitSession("MyApp")
*!*
*!*	lcScriptName = LOWER(JUSTFNAME(Request.GetPhysicalPath()))
*!*	llIgnoreLoginRequest = INLIST(lcScriptName,"default","login","logout")
*!*
*!*	IF !THIS.Authenticate("any","",llIgnoreLoginRequest) 
*!*	   IF !llIgnoreLoginRequest
*!*		  RETURN .F.
*!*	   ENDIF
*!*	ENDIF

*** Explicitly specify that pages should encode to UTF-8 
*** Assume all form and query request data is UTF-8
Response.Encoding = "UTF8"
Request.lUtf8Encoding = .T.


*** Add CORS header to allow cross-site access from other domains/mobile devices on Ajax calls
*!* Response.AppendHeader("Access-Control-Allow-Origin","*")
*!* Response.AppendHeader("Access-Control-Allow-Origin",Request.ServerVariables("HTTP_ORIGIN"))
*!* Response.AppendHeader("Access-Control-Allow-Methods","POST, GET, DELETE, PUT, OPTIONS")
*!* Response.AppendHeader("Access-Control-Allow-Headers","Content-Type, *")
*!* *** Allow cookies and auth headers
*!* Response.AppendHeader("Access-Control-Allow-Credentials","true")
*!* 
*!* *** CORS headers are requested with OPTION by XHR clients. OPTIONS returns no content
*!*	lcVerb = Request.GetHttpVerb()
*!*	IF (lcVerb == "OPTIONS")
*!*	   *** Just exit with CORS headers set
*!*	   *** Required to make CORS work from Mobile devices
*!*	   RETURN .F.
*!*	ENDIF   


RETURN .T.
ENDFUNC


*********************************************************************
FUNCTION TestPage
***********************
LPARAMETERS lvParm
*** Any posted JSON string is automatically deserialized
*** into a FoxPro object or value

#IF .F. 
* Intellisense for intrinsic objects
LOCAL Request as wwRequest, Response as wwPageResponse, Server as wwServer, ;
      Process as wwProcess, Session as wwSession
#ENDIF

*** Simply create objects, collections, values and return them
*** they are automatically serialized to JSON
loObject = CREATEOBJECT("EMPTY")
ADDPROPERTY(loObject,"name","TestPage")
ADDPROPERTY(loObject,"description",;
            "This is a JSON API method that returns an object.")
ADDPROPERTY(loObject,"entered",DATETIME())

*** To get proper case you have to override property names
*** otherwise all properties are serialized as lower case in JSON
Serializer.PropertyNameOverrides = "Name,Description,Entered"


RETURN loObject

*** To return a cursor use this string result:
*!* RETURN "cursor:TCustomers"


*** To return a raw Response result (non JSON) use:
*!*	JsonService.IsRawResponse = .T.   && use Response output
*!*	Response.ExpandScript()
*!*	RETURN                            && ignored

ENDFUNC


*********************************************************************
FUNCTION HelloScript()
***********************

SELECT TOP 10 time, script, querystr, verb, remoteaddr ;
  FROM wwRequestLog  ;
  INTO CURSOR TRequests ;
  ORDER BY Time Desc

loObj = CREATEOBJECT("EMPTY")
ADDPROPERTY(loObj,"message","Surprise!!! This is not a script response! Instead we'll return you a cursor as a JSON result.")
ADDPROPERTY(loObj,"requestName","Recent Requests")
ADDPROPERTY(loObj,"recentRequests","cursor:TRequests")
ADDPROPERTY(loObj,"recordCount",_Tally)

*** Normalize property names
Serializer.PropertyNameOverrides = "requestName,recentRequests,recordCount"

RETURN loObj
ENDFUNC




*************************************************************
*** PUT YOUR OWN CUSTOM METHODS HERE                      
*** 
*** Any method added to this class becomes accessible
*** as an HTTP endpoint with MethodName.Extension where
*** .Extension is your scriptmap. If your scriptmap is .rs
*** and you have a function called Helloworld your
*** endpoint handler becomes HelloWorld.rs
*************************************************************


FUNCTION PrintReport
*!*	DECLARE INTEGER Sleep IN WIN32API INTEGER
*!*	Sleep(1000)
SET STEP ON 

*!*	loProcess.Active = .T.
lcServerName = ''
lcRequestID= Request.QueryString("lcRequestID")
loXMLCursor= Request.QueryString("loXMLCursor")
ClientID= Request.QueryString("ClientID")
lcOptionGridID= Request.QueryString("lcOptionGridID")
lcServerName= Request.QueryString("lcServerName")
lcInstanceName= Request.QueryString("lcInstanceName")
lnPort= VAL(Request.QueryString("lnPort"))
 
lcTempPath =  Request.QueryString("lcTempPath")
lcinnerText1= Request.QueryString("lcinnerText1")
lcstr1= Request.QueryString("lcstr1")
lcstr2= Request.QueryString("lcstr2")
lcstr3= Request.QueryString("lcstr3")
llflag= Request.QueryString("llflag")
llflag= IIF(llflag='T',.T.,.F.)
*!*	PUBLIC gcInstanceName, gcServerName, gnPort,gcRequestID,gcClientID
*!*	gcInstanceName = lcInstanceName
*!*	gcServerName   = lcServerName
*!*	gnPort         = lnPort
*!*	SET CLASSLIB TO C:\aria\R13\Aria4xp\srvclss\sy\requesthandler.vcx ADDITIVE 
*!*	PUBLIC goRemoteCall
*!*	goRemoteCall = CREATEOBJECT("RemoteObject")
*!*	goRemoteCall.cInstanceName = lcInstanceName
*!*	goRemoteCall.cServerName   = lcServerName
*!*	goRemoteCall.nPort         = lnPort
*!*	gcRequestID =lcRequestID
*!*	gcClientID = ClientID
*!*	LOCAL loEnvironment
*!*	loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
*!*	loEnvironment.ClientId = ClientId
*!*	loEnvironment.ConnectionsRefresh()

PUBLIC gcInstanceName, gcServerName, gnPort,gcRequestID,gcClientID
gcInstanceName = lcInstanceName
gcServerName   = lcServerName
gnPort         = lnPort
*SET CLASSLIB TO D:\Shared\Aria4xp\srvclss\sy\requesthandler.vcx ADDITIVE 
PUBLIC goRemoteCall
goRemoteCall = CREATEOBJECT("RemoteObject")
goRemoteCall.cInstanceName = lcInstanceName
goRemoteCall.cServerName   = lcServerName
goRemoteCall.nPort         = lnPort

PRIVATE loAgentUpdate
loAgentUpdate = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
TRY 
  gcRequestID =lcRequestID
  gcClientID = ClientID
  LOCAL loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientId = ClientId
  loEnvironment.ConnectionsRefresh()

LOCAL lcSharePath, lcClientPath

lcSharePath = ADDBS(ALLTRIM(loEnvironment.Aria40SharedPath))

lcClientPath = ADDBS(ALLTRIM(loEnvironment.Aria40SystemFilesPath))
lcClientPath = STRTRAN(UPPER(lcClientPath), "SYSFILES\", "\")



LOCAL lcModule
lcModule = SUBSTR(lcOptionGridID, 1, 2)

DO CASE
CASE FILE(lcClientPath + "Reports\" + lcModule + "\" + lcOptionGridID + ".FXP")
  DO (lcClientPath + "Reports\" + lcModule + "\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcClientPath + "Reports\" + lcOptionGridID + ".FXP")
  DO (lcClientPath + "Reports\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcSharePath + "Reports\" + lcModule + "\" + lcOptionGridID + ".FXP")
  DO (lcSharePath + "Reports\" + lcModule + "\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcSharePath + "Reports\" + lcOptionGridID + ".FXP")
  DO (lcSharePath + "Reports\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID
*!*	loProcess.Active = .F.  
ENDCASE
IF TYPE('lcTempPath') ='C' AND DIRECTORY(lcTempPath)
  loAgentUpdate.CallAgentSendMail(lcTempPath, ClientID,lcinnerText1,lcstr1, lcstr2, lcstr3, llflag)
ELSE 
  loAgentUpdate.CallRequestNotifications(lcRequestID, ClientID)
ENDIF 
CATCH 
 loAgentUpdate.ErrorNoification(lcRequestID,MESSAGE(),ClientID)
ENDTRY

ENDFUNC 
FUNCTION Execute 
SET STEP ON 

*!*	loProcess.Active = .T.
lcServerName = ''
lcRequestID= Request.QueryString("lcRequestID")
loXMLCursor= Request.QueryString("loXMLCursor")
ClientID= Request.QueryString("ClientID")
lcOptionGridID= Request.QueryString("lcOptionGridID")
lcServerName= Request.QueryString("lcServerName")
lcInstanceName= Request.QueryString("lcInstanceName")
lnPort= VAL(Request.QueryString("lnPort"))
 
lcTempPath =  Request.QueryString("lcTempPath")
lcinnerText1= Request.QueryString("lcinnerText1")
lcstr1= Request.QueryString("lcstr1")
lcstr2= Request.QueryString("lcstr2")
lcstr3= Request.QueryString("lcstr3")
llflag= Request.QueryString("llflag")
llflag= IIF(llflag='T',.T.,.F.)
*!*	PUBLIC gcInstanceName, gcServerName, gnPort,gcRequestID,gcClientID
*!*	gcInstanceName = lcInstanceName
*!*	gcServerName   = lcServerName
*!*	gnPort         = lnPort
*!*	SET CLASSLIB TO C:\aria\R13\Aria4xp\srvclss\sy\requesthandler.vcx ADDITIVE 
*!*	PUBLIC goRemoteCall
*!*	goRemoteCall = CREATEOBJECT("RemoteObject")
*!*	goRemoteCall.cInstanceName = lcInstanceName
*!*	goRemoteCall.cServerName   = lcServerName
*!*	goRemoteCall.nPort         = lnPort
*!*	gcRequestID =lcRequestID
*!*	gcClientID = ClientID
*!*	LOCAL loEnvironment
*!*	loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
*!*	loEnvironment.ClientId = ClientId
*!*	loEnvironment.ConnectionsRefresh()

PUBLIC gcInstanceName, gcServerName, gnPort,gcRequestID,gcClientID
gcInstanceName = lcInstanceName
gcServerName   = lcServerName
gnPort         = lnPort
*SET CLASSLIB TO D:\Shared\Aria4xp\srvclss\sy\requesthandler.vcx ADDITIVE 
PUBLIC goRemoteCall
goRemoteCall = CREATEOBJECT("RemoteObject")
goRemoteCall.cInstanceName = lcInstanceName
goRemoteCall.cServerName   = lcServerName
goRemoteCall.nPort         = lnPort

PRIVATE loAgentUpdate
loAgentUpdate = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
TRY 
  gcRequestID =lcRequestID
  gcClientID = ClientID
  LOCAL loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientId = ClientId
  loEnvironment.ConnectionsRefresh()

LOCAL lcSharePath, lcClientPath

lcSharePath = ADDBS(ALLTRIM(loEnvironment.Aria40SharedPath))

lcClientPath = ADDBS(ALLTRIM(loEnvironment.Aria40SystemFilesPath))
lcClientPath = STRTRAN(UPPER(lcClientPath), "SYSFILES\", "\")

LOCAL lcModule
lcModule = SUBSTR(lcOptionGridID, 1, 2)

DO CASE
*E303349,2 SAB 09/26/2013 Fix Rebalance to run from Request Builder [T20120316.0004][Start]
CASE lcOptionGridID = 'SMREBAL' AND FILE(lcSharePath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP")
  DO (lcSharePath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP") WITH .F., .F., .F., .F., .F., .F., lcRequestID, loXMLCursor, ClientID
*E303349,2 SAB 09/26/2013 Fix Rebalance to run from Request Builder [T20120316.0004][End]
CASE FILE(lcClientPath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP")
  DO (lcClientPath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcClientPath + "Prgs\" + lcOptionGridID + ".FXP")
  DO (lcClientPath + "Prgs\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcSharePath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP")
  DO (lcSharePath + "Prgs\" + lcModule + "\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID

CASE FILE(lcSharePath + "Prgs\" + lcOptionGridID + ".FXP")
  DO (lcSharePath + "Prgs\" + lcOptionGridID + ".FXP") WITH lcRequestID, loXMLCursor, ClientID
  
ENDCASE
IF TYPE('lcTempPath') ='C' AND DIRECTORY(lcTempPath)
  loAgentUpdate.CallAgentSendMail(lcTempPath, ClientID,lcinnerText1,lcstr1, lcstr2, lcstr3, llflag)
ELSE 
  loAgentUpdate.CallRequestNotifications(lcRequestID, ClientID)
ENDIF 
CATCH 
 loAgentUpdate.ErrorNoification(lcRequestID,MESSAGE(),ClientID)
ENDTRY
ENDDEFINE
DEFINE CLASS RemoteObject  as Custom
  cInstanceName = ''
  cServerName = ''
  nPort = 0
  FUNCTION GetRemoteObject  
    LPARAMETERS lcObjectName
    LOCAL loRemoteCall
    loRemoteCall = CREATEOBJECT("Aria.Utilities.RemoteCall.AriaActivator")
    loRemoteObject = loRemoteCall.GetRemoteObject(lcObjectName + This.cInstanceName, This.cServerName, This.nPort)
    RETURN loRemoteObject
  ENDFUNC  
ENDDEFINE

