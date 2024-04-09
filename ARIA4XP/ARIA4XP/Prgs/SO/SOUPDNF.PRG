*!*****************************************************************************************
*! Name      : SOUPDNF.Prg
*! Developer : Mariam Mazher (MMT)
*! Date      : 08/15/2010
*! Purpose   : Upload and Download files from Amazon.com  
*! Entry no. : E302735.exe and E302736.122[T20100205.0005]
*!*****************************************************************************************
*Modifications
*B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[T20100205.0005]
*!*****************************************************************************************
#INCLUDE R:\Aria4XP\PRGs\SO\SOUPDNF.h
PARAMETERS lcRequestID, lcXMLFileName, ClientID
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()


  LOCAL lcCurrentProcedure
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID), ClientID


  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.Description = LANG_CONNECT
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)



  lcOldLib = SET("Classlib")
  goAriaEnvironment = oAriaEnvironment
  loAWS = CREATEOBJECT("AWS.Main")
  loAWS.Init("",lcXMLFileName,"")
  
  loProgress.Percent = 1.0
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)  
ELSE
  lcXmlFilName = gfTempName()
  lcOutFolder = oAriaApplication.WorkDir
  lcRequestXMLFileName  = ADDBS(lcOutFolder)+lcXmlFilName+'.XML' 
  lcExpr = gfOpGrid('SOUPDNF' , .T.)
ENDIF
*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Mariam Mazhar[MMT]
*: Date      : 08/15/2010 
*! Purpose   : the When Function of the OG
*!*************************************************************
*
FUNCTION lfwRepWhen

lcA27Path = ADDBS(STRTRAN(UPPER(oAriaApplication.SysPath),'\SYSFILES',''))
IF EMPTY(lcrpinPth)
  IF DIRECTORY(lcA27Path +'EDI\INBOX')
    lcrpinPth = lcA27Path +'EDI\INBOX'
  ENDIF  
ENDIF  

IF EMPTY(lcrpoutPth) 
  IF DIRECTORY(lcA27Path +'EDI\OUTBOX')
    lcrpoutPth = lcA27Path +'EDI\OUTBOX'
  ENDIF  
ENDIF  
*!*************************************************************
*! Name      : lfvInPth
*: Developer : Mariam Mazhar[MMT]
*: Date      : 08/15/2010 
*! Purpose   : Validate the IN Path
*!*************************************************************
*
FUNCTION lfvInPth
IF '?' $ lcrpinPth  OR !DIRECTORY(lcrpinPth)
  lcrpinPth = GETDIR('',LANG_STOREFILEIN) 
ENDIF 
*!*************************************************************
*! Name      : lfvOutPth
*: Developer : Mariam Mazhar[MMT]
*: Date      : 08/15/2010 
*! Purpose   : Validate the OUT Path
*!*************************************************************
*
FUNCTION lfvOutPth
IF '?' $ lcrpoutPth OR !DIRECTORY(lcrpoutPth)
  lcrpoutPth = GETDIR('',LANG_GETFILEFROM) 
ENDIF 
*!*************************************************************
*! Name      : lfvData
*: Developer : Mariam Mazhar[MMT]
*: Date      : 08/15/2010 
*! Purpose   : Validate the OG DATA
*!*************************************************************
*
FUNCTION lfvData
IF EMPTY(lcFileNameXml)
  IF EMPTY(lcrpMrchID)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_MARCHIDINVALID)
    RETURN .F.
  ENDIF 

  IF EMPTY(lcrpMkplID)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_MARKETPINVALID)
    RETURN .F.
  ENDIF 

  IF EMPTY(lcrpAcsKey)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDACCESSKEY)
    RETURN .F.
  ENDIF 

  IF EMPTY(lcrpSecKey)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDSECRETKEY)
    RETURN .F.
  ENDIF 

  IF !DIRECTORY(lcrpinPth)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDDEST)
    RETURN .F.
  ENDIF 

*
  IF EMPTY(lcrpInNam) OR (!EMPTY(lcrpInNam) AND UPPER(JUSTEXT(lcrpInNam))<> 'XML' )
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDFILENAME)
    RETURN .F.
  ENDIF 

  IF !DIRECTORY(lcrpoutPth) 
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDSOURCE)
    RETURN .F.
  ENDIF 

  IF EMPTY(lcrpOutNam) OR (!EMPTY(lcrpOutNam) AND UPPER(JUSTEXT(lcrpOutNam))<> 'XML' )
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_OUTNAMEFILE)
    RETURN .F.
  ENDIF 
  IF TYPE('gcRequestXMLFileName')<>'C' 
    lcFileNameXml = lcRequestXMLFileName  
    lcXml = loogscroll.ConvertVariablesTOXMl()
    = STRTOFILE(lcXml, lcFileNameXml )
    *B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[Start]
    WAIT WINDOW LANG_WAITUPLOADDOWNLOD NOWAIT 
    *B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[End]
    loAWS = CREATEOBJECT("AWS.Main")
    loAWS.Init("",lcFileNameXml,"")    
    *B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[Start]
    loAWS = NULL
	*B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[End]
    lcFileNameXml = .F.
    *Call DLL using this file
  ENDIF  
ENDIF