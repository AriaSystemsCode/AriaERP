*!*****************************************************************************************
*! Name      : SOUPDNF.Prg
*! Developer : Mariam Mazher (MMT)
*! Date      : 08/15/2010
*! Purpose   : Upload and Download files from Amazon.com
*! Entry no. : E302735.exe and E302736.122[T20100205.0005]
*!*****************************************************************************************
*Modifications
*B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[T20100205.0005]
*N000682,1 HIA 02/27/2013 Replace SQLDICTIONARY with sysfiles
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
  * N000682,1 HIA 02/27/2013 Replace SQLDICTIONARY with sysfiles [START]
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SYSFILES\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SYSFILES"), "", -1, 1, 1)
  * N000682,1 HIA 02/27/2013 Replace SQLDICTIONARY with sysfiles [End]
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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcrpinPth = GETDIR('',LANG_STOREFILEIN)
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcHFileXML = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\SO\" + ALLTRIM("SOUPDNF")+"_"+"H" +".XML"
  lcHeaderAlias =  oAriaApplication.GetClassHeaderFile(lcHFileXML)
ENDIF  
lcrpinPth = GETDIR('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STOREFILEIN,oAriaApplication.GetHeaderText("LANG_STOREFILEIN",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcrpoutPth = GETDIR('',LANG_GETFILEFROM)
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcHFileXML = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\SO\" + ALLTRIM("SOUPDNF")+"_"+"H" +".XML"
  lcHeaderAlias =  oAriaApplication.GetClassHeaderFile(lcHFileXML)
ENDIF  
lcrpoutPth = GETDIR('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GETFILEFROM,oAriaApplication.GetHeaderText("LANG_GETFILEFROM",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
*!*************************************************************
*! Name      : lfvData
*: Developer : Mariam Mazhar[MMT]
*: Date      : 08/15/2010
*! Purpose   : Validate the OG DATA
*!*************************************************************
*
FUNCTION lfvData
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcHFileXML = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\SO\" + ALLTRIM("SOUPDNF")+"_"+"H" +".XML"
  lcHeaderAlias =  oAriaApplication.GetClassHeaderFile(lcHFileXML)
ENDIF  

IF EMPTY(lcFileNameXml)
  IF EMPTY(lcrpMrchID)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_MARCHIDINVALID)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MARCHIDINVALID,oAriaApplication.GetHeaderText("LANG_MARCHIDINVALID",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF EMPTY(lcrpMkplID)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_MARKETPINVALID)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MARKETPINVALID,oAriaApplication.GetHeaderText("LANG_MARKETPINVALID",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF EMPTY(lcrpAcsKey)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDACCESSKEY)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDACCESSKEY,oAriaApplication.GetHeaderText("LANG_INVALIDACCESSKEY",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF EMPTY(lcrpSecKey)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDSECRETKEY)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDSECRETKEY,oAriaApplication.GetHeaderText("LANG_INVALIDSECRETKEY",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF !DIRECTORY(lcrpinPth)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDDEST)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDDEST,oAriaApplication.GetHeaderText("LANG_INVALIDDEST",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

*
  IF EMPTY(lcrpInNam) OR (!EMPTY(lcrpInNam) AND UPPER(JUSTEXT(lcrpInNam))<> 'XML' )
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDFILENAME)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDFILENAME,oAriaApplication.GetHeaderText("LANG_INVALIDFILENAME",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF !DIRECTORY(lcrpoutPth)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_INVALIDSOURCE)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDSOURCE,oAriaApplication.GetHeaderText("LANG_INVALIDSOURCE",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  IF EMPTY(lcrpOutNam) OR (!EMPTY(lcrpOutNam) AND UPPER(JUSTEXT(lcrpOutNam))<> 'XML' )
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_OUTNAMEFILE)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OUTNAMEFILE,oAriaApplication.GetHeaderText("LANG_OUTNAMEFILE",lcHeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
  IF TYPE('gcRequestXMLFileName')<>'C'
    lcFileNameXml = lcRequestXMLFileName
    lcXml = loogscroll.ConvertVariablesTOXMl()
    = STRTOFILE(lcXml, lcFileNameXml )
    *B609436,1 MMT 10/17/2010 Add wait window to Upload\Download amzon files programs[Start]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_WAITUPLOADDOWNLOD NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_WAITUPLOADDOWNLOD,oAriaApplication.GetHeaderText("LANG_WAITUPLOADDOWNLOD",lcHeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
