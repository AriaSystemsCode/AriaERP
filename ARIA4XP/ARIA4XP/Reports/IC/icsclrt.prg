*:***************************************************************************
*: Program file  : ICSLCRT
*: Program desc. : Scales Report
*: For Report    : (ICSLCRTE.FRX,ICSLCRTN.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : Inventory Control (IC)
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfModalGen,gfRange,lfwOgWhen,lfGetForm.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICSLCRT
*:***************************************************************************
*: This Report Program is due to E301117 ...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: N000621,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[T20090805.0001]
*: N000621,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[T20100512.0026]
*:***************************************************************************
*: N000621,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[Start]
*!*	lcStTime = TIME()
*!*	DIMENSION laPrePaks[1,1]
*!*	loogscroll.cCROrientation = 'P'
*: N000621,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[End]

*N000621,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[Start]

*: N000621,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[Start]
*!*	IF oAriaApplication.MULTIINST 
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\IC\ICSCLRT.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\IC\ICSCLRT.FXP    
*!*	ELSE
*!*	  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
*!*	  DO lcSrvRpt+"IC\ICSCLRT.FXP" WITH .F.,.F.
*!*	ENDIF 
PARAMETERS lcRequestID, lcXMLFileName, ClientID

*T20100512.0026 Hassan 2010 05 23 [END]
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL lcCurrentProcedure
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)

  lcCurrentProcedure = ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath))
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDICTIONARY\"), "", -1, 1, 1)

  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
  *T20100512.0026 Hassan 2010 05 23 [END]

  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  oAriaEnvironment.REPORT.gcAct_Appl = 'IC'

  PUBLIC gcAct_Appl
  gcAct_Appl = 'IC'
  oAriaEnvironment.activeModuleID = 'IC'

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.Report.cCROrientation = 'P'
ELSE
  loogscroll.cCROrientation = 'P'
ENDIF

lcStTime = TIME()
DIMENSION laPrePaks[1,1]
IF TYPE('lcXMLFileName') = 'C'
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir + 'SCALE' ,'SCALE','SH',.F.,.F.)
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir + 'SCALEHD' ,'EXTSCALE','SH',.F.,.F.)
ELSE
  gfOpenTable(oAriaApplication.DataDir + 'SCALE' ,'SCALE','SH',.F.,.F.)
  gfOpenTable(oAriaApplication.DataDir + 'SCALEHD' ,'EXTSCALE','SH',.F.,.F.)
ENDIF

*-- N.R.F. Expression
lcNrfExp = [!(EMPTY(cNRFCode1) AND EMPTY(cNRFCode2) AND EMPTY(cNRFCode3) AND EMPTY(cNRFCode4) AND EMPTY(cNRFCode5) AND EMPTY(cNRFCode6) AND EMPTY(cNRFCode7) AND EMPTY(cNRFCode8))]

SELECT SCALE
*-- Create temp. file have scales and prepacks. [Begin]
IF TYPE('lcXMLFileName') = 'C'
  lcTempScl  = oAriaEnvironment.CURSORS.GetCursorTempName()
ELSE
  lcTempScl = gfTempName()
ENDIF

DIMENSION laStruArr[1,4]
=AFIELDS(laStruArr)
CREATE CURSOR (lcTempScl) FROM ARRAY laStruArr
SELECT (lcTempScl)
ZAP

IF TYPE('lcXMLFileName') = 'C'
  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) &&OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
ELSE
  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) &&OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
ENDIF
*-- Create temp. file have scales and prepacks. [End  ]

SELECT SCALE
*-- Make report expression (Optimized expression) [Begin
IF lcRpExp = '.T.'
  lcRpExp = [type+scale+prepak = '']
ENDIF

*-- if Extended size scale
IF llExtSize
  SET RELATION TO LEFT(SCALE,lnExtWidth) INTO SCALEHD
ENDIF

lcRpExp = STRTRAN(lcRpExp,'SCALE.','')

*-- Make report expression (Optimized expression) [End..
*-- Loop Scale file to add filtered lines. [Begin
SCAN FOR &lcRpExp AND TYPE='S'
  IF TYPE('lcXMLFileName') = 'C'
    loProgress.DESCRIPTION = "Collecting Data..."
  ENDIF
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempScl) FROM MEMVAR
ENDSCAN

*-- Loop Scale file to add filtered lines. [End..
SELECT (lcTempScl)

IF RECCOUNT(lcTempScl) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  IF TYPE('lcXMLFileName') = 'C'
    =gfModalGen('TRM00052B40011','ALERT')
  ENDIF   
  RETURN
ENDIF

IF llRpPrepak OR llRpNRF
  SCAN FOR TYPE = 'S'

    lcCurrRec = 'S' + SCALE
    IF llRpPrepak 
       IF TYPE('lcXMLFileName') = 'C'
	     =oAriaEnvironment.remotetableaccess.SeekRecord('P'+&lcTempScl..SCALE,'SCALE')
	   ELSE  
	     =gfSeek('P'+&lcTempScl..SCALE,'SCALE')
	   ENDIF  
       SELECT SCALE
       SCAN REST WHILE TYPE+SCALE+PREPAK = 'P' + &lcTempScl..SCALE
         IF pPTot > 0
           SCATTER MEMVAR MEMO
           m.Cnt = &lcTempScl..CNT
           INSERT INTO (lcTempScl) FROM MEMVAR
         ENDIF
       ENDSCAN
       =Seek(lcCurrRec,lcTempScl)
     ENDIF

     SELECT(lcTempScl)

     IF llRpNRF AND EVALUATE(lcNrfExp)
       SCATTER MEMVAR MEMO
       m.Type      = 'N'
       m.PREPAK    = 'Z'
       INSERT INTO (lcTempScl) FROM MEMVAR
       =Seek(lcCurrRec,lcTempScl)
     ENDIF
  ENDSCAN
ENDIF

IF llExtSize
  IF TYPE('lcXMLFileName') = 'C'
    INDEX ON LEFT(SCALE,lnExtWidth) + PREPAK TAG (lcTempScl) &&OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
  ELSE
    INDEX ON LEFT(SCALE,lnExtWidth) + PREPAK TAG (lcTempScl) &&OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
  ENDIF
  *-- Make relation with header file.
  SELECT SCALE
  SET RELATION TO
  SELECT (lcTempScl)
  SET RELATION TO LEFT(SCALE,lnExtWidth) INTO SCALEHD
ELSE
  IF TYPE('lcXMLFileName') = 'C'
    INDEX ON SCALE + PREPAK TAG (lcTempScl) &&OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
  ELSE
    INDEX ON SCALE + PREPAK TAG (lcTempScl) &&OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
  ENDIF
ENDIF

STORE .T. TO llPackFlag,llNrfFlag
llHavePp   = .F.
lcPrepak   = ''
lcCurrType = ' '



IF TYPE('lcXMLFileName') = 'C'
  oAriaEnvironment.REPORT.OGLastForm = lcRpForm
  loProgress.Percent = 0.9
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  IF loProxy.GetRequest(lcRequestID, ClientID).STATUS = 3
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    *T20100512.0026 Hassan 2010 05 23 [BEGIN]
    *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    *T20100512.0026 Hassan 2010 05 23 [END]
  ENDIF
ELSE
  DO gfdispre WITH EVALUATE('lcRpForm')
ENDIF
USE IN (lcTempScl)
*-- end of program code.



FUNCTION lfRestFromXML
LPARAMETERS lcXML

LOCAL lobjDOMDocument
lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
lobjDOMDocument.LOADXML(lcXML)

LOCAL loRoot
loRoot = lobjDOMDocument.childNodes(1).childNodes(0)


PRIVATE  laOGHdFlt[1, 8]
PRIVATE  laOGFxFlt[1, 8]
PRIVATE  laOGVrFlt[1, 8]

LOCAL lnIndex
LOCAL loVariable, lcName, lcDataType, lcValue

FOR lnIndex = 0 TO loRoot.childNodes.LENGTH - 1
  loVariable = loRoot.childNodes(lnIndex)
  lcDataType = loVariable.childNodes(0).TEXT
  lcName     = loVariable.childNodes(1).TEXT
  lcValue    = loVariable.childNodes(2).TEXT

  DO CASE
  CASE UPPER(lcName) = UPPER('laOGHdFlt[')
    DIMENSION laOGHdFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue

  CASE UPPER(lcName) = UPPER('laOGFxFlt[')
    DIMENSION laOGFxFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue

  CASE UPPER(lcName) = UPPER('laOGVrFlt[')
    DIMENSION laOGVrFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue

  CASE UPPER(lcDataType) = UPPER('System.String')
    PRIVATE &lcName.
    &lcName. = lcValue

  CASE UPPER(lcDataType) = UPPER('System.Boolean')
    PRIVATE  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = IIF(lcValue = 'true', .T., .F.)
    ENDIF

  CASE UPPER(lcDataType) = UPPER('System.Decimal')
    PRIVATE  &lcName.
    &lcName. = VAL(lcValue)

  CASE UPPER(lcDataType) = UPPER('System.Datetime')
    PRIVATE  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = DATE(VAL(LEFT(lcValue, 4)), VAL(SUBSTR(lcValue, 5, 2)), VAL(SUBSTR(lcValue, 7, 2)))
    ENDIF

  CASE UPPER(lcDataType) = UPPER('System.Table')
    IF !EMPTY(lcValue) .AND. !EMPTY(lcName)
      LOCAL lnSelected
      lnSelected = SELECT()

      LOCAL loXMLParase
      loXMLParase = NEWOBJECT("wwxml")

      DIMENSION laStruct[loVariable.childNodes(3).childNodes.length,4]

      FOR nCount = 0 TO loVariable.childNodes(3).childNodes.LENGTH -1
        laStruct[nCount+1,1] = loVariable.childNodes(3).childNodes(nCount).childNodes (0).TEXT
        laStruct[nCount+1,2] = loVariable.childNodes(3).childNodes(nCount).childNodes (1).TEXT
        laStruct[nCount+1,3] = loVariable.childNodes(3).childNodes(nCount).childNodes (2).TEXT
        laStruct[nCount+1,4] = loVariable.childNodes(3).childNodes(nCount).childNodes (3).TEXT
      ENDFOR

      CREATE CURSOR (lcName) FROM ARRAY laStruct
      loXMLParase.XMLTOCURSOR(lcValue, lcName)

      INDEX ON KEYEXP TAG KEYEXP

      SELECT(lnSelected)

    ENDIF
  ENDCASE
ENDFOR
*: N000621,2 MMT 07/21/2010  Calling the program from reports folder to collect data and display report[End]
*N000621,1 AHS Calling the program from server reports folder to collect data and display report it not SAAS[End]

*-- end of program code.

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Mariam MAzhar (MMT)
*! Date      : 09/28/2008
*! Purpose   : OG when function
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************
*!
FUNCTION lfwOgWhen
= gfOpenTable(oAriaApplication.DataDir + 'SCALE' ,'SCALE','SH')
= gfOpenTable(oAriaApplication.DataDir + 'SCALEHD' ,'Extscale','SH')
*-- end of lfwOgWhen.

*!*************************************************************
*! Name      : lfFilMmVar
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Fill Memory variables.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfFilMmVar()
*!*************************************************************
*!
FUNCTION lfFilMmVar
PRIVATE laSetups

DIMENSION laSetups[2,2]
laSetups[1,1] = 'M_USEEXSSC'
laSetups[2,1] = 'M_EXTWIDTH'
=gfGetMemVar(@laSetups)
llExtSize  = laSetups[1,2]
lnExtWidth = laSetups[2,2]
lcRpForm   = IIF(llExtSize,'ICSCLRTE','ICSCLRTN')
*-- end of lfFilMmVar.


*!*************************************************************
*! Name      : lfSScale
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Scale In Range set function.
*!*************************************************************
*! Passed Parameters : Dummy Parameter for set only.
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfSScale()
*!*************************************************************
*!
FUNCTION lfSScale
PARAMETERS lcDummyPrm
IF llExtSize
  GO TOP IN SCALEHD
ELSE
  GO TOP IN SCALE
ENDIF
*-- end of lfSScale.
*!*************************************************************
*! Name      : lfPackFlag
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfPackFlag()
*!*************************************************************
*!
FUNCTION lfPackFlag
STORE (Type = 'S') TO llPackFlag,llNrfFlag
RETURN ''
*-- end of lfPackFlag.

*!*************************************************************
*! Name      : lfHavePp
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Rising Prepak flag.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfHavePp()
*!*************************************************************
*!
FUNCTION lfHavePp
llHavePp = .F.
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  IF pP&lcI > 0
    llHavePp = .T.
    EXIT
  ENDIF
ENDFOR
RETURN llHavePp
*-- end of lfHavePp.

*!*************************************************************
*! Name      : lfInitVal
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Initial prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfInitVal
lcPrepak = ''
RETURN lcPrepak
*-- end of lfInitVal.

*!*************************************************************
*! Name      : lfCurrVal
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Current prepak value.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfInitVal()
*!*************************************************************
*!
FUNCTION lfCurrVal
lcPrepak = PREPAK
RETURN ''
*-- end of lfCurrVal.
*!*************************************************************
*! Name      : lfCurrType
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Save current type
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfCurrType()
*!*************************************************************
*!
FUNCTION lfCurrType
lcCurrType = Type
RETURN ''
*!*************************************************************
*! Name      : lfNRFLine
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Return NRF codes.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : NRF Line
*!*************************************************************
*! Example     : =lfNRFLine()
*!*************************************************************
*!
FUNCTION lfNRFLine
PRIVATE lcNRFLine
IF llExtSize
  lcNRFLine = SPACE(3) + RIGHT(SCALE,(3-lnExtWidth))+SPACE(19)+;
              PADL(ALLTRIM(cNRFCode1),5)+' '+PADL(ALLTRIM(cNRFCode2),5)+' '+;
              PADL(ALLTRIM(cNRFCode3),5)+' '+PADL(ALLTRIM(cNRFCode4),5)+' '+;
              PADL(ALLTRIM(cNRFCode5),5)+' '+PADL(ALLTRIM(cNRFCode6),5)+' '+;
              PADL(ALLTRIM(cNRFCode7),5)+' '+PADL(ALLTRIM(cNRFCode8),5)
ELSE
  lcNRFLine = 'N.R.F.:  ' + SPACE(24)+;
              PADL(ALLTRIM(cNRFCode1),5)+' '+PADL(ALLTRIM(cNRFCode2),5)+' '+;
              PADL(ALLTRIM(cNRFCode3),5)+' '+PADL(ALLTRIM(cNRFCode4),5)+' '+;
              PADL(ALLTRIM(cNRFCode5),5)+' '+PADL(ALLTRIM(cNRFCode6),5)+' '+;
              PADL(ALLTRIM(cNRFCode7),5)+' '+PADL(ALLTRIM(cNRFCode8),5)
ENDIF              
RETURN lcNRFLine
*-- end of lfNRFLine.
*!*************************************************************
*! Name      : lfScalHd
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 09/28/2008
*! Purpose   : Scale Header extended scale case.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : =lfScalHd()
*!*************************************************************
*!
FUNCTION lfScalHd
PRIVATE lcScaleHd
IF ScaleHd.nnoofdim = 1
  lcScaleHd = 'One Dimension '
ELSE
  lcScaleHd = IIF(ScaleHd.nnoofdim=2,'Two ','Three ') + 'Dimensions '
ENDIF
RETURN lcScaleHd
*-- end of lfScalHd.