*:***************************************************************************
*: Program file  : ICSLCRT
*: Program desc. : Scales Report
*: For Report    : (ICSLCRTE.FRX,ICSLCRTN.FRX)
*: System        : Aria Advantage Series.(Aria5)
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
*:N000621 AHS 09/08/2009 Calling request builder fxp to collect data[T20090805.0001]
*:***************************************************************************

*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*ClientID = "GMA10"

*
*lcRequestID="ff5faeb4-6d4a-46d9-b70d-a625437023c8"
*lcXMLFileName = "\\ARIA-HASSAN\C\ARIA4XP\OUTPUT\XA0QFXOR.xml"

*Immediate
*lcRequestID="5218e24d-960b-474e-9267-d311b633e589"
*lcXMLFileName = "\\ARIA-HASSAN\C\Aria4XP\OUTPUT\XA0Q6U86.xml"

*T20100512.0026 Hassan 2010 05 23 [END]
*DEBUG
*SUSPEND

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
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

lcStTime = TIME()
DIMENSION laPrePaks[1,1]
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir + 'SCALE' ,'SCALE','SH',.F.,.F.)
  oAriaEnvironment.remotetableaccess.OPENTABLE(oAriaEnvironment.DataDir + 'SCALEHD' ,'EXTSCALE','SH',.F.,.F.)
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  gfOpenTable(oAriaApplication.DataDir + 'SCALE' ,'SCALE','SH',.F.,.F.)
  gfOpenTable(oAriaApplication.DataDir + 'SCALEHD' ,'EXTSCALE','SH',.F.,.F.)
ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

*-- N.R.F. Expression
lcNrfExp = [!(EMPTY(cNRFCode1) AND EMPTY(cNRFCode2) AND EMPTY(cNRFCode3) AND EMPTY(cNRFCode4) AND EMPTY(cNRFCode5) AND EMPTY(cNRFCode6) AND EMPTY(cNRFCode7) AND EMPTY(cNRFCode8))]



*!*	*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
*!*	IF TYPE('lcXMLFileName') = 'C'
*!*	*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
*!*	  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
*!*	*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
*!*	ELSE
*!*	  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
*!*	ENDIF


SELECT SCALE
*-- Create temp. file have scales and prepacks. [Begin]
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  lcTempScl  = oAriaEnvironment.CURSORS.GetCursorTempName()
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  lcTempScl = gfTempName()
ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

DIMENSION laStruArr[1,4]
=AFIELDS(laStruArr)
CREATE CURSOR (lcTempScl) FROM ARRAY laStruArr
SELECT (lcTempScl)
ZAP
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  INDEX ON TYPE + SCALE + PREPAK TAG (lcTempScl) OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
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
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    loProgress.DESCRIPTION = "Collecting Data..."
    *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ENDIF
  *N000622 AHS 09/08/2009 Calling request builder fxp to collect data[End]
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempScl) FROM MEMVAR
ENDSCAN

*-- Loop Scale file to add filtered lines. [End..
SELECT (lcTempScl)

IF RECCOUNT(lcTempScl) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  * =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

IF llRpPrepak OR llRpNRF
  SCAN FOR TYPE = 'S'

    lcCurrRec = 'S' + SCALE
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
      *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
      IF llRpPrepak AND oAriaEnvironment.remotetableaccess.SeekRecord('P'+SCALE,'SCALE')
        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
      ELSE
        IF llRpPrepak AND gfSeek('P'+SCALE,'SCALE')
        ENDIF
        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        SELECT SCALE
        SCAN FOR TYPE+SCALE+PREPAK = 'P' + &lcTempScl..SCALE
          IF pPTot > 0
            SCATTER MEMVAR MEMO
            m.Cnt = &lcTempScl..CNT
            INSERT INTO (lcTempScl) FROM MEMVAR
          ENDIF
        ENDSCAN

        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
          *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
          =oAriaEnvironment.remotetableaccess.SeekRecord(lcCurrRec,lcTempScl)
          *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        ELSE
          gfSeek(lcCurrRec,lcTempScl)
        ENDIF
        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
      ENDIF

      SELECT(lcTempScl)

      IF llRpNRF AND EVALUATE(lcNrfExp)
        SCATTER MEMVAR MEMO
        m.Type      = 'N'
        m.PREPAK    = 'Z'
        INSERT INTO (lcTempScl) FROM MEMVAR
        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        IF TYPE('lcXMLFileName') = 'C'
          *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
          =oAriaEnvironment.remotetableaccess.SeekRecord(lcCurrRec,lcTempScl)
          *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
        ELSE
          gfSeek(lcCurrRec,lcTempScl)
        ENDIF
        *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

      ENDIF
      *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
    ENDIF
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

  ENDSCAN
ENDIF

*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
*ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]


IF llExtSize
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    INDEX ON LEFT(SCALE,lnExtWidth) + PREPAK TAG (lcTempScl) OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    INDEX ON LEFT(SCALE,lnExtWidth) + PREPAK TAG (lcTempScl) OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
  ENDIF
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  *-- Make relation with header file.
  SELECT SCALE
  SET RELATION TO
  SELECT (lcTempScl)
  SET RELATION TO LEFT(SCALE,lnExtWidth) INTO SCALEHD
ELSE
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
    INDEX ON SCALE + PREPAK TAG (lcTempScl) OF (oAriaEnvironment.WorkDir+lcTempScl+'.CDX')
    *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
  ELSE
    INDEX ON SCALE + PREPAK TAG (lcTempScl) OF (oAriaApplication.WorkDir+lcTempScl+'.CDX')
  ENDIF
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
ENDIF

STORE .T. TO llPackFlag,llNrfFlag
llHavePp   = .F.
lcPrepak   = ''
lcCurrType = ' '



*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]
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

    *loProgress.Percent = 1.0
    *loProgress.DESCRIPTION = "Printing Report..."
    *T20100512.0026 Hassan 2010 05 23 [BEGIN]
    *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    *T20100512.0026 Hassan 2010 05 23 [END]
  ENDIF
  *N000621 AHS 09/08/2009 Calling request builder fxp to collect data[Start]
ELSE
  DO gfdispre WITH EVALUATE('lcRpForm')

ENDIF
*N000621 AHS 09/08/2009 Calling request builder fxp to collect data[End]

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
IF SCALEHD.nnoofdim = 1
  lcScaleHd = 'One Dimension '
ELSE
  lcScaleHd = IIF(SCALEHD.nnoofdim=2,'Two ','Three ') + 'Dimensions '
ENDIF
RETURN lcScaleHd
*-- end of lfScalHd.

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
lcCurrType = TYPE
RETURN ''
*-- end of lfCurrType.

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
STORE (TYPE = 'S') TO llPackFlag,llNrfFlag
RETURN ''
*-- end of lfPackFlag.
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
