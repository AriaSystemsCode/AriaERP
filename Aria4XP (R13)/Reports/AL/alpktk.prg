*!*****************************************************************************************
*! Name      : ALPKTK.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/16/2005
*! Purpose   : Picking Ticket Form A IN ARIA4
*! Entry no. : N000482 - Picking Ticket Form A. 
*!*****************************************************************************************
*! Modification
*: B607739,1 TNA 01/02/2006 Error while printing after preview
*: B131802,1 MMT 26/04/2006 make screen print button print report layout
*: B607878,1 MMT 12/14/2006 Bug in ship to if DC is used. T20061208.0056
*: B607979,1 AYM 02/19/2007 T20070209.0010 add the sort by style to the standard ,
*: B607979,1                       suppress this option if a custom form is used
*! N000592,1 HBG 02/28/2007 T20061201.0014 Print store ship to address or DC address according to flag in SO
*! B608044,1 MMT 04/16/2007 fix bug of wrong notes printed with multiple piktkt order
*! C200803,1 TMI 06/26/2007 Add three fields to the temp file lcTmpOrdL that will be used in the Alpktknu custom form
*! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [T20091103.0017]
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[T20110516.0005]
*! E302950,2 MMT 08/16/2011 Fix bug of wrong# of store in Audit report[T20110516.0005]
*! E302950,3 MMT 08/16/2011 Fix bug of Error while sorting by Pack Group without Summary Report[T20110516.0005]
*! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[T20120112.0012]
*! B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012]
*! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[T20120112.0012]
*! B609954,1 MMT 06/05/2012 Pick Ship Via from Customer file, alt ship via, if break weight greater than zero, and pick wight is greater than it [T20120524.0002]
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [T20121107.0003]
*! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media]
*! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029]
*! B610536,1 MMT 09/30/2013 Picking ticket sort by DC# does not display lines[T20130923.0025]
*! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13]
*! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004]
*! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003]
*! B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038]
*! E612626,1 MMT 09/11/2022 Modify Picking ticket form GP to have sort by option[T20220907.0002]
*! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002]
*! B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001]
*!*****************************************************************************************

*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID

*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
IF TYPE('lcRequestID') = 'C' .AND. 'TEMP.TXT' $ UPPER(lcRequestID)
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][Start]
  *STRTOFILE("2.0.0.1", lcRequestID, .F.)
  STRTOFILE("3.0.0.0", lcRequestID, .F.)
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][End]
  RETURN
ENDIF
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientID

  PRIVATE loAgent
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  PRIVATE loProgress
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *loProgress = goRemoteCall.GetRemoteObject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  loProgress.Percent = 0

  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)

  LOCAL loEnvironment
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  loEnvironment.ClientID = ClientID

  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()

  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientID)
  lcClientRoot = loEnvironment.Aria40SharedPath

  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientID, lcCurrentProcedure, loEnvironment

  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  PUBLIC gcAct_Appl
  gcAct_Appl  = 'AL'
  oAriaEnvironment.REPORT.gcAct_Appl = 'AL'
  oAriaEnvironment.ActiveModuleID = 'AL'
  oAriaEnvironment.RequestID = lcRequestID

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF

  oAriaEnvironment.REPORT.cCROrientation = 'P'

  llOgFltCh = .T.
  lfWrepWhen()
ELSE
  loogscroll.cCROrientation = 'P'
ENDIF
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]

*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
#INCLUDE R:\aria4xp\reports\al\alpktk.h
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
PRIVATE lnCount,lnOldpLen,lcPrintSt
llPrinter=.F.
lcCurChr = ALLTRIM(SET('CURRENCY' , 1))        && Variable to hold the currency symbol
llCurLeft = (SET('CURRENCY') = 'LEFT')         && Flag to know if the currency symbol is left
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcSeason = ''          && Variable to hold the Season Description
lcSpcInst = ''         && Variable to hold the Special Instructions Description
lcTerms = ''           && Variable to hold the Terms Description
lcDivLName = ''        && Variable to hold the Division long name
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcPrgName  = lcFormName
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
*llIsAparel = lfIsApparl(@lcPrgName)
IF TYPE('lcXMLFileName') = 'C'
  lcFormName = oAriaEnvironment.REPORT.GETFORM('ALPKTK')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
ELSE
  llIsAparel = lfIsApparl(@lcPrgName)
ENDIF
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp

*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
IF TYPE('lcXMLFileName') = 'C'
  IF TYPE('loObjects') <> 'O'
    loObjects = CREATEOBJECT("RemoteTable","OBJECTS","OBJECTID","OBJECTS",SET("DATASESSION"))
  ENDIF
  IF TYPE('loObjLink') <> 'O'
    loObjLink = CREATEOBJECT("RemoteTable","OBJLINK","OBJLNKTY",lcTempObjLink,SET("DATASESSION"))
  ENDIF
  IF TYPE('loPikLine') <> 'O'
    loPikLine = CREATEOBJECT("RemoteTable","PikLine","PikLine",lcTempPikLine,SET("DATASESSION"))
  ENDIF
  IF TYPE('loPikTkt') <> 'O'
    loPikTkt = CREATEOBJECT("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
  ENDIF
  IF TYPE('loCustomer') <> 'O'
    loCustomer = CREATEOBJECT("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))
  ENDIF
  IF TYPE('loNotePad') <> 'O'
    loNotePad = CREATEOBJECT("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION"))
  ENDIF
  IF TYPE('loStyle') <> 'O'
    loStyle = CREATEOBJECT("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION"))
  ENDIF
  IF TYPE('loScale') <> 'O'
    loScale = CREATEOBJECT("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION"))
  ENDIF
  IF TYPE('loOrdHdr') <> 'O'
    loOrdHdr   = CREATEOBJECT("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION"))
  ENDIF
  IF TYPE('loOrdLine') <> 'O'
    loOrdLine  = CREATEOBJECT("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION"))
  ENDIF
  IF TYPE('loWareHous') <> 'O'
    loWareHous  = CREATEOBJECT("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION"))
  ENDIF
  IF TYPE('loSpck_Lin') <> 'O'
    loSpck_Lin = CREATEOBJECT("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION"))
  ENDIF
ENDIF
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]


*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
IF TYPE('lcXMLFileName') = 'C'
  =gfOpenFile('PIKTKT','PIKTKT')
  =gfOpenFile('ORDHDR','ORDHDR')
  =gfOpenFile('ORDLINE','ORDLINE')
  =gfOpenFile('STYLE','STYLE')
ENDIF
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]

llLogo = IIF(loObjLink.SEEK('*' + 'LOGO') .AND. loObjects.SEEK(&lcTempObjLink..cObject_ID), .T. , .F.)        && Flag to know if we are to print the Company Logo

llAlpktk = .T.            && llAlpktk it will be a commen variable.

*--THE COLOR LENGTH
STORE 0 TO lnClrLnM1 , lnClrPosM1
DECLARE laItemSeg[1]
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('lcXMLFileName') = 'C'
  ItemMask = CREATEOBJECT("GetItemMask")
  =ItemMask.DO(@laItemSeg)
ELSE
  =gfItemMask(@laItemSeg)
ENDIF
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]

FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnM1  = LEN(laItemSeg[lnCount,3])
    lnClrPosM1 = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

lcTime = TIME()          && Variable to hold the Time

lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
LOCAL lnResult
lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult = 1
  SELECT &lcCompInfo
  lcCompFax       = cCom_Fax
  lcCompName      = cCom_Name
  lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
  lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
  laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
  laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
  lcCompFax = TRANSFORM(lcCompFax , '@R '+lcPhonPict)  && Fax No. Pic
  lcXphone  = TRANSFORM(lcCompPhon , '@R '+lcPhonPict) && variable hold the phone format to use it
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF

IF llOgFltCh
  IF USED(lcOrdhdr)
    USE IN (lcOrdhdr)
    SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE
    =lfMakeIndex(lcOrdhdr)
  ENDIF
  IF USED(lcNotePad)
    USE IN (lcNotePad)
    SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE
    =lfMakeIndex(lcNotePad)
  ENDIF
  IF USED(lcOrdLnTmp)
    USE IN (lcOrdLnTmp)
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE
    =lfMakeIndex(lcOrdLnTmp)
  ENDIF
  IF USED(lcPikTemp)
    USE IN (lcPikTemp)
    SELECT * FROM &lcTempPikTkt WHERE .F. INTO CURSOR &lcPikTemp READWRITE
    =lfMakeIndex(lcPikTemp)
  ENDIF
  *--MMT,Fixing problem of print status filter default value

  *B131802,1 MMT 26/04/2006 make screen print button print report layout[Start]
  IF TYPE('ladata') <> 'U' AND !EMPTY(ladata)
    lcRpPrtSt = 'A'
  ENDIF
  *B131802,1 MMT 26/04/2006 make screen print button print report layout[End]

  IF lcRpPrtSt = 'N'
    lcPrintSt = '" "'
  ELSE
    IF lcRpPrtSt = 'A'
      lcPrintSt = '""'
    ELSE
      lcPrintSt  ="'P'"
    ENDIF
  ENDIF

  lcRpExp = STRTRAN(lcRpExp ,'lcRpPrtSt','lcPrintSt')

  *--MMT,Fixing problem of print status filter default value
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
  IF FILE(oAriaApplication.DataDir+'ARCUST.MEM')
    RESTORE FROM (oAriaApplication.DataDir+'ARCUST.MEM') ADDITIVE
  ENDIF
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
  *--
  *--THE STYLE LENGTH
  STORE 0 TO lnLenthM1
  lnLenthM1 = LEN(gfItemMask('PM'))
  *--End Section to get the Style/Color Lengths --*

  =lfGTmpOrdL()

  SELECT (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH
  SET RELATION TO ORDER + PikTkt INTO &lcTmpOrdH

  SET RELATION TO PikTkt INTO &lcPiktktTemp ADDITIVE
  SET RELATION TO 'O' + ORDER INTO &lcOrdhdr ADDITIVE

  *--IF We are to Print Order Lines Note Pad
  IF llRpOrdLNt
    SET RELATION TO 'O' + ORDER + STR(LINENO,6) INTO &lcOrdLnTmp ADDITIVE
  ENDIF    && End of IF

  SET RELATION TO STYLE INTO &lcStyleFile ADDITIVE
  SET RELATION TO 'S' + SCALE INTO &lcScaleFile ADDITIVE
  SELECT(lcPiktktTemp)
  SET RELATION TO cWareCode INTO &lcWareHous
  SET RELATION TO IIF(EMPTY(STORE) , 'M' + Account ,;
    'S' + Account + STORE) INTO &lcCustomer ADDITIVE


ENDIF
SELECT (lcTmpOrdL)
LOCATE

*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
*loogScroll.cCROrientation = 'P'
*=lfOptProg()
IF TYPE('lcXMLFileName') = 'C'
  lcFormName = oAriaEnvironment.REPORT.GETFORM('ALPKTK')
  lcPrgName  = lcFormName

  IF !EMPTY(oAriaEnvironment.REPORT.lcOptProg)
    lcPrgName = oAriaEnvironment.REPORT.lcOptProg
  ENDIF
  IF oAriaEnvironment.multiinst AND  FILE(oAriaEnvironment.clientreporthome+lcPrgName+'.FXP')
    =lfOptProg(oAriaEnvironment.clientreporthome+lcPrgName)
  ELSE
    IF FILE(oAriaEnvironment.ReportHome+lcPrgName+'.FXP')
      =lfOptProg(oAriaEnvironment.ReportHome+lcPrgName)
    ENDIF
  ENDIF
ELSE
  =lfOptProg()
ENDIF

*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]

*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
  SELECT (lcTmpOrdL)
  SET ORDER TO 'PackSort'
  LOCATE
ENDIF
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]

*! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
  SELECT (lcTmpOrdL)
  SET ORDER TO 'DCSort'
  LOCATE
ENDIF
*! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][END]


*B607979,1 AYM  add the sort by style to the standard , suppress this option if a custom form is used [START]
*E612626,1 MMT 09/11/2022 Modify Picking ticket form GP to have sort by option[T20220907.0002][Start]
*llSuppress = LEN(lcFormName)=8 .OR. RIGHT(lcFormName,1)$'BE'
llSuppress = (LEN(lcFormName)=8 .OR. RIGHT(lcFormName,1)$'BE') AND RIGHT(lcFormName,2) != 'GP' 
*E612626,1 MMT 09/11/2022 Modify Picking ticket form GP to have sort by option[T20220907.0002][End]
IF !llSuppress .AND. lcRpStBy= 'S'
  lcKey = STRTRAN(UPPER(KEY()),'STR(LINENO,6)','STYLE+STR(LINENO,6)')
  INDEX ON &lcKey TAG ('Z'+SUBSTR(lcTmpOrdL,2))
ENDIF
*B607979,1 AYM  add the sort by style to the standard , suppress this option if a custom form is used [END]
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
IF IIF(lcFormName == "ALPKTKA" AND lcRpPkBy = 'G' AND llRpPrSm AND llRpPrSo,.F.,.T.)
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  IF llIsAparel
    lnOldpLen = _PLENGTH
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT

      SET DEVICE TO SCREEN

    ENDIF
    _PLENGTH  = lnOldpLen
  ELSE
    IF llAlpktk
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *DO gfDispRe WITH EVAL('lcFormName')
      IF TYPE('lcXMLFileName') = 'C'
        oAriaEnvironment.REPORT.OGLastForm = lcFormName&& lcRpForm
        loProgress.Percent = 0.9
        loProgress.DESCRIPTION = "Printing Report..."
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        PRIVATE loProxy
        *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
        *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
        loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
        *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

        oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
        loProgress.Percent = 1.0
        loProgress.DESCRIPTION = "Printing Report..."
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      ELSE
        DO gfDispRe WITH EVAL('lcFormName')
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      *    oAriaApplication.gcDevice ="SCREEN"
    ENDIF
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
ENDIF
IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G' AND llRpPrSm
  SELECT (lcTmpOrdL)
  SELECT MAX(ORDER) AS 'MAXM',MIN(ORDER) AS 'MINM' ,SUM(TotPik) AS 'Units' FROM (lcTmpOrdL) INTO CURSOR 'OrdCnter'
  llMutliOrd = IIF(ALLTRIM(OrdCnter.MAXM)=ALLTRIM(OrdCnter.MINM) ,.F.,.T.)
  lcTickCnt = 0
  SELECT DISTINCT PikTkt FROM (lcTmpOrdL) INTO CURSOR 'PikCnter'
  lcTickCnt = RECCOUNT('PikCnter')
  SELECT (lcTmpOrdL)
  LOCATE
  lcStoreCnt = 0

  lcOldTmp = lcOGTmpForm
  lcOGTmpForm = lcFormName
  lcOldPlatF  = lcOgPlatForm
  lcFormName= 'ALPKAUDS'
  =gfCrtFrm(lcFormName,"",llOGRefForm)
  =lfRepPltFr(lcOGTmpForm)
  SELECT(lcTmpPSm)
  LOCATE

  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *DO gfDispRe WITH EVAL('lcFormName')
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.REPORT.OGLastForm = lcFormName&& lcRpForm
    loProgress.Percent = 0.9
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    PRIVATE loProxy
    *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
    *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    *! E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    loProgress.Percent = 1.0
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  ELSE
    DO gfDispRe WITH EVAL('lcFormName')
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  lcOGTmpForm  = lcOldTmp
  lcOgPlatForm = lcOldPlatF
ENDIF
*! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
*-- for paper size
*!*B607739	IF oAriaApplication.gcDevice = 'PRINTER'
*! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [Start]
*IF llPrinter
*! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
*IF loOgScroll.ll2Printer
LOCAL ll2Printer
IF TYPE('lcXMLFileName') = 'C'
  ll2Printer = .T.
ELSE
  ll2Printer = loogscroll.ll2Printer
ENDIF
IF ll2Printer
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  *! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [End]
  SELECT (lcTmpOrdL)
  *!*B607739	  SET ORDER TO TAG (lcTmpOrdU)
  SET ORDER TO TAG (lcTmpOrdL)
  SCAN
    IF loPikTkt.SEEK(EVAL(lcTmpOrdL+'.PIKTKT'))
      loPikTkt.REPLACE("PrtFlag WITH 'P'")
    ENDIF
  ENDSCAN
  loPikTkt.TABLEUPDATE()
  *! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [Start]
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *loOgScroll.ll2Printer = .F.
  IF TYPE('lcXMLFileName') <> 'C'
    loogscroll.ll2Printer = .F.
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  *! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [End]

ENDIF    && End of IF


*!*  SELECT PIKTKT
*!*  SET RELATION TO



*IF The Temp. Order Line file is Opened in one of the work areas
*!*  IF USED(lcTmpOrdL)
*!*    SELECT (lcTmpOrdL)
*!*    SET RELATION TO
*!*    USE
*!*  ENDIF    && End of IF

*!*  *IF The Temp. Order Header file is Opened in one of the work areas
*!*  IF USED(lcTmpOrdH)
*!*    SELECT (lcTmpOrdH)
*!*    SET RELATION TO
*!*    USE
*!*  ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/16/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*
FUNCTION lfWrepWhen
  *PRIVATE lnWareHElm , lnVrFltElm , llRefresh

  *--Objects file
  IF TYPE('loObjects') <> 'O'
    loObjects = CREATEOBJECT("RemoteTable","OBJECTS","OBJECTID","OBJECTS",SET("DATASESSION"))
  ENDIF
  *--ObjLink file
  IF TYPE('loObjLink') <> 'O'
    loObjLink = CREATEOBJECT("RemoteTable","OBJLINK","OBJLNKTY",lcTempObjLink,SET("DATASESSION"))
  ENDIF

  *--PikLine file
  IF TYPE('loPikLine') <> 'O'
    loPikLine= CREATEOBJECT("RemoteTable","PikLine","PikLine",lcTempPikLine,SET("DATASESSION"))
  ENDIF

  *--Piktkt file
  IF TYPE('loPikTkt') <> 'O'
    loPikTkt   = CREATEOBJECT("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
    SELECT * FROM &lcTempPikTkt WHERE .F. INTO CURSOR &lcPikTemp READWRITE
    =lfMakeIndex(lcPikTemp)
  ENDIF

  *--Customer file
  IF TYPE('loCustomer') <> 'O'
    loCustomer = CREATEOBJECT("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE
    =lfMakeIndex(lcCustomer)
  ENDIF

  *--Notepad file
  IF TYPE('loNotePad') <> 'O'
    loNotePad  = CREATEOBJECT("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION"))
    SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE
    =lfMakeIndex(lcNotePad)
  ENDIF

  *--Style file
  IF TYPE('loStyle') <> 'O'
    loStyle    = CREATEOBJECT("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION"))
    SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE
    =lfMakeIndex(lcStyleFile)
  ENDIF

  *--Scale file
  IF TYPE('loScale') <> 'O'
    loScale    = CREATEOBJECT("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION"))
    SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE
    =lfMakeIndex(lcScaleFile)
  ENDIF

  *--Ordhdr file
  IF TYPE('loOrdHdr') <> 'O'
    loOrdHdr   = CREATEOBJECT("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION"))
    SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE
    =lfMakeIndex(lcOrdhdr)
  ENDIF

  *--Ordline file
  IF TYPE('loOrdLine') <> 'O'
    loOrdLine  = CREATEOBJECT("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION"))
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE
    =lfMakeIndex(lcOrdLnTmp)
  ENDIF

  *--WareHous  file
  IF TYPE('loWareHous') <> 'O'
    loWareHous  = CREATEOBJECT("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION"))
    SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE
    =lfMakeIndex(lcWareHous)
  ENDIF

  *--Spck_lin file
  IF TYPE('loSpck_Lin') <> 'O'
    loSpck_Lin = CREATEOBJECT("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION"))
  ENDIF

	*! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
	IF FILE(oAriaApplication.DataDir+'ARCUST.MEM')
	  RESTORE FROM (oAriaApplication.DataDir+'ARCUST.MEM') ADDITIVE
	ENDIF
	*! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]

  DIMENSION laTmpOrdLS[1,4]

  SELECT(lcTempOrdLine)
  DIMENSION laTmpOrdLS[1,18]
  lnFldLen = AFIELDS(laTmpOrdLS)
  *C200803,1 TMI [Start]
  *DIMENSION laTmpOrdLS[lnFldLen + 3 , 18]
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
  *!*	IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
  *!*	  DIMENSION laTmpOrdLS[lnFldLen + 8 , 18]
  IF lcFormName == "ALPKTKA"  AND lcRpPkBy $ 'DG'
    DO CASE
      CASE lcRpPkBy = 'G'
        DIMENSION laTmpOrdLS[lnFldLen + 8 , 18]
      CASE lcRpPkBy = 'D'
        DIMENSION laTmpOrdLS[lnFldLen + 6 , 18]
    ENDCASE
    *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]
  ELSE
    *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
    DIMENSION laTmpOrdLS[lnFldLen + 5 , 18]
    *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  *C200803,1 TMI [End  ]

  laTmpOrdLS[lnFldLen + 1 , 1] = 'cGrupDetal'
  laTmpOrdLS[lnFldLen + 1 , 2] = 'C'
  laTmpOrdLS[lnFldLen + 1 , 3] = 1
  laTmpOrdLS[lnFldLen + 1 , 4] = 0

  laTmpOrdLS[lnFldLen + 2 , 1] = 'CurrSmbl'
  laTmpOrdLS[lnFldLen + 2 , 2] = 'C'
  laTmpOrdLS[lnFldLen + 2 , 3] = 3
  laTmpOrdLS[lnFldLen + 2 , 4] = 0

  laTmpOrdLS[lnFldLen + 3 , 1] = 'CurrCode'
  laTmpOrdLS[lnFldLen + 3 , 2] = 'C'
  laTmpOrdLS[lnFldLen + 3 , 3] = 3
  laTmpOrdLS[lnFldLen + 3 , 4] = 0

  *C200803,1 TMI [Start]
  laTmpOrdLS[lnFldLen + 4 , 1] = 'LLASTPAGE'
  laTmpOrdLS[lnFldLen + 4 , 2] = 'L'
  laTmpOrdLS[lnFldLen + 4 , 3] = 1
  laTmpOrdLS[lnFldLen + 4 , 4] = 0

  laTmpOrdLS[lnFldLen + 5 , 1] = 'PAGENO'
  laTmpOrdLS[lnFldLen + 5 , 2] = 'N'
  laTmpOrdLS[lnFldLen + 5 , 3] = 4
  laTmpOrdLS[lnFldLen + 5 , 4] = 0
  *C200803,1 TMI [End  ]

  *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
    laTmpOrdLS[lnFldLen + 6 , 1] = 'DC'
    laTmpOrdLS[lnFldLen + 6 , 2] = 'C'
    laTmpOrdLS[lnFldLen + 6 , 3] = 8
    laTmpOrdLS[lnFldLen + 6 , 4] = 0
  ENDIF
  *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
    laTmpOrdLS[lnFldLen + 6 , 1] = 'PACKID'
    laTmpOrdLS[lnFldLen + 6 , 2] = 'C'
    laTmpOrdLS[lnFldLen + 6 , 3] = 10
    laTmpOrdLS[lnFldLen + 6 , 4] = 0

    laTmpOrdLS[lnFldLen + 7 , 1] = 'PKNUM'
    laTmpOrdLS[lnFldLen + 7 , 2] = 'N'
    laTmpOrdLS[lnFldLen + 7 , 3] = 5
    laTmpOrdLS[lnFldLen + 7 , 4] = 0

    laTmpOrdLS[lnFldLen + 8 , 1] = 'DCNUM'
    laTmpOrdLS[lnFldLen + 8 , 2] = 'C'
    laTmpOrdLS[lnFldLen + 8 , 3] = 8
    laTmpOrdLS[lnFldLen + 8 , 4] = 0
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  *C200803,1 TMI [Start]
  *FOR lnLoop = 1 TO 3
  FOR lnLoop = 1 TO ALEN(laTmpOrdLS,1)-lnFldLen
    *C200803,1 TMI [End  ]
    STORE ' ' TO  laTmpOrdLS[lnFldLen +lnLoop,7],laTmpOrdLS[lnFldLen +lnLoop,8],;
      laTmpOrdLS[lnFldLen +lnLoop,9],laTmpOrdLS[lnFldLen +lnLoop,10],;
      laTmpOrdLS[lnFldLen +lnLoop,11],laTmpOrdLS[lnFldLen +lnLoop,12],;
      laTmpOrdLS[lnFldLen +lnLoop,13],laTmpOrdLS[lnFldLen +lnLoop,14],;
      laTmpOrdLS[lnFldLen +lnLoop,15],laTmpOrdLS[lnFldLen +lnLoop,16]
    STORE 0 TO    laTmpOrdLS[lnFldLen +lnLoop,17] ,laTmpOrdLS[lnFldLen +lnLoop,18]

  ENDFOR

  *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[Start]
  lnOrdSize = ALEN(laTmpOrdLS,1)
  *! B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
  *DIMENSION laTmpOrdLS[lnOrdSize +7,18]
  *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
  *DIMENSION laTmpOrdLS[lnOrdSize +8,18]
  DIMENSION laTmpOrdLS[lnOrdSize +16,18]
  *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]
  *! B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][End]
  laTmpOrdLS[lnOrdSize + 1, 1] = 'STNAME'
  laTmpOrdLS[lnOrdSize + 1, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 1, 3] = 30
  laTmpOrdLS[lnOrdSize + 1, 4] = 0

  laTmpOrdLS[lnOrdSize + 2, 1] = 'CADDRESS1'
  laTmpOrdLS[lnOrdSize + 2, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 2, 3] = 30
  laTmpOrdLS[lnOrdSize + 2, 4] = 0

  laTmpOrdLS[lnOrdSize + 3, 1] = 'CADDRESS2'
  laTmpOrdLS[lnOrdSize + 3, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 3, 3] = 30
  laTmpOrdLS[lnOrdSize + 3, 4] = 0

  laTmpOrdLS[lnOrdSize + 4, 1] = 'CADDRESS3'
  laTmpOrdLS[lnOrdSize + 4, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 4, 3] = 30
  laTmpOrdLS[lnOrdSize + 4, 4] = 0

  laTmpOrdLS[lnOrdSize + 5, 1] = 'CADDRESS4'
  laTmpOrdLS[lnOrdSize + 5, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 5, 3] = 30
  laTmpOrdLS[lnOrdSize + 5, 4] = 0

  laTmpOrdLS[lnOrdSize + 6, 1] = 'CADDRESS5'
  laTmpOrdLS[lnOrdSize + 6, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 6, 3] = 30
  laTmpOrdLS[lnOrdSize + 6, 4] = 0

  laTmpOrdLS[lnOrdSize + 7, 1] = 'CADDRESS6'
  laTmpOrdLS[lnOrdSize + 7, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 7, 3] = 30
  laTmpOrdLS[lnOrdSize + 7, 4] = 0
  *! B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
  *FOR lnLoop = 1 TO 7
  laTmpOrdLS[lnOrdSize + 8, 1] = 'ShipVia'
  laTmpOrdLS[lnOrdSize + 8, 2] = 'C'
  laTmpOrdLS[lnOrdSize + 8, 3] = 30
  laTmpOrdLS[lnOrdSize + 8, 4] = 0
  *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
  *FOR lnLoop = 1 TO 8
  lnCntSizes = 1
  FOR lnCntSz = 9 TO 16
    laTmpOrdLS[lnOrdSize + lnCntSz, 1] = 'Size'+STR(lnCntSizes,1)
    laTmpOrdLS[lnOrdSize + lnCntSz, 2] = 'C'
    laTmpOrdLS[lnOrdSize + lnCntSz, 3] = 5
    laTmpOrdLS[lnOrdSize + lnCntSz, 4] = 0
    lnCntSizes = lnCntSizes + 1
  ENDFOR
  FOR lnLoop = 1 TO 16
    *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]
    *! B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][END]
    STORE ' ' TO  laTmpOrdLS[lnOrdSize +lnLoop,7],laTmpOrdLS[lnOrdSize +lnLoop,8],;
      laTmpOrdLS[lnOrdSize +lnLoop,9],laTmpOrdLS[lnOrdSize +lnLoop,10],;
      laTmpOrdLS[lnOrdSize +lnLoop,11],laTmpOrdLS[lnOrdSize +lnLoop,12],;
      laTmpOrdLS[lnOrdSize +lnLoop,13],laTmpOrdLS[lnOrdSize +lnLoop,14],;
      laTmpOrdLS[lnOrdSize +lnLoop,15],laTmpOrdLS[lnOrdSize +lnLoop,16]
    STORE 0 TO    laTmpOrdLS[lnOrdSize +lnLoop,17] ,laTmpOrdLS[lnOrdSize +lnLoop,18]
  ENDFOR
  *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[END]


  *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][Start]
  DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS,1)+1,18]
  laTmpOrdLS[ALEN(laTmpOrdLS,1), 1] = 'Notes'
  laTmpOrdLS[ALEN(laTmpOrdLS,1), 2] = 'C'
  laTmpOrdLS[ALEN(laTmpOrdLS,1), 3] = 254
  laTmpOrdLS[ALEN(laTmpOrdLS,1), 4] = 0

  STORE ' ' TO  laTmpOrdLS[ALEN(laTmpOrdLS,1),7],laTmpOrdLS[ALEN(laTmpOrdLS,1),8],;
    laTmpOrdLS[ALEN(laTmpOrdLS,1),9],laTmpOrdLS[ALEN(laTmpOrdLS,1),10],;
    laTmpOrdLS[ALEN(laTmpOrdLS,1),11],laTmpOrdLS[ALEN(laTmpOrdLS,1),12],;
    laTmpOrdLS[ALEN(laTmpOrdLS,1),13],laTmpOrdLS[ALEN(laTmpOrdLS,1),14],;
    laTmpOrdLS[ALEN(laTmpOrdLS,1),15],laTmpOrdLS[ALEN(laTmpOrdLS,1),16]
  STORE 0 TO    laTmpOrdLS[ALEN(laTmpOrdLS,1),17] ,laTmpOrdLS[ALEN(laTmpOrdLS,1),18]
  *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][End]

  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][Start]
  lnOrdNsize = ALEN(laTmpOrdLS,1)
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][end]

  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
  *DIMENSION laTmpOrdLS[lnOrdNsize+7,18]
  DIMENSION laTmpOrdLS[lnOrdNsize+17,18]
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][end]
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
  laTmpOrdLS[lnOrdNsize+ 1, 1] = 'ORDER_START'
  laTmpOrdLS[lnOrdNsize+ 1, 2] = 'D'
  laTmpOrdLS[lnOrdNsize+ 1, 3] = 8
  laTmpOrdLS[lnOrdNsize+ 1, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 2, 1] = 'ORDER_COMPLETE'
  laTmpOrdLS[lnOrdNsize+ 2, 2] = 'D'
  laTmpOrdLS[lnOrdNsize+ 2, 3] = 8
  laTmpOrdLS[lnOrdNsize+ 2, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 3, 1] = 'APPROVAL'
  laTmpOrdLS[lnOrdNsize+ 3, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 3, 3] = 10
  laTmpOrdLS[lnOrdNsize+ 3, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 4, 1] = 'DEPT'
  laTmpOrdLS[lnOrdNsize+ 4, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 4, 3] = 5
  laTmpOrdLS[lnOrdNsize+ 4, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 5, 1] = 'NOTE1'
  laTmpOrdLS[lnOrdNsize+ 5, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 5, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 5, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 6, 1] = 'NOTE2'
  laTmpOrdLS[lnOrdNsize+ 6, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 6, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 6, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 7, 1] = 'PHONE'
  laTmpOrdLS[lnOrdNsize+ 7, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 7, 3] = 16
  laTmpOrdLS[lnOrdNsize+ 7, 4] = 0
  
  SET STEP ON
  *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][Start] 
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
  laTmpOrdLS[lnOrdNsize+ 8, 1] = IIF(TYPE('lcField1')='U' Or EMPTY(lcField1) OR 'USER FIELD' $ UPPER(lcField1),'USERFIELD1',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField1),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 8, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 8, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 8, 4] = 0

  laTmpOrdLS[lnOrdNsize+ 9, 1] = IIF(TYPE('lcField2')='U' Or EMPTY(lcField2) OR 'USER FIELD' $ UPPER(lcField2),'USERFIELD2',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField2),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 9, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 9, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 9, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 10, 1] = IIF(TYPE('lcField3')='U' Or EMPTY(lcField3) OR 'USER FIELD' $ UPPER(lcField3),'USERFIELD3',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField3),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 10, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 10, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 10, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 11, 1] = IIF(TYPE('lcField4')='U' Or EMPTY(lcField4) OR 'USER FIELD' $ UPPER(lcField4),'USERFIELD4',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField4),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 11, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 11, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 11, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 12, 1] = IIF(TYPE('lcField5')='U' Or EMPTY(lcField5) OR 'USER FIELD' $ UPPER(lcField5),'USERFIELD5',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField5),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 12, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 12, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 12, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 13, 1] = IIF(TYPE('lcField6')='U' Or EMPTY(lcField6) OR 'USER FIELD' $ UPPER(lcField6),'USERFIELD6',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField6),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 13, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 13, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 13, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 14, 1] = IIF(TYPE('lcField7')='U' Or EMPTY(lcField7) OR 'USER FIELD' $ UPPER(lcField7),'USERFIELD7',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField7),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 14, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 14, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 14, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 15, 1] = IIF(TYPE('lcField8')='U' Or EMPTY(lcField8) OR 'USER FIELD' $ UPPER(lcField8),'USERFIELD8',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField8),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 15, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 15, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 15, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 16, 1] = IIF(TYPE('lcField9')='U' Or EMPTY(lcField9) OR 'USER FIELD' $ UPPER(lcField9),'USERFIELD9',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField9),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 16, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 16, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 16, 4] = 0
  
  laTmpOrdLS[lnOrdNsize+ 17, 1] = IIF(TYPE('lcField10')='U' Or EMPTY(lcField10) OR 'USER FIELD' $ UPPER(lcField10),'USERFLD10',STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField10),'-','_'),'&',''),'.',''),' ','_'),'#',''))
  laTmpOrdLS[lnOrdNsize+ 17, 2] = 'C'
  laTmpOrdLS[lnOrdNsize+ 17, 3] = 30
  laTmpOrdLS[lnOrdNsize+ 17, 4] = 0
  *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][End]
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][END]
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
  *FOR lnLoop = 1 TO 7
  FOR lnLoop = 1 TO 17
  *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
    STORE ' ' TO  laTmpOrdLS[lnOrdNsize+lnLoop,7],laTmpOrdLS[lnOrdNsize+lnLoop,8],;
      laTmpOrdLS[lnOrdNsize+lnLoop,9],laTmpOrdLS[lnOrdNsize+lnLoop,10],;
      laTmpOrdLS[lnOrdNsize+lnLoop,11],laTmpOrdLS[lnOrdNsize+lnLoop,12],;
      laTmpOrdLS[lnOrdNsize+lnLoop,13],laTmpOrdLS[lnOrdNsize+lnLoop,14],;
      laTmpOrdLS[lnOrdNsize+lnLoop,15],laTmpOrdLS[lnOrdNsize+lnLoop,16]
    STORE 0 TO    laTmpOrdLS[lnOrdNsize+lnLoop,17] ,laTmpOrdLS[lnOrdNsize+lnLoop,18]
  ENDFOR
  *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][End]

  =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'cCurrCode','SH')

  DIMENSION laTmpOrdHS[7,18]

  =ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
  =ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 19)

  laTmpOrdHS[3 , 1] = 'NWEIGHT'
  laTmpOrdHS[3 , 2] = 'N'
  laTmpOrdHS[3 , 3] = 10
  laTmpOrdHS[3 , 4] = 2

  laTmpOrdHS[4 , 1] = 'STATUS'
  laTmpOrdHS[4 , 2] = 'C'
  laTmpOrdHS[4 , 3] = 1
  laTmpOrdHS[4 , 4] = 0

  laTmpOrdHS[5 , 1] = 'CUSTPO'
  laTmpOrdHS[5 , 2] = 'C'
  laTmpOrdHS[5 , 3] = 15
  laTmpOrdHS[5 , 4] = 0

  laTmpOrdHS[6 , 1] = 'Account'
  laTmpOrdHS[6 , 2] = 'C'
  laTmpOrdHS[6 , 3] = 5
  laTmpOrdHS[6 , 4] = 0

  laTmpOrdHS[7 , 1] = 'Store'
  laTmpOrdHS[7 , 2] = 'C'
  laTmpOrdHS[7 , 3] = 8
  laTmpOrdHS[7 , 4] = 0

  FOR  N = 1 TO 7
    STORE ' ' TO  laTmpOrdHS[ n ,7] , laTmpOrdHS[ n ,8],;
      laTmpOrdHS[ n ,9] ,laTmpOrdHS[ n ,10],;
      laTmpOrdHS[ n ,11],laTmpOrdHS[ n ,12],;
      laTmpOrdHS[ n ,13],laTmpOrdHS[ n ,14],;
      laTmpOrdHS[ n ,15],laTmpOrdHS[ n ,16]
    STORE 0 TO    laTmpOrdHS[ n ,17],laTmpOrdHS[ n ,18]

  ENDFOR


  IF (TYPE("lcSpWhen") = "C") AND lcSpWhen = "Y"
    =lfUsrVldFn("lfSpcWhen",lcFormName)
  ENDIF
  =lfBrowStat()
  *-- end of lfwRepWhen.

  *!*************************************************************
  *! Name      : lfvOptMsg
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to get Optional Message from the User
  *!             [Validation function for the Push button Optional Message]
  *!*************************************************************
  *! Called from : Option Grid    [Optional Message option]
  *!*************************************************************
  *
FUNCTION lfvOptMsg
  PRIVATE laOptMsg
  DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

  laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
  laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
  laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
  laOptMsg[1,2] = 75                && Line length
  =gfOptMsg('laOptMsg')
  *-- end of lfvOptMsg.

  *!*************************************************************
  *! Name      : lfwOldVal
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : When function to get the Old value
  *!*************************************************************
  *! Called from : Some of the Option Grid fields
  *!*************************************************************
  *
FUNCTION lfwOldVal
  laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
  *-- end of lfwOldVal.

  *!*************************************************************
  *! Name      : lfGTmpOrdL
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to colect the neede data in a temp. Order lines file
  *!*************************************************************
  *! Called from : This program.
  *!*************************************************************
  *
FUNCTION lfGTmpOrdL
  PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
    lcCDXName, lcFullPath


  IF USED(lcTmpOrdL)
    USE IN (lcTmpOrdL)
  ENDIF    && End of IF



  *-- Check for option of Print By too in case of MBI
  IF TYPE('lcRpSortBy') = 'U' AND TYPE('lcRpPrtBy') = 'U'
    *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
    *=gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpOrdL,.T.)
    =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal +"+IIF(llRpPrnPck,"PACK_ID+",'')+" STR(LineNo , 6)",lcTmpOrdL,.T.)
    *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
  ELSE
    *-- IF Print By Picking Ticket Number, the sorting will be as is it.
    IF lcRpPrtBy = 'P'
      IF lcRpSortBy = 'S'
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
        *=gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal + STYLE",lcTmpOrdL,.T.)
        =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal +"+IIF(llRpPrnPck,"PACK_ID+",'')+"STYLE",lcTmpOrdL,.T.)
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
      ELSE
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
        *=gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpOrdL,.T.)
        =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"PikTkt + Order + cGrupDetal +"+IIF(llRpPrnPck,"PACK_ID+",'')+" STR(LineNo , 6)",lcTmpOrdL,.T.)
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
      ENDIF
    ELSE
      IF lcRpSortBy = 'S'
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
        *=gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"Account + PikTkt + Order + cGrupDetal + STYLE",lcTmpOrdL,.T.)
        =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"Account + PikTkt + Order + cGrupDetal +"+IIF(llRpPrnPck,"PACK_ID+",'')+"STYLE",lcTmpOrdL,.T.)
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
      ELSE
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
        * =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"Account + PikTkt + Order + cGrupDetal + STR(LineNo , 6)",lcTmpOrdL,.T.)
        =gfCrtTmp(lcTmpOrdL,@laTmpOrdLS,"Account + PikTkt + Order + cGrupDetal +"+IIF(llRpPrnPck,"PACK_ID+",'')+"STR(LineNo , 6)",lcTmpOrdL,.T.)
        *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
      ENDIF
    ENDIF
  ENDIF
  SELECT(lcTmpOrdL)

  *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
    lcKeyExp = KEY()
    lcKeyExp ='Order + DC + STORE +'+lcKeyExp
    INDEX ON &lcKeyExp. TAG 'DCSort' ADDITIVE
    SET ORDER TO (lcTmpOrdL)
  ENDIF
  *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]


  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
    lcKeyExp = KEY()
    lcKeyExp ='STR(PKNUM,5)+'+lcKeyExp
    INDEX ON &lcKeyExp. TAG 'PackSort' ADDITIVE
    INDEX ON PikTkt + cGrupDetal+STYLE+STR(PIK1,6)+STR(PIK2,6)+STR(PIK3,6)+STR(PIK4,6)+STR(PIK5,6)+STR(PIK6,6)+STR(PIK7,6)+STR(PIK8,6) TAG 'PackIndex' ADDITIVE
    SET ORDER TO (lcTmpOrdL)
    IF llRpPrSm
      DIMENSION laPackSum[18,4]
      laPackSum[1,1] = 'DCNUM'
      laPackSum[1,2] = 'C'
      laPackSum[1,3] = 8
      laPackSum[1,4] = 0
      laPackSum[2,1] = 'PKNUM'
      laPackSum[2,2] = 'N'
      laPackSum[2,3] = 5
      laPackSum[2,4] = 0
      laPackSum[3,1] = 'STYLE'
      laPackSum[3,2] = 'C'
      laPackSum[3,3] = 19
      laPackSum[3,4] = 0
      laPackSum[4,1] = 'TOTPIK'
      laPackSum[4,2] = 'N'
      laPackSum[4,3] = 10
      laPackSum[4,4] = 0
      laPackSum[5,1] = 'StCnt'
      laPackSum[5,2] = 'N'
      laPackSum[5,3] = 6
      laPackSum[5,4] = 0
      laPackSum[6,1] = 'PIK1'
      laPackSum[6,2] = 'N'
      laPackSum[6,3] = 5
      laPackSum[6,4] = 0
      laPackSum[7,1] = 'PIK2'
      laPackSum[7,2] = 'N'
      laPackSum[7,3] = 5
      laPackSum[7,4] = 0
      laPackSum[8,1] = 'PIK3'
      laPackSum[8,2] = 'N'
      laPackSum[8,3] = 5
      laPackSum[8,4] = 0
      laPackSum[9,1] = 'PIK4'
      laPackSum[9,2] = 'N'
      laPackSum[9,3] = 5
      laPackSum[9,4] = 0
      laPackSum[10,1] = 'PIK5'
      laPackSum[10,2] = 'N'
      laPackSum[10,3] = 5
      laPackSum[10,4] = 0
      laPackSum[11,1] = 'PIK6'
      laPackSum[11,2] = 'N'
      laPackSum[11,3] = 5
      laPackSum[11,4] = 0
      laPackSum[12,1] = 'PIK7'
      laPackSum[12,2] = 'N'
      laPackSum[12,3] = 5
      laPackSum[12,4] = 0
      laPackSum[13,1] = 'PIK8'
      laPackSum[13,2] = 'N'
      laPackSum[13,3] = 5
      laPackSum[13,4] = 0
      laPackSum[14,1] = 'TotStCnt'
      laPackSum[14,2] = 'N'
      laPackSum[14,3] = 6
      laPackSum[14,4] = 0
      laPackSum[15,1] = 'PackTot'
      laPackSum[15,2] = 'N'
      laPackSum[15,3] = 8
      laPackSum[15,4] = 0
      laPackSum[16,1] = 'cType'
      laPackSum[16,2] = 'C'
      laPackSum[16,3] = 1
      laPackSum[16,4] = 0
      laPackSum[17,1] = 'ColorDesc'
      laPackSum[17,2] = 'C'
      laPackSum[17,3] = 30
      laPackSum[17,4] = 0
      laPackSum[18,1] = 'TOTBOOK'
      laPackSum[18,2] = 'N'
      laPackSum[18,3] = 10
      laPackSum[18,4] = 0
      =gfCrtTmp(lcTmpPSm,@laPackSum,"cType + DCNUM+ STR(PKNUM,5) + STYLE + STR(PIK1,5) + STR(PIK2,5)+ STR(PIK3,5)+ STR(PIK4,5)+ STR(PIK5,5)+ STR(PIK6,5)+ STR(PIK7,5)+ STR(PIK8,5)",lcTmpPSm,.T.)
    ENDIF
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  *!*B607739	IF oAriaApplication.gcDevice <> 'SCREEN'
  *!*	  INDEX ON PikTkt TAG (lcTmpOrdU) &&UNIQUE
  *!*	ENDIF

  *IF The Temp. Order Header file is Opened in one of the work areas
  IF USED(lcTmpOrdH)
    USE IN (lcTmpOrdH)
  ENDIF    && End of IF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
    =gfCrtTmp(lcTmpOrdH,@laTmpOrdHS,"Order+PikTkt+STORE",lcTmpOrdH,.T.)
  ELSE
    *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
    =gfCrtTmp(lcTmpOrdH,@laTmpOrdHS,"Order + PikTkt",lcTmpOrdH,.T.)
    *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  SELECT (lcTmpOrdH)
  SET ORDER TO

  *- collecting data
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
  IF TYPE('lcXMLFileName') = 'C'
    lnPosPikTkt = ASCAN(laOgFXFlt,"PIKTKT.PIKTKT")
  ELSE
    lnPosPikTkt = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.PIKTKT")
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  IF lnPosPikTkt > 0
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikTkt = ASUBSCRIPT(laOgFXFlt,lnPosPikTkt,1)
    ELSE
      lnPosPikTkt = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikTkt,1)
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    lcPikTkteSel =IIF(!EMPTY(laOgFXFlt[lnPosPikTkt,6]),laOgFXFlt[lnPosPikTkt,6],'')
    IF !EMPTY(lcPikTkteSel) AND USED(lcPikTkteSel)
      SELECT(lcPikTkteSel)
      LOCATE
      IF !EOF()
        SCAN
          loPikTkt.SEEK(&lcPikTkteSel..PikTkt)
          SELECT(lcTempPikTkt)
          SCATTER MEMO MEMVAR
          INSERT INTO (lcPikTemp) FROM MEMVAR
        ENDSCAN
        SELECT(lcPikTemp)
        LOCATE
        lcSelFileds = lcPikTemp + ".*"
        lcSeleCond  = " "
        lcSeleCond = lcSeleCond + lcPikTemp + ".prtflag =" + lcPrintSt
        lcSeleFiles = lcPikTemp
        *--Status Check
        lcStatusValue = ""
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
        IF TYPE('lcXMLFileName') = 'C'
          lnPosPikStatus  = ASCAN(laOgFXFlt,"PIKTKT.STATUS")
        ELSE
          lnPosPikStatus  = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.STATUS")
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF lnPosPikStatus  > 0
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
          *lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
          IF TYPE('lcXMLFileName') = 'C'
            lnPosPikStatus  = ASUBSCRIPT(laOgFXFlt,lnPosPikStatus ,1)
          ELSE
            lnPosPikStatus  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikStatus ,1)
          ENDIF
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
          lcStatusValue = IIF(!EMPTY(laOgFXFlt[lnPosPikStatus,6]),laOgFXFlt[lnPosPikStatus,6],'C|X|H|O|P')
          lcStatusValue = "INLIST("+lcPikTemp+".Status,'" + STRTRAN(lcStatusValue,"|","','") +"')"
        ENDIF
        lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue)," AND " + lcStatusValue,"")
        *--Date Check
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
        IF TYPE('lcXMLFileName') = 'C'
          lnPosDate = ASCAN(laOgFXFlt,"PIKTKT.DATE")
        ELSE
          lnPosDate = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.DATE")
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF lnPosDate > 0
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
          *lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
          IF TYPE('lcXMLFileName') = 'C'
            lnPosDate = ASUBSCRIPT(laOgFXFlt,lnPosDate,1)
          ELSE
            lnPosDate = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosDate,1)
          ENDIF
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
          SDATE = SUBSTR(laOgFXFlt[lnPosDate,6],1,10)
          EDATE = SUBSTR(laOgFXFlt[lnPosDate,6],12,20)
          IF !EMPTY(EDATE) AND !EMPTY(SDATE)
            lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(&lcPikTemp..DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
          ENDIF
        ENDIF
        *--Check user selected accounts
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
        IF TYPE('lcXMLFileName') = 'C'
          lnPosPikAcc = ASCAN(laOgFXFlt,"PIKTKT.ACCOUNT")
        ELSE
          lnPosPikAcc = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.ACCOUNT")
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF lnPosPikAcc > 0
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
          *lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
          IF TYPE('lcXMLFileName') = 'C'
            lnPosPikAcc = ASUBSCRIPT(laOgFXFlt,lnPosPikAcc,1)
          ELSE
            lnPosPikAcc = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikAcc,1)
          ENDIF
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
          lcPikTktAccSel =IIF(!EMPTY(laOgFXFlt[lnPosPikAcc,6]),laOgFXFlt[lnPosPikAcc,6],'')
          IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
            SELECT(lcPikTktAccSel)
            LOCATE
            IF !EOF()
              lcSeleCond  = lcSeleCond  + IIF(!EMPTY(lcSeleCond)," AND ","")+lcPikTemp + ".ACCOUNT = " + lcPikTktAccSel + ".ACCOUNT"
              lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","") +  lcPikTktAccSel
            ENDIF
          ENDIF
        ENDIF
        *--Check user selected locations
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
        IF TYPE('lcXMLFileName') = 'C'
          lnPosPikWare = ASCAN(laOgFXFlt,"PIKTKT.CWARECODE")
        ELSE
          lnPosPikWare = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.CWARECODE")
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF lnPosPikWare > 0
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
          *lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
          IF TYPE('lcXMLFileName') = 'C'
            lnPosPikWare = ASUBSCRIPT(laOgFXFlt,lnPosPikWare,1)
          ELSE
            lnPosPikWare = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikWare,1)
          ENDIF
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
          lcPikTktWareSel =IIF(!EMPTY(laOgFXFlt[lnPosPikWare,6]),laOgFXFlt[lnPosPikWare,6],'')
          IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
            SELECT(lcPikTktWareSel)
            LOCATE
            IF !EOF()
              lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+lcPikTemp + ".CWARECODE = " + lcPikTktWareSel + ".CWARECODE"
              lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcPikTktWareSel
            ENDIF
          ENDIF
        ENDIF
        IF lcFormName = "ALPKTKGM"
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
          *lnOrdPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'ORDHDR.ORDER'),1)
          IF TYPE('lcXMLFileName') = 'C'
            lnOrdPos = ASUBSCRIPT(laOgFXFlt,ASCAN(laOgFXFlt,'ORDHDR.ORDER'),1)
          ELSE
            lnOrdPos = ASUBSCRIPT(loogscroll.laOgFXFlt,ASCAN(loogscroll.laOgFXFlt,'ORDHDR.ORDER'),1)
          ENDIF
          *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
          IF lnOrdPos > 0
            *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
            *lcTempOrder = loOgScroll.laOGFxFlt[lnOrdPos,6]
            IF TYPE('lcXMLFileName') = 'C'
              lcTempOrder = laOgFXFlt[lnOrdPos,6]
            ELSE
              lcTempOrder = loogscroll.laOgFXFlt[lnOrdPos,6]
            ENDIF
            *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
            IF !EMPTY(lcTempOrder) AND USED(lcTempOrder)
              SELECT(lcTempOrder)
              LOCATE
              IF !EOF()
                lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+lcPikTemp + ".ORDER = " + lcTempOrder + ".ORDER"
                lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcTempOrder
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF !EMPTY(lcSeleCond)
          SELECT &lcSelFileds FROM &lcSeleFiles  WHERE &lcSeleCond INTO CURSOR &lcPiktktTemp READWRITE
          =lfMakeIndex(lcPiktktTemp)
        ELSE
          SELECT &lcSelFileds FROM &lcSeleFiles INTO CURSOR &lcPiktktTemp READWRITE
          =lfMakeIndex(lcPiktktTemp)
        ENDIF

        SELECT(lcPiktktTemp)
        LOCATE
        IF !EOF()
          SCAN
            loOrdHdr.SEEK('O'+&lcPiktktTemp..ORDER)
            SELECT(lcTempOrdHdr)
            SCATTER MEMO MEMVAR
            INSERT INTO(lcOrdhdr) FROM MEMVAR
          ENDSCAN
        ENDIF
        *--check the order status on hold or not
        =lfGetData()
        RETURN
      ENDIF
    ENDIF
  ENDIF
  *-- In CASE of user does not select picking Ticket no. OR SELECT any other data
  IF loPikTkt.llNative && case files still fox
    lcSelFileds = "Piktkt.*"
    lcSeleCond  = "!EMPTY(Piktkt.PikTkt) AND Piktkt.PikTkt <> '******'  AND Piktkt.prtflag = " + lcPrintSt
    lcSeleFiles = "Piktkt"
    *--Status Check
    lcStatusValue = ""
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikStatus  = ASCAN(laOgFXFlt,"PIKTKT.STATUS")
    ELSE
      lnPosPikStatus  = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.STATUS")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikStatus  > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosPikStatus  = ASUBSCRIPT(laOgFXFlt,lnPosPikStatus ,1)
      ELSE
        lnPosPikStatus  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikStatus ,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcStatusValue = IIF(!EMPTY(laOgFXFlt[lnPosPikStatus,6]),laOgFXFlt[lnPosPikStatus,6],'C|X|H|O|P')
      lcStatusValue = "INLIST(Piktkt.Status,'" + STRTRAN(lcStatusValue,"|","','") +"')"
    ENDIF
    lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue), " AND " + lcStatusValue,"")
    *--Date Check
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosDate = ASCAN(laOgFXFlt,"PIKTKT.DATE")
    ELSE
      lnPosDate = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.DATE")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosDate > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosDate = ASUBSCRIPT(laOgFXFlt,lnPosDate,1)
      ELSE
        lnPosDate = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosDate,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      SDATE = SUBSTR(laOgFXFlt[lnPosDate,6],1,10)
      EDATE = SUBSTR(laOgFXFlt[lnPosDate,6],12,20)
      IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "BETWEEN(Piktkt.DATE,CTOD('"+SDATE+"'),CTOD('"+EDATE+"'))"
      ENDIF
    ENDIF
    *--Check user selected accounts
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikAcc = ASCAN(laOgFXFlt,"PIKTKT.ACCOUNT")
    ELSE
      lnPosPikAcc = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.ACCOUNT")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikAcc > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosPikAcc = ASUBSCRIPT(laOgFXFlt,lnPosPikAcc,1)
      ELSE
        lnPosPikAcc = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikAcc,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcPikTktAccSel =IIF(!EMPTY(laOgFXFlt[lnPosPikAcc,6]),laOgFXFlt[lnPosPikAcc,6],'')
      IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
        SELECT(lcPikTktAccSel)
        LOCATE
        IF !EOF()
          lcSeleCond  = lcSeleCond  + IIF(!EMPTY(lcSeleCond)," AND ","")+"Piktkt.ACCOUNT = " + lcPikTktAccSel + ".ACCOUNT"
          lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","") +  lcPikTktAccSel
        ENDIF
      ENDIF
    ENDIF
    IF lcFormName = "ALPKTKGM"
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnOrdPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'ORDHDR.ORDER'),1)
      IF TYPE('lcXMLFileName') = 'C'
        lnOrdPos = ASUBSCRIPT(laOgFXFlt,ASCAN(laOgFXFlt,'ORDHDR.ORDER'),1)
      ELSE
        lnOrdPos = ASUBSCRIPT(loogscroll.laOgFXFlt,ASCAN(loogscroll.laOgFXFlt,'ORDHDR.ORDER'),1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      IF lnOrdPos > 0
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lcTempOrder = loOgScroll.laOGFxFlt[lnOrdPos,6]
        IF TYPE('lcXMLFileName') = 'C'
          lcTempOrder = laOgFXFlt[lnOrdPos,6]
        ELSE
          lcTempOrder = loogscroll.laOgFXFlt[lnOrdPos,6]
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF !EMPTY(lcTempOrder) AND USED(lcTempOrder)
          SELECT(lcTempOrder)
          LOCATE
          IF !EOF()
            lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+"Piktkt.ORDER = " + lcTempOrder + ".ORDER"
            lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcTempOrder
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *--Check user selected locations
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikWare = ASCAN(laOgFXFlt,"PIKTKT.CWARECODE")
    ELSE
      lnPosPikWare = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.CWARECODE")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikWare > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosPikWare = ASUBSCRIPT(laOgFXFlt,lnPosPikWare,1)
      ELSE
        lnPosPikWare = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikWare,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcPikTktWareSel =IIF(!EMPTY(laOgFXFlt[lnPosPikWare,6]),laOgFXFlt[lnPosPikWare,6],'')
      IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
        SELECT(lcPikTktWareSel)
        LOCATE
        IF !EOF()
          lcSeleCond  = lcSeleCond +  IIF(!EMPTY(lcSeleCond)," AND ","")+"Piktkt.CWARECODE = " + lcPikTktWareSel + ".CWARECODE"
          lcSeleFiles = lcSeleFiles + IIF(!EMPTY(lcSeleFiles),",","")+lcPikTktWareSel
        ENDIF
      ENDIF
    ENDIF
    IF !EMPTY(lcSeleCond)
      SELECT &lcSelFileds FROM &lcSeleFiles  WHERE &lcSeleCond INTO CURSOR &lcPiktktTemp READWRITE
      =lfMakeIndex(lcPiktktTemp)
    ELSE
      SELECT &lcSelFileds FROM &lcSeleFiles INTO CURSOR &lcPiktktTemp READWRITE
      =lfMakeIndex(lcPiktktTemp)
    ENDIF
    SELECT(lcPiktktTemp)
    LOCATE
    IF !EOF()
      SCAN
        loOrdHdr.SEEK('O'+&lcPiktktTemp..ORDER)
        SELECT(lcTempOrdHdr)
        SCATTER MEMO MEMVAR
        INSERT INTO(lcOrdhdr) FROM MEMVAR
      ENDSCAN
    ENDIF
    =lfGetData()
    RETURN
  ELSE && files converted to SQL

    lcSelFileds = "Piktkt.*"
    lcSeleCond  = "!EMPTY(Piktkt.PikTkt) AND Piktkt.PikTkt <> '******'  AND Piktkt.prtflag = " + lcPrintSt
    lcSeleFiles = "Piktkt"
    *--Status Check
    lcStatusValue = ""
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikStatus  = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.STATUS")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikStatus  = ASCAN(laOgFXFlt,"PIKTKT.STATUS")
    ELSE
      lnPosPikStatus  = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.STATUS")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikStatus  > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikStatus  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikStatus ,1)
      IF TYPE('lcXMLFileName') = 'C'
      ELSE
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcStatusValue = IIF(!EMPTY(laOgFXFlt[lnPosPikStatus,6]),laOgFXFlt[lnPosPikStatus,6],'C|X|H|O|P')
      lcStatusValue = "Piktkt.Status IN('" + STRTRAN(lcStatusValue,"|","','") +"')"
    ENDIF
    lcSeleCond  = lcSeleCond + IIF(!EMPTY(lcStatusValue)," AND " + lcStatusValue,"")
    *--Date Check
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosDate = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.DATE")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosDate = ASCAN(laOgFXFlt,"PIKTKT.DATE")
    ELSE
      lnPosDate = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.DATE")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosDate > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosDate = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDate,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosDate = ASUBSCRIPT(laOgFXFlt,lnPosDate,1)
      ELSE
        lnPosDate = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosDate,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      SDATE = SUBSTR(laOgFXFlt[lnPosDate,6],1,10)
      EDATE = SUBSTR(laOgFXFlt[lnPosDate,6],12,20)
      IF !EMPTY(EDATE) AND !EMPTY(SDATE)
        lcSeleCond = lcSeleCond + IIF(EMPTY(lcSeleCond),""," AND ")+ "Piktkt.DATE BETWEEN "+CTOD(SDATE)+" AND " + CTOD(EDATE)
      ENDIF
    ENDIF
    *--Check user selected accounts
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikAcc = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.ACCOUNT")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikAcc = ASCAN(laOgFXFlt,"PIKTKT.ACCOUNT")
    ELSE
      lnPosPikAcc = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.ACCOUNT")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikAcc > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikAcc = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikAcc,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosPikAcc = ASUBSCRIPT(laOgFXFlt,lnPosPikAcc,1)
      ELSE
        lnPosPikAcc = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikAcc,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcPikTktAccSel =IIF(!EMPTY(laOgFXFlt[lnPosPikAcc,6]),laOgFXFlt[lnPosPikAcc,6],'')
      IF !EMPTY(lcPikTktAccSel) AND USED(lcPikTktAccSel)
        SELECT(lcPikTktAccSel)
        LOCATE
        IF !EOF()
          lcCurName = lcPikTktAccSel
          IF !EMPTY(lcCurName)
            SELECT &lcCurName
            IF (RECCOUNT() > 0)
              *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
              *lcSQLAcc = loOgScroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')*-*-
              IF TYPE('lcXMLFileName') = 'C'
                lcSQLAcc = loogscroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')
              ELSE
                lcSQLAcc = loogscroll.gfSQLTempName('','Account C(5)',lcCurName,'Account')
              ENDIF
              *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
              lcSeleFiles = lcSeleFiles + "," + lcSQLAcc
              lcSeleCond = IIF(EMPTY(lcSeleCond ),""," AND ") + "PikTkt.ACCOUNT = " + lcSQLAcc + ".ACCOUNT"
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *-- in case of custom report
    IF lcFormName = "ALPKTKGM"
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnOrdPos = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'ORDHDR.ORDER'),1)
      IF TYPE('lcXMLFileName') = 'C'
        lnOrdPos = ASUBSCRIPT(laOgFXFlt,ASCAN(laOgFXFlt,'ORDHDR.ORDER'),1)
      ELSE
        lnOrdPos = ASUBSCRIPT(loogscroll.laOgFXFlt,ASCAN(loogscroll.laOgFXFlt,'ORDHDR.ORDER'),1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      IF lnOrdPos > 0
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lcTempOrder = loOgScroll.laOGFxFlt[lnOrdPos,6]
        IF TYPE('lcXMLFileName') = 'C'
          lcTempOrder = laOgFXFlt[lnOrdPos,6]
        ELSE
          lcTempOrder = loogscroll.laOgFXFlt[lnOrdPos,6]
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF !EMPTY(lcTempOrder) AND USED(lcTempOrder)
          SELECT(lcTempOrder)
          LOCATE
          IF !EOF()
            lcCurName = lcTempOrder
            IF !EMPTY(lcCurName)
              SELECT &lcCurName
              IF (RECCOUNT() > 0)
                *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
                *lcSQLWare = loOgScroll.gfSQLTempName('','ORDER C(6)',lcCurName,'ORDER')*-*-
                IF TYPE('lcXMLFileName') = 'C'
                  lcSQLWare = loogscroll.gfSQLTempName('','ORDER C(6)',lcCurName,'ORDER')
                ELSE
                  lcSQLWare = loogscroll.gfSQLTempName('','ORDER C(6)',lcCurName,'ORDER')
                ENDIF
                *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
                lcSeleFiles = lcSeleFiles + "," + lcSQLWare
                lcSeleCond  =IIF(EMPTY(lcSeleCond),""," AND ") + "PikTkt.ORDER = " + lcSQLWare + ".ORDER"
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *--Check user selected locations
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnPosPikWare = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.CWARECODE")
    IF TYPE('lcXMLFileName') = 'C'
      lnPosPikWare = ASCAN(laOgFXFlt,"PIKTKT.CWARECODE")
    ELSE
      lnPosPikWare = ASCAN(loogscroll.laOgFXFlt,"PIKTKT.CWARECODE")
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    IF lnPosPikWare > 0
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosPikWare = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikWare,1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosPikWare = ASUBSCRIPT(laOgFXFlt,lnPosPikWare,1)
      ELSE
        lnPosPikWare = ASUBSCRIPT(loogscroll.laOgFXFlt,lnPosPikWare,1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      lcPikTktWareSel =IIF(!EMPTY(laOgFXFlt[lnPosPikWare,6]),laOgFXFlt[lnPosPikWare,6],'')
      IF !EMPTY(lcPikTktWareSel) AND USED(lcPikTktWareSel)
        SELECT(lcPikTktWareSel)
        LOCATE
        IF !EOF()
          lcCurName = lcPikTktWareSel
          IF !EMPTY(lcCurName)
            SELECT &lcCurName
            IF (RECCOUNT() > 0)
              *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
              *lcSQLWare = loOgScroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')*-*-
              IF TYPE('lcXMLFileName') = 'C'
                lcSQLWare = loogscroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
              ELSE
                lcSQLWare = loogscroll.gfSQLTempName('','Cwarecode C(10)',lcCurName,'Cwarecode')
              ENDIF
              *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
              lcSeleFiles = lcSeleFiles + "," + lcSQLWare
              lcSeleCond  =IIF(EMPTY(lcSeleCond),""," AND ") + "PikTkt.CWARECODE = " + lcSQLWare + ".CWARECODE"
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    =lfOpenSql(lcSelFileds ,lcSeleFiles  , lcPiktktTemp ,lcSeleCond )
    SELECT(lcPiktktTemp)
    LOCATE
    IF !EOF()
      SCAN
        loOrdHdr.SEEK('O'+&lcPiktktTemp..ORDER)
        SELECT(lcTempOrdHdr)
        SCATTER MEMO MEMVAR
        INSERT INTO(lcOrdhdr) FROM MEMVAR
      ENDSCAN
    ENDIF
    =lfGetData()
    RETURN
  ENDIF
  *-- end of lfGTmpOrdL.

  *!*************************************************************
  *! Name      : lfSolSpAdr
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to Get the Sold to Address & Ship to Address
  *!             & the Description of the Ship Via , Season ,
  *!             Special Instructions , Terms
  *!*************************************************************
  *! Called from : ALPKTKTA.FRX (Inside the FRX)
  *!*************************************************************
  *
FUNCTION lfSolSpAdr
  PRIVATE lcDistCntr

  llEndGroup = .F.
  =gfRltFld(EVALUATE(lcOrdhdr+'.cDivision') , @laDivLName , 'CDIVISION')

  SELECT(lcCustomer)
  SEEK IIF(EMPTY(&lcPiktktTemp..STORE),'M','S')+ &lcPiktktTemp..Account + &lcPiktktTemp..STORE
  *! B609954,1 MMT 06/05/2012 Pick Ship Via from Customer file, alt ship via, if break weight greater than zero, and pick wight is greater than it [T20120524.0002][Begin]
  *!*	lcShipVia = gfCodDes(IIF(&lcCUSTOMER..nBrkWeight <> 0 .AND.;
  *!*	                         &lcTmpOrdH..nWeight > &lcCUSTOMER..nBrkWeight ,;
  *!*	                         &lcCUSTOMER..cAltShpvia ,IIF(&lcORDHDR..ShipVia ='*',&lcCUSTOMER..ShipVia,&lcORDHDR..ShipVia)), 'SHIPVIA')

  lcShipVia = gfCodDes(IIF(&lcCustomer..nBrkWeight <> 0 .AND.;
    &lcTmpOrdH..nWeight > &lcCustomer..nBrkWeight  AND !EMPTY(&lcCustomer..cAltShpvia),;
    &lcCustomer..cAltShpvia ,IIF(&lcOrdhdr..ShipVia ='*',&lcCustomer..ShipVia,&lcOrdhdr..ShipVia)), 'SHIPVIA')
  *! B609954,1 MMT 06/05/2012 Pick Ship Via from Customer file, alt ship via, if break weight greater than zero, and pick wight is greater than it [T20120524.0002][End]

  lcSeason = gfCodDes(&lcOrdhdr..Season , 'SEASON')
  lcSpcInst = gfCodDes(&lcOrdhdr..SpcInst , 'SPCINST')
  lcTerms = gfCodDes(&lcOrdhdr..cTermCode , 'CTERMCODE')

  SELECT(lcCustomer)
  SEEK IIF(EMPTY(&lcPiktktTemp..STORE) , 'M' , 'S') + &lcPiktktTemp..Account + &lcPiktktTemp..STORE

  lcSolTName = BTName
  lcShpTName = IIF(&lcOrdhdr..Alt_ShpTo , &lcOrdhdr..STName , IIF(EMPTY(DBA) , STName , DBA))

  laSoldTo[1] = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')
  *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
  *laSoldTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(&lcCustomer..Phone1 , '@R '+lcPhonPict)
  laSoldTo[5] = ALLTRIM(gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2'))+ 'Phone# '+ TRANSFORM(&lcCustomer..Phone1 , '@R '+lcPhonPict)
  *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
  =lfAdrShift('laSoldTo')

  IF &lcOrdhdr..Alt_ShpTo

    SELECT (lcOrdhdr)
    lcShpTName = STName
    laShipTo[1] = cAddress1
    laShipTo[2] = cAddress2
    laShipTo[3] = cAddress3
    laShipTo[4] = cAddress4
    laShipTo[5] = cAddress5

  ELSE    && Else
    SELECT(lcCustomer)
    lcDistCntr = &lcCustomer..Dist_Ctr
    *--If there is a distribution center
    *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
    *IF !EMPTY(lcDistCntr)
    IF !EMPTY(lcDistCntr) AND !(&lcOrdhdr..lStrDirct)
      *N000592,1 HBG [End]
      SEEK 'S' + &lcPiktktTemp..Account + lcDistCntr
    ENDIF

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    laShipTo[1] = gfGetAdr(lcCustomer , '' , '' , '' , 1)
    laShipTo[2] = gfGetAdr(lcCustomer , '' , '' , '' , 2)
    laShipTo[3] = gfGetAdr(lcCustomer , '' , '' , '' , 3)
    laShipTo[4] = gfGetAdr(lcCustomer , '' , '' , '' , 4)
    *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
    *laShipTo[5] = ALLTRIM(gfGetAdr(lcCUSTOMER , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(&lcCustomer..Phone1 ,'@R '+lcPhonPict)
    laShipTo[5] = ALLTRIM(gfGetAdr(lcCustomer , '' , '' , '' , 5)) + 'Phone#' + TRANSFORM(&lcCustomer..Phone1 ,'@R '+lcPhonPict)
    *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]
  ENDIF    && End of IF

  =lfAdrShift('laShipTo')

  SELECT (lcTmpOrdL)
  RETURN ''
  *-- end of lfSolSpAdr.

  *!*************************************************************
  *! Name      : lfAdrShift
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to Shift the Address array if there is any
  *!             empty lines in the address
  *!*************************************************************
  *! Called from : This program , lfSolSpAdr()
  *!*************************************************************
  *
FUNCTION lfAdrShift

  PARAMETERS lcArrayNam

  *FOR Loop to loop the Address Array
  FOR lnCount = 1 TO 5

    *IF The current Array element is of type character and empty
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
        EMPTY(&lcArrayNam.[lnCount])

      =ADEL(&lcArrayNam , lnCount)
      lnCount = lnCount - 1
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop

  *FOR Loop to loop the Address Array
  FOR lnCount = 1 TO 5

    *IF The current Array element is not of type character
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
      &lcArrayNam.[lnCount] = ''
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
  *-- end of lfAdrShift.

  *!*************************************************************
  *! Name      : lfEndGroup
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to Update the End of Group flag and to update
  *!             the PrtFlag field in the PIKTKT file if the divice is not
  *!             Screen
  *!*************************************************************
  *! Called from : ALPKTKTA.FRX (Inside the FRX)
  *!*************************************************************
  *
FUNCTION lfEndGroup
  llEndGroup = .T.
  *--IF The Divice is not Screen
  *!*  IF oAriaApplication.gcDevice <> 'SCREEN'
  *!*    REPLACE PIKTKT.PrtFlag WITH 'P'
  *!*  ENDIF    && End of IF
  RETURN ''
  *-- end of lfEndGroup.

  *!*************************************************************
  *! Name      : lfPktktSet
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Function to set and release the relations needed
  *!             for the Pick ticket # field [For the In range]
  *!*************************************************************
  *! Called from : Picking ticket field [Option Grid]
  *!*************************************************************
  *!
FUNCTION lfPktktSet
  PARAMETERS lcParm
  IF lcParm = 'S'
    SELECT PikTkt
    SET RELATION TO 'O' + ORDER INTO ORDHDR
    SET RELATION TO IIF(EMPTY(STORE) , 'M' + Account ,;
      'S' + Account + STORE) INTO CUSTOMER ADDITIVE
  ELSE
    SELECT PikTkt
    SET RELATION TO
  ENDIF
  *-- end of lfPktktSet.

  *!*************************************************************
  *! Name      : lfBrowStat
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Calling from the when function to fill lcText.
  *!*************************************************************
  *! Called from : This program
  *!*************************************************************
  *!
FUNCTION lfBrowStat

  STORE SPACE(0) TO lcText1 , lcText
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
  IF TYPE('lcXMLFileName') = 'C'
    lnDataPos = ASCAN(laOgFXFlt,'PIKTKT.STATUS')
  ELSE
    lnDataPos = ASCAN(loogscroll.laOgFXFlt,'PIKTKT.STATUS')
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  IF lnDataPos > 0
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
    *lcText1 = ALLTRIM(loOGScroll.laOGFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
    IF TYPE('lcXMLFileName') = 'C'
      lnDataPos  = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
      lcText1 = ALLTRIM(laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
    ELSE
      lnDataPos  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnDataPos,1)
      lcText1 = ALLTRIM(loogscroll.laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  ENDIF
  IF EMPTY(lcText1)
    lcText = "FOR PIKTKT # '******' "
  ELSE
    lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
  ENDIF
  RETURN lcText
  *--End of lfBrowStat
  *!*****************************************************************************************
  *! Name      : RefreshOptMsg
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Refresh the optional message area
  *!*****************************************************************************************
  *!
FUNCTION RefreshOptMsg
  IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
    RETURN ""
  ENDIF

  RETURN ALLTRIM(lcRpMsg1) + ", " +;
    ALLTRIM(lcRpMsg2) + ", " +;
    ALLTRIM(lcRpMsg3)
ENDFUNC
*-- end of RefreshStatus.

*!*************************************************************
*! Name      : lfvstate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/16/2005
*! Purpose   : Called from PIKTKT.STATUS field to match the user selection.
*!*************************************************************
*! Called from : This program
*!*************************************************************
*!
FUNCTION lfvstate
  PARAMETERS lcParm

  DO CASE
    CASE lcParm = 'V'  && Set code

      STORE SPACE(0) TO lcText1 , lcText
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnDataPos = ASCAN(loOGScroll.laOGFxFlt,'PIKTKT.STATUS')
      IF TYPE('lcXMLFileName') = 'C'
        lnDataPos = ASCAN(laOgFXFlt,'PIKTKT.STATUS')
      ELSE
        lnDataPos = ASCAN(loogscroll.laOgFXFlt,'PIKTKT.STATUS')
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      IF lnDataPos > 0
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDataPos,1)
        *lcText1 = ALLTRIM(loOGScroll.laOGFxFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgfFxFlt[lnDataPos,6] + '"' )
        IF TYPE('lcXMLFileName') = 'C'
          lnDataPos  = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
          lcText1 = ALLTRIM(laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgfFxFlt[lnDataPos,6] + '"' )
        ELSE
          lnDataPos  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnDataPos,1)
          lcText1 = ALLTRIM(loogscroll.laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgfFxFlt[lnDataPos,6] + '"' )
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
      ENDIF
      IF EMPTY(lcText1)
        lcText = "FOR PIKTKT # '******' "
      ELSE
        lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
      ENDIF
  ENDCASE
  *lcText  = lcText1  + " .AND. PIKTKT # '******' "
  *--End of lfvstate.
  *!*************************************************************
  *! Name      : lfSrvPik
  *! Developer : Mariam Mazhar Tawfik [MMT]
  *! Date      : 02/16/2005
  *! Purpose   : Calling from the when function to fill lcText.
  *!*************************************************************
  *! Called from : This program
  *!*************************************************************
  *!
FUNCTION lfSrvPik
  PARAMETERS lcParm
  PRIVATE lcAlias,lcSeleStatus
  lnAlias = SELECT()
  SELECT PikTkt
  SET FILTER TO
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
  IF TYPE('lcXMLFileName') = 'C'
    lnDataPos = ASCAN(laOgFXFlt,'PIKTKT.STATUS')
  ELSE
    lnDataPos = ASCAN(loogscroll.laOgFXFlt,'PIKTKT.STATUS')
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  IF lnDataPos > 0
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
    IF TYPE('lcXMLFileName') = 'C'
      lnDataPos  = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
    ELSE
      lnDataPos  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnDataPos,1)
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    lcSeleStatus =IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
  ENDIF
  DO CASE
    CASE lcParm = 'S'  && Set code
      STORE SPACE(0) TO lcText1 , lcText
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnDataPos = ASCAN(loOGScroll.laOGFXFlt,'PIKTKT.STATUS')
      IF TYPE('lcXMLFileName') = 'C'
        lnDataPos = ASCAN(laOgFXFlt,'PIKTKT.STATUS')
      ELSE
        lnDataPos = ASCAN(loogscroll.laOgFXFlt,'PIKTKT.STATUS')
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      IF lnDataPos > 0
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lnDataPos  = ASUBSCRIPT(loOGScroll.laOGFXFlt,lnDataPos,1)
        *lcText1 = ALLTRIM(loOGScroll.laOGFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFxFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFxFlt[lnDataPos,6] + '"' )
        IF TYPE('lcXMLFileName') = 'C'
          lnDataPos  = ASUBSCRIPT(laOgFXFlt,lnDataPos,1)
          lcText1 = ALLTRIM(laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
        ELSE
          lnDataPos  = ASUBSCRIPT(loogscroll.laOgFXFlt,lnDataPos,1)
          lcText1 = ALLTRIM(loogscroll.laOgFXFlt[lnDataPos,1]) + " $ " + IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        lcSeleStatus =IIF(EMPTY(laOgFXFlt[lnDataPos,6]) , '"CXHOP"' , '"' + laOgFXFlt[lnDataPos,6] + '"' )
      ENDIF
      IF EMPTY(lcText1)
        lcText = " PIKTKT # '******' "
        *    lcText = "FOR PIKTKT # '******' "
      ELSE
        *    lcText  = "FOR " + lcText1  + " .AND. PIKTKT # '******' "
        lcText  =  lcText1  + " .AND. PIKTKT # '******' "
      ENDIF
      SELECT PikTkt
      SET FILTER TO &lcText
      *  RETURN lcText
    CASE lcParm = 'R'
      STORE SPACE(0) TO  lcTempPik
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      *lnPosition = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'PIKTKT.PIKTKT'),1)
      IF TYPE('lcXMLFileName') = 'C'
        lnPosition = ASUBSCRIPT(laOgFXFlt,ASCAN(laOgFXFlt,'PIKTKT.PIKTKT'),1)
      ELSE
        lnPosition = ASUBSCRIPT(loogscroll.laOgFXFlt,ASCAN(loogscroll.laOgFXFlt,'PIKTKT.PIKTKT'),1)
      ENDIF
      *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
      IF lnPosition > 0
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
        *lcTempPik = loOgScroll.laOGFxFlt[lnPosition,6]
        IF TYPE('lcXMLFileName') = 'C'
          lcTempPik = laOgFXFlt[lnPosition,6]
        ELSE
          lcTempPik = loogscroll.laOgFXFlt[lnPosition,6]
        ENDIF
        *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
        IF !EMPTY(lcTempPik) AND USED(lcTempPik)
          SELECT(lcTempPik)
          SCAN
            lcPikTk = &lcTempPik..PikTkt
            IF SEEK(&lcTempPik..PikTkt,'piktkt')
              IF !(PikTkt.STATUS $ lcSeleStatus)
                DELETE FROM &lcTempPik WHERE  &lcTempPik..PikTkt = lcPikTk
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
  ENDCASE
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfMakeIndex
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 02/16/2005
  *! Purpose   : function to make index on a temp. file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMakeIndex
  PARAMETERS lcTempName
  PRIVATE laIndex
  DIMENSION laIndex[1,2]

  lcCursor = lcTempName
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

  *!*************************************************************
  *! Name      : lfCrtindex
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 02/16/2005
  *! Purpose   : function to Set the index for the SQL files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfCrtindex

  LPARAMETERS lcTable
  DO CASE

    CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcPikTemp

      *--temp. Customer File
    CASE UPPER(lcTable) = lcCustomer
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
      laIndex[1,2] = lcCustomer

      *--temp. ordhdr file
    CASE UPPER(lcTable) =  lcOrdhdr
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CORDTYPE+ORDER'
      laIndex[1,2] = lcOrdhdr

      *--temp. ordline file
    CASE UPPER(lcTable) =  lcOrdLnTmp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'CORDTYPE+ORDER+STR(LINENO,6)'
      laIndex[1,2] = lcOrdLnTmp

      *--temp. pikline file
    CASE UPPER(lcTable) =  lcNotePad
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+key'
      laIndex[1,2] = lcNotePad

    CASE UPPER(lcTable) =  lcStyleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

    CASE UPPER(lcTable) =  lcScaleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+scale+prepak'
      laIndex[1,2] = lcScaleFile

    CASE UPPER(lcTable) =  lcWareHous
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cWareCode'
      laIndex[1,2] = lcWareHous
      *--lcPikTemp
    CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'ACCOUNT'
      laIndex[1,2] = lcPikTemp
      laIndex[1,1] = 'CWARECODE'
      laIndex[1,2] = lcPikTempInd

      *--lcPiktktTemp
    CASE UPPER(lcTable) =  lcPiktktTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'PikTkt'
      laIndex[1,2] = lcPiktktTemp

  ENDCASE
  *!*************************************************************
  *! Name      : lfGetData
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 02/16/2005
  *! Purpose   : function to Set the index for the SQL files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfGetData

  SELECT(lcPiktktTemp)
  SET ORDER TO
  SET RELATION TO 'O' + ORDER INTO &lcOrdhdr


  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  IF TYPE('lcXMLFileName') <> 'C'
    * Thabet Handle globalization issues [Start]
    *!*	  WAIT 'Selecting picking tickets...' WINDOW NOWAIT
    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTING_PICKING_TICKETS,oAriaApplication.GetHeaderText("LANG_SELECTING_PICKING_TICKETS",AHEADERFILE)) WINDOW NOWAIT
    * Thabet Handle globalization issues [End  ]
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  SELECT(lcPiktktTemp)
  LOCATE

  *B131802,1 MMT 26/04/2006 make screen print button print report layout[Start]
  IF TYPE('ladata') <> 'U' AND !EMPTY(ladata)
    IF &lcOrdhdr..STATUS = 'H'
      llRpInHOrd = .T.
    ELSE
      llRpInHOrd = .F.
    ENDIF
  ENDIF
  *B131802,1 MMT 26/04/2006 make screen print button print report layout[End]

  IF llRpInHOrd
    SCAN
      INSERT INTO (lcTmpOrdH) (ORDER , PikTkt , STATUS , Account,STORE) VALUES;
        (&lcPiktktTemp..ORDER ,&lcPiktktTemp..PikTkt ,&lcPiktktTemp..STATUS ,&lcPiktktTemp..Account,&lcPiktktTemp..STORE)
    ENDSCAN
  ELSE
    SCAN  FOR  &lcOrdhdr..STATUS <> 'H'
      INSERT INTO (lcTmpOrdH) (ORDER , PikTkt , STATUS , Account,STORE) VALUES;
        (&lcPiktktTemp..ORDER ,&lcPiktktTemp..PikTkt ,&lcPiktktTemp..STATUS ,&lcPiktktTemp..Account,&lcPiktktTemp..STORE)
    ENDSCAN
  ENDIF

  SELECT(lcPiktktTemp)
  SET ORDER TO TAG &lcPiktktTemp
  SET RELATION TO

  SELECT(lcTempOrdLine)
  SET RELATION TO PikTkt INTO &lcPiktktTemp


  SELECT (lcTmpOrdH)
  LOCATE
  SET ORDER TO TAG (lcTmpOrdH)
  SET RELATION TO 'O'+ORDER INTO &lcOrdhdr ADDITIVE
  lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
  lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

  SELECT (lcTmpOrdH)
  LOCATE

  SCAN FOR STATUS <> 'X'

    lnCurRec = lnCurRec + 1
    lnSavRec = RECNO()
    *IF There is one or more records for this Order in the ORDLINE file
    IF loOrdLine.SEEK('O' + &lcTmpOrdH..ORDER)
      *  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')
      SELECT (lcTmpOrdH)
      lcPikTkt = PikTkt
      m.cGrupDetal = 'D'
      lcOrder = ORDER
      SELECT(lcTempOrdLine)
      *-- Scan Loop to scan the ORDLINE file FOR The current Order and FOR the Option Grid condition
      SCAN REST;
          WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrder FOR  &lcTempOrdLine..PikTkt = lcPikTkt
        SELECT(lcTmpOrdH)
        SEEK &lcTempOrdLine..ORDER + &lcTempOrdLine..PikTkt
        =loStyle.SEEK(&lcTempOrdLine..STYLE)
        SELECT (lcTmpOrdH)
        REPLACE nWeight WITH nWeight + (&lcTempStyle..nStyWeight * &lcTempOrdLine..TotPik)

        IF(SEEK("O"+&lcTempOrdLine..ORDER,lcOrdhdr) AND &lcOrdhdr..Multipo)
          REPLACE Custpo WITH IIF(Custpo <> &lcTempOrdLine..Custpo,IIF(EMPTY(Custpo),&lcTempOrdLine..Custpo,;
            "Multi PO"),&lcTempOrdLine..Custpo)
        ELSE
          REPLACE Custpo WITH &lcOrdhdr..Custpo
        ENDIF

        SELECT(lcTempOrdLine)
        SCATTER MEMVAR MEMO

        =SEEK(&lcOrdhdr..cCurrCode,'SYCCURR')
        m.CurrCode = &lcOrdhdr..cCurrCode
        m.CurrSmbl = SYCCURR.ccurrsmbl
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
        IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
          m.DC = ''
        ENDIF
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]
        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][Start]
        m.Notes = IIF(loNotePad.SEEK('B' + m.Order) ,ALLTRIM(EVAL(lcTempNotePad+'.mNotes')),'')
        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][End]
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
        loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
        FOR lnX=1 TO 10
          lcX = ALLTRIM(STR(lnX))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][Start]
          *lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(ALLTRIM(lcField&lcX. ),' ','_'),'#',''))
          lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField&lcX.),'-','_'),'.',''),'&',''),' ','_'),'#',''))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][End]
          m.&lcField. = &lcTempCustomer..USR_DFND&lcX.
        ENDFOR 
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[Start]
        IF &lcOrdhdr..Alt_ShpTo
          m.STName    = &lcOrdhdr..STName
          m.cAddress1 = &lcOrdhdr..cAddress1
          m.cAddress2 = &lcOrdhdr..cAddress2
          m.cAddress3 = &lcOrdhdr..cAddress3
          m.cAddress4 = &lcOrdhdr..cAddress4
          m.cAddress5 = &lcOrdhdr..cAddress5
          m.cAddress6 = ''
        ELSE
          loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
          lcDistCenter = &lcTempCustomer..Dist_Ctr
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
          IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
            m.DC = lcDistCenter
          ENDIF
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

          IF !EMPTY(lcDistCenter) AND !(&lcOrdhdr..lStrDirct)
            loCustomer.SEEK('S' + m.Account + lcDistCenter,'Customer')
          ENDIF
          m.STName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
          m.cAddress1 = &lcTempCustomer..cAddress1
          m.cAddress2 = &lcTempCustomer..cAddress2
          m.cAddress3 = &lcTempCustomer..cAddress3
          m.cAddress4 = &lcTempCustomer..cAddress4
          m.cAddress5 = &lcTempCustomer..cAddress5
          m.cAddress6 = &lcTempCustomer..cAddress6
        ENDIF
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
        m.ShipVia = gfCodDes(&lcOrdhdr..ShipVia, 'SHIPVIA')
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
        m.ORDER_START= &lcOrdhdr..START
        m.ORDER_COMPLETE= &lcOrdhdr..COMPLETE
        m.APPROVAL= &lcOrdhdr..APPROVAL
        m.DEPT= &lcOrdhdr..DEPT
        m.NOTE1= &lcOrdhdr..NOTE1
        m.NOTE2= &lcOrdhdr..NOTE2
        m.PHONE= &lcOrdhdr..PHONE
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][End]
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
        =loScale.SEEK('S'+&lcTempStyle..SCALE)
        FOR lnCntSz = 1 TO 8
          lcCntSz = STR(lnCntSz,1)
          m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
        ENDFOR
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]

        INSERT INTO (lcTmpOrdL)  FROM MEMVAR
      ENDSCAN    && End of SCAN Loop

      SCATTER MEMVAR MEMO BLANK
      SELECT (lcTmpOrdH)
      GO lnSavRec
      m.Order = ORDER
      *! B610536,1 MMT 09/30/2013 Picking ticket sort by DC# does not display lines[T20130923.0025][Start]
      IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
        m.Store = STORE
      ENDIF
      *! B610536,1 MMT 09/30/2013 Picking ticket sort by DC# does not display lines[T20130923.0025][End]
      *-- IF We are to Print the Order Note Pad
      IF llRpOrdNot .AND. loNotePad.SEEK('B'+ORDER)
        m.cGrupDetal = 'H'
        lnCurRec = lnCurRec - 1
        *SCAN Loop to scan the Temp. Order Header file FOR the current Order
        *and FOR Status <> 'X' [Not Released]

        *B608044,1 MMT 04/16/2007 fix bug of wrong notes printed with multiple piktkt order [Start]
        *SCAN REST;
        WHILE Order = M.Order;
        FOR Status <> 'X' AND Status <> 'C'
        SCAN REST;
            WHILE ORDER = M.Order AND PikTkt = lcPikTkt;
            FOR STATUS <> 'X' AND STATUS <> 'C'
          *B608044,1 MMT 04/16/2007 fix bug of wrong notes printed with multiple piktkt order [End]
          m.Account = Account
          lnCurRec = lnCurRec + 1
          m.PikTkt = PikTkt

          =SEEK(&lcOrdhdr..cCurrCode,'SYCCURR')
          m.CurrCode = &lcOrdhdr..cCurrCode
          m.CurrSmbl = SYCCURR.ccurrsmbl
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
          IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
            m.DC = ''
          ENDIF
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]
          *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][Start]
          m.Notes = IIF(loNotePad.SEEK('B' + m.Order) ,ALLTRIM(EVAL(lcTempNotePad+'.mNotes')),'')
          *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][End]
          *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
          loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
          FOR lnX=1 TO 10
            lcX = ALLTRIM(STR(lnX))
            *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][Start]
            *lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(ALLTRIM(lcField&lcX. ),' ','_'),'#',''))
            lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField&lcX.),'-','_'),'.',''),'&',''),' ','_'),'#',''))
            *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][End]
            m.&lcField. = &lcTempCustomer..USR_DFND&lcX.
          ENDFOR 
          *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]

          *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[Start]
          IF &lcOrdhdr..Alt_ShpTo
            m.STName    = &lcOrdhdr..STName
            m.cAddress1 = &lcOrdhdr..cAddress1
            m.cAddress2 = &lcOrdhdr..cAddress2
            m.cAddress3 = &lcOrdhdr..cAddress3
            m.cAddress4 = &lcOrdhdr..cAddress4
            m.cAddress5 = &lcOrdhdr..cAddress5
            m.cAddress6 = ''
          ELSE
            loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
            lcDistCenter = &lcTempCustomer..Dist_Ctr
            *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
            IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
              m.DC = lcDistCenter
            ENDIF
            *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

            IF !EMPTY(lcDistCenter) AND !(&lcOrdhdr..lStrDirct)
              loCustomer.SEEK('S' + m.Account + lcDistCenter,'Customer')
            ENDIF
            m.STName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
            m.cAddress1 = &lcTempCustomer..cAddress1
            m.cAddress2 = &lcTempCustomer..cAddress2
            m.cAddress3 = &lcTempCustomer..cAddress3
            m.cAddress4 = &lcTempCustomer..cAddress4
            m.cAddress5 = &lcTempCustomer..cAddress5
            m.cAddress6 = &lcTempCustomer..cAddress6
          ENDIF
          *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[END]
          *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
          m.ShipVia = gfCodDes(&lcOrdhdr..ShipVia, 'SHIPVIA')
          *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
          m.ORDER_START= &lcOrdhdr..START
          m.ORDER_COMPLETE= &lcOrdhdr..COMPLETE
          m.APPROVAL= &lcOrdhdr..APPROVAL
          m.DEPT= &lcOrdhdr..DEPT
          m.NOTE1= &lcOrdhdr..NOTE1
          m.NOTE2= &lcOrdhdr..NOTE2
          m.PHONE= &lcOrdhdr..PHONE
          *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][END]
          *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][End]
          *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
          =loScale.SEEK('S'+&lcTempStyle..SCALE)
          FOR lnCntSz = 1 TO 8
            lcCntSz = STR(lnCntSz,1)
            m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
          ENDFOR
          *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]

          INSERT INTO (lcTmpOrdL)  FROM MEMVAR
        ENDSCAN    && End of SCAN Loop
      ELSE    && Else
        LOCATE REST FOR ORDER + PikTkt > M.Order
      ENDIF    && End of IF
      GO RECORD lnSavRec
      **      SKIP -1
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop

  SELECT (lcTmpOrdH)

  *SCAN Loop to scan the Temp. Order Header file FOR Status = 'X' [Released]
  SCAN FOR STATUS $ 'XC'

    lnCurRec = lnCurRec + 1

    *IF There is one or more records for this Order in the PIKLINE file
    IF loPikLine.SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..ORDER)
      m.cGrupDetal = 'D'
      SELECT(lcTempPikLine)
      SCAN REST;
          WHILE PikTkt + ORDER = &lcTmpOrdH..PikTkt + &lcTmpOrdH..ORDER

        SELECT (lcTmpOrdH)
        =loStyle.SEEK(&lcTempPikLine..STYLE)
        SELECT (lcTmpOrdH)
        REPLACE nWeight WITH nWeight + (&lcTempStyle..nStyWeight * &lcTempPikLine..TotPik)

        SELECT(lcTempPikLine)
        SCATTER MEMVAR MEMO
        =SEEK(&lcOrdhdr..cCurrCode,'SYCCURR')
        m.CurrCode = &lcOrdhdr..cCurrCode
        m.CurrSmbl = SYCCURR.ccurrsmbl
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
        IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
          m.DC = ''
        ENDIF
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]
        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][Start]
        m.Notes = IIF(loNotePad.SEEK('B' + m.Order) ,ALLTRIM(EVAL(lcTempNotePad+'.mNotes')),'')
        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][End]
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
        loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
        FOR lnX=1 TO 10
          lcX = ALLTRIM(STR(lnX))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][Start]
          lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField&lcX.),'-','_'),'.',''),'&',''),' ','_'),'#',''))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][End]
          m.&lcField. = &lcTempCustomer..USR_DFND&lcX.
        ENDFOR 
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[Start]
        IF &lcOrdhdr..Alt_ShpTo
          m.STName    = &lcOrdhdr..STName
          m.cAddress1 = &lcOrdhdr..cAddress1
          m.cAddress2 = &lcOrdhdr..cAddress2
          m.cAddress3 = &lcOrdhdr..cAddress3
          m.cAddress4 = &lcOrdhdr..cAddress4
          m.cAddress5 = &lcOrdhdr..cAddress5
          m.cAddress6 = ''
        ELSE
          loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
          lcDistCenter = &lcTempCustomer..Dist_Ctr
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
          IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
            m.DC = lcDistCenter
          ENDIF
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

          IF !EMPTY(lcDistCenter) AND !(&lcOrdhdr..lStrDirct)
            loCustomer.SEEK('S' + m.Account + lcDistCenter,'Customer')
          ENDIF
          m.STName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
          m.cAddress1 = &lcTempCustomer..cAddress1
          m.cAddress2 = &lcTempCustomer..cAddress2
          m.cAddress3 = &lcTempCustomer..cAddress3
          m.cAddress4 = &lcTempCustomer..cAddress4
          m.cAddress5 = &lcTempCustomer..cAddress5
          m.cAddress6 = &lcTempCustomer..cAddress6
        ENDIF
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
        m.ShipVia = gfCodDes(&lcOrdhdr..ShipVia, 'SHIPVIA')
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
        m.ORDER_START= &lcOrdhdr..START
        m.ORDER_COMPLETE= &lcOrdhdr..COMPLETE
        m.APPROVAL= &lcOrdhdr..APPROVAL
        m.DEPT= &lcOrdhdr..DEPT
        m.NOTE1= &lcOrdhdr..NOTE1
        m.NOTE2= &lcOrdhdr..NOTE2
        m.PHONE= &lcOrdhdr..PHONE
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][End]
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
        =loScale.SEEK('S'+&lcTempStyle..SCALE)
        FOR lnCntSz = 1 TO 8
          lcCntSz = STR(lnCntSz,1)
          m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
        ENDFOR
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]
        INSERT INTO (lcTmpOrdL)  FROM MEMVAR
      ENDSCAN    && End of SCAN Loop

      SCATTER MEMVAR MEMO BLANK
      SELECT (lcTmpOrdH)
      *! B610536,1 MMT 09/30/2013 Picking ticket sort by DC# does not display lines[T20130923.0025][Start]
      IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
        m.Store = STORE
      ENDIF
      *! B610536,1 MMT 09/30/2013 Picking ticket sort by DC# does not display lines[T20130923.0025][End]
      *IF We are to Print the Order Note Pad
      IF llRpOrdNot .AND. loNotePad.SEEK('B'+ORDER)
        m.cGrupDetal = 'H'
        m.Account = Account
        m.PikTkt = PikTkt
        m.Order = ORDER

        =SEEK(&lcOrdhdr..cCurrCode,'SYCCURR')
        m.CurrCode = &lcOrdhdr..cCurrCode
        m.CurrSmbl = SYCCURR.ccurrsmbl
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
        IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
          m.DC = ''
        ENDIF
        *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][Start]
        m.Notes = IIF(loNotePad.SEEK('B' + m.Order) ,ALLTRIM(EVAL(lcTempNotePad+'.mNotes')),'')
        *! E303574,1 MMT 04/23/2015 Add notes column to excel exported from picking ticket form[T20150410.0004][End]
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][Start]
        loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
        FOR lnX=1 TO 10
          lcX = ALLTRIM(STR(lnX))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][Start]
          *lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(ALLTRIM(lcField&lcX. ),' ','_'),'#',''))
          lcField = IIF(TYPE('lcField&lcX.')='U' Or EMPTY(lcField&lcX.),'USR_DFND'+lcX ,STRTRAN(STRTRAN(STRTRAN(STRTRAN(STRTRAN(ALLTRIM(lcField&lcX.),'-','_'),'.',''),'&',''),' ','_'),'#',''))
          *B612704,1 MMT 02/11/2024 Fix the error happens if the customer UDF has special chars[T-ERP-20240206.0001][End]
          m.&lcField. = &lcTempCustomer..USR_DFND&lcX.
        ENDFOR 
        *! E612702,1 MMT 11/19/2023 Add The customer user fields to the fields list that are exported from the Picking ticket form to Excel[T-ERP-20231102.0002][End]
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[Start]
        IF &lcOrdhdr..Alt_ShpTo
          m.STName    = &lcOrdhdr..STName
          m.cAddress1 = &lcOrdhdr..cAddress1
          m.cAddress2 = &lcOrdhdr..cAddress2
          m.cAddress3 = &lcOrdhdr..cAddress3
          m.cAddress4 = &lcOrdhdr..cAddress4
          m.cAddress5 = &lcOrdhdr..cAddress5
          m.cAddress6 = ''
        ELSE
          loCustomer.SEEK(IIF(EMPTY(m.Store) , 'M' + m.Account ,'S' + m.Account + m.Store),'Customer')
          lcDistCenter = &lcTempCustomer..Dist_Ctr
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][Start]
          IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'D'
            m.DC = lcDistCenter
          ENDIF
          *! E303388,1 MMT 06/13/2013 Add Sort by DC# option to Picking ticket form A[T20130213.0029][End]

          IF !EMPTY(lcDistCenter) AND !(&lcOrdhdr..lStrDirct)
            loCustomer.SEEK('S' + m.Account + lcDistCenter,'Customer')
          ENDIF
          m.STName    = IIF(EMPTY(&lcTempCustomer..DBA) , &lcTempCustomer..STName, &lcTempCustomer..DBA)
          m.cAddress1 = &lcTempCustomer..cAddress1
          m.cAddress2 = &lcTempCustomer..cAddress2
          m.cAddress3 = &lcTempCustomer..cAddress3
          m.cAddress4 = &lcTempCustomer..cAddress4
          m.cAddress5 = &lcTempCustomer..cAddress5
          m.cAddress6 = &lcTempCustomer..cAddress6
        ENDIF
        *! B609804,1 MMT 01/26/2012 Pick ticket form does not export ship to address to excel[END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][Start]
        m.ShipVia = gfCodDes(&lcOrdhdr..ShipVia, 'SHIPVIA')
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][start]
        m.ORDER_START= &lcOrdhdr..START
        m.ORDER_COMPLETE= &lcOrdhdr..COMPLETE
        m.APPROVAL= &lcOrdhdr..APPROVAL
        m.DEPT= &lcOrdhdr..DEPT 
        m.NOTE1= &lcOrdhdr..NOTE1
        m.NOTE2= &lcOrdhdr..NOTE2
        m.PHONE= &lcOrdhdr..PHONE 
        *B611500,1 AHH 20/12/2017 Picking ticket form A randomly displays incorrect order information [T20171128.0038][END]
        *B609826,1 MMT 02/09/2012 Export ShipVia Field while exporting Pick ticket form to XLS[T20120112.0012][End]
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[Start]
        =loScale.SEEK('S'+&lcTempStyle..SCALE)
        FOR lnCntSz = 1 TO 8
          lcCntSz = STR(lnCntSz,1)
          m.Size&lcCntSz. = &lcTempScale..Sz&lcCntSz.
        ENDFOR
        *! B609826,2 MMT 02/15/2012 Export Sizes description fields while exporting Pick ticket form to XLS[END]

        INSERT INTO (lcTmpOrdL)  FROM MEMVAR
      ENDIF    && End of IF
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[Start]
  IF lcFormName == "ALPKTKA" AND lcRpPkBy = 'G'
    lcFrstPktkt = ''
    lnPackCntr = 1
    DIMENSION laPackArr[1,4]
    laPackArr = ''
    SELECT (lcTmpOrdL)
    LOCATE
    DO WHILE !EOF()
      lcFileKey =IIF(TYPE('lcRpPrtBy') <> 'U' AND lcRpPrtBy <> 'P',Account,'')+PikTkt
      m.DCNUM = SPACE(8)
      =SEEK('O'+&lcTmpOrdL..ORDER,lcOrdhdr)
      =loCustomer.SEEK(IIF(!EMPTY(&lcTmpOrdL..STORE),'S','M')+&lcTmpOrdL..Account+&lcTmpOrdL..STORE,'Customer')
      m.DCNUM = IIF(!EMPTY(&lcTempCustomer..Dist_Ctr) AND !(&lcOrdhdr..lStrDirct),&lcTempCustomer..Dist_Ctr,SPACE(8))
      lcPikTkt = PikTkt
      llPackFound = .F.
      lcCompPik = ''
      lcPackID = ''
      lcCompExp = ''
      lnRecCntCmp = 0
      lnCompLines = 0
      lnPackNum = 0
      FOR lnT =1 TO IIF(EMPTY(lcFrstPktkt),1,ALEN(laPackArr,1))
        lcCompPik = laPackArr[lnT,2]
        lcPackID = laPackArr[lnT,1]
        lnCompLines =laPackArr[lnT,3]
        lnPackNum =laPackArr[lnT,4]
        lcCompExp = ''
        =SEEK(lcFileKey)
        lnRecCntCmp = 0
        lnRecCnt = 0
        SCAN REST WHILE EVALUATE(KEY())=lcFileKey FOR  cGrupDetal = 'D'
          lnRecCntCmp = lnRecCntCmp + 1
          lnRecCnt = lnRecCnt + 1
          IF EMPTY(lcFrstPktkt) OR  lcFrstPktkt = lcPikTkt
            lcFrstPktkt = lcPikTkt
            m.PKNUM = lnPackCntr
            m.PACKID = 'PACK '+ALLTRIM(STR(lnPackCntr))
            laPackArr[1,1] = m.PACKID
            laPackArr[1,2] = lcPikTkt
            laPackArr[1,4] = lnPackCntr
            LOOP
          ELSE
            IF lcFrstPktkt <> lcPikTkt
              lcCurRecKey = EVALUATE(KEY())
              m.PACKID = ""
              m.PKNUM = 0
              lcCompExp ='D'+STYLE+STR(PIK1,6)+STR(PIK2,6)+STR(PIK3,6)+STR(PIK4,6)+STR(PIK5,6)+STR(PIK6,6)+STR(PIK7,6)+STR(PIK8,6)
              IF SEEK(lcCompPik + lcCompExp ,lcTmpOrdL , 'PackIndex')
                llPackFound = .T.
                =SEEK(lcCurRecKey)
              ELSE
                =SEEK(lcCurRecKey)
                llPackFound = .F.
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
        IF !EMPTY(lcFrstPktkt) AND  lcFrstPktkt <> lcPikTkt
          IF lnCompLines <> lnRecCntCmp
            llPackFound = .F.
          ENDIF
        ENDIF
        IF llPackFound
          EXIT
        ENDIF
      ENDFOR
      IF lcFrstPktkt = lcPikTkt
        laPackArr[1,3] = lnRecCnt
        =SEEK(lcFileKey)
        SCAN REST WHILE EVALUATE(KEY()) = lcFileKey
          IF llRpPrSm AND cGrupDetal = 'D'
            IF !SEEK(SPACE(1)+m.DCNUM +STR(1,5)+&lcTmpOrdL..STYLE+STR(&lcTmpOrdL..PIK1,5)+STR(&lcTmpOrdL..PIK2,5)+STR(&lcTmpOrdL..PIK3,5)+;
                STR(&lcTmpOrdL..PIK4,5)+STR(&lcTmpOrdL..PIK5,5)+STR(&lcTmpOrdL..PIK6,5)+STR(&lcTmpOrdL..PIK7,5)+STR(&lcTmpOrdL..PIK8,5),lcTmpPSm)
              INSERT INTO (lcTmpPSm) (DCNUM,PKNUM,STYLE,PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TotPik,cType,ColorDesc,TOTBOOK);
                VALUES (m.DCNUM ,1,&lcTmpOrdL..STYLE,&lcTmpOrdL..PIK1,&lcTmpOrdL..PIK2,&lcTmpOrdL..PIK3,;
                &lcTmpOrdL..PIK4,&lcTmpOrdL..PIK5,&lcTmpOrdL..PIK6,&lcTmpOrdL..PIK7,&lcTmpOrdL..PIK8,&lcTmpOrdL..TotPik,'',gfCodDes(SUBSTR(&lcTmpOrdL..STYLE,lnClrPosM1,lnClrLnM1), 'COLOR     '),&lcTmpOrdL..TOTBOOK)
            ELSE
              REPLACE TOTBOOK WITH TOTBOOK+ &lcTmpOrdL..TOTBOOK IN (lcTmpPSm)
            ENDIF
          ENDIF
          REPLACE PACKID WITH laPackArr[1,1],;
            PKNUM  WITH 1,;
            DCNUM  WITH  m.DCNUM

        ENDSCAN
      ELSE
        IF !llPackFound AND  !EMPTY(lcCompExp)
          lnPackCntr = lnPackCntr + 1
          m.PACKID = 'PACK '+ALLTRIM(STR(lnPackCntr))
          m.PKNUM = lnPackCntr
          =SEEK(lcFileKey)
          lnRecCnt = 0
          SCAN REST  WHILE EVALUATE(KEY()) = lcFileKey
            *! E302950,3 MMT 08/16/2011 Fix bug of Error while sorting by Pack Group without Summary Report[Start]
            *IF cGrupDetal = 'D'
            IF llRpPrSm AND cGrupDetal = 'D'
              *! E302950,3 MMT 08/16/2011 Fix bug of Error while sorting by Pack Group without Summary Report[END]
              lnRecCnt = lnRecCnt + 1
              *! E302950,3 MMT 08/16/2011 Fix bug of Error while sorting by Pack Group without Summary Report[Start]
              *!*	            IF llRpPrSm AND !SEEK(SPACE(1)+m.DCNUM +STR(m.PKNUM,5)+&lcTmpOrdL..STYLE+STR(&lcTmpOrdL..PIK1,5)+STR(&lcTmpOrdL..PIK2,5)+STR(&lcTmpOrdL..PIK3,5)+;
              *!*	                  STR(&lcTmpOrdL..PIK4,5)+STR(&lcTmpOrdL..PIK5,5)+STR(&lcTmpOrdL..PIK6,5)+STR(&lcTmpOrdL..PIK7,5)+STR(&lcTmpOrdL..PIK8,5),lcTmpPSm)
              IF !SEEK(SPACE(1)+m.DCNUM +STR(m.PKNUM,5)+&lcTmpOrdL..STYLE+STR(&lcTmpOrdL..PIK1,5)+STR(&lcTmpOrdL..PIK2,5)+STR(&lcTmpOrdL..PIK3,5)+;
                  STR(&lcTmpOrdL..PIK4,5)+STR(&lcTmpOrdL..PIK5,5)+STR(&lcTmpOrdL..PIK6,5)+STR(&lcTmpOrdL..PIK7,5)+STR(&lcTmpOrdL..PIK8,5),lcTmpPSm)
                *! E302950,3 MMT 08/16/2011 Fix bug of Error while sorting by Pack Group without Summary Report[END]
                INSERT INTO (lcTmpPSm) (DCNUM,PKNUM,STYLE,PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TotPik,cType,ColorDesc,TOTBOOK);
                  VALUES (m.DCNUM , m.PKNUM,&lcTmpOrdL..STYLE,&lcTmpOrdL..PIK1,&lcTmpOrdL..PIK2,&lcTmpOrdL..PIK3,;
                  &lcTmpOrdL..PIK4,&lcTmpOrdL..PIK5,&lcTmpOrdL..PIK6,&lcTmpOrdL..PIK7,&lcTmpOrdL..PIK8,&lcTmpOrdL..TotPik,'',gfCodDes(SUBSTR(&lcTmpOrdL..STYLE,lnClrPosM1,lnClrLnM1), 'COLOR     '),&lcTmpOrdL..TOTBOOK)
              ELSE
                REPLACE TOTBOOK WITH TOTBOOK+ &lcTmpOrdL..TOTBOOK IN (lcTmpPSm)

              ENDIF
            ENDIF
            REPLACE PACKID WITH m.PACKID,;
              PKNUM  WITH m.PKNUM ,;
              DCNUM WITH  m.DCNUM
          ENDSCAN
          DIMENSION laPackArr[ALEN(laPackArr,1)+1,4]
          laPackArr[ALEN(laPackArr,1),1] = m.PACKID
          laPackArr[ALEN(laPackArr,1),2] = lcPikTkt
          laPackArr[ALEN(laPackArr,1),3] = lnRecCnt
          laPackArr[ALEN(laPackArr,1),4] = lnPackCntr
        ELSE
          =SEEK(lcFileKey)
          lnRecCnt = 0
          SCAN REST  WHILE EVALUATE(KEY()) = lcFileKey
            REPLACE PACKID WITH lcPackID ,;
              PKNUM  WITH lnPackNum,;
              DCNUM WITH  m.DCNUM
            IF  llRpPrSm AND cGrupDetal = 'D'
              IF !SEEK(SPACE(1)+m.DCNUM +STR(lnPackNum,5)+&lcTmpOrdL..STYLE+STR(&lcTmpOrdL..PIK1,5)+STR(&lcTmpOrdL..PIK2,5)+STR(&lcTmpOrdL..PIK3,5)+;
                  STR(&lcTmpOrdL..PIK4,5)+STR(&lcTmpOrdL..PIK5,5)+STR(&lcTmpOrdL..PIK6,5)+STR(&lcTmpOrdL..PIK7,5)+STR(&lcTmpOrdL..PIK8,5),lcTmpPSm)
                INSERT INTO (lcTmpPSm) (DCNUM,PKNUM,STYLE,PIK1,PIK2,PIK3,PIK4,PIK5,PIK6,PIK7,PIK8,TotPik,cType,ColorDesc,TOTBOOK);
                  VALUES (m.DCNUM , lnPackNum,&lcTmpOrdL..STYLE,&lcTmpOrdL..PIK1,&lcTmpOrdL..PIK2,&lcTmpOrdL..PIK3,;
                  &lcTmpOrdL..PIK4,&lcTmpOrdL..PIK5,&lcTmpOrdL..PIK6,&lcTmpOrdL..PIK7,&lcTmpOrdL..PIK8,&lcTmpOrdL..TotPik,'', gfCodDes(SUBSTR(&lcTmpOrdL..STYLE,lnClrPosM1,lnClrLnM1), 'COLOR     '),&lcTmpOrdL..TOTBOOK)
              ELSE
                REPLACE TOTBOOK WITH TOTBOOK+ &lcTmpOrdL..TOTBOOK IN (lcTmpPSm)
              ENDIF
            ENDIF

          ENDSCAN
        ENDIF
      ENDIF
    ENDDO
    IF llRpPrSm
      SELECT(lcTmpPSm)
      SCAN FOR StCnt = 0 OR TotStCnt = 0 OR PackTot = 0
        lnCurRecord = RECNO()
        lcDc = &lcTmpPSm..DCNUM
        lcPk =  &lcTmpPSm..PKNUM
        *! E302950,2 MMT 08/16/2011 Fix bug of wrong# of store in Audit report[Start]
        *!*	      SELECT DISTINC STORE FROM (lcTmpOrdL) WHERE DCNUM = lcDc   AND PKNUM = lcPk  INTO CURSOR 'CntStore'
        *!*	      SELECT DISTINC STORE FROM (lcTmpOrdL) WHERE DCNUM = lcDc   INTO CURSOR 'TCntStore'
        *!*	      SELECT SUM(TotPik) AS PACKTOT FROM (lcTmpPSm) WHERE PKNUM = lcPk AND DcNUM = lcDc INTO CURSOR 'PackCnt'
        SELECT DISTINC STORE FROM (lcTmpOrdL) WHERE DCNUM = lcDc   AND PKNUM = lcPk AND cGrupDetal = 'D' INTO CURSOR 'CntStore'
        SELECT DISTINC STORE FROM (lcTmpOrdL) WHERE DCNUM = lcDc  AND cGrupDetal = 'D' INTO CURSOR 'TCntStore'
        SELECT SUM(TotPik) AS PackTot FROM (lcTmpPSm) WHERE PKNUM = lcPk AND DCNUM = lcDc INTO CURSOR 'PackCnt'
        *! E302950,2 MMT 08/16/2011 Fix bug of wrong# of store in Audit report[ENd]

        SELECT (lcTmpPSm)
        REPLACE StCnt WITH RECCOUNT('CntStore') ALL FOR DCNUM = lcDc  AND PKNUM =lcPk
        REPLACE TotStCnt WITH RECCOUNT('TCntStore') ALL FOR DCNUM = lcDc
        REPLACE PackTot WITH PackCnt.PackTot ALL FOR DCNUM = lcDc  AND PKNUM =lcPk
        IF BETWEEN(lnCurRecord,1,RECCOUNT())
          GO RECORD lnCurRecord
        ENDIF
      ENDSCAN
      SELECT (lcTmpPSm)
      LOCATE
      IF !EOF()
        SELECT SUM(TotPik*StCnt) AS 'Total',SUM(TOTBOOK) AS 'TBOOK', STYLE AS 'STYLE' FROM  (lcTmpPSm) GROUP BY STYLE INTO CURSOR 'StyleTot'
        SELECT (lcTmpPSm)
        APPEND BLANK
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        REPLACE cType WITH 'A',;
          STYLE WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALPKTK_COLORTOT,oAriaApplication.GetHeaderText("LANG_ALPKTK_COLORTOT",AHEADERFILE))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        SELECT 'StyleTot'
        SCAN
          SELECT (lcTmpPSm)
          APPEND BLANK
          REPLACE cType  WITH 'Z' ,;
            STYLE  WITH StyleTot.STYLE,;
            TotPik WITH StyleTot.TOTAL,;
            TOTBOOK WITH StyleTot.TBOOK
        ENDSCAN
      ENDIF
    ENDIF
  ENDIF
  *! E302950,1 MMT 08/04/2011 Sort Piktkt Form A with Packing Group[END]
  SELECT (lcTmpOrdH)
  SET RELATION TO
  = lfGetTempFiles()

  *!*************************************************************
  *! Name      : lfGetTempFiles
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 02/16/2005
  *! Purpose   : function to get the temp files of master files
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************

FUNCTION lfGetTempFiles
  PRIVATE lcType
  *--NotePad File
  SELECT (lcTmpOrdH)
  LOCATE
  SCAN
    loNotePad.SEEK('B'+ORDER)
    SELECT(lcTempNotePad)
    SCATTER MEMO MEMVAR
    INSERT INTO (lcNotePad) FROM MEMVAR
  ENDSCAN
  *--Ordline file
  SELECT (lcTmpOrdH)
  LOCATE
  SCAN
    loOrdLine.SEEK('O'+ &lcTmpOrdH..ORDER + &lcTmpOrdH..STORE)
    SELECT(lcTempOrdLine)
    SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+ &lcTmpOrdH..ORDER + &lcTmpOrdH..STORE
      SCATTER MEMVAR MEMO
      INSERT INTO (lcOrdLnTmp) FROM MEMVAR
    ENDSCAN
  ENDSCAN

  *--Customer file
  SELECT (lcPiktktTemp)
  LOCATE
  SCAN
    IF EMPTY(STORE)
      lcType = 'M'
      loCustomer.SEEK('M'+ Account + STORE)
    ELSE
      lcType = 'S'
      loCustomer.SEEK('S'+ Account + STORE)
    ENDIF

    IF !SEEK(lcType + &lcPiktktTemp..Account + &lcPiktktTemp..STORE,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR
      INSERT INTO (lcCustomer) FROM MEMVAR
      *B607878,1 MMT 12/14/2006 Bug in ship to if DC is used [Start]
      IF !EMPTY(m.Dist_Ctr) AND  !SEEK(lcType + m.Dist_Ctr,lcCustomer)
        IF loCustomer.SEEK('S'+ &lcPiktktTemp..Account + m.Dist_Ctr)
          SELECT(lcTempCustomer)
          SCATTER MEMO MEMVAR
          INSERT INTO (lcCustomer) FROM MEMVAR
        ENDIF
      ENDIF
      *B607878,1 MMT 12/14/2006 Bug in ship to if DC is used [End]
    ENDIF

  ENDSCAN

  *--Style File
  SELECT(lcTmpOrdL)
  LOCATE
  SCAN
    loStyle.SEEK(STYLE)
    SELECT(lcStyleFile)
    IF !SEEK(&lcTempStyle..STYLE,lcStyleFile)
      SELECT(lcTempStyle)
      SCATTER MEMO MEMVAR
      INSERT INTO (lcStyleFile) FROM MEMVAR
    ENDIF
  ENDSCAN

  *--Scale File
  SELECT(lcStyleFile)
  LOCATE
  SCAN
    loScale.SEEK('S'+ SCALE)
    IF !SEEK('S'+ &lcTempScale..SCALE,lcScaleFile)
      SELECT(lcTempScale)
      SCATTER MEMO MEMVAR
      INSERT INTO (lcScaleFile) FROM MEMVAR
    ENDIF
  ENDSCAN

  *--WAREHOUS FILE
  SELECT(lcPiktktTemp)
  LOCATE
  SCAN
    loWareHous.SEEK(&lcPiktktTemp..cWareCode)
    IF !SEEK(&lcTempWareHous..cWareCode,lcWareHous)
      SELECT(lcTempWareHous)
      SCATTER MEMO MEMVAR
      INSERT INTO (lcWareHous) FROM MEMVAR
    ENDIF
  ENDSCAN
  *!*************************************************************
  *! Name      : lfOpenSql
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 02/16/2005
  *! Purpose   : function to open SQL tables
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfOpenSql

  LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
  LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
  PRIVATE laIndex
  DIMENSION laIndex[1,2]

  lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
  *lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3, 'BROWSE',SET("DATASESSION"))*-*-
  IF TYPE('lcXMLFileName') = 'C'
    lnConnectionHandlar = loogscroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3, 'BROWSE',SET("DATASESSION"))
  ELSE
    lnConnectionHandlar = loogscroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3, 'BROWSE',SET("DATASESSION"))
  ENDIF
  *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]

  IF lnConnectionHandlar = 1
    lnBuffering = CURSORGETPROP("Buffering",lcCursor)
    =CURSORSETPROP("Buffering",3,lcCursor)
    *-- To initialize the indecis that will be created for each file
    =lfCrtindex(lcCursor)
    SELECT (lcCursor)
    FOR lnI = 1 TO ALEN(laIndex,1)
      lcIndex = laIndex[lnI,1]
      lcTag   = laIndex[lnI,2]
      INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
    ENDFOR
    lcTag = laIndex[1,2]
    SET ORDER TO TAG (lcTag)

  ELSE
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *=loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)*-*-
    IF TYPE('lcXMLFileName') = 'C'
      =loogscroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    ELSE
      =loogscroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
    RETURN .F.
  ENDIF
  *-- end of lfOpenSql.


  *!*************************************************************
  *! Name      : lfUpdate
  *: Developer : Tarek Noaman (TNA)
  *: Date      : 02/20/2006
  *! Purpose   : function to raise printer flag  B607739
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfUpdate
  llPrinter = (SYS(2040)='2')

  *! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [Start]
  IF llPrinter
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [Start]
    *loOgScroll.ll2Printer = .T.
    IF TYPE('lcXMLFileName') <> 'C'
      loogscroll.ll2Printer = .T.
    ENDIF
    *! E303322,1 SAB 12/06/2012 Modify the report to run from request builder [End]
  ENDIF
  *! B609080,1 MMT 11/10/2009 Fix bug of not updating Print Status Flag when piktkt is printed [End]

  RETURN .T.
  *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][Start]
  *!*************************************************************
  *! Name      : lfGetPackDesc
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 04/04/2017
  *! Purpose   : function to get Pack Description
  *!*************************************************************
FUNCTION lfGetPackDesc
  LPARAMETERS lcAccount,lcPACK_ID
  lnAlias = SELECT(0)
  IF !USED('SPCK_HDR')
    =gfOpenTable('SPCK_HDR','SPCK_HDR','SH')  && TYPE+ACCOUNT+PACK_ID
  ENDIF
  lcDesc = ""
  IF gfSeek('P'+lcAccount+lcPACK_ID,'SPCK_HDR','SPCK_HDR')
    lcDesc = SPCK_HDR.DESC
  ELSE
    IF gfSeek('P'+'*****'+lcPACK_ID,'SPCK_HDR','SPCK_HDR')
      lcDesc = SPCK_HDR.DESC
    ENDIF
  ENDIF
  SELECT (lnAlias)
  RETURN lcDesc
  *!*************************************************************
  *! Name      : lfGetPackPrice
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 04/04/2017
  *! Purpose   : function to get Pack Price
  *!*************************************************************
FUNCTION lfGetPackPrice
  LPARAMETERS lcPACK_ID,lcOrder,lcPikTkt
  lnAlias = SELECT(0)
  lnPackPr = 0
  IF &lcPiktktTemp..STATUS $ 'CX'
    IF loPikLine.SEEK(lcPikTkt+lcOrder)
      SELECT(lcTempPikLine)
      SUM Price*TotPik TO lnPackPr REST WHILE PikTkt + ORDER = lcPikTkt+lcOrder FOR pack_id =lcPACK_ID
    ENDIF
  ELSE
    SELECT (lcOrdLnTmp)
    lcKeyVal = EVALUATE(KEY())
    IF SEEK('O'+lcOrder)
      SUM Price*TotPik TO lnPackPr REST WHILE cOrdType+ORDER+STR(LINENO,6)='O'+lcOrder FOR PikTkt = lcPikTkt AND  pack_id =lcPACK_ID
    ENDIF
    =SEEK(lcKeyVal)
  ENDIF
  SELECT (lnAlias)
  RETURN lnPackPr
  *!*************************************************************
  *! Name      : lfGetNoPack
  *: Developer : Mariam Mazhar (MMT)
  *: Date      : 04/04/2017
  *! Purpose   : function to number of packs
  *!*************************************************************
FUNCTION lfGetNoPack
  LPARAMETERS lcPACK_ID,lcAccount,lcPikTkt,lcStyle,lnPikQty
  lnAlias = SELECT(0)
  IF !USED('SPCK_LIN')
    =gfOpenTable('SPCK_LIN','SPCK_LIN','SH')  && TYPE+ACCOUNT+PACK_ID+STYLE+DYELOT
  ENDIF
  lnNoPack = 0
  IF gfSeek('P'+lcAccount+lcPACK_ID+lcStyle,'SPCK_LIN','SPCK_LIN')
    lnNoPack = lnPikQty/Spck_lin.TotQty
  ELSE
    IF gfSeek('P'+'*****'+lcPACK_ID+lcStyle,'SPCK_LIN','SPCK_LIN')
      lnNoPack = lnPikQty/Spck_lin.TotQty
    ENDIF
  ENDIF
  SELECT (lnAlias)
  RETURN lnNoPack
  *! E303785,1 MMT 04/04/2017 Add option to Picking ticket form A to print by Pack[T20160603.0003][End]