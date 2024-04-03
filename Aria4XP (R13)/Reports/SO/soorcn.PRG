*:***************************************************************************
*: Program file  : Soorcn
*: Program desc. : Order Confirmation
*: For Report    : SOORCNA.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Heba Mohamed Amin (HMA)
*: Date          : 06/23/2004
*: Reference     : N037461,1
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes,gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep,lfsChOrder
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : All IF llFrTime Blocks executed one time in the option grid seasson.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time.
*:***************************************************************************
*: Example : DO Soorcn
*:***************************************************************************
*: Modifications:
*: C038190,1 HMA 10/03/2004 Check Configuration Setup &use it in Custom report layout of GMA
*: B037461,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division.
*: HMA 14/12/2004 convert SEASON filter from Variable  filter into fixed filter
*: HMA 14/12/2004 Fix error in the last filter added to lcRpExp
*: B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line
*:                in the Option grid if the order status or order type has changed.
*: E127836,1 HMA 06/15/2005 use order status as mover screen to enable the user
*:                to select more than one status.
*:B131798,1 AYM 04/15/2006  Order confirmation is hanged when you try to preview multi store order.
*:N000592,1 HBG 02/28/2007 T20061201.0014 Print store ship to address or DC address according to flag in SO
*:B608070,1 MMT 05/02/2007 fix bug of error while preview EDI Temp Orders [T20070426.0007]
*:C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation
*:E320552,1 MMT 08/18/2008 Add complete date to option grid [T20080721.0012]
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[T20090831.0015]
*!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [T20100304.0015]
*!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[T20100222.0005]
*!B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[T20110406.0070]
*!B609618,1 MMT 06/16/2011 Cannot print order confirmation of EDI Temp. Order from scr.[T20110608.0006]
*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025]
*!B610090,1 MMT 09/19/2012 Order confirmation report prints currency symbol always on right side of price and amount[T20120827.0042]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[T20121019.0003]
*!B610225,1 HIA 02/03/2013 SO - Account COde in List is missing from the Order Confirmation print [T20130123.0033]
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044]
*!E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*!B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019]
*! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13]
*:E303489,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81][Start]
*!B610954,1 MMT 03/01/2015 Order Confirmation report error because of style code structure[T20150224.0007]
*:***************************************************************************
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientId

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
  PRIVATE loAgent
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  PRIVATE loProgress
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *loProgress = goRemoteCall.GetRemoteObject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  LOCAL loEnvironment
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  loEnvironment.ClientId = ClientId
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  lcActiveMod = 'SO'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'SO'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.Report.cCROrientation = 'P'
  =gfOpenFile('NOTEPAD','NOTEPAD')
  =gfOpenFile('ORDHDR','ORDHDR')
  =gfOpenFile('CUSTOMER','CUSTOMER')
  =gfOpenFile('Warehous','WAREHOUS')
  =gfOpenFile('Ordline','Ordline')
  =gfOpenFile('OBJLINK','OBJLNKTY')
  =gfOpenFile('OBJECTS','OBJECTID')
  =gfOpenFile('OBJLINK','OBJLNKTY','SH','OBJLINK_A')
  =gfOpenFile('OBJECTS','OBJECTID','SH','OBJECTS_A')
  =gfOpenFile('SCALE','SCALE')
  =gfOpenFile('Style','Style')
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]

#INCLUDE R:\Aria4xp\reports\so\soorcn.H
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[Start]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*lcRpPrSt = IIF(oAriaApplication.PROCESSID = 'SOORCN',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
lcRpPrSt = IIF(TYPE('lcXMLFileName')='C' or oAriaApplication.PROCESSID = 'SOORCN',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
CREATE CURSOR (lcOrdPrtUp) (ORDER C(6),CORDTYPE C(1))
INDEX ON CORDTYPE+ORDER TAG (lcOrdPrtUp)
llPrinter = .F.
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[End]

*!B609618,1 MMT 06/16/2011 Cannot print order confirmation of EDI Temp. Order from scr.[Start]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*lcRpOrdTyp = IIF(oAriaApplication.PROCESSID = 'SOORCN',lcRpOrdTyp ,"A")
lcRpOrdTyp = IIF(TYPE('lcXMLFileName')='C' OR oAriaApplication.PROCESSID = 'SOORCN',lcRpOrdTyp ,"A")
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*!B609618,1 MMT 06/16/2011 Cannot print order confirmation of EDI Temp. Order from scr.[End]


lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note.
lcNotes    = ''                         && Notes.
llPrintBox = .F.                        && Box around Notes.
llPrtCmpdt = gfGetMemVar('M_CMPDOLN')  && Checking Comp. Date by Order line. used in the complete date in the frx form A.
*Initial variable to check if print the report from the main or from the optional prorgram.
llSalsOrd = .T.          && llarpinv it will be a commen variable.


*-- Note Variables [end]


IF !llFrTime
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lcFormName = lfGetForm()
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ELSE
    lcFormName = oAriaEnvironment.REPORT.GetForm('SOORCN')
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),loOgScroll.gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcFormName,'',llOGRefForm)  && Create Temp. file for new form.
ENDIF

*Get the form name.
llNoRec= .T.
lcPrgName  = lcFormName
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
llIsAparel = lfIsApparl(@lcPrgName)
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
  lcFormName = oAriaEnvironment.REPORT.GetForm('SOORCN')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]

llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = llRpOrdLnt AND llRpOrdNot  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval
llTitle    = .T.                        && Flag to print Detail header.

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'
*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][Begin]
DIMENSION laMajSegs[1,1]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') == 'C'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laMajSegs)
  lnMajLen = loItemMask.Do('SM')  && No. of major segments.
ELSE
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[END]
= gfItemMask(@laMajSegs)
lnMajLen = gfItemMask('SM')  && No. of major segments.
*-- Loop Around Non Major elements.
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*!B610954,1 MMT 03/01/2015 Order Confirmation report error because of style code structure[T20150224.0007][Start]
STORE 0 TO lnMajPos ,lnMajLen ,lnClrPos ,lnClrLen ,lnSclPos ,lnSclLen 
*!B610954,1 MMT 03/01/2015 Order Confirmation report error because of style code structure[T20150224.0007][End]
STORE '' TO lcNonMajPi
FOR lnI =  1 TO ALEN(laMajSegs,1)
  DO CASE
  CASE laMajSegs[lnI ,1] = 'F'
    lnMajPos = laMajSegs[lnI ,4]
    lnMajLen = LEN(laMajSegs[lnI ,3])
  CASE laMajSegs[lnI ,1] = 'C'
    lnClrPos = laMajSegs[lnI ,4]
    lnClrLen = LEN(laMajSegs[lnI ,3])
  CASE laMajSegs[lnI ,1] = 'S'
    lnSclPos = laMajSegs[lnI ,4]
    lnSclLen = LEN(laMajSegs[lnI ,3])
  ENDCASE
ENDFOR
*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][End]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = gfItemMask('HI')        && Title of the style.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX

  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
      SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF
*--create temp file for ordline file
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  lcTempOrd = loOgScroll.gfTempName()
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
  lcTempOrd = gfTempName()
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [Start]
IF llRpStrOrd
  DIMENSION laStoreTmp[9,4]
  laStoreTmp[1,1] = 'lSelect'
  laStoreTmp[1,2] = 'L'
  laStoreTmp[1,3] = 1
  laStoreTmp[1,4] = 0

  laStoreTmp[2,1] = 'ORDER'
  laStoreTmp[2,2] = 'C'
  laStoreTmp[2,3] = 6
  laStoreTmp[2,4] = 0

  laStoreTmp[3,1] = 'STORE'
  laStoreTmp[3,2] = 'C'
  laStoreTmp[3,3] = 8
  laStoreTmp[3,4] = 0

  laStoreTmp[4,1] = 'STNAME'
  laStoreTmp[4,2] = 'C'
  laStoreTmp[4,3] = 30
  laStoreTmp[4,4] = 0

  laStoreTmp[5,1] = 'cAddress1'
  laStoreTmp[5,2] = 'C'
  laStoreTmp[5,3] = 30
  laStoreTmp[5,4] = 0

  laStoreTmp[6,1] = 'cAddress2'
  laStoreTmp[6,2] = 'C'
  laStoreTmp[6,3] = 30
  laStoreTmp[6,4] = 0

  laStoreTmp[7,1] = 'cAddress3'
  laStoreTmp[7,2] = 'C'
  laStoreTmp[7,3] = 30
  laStoreTmp[7,4] = 0

  laStoreTmp[8,1] = 'cAddress4'
  laStoreTmp[8,2] = 'C'
  laStoreTmp[8,3] = 30
  laStoreTmp[8,4] = 0

  laStoreTmp[9,1] = 'cAddress5'
  laStoreTmp[9,2] = 'C'
  laStoreTmp[9,3] = 30
  laStoreTmp[9,4] = 0

  =gfCrtTmp(lcTmpStr,@laStoreTmp,'Order+Store',lcTmpStr)
  DIMENSION laOrdArr[1,4]
  laOrdArr[1,1] = 'ORDER'
  laOrdArr[1,2]='C'
  laOrdArr[1,3]=6
  laOrdArr[1,4]=0
  =gfCrtTmp(lcTmpOrStr,@laOrdArr,'Order',lcTmpOrStr)
ENDIF
*!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [END]


*--- Relation between opened files
*-- Note that the files was opened in Rep. Gen.

SELECT ORDHDR
SET RELATION TO cordtype+ ORDER INTO Ordline ADDITIVE

DIMENSION laFilStruc[1]
SELECT ORDLINE
= AFIELDS(laFilStruc)

*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][Begin]
DIMENSION laFilStruc[ALEN(laFilStruc,1)+1,ALEN(laFilStruc,2)]
*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][Begin]
laFilStruc[ALEN(laFilStruc,1),1]='ClrDesc'
laFilStruc[ALEN(laFilStruc,1),2]='C'
laFilStruc[ALEN(laFilStruc,1),3]=30
laFilStruc[ALEN(laFilStruc,1),4]=0
FOR I = 5 TO ALEN(laFilStruc,2)
  laFilStruc[ALEN(laFilStruc,1),I] = laFilStruc[ALEN(laFilStruc,1)-1,I]
ENDFOR
*!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][End]


*--Check if Mrgret O'Leary Custom form Add Some New Fields To the Temp. File
IF TYPE('lcMargFrm')='C' AND lcMargFrm = 'Y'
  STORE .T. TO llMargFrm
  =lfUsrVldFn('lfAddFlds','SOORCNMO')
ELSE
  STORE .F. TO llMargFrm
ENDIF


*CREATE TABLE (oAriaApplication.WorkDir+lcTempOrd) FROM ARRAY laFilStruc
gfCrtTmp(lcTempOrd,@laFilStruc)

IF lcRpOrdTyp = "T"
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[END]
    lcTempRcp = loOgScroll.gfTempName()   && Temp file to collect data for recap
    lcTempRpt = loOgScroll.gfTempName()   && Temp file to collect DOS resords for recap report
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ELSE
    lcTempRcp = gfTempName()   && Temp file to collect data for recap
    lcTempRpt = gfTempName()   && Temp file to collect DOS resords for recap report
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  DIMENSION laTmpRcp[17,4]

  laTmpRcp[1,1]='Account'
  laTmpRcp[1,2]='C'
  laTmpRcp[1,3]=5
  laTmpRcp[1,4]=0

  laTmpRcp[2,1]='Style'
  laTmpRcp[2,2]='C'
  laTmpRcp[2,3]=19
  laTmpRcp[2,4]=0

  laTmpRcp[3,1]='Qty1'
  laTmpRcp[3,2]='N'
  laTmpRcp[3,3]=7
  laTmpRcp[3,4]=0

  laTmpRcp[4,1]='Qty2'
  laTmpRcp[4,2]='N'
  laTmpRcp[4,3]=7
  laTmpRcp[4,4]=0

  laTmpRcp[5,1]='Qty3'
  laTmpRcp[5,2]='N'
  laTmpRcp[5,3]=7
  laTmpRcp[5,4]=0

  laTmpRcp[6,1]='Qty4'
  laTmpRcp[6,2]='N'
  laTmpRcp[6,3]=7
  laTmpRcp[6,4]=0

  laTmpRcp[7,1]='Qty5'
  laTmpRcp[7,2]='N'
  laTmpRcp[7,3]=7
  laTmpRcp[7,4]=2

  laTmpRcp[8,1]='Qty6'
  laTmpRcp[8,2]='N'
  laTmpRcp[8,3]=7
  laTmpRcp[8,4]=0

  laTmpRcp[9,1]='Qty7'
  laTmpRcp[9,2]='N'
  laTmpRcp[9,3]=7
  laTmpRcp[9,4]=0

  laTmpRcp[10,1]='Qty8'
  laTmpRcp[10,2]='N'
  laTmpRcp[10,3]=7
  laTmpRcp[10,4]=0

  laTmpRcp[11,1]='TotQty'
  laTmpRcp[11,2]='N'
  laTmpRcp[11,3]=8
  laTmpRcp[11,4]=0

  laTmpRcp[12,1]='Amount'
  laTmpRcp[12,2]='N'
  laTmpRcp[12,3]=13
  laTmpRcp[12,4]=2

  laTmpRcp[13,1]='nSRP'
  laTmpRcp[13,2]='N'
  laTmpRcp[13,3]=8
  laTmpRcp[13,4]=2

  laTmpRcp[14,1]='Scale'
  laTmpRcp[14,2]='C'
  laTmpRcp[14,3]=3
  laTmpRcp[14,4]=0

  laTmpRcp[15,1]='Note1'
  laTmpRcp[15,2]='C'
  laTmpRcp[15,3]=30
  laTmpRcp[15,4]=0

  laTmpRcp[16,1]='Note2'
  laTmpRcp[16,2]='C'
  laTmpRcp[16,3]=20
  laTmpRcp[16,4]=0

  laTmpRcp[17,1]='Dyelot'
  laTmpRcp[17,2]='C'
  laTmpRcp[17,3]=10
  laTmpRcp[17,4]=0

  IF !llConfig
    lcRcpIdx="Account+Style"
  ELSE
    lcRcpIdx="Account+Style+Dyelot"
  ENDIF
  gfCrtTmp(lcTempRcp,@laTmpRcp,lcRcpIdx,lcTempRcp,.F.)


ENDIF

SELECT (lcTempOrd)
IF llMargFrm
  INDEX ON &lcTmpIndx TAG (lcTempOrd)
ELSE
  IF lcRpSortBy = 'S'
    INDEX ON CORDTYPE + ORDER + STORE + STYLE TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ELSE
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ENDIF
ENDIF
*-- B037461,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division.[Begin]
*!*	*Add the default LEDIORDER = .F. to the filter expression.
*!*	lcEdiExpr = IIF(lcRpEDIFlt='O',[!ORDHDR.lEDIOrder],IIF(lcRpEDIFlt='E',[ORDHDR.lEDIOrder],""))
*!*	IF !EMPTY(lcEdiExpr)
*!*	  IF !EMPTY(lcRpExp)
*!*	    lcRpExp = lcRpExp + [ AND ]
*!*	  ENDIF
*!*	  lcRpExp = lcRpExp + lcEdiExpr
*!*	ENDIF

*!*	SELECT ORDHDR
*!*	SCAN FOR &lcRpExp

SELECT ORDHDR
lcNewExp=""
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*!*  IF LEN(loOGScroll.lcRpExp) > 1
*!*    lcExp = loOGScroll.lcRpExp
IF LEN(IIF(TYPE('lcXMLFileName') <> 'C',loOGScroll.lcRpExp,lcRpExp)) > 1
  lcExp = IIF(TYPE('lcXMLFileName') <> 'C',loOGScroll.lcRpExp,lcRpExp)
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  lnOccur = OCCURS(' AND',lcExp)
  IF lnOccur > 0
    FOR lnCount = 1 TO lnOccur + 1
      lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 5)
      lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC(' AND',lcExp,lnCount))
      lnLength = lnEnd - lnStart +IIF(lnCount = lnOccur + 1,1,0)
      lcTake = SUBSTR(lcExp,lnStart,lnLength-1)
      *HMA 14/12/2004 Fix error in the last filter added to lcRpExp [Start]
      IF ATC(')',lcTake)=0  OR ATC('ENTERED',lcTake)>1
        lcTake = SUBSTR(lcExp,lnStart,lnLength)
      ENDIF
      *HMA 14/12/2004 Fix error in the last filter added to lcRpExp [End]
      *:E320552,1 MMT 08/18/2008 Add complete date to option grid [Start]
      IF ATC(')',lcTake)=0  OR ATC('COMPLETE',lcTake)>1
        lcTake = SUBSTR(lcExp,lnStart,lnLength)
      ENDIF
      *:E320552,1 MMT 08/18/2008 Add complete date to option grid [End]
      lnoccurs=ATC('INLIST',lcTake)
      lnSeaOcurr=ATC('SEASON',lcTake)
      lnDivOcurr=ATC('CDIVISION',lcTake)
      IF lnoccurs > 0
        IF  (lnSeaOcurr > 0 OR lnDivOcurr > 0)
          lcTake = ""
        ELSE
          lcTake = SUBSTR(lcExp,lnStart,lnLength)
        ENDIF
      ENDIF
      IF !EMPTY(lcNewExp)
        IF !EMPTY(lcTake)
          lcNewExp = lcNewExp + ' .AND. '+ lcTake
        ENDIF
      ELSE
        lcNewExp = lcTake
      ENDIF
    ENDFOR
  ENDIF
ENDIF
*lcRpExp =lcNewExp
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*lcRpExp = IIF(EMPTY(lcNewExp),loOGScroll.lcRpExp,lcNewExp)
lcRpExp = IIF(EMPTY(lcNewExp),IIF(TYPE('lcXMLFileName') <> 'C',loOGScroll.lcRpExp,lcRpExp),lcNewExp)
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*Add the default LEDIORDER = .F. to the filter expression.
lcEdiExpr = IIF(lcRpEDIFlt='O',[!ORDHDR.lEDIOrder],IIF(lcRpEDIFlt='E',[ORDHDR.lEDIOrder],""))
IF !EMPTY(lcEdiExpr)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcEdiExpr
ENDIF

*--Make Temp File For Selected Orders Divisions
lcDivCursor =""
*HMA 14/12/2004 convert division filter from Variable  filter into fixed filter [Start]
*lnOrdDivision = ASCAN(loOgScroll.laOgVrFlt,"ORDHDR.CDIVISION")
*lnOrdDivision = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnOrdDivision,1)
*lcDivisions= loOgScroll.laOgVrFlt[lnOrdDivision,6]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  lnOrdDivision = ASCAN(loOgScroll.laOgFxFlt,"ORDHDR.CDIVISION")
  lnOrdDivision = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnOrdDivision,1)
  lcDivisions= loOgScroll.laOgFxFlt[lnOrdDivision,6]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
  lnOrdDivision = ASCAN(laOgFxFlt,"ORDHDR.CDIVISION")
  lnOrdDivision = ASUBSCRIPT(laOgFxFlt,lnOrdDivision,1)
  lcDivisions= laOgFxFlt[lnOrdDivision,6]
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*HMA 14/12/2004 convert division filter from Variable  filter into fixed filter [END]
IF !EMPTY(lcDivisions)
  IF lnOrdDivision > 0
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
    *lcDivCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
    lcDivCursor = IIF(TYPE('lcXMLFileName') <> 'C',loOgScroll.gfTempName(),gfTempName()) &&Cursor Hold Selected Divisions
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
    IF !EMPTY(lcDivisions)
      lnStart=1
      lnEnd=AT('|',lcDivisions)
      DO WHILE lnEnd <> 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
        lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcDivisions)
      ENDDO
      IF lnEnd = 0
        SELECT(lcDivCursor)
        APPEND BLANK
        REPLACE CDIVISION WITH lcDivisions
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(lcDivCursor)
  SET ORDER TO TAG (lcDivCursor)
ENDIF

*--Make Temp File For Selected Orders Seasons
lcSeaCursor =""
*HMA 14/12/2004 convert SEASON filter from Variable  filter into fixed filter [Start]
*lnOrdSeason = ASCAN(loOgScroll.laOgVrFlt,"ORDHDR.SEASON")
*lnOrdSeason = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnOrdSeason,1)
*lcSeasons= loOgScroll.laOgVrFlt[lnOrdSeason,6]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  lnOrdSeason = ASCAN(loOgScroll.laOgFxFlt,"ORDHDR.SEASON")
  lnOrdSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnOrdSeason,1)
  lcSeasons= loOgScroll.laOgFxFlt[lnOrdSeason,6]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
  lnOrdSeason = ASCAN(laOgFxFlt,"ORDHDR.SEASON")
  lnOrdSeason = ASUBSCRIPT(laOgFxFlt,lnOrdSeason,1)
  lcSeasons= laOgFxFlt[lnOrdSeason,6]
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*HMA 14/12/2004 convert SEASON filter from Variable  filter into fixed filter [End]
IF !EMPTY(lcSeasons)
  IF lnOrdSeason > 0
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
    *lcSeaCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
    lcSeaCursor = IIF(TYPE('lcXMLFileName') <> 'C',loOgScroll.gfTempName(),gfTempName()) &&Cursor Hold Selected Seasons
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='SEASON'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    gfCrtTmp(lcSeaCursor,@laTempacstru,"SEASON",lcSeaCursor,.T.)
    IF !EMPTY(lcSeasons)
      lnStart=1
      lnEnd=AT('|',lcSeasons)
      DO WHILE lnEnd <> 0
        SELECT(lcSeaCursor)
        APPEND BLANK
        REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
        lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
        lnEnd=AT('|',lcSeasons)
      ENDDO
      IF lnEnd = 0
        SELECT(lcSeaCursor)
        APPEND BLANK
        REPLACE SEASON WITH lcSeasons
      ENDIF
    ENDIF
  ENDIF
ENDIF
IF !EMPTY(lcSeaCursor)
  SET ORDER TO TAG (lcSeaCursor)
ENDIF

*!B610225,1 HIA 02/03/2013 SO - Account COde in List is missing from the Order Confirmation print [T20130123.0033] [Start]
*--Make Temp File For Selected Orders Customers
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][Start]
*lcAccCursor =""
lcAccounts =""
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][End]
lnOrdAcc   = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.ACCOUNT")
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][Start]
IF lnOrdAcc > 0
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][END]
lnOrdacc   = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnOrdacc,1)
lcAccounts = loOgScroll.laOgFxFlt[lnOrdAcc,6]
*!B610225,1 HIA 02/03/2013 SO - Account COde in List is missing from the Order Confirmation print [T20130123.0033] [End]
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][Start]
ENDIF
*!B610240,1 MMT 02/12/2013 Error while preview order confirmation if has saved filters[T20130211.0044][END]

*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[Start]
lcRpExp = lcRpExp + IIF(!EMPTY(lcRpExp)," AND ","")+" ordhdr.prtflag=lcRpPrSt"
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[End]


SELECT ORDHDR
SCAN FOR &lcRpExp
  IF !EMPTY(lcDivCursor)
    IF !SEEK(CDIVISION,lcDivCursor)
      LOOP
    ENDIF
  ENDIF
  IF !EMPTY(lcSeaCursor)
    IF !SEEK(SEASON,lcSeaCursor)
      LOOP
    ENDIF
  ENDIF
  *!B610225,1 HIA 02/03/2013 SO - Account COde in List is missing from the Order Confirmation print [T20130123.0033] [Start]
  IF !EMPTY(lcAccounts) AND USED(lcAccounts) AND RECCOUNT(lcAccounts)>0
    IF !SEEK(OrdHdr.Account,lcAccounts)
      LOOP
    ENDIF
  ENDIF
  *!B610225,1 HIA 02/03/2013 SO - Account COde in List is missing from the Order Confirmation print [T20130123.0033] [End]
  *-- B037461,1 HMA 11/01/2004 fix the bug of select more than 24 value in season or division.[END]
  SELECT ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SCATTER MEMVAR MEMO
    *--Check if Mrgret O'Leary Custom form Replace the New Fields on the Temp. File
    IF llMargFrm
      =lfUsrVldFn('lfRepFlds','SOORCNMO')
    ENDIF
    *!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][Begin]
    m.ClrDesc = ''
    m.ClrDesc = gfCodDes(SUBSTR(ALLTRIM(m.Style),lnClrPos,lnClrLen),"COLOR")
    *!B610032,1 HIA 07/30/2012 Cannot print order confirmation of EDI Temp. Order from scr.[T20120717.0025][End]
    INSERT INTO (lcTempOrd) FROM MEMVAR
    *!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [Start]
    IF llRpStrOrd AND ordhdr.MULTI = 'Y'
      IF !SEEK(m.Order+PADR(m.Store,8),lcTmpStr)
        =SEEK('S'+m.Account+m.Store,'Customer')
        INSERT INTO (lcTmpStr) FROM MEMVAR
        REPLACE cAddress1 WITH Customer.caddress1,;
          cAddress2 WITH Customer.caddress2,;
          cAddress3 WITH Customer.caddress3,;
          cAddress4 WITH Customer.caddress4,;
          cAddress5 WITH Customer.caddress5,;
          stname  WITH Customer.stname ,;
          lSelect WITH .T. IN  (lcTmpStr)
      ENDIF
      IF !SEEK(m.Order,lcTmpOrStr)
        INSERT INTO (lcTmpOrStr) FROM MEMVAR
      ENDIF
    ENDIF
    *!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [End]
    IF lcRpOrdTyp = "T"
      SELECT (lcTempRcp)
      IF !llConfig
        llSeek=SEEK(m.Account+m.Style)
      ELSE
        llSeek=SEEK(m.Account+m.Style+m.Dyelot)
      ENDIF
      IF !llSeek
        INSERT INTO (lcTempRcp) (Account,STYLE,Dyelot,nSRP,SCALE) VALUES ;
          (m.Account,m.Style,m.Dyelot,m.nSugRetPri,m.Scale)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
        Qty2   WITH Qty2   + m.Qty2 ,;
        Qty3   WITH Qty3   + m.Qty3 ,;
        Qty4   WITH Qty4   + m.Qty4 ,;
        Qty5   WITH Qty5   + m.Qty5 ,;
        Qty6   WITH Qty6   + m.Qty6 ,;
        Qty7   WITH Qty7   + m.Qty7 ,;
        Qty8   WITH Qty8   + m.Qty8 ,;
        TotQty WITH TotQty + m.TotQty ,;
        Amount WITH Amount + m.TotQty*m.Price
    ENDIF
  ENDSCAN
ENDSCAN
*--Check if Mrgret O'Leary Custom form Replace the New Fields on the Temp. File
IF llMargFrm
  =lfUsrVldFn('lfChcFlds','SOORCNMO')
ENDIF

GO TOP
SELECT ORDHDR
SET RELATION OFF INTO ORDLINE

SELECT ORDHDR
SET RELATION TO cordtype+ ORDER INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(STORE) , 'M' + Account ,;
  'S' + Account + STORE) INTO CUSTOMER ADDITIVE
SELECT (lcTempOrd)
*!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
IF lcRpBook <> 'Y'
  *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
  SET FILTER TO TotQty != 0
  *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
ENDIF
*!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
SET RELATION TO 'S'+SUBSTR(STYLE,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT OBJLINK_A
SET RELATION TO
SET RELATION TO cobject_id INTO OBJECTS_A ADDITIVE
IF llFrTime
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laCompAdd = ''                    && Array to hold the Company address
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  *-- Get company Address
  lcWorkArea = SELECT()
  PRIVATE  lcSqlCommand , lnResult
  lcSqlCommand=[SELECT cCom_Name,cCom_Phon,cCom_fax,cCont_code,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6 FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') = 'C'
    lnResult   = oariaenvironment.remotesystemdata.execute(lcSqlCommand,'',"syccomp","",oariaenvironment.SystemConnectionString,3,"",SET("Datasession"))
  ELSE
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF lnResult >= 1
    lcCompName = cCom_Name
    lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
    lcCompFax  = cCom_fax              && Variable to hold the Company Fax
    lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format

    *-- Load Company address.
    laCompAdd[1] = gfGetAdr('SYCCOMP', '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('SYCCOMP', '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('SYCCOMP', '' , '' , '' , 3)
    laCompAdd[4] = gfGetAdr('SYCCOMP', '' , '' , '' , 4)
    laCompAdd[5] = gfGetAdr('SYCCOMP', '' , '' , '' , 5)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF

  SELECT (lcWorkArea)
ENDIF
*!B610090,1 MMT 09/19/2012 Order confirmation report prints currency symbol always on right side of price and amount[Start]
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*llMulCurr = gfGetMemVar('llMulCurr',oAriaApplication.ActiveCompanyID)
llMulCurr = gfGetMemVar('llMulCurr')
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[ENd]
lcCurrPost = "LEFT"                              && Default Value.
IF llMulCurr
  lcSelectCommand = [SELECT cCurrency, cCurrencyI FROM SYCINT WHERE ccont_code = '] + SycComp.cCont_Code + [']
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') = 'C'
    lnRemoteResult= oariaenvironment.remotesystemdata.execute(lcSelectCommand,'',"SYCINT","",oariaenvironment.SystemConnectionString,3,"",SET("Datasession"))
  ELSE
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",;
       oAriaApplication.SystemConnectionString,3,"")
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF lnRemoteResult >= 1
    SELECT SYCINT
    LOCATE
    IF FOUND()
      lcCurrPost = SycInt.cCurrency
    ENDIF
  ENDIF
ENDIF
*!B610090,1 MMT 09/19/2012 Order confirmation report prints currency symbol always on right side of price and amount[END]
lcSkipExpr  = [&lcTempOrd]

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created
*--           : one for every optional grid seasson run.
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
*lcNoteLns = IIF(EMPTY(lcNoteLns),loOgScroll.gfTempName(),lcNoteLns)
lcNoteLns =  IIF(EMPTY(lcNoteLns),IIF(TYPE('lcXMLFileName') <> 'C',loOgScroll.gfTempName(),gfTempName()),lcNoteLns)
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*-- if you don't find temp. file, create it if you have both types of notes.
IF !USED(lcNoteLns) AND llPrntBoth
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON cRecord TAG (lcNoteLns) OF (oAriaApplication.WorkDir+lcNoteLns)
  INDEX ON cRecord TAG (lcNoteLns)
  *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

IF llPrntBoth
  SELECT (lcTempOrd)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [&lcTempOrd,&lcNoteLns]
ENDIF

*-- if we are in case of print both types of notes.

*-- Select Master report file.
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcTempOrd) = lnLastRec ,.T.),.T.)]
lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)

*!E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') = 'C'
  loOGScroll   = oAriaEnvironment.report
ENDIF
*!E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

*C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [Begin]
loogScroll.cCROrientation = 'P'
*C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [End]

*!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [Start]
IF llRpStrOrd
  lcOldAlias = SELECT()
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+oAriaApplication.DefaultCountry+"'",'',"SYCINTTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
  ELSE
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lnRemResult = oariaenvironment.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+oAriaApplication.DefaultCountry+"'",'',"SYCINTTMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF lnRemResult=1
    LOCATE
  ENDIF
  lcBrowFlds= "STORE :h='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Store,oAriaApplication.GetHeaderText("LANG_Soorcn_Store",AHEADERFILE))+"',stName:23:h='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LabelSName,oAriaApplication.GetHeaderText("LANG_LabelSName",AHEADERFILE)) +"',"+;
    "cAddress1 :H='ST '+SycIntTmp.cPart1Lab :R :P=REPLICATE('X',SycIntTmp.nPart1Len),"+;
    "cAddress2 :H='ST '+SycIntTmp.cPart2Lab :R :P=REPLICATE('X',SycIntTmp.nPart2Len),"+;
    "cAddress3 :H='ST '+SycIntTmp.cPart3Lab :R :P=REPLICATE('X',SycIntTmp.nPart3Len),"+;
    "cAddress4 :H='ST '+SycIntTmp.cPart4Lab :R :P=REPLICATE('X',SycIntTmp.nPart4Len),"+;
    "cAddress5 :H='ST '+SycIntTmp.cPart5Lab :R :P=REPLICATE('X',SycIntTmp.nPart5Len)"
  SELECT(lcTmpOrStr)
  SCAN
    lcOrderNUm = ORDER
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
    *IF gfModalGen('QRM32147B32000',;
        'ALERT',lcOrderNUm)=1
    IF IIF(TYPE('lcXMLFileName') <> 'C',gfModalGen('QRM32147B32000',;
            'ALERT',lcOrderNUm)=1,.F.)
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
      SELECT(lcTmpStr)
      CREATE CURSOR (lcStoreTmp) (KEYEXP C(8),STORE C(8))
      SELECT (lcStoreTmp)
      INDEX ON KEYEXP TAG (lcStoreTmp)
      SELECT(lcTmpStr)
      =SEEK(lcOrderNUm)
      SCAN REST WHILE ORDER+STORE = lcOrderNUm
        INSERT INTO (lcStoreTmp) (keyExp,STORE) VALUES (&lcTmpStr..STORE,&lcTmpStr..STORE)
      ENDSCAN
      SELECT(lcTmpStr)
      llContinue = gfBrowse(lcBrowFlds,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Store,oAriaApplication.GetHeaderText("LANG_Soorcn_Store",AHEADERFILE)),lcTmpStr,[']+lcOrderNUm +['],.F.,.F.,.T.,.F.,.F.,.F.,;
        lcStoreTmp,"STORE",.F.,.F.,.F.,.F.,.F.,.F.,lcTmpStr)

      SELECT(lcTmpStr)
      =SEEK(lcOrderNUm)
      SCAN REST WHILE ORDER+STORE = lcOrderNUm
        IF !SEEK(STORE,lcStoreTmp)
          REPLACE lSelect WITH .F.
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
  lcRpExp = lcRpExp + IIF(!EMPTY(lcRpExp),' AND ','')+"IIF(ordhdr.multi = 'Y',Seek(&lcTempOrd..ORDER+&lcTempOrd..STORE,lcTmpStr) AND &lcTmpStr..lSelect,.T.)"
  SELECT(lcOldAlias)
ENDIF
*!B609202,1 MMT 04/07/10 Add option to print multi store order Store by Store [End]


*Add the optional program function to be used in getting anything special not found in this program(soorcn).
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') = 'C'
  IF FILE(oAriaEnvironment.clientreporthome+"SO\"+lcPrgName+'.FXP')
    =lfOptProg(oAriaEnvironment.clientreporthome+"SO\"+lcPrgName)
  ELSE
    IF FILE(oAriaEnvironment.ReportHome+"SO\"+lcPrgName+'.FXP')
      =lfOptProg(oAriaEnvironment.ReportHome+"SO\"+lcPrgName)
    ENDIF
  ENDIF
ELSE
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  =lfOptProg()
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*--Dont print some fields incase of cons. order
IF llMargFrm
  =lfUsrVldFn('lfDntPrn','SOORCNMO')
ENDIF

IF lcRpOrdTyp <> "T" OR !llRcpOnly &&Do not print order confirmation in case of print recap only
  IF llIsAparel
    DO EVAL('lcPrgName')
  ELSE
    lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
    *Check if print the report from the main program or the optional program.
    IF llSalsOrd
      *hma
      *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [Begin]
      *loogScroll.cCROrientation = 'P'
      *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [End]
      *hma
      *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
      IF TYPE('lcXMLFileName') <> 'C'
      *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[ENd]
        DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
      *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
      ELSE
        SELECT ORDHDR
        Set FILTER TO &lcRpExp
        LOCATE
        IF EOF()
          Set FILTER TO
          RETURN
        ENDIF
        loProgress.Percent = 0.9
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
		*loProgress.Description ="Printing Report..."&& LANG_Soorcn_Printreport
		loProgress.Description ="Printing Report..."&& LANG_Soorcn_Printreport
		*N000682,1 11/20/2012 MMT Globlization changes[End]

        loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
        PRIVATE loProxy
        *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
        *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
        loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
        *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
        IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
          loOGScroll   = oAriaEnvironment.report
          oAriaEnvironment.report.OGLastForm = lcFormName
          oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
          loProgress.Percent = 1.0
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
		  *loProgress.Description = "Printing Report..."&&LANG_Soorcn_Printreport
	      loProgress.Description = "Printing Report..."&&LANG_Soorcn_Printreport
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
        ENDIF
        Set FILTER TO
      ENDIF
      *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]

    ENDIF
  ENDIF
ENDIF

SELECT ORDHDR
SET RELATION TO

IF lcRpOrdTyp = "T"  && print recap order report always for EDI order
  *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [Begin]
  loogScroll.cCROrientation = 'P'
  *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [End]
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lcRCFrmNam = lfGetForm('RC')
    lcPrgName  = lcRCFrmNam
    llIsAparel = lfIsApparl(@lcPrgName)
    =lfOptProg()
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ELSE
    lcRCFrmNam = oAriaEnvironment.REPORT.GetForm('SOORCN','RC')
    lcPrgName  = lcRCFrmNam
    llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
    IF oAriaEnvironment.multiinst AND  FILE(oAriaEnvironment.clientreporthome+"SO\"+lcPrgName+'.FXP')
      =lfOptProg(oAriaEnvironment.clientreporthome+"SO\"+lcPrgName)
    ELSE
      IF FILE(oAriaEnvironment.ReportHome+"SO\"+lcPrgName+'.FXP')
        =lfOptProg(oAriaEnvironment.ReportHome+"SO\"+lcPrgName)
      ENDIF
    ENDIF
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  SELECT (lcTempRcp)
  SET RELATION TO 'S'+SUBSTR(STYLE,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
  SET RELATION TO 'S' + SCALE INTO SCALE ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SET RELATION TO 'M'+Account INTO Customer ADDITIVE
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
    lcOldForm = lcOGTmpForm
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ELSE
    lcOldForm = oAriaEnvironment.report.lcOGTmpForm
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF llIsAparel
    DO EVAL('lcPrgName')
  ELSE
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
    IF TYPE('lcXMLFileName') <> 'C'
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
      lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),loOgScroll.gfTempName(),lcOgTmpForm)
      =gfCrtFrm(lcRCFrmNam,'',llOGRefForm)  && Create Temp. file for new form.
      SELECT (lcTempRcp)
      GO TOP
      *hma
      *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [Begin]
      *loogScroll.cCROrientation = 'P'
      *C200810,1 WLD 07/09/2007 fix bug that Sandard Orientation overwrite Custom Form Orientation [End]
      *hma
      DO gfDispRe WITH EVAL('lcRCFrmNam')
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
    ELSE
       lcOgTmpForm = IIF(EMPTY(oAriaEnvironment.report.lcOgTmpForm),gfTempName(),oAriaEnvironment.report.lcOgTmpForm)
       llOGRefForm = .F.
       =gfCrtFrm(lcRCFrmNam,'',llOGRefForm)  && Create Temp. file for new form.
       SELECT (lcTempRcp)
       GO TOP
       loProgress.Percent = 0.9
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
	   *loProgress.Description = LANG_Soorcn_Printreport
	   loProgress.Description = LANG_Soorcn_Printreport
	   *N000682,1 11/20/2012 MMT Globlization changes[End]

       loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
       PRIVATE loProxy
       *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
       *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
       loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
       *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
       IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
         loOGScroll   = oAriaEnvironment.report
         oAriaEnvironment.report.lAdditive = .T.
         oAriaEnvironment.report.OGLastForm = lcRCFrmNam
         oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
         loProgress.Percent = 1.0
         *N000682,1 11/20/2012 MMT Globlization changes[Start]
		 *loProgress.Description = LANG_Soorcn_Printreport
		 loProgress.Description = LANG_Soorcn_Printreport
		 *N000682,1 11/20/2012 MMT Globlization changes[End]

         loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
       ENDIF
    ENDIF
    *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  ENDIF
  lcOGTmpForm = lcOldForm
ENDIF

IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (oAriaApplication.WorkDir+lcTempOrd+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTempOrd+'.CDX')
ENDIF
*--Return false if no records to Display &it was Crystal Report
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF loOgScroll.llCrystal .AND. llNoRec=.T.
    RETURN .F.
  ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
*-- End of Report code.
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[Start]
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
IF !(lcRpOrdTyp = 'T' AND llRcpOnly)
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF loOgScroll.ll2Printer
    SELECT (lcOrdPrtUp)
    LOCATE
    SCAN
      SELECT ORDHDR
      =SEEK(EVAL(lcOrdPrtUp +'.CORDTYPE')+EVAL(lcOrdPrtUp +'.ORDER'),'ORDHDR')
      REPLACE PrtFlag WITH 'P'
    ENDSCAN
    loOGScroll.llPrinted = .F.
  ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
    SELECT (lcOrdPrtUp)
    LOCATE
    SCAN
      SELECT ORDHDR
      =SEEK(EVAL(lcOrdPrtUp +'.CORDTYPE')+EVAL(lcOrdPrtUp +'.ORDER'),'ORDHDR')
      REPLACE PrtFlag WITH 'P'
    ENDSCAN
    SELECT ORDHDR
    =gfTableUpdate()
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
  IF USED(lcOrdPrtUp)
    USE IN (lcOrdPrtUp)
  ENDIF
ENDIF
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[End]

*-- Function section
*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to Save the company logo in temp. file
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : loOgScroll.gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[END]
  lcLogoPic = loOgScroll.gfTempName()
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
ELSE
  lcLogoPic = gfTempName()
ENDIF
*!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[Start]
*!*	SELECT gobject;
*!*	 FROM Objects         ;
*!*	 WHERE Objects.cobject_id = lcObj_Id ;
*!*	 INTO CURSOR (lcLogoPic)
SELECT mimgpath;
  FROM OBJECTS         ;
  WHERE OBJECTS.cobject_id = lcObj_Id ;
  INTO CURSOR (lcLogoPic)
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[End]
*-- End of lfGetLogo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
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
*-- End of lfAdrShift.

*!*************************************************************
*! Name       : lfHeadVar
*! Developer  : Heba Mohamed Amin (HMA)
*! Date       : 06/23/2004
*! Purpose    : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : SORDCONA.FRX [Header Band]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSolSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- End of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr

lnSavAlias = SELECT(0)
lcStore = &lcTempOrd..STORE

*:B131798,1 AYM 04/15/2006  Order confirmation is hanged with multi store order...[Begin]
*!*= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lnrecord=RECNO(lcTempOrd)
= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

*:B608070,1 MMT 05/02/2007 fix bug of error while preview EDI Temp Orders [Start]
IF BETWEEN(lnrecord,1,RECCOUNT(lcTempOrd))
  *:B608070,1 MMT 05/02/2007 fix bug of error while preview EDI Temp Orders [End]

  GO lnrecord IN (lcTempOrd)

  *:B608070,1 MMT 05/02/2007 fix bug of error while preview EDI Temp Orders [Start]
ENDIF
*:B608070,1 MMT 05/02/2007 fix bug of error while preview EDI Temp Orders [End]

*:B131798,1 AYM 04/15/2006  Order confirmation is hanged with store order...[END]

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..STORE , "CUSTOMER")
  IF ALLTRIM(ORDHDR.ShipVia) = '*'  && Get ShipVia at store level
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
  *IF !EMPTY(CUSTOMER.DIST_CTR)  && Print distribution center address if found.
  IF !EMPTY(CUSTOMER.DIST_CTR) AND !ORDHDR.lStrDirct && Print distribution center address if found.
    *N000592,1 HBG [End]
    lcCurrKey = 'S' + Customer.Account + Customer.STORE
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
	*lcStore = lcStore + LANG_Soorcn_DistCenter + Customer.STORE
	lcStore = lcStore + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_DistCenter,oAriaApplication.GetHeaderText("LANG_Soorcn_DistCenter",AHEADERFILE)) + Customer.STORE
	*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
ENDIF    && End of IF

= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.

SELECT (lnSavAlias)

*-- End of lfSolSpAdr.


*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to fill the approparate Note data for report Notes.
*!           : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

*Get the notes title only if we will print a notes

lcNotes    = lfNoteData()     && Note Data.

IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
ELSE
  lcTitle    =""
ENDIF

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- End of lfGetNotes.

*!*************************************************************
*! Name        : lfNoteHead
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteHead()
*!*************************************************************
FUNCTION lfNoteHead
lcNoteHead = ''
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
	  *lcNoteHead = LANG_Soorcn_Line + ALLTRIM(STR(&lcTempOrd..LINENO)) +' '+LANG_Soorcn_NotePad
  	lcNoteHead = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Line,oAriaApplication.GetHeaderText("LANG_Soorcn_Line",AHEADERFILE)) + ALLTRIM(STR(&lcTempOrd..LINENO)) +' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_NotePad,oAriaApplication.GetHeaderText("LANG_Soorcn_NotePad",AHEADERFILE))
	  *N000682,1 11/20/2012 MMT Globlization changes[End]


    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+ORDER,'NOTEPAD') AND ;
        !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
      IIF(OrdHdr.cOrdType='O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OrdNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_OrdNotPad",AHEADERFILE)),IIF(OrdHdr.cOrdType='T',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_EdiTmpNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_EdiTmpNotPad",AHEADERFILE)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_ContNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_ContNotPad",AHEADERFILE)))),'')
    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
    CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
	  *lcNoteHead = LANG_Soorcn_Line + ALLTRIM(STR(&lcTempOrd..LINENO)) + LANG_Soorcn_NotePad
	  lcNoteHead = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Line,oAriaApplication.GetHeaderText("LANG_Soorcn_Line",AHEADERFILE)) + ALLTRIM(STR(&lcTempOrd..LINENO)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_NotePad,oAriaApplication.GetHeaderText("LANG_Soorcn_NotePad",AHEADERFILE))
      *N000682,1 11/20/2012 MMT Globlization changes[End]


    CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+ORDER,'NOTEPAD') AND ;
        !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
	  IIF(OrdHdr.cOrdType='O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OrdNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_OrdNotPad",AHEADERFILE)),IIF(OrdHdr.cOrdType='T',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_EdiTmpNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_EdiTmpNotPad",AHEADERFILE)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_ContNotPad,oAriaApplication.GetHeaderText("LANG_Soorcn_ContNotPad",AHEADERFILE)))),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- End of lfNoteHead

*!*************************************************************
*! Name      : lfNoteData
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to fill the approparate Note Data Field in report.
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteData()
*!*************************************************************
FUNCTION lfNoteData
lcNoteData  = ''
lcPrntNote = '' && Define Variable To Hold Line Notes.

IF ORDHDR.LastLine > 0  && If you have order lines.
  IF llPrntBoth && If you print both notes.
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
    CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      FOR lnNotLine = 1 TO MEMLINES(&lcTempOrd..Note_Mem)
        lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
        IF LEFT(lcCurrLine,1) # '*'
          lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
        ENDIF
      ENDFOR
      lcNoteData  = lcPrntNote

    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+ORDER,'NOTEPAD') AND ;
        !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      FOR lnNotLine = 1 TO MEMLINES(NOTEPAD.MNOTES)
        lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
        *B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][Start]
        *IF LEFT(lcCurrLine,1) # '*'
        *B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][ENd]
        lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
        *B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][Start]  
        *ENDIF
        *B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][END]
      ENDFOR
      lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
    CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
      FOR lnNotLine = 1 TO MEMLINES(&lcTempOrd..Note_Mem)
        lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
        IF LEFT(lcCurrLine,1) # '*'
          lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
        ENDIF
      ENDFOR
      lcNoteData  = lcPrntNote

    CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+ORDER,'NOTEPAD') AND ;
        !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      FOR lnNotLine = 1 TO MEMLINES(NOTEPAD.MNOTES)
        lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
        *!B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][Start]
        *IF LEFT(lcCurrLine,1) # '*'
        *!B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][End]
        lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
        *!B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][Start]  
        *ENDIF
        *!B610582,1 MMT 11/12/2013 Order confirmation does not print notes lines starts with * [T20131104.0019][End]
      ENDFOR
      lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')

    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- End of lfNoteData.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to Update the End of Group flag
*!*************************************************************
*! Called from : SORDCONA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup

llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[Start]
INSERT INTO (lcOrdPrtUp) (ORDER,CORDTYPE) VALUES (ORDHDR.ORDER,ORDHDR.CORDTYPE)
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[ENd]
RETURN '    '
*-- End of lfEndGroup.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modifications     :E127836,1
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lnPos1


*--IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  llRpOrdLnt = .F.
  LNPOS1 = ASCAN(loOGScroll.laOGObjType,'LLRPORDLNT')
  IF lnPos1 > 0
    lnPos1= ASUBSCRIPT(loOGScroll.laOGObjType,lnPos1,1)
    loOGScroll.laOGObjCnt[lnPos1] = .F.
    =LFOGSHOWGET('LLRPORDLNT')  && Disable the Print order line notepad Object
  ENDIF
ENDIF    && End of IF


lnPos = ASCAN(loOGScroll.laOGObjType,'LLRCPONLY')
IF lnPos > 0
  lnPos= ASUBSCRIPT(loOGScroll.laOgObjType,lnPos,1)
  loOGScroll.laOGObjCnt[lnPos] = LCRPORDTYP = "T"
  = lfOGShowGet('llRcpOnly')
ENDIF


*--Check Configuration option in IC Module Setup
lnModule=OCCURS("IC",oAriaApplication.CompanyInstalledModules)
IF lnModule >0
  llConfig = (ALLTRIM(UPPER(gfGetMemVar('M_STYCNFG'))) = 'Y')
ENDIF
*E127836,1 HMA 06/15/2005 use order status as mover screen to enable the user
*to select more than one status. [Begin]
IF EMPTY(laRpSource)
  *Add Feature to allow printing the Bid status.
  *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
  *DECLARE laRpSource[3],laRpTarget[3]  && Redeclare the source and target arrays.
  DECLARE laRpSource[4],laRpTarget[4]  && Redeclare the source and target arrays.
  *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
  *Redeclare the source and arrays with new status Bid.[Begin]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_Soorcn_Open      TO laRpSource[1],laRpTarget[1]
*STORE LANG_Soorcn_Hold      TO laRpSource[2],laRpTarget[2]
*STORE LANG_Soorcn_Bid       TO laRpSource[3],laRpTarget[3]
*STORE LANG_Soorcn_Complete       TO laRpSource[4],laRpTarget[4]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Open,oAriaApplication.GetHeaderText("LANG_Soorcn_Open",AHEADERFILE))      TO laRpSource[1],laRpTarget[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Hold,oAriaApplication.GetHeaderText("LANG_Soorcn_Hold",AHEADERFILE))      TO laRpSource[2],laRpTarget[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Bid,oAriaApplication.GetHeaderText("LANG_Soorcn_Bid",AHEADERFILE))       TO laRpSource[3],laRpTarget[3]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Complete,oAriaApplication.GetHeaderText("LANG_Soorcn_Complete",AHEADERFILE))       TO laRpSource[4],laRpTarget[4]
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
  *Redeclare the source and arrays with new status Bid. [End]

ENDIF
*E127836,1 HMA 06/15/2005 use order status as mover screen to enable the user
*to select more than one status. [End]




SET ORDER TO ORDHDR IN ORDHDR  && To use it to validate orders in option grid.
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- End of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
*-- End of lfwOldVal.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder

*C121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]
*!*	IF gfDoTriger('SOORCN',PADR('COMPELET',10))
*!*	  RETURN
*!*	ENDIF
*C121868,1 BWA 04/28/2004.[END]

PRIVATE lcVar , lcObj , laTemp

lcVar = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpOrdTyp = 'A'  ,;
    SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') ,;
    SEEK(lcRpOrdTyp+lcObj , 'ORDHDR')))

  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value


  lcBrFields = "CTYPE=IIF(cOrdType='C','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Contract,oAriaApplication.GetHeaderText("LANG_Soorcn_Contract",AHEADERFILE))+;
    "',IIF(cOrdType='O','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Order,oAriaApplication.GetHeaderText("LANG_Soorcn_Order",AHEADERFILE))+;
    "',IIF(cOrdType='T','"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_EDITemp,oAriaApplication.GetHeaderText("LANG_Soorcn_EDITemp",AHEADERFILE))+"','')))"+;
    ":R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OrdType,oAriaApplication.GetHeaderText("LANG_Soorcn_OrdType",AHEADERFILE))+"' :20, "          +;
    "ORDER     :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OrdNum,oAriaApplication.GetHeaderText("LANG_Soorcn_OrdNum",AHEADERFILE))+"' , "   +;
    "ACCOUNT   :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Account,oAriaApplication.GetHeaderText("LANG_Soorcn_Account",AHEADERFILE))+"' ,"    +;
    "STORE     :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Store,oAriaApplication.GetHeaderText("LANG_Soorcn_Store",AHEADERFILE))+"' ,"      +;
    "ENTERED   :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_EntDate,oAriaApplication.GetHeaderText("LANG_Soorcn_EntDate",AHEADERFILE))+"',"+;
    "SEASON    :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Season,oAriaApplication.GetHeaderText("LANG_Soorcn_Season",AHEADERFILE))+"' ,"     +;
    "cDIVISION :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Division,oAriaApplication.GetHeaderText("LANG_Soorcn_Division",AHEADERFILE))+"' ,"   +;
    "CTERMCODE :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Terms,oAriaApplication.GetHeaderText("LANG_Soorcn_Terms",AHEADERFILE))+"' ,"      +;
    "SHIPVIA   :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_ShipVia,oAriaApplication.GetHeaderText("LANG_Soorcn_ShipVia",AHEADERFILE))+"'  ,"    +;
    "STATUS    :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Status,oAriaApplication.GetHeaderText("LANG_Soorcn_Status",AHEADERFILE))+"' ,"    +;
    "OPEN      :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OpenAmt,oAriaApplication.GetHeaderText("LANG_Soorcn_OpenAmt",AHEADERFILE))+"' ,"  +;
	"BULK      :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Bulk,oAriaApplication.GetHeaderText("LANG_Soorcn_Bulk",AHEADERFILE))+"'"

  lcFile_Ttl = IIF(lcRpOrdTyp = 'A',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_OrdOrCont,oAriaApplication.GetHeaderText("LANG_Soorcn_OrdOrCont",AHEADERFILE)),IIF(lcRpOrdTyp='O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Orders,oAriaApplication.GetHeaderText("LANG_Soorcn_Orders",AHEADERFILE)),;
			   IIF(lcRpOrdTyp='T',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_EDITmpOrd,oAriaApplication.GetHeaderText("LANG_Soorcn_EDITmpOrd",AHEADERFILE)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Contracts,oAriaApplication.GetHeaderText("LANG_Soorcn_Contracts",AHEADERFILE)))))
  lcBrowCond = [FOR (IIF(lcRpOrdSta = 'A' , !(ORDHDR.STATUS $ "XC") , ;
               ORDHDR.STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
               ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >= 1) AND ] + ;
    [ IIF(lcRpOrdTyp = "A",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
    [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E' ,                ORDHDR.LEDIORDER,.T.)))]

  = gfBrows(lcBrowCond,'ORDER','laTemp')


  IF !EMPTY(laTemp[1]) && IF The user selected a record
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF

ENDIF    && End of IF

&lcVar = lcObj      && Update the field

*-- End of lfvOrder.

*!*************************************************************
*! Name      : lfAprvFlag
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function that control appearance of
*!           : Approval objects in SORDCONA.FRX
*!*************************************************************
*! Called from : [Option Grid] "Approval Object" in Header Band.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfAprvFlag()
*!*************************************************************
FUNCTION lfAprvFlag

llAprvFlag = !EMPTY(OrdHdr.Approval)
RETURN llAprvFlag
*-- End of lfAprvFlag.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Called from : [SORDCONA.FRX, ORDER GROUP HEADER BAND]
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec
PRIVATE lcThAlias,lnThRec,lcThStore

lcThAlias = ALIAS()           && Save Current Alias.

SELECT (lcTempOrd)
lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = STORE
LOCATE REST FOR ( cordtype+ORDER+STORE+STYLE+STR(LINENO,6) > OrdHdr.cordtype + OrdHdr.ORDER + lcThStore)

IF (ORDER != OrdHdr.ORDER) OR (STORE != lcThStore)
  SKIP -1
ENDIF

lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF

SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*-- End of lfLastRec.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
*-- Close temp. opended files, if it used.
IF USED(lcLogoPic)
  USE IN (lcLogoPic)
ENDIF

IF USED(lcNoteLns)
  USE IN (lcNoteLns)
ENDIF
*-- End of lfClearRep.

*!*************************************************************
*! Name      : lfsChOrder
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsChOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
*! Modifications     :E127836,1
*!*************************************************************
FUNCTION lfsChOrder
PARAMETERS lcParm

*C121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]
*IF gfDoTriger('SOORCN',PADR('SETRESET',10))
*  RETURN
*ENDIF
*C121868,1 BWA 04/28/2004.[END]


DO CASE
CASE lcParm = 'S'

  SELECT ORDHDR
  * E127836,1 HMA 06/15/2005 use order status as mover screen to enable the user
  *                to select more than one status.[Begin]
  *!*      DO CASE
  *!*        CASE lcRpOrdTyp = "A"
  *!*          SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
  *!*          STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
  *!*          ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*          LOCATE

  *!*        CASE lcRpOrdTyp = "O"
  *!*          SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
  *!*          STATUS = lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*          LOCATE FOR CORDTYPE+ORDER = "O"

  *!*        CASE lcRpOrdTyp = "C"
  *!*          SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
  *!*          STATUS = lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*          LOCATE FOR CORDTYPE+ORDER = "C"

  *!*        CASE lcRpOrdTyp = "T"
  *!*          SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
  *!*          STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
  *!*          ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*          LOCATE FOR CORDTYPE+ORDER = "T"

  *!*      ENDCASE
  DO CASE
  CASE lcRpOrdTyp = "A"
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
    *!*          SET FILTER TO IIF(EMPTY(lcRpOrdSta),!(ORDHDR.STATUS $ "XC")  ,;
    *!*          STATUS $ lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
    *!*          ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
    *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    SET FILTER TO IIF(EMPTY(lcRpOrdSta),!(ORDHDR.STATUS = "X")  ,;
      STATUS $ lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
      ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
      IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
    LOCATE

  CASE lcRpOrdTyp = "O"
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
    *!*          SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS $ "XC")  ,;
    *!*          STATUS $ lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
    *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS ="X")  ,;
      STATUS $ lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
      IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
    LOCATE FOR CORDTYPE+ORDER = "O"

  CASE lcRpOrdTyp = "C"
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
    *!*          SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS $ "XC")  ,;
    *!*          STATUS $ lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
    *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS = "X")  ,;
      STATUS $ lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
      IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
    LOCATE FOR CORDTYPE+ORDER = "C"

  CASE lcRpOrdTyp = "T"
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
    *!*          SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS $ "XC")  ,;
    *!*          STATUS $ lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
    *!*          ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
    *!*          IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(EMPTY(lcRpOrdSta), !(ORDHDR.STATUS = "X")  ,;
      STATUS $ lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
      ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
      IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
    LOCATE FOR CORDTYPE+ORDER = "T"

  ENDCASE
  * E127836,1 HMA 06/15/2005 use order status as mover screen to enable the user
  *                to select more than one status.[End]
CASE lcParm = 'R'

  SELECT ORDHDR
  SET FILTER TO
  llClearSel = .F.
ENDCASE
*-- End of lfsChOrder.


*!*************************************************************
*! Name      : lfvType
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Transaction Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
*
FUNCTION lfvType

IF lcRpOrdTyp $ "TC"
  lcRpEDIFlt = "B"
ELSE
  IF (laOldVal = 'T') .OR. (laOldVal = 'C')
    lcRpEDIFlt = "B"
  ENDIF
ENDIF
ClearRead()
llClearSel = .T.
*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogVrflt,'ORDHDR.ORDER')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laogVrflt,lnOrdPos,1)
  lcOrders = loOGScroll.laogVrflt[lnOrdPos,6]
  IF !EMPTY(lcOrders)
    IF USED(lcOrders)
      SELECT(lcOrders)
      ZAP
    ENDIF
  ENDIF
ENDIF

*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line
*in the Option grid if the order status or order type has changed. [End]
*-- End of lfvType.


*!*************************************************************
*! Name      : lfModeVld
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Report Mode Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfModeVld()
*!*************************************************************

FUNCTION lfModeVld

ClearRead()
*-- End of lfModeVld.

*!*************************************************************
*! Name      : lfvOrdSta
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Order Status Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvOrdSta()
*!*************************************************************

FUNCTION lfvOrdSta

llClearSel = .T.
*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogVrflt,'ORDHDR.ORDER')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laogVrflt,lnOrdPos,1)
  lcOrders = loOGScroll.laogVrflt[lnOrdPos,6]
  IF !EMPTY(lcOrders)
    IF USED(lcOrders)
      SELECT(lcOrders)
      ZAP
    ENDIF
  ENDIF
ENDIF

*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line
*in the Option grid if the order status or order type has changed. [End]
*-- End of lfvOrdSta.

*!*************************************************************
*! Name      : lfGetSt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Get EDI Status
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSt()
*!*************************************************************
FUNCTION lfGetSt

PRIVATE lcRetStat
lcRetStat = SPACE(0)
IF EVAL(lcTempOrd +'.cOrdType')#'T' .OR. ORDHDR.MON_FLG # 'G'
  RETURN ''
ENDIF

DO CASE
CASE EVAL(lcTempOrd +'.CLINESTAT')='AI'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_NewItem
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_NewItem,oAriaApplication.GetHeaderText("LANG_Soorcn_NewItem",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE EVAL(lcTempOrd +'.CLINESTAT')='DI'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_DelItem
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_DelItem,oAriaApplication.GetHeaderText("LANG_Soorcn_DelItem",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE EVAL(lcTempOrd +'.CLINESTAT')='QD'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_QtyDecreased
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_QtyDecreased,oAriaApplication.GetHeaderText("LANG_Soorcn_QtyDecreased",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE EVAL(lcTempOrd +'.CLINESTAT')='QI'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_QtyIncreased
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_QtyIncreased,oAriaApplication.GetHeaderText("LANG_Soorcn_QtyIncreased",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE EVAL(lcTempOrd +'.CLINESTAT')='PC'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_PrcChange
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_PrcChange,oAriaApplication.GetHeaderText("LANG_Soorcn_PrcChange",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

CASE EVAL(lcTempOrd +'.CLINESTAT')='PQ'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcRetStat = LANG_Soorcn_QtyChange
lcRetStat = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_QtyChange,oAriaApplication.GetHeaderText("LANG_Soorcn_QtyChange",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE

RETURN lcRetStat

*-- End of lfGetSt.

*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************


FUNCTION lfSortDumy

IF RIGHT(lcFormName,2) ='GM'
  DIMENSION laSortDesc[3,1] , laSortVal[3,1]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1] = LANG_Soorcn_Style
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Style,oAriaApplication.GetHeaderText("LANG_Soorcn_Style",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2] = LANG_Soorcn_LineNo
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_LineNo,oAriaApplication.GetHeaderText("LANG_Soorcn_LineNo",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[3] = LANG_Soorcn_Pack
laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Pack,oAriaApplication.GetHeaderText("LANG_Soorcn_Pack",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  laSortVal[1] = 'S'
  laSortVal[2] = 'L'
  laSortVal[3] = 'P'
ELSE

  *C102680,1 RAE 08/27/2002 Add new sort option (Location) to the Sort filter. [Begin]
  *IF gfDoTriger('SOORCN',PADR('SRTBYLOC',10))

  *ELSE
  *C102680,1 RAE 08/27/2002 Add new sort option (Location) to the Sort filter. [END]
  DIMENSION laSortDesc[2,1] , laSortVal[2,1]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1] = LANG_Soorcn_Style
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Style,oAriaApplication.GetHeaderText("LANG_Soorcn_Style",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2] = LANG_Soorcn_LineNo
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_LineNo,oAriaApplication.GetHeaderText("LANG_Soorcn_LineNo",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  laSortVal[1] = 'S'
  laSortVal[2] = 'L'
  *ENDIF

ENDIF

*-- End of lfSortDumy.

*!*************************************************************
*! Name      : lfvPrntDtl
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/23/2004
*! Purpose   : Change the sort by option to Pack when Print
*!             Pack Detail is "No".
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrntDtl()
*!*************************************************************

FUNCTION lfvPrntDtl

IF RIGHT(lcFormName,2) ='GM'
  IF !llRpPrTPak
    lcRpSortBy = 'P'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1] = LANG_Soorcn_Pack
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Pack,oAriaApplication.GetHeaderText("LANG_Soorcn_Pack",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2] = LANG_Soorcn_LineNo
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_LineNo,oAriaApplication.GetHeaderText("LANG_Soorcn_LineNo",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[3] = LANG_Soorcn_Style
laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Style,oAriaApplication.GetHeaderText("LANG_Soorcn_Style",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


    laSortVal[1] = 'P'
    laSortVal[2] = 'L'
    laSortVal[3] = 'S'
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfSlctFox
*! Developer : Heba Mohamed Amin	(HMA)
*! Date      : 11/01/2004
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example   : =lfSlctFox()
*!*************************************************************

FUNCTION lfSlctFox
PARAMETERS  lcSelFields ,lcTable ,lcWherecond, lcorder ,lccursor

=lfSlctFox('*',lcOrdTable,"",lcOrdHdr,'ORDHDR','LCORDHDR')

IF !EMPTY(lcWhereCond)
  SELECT &lcSelFields  FROM  &lcTable WHERE &lcWherecond INTO CURSOR &lccursor
ELSE
  SELECT &lcSelFields  FROM  &lcTable  INTO CURSOR &lccursor
ENDIF

SELECT &lccursor
SET ORDER TO &lcOrder


*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/15/2005
*! Purpose   : - Evaluate Status expression.
*!           :Due to E127836,1
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpOrdSta  && Save old status value.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,LANG_Soorcn_SelectOrdStat,.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_SelectOrdStat,oAriaApplication.GetHeaderText("LANG_Soorcn_SelectOrdStat",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]


lcRpOrdSta = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
    *lcRpOrdSta = lcRpOrdSta + IIF(laRpTarget[lnI] = LANG_Soorcn_Open,'O',;
    IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Hold,oAriaApplication.GetHeaderText("LANG_Soorcn_Hold",AHEADERFILE)),'H',;
	 IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Bid,oAriaApplication.GetHeaderText("LANG_Soorcn_Bid",AHEADERFILE)),'B','')))
    lcRpOrdSta = lcRpOrdSta + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Open,oAriaApplication.GetHeaderText("LANG_Soorcn_Open",AHEADERFILE)),'O',;
      IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Hold,oAriaApplication.GetHeaderText("LANG_Soorcn_Hold",AHEADERFILE)),'H',;
	  IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Bid,oAriaApplication.GetHeaderText("LANG_Soorcn_Bid",AHEADERFILE)),'B',IIF(laRpTarget[lnI] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soorcn_Complete,oAriaApplication.GetHeaderText("LANG_Soorcn_Complete",AHEADERFILE)) ,'C',''))))
    *!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
  ENDFOR
ENDIF
*!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[Start]
*lcRpOrdSta = IIF(EMPTY(lcRpOrdSta),'OHB',ALLTRIM(lcRpOrdSta))
lcRpOrdSta = IIF(EMPTY(lcRpOrdSta),'OHBC',ALLTRIM(lcRpOrdSta))
*!B609258,1 MMT 05/19/2010 Allow report to print completed Orders[End]
*--delete the selected records from Order # Line
*in the Option grid if the order status or order type has changed.
lnOrdPos = ASCAN(loOGScroll.laogVrflt,'ORDHDR.ORDER')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laogVrflt,lnOrdPos,1)
  lcOrders = loOGScroll.laogVrflt[lnOrdPos,6]
  IF !EMPTY(lcOrders)
    IF USED(lcOrders)
      SELECT(lcOrders)
      ZAP
    ENDIF
  ENDIF
ENDIF

llClearSel = .T.
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/15/2005
*! Purpose   : -Return the selected status in the ReadBox
*!             -Due to E127836,1
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************


FUNCTION RefreshStatus
LOCAL lcStatusStr, lnTarget
lcStatusStr = ""
IF !EMPTY(laRpTarget)
  FOR lnTarget = 1 TO ALEN(laRpTarget,1)
    lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
  ENDFOR
  lcStatusStr = SUBSTR(lcStatusStr,3)
ENDIF
RETURN lcStatusStr
ENDFUNC

*-- end of RefreshStatus.

*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[Start]
*!*************************************************************
*! Name      : lfUpdate
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/17/2011
*! Purpose   : CHECK IF Order PRINTED
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfUpdate

IF SYS(2040)='2'
  llPrinter = .T.
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  IF  TYPE('lcXMLFileName') <> 'C'
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[END]
    loOgScroll.ll2Printer=.T.
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[Start]
  ENDIF
  *!E303292,1 MMT 11/07/2012 Enhance Order confirmation report to work from request builder[End]
ENDIF
RETURN .T.
*!E302898,1 MMT 05/17/2011 add Filter to Order confirmation report on Print Flag[End]


*:E303489,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81][Start]
**!**************************************************************************
*! Name      : lfSRPackId
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/06/2014
*! Purpose   : Go top in the Spck_Hdr file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSRPackId()
*!**************************************************************************
FUNCTION lfSRPackId
PARAMETERS OpGrdParm
DO CASE
  CASE  OpGrdParm='S'       
    IF !USED('SPCK_HDR')
      =gfOpenTable('SPCK_HDR','SPCK_HDRST','SH')
    ENDIF
    lcTmpIndex =gfTempName()
    SELECT  Pack_Id,cPkColor,cPckSize,cPkVersion FROM SPCK_HDR INTO DBF (gcWorkDir + lcTmpIndex)
    gfCloseTable('SPCK_HDR')
    use in (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS SPCK_HDR EXCLUSIVE 
    SELECT SPCK_HDR
    INDEX ON Pack_Id+cPkColor+cPckSize+cPkVersion TAG SPCK_HDRVR UNIQUE
    GO TOP
  CASE  OpGrdParm='R'
    USE IN SPCK_HDR
    *=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDRVR','SH')
    =gfOpenTable('SPCK_HDR','SPCK_HDRST','SH')
ENDCASE


*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : Mariam Mazhar(MMT)
*! Date      : 07/06/2014
*! Purpose   : Function To get Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************
FUNCTION lfGetGmSz
PARAMETER lcPackSize

lnAlias = SELECT(0)
PRIVATE lcNombr,lnScalePos
IF !EMPTY(lcPackSize)
  *--svae Scale Positon
  lnScalePos = RECNO('SCALE')
  =gfSEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcSize=EVAL('SCALE.SZ'+lcNombr)
  IF BETWEEN(lnScalePos,1,RECCOUNT('SCALE'))
    GOTO lnScalePos In SCALE
  ENDIF
ELSE
  lcSize ='*****'
ENDIF  

SELECT(lnAlias)
RETURN lcSize 
*:E303489,1 MMT 07/06/2014 Add Pack ID filter to the Order confirmation Report Option Grid[T20131219.0020 - Issue#81][End]