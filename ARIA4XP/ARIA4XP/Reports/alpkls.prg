****************************************************************************
*: Program file  : ALPKLS.PRG --- ALPKLSA.FRX
*: Program desc. : Print Packing List
*: System        : Aria Apparel System (Aria4XP).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : Mariam Mazhar - (MMT) Due to issue #037543
*: Date          : 12/14/2004
*:**************************************************************************
*: Calls : FUNCTIONS  : lfWOgWhen  ,lfWorkFile ,lfSRInv    ,lfAdrShift
*:                    : lfCollect  ,lfCollTime ,lfHeadVar  ,lfGrpSetes  
*:                    : lfChngScle ,lfStyMasks ,lfEvlMajNn ,lfNonMjDes
*:                    : lfSpckln   ,lfEndGroup ,lfScalHead ,lfClearRep.
*:                    : gfDispRe    ..........
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*! Modification:
*! B127377,1 MMT 04/13/2005 , FIX BUG OF NOT PRINTING SHIP TO ADDRESS
*! B128185,1 MMT 05/25/05 ,fix bug of wrong packing list format[Start]
*! B132411,1 MMT 11/13/2006 T20060709.0009
*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY
*! B607990,1 HIA 02/20/2007 increase the bill to address to 6 attributes
*! N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR
*! B607990,1 MMT 03/15/2007 fix error while prview all invoices
*! B608381,1 WAM 12/13/2007 Get the cust PO# from the piktkt file
*! B608560,1 ALA 08/06/2008 to get name of division in temporary file not from ORDHDR master file
*! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[T20101029.0010]
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*! E303464,1 HES 04/23/2014 Add new print flag field in P\L and populate it when P\L's printed [T20140414.0001]
*:**************************************************************************
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientId
 
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
 
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
 
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
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
  lcActiveMod = 'AL'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'AL'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.Report.cCROrientation = 'P' 
ELSE
  loOGScroll.cCROrientation = 'P'
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]

*-- llRpPrtDoz : Print by dozen 'Special Instrutions Code For Dozen'
*-- lcStTime   :Variable to hold the start Time
lcStTime   = TIME()
*--we add record in SYREPUVR for lcFormName 
lcPrgName  = lcFormName
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*llIsAparel = lfIsApparl(@lcPrgName)
IF TYPE('lcXMLFileName') <> 'C'
  llIsAparel = lfIsApparl(@lcPrgName)
ELSE
  STORE '' TO loPack_Hdr, loPack_Lin, loPikTkt, loCustomer, loNotePad, loStyle, loScale
  STORE '' TO loOrdHdr, loOrdLine, loInvLine, loInvHdr, loWareHous, loICSEGVAL, loSpck_Lin
  STORE 0 TO lcBuild
  STORE .F. TO llPrntComp, llFactor, lcPrtAdr
  
  DIMENSION laMajSegs[1,1]  
  lcFormName = oAriaEnvironment.REPORT.GetForm('ALPKLS')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.Isapparell(@lcPrgName)
      
  =lfWOgWhen()
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
lcCompFax = TRANSFORM(&lcCompInfo..cCom_Fax , '@R '+lcPhonPict)  && && Variable hold the Company Fax.
llALPakLst = .T.

*-- Define variables that defined in OG for this report [Begin]
DIMENSION laSpckTemp[8] , laTemp[8] 

STORE ''TO lcColor, lcScale, lcNotes, lcTitle,;
           laSpckTemp, laTemp , lcPackNo,lcLinFile,lcStyleExp

lnGrdTotWg = 0
DO CASE 
  CASE lcRpSelcBy = "I"
    lcLinFile = lcInvLnTmp
  CASE lcRpSelcBy = "P"
    lcLinFile = lcPakLnTmp
  CASE lcRpSelcBy = "T"
    lcLinFile = lcOrdLnTmp
ENDCASE  

llEndGroup = .F.
lnLastRec  = 0

STORE SPACE(16) TO lcPackId 
IF loPack_Hdr.llnative
  SELECT(lcTempPack_Hdr)
  LOCATE 
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
     *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
     *=gfModalGen('TRM00052B00000','DIALOG')
     IF TYPE('lcXMLFileName') <> 'C'
       =gfModalGen('TRM00052B00000','DIALOG')
     ENDIF
     *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
   RETURN
  ENDIF
ENDIF 

*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
IF TYPE('lcXMLFileName') = 'C'
  =gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER')
  =gfOpenTable('NOTEPAD','NOTEPAD','SH','NOTEPAD')
  =gfOpenTable('INVHDR','INVHDR','SH','INVHDR')
  =gfOpenTable('INVLINE','INVLINE','SH','INVLINE')
  =gfOpenTable('PACK_HDR','PACK_HDR','SH','PACK_HDR')
  =gfOpenTable('PACK_LIN','PACK_LIN','SH','PACK_LIN')
  *=gfOpenTable('STYLE','STYLE','SH','STYLE')
  =gfOpenFile('STYLE','STYLE')
  =gfOpenTable('SCALE','SCALE','SH','SCALE')
  =gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
  =gfOpenTable('SPCK_LIN','SPCKLINS','SH','SPCK_LIN')
  =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS')
  =gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK')
  =gfOpenTable('SYCCOMP','CCOMP_ID','SH','SYCCOMP')
  =gfOpenTable('POFHDR','POFHDR','SH','POFHDR')
  =gfOpenTable('ICSEGVAL','SEGVAL','SH','ICSEGVAL')
  =gfOpenTable('ORDLINE','ORDLINST','SH','ORDLINE')
  =gfOpenTable('PIKTKT','PIKTKT','SH','PIKTKT')
  =gfOpenTable('CODES','CODES','SH','CODES')
  =gfOpenTable('WAREHOUS','WAREHOUS','SH','WAREHOUS')
  llOGFltCh = loOGScroll.llOGFltCh
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]

*-- if user change filter criteria then you must collect data again 
IF llOGFltCh

*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [BEGIN]
   DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2] , laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  laCompAdd = ''           && Array to hold the Company address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  *--selecting company information from syccomp remotely[Begin]
  lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
  lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult = 1
    SELECT &lcCompInfo
    lcCompName      = cCom_Name
    lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    DIMENSION laCompAdd[6,1]
    laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [END]

  *-- NotePad File  
  IF lcNotePad   = lcTempNotePad
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcNotePad = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcNotePad = loogScroll.gfTempName()
    ELSE
      lcNotePad = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
    =lfMakeIndex(lcNotePad)
  ELSE 
    IF USED(lcNotePad) 
      USE IN (lcNotePad)
      SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
      =lfMakeIndex(lcNotePad)
    ENDIF  
  ENDIF 
  *--customer file
  IF lcCustomer  = lcTempCustomer
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcCustomer = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcCustomer = loogScroll.gfTempName()
    ELSE
      lcCustomer = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
  ELSE
    IF USED(lcCustomer)
      USE IN (lcCustomer)
    SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
    =lfMakeIndex(lcCustomer)
    ENDIF 
  ENDIF 
  *--OrdHdr File
  IF lcOrdhdr    = lcTempOrdhdr
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcOrdhdr = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcOrdhdr = loogScroll.gfTempName()
    ELSE
      lcOrdhdr = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
    =lfMakeIndex(lcOrdhdr)
  ELSE
    IF USED(lcOrdhdr)
      USE IN (lcOrdhdr)
      SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
      =lfMakeIndex(lcOrdhdr)
    ENDIF 
  ENDIF 
  *--Ordline File
  IF lcOrdLnTmp  = lcTempOrdline
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcOrdLnTmp = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcOrdLnTmp = loogScroll.gfTempName()
    ELSE
      lcOrdLnTmp = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
    =lfMakeIndex(lcOrdLnTmp)
  ELSE
    IF USED(lcOrdLnTmp)
      USE IN (lcOrdLnTmp)
      SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
      =lfMakeIndex(lcOrdLnTmp)
    ENDIF 
  ENDIF 
  *--Invoice line File
  IF   lcInvLnTmp  = lcTempInvLine
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcInvLnTmp = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcInvLnTmp = loogScroll.gfTempName()
    ELSE
      lcInvLnTmp = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
    =lfMakeIndex(lcInvLnTmp)
  ELSE
    IF USED(lcInvLnTmp)
      USE IN (lcInvLnTmp)
      SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
      =lfMakeIndex(lcInvLnTmp)
    ENDIF 
  ENDIF 
  *--style file
  IF lcStyleFile = lcTempStyle
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcStyleFile = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcStyleFile = loogScroll.gfTempName()      
    ELSE
      lcStyleFile = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
    =lfMakeIndex(lcStyleFile)
  ELSE 
    IF USED(lcStyleFile)
      USE IN (lcStyleFile)
      SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
      =lfMakeIndex(lcStyleFile)
    ENDIF 
  ENDIF 
  *--scale file
  IF lcScaleFile = lcTempScale
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcScaleFile = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcScaleFile = loogScroll.gfTempName()
    ELSE
      lcScaleFile = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
    =lfMakeIndex(lcScaleFile)
  ELSE 
    IF USED(lcScaleFile)
      USE IN (lcScaleFile)
      SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
      =lfMakeIndex(lcScaleFile)
    ENDIF 
  ENDIF 
  *--WareHous File
  IF lcWareHous  = lcTempWareHous
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lcWareHous = loogScroll.gfTempName()
    IF TYPE('lcXMLFileName') <> 'C'
      lcWareHous = loogScroll.gfTempName()
    ELSE
      lcWareHous = gfTempName()
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
    =lfMakeIndex(lcWareHous)
  ELSE 
    IF USED(lcWareHous)
      USE IN (lcWareHous)
      SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
      =lfMakeIndex(lcWareHous)
    ENDIF 
  ENDIF 
  *-- Collecting Code...
  IF !USED(lcPackTmp) OR (RECCOUNT(lcPackTmp) > 0)
    =lfWorkFile()
  ENDIF

  =lfCollect()               && Collect Aging data.
  SELECT (lcPackTmp)
  *B608381,1 WAM 12/13/2007 Get the cust PO# from the piktkt file
  *SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO &lcCustomer ,;
                                      "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
                                      "B" + ORDER             INTO &lcNotePad  ,;  
                                      PACK_NO                 INTO (lcPakLnTmp) ,;           
                                      "O" + ORDER             INTO &lcOrdHdr  ,;
                                      invoice                 INTO &lcInvLnTmp ADDITIVE    
  SET RELATION TO IIF(EMPTY(STORE),'M','S') + Account + Store INTO &lcCustomer ,;
                                      "O" + ORDER + STORE     INTO &lcOrdLnTmp,;
                                      "B" + ORDER             INTO &lcNotePad  ,;  
                                      PACK_NO                 INTO (lcPakLnTmp) ,;           
                                      "O" + ORDER             INTO &lcOrdHdr  ,;
                                      invoice                 INTO &lcInvLnTmp ,;
                                      piktkt                  INTO &lcTempPikTkt ADDITIVE
  *B608381,1 WAM 12/13/2007 (End)

ENDIF

*-- Asking if no records (Display message) otherwise print report 
SELECT (lcPackTmp)
IF RECCOUNT(lcPackTmp) = 0
 *---Text : 'No Record Selected for the report..!'
 *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
 *=gfModalGen('TRM00052B00000','DIALOG')
 IF TYPE('lcXMLFileName') <> 'C'
   =gfModalGen('TRM00052B00000','DIALOG')
 ENDIF
 *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
 RETURN
ENDIF

SELECT (lcPackTmp)

SELECT(lcSTYLEfile)
SET RELATION TO  "S" + scale  INTO (lcScaleFile)

SELECT (lcPackTmp)
GOTO TOP

lnMajorPic = LEN(lcMajorPic)  
*-- llUseExtSc : This variable was defined at OG level, to keep track of using 
*--            : extended size scale.
IF llUseExtSc
  lcItemTitl = PADR(lcMajorTlt,lnMajorPic)+'-'+;
               SUBSTR(lcNnMajTlt,1,(lnMajorPic-lnExtScLen))
  lnNMajorPc = SUBSTR(lcNnMajTlt,1,(lnMajorPic-lnExtScLen))
ELSE
  lcItemTitl = PADR(lcMajorTlt,LEN(lcMajorPic)) +;
               PADR(lcNnMajTlt,LEN(lcNnMajPic))
  lnNMajorPc = PADR(lcNnMajTlt,LEN(lcNnMajPic))
ENDIF  

*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*=lfOptProg()
IF TYPE('lcXMLFileName') <> 'C'
  =lfOptProg()
ELSE
  IF !EMPTY(oAriaEnvironment.Report.lcOptProg)
	lcPrgName = oAriaEnvironment.Report.lcOptProg
  ENDIF
  IF oAriaEnvironment.MultiInst AND  FILE(oAriaEnvironment.ClientReportHome+lcPrgName+'.FXP')
    =lfOptProg(oAriaEnvironment.ClientReportHome+lcPrgName)
  ELSE
    IF FILE(oAriaEnvironment.ReportHome+lcPrgName+'.FXP')
      =lfOptProg(oAriaEnvironment.ReportHome+lcPrgName)
	ENDIF
  ENDIF
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.

IF llALPakLst
  *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
  *WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2
  IF TYPE('lcXMLFileName') <> 'C'
    WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2
  ENDIF
  *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
*--  MESSAGEBOX('Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...')
ENDIF
*-- Sort by Carton No. Index
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)

loogScroll.cCROrientation = 'P'


*--We add llNoRec in SYREPUVR.DBF and get initiale value .F. 
IF llIsAparel
  lnOldpLen = _pLength 
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
  _pLength  = lnOldpLen 
ELSE
  IF llALPakLst
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *DO gfDispRe WITH EVAL('lcFormName')
    IF TYPE('lcXMLFileName') <> 'C'
      DO gfDispRe WITH EVAL('lcFormName')
    ELSE
      oAriaEnvironment.Report.cCROrientation = 'P'
      oAriaEnvironment.REPORT.OGLastForm = EVAL('lcFormName')
      loProgress.DESCRIPTION = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      PRIVATE loProxy
      loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
  ENDIF
ENDIF  

*! E303464,1 HES 04/23/2014 Add new print flag field in P\L and populate it when P\L's printed [START]
SET STEP ON 
IF loOgScroll.ll2Printer
  =gfOpenTable('PACK_HDR','PACK_HDR', 'SH', 'PACK_HDR_T')
  SELECT (lcPackTmp)
  SCAN
    SELECT PACK_HDR_T
    IF gfSeek(EVAL(lcPackTmp+'.pack_no'))
      =gfReplace(" PrtFlag WITH 'P'")
    ENDIF
  ENDSCAN
  loOGScroll.llPrinted = .F.
 
  SELECT PACK_HDR_T
  =gfTableUpdate()
ENDIF  
*! E303464,1 HES 04/23/2014 Add new print flag field in P\L and populate it when P\L's printed [END  ]

*!**************************************************************************
*-- Functions and Procedures :
*!**************************************************************************
*! Name      : lfWOgWhen()    
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To Set When running the Option Grid.
*!**************************************************************************
*! Called from : "When" Option Grid.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfWOgWhen()    
*!**************************************************************************
*
FUNCTION lfWOgWhen

*--Pack_Hdr file
IF TYPE('loPack_Hdr') <> 'O'
  loPack_Hdr = CreateObject("RemoteTable","Pack_Hdr","Pack_Hdr",lcTempPack_Hdr,SET("DATASESSION")) 
ENDIF 

*--Pack_lin file
IF TYPE('loPack_Lin') <> 'O'
  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 
ENDIF 

*--Piktkt file
IF TYPE('loPikTkt') <> 'O'
   loPikTkt   = CreateObject("RemoteTable","PikTkt","PikTkt",lcTempPikTkt,SET("DATASESSION"))
ENDIF

*--Customer file
IF TYPE('loCustomer') <> 'O'
  loCustomer = CreateObject("RemoteTable","Customer","Customer",lcTempCustomer,SET("DATASESSION"))  
  SELECT * FROM &lcTempCustomer WHERE .F. INTO CURSOR &lcCustomer READWRITE 
  =lfMakeIndex(lcCustomer)
ENDIF   

*--Notepad file
IF TYPE('loNotePad') <> 'O'
  loNotePad  = CreateObject("RemoteTable","NotePad","NotePad",lcTempNotePad,SET("DATASESSION")) 
  SELECT * FROM &lcTempNotePad WHERE .F. INTO CURSOR &lcNotePad READWRITE 
  =lfMakeIndex(lcNotePad)
ENDIF  

*--Style file
IF TYPE('loStyle') <> 'O' 
  loStyle    = CreateObject("RemoteTable","Style","Style",lcTempStyle,SET("DATASESSION")) 
  SELECT * FROM &lcTempStyle WHERE .F. INTO CURSOR &lcStyleFile READWRITE 
  =lfMakeIndex(lcStyleFile)
ENDIF

*--Scale file
IF TYPE('loScale') <> 'O'
  loScale    = CreateObject("RemoteTable","Scale","Scale",lcTempScale,SET("DATASESSION")) 
  SELECT * FROM &lcTempScale WHERE .F. INTO CURSOR &lcScaleFile READWRITE 
  =lfMakeIndex(lcScaleFile)
ENDIF 
  
*--Ordhdr file  
IF TYPE('loOrdHdr') <> 'O'
  loOrdHdr   = CreateObject("RemoteTable","OrdHdr","OrdHdr",lcTempOrdHdr,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdHdr WHERE .F. INTO CURSOR &lcOrdhdr READWRITE 
  =lfMakeIndex(lcOrdhdr)
ENDIF

*--Ordline file 
IF TYPE('loOrdLine') <> 'O'  
  loOrdLine  = CreateObject("RemoteTable","OrdLine","ORDLINST",lcTempOrdLine,SET("DATASESSION")) 
  SELECT * FROM &lcTempOrdLine WHERE .F. INTO CURSOR &lcOrdLnTmp READWRITE 
  =lfMakeIndex(lcOrdLnTmp)
ENDIF   

*--Invline File
IF TYPE('loInvLine') <> 'O'  
  loInvLine  = CreateObject("RemoteTable","InvLine","InvLine",lcTempInvLine,SET("DATASESSION")) 
  SELECT * FROM &lcTempInvLine WHERE .F. INTO CURSOR &lcInvLnTmp READWRITE 
  =lfMakeIndex(lcInvLnTmp)
ENDIF   

*--Invhdr file
IF TYPE('loInvHdr') <> 'O'  
  loInvHdr  = CreateObject("RemoteTable","InvHdr","InvHdr",lcTempInvHdr,SET("DATASESSION")) 
ENDIF   
 
*--WareHous  file
IF TYPE('loWareHous') <> 'O'  
  loWareHous  = CreateObject("RemoteTable","WareHous","WareHous",lcTempWareHous,SET("DATASESSION")) 
  SELECT * FROM &lcTempWareHous WHERE .F. INTO CURSOR &lcWareHous READWRITE 
  =lfMakeIndex(lcWareHous)
ENDIF   

*--ICSEGVAL file
IF TYPE('loICSEGVAL') <> 'O'  
  loICSEGVAL = CreateObject("RemoteTable","ICSEGVAL","SEGVAL",lcTempICSEGVAL,SET("DATASESSION")) 
ENDIF   

*--Spck_lin file
IF TYPE('loSpck_Lin') <> 'O'  
  loSpck_Lin = CreateObject("RemoteTable","Spck_Lin","SPKLNSTCN",lcTempSpck_Lin,SET("DATASESSION")) 
ENDIF   

IF TYPE('lcBuild') = 'N'
  lcBuild = 'OK'
  SELECT(lcTempPack_Hdr)
  DIMENSION laTempStru[1,18]
  lnFldLen = AFIELDS(laTempStru)
  DIMENSION laTempStru[lnFldLen + 2 , 18]

  laTempStru[lnFldLen + 1 , 1] = 'nRprtTyp'
  laTempStru[lnFldLen + 1 , 2] = 'N'
  laTempStru[lnFldLen + 1 , 3] = 1
  laTempStru[lnFldLen + 1 , 4] = 0

  laTempStru[lnFldLen + 2 , 1] = 'Invoice'
  laTempStru[lnFldLen + 2 , 2] = 'C'
  laTempStru[lnFldLen + 2 , 3] = 6
  laTempStru[lnFldLen + 2 , 4] = 0



 FOR lnLoop = 1 to  2
  STORE ' ' TO  laTempStru[lnFldLen +lnLoop,7],laTempStru[lnFldLen +lnLoop,8],;
                laTempStru[lnFldLen +lnLoop,9],laTempStru[lnFldLen +lnLoop,10],;
                laTempStru[lnFldLen +lnLoop,11],laTempStru[lnFldLen +lnLoop,12],;
                laTempStru[lnFldLen +lnLoop,13],laTempStru[lnFldLen +lnLoop,14],;
                laTempStru[lnFldLen +lnLoop,15],laTempStru[lnFldLen +lnLoop,16]
  STORE 0 TO    laTempStru[lnFldLen +lnLoop,17] ,laTempStru[lnFldLen +lnLoop,18]

 ENDFOR
 
 
  SELECT(lcTempPack_Lin)
  DIMENSION laPckLinSt[1,18]
  =AFIELDS(laPckLinSt)

  lnFldLnPck = AFIELDS(laPckLinSt)
  DIMENSION laPckLinSt[lnFldLnPck + 2 , 18]

  *-- Field for the printing lines.
  laPckLinSt[lnFldLnPck + 1 , 1] = 'llPrint'
  laPckLinSt[lnFldLnPck + 1 , 2] = 'L'
  laPckLinSt[lnFldLnPck + 1 , 3] = 1
  laPckLinSt[lnFldLnPck + 1 , 4] = 0

  *-- Field to change the group.
  laPckLinSt[lnFldLnPck + 2 , 1] = 'lcType'
  laPckLinSt[lnFldLnPck + 2 , 2] = 'C'
  laPckLinSt[lnFldLnPck + 2 , 3] = 1
  laPckLinSt[lnFldLnPck + 2 , 4] = 0
 
 FOR lnLoop = 1 to  2
   STORE ' ' TO  laPckLinSt[lnFldLnPck +lnLoop,7],laPckLinSt[lnFldLnPck +lnLoop,8],;
                 laPckLinSt[lnFldLnPck +lnLoop,9],laPckLinSt[lnFldLnPck +lnLoop,10],;
                 laPckLinSt[lnFldLnPck +lnLoop,11],laPckLinSt[lnFldLnPck +lnLoop,12],;
                 laPckLinSt[lnFldLnPck +lnLoop,13],laPckLinSt[lnFldLnPck +lnLoop,14],;
                 laPckLinSt[lnFldLnPck +lnLoop,15],laPckLinSt[lnFldLnPck +lnLoop,16]
   STORE 0 TO    laPckLinSt[lnFldLnPck +lnLoop,17] ,laPckLinSt[lnFldLnPck +lnLoop,18]

  ENDFOR

   =lfWorkFile()  && Fill arrays then create files.

  *-- llHaveLogo : If Company Have a Logo. 
  IF TYPE('loObjects') <> 'O'  
    loObjects = CreateObject("RemoteTable","OBJECTS","OBJECTID",lcTempOBJECTS,SET("DATASESSION")) 
  ENDIF 
  
  IF TYPE('loObjLink') <> 'O'  
    loObjLink = CreateObject("RemoteTable","OBJLINK","OBJLNKTY",lcTempObjLink,SET("DATASESSION")) 
  ENDIF   
  llHaveLogo = loObjLink.SEEK('*' + 'LOGO') AND loObjects.SEEK(&lcTempObjLink..cObject_ID)
  *--
   DIMENSION laCompAdd[5,1] , laShipTo[5,1] , laDivLName[1,2] , laSoldTo[5,1]
  laSoldTo = ''           && Array to hold the Sold To address
  lcSolTName = ''         && Variable to hold the Sold to name
  laCompAdd = ''           && Array to hold the Company address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'
  *--selecting company information from syccomp remotely[Begin]
  lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
  lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+oAriaApplication.ActiveCompanyID+"'  "
  LOCAL lnResult
  lnResult  = oAriaApplication.RemoteSystemData.Execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
 
 
  IF lnResult = 1
    SELECT &lcCompInfo
    lcCompName      = cCom_Name
    lcCompPhon      = cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    DIMENSION laCompAdd[6,1]
    laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
ENDIF


*-- End Of lfWOgWhen.
*!**************************************************************************
*! Name      : lfWorkFile
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Create work cursors.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfWorkFile()
*!**************************************************************************
*
FUNCTION lfWorkFile
*-- First Close all temporary files if it was opened before 
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
ENDIF
*-- First Close all temporary files if it was opened before
=gfCrtTmp(lcPackTmp,@laTempStru,"PACK_NO",lcPackTmp,.T.)
SELECT (lcPackTmp)
*-- You Must Make A Zap To decoument The File After Cursor on The Hard Disk
ZAP
INDEX ON PACK_NO TAG (lcPackTmp)

*-- First Close all temporary files if it was opened before 
IF USED(lcPakLnTmp)
  USE IN (lcPakLnTmp)
ENDIF


=gfCrtTmp(lcPakLnTmp,@laPckLinSt,"PACK_NO+STYLE+STR(nOrdLineNO,6)",lcPakLnTmp,.T.)

SELECT (lcPakLnTmp)
ZAP
INDEX ON PACK_NO+STYLE+STR(nOrdLineNO,6) TAG (lcPakLnTmp) 

INDEX ON pack_no+STR(no_cart,4)+style TAG 'PACKCRTN' ADDITIVE
*-- End Of lfWorkFile.

*!**************************************************************************
*! Name      : lfSRInv   
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRInv (S-> , R->)
*!**************************************************************************
*
FUNCTION lfSRInv
PARAMETERS lcParm

SET ORDER TO TAG Customer IN Customer
SET ORDER TO TAG Invhdr   IN Invhdr
IF lcParm = "S"
  SELECT INVHDR
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  GO TOP
ELSE
  SELECT INVHDR
  SET RELATION TO
ENDIF
*-- End Of lfSRInv.

*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : used to shift the lashipto array
*!**************************************************************************
*! Passed Parameters  : lcArrayNam : Array hold The address.
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End Of lfAdrShift

*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : For Collecting Data In The First Time.
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None.
*!**************************************************************************
*! Example   : = lfCollect()
*!**************************************************************************
FUNCTION lfCollect

DO CASE 
  CASE lcRpSelcBy = 'I'      && select by invoice    
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lnPosInvoice = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.INVOICE")
    IF TYPE('lcXMLFileName') <> 'C'
      lnPosInvoice = ASCAN(loOgScroll.laOgFXFlt,"INVHDR.INVOICE")
    ELSE
      lnPosInvoice = ASCAN(laOgFXFlt,"INVHDR.INVOICE")
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    IF lnPosInvoice > 0 
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
      *lnPosInvoice = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosInvoice,1)
      IF TYPE('lcXMLFileName') <> 'C'
        lnPosInvoice = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosInvoice,1)
      ELSE
        lnPosInvoice = ASUBSCRIPT(laOgFxFlt,lnPosInvoice,1)
      ENDIF
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
      lcInvoiceSel =IIF(!EMPTY(laOgFxFlt[lnPosInvoice,6]),laOgFxFlt[lnPosInvoice,6],'')
      *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[Start] 
      *IF !EMPTY(lcInvoiceSel)      
      IF !EMPTY(lcInvoiceSel) AND USED(lcInvoiceSel)
	  *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[End]
        SELECT(lcInvoiceSel)
        LOCATE
        IF !EOF()
          SCAN 
            *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
            *WAIT WINDOW "Collect data for Invoice #" + &lcInvoiceSel..Invoice  + ' ...' NOWAIT
            IF TYPE('lcXMLFileName') <> 'C'
              WAIT WINDOW "Collect data for Invoice #" + &lcInvoiceSel..Invoice  + ' ...' NOWAIT
            ENDIF
            *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
            loInvHdr.Seek(&lcInvoiceSel..INVOICE)
            *-- If Find this Piktik in Pack_Hdr file.
            *--
            loPack_Hdr.setorder("ORDERPCK")   && ORDER+STORE+PACK_NO 
            *--B128185,1 mmt 05/25/05 fix bug of wrong packing list format[Start]
            *IF loPack_Hdr.SEEK(&lcTempInvHdr..order)
            IF loPack_Hdr.SEEK(&lcTempInvHdr..order+&lcTempInvHdr..STORE)
            *--B128185,1 mmt 05/25/05 fix bug of wrong packing list format[End]
            *--
*--            IF !EMPTY(&lcTempInvHdr..PikTkt) .AND. loPack_Hdr.SEEK(&lcTempInvHdr..PikTkt)
              SELECT (lcTempPack_Hdr)
              SCATTER MEMVAR MEMO
*--               m.nRprtTyp  = 2
              m.nRprtTyp  = 1
              m.Invoice   = &lcTempInvHdr..Invoice
              M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempInvHdr..CWARECODE)
              m.shipvia   = &lcTempInvHdr..shipvia
              loPack_Hdr.setorder("Pack_Hdr")
            ELSE  && pick ticket not found in pack header file. 
              SELECT(lcTempInvHdr)
              m.Pack_No    = Invoice
              m.Account    = Account
              m.Order      = Order        
              m.Store      = Store
              m.Note       = Note2
              m.Tot_Wght   = Weight
              m.Tot_Cart   = Cartons
              m.Tot_Pcs    = Ship
              m.CWARECODE  =  CWARECODE
              m.nRprtTyp   = 1
              m.Invoice    = Invoice
              m.shipvia   = &lcTempInvHdr..shipvia
            ENDIF
            INSERT INTO (lcPackTmp) FROM MEMVAR
          ENDSCAN 
          WAIT CLEAR

          *--function to collect the lines 
          =lfGetLines()
          =lfGetData()
        ELSE
          =lfNoInvSel()
        ENDIF 
      ELSE
        =lfNoInvSel()
      ENDIF 
    ENDIF       

  CASE lcRpSelcBy = 'P'      && select by Pack List    
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *lnPosPackList = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")
    IF TYPE('lcXMLFileName') <> 'C'
      lnPosPackList = ASCAN(loOgScroll.laOgFXFlt,"PACK_HDR.PACK_NO")
    ELSE
      lnPosPackList = ASCAN(laOgFXFlt,"PACK_HDR.PACK_NO")
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]

    *B608381,1 WAM 12/13/2007 Get the cust PO# from the piktkt file
    loPack_Hdr.setorder("Pack_Hdr")
    *B608381,1 WAM 12/13/2007 (End)

    IF lnPosPackList > 0 
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
      *lnPosPackList = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPackList,1)
      IF TYPE('lcXMLFileName') <> 'C'
        lnPosPackList = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPackList,1)
      ELSE
        lnPosPackList = ASUBSCRIPT(laOgFxFlt,lnPosPackList,1)
      ENDIF
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
      lcPackLstSel =IIF(!EMPTY(laOgFxFlt[lnPosPackList,6]),laOgFxFlt[lnPosPackList,6],'')
      *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[Start]       
      *IF !EMPTY(lcPackLstSel)      
      IF !EMPTY(lcPackLstSel) AND USED(lcPackLstSel)
      *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[End]       
        SELECT(lcPackLstSel)
        LOCATE
        IF !EOF()
          SCAN 
            *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
            *WAIT WINDOW "Collect data for PACK_NO #" + &lcPackLstSel..PACK_NO  + ' ...' NOWAIT
            IF TYPE('lcXMLFileName') <> 'C'
              WAIT WINDOW "Collect data for PACK_NO #" + &lcPackLstSel..PACK_NO  + ' ...' NOWAIT
            ENDIF
            *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
            loPack_hdr.Seek(&lcPackLstSel..PACK_NO)
            SELECT(lcTempPack_Hdr)
            SCATTER MEMVAR MEMO
            m.nRprtTyp = 2
            m.Invoice = ''
            IF EMPTY(M.CWARECODE)
              loOrdHdr.Seek('O' + m.Order)
              M.CWARECODE = &lctempOrdHdr..CWARECODE
            ENDIF 
           * M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,ORDHDR.CWARECODE)
            INSERT INTO (lcPackTmp) FROM MEMVAR
          ENDSCAN 
          WAIT CLEAR
          *--function to collect the lines 
          =lfGetLines()
          =lfGetData()

        ELSE
          =lfNoPackSel()
        ENDIF  
      ELSE
        =lfNoPackSel()
      ENDIF 
    ENDIF    
    

  CASE lcRpSelcBy = 'T'      && select by pickticket
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]    
    *lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
    IF TYPE('lcXMLFileName') <> 'C'
      lnPosPikTkt = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")      
    ELSE
      lnPosPikTkt = ASCAN(laOgFXFlt,"PIKTKT.PIKTKT")
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    IF lnPosPikTkt > 0 
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
      *lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
      IF TYPE('lcXMLFileName') <> 'C'
        lnPosPikTkt = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPikTkt,1)
      ELSE
        lnPosPikTkt = ASUBSCRIPT(laOgFxFlt,lnPosPikTkt,1)
      ENDIF
      *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
      lcPikTktSel =IIF(!EMPTY(laOgFxFlt[lnPosPikTkt,6]),laOgFxFlt[lnPosPikTkt,6],'')
      *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[Start]             
      *IF !EMPTY(lcPikTktSel)      
      IF !EMPTY(lcPikTktSel) AND USED(lcPikTktSel)
      *! B609458,1 MMT 11/11/2010 Check if cursors are opened before using it[End]             
        SELECT(lcPikTktSel)
        LOCATE
        IF !EOF()
          SCAN 
            loPikTkt.Seek(&lcPikTktSel..PIKTKT)
            SELECT(lcTempPikTkt)
            m.Pack_No    = PikTkt
            m.Account    = Account
            m.Order      = Order        
            m.Store      = Store
            m.nRprtTyp   = 3
            m.CWARECODE =  CWARECODE
            *B608381,1 WAM 12/13/2007 store pick ticket#
            m.piktkt = piktkt
            *B608381,1 WAM 12/13/2007 (End)
            
            *--Update cwarecode if empty
            IF EMPTY(M.CWARECODE)
              loOrdHdr.Seek('O' + m.Order)
              M.CWARECODE = &lctempOrdHdr..CWARECODE
            ENDIF 
            INSERT INTO (lcPackTmp) FROM MEMVAR
          ENDSCAN   
          WAIT CLEAR  
          *--function to collect the lines 
          =lfGetLines()
          =lfGetData()

        ELSE
          =lfNoPickSel()
        ENDIF 
      ELSE
        =lfNoPickSel()  
      ENDIF 
    ENDIF   
ENDCASE

*-- END OF lfCollect.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Calcualte spent time in data collection.
*!**************************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!**************************************************************************
*! Returns            : Spent time.
*!**************************************************************************
*! Example   : =lfCollTime()
*!**************************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- END OF lfCollTime.

*!**************************************************************************
*! Name      : lfHeadVar
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 1 - Hold Address In Shipto Array
*!           : 2 - Get the division long name.
*!**************************************************************************
*! Called from : Report Group Header band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfHeadVar()
*!**************************************************************************
FUNCTION lfHeadVar
**B608560,1 to get name of division in temporary file not from ORDHDR master file beg[ALA].
**= gfRltFld(OrdHdr.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
**B608560,1 to get name of division in temporary file not from ORDHDR master file end[ALA].
**B608560,1 to get name of division in temporary file not from ORDHDR master file beg[ALA].
= gfRltFld(&lcOrdHdr..CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
**B608560,1 to get name of division in temporary file not from ORDHDR master file end[ALA].

DIMENSION laShipTo[5,1]
laShipTo = ''

*B607990,1 HIA increase the bill to address to 6 attributes
*DIMENSION laSoldTo[5,1]
DIMENSION laSoldTo[6,1]
*B607990,1 HIA increase the bill to address to 6 attributes
laSoldTo = ''

lcSolTName = &lcCustomer..BTName
laSoldTo[1] = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2')
*B607990,1 HIA increase the bill to address to 6 attributes
laSoldTo[6] = gfGetAdr(lcCustomer , '' , '' , '' , 6 , '2')
*B607990,1 HIA increase the bill to address to 6 attributes
=lfAdrShift('laSoldTo')

PRIVATE lcDistCntr , lcAlasCust
lcAlasCust = SELECT(0)
SELECT(lcCustomer)

IF &lcOrdHdr..Alt_ShpTo
  DIMENSION laShipTo[6,1]
  laShipTo = ''
  laShipTo[1] = &lcOrdHdr..STName
  laShipTo[2] = &lcOrdHdr..cAddress1
  laShipTo[3] = &lcOrdHdr..cAddress2
  laShipTo[4] = &lcOrdHdr..cAddress3
  laShipTo[5] = &lcOrdHdr..cAddress4
  laShipTo[6] = &lcOrdHdr..cAddress5
  =lfAdrShift('laShipTo')
ELSE
  lnCUSRec = 0
  *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
  *IF !EMPTY(&lcCustomer..Store) AND !EMPTY(&lcCustomer..Dist_ctr)
  IF !EMPTY(&lcCustomer..Store) AND !EMPTY(&lcCustomer..Dist_ctr) AND !EVALUATE(lcOrdHdr+'.lStrDirct')
  *N000592,1 HBG [End]
    lnCUSRec = IIF(!EOF(lcCustomer),RECNO(lcCustomer),0)
    =SEEK('S'+&lcCustomer..Account+&lcCustomer..Dist_ctr)
  ENDIF

  =gfGetAdr(lcCustomer , '' , '' , '' , @laShipTo)
  =lfAdrShift('laShipTo')

  DIMENSION laShipTo[6,1]
  =AINS(laShipTo,1)

  laShipTo[1,1] = &lcCustomer..StName
  =lfAdrShift('laShipTo')

  IF BETWEEN(lnCusRec , 1 , RECCOUNT(lcCUSTOMER))
    GOTO lnCusRec IN &lcCUSTOMER
  ENDIF
ENDIF
SELECT(lcAlasCust)


*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [BEGIN]
*!*	DIMENSION laCompAdd[6,1]
*!*	laCompAdd = ''
*!*	STORE '' TO lcCompName,lcCompPhon
*!*	PRIVATE lnSlct
*!*	lnSlct = SELECT(0)
*!*	SELECT(lcWareHous)
*!*	SEEK &lcPackTmp..CWARECODE
*!*	IF llPrntComp   && If no need to print the warehous data
PRIVATE lnSlct
lnSlct = SELECT(0)
IF llPrntComp AND lcRpAddres = 'W'   && If no need to print the warehous data
  DIMENSION laCompAdd[6,1]
  laCompAdd = ''
  STORE '' TO lcCompName,lcCompPhon
  SELECT(lcWareHous)
  SEEK &lcPackTmp..CWARECODE
*! B607984,1 AYM 02/20/2007 T20061222.0001 A PROBLEM WITH OPTION PRINT ADDRESS BY [END]

  DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
  =gfGetAdr(lcWareHous , '' , '' , '' , @laCompAdd)

  && Get the warehous name ane its phone in the variables   lcCompName , lcCompPhon
  lcCompName = &lcWareHous..CDESC  && WAREHOUS Name.
  lcCompPhon = &lcWareHous..CPHONE && WAREHOUS Phone.

  DIMENSION laCompAdd[6,1]
  laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , '@R '+lcPhonPict)
  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
SELECT (lnSlct)

STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfHeadVar

*!**************************************************************************
*! Name      : lfGrpSetes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : 
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfGrpSetes()
*!**************************************************************************
FUNCTION lfGrpSetes
PARAMETERS llDosMode
lnGrdTotWg = 0

PRIVATE lcSkipExpr , lnCurAlias , lcSkipTag , lcCurRec
lnCurAlias = SELECT(0)

lcSkipExpr = ''
SET SKIP TO

SELECT (lcInvLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT (lcPakLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT(lcOrdLnTmp)
SET RELATION OFF INTO &lcSTYLEFile

SELECT (lnCurAlias)

PRIVATE lcLocExpr

DO CASE 
  CASE nRprtTyp = 1
    SELECT(lcInvLnTmp)
    lcLinFile = lcInvLnTmp

    lcPackNo = INVOICE
    lcLocExpr = "INVOICE = '" + lcPackNo + "' AND ORDER = '" + &lcORDHDR..ORDER + "'"

  CASE nRprtTyp = 2
    SELECT (lcPakLnTmp)
    lcLinFile = lcPakLnTmp

    lcPackNo = pack_no
    lcLocExpr = ""
     
  CASE nRprtTyp = 3
    SELECT(lcOrdLnTmp)
    lcLinFile = lcOrdLnTmp

    lcPackNo = "O"+ORDER
    lcLocExpr = "cORDTYPE+ORDER = '" + lcPackNo + "' AND PIKTKT = '" + &lcOrdLnTmp..PIKTKT + "'"
    
ENDCASE  
 
  lcSkipTag = ORDER()
  lcCurRec = EVALUATE(KEY())
  SET ORDER TO (lcSkipTag) DESC
  IF SEEK(lcPackNo)

    IF !EMPTY(lcLocExpr)    
      LOCATE REST WHILE &lcLocExpr
    ENDIF
    lnLastRec = RECNO()
  ENDIF
  SET ORDER TO (lcSkipTag) ASCE
  =SEEK(lcCurRec)
  SET RELATION TO  STYLE INTO &lcStyleFile ADDITIVE
SELECT (lnCurAlias)
lcSkipExpr = IIF(nRprtTyp=1,lcInvLnTmp,IIF(nRprtTyp=2,lcPakLnTmp,lcOrdLnTmp))
SET SKIP TO &lcSkipExpr
llEndGroup = .F.

RETURN ""
*-- END OF  lfGrpSetes

*!**************************************************************************
*! Name      : lfChngScle
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To Print New Scale If Changed
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfChngScle()
*!**************************************************************************
*
FUNCTION lfChngScle
lcScale = &lcStyleFile..Scale
RETURN ''
*-- End of lfChngScle 

*!**************************************************************************
*! Name      : lfStyMasks
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfStyMasks()
*!**************************************************************************
*
FUNCTION lfStyMasks
PRIVATE lnI , lnSegLen
STORE 0 TO lnI , lnClrStart , lnSegLen
lnMajSeg = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*= gfItemMask(@laMajSegs)
IF TYPE('lcXMLFileName') <> 'C'
  =gfItemMask(@laMajSegs)
ELSE
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laMajSegs)
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
STORE '' TO lcMajorTlt , lcMajorPic , lcNnMajTlt , lcNnMajPic

*-- lcMajorTlt : Variable Hold Major Title    
*-- lcMajorPic : Variable Hold Major Picture      
*-- lcNnMajTlt : Variable Hold Non   Major Title    
*-- lcNnMajPic : Variable Hold Non   Major Picture  
*-- lnClrStart : Variable Hold Color Start Position
*-- lnClrLen   : Variable Hold Color Length

FOR lnI = 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScSep = IIF(lnExtScSep=0,LEN(laMajSegs[lnI,6]),lnExtScLen)    
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen) + lnExtScSep
  ENDIF
  
  *-- Evaluate Major title and picture
  lnSegLen = LEN(laMajSegs[lnI,3])
  IF lnI <= lnMajSeg
    = lfEvlMajNn(@lcMajorTlt , @lcMajorPic)
  ELSE  && else Evaluate Non Major title and picture
    = lfEvlMajNn(@lcNnMajTlt , @lcNnMajPic)
    IF laMajSegs[lnI,1] = "C"
      lnClrStart = laMajSegs[lnI,4]
      lnClrLen   = lnSegLen
    ENDIF
  ENDIF
ENDFOR
*-- End Of lfStyMasks.

*!**************************************************************************
*! Name      : lfEvlMajNn
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Passed Parameters : 1 - (lcTltValue) 
*!                     2 - (lcPicValue)
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfEvlMajNn()
*!**************************************************************************
*
FUNCTION lfEvlMajNn
PARAMETERS lcTltValue , lcPicValue
IF !EMPTY(lcTltValue)
  lcTltValue = lcTltValue + laMajSegs[lnI-1,6]
  lcPicValue = lcPicValue + laMajSegs[lnI-1,6]
ENDIF
lcTltValue = lcTltValue + PADR(laMajSegs[lnI,2],lnSegLen)
lcPicValue = lcPicValue + laMajSegs[lnI,3]
*-- End Of lfEvlMajNn.

*!**************************************************************************
*! Name      : lfNonMjDes
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Evaluate Non Major Code and Description
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfNonMjDes()
*!**************************************************************************
*
FUNCTION lfNonMjDes

PRIVATE lnI , lcTemp , lcStyle , lcNonMjDes,lnAlias
STORE '' TO lcTemp , lcNonMjDes , lnAlias
lnAlias = SELECT()
SELECT(lcPackTmp)

lcStyle = IIF(&lcPackTmp..nRprtTyp=1,&lcInvLnTmp..STYLE,IIF(&lcPackTmp..nRprtTyp=2,&lcPakLnTmp..STYLE,&lcOrdLnTmp..STYLE))
lcDyelot = IIF(&lcPackTmp..nRprtTyp=1,&lcInvLnTmp..dyelot,IIF(&lcPackTmp..nRprtTyp=3,&lcOrdLnTmp..Dyelot,&lcPakLnTmp..Dyelot))
lnI = 0

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  lcTemp = ''

  DO CASE
    *-- Free, Other, Make, or Quality Segment.
    CASE laMajSegs[lnI,1] $ "FOTQ"
      IF loICSEGVAL.SEEK(STR(lnI,1)+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])))
        lcTemp = ALLTRIM(&lcTempICSEGVAL..cISgValSd)
      ENDIF
    *-- Season, Color, Division, or lcStyle group Segment.
    CASE laMajSegs[lnI,1] $ "ZCDG"
      DO CASE
        CASE laMajSegs[lnI,1] = "Z"
          lcCodeExpr = "SEASON"    
        CASE laMajSegs[lnI,1] = "C"
          lcCodeExpr = "COLOR"    
        CASE laMajSegs[lnI,1] = "D"
          lcCodeExpr = "CDIVISION"    
        OTHERWISE
          lcCodeExpr = "CSTYGROUP"    
      ENDCASE
      
     lcTemp = ALLTRIM(gfCodDes(SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcCodeExpr,.F.))
     *-- Size Seqment case.
    OTHERWISE
      IF SEEK("S"+SUBSTR(lcStyle,laMajSegs[lnI,4],LEN(laMajSegs[lnI,3])),lcScaleFile)
        lcTemp = ALLTRIM(&lcScaleFile..cScl_desc)
      ENDIF
    
  ENDCASE
  lcNonMjDes = IIF(EMPTY(lcNonMjDes),lcTemp,lcNonMjDes + IIF(EMPTY(lcTemp),'','-') + lcTemp)
 ENDFOR    && end Loop Around Non Major elements.

lcStyle    = IIF(lnExtScPos = 0,lcStyle,LEFT(lcStyle,LEN(lcStyle)-lnExtScLen))
IF llUse_config
  lcStyleExp = lcStyle
ELSE
  lcStyleExp = lcStyle+' '+lcNonMjDes
ENDIF   
SELECT(lnAlias)
RETURN ''

*-- End Of lfNonMjDes.

*!**************************************************************************
*! Name      : lfSpckln
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   :  Fill The Array With Spck_lin & Scale
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfSpckln()
*!**************************************************************************
*
FUNCTION lfSpckln
PRIVATE lcStyls , lnCount , lcCount ,lcAlias
STORE '' TO  lcStyls , lnCount , lcCount
DIMENSION laTemp[8] 

STORE "" TO laSpckTemp , laTemp
lcAlias = SELECT (0)

IF lcRpForm = "ALPKLSB" AND llPrnPack   
  IF llUse_config 
    =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style,&lcLinFile..DYELOT)
  ELSE 
    =lfGetPack(&lcPackTmp..Order, &lcPackTmp..Store, &lcLinFile..Style,'')
  ENDIF   
ENDIF

PRIVATE lcAlasPck , lcOrdrLin , lnRcNoOrd , lcOrdrSek , lcLnNoFld
lcAlasPck = SELECT(0)
SELECT(lcOrdLnTmp)
lnRcNoOrd = RECNO()
lcOrdrLin = ORDER()
IF lcOrdLnTmp <> lcTempOrdline
  SET ORDER TO TAG &lcOrdLnTmp
ELSE
  SET ORDER TO tag ordline 
ENDIF 
*SET ORDER TO TAG &lcOrdLnIndTmp
lcOrdrSek = EVAL(lcPackTmp+'.ORDER')
LOCATE

DO CASE
  CASE lcRpSelcBy $ "TI"
    lcLnNoFld = STR(EVAl(lcLinFile+'.LINENO'),6)
  CASE lcRpSelcBy = "P"
    lcLnNoFld = STR(EVAl(lcLinFile+'.nordlineno'),6)

ENDCASE

IF SEEK("O" + lcOrdrSek + lcLnNoFld) AND !EMPTY(&lcOrdLnTmp..PACK_ID)
    STORE SPACE(0) TO laSpckTemp[1]
    laSpckTemp[1] = "Pack ID : " + ALLTRIM(&lcOrdLnTmp..PACK_ID)
ELSE
  *--
  IF llUse_config 
    IF loSpck_lin.Seek('S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style + &lcLinFile..Dyelot)
      IF &lcTempSpck_Lin..TotQty = 0
        laSpckTemp[1]='SKU #:' + &lcTempSpck_Lin..Pack_Id
      ELSE
        lnCount = 1 
        SELECT(lcTempSpck_Lin) 
        lnSavRec = RECNO()
        FOR lnCount = 1 To 8
          GOTO lnSavRec
          lcCount = STR(lnCount, 1 )
          IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)) 
            SCAN REST WHILE TYPE+ACCOUNT+STYLE+DYELOT+PACK_ID = 'S' + &lcPackTmp..Account+&lcLinFile..Style + &lcLinFile..Dyelot;
              FOR lnCount <= 8
                IF !EMPTY(&lcTempSpck_Lin..Qty&lcCount)
                  laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',&lcScaleFile..Sz&lcCount + ':' + &lcTempSpck_Lin..Pack_Id)
                ENDIF
            ENDSCAN
          ENDIF 
        ENDFOR
      ENDIF
    ENDIF
  ELSE 
    IF loSpck_lin.SEEK( 'S' + &lcPackTmp..ACCOUNT + &lcLinFile..Style )
      IF &lcTempSpck_Lin..TotQty = 0
        laSpckTemp[1]='SKU #:' + &lcTempSpck_Lin..Pack_Id
      ELSE
        lnCount = 1  
        SELECT(lcTempSpck_Lin)
        lnSavRec = RECNO()
        FOR lnCount = 1 To 8
          GOTO lnSavRec
          lcCount = STR(lnCount, 1 )
          IF  !EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)) 
            SCAN REST WHILE Type+Account+Style+Pack_id = 'S' + &lcPackTmp..Account+&lcLinFile..Style;
              FOR lnCount <= 8
                IF !EMPTY(&lcTempSpck_Lin..Qty&lcCount)
                  laTemp[lnCount] =IIF(EMPTY(EVAL(lcLinFile+'.Qty'+lcCount)),'',&lcScaleFile..Sz&lcCount + ':' + &lcTempSpck_Lin..Pack_Id)
                ENDIF
            ENDSCAN
          ENDIF 
        ENDFOR
      ENDIF
    ENDIF
  ENDIF 
  SELECT(lcOrdLnTmp)
  IF BETWEEN(lnRcNoOrd,1,RECCOUNT(lcOrdLnTmp))
    GOTO lnRcNoOrd IN &lcOrdLnTmp
  ENDIF
  SET ORDER TO TAG &lcOrdrLin
  SELECT(lcAlasPck)
ENDIF

lnNtpty = 1
FOR I   = 1 TO 8
  IF !EMPTY(laTemp[I])
    laSpckTemp[lnNtpty] = laTemp[I]
    lnNtpty = lnNtpty + 1    
  ENDIF
ENDFOR 

*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[Start]
SELECT(lcOrdLnTmp)
SET ORDER TO TAG &lcOrdrLin
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[End]

SELECT (lcalias)
RETURN ''
*-- End Of lfSpckln

*!**************************************************************************
*! Name      : lfEndGroup
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Rise End Group Flag Which Control Page Footer Data.
*!**************************************************************************
*! Called from : Group fotter band.
*!**************************************************************************
*! Passed Parameters : None.
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfEndGroup()
*!**************************************************************************
FUNCTION lfEndGroup
*-- Set this variable .T. to don't print the word "CONTINUE NEXT PAGE"
*-- and then print Totals.
llEndGroup = .T.
lnLastRec = 0
RETURN ''
*-- End Of lfEndGroup.

*!**************************************************************************
*! Name      : lfScalHead
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Return The New Scales If The Scale Changed
*!**************************************************************************
*! Called from : Pomat Details Band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfScalHead()
*!**************************************************************************
*
FUNCTION lfScalHead
PRIVATE lcScalHead
lcScalHead = ""

IF lcScale != &lcStyleFile..Scale
  lcScale = &lcStyleFile..Scale
  lcScalHead = lcScale+" " + PADL(ALLTRIM(&lcScaleFile..SZ1),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ2),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ3),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ4),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ5),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ6),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ7),5)+" " +;
                             PADL(ALLTRIM(&lcScaleFile..SZ8),5)
ENDIF
RETURN lcScalHead
*-- End Of lfScalHead.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Close any opened files if user press OG <Close> Button
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .F.  

*-- Close Temporary Cursors [Begin]
IF USED (lcPackTmp )
 USE IN (lcPackTmp )
ENDIF
*-- Close Temporary Cursors [End]
*-- End Of lfClearRep.
*!**************************************************************************
*! Name      : lfSRPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To set the relations or To release the relations when browse
*!         packing list No.   
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
FUNCTION lfSRPack
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PACK_HDR
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  GO TOP
ELSE
  SELECT PACK_HDR
  SET RELATION TO
ENDIF

*!**************************************************************************
*! Name      : lfVSelcBy
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : to redisplay the element of option grad again after change the select
*!             the select by from (invoice -> pack list )or(Pack list -> invoice)
*!*************************************************************
*! Passed Parameters :
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfVSelcBy()
*!**************************************************************************
FUNCTION lfVSelcBy
llClearInv = (lcRpSelcBy # 'I')    && to clear lcRpExp 
llClearPak = (lcRpSelcBy # 'P')     && to clear lcRpExp 
llClearPik = (lcRpSelcBy # 'T')     && to clear lcRpExp 

CLEARREAD()              && to redisplay (reread)

*!**************************************************************************
*! Name      : lfSRPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : To set the relations or To release the relations when browse
*!         packing list No.   
*!**************************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfSRPack (S-> , R->)
*!**************************************************************************
FUNCTION lfSRPick
PARAMETERS lcParm
SET ORDER TO TAG Customer IN Customer

IF lcParm = "S"
  SELECT PikTkt
  SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
  SET RELATION TO Piktkt.Order INTO ordline  ADDITIVE
  GO TOP
ELSE
  SELECT PikTkt
  SET RELATION TO
ENDIF


*!**************************************************************************
*! Name      : lfGrdTot
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Calculate Grand Total for Weight
*!**************************************************************************
FUNCTION lfGrdTot
lnGrdTotWg = LNGRDWHGT
RETURN ""

*!**************************************************************************
*! Name      : lfGetLines
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : to collect the detaile line from pack_line Into Temp File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : =lfGetLines()
*!**************************************************************************
FUNCTION lfGetLines
*  loPack_Lin = CreateObject("RemoteTable","Pack_Lin","Pack_Lin",lcTempPack_Lin,SET("DATASESSION")) 

loPack_Lin.SETORDER('PACKSTYLE')
*SET ORDER TO PackStyle IN Pack_lin
SET ORDER TO TAG PACKCRTN IN (lcPakLnTmp)
SELECT (lcPackTmp)
DO CASE 
  CASE lcRpSelcBy = "P"
    *--filter the data to used only record with nRprtTyp = 2 (Select by packing list  )
     SCAN FOR nRprtTyp  = 2
    lnStyles = 0
    loPack_Lin.SEEK(&lcPackTmp..Pack_no)
    SELECT(lcTempPack_Lin)
    DO WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no
      lnCartonNO = no_cart
      llSameCarton = .F.
      IF lcRpForm <> "ALPKLSB" .AND. lfSameCartn(&lcPackTmp..Pack_no,lnCartonNO,lnStyles)
        llSameCarton  = .T.
      ELSE  
        lnStyles = 0
      ENDIF
      SCAN REST WHILE pack_no+STR(no_cart,4)+style = &lcPackTmp..Pack_no+STR(lnCartonNO,4)
        SCATTER MEMVAR MEMO
        IF llSameCarton
          SELECT (lcPakLnTmp)
          =SEEK(m.Pack_No+STR(lnCartonNO-1,4)+m.style)
          REPLACE To_Crt  WITH lnCartonNO  ,;
                  No_Cart WITH lnCartonNO ,;
                  Qty1    WITH Qty1 + m.Qty1 ,;
                  Qty2    WITH Qty2 + m.Qty2 ,;
                  Qty3    WITH Qty3 + m.Qty3 ,;
                  Qty4    WITH Qty4 + m.Qty4 ,;
                  Qty5    WITH Qty5 + m.Qty5 ,;
                  Qty6    WITH Qty6 + m.Qty6 ,;
                  Qty7    WITH Qty7 + m.Qty7 ,;
                  Qty8    WITH Qty8 + m.Qty8 ,;
                  TotQty  WITH TotQty + m.TotQty ,;
                  Weight  WITH Weight + m.Weight
        ELSE  
          STORE m.No_Cart TO m.From_Crt,To_Crt,lnFromCrtn
          INSERT INTO (lcPakLnTmp) FROM MEMVAR
          lnStyles = lnStyles + 1
        ENDIF
      ENDSCAN
    ENDDO
  ENDSCAN
  CASE lcRpSelcBy = "I"
    SCAN FOR nRprtTyp  = 1
      loInvLine.Seek(INVOICE)
      SELECT(lcTempInvLine)
      SCAN REST WHILE INVOICE+STR(LINENO,6) =  &lcPackTmp..invoice
        SCATTER MEMVAR MEMO
        INSERT INTO (lcInvLnTmp) FROM MEMVAR
      ENDSCAN 
  ENDSCAN 
ENDCASE 
*!**************************************************************************
*! Name      : lfSameCartn
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Check if the previous carton has the same information
*!**************************************************************************
*! Passed Parameters : lcPackNo    : Packinf list# 
*!                     lnCartnNo   : Carton #
*!                     lnSameLines : Previous carton total lines number
*!**************************************************************************
*! Return      : .T. If the previous carton has the same information
*!**************************************************************************
*! Example     : =lfSameCartn('000001',3,20)
*!**************************************************************************
FUNCTION lfSameCartn
PARAMETERS lcPackNo,lnCartnNo,lnSameLines
PRIVATE llSameCrtn,lnCrtnLines

llSameCrtn  = .T.
lnCrtnLines = 0

SELECT(lcTempPack_Lin)
SCAN REST WHILE pack_no+STR(no_cart,4)+style = lcPackNo+STR(lnCartnNo,4)
  =SEEK(lcPackNo+STR(lnCartnNo-1,4)+style,lcPakLnTmp)
  lnCrtnRange = &lcPakLnTmp..To_Crt - &lcPakLnTmp..From_Crt+1
  IF EOF(lcPakLnTmp) OR ;
     Qty1*lnCrtnRange  <> &lcPakLnTmp..Qty1 OR Qty2*lnCrtnRange <>  &lcPakLnTmp..Qty2 OR ;
     Qty3*lnCrtnRange  <> &lcPakLnTmp..Qty3 OR Qty4*lnCrtnRange <>  &lcPakLnTmp..Qty4 OR ;
     Qty5*lnCrtnRange  <> &lcPakLnTmp..Qty5 OR Qty6*lnCrtnRange <>  &lcPakLnTmp..Qty6 OR ;
     Qty7*lnCrtnRange  <> &lcPakLnTmp..Qty7 OR Qty8*lnCrtnRange <>  &lcPakLnTmp..Qty8 OR ;
     Weight*lnCrtnRange <> &lcPakLnTmp..Weight
    llSameCrtn = .F.
    EXIT
  ENDIF
  lnCrtnLines = lnCrtnLines + 1
ENDSCAN
llSameCrtn = llSameCrtn AND (lnCrtnLines=lnSameLines)
loPack_Lin.SEEK(lcPackNo+STR(lnCartnNo,4))
RETURN(llSameCrtn)
*!**************************************************************************
*! Name      : lfChkModls
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Cheak if moduls AL & AS instaled in the company.
*!**************************************************************************
*! Passed Parameters  : .........
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfChkModls()
*!**************************************************************************
FUNCTION lfChkModls
llModInst = .F.
llModInst = ('AL' $ oAriaApplication.CompanyInstalledModules  .OR. 'AS' $ oAriaApplication.CompanyInstalledModules  )
*-- llogtrmnat:- Variable Terminate The Option Grid.
IF !llModInst 
  *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
  *=gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('TRM42083B00000','DIALOG','Advanced Shipment and Sales Order Allocation')
  ENDIF
  *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
  llOgTrmnat = .T.
  CLEARREAD()
  RETURN
ENDIF
*-- End Of lfChkModls.

*!**************************************************************************
*! Name      : lfGetPack
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/14/2004
*! Purpose   : Retrieve Pack ID
*!**************************************************************************
*! Passed Parameters  : lcOrder,lcLineNo
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   :  =lfGetPack()
*!**************************************************************************
FUNCTION lfGetPack
PARAMETERS lcOrder,lcStore,lcStyle,lcStyDyelot

PRIVATE lcAlias,lcCurTag

STORE "" TO lcPackId

lcAlias = ALIAS()

SELECT(lcOrdLnTmp)
lcCurTag = TAG()
*B132411,1 MMT 11/13/2006 Error when you press preview [Start]
*SET ORDER TO TAG &lcOrdLnIndTmp
IF lcOrdLnTmp  = lcTempOrdline
  SET ORDER TO ORDLINST
ELSE 
  SET ORDER TO TAG &lcOrdLnIndTmp
ENDIF 
*B132411,1 MMT 11/13/2006 Error when you press preview [End]

IF llUse_config 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id) AND DYELOT = lcStyDyelot
    lcPackId = Pack_Id
  ENDIF
ELSE 
  IF SEEK("O"+ lcOrder + lcStore +lcStyle,lcOrdLnTmp) AND !EMPTY(Pack_Id)
    lcPackId = Pack_Id
  ENDIF
ENDIF 
SET ORDER TO &lcCurTag
SELECT &lcAlias
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 01/03/05
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
*: Date      : 12/27/04
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
  DO CASE

   *--temp. Customer File
   CASE UPPER(lcTable) = lcCustomer
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'TYPE+ACCOUNT+STORE'
     laIndex[1,2] = lcCustomer

   *--temp. ordhdr file
   CASE UPPER(lcTable) =  lcOrdHdr
     DIMENSION laIndex[1,2]
     laIndex[1,1] = 'CORDTYPE+ORDER'
     laIndex[1,2] = lcOrdHdr

   *--temp. ordline file
   CASE UPPER(lcTable) =  lcOrdLnTmp
      DIMENSION laIndex[2,2]
      laIndex[1,1] = 'CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)'
      laIndex[1,2] = lcOrdLnIndTmp
      laIndex[2,1] = 'CORDTYPE+ORDER+STR(LINENO,6)'
      laIndex[2,2] = lcOrdLnTmp
      
   *-- temp. piktkt file lcPickTkTmp
   CASE UPPER(lcTable) =lcInvLnTmp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'INVOICE+STR(LINENO,6)'
      laIndex[1,2] = lcInvLnTmp
 
   *--temp. pikline file
   CASE UPPER(lcTable) =  lcNotePad
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+key'
      laIndex[1,2] = lcNotePad

   CASE UPPER(lcTable) =  lcStyleFile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'Style'
      laIndex[1,2] = lcStyleFile

   CASE UPPER(lcTable) =  lcScalefile
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'type+scale+prepak'
      laIndex[1,2] = lcScalefile

   CASE UPPER(lcTable) =  lcWareHous
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'cWareCode'
      laIndex[1,2] = lcWareHous
   *--lcPikTemp
   CASE UPPER(lcTable) =  lcPikTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'piktkt'
      laIndex[1,2] = lcPikTemp
   *--lcPckHdTemp
   CASE UPPER(lcTable) =  lcPckHdTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'pack_no'
      laIndex[1,2] = lcPckHdTemp

   CASE UPPER(lcTable) =  lcInvhdTemp
      DIMENSION laIndex[1,2]
      laIndex[1,1] = 'invoice'
      laIndex[1,2] = lcInvHdTemp

ENDCASE
*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to get the data from ordhdr and customer files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfGetData
SELECT (lcPackTmp)
LOCATE 
*! B127377,1 MMT 04/13/2005 , FIX BUG OF NOT PRINTING SHIP TO ADDRESS[Start]
SCAN 
  *--IF EMPTY(Store)
  =loCustomer.Seek('M'+ &lcPackTmp..Account)
  *--ELSE
   *-- loCustomer.Seek('S'+ Account + Store)
*--  ENDIF 
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'M'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
  =loCustomer.Seek('S'+ &lcPackTmp..Account)
  SELECT(lcTempCustomer)
  SCAN REST WHILE &lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store = 'S'+ &lcPackTmp..Account
    IF !SEEK(&lcTempCustomer..Type+&lcTempCustomer..Account+&lcTempCustomer..Store,lcCustomer)
      SELECT(lcTempCustomer)
      SCATTER MEMO MEMVAR 
      INSERT INTO (lcCustomer) FROM MEMVAR 
    ENDIF   
  ENDSCAN   
ENDSCAN 
*! B127377,1 MMT 04/13/2005 , FIX BUG OF NOT PRINTING SHIP TO ADDRESS[End]
SELECT (lcPackTmp)
LOCATE 
SCAN 
  loNotepad.seek("B" + ORDER)
  SELECT(lcTempNotepad)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcNotePad) FROM MEMVAR 
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  looRDHDR.SEEK("O" + ORDER)
  SELECT(lcTempordhdr)
  SCATTER MEMO MEMVAR 
  INSERT INTO (lcordhdr) FROM MEMVAR 
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loOrdLine.Seek('O'+ &lcPackTmp..order + &lcPackTmp..Store)
  SELECT(lcTempOrdLine)
  *B608381,1 WAM 12/13/2007 Get only pick ticket lines
  *SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store
   SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+ &lcPackTmp..order + &lcPackTmp..Store ;
             FOR   IIF(INLIST(lcRpSelcBy,'P','T'),piktkt = &lcPackTmp..piktkt,.T.)
  *B608381,1 WAM 12/13/2007 (End)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdLnTmp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 
DO CASE 
  CASE lcRpSelcBy = "I"
    lcLinFile = lcInvLnTmp
    SELECT(lcInvLnTmp)
    SCAN 
      loStyle.Seek(Style)
      SELECT(lcStyleFile)
      IF !SEEK(&lcTempStyle..Style,lcStyleFile)
        SELECT(lcTempStyle)
        SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
      ENDIF 
    ENDSCAN 
  CASE lcRpSelcBy = "P"
    SELECT(lcPakLnTmp)
    SCAN 
      loStyle.Seek(Style)
      SELECT(lcStyleFile)
      IF !SEEK(&lcTempStyle..Style,lcStyleFile)
        SELECT(lcTempStyle)
        SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
      ENDIF
    ENDSCAN 
  CASE lcRpSelcBy = "T"
    SELECT(lcOrdLnTmp)
    SCAN 
    loStyle.Seek(Style)
    SELECT(lcStyleFile)
      IF !SEEK(&lcTempStyle..Style,lcStyleFile)
        SELECT(lcTempStyle)
        SCATTER MEMO MEMVAR 
    INSERT INTO (lcStyleFile) FROM MEMVAR 
      ENDIF
    ENDSCAN 
ENDCASE 

SELECT(lcStyleFile)
SCAN 
  loScale.Seek('S'+ Scale)
  IF !SEEK('S'+ &lcTempScale..Scale,lcScalefile)
    SELECT(lcTempScale)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcScaleFile) FROM MEMVAR 
  ENDIF   
ENDSCAN 

SELECT (lcPackTmp)
LOCATE 
SCAN 
  loWareHous.Seek(&lcPackTmp..cWareCode)
  IF !SEEK(&lcTempWareHous..cWareCode,lcWareHous)
    SELECT(lcTempWareHous)
    SCATTER MEMO MEMVAR 
    INSERT INTO (lcWareHous) FROM MEMVAR 
  ENDIF   
ENDSCAN 

*!*************************************************************
*! Name      : lfNoInvSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfNoInvSel

IF loInvhdr.llnative
  SELECT(lcTempInvHdr)
  SCAN FOR Status <> 'V'
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *WAIT WINDOW "Collect data for Invoice #" + Invoice  + ' ...' NOWAIT
    IF TYPE('lcXMLFileName') <> 'C'
      WAIT WINDOW "Collect data for Invoice #" + Invoice  + ' ...' NOWAIT
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    *-- If Find this Piktik in Pack_Hdr file.
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[Start]
*!*	    IF !EMPTY(&lcTempInvHdr..PikTkt) .AND. loPack_Hdr.SEEK(&lcTempInvHdr..PikTkt)
*!*	      SELECT (lcTempPack_Hdr)
*!*	      SCATTER MEMVAR MEMO
*!*	      m.nRprtTyp  = 2
*!*	      m.Invoice   = &lcTempInvHdr..Invoice
*!*	      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempInvHdr..CWARECODE)
*!*	      m.shipvia   = &lcTempInvHdr..shipvia
   loPack_Hdr.setorder("ORDERPCK")   && ORDER+STORE+PACK_NO 
   IF loPack_Hdr.SEEK(&lcTempInvHdr..order+&lcTempInvHdr..STORE)
     SELECT (lcTempPack_Hdr)
     SCATTER MEMVAR MEMO
     m.nRprtTyp  = 1
     m.Invoice   = &lcTempInvHdr..Invoice
     M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempInvHdr..CWARECODE)
     m.shipvia   = &lcTempInvHdr..shipvia
    loPack_Hdr.setorder("Pack_Hdr")
*! B607990,1 MMT 02/20/2007 fix error while prview all invoices[End]

    ELSE  && pick ticket not found in pack header file. 
      SELECT(lcTempInvHdr)
      m.Pack_No    = Invoice
      m.Account    = Account
      m.Order      = Order        
      m.Store      = Store
      m.Note       = Note2
      m.Tot_Wght   = Weight
      m.Tot_Cart   = Cartons
      m.Tot_Pcs    = Ship
      m.CWARECODE  = CWARECODE
      m.nRprtTyp   = 1
      m.Invoice    = Invoice
      m.shipvia    = &lcTempInvHdr..shipvia
    ENDIF
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  WAIT CLEAR
  *--function to collect the lines 
*  =lfGetLines()
  lcInvLnTmp  = lcTempInvLine
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous
ELSE 
  lcSelFile =  "Invhdr"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile  ,lcInvHdTemp,lcSelCond)  
  SELECT(lcInvHdTemp)
  SCAN
    IF !EMPTY(&lcInvHdTemp..PikTkt) .AND. loPack_Hdr.SEEK(&lcInvHdTemp..PikTkt)
      SELECT (lcTempPack_Hdr)
      SCATTER MEMVAR MEMO
      m.nRprtTyp  = 2
      m.Invoice   = &lcInvHdTemp..Invoice
      M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcInvHdTemp..CWARECODE)
      m.shipvia   = &lcInvHdTemp..shipvia
    ELSE  && pick ticket not found in pack header file. 
      SELECT(lcInvHdTemp)
      m.Pack_No    = Invoice
      m.Account    = Account
      m.Order      = Order        
      m.Store      = Store
      m.Note       = Note2
      m.Tot_Wght   = Weight
      m.Tot_Cart   = Cartons
      m.Tot_Pcs    = Ship
      m.CWARECODE  = CWARECODE
      m.nRprtTyp   = 1
      m.Invoice    = Invoice
      m.shipvia    = shipvia
    ENDIF
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
   =lfGetLines()
   =lfGetData()
ENDIF 
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
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

*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
IF TYPE('lcXMLFileName') <> 'C'
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("DATASESSION"))
ELSE
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, lcTable, oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
ENDIF
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]

*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
*IF lnConnectionHandlar = 1
IF lnConnectionHandlar = 1 AND TYPE('lcXMLFileName') <> 'O'
*! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
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
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfNoPackSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfNoPackSel
IF loPack_hdr.llnative
*--  loPack_Hdr = CreateObject("RemoteTable","Pack_Hdr","Pack_Hdr",lcTempPack_Hdr,SET("DATASESSION")) 
  SELECT(lcTempPack_Hdr)
  SCAN FOR Status <> 'V'
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    IF TYPE('lcXMLFileName') <> 'C'
      WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SCATTER MEMVAR MEMO
    m.nRprtTyp = 2
    m.Invoice = ''
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
   * M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,&lcTempORDHDR..CWARECODE)
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN
  WAIT CLEAR
  =lfGetLines()
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous

ELSE
  lcSelFile =  "Pack_hdr"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile ,lcPckHdTemp,lcSelCond)  
  SELECT(lcPckHdTemp)
  SCAN
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][Start]
    *WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    IF TYPE('lcXMLFileName') <> 'C'
      WAIT WINDOW "Collect data for PACK_NO #" + PACK_NO  + ' ...' NOWAIT
    ENDIF
    *! E303451,1 SAB 03/17/2014 Convert Packing List Report to Run From RB [T20140113.0002][End]
    SCATTER MEMVAR MEMO
    m.nRprtTyp = 2
    m.Invoice = ''
    IF EMPTY(M.CWARECODE)
       loOrdHdr.Seek('O' + m.Order)
       M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
   * M.CWARECODE = IIF(!EMPTY(M.CWARECODE),M.CWARECODE,ORDHDR.CWARECODE)
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
  =lfGetLines()
  =lfGetData()

ENDIF 
*!*************************************************************
*! Name      : lfNoPickSel
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/27/04
*! Purpose   : function to be called in case of user does not
*!         select invoices
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfNoPickSel
IF loPikTkt.llNative
  SELECT(lcTempPikTkt)
  SCAN FOR  STATUS $ "HOP" .AND. PIKTKT # '******' 
    m.Pack_No    = PikTkt
    m.Account    = Account
    m.Order      = Order        
    m.Store      = Store
    m.nRprtTyp   = 3
    m.CWARECODE =  CWARECODE
    *--Update cwarecode if empty
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lctempOrdHdr..CWARECODE
    ENDIF 
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN  
  WAIT CLEAR
 * =lfGetLines()
  lcCustomer  = lcTempCustomer
  lcNotePad   = lcTempNotePad
  lcOrdhdr    = lcTempOrdhdr
  lcOrdLnTmp  = lcTempOrdline
  lcStyleFile = lcTempStyle
  lcScaleFile = lcTempScale
  lcWareHous  = lcTempWareHous
ELSE
  lcSelFile =  "PikTkt"
  lcSelFld  = " * "
  lcSelCond = " Status <> 'V'"
  =lfOpenSql(lcSelFld ,lcSelFile ,lcPikTemp,lcSelCond)  
  SELECT(lcPikTemp)
  SCAN
    m.Pack_No    = PikTkt
    m.Account    = Account
    m.Order      = Order        
    m.Store      = Store
    m.nRprtTyp   = 3
    m.CWARECODE =  CWARECODE
    *--Update cwarecode if empty
    IF EMPTY(M.CWARECODE)
      loOrdHdr.Seek('O' + m.Order)
      M.CWARECODE = &lcTempOrdHdr..CWARECODE
    ENDIF 
    INSERT INTO (lcPackTmp) FROM MEMVAR
  ENDSCAN 
  *--function to collect the lines 
  =lfGetLines()
  =lfGetData()

ENDIF   

*!*************************************************************
*! Name      : lfGetAddr
*: Developer : AYMAN MAHMOUD AHMED (AYM)
*: Date      : 12/27/04
*! Purpose   : to CHANGE COMPANY ADDRESS TO WAREHOUS ADDRESS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGetAddr
lcAlas= SELECT(0)
SELECT WAREHOUS
SEEK &lcPackTmp..CWARECODE

IF llPrntComp   && If no need to print the warehous data
  IF lcRpAddres = 'W'
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    =gfGetAdr('WAREHOUS' , '' , '' , '' , @laCompAdd)
    lcCompName = WAREHOUS.CDESC  && WAREHOUS Name.
    lcCompPhon = WAREHOUS.CPHONE && WAREHOUS Phone.
  ELSE
    DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
    = gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)

    SELECT SYCCOMP
    SEEK gcAct_Comp
    lcCompName = cCom_Name             && Company Name.
    lcCompPhon = cCom_Phon             && Company Phone.
  ENDIF
  DIMENSION laCompAdd[6,1]
  laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)

  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
SELECT (lnSlct)

SELECT(lcAlas)

RETURN ""
