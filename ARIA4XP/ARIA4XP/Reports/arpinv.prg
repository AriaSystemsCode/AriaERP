*:************************************************************************
*: Procedure file: ARPINV.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Haytham el_Sheltawi
*: Aria4   Author: Mohamed Atia Badran
*:      Copyright (c)
*:  Last modified: 12/09/2002 - N000481,1 (ARIA4)
*:
*:  Procs & Fncts: lfOptMsg
*:               : lfwOldVal
*:               : lfvAccount
*:               : lfvInvNo
*:               : lfAdrShift
*:               : lfSolSpAdr
*:               : lfEndGroup
*:               : lfNotePad
*:               : lfNoteRec
*:
*:************************************************************************
*! Modifications :
*! #B038981, HFK, 01/27/2005, add a condition after preview the reprot 
*! to change print status if true only if the file is printed (because it was changing
*! it even if it is previewed or exported too
*! #B038981,2 HFK, 01/30/2005 Check that file is used before closing it
*! #B038981,3 HFK, 02/01/2005 Fix bug of ( default is not printed) in Aria27
*! and (All) in Aria4XP
*! #B128207,1 HFK, 05/25/2005 Fix bug of not showing the DC address in case of multi store 
*! consolidated invoice
*! B039381,1 HMA 06/06/2005 preview fax &phone in the right format(Fix in Prg&report Layout)
*! E128427,1 MMT 06/20/2005 Print dist_ctr instead of store in case of conslidated by DC
*! N130335,1 TNA 03/29/2006  Add the tax reference.
*! N000592,1 HBG 02/28/2007 T20061201.0014 Print store ship to address or DC address according to flag in SO
*! B608260,1 MMT 09/09/2007 Fix bug of wrong description of the alternative style[T20070823.0014]
*! B608361,1 WAM 11/28/2007 Fix wrong message "No record to display"
*! B608441,1 WAM 02/18/2008 Reset factor name and address for each invoice printed.
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Task:T20081225.0022]
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[T20100226.0004]
*! B609470,1 MMT 11/29/2010 Custom Invoice Forms does not work from the request builder[T20100226.0004]
*! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[T20100226.0004]
*! B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [T20120201.0193]
*! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [T20120201.0193]
*! B609901,1 MMT 04/30/2012 Fix the invoice form program to work from RB[T20120411.0017]
*! B609901,1 MMT 07/03/2012 Fix Bug of Access is Denied when Printing Invoice [T20120702.0010]
*! E303247,1 MMT 09/10/2012 Single size invoice and Packing list forms[T20120821.0021]
*! E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*! B610717,1 HES 04/22/2014 Allow the invoice details to be exported to excel beside the header values [T20140404.0006]
*! T20140522.0001,1 TMI 06/01/2014 Append Request Builder Changes 
*! B610864,1 MMT 09/24/2014 Invoice Form is too slow if option print invoice notes is set to NO[T20140703.0016]
*! B611011,1 MMT 06/04/2015 Error while exporting invoice form to Excel[T20150601.0001]
*:************************************************************************
*!*	ACTIVATE WINDOW trace
*!*	_screen.Visible=.t.
*!*	SUSPEND
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID

*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
IF TYPE('lcRequestID') = 'C' .AND. 'TEMP.TXT' $ UPPER(lcRequestID)
  STRTOFILE("2.0.0.1", lcRequestID, .F.)
  RETURN
ENDIF
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId

  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
  loProgress.Percent = 0
  loProgress.Description = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  
  LOCAL lcCurrentProcedure
  *! B609901,1 MMT 04/30/2012 Fix the invoice form program to work from RB[Start]
*!*	  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
*!*	  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*!*	  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID),ClientID
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  SET DEFAULT TO &lcCurrentProcedure.
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment
  
  oAriaEnvironment.ProcessID = "ARPINV"
  
  SET DEFAULT TO &lcCurrentProcedure.
  *! B609901,1 MMT 04/30/2012 Fix the invoice form program to work from RB[End]
  =gfOpenFile('INVHDR','INVHDR')
  =gfOpenFile('INVLINE','INVLINE')
  =gfOpenFile('STYLE','STYLE') 
  =gfOpenFile('SCALE','SCALE') 
  =gfOpenFile('ORDHDR','ORDHDR') 
  =gfOpenFile('SPCK_LIN','SPCKLINS') 
  =gfOpenFile('CODES','CODES')       
  =gfOpenFile('NOTEPAD','NOTEPAD')       
  =gfOpenFile('SPCK_HDR','SKU_STYLE')         
  =gfOpenFile('ARINSTMD','ARINSTMD')           
  =gfOpenFile('CONSINVL','CONSINVL')             
  =gfOpenFile('CONSINVH','CONSINVH')               
  =gfOpenFile('SKUTMPL','SKUTMPL')                 
  =gfOpenFile('OBJLINK','OBJLNKTY')   
  =gfOpenFile('OBJECTS',"OBJECTID")
  =gfOpenFile('SYCCOMP','CCOMP_ID')
  =gfOpenFile('Customer','Customer')
    oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
    oAriaEnvironment.Report.gcAct_Appl = 'AR'

    PUBLIC gcAct_Appl 
    gcAct_Appl = 'AR'
    oariaenvironment.activeModuleID = 'AR' 

    IF LEFT(gcDevice, 7) = "PRINTER"
      oAriaEnvironment.gcDevice = "PRINTER"
    ELSE
      oAriaEnvironment.gcDevice = "FILE"
    ENDIF
     oAriaEnvironment.Report.cCROrientation = 'P'

ELSE
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
  loogScroll.cCROrientation = 'P'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ENDIF
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF
ENDIF
PRIVATE lcskpexpr
llPrinter = .F.
*-- Open the company table remotly ..... BEGIN
LOCAL lnRemoteResult, lcSelectCommand
lcSelectCommand = [SELECT * FROM SYCCOMP WHERE CCOMP_ID = '] + oAriaApplication.ActiveCompanyID+ [']
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
  lnRemoteResult = loOGScroll.SQLExecute("SYCCOMP", lcSelectCommand,"","SYCCOMP","",;
       oAriaApplication.SystemConnectionString,3,"")
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ELSE
  lnRemoteResult = oariaenvironment.remotesystemdata.execute(lcSelectCommand ,'',"syccomp","",oariaenvironment.SystemConnectionString,3,"",SET("Datasession"))
ENDIF
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
IF lnRemoteResult >= 1 
  SELECT SYCCOMP
  LOCATE 
  IF EOF()
    RETURN .F.
  ENDIF 
ELSE
  RETURN .F.
ENDIF 
*-- Open the company table remotly ..... END

llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)
lcCurrPost = "LEFT "                              && Default Value.
IF llMulCurr
  *-- MAB (New in version 4) ... BEGIN
  *-- [Use system files remotly.]
  *IF !USED('SYCINT')
  *  =gfOpenFile(gcsyshome + "SYCINT" , "CCONTCODE" , 'SH')
  *ENDIF
  *=SEEK(SycComp.cCont_Code,'SYCINT')
  *lcCurrPost = SycInt.cCurrency

  lcSelectCommand = [SELECT cCurrency, cCurrencyI FROM SYCINT WHERE cCurrCode = '] + SycComp.cCont_Code + [']
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  IF TYPE('lcXMLFileName') = 'C'
    lnRemoteResult = oariaenvironment.remotesystemdata.execute(lcSelectCommand ,'',;
          "SYCINT","",oariaenvironment.SystemConnectionString,3,"",SET("Datasession"))
  ELSE
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    lnRemoteResult = loOGScroll.SQLExecute("SYCINT", lcSelectCommand,"","SYCINT","",;
       oAriaApplication.SystemConnectionString,3,"")
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  ENDIF 
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]

  IF lnRemoteResult >= 1 
    SELECT SYCINT
    LOCATE 
    IF FOUND()
      lcCurrPost = SycInt.cCurrency
    ENDIF 
  ENDIF 
  *-- MAB (New in version 4) ... END

  IF !USED('SYCCURR')
    =gfOpenFile(gcsyshome + "SYCCURR" , "CCURRCODE" , 'SH')
  ENDIF
ENDIF

llarpinv = .T.            && llarpinv it will be a commen variable.

lcScalCont = 0          &&  Scale Counter
lcScaleExp = ""         &&  Scale Expression

lcScalfl = SELECT(0)
SELECT SCALE
SCAN FOR Type+Scale+Prepak = 'S' AND lcScaleExp # SCALE.SCALE
  lcScalCont = lcScalCont + 1
  lcScaleExp = SCALE.SCALE
ENDSCAN
SELECT(lcScalfl)

DIMENSION laScale[lcScalCont,9]

STORE SPACE(0) TO laScale
lcOldInv = SPACE(6)

llPrntInst = TYPE('lcPrntInst') = 'C' .AND. lcPrntInst='Y'
lcTime = TIME()          && Variable to hold the Time
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
STORE 0 TO TAXABLE , NTAXABLE , lnTaxable , lnLines
STORE '' TO lcStrToPrn
STORE .F. TO llPrtSku , llLineFlag , llInvFlag
STORE '' TO lcNotes , lcNotesTtl ,  lcLNoteTtl , lcLNotes ,lcINoteTtl , lcINotes
STORE .F. TO llLPrtNote , llIPrtNote

lnMajor  = LEN(gfItemMask('PM'))
lnNMajor = LEN(gfItemMask('PN'))
lcDCCode = ''

lcPrgName  = lcFormName
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
llIsAparel = lfIsApparl(@lcPrgName)
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ELSE
  *! B609470,1 MMT 11/29/2010 Custom Invoice Forms does not work from the request builder[Start]
  *llIsAparel = .F.
  lcFormName = oAriaEnvironment.REPORT.GetForm('ARPINV')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
  *! B609470,1 MMT 11/29/2010 Custom Invoice Forms does not work from the request builder[End]  
Endif
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
DIME laSku[8]
STORE '' TO laSku
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
lcShipVia = ''         && Variable to hold the Ship Via Description
lcTerms = ''           && Variable to hold the Terms Description
lcSpkLin = ''          && Variable to hold the Size # of the SKU
llEndGroup = .F.       && Flag to know if we are at the end of the Group
lcDivLName = ''        && Variable to hold the Division long name

lcFacName  = ''
*B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [BEGIN]
STORE '' TO lcFacTel
*B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [END]

lcTmpDbt = gfTempName()

lcInvPrtUp = gfTempName()
CREATE CURSOR (lcInvPrtUp) (Invoice C(6))
INDEX ON Invoice TAG Invoice of (gcWorkDir+lcInvPrtUp)
*! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [Start]
*DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laFactor[5,1]
DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laFactor[6,1]
*! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [End]
laFactor = ''

laCompAdd = ''          && Array to hold the Company address
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*=gfOpenFile(gcsyshome+'SYCCOMP',gcsyshome+'CCOMP_id','SH')

=gfOpenFile(gcsyshome+'SYCFACT',gcsyshome+'cfaccode','SH')
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH', @LCInvLine_A, .T.)

llTax      = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')
lcDunsNo  = gfGetMemVar('XDUNS')
llExtSize = gfGetMemVar('M_USEEXSSC')

*N130335,1 TNA 03/29/2006 (Begin) Add the tax reference.
lcTaxRefDs = ALLTRIM(gfGetMemVar('M_TAX_REFE'))
*N130335,1 TNA 03/29/2006 (End)

SELECT SYCCOMP
*SEEK gcAct_Comp
lcCompName = cCom_Name             && Variable to hold the Company Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax = cCom_Fax               && Variable to hold the Company Fax
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
*-hfk, 08/29/2004, modified it for proper phone mask
*-laCompAdd[6] = TRANSFORM(lcCompPhon , lcPhonPict)
laCompAdd[6] = TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
*-hfk
*B039381,1 HMA 06/06/2005 preview fax &phone in the right format(Fix in Prg&report Layout)  [Begin]
*lcCompFax = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
lcCompFax = TRANSFORM(lcCompFax , '@R ' +  lcPhonPict)  && Fax No. Pic
*B039381,1 HMA 06/06/2005 preview fax &phone in the right format(Fix in Prg&report Layout) [End]

=lfAdrShift('laCompAdd')

SET ORDER TO TAG INVLINE IN INVLINE
CREATE CURSOR (lcTmpDbt) (CFILE_NUM C(1))
IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    INSERT INTO   (lcTmpDbt) VALUES('1')
    IF llRpInvNot
      INSERT INTO (lcTmpDbt) VALUES('2')
    ENDIF
    IF llPrntInst
      INSERT INTO (lcTmpDbt) VALUES('3')
    ENDIF
    SELECT (lcTmpDbt)
    *! B609901,1 MMT 07/03/2012 Fix Bug of Access is Denied when Printing Invoice [Start]
    *INDEX ON CFILE_NUM TAG CFILE_NUM of (lcTmpDbt)
    INDEX ON CFILE_NUM TAG CFILE_NUM of (oAriaApplication.WorkDir+lcTmpDbt)
    *! B609901,1 MMT 07/03/2012 Fix Bug of Access is Denied when Printing Invoice [End]

    SELECT INVHDR
    SET RELATION TO '' INTO (lcTmpDbt)
    SELECT (lcTmpDbt)
    SET RELATION TO IIF(CFILE_NUM = '3', INVHDR.Invoice, '*') INTO ARINSTMD
    SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE

  ELSE
    SELECT INVHDR
    SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
  ENDIF
ELSE
  SELECT INVHDR
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
SET ORDER TO TAG STYLE IN STYLE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE ,INVLINE.Style) INTO STYLE
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET ORDER TO TAG SCALE IN SCALE
SET RELATION TO 'S' + Scale INTO SCALE

SELECT CUSTOMER
SET ORDER TO TAG CUSTOMER
SELECT INVHDR

*-- #B128207,1 HFK, 05/25/2005 [Start]
*!*  SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
*!*                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO IIF(EMPTY(Store) OR Store = "********",IIF (EMPTY(dist_ctr),'M' + Account,'S' + Account + dist_ctr),'S' + Account + Store) INTO CUSTOMER ADDITIVE
*-- #B128207,1 HFK, 05/25/2005 [End]

IF !llIsAparel
  SET SKIP TO INVLINE , SPCK_LIN
ENDIF
SELECT INVHDR
SET RELATION TO 'O' + Invhdr.order INTO Ordhdr ADDITIVE

IF llPrntInst .OR. llRpInvNot
  IF !llIsAparel
    SET SKIP TO (lcTmpDbt) , INVLINE , ARINSTMD
  ENDIF
ELSE
  IF !llIsAparel
    SET SKIP TO INVLINE
  ENDIF
ENDIF
lcRpExp = lcRpExp + IIF (!EMPTY(lcRpExp) .AND. lcFactrInv <> 'B' ,;
          ' .AND. ' , '') + IIF(lcFactrInv = 'F' , '!EMPTY(INVHDR.cFacCode)' ,;
          IIF(lcFactrInv = 'N' , 'EMPTY(INVHDR.cFacCode)' , ''))

lcRpExp = lcRpExp + " AND INVHDR.STATUS <> 'V'"

lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
llPrntComp = lcPrnComp  = "Y"
llPrnFact  = IIF(TYPE('llFactor ') = 'L', llFactor, .F.)

llLineNote = llRpInvNot
llNotePad  = llRpInvLNt
SELECT INVHDR

lnFrmPos=AT("ARPINV",lcFormName)+6
*!*  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
lcOptProg = ""
*!*  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
IF !lfGetPath()
  * Message "This form does not exist.Please check your company information settings."
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    =gfModalGen('TRM40170B00000','DIALOG','This form')
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  ENDIF
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
  RETURN
ENDIF

*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') = 'C'
  *! B609470,1 MMT 11/29/2010 Custom Invoice Forms does not work from the request builder[Start]
  IF !EMPTY(oAriaEnvironment.Report.lcOptProg)
    lcPrgName = oAriaEnvironment.Report.lcOptProg
  ENDIF
  *! B609470,1 MMT 11/29/2010 Custom Invoice Forms does not work from the request builder[End]   
  IF oAriaEnvironment.multiinst AND  FILE(oAriaEnvironment.clientreporthome+lcPrgName+'.FXP')
    *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[Start]
    *=lfOptProg(oAriaEnvironment.clientreporthome+lcPrgName+'.FXP')    
    =lfOptProg(oAriaEnvironment.clientreporthome+lcPrgName)
    *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[End]    
  ELSE
    IF FILE(oAriaEnvironment.ReportHome+lcPrgName+'.FXP')
      *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[Start]    
      *=lfOptProg(oAriaEnvironment.ReportHome+lcPrgName+'.FXP')      
      =lfOptProg(oAriaEnvironment.ReportHome+lcPrgName)
      *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[End]      
    ENDIF    
  ENDIF   
ELSE 
  =lfOptProg()
ENDIF 
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]

IF llIsAparel
  IF FILE(lcPrgName +'.FXP')
    DO EVAL('lcPrgName')
  ELSE
    * Message "Form 'XX' does not exist.Please check your company information settings."
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
    IF TYPE('lcXMLFileName') <> 'C'
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcPrgName,lnFrmPos) + "'" )
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
    ENDIF
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    RETURN
  ENDIF
  *E128427,1 MMT 06/20/2005 , Fix bug of error after report preview in text format[Start]  
  *IF !llNoRec 
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
  IF !llNoRec AND !loOGScroll.llCrystal
  *E128427,1 MMT 06/20/2005 , Fix bug of error after report preview in text format[End]  
    DO ENDREPORT
    llReturn = .T.
  ENDIF
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  ENDIF
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
ELSE
  lcSavPAdv = _PADVANCE
  IF !lfGetFrx()
    * Message "Form 'XX' does not exist.Please check your company information settings."
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
    IF TYPE('lcXMLFileName') <> 'C'
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
      =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcFormName,lnFrmPos) + "'" )
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
    ENDIF
    *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[END]
    RETURN
  ENDIF
*- #B038981,3 HFK, 02/01/2005 [Start]
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ELSE
  lcRpPrSt =   IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)
ENDIF
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
*- #B038981,3 HFK, 02/01/2005 [End]
*! E303247,1 MMT 09/10/2012 Single size invoice and Packing list forms[T20120821.0021][Start]
lfGetColorLen()
*! E303247,1 MMT 09/10/2012 Single size invoice and Packing list forms[T20120821.0021][END]
*B608260,1 MMT 09/09/2007 Fix bug of wrong description of the alternative style[Start]
IF !USED('ORdLINE')
  =gfOpenFile(oAriaApplication.DataDir+'ORdLINE',oAriaApplication.DataDir+'ORdLINE','SH')
ENDIF
*B608260,1 MMT 09/09/2007 Fix bug of wrong description of the alternative style[End]

*B608361,1 WAM 11/28/2007 Fix wrong message "No record to display"
SELECT INVHDR
*B608361,1 WAM 11/28/2007 (End)

  *! B610717,1 HES 04/22/2014 Allow the invoice details to be exported to excel beside the header values [START]
  IF "EXCEL" $ loogScroll.cTextRepType
    lcExpTemp = gfTempName()
    DIMENSION laFileStru[1,1]
    =AFIELDS(laFileStru)
   
    lnFileStru = ALEN(laFileStru , 1)    && array rows
    DIMENSION laFileStru[lnFileStru + 6 , ALEN(laFileStru , 2)]
    I = 1
    
    laFileStru[lnFileStru + 1 , 1]  = 'Vendor'
    laFileStru[lnFileStru + 1 , 2]  = 'C'
    laFileStru[lnFileStru + 1 , 3]  = 15

    laFileStru[lnFileStru + 2 , 1]  = 'Terms'
    laFileStru[lnFileStru + 2 , 2]  = 'C'
    laFileStru[lnFileStru + 2 , 3]  = 30

    laFileStru[lnFileStru + 3 , 1]  = 'Shipvia_Ds'
    laFileStru[lnFileStru + 3 , 2]  = 'C'
    laFileStru[lnFileStru + 3 , 3]  = 30
    
    laFileStru[lnFileStru + 4 , 1]  = 'STYLE'
    laFileStru[lnFileStru + 4 , 2]  = 'C'
    laFileStru[lnFileStru + 4 , 3]  = 19   
    
    laFileStru[lnFileStru + 5 , 1]  = 'ALTSTYLE'
    laFileStru[lnFileStru + 5 , 2]  = 'C'
    laFileStru[lnFileStru + 5 , 3]  = 19      
    
    laFileStru[lnFileStru + 6 , 1]  = 'SCALE'
    laFileStru[lnFileStru + 6 , 2]  = 'C'
    laFileStru[lnFileStru + 6 , 3]  = 3      

    FOR I = 1 TO 6
      laFileStru[lnFileStru + I , 4]  = 0
      laFileStru[lnFileStru + I , 5]  = .F.
      laFileStru[lnFileStru + I , 6]  = .F.
      laFileStru[lnFileStru + I , 7]  = ""
      laFileStru[lnFileStru + I , 8]  = ""
      laFileStru[lnFileStru + I , 9]  = ""
      laFileStru[lnFileStru + I , 10] = ""
      laFileStru[lnFileStru + I , 11] = ""
      laFileStru[lnFileStru + I , 12] = ""
      laFileStru[lnFileStru + I , 13] = ""
      laFileStru[lnFileStru + I , 14] = ""
      laFileStru[lnFileStru + I , 15] = ""
      laFileStru[lnFileStru + I , 16] = ""
      laFileStru[lnFileStru + I , 17] = .F.
      laFileStru[lnFileStru + I , 18] = .F.
    ENDFOR

    lnFileStru = ALEN(laFileStru , 1)   && array rows
    DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
    I = 1
    FOR I = 1 TO 8
      laFileStru[lnFileStru + I , 1]  = 'SIZE'+ALLTRIM(STR(I))
      laFileStru[lnFileStru + I , 2]  = 'C'
      laFileStru[lnFileStru + I , 3]  = 5
      laFileStru[lnFileStru + I , 4]  = 0
      laFileStru[lnFileStru + I , 5]  = .F.
      laFileStru[lnFileStru + I , 6]  = .F.
      laFileStru[lnFileStru + I , 7]  = ""
      laFileStru[lnFileStru + I , 8]  = ""
      laFileStru[lnFileStru + I , 9]  = ""
      laFileStru[lnFileStru + I , 10] = ""
      laFileStru[lnFileStru + I , 11] = ""
      laFileStru[lnFileStru + I , 12] = ""
      laFileStru[lnFileStru + I , 13] = ""
      laFileStru[lnFileStru + I , 14] = ""
      laFileStru[lnFileStru + I , 15] = ""
      laFileStru[lnFileStru + I , 16] = ""
      laFileStru[lnFileStru + I , 17] = .F.
      laFileStru[lnFileStru + I , 18] = .F.
    ENDFOR

    lnFileStru = ALEN(laFileStru , 1)   && array rows
    DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
    I = 1
    FOR I = 1 TO 8
      laFileStru[lnFileStru + I , 1]  = 'QTY'+ALLTRIM(STR(I))
      laFileStru[lnFileStru + I , 2]  = 'N'
      laFileStru[lnFileStru + I , 3]  = 6
      laFileStru[lnFileStru + I , 4]  = 0
      laFileStru[lnFileStru + I , 5]  = .F.
      laFileStru[lnFileStru + I , 6]  = .F.
      laFileStru[lnFileStru + I , 7]  = ""
      laFileStru[lnFileStru + I , 8]  = ""
      laFileStru[lnFileStru + I , 9]  = ""
      laFileStru[lnFileStru + I , 10] = ""
      laFileStru[lnFileStru + I , 11] = ""
      laFileStru[lnFileStru + I , 12] = ""
      laFileStru[lnFileStru + I , 13] = ""
      laFileStru[lnFileStru + I , 14] = ""
      laFileStru[lnFileStru + I , 15] = ""
      laFileStru[lnFileStru + I , 16] = ""
      laFileStru[lnFileStru + I , 17] = .F.
      laFileStru[lnFileStru + I , 18] = .F.
    ENDFOR
    
    lnFileStru = ALEN(laFileStru , 1)   && array rows
    DIMENSION laFileStru[lnFileStru + 1 , ALEN(laFileStru , 2)]
    lnFileStru = lnFileStru + 1
    
    laFileStru[lnFileStru , 1]  = 'TotQTY'
    laFileStru[lnFileStru , 2]  = 'N'
    laFileStru[lnFileStru , 3]  = 6   
    laFileStru[lnFileStru , 4]  = 0
    laFileStru[lnFileStru , 5]  = .F.
    laFileStru[lnFileStru , 6]  = .F.
    laFileStru[lnFileStru , 7]  = ""
    laFileStru[lnFileStru , 8]  = ""
    laFileStru[lnFileStru , 9]  = ""
    laFileStru[lnFileStru , 10] = ""
    laFileStru[lnFileStru , 11] = ""
    laFileStru[lnFileStru , 12] = ""
    laFileStru[lnFileStru , 13] = ""
    laFileStru[lnFileStru , 14] = ""
    laFileStru[lnFileStru , 15] = ""
    laFileStru[lnFileStru , 16] = ""
    laFileStru[lnFileStru , 17] = .F.
    laFileStru[lnFileStru , 18] = .F.     

    lnFileStru = ALEN(laFileStru , 1)   && array rows
    DIMENSION laFileStru[lnFileStru + 8 , ALEN(laFileStru , 2)]
    I = 1
    FOR I = 1 TO 8
      laFileStru[lnFileStru + I , 1]  = 'SKU'+ALLTRIM(STR(I))
      laFileStru[lnFileStru + I , 2]  = 'C'
      laFileStru[lnFileStru + I , 3]  = 16
      laFileStru[lnFileStru + I , 4]  = 0
      laFileStru[lnFileStru + I , 4]  = 0
      laFileStru[lnFileStru + I , 5]  = .F.
      laFileStru[lnFileStru + I , 6]  = .F.
      laFileStru[lnFileStru + I , 7]  = ""
      laFileStru[lnFileStru + I , 8]  = ""
      laFileStru[lnFileStru + I , 9]  = ""
      laFileStru[lnFileStru + I , 10] = ""
      laFileStru[lnFileStru + I , 11] = ""
      laFileStru[lnFileStru + I , 12] = ""
      laFileStru[lnFileStru + I , 13] = ""
      laFileStru[lnFileStru + I , 14] = ""
      laFileStru[lnFileStru + I , 15] = ""
      laFileStru[lnFileStru + I , 16] = ""
      laFileStru[lnFileStru + I , 17] = .F.
      laFileStru[lnFileStru + I , 18] = .F.
    ENDFOR

    CREATE CURSOR (lcExpTemp) FROM ARRAY laFileStru
    *CREATE TABLE ADDBS(oAriaApplication.WorkDir)+ lcExpTemp+'.dbf' FROM ARRAY laFileStru
    SELECT (lcExpTemp)
    INDEX ON INVOICE+STYLE TAG INVSTYLE
    SET ORDER TO 
    

    SELECT INVHDR
    lcGenTemp = gfTempName()
    COPY TO ADDBS(oAriaApplication.WorkDir)+lcGenTemp+'.dbf' FOR &lcRpExp
    
    SELECT (lcExpTemp)
    APPEND FROM ADDBS(oAriaApplication.WorkDir)+lcGenTemp+'.dbf'
    DELETE FILE ADDBS(oAriaApplication.WorkDir)+lcGenTemp+'.dbf'
    *! B611011,1 MMT 06/04/2015 Error while exporting invoice form to Excel[T20150601.0001][Start]
    TRY
    *! B611011,1 MMT 06/04/2015 Error while exporting invoice form to Excel[T20150601.0001][End]
   
    SELECT (lcExpTemp)
    lcOldInv = ""
    SCAN
      lcInv = Invoice

      SELECT INVLINE
      =SEEK(lcInv)
      SCAN REST WHILE Invoice+STR(LINENO,6) = lcInv
        
        SELECT (lcExpTemp)
        REPLACE TERMS WITH lcTerms
        REPLACE VENDOR WITH IIF(!EMPTY(Ordhdr.INT_VEND) ,Ordhdr.INT_VEND, CUSTOMER.CCUSVEND)
        REPLACE SHIPVIA_DS WITH lcShipVia
        
        SELECT INVLINE
        SCATTER FIELDS ALTSTYLE,STYLE,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TotQty MEMVAR
        SELECT (lcExpTemp)
        GATHER MEMVAR
        
        SELECT SPCK_LIN
        IF !EMPTY(PACK_ID)
          lnI = 1
          lcSkuTmpl=IIF(!EMPTY(CUSTOMER.SkuTmpl),CUSTOMER.SkuTmpl,'DEF')
          IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
            lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
            lnDime2 = SkuTmpl.Len4
          ELSE
            lnDime1 = 8  &&Default
            lnDime2 = 8  &&Default
          ENDIF

          IF llExtSize
            =SEEK(TYPE+Account+SUBSTR(STYLE,1,LEN(ALLTRIM(STYLE))-4),'SPCK_HDR')
          ELSE
            =SEEK(TYPE+Account+STYLE,'SPCK_HDR')
          ENDIF

          lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))

          SCAN WHILE TYPE+Account+STYLE = 'S'+INVLINE.Account+INVLINE.STYLE .AND. lnI < 9
            FOR lnX=1 TO 8
              Z=STR(lnX,1)
              IF QTY&Z > 0
                SELECT (lcExpTemp)
                REPLACE sku&Z WITH SUBSTR(PACK_ID,lnDime1+1,lnDime2)
                EXIT
              ENDIF
            ENDFOR
            lnI = lnI + 1
          ENDSCAN
        ENDIF
        SELECT (lcExpTemp)
        REPLACE SCALE with SCALE.SCALE 
        FOR I=1 TO 8
          lcI = STR(I,1)
          REPLACE SIZE&lcI WITH SCALE.SZ&lcI
        ENDFOR 
        
        SELECT (lcExpTemp)
          SKIP
      ENDSCAN
    ENDSCAN
	*! B611011,1 MMT 06/04/2015 Error while exporting invoice form to Excel[T20150601.0001][Start]
	CATCH
	ENDTRY
	*! B611011,1 MMT 06/04/2015 Error while exporting invoice form to Excel[T20150601.0001][End]
    
    SELECT (lcExpTemp)
    SET ORDER TO INVSTYLE
    lcRpExp = ".T."
  ENDIF
  *! B610717,1 HES 04/22/2014 Allow the invoice details to be exported to excel beside the header values [END  ]

IF llarpinv 
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  ELSE
    SELECT INVHDR
    Set FILTER TO &lcRpExp
    LOCATE 
    IF EOF()
      *! B609470,2 MMT 12/13/2010 Custom Invoice Forms does not work from the request builder[Start]
      SET FILTER TO 
      *! B609470,2 MMT 12/13/2010 Custom Invoice Forms does not work from the request builder[End]      
      RETURN 
    ENDIF  
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

    IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
      loOGScroll   = oAriaEnvironment.report
      oAriaEnvironment.report.OGLastForm = lcFormName
      oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.Description = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF
    *! B609470,2 MMT 12/13/2010 Custom Invoice Forms does not work from the request builder[Start]
    SELECT INVHDR
    SET FILTER TO 
    *! B609470,2 MMT 12/13/2010 Custom Invoice Forms does not work from the request builder[End]      

  ENDIF 
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
ENDIF
  _PADVANCE = lcSavPAdv
ENDIF
*- #B038981,3 HFK, 02/01/2005 [Start]
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
*- #B038981,3 HFK, 02/01/2005 [End]


*- #038981, HFK, 01/27/2005, Add Condition [Start]
*!*  SELECT (lcInvPrtUp)
*!*  SCAN
*!*    SELECT INVHDR
*!*    =SEEK(EVAL(lcInvPrtUp+'.INVOICE'))
*!*    REPLACE PrtFlag WITH 'P'
*!*  ENDSCAN

*-- HFK, 03/16/2005 [Start]
*-- IF oAriaApplication.gcDevice = 'PRINTER' 
*--IF oAriaApplication.gcDevice = 'PRINTER' .AND. loOGScroll.llPrinted

*B608106,1 SSH 5-31-2007 Update PRINTED flag.
*IF llPrinter 
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
IF loOgScroll.ll2Printer
*B608106,1 SSH 5-31-2007 Commented out

*--MMT
*AND loOGScroll.llOGFltCh
*--MTM
*-- HFK, 03/16/2005 [End]
  SELECT (lcInvPrtUp)
  SCAN
    SELECT INVHDR
    =SEEK(EVAL(lcInvPrtUp+'.INVOICE'))
    REPLACE PrtFlag WITH 'P'
  ENDSCAN
  *E128427,1 MMT 06/20/2005 , fix bug of error after printing from preview [Start]
  loOGScroll.llPrinted = .F.
  *E128427,1 MMT 06/20/2005 , fix bug of error after printing from preview [End]
  
ENDIF  
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ELSE
  SELECT (lcInvPrtUp)
  SCAN
    SELECT INVHDR
    =SEEK(EVAL(lcInvPrtUp+'.INVOICE'))
    REPLACE PrtFlag WITH 'P'
  ENDSCAN
  
  *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[Start]
  SELECT INVHDR
  =gfTableUpdate()
  *! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[End]

ENDIF 
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
*- #038981,1 HFK, 01/27/2005, Add Condition [End]

*- #B038981,2 HFK, 01/30/2005 [Start]
*- USE IN (lcInvPrtUp)
IF USED('&lcInvPrtUp')
  USE IN (lcInvPrtUp)
ENDIF 
*- #B038981,2 HFK, 01/30/2005 [End]
SELECT INVHDR
SET SKIP TO
SET RELATION TO
SELECT CUSTOMER
SET RELATION TO
SELECT STYLE
SET RELATION TO
SELECT INVLINE
SET RELATION TO
IF USED(lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO
  USE
ENDIF
ERASE (gcWorkDir + (lcTmpDbt)+'.*')
*- hfk, 08/29/2004, added this condition in order to not enter crystal when 
*- there are no records IN optional program
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
IF loOGScroll.llCrystal .AND. llNoRec
  RETURN .F.
ENDIF
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
ENDIF
*! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
*-hfk
*-- END OF MAIN PROGRAM CODE.

*!*************************************************************
*! Name      : lfOptMsg
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
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
*
FUNCTION lfOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 2nd. line Variable

laOptMsg[1,2] = 75                && Line length

IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)

  IF FILE(gcDataDir+lcTypInv+'.MEM')
    RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
  ENDIF

  =gfOptMsg('laOptMsg')
  *- hfk, 08/30/2004, if optional message was not empty and you emptied it, it won't
  *- catch this change and will print it, move this line after the endif statement in order 
  *- to save the optional message whether empty or not into the memo
  *!*    SET MEMOWIDTH TO 75              && the length of the memo field.
  *!*    SAVE TO gcDataDir+lcTypInv+'.MEM' ALL LIKE lcRpMsg*

ELSE
  =gfOptMsg('laOptMsg')
ENDIF
SET MEMOWIDTH TO 75              && the length of the memo field.
SAVE TO gcDataDir+lcTypInv+'.MEM' ALL LIKE lcRpMsg*


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
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
*
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

*!*  *!*************************************************************
*!*  *! Name      : lfvAccount
*!*  *! Developer : Haytham El_Sheltawi
*!*  *! Date      : 01/11/1998
*!*  *! Purpose   : Validation function for the Account field
*!*  *!*************************************************************
*!*  *! Called from : Account field [Option Grid]
*!*  *!*************************************************************
*!*  *! Calls       : CusBrowM()
*!*  *!*************************************************************
*!*  *! Passed Parameters : None
*!*  *!*************************************************************
*!*  *! Return      : None
*!*  *!*************************************************************
*!*  *
*!*  FUNCTION lfvAccount

*!*  PRIVATE lcObjName , lcObjVal , llObjRet

*!*  lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
*!*  lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*!*  *IF The user want to Browse or if the Account he entered is not in the file
*!*  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
*!*    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
*!*    lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
*!*    &lcObjName = lcObjVal
*!*  ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvNo
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))

  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"

  lcFile_Ttl = 'Receivable invoices'

  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]

  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && Else
    &lcObjName = laOldVal
  ENDIF    && End of IF

  SET RELATION OFF INTO CUSTOMER
ENDIF    && End of IF

*IF The INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF

*IF The CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 6

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Terms
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF
lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
*! B610864,1 MMT 09/24/2014 Invoice Form is too slow if option print invoice notes is set to NO[T20140703.0016][Start]
*COUNT TO lnLines WHILE INVLINE.INVOICE = INVHDR.INVOICE
*! B610864,1 MMT 09/24/2014 Invoice Form is too slow if option print invoice notes is set to NO[T20140703.0016][End]
IF lnInvLnRec > 0
  GO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN INVHDR
ENDIF

*B608441,1 WAM 02/18/2008 Reset factor name and address for each invoice printed.
*! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [Start]
*DECLARE laFactor[5,1]
DECLARE laFactor[6,1]
*! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [End]
STORE '' TO laFactor,lcFacName   
*B608441,1 WAM 02/18/2008 (End)

*B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [BEGIN]
STORE '' TO lcFacTel
*B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [END]


*-- Fill laFactor with factor address
IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [BEGIN]
    lcFacTel    = ALLTRIM(SYCFACT.cphoneno)
    *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [END]
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [Start]
    laFactor[6] = TRANSFORM(lcFacTel, '@R ' + lcPhonPict)
    *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [End]
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo is .T.
SELECT INVHDR
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else

  lnCUSRec = 0
  *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
  *IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
  *N000592,1 HBG [End]
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

  SELECT INVHDR

IF lnTmpDbt <> 0
  GO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN INVLINE
  IF !EOF('INVLINE')
    SKIP IN INVLINE
  ENDIF
ELSE
  GO lnInvLnRec IN INVLINE
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF
RETURN ''

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the INVHDR file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ARPINVA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup
llEndGroup = .T.

*If the Device is not Screen
*!*	IF oAriaApplication.gcDevice <> 'SCREEN'
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
*!*	ENDIF    && End of IF
RETURN ''

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Haytham El_Sheltawi
*! Date      : 08/19/1998
*! Purpose   : Set function for the invoice number option in case
*!             of In Range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfInvSet

PARAMETERS lcParm
IF lcParm = 'S'
  lcRpOld = lcRpPrSt
  lcInvHdTag = ORDER('INVHDR')
  lcCstmrTag = ORDER('CUSTOMER')
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG CUSTOMER IN CUSTOMER

ELSE

  *IF The INVHDR file did not have an active index
  IF EMPTY(lcInvHdTag)
    SET ORDER TO 0 IN INVHDR
  ELSE    && Else
    SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  ENDIF    && End of IF

  *IF The CUSTOMER file did not have an active index
  IF EMPTY(lcCstmrTag)
    SET ORDER TO 0 IN CUSTOMER
  ELSE    && Else
    SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
  ENDIF    && End of IF
  lcRpPrSt = lcRpOld
ENDIF
*------------------------
FUNCTION lfCalcTax
lnTaxable = lnTaxable + (INVHDR.TAX_RATE * INVlINE.PRICE)
RETURN lnTaxable

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Timour Abdalla Khalil.
*! Date : 07/20/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
FUNCTION lfPrtSku
PRIVATE lnPrevAl
STORE ' ' TO lcStrToPrn

STORE '' TO laSku

IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF

  IF llExtSize
    =SEEK(TYPE+ACCOUNT+SUBSTR(Style,1,LEN(ALLTRIM(Style))-4),'SPCK_HDR')
  ELSE
    =SEEK(TYPE+ACCOUNT+STYLE,'SPCK_HDR')
  ENDIF

  lnDime1 = MIN(lnDime1,LEN(ALLTRIM(SPCK_HDR.SKU)))

   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku[lnX]=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
  =SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()
      lcI = STR(lnI,1)
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
  ENDDO
ENDIF
SELECT (lnPrevAl)
IF EMPTY(lcStrToPrn)
  STORE '' TO laSku
  llPrtSku = .F.
  RETURN .F.
ELSE
  llPrtSku = .T.
  RETURN .T.
ENDIF


*!*************************************************************
*! Name      : lfvSkuPck
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/1999
*! Purpose   : Function to valid the shus option in the option grid.
*!*************************************************************
*! Called from : Option Grid    [Print sku/pack]
*!*************************************************************
*! Refer to C101385
*!*************************************************************

FUNCTION lfvSkuPck

ClearRead()
llRpSkuSize = (llRpPrtSku)


*!*************************************************************
*! Name      : lfSuprSKU
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/24/1999
*! Purpose   : Function to determine wheather the skus option in
*!           : the option grid will appear or not .
*!*************************************************************
*! Called from : Suppress field in the FORMCDDT file.
*!*************************************************************
*! Refer to C101385
*!*************************************************************

FUNCTION lfSuprSKU

*-- If we do not want the SKUs options to be browsed in the option grid
*-- Flase the two SKUs variables to be used in the Forms programs.
IF !(TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')
  STORE .F. TO llRpPrtSku,llRpSkuSize
ENDIF
RETURN (TYPE('lcPrntSKU') = 'C' .AND. lcPrntSKU='Y')

*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 01/28/1999
*! Purpose   : Function to Evaluate Notes To be Printed
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************


FUNCTION lfPrtNotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
       .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')

    FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
      IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
        lcNotes    = ALLTRIM(NOTEPAD.mNotes)
      ENDIF
    ENDFOR
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = ALLTRIM(NOTEPAD.mNotes)
  CASE llRpInvLNt .AND. !EMPTY(Invline.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(Invline.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(Invline.note_mem)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate Line Notes Only To be Printed
*! Returns   : Printed Notes
*!           Due to E301138,1
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************

FUNCTION lfPrtLNotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvLNt .AND. !EMPTY(Invline.note_mem) .AND. LEFT(ALLTRIM(STRTRAN(Invline.note_mem,CHR(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Line Notes'
    lcNotes    = ALLTRIM(Invline.note_mem)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)


*!*************************************************************
*! Name      : lfPrtLNotes
*! Developer : Hossam Eldin Mahmoud El Etreby(HDM)
*! Date      : 02/15/1999
*! Purpose   : Function to Evaluate Invoice Notes Only To be Printed
*! Returns   : Printed Notes
*!           Due to E301138,1
*!*************************************************************
*! Called from : ARPINVA.FRX (Dos Format)
*!*************************************************************

FUNCTION lfPrtINotes
PRIVATE lcReturn

DO CASE
  CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2' .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,(13)+CHR(10),' ')),1)<>'*'
    lcNotesTtl = 'Invoice Notes'
    lcNotes    = SUBSTR(ALLTRIM(NOTEPAD.mNotes),1,5)

  OTHERWISE
    STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)


*!*************************************************************
*! Name        : lfSclSze
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get the scale and the size
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C101651
*!*************************************************************
*! Example     : lfSclSze
*!*************************************************************
FUNCTION lfSclSze

PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv   = INVHDR.INVOICE
lcAlias = ALIAS()
SELECT InvLine
lcKey = Invoice+STR(LineNo,6)
SEEK InvHdr.Invoice
lnSclCnt = 0

STORE SPACE(0) TO laScale
SCAN WHILE Invoice = InvHdr.Invoice  .AND. lnSclCnt < lcScalCont + 1

  IF ASCAN(laScale,INVLINE.SCALE) = 0 .AND. SEEK('S'+INVLINE.SCALE,'SCALE')
      lnSclCnt = lnSclCnt + 1
      laScale[lnSclCnt,1] = SCALE.Scale
      For lnCountSc = 1 TO 8
        lcX = STR(lnCountSc,1)
        laScale[lnSclCnt,lnCountSc+1] = SCALE.SZ&lcX
      ENDFOR
  ENDIF
ENDSCAN
SEEK(lcKey)
SELECT (lcAlias)
RETURN ''

*!*************************************************************
*! Name        : lfEndLine
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get the Record count of the InvLine.DBF
*!*************************************************************
*! Called from : ARPINVL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : C101651
*!*************************************************************
*! Example     : lfEndLine
*!*************************************************************
FUNCTION lfEndLine
PRIVATE lcAlias
*lnInvRecNo      && Variable to hold the number of the last Invoice Lines per
                 && invoice and the variable in Syrepuvr.dbf
lcAlias = ALIAS()
SELECT (lcInvLine_A)
SET ORDER TO TAG InvLine DESCENDING
=SEEK(InvHdr.Invoice,lcInvLine_A)
lnInvRecNo = IIF(EOF(lcInvLine_A) , 0 , RECNO(lcInvLine_A))
SET ORDER TO TAG InvLine ASCENDING
SELECT (lcAlias)
RETURN ''

*!*************************************************************
*! Name        : lfEndGrRtn
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 08/24/1999
*! Purpose     : To get this variable <llEndGroup> its initial value
*!******************************************************************
*! Called from : ARPINVL.FRX  <DOS>
*!******************************************************************
*! Passed Parameters : None
*!******************************************************************
*! Return      : None
*!******************************************************************
*! Refer to    : C101651
*!******************************************************************
*! Example     : lfEndGrRtn
*!******************************************************************
FUNCTION lfEndGrRtn
llEndGroup = .F.
RETURN ''

*!*************************************************************
*! Name      : lfGetPath            
*! Developer : Nader Anis
*! Date      : 06/07/2000
*! Purpose   : Check If the file exist or not..
*! Ref       : B603498,1
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Example           : =lfGetPath
*!*************************************************************
FUNCTION lfGetPath

IF !EMPTY(lcOptProg)
  lcPath = ALLTRIM(lcOptProg)
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  IF oAriaApplication.multiinst 
    lcPath = IIF(FILE(oAriaApplication.clientreporthome +lcPath+'.FXP'),;
                   oAriaApplication.clientreporthome +lcPath,;
             IIF(FILE(oAriaApplication.clientreporthome +gcAct_Appl+'\'+lcPath+'.FXP'),;
                      oAriaApplication.clientreporthome +gcAct_Appl+'\'+lcPath,;
                      oAriaApplication.clientreporthome +LEFT(lcPath,2)+'\'+lcPath))
                      
    IF !FILE(ALLTRIM(lcpath) +'.FXP')
      lcPath = IIF(FILE(gcRepHome +lcPath+'.FXP'),;
                   gcRepHome +lcPath,;
  			       IIF(FILE(gcRepHome +gcAct_Appl+'\'+lcPath+'.FXP'),;
                   gcRepHome +gcAct_Appl+'\'+lcPath,;
                   gcRepHome +LEFT(lcPath,2)+'\'+lcPath))
      
    ENDIF 
  ELSE
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
    lcPath = IIF(FILE(gcRepHome+lcPath+'.FXP'),;
                   gcRepHome+lcPath,;
             IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcPath+'.FXP'),;
                      gcRepHome+gcAct_Appl+'\'+lcPath,;
                      gcRepHome+LEFT(lcPath,2)+'\'+lcPath))
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  ENDIF                    
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
  RETURN FILE(ALLTRIM(lcpath) +'.FXP')
ENDIF

*!*************************************************************
*! Name      : lfGetFrx            
*! Developer : Nader Anis
*! Date      : 06/07/2000
*! Purpose   : Check If the file exist or not.
*! Ref       : B603498,1
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Example           : =lfGetFrx
*!*************************************************************
FUNCTION lfGetFrx
IF RAT('\',lcFormName)=0
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  IF oAriaApplication.multiinst
   
    lcFrxPath= IIF(FILE(oAriaApplication.clientreporthome+lcFormName+'.FRX') ;
               .OR. FILE(oAriaApplication.clientreporthome+lcFormName+'.LBX'),;
               oAriaApplication.clientreporthome+lcFormName,oAriaApplication.clientreporthome+gcAct_Appl+'\'+lcFormName)
    
    IF !(FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX'))               
      lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
               .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
               gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)

    ENDIF 
  ELSE
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
               .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
               gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
  ENDIF 
  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]               
ENDIF          
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')          

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Bassem Rafaat ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(INVHDR.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign

*!*****************************************************************************************
*! Name      : RefreshOptMsg
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 11:18:46 PM
*! Purpose   : Refresh the optional message area
*!*****************************************************************************************
*! Modifications : hfk, 08/29/2004, #038272, modified returned character to diaplay optional
*! message correctly
*!*****************************************************************************************
FUNCTION RefreshOptMsg
  IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
    RETURN ""
  ELSE 
    RETURN ALLTRIM(lcRpMsg1) + IIF(EMPTY(lcRpMsg2),"",", ") +;
           ALLTRIM(lcRpMsg2) + IIF(EMPTY(lcRpMsg3),"",", ") +;
           ALLTRIM(lcRpMsg3)
  ENDIF 
ENDFUNC 
*-- end of RefreshStatus.

*!*****************************************************************************************
*! Name      : lfMsgDefa
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 11:18:46 PM
*! Purpose   : Optional message default value
*!*****************************************************************************************
*!
FUNCTION lfMsgDefa
  IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
    IF FILE(gcDataDir+lcTypInv+'.MEM')
      RESTORE FROM gcDataDir+lcTypInv+'.MEM' ADDITIVE
    ENDIF
  ENDIF
  RETURN 0
ENDFUNC 
*-- end of lfMsgDefa.
*!*************************************************************
*! Name      : lfFillForm
*! Developer : Mohamed Badran (MAB)
*! Date      : 08/18/1998
*! Purpose   : Fill report format arrays called from dummy variable in rep. gen.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfFillFrm
DIMENSION laFrmDesc[3,1] , laFrmVal[3,1]
laFrmDesc[1,1] = 'All'
laFRmDesc[2,1] = 'Not Printed'
laFrmDesc[3,1] = 'Printed'

laFrmVal[1,1]  = ''
laFrmVal[2,1]  = SPACE(1)
laFrmVal[3,1]  = 'P'
*-- end of lfFillForm.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Fathi (HFK)
*! Date      : 02/11/2004
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.


*!*************************************************************
*! Name      : lfUpdate
*! Developer : Ayman M. Radwan (aym)
*! Date      : 15/02/2006
*! Purpose   : CHECK IF INVOICE PRINTED
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfUpdate

IF SYS(2040)='2'
  llPrinter = .T.
  *B608106,1 SSH 5-31-2007 Update PRINTED flag.
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  IF  TYPE('lcXMLFileName') <> 'C'
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
    loOgScroll.ll2Printer=.T.  
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[Start]
  ENDIF
  *! E302715,1 MMT 07/13/2010 Convert Invoice Form to work from the request builder[End]
  *B608106,1 SSH 5-31-2007 Update PRINTED flag.

ENDIF
RETURN .T.
*! E303247,1 MMT 09/10/2012 Single size invoice and Packing list forms[T20120821.0021][Start]
*!*************************************************************
*! Name      : lfGetColorLen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/10/2012
*! Purpose   : Get Color Lengh and Position
*!*************************************************************
FUNCTION lfGetColorLen
DECLARE laItemSeg[1]
PRIVATE lnCount 
*! T20140522.0001,1 TMI 06/01/2014 Append Request Builder Changes [Start]
*=gfItemMask(@laItemSeg)
IF TYPE('oAriaEnvironment') == 'O'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laItemSeg)
ELSE
=gfItemMask(@laItemSeg)
ENDIF
*! T20140522.0001,1 TMI 06/01/2014 Append Request Builder Changes [End]

FOR lnCount = 1 TO ALEN(laItemSeg,1)
 IF laItemSeg[lnCount,1]='C'
   lnColorL = LEN(laItemSeg[lnCount,3])
   lnColorP = laItemSeg[lnCount,4]
   EXIT
 ENDIF
ENDFOR
*! E303247,1 MMT 09/10/2012 Single size invoice and Packing list forms[T20120821.0021][End]