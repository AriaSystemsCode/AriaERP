***********************************************************************
*:  Program file : ARCNINV.PRG
*:  Program desc.: Auto create concession invoices and credit notes
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar[MMT]
*:           Date: 09/18/2011
*:      Reference: C201384.Exe,C201383.122[T20110621.0057]
*:************************************************************************
*:C201384,1 MMT 11/02/2011 Fix the Problem of Error while saving CM[T20110621.0057]
*:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[T20110621.0057]
*:C201384,3 MMT 12/08/2011 Fix the Problem of not checking style price before increasing Qty[T20110621.0057]
*:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[T20110621.0057]
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[T20110621.0057]
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057]
*!C201468,1 MMT 03/25/2012 Custom Concession program for GPS (Top Shop case)[T20120130.0001]
*!C201384,7 MMT 04/22/2012 Enhance program for ticket[T20120130.0001]
*!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[T20120517.0001]
*!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[T20120130.0001]
*!E303203,1 HIA 07/24/2012 Issue# 3, add store to warhous and concession setup table [T20120130.0001][Begin]
*!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001]
*!B609963,3 MMT 09/03/2012 Fix Issue#6 in Concession Project[T20120130.0001]
*!C201515,1 MMT 09/09/2012 Add option to concession invoicing to deal with leading zeros barcodes[T20120816.0001]
*!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001]
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [T20121108.0001]
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [T20121004.0002]
*!C201539,2 SAB 12/18/2012 Add New option Merge by season [T20121217.0001]
*!B610193,1 HIA 10/13/2013 AR - concession invoice not checking financial period [T20121230.0002]
*!B610204,1 HIA 01/17/2013 AR - concession invoice not checking financial period [T20121230.0002]
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012]
*!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2]
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2]
*!B610382,3 SAB 09/26/2013 Fix concession issu# 2 Negative Adjustment Problem [T20120108.0010][Issue2]
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010]
*!B610637,1 MMT 12/26/2013 Concession invoicing program calculates wrong style ave. cost[T20131212.0012]
*!B610637,2 MMT 12/29/2013 Error if user selected Auto adjust option NO[T20131212.0012]
*!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001]
*!B611001,1 MMT 05/13/2015 Negative stock in Concession invoicing program [T20150506.0015]
*!B611100,1 MMT 01/12/2016 Concession invoicing program prints wrong data in its related report[T20160104.0051]
*!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078]
*!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1]    
*!B611115,2 MMT 02/17/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1]
*!B611115,3 MMT 02/23/2016 Issue#2 - Incorrect Tax amount in Invoice created by concession [P20160126.0001-Issue#2]
*!B611149,1 MMT 05/25/2016 Incorrect Price calculation in Custom  AR concession program[T20160518.0001]
*!B611162,1 MMT 06/27/2016 Custom Concession invoicing calculates CR TotCredit incorrctly[T20160623.0206]
*:************************************************************************
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*TRY
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()
  LOCAL lcCurrentProcedure

  *:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[Start]
  *lcCurrentProcedure = ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath))
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDICTIONARY\"), "", -1, 1, 1)
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
  lcCurrentProcedure = loEnvironment.Aria40SharedPath

  SET DEFAULT TO &lcCurrentProcedure.
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment
  SET DEFAULT TO &lcCurrentProcedure.
  *:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[End]


  lcRootAriaFolder = ADDBS(oAriaApplication.defaultpath)

  oAriaApplication.ApplicationHome = ALLTRIM(lcRootAriaFolder)+"\PRGS\"
  oAriaApplication.SCREENHOME =   ALLTRIM(lcRootAriaFolder)+"\SCREENS\"
  lcClassHome =  ALLTRIM(lcRootAriaFolder)+"\CLASSES\"
  loOldApp = oAriaApplication

  SET DEFAULT TO  (ALLTRIM(lcRootAriaFolder))
  SET CLASSLIB TO lcClassHome +'MAIN.VCX' ADDITIVE
  SET CLASSLIB TO lcClassHome +'UTILITY.VCX' ADDITIVE
  SET CLASSLIB TO lcClassHome +'GLOBALS.VCX' ADDITIVE
  SET PROCEDURE TO lcRootAriaFolder+'ARIA.EXE'  ADDITIVE
  SET PROCEDURE TO (lcRootAriaFolder)+'PRGS\SY\gfSTYcrl.fxp' ADDITIVE
  *:C201384,5 MMT 01/11/2012 Fix the Problem of not updating BININVJL Table[Start]
  IF FILE((lcRootAriaFolder)+'PRGS\SY\ARIAMAIN.fxp')
    SET PROCEDURE TO (lcRootAriaFolder)+'PRGS\SY\ARIAMAIN.fxp' ADDITIVE
  ENDIF

  *:C201384,5 MMT 01/11/2012 Fix the Problem of not updating BININVJL Table[END]
  SET DEFAULT TO &lcCurrentProcedure.
  *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
  *oAriaApplication = CREATEOBJECT('CustomAriaApplication',loOldApp.systemfilespath)
  oAriaApplication = CREATEOBJECT('NonSAASAriaApplication',loOldApp.systemfilespath, ClientID)

  TRY
    oAriaApplication.IndexW = 10
    oAriaApplication.fieldw = 10
    oAriaApplication.filew = 8
  CATCH
  ENDTRY
  *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]
  lcActCompID = loAgent.GetRequestCompany(lcRequestID, ClientID )
  oAriaApplication.activecompanyid = lcActCompID
  oAriaApplication.getcompanyinformation (lcActCompID)
  *:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[Start]
  lnProp = AMEMBERS(laProp,oAriaApplication)
  FOR lnp = 1 TO lnProp
    lcPropty = 'oAriaApplication.'+ laProp[lnp]
    IF TYPE('&lcPropty.') = 'C' AND ':\' $ EVALUATE('oAriaApplication.'+ laProp[lnp])
      lnDrPos = ATC(":\",&lcPropty.)
      IF lnDrPos > 0
        lcStrDriv = SUBSTR(&lcPropty.,lnDrPos-1,3)
        lcNewDrv = loEnvironment.ResolveMappedDrive(lcStrDriv)
        TRY
          &lcPropty. = STRTRAN(&lcPropty.,lcStrDriv,ALLTRIM(lcNewDrv))
        CATCH
        ENDTRY
      ENDIF
    ENDIF
  ENDFOR
  *:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[End]

  LOCAL lcPath
  lcPath = oAriaApplication.Screenhome+'SY\;'+oAriaApplication.Programhome+'SY\'
  SET PATH TO (lcPath) ADDITIVE
  RELEASE lcPath

  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  oAriaApplication.user_id = oAriaEnvironment.User_ID
  oariaApplication.user_id = GCUSER_ID
  oAriaEnvironment.REPORT.gcAct_Appl = 'AR'
  PUBLIC gcAct_Appl
  gcAct_Appl = 'AR'
  oAriaEnvironment.activeModuleID = 'AR'
  oAriaEnvironment.gcoutfile = FORCEEXT(oAriaEnvironment.gcoutfile,'PDF')
  oAriaEnvironment.REPORT.cTextRepType= 'PDF'
    
  lfCreatExp()
  
ELSE
  lcExpr = gfOpGrid('ARCNINV' , .T.)&&,.F.,.F.,.T.,.T.)  
  
ENDIF
*!*	CATCH TO ExcepVar
*!*	  ERROR ExcepVar.Message
*!*	  loAgent.UpdateRequestStatus(lcRequestID, 6, ExcepVar.Message, ClientID)
*!*	ENDTRY



*!*************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate Input location
*!*************************************************************
FUNCTION lfvPath
IF ALLTRIM(lcRpPath)  = "?" OR !DIRECTORY(lcRpPath)
  lcRpPath  = GETDIR('','Select Input File Location')
ENDIF


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate Account
*!*************************************************************
FUNCTION lfvAccount
PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !gfSEEK('M' +lcRpAcct, 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct, '' , 'M')
  lcRpAcct= IIF(llObjRet , lcRpAcct, '')
ENDIF

SELECT CUSTOMER
gfSETORDER(lcCustOrd)
SELECT(lnAlsNo)
*!*************************************************************
*! Name      : lfvHist
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate History Path
*!*************************************************************
FUNCTION lfvHist
IF ALLTRIM(lcRpHist)  = "?" OR !DIRECTORY(lcRpHist)
  lcRpHist = GETDIR('','Select History File Location')
ENDIF

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate input info and start processing
*!*************************************************************
FUNCTION lfCreatExp
*!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
IF LCRPFLTY ='H' AND LRPCSTVAT AND EMPTY(lcRpTaxCd)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please select a Tax code.')
  RETURN .F.
ENDIF
*!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]

*!B610193,1 HIA 10/13/2013 AR - concession invoice not checking financial period [T20121230.0002][Start]
llRet = .T.
lcGlFYear  = ''
lcGlPeriod = ''

*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
IF llRpAutAdj  
  lcScreenTemp = gfTempName()
  =lfCrtAdjTmp()
ENDIF
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
  
*!B610204,1 HIA 01/17/2013 AR - concession invoice not checking financial period [T20121230.0002][Start]
*llRet = CHECKPRD(ldRpInvDat,'lcGLFYear','lcGLPeriod','IA',.T.)
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
llRet = CHECKPRD(ldRpInvDat,'lcGLFYear','lcGLPeriod','IN')
*!B610204,1 HIA 01/17/2013 AR - concession invoice not checking financial period [T20121230.0002][End]

IF !llRet
  RETURN llRet
ENDIF 
*!B610193,1 HIA 10/13/2013 AR - concession invoice not checking financial period [T20121230.0002][End]
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [Start]
lnDivPos = ASCAN(LAOGFXFLT,"CUSTOMER.CDIVISION")
lnDivPos = ASUBSCRIPT(LAOGFXFLT,lnDivPos ,1)
lcDivVal = LAOGFXFLT[lnDivPos ,6]
IF !EMPTY(lcDivVal)
  lcDefRetReas = ALLTRIM(lfGetRtlFldVlu("REASON", "CDIVISION", lcDivVal))
  lnDefCost    = lfGetRtlFldVlu("NCNCDEFCS", "CDIVISION", lcDivVal)
  lcDefStyle   = ALLTRIM(lfGetRtlFldVlu("CCNCDEFST", "CDIVISION", lcDivVal))
  lcDefDamSty  = PADR(ALLTRIM(lfGetRtlFldVlu("CCNDDEFST", "CDIVISION", lcDivVal)), 19)
  
  DO CASE
  CASE EMPTY(lcDefRetReas)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Default Return Reason missing from Codes setup - please add this value and then rerun the process')
    RETURN .F.
  CASE EMPTY(lnDefCost)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Concession Default Cost % missing from Codes setup - please add this value and then rerun the process')
    RETURN .F.
  CASE EMPTY(lcDefStyle)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Concession Default Style missing from Codes setup - please add this value and then rerun the process')
    RETURN .F.
  CASE EMPTY(lcDefDamSty)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Default Damaged Style missing from Codes setup - please add this value and then rerun the process')
    RETURN .F.
  OTHERWISE
    
  ENDCASE
ENDIF
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [End]

*!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
*IF EMPTY(lcRpAcct)
lnAccPos = ASCAN(LAOGFXFLT,"CUSTOMER.ACCOUNT")
lcAccTmp = ''
llAccSelec = .F.
IF lnAccPos  > 0
  lnAccPos = ASUBSCRIPT(LAOGFXFLT,lnAccPos ,1)
  lcAccTmp = LAOGFXFLT[lnAccPos ,6]
ENDIF
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF

IF !EMPTY(lcAccTmp) AND USED(lcAccTmp)
  SELECT (lcAccTmp)
  LOCATE
  SCAN
    IF gfSeek('M'+&lcAccTmp..ACCOUNT,'Customer','Customer') AND !INLIST(Customer.STATUS,'H','X')
      llAccSelec = .T.
    ENDIF
  ENDSCAN
ENDIF
IF !llAccSelec
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to select at least one active Account')
  ENDIF
  RETURN .F.
ENDIF
IF LLRPADST AND EMPTY(lcRpAsAc)
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  IF TYPE('lcXMLFileName') <> 'C'
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to select Account')
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You have to select Account to assign stores')
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  ENDIF
  RETURN .F.
ELSE
  IF !USED('CUSTOMER')
    =gfOpenTable('Customer','Customer')
  ENDIF
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *=gfSeek('M'+lcRpAcct,'Customer','Customer')
  =gfSeek('M'+lcRpAsAc,'Customer','Customer')
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  IF INLIST(Customer.STATUS,'H','X')
    IF TYPE('lcXMLFileName') <> 'C'
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
      *=gfModalGen('INM40172B00000',.F.)
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Account to assign stores to is a non-active account. Cannot proceed.')
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    ENDIF
    RETURN .F.
  ENDIF
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *!*	  IF !gfSeek('S'+lcRpAcct,'Customer','Customer')
  *!*	    IF TYPE('lcXMLFileName') <> 'C'
  *!*	      = gfModalGen('INM32048B00000','DIALOG',lcRpAcct)
  *!*	    ENDIF
  *!*	    RETURN .F.
  *!*	  ENDIF
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
ENDIF

IF EMPTY(lcRpPath) OR !DIRECTORY(lcRpPath)
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid Input File Location')
  ENDIF
  RETURN .F.
ENDIF
IF (EMPTY(lcRpHist) OR !DIRECTORY(lcRpHist)) OR ALLTRIM(UPPER(lcRpPath)) == ALLTRIM(UPPER(lcRpHist))
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid History File Location')
  ENDIF
  RETURN .F.
ENDIF

IF TYPE("gcRequestXMLFileName") = 'C'
  RETURN
ENDIF
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
IF TYPE('lcXMLFileName') = 'C'
  lcRpPath = loEnvironment.ResolveMappedDrive(lcRpPath)
  lcRpHist = loEnvironment.ResolveMappedDrive(lcRpHist)
ENDIF
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]
lcDefaPath = FULLPATH('')
SET DEFAULT TO (lcRpPath)
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
IF LCRPFLTY = 'H'
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
  lnXmlCnt =ADIR(laXmlFiels,'*.XML')
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
ELSE
  lnXmlCnt =ADIR(laXmlFiels,'*.MBR')
  lcTempCursMemo = gfTempName()
ENDIF
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
IF lnXmlCnt  = 0
  IF TYPE('lcXMLFileName') <> 'C'
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    IF LCRPFLTY = 'H'
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
      =gfModalGen('INM00000B00000',.F.,.F.,.F., 'There is No XML files in '+ALLTRIM(lcRpPath))
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F., 'There is No MBR files in '+ALLTRIM(lcRpPath))
    ENDIF
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
  ENDIF
  SET DEFAULT TO (lcDefaPath)
  RETURN .F.
ENDIF
lcXmlData = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())

lfCreateTmp()

SET DEFAULT TO (lcDefaPath)
lcRpPath = ADDBS(ALLTRIM(lcRpPath))
lcRpHist = ADDBS(ALLTRIM(lcRpHist))

*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [Start]
**:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[Start]
*lcDefStyle =ALLT(gfGetMemVar('M_CNCDEFST',oAriaApplication.ActiveCompanyID))
**:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[END]
**!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
*lcDefRetReas = ALLT(gfGetMemVar('M_CNDDEFRC',oAriaApplication.ActiveCompanyID))
**!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
IF EMPTY(lcDivVal)
  lcDefStyle   = ALLT(gfGetMemVar('M_CNCDEFST',oAriaApplication.ActiveCompanyID))
  lcDefRetReas = ALLT(gfGetMemVar('M_CNDDEFRC',oAriaApplication.ActiveCompanyID))
ENDIF
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [End]

*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [Start]
lcOrderNumSeq  = ALLTRIM(gfGetMemVar('M_GenOrNum',oAriaApplication.ActiveCompanyID))
*!B610204,1 HIA 01/17/2013 AR - concession invoice not checking financial period [T20121230.0002][Start]
*STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
*=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
*!B610204,1 HIA 01/17/2013 AR - concession invoice not checking financial period [T20121230.0002][End]
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [End]


*!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
lcDefStyUpc = ''
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYUPCN')
  *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
  *IF gfSeek(lcDefStyle,'STYLEUPC','STYLEUPC')
  IF llRpUsDfSt .AND. gfSeek(lcDefStyle,'STYLEUPC','STYLEUPC')
  *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
    lcDefStyUpc = ALLTRIM(STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3)
  ENDIF
ENDIF
*!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]

FOR lnC = 1 TO ALEN(laXmlFiels,1)
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
  IF LCRPFLTY = 'H'
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
    lfProcessXML(lcRpPath+ALLTRIM(laXmlFiels[lnC ,1]))
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
  ELSE
    lfProcessMBR(lcRpPath+ALLTRIM(laXmlFiels[lnC ,1]))
  ENDIF
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
ENDFOR
llFileHasProb = .F.
FOR lnC = 1 TO ALEN(laXmlFiels,1)
  lfVerifyXMLData(lcRpPath+ALLTRIM(laXmlFiels[lnC ,1]))
ENDFOR

*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
*lfVerifyStkQty()
IF !llRpAutAdj
  lfVerifyStkQty()
ENDIF
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]

IF llFileHasProb
  SELECT TMPSTR
  IF TYPE('lcXMLFileName') <> 'C'
    DO FORM (oAriaApplication.ScreenHome + 'AR\arcninl.SCX')
    USE IN TMPSTR
    IF gfModalGen('QRM00274B40000','ALERT','Do you want process the files and omit these transactions')<> 1
      RETURN .F.
    ELSE
      *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
      * opross = CREATEOBJECT('ariaprogressbar')
      *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
    ENDIF
  ELSE
    IF LEFT(gcDevice, 7) = "PRINTER"
      oAriaEnvironment.gcDevice = "PRINTER"
    ELSE
      oAriaEnvironment.gcDevice = "FILE"
    ENDIF
    lcRpForm = 'ARCNINL'
    oAriaEnvironment.REPORT.cCROrientation = 'P'
    oAriaEnvironment.REPORT.OGLastForm = lcRpForm

    loProgress.DESCRIPTION = "Printing Log..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    IF TYPE('lcXMLFileName') = 'C'
      IF FILE(oAriaEnvironment.gcoutfile)
        loRequestObj= loAgent.GetRequest(lcRequestID,ClientID)
        IF !loRequestObj.CompleteNotification.Attachment.ContainsKey(oAriaEnvironment.gcoutfile)
          *THISFORM.loRequest.CompleteNotification.ConvertAttachmentFromString(THIS.Value)
          lcOldAttch = loRequestObj.CompleteNotification.ConvertAttachmentToString()
          lcOldAttch = lcOldAttch + IIF(EMPTY(lcOldAttch),'',',')+ oAriaEnvironment.gcoutfile
          loRequestObj.CompleteNotification.ConvertAttachmentFromString(lcOldAttch)
          *oRequestObj.CompleteNotification.Attachment.add(oAriaEnvironment.gcoutfile,oAriaEnvironment.gcoutfile)
          loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
          loProxy.AddRequest(loRequestObj,ClientID)
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
IF TYPE('lcXMLFileName') <> 'C'
  opross = CREATEOBJECT('ariaprogressbar')
ENDIF
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
lcStyleDet = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
lcTransRange = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
lfCreateTmpDet()

*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [Start]
**:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[Start]
**lcDefStyle = ALLTRIM(gfGetMemVar('M_CNCDEFST',oAriaApplication.ActiveCompanyID))
**:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[End]
**:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[Start]
*lcDefDamSty = PADR(ALLT(gfGetMemVar('M_CNDDEFST',oAriaApplication.ActiveCompanyID)),19)
**:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[End]
*lnDefCost =gfGetMemVar('M_CNCDEFCS',oAriaApplication.ActiveCompanyID)
IF EMPTY(lcDivVal)
  lcDefDamSty = PADR(ALLT(gfGetMemVar('M_CNDDEFST',oAriaApplication.ActiveCompanyID)),19)
  lnDefCost   = gfGetMemVar('M_CNCDEFCS',oAriaApplication.ActiveCompanyID)
ENDIF
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [End]

llUse_GL = ALLTRIM(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)) ='Y'
lcGlFYear  = ''
lcGlPeriod = ''
*MT02/26
*=CHECKPRD(oAriaApplication.SystemDate,'lcGLFYear','lcGLPeriod','IA',.T.)
=CHECKPRD(ldRpInvDat,'lcGLFYear','lcGLPeriod','IA',.T.)
*MT
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
IF llRpAutAdj
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
  lncAlias = SELECT(0)
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
  IF USED('TMPSTR')
    USE IN 'TMPSTR'
  ENDIF
  CREATE CURSOR TMPSTR (mStrRep M(10))
  
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
  SELECT(lncAlias)
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
  *SET FILTER TO !EMPTY(ALLTRIM(&lcXmlData..RETCLSS))
  SELECT(lcXmlData) 
  SET FILTER TO IN (lcXmlData) 
  SET FILTER TO ALLTRIM(UPPER(RETCLSS)) = 'RETURNS' IN (lcXmlData)
  LOCATE 
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
  FOR lnCTI = 1 TO ALEN(laXmlFiels,1)  
    lfCreateTrans(lcRpPath+ALLTRIM(laXmlFiels[lnCTI ,1]))
  ENDFOR
  
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
  SELECT(lncAlias)
  *!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
  *SET FILTER TO EMPTY(ALLTRIM(&lcXmlData..RETCLSS))
  SELECT(lcXmlData) 
  SET FILTER TO IN (lcXmlData) 
  SET FILTER TO ALLTRIM(UPPER(RETCLSS)) <> 'RETURNS' IN (lcXmlData)
  LOCATE 
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
  =lfVerifyStkQty()  && and create Auto Adjustment if needed
ENDIF
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]

***Start Processing files to create Invoice or credit memo
FOR lnCT = 1 TO ALEN(laXmlFiels,1)
  IF TYPE('lcXMLFileName') = 'C'
    loProgress.Percent = (lnCT /ALEN(laXmlFiels,1))
    loProgress.DESCRIPTION = "Creating Transactions for File:"+ALLTRIM(laXmlFiels[lnCT ,1])
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
  ELSE
    *!*      LOOGSCROLL.parent.Visible = .F.
    *!*      oPross.lblFirstLabel.Caption = "Creating Transactions for File:"+ALLTRIM(laXmlFiels[lnCT ,1])
    *!*      oPross.TotalProgress = (lnCT /ALEN(laXmlFiels,1))
    *!*      oPross.AutoCenter = .T.
    *!*      opross.Visible = .T.
    *!*      oPross.Show()
    *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
  ENDIF
  lfCreateTrans(lcRpPath+ALLTRIM(laXmlFiels[lnCT ,1]))
ENDFOR
*:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
IF TYPE('lcXMLFileName') <> 'C'
  oPross = NULL
  LOOGSCROLL.PARENT.VISIBLE = .T.
ENDIF
*:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
SELECT  (lcTransRange)
LOCATE
IF !EOF()
  *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
  *IF USED('TMPSTR')
  *  USE IN 'TMPSTR'
  *ENDIF
  *CREATE CURSOR TMPSTR (mStrRep M(10))
  IF !llRpAutAdj
    IF USED('TMPSTR')
      USE IN 'TMPSTR'
    ENDIF
    CREATE CURSOR TMPSTR (mStrRep M(10))
  ENDIF
  *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
  SELECT TMPSTR
  APPEND BLANK
  REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
    "*                                Processing Result                                                                 *" + CHR(13) +;
    REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)

  SELECT  (lcTransRange)
  SCAN
    REPLACE mStrRep WITH mStrRep +"File:"+ALLTRIM(&lcTransRange..cFileName)+" STORE:"+ALLTRIM(&lcTransRange..STORE)+;
      " "+IIF(&lcTransRange..TrnType = 'R','Credit memo: ','Invoice: ')+&lcTransRange..TrnNum+ CHR(13) +CHR(10) IN TMPSTR
  ENDSCAN
  SELECT TMPSTR
  IF TYPE('lcXMLFileName') <> 'C'
    DO FORM (oAriaApplication.ScreenHome + 'AR\arcninl.SCX')
  ELSE
    RELEASE CLASSLIB (lcClassHome +'UTILITY.VCX')
    IF LEFT(gcDevice, 7) = "PRINTER"
      oAriaEnvironment.gcDevice = "PRINTER"
    ELSE
      oAriaEnvironment.gcDevice = "FILE"
    ENDIF
    lcRpForm = 'ARCNINL'
    oAriaEnvironment.REPORT.cCROrientation = 'P'
    oAriaEnvironment.REPORT.OGLastForm = lcRpForm
    oAriaEnvironment.REPORT.lAdditive = .T.
    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
  ENDIF
  USE IN TMPSTR
ENDIF
FOR lnC = 1 TO ALEN(laXmlFiels,1)
  COPY FILE (lcRpPath+ALLTRIM(laXmlFiels[lnC ,1])) TO (lcRpHist+ALLTRIM(laXmlFiels[lnC ,1]))
  ERASE (lcRpPath+ALLTRIM(laXmlFiels[lnC ,1]))
ENDFOR
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
IF TYPE('lcXMLFileName') = 'C'
  loProgress.Percent = 100
  loProgress.DESCRIPTION = "End of Processing"
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
ENDIF
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]

*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*!B610637,2 MMT 12/29/2013 Error if user selected Auto adjust option NO[T20131212.0012][Start]
IF llRpAutAdj AND USED(lcScreenTemp)
*!B610637,2 MMT 12/29/2013 Error if user selected Auto adjust option NO[T20131212.0012][END]
  SELECT (lcScreenTemp)
  SCAN
    SCATTER MEMO MEMVAR
    SELECT INVTADJ
    =gfAppend('INVTADJ', .T.)  
  ENDSCAN
  SELECT INVTADJ
  =gfTableUpdate()
*!B610637,2 MMT 12/29/2013 Error if user selected Auto adjust option NO[T20131212.0012][Start]
ENDIF
*!B610637,2 MMT 12/29/2013 Error if user selected Auto adjust option NO[T20131212.0012][End]
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]

*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
*--Call report print.
*--Would you like to print the Inventory Adjustment journal ?
*!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*IF RECCOUNT(lcScreenTemp) > 0 .AND. gfModalGen('QRM42066B42002','DIALOG') = 1
IF llRpAutAdj .AND. RECCOUNT(lcScreenTemp) > 0 .AND. gfModalGen('QRM42066B42002','DIALOG') = 1
*!B610382,1 SAB 06/16/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
  =lfPrintInvAdj()
ENDIF
*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]

*!*************************************************************
*! Name      : lfProcessXML
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Read XML file
*!*************************************************************
FUNCTION lfProcessXML
LPARAMETERS lcImpXMLFile

lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
lobjDOMDocument.LOAD(lcImpXMLFile)
loRoot = lobjDOMDocument.childNodes(1)
m.cFileName = lcImpXMLFile
FOR lnIndex = 0 TO loRoot.childNodes(0).childNodes.LENGTH - 1
  loVariable = loRoot.childNodes(0).childNodes (lnIndex).childNodes (0)
  m.STORE = ''
  m.UPC =''
  m.DESC =''
  m.RETCLSS = ''
  m.RETREAS = ''
  m.RETRESN = ''
  m.QTY = 0
  m.PRICE = 0
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  m.CQUALIF = ''
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
  m.EuroPrice = 0
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]

  FOR lnSIndex = 0 TO loVariable.childNodes.LENGTH -1
    DO CASE
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "STORENUM"
      m.STORE = loVariable.childNodes(lnSIndex).TEXT
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "PRODUCTDESCRIPTION"
      m.DESC = loVariable.childNodes(lnSIndex).TEXT
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "BARCODE"
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
      *m.UPC  = loVariable.childNodes(lnSIndex).TEXT
      m.UPC  = LEFT(loVariable.childNodes(lnSIndex).TEXT,13)
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "TRANSACTIONQTY"
      m.QTY = VAL(loVariable.childNodes(lnSIndex).TEXT)
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "STERLINGVALUE"
      m.PRICE = VAL(loVariable.childNodes(lnSIndex).TEXT)/100
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "RETURNREASONCLASS"
      m.RETCLSS = loVariable.childNodes(lnSIndex).TEXT
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "RETURNREASONCODE"
      m.RETREAS = loVariable.childNodes(lnSIndex).TEXT
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "RETURNREASONCODENAME"
      m.RETRESN  = loVariable.childNodes(lnSIndex).TEXT
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "TRANSACTIONQUALIFIER"
      m.CQUALIF = loVariable.childNodes(lnSIndex).TEXT
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
    CASE UPPER(loVariable.childNodes(lnSIndex).baseName) == "EUROVALUE"
      m.EuroPrice = VAL(loVariable.childNodes(lnSIndex).TEXT)/100
    *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]
    ENDCASE
  ENDFOR
  
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
  IF LCRPFLTY ='H' AND LRPCSTVAT 
    m.PRICE = m.EuroPrice
  ENDIF
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]
  *!B611149,1 MMT 05/25/2016 Incorrect Price calculation in Custom  AR concession program[T20160518.0001][Start]
  m.PRICE = m.PRICE/m.QTY 
  *!B611149,1 MMT 05/25/2016 Incorrect Price calculation in Custom  AR concession program[T20160518.0001][End]
  *IF !SEEK(PADR(m.STORE,8) +PADR(m.UPC,15) ,lcXmlData)
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *INSERT INTO (lcXmlData) (STORE ,UPC,DESC,RETCLSS,RETREAS ,RETRESN ,QTY ,PRICE,cFileName) VALUES (m.STORE ,m.UPC,m.DESC,m.RETCLSS,m.RETREAS ,m.RETRESN ,m.QTY ,m.PRICE,m.cFileName)
  INSERT INTO (lcXmlData) (STORE ,UPC,DESC,RETCLSS,RETREAS ,RETRESN ,QTY ,PRICE,cFileName,CQUALIF ) VALUES (m.STORE ,m.UPC,m.DESC,m.RETCLSS,m.RETREAS ,m.RETRESN ,m.QTY ,m.PRICE,m.cFileName,m.CQUALIF)
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  *ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfCreateTmp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Create Tmp.
*!*************************************************************
FUNCTION lfCreateTmp
DIMENSION laStuctArr[13,4]
laStuctArr[1,1] = 'STORE'
laStuctArr[1,2] = 'C'
laStuctArr[1,3] = 8
laStuctArr[1,4] = 0

laStuctArr[2,1] = 'UPC'
laStuctArr[2,2] = 'C'
laStuctArr[2,3] = 15
laStuctArr[2,4] = 0

laStuctArr[3,1] = 'DESC'
laStuctArr[3,2] = 'C'
laStuctArr[3,3] = 60
laStuctArr[3,4] = 0

laStuctArr[4,1] = 'RETCLSS'
laStuctArr[4,2] = 'C'
laStuctArr[4,3] = 20
laStuctArr[4,4] = 0

laStuctArr[5,1] = 'RETREAS'
laStuctArr[5,2] = 'C'
laStuctArr[5,3] = 10
laStuctArr[5,4] = 0

laStuctArr[6,1] = 'RETRESN'
laStuctArr[6,2] = 'C'
laStuctArr[6,3] = 60
laStuctArr[6,4] = 0

laStuctArr[7,1] = 'QTY'
laStuctArr[7,2] = 'N'
laStuctArr[7,3] = 6
laStuctArr[7,4] = 0

laStuctArr[8,1] = 'PRICE'
laStuctArr[8,2] = 'N'
laStuctArr[8,3] = 10
laStuctArr[8,4] = 2

laStuctArr[9,1] = 'cFileName'
laStuctArr[9,2] = 'C'
laStuctArr[9,3] = 100
laStuctArr[9,4] = 0

laStuctArr[10,1] = 'lIgnore'
laStuctArr[10,2] = 'L'
laStuctArr[10,3] = 1
laStuctArr[10,4] = 0

laStuctArr[11,1] = 'STYLE'
laStuctArr[11,2] = 'C'
laStuctArr[11,3] = 19
laStuctArr[11,4] = 0

laStuctArr[12,1] = 'SIZE'
laStuctArr[12,2] = 'C'
laStuctArr[12,3] = 2
laStuctArr[12,4] = 0

laStuctArr[13,1] = 'TRANS'
laStuctArr[13,2] = 'C'
laStuctArr[13,3] = 6
laStuctArr[13,4] = 0

*!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
DIMENSION laStuctArr[ALEN(laStuctArr,1)+1,4]
laStuctArr[ALEN(laStuctArr,1),1] = 'CQUALIF'
laStuctArr[ALEN(laStuctArr,1),2] = 'C'
laStuctArr[ALEN(laStuctArr,1),3] = 10
laStuctArr[ALEN(laStuctArr,1),4] = 0
*!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]

=gfCrtTmp(lcXmlData ,@laStuctArr,'STORE+UPC',lcXmlData)
SELECT(lcXmlData)
*:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[Start]
*INDEX ON cFileName TAG 'cFileName'
INDEX ON cFileName TAG 'cFileName' ADDITIVE
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*INDEX on cFileName + STORE  + SIZE + UPC TAG 'FileDesc' ADDITIVE
INDEX ON STORE  + SIZE + UPC TAG 'FileDesc' ADDITIVE
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
*:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[ENd]

SET ORDER TO (lcXmlData)

IF USED('TMPSTR')
  USE IN TMPSTR
ENDIF
CREATE CURSOR TMPSTR (mStrRep M(10))
SELECT TMPSTR
APPEND BLANK
REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
  "*                                Error log                                                                 *" + CHR(13) +;
  REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)


*!*************************************************************
*! Name      : lfVerifyXMLData
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Verify data
*!*************************************************************
FUNCTION lfVerifyXMLData
LPARAMETERS  lcFileNam

SELECT (lcXmlData)
SET ORDER TO 'cFileName'
REPLACE mStrRep WITH mStrRep + CHR(13) +CHR(10)+"File:"+lcFileNam + CHR(13) +CHR(10)+ CHR(13) +CHR(10) IN  TMPSTR
lcFile = ''
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYUPCN')
ENDIF

IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE')
ENDIF
IF !USED('CODES')
  =gfOpenTable('CODES','CCODE_NO')
  *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[Start]
ELSE
  SELECT CODES
  =gfSetOrder('CCODE_NO')
  *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[END]
ENDIF
*!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[Start]
lcDivCursor   = ''
llSelectDivision = .F.
lnPosDIV = ASCAN(laOgFXFlt,"CUSTOMER.CDIVISION")
IF lnPosDIV > 0
  lnPosDIV = ASUBSCRIPT(laOgFxFlt,lnPosDIV,1)
  lcDivisions= laOgFxFlt[lnPosDIV,6]
  IF !EMPTY(lcDivisions)
    llSelectDivision = .T.
    lcDivCursor = gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CDIVISION'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcDivCursor ,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcDivisions)
    DO WHILE lnEnd <> 0
      SELECT(lcDivCursor)
      APPEND BLANK
      REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
      lcDivisions = STUFF(lcDivisions,lnStart,lnEnd,"")
      lnEnd=AT('|',lcDivisions)
    ENDDO
    IF lnEnd = 0
      SELECT(lcDivCursor)
      APPEND BLANK
      REPLACE CDIVISION WITH lcDivisions
    ENDIF
  ENDIF
ENDIF
*!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[END]
SELECT (lcXmlData)
=SEEK(lcFileNam)
llHasProblem = .F.
SCAN REST WHILE cFileName = lcFileNam
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *IF !gfSeek('S'+lcRpAcct+&lcXmlData..STORE,'Customer','Customer')
  *   REPLACE mStrRep WITH mStrRep +"STORE: "+&lcXmlData..STORE+" does not exist for account "+lcRpAcct+ CHR(13) +CHR(10) in  TMPSTR
  *!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][Start]
  llConcSetFound = .T.
  lcrpacct = ''
  *!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][END]
  IF EMPTY(lfGetAcc(&lcXmlData..STORE))
    REPLACE mStrRep WITH mStrRep +"STORE: "+&lcXmlData..STORE+" does not exist for any of the selected accounts"+CHR(13) +CHR(10) IN  TMPSTR
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    llHasProblem = .T.
    REPLACE lIgnore WITH .T. IN (lcXmlData)
  *!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][Start]
  ELSE
    IF !llConcSetFound
      REPLACE mStrRep WITH mStrRep +"Setup not done for STORE: "+&lcXmlData..STORE+" Account : "+lcrpacct+CHR(13) +CHR(10) IN  TMPSTR
      llHasProblem = .T.
      REPLACE lIgnore WITH .T. IN (lcXmlData)
    ENDIF
  *!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][END]
  ENDIF
  *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
  *IF (ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No') AND  (!gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND gfSeek(STYLEUPC.STYLE,'STYLE') AND ALLTRIM(STYLE.DESC1) <> ALLTRIM(&lcXmlData..DESC)))
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *IF (ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No') AND  (!gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND !gfSeek(STYLEUPC.STYLE,'STYLE')))
  IF (!gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND !gfSeek(STYLEUPC.STYLE,'STYLE')))
    *!C201515,1 MMT 09/09/2012 Add option to concession invoicing to deal with leading zeros barcodes[T20120816.0001][Start]
    *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
    *IF LCRPFLTY = 'T' AND LLRPLZBR AND !gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC','STYUPCN') AND SUBSTR(ALLTRIM(&lcXmlData..UPC),1,5) = REPLICATE('0',5) 
    IF llRpUsDfSt .AND. LCRPFLTY = 'T' AND LLRPLZBR AND !gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC','STYUPCN') AND SUBSTR(ALLTRIM(&lcXmlData..UPC),1,5) = REPLICATE('0',5) 
    *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
      REPLACE SIZE  WITH '1' IN (lcXmlData)
      REPLACE STYLE WITH lcDefStyle IN (lcXmlData)
      REPLACE DESC WITH 'Dept Default No' IN (lcXmlData)
    ELSE
    *!C201515,1 MMT 09/09/2012 Add option to concession invoicing to deal with leading zeros barcodes[T20120816.0001][END]
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *IF (!gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND !gfSeek(STYLEUPC.STYLE,'STYLE')))
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
      *:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][Start]
      *REPLACE mStrRep WITH mStrRep +"Barcode: "+&lcXmlData..UPC+" does not exist in the UPC file for style description "+&lcXmlData..DESC+ CHR(13) +CHR(10) in  TMPSTR
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *REPLACE mStrRep WITH mStrRep +"Barcode: "+&lcXmlData..UPC+'   '+ALLTRIM(&lcXmlData..SIZE)+"  does not exist in the UPC file for style description "+&lcXmlData..DESC+ CHR(13) +CHR(10) in  TMPSTR
      REPLACE mStrRep WITH mStrRep +"Barcode: "+&lcXmlData..UPC+'   '+ALLTRIM(&lcXmlData..SIZE)+"  does not exist in the UPC file"+IIF(LCRPFLTY = 'H',' for style description '+&lcXmlData..DESC,'')+ CHR(13) +CHR(10) IN  TMPSTR
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
      *:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][END]
      llHasProblem = .T.
      REPLACE lIgnore WITH .T. IN (lcXmlData)
    *!C201515,1 MMT 09/09/2012 Add option to concession invoicing to deal with leading zeros barcodes[T20120816.0001][Start]
    ENDIF 
    *!C201515,1 MMT 09/09/2012 Add option to concession invoicing to deal with leading zeros barcodes[T20120816.0001][END]
  ELSE
    *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[Start]
    IF llSelectDivision AND !EMPTY(lcDivCursor) AND USED(lcDivCursor)
      IF !SEEK(STYLE.CDIVISION,lcDivCursor)
        REPLACE lIgnore WITH .T. IN (lcXmlData)
      ENDIF
    ENDIF
    *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[END]
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    *IF (ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No')
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    *IF (ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No') &&OR (ALLTRIM(&lcXmlData..DESC) = 'Dept Default No' AND gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND gfSeek(STYLEUPC.STYLE,'STYLE'))
    *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[Start]
    *IF ALLTRIM(&lcXmlData..UPC) <> ALLTRIM(lcDefStyUpc) OR EMPTY(ALLTRIM(lcDefStyUpc))
    lcUpcDefSize = lfIsDefaultUPC(&lcXmlData..UPC)
    IF EMPTY(ALLTRIM(lcDefStyUpc)) OR EMPTY(lcUpcDefSize)
      *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[End]
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      REPLACE STYLE WITH STYLEUPC.STYLE,;
        SIZE  WITH STYLEUPC.SIZE IN (lcXmlData)
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
      *!*	      IF (ALLTRIM(&lcXmlData..DESC)) = 'Dept Default No' &&AND ALLTRIM(&lcXmlData..UPC) <> lcDefStyUpc)
      REPLACE DESC WITH STYLE.DESC IN (lcXmlData)
      *!*	      ENDIF
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *  *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
    ELSE
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
      *!*	      IF ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No' &&AND (!gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') OR (gfSeek(ALLTRIM(&lcXmlData..UPC),'STYLEUPC') AND !gfSeek(STYLEUPC.STYLE,'STYLE')))&&AND EMPTY(lcDefStyUpc) OR EMPTY(aLLTRIM(&lcXmlData..UPC))
      *!*	        REPLACE mStrRep WITH mStrRep +"Concession default style has invalid UPC#"+ CHR(13) +CHR(10) in  TMPSTR
      *!*	        llHasProblem = .T.
      *!*	        REPLACE lIgnore WITH .T. IN (lcXmlData)
      *!*	      ELSE
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[Start]
      *REPLACE SIZE  WITH '1' IN (lcXmlData)
      REPLACE SIZE  WITH lcUpcDefSize IN (lcXmlData)
      *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[End]
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
      REPLACE STYLE WITH lcDefStyle IN (lcXmlData)
      REPLACE DESC WITH 'Dept Default No' IN (lcXmlData)
      *!*	      ENDIF
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      *  *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]
    ENDIF
  ENDIF
  *:C201384,1 MMT 11/02/2011 Fix the Problem of Error while saving CM[Start]
  *!*    IF !gfSeek('NREASON    '+ALLTRIM(&lcXmlData..RETREAS),'CODES') OR (gfSeek('NREASON    '+ALLTRIM(&lcXmlData..RETREAS),'CODES') AND ALLTRIM(codes.cdiscrep) <> ALLTRIM(&lcXmlData..RETRESN))
  *!*      REPLACE mStrRep WITH mStrRep +"Returns Reason code "+ALLTRIM(&lcXmlData..RETREAS)+" for return reason "+ALLTRIM(&lcXmlData..RETRESN)+" does not exist" + CHR(13) +CHR(10) in  TMPSTR
  *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  *IF !EMPTY(ALLTRIM(&lcXmlData..RETRESN))
  IF !EMPTY(ALLTRIM(&lcXmlData..RETCLSS)) AND !EMPTY(ALLTRIM(&lcXmlData..RETRESN))
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    =gfSeek('NREASON    ','CODES')
    SELECT CODES
    *:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[Start]
    *   LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM =  'NREASON    ' FOR UPPER(ALLTRIM(&lcXmlData..RETRESN))$ UPPER(ALLTRIM(codes.cdiscrep))
    LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM =  'NREASON    ' FOR IIF(UPPER(ALLTRIM(&lcXmlData..RETRESN)) == 'FAULTY',UPPER(ALLTRIM(&lcXmlData..RETRESN))== UPPER(ALLTRIM(codes.cdiscrep)), UPPER(ALLTRIM(&lcXmlData..RETRESN))$ UPPER(ALLTRIM(codes.cdiscrep)))
    *:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[END]
    IF !FOUND()
      REPLACE mStrRep WITH mStrRep +"Return reason "+ALLTRIM(&lcXmlData..RETRESN)+" does not exist" + CHR(13) +CHR(10) IN  TMPSTR
      *:C201384,1 MMT 11/02/2011 Fix the Problem of Error while saving CM[END]
      llHasProblem = .T.
      REPLACE lIgnore WITH .T. IN (lcXmlData)
      *:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[Start]
    ELSE
      REPLACE RETREAS WITH  CODES.CCODE_NO IN (lcXmlData)
      *:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[END]
    ENDIF
    *:C201384,1 MMT 11/02/2011 Fix the Problem of Error while saving CM[Start]
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
  ELSE && Check Qualifier field
    IF LCRPFLTY = 'H'
      IF EMPTY(ALLTRIM(&lcXmlData..RETCLSS)) AND !EMPTY(ALLTRIM(&lcXmlData..CQUALIF)) AND ALLTRIM(&lcXmlData..CQUALIF) == '061'
        IF !EMPTY(lcDefRetReas)
          REPLACE RETREAS WITH lcDefRetReas,;
            RETCLSS WITH 'RETURNS'  IN (lcXmlData)
        ELSE
          REPLACE mStrRep WITH mStrRep +"Default Return reason code is empty" + CHR(13) +CHR(10) IN  TMPSTR
          llHasProblem = .T.
          REPLACE lIgnore WITH .T. IN (lcXmlData)
        ENDIF
      ENDIF
    ENDIF
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
  ENDIF
  IF !&lcXmlData..lIgnore AND !EMPTY(&lcXmlData..STYLE) AND !gfSeek(&lcXmlData..STYLE,'STYLE','STYLE')
    REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(&lcXmlData..STYLE)+" does not exist in Style file for barcode:"+ &lcXmlData..UPC+"   "+ALLTRIM(&lcXmlData..SIZE)+ CHR(13) +CHR(10) IN  TMPSTR
    llHasProblem = .T.
    REPLACE lIgnore WITH .T. IN (lcXmlData)
  ENDIF
  *:C201384,1 MMT 11/02/2011 Fix the Problem of Error while saving CM[END]
ENDSCAN

*:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[Start]
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*!*	SELECT (lcXmlData)
*!*	SET ORDER TO 'FileDesc'
*!*	LOCATE
*!*	DO WHILE !EOF()
*!*	  lcKeyExp = cFileName + STORE  + SIZE + UPC
*!*	  =SEEK(lcKeyExp)
*!*	  lnQtyOld = 0
*!*	  SCAN REST WHILE cFileName + STORE  + SIZE + UPC  = lcKeyExp FOR cFileName = lcFileNam AND EMPTY(ALLTRIM((RETCLSS))) AND !lIgnore AND ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
*!*	    lcQtyPos = ALLTRIM(&lcXmlData..SIZE)
*!*	    IF ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
*!*	      m.STYLE = &lcXmlData..STYLE
*!*	    ELSE
*!*	      m.STYLE = PADR(lcDefStyle,19)
*!*	    ENDIF
*!*	     lnCncSetSelect = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
*!*	                                                               " WHERE [Account]='"+lcrpacct+"' AND [Store]='"+&lcXmlData..STORE+"'",'',;
*!*	                                                            "CNSSETFILE","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))

*!*	     lcSelDefaultWare  = CNSSETFILE.Concession_Warehouse
*!*	     lcSelDefBin = CNSSETFILE.Concession_Default_Bin
*!*	     IF !USED('WHBINLOC_A')
*!*	       =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_A')
*!*	     ENDIF
*!*	     IF !gfSeek(PADR(lcSelDefaultWare,6)+PADR(lcSelDefBin ,10)+m.STYLE ,'WHBINLOC_A')
*!*	       REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE )+" barcode:"+ &lcXmlData..UPC+"   "+ALLTRIM(&lcXmlData..SIZE)+" is not found in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) in  TMPSTR
*!*	       llHasProblem = .T.
*!*	       REPLACE lIgnore WITH .T. IN (lcXmlData)
*!*	     ELSE
*!*	       IF  WHBINLOC_A.Qty&lcQtyPos. < lnQtyOld+&lcXmlData..Qty
*!*	         REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE)+" barcode:"+ &lcXmlData..UPC+"   "+ALLTRIM(&lcXmlData..SIZE)+" There is no enough stock in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) in  TMPSTR
*!*	         llHasProblem = .T.
*!*	         REPLACE lIgnore WITH .T. IN (lcXmlData)
*!*	       ELSE
*!*	         lnQtyOld = lnQtyOld + &lcXmlData..Qty
*!*	       ENDIF
*!*	     ENDIF
*!*	  ENDSCAN
*!*	ENDDO
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]

*MMT1
*!*	SELECT (lcXmlData)
*!*	SET ORDER TO 'FileDesc'
*!*	LOCATE
*!*	DO WHILE !EOF()
*!*	  lcKeyExp = cFileName + STORE + SIZE
*!*	  =SEEK(lcKeyExp)
*!*	  lnQtyOld = 0
*!*	  SCAN REST WHILE cFileName + STORE  + SIZE + UPC  = lcKeyExp FOR cFileName = lcFileNam AND EMPTY(ALLTRIM((RETCLSS))) AND !lIgnore AND ALLTRIM(&lcXmlData..DESC) = 'Dept Default No'
*!*	    lcQtyPos = ALLTRIM(&lcXmlData..SIZE)
*!*	    IF ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
*!*	      m.STYLE = &lcXmlData..STYLE
*!*	    ELSE
*!*	      m.STYLE = PADR(lcDefStyle,19)
*!*	    ENDIF
*!*	     lnCncSetSelect = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
*!*	                                                               " WHERE [Account]='"+lcrpacct+"' AND [Store]='"+&lcXmlData..STORE+"'",'',;
*!*	                                                            "CNSSETFILE","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))

*!*	     lcSelDefaultWare  = CNSSETFILE.Concession_Warehouse
*!*	     lcSelDefBin = CNSSETFILE.Concession_Default_Bin
*!*	     IF !USED('WHBINLOC_A')
*!*	       =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_A')
*!*	     ENDIF
*!*	     IF !gfSeek(PADR(lcSelDefaultWare,6)+PADR(lcSelDefBin ,10)+m.STYLE ,'WHBINLOC_A')
*!*	       REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE )+" barcode:"+ &lcXmlData..UPC+" is not found in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) in  TMPSTR
*!*	       llHasProblem = .T.
*!*	       REPLACE lIgnore WITH .T. IN (lcXmlData)
*!*	     ELSE
*!*	       IF  WHBINLOC_A.Qty&lcQtyPos. < lnQtyOld+&lcXmlData..Qty
*!*	         REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE)+" barcode:"+ &lcXmlData..UPC+" There is no enough stock in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) in  TMPSTR
*!*	         llHasProblem = .T.
*!*	         REPLACE lIgnore WITH .T. IN (lcXmlData)
*!*	       ELSE
*!*	         lnQtyOld = lnQtyOld + &lcXmlData..Qty
*!*	       ENDIF
*!*	     ENDIF
*!*	  ENDSCAN
*!*	ENDDO
*MMT1
*:C201384,4 MMT 12/12/2011 Fix the Problem of not checking style Qty in WHBINLOC Table[End]


*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*!*	IF !llHasProblem
*!*	  REPLACE mStrRep WITH mStrRep +"Has no errors" + CHR(13) +CHR(10) in  TMPSTR
*!*	ENDIF
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
IF !llFileHasProb
  llFileHasProb = llHasProblem
ENDIF
SELECT (lcXmlData)
SET ORDER TO (lcXmlData)
*!*************************************************************
*! Name      : lfCreateTmpDet
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Create Tmp Detail file
*!*************************************************************
FUNCTION lfCreateTmpDet
DIMENSION laStuctArr[16,4]
laStuctArr[1,1] = 'STORE'
laStuctArr[1,2] = 'C'
laStuctArr[1,3] = 8
laStuctArr[1,4] = 0

laStuctArr[2,1] = 'STYLE'
laStuctArr[2,2] = 'C'
laStuctArr[2,3] = 19
laStuctArr[2,4] = 0

laStuctArr[3,1] = 'RETREAS'
laStuctArr[3,2] = 'C'
laStuctArr[3,3] = 10
laStuctArr[3,4] = 0

laStuctArr[4,1] = 'RETRESN'
laStuctArr[4,2] = 'C'
laStuctArr[4,3] = 60
laStuctArr[4,4] = 0


laStuctArr[5,1] = 'PRICE'
laStuctArr[5,2] = 'N'
laStuctArr[5,3] = 10
laStuctArr[5,4] = 2


laStuctArr[6,1] = 'QTY1'
laStuctArr[6,2] = 'N'
laStuctArr[6,3] = 6
laStuctArr[6,4] = 0

laStuctArr[7,1] = 'QTY2'
laStuctArr[7,2] = 'N'
laStuctArr[7,3] = 6
laStuctArr[7,4] = 0

laStuctArr[8,1] = 'QTY3'
laStuctArr[8,2] = 'N'
laStuctArr[8,3] = 6
laStuctArr[8,4] = 0

laStuctArr[9,1] = 'QTY4'
laStuctArr[9,2] = 'N'
laStuctArr[9,3] = 6
laStuctArr[9,4] = 0

laStuctArr[10,1] = 'QTY5'
laStuctArr[10,2] = 'N'
laStuctArr[10,3] = 6
laStuctArr[10,4] = 0

laStuctArr[11,1] = 'QTY6'
laStuctArr[11,2] = 'N'
laStuctArr[11,3] = 6
laStuctArr[11,4] = 0

laStuctArr[12,1] = 'QTY7'
laStuctArr[12,2] = 'N'
laStuctArr[12,3] = 6
laStuctArr[12,4] = 0

laStuctArr[13,1] = 'QTY8'
laStuctArr[13,2] = 'N'
laStuctArr[13,3] = 6
laStuctArr[13,4] = 0

laStuctArr[14,1] = 'DEPDEF'
laStuctArr[14,2] = 'C'
laStuctArr[14,3] = 1
laStuctArr[14,4] = 0

laStuctArr[15,1] = 'SEASON'
laStuctArr[15,2] = 'C'
laStuctArr[15,3] = 6
laStuctArr[15,4] = 0

laStuctArr[16,1] = 'CDIVISION'
laStuctArr[16,2] = 'C'
laStuctArr[16,3] = 6
laStuctArr[16,4] = 0

=gfCrtTmp(lcStyleDet ,@laStuctArr,'STORE+SEASON+CDIVISION+STYLE+DEPDEF',lcStyleDet)
DIMENSION laRangeArr[4,4]
laRangeArr[1,1] = 'STORE'
laRangeArr[1,2] = 'C'
laRangeArr[1,3] = 8
laRangeArr[1,4] = 0
laRangeArr[2,1] = 'cFileName'
laRangeArr[2,2] = 'C'
laRangeArr[2,3] = 100
laRangeArr[2,4] = 0
laRangeArr[3,1] = 'TrnType'
laRangeArr[3,2] = 'C'
laRangeArr[3,3] = 1
laRangeArr[3,4] = 0
laRangeArr[4,1] = 'TrnNum'
laRangeArr[4,2] = 'C'
laRangeArr[4,3] = 6
laRangeArr[4,4] = 0
=gfCrtTmp(lcTransRange ,@laRangeArr,'cFileName+STORE+TrnType+TrnNum',lcTransRange)


*!*************************************************************
*! Name      : lfCreateTrans
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Create Transactions
*!*************************************************************
FUNCTION lfCreateTrans
LPARAMETERS lcFileName
lcDefaultWare= ''

IF !USED('noTepad')
  =gfOpenTAble('noTepad','noTepad')
ENDIF
IF !USED('WAREHOUS')
  =gfOpenTAble('WAREHOUS','WAREHOUS')
ENDIF
lcDefaultWare = ''
SELECT WAREHOUS
*!*  =gfSeek('')
*!*  LOCATE FOR lDefWare
*!*  IF FOUND()
*!*    lcDefaultWare = WAREHOUS.Cwarecode
*!*  ELSE
*!*    LOCATE
*!*    lcDefaultWare= WAREHOUS.Cwarecode
*!*  ENDIF
lcAdjReason = IIF(gfSeek("DCADJREASON",'Codes','CCODE_NO'),codes.ccode_no,'')
lcAdjAcct = ' '
IF llUse_GL AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF
lcDefBin = ''
lcRetBin = ''
lcWriteOffBin = ''
loDInv = ''
loCrMem = ''
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [Start]
*lcOrderNumSeq  = ALLTRIM(gfGetMemVar('M_GenOrNum',oAriaApplication.ActiveCompanyID))
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [End]

*:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][Start]
IF TYPE('lcXMLFileName') = 'C'
  RELEASE PROCEDURE (lcRootAriaFolder+'SRVPRGS\SY\ARIAMAIN.FXP')
  RELEASE CLASSLIB (lcRootAriaFolder+'SRVCLSS\SY\ARIAMAIN.VCX')
  ON ERROR X=10
ENDIF
*:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][END]

SELECT (lcStyleDet)
ZAP
SELECT (lcXmlData)
SET ORDER TO 'cFileName'
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
IF LCRPFLTY = 'H'
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
  SELECT DISTINCT STORE FROM (lcXmlData) WHERE cFileName = lcFileName AND !lIgnore  INTO ARRAY laStoreArr
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
ELSE
  SELECT DISTINCT STORE,TRANS FROM (lcXmlData) WHERE cFileName = lcFileName AND !lIgnore  INTO ARRAY laStoreArr
ENDIF
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]

IF _TALLY > 0

  FOR lnSt =1 TO ALEN(laStoreArr,1)
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    *lcrpacct = lfGetAcc(laStoreArr[lnSt])
    lcrpacct = lfGetAcc(IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]))
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    IF LCRPFLTY = 'H'
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
      lnCncSetSel = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
        " WHERE [Account]='"+lcrpacct+"' AND [Store]='"+laStoreArr[lnSt]+"'",'',;
        "CNSSET","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    ELSE
      lnCncSetSel = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
        " WHERE [Account]='"+lcrpacct+"' AND [Store]='"+laStoreArr[lnSt,1]+"'",'',;
        "CNSSET","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
    ENDIF
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]

    IF lnCncSetSel > 0
      lcDefaultWare  = CNSSET.Concession_Warehouse
      lcDefBin = CNSSET.Concession_Default_Bin
      lcRetBin = CNSSET.Concession_Return_Bin
      lcWriteOffBin  = CNSSET.Concession_WriteOff_Bin
    ENDIF
    lnTranType = 'I'
    SELECT (lcXmlData)
    =SEEK(lcFileName)
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    IF LCRPFLTY = 'H'
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
      LOCATE REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt] AND ALLTRIM(UPPER(RETCLSS)) = 'RETURNS'
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    ELSE
      LOCATE REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt,1] AND ALLTRIM(UPPER(RETCLSS)) = 'RETURNS' AND TRANS = laStoreArr[lnSt,2]
    ENDIF
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
    IF FOUND()
      lnTranType = 'R'
      lfCrtInvCr(lnTranType,lcFileName)
    ENDIF

    SELECT (lcXmlData)
    =SEEK(lcFileName)
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    IF LCRPFLTY = 'H'
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      LOCATE REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt] AND EMPTY(ALLTRIM(RETCLSS))
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    ELSE
      LOCATE REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt,1] AND EMPTY(ALLTRIM(RETCLSS)) AND TRANS = laStoreArr[lnSt,2]
    ENDIF
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
    IF FOUND()
      lnTranType = 'I'
      lfCrtInvCr(lnTranType,lcFileName)
    ENDIF
  ENDFOR

  IF TYPE('loDInv') = 'O'
    loDInv.RELEASE()
  ENDIF
  IF TYPE('loCrMem') = "O"
    loCrMem.RELEASE()
  ENDIF
  IF UPPER((oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP')) $ UPPER(SET("Procedure"))
    RELEASE PROCEDURE (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP')
  ENDIF

ENDIF
IF TYPE('lcXMLFileName') = 'C'
  SET PROCEDURE TO (lcRootAriaFolder+'SRVPRGS\SY\ARIAMAIN.FXP') ADDITIVE
  SET CLASSLIB TO (lcRootAriaFolder+'SRVCLSS\SY\ARIAMAIN.VCX') ADDITIVE
  ON ERROR
ENDIF
*!*************************************************************
*! Name      : lfCncWhen
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : OG When Function
*!*************************************************************
FUNCTION lfCncWhen


*!*************************************************************
*! Name      : lfCrtInvCr
*! Developer : Mariam Mazhar[MMT]
*! Date      : 11/16/2011
*! Purpose   : Function to create Invoice or Credit memo
*!*************************************************************
FUNCTION lfCrtInvCr
LPARAMETERS lnTranType,lcFileName
*:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[Start]
IF USED(lcStyleDet)
  SELE(lcStyleDet)
  ZAP
ENDIF
*:C201384,1 MMT 11/20/2011 Fix the Problem of problem while saving CM[End]

DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'
lnTaxRat = 0
SELECT (lcXmlData)
*:C201384,1 MMT 11/09/2011 Fix the Problem of Error while saving CM[Start]
*=SeeK(lcFileName)
lnRecordCnt = 0
IF SEEK(lcFileName)
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
  IF LCRPFLTY = 'H'
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
    COUNT REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt] AND !lIgnore AND IIF(lnTranType = 'R',ALLTRIM(UPPER(RETCLSS)) = 'RETURNS',EMPTY(ALLTRIM(RETCLSS)))  TO lnRecordCnt
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
  ELSE
    COUNT REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt,1] AND !lIgnore AND TRANS =laStoreArr[lnSt,2]  AND IIF(lnTranType = 'R',ALLTRIM(UPPER(RETCLSS)) = 'RETURNS',EMPTY(ALLTRIM(RETCLSS)))  TO lnRecordCnt
  ENDIF
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
  IF lnRecordCnt > 0
    IF TYPE('lcXMLFileName') <> 'C'
      oPross.TotalProgress = lnRecordCnt
    ENDIF
  ENDIF
ENDIF
lnCntLine = 0
=SEEK(lcFileName)
*:C201384,1 MMT 11/09/2011 Fix the Problem of Error while saving CM[END]
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
*SCAN REST WHILE cFileName = lcFileName FOR STORE = laStoreArr[lnSt] AND !lIgnore AND IIF(lnTranType = 'R',ALLTRIM(UPPER(RETCLSS)) = 'RETURNS',EMPTY(ALLTRIM(RETCLSS)))
SCAN REST WHILE cFileName = lcFileName FOR STORE = IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]) AND;
    IIF(LCRPFLTY = 'H',.T., TRANS =laStoreArr[lnSt,2])AND !lIgnore AND ;
    IIF(lnTranType = 'R',ALLTRIM(UPPER(RETCLSS)) = 'RETURNS',EMPTY(ALLTRIM(RETCLSS)))
  *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]

  *:C201384,1 MMT 11/09/2011 Fix the Problem of Error while saving CM[Start]
  lnCntLine = lnCntLine + 1
  IF TYPE('lcXMLFileName') <> 'C'
    LOOGSCROLL.PARENT.VISIBLE = .F.
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    *oPross.lblFirstLabel.Caption = "Creating Transactions for Store:"+ALLTRIM(laStoreArr[lnSt])
    oPross.lblFirstLabel.CAPTION = "Creating Transactions for Store:"+ALLTRIM(IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]))
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[end]
    oPross.CurrentProgress(lnCntLine)
    oPross.AUTOCENTER = .T.
    opross.VISIBLE = .T.
    oPross.SHOW()
  ENDIF
  *:C201384,1 MMT 11/09/2011 Fix the Problem of Error while saving CM[END]

  lcQtyPos = ALLTRIM(&lcXmlData..SIZE)
  m.Price = &lcXmlData..Price
  IF ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
    m.STYLE = &lcXmlData..STYLE
    m.DEPDEF = SPACE(1)
  ELSE
    m.STYLE = PADR(lcDefStyle,19)
    m.DEPDEF = 'Y'
    *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[Start]
    *lcQtyPos ='1'
    *!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[End]
  ENDIF
  =gfSeek(m.STYLE,'STYLE','STYLE')
  lnTaxRat = 0
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
  IF LCRPFLTY ='H' AND LRPCSTVAT AND !EMPTY(lcRpTaxCd)
     = gfRltFld(lcRpTaxCd , @laTaxRat , 'CTAXCODE')
  ELSE
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]
    IF !EMPTY(STYLE.CTAXCODE)
      = gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')
    ENDIF
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][Start]
  ENDIF
  *!C201771,1 MMT 01/27/2016 Add new options to concession invoicing program[T20160104.0078][End]
  IF lnTaxRat > 0
    *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[Start]
    *m.Price = m.Price -(m.Price*lnTaxRat/100)
    m.Price = m.Price/(1+(lnTaxRat/100))
    *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[END]
  ENDIF

  *!*       IF m.DEPDEF = 'Y'
  *!*         *m.Price = m.Price -(m.Price*lnDefCost/100)
  *!*       ENDIF
  IF !SEEK(&lcXmlData..STORE+STYLE.SEASON+STYLE.CDIVISION+m.STYLE+m.DEPDEF,lcStyleDet)
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
    *!*	       INSERT INTO (lcStyleDet) (STORE,STYLE,qty&lcQtyPos.,PRICE,RETREAS,RETRESN,DEPDEF,SEASON,CDIVISION) VALUES ;
    *!*	                                (laStoreArr[lnSt] ,m.STYLE,&lcXmlData..QTY,m.Price,&lcXmlData..RETREAS,&lcXmlData..RETRESN,m.DEPDEF,STYLE.SEASON,STYLE.CDIVISION)
    INSERT INTO (lcStyleDet) (STORE,STYLE,qty&lcQtyPos.,PRICE,RETREAS,RETRESN,DEPDEF,SEASON,CDIVISION) VALUES ;
      (IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),m.STYLE,&lcXmlData..QTY,m.Price,&lcXmlData..RETREAS,&lcXmlData..RETRESN,m.DEPDEF,STYLE.SEASON,STYLE.CDIVISION)
    *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
  ELSE
    *:C201384,3 MMT 12/08/2011 Fix the Problem of not checking style price before increasing Qty[Start]
    lnCurSelAls = SELECT()
    SELECT (lcStyleDet)
    LOCATE REST WHILE STORE+SEASON+CDIVISION+STYLE+DEPDEF = &lcXmlData..STORE+STYLE.SEASON+STYLE.CDIVISION+m.STYLE+m.DEPDEF FOR PRICE = m.Price
    IF FOUND()
      *:C201384,3 MMT 12/08/2011 Fix the Problem of not checking style price before increasing Qty[end]
      *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
      *REPLACE qty&lcQtyPos. WITH qty&lcQtyPos.+ &lcXmlData..QTY IN (lcXmlData)
      REPLACE qty&lcQtyPos. WITH qty&lcQtyPos.+ &lcXmlData..QTY IN (lcStyleDet)
      *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
      *:C201384,3 MMT 12/08/2011 Fix the Problem of not checking style price before increasing Qty[Start]
    ELSE
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *!*	         INSERT INTO (lcStyleDet) (STORE,STYLE,qty&lcQtyPos.,PRICE,RETREAS,RETRESN,DEPDEF,SEASON,CDIVISION) VALUES ;
      *!*	                                (laStoreArr[lnSt] ,m.STYLE,&lcXmlData..QTY,m.Price,&lcXmlData..RETREAS,&lcXmlData..RETRESN,m.DEPDEF,STYLE.SEASON,STYLE.CDIVISION)
      INSERT INTO (lcStyleDet) (STORE,STYLE,qty&lcQtyPos.,PRICE,RETREAS,RETRESN,DEPDEF,SEASON,CDIVISION) VALUES ;
        (IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),m.STYLE,&lcXmlData..QTY,m.Price,&lcXmlData..RETREAS,&lcXmlData..RETRESN,m.DEPDEF,STYLE.SEASON,STYLE.CDIVISION)
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
    ENDIF
    SELECT(lnCurSelAls)
    *:C201384,3 MMT 12/08/2011 Fix the Problem of not checking style price before increasing Qty[End]
  ENDIF
ENDSCAN

SELECT (lcStyleDet)
LOCATE
IF lnTranType = 'I'
  *!C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
  *SELECT DISTINCT  SEASON,CDIVISION  FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
  IF llRpMrgSeas
    SELECT DISTINCT  CDIVISION  FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
  ELSE
    SELECT DISTINCT  SEASON,CDIVISION  FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
  ENDIF
  *!C201539,2 SAB 12/18/2012 Add New option Merge by season [End]
ELSE
  SELECT DISTINCT  CDIVISION  FROM (lcStyleDet) INTO CURSOR  'DIFFTRAN'
ENDIF
*!*      IF TYPE('lcXMLFileName') = 'C'
*!*        lcRootAriaFolder = ADDBS(oAriaApplication.defaultpath)
*!*        oAriaApplication.ApplicationHome = ALLTRIM(lcRootAriaFolder)+"\PRGS\"
*!*        oAriaApplication.SCREENHOME =   ALLTRIM(lcRootAriaFolder)+"\SCREENS\"
*!*        lcClassHome =  ALLTRIM(lcRootAriaFolder)+"\CLASSES\"
*!*        loOldApp = oAriaApplication
*!*        SET DEFAULT TO  (ALLTRIM(lcRootAriaFolder))
*!*        SET CLASSLIB TO lcClassHome +'MAIN.VCX' ADDITIVE
*!*        SET CLASSLIB TO lcClassHome +'UTILITY.VCX' ADDITIVE
*!*        SET CLASSLIB TO lcClassHome +'GLOBALS.VCX' ADDITIVE
*!*        SET PROCEDURE TO lcRootAriaFolder+'ARIA.EXE'  ADDITIVE
*!*        oAriaApplication = CREATEOBJECT('AriaApplication',loOldApp.systemfilespath)
*!*  *!*        oAriaApplication.AddProperty ('ActiveProgramHelpCNTX', 0)
*!*  *!*        oAriaApplication.AddProperty ('User_Group','')
*!*  *!*        oAriaApplication.AddProperty ('User_Level','')
*!*  *!*        oAriaApplication.ClassDir =lcClassHome
*!*        *Need to make set procedure
*!*      ENDIF
IF !EOF()
  IF TYPE('lcXMLFileName') = 'C'
    *:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][Start]
    *RELEASE PROCEDURE lcRootAriaFolder+'ARIA.EXE'
    *SET PROCEDURE TO (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP') ADDITIVE
    *SET PROCEDURE TO lcRootAriaFolder+'ARIA.EXE'  ADDITIVE
    lcOldProcd = SET("Procedure")
    lcOldProcd  = (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP') +IIF(!EMPTY(lcOldProcd),",",'')+lcOldProcd
    SET PROCEDURE TO &lcOldProcd.
    *:C201384,5 MMT 01/15/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][END]
  ELSE
    *:C201384,5 MMT 01/10/2011 Fix the Problem of not updating BININVJL Table[Start]
    *SET PROCEDURE TO (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP') ADDITIVE
    lcOldProcd = SET("Procedure")
    lcOldProcd  = (oAriaApplication.ApplicationHome + 'ar\ARCNINC.FXP') +IIF(!EMPTY(lcOldProcd),",",'')+lcOldProcd
    SET PROCEDURE TO &lcOldProcd.
    *:C201384,5 MMT 01/10/2011 Fix the Problem of not updating BININVJL Table[END]
  ENDIF
  lnChoice = 1  

  IF lnTranType = 'I'
    SELECT DIFFTRAN
    *!C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
    *SCAN FOR !EMPTY(SEASON) AND !EMPTY(CDIVISION)
    SCAN FOR IIF(llRpMrgSeas, .T., !EMPTY(SEASON)) AND !EMPTY(CDIVISION)
    *!C201539,2 SAB 12/18/2012 Add New option Merge by season [End]

      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
      SELECT (lcStyleDet)
      IF llUse_GL
        =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
        SELECT GLDist
        lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
        lcWorkDir = oAriaApplication.workdir
        COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
        USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
        SELECT (lcTmpGlDt)
      ENDIF
      SELECT (lcStyleDet)
      LOCATE
      llHaveDef = .F.

      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
      *SCAN FOR SEASON = DIFFTRAN.SEASON AND CDIVISION = DIFFTRAN.CDIVISION AND  DEPDEF = 'Y'
      SCAN FOR IIF(llRpMrgSeas, .T., SEASON = DIFFTRAN.SEASON) AND CDIVISION = DIFFTRAN.CDIVISION AND  DEPDEF = 'Y'
      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [End]
        *!*          IF gfSeek(lcDefaultWare +lcDefBin+&lcStyleDet..STYLE,'WHBINLOC')
        *!*            SELECT WHBINLOC
        *!*            REPLACE QTY1 WITH QTY1 - &lcStyleDet..QTY1,;
        *!*                    QTY2 WITH QTY2 - &lcStyleDet..QTY2,;
        *!*                    QTY3 WITH QTY3 - &lcStyleDet..QTY3,;
        *!*                    QTY4 WITH QTY4 - &lcStyleDet..QTY4,;
        *!*                    QTY5 WITH QTY5 - &lcStyleDet..QTY5,;
        *!*                    QTY6 WITH QTY6 - &lcStyleDet..QTY6,;
        *!*                    QTY7 WITH QTY7 - &lcStyleDet..QTY7,;
        *!*                    QTY8 WITH QTY8 - &lcStyleDet..QTY8
        *!*            REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
        *!*            =gfReplace('')
        *!*          ENDIF
        IF DEPDEF = 'Y'
          *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
          IF .F.
            *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
            llHaveDef = .T.
            =gfSeek(&lcStyleDet..STYLE,'STYLE','STYLE')
            DIMENSION laAdjust[9]
            laAdjust = 0
            lnTotQty = 0
            FOR lnF =1 TO 8
              lcF=STR(lnF,1)
              laAdjust[lnF] = &lcStyleDet..QTY&lcF.
              lnTotQty = lnTotQty + &lcStyleDet..QTY&lcF.
            ENDFOR
            lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF'),"")
            IF llUse_GL

              DECLARE laGLDistAr[2,13]
              laGLDistAr[1,1] = lcLinkCode
              laGLDistAr[2,1] = lcLinkCode
              laGLDistAr[1,2] = '006'
              laGLDistAr[2,2] = '007'
              laGLDistAr[1,3] = 1
              laGLDistAr[2,3] = -1
              STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
              STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
              STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
              STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
              STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
              STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
              laGLDistAr[2,10] = lcAdjAcct
            ELSE
              DIME laGLDistAr[1,1]
              laGLDistAr = ''
            ENDIF
            laAdjust[9] = lnTotQty
            *:C201384,5 MMT 01/11/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][Start]
            IF !USED('STYDYE_AB')
              =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_AB')
            ENDIF
            IF  !gfSEEK(&lcStyleDet..STYLE+lcDefaultWare+SPACE(10),'STYDYE_AB')
              DO gpAdStyWar WITH &lcStyleDet..STYLE, SPACE(10), lcDefaultWare
            ENDIF
            *:C201384,5 MMT 01/11/2012 Fix the Problem of not updating BININVJL Table[T20110621.0057][END]
            lnRet=gfStyCrl('1',&lcStyleDet..STYLE,lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price -(&lcStyleDet..Price*lnDefCost/100)),;
              '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')


            lfUpdateBinLocFiles(&lcStyleDet..STYLE,lcDefaultWare,lcDefBin)
            *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
          ENDIF
          DIMENSION laAdjust[9]
          laAdjust = 0
          lnTotQty = 0
          FOR lnF =1 TO 8
            lcF=STR(lnF,1)
            laAdjust[lnF] = &lcStyleDet..QTY&lcF.
            lnTotQty = lnTotQty + &lcStyleDet..QTY&lcF.
          ENDFOR
          laAdjust[9] = lnTotQty
          lfUpBinWh(&lcStyleDet..STYLE,lcDefaultWare,lcDefBin)
          *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]

        ENDIF
      ENDSCAN
      *!*        SELECT WHBINLOC
      *!*        =gfTableUpdate()
      IF llUse_GL AND  llHaveDef
        SELECT (lcTmpGlDt)
        *-- Generate a unique session number.
        lcGlSess = gfSequence('GLSESSION')
        REPLACE ALL GLSESSION WITH lcGlSess
        USE
        SELECT GLDIST
        APPEND FROM (lcWorkDir+lcTmpGlDt+'.DBF')
        ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
        SELECT STYLE
        =gfTableUpdate()
      ENDIF
      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]
      lnChoice = 1
      IF TYPE('loDInv') <> "O"
        *            TRY
        DO FORM (oAriaApplication.ScreenHome+"ar\ardinv.scx") NOSHOW NAME loDInv LINKED
        *           CATCH
        *          ENDTRY
        loDInv.lBranchScreen=.F.
        loDInv.NAME = "AARARDINV"
        loDInv.minittriggers ()
      ENDIF
      *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[Start]
      *!*            loDInv.DefaultInvoiceDate = oAriaApplication.SystemDate
      *!*            loDInv.DefaultPostingDate= oAriaApplication.SystemDate
      loDInv.DefaultInvoiceDate = IIF(!EMPTY(ldRpInvDat),ldRpInvDat,oAriaApplication.SystemDate)
      loDInv.DefaultPostingDate = IIF(!EMPTY(ldRpInvDat),ldRpInvDat,oAriaApplication.SystemDate)
      *!C201486,1 MMT 05/22/2012 Add Date and Division filter to Concession invoicing program[END]
      loDInv.laSetups[18,2] = 'N'
      loDInv.mCreateTempfiles
      loDInv.DefaultWarecode = lcDefaultWare
      loDInv.changemode ('A')
      WITH loDInv.AriaForm1.AriaPageframe1.Page2.InvoiceEditRegion1
        STORE 0 TO .TaxDueAmount, .Merchandisetax, .TotalCartons
      ENDWITH
      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
      *loDInv.DefaultSeason =  DIFFTRAN.SEASON &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboSeason.CodeDefaultValue
      IF llRpMrgSeas
        loDInv.DefaultSeason =  '*'
      ELSE
        loDInv.DefaultSeason =  DIFFTRAN.SEASON &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboSeason.CodeDefaultValue
      ENDIF
      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [End]
      loDInv.DefaultDivision = DIFFTRAN.CDIVISION &&loDInv.AriaForm1.AriaPageFrame1.Page1.cboDivision.CodeDefaultValue
      loDInv.ariaform1.keyAccount.keytextbox.oldValue = SPACE(5)
      loDInv.ariaform1.keyAccount.keytextbox.VALUE = lcrpacct
      loDInv.ariaform1.keyAccount.keytextbox.VALID
      loDInv.ariaform1.keySTORE.keytextbox.oldValue = SPACE(5)
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *loDInv.ariaform1.keySTORE.keytextbox.Value = laStoreArr[lnSt]
      loDInv.ariaform1.keySTORE.keytextbox.VALUE = IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1])
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      loDInv.ariaform1.keySTORE.keytextbox.VALID
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *=SEEK('S'+lcrpacct+laStoreArr[lnSt],'CUSTOMER','CUSTOMER')
      =SEEK('S'+lcrpacct+IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),'CUSTOMER','CUSTOMER')
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      lnDisc = Customer.Disc

      lcDatse = SET("Datasession")
      SET DATASESSION TO loDInv.DATASESSIONID
      REPLACE  DiscPcnt   WITH lnDisc IN (loDInv.lcInvhdr)
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][Start]
      REPLACE CUSTPO WITH 'Auto Created' IN (loDInv.lcInvhdr)
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][End]
      SET DATASESSION TO lcDatse
      SELECT (lcStyleDet)
      LOCATE      
      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [Start]
      *SCAN FOR SEASON = DIFFTRAN.SEASON AND CDIVISION = DIFFTRAN.CDIVISION
      SCAN FOR IIF(llRpMrgSeas, .T., SEASON = DIFFTRAN.SEASON) AND CDIVISION = DIFFTRAN.CDIVISION
      *!C201539,2 SAB 12/18/2012 Add New option Merge by season [End]
        *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
        lnRetPriceValue = &lcStyleDet..PRICE
        *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()
        STORE .T. TO loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llNewline,loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.llAddLine
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.ENABLED = .T.
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.VALUE = &lcStyleDet..STYLE
        *loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.TxtItem.Valid
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.keyStyle.VALID(.T.,0,&lcStyleDet..STYLE,SPACE(19),&lcStyleDet..STYLE)
        
        *!B611115,3 MMT 02/23/2016 Issue#2 - Incorrect Tax amount in Invoice created by concession [P20160126.0001-Issue#2][Start]
		IF LCRPFLTY ='H' AND LRPCSTVAT AND !EMPTY(lcRpTaxCd)
          SET DATASESSION TO loDInv.DATASESSIONID
          REPLACE nTaxRate WITH lnTaxRat IN (loDInv.lcInvLine)
          SET DATASESSION TO lcDatse
        ENDIF  
        *!B611115,3 MMT 02/23/2016 Issue#2 - Incorrect Tax amount in Invoice created by concession [P20160126.0001-Issue#2][End]



        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.mResetControlSource ()


        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[Start]
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.VALUE  = &lcStyleDet..PRICE
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.LOSTFOCUS()
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.VALUE = 0
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[END]

        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty1.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty2.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty3.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty4.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty5.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty6.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty7.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty8.CONTROLSOURCE = ''
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.CONTROLSOURCE = ''

        FOR lnX= 1 TO 8
          lcX = STR(lnX,1)
          IF &lcStyleDet..Qty&lcX. > 0
            *loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..ControlSource = ''
            * loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..OldValue  = 0
            loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALUE  =  &lcStyleDet..Qty&lcX.
            loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtQty&lcX..VALID
          ENDIF
        ENDFOR
        *loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.txtTotQty.ControlSource = ''
        
        lcOldCostMth = loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.CostingMethod
        IF INLIST(loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.CostingMethod,'F','L') AND &lcStyleDet..DEPDEF = 'Y'
          loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.CostingMethod = 'A'
        ENDIF
        
        loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.cntQuantity.LOSTFOCUS()

        
        IF &lcStyleDet..DEPDEF = 'Y'
          loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.CostingMethod =lcOldCostMth
        ENDIF
        

        *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
        IF &lcStyleDet..DEPDEF = 'Y'
          lnStyDetPrice = &lcStyleDet..Price
          lcDatLInSe = SET("Datasession")
          SET DATASESSION TO loDInv.DATASESSIONID
          *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
*!*	          REPLACE COST WITH (lnStyDetPrice -(lnStyDetPrice*lnDefCost/100)),;
*!*	            CADD_USER WITH 'CONCESDEF'IN (loDInv.lcInvLine)
          REPLACE COST WITH (lnStyDetPrice*lnDefCost/100),;
                  CADD_USER WITH 'CONCESDEF'IN (loDInv.lcInvLine)
          *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
          SET DATASESSION TO lcDatLInSe
        ENDIF
        =gfAdd_Info(loDInv.lcInvLine,loDInv)
        *!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]
        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[Start]
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.OldValue  = 0
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.Value  = &lcStyleDet..PRICE
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.txtNetPrice.lostfocus()
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.oldValue = 0
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.Value = 0
        *!*              loDInv.AriaForm1.AriaPageFrame1.Page2.invoiceeditregion1.spnDiscount.LOSTFOCUS()
        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[END]
      ENDSCAN

      lcDatse = SET("Datasession")
      SET DATASESSION TO loDInv.DATASESSIONID
      REPLACE Binloc1 WITH lcDefBin ,;
        Binloc2 WITH lcDefBin ,;
        Binloc3 WITH lcDefBin ,;
        Binloc4 WITH lcDefBin ,;
        Binloc5 WITH lcDefBin ,;
        Binloc6 WITH lcDefBin ,;
        Binloc7 WITH lcDefBin ,;
        Binloc8 WITH lcDefBin ALL IN (loDInv.lcInvLine)
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][Start]
      REPLACE CUSTPO WITH 'Auto Created' ALL IN (loDInv.lcInvLine)
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][End]
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
      lnDetLins = RECCOUNT(loDInv.lcInvLine)
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
      
      *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[Start]
      IF lcOrderNumSeq = 'Y'
        REPLACE ORDER WITH gfSequence('ORDER','','',EVALUATE(loDInv.lcInvHdr+'.cDivision')) IN (loDInv.lcInvHdr)
      ENDIF
      *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[End]

      SET DATASESSION TO lcDatse

      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
      IF lnDetLins <= 0
        LOOP
      ENDIF
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]

      loDInv.AriaForm1.AriaPageFrame1.Page3.ACTIVATE()
      lnChoice = 2
      DIMENSION  laInv[1]
      laInv[1] = ''

      *BINDEVENT(loDInv.AriaForm1.KeyInvoice.KeyTextBox,'SetFocus',loDInv,'Documentation')
      loDInv.HIDE()
      loDInv.SaveFiles(.F.)
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *INSERT INTO (lcTransRange) (cFileName,STORE,TrnType,TrnNum) VALUES (lcFileName, laStoreArr[lnSt],'I',laInv[1])
      INSERT INTO (lcTransRange) (cFileName,STORE,TrnType,TrnNum) VALUES (lcFileName, IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),'I',laInv[1])
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      IF !EMPTY(laInv[1])
        SELECT noTepad
        APPEND BLANK
        *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
        *!*	            REPLACE TYPE   WITH 'C',;
        *!*	                    KEY    With laInv[1],;
        *!*	                    cDesc  WITH 'Notes For Invoice Number : '+laInv[1],;
        *!*	                    mnotes WITH 'Concession Invoice for Store '+ALLTRIM(laStoreArr[lnSt])+' dated '+DTOC(oAriaApplication.SYStemDate)
        REPLACE TYPE   WITH 'C',;
          KEY    WITH laInv[1],;
          cDesc  WITH 'Notes For Invoice Number : '+laInv[1],;
          mnotes WITH 'Concession Invoice for Store '+ALLTRIM(IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]))+' dated '+DTOC(oAriaApplication.SYStemDate)
        *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
        =gfAdd_Info('noTepad')
        =gfReplace('')
        =gfTableUpDate()
      ENDIF

      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
      *!*            SELECT (lcStyleDet)
      *!*            IF llUse_GL
      *!*              =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
      *!*              SELECT GLDist
      *!*              lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
      *!*              lcWorkDir = oAriaApplication.workdir
      *!*              COPY STRU TO (lcWorkDir+lcTmpGlDt)
      *!*              USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
      *!*              SELECT (lcTmpGlDt)
      *!*            ENDIF
      *!*            SELECT (lcStyleDet)
      *!*            SCAN FOR SEASON = DIFFTRAN.SEASON AND CDIVISION = DIFFTRAN.CDIVISION
      *!*              *!*          IF gfSeek(lcDefaultWare +lcDefBin+&lcStyleDet..STYLE,'WHBINLOC')
      *!*              *!*            SELECT WHBINLOC
      *!*              *!*            REPLACE QTY1 WITH QTY1 - &lcStyleDet..QTY1,;
      *!*              *!*                    QTY2 WITH QTY2 - &lcStyleDet..QTY2,;
      *!*              *!*                    QTY3 WITH QTY3 - &lcStyleDet..QTY3,;
      *!*              *!*                    QTY4 WITH QTY4 - &lcStyleDet..QTY4,;
      *!*              *!*                    QTY5 WITH QTY5 - &lcStyleDet..QTY5,;
      *!*              *!*                    QTY6 WITH QTY6 - &lcStyleDet..QTY6,;
      *!*              *!*                    QTY7 WITH QTY7 - &lcStyleDet..QTY7,;
      *!*              *!*                    QTY8 WITH QTY8 - &lcStyleDet..QTY8
      *!*              *!*            REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      *!*              *!*            =gfReplace('')
      *!*              *!*          ENDIF
      *!*              IF DEPDEF = 'Y'
      *!*                =gfSeek(&lcStyleDet..STYLE,'STYLE','STYLE')
      *!*                DIMENSION laAdjust[9]
      *!*                laAdjust = 0
      *!*                lnTotQty = 0
      *!*                FOR lnF =1 TO 8
      *!*                  lcF=STR(lnF,1)
      *!*                  laAdjust[lnF] = &lcStyleDet..QTY&lcF.
      *!*                  lnTotQty = lnTotQty + &lcStyleDet..QTY&lcF.
      *!*                ENDFOR
      *!*                lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(Style.Link_Code),Style.Link_Code,'DEFDEF'),"")
      *!*                IF llUse_GL
      *!*
      *!*                  DECLARE laGLDistAr[2,13]
      *!*                  laGLDistAr[1,1] = lcLinkCode
      *!*                  laGLDistAr[2,1] = lcLinkCode
      *!*                  laGLDistAr[1,2] = '006'
      *!*                  laGLDistAr[2,2] = '007'
      *!*                  laGLDistAr[1,3] = 1
      *!*                  laGLDistAr[2,3] = -1
      *!*                  STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
      *!*                  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
      *!*                  STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
      *!*                  STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      *!*                  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      *!*                  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
      *!*                  laGLDistAr[2,10] = lcAdjAcct
      *!*                ELSE
      *!*                  DIME laGLDistAr[1,1]
      *!*                  laGLDistAr = ''
      *!*                ENDIF
      *!*                laAdjust[9] = lnTotQty
      *!*                lnRet=gfStyCrl('1',&lcStyleDet..Style,lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price -(&lcStyleDet..Price*lnDefCost/100)),;
      *!*                               '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
      *!*
      *!*                *MT
      *!*                lfUpdateBinLocFiles(&lcStyleDet..Style,lcDefaultWare,IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty) and !EMPTY(lcWriteOffBin),lcWriteOffBin,lcRetBin))
      *!*                *MT
      *!*
      *!*              ENDIF
      *!*            ENDSCAN
      *!*            *!*        SELECT WHBINLOC
      *!*            *!*        =gfTableUpdate()
      *!*            IF llUse_GL
      *!*              SELECT (lcTmpGlDt)
      *!*              *-- Generate a unique session number.
      *!*              lcGlSess = gfSequence('GLSESSION')
      *!*              REPLACE ALL GLSESSION WITH lcGlSess
      *!*              USE
      *!*              SELECT GLDIST
      *!*              APPEND FROM (lcWorkDir+lcTmpGlDt)
      *!*              ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
      *!*            ENDIF
      *!*         ENDSCAN
      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]
    ENDSCAN
  ELSE
    **Credit memo Case



    SELECT DIFFTRAN
    SCAN FOR !EMPTY(CDIVISION)

      *:C201384,5 MMT 01/10/2011 Fix the Problem of not updating BININVJL Table[Start]
      SELECT (lcStyleDet)
      IF llUse_GL
        =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
        SELECT GLDist
        lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
        lcWorkDir = oAriaApplication.workdir
        COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
        USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
        SELECT (lcTmpGlDt)
      ENDIF
      SELECT (lcStyleDet)
      LOCATE
      llHaveDef = .F.
      SCAN FOR  CDIVISION = DIFFTRAN.CDIVISION AND DEPDEF = 'Y'
        IF DEPDEF = 'Y'
          llHaveDef = .T.
          =gfSeek(IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE),'STYLE','STYLE')
          DIMENSION laAdjust[9]
          laAdjust = 0
          lnTotQty = 0
          FOR lnF =1 TO 8
            lcF=STR(lnF,1)
            laAdjust[lnF] = -1 * &lcStyleDet..QTY&lcF.
            lnTotQty = lnTotQty + -1 * &lcStyleDet..QTY&lcF.
          ENDFOR
          lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(STYLE.Link_Code),STYLE.Link_Code,'DEFDEF'),"")
          IF llUse_GL

            DECLARE laGLDistAr[2,13]
            laGLDistAr[1,1] = lcLinkCode
            laGLDistAr[2,1] = lcLinkCode
            laGLDistAr[1,2] = '006'
            laGLDistAr[2,2] = '007'
            laGLDistAr[1,3] = 1
            laGLDistAr[2,3] = -1
            STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
            STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
            *MT02/26
            *STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
            STORE ldRpInvDat TO laGLDistAr[1,6],laGLDistAr[2,6]
            *MT
            STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
            STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
            STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
            laGLDistAr[2,10] = lcAdjAcct
          ELSE
            DIME laGLDistAr[1,1]
            laGLDistAr = ''
          ENDIF
          laAdjust[9] = lnTotQty
          IF !USED('STYDYE_AB')
            =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_AB')
          ENDIF
          IF  !gfSEEK(IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE)+lcDefaultWare+SPACE(10),'STYDYE_AB')
            DO gpAdStyWar WITH IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE), SPACE(10), lcDefaultWare
          ENDIF
          *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]  
*!*	          lnRet=gfStyCrl('1',IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE),lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price -(&lcStyleDet..Price*lnDefCost/100)),;
*!*	            '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
          =SEEK('S'+lcrpacct+IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),'CUSTOMER','CUSTOMER')
          *MT02/26
*!*            lnRet=gfStyCrl('1',IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE),lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price* lnDefCost/100),;
*!*              '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
          lnRet=gfStyCrl('1',IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE),lcDefaultWare,SPACE(10),ldRpInvDat,'',@laAdjust, (&lcStyleDet..Price* lnDefCost/100),;
            '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
          *MT   
          *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
          lfUpdateBinLocFiles(IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty),lcDefDamSty,&lcStyleDet..STYLE),lcDefaultWare,IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty) AND !EMPTY(lcWriteOffBin),lcWriteOffBin,lcRetBin))
        ENDIF
      ENDSCAN
      IF llUse_GL AND  llHaveDef
        SELECT (lcTmpGlDt)
        *-- Generate a unique session number.
        lcGlSess = gfSequence('GLSESSION')
        REPLACE ALL GLSESSION WITH lcGlSess
        USE
        SELECT GLDIST
        APPEND FROM (lcWorkDir+lcTmpGlDt+'.DBF')
        ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
        SELECT STYLE
        =gfTableUpdate()
      ENDIF
      *:C201384,5 MMT 01/10/2011 Fix the Problem of not updating BININVJL Table[END]

      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[Start]
      *!*	          IF llUse_GL
      *!*	            =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
      *!*	            SELECT GLDist
      *!*	            lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
      *!*	            lcWorkDir = oAriaApplication.workdir
      *!*	            COPY STRU TO (lcWorkDir+lcTmpGlDt)
      *!*	            USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
      *!*	            SELECT (lcTmpGlDt)
      *!*	          ENDIF
      *!*
      *!*	          SELECT (lcStyleDet)
      *!*	          SCAN FOR CDIVISION = DIFFTRAN.CDIVISION
      *!*	            IF DEPDEF = 'Y'
      *!*	              =gfSeek(&lcStyleDet..STYLE,'STYLE','STYLE')
      *!*	              DIMENSION laAdjust[9]
      *!*	              laAdjust = 0
      *!*	              lnTotQty = 0
      *!*	              FOR lnF =1 TO 8
      *!*	                lcF=STR(lnF,1)
      *!*	                laAdjust[lnF] = -1 * &lcStyleDet..QTY&lcF.
      *!*	                lnTotQty = lnTotQty + -1 * &lcStyleDet..QTY&lcF.
      *!*	              ENDFOR
      *!*	              lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(Style.Link_Code),Style.Link_Code,'DEFDEF'),"")
      *!*	              IF llUse_GL
      *!*
      *!*	                DECLARE laGLDistAr[2,13]
      *!*	                laGLDistAr[1,1] = lcLinkCode
      *!*	                laGLDistAr[2,1] = lcLinkCode
      *!*	                laGLDistAr[1,2] = '006'
      *!*	                laGLDistAr[2,2] = '007'
      *!*	                laGLDistAr[1,3] = 1
      *!*	                laGLDistAr[2,3] = -1
      *!*	                STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
      *!*	                STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
      *!*	                STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
      *!*	                STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      *!*	                STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      *!*	                STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
      *!*	                laGLDistAr[2,10] = lcAdjAcct
      *!*	              ELSE
      *!*	                DIME laGLDistAr[1,1]
      *!*	                laGLDistAr = ''
      *!*	              ENDIF
      *!*
      *!*	              laAdjust[9] = lnTotQty
      *!*	              lnRet=gfStyCrl('1',&lcStyleDet..Style,lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price -(&lcStyleDet..Price*lnDefCost/100)),;
      *!*	                  '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
      *!*	              *MT
      *!*	              lfUpdateBinLocFiles(&lcStyleDet..Style,lcDefaultWare,IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty) and !EMPTY(lcWriteOffBin),lcWriteOffBin,lcRetBin))
      *!*	              *MT
      *!*	            ENDIF
      *!*	          ENDSCAN
      *!*        SELECT WHBINLOC
      *!*        =gfTableUpdate()
      *!*	          IF llUse_GL
      *!*	            SELECT (lcTmpGlDt)
      *!*	            *-- Generate a unique session number.
      *!*	            lcGlSess = gfSequence('GLSESSION')
      *!*	            REPLACE ALL GLSESSION WITH lcGlSess
      *!*	            USE
      *!*	            SELECT GLDIST
      *!*	            APPEND FROM (lcWorkDir+lcTmpGlDt)
      *!*	            ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
      *!*	          ENDIF
      *:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]

      lnChoice = 1
      IF TYPE('loCrMem') <> "O"
        DO FORM (oAriaApplication.ScreenHome+"RMCRMEM.scx") NOSHOW NAME loCrMem LINKED
        loCrMem.lBranchScreen=.F.
        loCrMem.NAME = "AARRMCRMEM"
        loCrMem.minittriggers ()
        lcDatse = SET("Datasession")
        SET DATASESSION TO loCrMem.DATASESSIONID
        IF  ASCAN(loCrMem.laEvntTrig,PADR('ADDRMFLD',10)) <> 0 .AND. loCrMem.mDoTrigger(PADR('ISUSEBIN',10))
          loFormSet = loCrMem
          lcCrMmStyl = loFormSet.ariaform1.rmcrmbus.lcCrMmStyl
          lcCrMmLine = loFormSet.ariaform1.rmcrmbus.lcCrMmLine
          lcCrMemLin = loFormSet.ariaform1.rmcrmbus.lcCrMemLin
          llLink_GL  = loFormSet.ariaform1.rmcrmbus.llLink_GL
          DO lfCrtUnComp IN (oAriaApplication.ApplicationHome + 'RMSave.FXP') WITH .F. , .T.
        ENDIF
        SET DATASESSION TO lcDatse
      ENDIF
      loCrMem.ChangeMode ('A')
      
      loCrMem.AriaForm1.pgfReCrmem.pgheader.cntHeader.DtCrdDate.Text1.VALUE  =IIF(!EMPTY(ldRpInvDat),ldRpInvDat,oAriaApplication.SystemDate)
      loCrMem.AriaForm1.DtPostDate.Text1.VALUE = IIF(!EMPTY(ldRpInvDat),ldRpInvDat,oAriaApplication.SystemDate)
      
      loCrMem.AriaForm1.KBAccount.keyTextbox.VALUE = lcrpacct
      loCrMem.AriaForm1.KBAccount.keyTextbox.oldValue = SPACE(5)
      loCrMem.AriaForm1.KBAccount.keyTextbox.VALID
      loCrMem.ariaform1.KBSTORE.keytextbox.oldValue = SPACE(5)
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *loCrMem.ariaform1.KBSTORE.keytextbox.Value = laStoreArr[lnSt]
      loCrMem.ariaform1.KBSTORE.keytextbox.VALUE = IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1])
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      loCrMem.ariaform1.KBSTORE.keytextbox.VALID
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboDivision.VALUE =  DIFFTRAN.CDIVISION
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE =  &lcStyleDet..RETREAS
      loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboLocation.VALUE = lcDefaultWare
      loCrMem.ariaForm1.rmCRMBUS.mUpdHdrFl(loCrMem.ariaForm1.rmCRMBUS.lcCrMemHdr)
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][Start]
      loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Value= 'Auto Created'
      loCrMem.ariaForm1.pgfReCrmem.pgHeader.cntHeader.txtpONo.Valid
      *!C201666,1 MMT 04/21/2015 Concession invoice update Custpo field in RETHDR and INVHDR [T20150407.0001][End]
      *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[Start]
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *=SEEK('S'+lcrpacct+laStoreArr[lnSt],'CUSTOMER','CUSTOMER')
      =SEEK('S'+lcrpacct+IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),'CUSTOMER','CUSTOMER')
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]
      lnDisc = Customer.Disc
      *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[END]
      SELECT (lcStyleDet)
      SCAN FOR  CDIVISION = DIFFTRAN.CDIVISION
        *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[Start]
        lnRetPriceValue = &lcStyleDet..PRICE
        *:C201384,1 MMT 11/03/2011 Fix the Problem of Error while saving CM[END]
        loCrMem.ariaForm1.rmCRMBUS.llAddLine = .T.
        WITH loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail
          .cboReason.VALUE = loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE
          .cboReason.REQUERY()
          .cboQuality.VALUE = loCrMem.laStyGrade[1,2]
          .cboQuality.REQUERY()
          STORE " " TO .KbStyle.VALUE , .txtStyDesc.VALUE , loCrMem.ariaForm1.rmCRMBUS.lcCurLine ,;
            loCrMem.ariaForm1.rmCRMBUS.lcTranCd , .KbStyRetTo.VALUE ,;
            loCrMem.ariaForm1.rmCRMBUS.lcScale  , .kbConfig.KeyTExtBOx.VALUE , .kbDyelot.KeyTExtBOx.VALUE
          STORE 0   TO .txtTotAlQty.VALUE , .txtPrice.VALUE , .txtTotAmount.VALUE , .txtGrsPrice.VALUE , .txtDiscount.VALUE ,;
            .txtHstRat.VALUE , .txtPstRat.VALUE ,;
            loCrMem.ariaForm1.rmCRMBUS.lnScaleCnt , loCrMem.ariaForm1.rmCRMBUS.lnCost , loCrMem.ariaForm1.rmCRMBUS.lnDisc_Amt , loCrMem.ariaForm1.rmCRMBUS.lnTrde_Amt ,;
            loCrMem.ariaForm1.rmCRMBUS.lnPstTotal , loCrMem.ariaForm1.rmCRMBUS.lnDiscPcnt
          STORE '' TO .cboEmpl.VALUE
          STORE 0 TO .txtCost.VALUE
          .SbrkBackToStk.SCALE = ""
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            .SbrkBackToStk.txtQty&lcI..VALUE = 0
            .SbrkBackToStk.txtsizelbl&lcI..VALUE = ""
          ENDFOR
          .SbrkBackToStk.txtTotQty.VALUE =  0
          .SbrkBackToStk.txtTotQty.ENABLED = .F.
        ENDWITH

        loCrMem.AriaForm1.llLinStat  = .T.
        STORE .F. TO loCrMem.AriaForm1.llRetStat  ,loCrMem.AriaForm1.llDyeStat  , loCrMem.AriaForm1.llsizestat
        loCrMem.AriaForm1.mShowObj()
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALUE = " "
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.TxtItem.VALUE =&lcStyleDet..STYLE
        llOldCst = loCrMem.Ariaform1.RMCRMBUS.llStdCost
        loCrMem.Ariaform1.RMCRMBUS.llStdCost = .T.

        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.VALID(.T.,0,&lcStyleDet..STYLE,SPACE(19),&lcStyleDet..STYLE)
        *            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyle.TxtItem.Valid()
        loCrMem.Ariaform1.RMCRMBUS.llStdCost = llOldCst
        *!B609963,3 MMT 08/05/2012 Fix Issue#6 in Concession Project[T20120130.0001][Start]
        *lnCstValue = IIF(&lcStyleDet..DEPDEF = 'Y',(&lcStyleDet..Price*lnDefCost/100),&lcStyleDet..PRICE)
        lnCstValue = (&lcStyleDet..Price*lnDefCost/100)
        llDefaultSty = (&lcStyleDet..DEPDEF = 'Y')
        *!B609963,3 MMT 08/05/2012 Fix Issue#6 in Concession Project[T20120130.0001][END]
        lcDatse = SET("Datasession")
        SET DATASESSION TO loCrMem.DATASESSIONID
        lcCrMemLin = loCrMem.Ariaform1.RMCRMBUS.lcCrMemLin
        loCrMem.Ariaform1.RMCRMBUS.lnCost = loCrMem.Ariaform1.RMCRMBUS.mcostassign(.T.)
        *REPLACE COST WITH loCrMem.Ariaform1.RMCRMBUS.lnCost IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        *!B609963,3 MMT 08/05/2012 Fix Issue#6 in Concession Project[T20120130.0001][Start]
*!*	        loCrMem.Ariaform1.RMCRMBUS.lnCost = lnCstValue 
*!*	        REPLACE COST WITH lnCstValue  IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        loCrMem.Ariaform1.RMCRMBUS.lnCost = IIF(llDefaultSty ,lnCstValue ,loCrMem.Ariaform1.RMCRMBUS.lnCost)
        REPLACE COST WITH IIF(llDefaultSty ,lnCstValue ,loCrMem.Ariaform1.RMCRMBUS.lnCost) IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        *!B609963,3 MMT 08/05/2012 Fix Issue#6 in Concession Project[T20120130.0001][END]
        SET DATASESSION TO lcDatse
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALUE = &lcStyleDet..RETREAS
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboReason.VALID()
        loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.VALUE= &lcStyleDet..RETREAS
        *loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.cboReason.ValID
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.OldValue = 0
        *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = &lcStyleDet..PRICE
        *loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALUE = IIF(&lcStyleDet..DEPDEF = 'Y',(&lcStyleDet..Price/100),&lcStyleDet..PRICE)
        *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtGrsPrice.VALID

        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[Start]
        IF TYPE('loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue') <> 'N'
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.OldValue = 0
        ENDIF
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALUE = lnDisc
        loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtDiscount.VALID
        *:C201384,1 MMT 11/13/2011 Fix the Problem of Error while saving CM[END]

        FOR lnX= 1 TO 8
          lcX = STR(lnX,1)
          IF &lcStyleDet..Qty&lcX. > 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..OldValue  = 0
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALUE  =  &lcStyleDet..Qty&lcX.
            loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.SbrkBackToStk.txtQty&lcX..VALID
          ENDIF
        ENDFOR

        *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[Start]
        IF ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty)
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboQuality.VALUE = '3'
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboQuality.VALID()
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyRetTo.cquality = loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.cboQuality.VALUE
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyRetTo.TxtItem.VALUE =lcDefDamSty
          loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.KbStyRetTo.VALID(.T.,0,lcDefDamSty,SPACE(19),lcDefDamSty)
        ENDIF
        *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[End]
        *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
        *loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtCost.VALUE =IIF(&lcStyleDet..DEPDEF = 'Y',(&lcStyleDet..Price*lnDefCost/100),&lcStyleDet..PRICE)
        *loCrMem.AriaForm1.pgfReCrmem.pgDetail.cntDetail.txtCost.VALID()
        *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
      ENDSCAN
      *!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][Start]
      lnTaxRat = 0
	  IF LCRPFLTY ='H' AND LRPCSTVAT AND !EMPTY(lcRpTaxCd)
    	 = gfRltFld(lcRpTaxCd , @laTaxRat , 'CTAXCODE')
	  ENDIF
      *!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][End]
      lcDatse = SET("Datasession")
      SET DATASESSION TO loCrMem.DATASESSIONID
      REPLACE Binloc1 WITH lcRetBin,;
        Binloc2 WITH lcRetBin,;
        Binloc3 WITH lcRetBin,;
        Binloc4 WITH lcRetBin,;
        Binloc5 WITH lcRetBin,;
        Binloc6 WITH lcRetBin,;
        Binloc7 WITH lcRetBin,;
        Binloc8 WITH lcRetBin ALL IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
        
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
      lnDetLins2 = RECCOUNT(loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
      
      *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[Start]
      IF !EMPTY(lcWriteOffBin)
        REPLACE Binloc1 WITH lcWriteOffBin,;
          Binloc2 WITH lcWriteOffBin,;
          Binloc3 WITH lcWriteOffBin,;
          Binloc4 WITH lcWriteOffBin,;
          Binloc5 WITH lcWriteOffBin,;
          Binloc6 WITH lcWriteOffBin,;
          Binloc7 WITH lcWriteOffBin,;
          Binloc8 WITH lcWriteOffBin ALL FOR CSTYGRADE = '3' IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin)
      ENDIF
      *:C201384,1 MMT 11/17/2011 Fix the Problem of Error while saving CM[END]
      IF !USED('WHSLOC')
        =gfOpenTable('WHSLOC','WHSLOC')
      ENDIF
      *!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][Start]
      IF LCRPFLTY ='H' AND LRPCSTVAT AND !EMPTY(lcRpTaxCd)
        lnTotalTax = 0
        lcCrDet = loCrMem.Ariaform1.RMCRMBUS.lcCrMemlin
        SELECT (lcCrDet)
        LOCATE
        SCAN
	      lnTotalTax = lnTotalTax +  lfCalcLineTax(&lcCrDet..Style , &lcCrDet..GROS_PRICE , lcCrDet ,lcrpacct,lnTaxRat ,lnDisc)
        ENDSCAN
        REPLACE Tax_Amt WITH lnTotalTax  IN (loCrMem.Ariaform1.RMCRMBUS.lcCrMemHdr)
        *!B611162,1 MMT 06/27/2016 Custom Concession invoicing calculates CR TotCredit incorrctly[T20160623.0206][Start]
        loCrMem.AriaForm1.pgfReCrmem.pgHEADER.cntHeader.txtTaxAmnt.Value = lnTotalTax  
        *!B611162,1 MMT 06/27/2016 Custom Concession invoicing calculates CR TotCredit incorrctly[T20160623.0206][End]
      ENDIF  
      *!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][End]
      SET DATASESSION TO lcDatse
      
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
      IF lnDetLins2 <= 0
        LOOP
      ENDIF
      *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
      
      lnChoice = 2
      lcCrMemo = SPACE(6)

      loCrMem.SaveFiles(.F.)
      ***Update[Start]
      IF !EMPTY(lcCrMemo)
        *!B611115,2 MMT 02/17/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][Start]
        IF LCRPFLTY ='H' AND LRPCSTVAT AND !EMPTY(lcRpTaxCd)
          IF !USED('RETHDR_TAX')
            =gfOpenTable('RetHdr','RetHdr','SH','RETHDR_TAX')
          ENDIF
          IF gfSeek(lcCrMemo,'RETHDR_TAX')
            SELECT 'RETHDR_TAX'
            =gfReplace('Tax_amt WITH '+STR(lnTotalTax ,12,2))
            =gfTableUpdate()
          ENDIF
        ENDIF  
        *!B611115,2 MMT 02/17/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][End]
        SELECT noTepad
        APPEND BLANK
        *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
        *!*	            REPLACE TYPE   WITH 'R',;
        *!*	                    KEY    With lcCrMemo,;
        *!*	                    cDesc  WITH lcCrMemo,;
        *!*	                    mnotes WITH 'Concession Credit Note for Store '+ALLTRIM(laStoreArr[lnSt])+' dated '+DTOC(oAriaApplication.SYStemDate)
        REPLACE TYPE   WITH 'R',;
          KEY    WITH lcCrMemo,;
          cDesc  WITH lcCrMemo,;
          mnotes WITH 'Concession Credit Note for Store '+ALLTRIM(IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]))+' dated '+DTOC(oAriaApplication.SYStemDate)
        *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
        =gfAdd_Info('noTepad')
        =gfReplace('')
        =gfTableUpDate()
        *MT
        SET DATASESSION TO loCrMem.DATASESSIONID
        loCrMem.Ariaform1.RMCRMBUS.RETHDR.SEEK("******")
        SET DATASESSION TO lcDatse
        *MT
      ENDIF
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
      *INSERT INTO (lcTransRange) (cFileName,STORE,TrnType,TrnNum) VALUES (lcFileName, laStoreArr[lnSt],'R',lcCrMemo)
      INSERT INTO (lcTransRange) (cFileName,STORE,TrnType,TrnNum) VALUES (lcFileName, IIF(LCRPFLTY = 'H',laStoreArr[lnSt],laStoreArr[lnSt,1]),'R',lcCrMemo)
      *C201468,1 MMT Custom Concession program for GPS (Top Shop case)[End]
      SELECT (lcStyleDet)


      *!*            IF llUse_GL
      *!*              =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
      *!*              SELECT GLDist
      *!*              lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
      *!*              lcWorkDir = oAriaApplication.workdir
      *!*              COPY STRU TO (lcWorkDir+lcTmpGlDt)
      *!*              USE (lcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
      *!*              SELECT (lcTmpGlDt)
      *!*            ENDIF
      *!*
      *!*            SELECT (lcStyleDet)
      *!*            SCAN FOR CDIVISION = DIFFTRAN.CDIVISION
      *!*              IF DEPDEF = 'Y'
      *!*                =gfSeek(&lcStyleDet..STYLE,'STYLE','STYLE')
      *!*                DIMENSION laAdjust[9]
      *!*                laAdjust = 0
      *!*                lnTotQty = 0
      *!*                FOR lnF =1 TO 8
      *!*                  lcF=STR(lnF,1)
      *!*                  laAdjust[lnF] = -1 * &lcStyleDet..QTY&lcF.
      *!*                  lnTotQty = lnTotQty + -1 * &lcStyleDet..QTY&lcF.
      *!*                ENDFOR
      *!*                lcLinkCode = IIF(llUse_GL ,IIF(!EMPTY(Style.Link_Code),Style.Link_Code,'DEFDEF'),"")
      *!*                IF llUse_GL
      *!*
      *!*                  DECLARE laGLDistAr[2,13]
      *!*                  laGLDistAr[1,1] = lcLinkCode
      *!*                  laGLDistAr[2,1] = lcLinkCode
      *!*                  laGLDistAr[1,2] = '006'
      *!*                  laGLDistAr[2,2] = '007'
      *!*                  laGLDistAr[1,3] = 1
      *!*                  laGLDistAr[2,3] = -1
      *!*                  STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
      *!*                  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
      *!*                  STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]
      *!*                  STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      *!*                  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      *!*                  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
      *!*                  laGLDistAr[2,10] = lcAdjAcct
      *!*                ELSE
      *!*                  DIME laGLDistAr[1,1]
      *!*                  laGLDistAr = ''
      *!*                ENDIF
      *!*
      *!*                laAdjust[9] = lnTotQty
      *!*                lnRet=gfStyCrl('1',&lcStyleDet..Style,lcDefaultWare,SPACE(10),oAriaApplication.SystemDate,'',@laAdjust, (&lcStyleDet..Price -(&lcStyleDet..Price*lnDefCost/100)),;
      *!*                    '',.T.,lcAdjReason,0,'','',@laGLDistAr,0,"",'')
      *!*                *MT
      *!*                lfUpdateBinLocFiles(&lcStyleDet..Style,lcDefaultWare,IIF(ALLTRIM(UPPER(&lcStyleDet..RETRESN)) == 'FAULTY' AND !EMPTY(lcDefDamSty) and !EMPTY(lcWriteOffBin),lcWriteOffBin,lcRetBin))
      *!*                *MT
      *!*              ENDIF
      *!*            ENDSCAN
      *!*      *!*        SELECT WHBINLOC
      *!*      *!*        =gfTableUpdate()
      *!*            IF llUse_GL
      *!*              SELECT (lcTmpGlDt)
      *!*              *-- Generate a unique session number.
      *!*              lcGlSess = gfSequence('GLSESSION')
      *!*              REPLACE ALL GLSESSION WITH lcGlSess
      *!*              USE
      *!*              SELECT GLDIST
      *!*              APPEND FROM (lcWorkDir+lcTmpGlDt)
      *!*              ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
      *!*            ENDIF
      ***Update[END]
    ENDSCAN


  ENDIF
ENDIF
*:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[Start]
*************************************************************************
*FUNCTION CheckPrd
*DESC: Function to validate transaction date
*NOTE: This function is called from evry transaction program.
*DATE: 02/28/1994
*AUTH: Wael Aly Mohamed
*PARA: ldDate   : Transaction date to be check
*    : lcPeriod : Transaction Period
*    : lcFYear  : Transaction Fiscal Year
*    : lcTranTyp: Type of transaction calls this function
*! MODI:  WAM 09/19/94
*!        1) Modified to call the function 'gfDialog' instead of the
*!           function 'MsgCenter' to display messages when validate
*!           transactions dates. Function'MsgCenter' has been deleted also.
*!B602317,1 WAM 12/06/98 Open SBT system company file with another name
*!B802236,1 AHM 08/05/1999 Allow voiding invoice in prior period
*************************************************************************
FUNCTION Checkprd
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg
PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
  lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias

lnAlias = SELECT()
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [Start]
*STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

*=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
*!B610152,4 SAB 11/11/2012 Fix error when running concession invoicing [End]

lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  *B602317,1 Open SBT system company file with another name
  *=gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  *=SEEK(lcGlComp,'SYCCOMP')

  USE (lcSysDir+'SYCCOMP') ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  *B602317,1 (End)

  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
      WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period
  ENDIF
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither
        && 'Void invoice' nor 'void return'.
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        =oAriaApplication.MESSAGEBOX('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ELSE
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      *B602317,1 Open SBT system company file with another name
      *IF Yearprd <>  SYCCOMP.CURYR+SYCCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
        *B602317,1 (End)

        llDummy   =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        =oAriaApplication.MESSAGEBOX('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF
  ENDIF

  *B602317,1 Open SBT system company file with another name
  USE IN SBTCOMP
  *B602317,1 (End)
ELSE

  *  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.SysPath+'CCOMP_ID','SH')
  *  =SEEK(oAriaApplication.PrntCompanyID,'SYCCOMP')
  lcCompAlias = gfTempName()
  lcSelect = "Select * from SYCCOMP where ccomp_id='"+oAriaApplication.PrntCompanyID+"'"
  lnRemResult = oAriaApplication.remotesystemdata.execute(lcSelect,'',lcCompAlias,"",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))

  IF 'GL' $ &lcCompAlias..mModlset
    *=gfOpenFile(ALLTRIM(SYCCOMP.CCOM_DDIR)+'GLSETUP','','SH')
    USE (oAriaApplication.DataDir+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    llAllPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP
  ELSE
    lDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    llAllPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF
  *E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
  *=gfOpenFile(gcSysHome+'SYCFISHD',gcSysHome+'COMPFYEAR','SH')
  *=gfOpenFile(gcSysHome+'SYCFSPRD',gcSysHome+'COMFYRPRDI','SH')
  =gfOpenFile(oAriaApplication.DataDir+'FISHD',oAriaApplication.DataDir+'COMPFYEAR','SH')
  =gfOpenFile(oAriaApplication.DataDir+'FSPRD',oAriaApplication.DataDir+'COMFYRPRDI','SH')
  *E300692,5 end
  SELECT FSPRD
  llContinue = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
      WHILE (ldDate >= Dfsppbgdt)
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period
  ENDIF
  IF llHideMsg
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
    USE IN (lcCompAlias)
    *! B609159,2 MMT 03/17/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(&lcCompAlias..CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF
  IF llContinue
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    *-- AAMER 11/12/98
    *IF !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
      *-- Check if the system is linked to GL And Allow posting before beginning Balance (End)
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF
  ENDIF
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF
  IF llContinue
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1
        *E300420,1 Button : 00000
        *E300420,1 Ok
        IF lcTranTyp # 'VI1'
          =oAriaApplication.MESSAGEBOX('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        ENDIF
      ELSE
        IF lcTranTyp # 'VI2'
          llContinue = .F.
        ENDIF
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> &lcCompAlias..CCURR_YER+&lcCompAlias..CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
        *E300420,1 Message : 00274
        *E300420,1
        *E300420,1 Button : 00000
        *E300420,1 Ok
        =oAriaApplication.MESSAGEBOX('INM00274B00000','ALERT',lcErrorM1+lcErrorM2)
        *=gfDialog( 'I',lcErrorM1+lcErrorM2)
      ENDIF
    ENDIF
  ENDIF
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [Start]
  USE IN (lcCompAlias)
  *B609159,1 MMT 03/01/2010 CHKPRD open cursor from syccomp when called & doesn't close [End]
ENDIF
IF !llContinue             && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
    && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  *E300420,1 Message : 00274
  *E300420,1
  *E300420,1 Button : 00000
  *E300420,1 Ok
  IF lcTranTyp # 'VI2'
    =oAriaApplication.MESSAGEBOX('INM00274B00000','ALERT',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  ENDIF
  *=gfDialog( 'I',lcErrorM1+lcErrorM2+lcAddMes1+lcAddMes2)
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)
*:C201384,2 MMT 12/01/2011 Fix the Problem of prgram is not working from RB with Mapped Drives[End]


*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[STart]
*:**************************************************************************
*:* Name        : lfIncLineNO
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 12/21/2011
*:* Purpose     : update BININVJL table
*:***************************************************************************
FUNCTION lfUpdateBinLocFiles
LPARAMETERS lcStyle,lcLocation,lcBin
lcCurAlisSel = SELECT()
IF !USED('StyInvjl_bin')
  =gfOpenTable('StyInvjl','STYDATE','SH','StyInvjl_bin')
ENDIF
IF !USED('BININVJL_A')
  =gfOpenTable('BININVJL','WHBINSTY','SH','BININVJL_A')
ENDIF
IF !USED('WHBINLOC_Bin')
  =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_Bin')
ENDIF

IF !USED('WHSLOC_Bin')
  =gfOpenTable('WHSLOC','WHSLOC','SH','WHSLOC_Bin')
ENDIF
LOCAL lnMaxLnNo
lnMaxLnNo = 0

SELECT 'StyInvjl_bin'
SET ORDER TO 'STYDATE' DESCENDING
=gfSeek(lcStyle+lcLocation)
lcSessionID = StyInvjl_bin.CSESSION
SET ORDER TO 'STYDATE' ASCENDING
=gfSeek(lcStyle+lcLocation)
SCAN WHILE STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE = lcStyle+lcLocation FOR CSESSION = lcSessionID
  SCATT MEMVAR MEMO
  m.clocation = lcBin
  SELECT BININVJL_A
  *MT
  IF !gfSeek(m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+STR(m.LINENO,6) +m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE),'BININVJL_A','WHBINSTY')
  *MT
    =lfIncLineNO(@lnMaxLnNo)
    =gfAppend('BININVJL',.T.)
  *MT
  ENDIF
  *MT
ENDSCAN


SELECT WHBINLOC_Bin
lcReplace = ''
IF !gfSEEK(lcLocation +lcBin+lcStyle,'WHBINLOC_Bin')
  SELECT WHSLOC_Bin
  =gfSEEK(lcLocation  +lcBin +SPACE(19),'WHSLOC_Bin')
  SELECT WHBINLOC_Bin
  =gfAppend()
  lcReplace = lcReplace + ;
    'STYLE      WITH ['+lcStyle  +'] '+;
    'CWARECODE  WITH ['+lcLocation  +'] '+;
    'clocation  WITH ['+lcBin+'] '

  lcReplace = lcReplace +;
    'cBlkPck   WITH ['+WHSLOC_Bin.cBlkPck   +'] '+;
    'cSection  WITH ['+WHSLOC_Bin.cSection  +'] '+;
    'cBinClass WITH ['+WHSLOC_Bin.cBinClass +'] '

ENDIF

LOCAL lcKey,lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8
STORE 0 TO lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8

lnAdj1 = laAdjust[1]
lnAdj2 = laAdjust[2]
lnAdj3 = laAdjust[3]
lnAdj4 = laAdjust[4]
lnAdj5 = laAdjust[5]
lnAdj6 = laAdjust[6]
lnAdj7 = laAdjust[7]
lnAdj8 = laAdjust[8]

SELECT WHBINLOC_Bin
lcReplace = lcReplace + ;
  'Qty1       WITH '+ STR( Qty1 + lnAdj1 ) +' '+;
  'Qty2       WITH '+ STR( Qty2 + lnAdj2 ) +' '+;
  'Qty3       WITH '+ STR( Qty3 + lnAdj3 ) +' '+;
  'Qty4       WITH '+ STR( Qty4 + lnAdj4 ) +' '+;
  'Qty5       WITH '+ STR( Qty5 + lnAdj5 ) +' '+;
  'Qty6       WITH '+ STR( Qty6 + lnAdj6 ) +' '+;
  'Qty7       WITH '+ STR( Qty7 + lnAdj7 ) +' '+;
  'Qty8       WITH '+ STR( Qty8 + lnAdj8 )
=gfReplace(lcReplace)

lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
=gfReplace(lcReplace)

IF WHBINLOC_Bin.Qty1 = 0 .AND. WHBINLOC_Bin.Qty2 = 0 .AND. WHBINLOC_Bin.Qty3 = 0 .AND. WHBINLOC_Bin.Qty4 = 0 .AND. ;
    WHBINLOC_Bin.Qty5 = 0 .AND. WHBINLOC_Bin.Qty6 = 0 .AND. WHBINLOC_Bin.Qty7 = 0 .AND. WHBINLOC_Bin.Qty8 = 0 .AND. ;
    gfGetMemVar('M_DELZRBNR')
  =gfDelete()
ENDIF

SELECT WHBINLOC_Bin
=gfTableUpdate()
SELECT  BININVJL_A
=gfTableUpdate()
=gfCloseTable('BININVJL_A')
=gfCloseTable('StyInvjl_bin')
=gfCloseTable('WHBINLOC_Bin')
=gfCloseTable('WHSLOC_Bin')
SELECT(lcCurAlisSel)
*:**************************************************************************
*:* Name        : lfIncLineNO
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 12/21/2011
*:* Purpose     : used to increment the m.LINENO variable to keep the key of BININVJL
*:***************************************************************************
FUNCTION lfIncLineNO
LPARAMETERS lnMaxLnNo
SELECT BININVJL_A
=gfSeek(m.CWARECODE+m.CLOCATION+m.STYLE)
LOCATE
SCAN
  IF CWARECODE+CLOCATION+STYLE+CTRCODE+cirtype+CSESSION+crsession+DTOS(DTRDATE) = ;
      m.CWARECODE+m.CLOCATION+m.STYLE+m.CTRCODE+m.cirtype+m.CSESSION+m.crsession+DTOS(m.DTRDATE)
    lnMaxLnNo = MAX(LINENO,lnMaxLnNo)
    m.LineNo = lnMaxLnNo+1
  ENDIF
ENDSCAN
*:C201384,5 MMT 12/22/2011 Fix the Problem of not updating BININVJL Table[END]

*:C201384,4 MMT 01/24/2012 Fix the Problem of not checking style Qty for all XML files[Start]
*:**************************************************************************
*:* Name        : lfVerifyStkQty
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 01/24/2012
*:* Purpose     : Check XML Qty
*:***************************************************************************
FUNCTION lfVerifyStkQty
SELECT (lcXmlData)
llHasProblem = .F.
SET ORDER TO 'FileDesc'
LOCATE
DO WHILE !EOF()
  lcKeyExp = STORE  + SIZE + UPC
  =SEEK(lcKeyExp)
  lnQtyOld = 0
  SCAN REST WHILE STORE  + SIZE + UPC  = lcKeyExp FOR EMPTY(ALLTRIM((RETCLSS))) AND !lIgnore AND ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
    lcQtyPos = ALLTRIM(&lcXmlData..SIZE)
    IF ALLTRIM(&lcXmlData..DESC) <> 'Dept Default No'
      m.STYLE = &lcXmlData..STYLE
    ELSE
      m.STYLE = PADR(lcDefStyle,19)
    ENDIF
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    lcrpacct = lfGetAcc(&lcXmlData..STORE)
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    lnCncSetSelect = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
      " WHERE [Account]='"+lcrpacct+"' AND [Store]='"+&lcXmlData..STORE+"'",'',;
      "CNSSETFILE","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))

    lcSelDefaultWare  = CNSSETFILE.Concession_Warehouse
    lcSelDefBin = CNSSETFILE.Concession_Default_Bin
    IF !USED('WHBINLOC_A')
      =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_A')
    ENDIF
    *!B611001,1 MMT 05/13/2015 Negative stock in Concession invoicing program [T20150506.0015][Start]
    IF !USED('STYDYE_AA')
      =gfOpenTable("STYDYE","STYDYE",'SH','STYDYE_AA')
    ENDIF
    =gfSeek(PADR(m.STYLE ,19)+PADR(lcSelDefaultWare,6),'STYDYE_AA',"STYDYE")
    *!B611001,1 MMT 05/13/2015 Negative stock in Concession invoicing program [T20150506.0015][End]
    IF !gfSeek(PADR(lcSelDefaultWare,6)+PADR(lcSelDefBin ,10)+m.STYLE ,'WHBINLOC_A')
      *!B610382,3 SAB 09/26/2013 Fix concession issu# 2 Negative Adjustment Problem [T20120108.0010][Issue2] [Start]
      *REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE )+" barcode:"+ &lcXmlData..UPC+"  Size: "+ALLTRIM(&lcXmlData..SIZE)+" is not found in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) IN  TMPSTR
      *llHasProblem = .T.
      *REPLACE lIgnore WITH .T. IN (lcXmlData)
      IF !llRpAutAdj
        REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE )+" barcode:"+ &lcXmlData..UPC+"  Size: "+ALLTRIM(&lcXmlData..SIZE)+" is not found in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) IN  TMPSTR
        llHasProblem = .T.
        REPLACE lIgnore WITH .T. IN (lcXmlData)
      ELSE        
        lcCurrAlias = SELECT(0)
        lnAdjCost    = lfGetAdjCost(m.STYLE, lcSelDefaultWare)
        *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
        *=lfCreateAutAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcXmlData..SIZE), lnAdjCost)
        =lfCreateAutAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcXmlData..SIZE), lnAdjCost,&lcXmlData..Qty)
        *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
        lnQtyOld = lnQtyOld + &lcXmlData..Qty
        SELECT (lcCurrAlias)
      ENDIF      
      *!B610382,3 SAB 09/26/2013 Fix concession issu# 2 Negative Adjustment Problem [T20120108.0010][Issue2] [End]
    ELSE
      *!B611001,1 MMT 05/13/2015 Negative stock in Concession invoicing program [T20150506.0015][Start]
      * IF  WHBINLOC_A.Qty&lcQtyPos. < lnQtyOld+&lcXmlData..Qty
      IF  (WHBINLOC_A.Qty&lcQtyPos. < lnQtyOld+&lcXmlData..Qty) OR (STYDYE_AA.stk&lcQtyPos. < lnQtyOld+&lcXmlData..Qty)
      *!B611001,1 MMT 05/13/2015 Negative stock in Concession invoicing program [T20150506.0015][End]
        *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
        *REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE)+" barcode:"+ &lcXmlData..UPC+"  Size: "+ALLTRIM(&lcXmlData..SIZE)+" There is no enough stock in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) IN  TMPSTR
        *llHasProblem = .T.
        *REPLACE lIgnore WITH .T. IN (lcXmlData)
        IF !llRpAutAdj
          REPLACE mStrRep WITH mStrRep +"STYLE "+ALLTRIM(m.STYLE)+" barcode:"+ &lcXmlData..UPC+"  Size: "+ALLTRIM(&lcXmlData..SIZE)+" There is no enough stock in Bin: "+ALLTRIM(lcSelDefBin)+" at Concession "+ALLTRIM(&lcXmlData..STORE)+ CHR(13) +CHR(10) IN  TMPSTR
          llHasProblem = .T.
          REPLACE lIgnore WITH .T. IN (lcXmlData)
        ELSE
          *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
          *lnAdjDefCost = lfGetRtlFldVlu("NCNCDEFCS", "CDIVISION", lcDivVal)
          *lnAdjCost    = (&lcXmlData..Price -(&lcXmlData..Price*lnAdjDefCost/100))
          lnAdjCost    = lfGetAdjCost(m.STYLE, lcSelDefaultWare)
          *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
          *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
          *=lfCreateAutAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcXmlData..SIZE), lnAdjCost)
          =lfCreateAutAdj(lcSelDefaultWare, lcSelDefBin, m.STYLE, VAL(&lcXmlData..SIZE), lnAdjCost,lnQtyOld+&lcXmlData..Qty - WHBINLOC_A.Qty&lcQtyPos.)
          *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
          *- Fill the Auto Adjustment temp file that will be used in printing the adjustment journal report
          *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
          lnQtyOld = lnQtyOld + &lcXmlData..Qty
          *!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
        ENDIF 
        *!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
      ELSE
        lnQtyOld = lnQtyOld + &lcXmlData..Qty
      ENDIF
    ENDIF
  ENDSCAN
ENDDO

IF !llHasProblem AND !llFileHasProb
  REPLACE mStrRep WITH mStrRep +"Has no errors" + CHR(13) +CHR(10) IN  TMPSTR
ENDIF

IF !llFileHasProb
  llFileHasProb = llHasProblem
ENDIF
SELECT (lcXmlData)
SET ORDER TO (lcXmlData)
*:C201384,4 MMT 01/24/2012 Fix the Problem of not checking style Qty for all XML files[END]

*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][Start]
*:**************************************************************************
*:* Name        : lfUpBinWh
*:* Developer   : Mariam Mazhar[MMT]
*:* Date        : 01/29/2012
*:* Purpose     : Update WHBinloc
*:***************************************************************************
FUNCTION lfUpBinWh
LPARAMETERS lcStyle,lcLocation,lcBin
IF !USED('WHBINLOC_Bn')
  =gfOpenTable('WHBINLOC','WHBINLOC','SH','WHBINLOC_Bn')
ENDIF
IF !USED('WHSLOC_Bn')
  =gfOpenTable('WHSLOC','WHSLOC','SH','WHSLOC_Bn')
ENDIF
SELECT WHBINLOC_Bn
lcReplace = ''
IF !gfSEEK(lcLocation +lcBin+lcStyle,'WHBINLOC_Bn')
  SELECT WHSLOC_Bn
  =gfSEEK(lcLocation  +lcBin +SPACE(19),'WHSLOC_Bn')
  SELECT WHBINLOC_Bn
  =gfAppend()
  lcReplace = lcReplace + ;
    'STYLE      WITH ['+lcStyle  +'] '+;
    'CWARECODE  WITH ['+lcLocation  +'] '+;
    'clocation  WITH ['+lcBin+'] '

  lcReplace = lcReplace +;
    'cBlkPck   WITH ['+WHSLOC_Bn.cBlkPck   +'] '+;
    'cSection  WITH ['+WHSLOC_Bn.cSection  +'] '+;
    'cBinClass WITH ['+WHSLOC_Bn.cBinClass +'] '

ENDIF
LOCAL lcKey,lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8
STORE 0 TO lnAdj1,lnAdj2,lnAdj3,lnAdj4,lnAdj5,lnAdj6,lnAdj7,lnAdj8

lnAdj1 = laAdjust[1]
lnAdj2 = laAdjust[2]
lnAdj3 = laAdjust[3]
lnAdj4 = laAdjust[4]
lnAdj5 = laAdjust[5]
lnAdj6 = laAdjust[6]
lnAdj7 = laAdjust[7]
lnAdj8 = laAdjust[8]

SELECT WHBINLOC_Bn
lcReplace = lcReplace + ;
  'Qty1       WITH '+ STR( Qty1 + lnAdj1 ) +' '+;
  'Qty2       WITH '+ STR( Qty2 + lnAdj2 ) +' '+;
  'Qty3       WITH '+ STR( Qty3 + lnAdj3 ) +' '+;
  'Qty4       WITH '+ STR( Qty4 + lnAdj4 ) +' '+;
  'Qty5       WITH '+ STR( Qty5 + lnAdj5 ) +' '+;
  'Qty6       WITH '+ STR( Qty6 + lnAdj6 ) +' '+;
  'Qty7       WITH '+ STR( Qty7 + lnAdj7 ) +' '+;
  'Qty8       WITH '+ STR( Qty8 + lnAdj8 )
=gfReplace(lcReplace)
lcReplace = 'TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 '
=gfReplace(lcReplace)

IF WHBINLOC_Bn.Qty1 = 0 .AND. WHBINLOC_Bn.Qty2 = 0 .AND. WHBINLOC_Bn.Qty3 = 0 .AND. WHBINLOC_Bn.Qty4 = 0 .AND. ;
    WHBINLOC_Bn.Qty5 = 0 .AND. WHBINLOC_Bn.Qty6 = 0 .AND. WHBINLOC_Bn.Qty7 = 0 .AND. WHBINLOC_Bn.Qty8 = 0 .AND. ;
    gfGetMemVar('M_DELZRBNR')
  =gfDelete()
ENDIF

SELECT WHBINLOC_Bn
=gfTableUpdate()

=gfCloseTable('WHBINLOC_Bn')
=gfCloseTable('WHSLOC_Bn')
*!C201384,6 MMT 01/29/2012 Fix bug of wrong cost of issue records of invoice lines in Styinvjl[T20110621.0057][END]


*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[Start]
*!*************************************************************
*! Name      : lfProcessMBR
*! Developer : Mariam Mazhar[MMT]
*! Date      : 03/25/2012
*! Purpose   : Read MBR Files
*!*************************************************************
FUNCTION lfProcessMBR
LPARAMETERS lcImpMBRFile
lcMemWdth = SET("Memowidth")
SET MEMOWIDTH TO 90
CREATE CURSOR (lcTempCursMemo)  (MFile M(10))
SELECT (lcTempCursMemo)
APPEND BLANK
REPLACE MFile WITH FILETOSTR(lcImpMBRFile)
lnMemoLines = MEMLINES(MFile)
lcNewBatch = ''
lnFLBatchCnt = 0
m.cFileName = lcImpMBRFile
IF lnMemoLines  > 0
  FOR lnCntMemo = 1 TO lnMemoLines
    lcFileString  = MLINE(MFile,lnCntMemo)

    IF SUBSTR(lcFileString,1,2) =  '01'
      lnFLBatchCnt = lnFLBatchCnt+1
      lcNewBatch = STR(lnFLBatchCnt,6)
    ENDIF

    IF SUBSTR(lcFileString,1,2) =  '04'
      lnFLBatchCnt = lnFLBatchCnt+1
      lcNewBatch = STR(lnFLBatchCnt,6)
    ENDIF

    IF SUBSTR(lcFileString,1,2) <> '03'
      LOOP
    ENDIF

    m.STORE = SUBSTR(lcFileString,7,4)
    m.UPC   = SUBSTR(lcFileString,19,13)
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    IF ALLTRIM(m.UPC) == ALLTRIM(lcDefStyUpc)
      m.Desc = 'Dept Default No'
    ELSE
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
      m.DESC ='Import MBR File'
      *!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
    ENDIF
    *!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]
    m.RETCLSS = IIF(SUBSTR(lcFileString,88,1) = '1' ,'','RETURNS')
    m.RETREAS = ''
    IF SUBSTR(lcFileString,88,1) = '2'
      IF !USED('CODES')
        =gfOpenTable('CODES','CCODE_NO')
      ENDIF
      =gfSEEK('D'+PADR('REASON',10),'CODES','CCODE_NO')
      m.RETRESN = codes.cdiscrep
    ELSE
      m.RETRESN = ''
    ENDIF
    m.QTY = VAL(SUBSTR(lcFileString,59,2))
    m.PRICE = VAL(SUBSTR(lcFileString,32,7))/100
    INSERT INTO (lcXmlData) (STORE ,UPC,DESC,RETCLSS,RETREAS ,RETRESN ,QTY ,PRICE,cFileName,TRANS) VALUES (m.STORE ,m.UPC,m.DESC,m.RETCLSS,m.RETREAS ,m.RETRESN ,m.QTY ,m.PRICE,m.cFileName,lcNewBatch)
  ENDFOR
ENDIF
SET MEMOWIDTH TO lcMemWdth
*C201468,1 MMT Custom Concession program for GPS (Top Shop case)[END]

*!C201384,7 MMT 04/22/2012 Enhance program for ticket[Start]
*!*************************************************************
*! Name      : lfvAssAcc
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/22/2012
*! Purpose   : Validate Assign to Account
*!*************************************************************
FUNCTION lfvAssAcc
PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpAsAc .OR. (!EMPTY(lcRpAsAc) .AND. !gfSEEK('M' +lcRpAsAc, 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAsAc, '' , 'M')
  lcRpAsAc= IIF(llObjRet , lcRpAsAc, '')
ENDIF

SELECT CUSTOMER
gfSETORDER(lcCustOrd)
SELECT(lnAlsNo)
*!*************************************************************
*! Name      : lfGetAcc
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/22/2012
*! Purpose   : Get Account
*!*************************************************************
FUNCTION lfGetAcc
LPARAMETERS lcStoreVal
LOCAL lnSelectedAlias   ,llRetValue
lnSelectedAlias = SELECT()

SELECT (lcAccTmp)
LOCATE
lcRpAcct = ''
llRetValue = .F.
SCAN
  IF gfSeek('M'+&lcAccTmp..Account,'Customer','Customer') AND !Customer.STATUS $ 'XH'  AND gfSeek('S'+&lcAccTmp..Account+lcStoreVal,'Customer','Customer')
    lcRpAcct = &lcAccTmp..Account
    llRetValue = .T.
    EXIT
  ENDIF
ENDSCAN

*!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][Start]
llConcSetFound = .T.
lnCncSetSel = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
      " WHERE [Account]='"+lcRpAcct+"' AND [Store]='"+lcStoreVal+"'",'',;
      "CNSSET","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
IF lnCncSetSel > 0 
  SELECT "CNSSET"
  LOCATE 
  IF EOF()
    llConcSetFound = .F.
  ENDIF
ENDIF
*!B609963,4 MMT 09/12/2012 Fix Issue#7 in Concession Project[T20120130.0001][END]

IF !llRetValue AND LLRPADST AND !EMPTY(lcRpAsAc)
  IF !gfSeek('S'+lcRpAsAc+lcStoreVal,'Customer','Customer')
    =gfSeek('M'+lcRpAsAc,'Customer','Customer')
    SELECT Customer
    SCATTER MEMO MEMVAR
    m.Store = lcStoreVal
    m.TYPE = 'S'
    APPEND BLANK
    GATHER MEMO MEMVAR
    =gfReplace('')
    =gfTableUpdate()
    llRetValue = .T.
    lcRpAcct = lcRpAsAc
    *!E303203,1 HIA 07/24/2012 Issue# 3, add store to warhous and concession setup table [T20120130.0001][Begin]
    *! Check in concession, [if not found check in warhous, if not add to warhous and whloc], if found not do anything.
    IF !USED('WAREHOUS')
      =gfOpenTAble('WAREHOUS','WAREHOUS')
    ENDIF
    IF !USED('WHSLOC')
      =gfOpenTable('WHSLOC','WHSLOC')
    ENDIF
    
    *! Check in concession
    lnCncSetSel = oAriaApplication.RemoteCompanyData.execute("Select Concession_Warehouse,Concession_Default_Bin,Concession_Return_Bin,Concession_WriteOff_Bin from CONCESSION_SETUP_T"+;
      " WHERE [Account]='"+lcRpAsAc+"' AND [Store]='"+lcStoreVal+"'",'',;
      "CNSSET","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
    IF lnCncSetSel > 0 AND EOF("CNSSET") && Store Found in Concession, which mean it already has record in Warhous table.
                            && ELSE && Store Not Found in Concession
      SELECT WAREHOUS
      IF !gfSEEK(ALLTRIM(SUBSTR(lcStoreVal,1,6))) && found in warehous table, so we need to add in concession table
                                      && Not found in warehous table, so we need to add it to Warehous and whloc then in concession table
        SELECT WAREHOUS
        APPEND BLANK
        REPLACE cwarecode WITH ALLTRIM(lcStoreVal)
        REPLACE cdesc     WITH ALLTRIM(lcStoreVal)
        REPLACE lstyinv   WITH .T.
        =gfAdd_Info('WAREHOUS')
        =gfreplace('')
        =gftableupdate()
      ENDIF
      
      SELECT WHSLOC      &&SET ORDER TO WHSLOC   && CWARECODE+CLOCATION+STYLE+COLOR
      IF !gfSEEK(WAREHOUS.cwarecode+"STORE")
        APPEND BLANK
        REPLACE whsloc.cwarecode WITH ALLTRIM(lcStoreVal)
        REPLACE whsloc.clocation WITH "STORE"
        =gfreplace('')
        =gftableupdate()
      ENDIF
      
      IF !gfSEEK(WAREHOUS.cwarecode+"FAULTY")
        APPEND BLANK
        REPLACE whsloc.cwarecode WITH ALLTRIM(lcStoreVal)
        REPLACE whsloc.clocation WITH "FAULTY"
        =gfreplace('')
        =gftableupdate()
      ENDIF      

      &&so we need to add in concession table
      SELECT codes
      =gfSeek('DCCNCSSNGRD','CODES')
      LOCAL lnConnHandler ,lnUpSt 
      lnConnHandler = 0
      lnUpSt = 0

      lcInsertStatment = "Insert into CONCESSION_SETUP_T ([Account],[Store],[Concession_Grade],[Concession_Warehouse],[Concession_Default_Bin],[Concession_Return_Bin],"+;
        " [Concession_WriteOff_Bin],[Concession_Optimum_Stock] ) values ('"+lcRpAsAc+"','"+lcStoreVal+"','"+ALLTRIM(CODES.CCODE_NO)+"','"+ALLTRIM(WAREHOUS.cwarecode)+"','STORE','FAULTY','FAULTY',1)"

      lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcInsertStatment ,'',"CONCESSION_SETUP_T","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"),.T.,@lnConnHandler)

      IF lnUpSt > 0
        SQLCOMMIT(lnConnHandler)
      ELSE
        =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)
        SQLROLLBACK(lnConnHandler)
      ENDIF
    ENDIF
    *!E303203,1 HIA 07/24/2012 Issue# 3, add store to warhous and concession setup table [T20120130.0001][End]
  ELSE
    llRetValue = .T.
    lcRpAcct = lcRpAsAc
  ENDIF
ENDIF
SELECT(lnSelectedAlias)
RETURN (lcRpAcct)
*!C201384,7 MMT 04/22/2012 Enhance program for ticket[END]


*!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[Start]
*!*************************************************************
*! Name      : lfIsDefaultUPC
*! Developer : Mariam Mazhar[MMT]
*! Date      : 06/18/2012
*! Purpose   : Check if the UPC is the default style UPC
*!*************************************************************
FUNCTION lfIsDefaultUPC
LPARAMETERS lcUPCValue
lcCurAlias = SELECT()
LOCAL lcSizeValue
lcSizeValue = ''

IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYUPCN')
ENDIF
*!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
*IF gfSeek(lcDefStyle,'STYLEUPC','STYLEUPC')
*!*	  SELECT STYLEUPC
*!*	  lcOrderUPC = ORDER()
*!*	  =gfSetOrder('STYLEUPC')
SELECT STYLEUPC
lcOrderUPC = ORDER()
=gfSetOrder('STYLEUPC')
IF gfSeek(lcDefStyle)
*!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
  LOCATE REST WHILE STYLE+SIZE = lcDefStyle FOR ALLTRIM(CUPCNUM1+CUPCNUM2+CUPCNUM3) = ALLTRIM(lcUPCValue)
  IF FOUND()
    lcSizeValue  = STYLEUPC.SIZE
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
  ELSE
     =gfSeek(ALLTRIM(lcUPCValue),'STYLEUPC','STYUPCN')
  *!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
  ENDIF
  *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
  *=gfSetOrder(lcOrderUPC)
  *!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
  *MT
ELSE
  =gfSeek(ALLTRIM(lcUPCValue),'STYLEUPC','STYUPCN')
  *MT  
ENDIF
*!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][Start]
SELECT STYLEUPC
=gfSetOrder(lcOrderUPC)
*!B609963,2 MMT 08/05/2012 Fix Issue#4,5 in Concession Project[T20120130.0001][End]
SELECT(lcCurAlias)
RETURN lcSizeValue
*!B609963,1 MMT 06/18/2012 Fix bug of wrong default style sizes in concession program[End]


*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [Start]
*!*************************************************************
*! Name      : lfGetRtlFldVlu
*! Developer : Saber A Razek [SAB]
*! Date      : 12/11/2012
*! Purpose   : Get Code Related field value
*!*************************************************************
FUNCTION lfGetRtlFldVlu
LPARAMETERS lcRFName,lcCodeName,lcCodeVal

PRIVATE lcRFValue,llRFExist
lcRFValue = ""
DIMENSION laTermAry[1,2]
laTermAry[1,1] = lcRFName
laTermAry[1,2] = 'lcRFValue'
llRFExist = gfRltFld(lcCodeVal,@laTermAry,lcCodeName)

IF llRFExist
  RETURN lcRFValue
ELSE
  RETURN IIF(TYPE(lcRFValue)='N',0,IIF(TYPE(lcRFValue)='L',.F.,""))
ENDIF

ENDFUNC
*- End of lfGetRtlFldVlu
*!C201539,1 SAB 12/11/2012 Add Concession setups to DIVISION related fields [End]


*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][Start]
*!*************************************************************
*! Name      : lfGetDfltCode
*! Developer : Saber A Razek [SAB]
*! Date      : 04/24/2013
*! Purpose   : Get Default Code value
*!*************************************************************
FUNCTION lfGetDfltCode
LPARAMETERS lcCodeName

LOCAL lnAlias, lcDefCode, lcOrder 
lnAlias   = SELECT()
lcDefCode = ''

IF !USED('Codes')
  =gfOpenTable('Codes','CCODE_NO','SH')      && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
ENDIF

SELECT Codes
lcOrder = SET("Order")
SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
IF gfSeek('D'+PADR(lcCodeName, 10))
  lcDefCode = Codes.CCODE_NO
ENDIF

SET ORDER TO &lcOrder.
SELECT(lnAlias)

RETURN lcDefCode
*-End Of lfGetDfltCode



*!*************************************************************
*! Name      : lfCreateAutAdj
*! Developer : Saber A Razek [SAB]
*! Date      : 04/24/2013
*! Purpose   : Create Auto Inventory Adjustment
*!*************************************************************
FUNCTION lfCreateAutAdj
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
*LPARAMETERS lcAdjWare, lcAdjBin, lcAdjStyle, lnStySz, lnAdjCost
LPARAMETERS lcAdjWare, lcAdjBin, lcAdjStyle, lnStySz, lnAdjCost,lnQtyToAdj
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]

DIMENSION laAdjust[9]
laAdjust          = 0
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
*!*	laAdjust[lnStySz] = 1
*!*	laAdjust[9]       = 1
laAdjust[lnStySz] = lnQtyToAdj
laAdjust[9]       = lnQtyToAdj
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]
lcLinkCode = IIF(llUse_GL , IIF(!EMPTY(STYLE.Link_Code), STYLE.Link_Code, 'DEFDEF'), "")

lcAdjReason = IIF(gfSeek("DCADJREASON",'Codes','CCODE_NO'),codes.ccode_no,'')
lcAdjAcct = ' '
IF llUse_GL AND !EMPTY(lcAdjReason)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

IF llUse_GL
  =gfOpenFile(oAriaApplication.DataDir+'GLDist','GLDistAc','SH')
  SELECT GLDist
  lcTmpGlDt = IIF(TYPE('lcXMLFileName') <> 'C',loogScroll.gfTempName(),gfTempName())
  lcWorkDir = oAriaApplication.workdir
  COPY STRU TO (lcWorkDir+lcTmpGlDt+'.DBF')
  USE (lcWorkDir+lcTmpGlDt+'.DBF') IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF

IF llUse_GL
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IA' TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  *MT02/26 
  *STORE oAriaApplication.SystemDate TO laGLDistAr[1,6],laGLDistAr[2,6]  
  STORE ldRpInvDat TO laGLDistAr[1,6],laGLDistAr[2,6]
  *MT
  STORE lcGlFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][Start]
IF !USED('STYDYE_AB')
  =gfOpenTable('STYDYE','STYDYE','SH','STYDYE_AB')
ENDIF
IF  !gfSEEK(lcAdjStyle+lcAdjWare+SPACE(10),'STYDYE_AB')
  DO gpAdStyWar WITH lcAdjStyle, SPACE(10), lcAdjWare
ENDIF
*!B610545,1 MMT 10/10/2013 Custom Concession invoicing program does update whbinloc[T20130108.0010][End]

*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*lnRet=gfStyCrl('1', lcAdjStyle, lcAdjWare, SPACE(10), oAriaApplication.SystemDate, '', @laAdjust, lnAdjCost,;
               '', .T., 'INV_'+lcAdjWare, 0, '', '', @laGLDistAr, 0, '', '')
lnRet=gfStyCrl('1', lcAdjStyle, lcAdjWare, SPACE(10), ldRpInvDat, '', @laAdjust, lnAdjCost,;
               'INV_'+lcAdjWare, .T., 'INV_'+lcAdjWare, 0, '', '', @laGLDistAr, 0, '', '')
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]          

*MMT
IF llUse_GL 
  lcWorkDir = oAriaApplication.workdir
	SELECT (lcTmpGlDt)
	*-- Generate a unique session number.
	lcGlSess = gfSequence('GLSESSION')
	REPLACE ALL GLSESSION WITH lcGlSess
	USE
	SELECT GLDIST
	APPEND FROM (lcWorkDir+lcTmpGlDt+'.DBF')
	ERASE (lcWorkDir+lcTmpGlDt+'.DBF')
	SELECT STYLE
	=gfTableUpdate()
ENDIF
*MMT
          
lfUpdateBinLocFiles(lcAdjStyle, lcAdjWare, lcAdjBin)

*- Get GL Variables value
llRet = .T.
lcGlFYear  = ''
lcGlPeriod = ''
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',oAriaApplication.ActiveCompanyID)
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*llRet = CHECKPRD(ldRpInvDat,'lcGLFYear','lcGLPeriod','IN')
llRet = CHECKPRD(ldRpInvDat,'lcGLFYear','lcGLPeriod','IN',.T.)
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]

*- Fill Adjustment temp file
lcCurAlisSel = SELECT()
IF !USED('StyInvjl_Adj')
  =gfOpenTable('StyInvjl','STYDATE','SH','StyInvjl_Adj')
ENDIF

SELECT 'StyInvjl_Adj'
*!B610382,3 SAB 09/26/2013 Fix concession issu# 2 Negative Adjustment Problem [T20120108.0010][Issue2] [Start]
*SET ORDER TO 'STYDATE' DESCENDING
*!B611100,1 MMT 01/12/2016 Concession invoicing program prints wrong data in its related report[T20160104.0051][Start]
*SET ORDER TO STYINVJL  DESCENDING 
SET ORDER TO STYDATE DESCENDING 
*!B611100,1 MMT 01/12/2016 Concession invoicing program prints wrong data in its related report[T20160104.0051][End]
*!B610382,3 SAB 09/26/2013 Fix concession issu# 2 Negative Adjustment Problem [T20120108.0010][Issue2] [End]
=gfSeek(lcAdjStyle+lcAdjWare)
SCATTER MEMO MEMVAR
SELECT (lcScreenTemp)
APPEND BLANK
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*m.cReason   = StyInvjl_Adj.cAdjReason
m.cReason   = 'INV_'+lcAdjWare
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]
m.Date      = StyInvjl_Adj.dTrDate
m.dPostDate = StyInvjl_Adj.dTrDate
m.Type      = 'P'
m.Unt_Cost  = StyInvjl_Adj.nCost
m.Old_Cost  = StyInvjl_Adj.nCost
m.GLFYear   = lcGlFYear
m.GLPeriod  = lcGlPeriod
m.cFromWare = StyInvjl_Adj.cWareCode

FOR lnNum = 1 TO 8
  lcNum = ALLTRIM(STR(lnNum))
  m.Adj&lcNum.    = StyInvjl_Adj.nStk&lcNum.
  m.OldQty&lcNum. = StyInvjl_Adj.nStk&lcNum.
  m.nOldTo&lcNum. = StyInvjl_Adj.nStk&lcNum.
ENDFOR

m.TotAdj    = StyInvjl_Adj.nTotStk
m.TotOld    = StyInvjl_Adj.nTotStk
m.nTotOldTo = StyInvjl_Adj.nTotStk
GATHER MEMO MEMVAR

ENDFUNC 
*-End Of lfCreateAutAdj


*!*************************************************************
*! Name      : lfGetAutoAdjReason
*! Developer : Saber A Razek [SAB]
*! Date      : 04/24/2013
*! Purpose   : Create Adjustment Reason for Auto Adjustment 
*!*************************************************************
FUNCTION lfGetAutoAdjReason
LPARAMETERS lcStyle

LOCAL lnAlias, lcAdjResCode
lnAlias = SELECT()

IF !USED('STYLE_Adj')
  =gfOpenTable('STYLE', 'STYLE', 'STYLE_Adj')   && STYLE
ENDIF

IF !USED('CODES_Adj')
  =gfOpenTable('CODES', 'CCODE_NO', 'CODES_Adj')   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
ENDIF

SELECT STYLE_Adj
IF gfSeek(lcStyle)
  SELECT CODES_Adj
  LOCATE FOR cDefCode = 'N' AND cFld_Name = 'CADJREASON' AND cRltField = 'Y' AND cRltd_Nam = 'CDIVISION' AND cRltd_Vlu = STYLE_Adj.cDivision
  IF FOUND()
    lcAdjResCode = CODES_Adj.cCode_No
  ELSE
    lcAdjResCode = lcRpDefAdj
  ENDIF
ELSE
  lcAdjResCode = lcRpDefAdj
ENDIF


=gfCloseTable('STYLE_Adj')
=gfCloseTable('CODES_Adj')
SELECT (lnAlias)

RETURN lcAdjResCode

ENDFUNC 
*-End Of lfGetAutoAdjReason


*!*************************************************************
*! Name      : lfCrtAdjTmp
*! Developer : Saber A Razek [SAB]
*! Date      : 04/24/2013
*! Purpose   : Create Inventory Adjustment temp file
*!*************************************************************
FUNCTION lfCrtAdjTmp

=gfOpenTable('INVTADJ','INVTADJ')   && STYLE

*-- Create tmp adjustment file.
SELECT INVTADJ
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru, 'UNT_COST')
lnNo2=ASCAN(laFStru, 'OLD_COST')

*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)

DIMENSION laFStru[lnFStru+3, 18]

laFStru[lnFStru+1, 1] = 'cAdjReason'
laFStru[lnFStru+1, 2] = 'C'
laFStru[lnFStru+1, 3] = 6
laFStru[lnFStru+1, 4] = 0

laFStru[lnFStru+2, 1] = 'cRefer'
laFStru[lnFStru+2, 2] = 'C'
laFStru[lnFStru+2, 3] = 6
laFStru[lnFStru+2, 4] = 0

laFStru[lnFStru+3, 1] = 'cIssSess'
laFStru[lnFStru+3, 2] = 'C'
laFStru[lnFStru+3, 3] = 6
laFStru[lnFStru+3, 4] = 0

STORE .F. TO laFStru[lnFStru+1, 5],laFStru[lnFStru+1, 6],;
             laFStru[lnFStru+2, 5],laFStru[lnFStru+2, 6],;
             laFStru[lnFStru+3, 5],laFStru[lnFStru+3, 6]
STORE ""  TO laFStru[lnFStru+1, 7],laFStru[lnFStru+1, 8],laFStru[lnFStru+1, 9],laFStru[lnFStru+1,10],laFStru[lnFStru+1,11],;
             laFStru[lnFStru+2, 7],laFStru[lnFStru+2, 8],laFStru[lnFStru+2, 9],laFStru[lnFStru+2,10],laFStru[lnFStru+2,11],;
             laFStru[lnFStru+3, 7],laFStru[lnFStru+3, 8],laFStru[lnFStru+3, 9],laFStru[lnFStru+3,10],laFStru[lnFStru+3,11]
STORE ""  TO laFStru[lnFStru+1,12],laFStru[lnFStru+1,13],laFStru[lnFStru+1,14],laFStru[lnFStru+1,15],laFStru[lnFStru+1,16],;
             laFStru[lnFStru+2,12],laFStru[lnFStru+2,13],laFStru[lnFStru+2,14],laFStru[lnFStru+2,15],laFStru[lnFStru+2,16],;
             laFStru[lnFStru+3,12],laFStru[lnFStru+3,13],laFStru[lnFStru+3,14],laFStru[lnFStru+3,15],laFStru[lnFStru+3,16]
STORE 0   TO laFStru[lnFStru+1,17],laFStru[lnFStru+1,18],;
             laFStru[lnFStru+2,17],laFStru[lnFStru+2,18],;
             laFStru[lnFStru+3,17],laFStru[lnFStru+3,18]

CREATE DBF (oAriaApplication.WorkDir+lcScreenTemp) FROM ARRAY laFStru
USE
SELECT 0

USE (oAriaApplication.WorkDir+lcScreenTemp) EXCLUSIVE ALIAS (lcScreenTemp)

ENDFUNC 
*-End Of lfCrtAdjTmp


*!*************************************************************
*! Name      : lfPrintInvAdj
*! Developer : Saber A Razek [SAB]
*! Date      : 04/24/2013
*! Purpose   : Print Inventory Adjustment Report
*!*************************************************************
FUNCTION lfPrintInvAdj


*- Declare option grid arrays
DECLARE gaRepVars[1, 1]
DECLARE gaOGFxFlt[1, 1]
DECLARE gaOGVrFlt[1, 1]

*-- Preparing info to be used in any modal form branched from this form
*-- Run the report.
oAriaApplication.ObjectType = "Data"
oAriaApplication.ActiveKeyValue = ""
oAriaApplication.ActiveDataSessionID = SET("Datasession")
oAriaApplication.lcBaseFile = lcScreenTemp &&This.getmasterfile ()
oAriaApplication.lcKeyValue = ""
TRY 
  oAriaApplication.ActiveKeyValue = ALLTRIM(THIS.AriaForm1.Caption) + " # " + EVALUATE(This.key)
  oAriaApplication.lcKeyValue = EVALUATE(This.Key)
  ACOPY(This.laindexfields ,oAriaApplication.laIndexFields)
CATCH
ENDTRY 

USE IN (lcScreenTemp)
*-- Run the report.
=gfOpGrid('ICSTKJL',.F.,'P')

ENDFUNC 
*-End Of lfPrintInvAdj

*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][Start]
*!*************************************************************
*! Name      : lfGetAdjCost
*! Developer : Saber A Razek [SAB]
*! Date      : 07/22/2013
*! Purpose   : Get Style cost for the auto adjustment
*!*************************************************************
FUNCTION lfGetAdjCost
LPARAMETERS lcStyle, lcWareCode

LOCAL lnAdjCost, lnRecCnt, lcAlias
lnAdjCost = 0
lnRecCnt  = 0
lcAlias = SELECT()

IF !USED('STYDYE_CST')
  *=gfOpenTable('STYDYE','STYDYE','SH','STYDYE_CST')  && STYLE+CWARECODE+DYELOT
  USE (oAriaApplication.DataDir+'STYDYE') IN 0 SHARED AGAIN ALIAS STYDYE_CST ORDER STYDYE  && STYLE+CWARECODE+DYELOT
ENDIF

*IF gfSeek(lcStyle+lcWareCode,'STYDYE_CST')
IF SEEK(lcStyle+lcWareCode, 'STYDYE_CST')
  SELECT STYDYE_CST
  SCAN REST WHILE STYLE+CWARECODE+DYELOT = lcStyle+lcWareCode
    lnAdjCost = lnAdjCost + STYDYE_CST.Ave_Cost
    lnRecCnt  = lnRecCnt + 1
  ENDSCAN
ENDIF

lnAdjCost = lnAdjCost / lnRecCnt

*!B610637,1 MMT 12/26/2013 Concession invoicing program calculates wrong style ave. cost[T20131212.0012][Start]
IF !SEEK(lcStyle+lcWareCode, 'STYDYE_CST')
  IF !USED('STYLE_CST')
    =gfOpenTable('STYLE','STYLE','SH','STYLE_CST')
  ENDIF  
  IF gfSeek(lcStyle,'STYLE_CST')
    lnAdjCost = STYLE_CST.Ave_Cost
  ENDIF
ENDIF
*!B610637,1 MMT 12/26/2013 Concession invoicing program calculates wrong style ave. cost[T20131212.0012][End]

SELECT (lcAlias)

RETURN lnAdjCost
*-End Of lfGetAdjCost
*!B610382,2 SAB 06/26/2013 Fix concession issu# 2 [T20120108.0010][Issue2][End]

*!C201579,1 SAB 05/19/2013 Add Auto Adjust and Use Default Style Options to Concession [T20120108.0010][T20120130.0012][End]
*!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][Start]
*!*************************************************************
*! Name      : lfCalcLineTax
*! Developer : Mariam Mazhar [MMT]
*! Date      : 02/16/2016
*! Purpose   : Get Style tax amount
*!*************************************************************
FUNCTION lfCalcLineTax
PARAMETER lcStyle , lnPrice , lcFile,lcCust,lnPTaxRate,lnDiscPer

PRIVATE lcStyle , lnPrice , lcFile , lnCurTax , lnQuantity
STORE 0 TO lnCurTax , lnQuantity

IF !USED('Style_Tax')
  =gfOpenTable('STYLE','STYLE','SH','Style_Tax')
ENDIF
IF !USED('Customer_Tax')
  =gfOpenTable('Customer','Customer','SH','Customer_Tax')
ENDIF
=gfSeek('M'+lcCust,'Customer_Tax','Customer')
llVatExem  = Customer_Tax.lVatExem

IF gfSEEK(lcStyle,'Style_Tax','Style') .AND.  !llVatExem .AND. Style_Tax.nTaxBreak <> 0
  IF lnPTaxRate <> 0
    FOR lnCount = Style_Tax.nTaxBreak TO 8
      lcCount    = STR(lnCount,1)
      lnQuantity = lnQuantity + &lcFile..Qty&lcCount
    ENDFOR
    *!* B610175,1 HIA 12/16/2012 Fix error in discount calculation [T20121119.0002][Begin]
    *lnCurTax = lnQuantity * lnPrice * (lnTaxLine / 100) * ((100 - This.lnInvTrdDs)/100) * ((100 - This.lnDiscLNPcnt)/100)
    *E303566,1 MMT 03/31/2015 Remove trade discount from VAT calculations in UK companies[T20150319.0010][Start] 
    *lnCurTax = lnQuantity * (lnTaxLine / 100) * ((100 - This.lnInvTrdDs)/100) * ROUND((lnPrice *((100 - This.lnDiscLNPcnt)/100)),2)    
    lnCurTax = lnQuantity * (lnPTaxRate / 100) *  ROUND((lnPrice *((100 - lnDiscPer)/100)),2)
    *E303566,1 MMT 03/31/2015 Remove trade discount from VAT calculations in UK companies[T20150319.0010][End] 
    *!* B610175,1 HIA 12/16/2012 Fix error in discount calculation [T20121119.0002][End]
  ENDIF
ENDIF
RETURN lnCurTax
*!B611115,1 MMT 02/16/2016 Issue#1 - Incorrect Tax amount in credit memo created by concession [P20160126.0001-Issue#1][End]