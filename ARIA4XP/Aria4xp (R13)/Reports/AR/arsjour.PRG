*:***************************************************************************
*: Program file  : ARSJOUR
*: Program desc. : Sales Journal Report
*: Module        : Accounts receivable (AR)
*: Developer     : Heba Mohamed Amin  (HMA)
*: Tracking Job Number: 037248
*: Date : 11/12/2003
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfCodDes,gfGetMemVar,gfOptMsg,gfBrows,gfCrtTmp.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : 1- All IF llFrTime Blocks executed one time in the option grid seasson.
*:         :    and if user selection opens any temp. files that may be used later
*:         :    in another selection I take this file open to use untill user choice
*:         :    is to press < Close > button, to save more time.
*:         : 2- Any variable start by (llCh) means that some thing in
*:         :    selected critria was changed, you must collect data again.
*:***************************************************************************
*: Example : DO ARSJOUR
*:***************************************************************************
*:Modifications : ....
*E037378,1 HBG 24/12/2003 Add new field to SYREPUVR to differentiate between records belong to
*E037378,1                Aria27 and records belong to Aria4. {Begin]
*E038429,1 WSH 08/23/2004 Open SYREPUVR from Aria4 System Files not from Aria27 System files folder
*E999999,1 HMA 10/02/2005 Change Season,Division,Location,Payment Terms Filters from LIKE to INLIST
*B127496,1 MMT,04/19/2005,fix bug of not printing void invoice[Start]
*B607904,1 MMT 12/25/2006 Fix bug of error when Select All Accounts ,T20061211.0014
*B608247,1 WAM 08/30/2007 Do not export data to Excel file
*B609182,1 MMT 03/21/2010 Fix error while preview Sales journal report Sorted by State {T20100304.0016}
*B609339,1 MMT 07/11/2010 Sales Journal Report Collect data incorrectly IF State selected[T20100630.0033]
*E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[T20140508.0016]
*B611076,1 MMT 11/08/2015 Sales Journal report exports invoices amount incorrectly to Excel[T20151106.0001]
*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [T20180626.0018]
*B611886,1 ES 08/22/2019 Sales Journal report display value in vtotalchg when export field for Voided invoice has invdate and vdate falls in selected date range while same invoice show zeros in preview [T20190802.0002]
*B611908,1 ES 10/13/2019  Sales Journal report does not check if the Currency and Accout cursor contains data before using it[T20190919.0007]
*E612018,1 MMT 02/18/2020 Add new option to display new report collects data from new GL tables [GL Enhacement]
*B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003]
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder
*E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004]
*E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003]
*:**************************************************************************************************
*----------------------- Report Code Begin -----------------------------
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientId

IF TYPE('lcRequestID') = 'C' .AND. 'TEMP.TXT' $ UPPER(lcRequestID)  
  STRTOFILE("3.0.0.0", lcRequestID, .F.)
  RETURN
ENDIF
SET STEP ON 
IF TYPE('lcXMLFileName') = 'C'

  PRIVATE loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  LOCAL loEnvironment
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
 
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
  lcActiveMod = 'AR'
  oAriaEnvironment.REPORT.gcAct_Appl = lcActiveMod
  oAriaEnvironment.activeModuleID = 'AR'
  oAriaEnvironment.RequestID = lcRequestID
  PUBLIC gcAct_Appl
  gcAct_Appl = lcActiveMod
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  gcBaseCurr = oAriaEnvironment.BaseCurrency 
  oAriaApplication.SysPath = oAriaEnvironment.systemfilespath 
  oAriaEnvironment.Report.cCROrientation = 'P'
    llTaxes =gfGetMemVar('M_TAX')='Y'  
  =gfOpenFile('Invhdr','Invhdr')
  =gfOpenFile('CUSTOMER','CUSTOMER')
  =gfOpenFile('WAREHOUS','WAREHOUS')
  *=gfOpenFile('DEBIT','DEBIT')
  =gfOpenFile('SalesRep','SalesRep')
  *=gfOpenFile('RETHDR','RETHDR')
  *IF llMultCurr
    IF !USED('SYCCURR')
      =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
    ENDIF
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
 * ENDIF
    IF !USED('FSPRD')
	  =gfOpenTable('FSPRD','COMFYRPRDI','SH')   && CFISFYEAR+CFSPPRDID)
	ENDIF
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
#INCLUDE R:\Aria4xp\reports\ar\arsjour.H

*E612018,1 MMT 02/18/2020 Add new option to display new report collects data from new GL tables [GL Enhacement][Start]
IF llPrintMod
  lfReconWithGL()
  RETURN 
ENDIF
*E612018,1 MMT 02/18/2020 Add new option to display new report collects data from new GL tables [GL Enhacement][End]

lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON

*-- Use variable llOGFltCh that detect OG filter changes.[Begin]
IF llClearFn OR loOGScroll.llOGFltCh
  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = lcRpExp
ENDIF
*-- Use variable llOGFltCh that detect OG filter changes.[End]
*-- Show messages in status bar when collecting data. [begin]

*Include void invoices amount and ship amount if upper invoice date is less than void invoice date. [Begin]

lnInvPos = lfItmPos('INVHDR.INVDATE')
STORE {  /  /  } TO ldStrtDate , ldEndDate
*Fill the date variables.[START]

IF lcRpDateTP = "P"
  STORE {  /  /  } TO ldStrtDPst , ldEndDPst
  lnPostDate = lfItmPos('DPOSTDATE')
  *Variable Hold true If you print Void invoices only. [Begin]
  IF EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
  *Variable Hold true If you print Void invoices only.[End]
    ldStrtDPst = CTOD(PADR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])-1))
    ldEndDPst  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])+1))
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst)) OR (!BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))))]
  ENDIF
ENDIF
*Fill the date variables.[END]
*Don't get to this code in case posted date.[START]
IF lcRpDateTP # "P"
  IF EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6])
    lcVoidExpr = [llVoidOnly]
  ELSE
    ldStrtDate = CTOD(PADR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])-1))
    ldEndDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnInvPos,6],ATC('|',loOGScroll.laOGFxFlt[lnInvPos,6])+1))
    * Add Void between to Range.
    lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(INVDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(INVDATE,ldStrtDate,ldEndDate))))]
  ENDIF
ENDIF
*Don't get to this code in case posted date.[END]
*Include void invoices amount and ship amount if upper invoice date is less than [End]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.

  * Display HST Tax amount for Canada. [Begin]
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))+['+']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+;
              ['+' : ' + cCurrCode + " , ",'')] +;
              [+ IIF(llRpRepPrn,']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
              ['+' ' + Rep1 +  " " +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+['+" =  "  +;
              TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_PstAmount",AHEADERFILE))+[' +]+;
              [TRANSFORM(lnPstAmt,"99999999.99")+;
              SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_HstAmount",AHEADERFILE))+[' + ;
              TRANSFORM(lnHstAmt,"99999999.99")," ")]
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
               [' +TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+;
               [' +TRANSFORM(lnPstAmtCr,"99999999.99")+SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
               TRANSFORM(lnHstAmtCr,"99999999.99")," ")]
  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) +']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
             ['+' '+']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
             ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
             ['  +TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
             [+IIF(llCanada, SPACE(10) +']+ ;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+[' +;
             TRANSFORM(lnPstAmtGp,"99999999.99")+SPACE(10) + ']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
             TRANSFORM(lnHstAmtGp,"99999999.99")," ")]
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+iIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Grand,oAriaApplication.GetHeaderText("LANG_Arsjour_Grand",AHEADERFILE))+[' +;
               TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_PstGrand",AHEADERFILE))+[' +;
               TRANSFORM(lnPstAmtot,"99999999.99")+;
               SPACE(10) +']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_HstGrand",AHEADERFILE))+[' + ;
                               TRANSFORM(lnHstAmtot,"99999999.99")," ")]
  * Display HST Tax amount for Canada. [End]

  *-- Create temporary file that holding order InvHdr data. [begin]

  lcWorkFile = gfTempName()
  DIMENSION laTempStru[1,18]
  laTempStru = ''

  SELECT INVHDR
  =AFIELD(laTempStru)
  
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[begin]
  *!*	  lnTempStru=ALEN(laTempStru,1) + 1
  lnTempStru=ALEN(laTempStru,1) + 8
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[end]
  
  DIMENSION laTempStru[lnTempStru,18]
  
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[begin]
  laTempStru[lnTempStru-7,1] = 'cstname'
  laTempStru[lnTempStru-7,2] = 'C'
  laTempStru[lnTempStru-7,3] = 30
  laTempStru[lnTempStru-7,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-7,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-7,17], laTempStru[lnTempStru-7,18]
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[end]
  *E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][Start]
  laTempStru[lnTempStru-6,1] = 'stname'
  laTempStru[lnTempStru-6,2] = 'C'
  laTempStru[lnTempStru-6,3] = 30
  laTempStru[lnTempStru-6,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-6,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-6,17], laTempStru[lnTempStru-6,18]
  
  laTempStru[lnTempStru-5,1] = 'CADDRESS1'
  laTempStru[lnTempStru-5,2] = 'C'
  laTempStru[lnTempStru-5,3] = 30
  laTempStru[lnTempStru-5,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-5,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-5,17], laTempStru[lnTempStru-5,18]
  
  laTempStru[lnTempStru-4,1] = 'CADDRESS2'
  laTempStru[lnTempStru-4,2] = 'C'
  laTempStru[lnTempStru-4,3] = 30
  laTempStru[lnTempStru-4,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-4,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-4,17], laTempStru[lnTempStru-4,18]
  
  laTempStru[lnTempStru-3,1] = 'CADDRESS3'
  laTempStru[lnTempStru-3,2] = 'C'
  laTempStru[lnTempStru-3,3] = 30
  laTempStru[lnTempStru-3,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-3,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-3,17], laTempStru[lnTempStru-3,18]
  
  laTempStru[lnTempStru-2,1] = 'CADDRESS4'
  laTempStru[lnTempStru-2,2] = 'C'
  laTempStru[lnTempStru-2,3] = 30
  laTempStru[lnTempStru-2,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-2,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-2,17], laTempStru[lnTempStru-2,18]
  
  laTempStru[lnTempStru-1,1] = 'CADDRESS5'
  laTempStru[lnTempStru-1,2] = 'C'
  laTempStru[lnTempStru-1,3] = 30
  laTempStru[lnTempStru-1,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-1,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-1,17], laTempStru[lnTempStru-1,18]

  lnTempStru=ALEN(laTempStru,1) + 1
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[end]
  
  DIMENSION laTempStru[lnTempStru,18]
  
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[begin]
  laTempStru[lnTempStru-1,1] = 'CADDRESS6'
  laTempStru[lnTempStru-1,2] = 'C'
  laTempStru[lnTempStru-1,3] = 30
  laTempStru[lnTempStru-1,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-1,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-1,17], laTempStru[lnTempStru-1,18]
  *E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][End]  
  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make.
  laTempStru[lnTempStru,1] = 'cTempKey'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 62
  laTempStru[lnTempStru,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]

  *-- Create temporary file that holding order InvHdr data. [End]

  llFrTime = .F.  && After this time all of your variables have been defined,  you do not need to go to any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.

*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcWorkFile) OR !USED(lcWorkFile)
  *-- System Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24
  *-- System Setting for report [end]
  =lfCreatCur(lcWorkFile)  && Create work cursor.
ENDIF


*-- If user change report critria, Collect report data.
*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection, which is hidden filter.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection, which is hidden filter.
*Use variable llOGFltCh that detect OG filter changes.[Begin]
*B611076,1 MMT 11/08/2015 Sales Journal report exports invoices amount incorrectly to Excel[T20151106.0001][Start]
*IF llClearFn OR loOGScroll.llOGFltCh
IF llClearFn OR loOGScroll.llOGFltCh OR (loOGScroll.cTextRepType = "EXCEL") OR (oAriaApplication.gcDevice = "FILE")
*B611076,1 MMT 11/08/2015 Sales Journal report exports invoices amount incorrectly to Excel[T20151106.0001][End]
*Use variable llOGFltCh that detect OG filter changes.[End]
  llClearFn = .F.
  *-- If the file already have data, clear it.
  IF RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    =lfCreatCur(lcWorkFile)  && Create work cursor again.
    llNoIndex = .T.
  ENDIF  && end If the file already have data, clear it.
  *-- If User Change Index tag due to change sort by.
  IF llNoIndex OR (lcLastTag != lcIndexTg)
    = lfUserChTg()
  ENDIF    && end if User Change Index tag.

  *-- Note that : Optimized expression shape is [ACCOUNT+INVOICE = lcAccVal]
  *-- lcAccOp   : Direct Operator of optimized expression.
  *-- lcAccVal  : Value of optimized expression.
  *-- lcOperator: True Operator of optimized expression.
  STORE '' TO lcAccOp,lcAccVal,lcOperator
  STORE .T. TO llOpLogic  && Logic of operator (if it's .F. its logic is NOT)

  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  =lfCollData()  && Scan around invHdr master file to collect specific critria records.

  *-- Calculate From and To dates.
  *-- If you print Void invoices only.
  IF llVoidOnly
    lcInvDateF = PADR(SUBSTR(loOGScroll.laOGFxFlt[2,6],1,ATC('|',loOGScroll.laOGFxFlt[2,6])-1),10)
    lcInvDateT = PADL(SUBSTR(loOGScroll.laOGFxFlt[2,6],ATC('|',loOGScroll.laOGFxFlt[2,6])+1),10)
  ELSE && else Print either (Invoices) or (invoices and void ).
    lcInvDateF = PADR(SUBSTR(loOGScroll.laOGFxFlt[1,6],1,ATC('|',loOGScroll.laOGFxFlt[1,6])-1),10)
    lcInvDateT = PADL(SUBSTR(loOGScroll.laOGFxFlt[1,6],ATC('|',loOGScroll.laOGFxFlt[1,6])+1),10)
  ENDIF

  STORE .F. TO llChFactor,llChInv    && Unrise all hidden Critria variables.

ELSE  &&  user does not change report critria, print from the same data.

  *-- If User Change Index tag due to change sort by.
  IF lcLastTag != lcIndexTg
    =lfUserChTg()
  ENDIF    && end if User Change Index tag.

ENDIF       && end If user change report critria, Collect report data.

*-- Select Master report file.
SELECT (lcWorkFile)
*-- Relation Section [begin]
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO cWareCode INTO Warehous ADDITIVE
SET RELATION TO Rep1 INTO Salesrep ADDITIVE
*HMA(Case of Multi Currency)[Begin]
IF llMultCurr
  SET RELATION TO ccurrcode INTO Syccurr ADDITIVE
ENDIF
*HMA(Case of Multi Currency)[End]
*-- Relation Section [End]

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Arsjour_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(lcWorkFile))) + LANG_Arsjour_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Arsjour_SecondMsg TIMEOUT 2
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) TIMEOUT 2
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
ELSE
  loProgress.Description =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) 
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*-- Call Report [lcRpForm = 'ARSJOUR.FRX']


loogScroll.cCROrientation = 'P'
*B608247,1 WAM 08/30/2007 Do not export data to Excel file
*EXPORT TO Sales XLS
*B608247,1 WAM 08/30/2007 (End)
*B611076,1 MMT 11/08/2015 Sales Journal report exports invoices amount incorrectly to Excel[T20151106.0001][Start]
IF oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL" AND llMultCurr AND lcRpCurr <> 'F'
 llOGFltCh =.t. 
 SELECT (lcWorkFile)
 LOCATE 
 SCAN
   REPLACE VShipAmt  With gfAmntDisp(VShipAmt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           ShipAmt   With gfAmntDisp(ShipAmt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           Discount  With gfAmntDisp(Discount ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           VDiscount With gfAmntDisp(VDiscount ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           VTotalChg With gfAmntDisp(VTotalChg ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           TotalChg  With gfAmntDisp(TotalChg ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           VCommAmt1 With gfAmntDisp(VCommAmt1  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           VCommAmt2 With gfAmntDisp(VCommAmt2  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           CommAmt1  With gfAmntDisp(CommAmt1  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
           CommAmt2  With gfAmntDisp(CommAmt2 ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
   IF llEngLand
     REPLACE nCharges With gfAmntDisp(nCharges ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             nVCharges With gfAmntDisp(nVCharges ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
   ELSE
     REPLACE VFreight With gfAmntDisp(VFreight,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             VInsur  With gfAmntDisp(VInsur  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             VCod  With gfAmntDisp(VCod  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             Freight  With gfAmntDisp(Freight  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             Insur  With gfAmntDisp(Insur  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             Cod  With gfAmntDisp(Cod  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
   ENDIF
   IF llTaxes
     REPLACE VTax_Amt  With gfAmntDisp(VTax_Amt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             Tax_Amt  With gfAmntDisp(Tax_Amt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
             
   ENDIF 
   IF llCanada
     REPLACE nVPstAmt  With gfAmntDisp(nVPstAmt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             nPstAmt  With gfAmntDisp(nPstAmt  ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             nHstAmt With gfAmntDisp(nHstAmt ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.),;
             nVHstAmt With gfAmntDisp(nVHstAmt ,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.)
   ENDIF          
 ENDSCAN 
 LOCATE
ENDIF
*B611076,1 MMT 11/08/2015 Sales Journal report exports invoices amount incorrectly to Excel[T20151106.0001][End]
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
  DO gfDispRe WITH EVAL('lcRpForm')
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
ELSE
  loProgress.Percent = 0.9
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  PRIVATE loProxy
  loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  oAriaEnvironment.report.OGLastForm = lcRpForm
  oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
  loProgress.Percent = 1.0
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]


SET STATUS BAR &lcStatusBr    && Restore previous status bar status.


*----------------------- Report Code End -----------------------------


*-- Function Section
*-------------------------------------------
*!*************************************************************
*! Name      : lfStitle
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : 1- Get state title.
*!           : 2- Know in which country we are.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Country state title.
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfStitle

lcWorkArea = SELECT()
lcSelectCommand=[SELECT CCONT_CODE FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
  IF !USED('SYCINT')
    =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
  ENDIF
   lcSelectCommand1=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=SYCCOMP.CCONT_CODE]
   lnResult1 = oAriaApplication.remotesystemdata.execute(lcSelectCommand1,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  IF lnResult1 >= 1
    llCanada  = 'CAN' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
    llEngland = 'ENG' $ ALLTRIM(UPPER(SYCCOMP.CCONT_CODE))
  SELECT (lcWorkArea)
  RETURN (SYCINT.CPART4LAB)
  ENDIF
ENDIF
SELECT (lcWorkArea)
*-- End of lfStitle.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Option Grid When function
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwRepWhen
*-- if it's first time to run the report.
*-- using TYPE of variable instead of global llFirstTime, to control
*-- reset case which does not rise llFirsttime, but restore initial
*-- value for lnVarbEnd and advanced case which keep the variables same.
IF TYPE('lnVarbEnd') = 'C'

  SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate ACCOUNT # in option grid.
  SET ORDER TO SALESREP IN SALESREP  && To use it to validate REP     # in option grid.
  SET ORDER TO WAREHOUS IN WAREHOUS  && To use it to validate LOCATION# in option grid.
  SET ORDER TO Codes IN CODES        && To use it to validate STATE# in option grid.

  PRIVATE lcThAlias
  lcThAlias = ALIAS()
  *-- Compute Start of variable filter to control its apperance [begin]
  *-- in option grid.
  lnVarbEnd = 0
  *-- Calculate length of variables appears in option grid.
  *-- and items that we enable and disable.
  *E037378,1 HBG 24/12/2003 Select only records which belong to Aria4. {Begin]
  *lcSelectCommand2=[SELECT * FROM SYREPUVR WHERE Crep_id="ARSJOUR" AND cExpType="V" AND Nvarpos!=0]
  PRIVATE lcSelectCommand2,lnResult2

  lcSelectCommand2=[SELECT * FROM SYREPUVR WHERE Crep_id="ARSJOUR" AND cExpType="V" AND nVarPos!=0 AND (EMPTY(CVER) OR CVER = "A40")]

  *E037378,1 [End]
  *E038429,1 WSH Open SYREPUVR from Aria4 System Files [Start]
  *lnResult2 = oAriaApplication.remotesystemdata.execute(lcSelectCommand2,"","TMPREPUVR","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  lnResult2  = oAriaApplication.remotesystemdata.execute(lcSelectCommand2,"","TMPREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("DATASESSION"))
  *E038429,1 WSH [End]

  IF lnResult2 >= 1
    lnVarbEnd = lnVarbEnd + 1
  ENDIF

  SELECT (lcThAlias)

  IF EMPTY(laRpFltVal)
    *-- laRpFltVal : Array to hold D for disable and E for enable, to control
    *--              Fixed filter appearance in option grid.
    *-- laRpVarNow : Array to hold .T. or .F., to control variables
    *--              appearance in option grid.
    DECLARE laRpFltVal[ALEN(loOGScroll.laOGFxFlt,1)],laRpVarNow[lnVarbEnd]
    STORE 'E' TO laRpFltVal
    STORE .T. TO laRpVarNow

    llTaxes =gfGetMemVar('M_TAX')='Y'  && .T., if company use taxes.

    IF llMultCurr
      IF !USED('SYCCURR')
        =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
      ENDIF
      SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
    ENDIF
  ENDIF
ENDIF  && END IF you first time enter when function.
*HMA ,Disable the Sort#2 Option When the sort#1 is by invoice [Begin]
lnSortPos=lfVarPos('lcRpSort2')
DO lpShowObj
*HMA ,Disable the Sort#2 Option When the sort#1 is by invoice [End]
*-- note that disable and enable is according to value of laRpFltVal.
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 04/06/2004
*! Purpose   : OG when function
*!***************************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGObjType,UPPER(lcItmInFlt))
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVarPos.

*!*************************************************************
*! Name      : lfFltState
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 11/12/2003
*! Purpose   : Enable and disable selected objects.
*!*************************************************************
*! Parameters: 1- Filter Object Number
*!             2- 'E' for enable, 'D' for disable
*!*************************************************************
*! Returns   : ....
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFltState
PARAMETERS lnObjNum,lcObjState

IF lcObjState = 'D' AND !EMPTY(loOGScroll.laOGFxFlt[lnObjNum,6])
  loOGScroll.laOGFxFlt[lnObjNum,6] = ''
ENDIF
LAOGOBJCNT[LNOBJNUM + LNVARBEND] = (LCOBJSTATE = 'E')
= LFOGSHOWGET('loOGScroll.LAOGFXFLT[' + ALLTRIM(STR(LNOBJNUM)) + ',6]')  && ENABLE / DISABLE OBJECT .
*-- End of lfFltState.

*!*************************************************************
*! Name      : lfFormName
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfFormName

RETURN 'ARSJOUR'
*-- End of lfFormName.

*!*************************************************************
*! Name      : lfUserChTg
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Change Work file Index Tag.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfUserChTg
SELECT (lcWorkFile)
INDEX ON &lcIndexTg TAG (lcWorkFile)
IF llNoIndex
  llNoIndex = .F.
ELSE
  lcLastTag = lcIndexTg
ENDIF
*-- End of lfUserChTg.

*!*************************************************************
*! Name      : lfCollData
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollData

PRIVATE lcOptimize
lcOptimize = ''

*--B127496,MMT,04/19/2005,fix bug of not printing void invoice[Start]
*!*  =lfGetOpExp()

*!*  *-- lcRpExp : Report expression, and I know that it must not be empty
*!*  *--         " but I check for empty because any one can remove hidden filter.

*!*  lcRpExp = lcOptimize + IIF(EMPTY(lcRpExp),'',[ AND ] + lcRpExp)
*!*  lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")

*!*  *Add void date to filter expression  [Begin]

*!*  *Check for the posted filter also.[START]
*!*  IF !EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6]) OR IIF(lcRpDateTP = "P" , !EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6]) , .F.)
*!*  *Check for the posted filter also.[END]

*!*    *-- If Void Only.
*!*    IF lcRpList = "V"
*!*      lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
*!*    ENDIF

*!*    *Check for the posted filter also.[START]
*!*    IF lcRpList = "V" AND lcRpDateTP = "P"
*!*      lcRpExp = STRTRAN(lcRpExp,"DPOSTDATE","VDATE")
*!*    ENDIF
*!*    *Check for the posted filter also.[END]

*!*    *-- If Print Both Active and Void invoices.
*!*    IF lcRpList = "B"
*!*     *change the filter expr 'lcRpExp' to set the date range
*!*     *In the same level [start]
*!*     *The report gives error when using the transaction date.[START]
*!*     *Add check for date,change or to and in LCRPEXP.   [Start]
*!*
*!*     *Check for the posted filter also.[START]
*!*      IF (!EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)) OR IIF(lcRpDateTP = "P" , (!EMPTY(ldStrtDPst) OR !EMPTY(ldEndDPst)) , .F.)
*!*     *Check for the posted filter also.[END]

*!*        *Change AND to OR. [Begin]
*!*        DO lpGenExpr
*!*        *Change AND to OR. [End]

*!*      ENDIF
*!*      *Add check for date,change or to and in LCRPEXP.[End]
*!*      *The report gives error when using the transaction date.[END]
*!*      *in the same level[end]
*!*    ENDIF
*!*  ENDIF
*!*  *Add void date to filter expression  [End  ]

*!*  SELECT INVHDR
*!*  SET ORDER TO   && Activate rushmore optimizing.
*!*  SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer

*!*  *-- cTempKey : Field in work file that contains Country code, plus State code,
*!*  *--          : plus region code.
*!*  *--          : this field serves in sort by [Country,State,Region]


*!*  *When running the report to show both active and void invoices and for a specific
*!*  *division - the report prints the void invoices from all divisions.[START]
*!*  lcDivsion = ".T."
*!*  IF  "V" $ lcRpList OR "B" $ lcRpList
*!*    STORE 0 TO lnDataDiv
*!*    lnDataDiv = ASCAN(loOGScroll.laOGVrFlt,'INVHDR.CDIVISION')
*!*    IF lnDataDiv > 0
*!*      lnDataDiv  = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnDataDiv,1)
*!*      lcDivsion = IIF(!EMPTY(loOGScroll.laOGVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION $ laOgVrFlt[lnDataDiv,6]", ".T.")
*!*    ENDIF
*!*  ENDIF
*!*  *division - the report prints the void invoices from all divisions.[END]

*!*  *When running the report to show both active and void invoices and for a specific
*!*  *division - the report prints the void invoices from all divisions.[START]

*!*  *Replace the filter in case the posted date.[START]
*!*  IF lcRpDateTP = "P"

*!*    lcRpExp = STRTRAN(lcRpExp,"INVDATE","DPOSTDATE")
*!*    lcRpExp = STRTRAN(lcRpExp,"ldStrtDate","ldStrtDPst")
*!*    lcRpExp = STRTRAN(lcRpExp,"ldEndDate","ldEndDPst")
*!*  ENDIF
*!*  *Replace the filter in case the posted date.[END]
*!*
*--B127496,MMT,04/19/2005,fix bug of not printing void invoice[End]
*E999999,1 HMA 10/02/2005 Change Season,Division,Location,Payment Terms Filters from LIKE to INLIST [BEGIN]
lnSelect=SELECT()
SELECT INVHDR
lcNewExp=""
IF LEN(loOGScroll.lcRpExp) > 1
  lcExp = loOGScroll.lcRpExp
  lnOccur = OCCURS(' AND',lcExp)
  IF lnOccur > 0
    FOR lnCount = 1 TO lnOccur + 1
      lnStart = IIF(lnCount = 1 , 1 , ATC(' AND',lcExp,lnCount-1) + 5)
      lnEnd = IIF(lnCount = lnOccur + 1,LEN(lcExp),ATC(' AND',lcExp,lnCount))
      lnLength = lnEnd - lnStart +IIF(lnCount = lnOccur + 1,1,0)
      lcTake = SUBSTR(lcExp,lnStart,lnLength)
      lnoccurs=ATC('INLIST',lcTake)
      lnSeaOcurr=ATC('SEASON',lcTake)
      lnDivOcurr=ATC('CDIVISION',lcTake)
      lnTrmOcurr=ATC('CTERMCODE',lcTake)
      lnLocOcurr=ATC('CWARECODE',lcTake)

      *WSH [Start]
      *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][Start]
      lnInvOcurr=ATC('INVOICE',lcTake)
      *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][End]
      lnAccOcurr=ATC('ACCOUNT',lcTake)
      lnCurOcurr=ATC('CCURRCODE',lcTake)
      lnAdrOcurr=ATC('CADDRESS4',lcTake)
      lnRepOcurr=ATC('REP1',lcTake)
      *WSH [End]

      IF lnoccurs > 0

        *WSH [Start]
        *IF  (lnSeaOcurr > 0 OR lnDivOcurr > 0 OR lnTrmOcurr > 0 OR lnLocOcurr > 0)
        *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][Start] 
        *IF (lnSeaOcurr > 0 OR lnDivOcurr > 0 OR lnTrmOcurr > 0 OR lnLocOcurr > 0 OR;
            lnAccOcurr > 0 OR lnCurOcurr > 0 OR lnAdrOcurr > 0 OR lnRepOcurr > 0)
        IF (lnSeaOcurr > 0 OR lnDivOcurr > 0 OR lnTrmOcurr > 0 OR lnLocOcurr > 0 OR;
            lnAccOcurr > 0 OR lnCurOcurr > 0 OR lnAdrOcurr > 0 OR lnRepOcurr > 0 OR lnInvOcurr > 0)
        *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][End]
        *WSH [End]

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
lcRpExp = IIF(EMPTY(lcNewExp),loOGScroll.lcRpExp,lcNewExp)
SELECT(lnSelect)
*--Create temp files for Division,Season,Location,payment terms [BEGIN]
*--Make Temp File For Selected invoice Divisions

*WSH [Start]
*!*  lcDivCursor =""
*!*  lnInvDivision = ASCAN(loOgScroll.laOgVrFlt,"INVHDR.CDIVISION")
*!*  lnInvDivision = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnInvDivision,1)
*!*  lcDivisions= loOgScroll.laOgVrFlt[lnInvDivision,6]
*!*  IF !EMPTY(lcDivisions)
*!*    IF lnInvDivision > 0
*!*      lcDivCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
*!*      DIMENSION laTempacstru[1,4]
*!*      laTempacstru[1,1]='CDIVISION'
*!*      laTempacstru[1,2]='C'
*!*      laTempacstru[1,3]= 6
*!*      laTempacstru[1,4]= 0
*!*      gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
*!*      IF !EMPTY(lcDivisions)
*!*        lnStart=1
*!*        lnEnd=AT('|',lcDivisions)
*!*        DO WHILE lnEnd <> 0
*!*          SELECT(lcDivCursor)
*!*          APPEND BLANK
*!*          REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
*!*          lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
*!*          lnEnd=AT('|',lcDivisions)
*!*        ENDDO
*!*        IF lnEnd = 0
*!*          SELECT(lcDivCursor)
*!*          APPEND BLANK
*!*          REPLACE CDIVISION WITH lcDivisions
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF

*!*  IF !EMPTY(lcDivCursor)
*!*    SET ORDER TO TAG (lcDivCursor)
*!*  ENDIF
lcDivCursor = lfOGTempCurs("INVHDR.CDIVISION", "CDIVISION", "CDIVISION", "loOGScroll.laOgVrFlt", .T.)
*WSH [End]

*--Make Temp File For Selected Invoice Seasons

*WSH [Start]
*!*  lcSeaCursor =""
*!*  lnInvSeason = ASCAN(loOgScroll.laOgVrFlt,"INVHDR.SEASON")
*!*  lnInvSeason = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnInvSeason,1)
*!*  lcSeasons= loOgScroll.laOgVrFlt[lnInvSeason,6]
*!*  IF !EMPTY(lcSeasons)
*!*    IF lnInvSeason > 0
*!*      lcSeaCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
*!*      DIMENSION laTempacstru[1,4]
*!*      laTempacstru[1,1]='SEASON'
*!*      laTempacstru[1,2]='C'
*!*      laTempacstru[1,3]= 6
*!*      laTempacstru[1,4]= 0
*!*      gfCrtTmp(lcSeaCursor,@laTempacstru,"SEASON",lcSeaCursor,.T.)
*!*      IF !EMPTY(lcSeasons)
*!*        lnStart=1
*!*        lnEnd=AT('|',lcSeasons)
*!*        DO WHILE lnEnd <> 0
*!*          SELECT(lcSeaCursor)
*!*          APPEND BLANK
*!*          REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
*!*          lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
*!*          lnEnd=AT('|',lcSeasons)
*!*        ENDDO
*!*        IF lnEnd = 0
*!*          SELECT(lcSeaCursor)
*!*          APPEND BLANK
*!*          REPLACE SEASON WITH lcSeasons
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  IF !EMPTY(lcSeaCursor)
*!*    SET ORDER TO TAG (lcSeaCursor)
*!*  ENDIF
lcSeaCursor = lfOGTempCurs("INVHDR.SEASON", "SEASON", "SEASON", "loOGScroll.laOgVrFlt", .T.)
*WSH [End]

*--Make Temp File For Selected Invoice Locations

*WSH [Start]
*!*  lcLocCursor =""
*!*  lnInvLocation = ASCAN(loOgScroll.laOgVrFlt,"INVHDR.CWARECODE")
*!*  lnInvLocation = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnInvLocation,1)
*!*  lcLocs= loOgScroll.laOgVrFlt[lnInvLocation,6]
*!*  IF !EMPTY(lcLocs)
*!*    IF lnInvLocation > 0
*!*      lcLocCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Seasons
*!*      DIMENSION laTempacstru[1,4]
*!*      laTempacstru[1,1]='CWARECODE'
*!*      laTempacstru[1,2]='C'
*!*      laTempacstru[1,3]= 6
*!*      laTempacstru[1,4]= 0
*!*      gfCrtTmp(lcLocCursor,@laTempacstru,"CWARECODE",lcLocCursor,.T.)
*!*      IF !EMPTY(lcLocs)
*!*        lnStart=1
*!*        lnEnd=AT('|',lcLocs)
*!*        DO WHILE lnEnd <> 0
*!*          SELECT(lcLocCursor)
*!*          APPEND BLANK
*!*          REPLACE CWARECODE WITH SUBSTR(lcLocs,lnStart,lnEnd-1)
*!*          lcLocs = STUFF(lcLocs ,lnStart,lnEnd,"")
*!*          lnEnd=AT('|',lcLocs)
*!*        ENDDO
*!*        IF lnEnd = 0
*!*          SELECT(lcLocCursor)
*!*          APPEND BLANK
*!*          REPLACE CWARECODE WITH lcLocs
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  IF !EMPTY(lcLocCursor)
*!*    SET ORDER TO TAG (lcLocCursor)
*!*  ENDIF
lcLocCursor = lfOGTempCurs("INVHDR.CWARECODE", "CWARECODE", "CWARECODE", "loOGScroll.laOgVrFlt", .T.)
*WSH [End]

*--Make Temp File For Selected Invoice payment Term Codes

*WSH [Start]
*!*  lcTrmCursor =""
*!*  lnInvTerm = ASCAN(loOgScroll.laOgVrFlt,"INVHDR.CTERMCODE")
*!*  lnInvTerm = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnInvTerm,1)
*!*  lcTerms= loOgScroll.laOgVrFlt[lnInvTerm,6]
*!*  IF !EMPTY(lcTerms)
*!*    IF lnInvTerm > 0
*!*      lcTrmCursor = loOgScroll.gfTempName() &&Cursor Hold Selected payment Term Codes
*!*      DIMENSION laTempacstru[1,4]
*!*      laTempacstru[1,1]='CTERMCODE'
*!*      laTempacstru[1,2]='C'
*!*      laTempacstru[1,3]= 6
*!*      laTempacstru[1,4]= 0
*!*      gfCrtTmp(lcTrmCursor,@laTempacstru,"CTERMCODE",lcTrmCursor,.T.)
*!*      IF !EMPTY(lcTerms)
*!*        lnStart=1
*!*        lnEnd=AT('|',lcTerms)
*!*        DO WHILE lnEnd <> 0
*!*          SELECT(lcTrmCursor)
*!*          APPEND BLANK
*!*          REPLACE CTERMCODE WITH SUBSTR(lcTerms,lnStart,lnEnd-1)
*!*          lcTerms = STUFF(lcTerms ,lnStart,lnEnd,"")
*!*          lnEnd=AT('|',lcTerms)
*!*        ENDDO
*!*        IF lnEnd = 0
*!*          SELECT(lcTrmCursor)
*!*          APPEND BLANK
*!*          REPLACE CTERMCODE WITH lcTerms
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  IF !EMPTY(lcTrmCursor)
*!*    SET ORDER TO TAG (lcTrmCursor)
*!*  ENDIF
lcTrmCursor = lfOGTempCurs("INVHDR.CTERMCODE", "CTERMCODE", "CTERMCODE", "loOGScroll.laOgVrFlt", .T.)
*WSH [End]

*B999999,1 MMT 02/16/2005 Covert State option to Mover instead of browse [Start]

*WSH [Start]
*!*  lcStatCursor =""
*!*  lnCustStat = ASCAN(loOgScroll.laOgVrFlt,"CUSTOMER.CADDRESS4")
*!*  lnCustStat = ASUBSCRIPT(loOGScroll.laOgVrFlt,lnCustStat ,1)
*!*  lcAddress  = loOgScroll.laOgVrFlt[lnCustStat ,6]
*!*  IF !EMPTY(lcAddress)
*!*    IF lnCustStat > 0
*!*      lcStatCursor = loOgScroll.gfTempName() &&Cursor Hold Selected payment Term Codes
*!*      DIMENSION laTempacstru[1,4]
*!*      laTempacstru[1,1]='Address'
*!*      laTempacstru[1,2]='C'
*!*      laTempacstru[1,3]= 30
*!*      laTempacstru[1,4]= 0
*!*      gfCrtTmp(lcStatCursor ,@laTempacstru,"Address",lcStatCursor ,.T.)
*!*      IF !EMPTY(lcAddress  )
*!*        lnStart=1
*!*        lnEnd=AT('|',lcAddress  )
*!*        DO WHILE lnEnd <> 0
*!*          SELECT(lcStatCursor )
*!*          APPEND BLANK
*!*          REPLACE Address WITH SUBSTR(lcAddress  ,lnStart,lnEnd-1)
*!*          lcAddress   = STUFF(lcAddress   ,lnStart,lnEnd,"")
*!*          lnEnd=AT('|',lcAddress  )
*!*        ENDDO
*!*        IF lnEnd = 0
*!*          SELECT(lcStatCursor )
*!*          APPEND BLANK
*!*          REPLACE Address WITH lcAddress
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF
*!*  IF !EMPTY(lcStatCursor )
*!*    SET ORDER TO TAG (lcStatCursor )
*!*  ENDIF
lcStatCursor = lfOGTempCurs("CUSTOMER.CADDRESS4", "Address", "Address", "loOGScroll.laOgVrFlt", .T.)
lcAcctCursor = lfOGTempCurs("INVHDR.ACCOUNT", "ACCOUNT", "ACCOUNT", "loOGScroll.laOgVrFlt", .F.)
lcCurrCursor = lfOGTempCurs("INVHDR.CCURRCODE", "CCURRCODE", "CCURRCODE", "loOGScroll.laOgFxFlt", .F.)
lcRep1Cursor = lfOGTempCurs("INVHDR.REP1", "Rep1", "Rep1", "loOGScroll.laOgVrFlt", .F.)
*E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][Start]
lcInvCursor = lfOGTempCurs("INVHDR.INVOICE", "INVOICE", "INVOICE", "loOGScroll.laOgFxFlt", .F.)
IF !EMPTY(lcInvCursor ) AND USED(lcInvCursor )
  SELECT (lcInvCursor )
  LOCATE 
  IF EOF()
    lcInvCursor =''
  ENDIF
ENDIF
*E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][End]
*B611908,1 ES 10/13/2019  Sales Journal report does not check if the Currency and Accout cursor contains data before using it.[Start]
IF !EMPTY(lcCurrCursor) AND USED(lcCurrCursor)
  SELECT (lcCurrCursor)
  LOCATE 
  IF EOF()
    lcCurrCursor =''
  ENDIF
ENDIF

IF !EMPTY(lcAcctCursor) AND USED(lcAcctCursor)
  SELECT (lcAcctCursor)
  LOCATE 
  IF EOF()
    lcAcctCursor =''
  ENDIF
ENDIF

IF !EMPTY(lcRep1Cursor) AND USED(lcRep1Cursor)
  SELECT (lcRep1Cursor)
  LOCATE 
  IF EOF()
    lcRep1Cursor=''
  ENDIF
ENDIF

*B611908,1 ES 10/13/2019  Sales Journal report does not check if the Currency and Accout cursor contains data before using it.[End]



*WSH [End]

*B999999,1 MMT 02/16/2005 Covert State option to Mover instead of browse [End]

*--Create temp files for Division,Season,Location,payment terms [END]
*--B127496,MMT,04/19/2005,fix bug of not printing void invoice[Start]

*B607904,1 MMT 12/25/2006 Fix bug of error when Select All Accounts [Start]
*=lfGetOpExp()
*B607904,1 MMT 12/25/2006 Fix bug of error when Select All Accounts [End]

*-- lcRpExp : Report expression, and I know that it must not be empty
*--         " but I check for empty because any one can remove hidden filter.

*B607904,1 MMT 12/25/2006 Fix bug of error when Select All Accounts [Start]
*lcRpExp = lcOptimize + IIF(EMPTY(lcRpExp),'',[ AND ] + lcRpExp)
*B607904,1 MMT 12/25/2006 Fix bug of error when Select All Accounts [End]

lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")

*Add void date to filter expression  [Begin]

*Check for the posted filter also.[START]
IF !EMPTY(loOGScroll.laOGFxFlt[lnInvPos,6]) OR IIF(lcRpDateTP = "P" , !EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6]) , .F.)
*Check for the posted filter also.[END]

  *-- If Void Only.
  IF lcRpList = "V"
    lcRpExp = STRTRAN(lcRpExp,"INVDATE","VDATE")
  ENDIF

  *Check for the posted filter also.[START]
  IF lcRpList = "V" AND lcRpDateTP = "P"
    lcRpExp = STRTRAN(lcRpExp,"DPOSTDATE","VDATE")
  ENDIF
  *Check for the posted filter also.[END]

  *-- If Print Both Active and Void invoices.
  IF lcRpList = "B"
   *change the filter expr 'lcRpExp' to set the date range
   *In the same level [start]
   *The report gives error when using the transaction date.[START]
   *Add check for date,change or to and in LCRPEXP.   [Start]

   *Check for the posted filter also.[START]
    IF (!EMPTY(ldStrtDate) OR !EMPTY(ldEndDate)) OR IIF(lcRpDateTP = "P" , (!EMPTY(ldStrtDPst) OR !EMPTY(ldEndDPst)) , .F.)
   *Check for the posted filter also.[END]

      *Change AND to OR. [Begin]
      DO lpGenExpr
      *Change AND to OR. [End]

    ENDIF
    *Add check for date,change or to and in LCRPEXP.[End]
    *The report gives error when using the transaction date.[END]
    *in the same level[end]
  ENDIF
ENDIF
*Add void date to filter expression  [End  ]


*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [Begin]
IF !USED('Custom_Reg')
  =gfOpenTable('Customer','Customer','SH','Custom_Reg')
ENDIF
*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [End]

SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer

*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [Begin]
SET RELATION TO 'M'+ Account  INTO  Custom_Reg ADDITIVE
*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [End]


*-- cTempKey : Field in work file that contains Country code, plus State code,
*--          : plus region code.
*--          : this field serves in sort by [Country,State,Region]


*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[START]
lcDivsion = ".T."
IF  "V" $ lcRpList OR "B" $ lcRpList
  STORE 0 TO lnDataDiv
  lnDataDiv = ASCAN(loOGScroll.laOGVrFlt,'INVHDR.CDIVISION')
  IF lnDataDiv > 0
    lnDataDiv  = ASUBSCRIPT(loOGScroll.laOGVrFlt,lnDataDiv,1)
    lcDivsion = IIF(!EMPTY(loOGScroll.laOGVrFlt[lnDataDiv,6]) , "INVHDR.CDIVISION $ laOgVrFlt[lnDataDiv,6]", ".T.")
  ENDIF
ENDIF
*division - the report prints the void invoices from all divisions.[END]

*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[START]

*Replace the filter in case the posted date.[START]
IF lcRpDateTP = "P"
  lcRpExp = STRTRAN(lcRpExp,"INVDATE","DPOSTDATE")
  lcRpExp = STRTRAN(lcRpExp,"ldStrtDate","ldStrtDPst")
  lcRpExp = STRTRAN(lcRpExp,"ldEndDate","ldEndDPst")
ENDIF
*Replace the filter in case the posted date.[END]
*--B127496,MMT,04/19/2005,fix bug of not printing void invoice[End]

*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [Begin]
lcRpExp = STRTRAN(lcRpExp,'CUSTOMER.REGION','IIF(EMPTY(CUSTOMER.REGION),Custom_Reg.REGION,CUSTOMER.REGION)')
*B611612,1 Es 07/04/2018 Modify this issue-> "Missing accounts in selection grid while running report from A/R - Sales Journal" [End]
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName')  = 'C'
  SELECT INVHDR
  *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][Start]
*!*	  COUNT FOR &lcRpExp AND &lcDivsion AND (!EMPTY(lcAcctCursor) AND SEEK(Account, lcAcctCursor)) AND;
*!*	          (!EMPTY(lcCurrCursor) AND SEEK(cCurrCode, lcCurrCursor)) AND ;
*!*	          (!EMPTY(lcRep1Cursor) AND SEEK(Rep1, lcRep1Cursor)) AND ;
*!*	          (!EMPTY(lcDivCursor) AND SEEK(CDIVISION, lcDivCursor)) AND ;
*!*	          (!EMPTY(lcSeaCursor) AND SEEK(SEASON, lcSeaCursor)) AND ;
*!*	          (!EMPTY(lcLocCursor) AND SEEK(CWARECODE, lcLocCursor)) AND ;
*!*	          (!EMPTY(lcTrmCursor) AND SEEK(CTERMCODE, lcTrmCursor)) AND ;
*!*	          (!EMPTY(lcStatCursor) AND SEEK(LEFT(Customer.CADDRESS4,6), lcStatCursor))   TO lnCntProg
  COUNT FOR &lcRpExp AND &lcDivsion AND (!EMPTY(lcAcctCursor) AND SEEK(Account, lcAcctCursor)) AND;
          (!EMPTY(lcCurrCursor) AND SEEK(cCurrCode, lcCurrCursor)) AND ;
          (!EMPTY(lcRep1Cursor) AND SEEK(Rep1, lcRep1Cursor)) AND ;
          (!EMPTY(lcDivCursor) AND SEEK(CDIVISION, lcDivCursor)) AND ;
          (!EMPTY(lcSeaCursor) AND SEEK(SEASON, lcSeaCursor)) AND ;
          (!EMPTY(lcLocCursor) AND SEEK(CWARECODE, lcLocCursor)) AND ;
          (!EMPTY(lcTrmCursor) AND SEEK(CTERMCODE, lcTrmCursor)) AND ;
          (!EMPTY(lcInvCursor) AND SEEK(INVOICE, lcInvCursor)) AND ;
          (!EMPTY(lcStatCursor) AND SEEK(LEFT(Customer.CADDRESS4,6), lcStatCursor))   TO lnCntProg
  *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][End]
  lnCntProgNo = 0
  LOCATE 
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]

SELECT INVHDR
SCAN FOR &lcRpExp AND &lcDivsion
  *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][Start]
  IF !EMPTY(lcInvCursor) AND !SEEK(INVOICE, lcInvCursor)
    LOOP
  ENDIF
  *E612288,1 MMT 07/20/2021 Allow user to filter by Invoice#[T20201207.0004][End]
  
  *WSH [Start]
  IF !EMPTY(lcAcctCursor) AND !SEEK(Account, lcAcctCursor)
    LOOP
  ENDIF

  IF !EMPTY(lcCurrCursor) AND !SEEK(cCurrCode, lcCurrCursor)
    LOOP
  ENDIF

  IF !EMPTY(lcRep1Cursor) AND !SEEK(Rep1, lcRep1Cursor)
    LOOP
  ENDIF
  *WSH [End]

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
  IF !EMPTY(lcLocCursor)
    IF !SEEK(CWARECODE,lcLocCursor)
      LOOP
    ENDIF
  ENDIF
  IF !EMPTY(lcTrmCursor)
    IF !SEEK(CTERMCODE,lcTrmCursor)
      LOOP
    ENDIF
  ENDIF

  *B999999,1 MMT 02/16/2005 Covert State option to Mover instead of browse [Start]
  IF !EMPTY(lcStatCursor)

    *WSH [Start]
    *IF !SEEK(Customer.CADDRESS4,lcStatCursor)
    *B609339,1 MMT 07/11/2010 Sales Journal Report Collect data incorrectly IF State selected[Start]
    *IF !SEEK(ALLTRIM(Customer.CADDRESS4), lcStatCursor)
    IF !SEEK(LEFT(Customer.CADDRESS4,6), lcStatCursor)
    *B609339,1 MMT 07/11/2010 Sales Journal Report Collect data incorrectly IF State selected[End]
    *WSH [End]

      LOOP
    ENDIF
  ENDIF
  *B999999,1 MMT 02/16/2005 Covert State option to Mover instead of browse [End]

*SCAN FOR &lcRpExp AND &lcDivsion
*E999999,1 HMA 10/02/2005 Change Season,Division,Location,Payment Terms Filters from LIKE to INLIST [END]


*When running the report to show both active and void invoices and for a specific
*division - the report prints the void invoices from all divisions.[END]

  *Add wait window to show collecting data [Begin.]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Arsjour_ColDataMsg + Invoice NOWAIT
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            Invoice NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
ELSE 
  IF lnCntProg > 0
  lnCntProgNo = lnCntProgNo + 1
  lnPerCent = lnCntProgNo/lnCntProg
  IF MOD(lnCntProgNo,CEILING(lnCntProg/ 10)) = 0
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            Invoice
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  ENDIF 
  ENDIF       
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
  *Add wait window to show collecting data [End.]

  SCATTER MEMVAR MEMO
  *Empty value for the region in case any transaction for stores.[START]
  m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
  *Empty value for the region in case any transaction for stores.[END]

  *C101713,1 A custom process to calculate the shipped amount and the Mrech. Discount.[START]
  * =gfDoTriger('ARSJOUR',PADR('SALJORPRIC',10))
  *C101713,1 [END]
  
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[begin]
  m.cstname=CUSTOMER.btname
  *E303472,1 RAS 05/12/2014 add customer name field to sales journal excel report[end]

*B611886,1 ES 08/22/2019 Sales Journal report display value in vtotalchg when export field for Voided invoice has invdate and vdate falls in selected date range while same invoice show zeros in preview [Start]
IF STATUS = 'V' AND !EVALUATE(lcVoidExpr) 
    m.vtotalchg = 0
  ENDIF
*B611886,1 ES 08/22/2019 Sales Journal report display value in vtotalchg when export field for Voided invoice has invdate and vdate falls in selected date range while same invoice show zeros in preview [End]

  *Add the code that use the posted date.[START]
  IF lcRpDateTP = "P"
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDPst,ldEndDPst) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDPst,ldEndDPst))
      = lfNegValue() && Negative void values.
    ENDIF
  ELSE
    IF llVoidOnly OR (STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                     !BETWEEN(INVDATE,ldStrtDate,ldEndDate))
      = lfNegValue() && Negative void values.
    ENDIF
  ENDIF
  *Add the code that use the posted date.[END]
  *E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][Start]
  lcShpTName  =''
  DIMENSION laShipTo[6,1]
  lfGetShipToAddress(m.Order)
  m.stname = lcShpTName  
  m.CADDRESS1 = laShipTo[1]
  m.CADDRESS2 = laShipTo[2]
  m.CADDRESS3 = laShipTo[3]
  m.CADDRESS4 = laShipTo[4]    
  m.CADDRESS5 = laShipTo[5]
  m.CADDRESS6 = laShipTo[6]
  *E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][End]
  INSERT INTO (lcWorkFile) FROM MEMVAR
ENDSCAN
*-- End of lfCollData.

*!*************************************************************
*! Name      : lfGetOpExp
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 11/12/2003
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfGetOpExp
lcOptimize = ''
PRIVATE lcAlias

lnAccPos = CEILING(ASCAN(loOGScroll.laOGVrFlt,'INVHDR.ACCOUNT')/7) && Position of Account in variable filter.
*-- if you find [Account is <In List>] in the variable filter section.
IF lnAccPos != 0
  llOpLogic  = loOGScroll.laOGVrFlt[lnAccPos,4]
  lcAccOp    = loOGScroll.laOGVrFlt[lnAccPos,5]
  *HMA,Modify the Optimized expression due to change the Account Screen From INLIST into INRANGE [Begin]
  *!*      lcAccVal   = loOGScroll.laOGVrFlt[lnAccPos,6]
  IF loOGScroll.laOGVrFlt[lnAccPos,7]="V"
    lcAccVal   = loOGScroll.laOGVrFlt[lnAccPos,6]
  ELSE         &&loOGScroll.laOGVrFlt[lnAccPos,7]="R"       Case of using a cursor holding Selected Data
    lcAccVal = ""
    lcAlias  = Alias()
    lcCursor = loOGScroll.laOGVrFlt[lnAccPos,6]
    IF !EMPTY(lcCursor) AND USED(lcCursor)
      SELECT (lcCursor)
      SCAN
        IF EMPTY(lcAccVal)
          lcAccVal = Account
        ELSE
          lcAccVal=lcAccVal+"|"+Account
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (lcAlias)
  ENDIF
  *HMA,Modify the Optimized expression due to change the Account Screen From INLIST into INRANGE [End]
  lcOperator = ''

  *-- If user assign values for account selection.
  IF !EMPTY(lcAccVal)

    *-- if Logic of account filter "Is"
    IF llOpLogic

      DO CASE
        CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Like
          lcOperator = '='
        CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatThan
          lcOperator = '>'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessThan
          lcOperator = '<'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatOrEql
          lcOperator = '>='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessOrEql
          lcOperator = '<='
      ENDCASE
    ELSE  && else Logic of account filter "Is Not"

      DO CASE
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Like
          lcOperator = '!='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatThan
          lcOperator = '<='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessThan
          lcOperator = '>='
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_GreatOrEql
          lcOperator = '<'
		CASE ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_LessOrEql
          lcOperator = '>'
      ENDCASE

    ENDIF  && end if Logic of account filter "Is"

    *-- if Operator is [Contains or Between or InList]
    IF EMPTY(lcOperator)
      lcTrueFals = IIF(llOpLogic,'','!')  && Logic shape mask.
      IF ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Contain
        lcOptimize = [&lcTrueFals.(lcAccVal $ ACCOUNT+INVOICE)]
      ELSE  && Operator is either BETWEEN or INLIST.
        IF OCCUR('|',lcAccVal) = 0
          lcOptimize = [ACCOUNT+INVOICE &lcTrueFals.= lcAccVal]
        ELSE
		  lcOperator = IIF(ALLTRIM(UPPER(lcAccOp)) == LANG_Arsjour_Between,LANG_Arsjour_Between,LANG_Arsjour_InList)
          lcOptimize = [&lcTrueFals.&lcOperator(ACCOUNT+INVOICE,] + lfMakeOpt(lcAccVal) + [)]
        ENDIF

      ENDIF

    ELSE  && else Operator is some thing rather than [Contains or Between or InList]
      lcOptimize = [ACCOUNT+INVOICE &lcOperator. lcAccVal]
    ENDIF  && end if Operator is [Contains or Between or InList]

  ENDIF  && end If user assign values for account selection.

ENDIF  && end if you find [Account is <In List>] in the variable filter section.
*-- When there is no optimize expression [user delete Account from OG]
*-- or does not assign any values for account filter,
*-- INVOICE Index is faster than ACCOUNT+INVOICE Index in collecting data.
lcOptimize = IIF(EMPTY(lcOptimize),[INVOICE = ''],lcOptimize)
*-- End of lfGetOpExp.

*!*************************************************************
*! Name      : lfMakeOpt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Make mask for optimize expression if operator
*!           : is either BETWEEN or INLIST.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : Optimized mask.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfMakeOpt
PARAMETERS lcString
PRIVATE lnPipeNo,lcExpr

lnPipeNo = OCCUR('|',lcString)

lcExpr = ''
FOR lnI = 1 TO lnPipeNo
  lcExpr    = IIF(EMPTY(lcExpr),"'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'",;
              lcExpr + "," + "'" +;
              SUBSTR(lcString,1,ATC('|',lcString)-1) + "'")
  lcString      = SUBSTR(lcString,ATC('|',lcString)+1)
ENDFOR
RETURN (lcExpr + "," + "'" + lcString + "'")
*-- End of lfMakeOpt.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[1,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[1,2] = 65                && Line length
= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- End of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 11/12/2003
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
*-- End of lfwOldVal.
*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : 1- Put both index and group expressions for all sort cases.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfGetRepVr
LOCAL lnResult3,lcSelectCommand3
llSortGrp = llMultCurr AND (lcRpCurr = 'F') AND (lcRpSortBy !='U')
lcCurrGrp = IIF(llSortGrp,[CCURRCODE],'')

DO CASE
  CASE lcRpSortBy = 'I'  && Sort by invoice
    IF lcRpKind = 'D'
      lcIndexTg  = [INVOICE]    && Index expression.
    ELSE
      lcIndexTg  = [CCURRCODE + INVOICE]    && Index expression.
    ENDIF


    lcSubTitle = [Invoice]    && Sub Title
    lcGroup    = ''           && Report Group
    lcGrpFoot  = ['']         && Group title

  CASE lcRpSortBy = 'A'  && Sort by account
    IF llSortGrp
      lcIndexTg  = [ACCOUNT + CCURRCODE + INVOICE]
    ELSE
      lcIndexTg  = [ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Account]
    lcGroup    = [ACCOUNT]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcGrpFoot  = [LANG_Arsjour_Account+'# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]
    lcGrpFoot  = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))+;
                 ['+'# ' + ACCOUNT + " - " + ALLTRIM(CUSTOMER.BTNAME)]
    *N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'L'  && Sort by location
    IF llSortGrp
      lcIndexTg  = [CWARECODE + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [CWARECODE + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Location]
    lcGroup    = [CWARECODE]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcGrpFoot  = [LANG_Arsjour_Location+'# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]
    lcGrpFoot  = [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))+;
                ['+'# ' + cWareCode + " - " + ALLTRIM(WAREHOUS.CDESC)]
    *N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'R'  && Sort by primary sales rep.
    IF llSortGrp
      lcIndexTg  = [REP1 + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [REP1 + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [Primary Rep]
    lcGroup    = [REP1]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_SalesRep+'# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+'# ' + Rep1 + " - " + ALLTRIM(SALESREP.NAME)]
*N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'C'  && Sort by country

    IF llSortGrp
      lcIndexTg  = [LEFT(cTempKey,20) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [LEFT(cTempKey,20) + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Country]
    lcGroup    = [LEFT(cTempKey,20)]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_Country+':  ' + ALLTRIM(CUSTOMER.cAddress6)]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+':  ' + ALLTRIM(CUSTOMER.cAddress6)]
*N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'S'  && Sort by state
    IF llSortGrp
      lcIndexTg  = [SUBSTR(cTempKey,21,36)+ CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [SUBSTR(cTempKey,21,36) + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = SUBSTR(lcSTitle,1,8)

    lcGroup    = [SUBSTR(cTempKey,21,36)]


    IF !USED('SYCINT')
      =gfOpenFile(oAriaApplication.SysPath+'SYCINT',oAriaApplication.SysPath+'Ccontcode','SH')
    ENDIF
    *B609182,1 MMT 03/21/2010 Fix error while preview Sales journal report Sorted by State {Start}
    *lcSelectCommand3=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=CUSTOMER.CCONT_CODE]
    lcSelectCommand3=[SELECT CCONT_CODE,CPART4LAB FROM SYCINT WHERE SYCINT.CCONT_CODE=']+CUSTOMER.CCONT_CODE+[']
    *B609182,1 MMT 03/21/2010 Fix error while preview Sales journal report Sorted by State {End}
    lnResult3= oAriaApplication.remotesystemdata.execute(lcSelectCommand3,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))

    IF lnResult3 >= 1
      lcGrpFoot= [']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+['+': ' + LEFT(cTempKey,20) + '  ' +;
                  IIF(!EMPTY(ALLTRIM(SYCINT.CPART4LAB)), ALLTRIM(SYCINT.CPART4LAB) + ': ','') +;
                  ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
    ELSE
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot= [LANG_Arsjour_Country+': ' + LEFT(cTempKey,20) + '  ' +ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
lcGrpFoot= [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))+': ' + LEFT(cTempKey,20) + '  ' +ALLTRIM(SUBSTR(cTempKey,27,30)) + '  ' + gfCodDes(ALLTRIM(SUBSTR(cTempKey,27,30)),'STATE')]
*N000682,1 11/20/2012 MMT Globlization changes[End]


    ENDIF

  CASE lcRpSortBy = 'G'  && Sort by region
    IF llSortGrp
      lcIndexTg  = [RIGHT(cTempKey,6) + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [RIGHT(cTempKey,6) + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Region]
    lcGroup    = [RIGHT(cTempKey,6)]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_Region+': ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))+': ' + gfCodDes(RIGHT(cTempKey,6),'REGION')]
*N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'D'  && Sort by division
    IF llSortGrp
      lcIndexTg  = [cDIVISION + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cDIVISION + ACCOUNT + INVOICE]
    ENDIF


    lcSubTitle = [Division]
    lcGroup    = [cDIVISION]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_Division+'# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))+'# ' + cDIVISION + '  ' + gfCodDes(cDIVISION,'CDIVISION')]
*N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'T'  && Sort by terms
    IF llSortGrp
      lcIndexTg  = [cTermCode + CCURRCODE + ACCOUNT + INVOICE]
    ELSE
      lcIndexTg  = [cTermCode + ACCOUNT + INVOICE]
    ENDIF

    lcSubTitle = [P. Terms]
    lcGroup    = [cTermCode]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_Terms+':  ' + gfCodDes(cTermCode,'CTERMCODE')]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Terms,oAriaApplication.GetHeaderText("LANG_Arsjour_Terms",AHEADERFILE))+':  ' + gfCodDes(cTermCode,'CTERMCODE')]
*N000682,1 11/20/2012 MMT Globlization changes[End]


  CASE lcRpSortBy = 'U'  && Sort by currency
    lcIndexTg  = [CCURRCODE + ACCOUNT + INVOICE]
    lcSubTitle = [Currency]
    lcGroup    = [CCURRCODE]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGrpFoot  = [LANG_Arsjour_Currency+'# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
lcGrpFoot  = [IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+'# ' + CCURRCODE + ' - ' + SYCCURR.cCurrDesc]
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcCurrGrp  = ''
ENDCASE

* Add the second sort by option. [Begin]
IF lcRpSort2 <> 'N'
  PRIVATE lcAddSort , lc2ndTitle
  DO CASE
    CASE lcRpSort2 = 'I'  && Sort by invoice
      lcAddSort  = [INVOICE]
      lc2ndTitle = [Invoice]
    CASE lcRpSort2 = 'A'  && Sort by account
      lcAddSort  = [ACCOUNT]
      lc2ndTitle = [Account]
    CASE lcRpSort2 = 'L'  && Sort by location
      lcAddSort  = [CWARECODE]
      lc2ndTitle = [Location]
    CASE lcRpSort2 = 'R'  && Sort by primary sales rep.
      lcAddSort  = [REP1]
      lc2ndTitle = [Primary Rep]
    CASE lcRpSort2 = 'C'  && Sort by country
      lcAddSort  = [LEFT(cTempKey,20)]
      lc2ndTitle = [Country]
    CASE lcRpSort2 = 'S'  && Sort by state
      lcAddSort  = [SUBSTR(cTempKey,21,36)]
      lc2ndTitle = SUBSTR(lcSTitle,1,8)
    CASE lcRpSort2 = 'G'  && Sort by region
      lcAddSort  = [RIGHT(cTempKey,6)]
      lc2ndTitle = [Region]
    CASE lcRpSort2 = 'D'  && Sort by division
      lcAddSort  = [CDIVISION]
      lc2ndTitle = [Division]
    CASE lcRpSort2 = 'T'  && Sort by terms
      lcAddSort  = [CTERMCODE]
      lc2ndTitle = [P. Terms]
    CASE lcRpSort2 = 'U'  && Sort by currency
      lcAddSort  = [CCURRCODE]
      lc2ndTitle = [Currency]
  ENDCASE

  IF lcRpSortBy = 'I'
    lcIndexTg = lcIndexTg + [ + ] + lcAddSort
  ELSE
    lcIndexTg = SUBSTR(lcIndexTg,1,AT('+',lcIndexTg)) + [ ] + lcAddSort + [ + ] + SUBSTR(lcIndexTg,AT('+',lcIndexTg)+1)
  ENDIF
  lcSubTitle = lcSubTitle + [+] + lc2ndTitle
ENDIF
*Add the second sort by option. [End]
*-- End of lfGetRepVr.

*!*************************************************************
*! Name      : lfvChFact
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 11/12/2003
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvChFact

llChFactor = .T.
*-- End of lfvChFact.

*!*************************************************************
*! Name      : lfvList
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : 1- Change Invoices/Void Invoices logical variable To recollect data.
*!           : 2- Enable and disable Invoice date or Void date due to user selection.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvList
*!*  _screen.Visible =.T.

llChInv = .T.  && Logical invoice variable.
llVoidOnly = (lcRpList='V')  && To use it in .FRX
*-- End of lfvList.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
= lfGetRepVr()
*-- End of lfvCurDisp.

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Validate Currency code in SYCCURR file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp

lcVar = OGSYS18()                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , 'SYCCURR'))

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = [CCURRCODE:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+[' ,    ;
                CCURRSMBL:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Symbol,oAriaApplication.GetHeaderText("LANG_Arsjour_Symbol",AHEADERFILE))+[' ,      ;
                CCURRDESC:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Description,oAriaApplication.GetHeaderText("LANG_Arsjour_Description",AHEADERFILE))+[' , ;
				NCURRUNIT:H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Units,oAriaApplication.GetHeaderText("LANG_Arsjour_Units",AHEADERFILE))+[' ]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcFile_Ttl = LANG_Arsjour_Currencies
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currencies,oAriaApplication.GetHeaderText("LANG_Arsjour_Currencies",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  = gfBrows('','CCURRCODE','laTemp')

  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF

ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- End of lfvCurr.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Amin (HMA)
*! Date      : 04/08/2004
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
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Parameters: Start collection date,End collection date
*!*************************************************************
*! Returns   : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Create cursor
*!*************************************************************
*! Parameters: Cursor Name
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCreatCur
PARAMETERS lcCurName

gfCrtTmp(lcCurName,@laTempStru,"lcIndexTg",lcWorkFile,.F.)
*-- End of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.

*-- Close temp. opended files, if it used.

*-- Delete temporary work file.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF

*-- if user change setting [enter report <Preview> or <Run>]
IF !llFrTime
  *SET CENTURY &lcCentury
  SET HOURS TO &lcSetHour
ENDIF  && end if user change setting [enter report <Preview> or <Run>].
*-- End of lfClearRep.

*!*************************************************************
*! Name      : lfPreRun
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Notes     : 1- lcIndexTg : is master report file index due to sort case.
*!           : 2- While this function has one line of code calls another function
*!           :    I write it for other reasons and for any other to add any
*!           :    enhancement code.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfPreRun
= lfGetRepVr()      && Get Report variables such as groups and index.
RETURN .T.
*-- End of lfPreRun.

*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Fill Sort Array
*!*************************************************************
*! Parameters: ...
*!*************************************************************
*! Returns   : ...
*!*************************************************************
*! Modification : ....
*!*************************************************************
*-- Fill Sort Arrays.
FUNCTION lfSortDumy
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 8
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDesc[lnArrElmnt,1],laSortVal[lnArrElmnt,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[1] = LANG_Arsjour_Invoice
laSortDesc[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[2] = LANG_Arsjour_Account
laSortDesc[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[3] = LANG_Arsjour_SalRepresent
laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalRepresent,oAriaApplication.GetHeaderText("LANG_Arsjour_SalRepresent",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[4] = LANG_Arsjour_Country
laSortDesc[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSortDesc[5] = lcSTitle       && State variable Title
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[6] = LANG_Arsjour_Region
laSortDesc[6] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[7] = LANG_Arsjour_Division
laSortDesc[7] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[8] = LANG_Arsjour_PaymentTerms
laSortDesc[8] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PaymentTerms,oAriaApplication.GetHeaderText("LANG_Arsjour_PaymentTerms",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


laSortVal[1] = 'I'
laSortVal[2] = 'A'
laSortVal[3] = 'R'
laSortVal[4] = 'C'
laSortVal[5] = 'S'
laSortVal[6] = 'G'
laSortVal[7] = 'D'
laSortVal[8] = 'T'

IF llMultLoc
  =AINS(laSortDesc,3)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[3] = LANG_Arsjour_Location
laSortDesc[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  =AINS(laSortVal,3)
  laSortVal[3] = 'L'
ENDIF

IF llMultCurr
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDesc[ALEN(laSortDesc,1)] = LANG_Arsjour_Currency
laSortDesc[ALEN(laSortDesc,1)] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laSortVal[ALEN(laSortDesc,1)]  = 'U'
ENDIF
*-- End of lfSortDumy.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Parameters: ...
*!*************************************************************
*! Returns   : Position
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!*************************************************************
*! Name      : lfNegValue
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : -Ve Void values
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfNegValue
PRIVATE lnFldsCnt , lcMemField
lcMemField = ''
lnFldsCnt = 0
FOR lnFldsCnt = 1 TO FCOUNT()
  IF TYPE(FIELD(lnFldsCnt)) = "N"
    lcMemField = "m." + FIELD(lnFldsCnt)
    &lcMemField = -1 * &lcMemField
  ENDIF
ENDFOR
*-- End of lfNegValue.

*!*************************************************************
*! Name      : lfRegion
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : To get the data from the store fields.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfRegion
PRIVATE lcRegion , lnCUSRec


lcAlias = SELECT(0)
SELECT CUSTOMER
lnCUSRec = EVAL(KEY())

IF SEEK('M'+CUSTOMER.Account)
  lcRegion = PADR(CUSTOMER.REGION,6)
ENDIF

=SEEK(lnCUSRec , 'CUSTOMER')

SELECT(lcAlias)
RETURN lcRegion
*-- End of lfRegion.
*!**************************************************************************
*! Name      : lfSortDum2
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Fill second sort bu option
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
FUNCTION lfSortDum2
PRIVATE lnArrElmnt

llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
lnArrElmnt = 9
lnArrElmnt = IIF(llMultCurr,lnArrElmnt+1,lnArrElmnt)
lnArrElmnt = IIF(llMultLoc,lnArrElmnt+1,lnArrElmnt)

DIMENSION laSortDes2[lnArrElmnt,1],laSortVal2[lnArrElmnt,1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[1] = LANG_Arsjour_None
laSortDes2[1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_None,oAriaApplication.GetHeaderText("LANG_Arsjour_None",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[2] = LANG_Arsjour_Invoice
laSortDes2[2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[3] = LANG_Arsjour_Account
laSortDes2[3] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[4] = LANG_Arsjour_SalRepresent
laSortDes2[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalRepresent,oAriaApplication.GetHeaderText("LANG_Arsjour_SalRepresent",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[5] = LANG_Arsjour_Country
laSortDes2[5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Country,oAriaApplication.GetHeaderText("LANG_Arsjour_Country",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

laSortDes2[6] = lcSTitle       && State variable Title
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[7] = LANG_Arsjour_Region
laSortDes2[7] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Region,oAriaApplication.GetHeaderText("LANG_Arsjour_Region",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[8] = LANG_Arsjour_Division
laSortDes2[8] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Division,oAriaApplication.GetHeaderText("LANG_Arsjour_Division",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[9] = LANG_Arsjour_PaymentTerms
laSortDes2[9] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PaymentTerms,oAriaApplication.GetHeaderText("LANG_Arsjour_PaymentTerms",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


laSortVal2[1] = 'N'
laSortVal2[2] = 'I'
laSortVal2[3] = 'A'
laSortVal2[4] = 'R'
laSortVal2[5] = 'C'
laSortVal2[6] = 'S'
laSortVal2[7] = 'G'
laSortVal2[8] = 'D'
laSortVal2[9] = 'T'

IF llMultLoc
  =AINS(laSortDes2,4)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[4] = LANG_Arsjour_Location
laSortDes2[4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Location,oAriaApplication.GetHeaderText("LANG_Arsjour_Location",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  =AINS(laSortVal2,4)
  laSortVal2[4] = 'L'
ENDIF

IF llMultCurr
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laSortDes2[ALEN(laSortDes2,1)] = LANG_Arsjour_Currency
laSortDes2[ALEN(laSortDes2,1)] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  laSortVal2[ALEN(laSortDes2,1)]  = 'U'
ENDIF
*-- End of lfSortDum2.

*!**************************************************************************
*! Name      : lfvSort
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Valid function for both 1st , 2nd Sort by option.
*!**************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
FUNCTION lfvSort
DO lpShowObj
IF lcRpSortBy = lcRpSort2
  lcRpSort2 = 'N'
loOGScroll.EnableObject('lcRpSort2', lcRpSortBy <> 'I')
ENDIF
*-- End of lfvSort.

*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 04/06/2004
*! Purpose   : Enable/Disable all options in OG
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
PROCEDURE lpShowObj
PRIVATE llShwInRng

IF lnSortPos > 0
  IF lcRpSortBy = 'I'
    lcRpSort2 = 'N'
*    llShwInRng = .F.
*    loOGScroll.laOGObjCnt[lnSortPos] = .F.
  ELSE
*    llShwInRng = .T.
*    loOGScroll.laOGObjCnt[lnSortPos] = .T.
  ENDIF
*  =lfOGShowGet('lcRpSort2')  && Show get Object .
  loOGScroll.EnableObject('lcRpSort2',lcRpSortBy <> 'I')
ENDIF
*-- End of lShowObj.

*!***************************************************************************
*! Name      : lpGenExpr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Generate Expression.
*!***************************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!**************************************************************************
*Function to add the void expr. after the invoice date expr.
PROCEDURE lpGenExpr
PRIVATE lcAlias , lnX , lcInvExp , lcVoidExp

lcAlias = ALIAS()

*-- Copy all laOGFxFlt to another array to save the old value.
DIMENSION laTempExpr[1]
=ACOPY(loOGScroll.laOGFxFlt,laTempExpr)         && Copy Fixed Filter Array to Temp Array.

*-- Define new Fixed filter array to hold one expression only.
DIMENSION loOGScroll.laOGFxFlt[1,7]
*ssh
*loOGScroll.laOGFxFlt = ""

*-- Copy only Month Range expression to laOGFxFlt.

*Modify the array to use the posted date filter.[START]

IF lcRpDateTP = "P"
  =ACOPY(laTempExpr,loOGScroll.laOGFxFlt,AELEMENT(laTempExpr,lnPostDate,1),7)
ELSE
  =ACOPY(laTempExpr,loOGScroll.laOGFxFlt,AELEMENT(laTempExpr,lnInvPos,1),7)
ENDIF
*Modify the array to use the posted date filter.[END]

*-- Generate expression for Month Range.
lcInvExp = gfGenFlt('loOGScroll.laOGFxFlt',.T.,.T.)
lcInvExp = STRTRAN(lcInvExp,"INVHDR.","")

lcVoidExp = "( " + lcInvExp + " OR BETWEEN(IIF(STATUS='V',VDATE,INVDATE),ldStrtDate,ldEndDate) )"

*-- Remove Style Group from lcRpExp.
lcRpExp = STRTRAN(lcRpExp,lcInvExp,lcVoidExp)
*-- If user selected Style.

*-- Restore original laOGFxFlt.
DIMENSION loOGScroll.laOGFxFlt[1]
=ACOPY(laTempExpr,loOGScroll.laOGFxFlt)         && Restore Fixed Filter Array to Temp Array.

SELECT (lcAlias)
*-- End of lpGenExpr.

*!*************************************************************
*! Name      : lfClrRead
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 11/12/2003
*! Purpose   : Function used to suppressing the field in the grid.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Modifications:  ....
*!*************************************************************
* Refresh the option grid in case the posted date.[START]
FUNCTION lfClrRead
ClearRead()
* Refresh the option grid in case the posted date.[END]
*-- End of lfClrRead.

*WSH [Start]
*!*************************************************************
*! Name      : lfOGTempCurs
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 03/02/2006
*! Purpose   : Function used to create Temp Cursor from OG Expr.
*!*************************************************************
*! Parameters:
*!*************************************************************
*! Return    : Cursor Name
*!*************************************************************
FUNCTION lfOGTempCurs
LPARAMETERS lcExpr, lcFldName, lcIndexExp, laOgArray, llInList

LOCAL lnAlias, lcCursor, lnPos, lcValue, lnStart, lnEnd
lnAlias  = SELECT(0)
lnPos    = ASCAN(&laOgArray, lcExpr)
lnPos    = IIF(lnPos > 0, ASUBSCRIPT(&laOgArray, lnPos, 1), 0)
lcCursor = ""

IF lnPos > 0
  lcValue = &laOgArray.[lnPos,6]

  IF !llInList
    RETURN lcValue
  ENDIF

  IF !EMPTY(lcValue)
    lcCursor = loOgScroll.gfTempName() &&Cursor Hold Selected Divisions
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1] = lcFldName
    laTempacstru[1,2] = 'C'
    laTempacstru[1,3] = 6
    laTempacstru[1,4] = 0
    gfCrtTmp(lcCursor, @laTempacstru, lcIndexExp, lcCursor, .T.)
    IF !EMPTY(lcValue)
      lnStart = 1
      lnEnd   = AT('|', lcValue)
      DO WHILE lnEnd <> 0
        SELECT (lcCursor)
        APPEND BLANK
        REPLACE &lcFldName WITH SUBSTR(lcValue, lnStart, lnEnd - 1)

        lcValue = STUFF(lcValue, lnStart, lnEnd, "")
        lnEnd   = AT('|', lcValue)
      ENDDO
      IF lnEnd = 0
        SELECT (lcCursor)
        APPEND BLANK
        REPLACE &lcFldName WITH lcValue
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(lcCursor)
  SET ORDER TO TAG (lcCursor)
ENDIF

SELECT (lnAlias)
RETURN IIF(!EMPTY(lcCursor) AND USED(lcCursor), lcCursor, "")
*WSH [End]
*E612018,1 MMT 02/18/2020 Add new option to display new report collects data from new GL tables [GL Enhacement][Start]
*!*************************************************************
*! Name      : lfvReconcile
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/18/2020
*! Purpose   : Function used to Validate 'Reconcile with GL' option
*!*************************************************************
FUNCTION lfvReconcile
lcSessValue = lcSession
IF llPrintMod <> llOldMode
  llOldMode= llPrintMod 
  

  STORE '' TO loogScroll.aRepVrFlt, loogScroll.laOGFxFlt,loogScroll.laOGVRFlt,loogScroll.laOGHDFlt
  loogScroll.lcOGPrgName = IIF(llPrintMod ,'ARSJRREP','ARSJOUR')
  lnRemoteRslt = loogScroll.SQLExecute("SYDREPRT","Select * from SYDREPRT where cRep_ID='"+PADR(ALLTRIM(loogScroll.lcOGPrgName),loogScroll.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
  IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
    RETURN .F.
  ENDIF

  SELECT SYDREPRT
  loogScroll.lnRepIDLen = LEN(cRep_ID)
  WITH loogScroll
    STORE PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen) TO .lcOGPrgName, .lcOGRepID, .lcOGManRep
    IF (.lcOGPrgName != .lcOGRepID)
      USE IN SYDREPRT
    lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where  cRep_ID='"+PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen)+"'",;
                                       '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                        3,"",loogScroll.Parent.DataSessionID)
      IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
      .lcOGPrgName =  .lcOGRepID
        lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where  'cRep_ID='"+PADR(ALLTRIM(This.lcOGPrgName),This.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
      ENDIF
    ENDIF
  ENDWITH
  SELECT SYDREPRT
  LOCATE
  loogScroll.gcAct_Appl = cApp_ID
  IF !llFromWhen
    lcSession = lcSessValue
    loogScroll.InitRepHeader()
  ENDIF
  lcSession = lcSessValue
  loOGScroll.GetRepVar()
  lcSession = lcSessValue
  WITH loOGScroll.OuterContainer.InnerContainer
    DO WHILE .ControlCount > 0 AND !('PRINTMOD' $ UPPER(.Controls(.ControlCount).Name))
     .RemoveObject(.Controls(.ControlCount).Name)
    ENDDO
  ENDWITH
  lcSession = lcSessValue
  loOGScroll.defineobjects (.T.)
  lcSession = lcSessValue
  =loOGScroll.RefreshScroll()
  loogScroll.lcOGPrgName = 'ARSJOUR'
  loogScroll.lcOGManRep= 'ARSJOUR'

  IF llPrintMod 
    lcRpForm = "ARSJRREP"
    loOgScroll.lcOGLastForm = "ARSJRREP"
  
    IF !USED('FSPRD')
	  =gfOpenTable('FSPRD','COMFYRPRDI','SH')   && CFISFYEAR+CFSPPRDID)
	ENDIF
	lcAcMask   = SPACE(0)         && Account mask
	lnAcLen    = 0                && Account Length
	lcGlFld = SPACE(0)         && Chart of account browse fields.
	lcAcntFld  = SPACE(0)         && Account field that will be validated.
	lcAcntDesF = SPACE(0)         && Account Description.
	llTaxes =gfGetMemVar('M_TAX')='Y'  && .T., if company use taxes.
    IF !USED('SYCCURR')
	    =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
    ENDIF
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
	llNoThing  = lfSetGLMsk()

  ELSE
    lcRpForm = "ARSJOUR"
    loOgScroll.lcOGLastForm = "ARSJOUR"

  ENDIF
  loOgScroll.lcRpFrxMod = "Graphics"
  lcRpFrxMod = loOgScroll.lcRpFrxMod
  loogscroll.Parent.cntVariables.lcRepMode = loOgScroll.lcRpFrxMod
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.displayValue   = loogscroll.Parent.cntVariables.lcRepMode
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.F.  

ENDIF
*!*************************************************************
*! Name      : lfReconWithGL
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/18/2020
*! Purpose   : Function used to call the new report layout and collect its data
*!*************************************************************
FUNCTION lfReconWithGL
lcStTime   = TIME()    && Time in which we start collect data.
llNoIndex = .F.        && I don't make index for file.
*-- Show messages in status bar when collecting data. [begin]
lcStatusBr = SET('STATUS BAR')
SET STATUS BAR ON
lcRpDateTP ='B'
LCRPKIND ='D'
lcRpCurr = 'O'
LCRPSORT2 = 'N'
lcRpMsg1  =''
*-- Use variable llOGFltCh that detect OG filter changes.[Begin]
IF llClearFn OR loOGScroll.llOGFltCh
  STORE .F. TO llChAcc,llChTrnTyp,llClearFn
  lcLastExpr = lcRpExp
ENDIF
STORE {  /  /  } TO ldStrtDate , ldEndDate
*-- Use variable llOGFltCh that detect OG filter changes.[End]
*-- Show messages in status bar when collecting data. [begin]
IF lcRpActBy ='P'
  lnPeriodRow = ASCAN(loogscroll.laogfxflt, "lcPrdVar")
  lnPeriodRow= ASUBSCRIPT(loogscroll.laogfxflt, lnPeriodRow, 1)
  lcPeriod = ''
  STORE '' TO lcPerFrom ,lcPerTo  
  IF !EMPTY(loogscroll.laogfxflt[lnPeriodRow,6])
    SELECT (loogscroll.laogfxflt[lnPeriodRow,6])
    LOCATE 
    IF !EOF()
      lcPeriod =loogscroll.laogfxflt[lnPeriodRow,6]
      LOCATE
      lcPerFrom = STRTRAN(EVALUATE(lcPeriod +'.Keyexp') ,"-","")   && Remove the hyphen "-"
      GOTO bottom
      lcPerTo   = STRTRAN(EVALUATE(lcPeriod +'.Keyexp'),"-","")  && Remove the hyphen "-"
    ENDIF
  ENDIF
  IF !EMPTY(lcPerFrom) OR !EMPTY(lcPerTo)
    ldStrtDate = IIF(SEEK(lcPerFrom,'FsPrd'),FsPrd.dFsppBgDt,DTOC({}))
    ldEndDate = IIF(SEEK(lcPerTo,'FsPrd'),FsPrd.dFsppEnDt,DTOC({}))
  ENDIF  
ELSE
  lnPostDate= ASCAN(loogscroll.laogfxflt, "DPOSTDATE")
  lnPostDate= ASUBSCRIPT(loogscroll.laogfxflt, lnPostDate, 1)
  IF !EMPTY(loOGScroll.laOGFxFlt[lnPostDate,6])
    ldStrtDate = CTOD(PADR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])-1))
    ldEndDate= CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnPostDate,6],ATC('|',loOGScroll.laOGFxFlt[lnPostDate,6])+1))
  ENDIF    
ENDIF
*Include void invoices amount and ship amount if upper invoice date is less than void invoice date. [Begin]
*Fill the date variables.[START]
llVoidOnly = .F.
lcVoidExpr = [llVoidOnly OR (STATUS = 'V' AND ((BETWEEN(VDATE,ldStrtDate,ldEndDate) AND !BETWEEN(DPOSTDATE,ldStrtDate,ldEndDate)) OR (!BETWEEN(VDATE,ldStrtDate,ldEndDate) AND BETWEEN(DPOSTDATE,ldStrtDate,ldEndDate))))]
lcInvDateF = DTOC(ldStrtDate)
lcInvDateT = DTOC(ldEndDate)
*Don't get to this code in case posted date.[END]
*Include void invoices amount and ship amount if upper invoice date is less than [End]

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  *-- lcDetExp   : Detail band expression.
  *-- lcCurrExp  : Currency group footer expression.
  *-- lcGrpExp   : Variable group footer expression.
  *-- lcGrandExp : Summary band expression.

  * Display HST Tax amount for Canada. [Begin]
  lcDetExp = [IIF(llMultCurr AND (lcRpCurr != 'F') AND (lcRpSortBy !='U'),']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Invoice,oAriaApplication.GetHeaderText("LANG_Arsjour_Invoice",AHEADERFILE))+['+']+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Currency,oAriaApplication.GetHeaderText("LANG_Arsjour_Currency",AHEADERFILE))+;
              ['+' : ' + cCurrCode + " , ",'')] +;
              [+ IIF(llRpRepPrn,']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
              ['+' ' + Rep1 +  " " +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+['+" =  "  +;
              TRANSFORM(lnRepComm,"9999999.99") ," ")] +;
              [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_PstAmount",AHEADERFILE))+[' +]+;
              [TRANSFORM(lnPstAmt,"99999999.99")+;
              SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstAmount,oAriaApplication.GetHeaderText("LANG_Arsjour_HstAmount",AHEADERFILE))+[' + ;
              TRANSFORM(lnHstAmt,"99999999.99")," ")]
  lcCurrExp  = [IIF(llRpRepPrn, SPACE(10) +']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
               [' +TRANSFORM(lnCommCur,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+;
               [' +TRANSFORM(lnPstAmtCr,"99999999.99")+SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
               TRANSFORM(lnHstAmtCr,"99999999.99")," ")]
  lcGrpExp = [IIF(llRpRepPrn, SPACE(10) +']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
             ['+' '+']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
             ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Total,oAriaApplication.GetHeaderText("LANG_Arsjour_Total",AHEADERFILE))+;
             ['  +TRANSFORM(lnCommGrp,"9999999.99") ," ")] +;
             [+IIF(llCanada, SPACE(10) +']+ ;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_PstTotal",AHEADERFILE))+[' +;
             TRANSFORM(lnPstAmtGp,"99999999.99")+SPACE(10) + ']+;
             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstTotal,oAriaApplication.GetHeaderText("LANG_Arsjour_HstTotal",AHEADERFILE))+[' + ;
             TRANSFORM(lnHstAmtGp,"99999999.99")," ")]
  lcGrandExp = [IIF(llRpRepPrn, SPACE(10) + ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SalesRep,oAriaApplication.GetHeaderText("LANG_Arsjour_SalesRep",AHEADERFILE))+;
               ['+' '+']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_CommPayable,oAriaApplication.GetHeaderText("LANG_Arsjour_CommPayable",AHEADERFILE))+;
               ['+' '+']+iIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Grand,oAriaApplication.GetHeaderText("LANG_Arsjour_Grand",AHEADERFILE))+[' +;
               TRANSFORM(lnCommTot,"9999999.99") ," ")] +;
               [+IIF(llCanada, SPACE(10) +']+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_PstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_PstGrand",AHEADERFILE))+[' +;
               TRANSFORM(lnPstAmtot,"99999999.99")+;
               SPACE(10) +']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_HstGrand,oAriaApplication.GetHeaderText("LANG_Arsjour_HstGrand",AHEADERFILE))+[' + ;
                               TRANSFORM(lnHstAmtot,"99999999.99")," ")]
  * Display HST Tax amount for Canada. [End]

  *-- Create temporary file that holding order InvHdr data. [begin]
  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT INVHDR
  =AFIELD(laTempStru)
  lnTempStru=ALEN(laTempStru,1) + 2
  DIMENSION laTempStru[lnTempStru,18]
  laTempStru[lnTempStru-1,1] = 'cstname'
  laTempStru[lnTempStru-1,2] = 'C'
  laTempStru[lnTempStru-1,3] = 30
  laTempStru[lnTempStru-1,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru-1,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru-1,17], laTempStru[lnTempStru-1,18]
  
  
  *-- cTempKey : field used in most sort by case as the master key.
  *--          : note that field width is dependent on number of sort
  *--          : case he make.
  laTempStru[lnTempStru,1] = 'cTempKey'
  laTempStru[lnTempStru,2] = 'C'
  laTempStru[lnTempStru,3] = 62
  laTempStru[lnTempStru,4] = 0
  FOR  lnInc=7 TO 16
    STORE SPACE(1) TO laTempStru[lnTempStru,lnInc]
  ENDFOR
  STORE 0  TO laTempStru[lnTempStru,17], laTempStru[lnTempStru,18]

  *-- Create temporary file that holding order InvHdr data. [End]

  llFrTime = .F.  && After this time all of your variables have been defined,  you do not need to go to any llFrTime block again.
ENDIF  && end if it's first time you run option Grid.
 = lfGetRepVr()      && Get Report variables such as groups and index.
*-- Create temporary cursors from structure array. [begin]
IF EMPTY(lcWorkFile) OR !USED(lcWorkFile)
  *-- System Setting for report [begin]
  lcSetHour = SET('HOURS')
  SET HOURS TO 24
  *-- System Setting for report [end]
  =lfCreatCur(lcWorkFile)  && Create work cursor.
ENDIF

IF llClearFn OR loOGScroll.llOGFltCh OR (loOGScroll.cTextRepType = "EXCEL") OR (oAriaApplication.gcDevice = "FILE")
  llClearFn = .F.
 
  *-- If the file already have data, clear it.
  IF RECCOUNT(lcWorkFile) > 0
    USE IN (lcWorkFile)
    =lfCreatCur(lcWorkFile)  && Create work cursor again.
    llNoIndex = .T.
  ENDIF  && end If the file already have data, clear it.
  *-- If User Change Index tag due to change sort by.
  IF llNoIndex OR (lcLastTag != lcIndexTg)
    = lfUserChTg()
  ENDIF    && end if User Change Index tag.

  *-- Note that : Optimized expression shape is [ACCOUNT+INVOICE = lcAccVal]
  *-- lcAccOp   : Direct Operator of optimized expression.
  *-- lcAccVal  : Value of optimized expression.
  *-- lcOperator: True Operator of optimized expression.
  STORE '' TO lcAccOp,lcAccVal,lcOperator
  STORE .T. TO llOpLogic  && Logic of operator (if it's .F. its logic is NOT)

  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  =lfCollDataGL()  && Scan around invHdr master file to collect specific critria records.

  *-- Calculate From and To dates.
  *-- If you print Void invoices only.
  
  


  STORE .F. TO llChFactor,llChInv    && Unrise all hidden Critria variables.

ELSE  &&  user does not change report critria, print from the same data.

  *-- If User Change Index tag due to change sort by.
  IF lcLastTag != lcIndexTg
    =lfUserChTg()
  ENDIF    && end if User Change Index tag.

ENDIF       && end If user change report critria, Collect report data.

*-- Select Master report file.
SELECT (lcWorkFile)
*-- Relation Section [begin]
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer
SET RELATION TO cWareCode INTO Warehous ADDITIVE
SET RELATION TO Rep1 INTO Salesrep ADDITIVE
SET RELATION TO ccurrcode INTO Syccurr ADDITIVE

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.


*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[End]
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
ELSE 
  
  loProgress.Description =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SelectMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SelectMsg",AHEADERFILE)) +' '+ ;
            ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_RecInMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_RecInMsg",AHEADERFILE)) + ;
            ALLTRIM(STR(lnInterval,6,2)) + ;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_SecondMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_SecondMsg",AHEADERFILE)) 
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]

loogScroll.cCROrientation = 'P'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
DO gfDispRe WITH 'ARSJRREP'
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
ELSE
  loProgress.Percent = 0.9
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  PRIVATE loProxy
  loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  oAriaEnvironment.report.OGLastForm = 'ARSJRREP'
  oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
  loProgress.Percent = 1.0
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
ENDIF
*E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]

SET STATUS BAR &lcStatusBr    && Restore previous status bar status.

*!*************************************************************
*! Name      : lfCollDataGL
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/18/2020
*! Purpose   : Make optimized expression then Collecting data
*!             from InvHdr file into Work file.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollDataGL

PRIVATE lcOptimize
lcOptimize = ''

lnGLRow = ASCAN(loogscroll.laogfxflt, "lcRpGlAcc")
lnGLRow= ASUBSCRIPT(loogscroll.laogfxflt, lnGLRow, 1)
lcGlCondFileUse= ''
IF !EMPTY(loogscroll.laogfxflt[lnGLRow,6])
  SELECT (loogscroll.laogfxflt[lnGLRow,6])
  LOCATE 
  IF !EOF()
    lcGlCondFileUse=loogscroll.laogfxflt[lnGLRow,6]
  ENDIF
ENDIF
SELECT INVHDR
SET ORDER TO   && Activate rushmore optimizing.
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer


IF !USED('GLTRNHD')
  =gfOpenTable('GLTRNHD','GLTRNHD')
ENDIF
IF !USED('GLTRNDT')
  =gfOpenTable('GLTRNDT','GLTRNDTNO')
ENDIF
IF !EMPTY(lcGlCondFileUse) AND USED(lcGlCondFileUse)
  SELECT GLTRNDT
  =gfSetOrder('GLTRNDTAC')
  SELECT (lcGlCondFileUse)
  LOCATE
  SCAN 
    =gfSqlRun("Select DISTINCT cTRAN_NO FROM GLTRNDT WHERE cAcctCode = '"+&lcGlCondFileUse..cAcctCode+"' AND CGLREF = '' AND Tran_type IN ('IN','VI') AND"+;
               "  CATG_KEY IN ('001','003','004','005','014','029','030')"+;
               IIF(!EMPTY(ldStrtDate) AND !EMPTY(ldEndDate)," AND TRAN_DATE BETWEEN ?ldStrtDate and ?ldEndDate",""),'GLTRNDT',.F.,'GLTRNDT_A')
               
    SELECT 'GLTRNDT_A'
    LOCATE
    IF !EOF()   
      SCAN
        IF !gfSeek(SUBSTR(GLTRNDT_A.cTran_no,1,6),'INVHDR','INVHDR')
          LOOP 
        ENDIF
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
        IF TYPE('lcXMLFileName') <> 'C'
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
           WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
            Invhdr.Invoice NOWAIT
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
        ELSE
          loProgress.Description =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
					              Invhdr.Invoice
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        ENDIF
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
        SELECT INVHDR
        SCATTER MEMVAR MEMO
        m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
	    	m.cstname=CUSTOMER.btname
		    IF INVHDR.STATUS <> 'V'
          lcSeekExpr = GLTRNDT_A.CTRAN_NO+'IN'
          =gfSeek(lcSeekExpr ,'GLTRNDT','GLTRNDTNO')
          m.nglAmount = GLTRNDT.nglAmount 
          STORE 0 TO  m.shipamt , m.discount, m.freight, m.tax_amt ,  m.npstamt, m.nhstamt, m.Totalchg, m.vshipamt, m.vfreight, m.vTotalchg, m.vdiscount,  m.vtax_amt,m.nvhstamt, m.nvpstamt,m.insur,m.cod,m.vinsur ,m.vcod 
          SELECT GLTRNDT
          SCAN REST WHILE CTRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY =lcSeekExpr  
            DO CASE 
              CASE CATG_KEY ='001'
                m.Totalchg =  ABS(gltrndt.nglamount)
              CASE CATG_KEY ='003'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.shipamt  =  ABS(gltrndt.nglamount)
                 m.shipamt  = m.shipamt +   ABS(gltrndt.nglamount)
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='005'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.discount  = -1*gltrndt.nglamount
                m.discount  = m.discount +  -1*gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='004'
                m.freight = ABS(gltrndt.nglamount)
              CASE CATG_KEY ='014'
                m.tax_amt = ABS(gltrndt.nglamount)
              CASE CATG_KEY ='029'
                m.npstamt= ABS(gltrndt.nglamount)
              CASE CATG_KEY ='030'
                m.nhstamt= ABS(gltrndt.nglamount)
            ENDCASE 
          ENDSCAN 
        ELSE  
          lcSeekExpr = GLTRNDT_A.CTRAN_NO+'VI'
          =gfSeek(lcSeekExpr ,'GLTRNDT','GLTRNDTNO')
          m.nglAmount = GLTRNDT.nglAmount 
          STORE 0 TO  m.shipamt , m.discount, m.freight, m.tax_amt ,  m.npstamt, m.nhstamt, m.Totalchg, m.vshipamt, m.vfreight, m.vTotalchg, m.vdiscount,  m.vtax_amt,m.nvhstamt, m.nvpstamt,m.insur,m.cod,m.vinsur ,m.vcod 
          SELECT GLTRNDT
          SCAN REST WHILE CTRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY =lcSeekExpr  
            DO CASE 
              CASE CATG_KEY ='001'
                m.vTotalchg =  ABS(gltrndt.nglamount)
              CASE CATG_KEY ='003'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.vshipamt  =  gltrndt.nglamount
                m.vshipamt  = m.vshipamt +   gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='005'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.vdiscount  = gltrndt.nglamount
                m.vdiscount  = m.vdiscount +  gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='004'
                m.vfreight = gltrndt.nglamount
              CASE CATG_KEY ='014'
                m.vtax_amt = gltrndt.nglamount
              CASE CATG_KEY ='029'
                m.nvpstamt= gltrndt.nglamount
              CASE CATG_KEY ='030'
                m.nvhstamt= gltrndt.nglamount
            ENDCASE 
          ENDSCAN 
        ENDIF   
    		IF !EMPTY(ldStrtDate) AND !EMPTY(ldEndDate) AND STATUS = 'V' AND !EVALUATE(lcVoidExpr) 
          m.vtotalchg = 0
    		ENDIF
    		SELECT INVHDR
        *Add the code that use the posted date.[START]
        IF llVoidOnly OR (!EMPTY(ldStrtDate) AND !EMPTY(ldEndDate) AND STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDate,ldEndDate))
          = lfNegValue() && Negative void values.
        ENDIF
		    *Add the code that use the posted date.[END]
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDSCAN        
    ENDIF  
  ENDSCAN
ELSE
    =gfSqlRun("Select DISTINCT cTRAN_NO FROM GLTRNDT WHERE Tran_type IN ('IN','VI') AND CGLREF = '' AND"+;
               "  CATG_KEY IN ('001','003','004','005','014','029','030')"+;
               IIF(!EMPTY(ldStrtDate) AND !EMPTY(ldEndDate)," AND TRAN_DATE BETWEEN ?ldStrtDate and ?ldEndDate",""),'GLTRNDT',.F.,'GLTRNDT_A')
               
    SELECT 'GLTRNDT_A'
    LOCATE
    IF !EOF()   
      SCAN
        IF !gfSeek(SUBSTR(GLTRNDT_A.cTran_no,1,6),'INVHDR','INVHDR')
          LOOP
        ENDIF
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
        IF TYPE('lcXMLFileName') <> 'C'
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
          WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
          Invhdr.Invoice NOWAIT
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[Start]
        ELSE
          loProgress.Description = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_ColDataMsg,oAriaApplication.GetHeaderText("LANG_Arsjour_ColDataMsg",AHEADERFILE)) +;
						           Invhdr.Invoice
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
        ENDIF
        *E612429,1 MMT 07/05/2021 Convert Sales Journal Report to work from the Request Builder[End]
        
        SELECT INVHDR
        SCATTER MEMVAR MEMO
        m.cTempKey = PADR(CUSTOMER.CADDRESS6,20)+PADR(CUSTOMER.CCONT_CODE,6) + PADR(CUSTOMER.CADDRESS4,30) + IIF(lcRpSortBy = 'G' AND !EMPTY(INVHDR.STORE) , lfRegion() , PADR(CUSTOMER.REGION,6))
        m.cstname=CUSTOMER.btname
        IF INVHDR.STATUS <> 'V'
          lcSeekExpr = GLTRNDT_A.CTRAN_NO+'IN'
          =gfSeek(lcSeekExpr ,'GLTRNDT','GLTRNDTNO')
          m.nglAmount = GLTRNDT.nglAmount 
          STORE 0 TO  m.shipamt , m.discount, m.freight, m.tax_amt ,  m.npstamt, m.nhstamt, m.Totalchg, m.vshipamt, m.vfreight, m.vTotalchg, m.vdiscount,  m.vtax_amt,m.nvhstamt, m.nvpstamt,m.insur,m.cod,m.vinsur ,m.vcod 
          SELECT GLTRNDT
          SCAN REST WHILE CTRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY =lcSeekExpr  
            DO CASE 
              CASE CATG_KEY ='001'
                m.Totalchg =  ABS(gltrndt.nglamount)
              CASE CATG_KEY ='003'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.shipamt  =  ABS(gltrndt.nglamount)
                m.shipamt  = m.shipamt +  ABS(gltrndt.nglamount)
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='005'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.discount  = -1*gltrndt.nglamount
                m.discount  = m.discount +  -1*gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='004'
                m.freight = ABS(gltrndt.nglamount)
              CASE CATG_KEY ='014'
                m.tax_amt = ABS(gltrndt.nglamount)
              CASE CATG_KEY ='029'
                m.npstamt= ABS(gltrndt.nglamount)
              CASE CATG_KEY ='030'
                m.nhstamt= ABS(gltrndt.nglamount)
            ENDCASE 
          ENDSCAN 
        ELSE  
          lcSeekExpr = GLTRNDT_A.CTRAN_NO+'VI'
          =gfSeek(lcSeekExpr ,'GLTRNDT','GLTRNDTNO')
          m.nglAmount = GLTRNDT.nglAmount 
          STORE 0 TO  m.shipamt , m.discount, m.freight, m.tax_amt ,  m.npstamt, m.nhstamt, m.Totalchg, m.vshipamt, m.vfreight, m.vTotalchg, m.vdiscount,  m.vtax_amt,m.nvhstamt, m.nvpstamt,m.insur,m.cod,m.vinsur ,m.vcod 
          SELECT GLTRNDT
          SCAN REST WHILE CTRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY =lcSeekExpr  
            DO CASE 
              CASE CATG_KEY ='001'
                m.vTotalchg =  ABS(gltrndt.nglamount)
              CASE CATG_KEY ='003'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.vshipamt  =  gltrndt.nglamount
                m.vshipamt  = m.vshipamt +   gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][End]
              CASE CATG_KEY ='005'
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
                *m.vdiscount  = gltrndt.nglamount
                m.vdiscount  = m.vdiscount +  gltrndt.nglamount
                *B612241,1 MMT 10/25/2020 Incorrect Shipped amount displayed in case of 'Reconcile with GL' option = Yes[T20201009.0003][Start]
              CASE CATG_KEY ='004'
                m.vfreight = gltrndt.nglamount
              CASE CATG_KEY ='014'
                m.vtax_amt = gltrndt.nglamount
              CASE CATG_KEY ='029'
                m.nvpstamt= gltrndt.nglamount
              CASE CATG_KEY ='030'
                m.nvhstamt= gltrndt.nglamount
            ENDCASE 
          ENDSCAN 
        ENDIF   
        IF  !EMPTY(ldStrtDate) AND !EMPTY(ldEndDate) AND STATUS = 'V' AND !EVALUATE(lcVoidExpr) 
          m.vtotalchg = 0
        ENDIF
        SELECT INVHDR
        *Add the code that use the posted date.[START]
        IF llVoidOnly OR (!EMPTY(ldStrtDate) AND !EMPTY(ldEndDate) AND STATUS = 'V' AND BETWEEN(VDATE,ldStrtDate,ldEndDate) AND;
                                      !BETWEEN(DPOSTDATE,ldStrtDate,ldEndDate))
          = lfNegValue() && Negative void values.
        ENDIF
        *Add the code that use the posted date.[END]
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDSCAN        
    ENDIF      
ENDIF
*!*************************************************************
*! Name      : lfSetGLMsk
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/18/2020
*! Purpose   : Set GL Mask
*!*************************************************************
FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',oAriaApplication.ActiveCompanyID)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',oAriaApplication.ActiveCompanyID)))
IF llGlLink
  IF lcLinkWith $ "AO"
    USE (oAriaApplication.SysPath + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(oAriaApplication.ActiveCompanyID, "CompFile")
    lcPthToUse = oAriaApplication.DataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    ENDIF
    USE IN CompFile
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
 
    lcGlFld    = "cAcctCode: 25 :H='"+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))+;
    "',"+"cAccNlDes:65:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Description,oAriaApplication.GetHeaderText("LANG_Arsjour_Description",AHEADERFILE))+"'"
 
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , oAriaApplication.ActiveCompanyID))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', oAriaApplication.ActiveCompanyID))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
    IF !USED("lcLinkChar")
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcGlFld = "glAcnt: 25 :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Account,oAriaApplication.GetHeaderText("LANG_Arsjour_Account",AHEADERFILE))+;
              "',"+"glDesc:53:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Arsjour_Description,oAriaApplication.GetHeaderText("LANG_Arsjour_Description",AHEADERFILE))+"'"


    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
ENDIF
SELECT (lnAlias)
*E612018,1 MMT 02/18/2020 Add new option to display new report collects data from new GL tables [GL Enhacement][End]
*E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][Start]
FUNCTION lfGetShipToAddress
LPARAMETERS lcOrderNum
IF !USED('ORDHDR')
  =gfOpenTable('ORDHDR','ORDHDR','SH')
ENDIF

=gfSeek('O'+lcOrderNum,'ORDHDR','ORDHDR')
 IF Ordhdr.Alt_ShpTo
    lcShpTName  = Ordhdr.STName
    laShipTo[1] = Ordhdr.cAddress1
    laShipTo[2] = Ordhdr.cAddress2
    laShipTo[3] = Ordhdr.cAddress3
    laShipTo[4] = Ordhdr.cAddress4
    laShipTo[5] = Ordhdr.cAddress5
  ELSE    && Else

    lnCUSRec = 0
    IF !EMPTY(CUSTOMER.STORE) AND !EMPTY(CUSTOMER.dist_ctr) AND !Ordhdr.lStrDirct
      lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
      =SEEK('S'+CUSTOMER.Account+CUSTOMER.dist_ctr)
      lcDCCode    = CUSTOMER.STORE
    ELSE
      lcDCCode = ''
    ENDIF

    lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
      IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STName , CUSTOMER.DBA))

    laShipTo[1] = IIF(INVHDR.STORE = "********" , "" ,Customer.cAddress1) &&gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , Customer.cAddress2)
    laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , Customer.cAddress3)
    laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , Customer.cAddress4)
    laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , Customer.cAddress5)
	laShipTo[6] = IIF(INVHDR.STORE = "********" , "" , Customer.cAddress6)
    IF lnCUSRec <> 0
      GOTO lnCUSRec IN CUSTOMER
    ENDIF
  ENDIF    && End of IF

*  =lfAdrShift('laShipTo')
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
*E304201,1 MMT 07/24/2024 Add Ship To Addresses to the Export to Excel List[T-ERP-20240709.0003][End]