*:***************************************************************************
*: Program file  : SOORDHD
*: Program desc. : Order Header Report
*: For Report    : SOORDHD.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Heba Mohamed Amin (HMA)
*: Date          : 12/04/2003
*: Reference     : N037288,1
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfModalGen,gfPhoneTem,gfTempName,gfMover,gfGetAdr.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORDHD
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*! B037288,1 HMA 08/18/2004 Change the group expression in case of sort by periority.
*! B039069,1 MMT 02/22/2005 report peformance Improvement
*! HMA 05/16/2005 delete the selected records from Order # Line
*       in the Option grid if the order status or order type has changed.
*! B128322,1 SMM 06/02/2005 When saving settings the status seems not to be saved
*! E128374,1 HFK 06/05/2005 convert the in list containing order types to a mover and adding
*!                          EDI temporary orders
*! B129280,1 ASM 10/09/2005 Open amount wraps around to next line
*!                          EDI temporary orders
*! N000535,1 MMT 19/09/2006 convert to graphics
*! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[T20070426.0016]
*! B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly [T20070824.0037]
*! B608618,1 MMT 07/20/2008 Fix bug of wrong amounts when export report to EXCEL[T20071219.0002]
*! B608928,1 HES 07/07/2009 QUESTION ABOUT CANCELLATION REPORTS [T20090611.0003]
*! B609356,1 SMA 07/21/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[T20090617.0124]
*! B610022,1 HIA 07/25/2012 Add customer name to exported to excel file [T20120719.0002]
*! B610060,1 MMT 08/27/2012 Order Header does not export Ship To address to Excel[T20120817.0001]
*! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[T20120918.0006]
*! B610255,1 HIA 02/21/2013 Some account show the name and some do not, please see attached a copy of the order header [T20130213.0005]
*! B610320,1 HIA 04/29/13 T20130320.0001 - ARIA4XP - SO - ORDER HEADER REPORT
*!***************************************************************************
*----------------------- Report Code Begin -----------------------------

*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID


IF TYPE('lcXMLFileName') = 'C'
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  loEnvironment.ConnectionsRefresh()
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)

  LOCAL lcCurrentProcedure
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  SET DEFAULT TO &lcCurrentProcedure.
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment
  SET DEFAULT TO &lcCurrentProcedure.
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  oAriaEnvironment.REPORT.gcAct_Appl = 'SO'

  PUBLIC gcAct_Appl
  gcAct_Appl = 'SO'
  oAriaEnvironment.activeModuleID = 'SO'
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  oAriaEnvironment.REPORT.cCROrientation = 'P'
  =gfOpenFile('NOTEPAD','NOTEPAD')
  =gfOpenFile('ORDHDR','ORDHDR')
  =gfOpenFile('CUSTOMER','CUSTOMER')
  =gfOpenFile('SALESREP','SALESREP')
  lfAsignVar  (.T.)
  DECLARE  laShipTo[5,1]
  STORE ''         TO laShipTo
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

#INCLUDE R:\Aria4xp\reports\so\soordhd.h

lcStTime   = TIME()                     && Variable to hold the start Time.
lcLastRep1 = ''
lnCntRep   = 0
lnGrandAcc = 0

*Use Full Multi currency format capability [Begin]
STORE "" TO lcHedTitle,lcLineCurr
*HMA,uncomment the Commented code becaue we use these variables in report [Begin]
*!*	llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
*!*	                          ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
*!*	                            (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))

*!*	llCurInGrp = !llPrintTot
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
    ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
    (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))

  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
ELSE
  llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
    ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
    (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
llCurInGrp = !llPrintTot
*HMA,uncomment the Commented code becaue we use these variables in report [End]

=lfGetRepVr() && Get Report Variables...

*Use Full Multi currency format capability [End  ]

*-- If user change report critria, Collect report data.
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
*IF loOGScroll.llOGFltCh     && Variable to detect OG filter changes
IF IIF(TYPE('lcXMLFileName') <> 'C',loOGScroll.llOGFltCh,.T.)
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  *-B127856, HFK 05/10/2005 Add summary format to report
  *!*    llFilCodes = (lcRpFormat = "S")
  llFilCodes = (lcRpFormat = "S") .OR. (lcRpFormat = "H")
  *-B127856, HFK 05/10/2005 Add summary format to report
  *-- Unrise all Critria variables.
  llChSortBy = .F.  && We Fill index field with its corresponding data in data collection.

  *-- Create scan expression from selected filter. [begin]
  lcMultiExp = IIF(llRpMulti,[MULTI = 'Y'],'')
  lcReOrdExp = IIF(llRpReOrd,[CREORDER = 'Y'],'')
  lcBulkExp  = IIF(llRpBulk ,[BULK = 'Y'],'')


  *! B128322,1 SMM 06/02/2005 When saving settings the status seems not to be saved [START]
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[END]
    LOCAL lnCount
    IF !EMPTY(laRpTarget[1])
      *^*B608928,1 HES 07/07/2009 QUESTION ABOUT CANCELLATION REPORTS
      lcRpStatus = ' '
      *^*B608928,1 HES 07/07/2009 QUESTION ABOUT CANCELLATION REPORTS
      FOR lnCount = 1 TO ALEN(laRpTarget,1)
        lcRpStatus = lcRpStatus + IIF(laRpTarget[lnCount] = LANG_Soordhd_Bid,'B',;
          IIF(laRpTarget[lnCount] = LANG_Soordhd_Open,'O',;
          IIF(laRpTarget[lnCount] = LANG_Soordhd_Hold,'H',;
          IIF(laRpTarget[lnCount] = LANG_Soordhd_Completed,'C',;
          IIF(laRpTarget[lnCount] = LANG_Soordhd_Canceled,'X','')))))
      ENDFOR
    ENDIF
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
  *! B128322,1 SMM 06/02/2005 When saving settings the status seems not to be saved [END]

  lcStatExp  = IIF(EMPTY(lcRpStatus),'',[STATUS $ lcRpStatus])

  lcHiddExp  = IIF(EMPTY(lcMultiExp),'',[ AND ] + lcMultiExp) +;
    IIF(EMPTY(lcReOrdExp),'',[ AND ] + lcReOrdExp) +;
    IIF(EMPTY(lcBulkExp),'',[ AND ] + lcBulkExp)   +;
    IIF(EMPTY(lcStatExp),'',[ AND ] + lcStatExp)

  lcRpExp    =  IIF('.T.' $ lcRpExp,[(CORDTYPE+ORDER = 'O' OR CORDTYPE+ORDER = 'T')],;
    [(CORDTYPE+ORDER ='O' OR CORDTYPE+ORDER ='T')  AND ] + ;
    lcRpExp) + IIF(EMPTY(lcHiddExp),'', lcHiddExp)

  lcRpExp    = STRTRAN(lcRpExp,'ORDHDR.','')

  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [begin]
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
    IF !EMPTY(loOGScroll.laOGFxFlt[lnRepPos,6])
      lcRepFile  = loOGScroll.laOGFxFlt[lnRepPos,6]
      lnRepStPos = ATC('SEEK(REP1',lcRpExp)
      lnRepLen   = ATC(lcRepFile,lcRpExp) + 10 - lnRepStPos
      lcRepCond1 = SUBSTR(lcRpExp,lnRepStPos,lnRepLen)
      lcRepCond2 = STRTRAN(lcRepCond1,'REP1','REP2')
      lcRepCond  = '(' + lcRepCond1 + ' OR ' + lcRepCond2 + ')'
      lcRpExp    = STRTRAN(lcRpExp,lcRepCond1,lcRepCond)
    ENDIF
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  ELSE
    IF !EMPTY(laOGFxFlt[lnRepPos,6])
      lcRepFile  = laOGFxFlt[lnRepPos,6]
      lnRepStPos = ATC('SEEK(REP1',lcRpExp)
      lnRepLen   = ATC(lcRepFile,lcRpExp) + 10 - lnRepStPos
      lcRepCond1 = SUBSTR(lcRpExp,lnRepStPos,lnRepLen)
      lcRepCond2 = STRTRAN(lcRepCond1,'REP1','REP2')
      lcRepCond  = '(' + lcRepCond1 + ' OR ' + lcRepCond2 + ')'
      lcRpExp    = STRTRAN(lcRpExp,lcRepCond1,lcRepCond)
    ENDIF
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [end]

  *-- Create scan expression from selected filter. [end]

  *-- if you have previous data clear WorkFile then recreate it. [begin]
  IF !USED(WorkFile) OR RECCOUNT(WorkFile) > 0
    IF USED(WorkFile)
      USE IN (WorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear WorkFile then recreate it. [end]
  *-- Fill temporary file with report data [begin]
  SELECT ORDHDR
  SET ORDER TO
  SET RELATION TO 'M' + Account INTO CUSTOMER


  *Add Field LEDIORDER to the filter expression [Begin.]
  *- E128374,1 HFK 06/05/2005 [Start]
  lcRpEdiFlt = ""
  IF 'EB' $ oAriaApplication.CompanyInstalledModules
    DO CASE
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
      lcRpEdiFlt = [!OrdHdr.lEdiOrder]
    CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      lcRpEdiFlt = [(OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T')]
    CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
      lcRpEdiFlt = [(OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O')]
    CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      lcRpEdiFlt = [(!OrdHdr.lEdiOrder  .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T'))]
    CASE ("T" $ lcRpType .AND. "R" $ lcRpType .AND. !("O" $ lcRpType))
      lcRpEdiFlt = [OrdHdr.lEdiOrder]
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType )
      lcRpEdiFlt = [(!OrdHdr.lEdiOrder .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O'))]
    ENDCASE
  ENDIF
  *- E128374,1 HFK 06/05/2005 [End]
  IF !EMPTY(lcRpEdiFlt)
    IF !EMPTY(lcRpExp)
      lcRpExp = lcRpExp + [ AND ]
    ENDIF
    lcRpExp = lcRpExp + lcRpEdiFlt
  ENDIF
  *Add Field LEDIORDER to the filter expression [End.]

  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF TYPE('lcXMLFileName') = 'C'
    loProgress.DESCRIPTION = LANG_Soordhd_CollectData
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    SELECT ORDHDR
    COUNT FOR &lcRpExp TO lnDataCnt
    LOCATE
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

  SCAN FOR &lcRpExp
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
    IF TYPE('lcXMLFileName') = 'C'
      lnPerCent = RECNO()/lnDataCnt
      IF MOD(RECNO(),CEILING(lnDataCnt/ 10)) = 0
        loProgress.Percent = lnPerCent * 0.9
        loProgress.DESCRIPTION = LANG_Soordhd_CollectDataOrder+ORDER
        loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      ENDIF
    ENDIF
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

    SCATTER MEMVAR MEMO
    =lfFillData()
  ENDSCAN
  SET ORDER TO ORDHDR
  SET RELATION OFF INTO CUSTOMER
  *-- Fill temporary file with report data [end ]
ENDIF  && end If user change report critria, Collect report data.

IF RECCOUNT(WorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
    =gfModalGen('TRM00052B40011','ALERT')
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  RETURN
ENDIF

lcShpTName = ''
lcPhonePic = gfPhoneTem()

SELECT (WorkFile)
*-- if not first run for the same filter and start run is short format then fill codes. [Begin]
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  IF !loOGScroll.llOGFltCh AND llFilCodes AND (lcRpFormat = "L")
    llFilCodes = .F.
    SET ORDER TO
    SCAN
      =lfFillData(.T.)
      IF llChSortBy
        REPLACE cTempKey WITH EVALUATE(lcIndexTg)
      ENDIF
    ENDSCAN
    llChSortBy = .F.
    SET ORDER TO (WorkFile)
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
*-- if not first run for the same filter and start run is short format then fill codes. [End  ]

IF llChSortBy
  REPLACE ALL cTempKey WITH EVALUATE(lcIndexTg)
  llChSortBy = .F.
ENDIF

*-- Temp File used to Calculate No. of accounts [begin]
*-- in the current sales rep. and in grand totals.
CustFile = gfTempName()


*! B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[Start]
*CREATE CURSOR (CustFile) (ACCOUNT C(5) , SALESREP C(3), cCurrCode C(3) ,;
cShip1 C(30),cShip2 C(30),cShip3 C(30),cShip4 C(30),cShip5 C(30) ,;
STORE C(8))
lfvSortBy()
CREATE CURSOR (CustFile) (ACCOUNT C(5) , SALESREP C(3), cCurrCode C(3) ,;
  cShip1 C(30),cShip2 C(30),cShip3 C(30),cShip4 C(30),cShip5 C(30) ,;
  STORE C(8),llCount L(1))
*! B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[End]

SELECT (CustFile)
ZAP
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON ACCOUNT+STORE TAG (CustFile) OF (oAriaApplication.WorkDir+CustFile+'.CDX')
INDEX ON ACCOUNT+STORE TAG (CustFile)
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
IF lcRpSortBy = 'S'        && WHEN SORTING BY SALESREP
  CUSTFL1 = gfTempName()
  IF llMultCurr
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
    *INDEX ON SALESREP+CCURRCODE+ACCOUNT TAG (CUSTFL1) OF (oAriaApplication.WorkDir+CustFile+'.CDX')
    INDEX ON SALESREP+CCURRCODE+ACCOUNT TAG (CUSTFL1)
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  ELSE
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
    *INDEX ON SALESREP+ACCOUNT TAG (CUSTFL1) OF (oAriaApplication.WorkDir+CustFile+'.CDX')
    INDEX ON SALESREP+ACCOUNT TAG (CUSTFL1)
    *B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  ENDIF
ENDIF
SET ORDER TO TAG (CustFile)
*-- Temp File used to Calculate No. of accounts [end]
*---------

llSameCurr = .T.
SELECT (WorkFile)
LOCATE
lcFirstCurr = CCURRCODE
IF RECCOUNT() > 1
  SCAN
    IF lcFirstCurr <> CCURRCODE
      llSameCurr = .F.
      EXIT
    ENDIF
  ENDSCAN
ENDIF
*---------
SELECT (WorkFile)
GO TOP

*-- Set relation between temp. file and customer file.
SET RELATION TO IIF(EMPTY(STORE) , 'M' + Account,;
  'S' + Account + STORE) INTO CUSTOMER  && To customer file.
SET RELATION TO 'B'+ORDER INTO NOTEPAD ADDITIVE

lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  WAIT WINDOW LANG_Soordhd_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(WorkFile))) + LANG_Soordhd_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Soordhd_SecondMsg  NOWAIT
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
ENDIF
IF TYPE('lcXMLFileName') = 'C'
  loOGScroll   = oAriaEnvironment.REPORT
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

*! B129280,1 ASM 10/09/2005 Open amount wraps around to next line [Start]
loogscroll.nfontsize = 7
*! B129280,1 ASM 10/09/2005 Open amount wraps around to next line [End]

*! N000535,1 MMT 19/09/2006 convert to graphics [Start]
loOgScroll.cCRorientation = 'P'
*! N000535,1 MMT 19/09/2006 convert to graphics [End]

*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  DO gfDispRe WITH EVAL('lcRpForm')
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
ELSE
  loProgress.Percent = 0.9
  loProgress.DESCRIPTION = LANG_Soordhd_PrintReport
  loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

  IF loProxy.GetRequest(lcRequestID, ClientID).STATUS = 3
    loOGScroll   = oAriaEnvironment.REPORT
    oAriaEnvironment.REPORT.OGLastForm = lcRpForm
    oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    loProgress.Percent = 1.0
    loProgress.DESCRIPTION = LANG_Soordhd_PrintReport
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
  ENDIF
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

*--B99999,1,[MMT],01/27/2005,Fixing problem of error after exporting to HTML
IF USED(CustFile)
  USE IN (CustFile)
ENDIF
*--B99999,1,[MMT],01/27/2005,Fixing problem of error after exporting to HTML
RETURN
*-- end of report code.

*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : When function of OG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*
FUNCTION lfwRepWhen

lnRepPos  = lfItmPos('ORDHDR.REP1')
lnOrdPos  = lfItmPos('ORDHDR.ORDER')
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ENDIF

IF EMPTY(laRpSource)
  *Add Feature to allow printing the Bid status.
  DECLARE laRpSource[5],laRpTarget[5]  && Redeclare the source and target arrays.
  *Redeclare the source and arrays with new status Bid.[Begin]
  STORE LANG_Soordhd_Bid       TO laRpSource[1],laRpTarget[1]
  STORE LANG_Soordhd_Open      TO laRpSource[2],laRpTarget[2]
  STORE LANG_Soordhd_Hold      TO laRpSource[3],laRpTarget[3]
  STORE LANG_Soordhd_Canceled  TO laRpSource[4],laRpTarget[4]
  STORE LANG_Soordhd_Completed TO laRpSource[5],laRpTarget[5]
  *Redeclare the source and arrays with new status Bid. [End]

ENDIF
*- E128374,1 HFK 06/05/2005 [Start]
IF EMPTY(laRpTypSrc)
  DECLARE laRpTypSrc[3],laRpTypDst[3]  && Redeclare the source and target arrays.
  STORE "Orders"               TO laRpTypSrc[1],laRpTypDst[1]
  STORE "EDI Temporary Orders" TO laRpTypSrc[2],laRpTypDst[2]
  STORE "EDI Received Orders"  TO laRpTypSrc[3],laRpTypDst[3]
ENDIF
*- E128374,1 HFK 06/05/2005 [End]

IF !USED(WorkFile)

  DECLARE  laShipTo[5,1],laCodDesc[7,3]
  STORE ''         TO lcRpStatus,laShipTo,laCodDesc
  laCodDesc[1,2] = LANG_Soordhd_Season
  laCodDesc[2,2] = LANG_Soordhd_Cdivision
  laCodDesc[3,2] = LANG_Soordhd_CTermCode
  laCodDesc[4,2] = LANG_Soordhd_Shipvia
  laCodDesc[5,2] = LANG_Soordhd_SpecialInst
  laCodDesc[6,2] = LANG_Soordhd_Region
  laCodDesc[7,2] = LANG_Soordhd_Decl_Code
  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT ORDHDR
  lnFildLen = AFIELD(laTempStru)

  *-- Add Code fields.
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  *DIMENSION laTempStru[lnFildLen + 10, 18]
  DIMENSION laTempStru[lnFildLen + 11, 18]
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]
  laTempStru[lnFildLen + 1, 1] = 'cSeaDesc'
  laTempStru[lnFildLen + 1, 2] = 'C'
  laTempStru[lnFildLen + 1, 3] = 37
  laTempStru[lnFildLen + 1, 4] = 0

  laTempStru[lnFildLen + 2, 1] = 'cDivDesc'
  laTempStru[lnFildLen + 2, 2] = 'C'
  laTempStru[lnFildLen + 2, 3] = 37
  laTempStru[lnFildLen + 2, 4] = 0

  laTempStru[lnFildLen + 3, 1] = 'cTrmDesc'
  laTempStru[lnFildLen + 3, 2] = 'C'
  laTempStru[lnFildLen + 3, 3] = 37
  laTempStru[lnFildLen + 3, 4] = 0

  laTempStru[lnFildLen + 4, 1] = 'cViaDesc'
  laTempStru[lnFildLen + 4, 2] = 'C'
  laTempStru[lnFildLen + 4, 3] = 37
  laTempStru[lnFildLen + 4, 4] = 0

  laTempStru[lnFildLen + 5, 1] = 'cSpcDesc'
  laTempStru[lnFildLen + 5, 2] = 'C'
  laTempStru[lnFildLen + 5, 3] = 37
  laTempStru[lnFildLen + 5, 4] = 0

  laTempStru[lnFildLen + 6, 1] = 'cRegDesc'
  laTempStru[lnFildLen + 6, 2] = 'C'
  laTempStru[lnFildLen + 6, 3] = 37
  laTempStru[lnFildLen + 6, 4] = 0

  laTempStru[lnFildLen + 7, 1] = 'cCanDesc'
  laTempStru[lnFildLen + 7, 2] = 'C'
  laTempStru[lnFildLen + 7, 3] = 37
  laTempStru[lnFildLen + 7, 4] = 0

  laTempStru[lnFildLen + 8, 1] = 'cCurDesc'
  laTempStru[lnFildLen + 8, 2] = 'C'
  laTempStru[lnFildLen + 8, 3] = 37
  laTempStru[lnFildLen + 8, 4] = 0

  laTempStru[lnFildLen + 9, 1] = 'cDecl_code'
  laTempStru[lnFildLen + 9, 2] = 'C'
  laTempStru[lnFildLen + 9, 3] = 37
  laTempStru[lnFildLen + 9, 4] = 0

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[lnFildLen + 10, 1] = 'cTempKey'
  laTempStru[lnFildLen + 10, 2] = 'C'
  laTempStru[lnFildLen + 10, 3] = 26
  laTempStru[lnFildLen + 10, 4] = 0

  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  laTempStru[lnFildLen + 11, 1] = 'BtName'
  laTempStru[lnFildLen + 11, 2] = 'C'
  laTempStru[lnFildLen + 11, 3] = 30
  laTempStru[lnFildLen + 11, 4] = 0
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]

  FOR lnInc=7 TO 16
    *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
    *FOR lnRaw=1 TO 10
    FOR lnRaw=1 TO 11
      *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]
      STORE SPACE(1) TO laTempStru[lnFildLen + lnRaw,lnInc]
    ENDFOR
  ENDFOR
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  *FOR lnRaw=1 TO 10
  FOR lnRaw=1 TO 11
    *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]
    STORE 0  TO laTempStru[lnFildLen + lnRaw,17], laTempStru[lnFildLen + lnRaw,18]
  ENDFOR

  SET ORDER TO ORDHDR   IN ORDHDR
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO SALESREP IN SALESREP

  =lfWorkFile()

  =lfvSortBy()   &&Initially Rise Currency flags

ENDIF

*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
LOCAL lnCount
IF !EMPTY(laRpTarget[1])
  lcRpStatus = ' '
  FOR lnCount = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnCount] = LANG_Soordhd_Bid,'B',;
      IIF(laRpTarget[lnCount] = LANG_Soordhd_Open,'O',;
      IIF(laRpTarget[lnCount] = LANG_Soordhd_Hold,'H',;
      IIF(laRpTarget[lnCount] = LANG_Soordhd_Completed,'C',;
      IIF(laRpTarget[lnCount] = LANG_Soordhd_Canceled,'X','')))))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*!***************************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
* B123663,1 SMM Change gfMover to lfOGMover
*= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,LANG_Soordhd_SelectOrdStat,.T.,'')  && call mover function.
* B123663,1 SMM End

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = LANG_Soordhd_Bid,'B',;
      IIF(laRpTarget[lnI] = LANG_Soordhd_Open,'O',;
      IIF(laRpTarget[lnI] = LANG_Soordhd_Hold,'H',;
      IIF(laRpTarget[lnI] = LANG_Soordhd_Completed,'C',;
      IIF(laRpTarget[lnI] = LANG_Soordhd_Canceled,'X','')))))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*!*	*-- if length of current selected status differ from previous length
*!*	IF LEN(lcOldStat) != LEN(lcRpStatus)
*!*	  llOGFltCh  = .T.
*!*	  llClearOrd = .T.
*!*	ELSE  && else if length of current selected status equal previous length
*!*	  *-- loop to check if it's the same selected status or not.
*!*	  FOR lnJ = 1 TO LEN(lcOldStat)
*!*	    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
*!*	    IF !(lcCurrChr $ lcRpStatus)
*!*	      llOGFltCh  = .T.
*!*	      llClearOrd = .T.
*!*	      EXIT
*!*	    ENDIF
*!*	  ENDFOR  && end loop to check if it's the same selected status or not.
*!*	ENDIF
*!*	*-- Compare current selected status with old value  [end]

*!*	*-- Activate suppress expression [begin]
*!*	llCancNow = ('X' $ lcRpStatus) OR EMPTY(lcRpStatus)
*!*	llCancPrv = ('X' $ lcOldStat)  OR EMPTY(lcOldStat)
*!*	IF (llCancNow AND !llCancPrv) OR (!llCancNow AND llCancPrv)
*!*	  ClearRead()
*!*	ENDIF
*!*	*-- Activate suppress expression [end]

*HMA 05/16/2005 delete the selected records from Order # Line
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
  IF USED(lcOrderSel)
    SELECT(lcOrderSel)
    ZAP
  ENDIF
ENDIF

llClearOrd = .T.
*HMA 05/16/2005 delete the selected records from Order # Line
*in the Option grid if the order status or order type has changed. [End]

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
DO CASE
CASE lcParm = 'S'
  SELECT ORDHDR
  *!*	    DO CASE
  IF !EMPTY(lcRpStatus) .AND. LEN(lcRpStatus) <> 5
    SELECT ORDHDR
    *- E128374,1 HFK 06/05/2005 [Start]
    *- SET FILTER TO (ORDHDR.STATUS$lcRpStatus) ,;
    *- AND IIF(lcRpEdiPrn ='O',!ORDHDR.lEdiOrder,IIF(lcRpEdiPrn ='E',ORDHDR.lEdiOrder,.T.))
    DO CASE
    CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. "R" $ lcRpType ) .OR. EMPTY(lcRpType)
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus)
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. !lEdiOrder
    CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .AND. cOrdType = 'T')
    CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .AND. cOrdType = 'O')
    CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (!lEdiOrder .OR. (lEdiOrder .AND. cOrdType = 'T'))
    CASE ("T" $ lcRpType .AND. "R" $ lcRpType .AND. !("O" $ lcRpType))
      SET FILTER TO lEdiOrder .AND. (ORDHDR.STATUS$lcRpStatus)
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND."R" $ lcRpType )
      SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (!lEdiOrder .OR. (lEdiOrder .AND. cOrdType = 'O'))
    ENDCASE
    *- E128374,1 HFK 06/05/2005 [End]
    LOCATE
  ELSE
    SELECT ORDHDR
    *- E128374,1 HFK 06/05/2005 [Start]
    *-SET FILTER TO IIF(lcRpEdiPrn  ='O',!ORDHDR.lEdiOrder,IIF(lcRpEdiPrn  ='E',ORDHDR.lEdiOrder,.T.))
    DO CASE
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
      SET FILTER TO !lEdiOrder
    CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      SET FILTER TO (lEdiOrder .AND. cOrdType = 'T')
    CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
      SET FILTER TO (lEdiOrder .AND. cOrdType = 'O')
    CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
      SET FILTER TO (!lEdiOrder .OR. (lEdiOrder .AND. cOrdType = 'T'))
    CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND."R" $ lcRpType)
      SET FILTER TO lEdiOrder
    CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND."R" $ lcRpType )
      SET FILTER TO (!lEdiOrder .OR. (lEdiOrder .AND. cOrdType = 'O'))
    ENDCASE
    *- E128374,1 HFK 06/05/2005 [End]
    LOCATE
  ENDIF
  *--B99999,1 mmt fix bug of not ordering data in browser[Start]
  SET ORDER TO ORDHDR   && CORDTYPE+ORDER
  *--B99999,1 mmt fix bug of not ordering data in browser[End]
  *        LOCATE FOR CORDTYPE+ORDER = "O"

  *!*	      CASE lcRpEdiPrn = "C"
  *!*	        SET FILTER TO (CORDTYPE + ORDER = "C") AND (ORDHDR.STATUS$lcRpStatus) ,;
  *!*	        AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*	        IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*	        LOCATE FOR CORDTYPE+ORDER = "C"

  *!*	      CASE lcRpEdiPrn = "T"
  *!*	        SET FILTER TO (CORDTYPE + ORDER = "T") AND (ORDHDR.STATUS$lcRpStatus)  ,;
  *!*	        AND IIF(ORDHDR.cOrdType='T' .AND. ;
  *!*	        ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
  *!*	        IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
  *!*	        LOCATE FOR CORDTYPE+ORDER = "T"

  *!*	    ENDCASE

CASE lcParm = 'R'

  SELECT ORDHDR
  SET FILTER TO
ENDCASE
*-- End of lfsChOrder.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

DO CASE
CASE lcParm = 'S'
  GO TOP IN CUSTOMER
CASE lcParm = 'R'
  llClearAcc = .F.
ENDCASE

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
CASE lcParm = 'S'
  GO TOP IN SALESREP
CASE lcParm = 'R'
  llClearRep = .F.
ENDCASE

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Create work cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfWorkFile

*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') = 'C'
  DIMENSION laTempStru[1,18]
  laTempStru = ''
  SELECT ORDHDR
  lnFildLen = AFIELD(laTempStru)

  *-- Add Code fields.
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  *DIMENSION laTempStru[lnFildLen + 10, 18]
  DIMENSION laTempStru[lnFildLen + 11, 18]
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]
  laTempStru[lnFildLen + 1, 1] = 'cSeaDesc'
  laTempStru[lnFildLen + 1, 2] = 'C'
  laTempStru[lnFildLen + 1, 3] = 37
  laTempStru[lnFildLen + 1, 4] = 0

  laTempStru[lnFildLen + 2, 1] = 'cDivDesc'
  laTempStru[lnFildLen + 2, 2] = 'C'
  laTempStru[lnFildLen + 2, 3] = 37
  laTempStru[lnFildLen + 2, 4] = 0

  laTempStru[lnFildLen + 3, 1] = 'cTrmDesc'
  laTempStru[lnFildLen + 3, 2] = 'C'
  laTempStru[lnFildLen + 3, 3] = 37
  laTempStru[lnFildLen + 3, 4] = 0

  laTempStru[lnFildLen + 4, 1] = 'cViaDesc'
  laTempStru[lnFildLen + 4, 2] = 'C'
  laTempStru[lnFildLen + 4, 3] = 37
  laTempStru[lnFildLen + 4, 4] = 0

  laTempStru[lnFildLen + 5, 1] = 'cSpcDesc'
  laTempStru[lnFildLen + 5, 2] = 'C'
  laTempStru[lnFildLen + 5, 3] = 37
  laTempStru[lnFildLen + 5, 4] = 0

  laTempStru[lnFildLen + 6, 1] = 'cRegDesc'
  laTempStru[lnFildLen + 6, 2] = 'C'
  laTempStru[lnFildLen + 6, 3] = 37
  laTempStru[lnFildLen + 6, 4] = 0

  laTempStru[lnFildLen + 7, 1] = 'cCanDesc'
  laTempStru[lnFildLen + 7, 2] = 'C'
  laTempStru[lnFildLen + 7, 3] = 37
  laTempStru[lnFildLen + 7, 4] = 0

  laTempStru[lnFildLen + 8, 1] = 'cCurDesc'
  laTempStru[lnFildLen + 8, 2] = 'C'
  laTempStru[lnFildLen + 8, 3] = 37
  laTempStru[lnFildLen + 8, 4] = 0

  laTempStru[lnFildLen + 9, 1] = 'cDecl_code'
  laTempStru[lnFildLen + 9, 2] = 'C'
  laTempStru[lnFildLen + 9, 3] = 37
  laTempStru[lnFildLen + 9, 4] = 0

  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[lnFildLen + 10, 1] = 'cTempKey'
  laTempStru[lnFildLen + 10, 2] = 'C'
  laTempStru[lnFildLen + 10, 3] = 26
  laTempStru[lnFildLen + 10, 4] = 0

  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  laTempStru[lnFildLen + 11, 1] = 'BtName'
  laTempStru[lnFildLen + 11, 2] = 'C'
  laTempStru[lnFildLen + 11, 3] = 30
  laTempStru[lnFildLen + 11, 4] = 0
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[ENd]

  FOR lnInc=7 TO 16
    *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
    *FOR lnRaw=1 TO 10
    FOR lnRaw=1 TO 11
      *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[End]
      STORE SPACE(1) TO laTempStru[lnFildLen + lnRaw,lnInc]
    ENDFOR
  ENDFOR
  *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
  *FOR lnRaw=1 TO 10
  FOR lnRaw=1 TO 11
    *! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[END]
    STORE 0  TO laTempStru[lnFildLen + lnRaw,17], laTempStru[lnFildLen + lnRaw,18]
  ENDFOR
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
*! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[Start]
*! B610022,1 HIA 07/25/2012 Add customer name to exported to excel file [T20120719.0002][Begin]
*!*	IF TYPE('laTempStru') = "U"
*!*	  DIMENSION laTempStru[1,18]
*!*	  laTempStru = ''
*!*	  SELECT ORDHDR
*!*	  lnFildLen = AFIELD(laTempStru)
*!*	ENDIF

*!*	lnFildLen = Alen(laTempStru,1)
*!*	*-- Add Code fields.
*!*	DIMENSION laTempStru[lnFildLen + 1, 18]

*!*	*-- cTempKey :  field used in all sort by cases as the master key ,
*!*	  laTempStru[lnFildLen + 1, 1] = 'BtName'
*!*	  laTempStru[lnFildLen + 1, 2] = 'C'
*!*	  laTempStru[lnFildLen + 1, 3] = 30
*!*	  laTempStru[lnFildLen + 1, 4] = 0
*!*
*!*	FOR lnInc=7 TO 16
*!*	  STORE SPACE(1) TO laTempStru[lnFildLen + 1,lnInc]
*!*	ENDFOR
*!*	STORE 0  TO laTempStru[lnFildLen + 1,17], laTempStru[lnFildLen + 1,18]

*! B610022,1 HIA 07/25/2012 Add customer name to exported to excel file [T20120719.0002][End]
*! B610109,1 MMT 10/09/2012 Order Header report gives error when preview it twice with different criteria[END]
CREATE CURSOR (WorkFile) ;
  FROM ARRAY laTempStru

SELECT (WorkFile)
*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON cTempKey TAG (WorkFile) OF (oAriaApplication.WorkDir+WorkFile+'.CDX')
INDEX ON cTempKey TAG (WorkFile)
*B609356,1 SMA 07/21/2010 remove of clause to prevent empty *.cdx files from creation.....[END]

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfClearRep
loOGScroll.llOGFltCh = .T.

*-- Close temp. opended files, if it used.
*-- Delete temporary work file.
IF USED(WorkFile)
  USE IN (WorkFile)
ENDIF

IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

*Restore old currency setting before exit.
IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF USED("SYCINT")
    USE IN SYCINT
  ENDIF

ENDIF

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
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

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
IF TYPE('lcXMLFileName') <> 'C'
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
    ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
    (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[ENd]
ELSE
  llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
    ((!EMPTY(laOGFxFlt[lnCurrPos,6]) AND ATC("|",laOGFxFlt[lnCurrPos,6])=0) OR ;
    (!EMPTY(laOGFxFlt[lnOrdPos ,6]) AND USED(laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(laOGFxFlt[lnOrdPos,6])=1))
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
ENDIF
*! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
llCurInGrp = !llPrintTot

*!*************************************************************
*! Name      : lfShipToAd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate ship to address then calculate No. of accounts in range.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfShipToAd
PARAMETERS llShort
PRIVATE lnSeekedRc

*-- if short format.
IF llShort AND llMultCurr
  =lfChCurSm()
ENDIF


*!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[Start]
IF lcRpSortBy = 'S'
  IF !EMPTY(lcLastRep1) AND lcLastRep1 <> REP1+IIF(llMultCurr,CCURRCODE,'')
    lcRep1 = REP1
    *DELETE ALL IN (CustFile) FOR salesrep = REP1
    REPLACE ALL &CustFile..llCount WITH .F. FOR &CustFile..salesrep = lcRep1 IN (CustFile)
    lcLastRep1  = REP1+IIF(llMultCurr,CCURRCODE,'')
    lnCntRep = 0
  ENDIF
ENDIF
*!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[End]


*-- Calculate No. Of accounts [Begin]
SET ORDER TO TAG (CustFile) IN (CustFile)
lnSeekedRc = 0
* If printing "Long", ask about the store in custfile.[Begin]
IF SEEK(ACCOUNT+IIF(llShort,'',STORE) , CustFile)
  * If printing "Long", ask about the store in custfile.[End]
  IF Alt_ShpTo
    lcShpTName  = ALLTRIM(Customer.STName)
  ELSE
    lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  ENDIF

ELSE

  lnGrandAcc  = lnGrandAcc + 1
  lnCntRep    = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
  lcLastRep1  = REP1+IIF(llMultCurr,CCURRCODE,'')
  m.Account   = ACCOUNT
  m.Store     = STORE
  m.SalesRep  = REP1
  m.cCurrCode = cCurrCode

  *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[Start]
  m.llCount  = .T.
  *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[End]

  INSERT INTO (CustFile) FROM MEMVAR
  =lfUpdAdd()

ENDIF
lnSeekedRc = RECNO(CustFile)
*-- if sort by sales rep.
IF lcRpSortBy = 'S'
  SET ORDER TO TAG (CUSTFL1) IN (CustFile)
  lcSeekVal = IIF(llMultCurr,REP1+cCurrCode+ACCOUNT,REP1+ACCOUNT)
  IF !SEEK(lcSeekVal,CustFile)
    lnCntRep   = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
    lcLastRep1 = REP1+IIF(llMultCurr,CCURRCODE,'')
    m.Account  = ACCOUNT
    m.Store     = STORE
    m.SalesRep = REP1
    m.cCurrCode = cCurrCode

    *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[Start]
    m.llCount  = .T.
    *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[End]

    INSERT INTO (CustFile) FROM MEMVAR

    *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[Start]
  ELSE
    IF SEEK(lcSeekVal,CustFile)
      IF !llShort  AND !EMPTY(STORE)
        lcOrdStore = STORE
        lnOldAlias = SELECT(0)
        SELECT(CustFile)
        LOCATE REST WHILE IIF(llMultCurr,SALESREP+CCURRCODE+ACCOUNT,SALESREP+ACCOUNT) = lcSeekVal FOR STORE = lcOrdStore AND !&CustFile..llCount
        IF FOUND()
          lnCntRep   = lnCntRep + 1
          REPLACE llCount  WITH .T. IN (CustFile)
        ENDIF
        SELECT (lnOldAlias)
      ELSE
        IF EMPTY(STORE)
          lnOldAlias = SELECT(0)
          SELECT(CustFile)
          LOCATE REST WHILE IIF(llMultCurr,SALESREP+CCURRCODE+ACCOUNT,SALESREP+ACCOUNT) = lcSeekVal FOR EMPTY(STORE) AND !&CustFile..llCount
          IF FOUND()
            lnCntRep   = lnCntRep + 1
            REPLACE llCount  WITH .T. IN (CustFile)
          ENDIF
          SELECT (lnOldAlias)
        ENDIF
      ENDIF
    ENDIF
    *!B608281,1 MMT 09/24/2007 fix bug of  Order Header Report Calculating # of accounts incorrectly[End]
  ENDIF
ENDIF

IF lnSeekedRc <> 0
  GO lnSeekedRc IN (CustFile)
ENDIF

*-- Calculate No. Of accounts [End]
RETURN ''



*!*************************************************************
*! Name      : lfUpdAdd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Update Customer addresses.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfUpdAdd
PRIVATE lnCurrAls

lnCurrAls = SELECT(0)
laShipTo = ''
*-- IF alternate ship to address
IF Alt_ShpTo
  lcShpTName  = ALLTRIM(Customer.STName)
  IF !llShort
    laShipTo[1] = ALLTRIM(cAddress1)
    laShipTo[2] = ALLTRIM(cAddress2)
    laShipTo[3] = ALLTRIM(cAddress3)
    laShipTo[4] = ALLTRIM(cAddress4)
    laShipTo[5] = ALLTRIM(cAddress5)
  ENDIF
ELSE    && Else
  lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  IF !llShort
    * Adjust address function (gfGetAdr) to fasten report.
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
    IF IIF(TYPE('lcXMLFileName') = 'C',!EOF('CUSTOMER'),.T.)
      *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[END]
      =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)
      *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
    ENDIF
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]

  ENDIF
ENDIF    && End of IF

IF !llShort
  =lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
  *Remove charachters more than 21 to appear completly in report.[Begin]
  FOR lnCnt = 1 TO ALEN(laShipTo)
    laShipTo[lnCnt] = SUBSTR(laShipTo[lnCnt],1,21)
  ENDFOR
  *Remove charachters more than 21 to appear completly in report.[End]
  SELECT (CustFile)
  GATHER FROM laShipTo FIELDS cShip1,cShip2,cShip3,cShip4,cShip5
ENDIF
SELECT (lnCurrAls)


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return       : None
*!*************************************************************
*! Modification : ....
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
  ENDIF
ENDFOR

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfvFormat
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Change report format (Long/Short)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvFormat
*-B127856, HFK 05/10/2005 Add summary format to report
*!*  lcRpForm = IIF(lcRpFormat = 'L','SOORDHDL','SOORDHDS','SOORDHDS')

*! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[Start]
*lcRpForm = IIF(lcRpFormat = 'L','SOORDHDL',IIF(lcRpFormat = 'S','SOORDHDM','SOORDHDS'))
lcRpForm = IIF(lcRpFormat = 'L','SOORDHDL',IIF(lcRpFormat = 'S','SOORDHDS','SOORDHDM'))
*! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[End]

*-B127856, HFK 05/10/2005 Add summary format to report
= lfRepPltFr(lcRpForm)

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Get Report Variables
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*Use full Multi currency format capability.
FUNCTION lfGetRepVr
*-- ReIndex work file if user change sort By [Begin]
IF llChSortBy
  DO CASE
  CASE lcRpSortBy = 'O'		&& Sort by Order Case
    lcSortTitl= LANG_Soordhd_Order
    IF llCurInGrp
      lcBreak   = 'CCURRCODE'
      lcGrpExpr = [CCURRCODE + LANG_Soordhd_Total  +':']
      lcIndexTg = [CCURRCODE+ORDER]
    ELSE
      lcBreak   = ''
      lcGrpExpr = ''
      lcIndexTg = [ORDER]
    ENDIF


  CASE lcRpSortBy = 'A'		&& Sort by Account Case
    IF llCurInGrp
      lcBreak = 'ACCOUNT+CCURRCODE'
      lcGrpExpr = [ACCOUNT + "/" + CCURRCODE + LANG_Soordhd_Total +':']
      lcIndexTg = [ACCOUNT+CCURRCODE+ORDER]
    ELSE
      lcBreak = 'ACCOUNT'
      lcGrpExpr = [ACCOUNT + LANG_Soordhd_Total +':']
      lcIndexTg = [ACCOUNT+ORDER]
    ENDIF
    lcSortTitl= LANG_Soordhd_Account

  CASE lcRpSortBy = 'C'		&& Sort by CustPo Case
    IF llCurInGrp
      lcBreak = 'ACCOUNT+CUSTPO+CCURRCODE'
      lcGrpExpr = [ACCOUNT + '/' + CUSTPO + '/' + CCURRCODE + LANG_Soordhd_Total +':']
      lcIndexTg = [ACCOUNT+CUSTPO+CCURRCODE+ORDER]
    ELSE
      lcBreak = 'ACCOUNT+CUSTPO'
      lcGrpExpr = [ACCOUNT + '/' + CUSTPO + LANG_Soordhd_Total +':']
      lcIndexTg = [ACCOUNT+CUSTPO+ORDER]
    ENDIF

    lcSortTitl= LANG_Soordhd_CustPo

  CASE lcRpSortBy = 'P'		&& Sort by Priority Case
    IF llCurInGrp
      *B037288,1 HMA 08/18/2004 Change the group expression in case of sort by periority.[BEGIN]

      *lcBreak = 'DTOS(COMPLETE)+PRIORITY+CCURRCODE'
      *lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + '/' + CCURRCODE + LANG_Soordhd_Total +':']
      *lcIndexTg = [DTOS(COMPLETE)+PRIORITY+CCURRCODE+ACCOUNT+ORDER]
      *ELSE
      *lcBreak = 'DTOS(COMPLETE)+PRIORITY'
      *lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + LANG_Soordhd_Total +':']
      *lcIndexTg = [DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER]

      lcBreak = 'PRIORITY+DTOS(COMPLETE)+CCURRCODE'
      lcGrpExpr = ['Periority'+'('+ALLTRIM(PRIORITY)+')  '+DTOC(COMPLETE)+CCURRCODE +'    '+LANG_Soordhd_Total +':']
      lcIndexTg = [PRIORITY+DTOS(COMPLETE)+CCURRCODE+ACCOUNT+ORDER]
    ELSE
      lcBreak = 'PRIORITY+DTOS(COMPLETE)'
      lcGrpExpr = ['Periority'+'('+ALLTRIM(PRIORITY)+')  '+DTOC(COMPLETE)+'    '+ LANG_Soordhd_Total +':']
      *-- B999999,1 fix prblem of wrong sort with priority[Start]
      lcIndexTg = [PRIORITY+DTOS(COMPLETE)+ACCOUNT+ORDER]
      *--lcIndexTg = [DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER]
      *-- B999999,1 fix prblem of wrong sort with priority[End]
      *B037288,1 HMA 08/18/2004 Change the group expression in case of sort by periority.[END]
    ENDIF

    lcSortTitl= LANG_Soordhd_Priority


  CASE lcRpSortBy = 'S'		&& Sort by Primary sales rep. Case
    IF llCurInGrp
      lcBreak   = 'REP1+CCURRCODE'
      lcGrpExpr = [REP1 + '/' + CCURRCODE + LANG_Soordhd_Total +':']
      lcIndexTg = [REP1+CCURRCODE+ORDER]
    ELSE
      lcBreak   = 'REP1'
      lcGrpExpr = [REP1 + LANG_Soordhd_Total +':']
      lcIndexTg = [REP1+ORDER]
    ENDIF
    lcSortTitl= LANG_Soordhd_PSalesRep

  CASE lcRpSortBy = 'U'		&& Sort by Currency Case
    lcBreak   = 'CCURRCODE'
    lcGrpExpr = [lfCurrDesc() + LANG_Soordhd_Total +':']
    lcIndexTg = [CCURRCODE+ORDER]
    lcSortTitl= LANG_Soordhd_Currency

  ENDCASE

  IF llCurInGrp AND (lcRpSortBy <> 'U')

    IF lcRpSortBy="O"
      lcSortTitl = LANG_Soordhd_Currency+"/" + lcSortTitl
    ELSE
      lcSortTitl = lcSortTitl+"/"+LANG_Soordhd_Currency
    ENDIF

  ENDIF

ENDIF

IF llMultCurr
  lcLineCurr = lfCurrPrnt()
  lcHedTitle = lcSortTitl + ", "+LANG_Soordhd_Print+" : "
  DO CASE
  CASE lcRpCurr = "F"  && Foreign Currency.
    lcHedTitle = lcHedTitle + LANG_Soordhd_ForeignCurr
  CASE lcRpCurr = "O"  && Original Rates
    lcHedTitle = lcHedTitle + LANG_Soordhd_OriginalTrans
  CASE lcRpCurr = "D"  && Rates on date
    lcHedTitle = lcHedTitle + LANG_Soordhd_RateDefinedOn+ DTOC(ldRpExDate) + "."
  OTHERWISE            && Rates defined now.
    lcHedTitle = lcHedTitle + LANG_Soordhd_RateDefinedNow
  ENDCASE
  *!B610255,1 HIA 02/21/2013 Some account show the name and some do not, please see attached a copy of the order header [T20130213.0005][Start]
ELSE
  lcHedTitle = lcSortTitl
  *!B610255,1 HIA 02/21/2013 Some account show the name and some do not, please see attached a copy of the order header [T20130213.0005][End]

ENDIF

*-- ReIndex work file if user change sort By [End  ]

*!*************************************************************
*! Name      : lfAsignVar
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Assign all Report Variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*Use full Multi currency format capability.
FUNCTION lfAsignVar

*Call Function to open closed files in lfClearRep Function. [Begin]
PARAMETERS llFromPrg
*Call Function to open closed files in lfClearRep Function. [End  ]

*Define array if called from OG Only. [Begin]
IF !llFromPrg
  *-- Make Sort by Arrays and fill it. [Begin]
  DIMENSION laSortDesc[5,1],laSortVal[5,1]

  laSortDesc[1,1] = LANG_Soordhd_Order
  laSortDesc[2,1] = LANG_Soordhd_Account
  laSortDesc[3,1] = LANG_Soordhd_CustPo
  laSortDesc[4,1] = LANG_Soordhd_Priority
  laSortDesc[5,1] = LANG_Soordhd_PrimSalesRep

  laSortVal[1,1]  = "O"
  laSortVal[2,1]  = "A"
  laSortVal[3,1]  = "C"
  laSortVal[4,1]  = "P"
  laSortVal[5,1]  = "S"
  *-- Make Sort by Arrays and fill it. [Begin]

  llMultCurr = gfGetMemVar('llMulCurr')
ENDIF
*Define array if called from OG Only. [End  ]

IF llMultCurr

  IF !USED("SYCINT")
    lcSelectCommand=[SELECT * FROM SYCINT]
    lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  ENDIF

  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF  TYPE('lcXMLFileName') <> 'C'
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[END]
    *-- Add Currency item to sort by array. [Begin]
    DIMENSION laSortDesc[ALEN(laSortDesc,1) + 1 , 1] , laSortVal[ALEN(laSortVal,1) + 1 , 1]
    laSortDesc[ALEN(laSortDesc,1),1] = LANG_Soordhd_Currency
    laSortVal[ALEN(laSortDesc,1),1]  = "U"
    *-- Add Currency item to sort by array. [End  ]
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[END]
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
    IF TYPE('lcXMLFileName') = 'C'
      = gfOpenFile(oAriaApplication.systemfilespath+'SYCCURR',oAriaApplication.systemfilespath +'Ccurrcode','SH')
      SELECT SYCCURR
      SET ORDER TO CCURRCODE  && To VALIDATE currency code.
    ELSE
      *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
      = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
      *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
    ENDIF
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]

ENDIF


IF !USED('SYCCOMP')
  lcSelectCommand=[SELECT * FROM SYCCOMP ]
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
ENDIF

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCurrPrnt
PRIVATE lcCurrCode
*-- Not Multi Currency Or it is and any Equavelent method.
IF lcRpCurr <> "F"
  lcCurrCode = [oAriaApplication.BaseCurrency]
ELSE && Multi Currency and Print forign currency.
  lcCurrCode = [cCurrCode]
ENDIF
RETURN lcCurrCode


*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfChCurSm
PRIVATE lcCurrCurr,lnCurrAls,lcSelectCommand,lnResult

lnCurrAls = SELECT(0)
lcCurrCurr = ALLTRIM(EVALUATE(lcLineCurr))

*--Call Function to open closed files in lfClearRep Function.
IF !USED("SYCINT")
  =lfAsignVar(.T.)
ENDIF


lcSelectCommand=[SELECT cCurrCode,cCurrency,cCurrencyI FROM SYCINT WHERE cCurrCode=']+lcCurrCurr+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
  lcCurrRtL = ALLTRIM(cCurrency)
  lcCurrSet = ALLTRIM(cCurrencyI)

  SET CURRENCY TO lcCurrSet

  *! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[Start]
  IF !EMPTY(lcCurrRtL)
    *! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[End]
    SET CURRENCY &lcCurrRtL
    *! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[Start]
  ENDIF
  *! B608133,1 MMT 06/19/2007 Change the report Short format to be like 27[End]
ENDIF
SELECT (lnCurrAls)
RETURN ''

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc

lcCurDesc = ''
lcCurrVal  = ALLTRIM(cCurrCode)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF
RETURN PADR(ALLTRIM(lcCurDesc),20)

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
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


*!************************************************************
*! Name      : lfFillData
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate all memvars and insert new line.
*!************************************************************
*! Passed Parameters  : ...
*!************************************************************
*! Returns            : Position
*!************************************************************
*! Modification : ....
*! B039069,1 MMT 02/22/2005 report peformance Improvement
*!************************************************************
*TO Fasten the report long form.
FUNCTION lfFillData
PARAMETERS llEditOnly
*-- if it is long format
IF lcRpFormat = "L"
  PRIVATE lcCanReson,lcCurrDesc
  STORE "" TO lcCanReson,lcCurrDesc

  IF !EMPTY(ccancreson)
    lcCanReson = gfCodDes(CCANCRESON, 'CCANCRESON')
  ENDIF

  IF llMultCurr
    lcCurrDesc = lfCurrDesc()
  ENDIF

  m.cSeaDesc =  PADR(SEASON,6) + '-' + gfCodDes(SEASON, 'SEASON')
  m.cDivDesc =  PADR(cDIVISION,6) + '-' + gfCodDes(cDIVISION, 'CDIVISION')
  m.cTrmDesc =  gfCodDes(CTERMCODE, 'CTERMCODE')
  m.cViaDesc =  gfCodDes(SHIPVIA, 'SHIPVIA')
  m.cSpcDesc =  gfCodDes(SPCINST, 'SPCINST')
  m.cRegDesc =  gfCodDes(CUSTOMER.REGION, 'REGION')
  m.cDecl_code= gfCodDes(DECL_CODE, 'DECL_CODE')
  m.cCanDesc = lcCanReson
  m.cCurDesc = lcCurrDesc
ENDIF

IF llEditOnly
  REPLACE cSeaDesc WITH PADR(SEASON,6) + '-' + gfCodDes(SEASON, 'SEASON') ,;
    cDivDesc WITH PADR(cDIVISION,6) + '-' + gfCodDes(cDIVISION, 'CDIVISION') ,;
    cTrmDesc WITH gfCodDes(CTERMCODE, 'CTERMCODE') ,;
    cViaDesc WITH gfCodDes(SHIPVIA, 'SHIPVIA') ,;
    cSpcDesc WITH gfCodDes(SPCINST, 'SPCINST') ,;
    cRegDesc WITH gfCodDes(CUSTOMER.REGION, 'REGION'),;
    cDecl_code WITH gfCodDes(DECL_CODE, 'DECL_CODE') ,;
    cCanDesc WITH lcCanReson     ,;
    cCurDesc WITH lcCurrDesc
ELSE

  m.cTempKey = EVALUATE(lcIndexTg)


  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  IF TYPE('lcXMLFileName') <> 'C'
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
    *!B608618,1 MMT 07/20/2008 Fix bug of wrong amounts when export report to EXCEL[Start]
    m.Shipamt   = IIF(lcRpCurr = "F" OR cCurrCode=gcBaseCurr,Shipamt,gfAmntDisp(Shipamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.Cancelamt = IIF(lcRpCurr = "F" OR cCurrCode=gcBaseCurr,Cancelamt,gfAmntDisp(Cancelamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.Openamt   = IIF(lcRpCurr = "F" OR cCurrCode=gcBaseCurr,Openamt,gfAmntDisp(Openamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.bookamt   = IIF(lcRpCurr = "F" OR cCurrCode=gcBaseCurr,bookamt,gfAmntDisp(bookamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    *!B608618,1 MMT 07/20/2008 Fix bug of wrong amounts when export report to EXCEL[End]
    *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[Start]
  ELSE
    m.Shipamt   = IIF(lcRpCurr = "F" OR cCurrCode=oAriaApplication.BaseCurrency,Shipamt,gfAmntDisp(Shipamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.Cancelamt = IIF(lcRpCurr = "F" OR cCurrCode=oAriaApplication.BaseCurrency,Cancelamt,gfAmntDisp(Cancelamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.Openamt   = IIF(lcRpCurr = "F" OR cCurrCode=oAriaApplication.BaseCurrency,Openamt,gfAmntDisp(Openamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
    m.bookamt   = IIF(lcRpCurr = "F" OR cCurrCode=oAriaApplication.BaseCurrency,bookamt,gfAmntDisp(bookamt,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  ENDIF
  *! E302786,1 MMT 10/18/2010 Modify Report to be called from the request builder[End]
  *! B610022,1 HIA 07/25/2012 Add customer name to exported to excel file [T20120719.0002][Begin]
  m.BtName = ALLTRIM(customer.BtName)
  *! B610022,1 HIA 07/25/2012 Add customer name to exported to excel file [T20120719.0002][End]
  *B610060,1 MMT 08/27/2012 Order Header does not export Ship To address to Excel[T20120817.0001][Start]
  laShipTo = ''
  IF Alt_ShpTo
    m.stName = ALLTRIM(Customer.STName)
    laShipTo[1] = ALLTRIM(cAddress1)
    laShipTo[2] = ALLTRIM(cAddress2)
    laShipTo[3] = ALLTRIM(cAddress3)
    laShipTo[4] = ALLTRIM(cAddress4)
    laShipTo[5] = ALLTRIM(cAddress5)
  ELSE
    *!B610320,1 HIA 04/29/13 T20130320.0001 - ARIA4XP - SO - ORDER HEADER REPORT [Start]
    lcCustKey = ''
    IF !EMPTY(ordhdr.STORE)
      *B610320,3 TMI 05/05/2013 [Start] evaluate can not work correctly if the evaluated expression is related to a table in other work area
      *lcCustKey = EVALUATE(KEY('customer'))
      lnSlct = SELECT(0)
      SELECT CUSTOMER
      lcCustKey = EVALUATE(KEY())
      *B610320,3 TMI 05/05/2013 [End  ] 
      
      =SEEK('S'+ordhdr.AccounT+ordhdr.Store,'Customer')
      
      *B610320,3 TMI 05/05/2013 [Start] restore the originally selected area
      SELECT (lnSlct)
      *B610320,3 TMI 05/05/2013 [End  ] 
    ENDIF
    *!B610320,1 HIA 04/29/13 T20130320.0001 - ARIA4XP - SO - ORDER HEADER REPORT [End]
    m.stName = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
    =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)
    *!B610320,1 HIA 04/29/13 T20130320.0001 - ARIA4XP - SO - ORDER HEADER REPORT [Start]
    IF !EMPTY(lcCustKey)
      =SEEK(lcCustKey,'Customer')
    ENDIF
    *!B610320,1 HIA 04/29/13 T20130320.0001 - ARIA4XP - SO - ORDER HEADER REPORT [End]
    
  ENDIF
  =lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
  m.cAddress1 = laShipTo[1]
  m.cAddress2 = laShipTo[2]
  m.cAddress3 = laShipTo[3]
  m.cAddress4 = laShipTo[4]
  m.cAddress5 = laShipTo[5]
  *B610060,1 MMT 08/27/2012 Order Header does not export Ship To address to Excel[T20120817.0001][End]
  INSERT INTO (WorkFile) FROM MEMVAR

ENDIF
*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Modification : ....
*!**************************************************************************
*-Due to Enhancement 128374, this finction is no longer in use
*!*  FUNCTION lfvEdiOrd
*!*  lcRpEdiFlt = ""
*!*  *- E128374,1 HFK 06/05/2005 [Start]
*!*  *!*  IF 'EB' $ oAriaApplication.CompanyInstalledModules AND lcRpEdiPrn <> "B"
*!*  *!*    lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
*!*  *!*  ENDIF
*!*  IF 'EB' $ oAriaApplication.CompanyInstalledModules
*!*    DO CASE
*!*      CASE ("A" $ lcRpType .AND. !("B" $ lcRpType) .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder]
*!*      CASE (!("A" $ lcRpType) .AND. "B" $ lcRpType .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T']
*!*      CASE (!("A" $ lcRpType) .AND. !("B" $ lcRpType) .AND. "C" $ lcRpType)
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O']
*!*        SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .And. cOrdType = 'O')
*!*      CASE ("A" $ lcRpType .AND. "B" $ lcRpType .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder  .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T')]
*!*      CASE ("B" $ lcRpType .AND. "C" $ lcRpType .AND. !("A" $ lcRpType))
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder]
*!*      CASE ("A" $ lcRpType .AND. !("B" $ lcRpType) .AND."C" $ lcRpType )
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O')]
*!*    ENDCASE
*!*  ENDIF
*!*  *- E128374,1 HFK 06/05/2005 [End]
*!*  lnOrdPos = ASCAN(loOGScroll.laOGFxFlt,'ORDHDR.ORDER')
*!*  *-- Check if The user entered a date and get the date period to be printed.

*!*  IF lnOrdPos > 0
*!*    lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
*!*    lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
*!*    IF USED(lcOrderSel)
*!*      SELECT(lcOrderSel)
*!*      ZAP
*!*    ENDIF
*!*  ENDIF

*!*  llClearOrd = .T.

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
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
*!*************************************************************
*! Name      : lfGrpExp
*: Developer : Mariam Mazhar [MMT]
*: Date      : 01/02/05
*! Purpose   : function to put a group title in layout
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGrpExp
lcRetValue = IIF(lcRpSortBy='O','Order: '+ ORDER,IIF(lcRpSortBy='A','Account:'+ACCOUNT+' - '+CUSTOMER.BTNAME,IIF(lcRpSortBy='C','Cust PO.:'+account+'/'+CUSTPO,IIF(lcRpSortBy='P','Priority:'+'('+ALLTRIM(PRIORITy)+') '+DTOC(COMPLETE),''))))
lcRetValue = IIF(lcRpSortBy='S','Primary Sales Rep.:'+REP1,IIF(lcRpSortBy='U','Currency:'+lfCurrdesc(),lcRetValue))
RETURN lcRetValue




*!*************************************************************
*! Name      : lfvTypes
*! Developer : Heba Fathi (HFK)
*! Date      : 06/05/2005
*! Purpose   : - Evaluate order types expression.
*!*************************************************************
*- E128374,1 HFK 06/05/2005 [Start]
FUNCTION lfvTypes
PRIVATE lcOldTypes,lcCurrChr

lcOldTypes = lcRpType  && Save old status value.
= lfOGMover(@laRpTypSrc,@laRpTypDst,"Select Order Type",.T.,'')  && call mover function.

lcRpType = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTypDst[1])
  FOR lnI = 1 TO ALEN(laRpTypDst,1)
    lcRpType = lcRpType + IIF(laRpTypDst[lnI] = "Orders",'O',;
      IIF(laRpTypDst[lnI] = "EDI Temporary Orders",'T',;
      IIF(laRpTypDst[lnI] = "EDI Received Orders",'R','')))
  ENDFOR
ENDIF
lcRpType = IIF(EMPTY(lcRpType),'ABC',ALLTRIM(lcRpType))
lnOrdPos = ASCAN(loOGScroll.laOGFxFlt,'ORDHDR.ORDER')

*-- Check if The user entered a date and get the date period to be printed.
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
  IF USED(lcOrderSel)
    SELECT(lcOrderSel)
    ZAP
  ENDIF
ENDIF
llClearOrd = .T.
*- E128374,1 HFK 06/05/2005 [End]
*!*************************************************************
*! Name      : RefreshTypes
*! Developer : Heba Fathi (HFK)
*! Date      : 06/05/2005
*! Purpose   : Return the selected types in the ReadBox
*!*************************************************************
*- E128374,1 HFK 06/05/2005 [Start]
FUNCTION RefreshTypes
LOCAL lcTypesStr, lnTarget
lcTypesStr = ""
IF !EMPTY(laRpTypDst)
  FOR lnTarget = 1 TO ALEN(laRpTypDst,1)
    lcTypesStr = lcTypesStr+ "," + laRpTypDst[lnTarget]
  ENDFOR
  lcTypesStr = SUBSTR(lcTypesStr,2)
ENDIF
RETURN lcTypesStr
ENDFUNC
*- E128374,1 HFK 06/05/2005 [End]
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : - Evaluate Status expression.
*!           : - Raise change status flag.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
=gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.

IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
      IIF(laRpTarget[lnI] = 'Open','O',;
      IIF(laRpTarget[lnI] = 'Hold','H',;
      IIF(laRpTarget[lnI] = 'Complete','C',;
      IIF(laRpTarget[lnI] = 'Cancelled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- if length of current selected status differ from previous length
IF LEN(lcOldStat) != LEN(lcRpStatus)
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length

  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

DO lpChkStat
*-- end of lfvOStatus.
