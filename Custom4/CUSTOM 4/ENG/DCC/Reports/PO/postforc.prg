*:************************************************************************
*: Program file  	   : POSTFORC.PRG
*: Program desc. 	   : PO Style Forecasting Report
*: System        	   : Aria4XP
*: Module        	   : Purchase Order (PO)
*: Developer     	   : Hesham Elmasry (HES)
*: Date                : 05/21/2009
*: Tracking Job Number : C201147
*: Notes               : Custom For DCC
*:************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : lfwrepwhen(), lfSetFun(), lfCheckFilter(),...
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO POSTFORC
*:************************************************************************
*: MODIFICATIONS:
*: C201147,2 HES 08/25/2009 Handle the issue of multi FIT for one Style\Color [T20080808.0001]
*: C201147,3 HES 09/01/2009 Handle the issue of more than 20 sizes for one Style\Color\Fit [T20080808.0001]
*: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [T20080808.0001]
*! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[T20100927.0003]
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[T20101123.0018]
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[T20110117.0005]
*! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[T20110713.0006]
*! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[T20110117.0005]
*! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder []
*! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[T20111107.0005]
*! B610330,1 MMT 05/12/2013 incorrect Average monthly qty in custom forecasting report[T20110117.0005]
*! B610581,1 MMT 11/12/2013 Custom forcasting report included EDI orders in Booked QTY[T20131031.0004]
*! B610631,1 SAB 12/22/2013 Fix issue of not changing the date to project in scheduler request [T20131209.0001]
*! B610652,1 SAB 01/05/2014 Fix error in lfCollectData fucntion [T20131209.0001]
*! B610911,1 MMT 11/16/2014 Custom PO forecasting report shows wrong totals[T20141113.0010]
*! C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003]
*! B610924,1 MMT 01/12/2015 Fix the bug of incorrect new order qty if there is no sales done[T20141125.0003]
*! B610924,2 MMT 01/18/2015 Fix the bug of incorrect UK WIP[T20141125.0003]
*! B610924,3 MMT 01/18/2015 Fix the bug of incorrect UK WIP in case precedding style has scale count larger than the current style[T20141125.0003]
*! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006]
*! B611045,1 MMT 08/24/2015 Custom Forcasting report calculating Qty based on complete date[T20150820.0008]
*! C201697,2 MMT 09/13/2015 Fix issue# 1 on the project#[T20150629.0006]
*! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005]
*! C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001]
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001]
*! C201805,1 MMT 04/12/2016 Add option Apply uplift to Plan' to Custom Forecasting rep.[T20160228.0002]
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12]
*:************************************************************************
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID

IF TYPE('lcXMLFileName') = 'C'
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId  
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]
  
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment.ClientID = ClientID
  	
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath 
   
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
*!*	  *! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
*!*	*!*    lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
*!*	*!*    DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
*!*	  lcCurrentProcedure = loEnvironment.Aria40SharedPath
*!*	  SET DEFAULT TO &lcCurrentProcedure.
*!*	  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID, lcCurrentProcedure, loEnvironment
*!*	  SET DEFAULT TO &lcCurrentProcedure.  
*!*	  *! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
*!*	  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
*!*	*!*    LOCAL lcCurrentProcedure
*!*	*!*    lcCurrentProcedure = ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath))
*!*	*!*    lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDICTIONARY\"), "", -1, 1, 1)
*!*	*!*    DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID )  , ClientID
*!*	  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
  
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]    
  
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
  *oAriaEnvironment.REPORT.gcAct_Appl = 'PO'  
  *PUBLIC gcAct_Appl
  *gcAct_Appl = 'PO'
  *oAriaEnvironment.activeModuleID = 'PO'
  
  PUBLIC gcAct_Appl
  gcAct_Appl  = 'PO'  
  oAriaEnvironment.REPORT.gcAct_Appl = 'PO'
  oAriaEnvironment.ActiveModuleID = 'PO'
  oAriaEnvironment.RequestID = lcRequestID
  
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]  
  
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF 
  
  oAriaEnvironment.Report.cCROrientation = 'L'
  
  SET STEP ON
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
  *- This code added to the program as a reference 
  *STORE 0 TO lnDivPos, lnPatPos, lnSesPos, lnPurPos, lnGroPos, lnFabPos
  *STORE '' TO cStyTemp
  *STORE 0 TO lnMnPrj
  *STORE oAriaApplication.Systemdate TO ldprjFrm
  *STORE 1 TO lnMnSls  
  *lcMnthTemp = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Cursor For Actual Tmp
  *lcActTemp  = oAriaEnvironment.CURSORS.GetCursorTempName()        && Temp Cursor For Months Tmp
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]
  
  llOgFltCh = .T.
  lfwrepwhen()
*! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
ELSE
  loogscroll.cCROrientation = 'L'
*! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]
ENDIF
*C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][Start]
*IF llRPUpPl
IF llRPUpPl OR (llRPUpCSV AND llOgFltCh)
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][End]
  CREATE CURSOR 'StyleScle' (StyCode C(19), CNT N(3),FiT C(10))
  SELECT 'StyleScle' 
  INDEX oN StyCode TAG 'StyleScle' 
ENDIF
*C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]

*C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][Start]
IF llRPAUPJ
  ldprjFrm = DATE(YEAR(DATE()),MONTH(DATE()),1)-1
ENDIF  
*C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][END]

*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
IF llOgFltCh
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
  LCRPPURC =''
  lnPosPur = ASCAN(laOgFXFlt,"LCRPPURC")
  IF lnPosPur> 0 
    lnPosPur= ASUBSCRIPT(laOgFxFlt,lnPosPur,1)
    LCRPPURC= laOgFxFlt[lnPosPur,6]
  ENDIF
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
  
  *! B610631,1 SAB 12/22/2013 Fix issue of not changing the date to project in scheduler request [T20131209.0001][Start]
  IF TYPE('lcXMLFileName') = 'C'
    ldPrjFrm = DATE()
  ENDIF
  
  *C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][Start]
  IF llRPAUPJ
    ldprjFrm = DATE(YEAR(DATE()),MONTH(DATE()),1)-1
  ENDIF  
  *C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][END]
  
  *! B610631,1 SAB 12/22/2013 Fix issue of not changing the date to project in scheduler request [T20131209.0001][End]
  IF lfColctData()
    SELECT(lcActTemp)
  ELSE 
    *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
    IF TYPE('lcXMLFileName') <>'C'
    *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
      =gfModalGen('TRM00052B40011','ALERT')
    *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]  
    ENDIF  
    *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
    RETURN 
  ENDIF 
ENDIF 

SELECT(lcActTemp)
*! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[Start]
IF llRPSpZr
  LOCATE 
  SCAN   
    lnNewOrderTot = 0
    FOR lnCntOrd = 1 TO 20 
      lnRetValue = lfGetNewOrd(lnCntOrd)
      lnNewOrderTot = lnNewOrderTot +  IIF(TYPE('lnRetValue')<>'N',0,lnRetValue)
    ENDFOR 
    IF lnNewOrderTot = 0
      DELETE
    ENDIF            
  ENDSCAN              
  LOCATE               
ENDIF

*C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
IF llRPUpPl
  IF !USED('Style_Up')
    =gfOpenTable("Style","Style",'SH','Style_Up')
  ENDIF
  *! C201697,2 MMT 09/13/2015 Fix issue# 1 on the project#[T20150629.0006][Start]
*!*	  SELECT 'StyleScle' 
*!*	  LOCATE 
*!*	  SCAN  
*!*	    SELECT(lcActTemp)
*!*	    LOCATE FOR Style = SUBSTR(StyleScle.StyCode,1,LEN(StyleScle.StyCode)-1) AND fit =  StyleScle.FiT AND !DELETED()
*!*	    lnCntSz = 1
*!*	    IF FOUND() 
*!*	      IF gfSeek(StyleScle.StyCode,'Style_Up','Style')
*!*	        SELECT 'Style_Up'
*!*	        FOR lnI = 1 TO StyleScle.Cnt
*!*	          lcI = STR(lnI,1)
*!*	          REPLACE plan&lcI. WITH IIF(Eval(lcActTemp+'.nSlsMon') > 0, CEILING(EVAL(LCACTTEMP+'.TOTSZ'+ALLTRIM(STR(lnCntSz))) / Eval(lcActTemp+'.nSlsMon')), 0)
*!*	          lnCntSz = lnCntSz +  1
*!*	        ENDFOR  
*!*	        REPLACE totPlan WITH PLAN1+PLAN2+PLAN3+PLAN4+PLAN5+PLAN6+PLAN7+PLAN8
*!*	        =gfReplace('')
*!*	      ENDIF  
*!*	    ENDIF  
*!*	  ENDSCAN   
  SELECT(lcActTemp)
  LOCATE 
  SCAN FOR !DELETED()
    SELECT 'StyleScle' 
    LOCATE FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcActTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcActTemp..FiT AND !DELETED()
    IF FOUND()
      lnCntSz = 1
      SCAN REST FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcActTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcActTemp..FiT AND !DELETED()
        IF gfSeek(StyleScle.StyCode,'Style_Up','Style')
          SELECT 'Style_Up'
          FOR lnI = 1 TO StyleScle.Cnt
            lcI = STR(lnI,1)
            *! C201805,1 MMT 04/12/2016 Add option Apply uplift to Plan' to Custom Forecasting rep.[T20160228.0002][Start] 
            *REPLACE plan&lcI. WITH IIF(Eval(lcActTemp+'.nSlsMon') > 0, CEILING(EVAL(LCACTTEMP+'.TOTSZ'+ALLTRIM(STR(lnCntSz))) / Eval(lcActTemp+'.nSlsMon')), 0)
            REPLACE plan&lcI. WITH IIF(Eval(lcActTemp+'.nSlsMon') > 0, IIF(llRPUpLf AND Style_Up.NSTYFRSUPL > 0,Style_Up.NSTYFRSUPL,1) * CEILING(EVAL(LCACTTEMP+'.TOTSZ'+ALLTRIM(STR(lnCntSz))) / Eval(lcActTemp+'.nSlsMon')), 0)
            *! C201805,1 MMT 04/12/2016 Add option Apply uplift to Plan' to Custom Forecasting rep.[T20160228.0002][End] 
            lnCntSz = lnCntSz +  1
          ENDFOR  
          REPLACE totPlan WITH PLAN1+PLAN2+PLAN3+PLAN4+PLAN5+PLAN6+PLAN7+PLAN8
          =gfReplace('')
		ENDIF              
      ENDSCAN
    ENDIF
  ENDSCAN
  *! C201697,2 MMT 09/13/2015 Fix issue# 1 on the project#[T20150629.0006][End]
  SELECT 'Style_Up'
  =gfTableUpdate() 
  SELECT (lcActTemp)
  LOCATE 
ENDIF
*C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][Start]
IF llRPUpCSV
  lfUpdateOTSCSV()
ENDIF
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][End]
*! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[End]
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
IF TYPE('lcXMLFileName') = 'C'
  oAriaEnvironment.REPORT.OGLastForm = lcRpForm
  loProgress.Percent = 0.9
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
  
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [Start]
  *IF loProxy.GetRequest(lcRequestID, ClientID).STATUS = 3
  *  oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
  *  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *ENDIF
  oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
  loProgress.Percent = 1.0
  loProgress.DESCRIPTION = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  *! C201524,1 SAB 10/16/2012 Modify Forcasting Report to Run From Request Builder [End]
  
ELSE
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[END]
  =gfDispRe()
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
ENDIF
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
*************************************************************
*! Name      : lfwrepwhen
*! Developer : Hesham Elmasry
*! Date      : 05/14/2009
*! Purpose   : When Function
*!*************************************************************
FUNCTION lfwrepwhen

IF !USED('STYLE')
  =gfOpenTABLE(oAriaApplication.DATADIR+'STYLE',oAriaApplication.DATADIR+'STYLE','SH')
ENDIF 
IF !USED('SCALE')
  =gfOpenTABLE(oAriaApplication.DATADIR+'SCALE',oAriaApplication.DATADIR+'SCALE','SH')
ENDIF 
IF !USED('SCALEHD')
  =gfOpenTABLE(oAriaApplication.DATADIR+'SCALEHD',oAriaApplication.DATADIR+'EXTSCALE','SH')
ENDIF 
IF !USED('ORDLINE')
  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDLINE',oAriaApplication.DATADIR+'ORDLINES','SH')
ENDIF 
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[Start]
*!*  IF !USED('BOMLINE') 
*!*    =gfOpenTABLE(oAriaApplication.DATADIR+'BOMLINE',oAriaApplication.DATADIR+'BOMLINE','SH')
*!*  ENDIF 
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[End]
IF !USED('ORDCANLN') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDCANLN',oAriaApplication.DATADIR+'ORDCANLN','SH')
ENDIF 
IF !USED('ITEM') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'ITEM',oAriaApplication.DATADIR+'STYLE','SH')
ENDIF 

*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[Start]
IF !USED('BOM')
=gfOpenTable('BOM','MULTIBOM')
ENDIF
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[End]

lnDivPos = lfCheckFilter(3, "STYLE.CDIVISION", 2)
lnPatPos = lfCheckFilter(3, "STYLE.PATTERN", 2)
lnSesPos = lfCheckFilter(3, "STYLE.SEASON", 2)
lnPurPos = lfCheckFilter(3, "STYLE.CPURCODE", 2)
lnGroPos = lfCheckFilter(3, "STYLE.CSTYGROUP", 2)
lnFabPos = lfCheckFilter(1, "ITEM.CSTYMAJOR", 2)

* End of lfwrepwhen()

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry
*! Date      : 05/14/2009
*! Purpose   : Return the select values from filters
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter, lnRetTyp

LOCAL lcReturn, lnPOS   
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
IF TYPE('lcXMLFileName') ='C'
  DO CASE
    CASE lnArrayType = 1
      lnPOS = ASCAN(laOGFxFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(laOGFxFlt,lnPos,1)    
        lcReturn = laOGFxFlt[lnPOS,6] 
      ENDIF
    CASE lnArrayType = 2
      lnPOS = ASCAN(laOGHDFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(laOGHDFlt,lnPos,1)
        lcReturn = laOGHDFlt[lnPOS,6]  
      ENDIF
    CASE lnArrayType = 3  
      lnPOS = ASCAN(laOGvrFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(laOGvrFlt,lnPos,1)  
        lcReturn = laOGvrFlt[lnPOS,6] 
      ENDIF
  ENDCASE
ELSE
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  DO CASE
    CASE lnArrayType = 1
      lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)    
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6] 
      ENDIF
    CASE lnArrayType = 2
      lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]  
      ENDIF
    CASE lnArrayType = 3  
      lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
      IF lnPos > 0
        lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)  
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6] 
      ENDIF
  ENDCASE
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
ENDIF
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[ENd]
IF lnRetTyp = 1
  RETURN lcReturn 
ELSE 
  RETURN lnPos
ENDIF 
* End of lfCheckFilter

*!*************************************************************
*! Name      : lfVldPrjMn
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Validate months to project forward 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfVldPrjMn
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
*IF lnMnPrj > 0 AND lnMnPrj <= 12
IF lnMnPrj >= 0 AND lnMnPrj <= 12
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[END]
  RETURN
ELSE 
  gfModalGen('TRM34215B00000','ALERT')
  lnMnPrj = _SCREEN.ActiveForm.ACTIVECONTROL.OLDVALUE
  RETURN .F.
ENDIF 
* End of lfVldPrjMn

*!*************************************************************
*! Name      : lfVldSlsMn
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Validate months sales to use for projection  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfVldSlsMn
IF lnMnSls > 0 AND lnMnSls <= 12
  RETURN 
ELSE 
  gfModalGen('TRM34215B00000','ALERT')
  lnMnSls = _SCREEN.ActiveForm.ACTIVECONTROL.OLDVALUE
  RETURN .F.
ENDIF 
* End of lfVldMnth

*!*************************************************************
*! Name      : lfCretStyTemps
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Preparing Temp for the selected styles and its related data 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfCretStyTemps

*========= Style Months Temp ==========
IF USED(lcMnthTemp) AND RECCOUNT(lcMnthTemp) > 0
  SELECT (lcMnthTemp)
  USE IN (lcMnthTemp)
ENDIF
*-- Create File
IF !USED(lcMnthTemp)
  lnI = 1
  
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'STYLE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 18
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'FIT'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0
  
  *: C201147,3 HES Add Group field to handle the issue of handling more than 20 sizes [Start] 
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'GROUP'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 1
  laTempStru1[lnI,4] = 0
  *: C201147,3 HES Add Group field to handle the issue of handling more than 20 sizes [End]
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'YEAR'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 4
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'CYEAR'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 2
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'MONTH'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 2
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'CMONTH'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 9
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'TOTMONTH'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  FOR lnX = 1 TO 20
    lcX  = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru1,1)+1
    DIMENSION laTempStru1[lnI,4]
    laTempStru1[lnI,1] = 'BOK'+lcX
    laTempStru1[lnI,2] = 'N'
    laTempStru1[lnI,3] = 6
    laTempStru1[lnI,4] = 0
  ENDFOR
  
  DECLARE LAIndeces[1,2]
  
  *: C201147,3 HES Include the Group field in the Index [Start]
  *LAIndeces[1,1] = 'STYLE+FIT+STR(YEAR,4)+STR(MONTH,2)'
  LAIndeces[1,1] = 'STYLE+FIT+GROUP+STR(YEAR,4)+STR(MONTH,2)'   
  *: C201147,3 HES Include the Group field in the Index [End]
  
  LAIndeces[1,2] = 'STYMNTH'
  
  =gfCrtTmp(lcMnthTemp,@laTempStru1,@LAIndeces)
ENDIF 
*========= Style Inf Temp ==========
IF USED(lcActTemp) AND RECCOUNT(lcActTemp) > 0
  SELECT (lcActTemp)
  USE IN (lcActTemp)
ENDIF
*-- Create File
IF !USED(lcActTemp)
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STYLE'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 18
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STY_DESC'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 60
  laTempStru[lnI,4] = 0
  
  *: C201147,3 HES Add Group field to handle the issue of handling more than 20 sizes and a HASMORE Flag [Start]
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'GROUP'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0

  && If the Style\Fit has more than one page print in the report '"To Be Continue in the next page..."
  lnI = ALEN(laTempStru,1)+1 
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'HASMORE'
  laTempStru[lnI,2] = 'L'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
  *: C201147,3 HES Add Group field to handle the issue of handling more than 20 sizes and a HASMORE Flag [End]
   
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STY_COLR'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 60
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOT_SIZ'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 2
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'FIT'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'YEAR'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 4
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'MONTH'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 2
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PATTERN'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SUPPLIER'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DATE'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'USER_ID'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 10
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'COMPANY'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 2
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CRTN_QTY'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 3
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOTSTK'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOTWIP'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOTSZS'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DIM1DESC'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 5
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STYCOMP'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 0
  

  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'dProduce'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nSlsMon'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 4
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nstyfrsprj'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 2
  laTempStru[lnI,4] = 0
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
    
  
  FOR lnX = 1 TO 20
    lcX = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'SIZ'+lcX
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 5
    laTempStru[lnI,4] = 0
  ENDFOR
  
  FOR lnX = 1 TO 20
    lcX = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'STK'+lcX
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0
  ENDFOR
  
  FOR lnX = 1 TO 20
    lcX = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'WIP'+lcX
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0
  ENDFOR
  
  FOR lnX = 1 TO 20
    lcX = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'TOTSZ'+lcX
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0
  ENDFOR
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
  FOR lnX = 1 TO 20
    lcX = ALLTRIM(STR(lnX))
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'UKWIP'+lcX
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0
  ENDFOR
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOTUKWIP'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0  
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
  DECLARE LAIndeces1[1,2]
  
  *: C201147,3 HES Include the Group field in the Index [Start]
  *LAIndeces1[1,1] = 'STYLE+FIT'
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  *LAIndeces1[1,1] = 'STYLE+FIT+GROUP'
  LAIndeces1[1,1] = 'PATTERN+STYLE+FIT+GROUP'
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  *: C201147,3 HES Include the Group field in the Index [End]
  
  LAIndeces1[1,2] = 'STYINF'
  =gfCrtTmp(lcActTemp,@laTempStru,@LAIndeces1)
ENDIF
*=========== Set the Relation ==============
SELECT(lcActTemp)

*: C201147,3 HES Include the GREOUP field in the relation [Start]
*SET RELATION TO STYLE+FIT INTO &lcMnthTemp
SET RELATION TO STYLE+FIT+GROUP INTO &lcMnthTemp
*: C201147,3 HES Include the GREOUP field in the relation [End]

SET SKIP TO &lcMnthTemp

* End of lfCretStyTemps

*!*************************************************************
*! Name      : lfAddMonth
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Preparing Temp for the booked style Sizes for s
*!              pecific fit   
*!*************************************************************
*! Passed Parameters  : STYLE, MONTH, YEAR
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfAddMonth

PARAMETERS lcStyle, lnMonth, lnYear, lcFit 

LOCAL lnX, lnZ, lnY, ldDate
ldDate = DATE(lnYear,lnMonth,1)
lcYear = SUBSTR(STR(lnYear,4),3,2)

ldDate = DATE(lnYear,lnMonth,14)
lcMonth = CMONTH(ldDate)

SELECT(lcMnthTemp)
APPEND BLANK 

*: C201147,3 HES Update the Group field
*!*	REPLACE STYLE WITH lcStyle  ,;
*!*	        MONTH with lnMonth  ,;
*!*	        YEAR with lnYear    ,;
*!*	        FIT WITH lcFit      ,;
*!*	        CMONTH WITH lcMonth ,;
*!*	        CYEAR WITH lcYear   
        
REPLACE STYLE WITH lcStyle  ,;
        MONTH with lnMonth  ,;
        YEAR with lnYear    ,;
        FIT WITH lcFit      ,;
        CMONTH WITH lcMonth ,;
        CYEAR WITH lcYear   ,;
        GROUP WITH '0'        
*: C201147,3 HES Update the Group field

lcScl = RIGHT(lcStyle,2)
SELECT SCALE
SET ORDER TO SCALE 
=SEEK('S'+lcScl)
DIMENSION laScl[1,2]
STORE '' TO laScl
lnLen = 1
lnCnt = 1

SCAN REST WHILE TYPE+SCALE = 'S'+lcScl FOR SCALE.CSCL_DESC = lcFit
  DIMENSION laScl[lnLen,2]
  laScl[lnLen,1] = SCALE.SCALE
  laScl[lnLen,2] = lnCnt 
  lnCnt = lnCnt + scale.cnt  
  lnLen = lnLen + 1
ENDSCAN

SELECT ORDCANLN
SET ORDER TO ORDCANLN

SELECT ORDLINE
SET ORDER TO ORDLINES
SET RELATION TO "S"+SCALE INTO SCALE
SET RELATION TO 'O'+Ordline.ORDER+STR(ordline.LINENO,6) INTO ORDCANLN ADDITIVE
LOCATE
gfSeek(lcStyle)

lnBokCntr = 0
lcY = ''
*: C201147,3 HES 
lcC = ''
*: C201147,3 HES 
lcZ = ''
llExit = .F.

*: C201147,3 HES Dynamic declaration for just the needed BOOK memory variables
*STORE 0 TO m.BOK1,m.BOK2,m.BOK3,m.BOK4,m.BOK5,m.BOK6,m.BOK7,m.BOK8,m.BOK9,m.BOK10,m.BOK11,m.BOK12,m.BOK13,m.BOK14,m.BOK15,m.BOK16,m.BOK17,m.BOK18,m.BOK19,m.BOK20
lcBok = ''
FOR lnC = 1 TO lnGrpsSzs
  lcBok = 'm.BOK' + ALLTRIM(STR(lnC))
  STORE 0 TO &lcBok
ENDFOR 
*: C201147,3 HES Dynamic declaration for just the needed BOOK memory variables

m.TOTMONTH = 0 
*: C201147,2 HES
lcChFit = lcFit
llError = .F.
*: C201147,2 HES

*: C201147,3 HES HES
lnCntr = 0  && Variable holds the GROUP ID
*: C201147,3 HES HES
*! B610581,1 MMT 11/12/2013 Custom forcasting report included EDI orders in Booked QTY[T20131031.0004][START]
*SCAN REST WHILE STYLE = lcStyle FOR SCALE.CSCL_DESC = lcFit AND MONTH(COMPLETE) = lnMonth AND YEAR(COMPLETE) = lnYear
*! B611045,1 MMT 08/24/2015 Custom Forcasting report calculating Qty based on complete date[T20150820.0008][Start]
*SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)= lcStyle FOR cOrdType ='O' AND SCALE.CSCL_DESC = lcFit AND MONTH(COMPLETE) = lnMonth AND YEAR(COMPLETE) = lnYear 
IF !USED('Ordhdr_ENT')
 =gfOpenTable('ORDHDR','ORDHDR','SH','Ordhdr_ENT')
ENDIF

SELECT ORDLINE
SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)= lcStyle FOR cOrdType ='O' AND SCALE.CSCL_DESC = lcFit 
   =gfSeek(ORDLINE.CORDTYPE+ORDLINE.ORDER,'Ordhdr_ENT')
   *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][Start]
   *IF !(MONTH(Ordhdr_ENT.ENTERED) = lnMonth AND YEAR(Ordhdr_ENT.ENTERED) = lnYear)   
   IF (!(MONTH(Ordhdr_ENT.ENTERED) = lnMonth AND YEAR(Ordhdr_ENT.ENTERED) = lnYear)) OR Ordhdr_ENT.Status ='X'
   *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][End]
     LOOP 
   ENDIF
*! B611045,1 MMT 08/24/2015 Custom Forcasting report calculating Qty based on complete date[T20150820.0008][End]
*! B610581,1 MMT 11/12/2013 Custom forcasting report included EDI orders in Booked QTY[T20131031.0004][END]
  lnPos = ASCAN(laScl,ORDLINE.SCALE)
  lnBokCntr = laScl[lnPos+1]
  
  FOR lnZ = 1 TO SCALE.CNT
    
    *: C201147,3 HES Comment this code for the issue of handling more than 20 sizes [Start]
*!*	    *: C201147,2 HES
*!*	    IF lnBokCntr > 20
*!*	      IF EMPTY(lcChFit)
*!*	        lcChFit = 'EMPTY'
*!*	      ENDIF 
*!*	      MESSAGEBOX('Style (' +lcStyle+ ') with  Fit (' + lcChFit + ') has more than 20 Sizes, The Program will collect just the first 20 sizes.',560,'Unhandled Data!!')
*!*	      llError = .T.
*!*	      EXIT 
*!*	    ENDIF
*!*	    *: C201147,2 HES 
    *: C201147,3 HES Comment this code for the issue of handling more than 20 sizes [End]
    
    *: C201147,3 HES Add new line for the same Style\Color\Fit if its sizes more than 20 [Start]
    IF lnBokCntr > 20
      REPLACE TOTMONTH WITH m.TOTMONTH && To add the Total sales month for Previous group
      m.TOTMONTH = 0
      APPEND BLANK  && New line for the sizes over 20
      lnCntr = lnCntr + 1 && To change the group ID for this new line
      && Update the Group field Just to handle the technique of multi groups for one style
      REPLACE STYLE  WITH lcStyle ,;
              MONTH  with lnMonth ,;
              YEAR   with lnYear  ,;
              FIT    WITH lcFit   ,;
              CMONTH WITH lcMonth ,;
              CYEAR  WITH lcYear  ,;
              GROUP  with ALLTRIM(STR(lnCntr))    
      
      lnBokCntr = 1 && Reset it to 1 for the New line
    ENDIF 
    *: C201147,3 HES Add new line for the same Style\Color\Fit if its sizes more than 20 [End]
    
    lcY = ALLTRIM(STR(lnBokCntr))
    lcZ = ALLTRIM(STR(lnZ))
    m.BOK&lcY = ORDLINE.BOOK&lcZ - ORDCANLN.QTY&lcZ
    SELECT(lcMnthTemp)
    REPLACE BOK&lcY WITH BOK&lcY + m.BOK&lcY
    m.TOTMONTH = m.TOTMONTH + m.BOK&lcY
    lnBokCntr = lnBokCntr + 1
  ENDFOR
  
  
  
  *: C201147,2 HES
  IF llError
    EXIT
  ENDIF 
  *: C201147,2 HES
ENDSCAN

*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[Start]
*Get Styles that has this style as style comp.
lnSelected= SELECT()
=gfSEEK(lcStyle,'Style','Style')
*! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][Start]
*lcScale = SUBSTR(Style.Scale,lnSclPos,lnScaleLen)
lcScale = SUBSTR(Style.Scale,1,lnScaleLen)
*! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][End]
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
  SELECT BOM  
  =gfSqlRun("Select * From BOM where Item Like '"+SUBSTR(lcStyle,1,lnMajLen)+"%' AND CCATGTYP='S'",'BOM')
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]

SELECT Scale 
=gfSeek('S'+lcScale)
DIMENSION laStyleScale[1,4]
laStyleScale = ''
lnCntSty = 1
SCAN REST WHILE TYPE+SCALE+PREPAK ='S'+lcScale FOR SCALE.CSCL_DESC = lcFit
  lcItemScale = Scale.Scale
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
*!*	  SELECT BOM  
*!*	  =gfSqlRun("Select * From BOM where Item Like '"+SUBSTR(lcStyle,1,lnMajLen)+"%' AND CCATGTYP='S'",'BOM')
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
  SELECT BOM
  SCAN FOR '~'+lcItemScale $ mszcrosref AND (SUBSTR(BOM.ITEM,lcClrPos,lnClrLen)= REPLICATE('*',lnClrLen) OR SUBSTR(BOM.ITEM,lcClrPos,lnClrLen)=SUBSTR(lcStyle,lcClrPos,lnClrLen))
    lcCrosRefFld =mszcrosref
    DIMENSION laSizesArr[1]
    laSizesArr = ''
    =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
    FOR lnX = 1 TO ALEN(laSizesArr,1)
      *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][Start]
      if !'~'+lcItemScale $ laSizesArr[lnX]
        loop
      ENdif
      *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][End]
      DIMENSION laStyleScale[lnCntSty ,4]
      laStyleScale[lnCntSty ,1] = PADR(ALLTRIM(BOM.cItmMask),lnMajLen)
      laStyleScale[lnCntSty ,2] = IIF(SUBSTR(BOM.cItmMask,lcClrPos,lnClrLen)= REPLICATE('*',lnClrLen) ,SUBSTR(lcStyle,lcClrPos,lnClrLen),SUBSTR(BOM.cItmMask,lcClrPos,lnClrLen))
      laStyleScale[lnCntSty ,3] = laSizesArr[lnX]
      laStyleScale[lnCntSty ,4] = BOM.nestbomqty
      lnCntSty = lnCntSty + 1
    ENDFOR 
  ENDSCAN 
ENDSCAN   
IF !EMPTY(laStyleScale[1,1])
  FOR lnF =1 TO ALEN(laStyleScale,1)
    lcScanStyle = PADR(laStyleScale[lnF,1],lnMajLen)+lcClrSep+;
                  PADR(laStyleScale[lnF,2],lnClrLen)+lcSclSpr+SUBSTR(laStyleScale[lnF,3],1,lnSclLen)
    lnPosComm =  ATC(',',laStyleScale[lnF,3])                  
    lnParentSize = VAL(SUBSTR(laStyleScale[lnF,3],lnPosComm+1 ,1))
    lcParentScale =SUBSTR(laStyleScale[lnF,3],1,lnSclLen)
    lnPosWng =  ATC('~',laStyleScale[lnF,3])
    lnPosComm =  ATC(',',laStyleScale[lnF,3],2)                  
    lnCompSize =   VAL(SUBSTR(laStyleScale[lnF,3],lnPosComm+1 ,1))             
    lcCompScale =   SUBSTR(laStyleScale[lnF,3],lnPosWng +1,lnSclLen)
    SELECT ORDLINE
    gfSeek(lcScanStyle)
    *! B611045,1 MMT 08/24/2015 Custom Forcasting report calculating Qty based on complete date[T20150820.0008][Start]
    *SCAN REST WHILE STYLE = lcScanStyle FOR MONTH(COMPLETE) = lnMonth AND YEAR(COMPLETE) = lnYear    
    SCAN REST WHILE STYLE = lcScanStyle &&FOR MONTH(COMPLETE) = lnMonth AND YEAR(COMPLETE) = lnYear
      =gfSeek(ORDLINE.CORDTYPE+ORDLINE.ORDER,'Ordhdr_ENT')
      *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][Start]
      *IF !(MONTH(Ordhdr_ENT.ENTERED) = lnMonth AND YEAR(Ordhdr_ENT.ENTERED) = lnYear)
      IF (!(MONTH(Ordhdr_ENT.ENTERED) = lnMonth AND YEAR(Ordhdr_ENT.ENTERED) = lnYear)) OR Ordhdr_ENT.Status ='X'
      *! B611066,1 MMT 10/20/2015 Custom forecasting report calculates sales figures incorrectly[T20150930.0005][End]
        LOOP 
      ENDIF 
    *! B611045,1 MMT 08/24/2015 Custom Forcasting report calculating Qty based on complete date[T20150820.0008][End]
      lnPos = ASCAN(laScl,lcCompScale)
      IF lnPos = 0
        LOOP 
      ENDIF 
      lnBokCntr = laScl[lnPos+1]
      =SEEK(lcStyle+lcFit+ALLTRIM(STR(lnCntr))+STR(lnYear,4)+STR(lnMonth,2),lcMnthTemp)      
      lnZ = lnParentSize
      *FOR lnZ = 1 TO SCALE.CNT
        IF lnBokCntr > 20 AND !SEEK(lcStyle+lcFit+ALLTRIM(STR(lnCntr+1))+STR(lnYear,4)+STR(lnMonth,2),lcMnthTemp)      
          REPLACE TOTMONTH WITH m.TOTMONTH && To add the Total sales month for Previous group
          m.TOTMONTH = 0
          APPEND BLANK  && New line for the sizes over 20
          lnCntr = lnCntr + 1 && To change the group ID for this new line
          REPLACE STYLE  WITH lcStyle ,;
                  MONTH  with lnMonth ,;
                  YEAR   with lnYear  ,;
                  FIT    WITH lcFit   ,;
                  CMONTH WITH lcMonth ,;
                  CYEAR  WITH lcYear  ,;
                  GROUP  with ALLTRIM(STR(lnCntr))    
          lnBokCntr = 1 && Reset it to 1 for the New line
        ENDIF 
        lcY = ALLTRIM(STR(lnBokCntr+lnCompSize-1))
        lcZ = ALLTRIM(STR(lnZ))
        m.BOK&lcY =(ORDLINE.BOOK&lcZ - ORDCANLN.QTY&lcZ)*laStyleScale[lnF,4]
        SELECT(lcMnthTemp)
        REPLACE BOK&lcY WITH BOK&lcY + m.BOK&lcY
        m.TOTMONTH = m.TOTMONTH + m.BOK&lcY
        lnBokCntr = lnBokCntr + 1
     * ENDFOR
      IF llError
        EXIT
      ENDIF 
    ENDSCAN
  ENDFOR
ENDIF
SELECT(lnSelected)
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[End]


*: C201147,2 HES && Comment
SELECT(lcMnthTemp)
REPLACE TOTMONTH WITH m.TOTMONTH
*: C201147,2 HES
  
* End of lfAddMonth()

*!*************************************************************
*! Name      : lfColctData
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009ee
*! Purpose   : Collecting data to fill Temps   
*!*************************************************************
*! Passed Parameters  : STYLE, MONTH, YEAR
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfColctData

*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[Start]
lnScaleLen = gfGetMemVar('M_EXTWIDTH')
lnMajLen =LEN(gfItemMask('PM'))
LnLstSty = 0
DECLARE laItemSeg[1]

PRIVATE lnCount , lnSclLen ,lnClrLen ,lcClrPos,lcClrSep,lcSclSpr
STORE 0 TO lnSclPos ,lcClrPos

*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
IF TYPE('lcXMLFileName') = 'C'
  ItemMask = CREATEOBJECT("GetItemMask")
  =ItemMask.Do(@laItemSeg)
ELSE  
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  =gfItemMask(@laItemSeg)
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
ENDIF
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
FOR lnCount = 1 TO ALEN(laItemSeg,1)
 IF laItemSeg[lnCount,1]='S'
   lnSclLen = LEN(laItemSeg[lnCount,3])
   lnSclPos = laItemSeg[lnCount,4]
 ENDIF
 IF laItemSeg[lnCount,1]='C'
   lnClrLen = LEN(laItemSeg[lnCount,3])
   lcSclSpr = ALLT(laItemSeg[lnCount,6])
   lcClrPos =laItemSeg[lnCount,4]
 ENDIF
 IF laItemSeg[lnCount,1]='F'
   lcClrSep= ALLTRIM(laItemSeg[lnCount,6])
 ENDIF

ENDFOR
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[End]

=lfCretStyTemps()

cStyTemp = lfCheckFilter(1, "STYLE.STYLE", 1)

lnUDCnt = 0
IF !EMPTY(cStyTemp) AND USED(cStyTemp)
  SELECT (cStyTemp)
  COUNT TO lnUDCnt FOR !DELETED() 
ENDIF 

IF EMPTY(cStyTemp) OR (USED(cStyTemp) AND lnUDCnt = 0)
  cStyTemp = gfTempName()
  SELECT STYLE
   lcFlterExpr = "IIF(!EMPTY(laOgVrFlt[lnDivPos,6]),CDIVISION $ laOgVrFlt[lnDivPos,6], .T.) AND " +;
                 "IIF(!EMPTY(laOgVrFlt[lnSesPos,6]),SEASON $ laOgVrFlt[lnSesPos,6], .T.) AND " +;
                 "IIF(!EMPTY(laOgVrFlt[lnGroPos,6]),CSTYGROUP $ laOgVrFlt[lnGroPos,6], .T.) AND " +;
                 "IIF(!EMPTY(laOgVrFlt[lnPatPos,6]),PATTERN $ laOgVrFlt[lnPatPos,6], .T.) AND " +;
                 "IIF(!EMPTY(laOgVrFlt[lnPurPos,6]),CPURCODE $ laOgVrFlt[lnPurPos,6], .T.)"

  lnCnt = 0
  IF !EMPTY(laOgFxFlt[lnFabPos,6])
    SELECT(laOgFxFlt[lnFabPos,6])
    COUNT TO lnCnt FOR !DELETED() 
    IF lnCnt > 0
      lcFlterExpr = lcFlterExpr + " AND SEEK(FABRIC, laOgFxFlt[lnFabPos,6])"
    ENDIF 
  ENDIF 
  
  SELECT STYLE                                           
  COPY Fields STYLE TO (oAriaApplication.WorkDir+cStyTemp) FOR &lcFlterExpr 
  USE (oAriaApplication.WorkDir+cStyTemp) IN 0
  SELECT &cStyTemp
  INDEX ON 'STYLE' TAG &cStyTemp
ENDIF 

*====== > Collecting Data < ==========

SELECT STYLE
SET RELATION TO "S"+SCALE INTO SCALE 
SET RELATION TO SUBSTR(SCALE,1,2) INTO SCALEHD ADDITIVE 
DECLARE laSelStyl[1]
STORE '' TO laSelStyl
lnCout = 1

*: C201147,3 HES
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[Start]
*!*  SELECT BOMLINE
*!*  gfSeek('')
*! B609480,1 MMT 12/16/2010 modify report to get consider if style is component for another style[End]
*: C201147,3 HES


*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
IF !USED('ORDLINE_F')
  =gfOpenTable('ORDLINE','ORDLINES','SH','ORDLINE_F')
ENDIF
IF !USED('ORDHDR_F')
  =gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR_F')
ENDIF
IF !USED('SCALE_F')
  =gfOpenTable('SCALE','SCALE','SH','SCALE_F')
ENDIF
SELECT ORDLINE_F
SET RELATION TO "S"+SCALE INTO SCALE_F
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR_F ADDITIVE 
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]


SELECT (cStyTemp)
LOCATE 

*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
lnDataCnt = RECCOUNT(cStyTemp)
IF TYPE('lcXMLFileName') = 'C'
  loProgress.Description = "Collecting Data..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
ELSE
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [START]
  opross = CREATEOBJECT('ariaprogressbar')  
  oPross.TotalProgress = RECCOUNT()
  oPross.AutoCenter = .F.
  oPross.Top = 0
  lnOldTop = LOOGSCROLL.parent.Top 
  LOOGSCROLL.parent.Top = LOOGSCROLL.parent.Top +  oPross.Top+(oPross.HEight +50)
  opross.Visible = .T.
  oPross.Show()
  lnPrepRec = 0
  *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [END]
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
ENDIF
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
SCAN FOR !EOF() 

  SELECT STYLE
  SET ORDER TO STYLE
  SEEK(&cStyTemp..STYLE)
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  IF Style.Status <> 'A'
    LOOP 
  ENDIF
  *! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[Start]
  *IF Style.nstyfrsprj = 0 AND lnMnPrj = 0
  IF IIF(llRPPJBS,Style.nstyfrsprj = 0,lnMnPrj = 0)
  *! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[END]
    LOOP
  ENDIF
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  IF ASCAN(laSelStyl,SUBSTR(STYLE,1,18)+SCALE.CSCL_DESC) <> 0
    LOOP
  ENDIF 
 
  SELECT (cStyTemp)
  
  m.STYLE = SUBSTR(&cStyTemp..STYLE,1,LEN(&cStyTemp..STYLE)-1)
  
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  *! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[Start]
  *m.nstyfrsprj = IIF(lnMnPrj >0,lnMnPrj,Style.nstyfrsprj)
  m.nstyfrsprj = IIF(!llRPPJBS,lnMnPrj,Style.nstyfrsprj)
  *! C201549,1 MMT 01/21/2013 Enhance custom forecasting report for DCC[End]
  IF TYPE('lcXMLFileName') = 'C'
    lnPerCent = RECNO()/lnDataCnt
    IF MOD(RECNO(),CEILING(lnDataCnt/ 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description =  'Collecting Data for Style : ' + m.STYLE + ' With Fit : ' + SCALE.CSCL_DESC
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    ENDIF
  ELSE
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
    *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [START]
    *WAIT WINDOW NOWAIT 'Collecting Data for Style : ' + m.STYLE + ' With Fit : ' + SCALE.CSCL_DESC
    lnPrepRec = lnPrepRec + 1            
    oPross.CurrentProgress(lnPrepRec)
    oPross.Show()
    oPross.Caption = 'Collecting Data for Style : ' + m.STYLE + ' With Fit : ' + SCALE.CSCL_DESC
    *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [End]
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  ENDIF
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]  
  DIMENSION laSelStyl[lnCout]
  laSelStyl[lnCout] = m.STYLE+SCALE.CSCL_DESC  
  m.DATE = ldprjFrm
  m.COMPANY = oAriaApplication.activecompanyid
  m.USER_ID = oAriaApplication.User_ID
  lnColPos = AT('-',&cStyTemp..STYLE)
  m.STY_COLR = gfCodDes(PADR(SUBSTR(&cStyTemp..STYLE,14,3),6),'COLOR')

  SELECT STYLE
  SET ORDER TO STYLE
  SEEK(&cStyTemp..STYLE)
  
  m.PATTERN = STYLE.PATTERN
  *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[Start]
  *m.SUPPLIER = STYLE.CVENSTY
  m.supplier = style.vendor
  m.stycomp = style.cvensty
  *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[End]
  m.CRTN_QTY = STYLE.QTY_CTN
  m.STY_DESC = STYLE.DESC1
  
  m.FIT = SCALE.CSCL_DESC
  m.DIM1DESC = SCALEHD.CDIM1DESC
  
 *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  lcAlis = SELECT()
  ldProduce = {}
  SELECT  ORDLINE_F
  =gfSeek(m.Style)
  *C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][Start]
  LOCATE REST WHILE STYLE = m.Style FOR SCALE_F.CSCL_DESC =  m.FIT 
  IF !FOUND()
   ldProduce = lfGetProduceDateFromComp(m.Style,m.FIT)
  ELSE
  *C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][End]
  SCAN REST WHILE STYLE = m.Style FOR SCALE_F.CSCL_DESC =  m.FIT 
    *B610911,1 MMT 11/16/2014 Custom PO forecasting report shows wrong totals[T20141113.0010][Start]
    IF EOF('ORDHDR_F')
      LOOP
    ENDIF
    *B610911,1 MMT 11/16/2014 Custom PO forecasting report shows wrong totals[T20141113.0010][End]
    IF EMPTY(ldProduce)
      ldProduce = ORDHDR_F.ENTERED
    ELSE
      IF ORDHDR_F.ENTERED <ldProduce
        ldProduce = ORDHDR_F.ENTERED
      ENDIF
    ENDIF   
  ENDSCAN 
  *C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][Start]
  ENDIF
  *C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][END]
  m.dProduce  = ldProduce
  IF EMPTY(ldProduce)
    LOOP 
  ENDIF
  IF ldProduce > ldprjFrm 
    LOOP 
  ENDIF 
  SELECT(lcAlis)
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]  
  
  
  lnY = 1
  lcY = ''
  lcX = ''
  lnNoSizs = 0
  lntotStk = 0
  lntotWip = 0
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
  m.TotUKWip = 0  
  *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
  *: C201147,3 HES To make a dynamic declaration for STKs, WIPs and SIZs variables [Start]
*!*	  STORE 0 TO m.WIP1,m.WIP2,m.WIP3,m.WIP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.WIP9,m.WIP10,m.WIP11,m.WIP12,m.WIP13,m.WIP14,m.WIP15,m.WIP16,m.WIP17,m.WIP18,m.WIP19,m.WIP20
*!*	  STORE 0 TO m.STK1,m.STK2,m.STK3,m.STK4,m.STK5,m.STK6,m.STK7,m.STK8,m.STK9,m.STK10,m.STK11,m.STK12,m.STK13,m.STK14,m.STK15,m.STK16,m.STK17,m.STK18,m.STK19,m.STK20
*!*	  STORE 0 TO m.TOTSZ1,m.TOTSZ2,m.TOTSZ3,m.TOTSZ4,m.TOTSZ5,m.TOTSZ6,m.TOTSZ7,m.TOTSZ8,m.TOTSZ9,m.TOTSZ10,m.TOTSZ11,m.TOTSZ12,m.TOTSZ13,m.TOTSZ14,m.STK15,m.STK16,m.STK17,m.STK18,m.STK19,m.STK20
*!*	  STORE '' TO m.SIZ1,m.SIZ2,m.SIZ3,m.SIZ4,m.SIZ5,m.SIZ6,m.SIZ7,m.SIZ8,m.SIZ9,m.SIZ10,m.SIZ11,m.SIZ12,m.SIZ13,m.SIZ14,m.SIZ15,m.SIZ16,m.SIZ17,m.SIZ18,m.SIZ19,m.SIZ20
*! B610924,3 MMT 01/18/2015 Fix the bug of incorrect UK WIP in case precedding style has scale count larger than the current style[T20141125.0003][Start]
  STORE 0 TO m.UKWIP1,m.UKWIP2,m.UKWIP3,m.UKWIP4,m.UKWIP5,m.UKWIP6,m.UKWIP7,m.UKWIP8,m.UKWIP9,m.UKWIP10,;
             m.UKWIP11,m.UKWIP12,m.UKWIP13,m.UKWIP14,m.UKWIP15,m.UKWIP16,m.UKWIP17,m.UKWIP18,m.UKWIP19,m.UKWIP20
*! B610924,3 MMT 01/18/2015 Fix the bug of incorrect UK WIP in case precedding style has scale count larger than the current style[T20141125.0003][End]
  lnT = 1
  lcWipCnt   = ''
  lcStkCnt   = ''
  lcTotSzCnt = ''
  lcSizCnt   = ''
  lnRecNom = RECNO()
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
  *SCAN REST WHILE STYLE = m.Style AND SCALE.CSCL_DESC = m.FIT
  SCAN REST WHILE STYLE = m.Style FOR  SCALE.CSCL_DESC = m.FIT
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
    *C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
    *! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][Start]
    *IF llRPUpPl
    IF llRPUpPl OR llRPUpCSV
    *! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][End]
      INSERT INTO 'StyleScle' Values(Style.Style, SCALE.CNT,m.FIT) 
    ENDIF
    *C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
    FOR lnX = 1 TO SCALE.CNT 
      lcWipCnt   = 'm.WIP'   + ALLTRIM(STR(lnT))
      lcStkCnt   = 'm.STK'   + ALLTRIM(STR(lnT))
      lcTotSzCnt = 'm.TOTSZ' + ALLTRIM(STR(lnT))
      lcSizCnt   = 'm.SIZ'   + ALLTRIM(STR(lnT))
      *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
      lcWipUk = 'm.UKWIP'+ ALLTRIM(STR(lnT))
      STORE 0 TO &lcWipUk 
      *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]      
      STORE 0  TO &lcWipCnt   
      STORE 0  TO &lcStkCnt   
      STORE 0  TO &lcTotSzCnt 
      STORE '' TO &lcSizCnt   
      lnT = lnT + 1 && Fixed Counter
    ENDFOR
  ENDSCAN
  GOTO lnRecNom
  lnW = 0 && Variable holds the counter of the TotWip
  lnS = 0 && Variable holds the counter of the TotStk
  lnC = 1 && Counter to be reset every 20
  lnZ = 0 && Variable holds the Counter of Total Sizes for each Group
  lnAllSizes  = 0
  llSecGro    = .F.
  lnNewGrpSzs = 0 && Variable Holds the first SCALE.CNT for the second Group 
  *: C201147,3 HES To make a dynamic declaration for STKs, WIPs and SIZs variables [End]
  
  *: C201147,3 HES reset these needed variables if it has values
  FOR lnM = 0 TO 100 
    lcM = ALLTRIM(STR(lnM))
    IF TYPE('lntotWip&lcM') = 'N' 
      lntotWip&lcM = 0
    ELSE 
      EXIT && Just here because the three variables are the same.
    ENDIF 
    
    IF TYPE('lntotStk&lcM') = 'N' 
      lntotStk&lcM = 0  
    ENDIF 
    
    IF TYPE('lnNoSizs&lcM') = 'N' 
      lnNoSizs&lcM = 0
    ENDIF         
  ENDFOR 
  *: C201147,3 HES reset these needed variables if it has values
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
  *SCAN REST WHILE STYLE = m.Style AND SCALE.CSCL_DESC = m.FIT
  SCAN REST WHILE STYLE = m.Style FOR  SCALE.CSCL_DESC = m.FIT
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
    *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]  
    lfGetUKWIP(Style)
    *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]    
    FOR lnX = 1 TO SCALE.CNT
      lcY = ALLTRIM(STR(lnY))
      lcX = ALLTRIM(STR(lnX))
      *: C201147,3
      lcW = ALLTRIM(STR(lnW))
      lcS = ALLTRIM(STR(lnS))
      lcZ = ALLTRIM(STR(lnZ))
      *: C201147,3
      
      m.WIP&lcY = STYLE.WIP&lcX
      
      *: C201147,3
      *lntotWip = lntotWip + STYLE.WIP&lcX
      IF lnC > 20
        lnNoSizs&lcZ = 20 && We Sure now that this Style\Fit Has more than 20 Sizes
        lnNewGrpSzs = (SCALE.CNT + 1) - lnX
        llSecGro = .T.
        lnW = lnW + 1
        lnS = lnS + 1
        lnZ = lnZ + 1
        lcW = ALLTRIM(STR(lnW))
        lcS = ALLTRIM(STR(lnS))
        lcZ = ALLTRIM(STR(lnZ))
        lnC = 1
      ENDIF 
      && Make a dynamic totals initialization for each group [Start]
      IF TYPE('lntotWip&lcW') = 'U' 
        lntotWip&lcW = 0
      ENDIF 
      lntotWip&lcW = lntotWip&lcW + STYLE.WIP&lcX
      *: C201147,3
      *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[Start]
      *m.STK&lcY = STYLE.STK&lcX
      m.STK&lcY = STYLE.STK&lcX- style.ord&lcX
      *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[End]
      
      *: C201147,3
      *lnTotStk = lntotStk + STYLE.STK&lcX
      IF TYPE('lntotStk&lcS') = 'U' 
        lntotStk&lcS = 0
      ENDIF 
      *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[Start]
      *lntotStk&lcS = lntotStk&lcS + + STYLE.STK&lcX      
      lntotStk&lcS = lntotStk&lcS +  STYLE.STK&lcX - STYLE.ord&lcx
	  *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[End]
      *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]  
      SELECT POSLN_A
      LOCATE 
      DO WHILE !EOF()
        lcKeyPO = CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)
        STORE 0 TO m.QtyUK&lcX  
        SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6) = lcKeyPO
          IF TRANCD <> '1'
            *! B610652,1 SAB 01/05/2014 Fix error in lfCollectData fucntion [T20131209.0001][Start]
            *m.QtyUK&lcX. = m.QtyUK&lcCt. + MAX(m.QtyUK&lcX.-POSLN_A.QTY&lcX. ,0 ) 
            *! B610924,2 MMT 01/18/2015 Fix the bug of incorrect UK WIP[T20141125.0003][Start]
            *m.qtyuk&lcx. = m.qtyuk&lcx. + MAX(m.qtyuk&lcx.-posln_a.qty&lcx. ,0 )
            m.qtyuk&lcx. = MAX(m.qtyuk&lcx.-posln_a.qty&lcx. ,0 )
            *! B610924,2 MMT 01/18/2015 Fix the bug of incorrect UK WIP[T20141125.0003][End]
            *! B610652,1 SAB 01/05/2014 Fix error in lfCollectData fucntion [T20131209.0001][End]
          ELSE
            m.QtyUK&lcX. = m.QtyUK&lcX. + POSLN_A.QTY&lcX. 
          ENDIF  
        ENDSCAN 
        m.UKWip&lcY = m.UKWip&lcY +  m.QtyUK&lcX. 
        IF TYPE('m.TOTUKWIP') <> 'N'
          m.TOTUKWIP = 0
        ENDIF
        m.TOTUKWIP = m.TOTUKWIP + m.QtyUK&lcX. 
      ENDDO
      *! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]        
      lnC = lnC + 1 
      *: C201147,3
      
      m.SIZ&lcY = SCALE.SZ&lcX
      lnY = lnY + 1      
    ENDFOR
    
    *: C201147,3 HES
    IF TYPE('lnNoSizs&lcZ') = 'U'
      lnNoSizs&lcZ = 0
    ENDIF 
    IF llSecGro
      lnNoSizs&lcZ = lnNewGrpSzs
      llSecGro = .F.
    ELSE 
      lnNoSizs&lcZ = lnNoSizs&lcZ + SCALE.CNT
    ENDIF 
    *: C201147,3 HES
  ENDSCAN
  
  *: C201147,3 HES SUM all sizes
  FOR lnB = 0 TO lnZ && because the Group is Zero Indexed
    lcB = ALLTRIM(STR(lnB))
    lnAllSizes = lnAllSizes + lnNoSizs&lcB
  ENDFOR 
  *: C201147,3 HES SUM all sizes
  
  *: C201147,3 HES Specify how many Groups we'll work with and number of sizes included within these groups [Start]
  lnGroups  = INT((lnY-1)/20)
  lnGrpsSzs = lnAllSizes
  *: C201147,3 HES Specify how many Groups we'll work with and number of sizes included within these groups [End]
  
  *: C201147,3 HES We already make a dynamic totals initialization for each group [Start]
*!*	  m.TOTWIP = lnTotWip 
*!*	  m.TOTSTK = lnTotStk
*!*	  m.TOT_SIZ = lnNoSizs
  *: C201147,3 HES We already make a dynamic totals initialization for each group [End]
  
  lnMonth = MONTH(ldprjFrm)
  lnYear = YEAR(ldprjFrm)
  
  FOR lnY = 1 TO lnMnSls
    lnCnt = lnMonth+1 - lnY
    IF lnCnt <= 0
      lnYear = lnYear - 1
      m.YEAR = lnYear 
      m.MONTH = lnCnt + 12
      lfAddMonth(m.Style,m.MONTH,m.YEAR,m.FIT)
      lnYear = lnYear + 1
    ELSE
      m.YEAR = lnYear
      m.MONTH = lnCnt
      lfAddMonth(m.Style,m.MONTH,m.YEAR,m.FIT)
    ENDIF 
  ENDFOR
  
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
  IF MONTH(ldProduce) = MONTH(ldprjFrm) AND YEAR(ldProduce) = YEAR(ldprjFrm)
    lnCnt = 1
  ELSE 
    ldNextMnth = GOMONTH(ldProduce,1) 
    lnCnt =2
    *B610330,1 MMT 05/12/2013 incorrect Average monthly qty in custom forecasting report[T20110117.0005][Start]
    *DO WHILE MONTH(ldNextMnth) <> MONTH(ldprjFrm)
    DO WHILE MONTH(ldNextMnth) <> MONTH(ldprjFrm) OR YEAR(ldNextMnth) <> YEAR(ldprjFrm)
    *B610330,1 MMT 05/12/2013 incorrect Average monthly qty in custom forecasting report[T20110117.0005][End]
      ldNextMnth = GOMONTH(ldNextMnth ,1) 
      lnCnt = lnCnt + 1 
    ENDDO 
  ENDIF   
  m.nSlsMon = lnCnt &&CEILING((ldprjFrm - ldProduce)/30)
  m.nSlsMon = MIN(lnMnSls , m.nSlsMon)
  *! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  lnTotBokQty = 0
  lnTotSizs = 0
  m.TOTSZS = 0
  
  *: C201147,3 HES && Will be handled at the end [Start]
*!*	  IF m.TOT_SIZ > 0
*!*	    SELECT (lcMnthTemp)
*!*	    LOCATE 
*!*	      *: C201147,2 HES
*!*	      * FOR lnY = 1 TO m.TOT_SIZ
*!*	      FOR lnY = 1 TO IIF(m.TOT_SIZ <= 20, m.TOT_SIZ, 20)
*!*	        *: C201147,2 HES
*!*	        lcY = ALLTRIM(STR(lnY))
*!*	        SUM BOK&lcY TO lnTotBokQty FOR STYLE = m.Style AND FIT = m.FIT 
*!*	        m.TOTSZ&lcY = lnTotBokQty
*!*	        lnTotSizs = lnTotSizs + lnTotBokQty
*!*	      ENDFOR 
*!*	      m.TOTSZS = lnTotSizs
*!*	  ENDIF  
  *: C201147,3 HES Will be handled at the end [End]
  
  *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[Start]  
*!*    SELECT BOMLINE
*!*    LOCATE FOR STYLE = m.STYLE AND CCATGTYP = 'S'
*!*	  IF FOUND()
*!*	    m.STYCOMP = SUBSTR(BOMLINE.ITEM,1,12)
*!*	  ENDIF 
  *! B609480,1 MMT 12/15/2010 Fix bugs reported bu customer[END]
  *: C201147,3 HES adjust all memory variables to be added in the appropriate places in new lines [Start]
  lcA = ''
  lcX = ''
  lcG = ''
  
  FOR lnG = 0 TO lnGroups && Scan to add new line with its appropriate data for each group
    lcG = ALLTRIM(STR(lnG))
    FOR lnY = 1 TO lnNoSizs&lcG
      lnX = lnY + lnG*20
      lcX = ALLTRIM(STR(lnX))
      lcY = ALLTRIM(STR(lnY))
      m.WIP&lcY = m.WIP&lcX
      m.STK&lcY = m.STK&lcX
      m.SIZ&lcY = m.SIZ&lcX
    ENDFOR
    m.TotWip  = lnTotWip&lcG && Update the total WIP for each group
    m.TotStk  = lnTotStk&lcG && Update the total STK for each group
    m.TOT_SIZ = lnNoSizs&lcG && Update the total STK for each group
    m.GROUP = lcG
    IF lnNoSizs&lcG  < 20
      FOR x = 1 + lnNoSizs&lcG TO 20
        lcCX = ALLTRIM(STR(x))
        m.SIZ&lcCX = ''
        m.WIP&lcCX = 0
        m.STK&lcCX = 0
      ENDFOR 
    ENDIF
    *: C201147,3 HES adjust all memory variables to be added in the appropriate places in new lines [End]

    INSERT INTO &lcActTemp FROM MEMVAR 

  *: C201147,3 HES
  ENDFOR 
  *: C201147,3 HES
  
  *: C201147,3 HES SUM all BOOK quantities for each Group [Start]
  IF lnAllSizes > 0
    SELECT (lcActTemp)
    LOCATE 
    *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [Start]
    *SCAN FOR !EOF() 
    SCAN FOR Style = m.Style AND FIT = m.FIT
    *: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [End]
      && Handle HASMORE Flag
      SKIP 
      IF VAL(GROUP) > 0
        SKIP -1 
        REPLACE HASMORE WITH .T.
      ELSE 
        SKIP -1
      ENDIF 
      && Handle HASMORE Flag
      
      lnGroupSz = TOT_SIZ
      lnGroupID = GROUP
      lnTotSizs = 0
      SELECT (lcMnthTemp)
      LOCATE 
      
      FOR lnY = 1 TO lnGroupSz
        lcY = ALLTRIM(STR(lnY))
        SUM BOK&lcY TO lnTotBokQty FOR STYLE = m.Style AND FIT = m.FIT AND GROUP = lnGroupID
        
        SELECT (lcActTemp)
        REPLACE TOTSZ&lcY WITH lnTotBokQty && Update the total Booked quantities for each size in each group
        lnTotSizs = lnTotSizs + lnTotBokQty
        
        SELECT (lcMnthTemp)
      ENDFOR 
      SELECT (lcActTemp)
      REPLACE TOTSZS WITH lnTotSizs
 
    ENDSCAN
  ENDIF  
  *: C201147,3 HES SUM all BOOK quantities for each Group [End  ]
  
  lnCout = lnCout + 1
ENDSCAN

*: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [START]
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
IF TYPE('lcXMLFileName') <> 'C' 
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
  oPross = null
  LOOGSCROLL.parent.Top = lnOldTop  
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
ENDIF
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
*: C201147,4 MMT 09/15/2009 Fix bug of wrong totals [START]

SELECT (lcActTemp)
lnReCont = RECCOUNT()
IF lnReCont > 0
  RETURN .T.
ELSE 
  RETURN .F.
ENDIF 
* End of lfColctData()

*!*************************************************************
*! Name      : lfGetNewOrd
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Retuen the New Order  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfGetNewOrd
PARAMETERS lnIdx 

lnReturn1 = 0
lcIdx = IIF(lnIdx < 10, STR(lnIdx,1), STR(lnIdx,2))
lnFrstValue = &lcActTemp..WIP&lcIdx + &lcActTemp..STK&lcIdx
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]         
*lnValue = lnFrstValue  / IIF(IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1) <> 0 , ;
                             IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1),1)
*lnSubValue = lnMnPrj - lnValue                             
*lnReturn1 = IIF(lnValue < lnMnPrj , lnSubValue * IIF(IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1) <> 0 , ;
                                                     IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1),1) , 0)
lnValue = lnFrstValue  / IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                             IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1)

lnSubValue = &lcActTemp..nstyfrsprj - lnValue

lnReturn1 = IIF(lnValue < &lcActTemp..nstyfrsprj , lnSubValue * IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                                                     IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1) , 0)
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]
*! B610924,1 MMT 01/12/2015 Fix the bug of incorrect new order qty if there is no sales done[T20141125.0003][Start]
*IF lnReturn1 = 0 OR lnReturn1 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx)
IF lnReturn1 = 0 OR lnReturn1 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx) OR &lcActTemp..TOTSZ&lcIdx = 0
*! B610924,1 MMT 01/12/2015 Fix the bug of incorrect new order qty if there is no sales done[T20141125.0003][End]
  lnReturn1 = ''
ENDIF 

RETURN lnReturn1 
* End of lfGetNewOrd

*!*************************************************************
*! Name      : lfGetMnStk
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : retun the Month Stock  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfGetMnStk
PARAMETERS lnIdx

lnReturn2 = 0
lcIdx = IIF(lnIdx < 10, STR(lnIdx,1), STR(lnIdx,2))
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[Start]
*!*	lnReturn2 = (&LCACTTEMP..WIP&lcIdx + ;
*!*	             &LCACTTEMP..STK&lcIdx) / IIF(IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1) <> 0 , ;
*!*	                                          IIF(lnMnSls > 0, &lcActTemp..TOTSZ&lcIdx / lnMnSls, 1),1)
lnReturn2 = (&LCACTTEMP..WIP&lcIdx + ;
             &LCACTTEMP..STK&lcIdx) / IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                                          IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1)
*! C201348,1 MMT 05/29/2011 Modify Report to work from request builder and some other modification for DCC[End]

IF lnReturn2 = 0 OR lnReturn2 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx)
  lnReturn2 = ''
ENDIF 

RETURN lnReturn2 

* End of lfGetMnStk

*!*************************************************************
*! Name      : lfGetMnStk
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : retun the Month Stock  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfVldDate

IF ldprjFrm > DATE()
  gfModalGen('TRM40135B00000','Alert')
  ldprjFrm = DATE()
  RETURN .F.
ENDIF 
* End of lfVldDate

*!*************************************************************
*! Name      : lfSetFun
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/20/2009
*! Purpose   : Set Function for Browsing the Style File  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfSetFun
PARAMETERS lcParam

DO CASE
CASE lcParam = 'S'
  lcFltExp =  'IIF(!EMPTY(laOgVrFlt[lnDivPos,6]),CDIVISION $ laOgVrFlt[lnDivPos,6], .T.) and '+;
              'IIF(!EMPTY(laOgVrFlt[lnSesPos,6]),SEASON $ laOgVrFlt[lnSesPos,6], .T.)    and '+;
              'IIF(!EMPTY(laOgVrFlt[lnGroPos,6]),CSTYGROUP $ laOgVrFlt[lnGroPos,6], .T.) and '+;
              'IIF(!EMPTY(laOgVrFlt[lnPatPos,6]),PATTERN $ laOgVrFlt[lnPatPos,6], .T.)   and '+;
              'IIF(!EMPTY(laOgVrFlt[lnPurPos,6]),CPURCODE $ laOgVrFlt[lnPurPos,6], .T.)  and '+;
              'IIF(!EMPTY(laOgFxFlt[lnFabPos,6]) and RECCOUNT(laOgFxFlt[lnFabPos,6]) > 0 ,   '+;
              'SEEK(FABRIC, laOgFxFlt[lnFabPos,6]), .T.) '
              
  SELECT STYLE 
  SET FILTER TO &lcFltExp
OTHERWISE
ENDCASE
*! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
*!*************************************************************
*! Name      : lfGetDefPur
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/22/2011
*! Purpose   : Get Default Purchase group code
*!*************************************************************
FUNCTION lfGetDefPur
lcRetPVal  = ''
IF !USED('CODES_A')
  =gfOpenTable('CODES','CCODE_NO','SH','CODES_A')
ENDIF
*! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
*IF gfSEEK('D'+'CPURCODE','CODES_A','CCODE_NO')
IF gfSEEK('D'+'CFRCSTGRP','CODES_A','CCODE_NO')
*! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]
  lcRetPVal  = CODES_A.CCODE_NO
ENDIF
RETURN (lcRetPVal)
*!*************************************************************
*! Name      : lfGetUKWIP
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/22/2011
*! Purpose   : GET UK OPEN POs
*!*************************************************************
FUNCTION lfGetUKWIP
LPARAMETERS lcStyle
lnOldSel = SELECT()
IF !USED('POSLN_A')
  =gfOpentable('POSLN','POSLN','SH','POSLN_A')
ENDIF
SELECT 'POSLN_A' 
*! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[Start]
*!*  =gfSQLRun("Select POSLN.PO,POSLN.cStytype,POSLN.CINVTYPE,POSLN.STYLE,POSLN.[LINENO],POSLN.cBusdocu  ,POSLN.QTY1,POSLN.QTY2 ,POSLN.QTY3 ,POSLN.QTY4 ,POSLN.QTY5 ,POSLN.QTY6 ,POSLN.QTY7 ,POSLN.QTY8,POSLN.TRANCD "+;
*!*            " From POSLN INNER JOIN POSHDR ON  POSLN.cBusdocu = POSHDR.cBusdocu and POSLN.CSTYTYPE = POSHDR.cSTYTYPE"+;
*!*            " AND POSLN.PO = POSHDR.PO WHERE POSHDR.cStytype = 'P' and POSHDR.cBusdocu = 'P' AND POSHDR.CPURCODE = '"+lcRpPurc+"'   and POSLN.STYLE = '"+lcStyle+"' AND POSLN.TRANCD IN ('1','2','4','5') AND POSHDR.STATUS ='O'",'POSLN_A')
=gfSQLRun("Select POSLN.PO,POSLN.cStytype,POSLN.CINVTYPE,POSLN.STYLE,POSLN.[LINENO],POSLN.cBusdocu  ,POSLN.QTY1,POSLN.QTY2 ,POSLN.QTY3 ,POSLN.QTY4 ,POSLN.QTY5 ,POSLN.QTY6 ,POSLN.QTY7 ,POSLN.QTY8,POSLN.TRANCD "+;
          " From POSLN INNER JOIN POSHDR ON  POSLN.cBusdocu = POSHDR.cBusdocu and POSLN.CSTYTYPE = POSHDR.cSTYTYPE"+;
          " AND POSLN.PO = POSHDR.PO WHERE POSHDR.cStytype = 'P' and POSHDR.cBusdocu = 'P' AND POSHDR.CFRCSTGRP= '"+lcRpPurc+"'   and POSLN.STYLE = '"+lcStyle+"' AND POSLN.TRANCD IN ('1','2','4','5') AND POSHDR.STATUS ='O'",'POSLN_A')
*! C201387,2 MMT 11/12/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]          
SELECT(lnOldSel)

*!*************************************************************
*! Name      : lfGetMnStkUK
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/22/2011
*! Purpose   : retun the UK Month Stock  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfGetMnStkUK
PARAMETERS lnIdx
lnReturn2 = 0
lcIdx = IIF(lnIdx < 10, STR(lnIdx,1), STR(lnIdx,2))
lnReturn2 = (&LCACTTEMP..UKWIP&lcIdx + ;
             &LCACTTEMP..STK&lcIdx) / IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                                          IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1)
IF lnReturn2 = 0 OR lnReturn2 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx)
  lnReturn2 = ''
ENDIF 
RETURN lnReturn2 

*!*************************************************************
*! Name      : lfGetNewOrdUK
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/22/2011
*! Purpose   : Retuen the New Order  
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
FUNCTION lfGetNewOrdUK
PARAMETERS lnIdx 

lnReturn1 = 0
lcIdx = IIF(lnIdx < 10, STR(lnIdx,1), STR(lnIdx,2))
lnFrstValue = &lcActTemp..UKWIP&lcIdx + &lcActTemp..STK&lcIdx
lnValue = lnFrstValue  / IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                             IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1)

lnSubValue = &lcActTemp..nstyfrsprj - lnValue

lnReturn1 = IIF(lnValue < &lcActTemp..nstyfrsprj , lnSubValue * IIF(IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1) <> 0 , ;
                                                     IIF(&lcActTemp..nSlsMon > 0, &lcActTemp..TOTSZ&lcIdx / &lcActTemp..nSlsMon, 1),1) , 0)
*B610924,1 MMT 01/12/2015 Fix the bug of incorrect new order qty if there is no sales done[T20141125.0003][Start]                                                     
*IF lnReturn1 = 0 OR lnReturn1 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx)
IF lnReturn1 = 0 OR lnReturn1 = 0.00 OR EMPTY(&lcActTemp..SIZ&lcIdx) OR &lcActTemp..TOTSZ&lcIdx=0
*B610924,1 MMT 01/12/2015 Fix the bug of incorrect new order qty if there is no sales done[T20141125.0003][End]
  lnReturn1 = ''
ENDIF 

RETURN lnReturn1 
*! C201387,1 MMT 09/22/2011 Add new option in OG UK WIP Purchasing Group and Collect data based on it[END]  
*! C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][Start]
*!*************************************************************
*! Name      : lfGetProduceDateFromComp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 12/18/2014
*! Purpose   : Get Produce date from styles that current style is component in
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : Produce date
*!*************************************************************
FUNCTION  lfGetProduceDateFromComp
LPARAMETERS lcSeleStyle, lcSeleFIT
STORE {} to ldReturnDate
=gfSEEK(lcSeleStyle,'Style','Style')
lcScale = SUBSTR(Style.Style,lnSclPos,lnScaleLen)
*! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
SELECT BOM  
=gfSqlRun("Select * From BOM where Item Like '"+SUBSTR(lcSeleStyle,1,lnMajLen)+"%' AND CCATGTYP='S'",'BOM')
*! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
SELECT SCALE_F
=gfSeek('S'+lcScale)
DIMENSION laStyleScale[1,4]
laStyleScale = ''
lnCntSty = 1
SCAN REST WHILE TYPE+SCALE+PREPAK ='S'+lcScale FOR SCALE_F.CSCL_DESC = lcSeleFIT
  lcItemScale = SCALE_F.Scale
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][Start]
*!*	SELECT BOM  
*!*	=gfSqlRun("Select * From BOM where Item Like '"+SUBSTR(lcSeleStyle,1,lnMajLen)+"%' AND CCATGTYP='S'",'BOM')
  *! C201697,1 MMT 07/16/2015 Add Update Plan option to custom forcasting report[T20150629.0006][End]
SELECT BOM
  SCAN FOR '~'+lcItemScale $ mszcrosref AND (SUBSTR(BOM.ITEM,lcClrPos,lnClrLen)= REPLICATE('*',lnClrLen) OR SUBSTR(BOM.ITEM,lcClrPos,lnClrLen)=SUBSTR(lcSeleStyle,lcClrPos,lnClrLen))
    lcCrosRefFld =mszcrosref
    DIMENSION laSizesArr[1]
    laSizesArr = ''
    =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
    FOR lnX = 1 TO ALEN(laSizesArr,1)
*!*        lnPosComm =  ATC(',',laSizesArr[lnX])                  
*!*        lnParentSize = VAL(SUBSTR(laSizesArr[lnX],lnPosComm+1 ,1))
*!*        lcParentScale =SUBSTR(laSizesArr[lnX],1,lnSclLen)
*!*        lnPosWng =  ATC('~',laSizesArr[lnX])
*!*        lnPosComm =  ATC(',',laSizesArr[lnX],2)                  
*!*        lnCompSize =   VAL(SUBSTR(laSizesArr[lnX],lnPosComm+1 ,1))             
*!*        lcCompScale =   SUBSTR(laSizesArr[lnX],lnPosWng +1,lnSclLen)
        IF "~"+SCALE.Scale $ laSizesArr[lnX]
          DIMENSION laStyleScale[lnCntSty ,4]
          laStyleScale[lnCntSty ,1] = PADR(ALLTRIM(BOM.cItmMask),lnMajLen)
          laStyleScale[lnCntSty ,2] = IIF(SUBSTR(BOM.cItmMask,lcClrPos,lnClrLen)= REPLICATE('*',lnClrLen) ,SUBSTR(lcSeleStyle,lcClrPos,lnClrLen),SUBSTR(BOM.cItmMask,lcClrPos,lnClrLen))
          laStyleScale[lnCntSty ,3] = laSizesArr[lnX]
          laStyleScale[lnCntSty ,4] = BOM.nestbomqty
          lnCntSty = lnCntSty + 1
        ENDIF          
    ENDFOR 
ENDSCAN
ENDSCAN
IF !EMPTY(laStyleScale[1,1])
  IF USED('CompStyles')
    USE IN 'CompStyles' 
  ENDIF
  CREATE CURSOR 'CompStyles' (Style C(19)) 
  SELECT 'CompStyles' 
  INDEX ON STYLE TAG 'CompStyles' 
  FOR lnF =1 TO ALEN(laStyleScale,1)
    lcScanStyle = PADR(laStyleScale[lnF,1],lnMajLen)+lcClrSep+;
                  PADR(laStyleScale[lnF,2],lnClrLen)+lcSclSpr+SUBSTR(laStyleScale[lnF,3],1,lnSclLen)
    IF !SEEK(lcScanStyle,'CompStyles' ,'CompStyles')                     
      INSERT INTO 'CompStyles' (Style) values(lcScanStyle)
    ENDIF
  ENDFOR                  
  IF USED('CompStyles') AND RECCOUNT('CompStyles')>0
    SELECT 'CompStyles'
    LOCATE
    SCAN 
      SELECT  ORDLINE_F
      =gfSeek(CompStyles.Style)
      SCAN REST WHILE STYLE = CompStyles.Style &&FOR SCALE_F.CSCL_DESC =  m.FIT 
        *B610911,1 MMT 11/16/2014 Custom PO forecasting report shows wrong totals[T20141113.0010][Start]
        IF EOF('ORDHDR_F')
          LOOP
        ENDIF
        *B610911,1 MMT 11/16/2014 Custom PO forecasting report shows wrong totals[T20141113.0010][End]
        IF EMPTY(ldReturnDate)
          ldReturnDate= ORDHDR_F.ENTERED
        ELSE
          IF ORDHDR_F.ENTERED <ldReturnDate
            ldReturnDate= ORDHDR_F.ENTERED
          ENDIF
        ENDIF   
      ENDSCAN 
    ENDSCAN
  ENDIF
ENDIF

RETURN ldReturnDate
*! C201647,1 MMT 12/18/2014 Modify forecasting report to include component styles[T20141125.0003][End]
*C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][Start]
*!*************************************************************
*! Name      : lfvAutoClc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 10/22/2015
*! Purpose   : Get Produce date from styles that current style is component in
*!*************************************************************
*C201720 
FUNCTION lfvAutoClc
ldprjFrm = DATE(YEAR(DATE()),MONTH(DATE()),1)-1
*C201720,1 MMT 10/22/2015 Modify Custom Forecasting report to auto. calc. projection date[T20150918.0001][End]

*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][Start]
*!*************************************************************
*! Name      : lfUpdateOTSCSV
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/15/2016
*! Purpose   : Update OTS CSV File
*!*************************************************************
*! C201793
FUNCTION lfUpdateOTSCSV
IF !USED('Style_OTS')
  =gfOpenTable('STYLE','STYLE','SH','Style_OTS')
ENDIF
CREATE CURSOR 'TmpSty' (Style C(19), Vendor C(8),OTS1 N(12), OTS2 N(12), OTS3 N(12), OTS4 N(12), OTS5 N(12), OTS6 N(12),  OTS7 N(12), OTS8 N(12), TOTOTS N(14))
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]
CREATE CURSOR 'TmpForc'(Style C(19),Month N(2),YEAR N(4), cMonth C(15),cYear C(4),Qty1 N(12), Qty2 N(12), Qty3 N(12), Qty4 N(12), Qty5 N(12), Qty6 N(12),  Qty7 N(12), Qty8 N(12), TOTQty N(14))
SELECT 'TmpForc'
INDEX On Style  + STR(Year,4) + STR(Month,2) TAG 'TmpForc'
IF FILE(ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv")
  APPEND FROM (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv") TYPE CSV
ENDIF
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
SELECT 'TmpSty' 
INDEX On Style + Vendor TAG 'TmpSty' 
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]  
*!*	IF FILE(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
*!*	  APPEND  FROM  (ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv") TYPE CSV  
IF FILE(ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv")
  APPEND  FROM  (ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv") TYPE CSV  
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
ENDIF
SELECT(lcActTemp)
LOCATE 
SCAN FOR !DELETED()   
  SELECT 'StyleScle' 
  LOCATE FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcActTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcActTemp..FiT AND !DELETED()
  IF FOUND()
    lnCntSz = 1
    SCAN REST FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcActTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcActTemp..FiT AND !DELETED()
      IF gfSeek(StyleScle.StyCode,'Style_OTS','Style')
        STORE 0 TO lnNewOrdQty1,lnNewOrdQty2,lnNewOrdQty3,lnNewOrdQty4,lnNewOrdQty5,lnNewOrdQty6,lnNewOrdQty7,lnNewOrdQty8,lnNewOrdTotQty
        FOR lnI = 1 TO StyleScle.Cnt
          IF lnCntSz > 20
            EXIT 
          ENDIF
          lcI = STR(lnI,1)
          lnRetValue = lfGetNewOrd(lnCntSz)
          lnNewOrdQty&lcI. = IIF(TYPE('lnRetValue')<>'N',0,lnRetValue)
          lnCntSz = lnCntSz +  1
          IF lnCntSz > 20
            EXIT 
          ENDIF
        ENDFOR  
        lnNewOrdTotQty = lnNewOrdQty1+lnNewOrdQty2+lnNewOrdQty3+lnNewOrdQty4+lnNewOrdQty5+lnNewOrdQty6+lnNewOrdQty7+lnNewOrdQty8
        IF lnNewOrdTotQty <> 0
          IF !SEEK(Style_OTS.Style+IIF(EMPTY(ALLTRIM(Style_OTS.Vendor)),'TBA',Style_OTS.Vendor),'TmpSty','TmpSty')
            INSERT INTO 'TmpSty' (Style , Vendor ) VALUES (Style_OTS.Style,IIF(EMPTY(ALLTRIM(Style_OTS.Vendor)),'TBA',Style_OTS.Vendor)) 
          ENDIF
          REPLACE OTS1 WITH -1 * lnNewOrdQty1,;
            	  OTS2 WITH -1 * lnNewOrdQty2,;
            	  OTS3 WITH -1 * lnNewOrdQty3,;
            	  OTS4 WITH -1 * lnNewOrdQty4,;
            	  OTS5 WITH -1 * lnNewOrdQty5,;
            	  OTS6 WITH -1 * lnNewOrdQty6,;
            	  OTS7 WITH -1 * lnNewOrdQty7,;
            	  OTS8 WITH -1 * lnNewOrdQty8,;
            	  TOTOTS WITH -1 * lnNewOrdTotQty IN 'TmpSty'
		ENDIF
	  ENDIF	              
    ENDSCAN
  ENDIF
ENDSCAN 
SELECT 'TmpSty'
SET ORDER TO 
LOCATE
IF !EOF()
  *! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]
  *=STRTOFILE(CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)  
  =STRTOFILE(CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",0)  
  *! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
  SCAN FOR !EMPTY(ALLTRIM(Style))
    lcString = ""+Style+","+vendor+","+ALLTRIM(STR(OTS1))+","+ALLTRIM(STR(OTS2))+","+;
                  ALLTRIM(STR(OTS3))+","+ALLTRIM(STR(OTS4))+","+ALLTRIM(STR(OTS5))+","+ALLTRIM(STR(OTS6))+","+;
                  ALLTRIM(STR(OTS7))+","+ALLTRIM(STR(OTS8))+","+ALLTRIM(STR(TOTOTS))
    *! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]              
    *=STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)   
    =STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"PO.csv",1)   
    *! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
  ENDSCAN
ENDIF
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][Start]
SELECT(lcMnthTemp)
LOCATE 
SCAN FOR !DELETED()
  SELECT 'StyleScle' 
  LOCATE FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcMnthTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcMnthTemp..FiT AND !DELETED()
  IF FOUND()
    lnCntSz = 1
    SCAN REST FOR SUBSTR(StyCode,1,LEN(StyCode)-1) = SUBSTR(&lcMnthTemp..Style ,1,LEN(StyleScle.StyCode)-1) AND fit =  &lcMnthTemp..FiT AND !DELETED()
      STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
      m.Style = StyleScle.StyCode
      m.cMonth = &lcMnthTemp..CMONTH
      m.cYear = &lcMnthTemp..CYEAR
      m.Month = &lcMnthTemp..Month
      m.YEAR = &lcMnthTemp..YEAR
      FOR lnI = 1 TO StyleScle.Cnt
        IF lnCntSz > 20
          EXIT 
        ENDIF
        lcI = STR(lnI,1)
        m.Qty&lcI. = &lcMnthTemp..BOK&lcI.
        m.TotQty = m.TotQty + m.Qty&lcI. 
        lnCntSz = lnCntSz +  1
        IF lnCntSz > 20
          EXIT 
        ENDIF
      ENDFOR
      IF SEEK(m.Style+STR(m.Year,4)+STR(m.Month,2),'TmpForc','TmpForc')
        SELECT 'TmpForc'
        GATHER MEMO MEMVAR
      ELSE
        INSERT INTO 'TmpForc' FROM MEMVAR 
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
SELECT 'TmpForc'
LOCATE
IF !EOF()
  =STRTOFILE(CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv",0)  
  SCAN FOR !EMPTY(ALLTRIM(Style)) AND Month <> 0
    lcString = ""+Style+","+STR(Month ,2)+","+STR(YEAR,4) +","+cMonth+","+cYear+","+ALLTRIM(STR(QTY1))+","+ALLTRIM(STR(Qty2))+","+;
                  ALLTRIM(STR(Qty3))+","+ALLTRIM(STR(Qty4))+","+ALLTRIM(STR(Qty5))+","+ALLTRIM(STR(Qty6))+","+;
                  ALLTRIM(STR(Qty7))+","+ALLTRIM(STR(Qty8))+","+ALLTRIM(STR(TOTQTY))
    =STRTOFILE(lcString +CHR(13)+CHR(10),ADDBS(ADDBS(oAriaApplication.RESOURCEHOME)+ALLTRIM(OAriaApplication.ActiveCompanyID))+ALLTRIM(OAriaApplication.User_ID)+"POFORECAST.csv",1)   
  ENDSCAN
ENDIF
*! C201833,1 MMT 06/16/2016 Export the report data to CSV File to be used by Auto create PO screen[P20160510.0001-Issue#4-Point#12][End]
SELECT(lcActTemp)
LOCATE               
*! C201793,1 MMT 03/15/2016 Entity#9 - Modify custom forcasting report to update OTS CSV File[P20160119.0001][End]