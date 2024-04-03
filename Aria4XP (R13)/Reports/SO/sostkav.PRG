*:***************************************************************************
*: Program file  : SOSTKAV
*: Program desc. : Stock Availiablity Report
*: For Report    : (....)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 06/28/2006 (037675)
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrnCode,DISP_TITLES,ORD971,ORD972,SUB_ORDQTY,ACUM_OTS,
*:               : POST_NVE_VALUS,POST_BF,DISP_OTS_LINES,lpUpdOTS
*:    Functions  : gfModalGen,gfItemMask,gfThermo,gfCodDes,FaBrow,gfRange.
*:               : lfwRepWhen,lfStyTemp,lfwOldVal,lfEvalSegs,lfSRVSty,lfStySum,
*:               : lfvFabric,lfClearRep,lfCollTime,lfGtYTD.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Modification:
*!* B609356,1 SMA 07/26/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!* B610448,1 HIA 07/28/13 T20130723.0008 - Stock Availablity report problem:some colors are not showing up.
*!* E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014]
*!* E303691,1 derby 07/03/2016 - Convert Stock Availabilty report to work on request builder[T20160626.0017]
*!* B612418,1 MMT 06/27/2021 The stock availability report does not consider the shipments Qty done on POs in the report calculations[T20210518.0001]
*!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002]
*!* B612489,1 MMT 11/10/2021 Stock Availability report Excel format does not have clear columns names in case of Print Sizes option = No[T20211108.0002]
*:***************************************************************************
*: Example : DO SOSTKAV
*:***************************************************************************
#INCLUDE R:\ARIA4XP\REPORTS\SO\SOSTKAV.H
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
PARAMETERS lcRequestID, lcXMLFileName, ClientID
IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId
  PRIVATE loAgent
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  LOCAL loEnvironment
  
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  
  loEnvironment.ClientID = ClientID
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

  PUBLIC gcAct_Appl
  gcAct_Appl  = 'SO'
  oAriaEnvironment.REPORT.gcAct_Appl = 'SO'
  oAriaEnvironment.ActiveModuleID = 'SO'
  oAriaEnvironment.RequestID = lcRequestID

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF

  oAriaEnvironment.Report.cCROrientation = 'P'

  llOgFltCh = .T.
  =gfopentable("STYLE", "STYLE")
  lcworkfile2 = gftempname()
  lcFormName = "SOSTKAV"
  LODBFSTYLE    = CREATEOBJECT("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  LODBFPOSLN    = CREATEOBJECT("RemoteTable","POSLN","POSLNS",'POSLN',SET("DATASESSION"))&&,"",.T.)
  LODBFPOSHDR   = CREATEOBJECT("RemoteTable","POSHDR","POSHDR",'POSHDR',SET("DATASESSION"))&&,"",.T.)
  LODBFORDLINE  = CREATEOBJECT("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))&&,"",.T.)
  LODBFORDHDR   = CREATEOBJECT("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))&&,"",.T.)
  LODBFINVHDR   = CREATEOBJECT("RemoteTable","Invhdr","Invhdr",'Invhdr',SET("DATASESSION"))&&,"",.T.)
  LODBFINVLINE  = CREATEOBJECT("RemoteTable","invline","invline",'invline',SET("DATASESSION"))&&,"",.T.)
  LODBFSHPMTHDR = CREATEOBJECT("RemoteTable","Shpmthdr","Shpmthdr",'Shpmthdr',SET("DATASESSION"))&&,"",.T.)
  LODBFSCALE    = CREATEOBJECT("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
  LODBFCODES   = CREATEOBJECT("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
  LFWREPWHEN()
ELSE

ENDIF
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]

LCSTTIME = TIME()
LLOTSBASTR = GFGETMEMVAR('M_OTSBASTR',OARIAAPPLICATION.ACTIVECOMPANYID)

LLENGDATE = (ALLTRIM(OARIAAPPLICATION.DEFAULTCOUNTRY) = 'ENG' .OR. SET('DATE')='BRITISH')

DIMENSION LANETQTY[8]
LLHAVECLR = (LCFREE_CLR = 'C')

DIMENSION LOOGSCROLL.LACRPARAMS[7,2]

LOOGSCROLL.LACRPARAMS[1,1] = 'ReportName'

* N000682 ,1 Thabet Handle globalization issues [Start]
*loOgScroll.laCRParams[1,2] = LANG_Stock
LOOGSCROLL.LACRPARAMS[1,2] = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STOCK,OARIAAPPLICATION.GETHEADERTEXT("LANG_Stock",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]

LOOGSCROLL.LACRPARAMS[2,1] = 'llEngDate'
LOOGSCROLL.LACRPARAMS[2,2] = IIF(LLENGDATE,1,0)

LOOGSCROLL.LACRPARAMS[3,1] = 'llRpZeroAv'
LOOGSCROLL.LACRPARAMS[3,2] = IIF(LLRPZEROAV,1,0)

LOOGSCROLL.LACRPARAMS[4,1] = 'llRpSizes'
LOOGSCROLL.LACRPARAMS[4,2] = IIF(LLRPSIZES,1,0)

LOOGSCROLL.LACRPARAMS[5,1] = 'gdSysDate'
LOOGSCROLL.LACRPARAMS[5,2] = OARIAAPPLICATION.SYSTEMDATE

LOOGSCROLL.LACRPARAMS[6,1] = 'lcStyMajor'
LOOGSCROLL.LACRPARAMS[6,2] = LCSTYMAJOR

LOOGSCROLL.LACRPARAMS[7,1] = 'lcOtherTlt'
LOOGSCROLL.LACRPARAMS[7,2] = LCOTHERTLT



IF LOOGSCROLL.LLOGFLTCH && OG Filters changed
  STORE 0  TO LNCOUNTREC

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT WINDOW "Collecting Data......." NOWAIT
  WAIT WINDOW IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COLLECTING_DATA,OARIAAPPLICATION.GETHEADERTEXT("LANG_Collecting_Data",AHEADERFILE)) NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]
  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(LCWORKFILE) OR (RECCOUNT(LCWORKFILE) > 0)
    IF USED(LCWORKFILE)
      USE IN (LCWORKFILE)
    ENDIF
    = LFSTYTEMP()  && Create temporary cursor.
  ENDIF
  LFCOLLECT()
ELSE
  IF FILE(OARIAAPPLICATION.WORKDIR +  LCWORKFILE + ".DBF") AND !USED(LCWORKFILE)
    USE OARIAAPPLICATION.WORKDIR +  LCWORKFILE + ".DBF" IN 0
  ENDIF
ENDIF

LOOGSCROLL.LCOGLASTFORM = 'SOSTKAV'
LOOGSCROLL.CCRORIENTATION = 'L'

IF RECCOUNT(LCWORKFILE) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
  *=GFMODALGEN('TRM00052B40011','ALERT')
 IF TYPE('lcXMLFileName') = 'C'
 Else
  =GFMODALGEN('TRM00052B40011','ALERT')
 ENDIF
 *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
  RETURN

ENDIF
SELECT(LCWORKFILE)
IF !LLRPZEROAV
  LOCATE FOR PER11 <> 0  OR  PER12 <> 0
  IF !FOUND()
    *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
    *=GFMODALGEN('TRM00052B40011','ALERT')
    IF TYPE('lcXMLFileName') = 'C'
    Else
     =GFMODALGEN('TRM00052B40011','ALERT')
    ENDIF
    *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
    RETURN
  ENDIF
ENDIF
DIMENSION LOOGSCROLL.LACRTABLES[1]
LCEDTIME = TIME()  && Time in which we finish collect data.
LNINTERVAL = LFCOLLTIME(LCSTTIME,LCEDTIME)  && Calculate collecting data spent time.

* N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(lnCountRec)) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
WAIT WINDOW IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SELECTED,OARIAAPPLICATION.GETHEADERTEXT("LANG_Selected",AHEADERFILE)) + ALLTRIM(STR(LNCOUNTREC)) + IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_RECORDS_IN,OARIAAPPLICATION.GETHEADERTEXT("LANG_Records_in",AHEADERFILE)) + ALLTRIM(STR(LNINTERVAL,6,2)) + IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_SECONDS,OARIAAPPLICATION.GETHEADERTEXT("LANG_Seconds",AHEADERFILE)) NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]
*COPY TO oAriaApplication.WorkDir +  lcTmpFile+ ".DBF"
*loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpFile + ".DBF"

LOOGSCROLL.LACRTABLES[1] = OARIAAPPLICATION.WORKDIR +  LCWORKFILE + ".DBF"

*E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][Start]
IF !llRpSizes
 * IF  LOOGSCROLL.LLOGFLTCH
    *LCWORKFILE2 = LOOGSCROLL.GFTEMPNAME()
    DIMENSION laNewStrArry[1,4]
    laNewStrArry[1,1]= 'cMajor'
    laNewStrArry[1,2]= 'C'
    laNewStrArry[1,3]= 19
    laNewStrArry[1,4]= 0
    lnNewLen = ALEN(laNewStrArry,1)+1
    DIMENSION laNewStrArry[lnNewLen,4]
    laNewStrArry[lnNewLen,1]= 'Desc1'
    laNewStrArry[lnNewLen,2]= 'C'
    laNewStrArry[lnNewLen,3]= 60
    laNewStrArry[lnNewLen,4]= 0
    
    lnNewLen = ALEN(laNewStrArry,1)+1
    DIMENSION laNewStrArry[lnNewLen,4]
    laNewStrArry[lnNewLen,1]= 'lhasimg'
    laNewStrArry[lnNewLen,2]= 'L'
    laNewStrArry[lnNewLen,3]= 1
    laNewStrArry[lnNewLen,4]= 0
    
    
    FOR lnC = 1 TO 4
      lcC = STR(lnC,1)
      lnNewLen = ALEN(laNewStrArry,1)+1
      DIMENSION laNewStrArry[lnNewLen,4]
      laNewStrArry[lnNewLen,1]= 'cNonMajor'+lcC
      laNewStrArry[lnNewLen,2]= 'C'
      laNewStrArry[lnNewLen,3]= LNNONMAJLN
      laNewStrArry[lnNewLen,4]= 0
      
      lnNewLen = ALEN(laNewStrArry,1)+1
      DIMENSION laNewStrArry[lnNewLen,4]
      laNewStrArry[lnNewLen,1]= 'clrdesc'+lcC
      laNewStrArry[lnNewLen,2]= 'C'
      laNewStrArry[lnNewLen,3]= 30
      laNewStrArry[lnNewLen,4]= 0
      
      lnNewLen = ALEN(laNewStrArry,1)+1
      DIMENSION laNewStrArry[lnNewLen,4]
      laNewStrArry[lnNewLen,1]= 'BFILEPATH'+lcC
      laNewStrArry[lnNewLen,2]= 'W'
      laNewStrArry[lnNewLen,3]= 4
      laNewStrArry[lnNewLen,4]= 0


      
      FOR lnD = 1 TO 12
        lcD = ALLTRIM(STR(lnD,2))
        lnNewLen = ALEN(laNewStrArry,1)+1
        DIMENSION laNewStrArry[lnNewLen,4]
        laNewStrArry[lnNewLen,1]= 'Per'+lcD+lcC
        laNewStrArry[lnNewLen,2]= 'N'
        laNewStrArry[lnNewLen,3]= 7
        laNewStrArry[lnNewLen,4]= 0
      EnDFOR     
    ENDFOR   
    = GFCRTTMP(LCWORKFILE2 ,@laNewStrArry,"cMajor+cNonMajor1" ,LCWORKFILE2 ,.F.)
    SELECT(LCWORKFILE)  
    SELECT DISTINCT cMajor FROM (LCWORKFILE) WHERE IIF(!LLRPZEROAV,Per11 <> 0 OR Per12 <> 0,.T.) INTO CURSOR 'MajorCur'
    SELECT 'MajorCur'
    LOCATE 
    lcCurrMajor = SPACE(19)
    lnCntCol = 0
    LOCATE
    SCAN 
      IF lcCurrMajor <> cMajor
        lcCurrMajor = cMajor
        lnCntCol = 0
        SELECT (LCWORKFILE2)
        APPEND BLANK  
      ENDIF
      
      SELECT SUM(Per1) As Per1,SUM(Per2) As Per2,SUM(Per3) As Per3,SUM(Per4) As Per4,SUM(Per5) As Per5,SUM(Per6) As Per6,SUM(Per7) As Per7,SUM(Per8) As Per8,SUM(Per9) As Per9,;
             SUM(Per10) As Per10,SUM(Per11) As Per11,SUM(Per12) As Per12,cNonMajor FROM (LCWORKFILE) WHERE cMajor = MajorCur.cMajor AND IIF(!LLRPZEROAV,Per11 <> 0 OR Per12 <> 0,.T.) GROUP BY cNonMajor INTO CURSOR 'TotColor'
      SELECT  'TotColor'
      LOCATE 
      llFirstLine = .T.
      SCAN 
        =SEEK(MajorCur.cMajor +TotColor.cNonMajor,LCWORKFILE,LCWORKFILE)
        lnCntCol = lnCntCol + 1
        IF lnCntCol > 4
          lnCntCol = 1
          SELECT (LCWORKFILE2)
          APPEND BLANK  
        ENDIF     
        lcCntCol = STR(lnCntCol,1)
        SELECT (LCWORKFILE2)
        REPLACE cMajor WITH MajorCur.cMajor,;
                Desc1  WITH &LCWORKFILE..Desc1,;
                cNonMajor&lcCntCol WITH  TotColor.cNonMajor,;
                clrdesc&lcCntCol WITH  &LCWORKFILE..clrdesc
        IF llFirstLine
          REPLACE   BFILEPATH&lcCntCol WITH  &LCWORKFILE..BFILEPATH,;
                    lhasimg WITH IIF(!EMPTY(&LCWORKFILE..BFILEPATH),.T.,.F.)
          llFirstLine = .F.
        ENDIF  
        FOR lnD = 1 TO 12
          lcD = ALLTRIM(STR(lnD,2))
          lcVarFld = lcD+lcCntCol 
          REPLACE Per&lcVarFld WITH TotColor.Per&lcD
        ENDFOR 
      ENDSCAN  
      SELECT 'MajorCur'    
    ENDSCAN 
  lcTempvar = LCWORKFILE   
  LCWORKFILE = LCWORKFILE2
  *ENDIF  
  LOOGSCROLL.LCOGLASTFORM = 'SOSTKAVB'
  LOOGSCROLL.LACRTABLES[1] = OARIAAPPLICATION.WORKDIR +  LCWORKFILE + ".DBF"
  
  *!* B612489,1 MMT 11/10/2021 Stock Availability report Excel format does not have clear columns names in case of Print Sizes option = No[T20211108.0002][Start]
    IF !llRpSizes and (oAriaApplication.gcDevice = "FILE" .And. loogScroll.cTextRepType = "EXCEL")
    lcTempExcl = loogscroll.gfTempName()
    lcMonth1 = SUBSTR(cMONTH(oAriaApplication.Systemdate),1,3) && 0-15, 16-Last NOv
    lcMonth2 = SUBSTR(cMONTH(GOMONTH(oAriaApplication.Systemdate,1)),1,3) && 0-15, 16-Last
    lcMonth3 = SUBSTR(cMONTH(GOMONTH(oAriaApplication.Systemdate,2)),1,3) && 0-15, 16-Last
    lcMonth4 = SUBSTR(cMONTH(GOMONTH(oAriaApplication.Systemdate,3)),1,3) && 0-15, 16-Last
    lcMonth5 = SUBSTR(cMONTH(GOMONTH(oAriaApplication.Systemdate,4)),1,3)&& 0-15, 16-Last
    lnLastDay1 = GOMONTH(DATE(YEAR(oAriaApplication.Systemdate),MONTH(oAriaApplication.Systemdate),1),1) - 1
    lcLastDay1 = ALLTRIM(STR(day(lnLastDay1)))
    lnLastDay2 = GOMONTH(DATE(YEAR(oAriaApplication.Systemdate),MONTH(oAriaApplication.Systemdate),1),2) - 1    
    lcLastDay2 = ALLTRIM(STR(day(lnLastDay2)))
    lnLastDay3 = GOMONTH(DATE(YEAR(oAriaApplication.Systemdate),MONTH(oAriaApplication.Systemdate),1),3) - 1                                               
    lcLastDay3 = ALLTRIM(STR(day(lnLastDay3)))
    lnLastDay4 = GOMONTH(DATE(YEAR(oAriaApplication.Systemdate),MONTH(oAriaApplication.Systemdate),1),4) - 1                                               
    lcLastDay4 = ALLTRIM(STR(day(lnLastDay4)))
    lnLastDay5 = GOMONTH(DATE(YEAR(oAriaApplication.Systemdate),MONTH(oAriaApplication.Systemdate),1),5) - 1 
    lcLastDay5 = ALLTRIM(STR(Day(lnLastDay5)))
    LCOTHERTLTEx = STRTRAN(STRTRAN(LCOTHERTLT,'-','_'),' ','')
    lcNonMajTtl1 = ALLTRIM(LCOTHERTLTEx)+"1"
    lcNonMajTtl2 = ALLTRIM(LCOTHERTLTEx)+"2"
    lcNonMajTtl3 = ALLTRIM(LCOTHERTLTEx)+"3"
    lcNonMajTtl4 = ALLTRIM(LCOTHERTLTEx)+"4"    
    lcPer11Ttl =  ALLTRIM(lcMonth1)+'01_15'
    lcPer21Ttl =  ALLTRIM(lcMonth1)+'16_'+lcLastDay1
    lcPer41Ttl =  ALLTRIM(lcMonth2)+'16_'+lcLastDay2
    lcPer61Ttl =  ALLTRIM(lcMonth3)+'16_'+lcLastDay3
    lcPer81Ttl = ALLTRIM(lcMonth4)+'16_'+lcLastDay4
    lcPer101Ttl = ALLTRIM('FUT')+'16_'+lcLastDay5
    lcPer91Ttl = ALLTRIM(lcMonth5)+'01_15'
    lcPer31Ttl = ALLTRIM(lcMonth2)+'01_15'
    lcPer71Ttl = ALLTRIM(lcMonth4)+'01_15'
    lcPer51Ttl = ALLTRIM(lcMonth3)+'01_15'
    
    
    lcPer11Ttl2 =  ALLTRIM(lcMonth1)+'01_15_2'
    lcPer21Ttl2 =  ALLTRIM(lcMonth1)+'16_'+lcLastDay1+'_2'
    lcPer41Ttl2 =  ALLTRIM(lcMonth2)+'16_'+lcLastDay2+'_2'
    lcPer61Ttl2 =  ALLTRIM(lcMonth3)+'16_'+lcLastDay3+'_2'
    lcPer81Ttl2 =  ALLTRIM(lcMonth4)+'16_'+lcLastDay4+'_2'
    lcPer101Ttl2 = ALLTRIM('FUT')+'16_'+lcLastDay5+'_2'
    lcPer91Ttl2 = ALLTRIM(lcMonth5)+'01_15_2'
    lcPer31Ttl2 = ALLTRIM(lcMonth2)+'01_15_2'
    lcPer71Ttl2 = ALLTRIM(lcMonth4)+'01_15_2'
    lcPer51Ttl2 = ALLTRIM(lcMonth3)+'01_15_2'
    

    lcPer11Ttl3 =  ALLTRIM(lcMonth1)+'01_15_3'
    lcPer21Ttl3 =  ALLTRIM(lcMonth1)+'16_'+lcLastDay1+'_3'
    lcPer41Ttl3 =  ALLTRIM(lcMonth2)+'16_'+lcLastDay2+'_3'
    lcPer61Ttl3 =  ALLTRIM(lcMonth3)+'16_'+lcLastDay3+'_3'
    lcPer81Ttl3 = ALLTRIM(lcMonth4)+'16_'+lcLastDay4+'_3'
    lcPer101Ttl3 = ALLTRIM('FUT')+'16_'+lcLastDay5+'_3'
    lcPer91Ttl3 = ALLTRIM(lcMonth5)+'01_15_3'
    lcPer31Ttl3 = ALLTRIM(lcMonth2)+'01_15_3'
    lcPer71Ttl3 = ALLTRIM(lcMonth4)+'01_15_3'
    lcPer51Ttl3 = ALLTRIM(lcMonth3)+'01_15_3'
    
    
    
    lcPer11Ttl4 =  ALLTRIM(lcMonth1)+'01_15_4'
    lcPer21Ttl4 =  ALLTRIM(lcMonth1)+'16_'+lcLastDay1+'_4'
    lcPer41Ttl4 =  ALLTRIM(lcMonth2)+'16_'+lcLastDay2+'_4'
    lcPer61Ttl4 =  ALLTRIM(lcMonth3)+'16_'+lcLastDay3+'_4'
    lcPer81Ttl4 = ALLTRIM(lcMonth4)+'16_'+lcLastDay4+'_4'
    lcPer101Ttl4 = ALLTRIM('FUT')+'16_'+lcLastDay5+'_4'
    lcPer91Ttl4 = ALLTRIM(lcMonth5)+'01_15_4'
    lcPer31Ttl4 = ALLTRIM(lcMonth2)+'01_15_4'
    lcPer71Ttl4 = ALLTRIM(lcMonth4)+'01_15_4'
    lcPer51Ttl4 = ALLTRIM(lcMonth3)+'01_15_4'


    
    
    SELECT cMajor As '&LCSTYMAJOR.',Desc1 as 'Style_DESC', lhasimg  as 'Has_Image' ,cNonMajor1 as '&lcNonMajTtl1.' ,;
           clrdesc1 As 'Color1_Dsc',;
           Per11 AS '&lcPer11Ttl.' ,Per21 AS '&lcPer21Ttl.' ,;
           Per31 AS '&lcPer31Ttl.',Per41 AS '&lcPer41Ttl.' ,;
           Per51 AS '&lcPer51Ttl.',Per61 AS '&lcPer61Ttl.' ,;
           Per71 AS '&lcPer71Ttl.' ,Per81 AS '&lcPer81Ttl.' ,;
           Per91 AS '&lcPer91Ttl.' ,Per101 AS '&lcPer101Ttl.',;
           Per111 AS 'TotAvlbl',Per121 AS 'TotSldYTD',;
           cNonMajor2 as '&lcNonMajTtl2.'  , clrdesc2 As 'Color2__Dsc',;
           Per12 AS '&lcPer11Ttl2.' ,Per22 AS '&lcPer21Ttl2.' ,;
           Per32 AS '&lcPer31Ttl2.' ,Per42 AS '&lcPer41Ttl2.' ,;
           Per52 AS '&lcPer51Ttl2.' ,Per62 AS '&lcPer61Ttl2.' ,;
           Per72 AS '&lcPer71Ttl2.' ,Per82 AS '&lcPer81Ttl2.' ,;
           Per92 AS '&lcPer91Ttl2.' ,Per102 AS '&lcPer101Ttl2.' ,;
           Per112 AS 'TotAvlbl2',Per122 AS 'TotSldYTD2',;
           cNonMajor3 as '&lcNonMajTtl3.' , clrdesc3 As 'Color3__Dsc',;
           Per13 AS '&lcPer11Ttl3.' ,Per23 AS '&lcPer21Ttl3.' ,;
           Per33 AS '&lcPer31Ttl3.' ,Per43 AS '&lcPer41Ttl3.',;
           Per53 AS '&lcPer51Ttl3.' ,Per63 AS '&lcPer61Ttl3.',;
           Per73 AS '&lcPer71Ttl3.' ,Per83 AS '&lcPer81Ttl3.' ,;
           Per93 AS '&lcPer91Ttl3.' ,Per103 AS '&lcPer101Ttl3.',;
           Per113 AS 'TotAvlbl3',Per123 AS 'TotSldYTD3',;
           cNonMajor4 as '&lcNonMajTtl4.'  , clrdesc4 As 'Color4__Dsc',;
           Per14 AS '&lcPer11Ttl4.',Per24 AS '&lcPer21Ttl4.' ,;
           Per34 AS '&lcPer31Ttl4.',Per44 AS '&lcPer41Ttl4.' ,;
           Per54 AS '&lcPer51Ttl4.' ,Per64 AS '&lcPer61Ttl4.' ,;
           Per74 AS '&lcPer71Ttl4.' ,Per84 AS '&lcPer81Ttl4.' ,;
           Per94 AS '&lcPer91Ttl4.',Per104 AS '&lcPer101Ttl4.' ,;
           Per114 AS 'TotAvlbl4',Per124 AS 'TotSldYTD4'FROM (LCWORKFILE) INTO DBF (OARIAAPPLICATION.WORKDIR +  lcTempExcl+ ".DBF")
                     
       IF USED(lcTempExcl)
         USE IN (lcTempExcl)
       ENDIF
       DIMENSION   LOOGSCROLL.LACRTABLES[1]
       LOOGSCROLL.LACRTABLES[1] = OARIAAPPLICATION.WORKDIR +  lcTempExcl+ ".DBF"
       
  ENDIF
  *!* B612489,1 MMT 11/10/2021 Stock Availability report Excel format does not have clear columns names in case of Print Sizes option = No[T20211108.0002][End]
  
  SELECT(LCWORKFILE)
  USE
  *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
  lcFormName = "SOSTKAVB"
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
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
    *GFDISPRE()

  ELSE
  GFDISPRE()
 ENDIF
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
  LCWORKFILE  = lcTempvar 
  RETURN
ENDIF
*E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][End]
SELECT(LCWORKFILE)
USE
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
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
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
    *GFDISPRE()
    
  ELSE
  GFDISPRE()
 ENDIF
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
RETURN


*-- if company setuped to use Purshase order module.
*!*	IF 'PO' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT POSLN
*!*	  SET RELATION TO ShipNo INTO SHPMTHDR, cStyType + Po INTO POSHDR
*!*	ENDIF

*!*	*-- if company setuped to use Manufacturing module.
*!*	IF 'MF' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT CUTTKTL
*!*	  SET RELATION TO CUTTKT INTO CUTTKTH
*!*	ENDIF

*!*	*-- if company setuped to use Account Receivable module.
*!*	IF 'AR' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT INVLINE
*!*	  SET RELATION TO Invoice INTO INVHDR
*!*	ENDIF

*!*	SELECT ORDLINE
*!*	SET RELATION TO cOrdType + Order INTO ORDHDR

*!*	*-- Setup relation Section [End  ]

*!*	SELECT (lcWorkFile)
*!*	GO TOP


*!*	CLEAR TYPEAHEAD
*!*	SET DEVICE TO PRINT
*!*	DO lpPrnCode  && Do Print code.

*!*	*-- Calculate spent time in collecting data.
*!*	lcEdTime = TIME()  && Time in which we finish collect data.
*!*	lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.

*!*	WAIT WINDOW 'Selected ' + ALLTRIM(STR(lnCountRec)) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
*!*	*RECCOUNT(lcWorkFile)
*!*	DO ENDREPORT  && Display or print the report .
*!*	WAIT CLEAR

*!*	SET DEVICE TO SCREEN
*!*	*--- End   Report printing.

*!*	*-- Clean relation Section [Begin]

*!*	*-- if company setuped to use Purshase order module.
*!*	IF 'PO' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT POSLN
*!*	  SET RELATION TO
*!*	ENDIF

*!*	*-- if company setuped to use Manufacturing module.
*!*	IF 'MF' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT CUTTKTL
*!*	  SET RELATION TO
*!*	ENDIF

*!*	*-- if company setuped to use Account receivable module.
*!*	IF 'AR' $ oAriaApplication.CompanyInstalledModules
*!*	  SELECT INVLINE
*!*	  SET RELATION TO
*!*	ENDIF

*!*	SELECT ORDLINE
*!*	SET RELATION TO

*-- Clean relation Section [End  ]

*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/11/2006
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION LFWREPWHEN
IF !LLFRSTTIME
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
  *ODBFSTYLE    = CREATEOBJECT("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  *ODBFPOSLN    = CREATEOBJECT("RemoteTable","POSLN","POSLNS",'POSLN',SET("DATASESSION"))&&,"",.T.)
  *ODBFPOSHDR   = CREATEOBJECT("RemoteTable","POSHDR","POSHDR",'POSHDR',SET("DATASESSION"))&&,"",.T.)
  *ODBFORDLINE  = CREATEOBJECT("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))&&,"",.T.)
  *ODBFORDHDR   = CREATEOBJECT("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))&&,"",.T.)
  *ODBFINVHDR   = CREATEOBJECT("RemoteTable","Invhdr","Invhdr",'Invhdr',SET("DATASESSION"))&&,"",.T.)
  *ODBFINVLINE  = CREATEOBJECT("RemoteTable","invline","invline",'invline',SET("DATASESSION"))&&,"",.T.)
  *ODBFSHPMTHDR = CREATEOBJECT("RemoteTable","Shpmthdr","Shpmthdr",'Shpmthdr',SET("DATASESSION"))&&,"",.T.)
  *ODBFSCALE    = CREATEOBJECT("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
  *ODBFCODES   = CREATEOBJECT("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
IF TYPE('lcXMLFileName') = 'C'
ELSE
lcworkfile2 = gftempname()
  LODBFSTYLE    = CREATEOBJECT("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  LODBFPOSLN    = CREATEOBJECT("RemoteTable","POSLN","POSLNS",'POSLN',SET("DATASESSION"))&&,"",.T.)
  LODBFPOSHDR   = CREATEOBJECT("RemoteTable","POSHDR","POSHDR",'POSHDR',SET("DATASESSION"))&&,"",.T.)
  LODBFORDLINE  = CREATEOBJECT("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))&&,"",.T.)
  LODBFORDHDR   = CREATEOBJECT("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))&&,"",.T.)
  LODBFINVHDR   = CREATEOBJECT("RemoteTable","Invhdr","Invhdr",'Invhdr',SET("DATASESSION"))&&,"",.T.)
  LODBFINVLINE  = CREATEOBJECT("RemoteTable","invline","invline",'invline',SET("DATASESSION"))&&,"",.T.)
  LODBFSHPMTHDR = CREATEOBJECT("RemoteTable","Shpmthdr","Shpmthdr",'Shpmthdr',SET("DATASESSION"))&&,"",.T.)
  LODBFSCALE    = CREATEOBJECT("RemoteTable","Scale","Scale",'Scale',SET("DATASESSION"))&&,"",.T.)
  LODBFCODES   = CREATEOBJECT("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)
 ENDIF
*E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]

  LCSQLSTAT1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  LNRESULT1 = LOOGSCROLL.ORDA.SQLRUN(LCSQLSTAT1, LCTMPFAB, , OARIAAPPLICATION.ACTIVECOMPANYCONSTR, 3, "BROWSE", SET("Datasession"))
  LLFRSTTIME = .T.
  IF LNRESULT1 >= 1
    =LFCREATEINDECIES(LCTMPFAB, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF


IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(LCLASTEXPR)


  DIMENSION LATEMPSTRU[1,18]
  LATEMPSTRU = ''

  SELECT STYLE
  LNALLFLDS = AFIELDS(LATEMPSTRU)

  DIMENSION LATEMPSTRU[lnAllFlds + IIF(lnNonMajLn>0,17,16), 18]
  LATEMPSTRU[lnAllFlds+1,1] = 'COLOR'
  LATEMPSTRU[lnAllFlds+1,2] = 'C'
  LATEMPSTRU[lnAllFlds+1,3] = 6
  LATEMPSTRU[lnAllFlds+1,4] = 0

  LATEMPSTRU[lnAllFlds+2,1] = 'cMajor'
  LATEMPSTRU[lnAllFlds+2,2] = 'C'
  LATEMPSTRU[lnAllFlds+2,3] = LNMAJORLEN
  LATEMPSTRU[lnAllFlds+2,4] = 0

  LATEMPSTRU[lnAllFlds+3,1] = 'sz'
  LATEMPSTRU[lnAllFlds+3,2] = 'C'
  LATEMPSTRU[lnAllFlds+3,3] = 5
  LATEMPSTRU[lnAllFlds+3,4] = 0

  LATEMPSTRU[lnAllFlds+4,1] = 'per1'
  LATEMPSTRU[lnAllFlds+4,2] = 'N'
  LATEMPSTRU[lnAllFlds+4,3] = 7
  LATEMPSTRU[lnAllFlds+4,4] = 0

  LATEMPSTRU[lnAllFlds+5,1] = 'per2'
  LATEMPSTRU[lnAllFlds+5,2] = 'N'
  LATEMPSTRU[lnAllFlds+5,3] = 7
  LATEMPSTRU[lnAllFlds+5,4] = 0

  LATEMPSTRU[lnAllFlds+6,1] = 'per3'
  LATEMPSTRU[lnAllFlds+6,2] = 'N'
  LATEMPSTRU[lnAllFlds+6,3] = 7
  LATEMPSTRU[lnAllFlds+6,4] = 0

  LATEMPSTRU[lnAllFlds+7,1] = 'per4'
  LATEMPSTRU[lnAllFlds+7,2] = 'N'
  LATEMPSTRU[lnAllFlds+7,3] = 7
  LATEMPSTRU[lnAllFlds+7,4] = 0

  LATEMPSTRU[lnAllFlds+8,1] = 'per5'
  LATEMPSTRU[lnAllFlds+8,2] = 'N'
  LATEMPSTRU[lnAllFlds+8,3] = 7
  LATEMPSTRU[lnAllFlds+8,4] = 0

  LATEMPSTRU[lnAllFlds+9,1] = 'per6'
  LATEMPSTRU[lnAllFlds+9,2] = 'N'
  LATEMPSTRU[lnAllFlds+9,3] = 7
  LATEMPSTRU[lnAllFlds+9,4] = 0

  LATEMPSTRU[lnAllFlds+10,1] = 'per7'
  LATEMPSTRU[lnAllFlds+10,2] = 'N'
  LATEMPSTRU[lnAllFlds+10,3] = 7
  LATEMPSTRU[lnAllFlds+10,4] = 0

  LATEMPSTRU[lnAllFlds+11,1] = 'per8'
  LATEMPSTRU[lnAllFlds+11,2] = 'N'
  LATEMPSTRU[lnAllFlds+11,3] = 7
  LATEMPSTRU[lnAllFlds+11,4] = 0

  LATEMPSTRU[lnAllFlds+12,1] = 'per9'
  LATEMPSTRU[lnAllFlds+12,2] = 'N'
  LATEMPSTRU[lnAllFlds+12,3] = 7
  LATEMPSTRU[lnAllFlds+12,4] = 0

  LATEMPSTRU[lnAllFlds+13,1] = 'per10'
  LATEMPSTRU[lnAllFlds+13,2] = 'N'
  LATEMPSTRU[lnAllFlds+13,3] = 7
  LATEMPSTRU[lnAllFlds+13,4] = 0

  LATEMPSTRU[lnAllFlds+14,1] = 'per11'
  LATEMPSTRU[lnAllFlds+14,2] = 'N'
  LATEMPSTRU[lnAllFlds+14,3] = 7
  LATEMPSTRU[lnAllFlds+14,4] = 0

  LATEMPSTRU[lnAllFlds+15,1] = 'per12'
  LATEMPSTRU[lnAllFlds+15,2] = 'N'
  LATEMPSTRU[lnAllFlds+15,3] = 7
  LATEMPSTRU[lnAllFlds+15,4] = 0

  LATEMPSTRU[lnAllFlds+16,1] = 'ClrDesc'
  LATEMPSTRU[lnAllFlds+16,2] = 'C'
  LATEMPSTRU[lnAllFlds+16,3] = 30
  LATEMPSTRU[lnAllFlds+16,4] = 0


  IF LNNONMAJLN > 0
    LATEMPSTRU[lnAllFlds+17,1] = 'cNonMajor'
    LATEMPSTRU[lnAllFlds+17,2] = 'C'
    LATEMPSTRU[lnAllFlds+17,3] = LNNONMAJLN
    LATEMPSTRU[lnAllFlds+17,4] = 0
  ENDIF


  FOR LNLOOP = 1 TO  IIF(LNNONMAJLN>0,17,16)
    STORE ' ' TO  LATEMPSTRU[lnAllFlds+lnLoop,7],LATEMPSTRU[lnAllFlds+lnLoop,8],;
      LATEMPSTRU[lnAllFlds+lnLoop,9],LATEMPSTRU[lnAllFlds+lnLoop,10],;
      LATEMPSTRU[lnAllFlds+lnLoop,11],LATEMPSTRU[lnAllFlds+lnLoop,12],;
      LATEMPSTRU[lnAllFlds+lnLoop,13],LATEMPSTRU[lnAllFlds+lnLoop,14],;
      LATEMPSTRU[lnAllFlds+lnLoop,15],LATEMPSTRU[lnAllFlds+lnLoop,16]
    STORE 0 TO    LATEMPSTRU[lnAllFlds+lnLoop,17] ,LATEMPSTRU[lnAllFlds+lnLoop,18]
  ENDFOR

  *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][Start]
  DIMENSION LATEMPSTRU[ALEN(LATEMPSTRU,1)+2,18]
  LATEMPSTRU[ALEN(LATEMPSTRU,1),1] = "BFilePath"
  LATEMPSTRU[ALEN(LATEMPSTRU,1),2] = 'W'
  LATEMPSTRU[ALEN(LATEMPSTRU,1),3] = 4
  LATEMPSTRU[ALEN(LATEMPSTRU,1),4] = 0
  LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,1] = "lHasImage"
  LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,2] = 'L'
  LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,3] = 1
  LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,4] = 0

  STORE ' ' TO  LATEMPSTRU[ALEN(LATEMPSTRU,1),7],LATEMPSTRU[ALEN(LATEMPSTRU,1),8],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1),9],LATEMPSTRU[ALEN(LATEMPSTRU,1),10],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1),11],LATEMPSTRU[ALEN(LATEMPSTRU,1),12],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1),13],LATEMPSTRU[ALEN(LATEMPSTRU,1),14],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1),15],LATEMPSTRU[ALEN(LATEMPSTRU,1),16]
  STORE 0 TO LATEMPSTRU[ALEN(LATEMPSTRU,1),17] ,LATEMPSTRU[ALEN(LATEMPSTRU,1),18]
  STORE ' ' TO  LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,7],LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,8],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,9],LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,10],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,11],LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,12],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,13],LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,14],;
      LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,15],LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,16]
  STORE 0 TO LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,17] ,LATEMPSTRU[ALEN(LATEMPSTRU,1)-1,18]

  *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][End]

  =LFSTYTEMP()  && Create temporary used in collecting data.

ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfStyTemp
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStyTemp()
*!*************************************************************
*
FUNCTION LFSTYTEMP
*!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][Start]
*= GFCRTTMP(LCWORKFILE ,@LATEMPSTRU,"cMajor+"+ IIF(LCFREE_CLR = 'C',"Color" , "cNonMajor") ,LCWORKFILE,.F.)
= GFCRTTMP(LCWORKFILE ,@LATEMPSTRU,"cMajor+cNonMajor" ,LCWORKFILE,.F.)
*!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][End]
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
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
FUNCTION LFWOLDVAL
LAOLDVAL = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION LFEVALSEGS

LNMAJSEG    = GFITEMMASK('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION LAMAJSEGS[1,1]
= GFITEMMASK(@LAMAJSEGS)

*-- Loop Around Non Major elements.
FOR LNI = LNMAJSEG + 1 TO ALEN(LAMAJSEGS,1)
  IF LAMAJSEGS[lnI,1] $ 'CF'
    LCFREE_CLR = LAMAJSEGS[lnI,1]

    LNNONMAJST = IIF(LNNONMAJST=0 .OR. LAMAJSEGS[lnI,1]='C',LAMAJSEGS[lnI,4],LNNONMAJST)      && This item hold seg. start position.

    LCNONMAJPI = IIF(EMPTY(LCNONMAJPI) .OR. LAMAJSEGS[lnI,1]='C',;
      LAMAJSEGS[lnI,3],;
      LCNONMAJPI + LAMAJSEGS[lnI-1,6] + LAMAJSEGS[lnI,3])

    LCNONMAJTL = IIF(EMPTY(LCNONMAJTL) .OR. LAMAJSEGS[lnI,1]='C',;
      PADR(LAMAJSEGS[lnI,2],LEN(LAMAJSEGS[lnI,3])),;
      LCNONMAJTL + LAMAJSEGS[lnI-1,6] + PADR(LAMAJSEGS[lnI,2],LEN(LAMAJSEGS[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF LAMAJSEGS[lnI,1] = 'C' OR (!EMPTY(LCFREE_CLR) AND LAMAJSEGS[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(LCNONMAJPI) TO LNFREELEN , LNCOLORLEN
*N000682,1 MMT 03/25/2013 Fix Phase#3 issues of Testing R13[Start]
*lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
LCCOLORTLT = IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_ONLYTHESE,OARIAAPPLICATION.GETHEADERTEXT("LANG_ONLYTHESE",AHEADERFILE))+' ' + ;
  ALLTRIM(LCNONMAJTL) +IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_COLORS,OARIAAPPLICATION.GETHEADERTEXT("LANG_COLORS",AHEADERFILE))
*N000682,1 MMT 03/25/2013 Fix Phase#3 issues of Testing R13[End]
*-- Compute Free/Color Items in Style code Structure. [End]

*-- Compute Variables titles. [begin]
*N000682,1 MMT 03/25/2013 Fix Phase#3 issues of Testing R13[Start]
*!*	  lcStyStat = lcStyMajor + ' Status'
*!*	  lcStyMake = lcStyMajor + ' (Make/Buy)'
LCSTYSTAT = LCSTYMAJOR + ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_STATUS,OARIAAPPLICATION.GETHEADERTEXT("LANG_STATUS",AHEADERFILE))
LCSTYMAKE = LCSTYMAJOR + ' '+IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_MAKEBUY,OARIAAPPLICATION.GETHEADERTEXT("LANG_MAKEBUY",AHEADERFILE))
*N000682,1 MMT 03/25/2013 Fix Phase#3 issues of Testing R13[End]
*-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION LFSRVSTY
PARAMETERS LCPARM
IF LCPARM = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
  *!*  ELSE
  *!*    SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*  DO CASE
*!*    CASE lcParm = 'S'  && Set code
*!*      *-- open this file in another alias to set order to Style Major
*!*      *-- unique index.
*!*      USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
*!*      SELECT STYLE
*!*      SET ORDER TO TAG Cstyle
*!*      SET RELATION TO STYLE.STYLE INTO STYLE_X
*!*      GO TOP IN STYLE
*!*      llChStyle = .T.
*!*    CASE lcParm = 'R'  && Reset code
*!*      USE IN STYLE_X
*!*      SELECT STYLE
*!*      SET ORDER TO TAG STYLE
*!*      llClearSty = .F.
*!*  ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION LFSTYSUM
PARAMETERS LCSTY,LCCOMP,LNADDTOVAR
PRIVATE LNSTYREC
LNSTYREC = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
LNTOTCOMP = 0
SELECT STYLE_X
SUM &LCCOMP TO LNTOTCOMP WHILE STYLE = ALLTRIM(LCSTY)
SELECT STYLE
GO LNSTYREC
DO CASE
CASE LNADDTOVAR = 1
  LNO_T_S = LNTOTCOMP
CASE LNADDTOVAR = 2
  LNO_T_S = LNO_T_S + LNTOTCOMP
CASE LNADDTOVAR = 3
  LNO_T_S = LNO_T_S - LNTOTCOMP
ENDCASE
RETURN INT(LNTOTCOMP)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION LFVFABRIC

LCOBJNAME = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
LCOBJVAL = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ LCOBJVAL .OR. (!EMPTY(LCOBJVAL) .AND. !SEEK(LCOBJVAL , 'FABRIC'))
  LLOBJRET = FABROW(@LCOBJVAL , '*')
  LCOBJVAL = IIF(LLOBJRET , LCOBJVAL , LAOLDVAL)
  &LCOBJNAME = LCOBJVAL
ENDIF    && End of IF
*-- end of lfvFabric.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION LFCLEARREP
LLCLEARFN = .T.  && Rise clear done flag.
*-- Close temp. opended files, if it used.
IF USED(LCWORKFILE)
  USE IN (LCWORKFILE)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
FUNCTION LFCOLLTIME
PARAMETERS LCSTART,LCEND
LNSTHOUR  = IIF(VAL(LEFT(LCSTART,2)) = 0,VAL(LEFT(LCSTART,2))+24,VAL(LEFT(LCSTART,2)))
LNENDHOUR = IIF(VAL(LEFT(LCEND,2))   = 0,VAL(LEFT(LCEND,2))  +24,VAL(LEFT(LCEND,2)))
LNSTART = 3600 * LNSTHOUR  + 60 * VAL(SUBSTR(LCSTART,4,2)) + VAL(RIGHT(LCSTART,2))
LNEND   = 3600 * LNENDHOUR + 60 * VAL(SUBSTR(LCEND,4,2))   + VAL(RIGHT(LCEND,2))
RETURN (LNEND - LNSTART)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lpPrnCode
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/04/1999
*! Purpose   : Report Print code.
*!*************************************************************
*! Calls     :
*!             Procedures : DISP_TITLES,ORD971,ORD972,POST_NVE_VALUS,DISP_OTS_LINES,
*!             Functions  : lfGtYTD.
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lpPrnCode()
*!*************************************************************
PROCEDURE LPPRNCODE
* -- Initialize the necessary global variables.
*!*  R_TITLE    = 'Stock Availability Report'
*!*  PAGENO     = 00
*!*  ROW        = 99

*B606586,1 BWA 01/06/2003 Increase the printing size.[START]
*XMAXROW    = 47
*!*  XMAXROW    = 58
*!*  *B606586,1 BWA.[END]

*!*  XTODAY     = gdSysDate
*!*  C_MONTH    = MONTH (XTODAY)
*!*  C_YEAR     = YEAR  (XTODAY)
*!*  XSTYLE     = SPACE (12)
*!*  XCOLOR     = SPACE (06)
*!*  lcFullSty  = SPACE(19)
*!*  llPrintSty = .F.    && TAK 05/25/94 Flag to print style and
*!*                      && desc if style was changed in case of
*!*                      && llRpZeroAv = .F.

*!*  STORE 0 TO lnTitlLen,lnSzStart,lnClrStart,lnDescSt

* --- Create and initialize :
* -- 1) An array to hold the 10 periods. Each period contains 2 dates ,
* --    the first one is the start of the period and the last one is the end
* --    of the period , so ..
* --    the array will be "XPERIOD_ARR [10 * 2]" (Remember that this report
* --    deals with only 5 month).
* -- 2) An array to hold the end day for each month....
* --    The report titles variables are XLINE_1 , XLINE_2, And....
* --    to display XLINE_2 with the start and end of each period, we
* --    had to have an array "XYEAR" to hold the end day for each month.
* --    This array will be released after finishing of forming the titles.
* -- 3) Two dimensional array "STY_INFO", to hold the OTS quantities
* --    for each [Color\Size,Period], it will be initialized each time
* --    the style changes to fit the size scale for the current style.
* --    The array has 12 cols., 10 of these 12 are for the periods,
* --    and col. no.[11] is to hold the total of the periods,
* --    and col. no.[12] is to hold the quantities shipped for this
* --    color/size.
* --    The array also has a dynamic number of rows according to ..
* --    A) The size scale which changes from one style to another.
* --    B) The Report format : if not by size the array rows will be
* --       increased by one to hold the totals for each color,
* --       otherwise no rows will be added.
* -- 4) One dimensional array "XTOTAL", to hold the total OTS quantities
* --    for one style\color (Accumulater).

* -- Create and Initialive the arrays .
DIMENSION XPERIOD_ARR [20] , XYEAR [12] , STY_INFO [1,12] , XTOTAL [12]
XPERIOD_ARR = {}
XYEAR       = 31
STORE 30 TO XYEAR(4),XYEAR(6),XYEAR(9),XYEAR(11)
XYEAR (2)   = IIF (MOD(YEAR(XTODAY),4)=0 , 29 , 28)
STY_INFO    = 00
XTOTAL      = 00

* -- Initialize the first period with the first day of today's month .

*B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
*XPERIOD_ARR(1) = CTOD(ALLTRIM(STR(C_MONTH))+'/01/'+ALLTRIM(STR(C_YEAR)))
*B129693,1 EIH 10/02/2005 [End].

*ahmed
*XPERIOD_ARR(2) = CTOD(ALLTRIM(STR(C_MONTH))+'/15/'+ALLTRIM(STR(C_YEAR)))

IF LLENGDATE

  *B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
  XPERIOD_ARR(1) = CTOD('01/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
  *B129693,1 EIH 10/02/2005 [End].

  XPERIOD_ARR(2) = CTOD('15/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
ELSE

  *B129693,1 EIH 10/02/2005 Fix wrong date of first period in case of england [Begin].
  XPERIOD_ARR(1) = CTOD(ALLTRIM(STR(C_MONTH))+'/01/'+ALLTRIM(STR(C_YEAR)))
  *B129693,1 EIH 10/02/2005 [End].

  XPERIOD_ARR(2) = CTOD(ALLTRIM(STR(C_MONTH))+'/15/'+ALLTRIM(STR(C_YEAR)))
ENDIF
*ahmed end

* -- Initialize the title variables.
LNTITLLEN = 25 - ( LEN(LCSTYMAJOR) + LEN(LCOTHERTLT) )
LNSZSTART = LEN(LCSTYMAJOR) + 1
LNCLRSTART= LNSZSTART + 6
LNDESCST  = LNCLRSTART + LEN(LCOTHERTLT) + 1

XLINE_1   = SPACE (11)
XLINE_2   = LCSTYMAJOR + IIF (LLRPSIZES,'/Size  ',SPACE(7))+ ;
  LCOTHERTLT + ' Desc.' + SPACE(LNTITLLEN)


* -- Fill the "XPERIOD_ARR" array and form the report titles.
XLOOP   = 3
XPERIOD = 1
XMONTH  = 0
DO WHILE XPERIOD < 10
  * -- Fill the periods array.
  XPERIOD_ARR(XLOOP  ) = XPERIOD_ARR(XLOOP-1) + 1
  XPERIOD_ARR(XLOOP+1) = IIF ( MOD(XPERIOD,2) = 0 ,               ;
    XPERIOD_ARR(XLOOP) + 14          , ;
    GOMONTH (XPERIOD_ARR(XLOOP-2),1)-1 )
  * -- Form the titles.
  IF MOD (XPERIOD,2) <> 0
    XLINE_1 = XLINE_1 + PADC (UPPER(CMONTH(GOMONTH(XTODAY,XMONTH))) + ;
      IIF (XMONTH=4,' FUTURE','') , 16 , ' ' )
    XLINE_2 = XLINE_2 + '01-15   16-'+;
      ALLTRIM(STR(XYEAR(MONTH(GOMONTH(XTODAY,XMONTH)))))+;
      SPACE(3)
    XMONTH  = XMONTH + 1
  ENDIF

  XLOOP   = XLOOP   + 2
  XPERIOD = XPERIOD + 1
ENDDO

* -- To complete forming the title variabls.
XLINE_1 = XLINE_1 + ' TOTAL  TOTAL'
XLINE_2 = XLINE_2 +  'AVAIL SOLD YTD'
RELEASE XYEAR
* --- End of Initializing the necessary variables and arrays. ----------

*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� !*
*!                    *****                   *****                    !*
*!                    *** --- THE MAIN LOOP --- ***                    !*
*!                    *****                   *****                    !*
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� !*

SELECT (LCWORKFILE)

LNUPTHERMO = RECCOUNT(LCWORKFILE)
LNTHERMONO = 0


*B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
STORE '' TO LCOLDSCL
STORE ' ' TO  LCNEWSCL
*B129693,1 EIH 10/02/2005 [End] .


DO WHILE !EOF()

  IF ROW > XMAXROW - 2

    DO DISP_TITLES
    LOOP

  ELSE

    * -- Each time the style changes the following will be done :
    * -- 1) Update the current style variable "XSTYLE".
    * -- 2) Print the style title.
    * -- 3) Get the size scale for this style.
    * -- 4) Recreate "STY_INFO" to fit the current style\size scale.

    *B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
    *IF cMajor <> XSTYLE
    IF CMAJOR <> XSTYLE OR LCOLDSCL <> LCNEWSCL
      LCOLDSCL = LCNEWSCL
      *B129693,1 EIH 10/02/2005 [End] .

      XSTYLE    = CMAJOR
      SET DEVICE TO SCREEN
      LNTHERMONO = LNTHERMONO + 1
      = GFTHERMO(LNUPTHERMO,LNTHERMONO,'Calculate availability for ' + LCSTYMAJOR + ': ',XSTYLE)
      SET DEVICE TO PRINT

      Y =1                           && TMI 12/01/94

      *-- Added by TAK 05/25/94
      IF !LLRPZEROAV
        LLPRINTSTY = .T.
        XSTYLE  = CMAJOR
        XDESC  = DESC1
      ELSE

        @ ROW,00 SAY XSTYLE
        @ ROW,LNDESCST SAY DESC1                            && TAK 02/28/94
        ROW = ROW + 1

      ENDIF
      *-- End TAK 05/25/94 ---

      SELECT SCALE
      XSIZE_CNT = IIF ( SEEK ('S'+&LCWORKFILE..SCALE) , CNT , 1)

      *B129693,1 EIH 10/02/2005 Fix bug that print wrong scale in case of extended size scale [Begin] .
      LCNEWSCL = &LCWORKFILE..SCALE
      *B129693,1 EIH 10/02/2005 [End] .

      XSCALES   = GETSCALE (&LCWORKFILE..SCALE,SPACE(0))          && TMI 12/01/94
      DIMENSION STY_INFO [XSIZE_CNT+IIF(llRpSizes,0,1),12]
      STY_INFO  = 00
      SELECT (LCWORKFILE)
    ENDIF

    * -- Each time the color changes the following will be done :
    * -- 1) Display the new color on the screen.
    * -- 2) Update the current color variable "XCOLOR".
    * -- 4) Reinitialize "STY_INFO".
    * -- 5) Put the stock for this color on the 1st half or on the 2nd half
    * --    of the first month as a starting number for all the size scale
    * --    "STY_INFO [XSIZE_NO,(1 or 2)]".
    * -- 6) Put the shipped quantities on the last col. of the array for all
    * --    the size scale "STY_INFO [XSIZE_NO,12]" .
    * -- 7) If the report format is not by size then add the 2 quantities
    * --    (*No.5,6) on the last row of the array to accumulate them.

    XCOLOR    = IIF(LLHAVECLR,COLOR,CNONMAJOR)
    LCFULLSTY = STYLE

    STY_INFO = 0.00

    *B500568,1 Call a function to compute the ordered & shiped
    *B500568,1 quantities only for orders and invoices within
    *B500568,1 the current fiscal year.
    IF LLRPCURPRD
      STORE 0 TO LNORD1,LNORD2,LNORD3,LNORD4,LNORD5,LNORD6,LNORD7,LNORD8
      STORE 0 TO LNSHP1,LNSHP2,LNSHP3,LNSHP4,LNSHP5,LNSHP6,LNSHP7,LNSHP8
      = LFGTYTD()
    ENDIF

    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XSTK_ID = 'STK' + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,01] = &XSTK_ID
      *-- YMA 06/09/94 Added the ordered quantity to the last column.
      *B500568,1 If the user selected to print the sold year
      *B500568,1 to date for current year only, Accumulate
      *B500568,1 the order and ship quantities from the order
      *B500568,1 and invoice files otherwise get the order and
      *B500568,1 ship quantities from the style file.
      XSHP_ID = IIF(LLRPCURPRD,'lnShp','SHP') + ALLTRIM(STR(XSIZE_NO))
      LCORDID = IIF(LLRPCURPRD,'lnOrd','ORD') + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,12] = &XSHP_ID + &LCORDID
    ENDFOR

    SELECT (LCWORKFILE)

    *-- According to "MAKE" field please call either "ORD971" or "ORD972".
    IF MAKE

      IF 'MF' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES
        DO ORD972
      ENDIF

    ELSE  && Imported style

      IF 'PO' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES
        DO ORD971
      ENDIF

    ENDIF

    IF !LLOTSBASTR
      DO POST_NVE_VALUS
    ELSE
      DO LPUPDOTS
    ENDIF
    *B120121,1 MMM [End]

    * -- Then we will print the OTS lines .
    DO DISP_OTS_LINES
  ENDIF

  * -- Go to the next record .
  SELECT (LCWORKFILE)
  SKIP
ENDDO

SET DEVICE TO SCREEN
= GFTHERMO(LNUPTHERMO,LNUPTHERMO,'Finish Printing Stock availability',' ')
*WAIT WINDOW 'Calculate availability for ' + lcStyMajor + ': ' + XSTYLE NOWAIT
SET DEVICE TO PRINT

* --- This part is to print the "GRAND TOTAL" --------------------------
* --- It will be done when the report is finished .
IF ROW > XMAXROW
  DO DISP_TITLES
ENDIF

ROW = ROW + 1
@ ROW,0 SAY REPLICATE ('*',132)
ROW = ROW + 1
@ ROW,5 SAY 'GRAND TOTAL'
XCOL = 36
FOR XLOOP = 1 TO 12
  XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
  @ ROW,XCOL SAY XTOTAL [XLOOP] PICTURE '9999999'
  XCOL = XCOL + 8
ENDFOR
ROW = ROW + 1
@ ROW,0 SAY REPLICATE ('*',132)
* --- End of printing the "GRAND TOTAL" -------------------------------

*-- end of lpPrnCode. DO


*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! PROG : DISP_TITLES
*! DATE : JAN 24,94
*! AUTH : YASSER MOHAMMED ALY (YMA)
*! DESC : Print the report titles and initialize the necessary variables .
*! NOTE : Called from ORD970.PRG
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE DISP_TITLES

PAGENO  = PAGENO  + 1
DO RPT_HDR WITH 'SOSTKAV','',R_WIDTH
@ 5,26 SAY XLINE_1
@ 6,00 SAY XLINE_2
ROW = 7
* ------------------ END OF DISP_TITLES PROCEDURE ------------------ *



*********** BADRAN

*!*************************************************************
*! Name      : ORD971
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Procedure to compute the OTS quantites
*!*************************************************************
*!
PROCEDURE ORD971
IF SEEK('0001' + LCFULLSTY,'POSLN')
  *loDBFPOSLN.SEEK('0001' + lcFullSty,'Poslns')
  SELECT POSLN
  *B612418,1 MMT 06/27/2021 The stock availability report does not consider the shipments Qty done on POs in the report calculations[T20210518.0001][START]
*!*	  SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO)+TRANCD = '0001' +  LCFULLSTY ;
*!*	      FOR !(POSLN.CSTYTYPE $ 'C').AND. TRANCD $ '12' AND ;
*!*	      IIF(CBUSDOCU+CSTYTYPE = 'PU',STAT_PO $ 'OAH' ,STAT_PO $ 'HO')
  SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO)+TRANCD = '0001' +  LCFULLSTY ;
      FOR !(POSLN.CSTYTYPE $ 'C').AND. TRANCD $ '123' AND ;
      IIF(CBUSDOCU+CSTYTYPE = 'PU',STAT_PO $ 'OAH' ,STAT_PO $ 'HO')
  *B612418,1 MMT 06/27/2021 The stock availability report does not consider the shipments Qty done on POs in the report calculations[T20210518.0001][End]      
    *AND loDBFPOSHDR.SEEK(POSLN.cbusdocu+posln.cstytype+posln.po,.F.,.T.) AND CUTTKTH.STATUS $ 'OAH'
    *    XTRAN_DATE = IIF (EMPTY(POSHDR.AVAILABLE) ,;
    POSHDR.COMPLETE         ,;
    POSHDR.AVAILABLE )
    
    XTRAN_DATE = IIF (EMPTY(PO_AVL) ,;
      PO_COMP  ,;
      PO_AVL)
    
    LCTRSIGN = IIF(TRANCD='1',1,-1) * IIF(CSTYTYPE $ 'ADPNU',1,-1)
    DO ACUM_OTS WITH 'POSLN' , XTRAN_DATE , LCTRSIGN

    IF TRANCD ='3'
      LODBFSHPMTHDR.SEEK(POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.SHIPNO,.F.,.T.)
      DO ACUM_OTS WITH 'POSLN' , SHPMTHDR->ETA , 1
    ENDIF
  ENDSCAN
ENDIF
* -- To subtract the proper quantities in the "ORDLINE" file .
DO SUB_ORDQTY

*!*************************************************************
*! Name      : ORD972
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Procedure to compute the OTS quantites
*!*************************************************************
*!
PROCEDURE ORD972
IF SEEK('0001' + LCFULLSTY + 'PU','POSLN')
  *loDBFPOSLN.SEEK('0001' + lcFullSty+'PU','Poslns')
  SELECT POSLN
  SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO)+TRANCD ='0001' +  LCFULLSTY + 'PU' ;
      FOR TRANCD $ '12' AND  STAT_PO $ 'HO'
    *loDBFPOSHDR.SEEK(POSLN.cbusdocu+posln.cstytype+posln.po,.F.,.T.) AND ;
    POSHDR.STATUS $ 'OAH'

    * -- To accumulate the quantity in process.
    *    DO ACUM_OTS WITH 'POSLN' , POSHDR.COMPLETE , IIF (TRANCD='1',1,-1)
    DO ACUM_OTS WITH 'POSLN' , PO_COMP , IIF (TRANCD='1',1,-1)
  ENDSCAN
ENDIF
* -- To subtract the proper quantities in the "ORDLINE" file .
DO SUB_ORDQTY
*!*************************************************************
*! Name      : SUB_ORDQTY
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Procedure to substract the order line quantities from the
*!        OTS quantities .
*!*************************************************************
*!
PROCEDURE SUB_ORDQTY

* -- Remember that there is a relation between
* -- "ORDLINE" -> "ORDHDR"  to check on the status of the order .
IF LODBFORDLINE.SEEK(LCFULLSTY,'ORDLINES')
  SELECT ORDLINE
  SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = LCFULLSTY ;
      FOR LODBFORDHDR.SEEK(ORDLINE.CORDTYPE +ORDLINE.ORDER) AND ORDHDR.STATUS $ 'OH'

    * -- To accumulate the quantity in process .
    DO ACUM_OTS WITH 'ORDLINE' , START , -1
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : ACUM_OTS
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Procedure to accumulate the OTS quantities .
*!*************************************************************
*!
PROCEDURE ACUM_OTS
PARAMETERS XFILE , XTRANS_DATE , XTYPE

SELECT (XFILE)
STORE 0 TO LANETQTY
IF XFILE = 'POSLN'
  STORE 0 TO LANETQTY
  LNRECNO = RECNO(XFILE)
  IF TRANCD = '2'
    LCCURRKEY = '0001'+POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO+STR(POSLN.LINENO,6)
    LCSESSION = POSLN.CRSESSION
    =SEEK('0001'+POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO+STR(POSLN.LINENO,6),'POSLN')
    SCAN REST WHILE CINVTYPE + STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = LCCURRKEY
      IF TRANCD = '1'
        FOR XSIZE_NO = 1 TO XSIZE_CNT
          LCQTYNO = 'QTY' + ALLTRIM(STR(XSIZE_NO))
          LANETQTY[XSIZE_NO] = LANETQTY[XSIZE_NO] + &LCQTYNO
        ENDFOR
      ELSE
        FOR XSIZE_NO = 1 TO XSIZE_CNT
          LCQTYNO = 'QTY' + ALLTRIM(STR(XSIZE_NO))
          LANETQTY[XSIZE_NO] = LANETQTY[XSIZE_NO] - &LCQTYNO
        ENDFOR
      ENDIF
      IF (POSLN.CRSESSION = LCSESSION )
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  GOTO LNRECNO
ENDIF
* -- If the transaction date between the (1st OR 2nd) half of the first
* -- month and the last half of the last month we will do the following :
* -- ......
IF BETWEEN(XTRANS_DATE,XPERIOD_ARR[1],XPERIOD_ARR[20])
  * -- Loop around the size scale to check it all.
  FOR XSIZE_NO = 1 TO XSIZE_CNT
    XPER = 1
    * -- Search for the right period to process .
    DO WHILE XPER < 20

      * -- If the transaction date is between the current period "XPER" ..
      * -- Get all the quantities in process , then add them to the
      * -- proper cells in the array [Size,Period] .
      * -- And add them to the last row of the array [Max Size+1,Period]
      * -- to acumulate them (Get the sum) if the report format is not by
      * -- size.
      IF BETWEEN (XTRANS_DATE,XPERIOD_ARR[XPER],XPERIOD_ARR[XPER+1])
        XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
        IF LANETQTY[XSIZE_NO] < 0
          XQTY    =  (LANETQTY[XSIZE_NO] + &XQTY_ID) * XTYPE
        ELSE
          XQTY    =  &XQTY_ID * XTYPE
        ENDIF

        STY_INFO [XSIZE_NO,ROUND(XPER/2,0)] = ;
          STY_INFO [XSIZE_NO,ROUND(XPER/2,0)] + XQTY
        EXIT
      ENDIF
      XPER = XPER + 2
    ENDDO
  ENDFOR
ELSE
  * -- If the transaction date is after the last half of the last month
  * -- the following will be done :
  IF XTRANS_DATE > XPERIOD_ARR[20]
    *IF {XTRANS_DATE} > {XPERIOD_ARR[20]}
    * -- Add the quantities in process to the last period for all the
    * -- size scale STY_INFO [XSIZE_NO,10] .
    * -- And add them - for all the size scale - to the last row of the
    * -- array STY_INFO [XSIZE_CNT+1,10] to acumulate it if the report
    * -- format is not by size.
    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
      IF LANETQTY[XSIZE_NO] < 0
        XQTY    = (LANETQTY[XSIZE_NO] + &XQTY_ID) * XTYPE
      ELSE
        XQTY    =  &XQTY_ID * XTYPE
      ENDIF
      STY_INFO [XSIZE_NO,10] = STY_INFO [XSIZE_NO,10] + XQTY
    ENDFOR
  ELSE
    *-- that have a date prior to the first date to the first period
    *-- in the report.
    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XQTY_ID = 'QTY' + ALLTRIM(STR(XSIZE_NO))
      IF LANETQTY[XSIZE_NO] < 0
        XQTY    = (LANETQTY[XSIZE_NO] + &XQTY_ID) * XTYPE
      ELSE
        XQTY    =  &XQTY_ID * XTYPE
      ENDIF
      STY_INFO [XSIZE_NO,1] = STY_INFO [XSIZE_NO,1] + XQTY
    ENDFOR
  ENDIF
ENDIF

*!*************************************************************
*! Name      : POST_NVE_VALUS
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Procedure to post the negative values in any period to the
*!        nearst positive value of any next period.
*!        - We do this by looping the two dimensional array "STY_INFO"
*!          in case of the report format is by size , otherwise we will
*!          loop just the last row of the same array (which holds the
*!          total OTS quantities for this color) ,to do the following :
*!          - If there is a negative value , we will make it 0 and subtract
*!            it from the nearst next period has a POSITIVE value.
*!*************************************************************
*!
PROCEDURE POST_NVE_VALUS

XON_HAND = 0
FOR XSIZE_NO = 1 TO XSIZE_CNT
  FOR XPER = 1 TO 10
    IF STY_INFO [XSIZE_NO,XPER] < 0    && -VE NUMBER
      XON_HAND = STY_INFO [XSIZE_NO,XPER]
      STY_INFO [XSIZE_NO,XPER] = 0
      IF XPER = 1
        DO POST_BF WITH 2,10                     && GO_BACKWORD   ->
      ELSE
        DO POST_BF WITH XPER-1,1                 && GO_BACKWORD   <-
        IF (XON_HAND < 0) .AND. (XPER < 10)
          DO POST_BF WITH XPER+1,10              && GO_FOREWORD   ->
        ENDIF
      ENDIF

    ENDIF   && ---------------------->     -VE NUMBER
  ENDFOR
  *-- YMA 06/20/94 To put the remainning -ve quantity in the last period.
  IF XON_HAND < 0
    STY_INFO [XSIZE_NO,10] = XON_HAND
  ENDIF
  XON_HAND = 0
ENDFOR

*!*************************************************************
*! Name      : POST_BF
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   :
***************************************************************
PROCEDURE POST_BF
PARAMETERS INIT_PER , END_PER

X_LLOOP = IIF (INIT_PER > END_PER , 'INIT_PER TO END_PER  STEP -1', ;
  'INIT_PER TO END_PER'           )
FOR X_XPER = &X_LLOOP
  XON_HAND =  XON_HAND + STY_INFO [XSIZE_NO,X_XPER]
  IF XON_HAND <= 0
    STY_INFO [XSIZE_NO,X_XPER] = 0
  ELSE
    STY_INFO [XSIZE_NO,X_XPER] = XON_HAND
    XON_HAND = 0
    EXIT
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : DISP_OTS_LINES
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : PRINT THE OTS QUANTITIES.
***************************************************************
PROCEDURE DISP_OTS_LINES

*-- YMA 05/29/94 To Accumulate the OTS quantities.
IF !LLRPSIZES
  FOR XPER = 1 TO 12
    FOR XSIZE_NO = 1 TO XSIZE_CNT
      STY_INFO [XSIZE_CNT+1,XPER] = STY_INFO [XSIZE_CNT+1,XPER] + ;
        STY_INFO [XSIZE_NO,XPER]
    ENDFOR
  ENDFOR
ENDIF

* -- Loop to accumulate the the OTS lines and total to the 11th col.
* -- in the array .
FOR XSIZE_NO = 1 TO XSIZE_CNT+IIF (LLRPSIZES,0,1)
  *B120121,1 MMM 11/13/2003 Fix Bug of not Printing the OTS for each period IF the setting of
  *                         OTS based on exact transaction date = 'Yes' [Start]
  *FOR XPER = 1 TO 10
  *  STY_INFO [XSIZE_NO,11] = STY_INFO [XSIZE_NO,11] + ;
  STY_INFO [XSIZE_NO,XPER]
  *ENDFOR
  IF !LLOTSBASTR
    FOR XPER = 1 TO 10
      STY_INFO [XSIZE_NO,11] = STY_INFO [XSIZE_NO,11] + ;
        STY_INFO [XSIZE_NO,XPER]
    ENDFOR
  ELSE
    STY_INFO [XSIZE_NO,11] = STY_INFO[XSIZE_NO,10]
  ENDIF
  *B120121,1 MMM [End]
ENDFOR

XFIRST_TIME = .T.    && Flag to avoid printing the color more than once.

* -- If the report format is by size scale the following will be done
* -- for each color ...
*!*  IF llRpSizes
*!*    * -- loop around the Two dimensional array "STY_INFO" to print the
*!*    * -- OTS line for each [color\size] .

*!*    * -- This is the outer loop [sizes]
*!*    FOR  XSIZE_NO = 1 TO XSIZE_CNT

*!*      *-- Added by TAK 05/25/94
*!*      IF !llRpZeroAv
*!*       IF STY_INFO [XSIZE_NO,11] = 0 .AND. ;
*!*          STY_INFO [XSIZE_NO,12] = 0
*!*          LOOP
*!*        ENDIF
*!*      ENDIF

*!*      IF llPrintSty
*!*        *-- Print the style and the style description.
*!*        @ ROW,00 SAY XSTYLE
*!*        @ ROW,lnDescSt SAY XDESC
*!*        llPrintSty =.F.
*!*        ROW = ROW + 1
*!*      ENDIF
*!*      *--End TAK 05/25/94 ---

*!*      * -- Print the size .
*!*      XSIZE = SUBSTR (XSCALES,((XSIZE_NO-1)*5)+1,5)          && TMI 12/01/94

*!*      @ ROW,lnSzStart SAY XSIZE

*!*      * -- If we have not print the color then please print it.
*!*      IF XFIRST_TIME
*!*        @ ROW,lnClrStart SAY XCOLOR
*!*        lcClrDesc = PADR(gfCodDes(XCOLOR , 'COLOR'  ),lnTitlLen+3)
*!*        @ ROW,lnDescSt SAY lcClrDesc
*!*        XFIRST_TIME = .F.
*!*      ENDIF

*!*      * -- This is the inner loop [Periods].
*!*      XCOL  = 36
*!*      FOR XLOOP = 1 TO 12
*!*        XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
*!*        @ ROW,XCOL SAY STY_INFO [XSIZE_NO,XLOOP] PICTURE '9999999'
*!*        XCOL = XCOL + 8

*!*        * -- To add this line to the "XTOTAL" array .
*!*        XTOTAL [XLOOP] = XTOTAL [XLOOP] + STY_INFO [XSIZE_NO,XLOOP]
*!*      ENDFOR
*!*      ROW = ROW + 1
*!*    ENDFOR

*!*  ELSE

*!*    * -- If the report format is not by size the following will be done
*!*    * -- for each color ....

*!*    *-- Added by TAK 05/25/94
*!*    IF !llRpZeroAv
*!*     IF STY_INFO [XSIZE_CNT+1,11] = 0 .AND. ;
*!*        STY_INFO [XSIZE_CNT+1,12] = 0
*!*        RETURN
*!*      ENDIF
*!*    ENDIF

*!*    IF llPrintSty
*!*      *-- Print the style and the style description.
*!*      @ ROW,00 SAY XSTYLE
*!*      @ ROW,lnDescSt SAY XDESC
*!*      llPrintSty =.F.
*!*      ROW = ROW + 1
*!*    ENDIF
*!*    *--End TAK 05/25/94 ---

*!*    * -- Print the color and the color description.
*!*    @ ROW,lnClrStart SAY XCOLOR
*!*    lcClrDesc = PADR(gfCodDes(XCOLOR , 'COLOR'  ),lnTitlLen+3)
*!*    @ ROW,lnDescSt SAY lcClrDesc
*!*    XCOL  = 36

*!*    * -- Loop around the "STY_INFO" array to print a single line
*!*    * -- ( Total sizes for one color ) .
*!*    FOR XLOOP = 1 TO 12
*!*      * -- To print the quantities for this color.
*!*      XCOL = IIF ( XLOOP = 12 , XCOL+1 , XCOL )
*!*      @ ROW,XCOL SAY STY_INFO [XSIZE_CNT+1,XLOOP] PICTURE '9999999'
*!*      XCOL = XCOL + 8

*!*      * -- To accumulate the quantities in "XTOTAL" array.
*!*      XTOTAL [XLOOP] = XTOTAL [XLOOP] + STY_INFO [XSIZE_CNT+1,XLOOP]
*!*    ENDFOR

*!*    ROW = ROW + 1
*!*  ENDIF

* -------------------- END OF DISP_OTS_LINES PROCEDURE ---------------- *
*!*************************************************************
*! Name      : lfGtYTD
*: Developer : MAriam Mazhar (MMT)
*: Date      : 07/11/2006
*! Purpose   : Calculate ordered and shipped quantites.
*!*************************************************************
*!
FUNCTION LFGTYTD
PRIVATE LNALIAS,LCCENTUARY
LNALIAS = SELECT()
LCCENTUARY = SET('CENTURY')
SET CENTURY ON

IF LODBFORDLINE.SEEK(LCFULLSTY,'ORDLINES')
  SELECT ORDLINE
  SUM REST  QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 ;
    WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = LCFULLSTY ;
    FOR  LODBFORDHDR.SEEK(ORDLINE.CORDTYPE + ORDLINE.ORDER) AND  ORDHDR.STATUS $ 'OH' .AND. OARIAAPPLICATION.CURRENTYEAR = STR(YEAR(ORDHDR.ENTERED),4) ;
    TO    LNORD1,LNORD2,LNORD3,LNORD4,LNORD5,LNORD6,LNORD7,LNORD8
ENDIF

IF 'AR' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES AND LODBFINVLINE.SEEK(LCFULLSTY,'INVLINES')
  SELECT INVLINE
  SUM REST  QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8 ;
    WHILE STYLE+INVOICE+STR(LINENO,6) = LCFULLSTY ;
    FOR   LODBFINVHDR.SEEK(INVLINE.INVOICE) AND INVHDR.STATUS <> 'V' .AND. OARIAAPPLICATION.CURRENTYEAR = STR(YEAR(INVDATE),4) ;
    TO    LNSHP1,LNSHP2,LNSHP3,LNSHP4,LNSHP5,LNSHP6,LNSHP7,LNSHP8
ENDIF

SET CENTURY &LCCENTUARY
SELECT (LNALIAS)
RETURN (.T.)

*!*************************************************************
*! Name      : lpUpdOTS
*! Developer : Mariam Mazhar Tawfik (MMT)
*! Date      : 07/11/2006
*! Purpose   : Update OTS qty. in each period.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   :
*!*************************************************************
PROCEDURE LPUPDOTS

FOR XSIZE_NO = 1 TO XSIZE_CNT
  LNCURRENT = STY_INFO [XSIZE_NO,1]
  FOR I = 2 TO 10
    STY_INFO [XSIZE_NO,I] = STY_INFO [XSIZE_NO,I] + LNCURRENT
    LNCURRENT = STY_INFO[XSIZE_NO,I]
  ENDFOR
ENDFOR

*!*************************************************************
*! Name      : lfFabSum
*! Developer : MAriam Mazhar[MMT]
*! Date      : 06/11/2006
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION LFFABSUM
LPARAMETERS LCFAB, LCCOMP
LOCAL LNTOTCOMP,  LNALIAS
LNTOTCOMP = 0

IF SEEK(ALLTRIM(LCFAB), LCTMPFAB)
  SUM &LCTMPFAB..&LCCOMP. TO LNTOTCOMP WHILE &LCTMPFAB..FABRIC = LCFAB
ENDIF

RETURN LNTOTCOMP
*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/11/2006
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION LFCREATEINDECIES
LPARAMETERS LCCURSOR, LCINDEX, LCTAGES

LOCAL LNOLDBUFFMODE, LCINDEX1, LCTAGES1, LCINDEXP

*--If Query Successfully executed, Create Indexes if needed for the result cursor
LNOLDBUFFMODE = CURSORGETPROP("Buffering", LCCURSOR)
=CURSORSETPROP("Buffering", 3, LCCURSOR)

LCTAGES1 = LCTAGES
LCINDEX1 = LCINDEX
SELECT (LCCURSOR)
DO WHILE AT("|", LCINDEX1,1) <> 0
  LCINDEX  = SUBSTR(LCINDEX1, 1, AT("|", LCINDEX1, 1) - 1)
  LCINDEX1 = STRTRAN(LCINDEX1, LCINDEX + "|", "", 1, 1)
  LCTAGES  = SUBSTR(LCTAGES1, 1, AT("|", LCTAGES1, 1) - 1)
  LCTAGES1 = STRTRAN(LCTAGES1, LCTAGES + "|", "", 1, 1)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &LCINDEX. TAG (LCTAGES)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', LNBUFFMODE, LNOLDBUFFMODE), LCCURSOR)

RETURN .T.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/11/2006
*! Purpose   : Collect data
*!*************************************************************
FUNCTION LFCOLLECT

*E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][Start]
IF !USED("Objects")
  =gfOpenTable("Objects",'OBJECTID')
ENDIF
IF !USED("ObjLink")
  =gfOpenTable("ObjLink",'OBJDEFA')   && CDEF_OBJ+COBJLNKTYP+COBJLINK+COBJECT_ID
ENDIF
*E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][End]

LLUSESTYLE = .F.
LLUSEDIV   = .F.
LLUSESEA   = .F.
LLUSEGRP   = .F.
LLUSEFAB   = .F.
LLUSECLR   = .F.

*--Style major
LNPOSITION = ASUBSCRIPT(LOOGSCROLL.LAOGFXFLT,ASCAN(LOOGSCROLL.LAOGFXFLT,'STYLE.CSTYMAJOR'),1)
IF LNPOSITION > 0
  LCSTYLFILE = LOOGSCROLL.LAOGFXFLT[lnPosition,6]
  LLUSESTYLE = IIF(!EMPTY(LCSTYLFILE) .AND. USED(LCSTYLFILE) .AND. RECCOUNT(LCSTYLFILE)>0,.T.,.F.)
ENDIF
IF LLUSESTYLE
  SELECT(LCSTYLFILE)
  LOCATE
  IF EOF()
    LLUSESTYLE = .F.
  ENDIF
ENDIF

*---Division Filter
LNDIVPOS = ASUBSCRIPT(LOOGSCROLL.LAOGVRFLT,ASCAN(LOOGSCROLL.LAOGVRFLT,'STYLE.CDIVISION'),1)
IF LNDIVPOS  > 0
  LCDIVSTR = LOOGSCROLL.LAOGVRFLT[lnDivPos,6]
  LCDIVFILE = LOOGSCROLL.GFTEMPNAME()
  LLUSEDIV = IIF(LEN(LCDIVSTR)>0,.T.,.F.) AND LFCONVERTTOCURSOR(LCDIVSTR,'CDIVISION',LCDIVFILE)
ENDIF

*---Season Filter
LNSEAPOS = ASUBSCRIPT(LOOGSCROLL.LAOGVRFLT,ASCAN(LOOGSCROLL.LAOGVRFLT,'STYLE.SEASON'),1)
IF LNSEAPOS > 0
  LCSEASTR = LOOGSCROLL.LAOGVRFLT[lnSeaPos,6]
  LCSEAFILE = LOOGSCROLL.GFTEMPNAME()
  LLUSESEA = IIF(LEN(LCSEASTR)>0,.T.,.F.) AND LFCONVERTTOCURSOR(LCSEASTR,'SEASON',LCSEAFILE)
ENDIF

*--Style Group
LNGRPPOS = ASUBSCRIPT(LOOGSCROLL.LAOGVRFLT,ASCAN(LOOGSCROLL.LAOGVRFLT,'STYLE.CSTYGROUP'),1)
IF LNGRPPOS > 0
  LCGRPSTR = LOOGSCROLL.LAOGVRFLT[lnGrpPos,6]
  LCGRPFILE = LOOGSCROLL.GFTEMPNAME()
  LLUSEGRP = IIF(LEN(LCGRPSTR)>0,.T.,.F.) AND LFCONVERTTOCURSOR(LCGRPSTR,'CSTYGRP',LCGRPFILE)
ENDIF

*--Style Fabric
LNPOSITION = ASUBSCRIPT(LOOGSCROLL.LAOGVRFLT,ASCAN(LOOGSCROLL.LAOGVRFLT,'STYLE.FABRIC'),1)
IF LNPOSITION > 0
  LCFABFILE = LOOGSCROLL.LAOGVRFLT[lnPosition,6]
  LLUSEFAB = IIF(!EMPTY(LCFABFILE) .AND. USED(LCFABFILE) .AND. RECCOUNT(LCFABFILE)>0,.T.,.F.)
ENDIF
IF LLUSEFAB
  SELECT(LCFABFILE)
  LOCATE
  IF EOF()
    LLUSEFAB = .F.
  ENDIF
ENDIF


*--Style Color
LNCLRPOS = ASUBSCRIPT(LOOGSCROLL.LAOGFXFLT,ASCAN(LOOGSCROLL.LAOGFXFLT,'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)'),1)
IF LNCLRPOS > 0
  LCCLRSTR = LOOGSCROLL.LAOGFXFLT[lnClrPos,6]
  LCCLRFILE = LOOGSCROLL.GFTEMPNAME()
  LLUSECLR = IIF(LEN(LCCLRSTR)>0,.T.,.F.) AND LFCONVERTTOCURSOR(LCCLRSTR,'CSTYCLR',LCCLRFILE)
ENDIF

*--Pattern
LCPATTSTR = ""
LNPATTPOS = ASUBSCRIPT(LOOGSCROLL.LAOGVRFLT,ASCAN(LOOGSCROLL.LAOGVRFLT,'STYLE.PATTERN'),1)
IF LNPATTPOS > 0
  LCPATTSTR = LOOGSCROLL.LAOGVRFLT[lnPattPos,6]
ENDIF

LCSTATSTR  = ""
IF LCRPSTATUS <>'B'
  LCSTATSTR  = IIF(LCRPSTATUS='B','',"Status='" + LCRPSTATUS+"'")
ENDIF

XTODAY     = OARIAAPPLICATION.SYSTEMDATE
C_MONTH    = MONTH (XTODAY)
C_YEAR     = YEAR  (XTODAY)
XSTYLE     = SPACE (12)
XCOLOR     = SPACE (06)
LCFULLSTY  = SPACE(19)
LLPRINTSTY = .F.

DIMENSION XPERIOD_ARR [20] , XYEAR [12] , STY_INFO [1,12] , XTOTAL [12]
XPERIOD_ARR = {}
XYEAR       = 31
STORE 30 TO XYEAR(4),XYEAR(6),XYEAR(9),XYEAR(11)
XYEAR (2)   = IIF (MOD(YEAR(XTODAY),4)=0 , 29 , 28)
STY_INFO    = 00
XTOTAL      = 00
LNTITLLEN = 25 - ( LEN(LCSTYMAJOR) + LEN(LCOTHERTLT) )
* -- Initialize the first period with the first day of today's month .
IF LLENGDATE
  XPERIOD_ARR(1) = CTOD('01/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
  XPERIOD_ARR(2) = CTOD('15/'+ALLTRIM(STR(C_MONTH))+'/'+ALLTRIM(STR(C_YEAR)))
ELSE
  XPERIOD_ARR(1) = CTOD(ALLTRIM(STR(C_MONTH))+'/01/'+ALLTRIM(STR(C_YEAR)))
  XPERIOD_ARR(2) = CTOD(ALLTRIM(STR(C_MONTH))+'/15/'+ALLTRIM(STR(C_YEAR)))
ENDIF

* -- Fill the "XPERIOD_ARR" array and form the report titles.
XLOOP   = 3
XPERIOD = 1
XMONTH  = 0

DO WHILE XPERIOD < 10
  * -- Fill the periods array.
  XPERIOD_ARR(XLOOP  ) = XPERIOD_ARR(XLOOP-1) + 1
  XPERIOD_ARR(XLOOP+1) = IIF ( MOD(XPERIOD,2) = 0 ,               ;
    XPERIOD_ARR(XLOOP) + 14          , ;
    GOMONTH (XPERIOD_ARR(XLOOP-2),1)-1 )
  XLOOP   = XLOOP   + 2
  XPERIOD = XPERIOD + 1
ENDDO
LCSTYMAJ = SPACE(19)

IF LLUSESTYLE
  SELECT(LCSTYLFILE)
  SCAN
    *B610448,1 HIA 07/28/13 T20130723.0008 - Stock Availablity report problem:some colors are not showing up. [Begin]
    *=LODBFSTYLE.SEEK(&LCSTYLFILE..CSTYMAJOR,'CSTYLE')
    =LODBFSTYLE.SEEK(SUBSTR(&LCSTYLFILE..CSTYMAJOR,1,LNMAJORLEN),'STYLE')    
    *B610448,1 HIA 07/28/13 T20130723.0008 - Stock Availablity report problem:some colors are not showing up. [End]
    
    SELECT STYLE
    
    *B610448,1 HIA 07/28/13 T20130723.0008 - Stock Availablity report problem:some colors are not showing up. [Begin]
    *SCAN REST WHILE CSTYMAJOR = &LCSTYLFILE..CSTYMAJOR FOR IIF(LLUSECLR,SEEK(SUBSTR(STYLE.STYLE,LNNONMAJST,LNCOLORLEN),LCCLRFILE),.T.);
    *    AND IIF(LLUSEDIV,SEEK(STYLE.CDIVISION,LCDIVFILE),.T.);
    *    AND IIF(LLUSESEA,SEEK(STYLE.SEASON,LCSEAFILE),.T.) ;
    *    AND IIF(LLUSEGRP,SEEK(STYLE.CSTYGROUP,LCGRPFILE),.T.);
    *    AND IIF(LLUSEFAB,SEEK(STYLE.FABRIC,LCFABFILE),.T.) ;
    *    AND IIF(!EMPTY(LCPATTSTR),STYLE.PATTERN = LCPATTSTR,.T.);
    *    AND IIF(!EMPTY(LCSTATSTR),EVAL(LCSTATSTR),.T.)
        
    SCAN REST WHILE Style = SUBSTR(&LCSTYLFILE..CSTYMAJOR,1,LNMAJORLEN) FOR IIF(LLUSECLR,SEEK(SUBSTR(STYLE.STYLE,LNNONMAJST,LNCOLORLEN),LCCLRFILE),.T.);
        AND IIF(LLUSEDIV,SEEK(STYLE.CDIVISION,LCDIVFILE),.T.);
        AND IIF(LLUSESEA,SEEK(STYLE.SEASON,LCSEAFILE),.T.) ;
        AND IIF(LLUSEGRP,SEEK(STYLE.CSTYGROUP,LCGRPFILE),.T.);
        AND IIF(LLUSEFAB,SEEK(STYLE.FABRIC,LCFABFILE),.T.) ;
        AND IIF(!EMPTY(LCPATTSTR),STYLE.PATTERN = LCPATTSTR,.T.);
        AND IIF(!EMPTY(LCSTATSTR),EVAL(LCSTATSTR),.T.)        
    *B610448,1 HIA 07/28/13 T20130723.0008 - Stock Availablity report problem:some colors are not showing up. [End]
    
      * N000682 ,1 Thabet Handle globalization issues [Start]
      *WAIT WINDOW  'Calculate availability for ' + &lcStylFile..CSTYMAJOR NOWAIT
      WAIT WINDOW  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CALCULATE,OARIAAPPLICATION.GETHEADERTEXT("LANG_Calculate",AHEADERFILE)) + &LCSTYLFILE..CSTYMAJOR NOWAIT
      * N000682 ,1 Thabet Handle globalization issues [END]

      IF LCSTYMAJ <> STYLE.CSTYMAJOR

        LODBFPOSLN.SQLRUN("Select Posln.*,Poshdr.Status AS Stat_PO,"+;
          "POSHDR.AVAILABLE As Po_avl,POSHDR.COMPLETE  As Po_Comp from Posln(index = poslns)"+;
          " inner join poshdr(index = poshdr) on posln.cbusdocu = poshdr.cbusdocu "+;
          " and posln.cstytype = poshdr.cstytype and posln.po = poshdr.po where posln.cinvtype = '0001'"+;
          " and POSLN.STYLE LIKE '"+LEFT(STYLE.CSTYMAJOR,LNMAJORLEN)+"%'")
        LCSTYMAJ = STYLE.CSTYMAJOR
      ENDIF
      SELECT STYLE
      LNCOUNTREC = LNCOUNTREC + 1
      SCATTER MEMVAR MEMO
      m.CMAJOR    = LEFT(STYLE,LNMAJORLEN)
      *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][Start]
      *m.CNONMAJOR = RIGHT(STYLE,LNNONMAJLN)
      m.CNONMAJOR = SUBSTR(STYLE,lnNonMajSt,LNNONMAJLN)
      *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][End]
      m.COLOR     = IIF(LLHAVECLR,SUBSTR(STYLE,LNNONMAJST,LNCOLORLEN),SPACE(6))
      IF LODBFSCALE.SEEK('S'+ m.SCALE)
        XSIZE_CNT = SCALE.CNT
      ENDIF
      LCFULLSTY  = SPACE(19)
      XCOLOR    = IIF(LLHAVECLR,COLOR,CNONMAJOR)
      LCFULLSTY = STYLE
      DIMENSION STY_INFO [XSIZE_CNT+IIF(llRpSizes,0,1),12]
      STORE 0 TO STY_INFO
      IF LLRPCURPRD
        STORE 0 TO LNORD1,LNORD2,LNORD3,LNORD4,LNORD5,LNORD6,LNORD7,LNORD8
        STORE 0 TO LNSHP1,LNSHP2,LNSHP3,LNSHP4,LNSHP5,LNSHP6,LNSHP7,LNSHP8
        = LFGTYTD()
      ENDIF

      FOR  XSIZE_NO = 1 TO XSIZE_CNT
        XSTK_ID = 'STK' + ALLTRIM(STR(XSIZE_NO))
        STY_INFO [XSIZE_NO,01] = &XSTK_ID
        XSHP_ID = IIF(LLRPCURPRD,'lnShp','SHP') + ALLTRIM(STR(XSIZE_NO))
        LCORDID = IIF(LLRPCURPRD,'lnOrd','ORD') + ALLTRIM(STR(XSIZE_NO))
        STY_INFO [XSIZE_NO,12] = &XSHP_ID + &LCORDID
      ENDFOR
      SELECT (LCWORKFILE)
      IF 'PO' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES OR 'MF' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES
        DO ORD971
      ENDIF
      IF !LLOTSBASTR
        DO POST_NVE_VALUS
      ELSE
        DO LPUPDOTS
      ENDIF
      * -- Then we will print the OTS lines .
      DO DISP_OTS_LINES
      *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][Start]
      *m.CLRDESC = PADR(LFGETDESC(XCOLOR),LNTITLLEN+3)
      m.CLRDESC = PADR(LFGETDESC(SUBSTR(LCFULLSTY ,LNNONMAJST,LNCOLORLEN)),LNTITLLEN+3)
      *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][End]
      *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][Start]
      m.lHasImage = .F.
      m.BFilePath = ''
      IF gfSeek('D'+'S'+PADR(m.CMAJOR ,19),"ObjLink",'OBJDEFA') AND gfSeek(ObjLink.COBJECT_ID,'Objects','OBJECTID')
        m.lHasImage = .T.
         *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
          *m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
          if (file(allt(Objects.mimgpath)))
            m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
          ENDIF
          *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
      ELSE
        IF gfSeek('S'+PADR(m.CMAJOR ,19),"ObjLink",'OBJLNKTY') AND gfSeek(ObjLink.COBJECT_ID,'Objects','OBJECTID')  && COBJLNKTYP+COBJLINK  
          m.lHasImage = .T.
		   *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
           *m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
           if (file(allt(Objects.mimgpath)))
             m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
           ENDIF
           *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
        ENDIF
      ENDIF  
      *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][End]
      
      FOR D = 1 TO XSIZE_CNT
        Y = ALLTRIM(STR(D,2))
        m.SZ = SCALE.SZ&Y.
        FOR I=1 TO 12
          X = ALLTRIM(STR(I,2))
          m.PER&X. = STY_INFO[d,i]
        ENDFOR
        INSERT INTO (LCWORKFILE) FROM MEMVAR
      ENDFOR
    ENDSCAN
  ENDSCAN
ELSE

  SELECT STYLE
  SCAN FOR IIF(LLUSECLR,SEEK(SUBSTR(STYLE.STYLE,LNNONMAJST,LNCOLORLEN),LCCLRFILE),.T.);
      AND IIF(LLUSEDIV,SEEK(STYLE.CDIVISION,LCDIVFILE),.T.);
      AND IIF(LLUSESEA,SEEK(STYLE.SEASON,LCSEAFILE),.T.) ;
      AND IIF(LLUSEGRP,SEEK(STYLE.CSTYGROUP,LCGRPFILE),.T.);
      AND IIF(LLUSEFAB,SEEK(STYLE.FABRIC,LCFABFILE),.T.) ;
      AND IIF(!EMPTY(LCPATTSTR),STYLE.PATTERN = LCPATTSTR,.T.);
      AND IIF(!EMPTY(LCSTATSTR),EVAL(LCSTATSTR),.T.)

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *WAIT WINDOW  'Calculate availability for ' + STYLE.CSTYMAJOR NOWAIT
    WAIT WINDOW  IIF(OARIAAPPLICATION.OACTIVELANG.CLANG_ID = "EN",LANG_CALCULATE_AVAILABILITY,OARIAAPPLICATION.GETHEADERTEXT("LANG_Calculate_availability",AHEADERFILE)) + STYLE.CSTYMAJOR NOWAIT
    * N000682 ,1 Thabet Handle globalization issues [END]
    IF LCSTYMAJ <> STYLE.CSTYMAJOR
      LODBFPOSLN.SQLRUN("Select Posln.*,Poshdr.Status AS Stat_PO ,"+;
        "POSHDR.AVAILABLE As Po_avl,POSHDR.COMPLETE  As Po_Comp from Posln(index = poslns)"+;
        " inner join poshdr(index = poshdr) on posln.cbusdocu = poshdr.cbusdocu "+;
        " and posln.cstytype = poshdr.cstytype and posln.po = poshdr.po where posln.cinvtype = '0001'"+;
        " and POSLN.STYLE LIKE '"+LEFT(STYLE.CSTYMAJOR,LNMAJORLEN)+"%'")
      LCSTYMAJ = STYLE.CSTYMAJOR
    ENDIF
    LNCOUNTREC = LNCOUNTREC + 1
    SELECT STYLE
    SCATTER MEMVAR MEMO
    m.CMAJOR    = LEFT(STYLE,LNMAJORLEN)
    *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][Start]
    *m.CNONMAJOR = RIGHT(STYLE,LNNONMAJLN)
    m.CNONMAJOR = SUBSTR(STYLE,lnNonMajSt,LNNONMAJLN)
    *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][End]
    m.COLOR     = IIF(LLHAVECLR,SUBSTR(STYLE,LNNONMAJST,LNCOLORLEN),SPACE(6))
    IF LODBFSCALE.SEEK('S'+ m.SCALE)
      XSIZE_CNT = SCALE.CNT
    ENDIF
    LCFULLSTY  = SPACE(19)
    XCOLOR    = IIF(LLHAVECLR,COLOR,CNONMAJOR)
    LCFULLSTY = STYLE
    DIMENSION STY_INFO [XSIZE_CNT+IIF(llRpSizes,0,1),12]
    STORE 0 TO STY_INFO
    IF LLRPCURPRD
      STORE 0 TO LNORD1,LNORD2,LNORD3,LNORD4,LNORD5,LNORD6,LNORD7,LNORD8
      STORE 0 TO LNSHP1,LNSHP2,LNSHP3,LNSHP4,LNSHP5,LNSHP6,LNSHP7,LNSHP8
      = LFGTYTD()
    ENDIF
    FOR  XSIZE_NO = 1 TO XSIZE_CNT
      XSTK_ID = 'STK' + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,01] = &XSTK_ID
      XSHP_ID = IIF(LLRPCURPRD,'lnShp','SHP') + ALLTRIM(STR(XSIZE_NO))
      LCORDID = IIF(LLRPCURPRD,'lnOrd','ORD') + ALLTRIM(STR(XSIZE_NO))
      STY_INFO [XSIZE_NO,12] = &XSHP_ID + &LCORDID
    ENDFOR
    SELECT (LCWORKFILE)
    IF 'PO' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES OR 'MF' $ OARIAAPPLICATION.COMPANYINSTALLEDMODULES
      DO ORD971
    ENDIF
    IF !LLOTSBASTR
      DO POST_NVE_VALUS
    ELSE
      DO LPUPDOTS
    ENDIF
    * -- Then we will print the OTS lines .
    DO DISP_OTS_LINES
    *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][Start]
    *m.CLRDESC = PADR(LFGETDESC(XCOLOR),LNTITLLEN+3)
    m.CLRDESC = PADR(LFGETDESC(SUBSTR(LCFULLSTY ,LNNONMAJST,LNCOLORLEN)),LNTITLLEN+3)
    *!* B612454,1 MMT 08/24/2021 issues have been found related to the Stock Availability report when printed with "No sizes" optio[T20210822.0002][End]
    *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][Start]
    m.lHasImage = .F.
    m.BFilePath = ''
    IF gfSeek('D'+'S'+PADR(m.CMAJOR ,19),"ObjLink",'OBJDEFA') AND gfSeek(ObjLink.COBJECT_ID,'Objects','OBJECTID')
      m.lHasImage = .T.
      *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
      *m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
      if (file(allt(Objects.mimgpath)))
        m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
      ENDIF
      *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END]
    ELSE
      IF gfSeek('S'+PADR(m.CMAJOR ,19),"ObjLink",'OBJLNKTY') AND gfSeek(ObjLink.COBJECT_ID,'Objects','OBJECTID')  && COBJLNKTYP+COBJLINK  
        m.lHasImage = .T.
        *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[Start]
        *m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
        if (file(allt(Objects.mimgpath)))
          m.BFilePath = FILETOSTR(ALLTRIM(Objects.mimgpath))
        ENDIF
        *E303691,1 derby 07/03/2016 -Convert Stock Availabilty report to work on request builder[END] 
      ENDIF
    ENDIF  
    *E303683,1 MMT 06/23/2016 Add Style Image to SO stock Avalialbility report when Print sizes option is NO[T20160621.0014][End]
    
    FOR D = 1 TO XSIZE_CNT && loop on sizes
      Y = ALLTRIM(STR(D,2))
      m.SZ = SCALE.SZ&Y.
      FOR I=1 TO 12 && loop on periods
        X = ALLTRIM(STR(I,2))
        m.PER&X. = STY_INFO[d,i]
      ENDFOR
      INSERT INTO (LCWORKFILE) FROM MEMVAR
    ENDFOR
  ENDSCAN
ENDIF &&Style filter if Statement
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION LFCONVERTTOCURSOR
PARAMETERS LCSTRTOCONV,LCFIELDNAME ,LCNEWFILE
LCCURSORTEMP = LCNEWFILE &&Cursor Hold Selected values
DIMENSION LATEMPACSTRU[1,4]
LATEMPACSTRU[1,1] = LCFIELDNAME

DO CASE

CASE   ALLTRIM(LCFIELDNAME) = 'SEASON'
  LATEMPACSTRU[1,2]='C'
  LATEMPACSTRU[1,3]= 6
  LATEMPACSTRU[1,4]= 0

CASE   ALLTRIM(LCFIELDNAME) = 'CDIVISION'
  LATEMPACSTRU[1,2]='C'
  LATEMPACSTRU[1,3]= 6
  LATEMPACSTRU[1,4]= 0

CASE   ALLTRIM(LCFIELDNAME) = 'CSTYGRP'
  LATEMPACSTRU[1,2]='C'
  LATEMPACSTRU[1,3]= 6
  LATEMPACSTRU[1,4]= 0

CASE   ALLTRIM(LCFIELDNAME) = 'CSTYCLR'
  LATEMPACSTRU[1,2]='C'
  LATEMPACSTRU[1,3]= 6
  LATEMPACSTRU[1,4]= 0

ENDCASE
= GFCRTTMP(LCCURSORTEMP ,@LATEMPACSTRU,LCFIELDNAME ,LCCURSORTEMP ,.T.)
LCVALUESTOCONVERT = LCSTRTOCONV
IF !EMPTY(LCVALUESTOCONVERT)
  LNSTART=1
  LNEND=AT('|',LCVALUESTOCONVERT )
  DO WHILE LNEND <> 0
    SELECT(LCCURSORTEMP )
    APPEND BLANK
    REPLACE &LCFIELDNAME  WITH SUBSTR(LCVALUESTOCONVERT,LNSTART,LNEND-1)
    LCVALUESTOCONVERT = STUFF(LCVALUESTOCONVERT ,LNSTART,LNEND,"")
    LNEND=AT('|',LCVALUESTOCONVERT )
  ENDDO
  IF LNEND = 0
    SELECT(LCCURSORTEMP )
    APPEND BLANK
    REPLACE &LCFIELDNAME  WITH LCVALUESTOCONVERT
  ENDIF
ENDIF
RETURN .T.
*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION LFGETDESC
PARAMETERS LCMFCODE
LCMFNAME = ""
LODBFCODES.SETORDER('Codes')
IF LODBFCODES.SEEK('N'+LCMFCODE+'N'+'COLOR     ')
  LCMFNAME = CODES.CDISCREP
ENDIF
RETURN LCMFNAME

