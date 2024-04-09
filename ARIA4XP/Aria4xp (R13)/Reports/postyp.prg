*:***************************************************************************
*: Program file  : POSTYP
*: Program desc. : Print PO, Contract and Return PO
*: For Report    : POSTYPA.FRX
*: System        : Aria Advantage Series ARIA4XP  
*: Module        : Purchase Order (PO)
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSTYPO
*:***************************************************************************
*: Due to  #038132
*: B127974,1 HFK 08/02/2005 Get shipto and shipvia on line level
*: TNA 04/06/2006 Make report work with MFPRCSA.FRX when Print BOM is Selected
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[T20090831.0015]
*:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price" [T20090831.0015]
*:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [T20090831.0015]
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [T20121107.0002]
*:E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*:N000382,1 MMT 02/26/2013 Globalization changes
*:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002]
*:B610597,1 TMI 11/21/2013 fix a problem that the interlocation notes are not shown [T20131113.0042 ] 
*!E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13]
*E304062 ,1 HMS 08/30/2018  - Aria 5 - how to remove table header[T20180725.0012]
*:****************************************************************************************************

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
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

SET STEP ON 
*B610597,1 TMI 11/21/2013 11:39 [Start] modify the value of lcRpForm based on the value of oAriaApplication.PROCESSID to be 'N' in case of Inter -location and 'R' in case of Return PO
DO CASE 
CASE oAriaApplication.PROCESSID = 'ARPOINTRC' 
  lcRpForm = 'N'
CASE oAriaApplication.PROCESSID = 'ARRETPO'
  lcRpForm = 'R'
ENDCASE  
*B610597,1 TMI 11/21/2013 11:39 [End  ] 

IF TYPE('lcXMLFileName') = 'C'
  PUBLIC gcRequestID, gcClientID
  gcRequestID = lcRequestID
  gcClientID = ClientId  
  
  PRIVATE loAgent
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  
  PRIVATE loProgress
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *loProgress = goRemoteCall.GetRemoteObject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]  
  
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  
  LOCAL loEnvironment  
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]  
  loEnvironment.ClientID = ClientID
  	
  LOCAL lcCurrentProcedure
  lcCurrentProcedure =    loEnvironment.Aria40SharedPath
  loEnvironment.ConnectionsRefresh()
  
  LOCAL lcRequestCompany, lcClientRoot, lcEnvOutput
  lcRequestCompany = loAgent.GetRequestCompany(lcRequestID, ClientId)
  lcClientRoot = loEnvironment.Aria40SharedPath 
   
  lcEnvOutput = loEnvironment.GetAria27CompanyDataConnectionString(lcRequestCompany)
  DO (lcCurrentProcedure + "SRVPRGS\SY\AriaMain.fxp") WITH lcRequestCompany , ClientId, lcCurrentProcedure, loEnvironment
  
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  
  PUBLIC gcAct_Appl
  gcAct_Appl  = 'PO'
  oAriaEnvironment.REPORT.gcAct_Appl = 'PO'
  oAriaEnvironment.ActiveModuleID = 'PO'
  oAriaEnvironment.RequestID = lcRequestID
  
  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF 
  
  oAriaEnvironment.Report.cCROrientation = 'P'
  
  llOgFltCh = .T.
ELSE
  loogscroll.cCROrientation = 'P'
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

#INCLUDE R:\ARIA4XP\REPORTS\POSTYP.h

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
**B130303 START &&change oraition of printing 
*loogScroll.cCROrientation = 'P'
**B130303 END
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

*:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[Start]
lnTotAmt = 0
*:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[End]

DIMENSION laCompAdd[6,1], laDivLName[1,2], laVenAdr[1,5],laShpAdr[1,5]
STORE SPACE(0) TO laCompAdd, laShpAdr, lcVenName, lcDivDesc, lcTerms, ;
                  lcShpName, lcShipVia, lcStyTitle, lcDivLName,;
                  lcTitle, lcNotes, lcCompPhon,laVenAdr, lcCompFax

*                  lcShpName, lcShipVia, lcStyTitle, lcDivLName, lcLogoPic, 

* TNA add Variables for MFPRCSA.FRX [Begin]
STORE SPACE(0) TO lcOper
lnMajor  = LEN(gfItemMask('PM'))
lnNMajor = LEN(gfItemMask('PN'))
* TNA add Variables for MFPRCSA.FRX [End]

*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][Start]
lcPOPrtUp = gfTempName()
CREATE CURSOR (lcPOPrtUp) (CBUSDOCU C(1), CSTYTYPE C(1), PO C(6))
INDEX ON CBUSDOCU+CSTYTYPE+PO TAG PO of (gcWorkDir+lcPOPrtUp)
SELECT 0
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][End]

DIMENSION laOpTmpFls[1,2]
STORE " " TO laOpTmpFls
STORE SPACE(0) TO lcFax

 
llMulCurr = gfGetMemVar('llMulCurr',oAriaApplication.ActiveCompanyID)

lluse_config = (ALLTRIM(gfGetMemVar("M_STYCNFG")) = "Y")

IF !USED('SYCCURR')  
  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
  *=gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  IF TYPE('lcXMLFileName') = 'C'
    =gfOpenFile(oAriaApplication.SystemFilesPath+'SYCCURR',oAriaApplication.SystemFilesPath+'Ccurrcode','SH')
  ELSE
    =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ENDIF
  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
ENDIF

IF !USED('Scale')
  =gfOpenFile(oAriaApplication.DataDir+'scale',oAriaApplication.DataDir+'scale','SH')
ENDIF

=SEEK(oAriaApplication.BaseCurrency,'SycCurr')

loOGScroll.nFontSize = 14

llPoDisp = .T.            && llPoDisp it will be a global variable.

STORE .F. TO llEndGroup, llLogo, llPrintBox
llPrntBoth = llRpPrtSn AND llRpPrtPn     && Flag to know we print both Style and order notes.
lcSkipExpr = 'POSLN'                     && Skip expression
STORE .T. TO llPrtSn, llTitle            && Variable to print style notes one time.
lnLstLn = 1

lnDisc  = 0
STORE {} TO ldStart, ldEnd
DIMENSION laDisc[3,2]
laDisc[1,1] = 'DISCPCNT'
laDisc[1,2] = "lnDisc"
laDisc[2,1] = 'START'
laDisc[2,2] = "ldStart"
laDisc[3,1] = 'DENDATE'
laDisc[3,2] = "ldEnd"
*lcSqlConnection = CREATEOBJECT('remotedataaccess')
* If there is a company logo , create a cursor hold this logo

lcStyTitle = gfItemMask('HI')
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcTime     = TIME()                       && Variable to hold the Time

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
IF TYPE('lcXMLFileName') = 'C'
  =gfOpenTable('STYLE','STYLE', 'SH', 'STYLE')
  =gfOpenTable('SCALE','SCALE', 'SH', 'SCALE')
  =gfOpenTable('NOTEPAD','NOTEPAD', 'SH', 'NOTEPAD')
  =gfOpenTable('SYCCOMP','CCOMP_ID', 'SH', 'SYCCOMP')
  =gfOpenTable('CODES','', 'SH', 'CODES')
  =gfOpenTable('OBJECTS','OBJECTID', 'SH', 'OBJECTS')
  =gfOpenTable('OBJLINK','OBJLNKTY', 'SH', 'OBJLINK')
  =gfOpenTable('POSHDR','POSHDR', 'SH', 'POSHDR')
  =gfOpenTable('POSLN','POSLN', 'SH', 'POSLN')
  =gfOpenTable('APVENDOR','VENCODE', 'SH', 'APVENDOR')
  =gfOpenTable('CUSTOMER','CUSTOMER', 'SH', 'CUSTOMER')
  =gfOpenTable('WAREHOUS','WAREHOUS', 'SH', 'WAREHOUS')
  =gfOpenTable('SYCCURR','CCURRCODE', 'SH', 'SYCCURR')
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

* Open necessary tables and make necessary relations.
TmpObjects =  loOgScroll.gfTempName()
TmpObjLink =  loOgScroll.gfTempName()
*E304062 , 1 HMS , 08/30/2018  - Aria 5 - how to remove table header[T20180725.0012][Begin]
lcPrtHead = IIF(TYPE('lcPrtHead') <> 'C','Y',lcPrtHead)
*E304062 , 1 HMS , 08/30/2018  - Aria 5 - how to remove table header[T20180725.0012][End]
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
*USE (oAriaApplication.DataDir+'objects') AGAIN ALIAS &TmpObjects ORDER TAG objectid
IF TYPE('lcXMLFileName') = 'C'
  =gfOpenFile(oAriaApplication.DataDir+'OBJECTS', 'OBJECTID', 'SH', TmpObjects)
ELSE
  USE (oAriaApplication.DataDir+'objects') AGAIN ALIAS &TmpObjects ORDER TAG objectid
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

SELECT 0
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
*USE (oAriaApplication.DataDir+'objlink') AGAIN ALIAS &TmpObjLink ORDER TAG Objlnkty
IF TYPE('lcXMLFileName') = 'C'
  =gfOpenFile(oAriaApplication.DataDir+'OBJLINK', 'OBJLNKTY', 'SH', TmpObjLink)
ELSE
  USE (oAriaApplication.DataDir+'objlink') AGAIN ALIAS &TmpObjLink ORDER TAG Objlnkty
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

SET RELATION TO cobject_id INTO &TMPObjects ADDITIVE

SELECT objlink
SET RELATION TO cobject_id INTO Objects ADDITIVE

IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
  SEEK(OBJLINK.cObject_ID,'OBJECTS')
  = lfGetLogo()        && Function to Fill the temp cursor With company Logo.
ENDIF

=gfOpenFile(oAriaApplication.DataDir+'style',oAriaApplication.DataDir+'STYLE','SH')
=gfOpenFile(oAriaApplication.DataDir+'Codes',oAriaApplication.DataDir+'Codes','SH')

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
*IF llOGFltCh
*  lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
*
*  lnPos = ASCAN(loOGScroll.laFltExp,'POSHDR')
*  lcSelFld = " * "
*  IF lnPos <> 0 
*    lnPos = ASUBSCRIPT(loOGScroll.laFltExp,lnPos,1)
*    IF !EMPTY(loOGScroll.laFltExp[lnPos,2])
*  *!*      *********************temp till solve the problem of the double quotes
*  *!*      lnPosvqout = AT("''",loOGScroll.laFltExp[lnPos,2])
*  *!*       IF lnPosvqout > 0
*  *!*        loOGScroll.laFltExp[lnPos,2] = STRTRAN(loOGScroll.laFltExp[lnPos,2],"''","'")
*  *!*      ENDIF
*  *!*      ***********************
*      lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"' AND  " + loOGScroll.laFltExp[lnPos,2]
*    ELSE 
*      lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
*    ENDIF 
*  ELSE 
*    lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
*  ENDIF 
*
*  IF lfopensql(lcSelFld ,'POSHDR','POSHDR',lcSelCond)
*    SELECT POSHDR 
*    SET RELATION TO
*  ENDIF
*   
*  lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
*  *!*  lcSelFld = " POSLN.CBUSDOCU,POSLN.CSTYTYPE,POSLN.PO,POSLN.CVENSTY,Reference"
*  *!*  lcSelFld =lcSelFld+",POSLN.TOTQTY,POSLN.CWARECODE,POSLN.ACCOUNT"
*  *!*  lcSelFld =lcSelFld+",POSLN.STORE,POSLN.STYLE,POSLN.NFCOST1,POSLN.NFCOST2,POSLN.NFCOST3,POSLN.NFCOST4,POSLN.NFCOST5,POSLN.NICOST1,"
*  *!*  lcSelFld =lcSelFld+"POSLN.CINVTYPE,POSLN.TRANCD,POSLN.VENDOR,POSLN.SCALE,"
*  *!*  lcSelFld =lcSelFld+"POSLN.QTY1,POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,POSLN.QTY6,POSLN.QTY7,"
*  *!*  lcSelFld =lcSelFld+"POSLN.QTY8,POSLN.TOTQTY,DYELOT"
*  lcTable ="POSLN,POSHDR"
*  lcSelFld = "  POSLN.*  "
*  lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
*
*  IF !EMPTY(loogscroll.lcRpSqlExp)
*  *!*     **************this is temp code until the problem of the between problem solved 
*  *!*    lnPosvcout = AT("''",loogscroll.lcRpSqlExp)
*  *!*    IF lnPosvcout > 0
*  *!*      loogscroll.lcRpSqlExp = STRTRAN(loogscroll.lcRpSqlExp,"''","'")
*  *!*    ENDIF
*  *!*     **************
*    lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"' AND POSLN.TRANCD ='1'  AND    " + loogscroll.lcrpsqlexp
*    lcTable ="POSLN,POSHDR"
*  ELSE 
*
*    lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
*    IF lnPos <> 0 
*      lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
*      lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
*      llFirst = .T.
*      lcSelOrd =""
*      IF !EMPTY(lcOrders)
*      **********************************
*        SELECT &lcOrders 
*        LOCATE 
*        IF !EOF()
*          lcCurName = lcOrders 
*          IF !EMPTY(lcCurName)
*            SELECT &lcCurName    
*            IF (RECCOUNT() > 0) 
*              lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
*              lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='1'  AND  POSLN.PO = "+lcSQLOrder+'.PO'
*              lcTable = 'POSLN INNER JOIN '+lcSQLOrder +' ON posln.po ='+lcSQLOrder+'.PO'
*            ENDIF 
*          ENDIF 
*      **********************************
*  *!*        SELECT &lcOrders 
*  *!*        SCAN 
*  *!*          lcSelOrd = lcSelOrd + IIF(llFirst," ",",") + "'"+PO+"'"
*  *!*          llFirst = .F.
*  *!*        ENDSCAN 
*  *!*        IF !EMPTY(lcSelOrd)
*  *!*          lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='1'  AND  POSLN.PO IN  ("+lcSelOrd+")   "
*  *!*          lcTable ="POSLN"
*        ELSE
*          lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
*          lcTable ="POSLN,POSHDR"
*        ENDIF  
*      ENDIF   
*    ELSE 
*       lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'   AND  POSLN.CSTYTYPE ='"+lcType+"'    "
*       lcTable ="POSLN,POSHDR"
*    ENDIF 
*  ENDIF 
*
*  IF lfopensql(lcSelFld,lcTable,'POSLN',lcSelCond)
*    SELECT POSLN
*    SET RELATION TO
*    LOCATE
*  ENDIF 
*ENDIF

IF TYPE('lcXMLFileName') = 'C'
  IF llOGFltCh
    lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )

    lnPos = ASCAN(laFltExp,'POSHDR')
    lcSelFld = " * "
    IF lnPos <> 0 
      lnPos = ASUBSCRIPT(laFltExp,lnPos,1)
      IF !EMPTY(laFltExp[lnPos,2])
        lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"' AND  " + laFltExp[lnPos,2]
      ELSE 
        lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
      ENDIF 
    ELSE 
      lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
    ENDIF 
  
    IF lfopensql(lcSelFld ,'POSHDR','POSHDR',lcSelCond)
      SELECT POSHDR 
      SET RELATION TO
    ENDIF
   
    lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )    
    lcTable ="POSLN,POSHDR"
    lcSelFld = "  POSLN.*  "
    lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
  
    IF !EMPTY(lcRpSqlExp)
      lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"' AND POSLN.TRANCD ='1'  AND    " + lcRpSqlExp
      lcTable ="POSLN,POSHDR"
    ELSE 

      lnPos = ASCAN(laogFxflt,'POSHDR.PO')
      IF lnPos <> 0 
        lnPos = ASUBSCRIPT(laogFxflt,lnPos,1)
        lcOrders = IIF(EMPTY(laogFxflt[lnPos,6]),'',laogFxflt[lnPos,6])
        llFirst = .T.
        lcSelOrd =""
        IF !EMPTY(lcOrders)
          SELECT &lcOrders 
          LOCATE 
          IF !EOF()
            lcCurName = lcOrders 
            IF !EMPTY(lcCurName)
              SELECT &lcCurName    
              IF (RECCOUNT() > 0) 
                lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
                lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='1'  AND  POSLN.PO = "+lcSQLOrder+'.PO'
                lcTable = 'POSLN INNER JOIN '+lcSQLOrder +' ON posln.po ='+lcSQLOrder+'.PO'
              ENDIF 
            ENDIF 
          ELSE
            lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
            lcTable ="POSLN,POSHDR"
          ENDIF  
        ENDIF   
      ELSE 
         lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'   AND  POSLN.CSTYTYPE ='"+lcType+"'    "
         lcTable ="POSLN,POSHDR"
      ENDIF 
    ENDIF 
  
    IF lfopensql(lcSelFld,lcTable,'POSLN',lcSelCond)
      SELECT POSLN
      SET RELATION TO
      LOCATE
    ENDIF 
  ENDIF
ELSE
  IF llOGFltCh
    lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
  
    lnPos = ASCAN(loOGScroll.laFltExp,'POSHDR')
    lcSelFld = " * "
    IF lnPos <> 0 
      lnPos = ASUBSCRIPT(loOGScroll.laFltExp,lnPos,1)
      IF !EMPTY(loOGScroll.laFltExp[lnPos,2])
    *!*      *********************temp till solve the problem of the double quotes
    *!*      lnPosvqout = AT("''",loOGScroll.laFltExp[lnPos,2])
    *!*       IF lnPosvqout > 0
    *!*        loOGScroll.laFltExp[lnPos,2] = STRTRAN(loOGScroll.laFltExp[lnPos,2],"''","'")
    *!*      ENDIF
    *!*      ***********************
        lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"' AND  " + loOGScroll.laFltExp[lnPos,2]
      ELSE 
        lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
      ENDIF 
    ELSE 
      lcSelCond ="poshdr.CBUSDOCU ='"+lcRpForm+"'  AND  poshdr.CSTYTYPE ='"+lcType+"'  " 
    ENDIF 
  
    IF lfopensql(lcSelFld ,'POSHDR','POSHDR',lcSelCond)
      SELECT POSHDR 
      SET RELATION TO
    ENDIF
     
    lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
    *!*  lcSelFld = " POSLN.CBUSDOCU,POSLN.CSTYTYPE,POSLN.PO,POSLN.CVENSTY,Reference"
    *!*  lcSelFld =lcSelFld+",POSLN.TOTQTY,POSLN.CWARECODE,POSLN.ACCOUNT"
    *!*  lcSelFld =lcSelFld+",POSLN.STORE,POSLN.STYLE,POSLN.NFCOST1,POSLN.NFCOST2,POSLN.NFCOST3,POSLN.NFCOST4,POSLN.NFCOST5,POSLN.NICOST1,"
    *!*  lcSelFld =lcSelFld+"POSLN.CINVTYPE,POSLN.TRANCD,POSLN.VENDOR,POSLN.SCALE,"
    *!*  lcSelFld =lcSelFld+"POSLN.QTY1,POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,POSLN.QTY6,POSLN.QTY7,"
    *!*  lcSelFld =lcSelFld+"POSLN.QTY8,POSLN.TOTQTY,DYELOT"
    lcTable ="POSLN,POSHDR"
    lcSelFld = "  POSLN.*  "
    lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
  
    IF !EMPTY(loogscroll.lcRpSqlExp)
    *!*     **************this is temp code until the problem of the between problem solved 
    *!*    lnPosvcout = AT("''",loogscroll.lcRpSqlExp)
    *!*    IF lnPosvcout > 0
    *!*      loogscroll.lcRpSqlExp = STRTRAN(loogscroll.lcRpSqlExp,"''","'")
    *!*    ENDIF
    *!*     **************
      lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"' AND POSLN.TRANCD ='1'  AND    " + loogscroll.lcrpsqlexp
      lcTable ="POSLN,POSHDR"
    ELSE 

      lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
      IF lnPos <> 0 
        lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
        lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
        llFirst = .T.
        lcSelOrd =""
        IF !EMPTY(lcOrders)
        **********************************
          SELECT &lcOrders 
          LOCATE 
          IF !EOF()
            lcCurName = lcOrders 
            IF !EMPTY(lcCurName)
              SELECT &lcCurName    
              IF (RECCOUNT() > 0) 
                lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
                lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='1'  AND  POSLN.PO = "+lcSQLOrder+'.PO'
                lcTable = 'POSLN INNER JOIN '+lcSQLOrder +' ON posln.po ='+lcSQLOrder+'.PO'
              ENDIF 
            ENDIF 
        **********************************
    *!*        SELECT &lcOrders 
    *!*        SCAN 
    *!*          lcSelOrd = lcSelOrd + IIF(llFirst," ",",") + "'"+PO+"'"
    *!*          llFirst = .F.
    *!*        ENDSCAN 
    *!*        IF !EMPTY(lcSelOrd)
    *!*          lcSelCond ="POSLN.CBUSDOCU ='"+lcRpForm+"'  AND  POSLN.CSTYTYPE ='"+lcType+"'  AND POSLN.TRANCD ='1'  AND  POSLN.PO IN  ("+lcSelOrd+")   "
    *!*          lcTable ="POSLN"
          ELSE
            lcSelCond ="  POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'  AND  POSLN.CSTYTYPE ='"+lcType+"'   "
            lcTable ="POSLN,POSHDR"
          ENDIF  
        ENDIF   
      ELSE 
         lcSelCond =" POSLN.PO=POSHDR.PO AND POSLN.CBUSDOCU =POSHDR.CBUSDOCU AND  POSLN.CSTYTYPE = POSHDR.CSTYTYPE AND POSLN.CBUSDOCU ='"+lcRpForm+"'  AND POSLN.TRANCD ='1'   AND  POSLN.CSTYTYPE ='"+lcType+"'    "
         lcTable ="POSLN,POSHDR"
      ENDIF 
    ENDIF 
  
    IF lfopensql(lcSelFld,lcTable,'POSLN',lcSelCond)
      SELECT POSLN
      SET RELATION TO
      LOCATE
    ENDIF 
  ENDIF
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End] 

SELECT POSLN
SET RELATION TO POSLN.STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + POSLN.Scale INTO SCALE ADDITIVE
SET RELATION TO 'S'+SUBSTR(Posln.style,1,lnMajSize) INTO Objlink ADDITIVE
SET ORDER TO TAG POSLNW
IF lcRpPrice='P' .AND. lcRpForm = 'N'

  lcTmpRej = loOgScroll.gfTempName()

  DIMENSION laTempacstru[2,4]
  laTempacstru[1,1]='PO'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0
  
  laTempacstru[2,1]='STYLE'
  laTempacstru[2,2]='C'
  laTempacstru[2,3]= 19
  laTempacstru[2,4]= 0
    
  gfCrtTmp(lcTmpRej,@laTempacstru,"PO",lcTmpRej,.T.)

ENDIF

IF llrPrtCs .AND. lcRpForm='P'
  STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2,lcLotNo,lcStyMaj,lcPattrn,lcBOMTit
  lcType =IIF(INLIST(lcRpForm,'P','R'),'P', lcRpForm )
  lcMGroup  = 'CUTTKT'
  lcInGroup = 'CUTTKT+TYP'
  lcBOMTit  = 'Purchase Order#'

  *B131435,1 MMT 03/16/2006 fix error if print BOM [Start]
  *lcSelFld  = " CTKTBOM.ITEM,CTKTBOM.CUTTKT,TYP,CINVTYPE,CIMTYP,cCatgTyp,CTKTBOM.UNTQTY,CTKTBOM.UNTCOST,CTKTBOM.DYELOT"
  lcSelFld  = " CTKTBOM.ITEM,CTKTBOM.CUTTKT,TYP,CTKTBOM.CINVTYPE,CIMTYP,cCatgTyp,CTKTBOM.UNTQTY,CTKTBOM.UNTCOST,CTKTBOM.DYELOT,CTKTBOM.CUOMCODE"
  *B131435,1 MMT 03/16/2006 fix error if print BOM [End]

  lcSelFld = lcSelFld + ",[DESC],CTKTBOM.MFGCODE,CTKTBOM.ISSUE_QTY,CTKTBOM.REQ_QTY ,"
  lcSelFld = lcSelFld + "UOM.CUOM_V AS UOM"
  lcTable  = 'UOM,CTKTBOM,poshdr'
  lcSelCond =" CIMTYP ='I' AND cCatgTyp IN ('F','T','S') AND  UOM.CUOMCODE = CTKTBOM.CUOMCODE AND  CTKTBOM.CUTTKT = POSHDR.PO AND POSHDR.CBUSDOCU ='"+lcRpForm+"'  AND  POSHDR.CSTYTYPE ='"+lcType+"'"
  lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
  IF lnPos <> 0 
    lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
    lcOrders = IIF(EMPTY(loOGScroll.laogFxflt[lnPos,6]),'',loOGScroll.laogFxflt[lnPos,6])
    llFirst = .T.
    lcSelOrd =""
    IF !EMPTY(lcOrders)
      SELECT &lcOrders 
      **********************************
      LOCATE 
      IF !EOF()
        lcCurName = lcOrders 
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
            lcSelCond =" CIMTYP ='I'  AND  CUTTKT ="+ lcSQLOrder +".PO  AND cCatgTyp IN ('F','T','S') AND  UOM.CUOMCODE = CTKTBOM.CUOMCODE AND CTKTBOM.CUTTKT = POSHDR.PO AND POSHDR.CBUSDOCU ='"+lcRpForm+"'  AND  POSHDR.CSTYTYPE ='"+lcType+"'"
            lcTable = 'POSHDR,UOM,CTKTBOM INNER JOIN '+lcSQLOrder +' ON CTKTBOM.CUTTKT ='+lcSQLOrder+'.PO'
          ENDIF 
        ENDIF 
      *!*      **********************************
      *!*      SCAN 
      *!*        lcSelOrd = lcSelOrd + IIF(llFirst," ",",") + "'"+PO+"'"
      *!*        llFirst = .F.
      *!*      ENDSCAN 
      *!*      ELSE 
      *!*      lcSelCond =" CIMTYP ='I'  AND  CUTTKT IN ("+lcSelOrd+") AND cCatgTyp IN ('F','T','S') AND  UOM.CUOMCODE = CTKTBOM.CUOMCODE"
      ENDIF 
    ENDIF   
  ENDIF 
  IF lfopensql(lcSelFld ,lcTable,'CTKTBOM',lcSelCond)
    SELECT CTKTBOM
    SET ORDER TO TAG CTKTBOM
  ENDIF 

  * TNA Add width field for MFPRCSA.FRX [Begin]
*!*	  lcSelFld ="ITEM.VENDOR,ITEM.CITEMFLD1,ITEM.STYLE"
  lcSelFld ="ITEM.VENDOR,ITEM.CITEMFLD1,ITEM.CITEMFLD1 AS WIDTH,ITEM.STYLE"
  * TNA Add width field for MFPRCSA.FRX [End]  
  IF lfOpenSql(lcSelFld,'ITEM','Fabric',"CINVTYPE= '0002'") 
    SELECT FABRIC 
    SET ORDER TO TAG Fabric
  ENDIF 

  *-- Create array to hold the cost element titles.
  DIMENSION laCost[10,2]
  laCost[1,1]  = 'M_CMTYPE1 '
  laCost[2,1]  = 'M_CMTYPE2 '
  laCost[3,1]  = 'M_CMTYPE3 '
  laCost[4,1]  = 'M_CMTYPE4 '
  laCost[5,1]  = 'M_CMTYPE5 '
  laCost[6,1]  = 'M_CMSLBL1 '
  laCost[7,1]  = 'M_CMSLBL2 '
  laCost[8,1]  = 'M_CMSLBL3 '
  laCost[9,1]  = 'M_CMSLBL4 '
  laCost[10,1] = 'M_CMSLBL5 '
  =gfGetMemvar(@laCost,oAriaApplication.ActiveCompanyID)
ENDIF

  SELECT POSHDR
  SET RELATION TO POSHDR.VENDOR INTO Apvendor ADDITIVE
  SET RELATION TO 'P'+POSHDR.PO INTO &TMPObjlink ADDITIVE
  SET RELATION TO lcRpForm+IIF(lcRpForm $ 'PR','P',lcRpForm)+POSHDR.PO INTO POSLN ADDITIVE
  SET SKIP TO POSLN
IF lcRpPrice='P' .AND. lcRpForm = 'N'
  
  SELECT POSLN
  SCAN FOR  STYLE.nSugRetPri = 0
    m.PO    = POSLN.PO
    m.STYLE = POSLN.STYLE
    INSERT INTO (lcTmpRej) FROM MEMVAR
  ENDSCAN
  IF !EOF(lcTmpRej)
    *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
*!*	    lcBrFields = [PO :R :H=LANG_POSTYP_PONO ,STYLE :R :H = LANG_POSTYP_STYLE]
*!*	    lcFile_Ttl = LANG_POSTYP_BrowseTitle
    lcBrFields = [PO :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_PONO,oAriaApplication.GetHeaderText("LANG_POSTYP_PONO",AHEADERFILE))+;
                 [' ,STYLE :R :H =']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_STYLE,oAriaApplication.GetHeaderText("LANG_POSTYP_STYLE",AHEADERFILE))+[']
    lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_BrowseTitle,oAriaApplication.GetHeaderText("LANG_POSTYP_BrowseTitle",AHEADERFILE))
    *:N000382,1 MMT 02/26/2013 Globalization changes[End]
    *-- Message : One or more styles has no retail price, these styles
    *-- will be rejected. Do you want to view the rejected styles?
    *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
    *IF gfModalGen("QRM34141B34001","Dialog") = 1
    IF TYPE('lcXMLFileName') <> 'C' .AND. gfModalGen("QRM34141B34001","Dialog") = 1
    *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
      SELECT (lcTmpRej)
      LOCATE 
      =ARIABROW(.F.,lcFile_Ttl,.F.,.F.,.F.,.F.,"","Fi\<nd;\<Order by;\<Descending;Fi\<lter;;\?\<Close",lcBrFields)
    ENDIF
    USE IN (lcTmpRej)
    RETURN
  ENDIF
  USE IN (lcTmpRej)
ENDIF

lcSqlCommand="SELECT SYCCOMP.CCOM_NAME,SYCCOMP.CCOM_PHON,cCom_Fax,caddress1,caddress2,caddress3,caddress4,caddress5,caddress6,ccont_code "
lcSqlCommand=lcSqlCommand+"  FROM SYCCOMP WHERE CCOMP_ID ='"+OARIAAPPLICATION.ACTIVECOMPANYID+"'  "
LOCAL lnResult
lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"",lcCompInfo,"",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult = 1
  SELECT &lcCompInfo
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon              && Variable to hold the Company Phone
  lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
  lcCompFax = cCom_Fax               && Variable to hold the Company Fax
  * Get the company addresses
  laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
  laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
  laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
  * Pack the addresses array
  DO lfShiftArr WITH laCompAdd
ENDIF 
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

IF !USED(lcNoteLns) AND llPrntBoth

  DIMENSION laTempacstru[1,4]
  laTempacstru[1,1]='cRecord'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 2
  laTempacstru[1,4]= 0
   
  gfCrtTmp(lcNoteLns,@laTempacstru,'cRecord',lcNoteLns,.T.)
  SELECT &lcNoteLns
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

IF llPrntBoth
  SELECT POSLN
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [POSLN,&lcNoteLns]
ENDIF

lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO('POSLN') = lnLstLn,.T.),.T.)]
lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)

SELECT POSLN
SET ORDER TO TAG POSLNW 

*--B127546, HFK 04/24/2005 Fix Bug of error data displayed on exporting to excel
*!*  SELECT POSHDR
*!*  LOCATE 
*!*  SET SKIP TO &lcSkipExpr
IF !(oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
  SELECT POSHDR
  *--mariam
  *C130884,1 MMT 03/28/2006 Custom Program for OFF10[Start]
*!*	  IF RECCOUNT('POSHDR')> 0 
*!*	    WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(RECCOUNT('POSHDR'))) +  ' RECORDS FOR REPORT' TIMEOUT 1
*!*	  ENDIF   
  *C130884,1 MMT 03/28/2006 Custom Program for OFF10[End]
  *--
  LOCATE 
  SET SKIP TO &lcSkipExpr
ELSE
  lcSelFldPosln ="POSLN.PO,POSLN.CVENSTY,Reference"
  lcSelFldPosln =lcSelFldPosln+",POSLN.TOTQTY,POSLN.CWARECODE,POSLN.ACCOUNT"
  lcSelFldPosln =lcSelFldPosln+",POSLN.STORE,POSLN.STYLE,POSLN.NFCOST1,POSLN.NFCOST2,POSLN.NFCOST3,POSLN.NFCOST4,POSLN.NFCOST5,POSLN.NICOST1,"
  lcSelFldPosln =lcSelFldPosln+"POSLN.CINVTYPE,POSLN.TRANCD,POSLN.VENDOR,POSLN.SCALE,"
  lcSelFldPosln =lcSelFldPosln+"POSLN.QTY1,POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,POSLN.QTY6,POSLN.QTY7,"
  lcSelFldPosln =lcSelFldPosln+"POSLN.QTY8,DYELOT"
  SELECT &lcSelFldPosln FROM poshdr,posln WHERE &lcRpExp  AND POSHDR.PO = POSLN.PO INTO CURSOR &lcExcelTemp
  SELECT(lcExcelTemp)

ENDIF 
*--B127546, HFK 04/24/2005 Fix Bug of error data displayed on exporting to excel

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
*=lfOptProg()
*lcPrgName  = lcFormName
IF TYPE('lcXMLFileName') = 'C'
  lcPrgName  = lcFormName
  IF !EMPTY(oAriaEnvironment.Report.lcOptProg)
    lcPrgName = oAriaEnvironment.Report.lcOptProg
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
  lcPrgName  = lcFormName
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
*IF !lfIsApparl(@lcPrgName)
IF TYPE('lcXMLFileName') = 'C'
  lcFormName = oAriaEnvironment.REPORT.GetForm('POSTYP')
  lcPrgName  = lcFormName
  llIsAparel = oAriaEnvironment.REPORT.isapparell(@lcPrgName)
ELSE
  llIsAparel = lfIsApparl(@lcPrgName)
Endif

IF !llIsAparel
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
  IF llPoDisp

    *C130884,1 MMT 03/28/2006 Custom Program for OFF10[Start]
    IF RECCOUNT('POSHDR')> 0 
      *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
      *WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(RECCOUNT('POSHDR'))) +  ' RECORDS FOR REPORT' TIMEOUT 1
      IF TYPE('lcXMLFileName') <> 'C'
        *:N000382,1 MMT 02/26/2013 Globalization changes
        *WAIT WINDOW 'SELECTED ' + ALLTRIM(STR(RECCOUNT('POSHDR'))) +  ' RECORDS FOR REPORT' TIMEOUT 1
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTED,oAriaApplication.GetHeaderText("LANG_SELECTED",AHEADERFILE))+ ALLTRIM(STR(RECCOUNT('POSHDR'))) +;
                    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RECFORREP ,oAriaApplication.GetHeaderText("LANG_RECFORREP",AHEADERFILE)) TIMEOUT 1
        *:N000382,1 MMT 02/26/2013 Globalization changes
      ENDIF
      *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
    ENDIF   
    LOCATE 
    *C130884,1 MMT 03/28/2006 Custom Program for OFF10[End]

  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
  **--mmt,problem of export to excel
  * IF !(oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
  *    DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
  *  ELSE
  *    DO gfDispRe WITH EVAL('lcFormName')
  *  ENDIF  
  **   DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
  **--mmt,problem of export to excel
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.REPORT.OGLastForm = lcFormName&& lcRpForm
    loProgress.Percent = 0.9
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    PRIVATE loProxy    
    *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
    *loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    *:E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]  
        
    IF !(oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm, lcRpExp)
    ELSE
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
    ENDIF
    loProgress.Percent = 1.0
    loProgress.DESCRIPTION = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)

  ELSE
    IF !(oAriaApplication.gcDevice = "FILE" .AND. loOGScroll.cTextRepType = "EXCEL")
      DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
    ELSE
      DO gfDispRe WITH EVAL('lcFormName')
    ENDIF  
  ENDIF
  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
  ENDIF

ELSE

    DO EVAL('lcPrgName')
    IF !llNoRec			&& no record found
      DO ENDREPORT
    ENDIF


ENDIF

*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][Start]
IF TYPE('lcXMLFileName') <> 'C'
  IF loOgScroll.ll2Printer
    =gfOpenTable('POSHDR','POSHDR', 'SH', 'POSHDR_T')
    SELECT (lcPOPrtUp)
    SCAN
      SELECT POSHDR_T      
      IF gfSeek(EVAL(lcPOPrtUp+'.CBUSDOCU')+EVAL(lcPOPrtUp+'.CSTYTYPE')+EVAL(lcPOPrtUp+'.PO'))
        =gfReplace(" PrtFlag WITH 'P'")
      ENDIF
    ENDSCAN
    loOGScroll.llPrinted = .F.
    
    SELECT POSHDR_T
    =gfTableUpdate()
  ENDIF  
ELSE
  SELECT (lcPOPrtUp)
  SCAN
    SELECT POSHDR
    IF gfSeek(EVAL(lcPOPrtUp+'.CBUSDOCU')+EVAL(lcPOPrtUp+'.CSTYTYPE')+EVAL(lcPOPrtUp+'.PO'))
      =gfReplace(" PrtFlag WITH 'P'")
    ENDIF
  ENDSCAN
  
  SELECT POSHDR
  =gfTableUpdate()
ENDIF
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][End]


*-- Print the cost sheet if required.
IF llrPrtCs  .AND. lcRpForm='P' .AND. !EOF('CTKTBOM')

  * TNA Add Field TUOM.CUOM_V for MFPRCSA.FRX [Begin]
  IF TYPE('loUom') <> 'O'
    loUom = CREATEOBJECT('RemoteTable','UOM','UOMCODE','UOM',SET("Datasession"))
  ENDIF 
  LOUOM.SQLRUN("SELECT CUOMCODE,CUOM_V FROM UOM",'TUOM') 
  SELECT TUOM
  =CURSORSETPROP("Buffering",3,'TUOM')
  INDEX ON CUOMCODE TAG CODCX
  * TNA Add Field TUOM.CUOM_V for MFPRCSA.FRX [End]
  
  SELECT POSHDR
  SET RELATION TO 
  SELECT CTKTBOM
  SET RELATION TO ITEM  INTO Fabric ADDITIVE
  SET RELATION TO 'P'+CutTkt INTO POSHDR ADDITIVE
  * TNA
  SET RELATION TO CUOMCODE INTO TUOM ADDITIVE
    
  lcOldTmp = lcOGTmpForm
  lcOGTmpForm = lcFormName
  lcOldPlatF  = lcOgPlatForm
  lcCSFrmN = 'MFPRCS'+ STRTRAN(lcFormName,'POSTYP')
  IF !FILE(oAriaApplication.ReportHome+lcCSFrmN+'.FRX')
    lcCSFrmN = 'MFPRCSA'
  ENDIF
  IF lcRepMode = 'Text'
    =gfCrtFrm(lcCSFrmN ,"",llOGRefForm)
    =lfRepPltFr(lcOGTmpForm)

    lcOgPlatForm = 'DOS'
    lcRepMode    = 'Text'
    IF lcRpFrmMod = 'WINDOWS'
            SHOW GET LCREPMODE ENABLE
    ELSE
            SHOW GET LCREPMODE
    ENDIF

  ELSE
    =gfCrtFrm(lcCSFrmN ,"",llOGRefForm)
    =lfRepPltFr(lcOGTmpForm)
    lcOgPlatForm = 'WINDOWS'
    lcRepMode    = 'Graphics'
    IF lcRpFrmMod = 'WINDOWS'
            SHOW GET LCREPMODE ENABLE
    ELSE
            SHOW GET LCREPMODE
    ENDIF
  ENDIF

  =lfChPprSz()


  SELECT CTKTBOM
  LOCATE

 * loOGScroll.cCRPaperSize = 'A4'
  loogScroll.lAdditive = .T.
  loogScroll.cCROrientation = 'P'
  DO gfDispRe WITH (lcCSFrmN)
  lcOGTmpForm  = lcOldTmp
  lcOgPlatForm = lcOldPlatF
ENDIF

IF !EMPTY(laOpTmpFls)
  FOR ArrElmnt = 1 TO ALEN(laOpTmpFls,1)
    IF USED(laOpTmpFls[ArrElmnt,1])
      USE IN laOpTmpFls[ArrElmnt,1]
      *--If it is a cursor, no directory exists.
      IF !EMPTY(laOpTmpFls[ArrElmnt,2])
        ERASE laOpTmpFls[ArrElmnt,2]+laOpTmpFls[ArrElmnt,1]+'.DBF'
        ERASE laOpTmpFls[ArrElmnt,2]+laOpTmpFls[ArrElmnt,1]+'.CDX'
      ENDIF
    ENDIF
  ENDFOR
ENDIF

*!*  IF USED(TmpObjects)
*!*    USE IN (TmpObjects)
*!*  ENDIF

*!*  IF USED(TmpObjLink)
*!*    USE IN (TmpObjLink)
*!*  ENDIF

*!*  IF USED('POSHDR')
*!*    USE IN ('POSHDR')
*!*  ENDIF

*!*  IF USED('POSLN')
*!*    USE IN ('POSLN')
*!*  ENDIF

*!*  IF USED('CTKTBOM')
*!*    USE IN ('CTKTBOM')
*!*  ENDIF

*!*  IF USED('FABRIC')
*!*    USE IN ('FABRIC')
*!*  ENDIF
*lcSqlConnection = NULL


*!*************************************************************
*! Name      : lfGetCodes
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfRltFld(), gfCodDes(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
lcAlias = ALIAS()
SELECT Codes
SET ORDER TO TAG Codes
IF SEEK('N'+POSHDR.cDivision+'Y'+'CDIVISION')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+POSHDR.cDivision+'Y'+'CDIVISION'
    IF crltd_nam = 'DIVLNAME  '
      lcDivLName = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)

llEndGroup  = .F.
lcDivDesc   = gfCodDes(POSHDR.cDIVISION, 'CDIVISION')
lcShipVia   = gfCodDes(POSHDR.ShipVia , 'SHIPVIA')
lcTerms     = gfCodDes(POSHDR.CTERMCODE, 'CTERMCODE')

IF POSHDR.CBUSDOCU+POSHDR.cStyType # 'NN'
  lcVenName   = APVENDOR.CVenComp
  lcFax       = APVENDOR.CFAXNO
  * Get the vendor addresses
  laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
    =SEEK(LEFT(PosHdr.Vendor,6),'WAREHOUS')
    lcVenName   = WAREHOUS.cDesc
    * Get warehouse fax number
    lcFax       = WAREHOUS.cFAX
    laVenAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laVenAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laVenAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laVenAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laVenAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
* pack the addresses array
DO lfShiftArr WITH laVenAdr
SELECT POSHDR
* Get ship to Name and addresses

IF !POSHDR.lmultiware
  * If single warehouse get it's addresses from the header file.
  lcShpName   = POSHDR.ShpName
  laShpAdr[1] = COutAddr1
  laShpAdr[2] = COutAddr2
  laShpAdr[3] = COutAddr3
  laShpAdr[4] = COutAddr4
  laShpAdr[5] = COutAddr5
ELSE
  * The ship to is on line level
  IF EMPTY(POSLN.account)
    * If account field is Empty , the ship to is to warehouse.
    =SEEK(posln.cwarecode,'WAREHOUS')
    lcShpName   = WAREHOUS.cDesc
    laShpAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laShpAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laShpAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laShpAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laShpAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  ELSE
    * The ship to is to account, so get its addresses from the customer file.
    =SEEK(IIF(EMPTY(POSLN.Store),'M'+POSLN.Account,;
                       'S'+POSLN.account + POSLN.Store),'CUSTOMER')
    lcShpName   = CUSTOMER.stname
    laShpAdr[1] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 1)
    laShpAdr[2] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 2)
    laShpAdr[3] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 3)
    laShpAdr[4] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 4)
    laShpAdr[5] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 5)
  ENDIF
ENDIF
* Pack the addresses array
DO lfShiftArr WITH laShpAdr
* Get the discount percentage
lnDisc = 0
IF _PAGENO = 1 
  lnTotAmt = 0
ENDIF  
SELECT &lcAlias

RETURN ''

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To state that if we would print the word "Continued"
*!             and to initialize some variables.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
* Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.
* Initialize this variable here to print the style notes
llPrtSn    = .T.
* Initialize this variable here to print the style header
llTitle    = .T.

*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][Start]
IF !SEEK((POSHDR.CBUSDOCU+POSHDR.CSTYTYPE+POSHDR.PO), lcPOPrtUp)
  INSERT INTO (lcPOPrtUp) (CBUSDOCU, CSTYTYPE, PO) VALUES (POSHDR.CBUSDOCU, POSHDR.CSTYTYPE, POSHDR.PO)
ENDIF
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][End]
RETURN ''

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())

*!*************************************************************
*! Name      : lfvPO
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Valid function of the PO field.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfBrows().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
FUNCTION lfvPO

PRIVATE lcVar , lcObj , laTemp, lnAlias
lnAlias=SELECT(0)
lcVar = OGSYS18()             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(OGSYS18())   && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
SELECT POSHDR
SET ORDER TO TAG POSHDR
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcRpForm+IIF(lcRpForm $'PR','P',lcRpForm)+lcObj , 'POSHDR'))

  SELECT  APVENDOR
  SET ORDER TO TAG VenCode
  SELECT POSHDR
  SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

  DIMENSION laTemp[1]
  laTemp = ''                           && Array to hold the Selected value
  * Change the header of the field PO according the user choice of printing
  * (PO,Contract,Ret PO, All)
  *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
*!*	  lcHead = IIF(lcRpForm='P',LANG_POSTYP_PURCHASEORDER,;
*!*	                IIF(lcRpForm='C',LANG_POSTYP_CONTRACT,IIF(lcRpForm='R',LANG_POSTYP_RETURNPO,LANG_POSTYP_INTERLOC)))

*!*	  
*!*	   lcBrFields ="PO                :R :H= lcHead,"+;
*!*	               "STATUS            :R :H= LANG_POSTYP_STATUS ,"+;
*!*	               "Vendor            :R :H= LANG_POSTYP_VENDOR ,"+;
*!*	               "APVENDOR.cVenComp :R :H= LANG_POSTYP_NAME,"+;
*!*	               "Entered           :R :H= LANG_POSTYP_ENTERED ,"+;
*!*	               "Complete          :R :H= LANG_POSTYP_COMPLETE ,"+;
*!*	               "Open              :R :H= LANG_POSTYP_OPEN :P='999999' ,"+;
*!*	               "POTOTAL           :R :H= LANG_POSTYP_TOTAL:P='9999999999.99' "
*!*	  lcFile_Ttl = IIF(lcRpForm='P',;
*!*	                   LANG_POSTYP_PURCHORDER,IIF(lcRpForm='C',LANG_POSTYP_CONTORDER,;
*!*	                   IIF(lcRpForm='R',LANG_POSTYP_RETORDER,LANG_POSTYP_INTERORDER)))
  lcHead = IIF(lcRpForm='P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_PURCHASEORDER,oAriaApplication.GetHeaderText("LANG_POSTYP_PURCHASEORDER",AHEADERFILE)),;
                IIF(lcRpForm='C',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_CONTRACT,oAriaApplication.GetHeaderText("LANG_POSTYP_CONTRACT",AHEADERFILE)),;
                IIF(lcRpForm='R',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_RETURNPO,oAriaApplication.GetHeaderText("LANG_POSTYP_RETURNPO",AHEADERFILE)),;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_INTERLOC,oAriaApplication.GetHeaderText("LANG_POSTYP_INTERLOC",AHEADERFILE)))))
   lcBrFields ="PO                :R :H= lcHead,"+;
               "STATUS            :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_STATUS,oAriaApplication.GetHeaderText("LANG_POSTYP_STATUS",AHEADERFILE))+"' ,"+;
               "Vendor            :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_VENDOR,oAriaApplication.GetHeaderText("LANG_POSTYP_VENDOR",AHEADERFILE))+"' ,"+;
               "APVENDOR.cVenComp :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_NAME,oAriaApplication.GetHeaderText("LANG_POSTYP_NAME",AHEADERFILE))+"',"+;
               "Entered           :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_ENTERED,oAriaApplication.GetHeaderText("LANG_POSTYP_ENTERED",AHEADERFILE))+"' ,"+;
               "Complete          :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_COMPLETE,oAriaApplication.GetHeaderText("LANG_POSTYP_COMPLETE",AHEADERFILE))+"' ,"+;
               "Open              :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_OPEN,oAriaApplication.GetHeaderText("LANG_POSTYP_OPEN",AHEADERFILE)) +"':P='999999' ,"+;
               "POTOTAL           :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TOTAL,oAriaApplication.GetHeaderText("LANG_POSTYP_TOTAL",AHEADERFILE))+"':P='9999999999.99' "
  lcFile_Ttl = IIF(lcRpForm='P',;
                   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_PURCHORDER,oAriaApplication.GetHeaderText("LANG_POSTYP_PURCHORDER",AHEADERFILE)),;
                   IIF(lcRpForm='C',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_CONTORDER,oAriaApplication.GetHeaderText("LANG_POSTYP_CONTORDER",AHEADERFILE)),;
                   IIF(lcRpForm='R',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_RETORDER,oAriaApplication.GetHeaderText("LANG_POSTYP_RETORDER",AHEADERFILE)),;
                   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_INTERORDER,oAriaApplication.GetHeaderText("LANG_POSTYP_INTERORDER",AHEADERFILE)))))
  *:N000382,1 MMT 02/26/2013 Globalization changes[End]
  =gfBrows("FOR  lcRpForm + IIF(lcRpForm $'PR','P',lcRpForm)",'PO','laTemp')
  SET RELATION TO
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvVend
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend

PRIVATE lcVar, lcObj

lcVar = OGSYS18()             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(OGSYS18()) )  )   && Varible to hold the current field value
IF lcObj <> lcOldValue 
SELECT APVENDOR
SET ORDER TO TAG VenCode
*IF Statment to check if we are going to Browse

IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = lcOldValue
  ENDIF
ENDIF
ENDIF 
lcOldValue = &lcVar

*!*  PRIVATE lcVar, lcObj

*!*  lcVar = OGSYS18()             && Varible to hold  the name of the memory variable used to create the current GET control
*!*  lcObj = ALLTRIM(  ( EVALUATE(OGSYS18()) )  )   && Varible to hold the current field value
*!*  SELECT APVENDOR
*!*  SET ORDER TO TAG VenCode
*!*  *IF Statment to check if we are going to Browse

*!*  IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
*!*    =gfApVnBrow(@lcObj)
*!*    IF !EMPTY(lcObj)
*!*      &lcVar = lcObj      && Update the field
*!*    ELSE
*!*      &lcVar = laOldVal
*!*    ENDIF
*!*  ENDIF

*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Function to Save the company logo in temp. file
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
*-- select &TmpObjects
lcObj_Id = OBJLINK.cObject_ID
*-- Make cursor contain one field and one record holding the company logo 
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[Start]
*SELECT gobject FROM Objects WHERE Objects.cobject_id = lcObj_Id INTO CURSOR (lcLogoPic)
SELECT mimgpath FROM Objects WHERE Objects.cobject_id = lcObj_Id INTO CURSOR (lcLogoPic)
*:B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[End]
*-- end of lfGetLogo.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 06/30/2004
*! Purpose     : Function to fill the apropriate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : POSTYPA.FRX [Variable lcGetN in the report]
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
PRIVATE lnAlias
lcTitle = ''
lcNotes = ''
lnAlias = SELECT(0)

*B610597,1 TMI 11/21/2013 11:12 [Start] define lcStyType local 
LOCAL lcStyType, lcPONOTEPAD, lcCONTNOTEPAD, lcRETNOTEPAD, lcINTNOTEPAD, lcDYENOTEPAD
lcStyType = IIF(POSHDR.cStyType$'PC','P','N')
*- get the notepad title translations
lcPONOTEPAD = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PONOTEPAD,oAriaApplication.GetHeaderText("LANG_PONOTEPAD",AHEADERFILE))
lcCONTNOTEPAD = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONTNOTEPAD,oAriaApplication.GetHeaderText("LANG_CONTNOTEPAD",AHEADERFILE))
lcRETNOTEPAD = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RETNOTEPAD,oAriaApplication.GetHeaderText("LANG_RETNOTEPAD",AHEADERFILE))
lcINTNOTEPAD = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INTNOTEPAD,oAriaApplication.GetHeaderText("LANG_INTNOTEPAD",AHEADERFILE))
lcDYENOTEPAD = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DYENOTEPAD,oAriaApplication.GetHeaderText("LANG_DYENOTEPAD",AHEADERFILE))
*B610597,1 TMI 11/21/2013 11:12 [End  ] 

*-- If we are to print both notes.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Style Notepad.
  *-- ....... cRecord = 'N2' ............. PO or Contract , or return Notepad.

  DO CASE
    CASE &lcNoteLns..cRecord = 'N1' AND SEEK('F'+STYLE.cStyMajor,;
                    'Notepad') .AND. !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes = IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')

    *B610597,1 TMI 11/21/2013 11:10 [Start] allow to show the interlocation notes, type 'N'
    *CASE &lcNoteLns..cRecord = 'N2' AND SEEK('P' + POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK(lcStyType + POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      *B610597,1 TMI 11/21/2013 11:10 [End  ]                

      
      *B610597,1 TMI 11/21/2013 11:29 [Start] add the globalization translated labels
      *lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='PP','Purchase Order Notepad',;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='CC','Contract Notepad',;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='RP','Return Purchase Order Notepad' ,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='NN','Inter-Location P/O Notepad',;
                   'Dye Order Notepad')))),'')
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='PP',lcPONOTEPAD,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='CC',lcCONTNOTEPAD,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='RP',lcRETNOTEPAD ,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='NN',lcINTNOTEPAD,;
                   lcDYENOTEPAD)))),'')
      *B610597,1 TMI 11/21/2013 11:29 [End  ] 
      
      lcNotes = IIF(RECNO('POSLN') = lnLstLn, ;
                        ALLTRIM(NotePad.mNotes),'')

      llTitle = .F.
  ENDCASE

ELSE && Else You print either Style  or PO/Contract/Ret PO Notepad.
  DO CASE
    CASE llRpPrtSn AND SEEK('F'+STYLE.cStyMajor,'Notepad') .AND. ;
                       !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes  =  IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')

    *B610597,1 TMI 11/21/2013 11:15 [Start] allow to show the interlocation notes, type 'N' 
    *CASE llRpPrtPn .AND. SEEK('P'+ POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    CASE llRpPrtPn .AND. SEEK(lcStyType + POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      *B610597,1 TMI 11/21/2013 11:15 [End  ] 

      *B610597,1 TMI 11/21/2013 11:29 [Start] add the globalization translated labels
      *lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='PP','Purchase Order Notepad',;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='CC','Contract Notepad',;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='RP','Return Purchase Order Notepad',;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='NN','Inter-Location P/O Notepad',;
                   'Dye Order Notepad')))),'')
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='PP',lcPONOTEPAD,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='CC',lcCONTNOTEPAD,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='RP',lcRETNOTEPAD,;
                   IIF(POSHDR.CBUSDOCU+POSHDR.cStyType='NN',lcINTNOTEPAD,;
                   lcDYENOTEPAD)))),'')
      *B610597,1 TMI 11/21/2013 11:29 [End  ] 
      
      lcNotes  = IIF(RECNO('POSLN') = lnLstLn, ALLTRIM(NotePad.mNotes),'')
      llTitle = .F.
  ENDCASE
ENDIF
llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
* State that we have already printed the style notepad (required to be printed once)
llPrtSn    = .F.
SELECT (lnAlias)

RETURN ''

*!*************************************************************
*! Name        : lfShiftArr
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 06/30/2004
*! Purpose     : Function to Pack the passed array
*!*************************************************************
*! Calls       :
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfShiftArr()
*!*************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR

*!*************************************************************
*! Name        : lfGetLL
*! Developer   : Mariam Mazhar (MMT)
*! Date        : 06/30/2004
*! Purpose     : Function to get record number of last line in the PO
*!*************************************************************
*! Calls       :
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetLL()
*!*************************************************************
FUNCTION lfGetLL
PRIVATE lnAlias, lnRecNo
lnAlias = SELECT(0)
SELECT POSLN
lnRecNo = RECNO()
lcOldExp = CBUSDOCU+CSTYTYPE+PO
SET ORDER TO TAG POSLNW DESCENDING
LOCATE FOR CBUSDOCU+CSTYTYPE+PO+CWARECODE+ACCOUNT+STORE+STYLE+TRANCD = lcOldExp AND Trancd='1'
lnLstLn = RECNO()
SET ORDER TO TAG POSLNW ASCENDING
SELECT (lnAlias)

* Refresh the relation between POSHDR and POSLN
IF !EOF('POSHDR')
  GOTO RECNO('POSHDR') IN POSHDR
ENDIF

IF BETWEEN(lnRecNo,1,RECCOUNT('POSLN'))
  GOTO lnRecNo IN POSLN
ENDIF

IF llPrntBoth
  GOTO RECNO(lcNoteLns) IN &lcNoteLns
ENDIF 
RETURN ''

*!*************************************************************
*! Name      : lfvOMsg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOMsg()
*!*************************************************************
FUNCTION lfvOMsg

PRIVATE laOptMsg
DIMENSION laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To set relation on or off when running the in range function
*!             in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
    llClear = .F.
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE

*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To check if the system uses dyelot or not
*!*************************************************************
*! Called from : Option grid (Variable llDyelot)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************

FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To Disable the vendor setting in the option grid
*!             if the user chose to print Inter Location Purchase
*!             Order, and enable it otherwise
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfvRpForm()
*!*************************************************************
FUNCTION lfvRpForm
PRIVATE lnVendorPo, lnSrcLoc
lnVendorPo = 1

lnPos = ASCAN(loOGScroll.laogFxflt,'POSHDR.PO')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnPos,1)
 lcOrders = loOGScroll.laogFxflt[lnPos,6]
 IF !EMPTY(lcOrders)
   SELECT(lcOrders)
   ZAP 
 ENDIF 
ENDIF  
IF lcRpForm # 'N'
  lnPosVen = ASCAN(loOGScroll.laogFxflt,'POSLN.VENDOR')
  IF lnPosVen <> 0 
    lnPosVen = ASUBSCRIPT(loOGScroll.laogFxflt,lnPosVen,1)
    lcVendors = loOGScroll.laogFxflt[lnPosVen,6]
    IF !EMPTY(lcVendors)
      SELECT(lcVendors)
      ZAP 
    ENDIF 
  ENDIF  
ELSE 
 *****************************
  lnPosHdrVen = ASCAN(loOGScroll.laogFxflt,'POSHDR.VENDOR')
  IF lnPosHdrVen <> 0 
    lnPosHdrVen = ASUBSCRIPT(loOGScroll.laogFxflt,lnPosHdrVen,1)
    loOGScroll.laogFxflt[lnPosHdrVen,6] = ''
  ENDIF  
  *****************************  
ENDIF 
IF ASCAN(loOGScroll.laOGFxFlt,'POSHDR.VENDOR') # 0
    lnVendorPo = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'POSHDR.VENDOR'),1)
  IF lcRpForm # 'N'
    *-- Enable the vendor if not Inter Location PO
        LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = .T.

  ELSE
    *-- Disable the vendor if Inter Location PO
        LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = .F.

  ENDIF
  *-- Refresh the Option Grid
    = LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(LNVENDORPO)) + ',6]')

ENDIF

IF ASCAN(loOGScroll.laOGFxFlt,'POSLN.VENDOR') # 0
    lnSrcLoc   = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'POSLN.VENDOR'),1)
  IF lcRpForm # 'N'
        LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNSRCLOC] = .F.
  ELSE
        LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNSRCLOC] = .T.

  ENDIF
    = LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(LNSRCLOC)) + ',6]')

ENDIF


IF ASCAN(LAOGOBJTYPE,'LLRPPDIC') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPPDIC'),1)
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[Start]
    *LAOGOBJCNT[LNPOS] = (LCRPFORM = 'N') .AND. (LCRPPRICE = 'P')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [Start]
    *LAOGOBJCNT[LNPOS] = (LCRPPRICE = 'P')    
    LAOGOBJCNT[LNPOS] = (LCRPPRICE <> 'N')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [ENd]
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[End]
    = LFOGSHOWGET('LLRPPDIC')
ENDIF

llClear = .T.


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF ALEN(LAOGOBJCNT,1) > 1 
  IF ASCAN(loOGScroll.laOGFxFlt,'POSLN.VENDOR') # 0
  
      lnSrcLoc   = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'POSLN.VENDOR'),1)
      LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNSRCLOC] = ;
                                             IIF(lcRpForm # 'N',.F.,.T.)
      = LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(LNSRCLOC)) + ',6]')                                           
  ENDIF
  IF ASCAN(loOGScroll.laOGFxFlt,'POSHDR.VENDOR') # 0
  
    *-- Get the position of the vendor in the varaible filter
      lnVendorPo = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'POSHDR.VENDOR'),1)
      LAOGOBJCNT[ALEN(LAOGOBJCNT,1) - ALEN(LAOGFXFLT,1) + LNVENDORPO] = ;
                                              IIF(lcRpForm # 'N',.T.,.F.)
                                            ***********
      = LFOGSHOWGET('LAOGFXFLT[' + ALLTRIM(STR(lnVendorPo)) + ',6]')                                           
      ************************************
  ENDIF  

  IF ASCAN(LAOGOBJTYPE,'LLRPPDIC') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPPDIC'),1)
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[Start]
    *LAOGOBJCNT[LNPOS] = (LCRPFORM = 'N') .AND. (LCRPPRICE = 'P')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [Start]
    *LAOGOBJCNT[LNPOS] = (LCRPPRICE = 'P')    
    LAOGOBJCNT[LNPOS] = (LCRPPRICE <> 'N')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [END]
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[End]
    = LFOGSHOWGET('LLRPPDIC')
  ENDIF

  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
  IF ASCAN(LAOGOBJTYPE,'LCRPPRICE') # 0  
      LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LCRPPRICE'),1)
      LAOGOBJCNT[LNPOS] = LLCOSTPRV
    = LFOGSHOWGET('LCRPPRICE')
  ENDIF


  IF ASCAN(LAOGOBJTYPE,'LLRPRTCS') # 0
      LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPRTCS'),1)
      llrPrtCs = IIF(llCostPrv,llrPrtCs,llCostPrv)
      LAOGOBJCNT[LNPOS] = LLCOSTPRV
      = LFOGSHOWGET('LLRPRTCS')
  ENDIF
  IF ASCAN(LAOGOBJTYPE,'LLRPCOST') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPCOST'),1)
    LAOGOBJCNT[LNPOS] = LLRPRTCS
    =LFOGSHOWGET('LLRPCOST')
  ENDIF
ENDIF

lcRpFrmMod = lcogplatform

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Enable "Print price after discount" setting in case
*!             of inter location PO, else disable.
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfvPrice()
*!*************************************************************
FUNCTION lfvPrice
IF ASCAN(LAOGOBJTYPE,'LLRPPDIC') # 0
    LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPPDIC'),1)
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[Start]
    *LAOGOBJCNT[LNPOS] = (LCRPFORM = 'N') .AND. (LCRPPRICE = 'P')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [Start]
    *LAOGOBJCNT[LNPOS] = (LCRPPRICE = 'P')    
    LAOGOBJCNT[LNPOS] = (LCRPPRICE <> 'N')
    *:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [END]
    *:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[End]
    = LFOGSHOWGET('LLRPPDIC')
ENDIF

*!*************************************************************
*! Name      : lfGetTitle
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To get the cost element title.
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTitle()
*!*************************************************************
FUNCTION lfGetTitle
PRIVATE lcSetEx
lcCostItm = ''
lcSetEx = SET('EXACT')
SET EXACT ON
lnAlias = SELECT(0)
SELECT CTKTBOM
IF ASCAN(laCost,cCatgTyp) # 0
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,cCatgTyp),1)+5,2]
ENDIF
lcHead1   = ''
lcHead2   = ''

IF lcRepMode = 'Text'
  IF cCatgTyp = 'T'
    *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
*!*	    lcHead1   = LANG_POSTYP_TEXTHEAD1
*!*	    lcHead2   = LANG_POSTYP_TEXTHEAD2
    lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD1,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD1",AHEADERFILE))
    lcHead2   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD2,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD2",AHEADERFILE))
    *:N000382,1 MMT 02/26/2013 Globalization changes[End]
  ELSE
    IF cCatgTyp = 'F'
      *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
      *lcHead1   = LANG_POSTYP_TEXTHEAD      
      lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD",AHEADERFILE))
      *:N000382,1 MMT 02/26/2013 Globalization changes[End]
    ENDIF
  ENDIF
ELSE
  IF cCatgTyp = 'T'
    *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
*!*	    lcHead1   = LANG_POSTYP_TEXTHEAD1
*!*	    lcHead2   = LANG_POSTYP_TEXTHEAD2
    lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD1,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD1",AHEADERFILE))
    lcHead2   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD2,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD2",AHEADERFILE))
    *:N000382,1 MMT 02/26/2013 Globalization changes[End]
  ELSE
    IF cCatgTyp = 'F'
      *:N000382,1 MMT 02/26/2013 Globalization changes[Start]
      *lcHead1   = LANG_POSTYP_TEXTHEAD      
      lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTYP_TEXTHEAD,oAriaApplication.GetHeaderText("LANG_POSTYP_TEXTHEAD",AHEADERFILE))
      *:N000382,1 MMT 02/26/2013 Globalization changes[End]
    ENDIF
  ENDIF
ENDIF

SET EXACT &lcSetEx
SELECT (lnAlias)
RETURN ''


*!*************************************************************
*! Name      : lfGetOp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : to be done on the starting of page, used in lot cost
*!             sheet form
*!*************************************************************
*! Called from : MFPRCSA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetOp()
*!*************************************************************
FUNCTION lfGetOp
RETURN ''

*!*************************************************************
*! Name      : lfStGroup
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To Initialize the variable llEndGroup
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStGroup()
*!*************************************************************
FUNCTION lfStGroup
llEndGroup = .F.
RETURN ''


*!*************************************************************
*! Name      : lfvPrtCs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrtCs()
*!*************************************************************
FUNCTION lfvPrtCs
*-- Get the position of the print cost setting in the array to enable or
*-- disable it.
LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPCOST'),1)
LAOGOBJCNT[LNPOS] = LLRPRTCS
= LFOGSHOWGET('LLRPCOST')

*!*************************************************************
*! Name      : lfGetDisc
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To calculate the discount for each style.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Example     : = lfGetDisc()
*!*************************************************************
FUNCTION lfGetDisc

lnDisc = 0

IF !EMPTY(STYLE.cDiscCode)

  lcAlias = ALIAS()

  SELECT Codes
  SET ORDER TO TAG Codes
  IF SEEK('N'+STYLE.cDiscCode+'Y'+'CDISCCODE')
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+STYLE.cDiscCode+'Y'+'CDISCCODE'
      DO CASE 
        CASE cRltd_Nam = 'DISCPCNT'
          lnDisc = VAL(crltd_vlu)
        CASE crltd_nam = 'START'
          ldStart = CTOD(crltd_vlu)
        CASE crltd_nam = 'DENDATE'
          ldEnd  = CTOD(crltd_vlu)
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT (lcAlias)

  IF (EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. POSHDR.ENTERED < ldStart) .OR. ;
     (!EMPTY(ldEnd) .AND. EMPTY(ldStart) .AND. POSHDR.ENTERED > ldEnd) .OR. ;
     (!EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. !BETWEEN(POSHDR.ENTERED,ldStart,ldEnd))
    lnDisc = 0
  ENDIF
ENDIF
*:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[Start]
*:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [Start]
*lnTotAmt = lnTotAmt + ROUND(IIF(llPrntBoth AND EVAL(lcNoteLns+'.cRecord ')= 'N2',0,IIF(lcRpPrice='C',IIF(llrpfc,Posln.nfcost1, Posln.NICost1) ,IIF(lcRpPrice='P', IIF(llrpPdic, (1-lnDisc/100),1) *IIF(POSLN.NSELPRICE<>0,POSLN.NSELPRICE,STYLE.PRICEA)  ,0 ))* Posln.totqty),2)
lnTotAmt = lnTotAmt + ROUND(IIF(llPrntBoth AND EVAL(lcNoteLns+'.cRecord ')= 'N2',0,lfGetLineCost()* Posln.totqty),2)
*:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [END]
*:B609610,1 SAB 06/09/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is "Selling Price"[End]
RETURN ''

*!*************************************************************
*! Name      : lfGetDiscv
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : To calculate the discount for each style.
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Example     : = lfGetDiscv()
*!*************************************************************
FUNCTION lfGetDiscv

lnDiscv = 0
IF !EMPTY(STYLE.cDiscCode)
  lcAlias = ALIAS()

  SELECT Codes
  SET ORDER TO TAG Codes
  IF SEEK('N'+STYLE.cDiscCode+'Y'+'CDISCCODE')
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+STYLE.cDiscCode+'Y'+'CDISCCODE'
      DO CASE 
        CASE crltd_typ = 'DISCPCNT'
          lnDisc = VAL(crltd_vlu)
        CASE crltd_typ = 'START'
          ldStart = CTOD(crltd_vlu)
        CASE crltd_typ = 'DENDATE'
          ldEnd  = CTOD(crltd_vlu)
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT (lcAlias)
 
   
  lnDiscv = lnDisc
  IF (EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. POSHDR.ENTERED < ldStart) .OR. ;
     (!EMPTY(ldEnd) .AND. EMPTY(ldStart) .AND. POSHDR.ENTERED > ldEnd) .OR. ;
     (!EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. !BETWEEN(POSHDR.ENTERED,ldStart,ldEnd))
    lnDiscv = 0
  ENDIF

ENDIF

RETURN lnDiscv

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/30/2004
*! Purpose   : Get Color Length and Non major/free Length
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************

FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]

lnMajSeg  = gfItemMask('SM')  && No. of major segments.

DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''

*C130884,1 Custom report for OFF10 [Start]
lcNonMajPos = 0
*C130884,1 Custom report for OFF10 [Start]

*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
  
    *C130884,1 Custom report for OFF10 [Start]             
    lcNonMajPos = laMajSegs[lnI,4]
    *C130884,1 Custom report for OFF10 [Start]

    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

*C130884,1 Custom report for OFF10 [Start]
STORE lcNonMajPos TO lnNonMajPosition 
*C130884,1 Custom report for OFF10 [Start]

lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTl) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 06/30/2004
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

*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
*lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3, 'BROWSE',SET("DATASESSION"))
IF TYPE('lcXMLFileName') = 'C'
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcCursor, lcTable, oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
ELSE
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3, 'BROWSE',SET("DATASESSION"))
ENDIF
*:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]
IF lnConnectionHandlar = 1
  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [Start]
  *lnResult = loOGScroll.oRDA.mCloneStandardCursor (lcCursor, SET("DATASESSION"))
  IF TYPE('lcXMLFileName') = 'C'
    lnResult = oAriaApplication.RemoteCompanyData.clonestandardcursor(lcCursor, SET("DATASESSION"))
  ELSE
    lnResult = loOGScroll.oRDA.mCloneStandardCursor (lcCursor, SET("DATASESSION"))
  ENDIF  
  *:E303329,1 SAB 01/13/2013 Convert Report to run from Request Builder [End]

  
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) && OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  *=CURSORSETPROP("Buffering",lnBuffering,lcCursor)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 06/30/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE
  CASE UPPER(lcTable) = "POSHDR"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO'
    laIndex[1,2] = 'POSHDR'
    
  CASE UPPER(lcTable) = "POSLN"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CWARECODE+ACCOUNT+STORE+CINVTYPE+STYLE+TRANCD'
    laIndex[1,2] = 'POSLNW'

  CASE UPPER(lcTable) = "FABRIC"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'STYLE'
    laIndex[1,2] = 'FABRIC'

  CASE UPPER(lcTable) = "CTKTBOM"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CIMTYP+CUTTKT+TYP+CINVTYPE+ITEM+MFGCODE+DYELOT'
    laIndex[1,2] = 'CTKTBOM'

  *C130884,1 Custom report for OFF10 [Start]
  CASE UPPER(lcTable) = "BOMHEADR"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CITMMAJOR+CCSTSHT_ID'
    laIndex[1,2] = 'BOMHEADR'

  CASE UPPER(lcTable) = "CUTPICK"
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CTKTNO+STYLE'
    laIndex[1,2] = 'CUTPICK'
  *C130884,1 Custom report for OFF10 [Start]


ENDCASE
*!*************************************************************
*! Name      : lfGetDisc
*: Developer : Heba Fathi (HFK)
*: Date      : 08/02/2005
*! Purpose   : Get shipvia on line level and check if multiware
*! Note      : due to bug B127974
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION xxlfGetDisc

lnalias =SELECT()
llLine = .F.
IF poshdr.lmultiware
  llLine = .T.
ELSE
  SELECT distinct posln.shipVia FROM posln WHERE po=PosHdr.po INTO CURSOR lcShipRec
  SELECT lcShipRec
  IF RECCOUNT() > 1
    llLine = .T.
  ELSE
    llLine = .F.
  ENDIF
ENDIF
SELECT (lnAlias)

*:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [Start]
*!*************************************************************
*! Name      : lfGetDisc
*: Developer : Mariam Mazhar (MMT)
*: Date      : 07/06/2011
*! Purpose   : Get price value per line
*! Note      : due to bug B609610
*!*************************************************************
FUNCTION  lfGetLineCost
RETURN IIF(lcRpPrice='C',IIF(llrpfc,IIF(llrpPdic,Posln.nfcost1,Posln.GROS_PRICE),;
	   IIF(llrpPdic,Posln.NICost1,(Posln.NICost1*100/(100-Posln.DISC_PCNT)))) ,IIF(lcRpPrice='P',;
	   IIF(llrpPdic, (1-lnDisc/100),1) *IIF(POSLN.NSELPRICE<>0,POSLN.NSELPRICE,StYLE.PRICEA) ,0 )) 
*:B609610,1 MMT 07/06/2011 Enable Option "Print Price After Disc" When option "Print P.\Selling Price" is not "NONE" [END]
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][Start]
*!*************************************************************
*! Name      : lfUpdate
*! Developer : Saber A Razek
*! Date      : 16/07/2013
*! Purpose   : CHECK IF PO PRINTED
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Called From : Frx
*!*************************************************************
FUNCTION lfUpdate

IF SYS(2040)='2'
  llPrinter = .T.  
  IF  TYPE('lcXMLFileName') <> 'C'
    loOgScroll.ll2Printer=.T.  
  ENDIF
ENDIF

RETURN '' &&.T.

ENDFUNC
*-End Of lfUpdate
*:E303400,1 SAB 07/17/2013 Add Print Flag to the PO report [T20121107.0002][End]