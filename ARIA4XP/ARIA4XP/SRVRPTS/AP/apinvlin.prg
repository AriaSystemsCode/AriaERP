*:************************************************************************************
*: Program file       : APINVLIN.PRG
*: Program desc.      : AP Detial Report,To show Transactions detail from AP invoice lines.
*: System             : Aria Advantage Series.
*: Module             : Accounts Payable (AP)
*: Developer          : Mariam Mazhar(MMT)
*: Tracking Job Number: E302669
*: Date               : 02/17/2010
*:************************************************************************************
*: Calls :
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Notes   : ....
*:************************************************************************************
*: Example : DO APINVLNE.PRG
*:************************************************************************************
*: Modifications :
**************************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

lcStTime = TIME()
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]
  LOCAL loEnvironment
  loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  loEnvironment.ClientID= ClientID
  loEnvironment.ConnectionsRefresh()  
  *T20100512.0026 Hassan 2010 05 23 [END]
  
  LOCAL lcCurrentProcedure
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID)  , ClientID
  *T20100512.0026 Hassan 2010 05 23 [END]
  
  oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
  oAriaEnvironment.REPORT.gcAct_Appl = "AP"
  oAriaEnvironment.REPORT.cCROrientation = "L"
  oAriaEnvironment.activeModuleID = 'AP'
  PUBLIC gcAct_Appl
  gcAct_Appl = "AP"

  IF LEFT(gcDevice, 7) = "PRINTER"
    oAriaEnvironment.gcDevice = "PRINTER"
  ELSE
    oAriaEnvironment.gcDevice = "FILE"
  ENDIF
  =gfOpenTable("APINVHDR","INVVEND",'SH',lcTempAPINVHDR)
  SELECT * FROM &lcTempAPINVHDR  WHERE .F. INTO CURSOR &lcAPINVHDR READWRITE
  =lfMakeIndex(lcAPINVHDR)   && CINVNO+CVENDCODE
  =gfOpenTable("APVINVDT","ORGVINV",'SH',lcTempAPVINVDT)
  SELECT * FROM &lcTempAPVINVDT WHERE .F. INTO CURSOR &lcAPVINVDT READWRITE
  =lfMakeIndex(lcAPVINVDT) && CVENDCODE+CAPINVNO+CAPVILNO
  =gfOpenTable("APINVTKT","LNCONT",'SH',lcTempAPINVTKT)
  DIMENSION laRpCSr[1]
  DIMENSION laRpCTgt[1]
  =gfSubStr(lcRpSrcArr,@laRpCSr, ',')
  =gfSubStr(lcRpTrgArr,@laRpCTgt, ',')

ELSE
  loogScroll.cCROrientation = 'L'
ENDIF
*--- Start Valriable declaration.

IF  TYPE('lcXMLFileName') = 'C' OR (TYPE('lcXMLFileName') <> 'C' AND loogScroll.llOGFltCh)

  IF lcAPINVHDR <> lcFnlAPINVHDR AND lcFnlAPINVHDR <> lcTempAPINVHDR
    IF USED(lcFnlAPINVHDR)
      USE IN (lcFnlAPINVHDR)
    ENDIF
  ENDIF
  IF lcFnlAPINVHDR == lcTempAPINVHDR
    lcFnlAPINVHDR = gfTempName()
  ENDIF
  IF lcAPINVHDR == lcFnlAPINVHDR
    lcFnlAPINVHDR = gfTempName()
  ENDIF
  IF lcAPVINVDTFnl <> lcAPVINVDT
    IF USED(lcAPVINVDTFnl)
      USE IN (lcAPVINVDTFnl)
    ENDIF
  ENDIF
  IF lcAPVINVDTFnl == lcAPVINVDT
    lcAPVINVDTFnl = gfTempName()
  ENDIF
  IF USED(lcAPINVHDR)
    USE IN (lcAPINVHDR)
    SELECT * FROM &lcTempAPINVHDR  WHERE .F. INTO CURSOR &lcAPINVHDR READWRITE
    =lfMakeIndex(lcAPINVHDR)
  ENDIF
  IF USED(lcAPVINVDT)
    USE IN (lcAPVINVDT)
    SELECT * FROM &lcTempAPVINVDT WHERE .F. INTO CURSOR &lcAPVINVDT READWRITE
    =lfMakeIndex(lcAPVINVDT)
  ENDIF
  STORE '' TO lcDocName , lcCstTypFlt , lcCostName , lcDocTyp
  STORE SPACE(08) TO lcVendCode , lcInvAlias , lcOperList
  STORE SPACE(03) TO lcCurrCode
  llSelByInv = .F.
  STORE {} TO ldStrDat , ldEndDat
  =lfGetTyps()
  =lfvCrTemp()
  =lfUpdFltVar()
  =lfCollData()
  =lfTmpFltr()
ENDIF

SELECT (lcTempFile)
SET ORDER TO TAG &lcTempFile
LOCATE
IF EOF()
  *-- 'No Record Selected for the report..!'
  IF  TYPE('lcXMLFileName') <> 'C'
    *=gfModalGen('TRM00052B00000','DIALOG')
  ELSE
    RETURN
  ENDIF
ELSE
  IF TYPE('lcXMLFileName') <> 'C'
    DO gfDispRe WITH EVALUATE('lcRpForm')
  ELSE
    oAriaEnvironment.REPORT.OGLastForm = lcRpForm
    loProgress.Percent = 0.9
    loProgress.DESCRIPTION = "Printing Report..."
    *T20100512.0026 Hassan 2010 05 23 [BEGIN]
    *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    *T20100512.0026 Hassan 2010 05 23 [END]

    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

    IF loProxy.GetRequest(lcRequestID, ClientID).STATUS = 3
      oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.DESCRIPTION = "Printing Report..."
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetTyps
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Get the Cost types codes.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfGetTyps()
*!*************************************************************

FUNCTION lfGetTyps

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*-- Get the Document Type.
DO CASE
CASE lcRpDocTyp = 'I'
  lcDocTyp = 'I'
CASE lcRpDocTyp = 'R'
  lcDocTyp = 'R'
CASE lcRpDocTyp = 'S'
  lcDocTyp = 'S'
CASE lcRpDocTyp = 'M'
  lcDocTyp = 'M'
CASE lcRpDocTyp = 'L'
  lcDocTyp = 'L'
CASE lcRpDocTyp = 'F'
  lcDocTyp = 'F'
CASE lcRpDocTyp = 'T'
  lcDocTyp = 'T'
CASE lcRpDocTyp = 'G'
  lcDocTyp = 'G'
ENDCASE
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvCrTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Create Temp File.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvCrTemp()
*!*************************************************************

FUNCTION lfvCrTemp

*-- Create temporary file to collect the data.
DIMENSION laTempacStru[31,4]
laTempacStru[1,1]='Vendor'
laTempacStru[1,2]='C'
laTempacStru[1,3]= 8
laTempacStru[1,4]= 0

laTempacStru[2,1]='CostName'
laTempacStru[2,2]='C'
laTempacStru[2,3]= 20
laTempacStru[2,4]= 0

laTempacStru[3,1]='DocTyp'
laTempacStru[3,2]='C'
laTempacStru[3,3]= 1
laTempacStru[3,4]= 0

laTempacStru[4,1]='Color'
laTempacStru[4,2]='C'
laTempacStru[4,3]= 6
laTempacStru[4,4]= 0

laTempacStru[5,1]=' VilNo'
laTempacStru[5,2]='C'
laTempacStru[5,3]= 4
laTempacStru[5,4]= 0

laTempacStru[6,1]='LotNo'
laTempacStru[6,2]='C'
laTempacStru[6,3]= 2
laTempacStru[6,4]= 0

laTempacStru[7,1]='Bom_Type'
laTempacStru[7,2]='C'
laTempacStru[7,3]= 1
laTempacStru[7,4]= 0

laTempacStru[8,1]='Codes'
laTempacStru[8,2]='C'
laTempacStru[8,3]= 6
laTempacStru[8,4]= 0

laTempacStru[9,1]='Session'
laTempacStru[9,2]='C'
laTempacStru[9,3]= 6
laTempacStru[9,4]= 0

laTempacStru[10,1]='Amount'
laTempacStru[10,2]='N'
laTempacStru[10,3]= 13
laTempacStru[10,4]= 3

laTempacStru[11,1]='Price'
laTempacStru[11,2]='N'
laTempacStru[11,3]= 13
laTempacStru[11,4]= 3

laTempacStru[12,1]='TotQty'
laTempacStru[12,2]='N'
laTempacStru[12,3]= 11
laTempacStru[12,4]= 3

laTempacStru[13,1]='Size8'
laTempacStru[13,2]='N'
laTempacStru[13,3]= 6
laTempacStru[13,4]= 0

laTempacStru[14,1]='Size7'
laTempacStru[14,2]='N'
laTempacStru[14,3]= 6
laTempacStru[14,4]= 0

laTempacStru[15,1]='Size6'
laTempacStru[15,2]='N'
laTempacStru[15,3]= 6
laTempacStru[15,4]= 0

laTempacStru[16,1]='Size5'
laTempacStru[16,2]='N'
laTempacStru[16,3]= 6
laTempacStru[16,4]= 0

laTempacStru[17,1]='Size4'
laTempacStru[17,2]='N'
laTempacStru[17,3]= 6
laTempacStru[17,4]= 0

laTempacStru[18,1]='Size3'
laTempacStru[18,2]='N'
laTempacStru[18,3]= 6
laTempacStru[18,4]= 0

laTempacStru[19,1]='Size2'
laTempacStru[19,2]='N'
laTempacStru[19,3]= 6
laTempacStru[19,4]= 0

laTempacStru[20,1]='Size1'
laTempacStru[20,2]='N'
laTempacStru[20,3]= 6
laTempacStru[20,4]= 0

laTempacStru[21,1]='STYLE'
laTempacStru[21,2]='C'
laTempacStru[21,3]= 19
laTempacStru[21,4]= 0

laTempacStru[22,1]='Operation'
laTempacStru[22,2]='C'
laTempacStru[22,3]= 30
laTempacStru[22,4]= 0

laTempacStru[23,1]='Cost_Type'
laTempacStru[23,2]='C'
laTempacStru[23,3]= 1
laTempacStru[23,4]= 0

laTempacStru[24,1]='DocNo'
laTempacStru[24,2]='C'
laTempacStru[24,3]= 6
laTempacStru[24,4]= 0

laTempacStru[25,1]='Doc_Type'
laTempacStru[25,2]='C'
laTempacStru[25,3]= 40
laTempacStru[25,4]= 0

laTempacStru[26,1]='Inv_Date'
laTempacStru[26,2]='D'
laTempacStru[26,3]= 8
laTempacStru[26,4]= 0

laTempacStru[27,1]='Invoice'
laTempacStru[27,2]='C'
laTempacStru[27,3]= 12
laTempacStru[27,4]= 0

laTempacStru[28,1]='Currency '
laTempacStru[28,2]='C'
laTempacStru[28,3]= 3
laTempacStru[28,4]= 0

laTempacStru[29,1]='Ven_name'
laTempacStru[29,2]='C'
laTempacStru[29,3]= 24
laTempacStru[29,4]= 0

laTempacStru[30,1]='Dyelot'
laTempacStru[30,2]='C'
laTempacStru[30,3]= 10
laTempacStru[30,4]= 0

laTempacStru[31,1]='PrtHead'
laTempacStru[31,2]='L'
laTempacStru[31,3]= 1
laTempacStru[31,4]= 0

IF USED(lcTempFile)
  USE IN (lcTempFile)
ENDIF
=gfCrtTmp(lcTempFile,@laTempacStru,"Vendor + Currency + Invoice + Doc_Type+Bom_Type",lcTempFile,.T.)
SELECT(lcTempFile)
INDEX ON Codes     TAG (lcTempF2)
INDEX ON SESSION   TAG (lcTempF3)
INDEX ON LotNo     TAG (lcTempF4)
INDEX ON Cost_Type TAG (lcTempF5)
*!*************************************************************
*! Name      : lfCollData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Data Collection.
*!*************************************************************
*! Calls     : lfDocType() , lfPrntCTyp() , gfAmntDisp()
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
*--function to get filter files
=lfGetInvoices()
SELECT (lcTempFile)
LOCATE

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Get the values of filter Exp. variables.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfUpdFltVar()
*!*************************************************************

FUNCTION lfUpdFltVar

*--APINVHDR.CINVNO
lnPosInvNo = ASCAN(laOgFXFlt,"APINVHDR.CINVNO")
IF lnPosInvNo > 0
  lnPosInvNo = ASUBSCRIPT(laOgFXFlt,lnPosInvNo,1)
  lcInvAlias = laOgFXFlt[lnPosInvNo,6]
  IF !EMPTY(lcInvAlias)
    llSelByInv = (USED(lcInvAlias) .AND. RECCOUNT(lcInvAlias) <> 0)
  ENDIF
ENDIF

*--Currency
lnPosCurr = ASCAN(laOgFXFlt,"APINVHDR.CCURRCODE")
IF lnPosCurr > 0
  lnPosCurr = ASUBSCRIPT(laOgFXFlt,lnPosCurr,1)
  lcCurrCode= laOgFXFlt[lnPosCurr,6]
ENDIF

*--Vendor code
lnPosVend = ASCAN(laOgFXFlt,"APVENDOR.CVENDCODE")
IF lnPosVend > 0
  lnPosVend = ASUBSCRIPT(laOgFXFlt,lnPosVend,1)
  lcVendCode = laOgFXFlt[lnPosVend,6]
ENDIF

*--DATE
lnPosDate = ASCAN(laOgFXFlt,"APINVHDR.DINVDATE")
IF lnPosDate > 0
  lnPosDate = ASUBSCRIPT(laOgFXFlt,lnPosDate,1)
  ldStrDat = CTOD(ALLTRIM(SUBSTR(laOgFXFlt[lnPosDate,6],1,10)))
  ldEndDat = CTOD(ALLTRIM(SUBSTR(laOgFXFlt[lnPosDate,6],12,20)))
ENDIF

*--Operation
lnPosOper = ASCAN(laOgFXFlt,"APVINVDT.COPRCODE")
IF  lnPosOper > 0
  lnPosOper = ASUBSCRIPT(laOgFXFlt,lnPosOper,1)
  lcOperList = laOgFXFlt[lnPosOper,6]
ENDIF



*!*************************************************************
*! Name      : lfTmpFltr
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Filter the temp file.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfTmpFltr()
*!*************************************************************

FUNCTION lfTmpFltr

lcFinalFlt = ".T."
IF !EMPTY(lcOperList)
  lcFinalFlt = lcFinalFlt + ".AND. Codes $ lcOperList"
ENDIF

IF lcRpIncCont ='Y'
  IF !EMPTY(lcRpSess)
    lcRpSess = ALLTRIM(lcRpSess)
    lcFinalFlt = lcFinalFlt + " .AND. Session = lcRpSess"
  ENDIF
ENDIF
IF !EMPTY(lcRpLot)
  lcRpLot = ALLTRIM(lcRpLot)
  lcFinalFlt = lcFinalFlt + " .AND. LotNo = lcRpLot"
ENDIF
IF !EMPTY(laRpCTgt[1])
  FOR lnCout = 1 TO ALEN(laRpCTgt,1)
    lcCstTypFlt = lcCstTypFlt + SUBSTR(laRpCTgt[lnCout],1,1) + '|'
  ENDFOR
  lcFinalFlt = lcFinalFlt + " .AND. Cost_Type $ lcCstTypFlt"
ENDIF
SELECT (lcTempFile)
DELETE ALL FOR !(&lcFinalFlt)
LOCATE

*!*************************************************************
*! Name      : lfDocType
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Get the name of Document Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcDocName
*!*************************************************************
*! Example            : =lfDocType()
*!*************************************************************

FUNCTION lfDocType
PARAMETERS Doctype
DO CASE
CASE Doctype = 'I'
  lcDocName = 'Style Purchase Order'
CASE Doctype = 'R'
  lcDocName = 'Style Purchase Order Return'
CASE Doctype = 'S'
  lcDocName = 'Shipment'
CASE Doctype= 'M'
  lcDocName = 'Cutting Ticket'
CASE Doctype = 'L'
  lcDocName = 'Material PO'
CASE Doctype = 'F'
  lcDocName = 'Material PO Return'
CASE Doctype = 'T'
  lcDocName = 'Material MFG Order'
CASE Doctype = 'G'
  lcDocName = 'General Expense'
ENDCASE
RETURN lcDocName

*!*************************************************************
*! Name      : lfPrntCTyp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : Get the name of Cost Type to be printed.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : lcCostName
*!*************************************************************
*! Example            : =lfPrntCTyp()
*!*************************************************************

FUNCTION lfPrntCTyp
IF m.DocTyp $'LF'
  DO CASE
  CASE m.Cost_Type = 'P' .AND. m.Bom_Type = '1'
    lcCostName = 'Purchase Price'
  CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '2'
    lcCostName = 'Freight'
  CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '3'
    lcCostName = 'Tax'
  CASE m.Cost_Type = 'D' .AND. m.Bom_Type = '4'
    lcCostName = 'Quota'
  ENDCASE
ENDIF
RETURN lcCostName
*!*************************************************************
*! Name      : lfGetInvoices
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/17/2010
*! Purpose   : When run OG function.
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
*! Example   : =lfWRunGrid()
*!*************************************************************
FUNCTION lfGetInvoices


PRIVATE llCallGfAm
llCallGfAm = EMPTY(lcCurrCode) .OR. (!EMPTY(lcCurrCode) .AND. lcRpCurr <> 'F')
*---------------------
IF lcRpDocTyp $ 'IRS'
  STORE '' TO M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,;
    M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7
  =gfGetMemVar('M_CISLBL1,M_CISLBL2,M_CISLBL3,M_CISLBL4,M_CISLBL5,M_CISLBL6,M_CISLBL7,M_CITYPE1,M_CITYPE2,M_CITYPE3,M_CITYPE4,M_CITYPE5,M_CITYPE6,M_CITYPE7',oAriaApplication.ActiveCompanyID)
ENDIF
IF lcRpDocTyp $'MT'
  STORE '' TO M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,;
    M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7
  =gfGetMemVar('M_CMTYPE1,M_CMTYPE2,M_CMTYPE3,M_CMTYPE4,M_CMTYPE5,M_CMTYPE6,M_CMTYPE7,M_CMSLBL1,M_CMSLBL2,M_CMSLBL3,M_CMSLBL4,M_CMSLBL5,M_CMSLBL6,M_CMSLBL7',oAriaApplication.ActiveCompanyID)
ENDIF
llVendSelect = .F.
IF !EMPTY(lcVendCode) AND USED(lcVendCode)
  SELECT (lcVendCode)
  LOCATE
  IF !EOF()
    llVendSelect = .T.
  ENDIF
ENDIF
llCurSelect = .F.
IF !EMPTY(lcCurrCode) AND USED(lcCurrCode)
  SELECT (lcCurrCode)
  LOCATE
  IF !EOF()
    llCurSelect = .T.
  ENDIF
ENDIF
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW "Collecting Data..." NOWAIT
ENDIF
*----------------------
IF llSelByInv
  SELECT(lcInvAlias)
  LOCATE
  lcInsTR = ""
  lnRecNo = 0
  lcSelCond1 = ''
  lcSleFils = "APINVHDR"
  lcSeleFlds=  "APINVHDR.*"
  IF !EMPTY(ldStrDat) AND !EMPTY(ldEndDat)
    lcSelCond1 = lcSelCond1 + IIF(!EMPTY(lcSelCond1)," AND ","")+;
      " APINVHDR.dinvdudat BETWEEN '"+ALLTRIM(DTOS(ldStrDat))+;
      "' and '"+ALLTRIM(DTOS(ldEndDat))+"'"
  ENDIF

  SCAN
    lnRecNo = lnRecNo + 1
    lccInvNo = cInvNo
    IF "'" $ cInvNo
      lccInvNo = STRTRAN(cInvNo,"'","''")
    ENDIF
    lcInsTR =  lcInsTR +"'" + lccInvNo + "',"
    IF lnRecNo = 1000
      lnRecNo = 0
      lcInsTR = "("+SUBSTR(lcInsTR,1,LEN(lcInsTR)-1)+ ")"
      lcSelCond =lcSelCond1 + IIF(!EMPTY(lcSelCond1)," AND ","")+ "APINVHDR.cInvNo IN "+lcInsTR
      SELECT(lcTempAPINVHDR)
      =gfSqlRun("Select APINVHDR.* from APINVHDR INNER JOIN APVINVDT ON  APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO AND APVINVDT.cimtyp ='"+lcDocTyp+"' "+IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVHDR,.T.,'TmpInvHdr')
      SELECT (lcTempAPINVHDR)
      APPEND FROM DBF('TmpInvHdr')
      SELECT(lcTempAPVINVDT)
      =gfSqlRun("Select APVINVDT.* from APVINVDT INNER JOIN APINVHDR  ON APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO WHERE APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
        IIF(!EMPTY(lcSelCond)," AND "+lcSelCond,''),lcTempAPVINVDT,.T.,'TmpInvDt')

      SELECT(lcTempAPVINVDT)
      APPEND FROM DBF('TmpInvDt')
      SELECT(lcTempAPINVTKT)
      =gfSqlRun("Select APINVTKT.* from APINVTKT INNER JOIN APINVHDR ON "+;
        "APINVTKT.CVENDCODE =APINVHDR.CVENDCODE AND APINVTKT.CAPINVNO=APINVHDR.CINVNO INNER join APVINVDT ON "+;
        " APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO "+;
        " AND  APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
        IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVTKT,.T.,'TmpKtk')

      SELECT(lcTempAPINVTKT)
      APPEND FROM DBF('TmpKtk')
      lcInsTR = ""
    ENDIF
  ENDSCAN
  IF lnRecNo >0
    lcInsTR = "("+SUBSTR(lcInsTR,1,LEN(lcInsTR)-1)+ ")"
    lcSelCond =lcSelCond1 + IIF(!EMPTY(lcSelCond1)," AND ","")+ "APINVHDR.cInvNo IN "+lcInsTR



    SELECT(lcTempAPINVHDR)
    =gfSqlRun("Select APINVHDR.* from APINVHDR INNER JOIN APVINVDT ON  APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO AND APVINVDT.cimtyp ='"+lcDocTyp+"' "+IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVHDR,.T.,'TmpInvHdr')
    SELECT (lcTempAPINVHDR)
    APPEND FROM DBF('TmpInvHdr')

    SELECT(lcTempAPVINVDT)
    =gfSqlRun("Select APVINVDT.* from APVINVDT INNER JOIN APINVHDR  ON APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO WHERE APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
      IIF(!EMPTY(lcSelCond)," AND "+lcSelCond,''),lcTempAPVINVDT,.T.,'TmpInvDt')

    SELECT(lcTempAPVINVDT)
    APPEND FROM DBF('TmpInvDt')

    SELECT(lcTempAPINVTKT)
    =gfSqlRun("Select APINVTKT.* from APINVTKT INNER JOIN APINVHDR ON "+;
      "APINVTKT.CVENDCODE =APINVHDR.CVENDCODE AND APINVTKT.CAPINVNO=APINVHDR.CINVNO INNER join APVINVDT ON "+;
      " APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO "+;
      " AND  APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
      IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVTKT,.T.,'TmpKtk')
    SELECT(lcTempAPINVTKT)
    APPEND FROM DBF('TmpKtk')
  ENDIF
  lcFnlAPINVHDR = lcTempAPINVHDR
ELSE

  lcSelCond = ""
  lcSleFils = "APINVHDR"
  lcSeleFlds=  "APINVHDR.*"

  IF llVendSelect
    SELECT (lcVendCode)
    LOCATE
    lcInsTR = ""
    SCAN
      lccCVENDCODE  = STRTRAN(CVENDCODE,"'","''")
      lcInsTR =  lcInsTR +"'" + lccCVENDCODE + "',"
    ENDSCAN
    lcInsTR = "("+SUBSTR(lcInsTR,1,LEN(lcInsTR)-1) +  ")"
    lcSelCond =  IIF(!EMPTY(lcSelCond)," AND ","") + "APINVHDR.CVENDCODE IN "+lcInsTR
  ENDIF

  IF llCurSelect
    SELECT (lcCurrCode)
    LOCATE
    lcCInsTR = ""
    SCAN
      lccCur = STRTRAN(CCURRCODE,"'","''")
      lcCInsTR =  lcCInsTR +"'" + lccCur + "',"
    ENDSCAN
    lcCInsTR = "("+SUBSTR(lcCInsTR,1,LEN(lcCInsTR)-1)+  ")"
    lcSelCond =  IIF(!EMPTY(lcSelCond)," AND ","") + "APINVHDR.CCURRCODE IN "+lcCInsTR
  ENDIF


  IF !EMPTY(ldStrDat) AND !EMPTY(ldEndDat)
    lcSelCond = lcSelCond + IIF(!EMPTY(lcSelCond)," AND ","")+" APINVHDR.dinvdudat BETWEEN '"+ALLTRIM(DTOS(ldStrDat))+"' and '"+ALLTRIM(DTOS(ldEndDat))+"'"
  ENDIF

  SELECT(lcTempAPINVHDR)
  =gfSqlRun("Select APINVHDR.* from APINVHDR INNER JOIN APVINVDT ON  APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO AND APVINVDT.cimtyp ='"+lcDocTyp+"' "+IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVHDR)
  SELECT(lcTempAPVINVDT)
  =gfSqlRun("Select APVINVDT.* from APVINVDT INNER JOIN APINVHDR ON APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO WHERE APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
    IIF(!EMPTY(lcSelCond)," AND "+lcSelCond,''),lcTempAPVINVDT)
  SELECT(lcTempAPINVTKT)
  =gfSqlRun("Select APINVTKT.* from APINVTKT INNER JOIN APINVHDR ON "+;
    " APINVTKT.CVENDCODE =APINVHDR.CVENDCODE AND APINVTKT.CAPINVNO=APINVHDR.CINVNO INNER join APVINVDT ON "+;
    " APVINVDT.CVENDCODE =APINVHDR.CVENDCODE AND APVINVDT.CAPINVNO=APINVHDR.CINVNO "+;
    " AND  APVINVDT.cimtyp ='"+lcDocTyp+"' "+;
    IIF(!EMPTY(lcSelCond)," Where "+lcSelCond,''),lcTempAPINVTKT)
  lcFnlAPINVHDR = lcTempAPINVHDR
ENDIF
lcShipNo = ''
lcPosNo  = ''
IF lcDocTyp = 'S'
  lnPosShpNo = ASCAN(laOgFXFlt,"SHPMTHDR.SHIPNO")
  IF lnPosShpNo > 0
    lnPosShpNo = ASUBSCRIPT(laOgFXFlt,lnPosShpNo,1)
    lcShipNo = laOgFXFlt[lnPosShpNo,6]
    IF !EMPTY(lcShipNo) AND USED(lcShipNo) AND RECCOUNT(lcShipNo)> 0
      SELECT(lcShipNo)
      INDEX ON ShipNo TAG &lcShipNo
    ENDIF
  ENDIF
ELSE
  lnPosNo = ASCAN(laOgFXFlt,"POSHDR.PO")
  IF lnPosNo > 0
    lnPosNo = ASUBSCRIPT(laOgFXFlt,lnPosNo,1)
    lcPosNo = laOgFXFlt[lnPosNo,6]
  ENDIF
  IF !EMPTY(lcPosNo) AND USED(lcPosNo) AND RECCOUNT(lcPosNo)>0
    SELECT (lcPosNo)
    INDEX ON po TAG &lcPosNo
  ENDIF
ENDIF
IF lcDocTyp = 'S'
  lcForCond = 'cimtyp+SHIPNO = lcDocTyp '
ELSE
  lcForCond = 'cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot = lcDocTyp'
ENDIF
llHasShip = (lcDocTyp = 'S' AND !EMPTY(lcShipNo) AND USED(lcShipNo) AND  RECCOUNT(lcShipNo)>0)
llHasPO   = (!EMPTY(lcPosNo) AND USED(lcPosNo) AND RECCOUNT(lcPosNo)>0)
*--
SELECT (lcTempAPVINVDT)
=gfSETORDER("ITEM")
IF !SEEK(lcDocTyp)
  gfSETORDER("ORGVINV")
  RETURN
ENDIF
SELECT(lcTempAPVINVDT)
=gfSETORDER("ORGVINV")
SELECT(lcFnlAPINVHDR)
SET ORDER TO
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW "Collecting Data..." NOWAIT
ENDIF
SCAN
  IF llVendSelect AND !SEEK(CVENDCODE,lcVendCode)
    LOOP
  ENDIF
  IF llCurSelect AND !SEEK(CCURRCODE,lcCurrCode)
    LOOP
  ENDIF
  m.Invoice  = cInvNo
  m.Vendor   = CVENDCODE
  m.Ven_Name = COUTCOMP
  m.Currency = CCURRCODE
  m.Inv_Date = DINVDATE
  IF TYPE('lcXMLFileName') = 'C'
    lnPerCent = RECNO()/RECCOUNT(lcFnlAPINVHDR)
    IF MOD(RECNO(lcFnlAPINVHDR),CEILING(RECCOUNT(lcFnlAPINVHDR) / 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
      loProgress.DESCRIPTION = "Validating vendor "+m.Vendor+" Transactions"
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF
  ELSE
    IF MOD(RECNO(lcFnlAPINVHDR),CEILING(RECCOUNT(lcFnlAPINVHDR) / 10)) = 0
      WAIT WINDOW   "Validating vendor "+m.Vendor+" Transactions" NOWAIT
    ENDIF
  ENDIF
  SELECT(lcTempAPVINVDT)
  IF SEEK(m.Vendor+ m.Invoice)

    SELECT(lcTempAPVINVDT)
    SCAN REST WHILE CVENDCODE+CAPINVNO+CAPVILNO = m.Vendor+ m.Invoice;
        FOR cimtyp+ctktno+clotno+STR(LINENO,6)+cbomtype+coprcode+ITEM+COLOR+cdyelot = lcDocTyp AND ;
        IIF(llHasShip,SEEK(ShipNo,lcShipNo),IIF(llHasPO,SEEK(ctktno,lcPosNo),.T.))

      m.VilNo     = CAPVILNO
      m.Cost_Type = CCOSTTYPE
      m.Bom_Type  = cbomtype
      IF TYPE('lcXMLFileName') = 'C'
        m.Operation =  oAriaEnvironment.Codes.getcodedescription(coprcode, PADR('MFGCODE',10))
      ELSE
        m.Operation = gfCodDes(coprcode,PADR('MFGCODE',10))
      ENDIF
      IF m.Operation = 'All'
        m.Operation = ''
      ENDIF
      m.Codes     =coprcode
      m.LotNo     =clotno
      m.DocNo     = IIF(lcDocTyp = 'S',ShipNo,ctktno)

      m.DocTyp    = cimtyp
      IF !EMPTY(m.Cost_Type)
        m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfPrntCTyp()))
      ELSE
        m.CostName = ""
      ENDIF
      m.Doc_Type  = lfDocType(m.DocTyp)
      m.Style     = ITEM
      m.Color     = COLOR
      m.Session   = ""
      m.Size1     = napaprqty1
      m.Size2     = napaprqty2
      m.Size3     = napaprqty3
      m.Size4     = napaprqty4
      m.Size5     = napaprqty5
      m.Size6     = napaprqty6
      m.Size7     = napaprqty7
      m.Size8     = napaprqty8
      IF INLIST(lcDocTyp,'L','F','T','G')
        m.TotQty = naptaprQTY
      ELSE
        m.TotQty = napaprqty1+napaprqty2+napaprqty3+napaprqty4+napaprqty5+napaprqty6+napaprqty7+napaprqty8
      ENDIF
      m.Price     = napaprPRIC
      m.dyelot     = cdyelot
      m.PrtHead   = .F.
      lnCount     = 0
      SELECT(lcFnlAPINVHDR)
      llInserted = .F.
      m.Amount    = m.TotQty * IIF(llCallGfAm,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
      m.Price     = m.Amount / m.TotQty

      IF lcRpIncCont ='Y'
        SELECT(lcTempAPINVTKT)
        IF SEEK(m.Vendor+m.Invoice+m.VilNo,lcTempAPINVTKT,'LNCONT')
          SELECT(lcTempAPINVTKT)
          SCAN REST WHILE CVENDCODE+CAPINVNO+CAPVILNO+crsession = m.Vendor+m.Invoice+m.VilNo ;
              FOR cimtyp+ctktno+STR(LINENO,6)+crsession = lcDocTyp &&AND !EMPTY(crsession)
            IF !llInserted
              INSERT INTO (lcTempFile) FROM MEMVAR
              llInserted = .T.
            ENDIF
            IF EMPTY(crsession)
              LOOP
            ENDIF
            m.VilNo     = EVALUATE(lcTempAPVINVDT+'.CAPVILNO')
            m.Cost_Type = EVALUATE(lcTempAPVINVDT+'.CCOSTTYPE')
            m.Bom_Type  = EVALUATE(lcTempAPVINVDT+'.CBOMTYPE')
            IF TYPE('lcXMLFileName') = 'C'
              m.Operation =  oAriaEnvironment.Codes.getcodedescription(EVALUATE(lcTempAPVINVDT+'.COPRCODE'), PADR('MFGCODE',10))
            ELSE
              m.Operation = gfCodDes(EVALUATE(lcTempAPVINVDT+'.COPRCODE'),PADR('MFGCODE',10))
            ENDIF
            IF m.Operation = 'All'
              m.Operation = ''
            ENDIF
            lnCount = lnCount + 1
            IF lnCount = 1
              m.PrtHead = .T.
            ELSE
              m.PrtHead = .F.
            ENDIF
            m.Codes     =EVALUATE(lcTempAPVINVDT+'.COPRCODE')
            m.LotNo     =EVALUATE(lcTempAPVINVDT+'.CLOTNO')
            m.DocNo     = IIF(lcDocTyp = 'S',EVALUATE(lcTempAPVINVDT+'.SHIPNO'),EVALUATE(lcTempAPVINVDT+'.CTKTNO'))
            m.DocTyp    = cimtyp
            IF !EMPTY(m.Cost_Type)
              m.CostName  = IIF(m.DocTyp $'IRS',EVAL('M_CISLBL'+m.Bom_Type),IIF(m.DocTyp $'MT',EVAL('M_CMSLBL'+ m.Bom_Type),lfPrntCTyp()))
            ELSE
              m.CostName = ""
            ENDIF
            m.Doc_Type  = lfDocType(m.DocTyp)
            m.Style     = ITEM
            m.Color     = COLOR
            m.Session   = crsession
            m.Size1     = NAPAPLQTY1
            m.Size2     = NAPAPLQTY2
            m.Size3     = NAPAPLQTY3
            m.Size4     = NAPAPLQTY4
            m.Size5     = NAPAPLQTY5
            m.Size6     = NAPAPLQTY6
            m.Size7     = NAPAPLQTY7
            m.Size8     = NAPAPLQTY8

            IF INLIST(lcDocTyp,'L','F','T','G')
              m.TotQty = NAPTAPLQTY
            ELSE
              m.TotQty = NAPAPLQTY1+NAPAPLQTY2+NAPAPLQTY3+NAPAPLQTY4+NAPAPLQTY5+NAPAPLQTY6+NAPAPLQTY7+NAPAPLQTY8
            ENDIF

            m.Price     = NAPAPLPRIC
            m.dyelot    = cdyelot
            SELECT(lcFnlAPINVHDR)
            m.Amount    = m.TotQty * IIF(llCallGfAm,gfAmntDisp(m.Price,lcRpCurr,ldRpExDate,lcRpTmpNam),m.Price)
            m.Price     = m.Amount / m.TotQty
            INSERT INTO (lcTempFile) FROM MEMVAR
          ENDSCAN
        ENDIF
      ELSE

        SELECT(lcTempAPINVTKT)
        IF SEEK(m.Vendor+m.Invoice+m.VilNo,lcTempAPINVTKT,'LNCONT')
          SELECT(lcTempAPINVTKT)
          LOCATE REST WHILE CVENDCODE+CAPINVNO+CAPVILNO+crsession = m.Vendor+m.Invoice+m.VilNo ;
            FOR cimtyp+ctktno+STR(LINENO,6)+crsession = lcDocTyp
          IF FOUND()
            INSERT INTO (lcTempFile) FROM MEMVAR
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
*!*************************************************************
*! Name      : lfMakeIndex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/17/2010
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
  INDEX ON &lcIndex. TAG (lcTag)
ENDFOR
lcTag = laIndex[1,2]
SET ORDER TO TAG (lcTag)

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 02/17/2010
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE

CASE UPPER(lcTable) =  lcAPINVHDR
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'ccurrcode'
  laIndex[1,2] = lcAPINVHDR

  laIndex[2,1] = 'cvendcode'
  laIndex[2,2] = 'cvendcode'

CASE UPPER(lcTable) = lcAPVINVDT
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'cimtyp+SHIPNO'
  laIndex[1,2] = lcAPVINVDT

  laIndex[2,1] = 'cimtyp+ctktno'
  laIndex[2,2] = 'ctktno'

CASE UPPER(lcTable) =lcAPVINVDTFnl
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CVENDCODE+CAPINVNO'
  laIndex[1,2] = lcAPVINVDTFnl

ENDCASE


