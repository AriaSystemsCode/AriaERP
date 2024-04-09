*:**********************************************************************
*: Program file         : MAPMMO
*: Program description  : Print Material Manufacturing Order
*: Module               : Material (MA)
*: Developer            : Mariam Mazhar (MMT)
*: Tracking Job Number  : N037640{T20110914.0019}
*: Date                 : 10/08/2012
*:**********************************************************************
*: Calls: None
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*: Example: DO MAPMMO
*:**********************************************************************
*: Modification :
*: B610112,1 MMT 10/14/2012 Modify Material screen to show foreign costs in case domestic Fabrics[T20110914.0019-Issue1]
*:**********************************************************************

#INCLUDE R:\ARIA4XP\REPORTS\MA\MAPMMO.H
DECLARE INTEGER FindWindow IN user32 STRING lpClassName, STRING lpWindowName
DECLARE INTEGER GetParent IN user32 INTEGER hwnd
DECLARE INTEGER GetFocus IN user32

IF CallFromExportorEmail()
  SetGlobals()
ENDIF
lcStartTime = TIME()
*- property to define paper orienatation = 'Portrait'
loogScroll.cCROrientation = 'P'
STORE .F. TO llEndGroup
llPoDisp = .T.
lnMajor  = LEN(gfItemMask('PM',.F.,'0002'))
lnNMajor = LEN(gfItemMask('PN',.F.,'0002'))
STORE .F. TO llLogo
DIMENSION laCost[14,2]

laCost[1,1]  = 'M_CTTYPE1 '
laCost[2,1]  = 'M_CTTYPE2 '
laCost[3,1]  = 'M_CTTYPE3 '
laCost[4,1]  = 'M_CTTYPE4 '
laCost[5,1]  = 'M_CTTYPE5 '
laCost[6,1]  = 'M_CTTYPE6 '
laCost[7,1]  = 'M_CTTYPE7 '

laCost[8,1]   = 'M_CTSLBL1 '
laCost[9,1]   = 'M_CTSLBL2 '
laCost[10,1]  = 'M_CTSLBL3 '
laCost[11,1]  = 'M_CTSLBL4 '
laCost[12,1]  = 'M_CTSLBL5 '
laCost[13,1]  = 'M_CTSLBL6 '
laCost[14,1]  = 'M_CTSLBL7 '

=gfGetMemvar(@laCost,oAriaApplication.ActiveCompanyID)

STORE SPACE(0) TO laCompAdd,lcSt, lcStyTitle, lcShpName, laShpAdr, lcOper
lcTime   = TIME()                       && Variable to hold the Time
lcMGroup = 'PO'
DIMENSION laCompAdd[6]                 && Array to hold company addresses
*-- Get company information
lcGetCompinfo =[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcGetCompinfo,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
IF lnResult >= 1
  SELECT SYCCOMP
  DECLARE laDivLNam[1,2]
  STORE ''  TO laDivLNam , lcDivLNam
  laDivLNam[1,1] = 'DIVLNAME'
  laDivLNam[1,2] = 'lcDivLNam'
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  * Get the company addresses
  laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  *- laCompAdd[6]    = 'Phone# : ' + TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laCompAdd[6]    = LANG_MAPMMO_Phone + TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
  laCompAdd[6]    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Phone,oAriaApplication.GetHeaderText("LANG_MAPMMO_Phone",AHEADERFILE)) + TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  DO lfShiftArr WITH laCompAdd
ENDIF
DIMENSION laShpAdr[5]
DIMENSION laSum[9]                     && Array to sum size quantities

IF TYPE('loStyle') <> 'O'
  loStyle = CreateObject('RemoteTable','ITEM','STYLE','FABRIC_A',SET("Datasession"))
ENDIF
IF TYPE('loUom') <> 'O'
  loUom = CREATEOBJECT('RemoteTable','UOM','UOMCODE','UOM',SET("Datasession"))
ENDIF

IF llrPrtCs
  STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2
  *-- Initialize variable to use instead of fields
  STORE SPACE(0) TO lcLotNo , lcStyMaj , lcPattrn
  *-- Adjust the inner group of the BOM report
lcMGroup = 'CUTTKT'
lcInGroup = 'CUTTKT+Typ'
  IF TYPE('loFabric') <> 'O'
    loFabric = CreateObject('RemoteTable','ITEM','STYLE','FABRIC_x',SET("Datasession"))
  ENDIF
  *-- Create array to hold the cost element titles.
  IF TYPE('loUom') <> 'O'
    loUom = CREATEOBJECT('RemoteTable','UOM','UOMCODE','UOM',SET("Datasession"))
  ENDIF

  IF TYPE('loCtktBom') <> 'O'
    loCtktBom = CREATEOBJECT('RemoteTable','CTKTBOM','CTKTBOM','CTKTBOM',SET("Datasession"))
  ENDIF
ENDIF

*-- If we will print the cost sheet
lcBOMTit  = ALLTRIM(gfGetMemvar('M_MMOLNLBL',oAriaApplication.ActiveCompanyID))
lcBOMTit  = IIF(RIGHT(lcBOMTit,1) ='#', lcBOMTit,lcBOMTit+'#')

IF loOGScroll.llOgFltCh
  lcCostF   = loOGScroll.gfTempName()
  *Variable to indicate if Extended Size Scale .
  DIMENSION laVenAdr[5]
  STORE SPACE(0) TO lcVenName

  *-- Temporary file of the cutting ticket report.
  lcMainF = loOGScroll.gfTempName()

  = lfCrtTmp()
  *-select statement for Header
  lcHdrStmnt = "Select Distinct(poshdr.po) as PO,poshdr.ctkttype,poshdr.style as hdrstyle,poshdr.pattern ,poshdr.entered,poshdr.cdivision"
  lcHdrStmnt = lcHdrStmnt + ",poshdr.status,poshdr.complete from poshdr(index=poshdr) inner join posln (index=posln) "
  lcHdrStmnt = lcHdrStmnt + "on poshdr.cbusdocu+poshdr.cstytype+poshdr.po = posln.cbusdocu+posln.cstytype+posln.po"
  lcHdrStmnt = lcHdrStmnt + " where posln.trancd = 1 and poshdr.cbusdocu = 'P' and poshdr.cstytype = 'F'"
  IF !EMPTY(loOGScroll.lcRpSqlExp)
    lcHdrStmnt = lcHdrStmnt + " AND " + loOGScroll.lcRpSqlExp
  ENDIF
  StatCount =  LEN(lcStatus)
  IF StatCount <> 7
    lcStatusFlt = " POSHDR.STATUS IN ("
    lnCnt = 1
    FOR I = 1 TO StatCount
      lcStatusFlt = IIF(lnCnt = 1,lcStatusFlt + " '" + SUBSTR(lcStatus,I,1) + "' ",lcStatusFlt + ",'" + SUBSTR(lcStatus,I,1) + "' ")
      lnCnt = 2
    ENDFOR
    lcStatusFlt = lcStatusFlt + ")"
    IF !EMPTY(lcStatusFlt)
      lcHdrStmnt = lcHdrStmnt + " AND " + lcStatusFlt
    ENDIF
  ENDIF
  lcTempMain = loOGScroll.gfTempName()
  lnMainResult = loOGScroll.oRDA.SqlRun(lcHdrStmnt,lcTempMain,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
  IF lnMainResult > 0
    SELECT &lcTempMain
    lnBuffering = CURSORGETPROP("Buffering","&lcTempMain")
    =CURSORSETPROP("Buffering",3,"&lcTempMain")
    INDEX ON PO TAG &lcTempMain
    SET ORDER TO TAG &lcTempMain
    *-make a sql file carrying selected POs
    lcSqlPOs = loOgScroll.gfSQLTempName('','PO C(6)',lcTempMain,'PO')
  ENDIF

  lcStyTitle = gfItemMask('HI',.F.,'0002')
  *-- Set necessary relations

  SELECT OBJLINK
  SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

  IF TYPE('loPosLn') <> 'O'
    loPosLn = CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',SET("Datasession"))
    *-CBUSDOCU+CSTYTYPE+PO
  ENDIF

  *-- Store empty to vendor & M. Opration expertion.
  STORE "" TO lcVenExp , lcMopExp

  SELECT &lcTempMain
  SCAN
    lcPo = &lcTempMain..PO
    lcWareCode = SPACE(8)
    lcStyle    = SPACE(20)
    IF loPosLn.SEEK('P'+'F'+lcPo)
      SELECT POSLN
      SCAN FOR posln.TranCd = '1'
        SCATTER MEMVAR
        m.NoteFlag = 'N'
        m.Notes    = ''
        loStyle.Seek("0002"+POSLN.STYLE)
        m.Desc = FABRIC_A.Desc
        m.Pattern = FABRIC_A.Pattern
        loUom.Seek(FABRIC_A.cconvbuy)
        m.UOMBUY = UOM.CUOM_B
        INSERT INTO (lcMainF) FROM MEMVAR
        REPLACE &lcMainF..cTktType    WITH &lcTempMain..CtktType ,;
        		&lcMainF..HPattern   WITH &lcTempMain..Pattern  ,;
				&lcMainF..HdrStyle   WITH &lcTempMain..HdrStyle ,;
				&lcMainF..HEntered   WITH &lcTempMain..Entered  ,;
				&lcMainF..Status     WITH &lcTempMain..Status   ,;
				&lcMainF..cDivision  WITH &lcTempMain..cDivision ,;
				&lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)
        *-- Get last style in the cuttkt to put the notes lines directly after it.
        IF lcStyle < Style
          lcStyle = Style
        ENDIF
        *-- We will print notes with each warehouse.
        IF cWareCode # lcWareCode .AND. !EMPTY(lcWareCode)
          m.cWareCode = lcWareCode
          lcWareCode  = cWareCode
          m.Style     = lcStyle
          m.Dyelot    = CHR(255)
          M.QTY1      = 0
          M.TOTQTY    = 0
          *-- Get the style note.
          IF llRPrtSn .AND. SEEK('G'+&lcTempMain..HdrStyle,'NotePad')
            m.NoteFlag = 'S'
            m.Notes    = Notepad.MNotes
            REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,;
                    &lcMainF..HPattern   WITH &lcTempMain..Pattern ,;
                    &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,;
                    &lcMainF..HEntered   WITH &lcTempMain..Entered ,;
                    &lcMainF..Status    WITH &lcTempMain..Status ,;
                    &lcMainF..cDivision WITH &lcTempMain..cDivision ,;
                    &lcMainF..HComplete  WITH &lcTempMain..Complete ,;
                    &lcMainF..NoteFlag  WITH m.NoteFlag ,;
                    &lcMainF..Notes    WITH m.Notes IN (lcMainF)
          ENDIF
          *-- Get the cutting ticket note.
          IF llrPrtMmon .AND. SEEK('O'+m.PO,'NotePad')
            m.NoteFlag = 'T'
            m.Notes    = Notepad.MNotes

            INSERT INTO (lcMainF) FROM MEMVAR
            REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,;
                    &lcMainF..HPattern   WITH &lcTempMain..Pattern ,;
                    &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,;
                    &lcMainF..HEntered   WITH &lcTempMain..Entered ,;
                    &lcMainF..Status    WITH &lcTempMain..Status ,;
                    &lcMainF..cDivision WITH &lcTempMain..cDivision ,;
                    &lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)
          ENDIF
        ENDIF
        IF EMPTY(lcWareCode)
          lcWareCode = cWareCode
          lcStyle    = Style
        ENDIF
      ENDSCAN
      m.cWareCode = lcWareCode
      m.Style     = lcStyle
      m.Dyelot    = CHR(255)
      *-- Print notes for last warehouse.
      IF llRPrtSn .AND. SEEK('G'+&lcTempMain..HdrStyle,'NotePad')
        m.NoteFlag = 'S'
        m.Notes    = Notepad.MNotes
        INSERT INTO (lcMainF) FROM MEMVAR
        REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,;
                 &lcMainF..HPattern   WITH &lcTempMain..Pattern ,;
                 &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,;
                 &lcMainF..HEntered   WITH &lcTempMain..Entered ,;
                 &lcMainF..Status    WITH &lcTempMain..Status ,;
                 &lcMainF..cDivision WITH &lcTempMain..cDivision ,;
                 &lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)
      ENDIF
      IF llrPrtMmon .AND. SEEK('O'+m.PO,'NotePad')
        m.NoteFlag = 'T'
        m.Notes    = Notepad.MNotes
        INSERT INTO (lcMainF) FROM MEMVAR
        REPLACE &lcMainF..cTktType WITH &lcTempMain..CtktType ,;
                &lcMainF..HPattern  WITH &lcTempMain..Pattern ,;
                &lcMainF..HdrStyle WITH &lcTempMain..HdrStyle ,;
                &lcMainF..HEntered  WITH &lcTempMain..Entered ,;
                &lcMainF..Status  WITH &lcTempMain..Status ,;
                &lcMainF..cDivision WITH &lcTempMain..cDivision ,;
                &lcMainF..HComplete WITH &lcTempMain..Complete  IN (lcMainF)
      ENDIF
      go top in (lcMainF)
    ENDIF

  ENDSCAN  && End loop of CUTTKTH file.
      *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
  IF llrPrtCs
    lcCtktBomSlct = "Select CtktBom.CutTkt ,CtktBom.Item,cTktBom.Typ,cTktBom.cCatgTyp,CtktBom.UntQty,CtktBom.UntCost"
    lcCtktBomSlct = lcCtktBomSlct + ",CtktBom.Req_Qty,CtktBom.Issue_Qty,CtktBom.[Desc],CtktBom.DyeLot,CUOMCODE  "
    lcCtktBomSlct = lcCtktBomSlct + "From CtktBom (Index=CtktBom) "
    lcCtktBomSlct = lcCtktBomSlct + " INNER JOIN " + lcSqlPOs + " TmpPO ON CtktBom.CutTkt = TmpPO.PO "
    lcCtktBomSlct = lcCtktBomSlct + " WHERE CtktBom.cImTyp = 'T' and CtktBom.cCatgTyp IN('F','T','S')"
    lnCtktBomResult = loOGScroll.oRDA.SqlRun(lcCtktBomSlct,lcCostF,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnCtktBomResult > 0
      SELECT &lcCostF
      lnBuffering = CURSORGETPROP("Buffering","&lcCostF")
      =CURSORSETPROP("Buffering",3,"&lcCostF")
      INDEX ON CutTkt+Typ+cCatgTyp+Item TAG &lcCostF
      SET ORDER TO TAG &lcCostF
    ENDIF
    LOUOM.SQLRUN("SELECT CUOMCODE,CUOM_V FROM UOM",'TUOM')
    SELECT TUOM
    =CURSORSETPROP("Buffering",3,'TUOM')
    INDEX ON CUOMCODE TAG CODCX
  ENDIF

*!*	  SELECT (lcMainF)
*!*	  SET RELATION TO STYLE INTO FABRIC_x ADDITIVE

  IF llRpPic
    SELECT &lcMainF
    SET RELATION TO 'M'+ &lcMainF..Hdrstyle INTO Objlink ADDITIVE
  ENDIF

ELSE
  lcOldAlias = SELECT(0)
*!*	  SELECT FABRIC_x
*!*	  IF EMPTY(SET("Relation"))
*!*	    SELECT FABRIC_x
*!*	    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
*!*	  ENDIF

  SELECT OBJLINK
  IF EMPTY(SET("Relation"))
     SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE
  ENDIF
  SELECT (lcOldAlias)

ENDIF


IF SEEK('*' + 'LOGO','ObjLink') AND SEEK(OBJLINK.cObject_ID,'Objects')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT mimgpath;
  FROM Objects         ;
  WHERE Objects.cobject_id = lcObj_Id ;
  INTO CURSOR (lcLogoPic)
ENDIF

lcOgPlatForm = 'WINDOWS'
SELECT (lcMainF)
COUNT FOR !EMPTY(po) TO lnRreccount
lcRreccount=ALLTRIM(STR(lnRreccount))
*-- Add this line to check if there is an optional program or not
=lfOptProg()
*-- print useing the variable that holds the FRX name [start]

llNoRec= .T.
lcPrgName  = lcFormNam
llIsAparel = lfIsApparl(@lcPrgName)
IF llIsAparel
  =gfSetDevice()
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  IF llPoDisp
    lcEndTime = TIME()  && Time in which we finish collect data.
    lnInterval = lfCollTime(lcStartTime,lcEndTime)  && Calculate collecting data spent time.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *WAIT WINDOW LANG_MAPMMO_Selected + lcRreccount +LANG_MAPMMO_Records+ ALLTRIM(STR(lnInterval,6,2)) + LANG_MAPMMO_Second NOWAIT
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Selected,oAriaApplication.GetHeaderText("LANG_MAPMMO_Selected",AHEADERFILE)) + lcRreccount +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Records,oAriaApplication.GetHeaderText("LANG_MAPMMO_Records",AHEADERFILE))+ ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Second,oAriaApplication.GetHeaderText("LANG_MAPMMO_Second",AHEADERFILE)) NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[End]


	loOgScroll.lnActivePDFViewerNo  = 1

    DO gfDispRe WITH EVALUATE('lcFormNam')

	IF CallFromExport()
      OpenMainFile()
    ENDIF

  ENDIF
ENDIF

loogScroll.cCROrientation = 'P'

IF llrPrtCs
  GO TOP IN (lcCostF)
ENDIF

lcCStartTime = TIME()
*-- Print the cost sheet if required.
IF llrPrtCs  .AND. !EOF(lcCostF)
  IF TYPE('loPosHdr') <> 'O'
    loPosHdr = CREATEOBJECT('RemoteTable','POSHDR','POSHDR','POSHDR',SET("Datasession"))
  ENDIF
  SELECT (lcCostF)
  IF loOGScroll.llogFltCh
    loFabric.SQLRUN("SELECT STYLE,CITEMFLD1 AS WIDTH,VENDOR FROM ITEM",'Fabric')
    =CURSORSETPROP("Buffering" ,3,'Fabric')
    SELECT Fabric
    INDEX on style TAG Fabric
    loPosHdr.SQLRUN("SELECT STYLE,PO,CSTYTYPE,PATTERN FROM POSHDR WHERE CSTYTYPE='F'",'TPOSHDR')
    =CURSORSETPROP("Buffering" ,3,'TPOSHDR')
    SELECT TPOSHDR
    INDEX on PO  TAG TPOSHDR
  ENDIF
    SELECT (lcCostF)
    SET RELATION to
    SET RELATION TO ITEM INTO Fabric ADDITIVE
    SET RELATION TO CutTkt  INTO TPOSHDR ADDITIVE
    SET RELATION TO CUOMCODE INTO TUOM ADDITIVE

  *-- store the old .FRX name [start]
  lcOldTmp = lcOGTmpForm
  lcOGTmpForm = ""
  lcOldOptProg = lcOptProg
  *-- Add these lines to check if there is custom cost sheet or not
  lcFrxNam = SUBSTR(lcFormNam , 7,2)
  lcFrxNam = 'MFPRCS' + lcFrxNam

  lcOGTmpForm = gfTempName()
  =gfCrtFrm(lcFrxNam ,"",llOGRefForm)

  IF CallFromExportorEmail()
	SetExtraFileName("Second")
  ENDIF

  loOgScroll.lnActivePDFViewerNo  = 2
  IF FILE(gcRepHome + 'MF\' + lcFrxNam + '.FRX')
    llPrnAgan = .T.
    IF llPoDisp
      lcCEndTime = TIME()  && Time in which we finish collect data.
      lnInterval = lfCollTime(lcCStartTime,lcCEndTime)  && Calculate collecting data spent time.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *WAIT WINDOW LANG_MAPMMO_Selected   + lcRreccount +LANG_MAPMMO_Records    + ALLTRIM(STR(lnInterval,6,2)) + LANG_MAPMMO_Second      NOWAIT
      WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Selected,oAriaApplication.GetHeaderText("LANG_MAPMMO_Selected",AHEADERFILE))   + lcRreccount +IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Records,oAriaApplication.GetHeaderText("LANG_MAPMMO_Records",AHEADERFILE))    + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Second,oAriaApplication.GetHeaderText("LANG_MAPMMO_Second",AHEADERFILE))      NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      DO gfDispRe WITH EVALUATE('lcFrxNam')
    ENDIF
  ELSE
    *-- Support printing @,Say Forms
    IF llIsAparel
      =gfSetDevice()
      IF FILE(gcRepHome + 'MF\' + lcFrxNam + '.FXP')
        DO EVAL('lcFrxNam')
        IF !llNoRec
          DO ENDREPORT
        ENDIF
      ENDIF
    ELSE
      IF llPoDisp
        lcCEndTime = TIME()  && Time in which we finish collect data.
        lnInterval = lfCollTime(lcCStartTime,lcCEndTime)  && Calculate collecting data spent time.
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *WAIT WINDOW LANG_MAPMMO_Selected   + lcRreccount  + LANG_MAPMMO_Records    + ALLTRIM(STR(lnInterval,6,2)) + LANG_MAPMMO_Second      NOWAIT
        WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Selected,oAriaApplication.GetHeaderText("LANG_MAPMMO_Selected",AHEADERFILE))   + lcRreccount  + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Records,oAriaApplication.GetHeaderText("LANG_MAPMMO_Records",AHEADERFILE))    + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Second,oAriaApplication.GetHeaderText("LANG_MAPMMO_Second",AHEADERFILE))      NOWAIT
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        DO gfDispRe WITH 'MFPRCSA'
      ENDIF
    ENDIF
  ENDIF

  IF CallFromExport()
    OpenExtraFile("Second")
  ENDIF

  IF CallFromEmail()
    AddExtraAttachment("Second")
  ENDIF
  *-- retrive the old .FRX name [start]
  lcOGTmpForm = lcOldTmp
  lcOptProg   = lcOldOptProg
ENDIF
*-- If printing the BOM restore the order
IF llrPrtCs
  loFabric.SetOrder('CSTYLE')
ENDIF
*!
*!*************************************************************
*! Name      : lfGetCodes
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : MFPRCTA.FRX
*!*************************************************************
*! Calls       : gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfGetCodes
*-- Print Div Long Name If Exist Instead of Company NAme
= gfRltFld(cDivision,@laDivLNam,'CDIVISION')
llEndGroup = .F.
*-- Get the suitable title due to status to be printed on the .FRX
DO CASE
  CASE STATUS='O'
    *- lcSt = 'OPEN'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Open
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Open,oAriaApplication.GetHeaderText("LANG_MAPMMO_Open",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='H'
    *- lcSt = 'HOLD'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Hold
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Hold,oAriaApplication.GetHeaderText("LANG_MAPMMO_Hold",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='X'
    *- lcSt = 'CANCELLED'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Cancelled
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Cancelled,oAriaApplication.GetHeaderText("LANG_MAPMMO_Cancelled",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='S'
    *- lcSt = 'CLOSED'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Closed
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Closed,oAriaApplication.GetHeaderText("LANG_MAPMMO_Closed",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='A'
    *- lcSt = 'ACTUAL'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Actual
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Actual,oAriaApplication.GetHeaderText("LANG_MAPMMO_Actual",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='C'
    *- lcSt = 'COMPLETED'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Complete
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Complete,oAriaApplication.GetHeaderText("LANG_MAPMMO_Complete",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE STATUS='B'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcSt = LANG_MAPMMO_Bid
    lcSt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Bid,oAriaApplication.GetHeaderText("LANG_MAPMMO_Bid",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE
*-- Get the warehouse addresses
IF TYPE('loWareHous') <> 'O'
  loWareHous = CreateObject('RemoteTable','WAREHOUS','WAREHOUS','WAREHOUS',SET("Datasession"))
ENDIF
IF loWareHous.SEEK(ALLTRIM(cWareCode))
  lcShpName   = WAREHOUS.cDesc
  laShpAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laShpAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laShpAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laShpAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laShpAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  DO lfShiftArr WITH laShpAdr
ELSE
  STORE SPACE(0) TO lcShpName, laShpAdr
ENDIF

RETURN ''
*!
*!*************************************************************
*! Name        : lfShiftArr
*! Developer   : AHMED MOHAMMED IBRAHIM (AMM)
*! Date        : 07/15/1998
*! Purpose     : Function to Pack the passed array
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : DO lfShiftArr WITH laCompAdd
*!*************************************************************
*!
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
*!
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To state that if we would print the word "Continued"
*!             and to initialize some variables.
*!*************************************************************
*! Called from : MFPRCTA.FRX, MFPRTCSA,FRX
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfEndGroup
* Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.
RETURN ''

*!*************************************************************
*! Name      : lfSum
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To sum size quantities for each warehouse.
*!*************************************************************
*! Called from : MFPRCTA.FRX
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfSum
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT POSLN
loPosln.seek('P'+'F'+&lcMainf..PO)
laSum = 0
*-- Sum size quantities for the budget lines for each warehouse.
SUM INT(Qty1),INT(Qty2),INT(Qty3),INT(Qty4),INT(Qty5),INT(Qty6),INT(Qty7),INT(Qty8),INT(TotQty);
    FOR cWareCode+TranCD=PADR(ALLTRIM(&lcMainF..cWareCode),6)+'1' TO ARRAY laSum
SELECT (lnAlias)
RETURN ''
*!
*!*************************************************************
*! Name      : lfGetTitle
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To get the cost element title.
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfGetTitle

PRIVATE lcSetEx
lcCostItm = ''
lcSetEx = SET('EXACT')
SET EXACT ON
lnAlias = SELECT(0)
SELECT (lcCostF)
IF ASCAN(laCost,"M_CTTYPE" + Typ) # 0 AND laCost[ASUBSCRIPT(laCost,ASCAN(laCost,"M_CTTYPE" + Typ) ,1),2] = cCatgTyp
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,"M_CTTYPE" + Typ) ,1)+7,2]
ENDIF

lcHead1   = ''
lcHead2   = ''
IF cCatgTyp = 'T'
  *- lcHead1   = '                  DATE                 QTY      DATE     DIRECT'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHead1   = LANG_MAPMMO_lcHead1
  lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_lcHead1,oAriaApplication.GetHeaderText("LANG_MAPMMO_lcHead1",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *- lcHead2   = 'VENDOR  ORDERD PO #  ORDRD RECVD. SHIPD.'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcHead2   = LANG_MAPMMO_lcHead2
  lcHead2   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_lcHead2,oAriaApplication.GetHeaderText("LANG_MAPMMO_lcHead2",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  IF cCatgTyp = 'F'
    *- lcHead1   = '[-------------------- PULLED -------------------]  TOTAL'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
   *lcHead1   = LANG_MAPMMO_lcHead11
   lcHead1   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_lcHead11,oAriaApplication.GetHeaderText("LANG_MAPMMO_lcHead11",AHEADERFILE))
   *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
ENDIF

SET EXACT &lcSetEx
SELECT (lnAlias)
RETURN ''
*!
*!*************************************************************
*! Name      : lfStGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To Initialize the variable llEndGroup
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfStGroup
llEndGroup = .F.
RETURN ''
*!
*!*************************************************************
*! Name      : lfvOMsg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.
*!
*!*************************************************************
*! Name      : lfvPrtCs
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvPrtCs
*-- Get the position of the print cost setting in the array to enable or
*-- disable it.
LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LLRPCOST'),1)
LAOGOBJCNT[LNPOS] = LLRPRTCS
= LFOGSHOWGET('LLRPCOST')
*!
*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : When function of the option grid.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfwOGWhen
DECLARE laRpSource[7] , laRpTarget[1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_MAPMMO_Open2      TO laRpSource[1]
*STORE LANG_MAPMMO_Hold2      TO laRpSource[2]
*STORE LANG_MAPMMO_Actual2    TO laRpSource[3]
*STORE LANG_MAPMMO_Closed2    TO laRpSource[4]
*STORE LANG_MAPMMO_Cancelled2 TO laRpSource[5]
*STORE LANG_MAPMMO_Complete2  TO laRpSource[6]
*STORE LANG_MAPMMO_Bid        TO laRpSource[7]
*STORE LANG_MAPMMO_Open2      TO laRpTarget[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Open2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Open2",AHEADERFILE))      TO laRpSource[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Hold2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Hold2",AHEADERFILE))      TO laRpSource[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Actual2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Actual2",AHEADERFILE))    TO laRpSource[3]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Closed2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Closed2",AHEADERFILE))    TO laRpSource[4]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Cancelled2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Cancelled2",AHEADERFILE)) TO laRpSource[5]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Complete2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Complete2",AHEADERFILE))  TO laRpSource[6]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Bid,oAriaApplication.GetHeaderText("LANG_MAPMMO_Bid",AHEADERFILE))        TO laRpSource[7]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Open2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Open2",AHEADERFILE))      TO laRpTarget[1]
*N000682,1 11/20/2012 MMT Globlization changes[End]



lcStatus = 'O'
loPosLn  = CreateObject('RemoteTable','POSLN','POSLN','POSLN',SET("Datasession"))
*!
*!*************************************************************
*! Name      : lfGetOp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : to be done on the starting of page, used in lot cost
*!             sheet form
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfGetOp

*-- set the variables to display on the .FRX
lcStyMaj = TPOSHDR.style
lcPattrn = TPOSHDR.pattern
*!
*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get the MFG order title.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*!
FUNCTION lfGetTit
PRIVATE lcTit
lcTit  = ALLTRIM(gfGetMemvar('M_MMOLNLBL',oAriaApplication.ActiveCompanyID))
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+' #')
RETURN lcTit
*!
*!*************************************************************
*! Name      : lfsrStyle
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Rise change order of style file in range browse screen.
*! Reference : C101931,1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrStyle
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SET ORDER TO Cstyle IN STYLE
    GOTO TOP
  CASE lcParm = 'R'
    SET ORDER TO Style IN STYLE
    GOTO TOP
ENDCASE
*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : ABDOU ELGENDI        - (ABD)
*! Date      : 08/24/2000
*! Purpose   : Evaluate Status expression.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,LANG_MAPMMO_Select,.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Select,oAriaApplication.GetHeaderText("LANG_MAPMMO_Select",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcStatus = ' '

*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcStatus = lcStatus + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Open2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Open2",AHEADERFILE))      ,'O',;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Hold2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Hold2",AHEADERFILE))      ,'H',;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Actual2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Actual2",AHEADERFILE))    ,'A',;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Closed2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Closed2",AHEADERFILE))    ,'S',;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Bid,oAriaApplication.GetHeaderText("LANG_MAPMMO_Bid",AHEADERFILE))        ,'B',;
                          IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Cancelled2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Cancelled2",AHEADERFILE)) ,'X',;
					  	IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAPMMO_Complete2,oAriaApplication.GetHeaderText("LANG_MAPMMO_Complete2",AHEADERFILE))  ,'C','')))))))

  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
lcStatus = IIF(EMPTY(lcStatus),'OHASXCB',ALLTRIM(lcStatus))
*-- End of lfvOStatus.
*!
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Albert Raif (ALB)
*! Date      : 11/24/2002
*! Purpose   : Get Color Length and Non major/free Length
*! Reference : C102730
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
*!
FUNCTION lfEvalSegs

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM',.F.,'0002')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
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
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.
*!
*!*****************************************************************************************
*! Name      : RefreshOptMsg
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 11:18:46 PM
*! Purpose   : Refresh the optional message area
*!*****************************************************************************************
*!
FUNCTION RefreshOptMsg
  IF EMPTY(lcOMsg1) .AND. EMPTY(lcOMsg2) .AND. EMPTY(lcOMsg3)
    RETURN ""
  ELSE
    RETURN ALLTRIM(lcOMsg1) + IIF(EMPTY(lcOMsg2),"",", ") +;
           ALLTRIM(lcOMsg2) + IIF(EMPTY(lcOMsg3),"",", ") +;
           ALLTRIM(lcOMsg3)
  ENDIF
ENDFUNC
*!
*!************************************************************
*! Name      : RefreshStatus
*: Developer : Heba Fathi (HFK)
*: Date      : 08/17/2004
*! Purpose   : function to Set the index for the SQL files
*!************************************************************
*! Parameters: None
*!************************************************************
*! Returns   : None
*!************************************************************
*!
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
*!
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Heba Fathi (HFK)
*! Date      : 07/06/2005
*! Purpose   : create temp file
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*
FUNCTION lfCrtTmp

SELECT POSLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+13,18]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cTktType'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HPattern'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HdrStyle'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HEntered'
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HComplete'
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Desc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Desc1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 60
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'OComplete'
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'NoteFlag'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Notes'
laFileStru[lnFileStru,2] = 'M'
laFileStru[lnFileStru,3] = 0
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cDivision'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Status'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'UOMBUY'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 3
laFileStru[lnFileStru,4] = 0


FOR  lnLen = 7 TO 18
  FOR lnCount = 0 TO 13
    STORE SPACE(1) TO laFileStru[lnFileStru - lnCount,lnLen]
  ENDFOR
ENDFOR

gfCrtTmp(lcMainF,@laFileStru,"PO+cWareCode+Style+Dyelot+NoteFlag",lcMainF,.F.)

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*!
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.


*****************************************************************************************************************************************
FUNCTION CallFromExport
RETURN TYPE("Export") = 'O' .AND. TYPE('Export.Name') = 'C'


*****************************************************************************************************************************************
FUNCTION CallFromEmail
RETURN TYPE('_SCREEN.ActiveForm') = 'O'  .AND. UPPER(_SCREEN.ActiveForm.Name) = UPPER("Sendmail")


*****************************************************************************************************************************************
FUNCTION CallFromExportorEmail
RETURN (TYPE("Export") = 'O' .AND. TYPE('Export.Name') = 'C') .OR. ;
       (TYPE('_SCREEN.ActiveForm') = 'O'  .AND. UPPER(_SCREEN.ActiveForm.Name) = UPPER("Sendmail"))

*****************************************************************************************************************************************
FUNCTION SetGlobals
PUBLIC gcExportType, gcExportFileName
IF TYPE('Export') = 'O' .AND. TYPE('Export.Name') = 'C'
  gcExportType = IIF(ALLTRIM(Export.txtPath.Text) == oAriaApplication.gcOutFile, 'E', 'P')
  Export.StopPreview = .T.
ENDIF
gcExportFileName = oAriaApplication.gcOutFile

*****************************************************************************************************************************************
FUNCTION GetMainFileName
IF TYPE('Export') = 'O' .AND. TYPE('Export.Name') = 'C' .AND. !EMPTY(ALLTRIM(Export.txtPath.Text))
  RETURN ALLTRIM(Export.txtPath.Text)
ELSE
  RETURN gcExportFileName
ENDIF

*****************************************************************************************************************************************
FUNCTION GetExtraFileName
LPARAMETERS Extension

LOCAL lcMainFileName
lcMainFileName = GetMainFileName()

RETURN SUBSTR(lcMainFileName, 1, LEN(lcMainFileName) - 4) + Extension + SUBSTR(lcMainFileName, LEN(lcMainFileName) - 3)


*****************************************************************************************************************************************
FUNCTION SetExtraFileName
LPARAMETERS Extension

oAriaApplication.gcOutFile = GetExtraFileName(Extension)


*****************************************************************************************************************************************
FUNCTION OpenMainFile

*-- Check if the user select preview
IF gcExportType = 'P'
  LOCAL loRun
  IF FILE(GetMainFileName())
    loRun = CreateObject("WScript.Shell")
    loRun.Run(GetMainFileName(), 3)
    loRun = NULL
  ENDIF
ENDIF


*****************************************************************************************************************************************
FUNCTION OpenExtraFile
LPARAMETERS Extension

IF gcExportType = 'P'
  LOCAL loRun
  IF FILE(GetExtraFileName(Extension))
    loRun = CreateObject("WScript.Shell")
    loRun.Run(GetExtraFileName(Extension), 3)
    loRun = NULL
  ENDIF
ENDIF


*****************************************************************************************************************************************
FUNCTION AddExtraAttachment
LPARAMETERS Extension

IF TYPE('_SCREEN.ActiveForm.laExtraAttach') = 'L'
  DIMENSION _SCREEN.ActiveForm.laExtraAttach[1]
ELSE
  DIMENSION _SCREEN.ActiveForm.laExtraAttach[ALEN(_SCREEN.ActiveForm.laExtraAttach, 1) + 1]
ENDIF

_SCREEN.ActiveForm.laExtraAttach[ALEN(_SCREEN.ActiveForm.laExtraAttach, 1)] = GetExtraFileName(Extension)

FUNCTION lfGetClr
lcAlias = ALIAS()
PRIVATE lcClrLD
*B610112,1 MMT 10/14/2012 Modify Material screen to show foreign costs in case domestic Fabrics[Start]
lcClrLD  = ""
*B610112,1 MMT 10/14/2012 Modify Material screen to show foreign costs in case domestic Fabrics[End]
SELECT Codes
SET ORDER TO TAG Codes
IF SEEK('N'+SUBSTR(&lcMainF..Style,lnNonMajSt,lnColorLen) +'Y'+'COLOR')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+SUBSTR(&lcMainF..Style,lnNonMajSt,lnColorLen)+'Y'+'COLOR'
    IF crltd_nam = 'CLRLNAME'
      lcClrLD = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
RETURN lcClrLD
