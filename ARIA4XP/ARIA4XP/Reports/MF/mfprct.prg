*:**********************************************************************
*: Program file         : MFPRCT
*: Program description  : Print CUTTING TICKETS
*: Module               : Manufacturing (MF)
*: Developer            : Heba Fathi (HFK)
*: Tracking Job Number  : N040028
*: Date                 : 07/06/2005
*:**********************************************************************
*: Calls: None
*:**********************************************************************
*: Passed Parameters: None
*:**********************************************************************
*: Example: DO MFPRTCT
*:**********************************************************************
*: Modification :Make this report syncronized with MFPRCSA report[AYM]
*: B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material ,T20061129.0038
*: B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T. (T20061218.0011)
*: C200748,1 WLD Restore paper orienatation to be 'Portrait'- Replace command not correct
*: B608864,1 WAM 05/18/2009 Fix problem that the BOM doesn't show in the second preview after export to PDF
*: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[T20090831.0015]
*: B609178,1 MMT Error while preview of BOM from Cut tkt form on SAAS[T20100226.0008]
*! B609333,1 MMT 07/06/2010 Fix bug of repeated SO lines with Each CT Line[T20100609.0024]
*:********************************************************************** 

#INCLUDE R:\ARIA4XP\REPORTS\MF\MFPRCT.H

*B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
DECLARE INTEGER FindWindow IN user32 STRING lpClassName, STRING lpWindowName
DECLARE INTEGER GetParent IN user32 INTEGER hwnd
DECLARE INTEGER GetFocus IN user32

*-- Mahmoud Said Start
*-- 
IF CallFromExportorEmail()
  SetGlobals()  
ENDIF
*-- Mahmoud Said End
*B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]

lcStartTime = TIME()
*- property to define paper orienatation = 'Portrait'
loogScroll.cCROrientation = 'P'
STORE .F. TO llEndGroup
llPoDisp = .T.
lnMajor  = LEN(gfItemMask('PM'))
lnNMajor = LEN(gfItemMask('PN'))
STORE .F. TO llLogo
 
*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material [Start]
*DIMENSION laCost[10,2]
DIMENSION laCost[14,2]
*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material [End]

laCost[1,1]  = 'M_CMTYPE1 '
laCost[2,1]  = 'M_CMTYPE2 '
laCost[3,1]  = 'M_CMTYPE3 '
laCost[4,1]  = 'M_CMTYPE4 '
laCost[5,1]  = 'M_CMTYPE5 '

*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material [Start]
*!*	laCost[6,1]  = 'M_CMSLBL1 '
*!*	laCost[7,1]  = 'M_CMSLBL2 '
*!*	laCost[8,1]  = 'M_CMSLBL3 '
*!*	laCost[9,1]  = 'M_CMSLBL4 '
*!*	laCost[10,1] = 'M_CMSLBL5 '

laCost[6,1]   = 'M_CMTYPE6 '
laCost[7,1]   = 'M_CMTYPE7 '
laCost[8,1]   = 'M_CMSLBL1 '
laCost[9,1]   = 'M_CMSLBL2 '
laCost[10,1]  = 'M_CMSLBL3 '
laCost[11,1]  = 'M_CMSLBL4 '
laCost[12,1]  = 'M_CMSLBL5 '
laCost[13,1]  = 'M_CMSLBL6 '
laCost[14,1]  = 'M_CMSLBL7 '

*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material[End] 




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
  laCompAdd[6]    = LANG_MfPrCT_Phone + TRANSFORM(lcCompPhon , '@R ' + lcPhonPict)    
  DO lfShiftArr WITH laCompAdd
ENDIF 
DIMENSION laShpAdr[5]
DIMENSION laSum[9]                     && Array to sum size quantities

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
*!*	  loFabric.SetOrder("STYLE")  
  *-- Create array to hold the cost element titles.
  IF TYPE('loUom') <> 'O'
    loUom = CREATEOBJECT('RemoteTable','UOM','UOMCODE','UOM',SET("Datasession"))
  ENDIF 
 
  IF TYPE('loCtktBom') <> 'O'
    loCtktBom = CREATEOBJECT('RemoteTable','CTKTBOM','CTKTBOM','CTKTBOM',SET("Datasession"))
  ENDIF 
ENDIF

*-- If we will print the cost sheet
lcBOMTit  = ALLTRIM(gfGetMemvar('M_PRDLNLBL',oAriaApplication.ActiveCompanyID))
lcBOMTit  = IIF(RIGHT(lcBOMTit,1) ='#', lcBOMTit,lcBOMTit+'#')

IF loOGScroll.llOgFltCh
  lcCostF   = loOGScroll.gfTempName()
  *Variable to indicate if Extended Size Scale .
  DIMENSION laVenAdr[5]
  STORE SPACE(0) TO lcVenName

  *-- Temporary file of the cutting ticket report.
  lcMainF = loOGScroll.gfTempName()

  = lfCrtTmp()
  *-Select statement for Cutpick file
  lcCutPickSel = "Select CtktNo,TranCd,Style,CAST(Qty1 As Int) as Qty1,CAST(Qty2 As Int) as Qty2,CAST(Qty3 As Int) as Qty3,CAST(Qty4 As Int) as Qty4,CAST(Qty5 As Int) as Qty5,CAST(Qty6 As Int) as Qty6,CAST(Qty7 As Int) as Qty7,"
  *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[Start]
  lcCutPickSel = lcCutPickSel + "CTKTLINENO,"
  *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[End]
  lcCutPickSel = lcCutPickSel + "CAST(Qty8 As Int) as Qty8,CAST(TotQty As Int) as TotQty,[Order] From CutPick (index=CutPick)"
  lnCPResult = loOGScroll.oRDA.SqlRun(lcCutPickSel,"CUTPICKF",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))

  *-select statement for Header
  lcHdrStmnt = "Select Distinct(poshdr.po) as PO,poshdr.ctkttype,poshdr.style as hdrstyle,poshdr.pattern ,poshdr.entered,poshdr.cdivision"
  lcHdrStmnt = lcHdrStmnt + ",poshdr.status,poshdr.complete from poshdr(index=poshdr) inner join posln (index=posln) "
  lcHdrStmnt = lcHdrStmnt + "on poshdr.cbusdocu+poshdr.cstytype+poshdr.po = posln.cbusdocu+posln.cstytype+posln.po"
  lcHdrStmnt = lcHdrStmnt + " where posln.trancd = 1 and poshdr.cbusdocu = 'P' and poshdr.cstytype = 'U'"
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
  IF lnMainResult > 0 .AND. lnCPResult > 0
    SELECT &lcTempMain
    lnBuffering = CURSORGETPROP("Buffering","&lcTempMain")
    =CURSORSETPROP("Buffering",3,"&lcTempMain")
    INDEX ON PO TAG &lcTempMain
    SET ORDER TO TAG &lcTempMain
    *-make a sql file carrying selected POs
    lcSqlPOs = loOgScroll.gfSQLTempName('','PO C(6)',lcTempMain,'PO')
    SELECT CutPICKF
    lnBuffering = CURSORGETPROP("Buffering","CutPICKF")
    =CURSORSETPROP("Buffering",3,"CutPICKF")
    SELECT CutPICKF
    *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[Start]
    *INDEX on TranCd+CtktNo+Style TAG CUTPICKF
    INDEX on TranCd+CtktNo+Style+CTKTLINENO TAG CUTPICKF
    *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[End]
    SET ORDER TO TAG CutPICKF
  ENDIF 

  lcStyTitle = gfItemMask('HI')
  *-- Set necessary relations
  
  SELECT OBJLINK
  SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

  SELECT STYLE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

  SELECT CUTPICKF
  SET RELATION TO 'O'+ CutpickF.order INTO Ordhdr ADDITIVE

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
    IF loPosLn.SEEK('P'+'U'+lcPo)
      SELECT POSLN
      SCAN FOR posln.TranCd = '1'
        SCATTER MEMVAR
        m.NoteFlag = 'N'
        m.Notes    = ''
        INSERT INTO (lcMainF) FROM MEMVAR        
        *C200748,1 WLD Replace command not correct 03/12/2007 [Begin]
       * REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType;
       *         &lcMainF..HPattern   WITH &lcTempMain..Pattern;
       *         &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle;
       *         &lcMainF..HEntered   WITH &lcTempMain..Entered;
       *         &lcMainF..Status    WITH &lcTempMain..Status;              
       *         &lcMainF..cDivision WITH &lcTempMain..cDivision ;              
       *         &lcMainF..HComplete  WITH &lcTempMain..Complete
     REPLACE &lcMainF..cTktType    WITH &lcTempMain..CtktType ,; 
        		&lcMainF..HPattern   WITH &lcTempMain..Pattern  ,; 
				&lcMainF..HdrStyle   WITH &lcTempMain..HdrStyle ,; 
				&lcMainF..HEntered   WITH &lcTempMain..Entered  ,; 
				&lcMainF..Status     WITH &lcTempMain..Status   ,; 
				&lcMainF..cDivision  WITH &lcTempMain..cDivision ,; 
				&lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)

       *C200748,1 WLD Replace command not correct 03/12/2007 [End]
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
          IF llRPrtSn .AND. SEEK('F'+&lcTempMain..HdrStyle,'NotePad')
            m.NoteFlag = 'S'
            m.Notes    = Notepad.MNotes
            *C200748,1 WLD Replace command not correct 03/12/2007 [Begin]
            *REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType;
            *        &lcMainF..HPattern   WITH &lcTempMain..Pattern;
            *        &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle;
            *        &lcMainF..HEntered   WITH &lcTempMain..Entered;
            *        &lcMainF..Status    WITH &lcTempMain..Status;                                
            *        &lcMainF..cDivision WITH &lcTempMain..cDivision ;                                
            *        &lcMainF..HComplete  WITH &lcTempMain..Complete,;
            *        &lcMainF..NoteFlag  WITH m.NoteFlag;
            *        &lcMainF..Notes    WITH m.Notes
            REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
                    &lcMainF..HPattern   WITH &lcTempMain..Pattern ,; 
                    &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
                    &lcMainF..HEntered   WITH &lcTempMain..Entered ,; 
                    &lcMainF..Status    WITH &lcTempMain..Status ,; 
                    &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
                    &lcMainF..HComplete  WITH &lcTempMain..Complete ,; 
                    &lcMainF..NoteFlag  WITH m.NoteFlag ,; 
                    &lcMainF..Notes    WITH m.Notes IN (lcMainF)
            *C200748,1 WLD Replace command not correct 03/12/2007 [End]                    
          ENDIF
          *-- Get the cutting ticket note.
          IF llRPrtCtn .AND. SEEK('I'+m.PO,'NotePad')
            m.NoteFlag = 'T'
            m.Notes    = Notepad.MNotes
            INSERT INTO (lcMainF) FROM MEMVAR            
            *C200748,1 WLD Replace command not correct 03/12/2007 [Begin]
            *REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType;
            *        &lcMainF..HPattern   WITH &lcTempMain..Pattern;
            *        &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle;
            *        &lcMainF..HEntered   WITH &lcTempMain..Entered;
            *        &lcMainF..Status    WITH &lcTempMain..Status;                                
            *        &lcMainF..cDivision WITH &lcTempMain..cDivision ;                                
            *        &lcMainF..HComplete  WITH &lcTempMain..Complete
            REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
                    &lcMainF..HPattern   WITH &lcTempMain..Pattern ,; 
                    &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
                    &lcMainF..HEntered   WITH &lcTempMain..Entered ,; 
                    &lcMainF..Status    WITH &lcTempMain..Status ,; 
                    &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
                    &lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)
            *C200748,1 WLD Replace command not correct 03/12/2007 [End]                    
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
      IF llRPrtSn .AND. SEEK('F'+&lcTempMain..HdrStyle,'NotePad')
        m.NoteFlag = 'S'
        m.Notes    = Notepad.MNotes
        INSERT INTO (lcMainF) FROM MEMVAR        
        *C200748,1 WLD Replace command not correct 03/12/2007 [Begin]
        *REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ;
        *         &lcMainF..HPattern   WITH &lcTempMain..Pattern ;
        *         &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ;
        *         &lcMainF..HEntered   WITH &lcTempMain..Entered ;
        *         &lcMainF..Status    WITH &lcTempMain..Status;                            
        *         &lcMainF..cDivision WITH &lcTempMain..cDivision ;                            
        *         &lcMainF..HComplete  WITH &lcTempMain..Complete
        REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
                 &lcMainF..HPattern   WITH &lcTempMain..Pattern ,; 
                 &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
                 &lcMainF..HEntered   WITH &lcTempMain..Entered ,; 
                 &lcMainF..Status    WITH &lcTempMain..Status ,; 
                 &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
                 &lcMainF..HComplete  WITH &lcTempMain..Complete IN (lcMainF)
       *C200748,1 WLD Replace command not correct 03/12/2007 [End]                
      ENDIF
      IF llRPrtCtn .AND. SEEK('I'+m.PO,'NotePad')
        m.NoteFlag = 'T'
        m.Notes    = Notepad.MNotes
        INSERT INTO (lcMainF) FROM MEMVAR        
        *C200748,1 WLD Replace command not correct 03/12/2007 [Begin]
        *REPLACE &lcMainF..cTktType WITH &lcTempMain..CtktType;
        *        &lcMainF..HPattern  WITH &lcTempMain..Pattern;
        *        &lcMainF..HdrStyle WITH &lcTempMain..HdrStyle;
        *        &lcMainF..HEntered  WITH &lcTempMain..Entered;
        *        &lcMainF..Status  WITH &lcTempMain..Status;                            
        *        &lcMainF..cDivision WITH &lcTempMain..cDivision ;                            
        *        &lcMainF..HComplete WITH &lcTempMain..Complete
        REPLACE &lcMainF..cTktType WITH &lcTempMain..CtktType ,; 
                &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
                &lcMainF..HdrStyle WITH &lcTempMain..HdrStyle ,; 
                &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
                &lcMainF..Status  WITH &lcTempMain..Status ,; 
                &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
                &lcMainF..HComplete WITH &lcTempMain..Complete  IN (lcMainF)
        *C200748,1 WLD Replace command not correct 03/12/2007 [End]                
      ENDIF
      go top in (lcMainF)
    ENDIF

  ENDSCAN  && End loop of CUTTKTH file.
      *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
  IF llrPrtCs 
    lcCtktBomSlct = "Select CtktBom.CutTkt ,CtktBom.Item,cTktBom.Typ,cTktBom.cCatgTyp,CtktBom.UntQty,CtktBom.UntCost"
*!*	    lcCtktBomSlct = lcCtktBomSlct + ",CtktBom.Req_Qty,CtktBom.Issue_Qty,CtktBom.[Desc],CtktBom.DyeLot,Uom.cUom_V as UOM "
    lcCtktBomSlct = lcCtktBomSlct + ",CtktBom.Req_Qty,CtktBom.Issue_Qty,CtktBom.[Desc],CtktBom.DyeLot,CUOMCODE  "
*!*	    lcCtktBomSlct = lcCtktBomSlct + "From CtktBom (Index=CtktBom) Inner Join Uom (Index=UomCode) on CtktBom.cUomCode = Uom.cUomCode "
    lcCtktBomSlct = lcCtktBomSlct + "From CtktBom (Index=CtktBom) "
    lcCtktBomSlct = lcCtktBomSlct + " INNER JOIN " + lcSqlPOs + " TmpPO ON CtktBom.CutTkt = TmpPO.PO "
    lcCtktBomSlct = lcCtktBomSlct + " WHERE CtktBom.cImTyp = 'M' and CtktBom.cCatgTyp IN('F','T','S')"
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
  
  SELECT (lcMainF)
  *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[Start]
  *SET RELATION TO "1"+PO+IIF(&lcMainF..NoteFlag='N',style,SPACE(20)) INTO CutpickF ADDITIVE
  SET RELATION TO "1"+PO+IIF(&lcMainF..NoteFlag='N',style,SPACE(20))+STR(LINENO ,6) INTO CutpickF ADDITIVE
  *! B609333,1 MMT Fix bug of repeated SO lines with Each CT Line[End]
  SET RELATION TO STYLE INTO STYLE ADDITIVE  

  IF llRPrtAlo
    SET SKIP TO CUTPICKF
  ENDIF
  *C131670,1 MMT 04/06/2006 Convert Custom form [Start]
  *  lcLogoPic = loOGScroll.gfTempName()
  *C131670,1 MMT 04/06/2006 Convert Custom form [End]
  IF llRpPic
    SELECT &lcMainF
    SET RELATION TO 'S'+ &lcMainF..Hdrstyle INTO Objlink ADDITIVE
  ENDIF  
  
*B607895,1 MMT 01/10/2007 fix bug of Wrong printing for the C/T [Start]
ELSE
  lcOldAlias = SELECT(0)
  SELECT Style
  IF EMPTY(SET("Relation"))
    SELECT STYLE
    SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  ENDIF 

  SELECT OBJLINK
  IF EMPTY(SET("Relation"))
     SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE
  ENDIF 
  SELECT (lcOldAlias)
*B607895,1 MMT 01/10/2007 fix bug of Wrong printing for the C/T [End]

ENDIF 


IF SEEK('*' + 'LOGO','ObjLink') AND SEEK(OBJLINK.cObject_ID,'Objects')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  *: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[Start]
*!*	  SELECT gobject;
*!*	  FROM Objects         ;
*!*	  WHERE Objects.cobject_id = lcObj_Id ;
*!*	  INTO CURSOR (lcLogoPic)
  SELECT mimgpath;
  FROM Objects         ;
  WHERE Objects.cobject_id = lcObj_Id ;
  INTO CURSOR (lcLogoPic)
  *: B609014,1 MMT 09/23/2009 Modify Report to use Picture path instead of General Field[End]
ENDIF

lcOgPlatForm = 'WINDOWS'
SELECT (lcMainF)
COUNT FOR !EMPTY(po) TO lnRreccount
lcRreccount=ALLTRIM(STR(lnRreccount))
*lcRreccount=ALLTRIM(STR(RECCOUNT(lcMainF)))
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
    WAIT WINDOW LANG_MfPrCT_Selected + lcRreccount + LANG_MfPrCT_Records + ALLTRIM(STR(lnInterval,6,2)) + LANG_MfPrCT_Second NOWAIT
    
    *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
    *-- Mahmoud Said Start
	loOgScroll.lnActivePDFViewerNo  = 1
	*-- Mahmoud Said End
    *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]
    
    DO gfDispRe WITH EVALUATE('lcFormNam')
    
    *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
    *-- Mahmoud Said Start
	*-- 
	IF CallFromExport()
      OpenMainFile()
    ENDIF
    *-- Mahmoud Said End
    *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]
  ENDIF
ENDIF

*C200748,1 WLD Restore paper orienatation to be 'Portrait' [Begin]
loogScroll.cCROrientation = 'P'
*C200748,1 WLD [End]

*B608864,1 WAM 05/18/2009 Move table pointer to top of the file
IF llrPrtCs
  GO TOP IN (lcCostF)
ENDIF
*B608864,1 WAM 05/18/2009 (End)

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
    loPosHdr.SQLRUN("SELECT STYLE,PO,CSTYTYPE,PATTERN FROM POSHDR WHERE CSTYTYPE='U'",'TPOSHDR')  
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
  
  *: B609178,1 MMT Error while preview of BOM from Cut tkt form on SAAS[Start]
  lcOGTmpForm = gfTempName()
  =gfCrtFrm(lcFrxNam ,"",llOGRefForm)  
  *: B609178,1 MMT Error while preview of BOM from Cut tkt form on SAAS[End]  
  
  *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
  *-- Mahmoud Said Start
  IF CallFromExportorEmail()
  	SetExtraFileName("Second")
  ENDIF

  loOgScroll.lnActivePDFViewerNo  = 2
  *-- Mahmoud Said End
  *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]

  IF FILE(gcRepHome + 'MF\' + lcFrxNam + '.FRX')
    llPrnAgan = .T.
    IF llPoDisp
      lcCEndTime = TIME()  && Time in which we finish collect data.
      lnInterval = lfCollTime(lcCStartTime,lcCEndTime)  && Calculate collecting data spent time.
      WAIT WINDOW LANG_MfPrCT_Selected + lcRreccount + LANG_MfPrCT_Records + ALLTRIM(STR(lnInterval,6,2)) + LANG_MfPrCT_Second NOWAIT
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
        WAIT WINDOW LANG_MfPrCT_Selected + lcRreccount  + LANG_MfPrCT_Records + ALLTRIM(STR(lnInterval,6,2)) + LANG_MfPrCT_Second NOWAIT
        DO gfDispRe WITH 'MFPRCSA'
      ENDIF
    ENDIF
  ENDIF
  
  *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
    *-- Mahmoud Said Start
  *-- 
  IF CallFromExport()
    OpenExtraFile("Second")
  ENDIF
  
  IF CallFromEmail()
    AddExtraAttachment("Second")
  ENDIF
  *-- Mahmoud Said End

  *B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]
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
    lcSt = LANG_MfPrCT_Open
  CASE STATUS='H'
    *- lcSt = 'HOLD'
    lcSt = LANG_MfPrCT_Hold
  CASE STATUS='X'
    *- lcSt = 'CANCELLED'
    lcSt = LANG_MfPrCT_Cancelled    
  CASE STATUS='S'
    *- lcSt = 'CLOSED'
    lcSt = LANG_MfPrCT_Closed
  CASE STATUS='A'
    *- lcSt = 'ACTUAL'
    lcSt = LANG_MfPrCT_Actual
  CASE STATUS='C'
    *- lcSt = 'COMPLETED'
    lcSt = LANG_MfPrCT_Complete 
  CASE STATUS='B'
    lcSt = LANG_MfPrCT_Bid
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
loPosln.seek('P'+'U'+&lcMainf..PO)
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
*-- the LACOST array has 10 rows, 5 for category type, and 5 for the
*-- coresponding titles.  so, get the row of the cost element and add 5 to
*-- the row number to get the cost element title row number.

*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material [Start]
*!*	IF ASCAN(laCost,cCatgTyp) # 0
*!*	  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,cCatgTyp),1)+5,2]
*!*	ENDIF
IF ASCAN(laCost,"M_CMTYPE" + Typ) # 0 AND laCost[ASUBSCRIPT(laCost,ASCAN(laCost,"M_CMTYPE" + Typ) ,1),2] = cCatgTyp
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,"M_CMTYPE" + Typ) ,1)+7,2]
ENDIF   
*B607849,1 MMT 11/30/2006 fix bug of wrong labels in Bill of Material [End]


lcHead1   = ''
lcHead2   = ''
IF cCatgTyp = 'T'
  *- lcHead1   = '                  DATE                 QTY      DATE     DIRECT'
  lcHead1   = LANG_MfPrCT_lcHead1
  *- lcHead2   = 'VENDOR  ORDERD PO #  ORDRD RECVD. SHIPD.'
  lcHead2   = LANG_MfPrCT_lcHead2  
ELSE
  IF cCatgTyp = 'F'
    *- lcHead1   = '[-------------------- PULLED -------------------]  TOTAL'
    lcHead1   = LANG_MfPrCT_lcHead11
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
*!*  STORE 'Open'     TO laRpSource[1]
*!*  STORE 'Hold'     TO laRpSource[2]
*!*  STORE 'Actual'   TO laRpSource[3]
*!*  STORE 'Closed'   TO laRpSource[4]
*!*  STORE 'Canceled' TO laRpSource[5]
*!*  STORE 'Complete' TO laRpSource[6]
*!*  STORE 'Open'     TO laRpTarget[1]
STORE LANG_MfPrCT_Open2      TO laRpSource[1]
STORE LANG_MfPrCT_Hold2      TO laRpSource[2]
STORE LANG_MfPrCT_Actual2    TO laRpSource[3]
STORE LANG_MfPrCT_Closed2    TO laRpSource[4]
STORE LANG_MfPrCT_Cancelled2 TO laRpSource[5]
STORE LANG_MfPrCT_Complete2  TO laRpSource[6]
STORE LANG_MfPrCT_Bid        TO laRpSource[7]
STORE LANG_MfPrCT_Open2      TO laRpTarget[1]


lcStatus = 'O'
*!*	IF TYPE('loObjLink') <> 'O'
*!*	  loObjLink = CreateObject('RemoteTable','OBJLINK','OBJLNKTY','OBJLINK',SET("Datasession"))
*!*	ENDIF 
*!*	IF TYPE('loObjects') <> 'O'
*!*	  loObjects = CreateObject('RemoteTable','OBJECTS','OBJECTID','OBJECTS',SET("Datasession"))
*!*	ENDIF 
*!*	IF TYPE('loNotePad') <> 'O'
*!*	  loNotePad = CreateObject('RemoteTable','NOTEPAD','NOTEPAD','NOTEPAD',SET("Datasession"))
*!*	ENDIF 
*!*	IF TYPE('loStyle') <> 'O'
*!*	  loStyle  = CreateObject('RemoteTable','STYLE','STYLE','STYLE',SET("Datasession"))
*!*	ENDIF
*!*	IF TYPE('loScale') <> 'O'
*!*	  loScale  = CreateObject('RemoteTable','SCALE','SCALE','SCALE',SET("Datasession"))
*!*	ENDIF
*!*	IF TYPE('loOrdHdr') <> 'O'
*!*	  loOrdHdr  = CreateObject('RemoteTable','ORDHDR','ORDHDR','ORDHDR',SET("Datasession"))
*!*	ENDIF
*!*	IF TYPE('loPosLn') <> 'O'
  loPosLn  = CreateObject('RemoteTable','POSLN','POSLN','POSLN',SET("Datasession"))
*!*	ENDIF
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
lcTit  = ALLTRIM(gfGetMemvar('M_PRDLNLBL',oAriaApplication.ActiveCompanyID))
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

*- = lfOGMover(@laRpSource,@laRpTarget,'Select Cutting Ticket Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,LANG_MfPrCT_Select,.T.,'')  && call mover function.
lcStatus = ' '

*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
*!*      lcStatus = lcStatus + IIF(laRpTarget[lnI]     = 'Open'        ,'O',;
*!*                            IIF(laRpTarget[lnI] = 'Hold'    ,'H',;
*!*                            IIF(laRpTarget[lnI] = 'Actual'  ,'A',;
*!*                            IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
*!*                            IIF(laRpTarget[lnI] = 'Canceled','X',;
*!*                            IIF(laRpTarget[lnI] = 'Complete','C',''))))))
    lcStatus = lcStatus + IIF(laRpTarget[lnI] = LANG_MfPrCT_Open2,'O',;
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Hold2,'H',;
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Actual2,'A',;
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Closed2,'S',;
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Bid,'B',;                          
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Cancelled2,'X',;
                          IIF(laRpTarget[lnI] = LANG_MfPrCT_Complete2,'C','')))))))
                          
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
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
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
DIMENSION laFileStru[lnFileStru+12,18]

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
laFileStru[lnFileStru,3] = 20
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

FOR  lnLen = 7 TO 18
  FOR lnCount = 0 TO 12
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


*B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [Start]
*-- Mahmoud Said Start
*-- 
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
*-- Mahmoud Said End


*B607895,1 MMT 12/20/2006 fix bug of Wrong printing for the C/T [End]