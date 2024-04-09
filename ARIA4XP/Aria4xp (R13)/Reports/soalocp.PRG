*!**********************************************************************
*! Program file        : SOALOCP.PRG (E# 301381)
*! Program description : CutTkt/Po Allocation Report.
*! Module              : SO (Sales Order)
*! Developer           : Heba Fathi (HFK)
*! Tracking Job Number : #038725
*! Date                : 01/11/2005
*!**********************************************************************
*! Calls:
*!      Programs       :
*!      Screens        :
*!      Global Function:
*!**********************************************************************
*! Called From         :
*!**********************************************************************
*! Passed Parameters: None
*!**********************************************************************
*! Modification:
*!* B609356,1 SMA 07/25/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*!**********************************************************************
*!*B608899,1 AHS 06/18/2009 Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
*!**********************************************************************
#INCLUDE R:\Aria4xp\reports\soalocp.h
IF lcRpBaseOn = 'P' && Based on purchase order
  DO lpPOCollect
ELSE
  DO lpStyleCollect
ENDIF
*!
*!**********************************************************************
*! Name      : lpPOCollect
*! Developer : Heba Fathi (HFK)
*! Date      : 02/22/2005
*! Purpose   : Procedure for PO based collect
*!**********************************************************************
*
PROCEDURE lpPOCollect


PRIVATE llExit
llExit = .F.
*- Function to check installed modules
=lfChckMod()
IF llExit
  RETURN
ENDIF
*--- Get Style Picture
lcStyPic = gfItemMask('HI')
IF loOGScroll.llOgFltCh
  *- First check that there is a filter on style file, if there is so we get style fields
  *- needed & sent it to Sql to make join with Cutpick, if ther is not then we get fields
  *- from Sql files first then we get style fields needed from them.
  lnStyleCondition = ASCAN(loOGScroll.laFltExp,'STYLE')
  lnStyleCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnStyleCondition,1)
  lcStyleCondition = loOGScroll.laFltExp[lnStyleCondition,2]
  IF OCCURS('AND',lcStyleCondition )= 0  && if there are no styles selected, don't get style records
    llContinue = .T.
    lcSqlStyle = ""
  ELSE  && get data from style file first
    *- Get style file records that match style expression
    =lfGetStyles()
  ENDIF

  IF llContinue = .F.
    RETURN .F.
  ELSE
    *- Get fields from poshdr file and cutpick file
    lcSqlFile = loOGScroll.gfTempName()
    lcSqlStatement = " SELECT PosHdr.PO,PosHdr.Status,PosHdr.Complete,PosHdr.nStyOrder,PosHdr.Receive,PosHdr.Damage,PosHdr.Cancel,PosHdr.[Open],"
    lcSqlStatement = lcSqlStatement + "CutPick.cTktNo,CutPick.[Order],CutPick.Style,CutPick.Qty1,CutPick.Qty2,CutPick.Qty3,"
    lcSqlStatement = lcSqlStatement + "CutPick.Qty4,CutPick.Qty5,CutPick.Qty6,CutPick.Qty7,CutPick.Qty8,CutPick.TotQty,CutPick.cOrdLine"
    lcSqlStatement = lcSqlStatement + " FROM POSHDR (INDEX = POSHDR) INNER JOIN CUTPICK (INDEX = CUTPICK) ON POSHDR.PO = CUTPICK.CTKTNO "
    IF !EMPTY(lcSqlStyle)&& Styles selected
      lcSqlStatement = lcSqlStatement + " INNER JOIN " + lcSqlStyle + " TmpStyle ON CUTPICK.Style = TmpStyle.Style "
    ENDIF
    *- first identify POSHDR condition
    lcSqlCondition = " POSHDR.Status <> 'X' AND POSHDR.CBUSDOCU = 'P' AND "
    lcSqlCondition = lcSqlCondition + IIF(lcRpPrintBy = 'C'," POSHDR.CSTYTYPE = 'U' "," POSHDR.CSTYTYPE = 'P' ")
    lcSqlCondition = lcSqlCondition + " AND CUTPICK.TranCd = " + IIF(lcRpPrintBy = 'C',"1","2")
    IF !EMPTY(loOGScroll.lcRpSqlExp)
      lcSqlCondition = lcSqlCondition  + " AND " + loOGScroll.lcRpSqlExp
    ENDIF
    lcSqlSelect = lcSqlStatement + " WHERE " + lcSqlCondition
    lnSqlResult = loOGScroll.oRDA.SqlRun(lcSqlSelect,lcSqlFile,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnSqlResult > 0
      SELECT &lcSqlFile
      IF lfNoRecord()  && no records found
        RETURN .F.
      ELSE
        lnBuffering = CURSORGETPROP("Buffering","&lcSqlFile")
        =CURSORSETPROP("Buffering",3,"&lcSqlFile")
        INDEX ON PO TAG &lcSqlFile
      ENDIF
    ELSE
      =loOGScroll.oRDA.CheckRetResult("SqlRun",lnSqlResult,.T.)
      RETURN .F.
    ENDIF
    *- Get dyelot ot configuration from POSLN file
    IF !llGetDye  && if we didn't get dyelot info. before
      IF llDyelot .AND. llPrint
        =lfGetDyelot()
        llGetDye = .T.
      ENDIF
    ENDIF

    *- Call function to create temp files
    =lfCreatTmp()

    *- Call function to collect data
    =lfGetCutPo()
  ENDIF
ENDIF
lcOldRpExp = loOGScroll.lcRpExp
*--- Start Displaying Report.
lcPath = oAriaApplication.WorkDir
lcFileName = lcRpOrder + ".DBF"

IF USED(lcRpOrder)
  SELECT(lcRpOrder)
  GOTO TOP
  WAIT CLEAR

  DO gfDispRe WITH EVAL('lcRpForm')
ELSE
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
ENDIF
*!
*!**********************************************************************
*! Name      : lpStyleCollect
*! Developer : Heba Fathi (HFK)
*! Date      : 02/22/2005
*! Purpose   : Procedure for style based  collect
*!**********************************************************************
*
PROCEDURE lpStyleCollect
SELECT STYLE
SET ORDER TO TAG STYLE
STORE 0 TO lnOrdQty, lnTotQty
STORE .F. TO llAddStyle
loogScroll.cCROrientation = 'L'
*-loOGScroll.llOgFltCh
IF !(loOGScroll.lcRpExp == lcOldRpExp) .OR. !(lcOldStat == lcRpStatus)
  lnHold = ATC('H',lcRpStatus)
  lnOpen = ATC('O',lcRpStatus)
  IF (lnHold > 0 .AND. lnOpen > 0) .OR. EMPTY(lcRpStatus)
    lcStatusExp = " INLIST(Status,'O','H') "
  ELSE
    IF lnHold > 0
      lcStatusExp = " Status = 'H' "
    ELSE
      lcStatusExp = " Status = 'O' "
    ENDIF
  ENDIF

  *-- Call function to create temp file.
  =lfCreateTmpFile()
  *-- To get the selected status if any.
  *-- to be sure that the status taken will be only hold and open status

  *-- to be sure that the status taken will be only hold and open status
  *--hfk, 05/09/2005
  loDeptHd = CreateObject("RemoteTable","ICDEPTHD","DEPTHD","ICDEPTHDR",SET("Datasession"))
  loDeptDt = CreateObject("RemoteTable","ICDEPTDT","DEPTDTS","ICDEPTDTR",SET("Datasession"))
  lcFoxSelect = " Select Style.CSTYMAJOR,Style.CVENSTY,Style.STYLE,Style.DESC,Style.TOTCOST,Style.TOTSTK,Style.TOTALO,Style.TOTWIP,Style.TOTORD FROM Style"
  *!*    lcFoxSelect = " Select ICDEPTHD.DEPT,ICDEPTHD.CDEPTDESC,ICDEPTDT.CSTYGROUP,STYLE.CSTYMAJOR,"
  *!*    lcFoxSelect = lcFoxSelect + " STYLE.CVENSTY,STYLE.STYLE,STYLE.DESC,STYLE.TOTCOST,STYLE.TOTSTK,"
  *!*    lcFoxSelect = lcFoxSelect + " STYLE.TOTALO,STYLE.TOTWIP,Style.TOTORD FROM icdeptdt INNER JOIN "
  *!*    lcFoxSelect = lcFoxSelect + " icdepthd ON icdepthd.dept+icdepthd.cstygroup =icdeptdt.dept+icdeptdt.cstygroup "
  *!*    lcFoxSelect = lcFoxSelect + " Inner Join Style on IcDeptDt.Style = Style.Style "
  *--hfk, 05/09/2005

  lcStyles = loOGScroll.gfTempName()

  *- Create an object of remote data access
  IF TYPE('loSqlConnection') <> 'O'
    loSqlConnection = CREATEOBJECT('RemoteDataAccess')
  ENDIF
  llGroupExp = .F.
  llDeptExp = .F.
  *- Copy groups selected to temp file in order to be joined with main file
  lcGroupStat  = ""
  lnGroupCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.CSTYGROUP')
  lnGroupCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnGroupCondition,1)
  lcGroupCondition = loOGScroll.laOGFxFlt[lnGroupCondition,6]
  IF !EMPTY(lcGroupCondition)
    llGroupExp = .T.
    lcGroupFile = loOGScroll.gfTempName()
    gfCrtTmp(lcGroupFile,"(cStyGroup C(6))",,"",.F.)
    lnSepOccur1 = OCCURS("|",lcGroupCondition)
    IF lnSepOccur1 = 0
      lcGroup = lcGroupCondition
      INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
    ELSE
      FOR lnGroups = 1 TO lnSepOccur1+1
        lcGroup = IIF(lnGroups=1,SUBSTR(lcGroupCondition,1,6),SUBSTR(lcGroupCondition,ATC('|',lcGroupCondition,lnGroups-1)+1,6))
        INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
      ENDFOR
    ENDIF
*!*      lcTmpGroup = loOGScroll.gfTempName()
*!*      SELECT &lcGroupFile
*!*      COPY TO oAriaApplication.WorkDir+lcTmpGroup+".dbf"
*!*      IF !USED('&lcTmpGroup')
*!*        USE oAriaApplication.WorkDir + lcTmpGroup + ".DBF" IN 0
*!*      ENDIF
*!*      lcGroupStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpGroup+".dbf' ON Style.CSTYGROUP = "+lcTmpGroup+".CSTYGROUP"
  ENDIF
  *- Copy Departments selected to temp file in order to be joined with main file
  lcDeptStat  = ""
  lnDeptCondition = ASCAN(loOGScroll.laOGFxFlt,'ICDEPTDT.DEPT')
  lnDeptCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDeptCondition,1)
  lcDeptFile = loOGScroll.laOGFxFlt[lnDeptCondition,6]
  IF !EMPTY(lcDeptFile)
    SELECT (lcDeptFile)
    LOCATE
    IF !EMPTY(Dept)
       llDeptExp = .T.
*!*        lcTmpDept = loOGSCroll.gfTempName()
*!*        COPY TO oAriaApplication.WorkDir+lcTmpDept+".dbf"
*!*        IF !USED('&lcTmpDept')
*!*          USE oAriaApplication.WorkDir + lcTmpDept + ".DBF" IN 0
*!*        ENDIF
*!*        lcDeptStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpDept+".dbf' ON Style.Dept = "+lcTmpDept+".Dept"
    ENDIF
  ENDIF

  *- Copy Styles selected to temp file in order to be joined with main file
  lcStyleStat  = ""
  lnStyleCondition = ASCAN(loOGScroll.laOGFxFlt,'POSLN.STYLE')
  lnStyleCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnStyleCondition,1)
  lcStyleFile = loOGScroll.laOGFxFlt[lnStyleCondition,6]
  IF !EMPTY(lcStyleFile)
    SELECT (lcStyleFile)
    LOCATE
    IF !EMPTY(cStyMajor)
      lcTmpStyle = loOGSCroll.gfTempName()
      COPY TO oAriaApplication.WorkDir+lcTmpStyle+".dbf"
      IF !USED('&lcTmpStyle')
        USE oAriaApplication.WorkDir + lcTmpStyle + ".DBF" IN 0
      ENDIF
      lcStyleStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpStyle+".dbf' ON Style.cStyMajor = "+lcTmpStyle+".cStyMajor"
    ENDIF
  ENDIF

  *-- Add groups, departments ,styles temp. files to the select statement
*!*    IF !EMPTY(lcGroupStat)
*!*      lcFoxSelect= lcFoxSelect + lcGroupStat
*!*    ENDIF
  IF !EMPTY(lcDeptStat)
    lcFoxSelect= lcFoxSelect + lcDeptStat
  ENDIF
  IF !EMPTY(lcStyleStat)
    lcFoxSelect= lcFoxSelect + lcStyleStat
  ENDIF

  *-- add the 'where' condition
  lcFoxSelect = lcFoxSelect + " Where STYLE.STATUS = 'A' .AND. (STYLE.TOTWIP <> 0 .OR. STYLE.TOTORD <> 0 .OR. STYLE.TOTSTK <> 0) "
  *- Get Fox data.
  lcFoxFile = loOGScroll.gfTempName()
  lnConnectionHandlar = loSqlConnection.SqlRun(lcFoxSelect,lcFoxFile,,oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DATASESSION"))
  *-- select orders from OrdHdr File
  lcOrdHdrFile = loOGScroll.gfTempName()
  lcOrderSelect = " Select Order From OrdHdr Where cOrdType = 'O' AND "+lcStatusExp
  lnOrderResult = loSqlConnection.SqlRun(lcOrderSelect,lcOrdHdrFile,,oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DATASESSION"))
  IF lnConnectionHandlar > 0  .AND. lnOrderResult > 0
    *- convert file carrying styles to Sql
    SELECT &lcFoxFile
    IF lfNoRecord()  && no records found
      RETURN .F.
    ELSE
      lnBuffering = CURSORGETPROP("Buffering","&lcFoxFile")
      =CURSORSETPROP("Buffering",3,"&lcFoxFile")
      INDEX ON Style TAG &lcFoxFile
      SET ORDER TO TAG &lcFoxFile
    ENDIF

    *- convert file carrying Orders to Sql
    lcTmpOrders = loOGScroll.gfTempName()
    SELECT &lcOrdHdrFile
    COPY TO oAriaApplication.WorkDir+lcTmpOrders+".dbf"
    IF !USED('&lcTmpOrders')
      USE oAriaApplication.WorkDir + lcTmpOrders + ".DBF" IN 0
    ENDIF
    lcOrdersFile = loOGScroll.gfSqlTempName('','[Order] C(6)',lcTmpOrders,'Order')
    IF EMPTY(lcOrdersFile)
      *-- SQL connection Error. Can't Proceed
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF

    *-- Get Sql data
    lcSqlSelect = "SELECT POSLN.STYLE,POSLN.PO,POSLN.DYELOT,POSHDR.COMPLETE,POSHDR.AVAILABLE,POSHDR.VENDOR,CUTPICK.[ORDER],CUTPICK.TOTQTY,  "
    lcSqlSelect = lcSqlSelect + " STR(POSLN.[LINENO],6) as Line,CutPick.CORDLINE,CutPick.TranCd,CutPick.cTktNo,CutPick.cTktLineNo,CutPick.Style "
    lcSqlSelect = lcSqlSelect + " FROM POSHDR (Index = POSHDR) INNER JOIN POSLN (Index = POSLN) ON POSHDR.PO+POSHDR.CBUSDOCU+POSHDR.CSTYTYPE = "
    lcSqlSelect = lcSqlSelect + " POSLN.PO+POSLN.CBUSDOCU+POSLN.CSTYTYPE  "
    *-hfk, 05/09/2005
    *--lcSqlSelect = lcSqlSelect + " INNER JOIN CUTPICK (Index = CutPick) ON POSLN.PO+STR(POSLN.[LINENO],6) =CUTPICK.CTKTNO+CTKTLINENO "
    IF ("POSHDR.VENDOR" $ loOGScroll.lcRpSqlExp)
      lcSqlSelect = lcSqlSelect + " Left Outer JOIN CUTPICK (Index = CutPick) ON POSLN.PO = CUTPICK.CTKTNO"
    ELSE
      lcSqlSelect = lcSqlSelect + " INNER JOIN CUTPICK (Index = CutPick) ON POSLN.PO+STR(POSLN.[LINENO],6) =CUTPICK.CTKTNO+CTKTLINENO "
      IF !EMPTY(lcOrdersFile)
        lcSqlSelect = lcSqlSelect + " INNER JOIN " + lcOrdersFile + " TmpOrders ON CutPick.[Order] = TmpOrders.[Order] "
      ENDIF
    ENDIF
    *-hfk, 05/09/2005

    *-hfk, 05/09/2005
    *-- lcSqlSelect = lcSqlSelect + " WHERE POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE ='P' AND POSLN.TRANCD = '1' AND CUTPICK.TRANCD = '2' "
    IF ("POSHDR.VENDOR" $ loOGScroll.lcRpSqlExp)
      lcSqlSelect = lcSqlSelect + " WHERE POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE ='P' AND POSLN.TRANCD = '1'  "
    ELSE
      lcSqlSelect = lcSqlSelect + " WHERE POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE ='P' AND POSLN.TRANCD = '1' AND CUTPICK.TRANCD = '2' "
    ENDIF
    *-hfk, 05/09/2005
    IF !EMPTY(loOGScroll.lcRpSqlExp)
      lcFilter = STRTRAN(loOGScroll.lcRpSqlExp,'POSLN.STYLE','LEFT(POSLN.STYLE,12)')
        lcSqlSelect = lcSqlSelect + " AND " + lcFilter
    ENDIF

    lcSqlFile = loOGScroll.gfTempName()
    lnSqlResult = loOGScroll.oRDA.SqlRun (lcSqlSelect,lcSqlFile,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnSqlResult > 0
      SELECT &lcSqlFile
      IF RECCOUNT()=0
        IF ("POSHDR.VENDOR" $ loOGScroll.lcRpSqlExp)
          =lfNoRecord()
          RETURN
        ELSE
          llNoSql = .T.
        ENDIF
      ELSE
        lnBuffering = CURSORGETPROP("Buffering","&lcSqlFile")
        =CURSORSETPROP("Buffering",3,"&lcSqlFile")
        INDEX ON Style TAG &lcSqlFile
      ENDIF
*-*-HFK,05/09/2005
llTmpStyle = .F.
IF llDeptExp .OR. llGroupExp
  lcTmpStyles = loOGScroll.gfTempName()
  lcDeptGrp = " ICDEPTDTR.Style,icdeptdtr.cstygroup FROM icdeptdtr INNER JOIN icdepthdr ON icdeptdtr.dept +icdeptdtr.cstygroup="
  lcDeptGrp =lcDeptGrp +"icdepthdr.dept +icdepthdr.cstygroup "
  IF llGroupExp
    lcDeptGrp =lcDeptGrp +  "INNER JOIN &lcGroupFile ON &lcGroupFile..cStyGroup = icdepthdr.cstygroup "
  ENDIF
  IF llDeptExp
  lcDeptGrp =lcDeptGrp +"INNER JOIN &lcDeptFile ON &lcDeptFile..Dept = icdepthdr.Dept "
  ENDIF
  SELECT &lcDeptGrp INTO CURSOR lcTmpCursor
  SELECT lcTmpCursor
  IF lfnorecord()
    RETURN
  ELSE
    COPY TO oAriaApplication.WorkDir+lcTmpStyles+".dbf"
    IF !USED('&lcTmpGroup')
      USE oAriaApplication.WorkDir + lcTmpStyles + ".DBF" Excl IN 0
      llTmpStyle  = .T.
      SELECT &lcTmpStyles
      lnBuffering = CURSORGETPROP("Buffering","&lcTmpStyles")
      =CURSORSETPROP("Buffering",3,"&lcTmpStyles")
      INDEX ON Style TAG &lcTmpStyles
    ENDIF
  ENDIF
ENDIF

IF ("POSHDR.VENDOR" $ loOGScroll.lcRpSqlExp)
  SELECT &lcSqlFile
  SET RELATION TO Style INTO &lcFoxFile
  SCAN
    IF !EOF('&lcFoxFile')
      lcStyle = &lcFoxFile..Style
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
	  *WAIT WINDOW LANG_SoAlocp_SelStyle + &lcFoxFile..Style NOWAIT
	  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_SelStyle,oAriaApplication.GetHeaderText("LANG_SoAlocp_SelStyle",AHEADERFILE)) + &lcFoxFile..Style NOWAIT
	  *N000682,1 11/20/2012 MMT Globlization changes[End]

      IF llTmpStyle .AND. SEEK(lcStyle,lctmpStyles) .OR. !llTmpStyle
        STORE .F. TO llAddStyle
        =lfReplace()
        STORE .T. TO llAddStyle
      ENDIF
    ENDIF
  ENDSCAN
  IF llAddStyle
    STORE .F. TO llAddStyle
  ELSE
    STORE .T. TO llAddStyle
    lcStyle = &lcFoxFile..Style
    IF llTmpStyle .AND. SEEK(lcStyle,lctmpStyles) .OR. !llTmpStyle
      =lfReplace()
      STORE .F. TO llAddStyle
    ENDIF
  ENDIF
ELSE
  SELECT &lcFoxFile
  SCAN
    lnAlias = SELECT(0)
    lcStyle = &lcFoxFile..Style
    IF llTmpStyle .AND. SEEK(lcStyle,lctmpStyles) .OR. !llTmpStyle
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
	  *WAIT WINDOW LANG_SoAlocp_SelStyle + lcStyle NOWAIT
  	WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_SelStyle,oAriaApplication.GetHeaderText("LANG_SoAlocp_SelStyle",AHEADERFILE)) + lcStyle NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      IF llNoSql .OR. !SEEK(lcStyle,'&lcSqlFile')
        STORE .T. TO llAddStyle
        =lfReplace()
        STORE .F. TO llAddStyle
      ELSE
        SELECT &lcSqlFile
        =SEEK(lcStyle)
        SCAN REST WHILE Style = lcStyle
          IF !EOF('&lcFoxFile')
            STORE .F. TO llAddStyle
            =lfReplace()
            STORE .T. TO llAddStyle
          ENDIF
        ENDSCAN
        IF llAddStyle
          STORE .F. TO llAddStyle
        ELSE
          STORE .T. TO llAddStyle
          =lfReplace()
          STORE .F. TO llAddStyle
        ENDIF
      ENDIF
    ENDIF
    SELECT (lnAlias)
  ENDSCAN
ENDIF


*!*        SELECT &lcSqlFile
*!*        SET RELATION TO Style INTO &lcFoxFile
*!*        SCAN
*!*          IF !EOF('&lcFoxFile')
*!*            WAIT WINDOW LANG_SoAlocp_SelStyle + &lcFoxFile..Style NOWAIT
*!*            STORE .F. TO llAddStyle
*!*            =lfReplace()
*!*            STORE .T. TO llAddStyle
*!*          ENDIF
*!*        ENDSCAN
*!*        IF llAddStyle
*!*          STORE .F. TO llAddStyle
*!*        ELSE
*!*          STORE .T. TO llAddStyle
*!*          =lfReplace()
*!*          STORE .F. TO llAddStyle
*!*        ENDIF
*-HFK,05/09/2005
    ELSE
      =loOGScroll.oRDA.CheckRetResult("SqlRun",lnSqlResult,.T.)
      RETURN .F.
    ENDIF
  ELSE
    =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    RETURN .F.
  ENDIF
ENDIF

SELECT STYLE
SET ORDER TO TAG CSTYLE



IF !USED('&lcTmpFile')
  USE oariaapplication.workdir + lcTmpFile + ".dbf" IN 0 EXCL
  SELECT(lcTmpFile)
  INDEX ON CSTYMAJOR+COLOR+DEPT+GROUPID+PO TAG (lcTmpFile)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON GROUPID+DEPT+CSTYMAJOR+COLOR+PO TAG (lcTmpFile1) OF (lcTmpFile)
  *INDEX ON DEPT+GROUPID+CSTYMAJOR+COLOR+PO TAG (lcTmpFile2) OF (lcTmpFile)
  INDEX ON GROUPID+DEPT+CSTYMAJOR+COLOR+PO TAG (lcTmpFile1)
  INDEX ON DEPT+GROUPID+CSTYMAJOR+COLOR+PO TAG (lcTmpFile2)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDIF

SELECT(lcTmpFile)

SET RELATION TO
IF lfNoRecord()  && no records found
  RETURN .F.
ELSE
  DO CASE
    CASE lcRpSortBy = 'S'
      SET ORDER TO TAG (lcTmpFile)
    CASE lcRpSortBy = 'G'
      SET ORDER TO TAG (lcTmpFile1)
    CASE lcRpSortBy = 'D'
      SET ORDER TO TAG (lcTmpFile2)
  ENDCASE
ENDIF

REPLACE LGRANDTOT WITH .T.
LOCATE
lcOldStat = lcRpStatus
lcOldRpExp = loOGScroll.lcRpExp
*--- Start printing report

DO gfDispRe WITH EVAL('lcRpForm')
SET DEVICE TO SCREEN

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF
*------------------ Procedures and Functions section ------------------*
*!
*!**********************************************************************
*! Name      : lfGetCutPo
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/24/00
*! Purpose   : Function to collect data.
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*!
FUNCTION lfGetCutPo

SELECT &lcSqlFile
SET RELATION TO
*-SET RELATION TO STYLE INTO STYLE ADDIT
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
IF llGetDye
  SET RELATION TO cTktNo + Style INTO &lcDyeFile ADDITIVE
ENDIF

lcStatus = IIF(EMPTY(lcRpStatus),'.T.','ORDHDR.STATUS $ lcRpStatus')
SELECT &lcSqlFile
SCAN FOR EVALUATE(lcStatus)

  =SEEK('O'+&lcSqlFile..ORDER+&lcSqlFile..CORDLINE,'ORDLINE')

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW LANG_SoAlocp_Select+IIF(lcRpPrintBy='C',LANG_SoAlocp_CutTktA,LANG_SoAlocp_PoA)+&lcSqlFile..cTktNo NOWAIT
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Select,oAriaApplication.GetHeaderText("LANG_SoAlocp_Select",AHEADERFILE))+IIF(lcRpPrintBy='C',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_CutTktA,oAriaApplication.GetHeaderText("LANG_SoAlocp_CutTktA",AHEADERFILE)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoA,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoA",AHEADERFILE)))+&lcSqlFile..cTktNo NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  INSERT INTO (lcRpTrans) VALUES (&lcSqlFile..PO,&lcSqlFile..Status,&lcSqlFile..Complete,;
                                  &lcSqlFile..nStyOrder,&lcSqlFile..Receive,&lcSqlFile..Damage,;
                                  &lcSqlFile..Cancel,&lcSqlFile..Open)

*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
  IF llGetDye  &&There is dyelot
    INSERT INTO (lcRpOrder) VALUES (&lcSqlFile..cTktNo,&lcSqlFile..Order,&lcSqlFile..Style,;
                                    &lcSqlFile..Qty1,&lcSqlFile..Qty2,&lcSqlFile..Qty3,&lcSqlFile..Qty4,&lcSqlFile..Qty5,;
                                    &lcSqlFile..Qty6,&lcSqlFile..Qty7,&lcSqlFile..Qty8,&lcSqlFile..TotQty,&lcSqlFile..cOrdLine,&lcDyeFile..DyeLot,Ordline.Piktkt,Ordline.PikDate,Ordline.Complete,Ordline.Account)
  ELSE  && there is no dyelot
    INSERT INTO (lcRpOrder) VALUES (&lcSqlFile..cTktNo,&lcSqlFile..Order,&lcSqlFile..Style,;
                                    &lcSqlFile..Qty1,&lcSqlFile..Qty2,&lcSqlFile..Qty3,&lcSqlFile..Qty4,&lcSqlFile..Qty5,;
                                    &lcSqlFile..Qty6,&lcSqlFile..Qty7,&lcSqlFile..Qty8,&lcSqlFile..TotQty,&lcSqlFile..cOrdLine,SPACE(10),Ordline.Piktkt,Ordline.PikDate,Ordline.Complete,Ordline.Account)
  ENDIF
*!*	  IF llGetDye  &&There is dyelot
*!*	    INSERT INTO (lcRpOrder) VALUES (&lcSqlFile..cTktNo,&lcSqlFile..Order,&lcSqlFile..Style,;
*!*	                                    &lcSqlFile..Qty1,&lcSqlFile..Qty2,&lcSqlFile..Qty3,&lcSqlFile..Qty4,&lcSqlFile..Qty5,;
*!*	                                    &lcSqlFile..Qty6,&lcSqlFile..Qty7,&lcSqlFile..Qty8,&lcSqlFile..TotQty,&lcSqlFile..cOrdLine,&lcDyeFile..DyeLot)
*!*	  ELSE  && there is no dyelot
*!*	    INSERT INTO (lcRpOrder) VALUES (&lcSqlFile..cTktNo,&lcSqlFile..Order,&lcSqlFile..Style,;
*!*	                                    &lcSqlFile..Qty1,&lcSqlFile..Qty2,&lcSqlFile..Qty3,&lcSqlFile..Qty4,&lcSqlFile..Qty5,;
*!*	                                    &lcSqlFile..Qty6,&lcSqlFile..Qty7,&lcSqlFile..Qty8,&lcSqlFile..TotQty,&lcSqlFile..cOrdLine,SPACE(10))
*!*	  ENDIF
*B608899,1 AHS [End]

ENDSCAN
*!
*!**********************************************************************
*! Name      : lfWRep  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/24/00
*! Purpose   : When Rep Function.
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*!
FUNCTION lfWRep
LOCAL lnResult1
IF lcRpBaseOn = 'P' && based on purchase order
  lcRpForm = IIF(llDyeLot .AND. llPrint,'SOALOCPD','SOALOCP')
  DECLARE laRpSource[5],laRpTarget[5]  && Redeclare the source and target arrays.
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *STORE LANG_SoAlocp_Bid      TO laRpSource[1],laRpTarget[1]
  *STORE LANG_SoAlocp_Open     TO laRpSource[2],laRpTarget[2]
  *STORE LANG_SoAlocp_Hold     TO laRpSource[3],laRpTarget[3]
  *STORE LANG_SoAlocp_Complete TO laRpSource[4],laRpTarget[4]
  *STORE LANG_SoAlocp_Cancelled TO laRpSource[5],laRpTarget[5]  
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Bid,oAriaApplication.GetHeaderText("LANG_SoAlocp_Bid",AHEADERFILE))      TO laRpSource[1],laRpTarget[1]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))     TO laRpSource[2],laRpTarget[2]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Hold,oAriaApplication.GetHeaderText("LANG_SoAlocp_Hold",AHEADERFILE))     TO laRpSource[3],laRpTarget[3]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE)) TO laRpSource[4],laRpTarget[4]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Cancelled,oAriaApplication.GetHeaderText("LANG_SoAlocp_Cancelled",AHEADERFILE)) TO laRpSource[5],laRpTarget[5]
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  IF !llFrstTime
    lcTmpFab = loOGScroll.gfTempName()
    lcSelected = " SELECT ITEMLOC.TOTWIP,ITEMLOC.TOTSTK,ITEM.CSTYMAJOR AS FABRIC FROM ITEM (INDEX = CSTYLE)INNER JOIN ITEMLOC (INDEX = STYDYE) ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEM.CDEFWARE = ITEMLOC.CWARECODE "
    lcWhereCondition = " ITEMLOC.DYELOT = '   ' AND ITEM.CINVTYPE = 0002 AND ITEMLOC.CINVTYPE = 0002 "
    lcSqlStatement = lcSelected + " WHERE " + lcWhereCondition
    lnResult1 = loOGScroll.orda.SqlRun (lcSqlStatement,lcTmpFab,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
    llFrstTime = .T.
    IF lnResult1 >= 1
      lnBuffering = CURSORGETPROP("Buffering",lcTmpFab)
      =CURSORSETPROP("Buffering",3,lcTmpFab)
      SELECT (lcTmpFab)
      INDEX ON Fabric TAG &lcTmpFab
      SET ORDER TO TAG &lcTmpFab
    ENDIF
  ENDIF
  IF ("MF" $ oAriaApplication.CompanyInstalledModules)
    IF lcRpPrintBy = 'P'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcPoHdr = LANG_SOALOCP_POB
      lcPoHdr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCP_POB,oAriaApplication.GetHeaderText("LANG_SOALOCP_POB",AHEADERFILE))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      lcPoFld = [PO :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoB,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoB",AHEADERFILE))+;
                [', Status :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,oAriaApplication.GetHeaderText("LANG_STATUS",AHEADERFILE))+;
                [' ,Vendor :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Vendor,oAriaApplication.GetHeaderText("LANG_SoAlocp_Vendor",AHEADERFILE))+;
				[', APVENDOR.cVenComp :20 :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Name,oAriaApplication.GetHeaderText("LANG_SoAlocp_Name",AHEADERFILE))+[']
      lcPoFld = lcPoFld + [, Complete :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE))+;
                [',NStyOrder :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_TotQty,oAriaApplication.GetHeaderText("LANG_SoAlocp_TotQty",AHEADERFILE))+;
                [',PoTotal :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Amount,oAriaApplication.GetHeaderText("LANG_SoAlocp_Amount",AHEADERFILE))+;
                [',Receive :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Receive,oAriaApplication.GetHeaderText("LANG_SoAlocp_Receive",AHEADERFILE))+;
				[',Open :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))+[']
      lcKeyExpr = 'PP'
    ELSE
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
	  *lcPoHdr = LANG_SOALOCP_CUTTKTB
  	lcPoHdr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SOALOCP_CUTTKTB,oAriaApplication.GetHeaderText("LANG_SOALOCP_CUTTKTB",AHEADERFILE))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      lcPoFld = [PO :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_CutTktB,oAriaApplication.GetHeaderText("LANG_SoAlocp_CutTktB",AHEADERFILE))+;
                [', Style:R :H= lcRpStyPic , Status :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Status,oAriaApplication.GetHeaderText("LANG_SoAlocp_Status",AHEADERFILE))+;
				[', Entered :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Issue,oAriaApplication.GetHeaderText("LANG_SoAlocp_Issue",AHEADERFILE))+[']
	
      lcPoFld = lcPoFld + [, Complete :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE))+;
                          [', Season :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Season,oAriaApplication.GetHeaderText("LANG_SoAlocp_Season",AHEADERFILE))+;
						  [', cDivision :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Div,oAriaApplication.GetHeaderText("LANG_SoAlocp_Div",AHEADERFILE))+[']
      lcPoFld = lcPoFld + [, nStyOrder :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Budget,oAriaApplication.GetHeaderText("LANG_SoAlocp_Budget",AHEADERFILE))+;
                          [', Damage :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Damage,oAriaApplication.GetHeaderText("LANG_SoAlocp_Damage",AHEADERFILE))+;
						  [', Open :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))+[']
      lcKeyExpr = 'PU'
    ENDIF
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcPoHdr = LANG_SoAlocp_PoB
    lcPoHdr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoB,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoB",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    lcPoFld = [PO :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoB,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoB",AHEADERFILE))+;
              [', Status :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,oAriaApplication.GetHeaderText("LANG_STATUS",AHEADERFILE))+;
              [' ,Vendor :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Vendor,oAriaApplication.GetHeaderText("LANG_SoAlocp_Vendor",AHEADERFILE))+;
			  [', APVENDOR.cVenComp :20 :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Name,oAriaApplication.GetHeaderText("LANG_SoAlocp_Name",AHEADERFILE))+[']
    lcPoFld = lcPoFld + [, Complete :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE))+;
                        [',NStyOrder :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_TotQty,oAriaApplication.GetHeaderText("LANG_SoAlocp_TotQty",AHEADERFILE))+;
                        [',PoTotal :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Amount,oAriaApplication.GetHeaderText("LANG_SoAlocp_Amount",AHEADERFILE))+;
                        [',Receive :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Receive,oAriaApplication.GetHeaderText("LANG_SoAlocp_Receive",AHEADERFILE))+;
						[',Open :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))+[']

    lcKeyExpr = 'PP'
  ENDIF
ELSE  && based on style
  lcRpForm = IIF(llDyeLot .AND. llPrint,'SOALOCSD','SOALOCS')
  DECLARE laRpSource[2],laRpTarget[2]  && Redeclare the source and target arrays.
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *STORE LANG_SoAlocp_Open  TO laRpSource[1],laRpTarget[1]
  *STORE LANG_SoAlocp_Hold  TO laRpSource[2],laRpTarget[2]  
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))  TO laRpSource[1],laRpTarget[1]
  STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Hold,oAriaApplication.GetHeaderText("LANG_SoAlocp_Hold",AHEADERFILE))  TO laRpSource[2],laRpTarget[2]
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  LOCAL loSqlConnection
  *- create object from data access class
  IF TYPE('loSqlConnection') <> 'O'
    loSqlConnection = CREATEOBJECT('RemoteDataAccess')
  ENDIF
  lcRpTmpDpt = loOGScroll.gfTempName()
  lcDeptStatement = 'SELECT DISTINCT DEPT,CDEPTDESC FROM ICDEPTHD '
  lnDeptResult = loOGScroll.oRDA.SqlRun(lcDeptStatement,lcRpTmpDpt,,oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DATASESSION"))
  IF lnDeptResult > 0
    lnBuffering = CURSORGETPROP("Buffering","&lcRpTmpDpt")
    =CURSORSETPROP("Buffering",3,"&lcRpTmpDpt")
    SELECT &lcRpTmpDpt
    INDEX ON DEPT TAG (lcRpTmpDpt)
  ELSE  && Sql result is < 0
    =loSqlConnection.CheckRetResult("sqlrun",lnDeptResult,.T.)
    RETURN .F.
  ENDIF
ENDIF
IF llDyelot .AND. !llConfg
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcPrintTtl = LANG_SoAlocp_PrntDye
  lcPrintTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PrntDye,oAriaApplication.GetHeaderText("LANG_SoAlocp_PrntDye",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]
ELSE
  IF llDyelot .AND. llConfg
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcPrintTtl = LANG_SoAlocp_PrntCnfg
    lcPrintTtl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PrntCnfg,oAriaApplication.GetHeaderText("LANG_SoAlocp_PrntCnfg",AHEADERFILE))
    *N000682,1 11/20/2012 MMT Globlization changes[End]
  ENDIF
ENDIF
*!
*!**********************************************************************
*! Name      : lfClrRed  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/24/00
*! Purpose   : Clear read function(WHEN CHANGING SELECT BY CONDITION)
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*!
FUNCTION lfClrRed
ClearRead()

IF lcRpPrintBy =  'C'
  lcRpStatus = 'OH'
  lcPoFld = [PO :R :H=']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_CutTktB,oAriaApplication.GetHeaderText("LANG_SoAlocp_CutTktB",AHEADERFILE))+;
            [', Style:R :H= lcRpStyPic , Status :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Status,oAriaApplication.GetHeaderText("LANG_SoAlocp_Status",AHEADERFILE))+;
			[', Entered :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Issue,oAriaApplication.GetHeaderText("LANG_SoAlocp_Issue",AHEADERFILE))+[']

  lcPoFld = lcPoFld + [, Complete :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE))+;
            [', Season :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Season,oAriaApplication.GetHeaderText("LANG_SoAlocp_Season",AHEADERFILE))+;
			[', cDivision :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Div,oAriaApplication.GetHeaderText("LANG_SoAlocp_Div",AHEADERFILE))+[']
  lcKeyExpr = 'PU'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcPoHdr = LANG_SoAlocp_CutTktB
  lcPoHdr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_CutTktB,oAriaApplication.GetHeaderText("LANG_SoAlocp_CutTktB",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  lcRpStatus = 'BOHCX'
  lcPoFld = [PO :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoB,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoB",AHEADERFILE))+;
            [', Status :R :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,oAriaApplication.GetHeaderText("LANG_STATUS",AHEADERFILE))+;
            [' ,Vendor :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Vendor,oAriaApplication.GetHeaderText("LANG_SoAlocp_Vendor",AHEADERFILE))+;
			[', APVENDOR.cVenComp :20 :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Name,oAriaApplication.GetHeaderText("LANG_SoAlocp_Name",AHEADERFILE))+[']
  lcPoFld = lcPoFld + [, Complete :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE))+;
            [',NStyOrder :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_TotQty,oAriaApplication.GetHeaderText("LANG_SoAlocp_TotQty",AHEADERFILE))+;
            [',PoTotal :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Amount,oAriaApplication.GetHeaderText("LANG_SoAlocp_Amount",AHEADERFILE))+;
            [',Receive :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Receive,oAriaApplication.GetHeaderText("LANG_SoAlocp_Receive",AHEADERFILE))+;
			[',Open :R :H=']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE))+[']
  lcKeyExpr = 'PP'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcPoHdr = LANG_SoAlocp_PoB
  lcPoHdr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_PoB,oAriaApplication.GetHeaderText("LANG_SoAlocp_PoB",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
*!
*!**********************************************************************
*! Name      : lfsrCut  (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Rise change account flag, in range browse screen.
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*!
FUNCTION lfsrSPO
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN POSHDR
  CASE lcParm = 'R'
    llClearSPO = .F.
ENDCASE
*-- end of lfsrAcc.
*!
*!**********************************************************************
*! Name      : lfwOldVal     (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : When Old Value
*!**********************************************************************
*! Passed Parameters : None
*!**********************************************************************
*! Return      : None
*!**********************************************************************
*!
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18(.T.))      && Varible to hold the old value
*!
*!**********************************************************************
*! Name      : lfClearRep     (E# 301381)
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/24/00
*! Purpose   : Clear Temp.
*!**********************************************************************
*! Passed Parameters : None
*!**********************************************************************
*! Return      : None
*!**********************************************************************
*!
FUNCTION lfClearRep

loOGScroll.llOgFltCh = .T.
USE IN IIF(USED(lcRpTrans),lcRpTrans,0)
ERASE &oAriaApplication.WorkDir.&lcRpTrans+'.DBF'
ERASE &oAriaApplication.WorkDir.&lcRpTrans+'.CDX'

USE IN IIF(USED(lcRpOrder),lcRpOrder,0)
ERASE &oAriaApplication.WorkDir.&lcRpOrder+'.DBF'
ERASE &oAriaApplication.WorkDir.&lcRpOrder+'.CDX'
*!
*!**********************************************************************
*! Name      : lfChckMod  (B# 804356)
*! Developer : AHMED Abdel Naby - (AAN)
*! Date      : 09/20/2001
*! Purpose   : Function To Check for the modules.
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*!
FUNCTION lfChckMod
IF lcRpPrintBy='C'
  IF OCCURS('MF',oAriaApplication.CompanyInstalledModules)=0
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
    *=gfModalGen('TRM32096B00000','ALERT','MF'))
    =gfModalGen('TRM32096B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MF,oAriaApplication.GetHeaderText("LANG_MF",AHEADERFILE)))
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
    llExit = .T.
    RETURN
  ELSE
    llExit = .F.
  ENDIF
ELSE
  IF OCCURS('PO',oAriaApplication.CompanyInstalledModules)=0
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
    *=gfModalGen('TRM32096B00000','ALERT',LANG_PO))
	=gfModalGen('TRM32096B00000','ALERT',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PO,oAriaApplication.GetHeaderText("LANG_PO",AHEADERFILE)))
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
    llExit = .T.
    RETURN
  ELSE
    llExit = .F.
  ENDIF
ENDIF
*!
*!**********************************************************************
*! Name      : lfFabSum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : Calculated field value.
*!**********************************************************************
*!
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
LOCAL lnAlias
lnAlias = SELECT()
lnTotcomp = 0
SELECT(lcTmpFab)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcTmpFab)
  LOCATE
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE Fabric = lcFab
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN INT(lnTotcomp)
*!
*!**********************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change style flag, in range browse screen.
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : None
*!**********************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**********************************************************************
*!
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
  OTHERWISE      && Valid code
ENDCASE
*-- end of lfsrvSty.
*!
*!**********************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current style in style file
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : Calculated field value.
*!**********************************************************************
*!
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!
*!**********************************************************************
*! Name      : lfGetStyles
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current style in style file
*!**********************************************************************
*! Passed Parameters  : None
*!**********************************************************************
*! Returns            : Calculated field value.
*!**********************************************************************
*!
FUNCTION lfGetStyles

lnStyleExp = ASCAN(loOGScroll.laFltExp,'STYLE')
lcGroupStat = ""
lcSeasonStat = ""
lcDivisionStat = ""
IF lnStyleExp > 0
  *- create object from data access class
  IF TYPE('loSqlConnection') <> 'O'
    loSqlConnection = CREATEOBJECT('RemoteDataAccess')
  ENDIF
  lnStyleRow = ASUBSCRIPT(loOGScroll.laFltExp,lnStyleExp,1)
  lcStyleExp = loOGScroll.laFltExp[lnStyleRow,2]
  *-- Scatter group expression
  lcNewExp = ""
  IF !EMPTY(lcStyleExp)
    lnOccur= OCCURS('.AND.',lcStyleExp)
    IF lnOccur > 0
      FOR lnCount= 1 TO lnOccur+1
        lnStart = IIF(lnCount=1,1,ATC('.AND.',lcStyleExp,lnCount-1)+5)
        lnEnd = IIF(lnCount = lnOccur+1,LEN(lcStyleExp)+1,ATC('.AND.',lcStyleExp,lnCount))
        lnLength = lnEnd - lnStart
        lcScatteredExp = SUBSTR(lcStyleExp,lnStart,lnLength)
        llGroup = ATC('STYLE.CSTYGROUP',lcScatteredExp)>0
        llSeason = ATC('STYLE.SEASON',lcScatteredExp)> 0
        llDivision = ATC('STYLE.CDIVISION',lcScatteredExp)> 0
        llStatus = ATC('STYLE.STATUS',lcScatteredExp)> 0
        IF llGroup .OR. llSeason .OR. llDivision .OR. llStatus
          DO CASE
            CASE llStatus
              lcGroupExp = ""
            CASE llGroup
              lnGroupCondition = ASCAN(loOGScroll.laOgFxFlt,'STYLE.CSTYGROUP')
              lnGroupCondition = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnGroupCondition,1)
              lcGroupCondition = loOGScroll.laOgFxFlt[lnGroupCondition,6]
              IF !EMPTY(lcGroupCondition)
                lcGroupFile = loOGScroll.gfTempName()
                gfCrtTmp(lcGroupFile,"(cStyGroup C(6))",,"",.F.)
                SELECT &lcGroupFile
                lnSepOccur1 = OCCURS("|",lcGroupCondition)
                IF lnSepOccur1 = 0
                  lcGroup = lcGroupCondition
                  INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
                ELSE
                  FOR lnGroups = 1 TO lnSepOccur1+1
                    lcGroup = IIF(lnGroups=1,SUBSTR(lcGroupCondition,1,6),SUBSTR(lcGroupCondition,ATC('|',lcGroupCondition,lnGroups-1)+1,6))
                    INSERT INTO &lcGroupFile (cStyGroup) VALUES (lcGroup)
                  ENDFOR
                ENDIF
                SELECT &lcGroupFile
                lcTmpGroup  = loOGScroll.gfTempName()
                COPY TO oAriaApplication.WorkDir+lcTmpGroup+".dbf"
                IF !USED('&lcTmpGroup')
                  USE oAriaApplication.WorkDir + lcTmpGroup + ".DBF" IN 0
                ENDIF
                lcGroupStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpGroup+".dbf' ON Style.cStyGroup = "+lcTmpGroup+".cStyGroup"
                lcGroupExp = ""
              ENDIF

            CASE llSeason
              *- Copy seasons selected to temp file in order to be joined with main file
              lnSeasonCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.SEASON')
              lnSeasonCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnSeasonCondition,1)
              lcSeasonCondition = loOGScroll.laOGFxFlt[lnSeasonCondition,6]
              IF !EMPTY(lcSeasonCondition)
                lcSeasonFile = loOGScroll.gfTempName()
                gfCrtTmp(lcSeasonFile,"(Season C(6))",,"",.F.)
                lnSepOccur1 = OCCURS("|",lcSeasonCondition)
                IF lnSepOccur1 = 0
                  lcSeason = lcSeasonCondition
                  INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
                ELSE
                  FOR lnSeasons = 1 TO lnSepOccur1+1
                    lcSeason = IIF(lnSeasons=1,SUBSTR(lcSeasonCondition,1,6),SUBSTR(lcSeasonCondition,ATC('|',lcSeasonCondition,lnSeasons-1)+1,6))
                    INSERT INTO &lcSeasonFile (Season) VALUES (lcSeason)
                  ENDFOR
                ENDIF
                SELECT &lcSeasonFile
                lcTmpSeason  = loOGScroll.gfTempName()
                COPY TO oAriaApplication.WorkDir+lcTmpSeason+".dbf"
                IF !USED('&lcTmpSeason')
                  USE oAriaApplication.WorkDir + lcTmpSeason + ".DBF" IN 0
                ENDIF
                lcSeasonStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpSeason+".dbf' ON Style.Season = "+lcTmpSeason+".Season"
                lcGroupExp = ""
              ENDIF
            CASE llDivision
              *- Copy divisions selected to temp file in order to be joined with main file
              lnDivisionCondition = ASCAN(loOGScroll.laOGFxFlt,'STYLE.CDIVISION')
              lnDivisionCondition = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDivisionCondition,1)
              lcDivisionCondition = loOGScroll.laOGFxFlt[lnDivisionCondition,6]
              IF !EMPTY(lcDivisionCondition)
                lcDivisionFile = loOGScroll.gfTempName()
                gfCrtTmp(lcDivisionFile,"(Division C(6))",,"",.F.)
                lnSepOccur1 = OCCURS("|",lcDivisionCondition)
                IF lnSepOccur1 = 0
                  lcDivision = lcDivisionCondition
                  INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
                ELSE
                  FOR lnDivisions = 1 TO lnSepOccur1+1
                    lcDivision = IIF(lnDivisions=1,SUBSTR(lcDivisionCondition,1,6),SUBSTR(lcDivisionCondition,ATC('|',lcDivisionCondition,lnDivisions-1)+1,6))
                    INSERT INTO &lcDivisionFile (Division) VALUES (lcDivision)
                  ENDFOR
                ENDIF
                SELECT &lcDivisionFile
                lcTmpDivision  = loOGScroll.gfTempName()
                COPY TO oAriaApplication.WorkDir+lcTmpDivision+".dbf"
                IF !USED('&lcTmpDivision')
                  USE oAriaApplication.WorkDir + lcTmpDivision + ".DBF" IN 0
                ENDIF

                lcDivisionStat = " INNER JOIN '"+oAriaApplication.WorkDir+lcTmpDivision+".dbf' ON Style.cDivision = "+lcTmpDivision+".Division"
                lcGroupExp = ""
              ENDIF
          ENDCASE
        ELSE
          lcGroupExp = lcScatteredExp
        ENDIF
        IF !EMPTY(lcGroupExp)
          lcNewExp = IIF(EMPTY(lcNewExp),lcGroupExp,lcNewExp+' AND ' +lcGroupExp)
        ENDIF
      ENDFOR
    ELSE  && If occurance of AND is equal Zero
      lcNewExp = " "
    ENDIF
  ENDIF
  lcStyleSql = " Select Style From Style "
  IF !EMPTY(lcGroupStat)
    lcStyleSql = lcStyleSql + lcGroupStat
  ENDIF
  IF !EMPTY(lcSeasonStat)
    lcStyleSql = lcStyleSql + lcSeasonStat
  ENDIF
  IF !EMPTY(lcDivisionStat)
    lcStyleSql = lcStyleSql + lcDivisionStat
  ENDIF
  IF !EMPTY(lcNewExp)
    lcStyleSql = lcStyleSql + " Where " + lcNewExp
  ENDIF
  *- Get data from style file.
  lnStyleResult = loSqlConnection.SqlRun(lcStyleSql,lcStyles,,oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DATASESSION"))
  IF lnStyleResult = 1
    SELECT &lcStyles
    lcTempStyles = loOGScroll.gfTempName()
    COPY TO oAriaApplication.WorkDir+lcTempStyles+".dbf"
    IF !USED('&lcTempStyles')
      USE oAriaApplication.WorkDir + lcTempStyles + ".DBF" IN 0
    ENDIF
    IF lfNoRecord()
      llContinue = .F.
    ELSE
      lcSqlStyle = loOgScroll.gfSQLTempName('','Style C(19)',lcTempStyles,'Style') && SQL Temp File
    ENDIF
  ELSE
    =loSqlConnection.CheckRetResult("sqlrun",lnStyleResult,.T.)
    RETURN .F.
  ENDIF
ENDIF
*!
*!**********************************************************************
*! Name      : lfReplace
*! Developer : (AMH) Ahmed Maher
*! Date      : 02/21/2000
*! Purpose   : Add new record to lctmpfile
*!**********************************************************************
*!
FUNCTION lfReplace

IF loDeptDt.Seek(lcStyle)
  loDeptHd.Seek(ICDEPTDTR.Dept)
  lcDepartment = ICDEPTDTR.Dept
  lcDeptDesc = IcDeptHdR.cDeptDesc
  lcStyGroup = IcDeptDtR.cStyGroup
ELSE
  lcDepartment = ""
  lcDeptDesc = ""
  lcStyGroup = ""
ENDIF

IF llNoSql .OR. ISNULL(&lcSqlFile..order)
  SELECT (lcTmpFile)
  APPEND BLANK

  REPLACE DEPT      WITH lcDepartment,;
          CDEPTDESC WITH lcDeptDesc,;
          GROUPID   WITH lcStyGroup,;
          GROUPDESC WITH gfCodDes(lcStyGroup,'CSTYGROUP'),;
          CSTYMAJOR WITH &lcFoxFile..CSTYMAJOR,;
          CVENSTY   WITH &lcFoxFile..CVENSTY,;
          COLOR     WITH ALLTRIM(SUBSTR(&lcFoxFile..STYLE,lnMajorLen+2)),;
          cDesc     WITH &lcFoxFile..DESC,;
          TOTCOST   WITH &lcFoxFile..TOTCOST,;
          TOTSTK    WITH &lcFoxFile..TOTSTK,;
          TOTALO    WITH &lcFoxFile..TOTALO,;
          TOTWIP    WITH &lcFoxFile..TOTWIP,;
          OTS       WITH (&lcFoxFile..TOTSTK+&lcFoxFile..TOTWIP)-&lcFoxFile..TOTORD,;
          TOTORD    WITH &lcFoxFile..TOTORD,;
		  PO        WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_LblZ,oAriaApplication.GetHeaderText("LANG_SoAlocp_LblZ",AHEADERFILE))

ELSE
  =SEEK('O'+&lcSqlFile..ORDER,'ORDHDR')
  =SEEK('O'+&lcSqlFile..ORDER+&lcSqlFile..CORDLINE,'ORDLINE')
  SELECT (lcTmpFile)

  APPEND BLANK
  REPLACE DEPT      WITH lcDepartment,;
          CDEPTDESC WITH lcDeptDesc,;
          GROUPID   WITH lcStyGroup,;
          GROUPDESC WITH gfCodDes(lcStyGroup,'CSTYGROUP'),;
          CSTYMAJOR WITH &lcFoxFile..CSTYMAJOR,;
          CVENSTY   WITH &lcFoxFile..CVENSTY,;
          COLOR     WITH ALLTRIM(SUBSTR(&lcFoxFile..STYLE,lnMajorLen+2)),;
          cDesc     WITH &lcFoxFile..DESC,;
          TOTCOST   WITH &lcFoxFile..TOTCOST,;
          TOTSTK    WITH &lcFoxFile..TOTSTK,;
          TOTALO    WITH &lcFoxFile..TOTALO,;
          TOTWIP    WITH &lcFoxFile..TOTWIP,;
          OTS       WITH (&lcFoxFile..TOTSTK+&lcFoxFile..TOTWIP)-&lcFoxFile..TOTORD,;
          TOTORD    WITH &lcFoxFile..TOTORD,;
          PO        WITH IIF(EOF('&lcSqlFile') .OR. llAddStyle,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_LblZ,oAriaApplication.GetHeaderText("LANG_SoAlocp_LblZ",AHEADERFILE)),IIF(EMPTY(&lcSqlFile..PO),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_LblZ,oAriaApplication.GetHeaderText("LANG_SoAlocp_LblZ",AHEADERFILE)),&lcSqlFile..PO)),;
          COMPLETE  WITH &lcSqlFile..COMPLETE,;
          AVAILABLE WITH &lcSqlFile..AVAILABLE,;
          VENDOR    WITH &lcSqlFile..VENDOR,;
          ORDER     WITH &lcSqlFile..ORDER,;
          Dyelot    WITH &lcSqlFile..Dyelot

*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
  REPLACE PIKTKT    WITH Ordline.PikTkt
  REPLACE PIKDATE    WITH Ordline.PikDate
  REPLACE COMPLETE   WITH Ordline.Complete
  REPLACE ACCOUNT    WITH Ordline.Account
*B608899,1 AHS [End]

  lnOrdQty = ORDLINE.TOTQTY
  lnTotQty = &lcSqlFile..TOTQTY
  IF OrdHdr.Multi = 'Y'
    =SEEK('S'+OrdHdr.Account,'Customer')
    =lfSum()
  ELSE
    =SEEK('M'+OrdHdr.Account,'Customer')
  ENDIF
  lcCustPO = ''
  IF !(OrdHdr.MultiPO)
    lcCustPO = OrdHdr.CustPo
  ELSE
    =SEEK('O'+CutPick.Order+PADL(&lcSqlFile..CORDLINE,6,' '),'OrdLine')
    lcCustPO = OrdLine.CustPo
  ENDIF
  REPLACE BTNAME    WITH CUSTOMER.BTNAME,;
          CUSTPO    WITH lcCustPO       ,;
          START     WITH ORDHDR.START,;
          CANCEL    WITH ORDHDR.COMPLETE,;
          PRICE     WITH ORDLINE.PRICE,;
          ONORDER   WITH lnOrdQty,;
          POALLO    WITH lnTotQty,;
          TOTUNALLO WITH TOTORD - TOTALO,;
          LGRANDTOT WITH .F.

  lcStyleMajor = SUBSTR(&lcSqlFile..Style,1,lnMajorLen)
  lcColor = SUBSTR(&lcSqlFile..Style,lnMajorLen+2)
  IF SEEK(lcStyleMajor+lcColor) .AND. !llAddStyle
    REPLACE TOTUNALLO WITH TOTUNALLO - lnTotQty
  ENDIF
ENDIF
*-- end of lfReplace.
*!
*!**********************************************************************
*! Name      : lfSum
*! Developer : (AMH) Ahmed Maher
*! Date      : 05/13/2001
*! Purpose   : Consolidate the Cutpick lines of same Po line.
*!**********************************************************************
*!
FUNCTION lfSum

PRIVATE lnAlias
lnAlias = SELECT(0)
STORE 0 TO lnOrdQty, lnTotQty
SELECT &lcSqlFile
lcOrderNo = ORDER
SCAN REST WHILE Order = lcOrderNo
  lnTotQty = lnTotQty + TOTQTY
  IF SEEK('O'+CutPick.Order+PADL(&lcSqlFile..CORDLINE,6,' '),'OrdLine')
    lnOrdQty = lnOrdQty + &lcSqlFile..TOTQTY
  ENDIF
ENDSCAN
IF !BOF()
  SKIP -1
ENDIF
SELECT (lnAlias)
*-- end lfSum.
*!
*!**********************************************************************
*! Name      : lfCreateTmpFile
*! Developer : Heba Fathi
*! Date      : 02/02/2005
*! Purpose   : Create temp. files
*!**********************************************************************
*!
FUNCTION lfCreateTmpFile
lcTmpFile  = loOGScroll.gfTempName()
lcTmpFile1 = loOGScroll.gfTempName()
lcTmpFile2 = loOGScroll.gfTempName()

*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
*DIMENSION laTmpFile[29,4]
DIMENSION laTmpFile[32,4]
*B608899,1 AHS [End]

laTmpFile[1,1]= 'DEPT'
laTmpFile[1,2]= 'C'
laTmpFile[1,3]= 5
laTmpFile[1,4]= 0

laTmpFile[2,1]= 'CDEPTDESC'
laTmpFile[2,2]= 'C'
laTmpFile[2,3]= 30
laTmpFile[2,4]= 0

laTmpFile[3,1]= 'GROUPID'
laTmpFile[3,2]= 'C'
laTmpFile[3,3]= 6
laTmpFile[3,4]= 0

laTmpFile[4,1]= 'GROUPDESC'
laTmpFile[4,2]= 'C'
laTmpFile[4,3]= 30
laTmpFile[4,4]= 0

IF lnMajorLen = 0
  lnMajorLen = 12
ENDIF


laTmpFile[5,1]= 'CSTYMAJOR'
laTmpFile[5,2]= 'C'
laTmpFile[5,3]= lnMajorLen
laTmpFile[5,4]= 0

laTmpFile[6,1]= 'CVENSTY'
laTmpFile[6,2]= 'C'
laTmpFile[6,3]= 19
laTmpFile[6,4]= 0

laTmpFile[7,1]= 'COLOR'
laTmpFile[7,2]= 'C'
laTmpFile[7,3]= 6
laTmpFile[7,4]= 0

laTmpFile[8,1]= 'cDesc'
laTmpFile[8,2]= 'C'
laTmpFile[8,3]= 20
laTmpFile[8,4]= 0

laTmpFile[9,1]= 'TOTCOST'
laTmpFile[9,2]= 'N'
laTmpFile[9,3]= 13
laTmpFile[9,4]= 2

laTmpFile[10,1]= 'TOTSTK'
laTmpFile[10,2]= 'N'
laTmpFile[10,3]= 7
laTmpFile[10,4]= 0

laTmpFile[11,1]= 'TOTALO'
laTmpFile[11,2]= 'N'
laTmpFile[11,3]= 7
laTmpFile[11,4]= 0

laTmpFile[12,1]= 'TOTWIP'
laTmpFile[12,2]= 'N'
laTmpFile[12,3]= 7
laTmpFile[12,4]= 0

laTmpFile[13,1]= 'OTS'
laTmpFile[13,2]= 'N'
laTmpFile[13,3]= 7
laTmpFile[13,4]= 0

laTmpFile[14,1]= 'TOTORD'
laTmpFile[14,2]= 'N'
laTmpFile[14,3]= 7
laTmpFile[14,4]= 0

laTmpFile[15,1]= 'PO'
laTmpFile[15,2]= 'C'
laTmpFile[15,3]= 6
laTmpFile[15,4]= 0

laTmpFile[16,1]= 'COMPLETE'
laTmpFile[16,2]= 'D'
laTmpFile[16,3]= 0
laTmpFile[16,4]= 0

laTmpFile[17,1]= 'AVAILABLE'
laTmpFile[17,2]= 'D'
laTmpFile[17,3]= 0
laTmpFile[17,4]= 0

laTmpFile[18,1]= 'VENDOR'
laTmpFile[18,2]= 'C'
laTmpFile[18,3]= 8
laTmpFile[18,4]= 0

laTmpFile[19,1]= 'ORDER'
laTmpFile[19,2]= 'C'
laTmpFile[19,3]= 6
laTmpFile[19,4]= 0

laTmpFile[20,1]= 'BTNAME'
laTmpFile[20,2]= 'C'
laTmpFile[20,3]= 30
laTmpFile[20,4]= 0

laTmpFile[21,1]= 'CUSTPO'
laTmpFile[21,2]= 'C'
laTmpFile[21,3]= 15
laTmpFile[21,4]= 0

laTmpFile[22,1]= 'START'
laTmpFile[22,2]= 'D'
laTmpFile[22,3]= 0
laTmpFile[22,4]= 0

laTmpFile[23,1]= 'CANCEL'
laTmpFile[23,2]= 'D'
laTmpFile[23,3]= 0
laTmpFile[23,4]= 0

laTmpFile[24,1]= 'PRICE'
laTmpFile[24,2]= 'N'
laTmpFile[24,3]= 12
laTmpFile[24,4]= 2

laTmpFile[25,1]= 'ONORDER'
laTmpFile[25,2]= 'N'
laTmpFile[25,3]= 7
laTmpFile[25,4]= 0

laTmpFile[26,1]= 'POALLO'
laTmpFile[26,2]= 'N'
laTmpFile[26,3]= 7
laTmpFile[26,4]= 0

laTmpFile[27,1]= 'TOTUNALLO'
laTmpFile[27,2]= 'N'
laTmpFile[27,3]= 7
laTmpFile[27,4]= 0

laTmpFile[28,1]= 'LGRANDTOT'
laTmpFile[28,2]= 'L'
laTmpFile[28,3]= 0
laTmpFile[28,4]= 0

laTmpFile[29,1]= 'DyeLot'
laTmpFile[29,2]= 'C'
laTmpFile[29,3]= 10
laTmpFile[29,4]= 0


*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
laTmpFile[30,1]= 'Piktkt'
laTmpFile[30,2]= 'C'
laTmpFile[30,3]= 5
laTmpFile[30,4]= 0

laTmpFile[31,1]= 'PikDate'
laTmpFile[31,2]= 'D'
laTmpFile[31,3]= 0
laTmpFile[31,4]= 0

*!*	laTmpFile[32,1]= 'Complete'
*!*	laTmpFile[32,2]= 'D'
*!*	laTmpFile[32,3]= 0
*!*	laTmpFile[32,4]= 0

laTmpFile[32,1]= 'Account'
laTmpFile[32,2]= 'C'
laTmpFile[32,3]= 5
laTmpFile[32,4]= 0
*B608899,1 AHS [End]

gfCrtTmp(lcTmpFile,@laTmpFile,"CSTYMAJOR+COLOR+DEPT+GROUPID+PO",lcTmpFile)

SELECT &lcTmpFile
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON GROUPID+DEPT+CSTYMAJOR+COLOR+PO TAG (lcTmpFile1) OF (lcTmpFile)
*INDEX ON DEPT+GROUPID+CSTYMAJOR+COLOR+PO TAG (lcTmpFile2) OF (lcTmpFile)
INDEX ON GROUPID+DEPT+CSTYMAJOR+COLOR+PO TAG (lcTmpFile1)
INDEX ON DEPT+GROUPID+CSTYMAJOR+COLOR+PO TAG (lcTmpFile2)
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
SELECT (lcTmpFile)
SET ORDER TO (lcTmpFile)
*!
*!**********************************************************************
*! Name      : lfvBaseOn
*! Developer : Heba Fathi
*! Date      : 02/02/2005
*! Purpose   : to refresh OG after selecting criteria to base on
*!**********************************************************************
*!
FUNCTION lfvBaseOn
ClearRead()
lcStatusStr = ""
lcOldRpExp = ""
*!
*!**********************************************************************
*! Name      : lfNoRecord
*! Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Dectect if no records match criteria
*!**********************************************************************
*
FUNCTION lfNoRecord
GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*!
*!**********************************************************************
*! Name      : lfCreatTmp
*! Developer : Heba Fathi (HFK)
*! Date      : 01/11/2005
*! Purpose   : Function Create Temp Files.
*!**********************************************************************
*!
FUNCTION lfCreatTmp

loOGScroll.llOgFltCh = .T.
DIMENSION laRpTrans[8,4]

laRpTrans[1,1] = 'PO'
laRpTrans[1,2] = 'C'
laRpTrans[1,3] = 6
laRpTrans[1,4] = 0

laRpTrans[2,1] = 'Status'
laRpTrans[2,2] = 'C'
laRpTrans[2,3] = 1
laRpTrans[2,4] = 0

laRpTrans[3,1] = 'Complete'
laRpTrans[3,2] = 'D'
laRpTrans[3,3] = 8
laRpTrans[3,4] = 0

laRpTrans[4,1] = 'nStyOrder'
laRpTrans[4,2] = 'N'
laRpTrans[4,3] = 13
laRpTrans[4,4] = 3

laRpTrans[5,1] = 'Receive'
laRpTrans[5,2] = 'N'
laRpTrans[5,3] = 13
laRpTrans[5,4] = 3

laRpTrans[6,1] = 'Damage'
laRpTrans[6,2] = 'N'
laRpTrans[6,3] = 13
laRpTrans[6,4] = 3

laRpTrans[7,1] = 'Cancel'
laRpTrans[7,2] = 'N'
laRpTrans[7,3] = 13
laRpTrans[7,4] = 3

laRpTrans[8,1] = 'Open'
laRpTrans[8,2] = 'N'
laRpTrans[8,3] = 13
laRpTrans[8,4] = 3

gfCrtTmp(lcRpTrans,@laRpTrans,"PO",lcRpTrans,.T.)

*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
*!*	DIMENSION laRpOrder[14,4]
DIMENSION laRpOrder[18,4]
*B608899,1 AHS [End]

IF lcRpPrintBy = 'C'
  laRpOrder[1,1] = 'CtktnO'
  laRpOrder[1,2] = 'C'
  laRpOrder[1,3] = 6
  laRpOrder[1,4] = 0
ELSE
  laRpOrder[1,1] = 'PO'
  laRpOrder[1,2] = 'C'
  laRpOrder[1,3] = 6
  laRpOrder[1,4] = 0
ENDIF

laRpOrder[2,1] = 'Order'
laRpOrder[2,2] = 'C'
laRpOrder[2,3] = 6
laRpOrder[2,4] = 0

laRpOrder[3,1] = 'Style'
laRpOrder[3,2] = 'C'
laRpOrder[3,3] = 19
laRpOrder[3,4] = 0

laRpOrder[4,1] = 'Qty1'
laRpOrder[4,2] = 'N'
laRpOrder[4,3] = 11
laRpOrder[4,4] = 3

laRpOrder[5,1] = 'Qty2'
laRpOrder[5,2] = 'N'
laRpOrder[5,3] = 11
laRpOrder[5,4] = 3

laRpOrder[6,1] = 'Qty3'
laRpOrder[6,2] = 'N'
laRpOrder[6,3] = 11
laRpOrder[6,4] = 3

laRpOrder[7,1] = 'Qty4'
laRpOrder[7,2] = 'N'
laRpOrder[7,3] = 11
laRpOrder[7,4] = 3

laRpOrder[8,1] = 'Qty5'
laRpOrder[8,2] = 'N'
laRpOrder[8,3] = 11
laRpOrder[8,4] = 3

laRpOrder[9,1] = 'Qty6'
laRpOrder[9,2] = 'N'
laRpOrder[9,3] = 11
laRpOrder[9,4] = 3

laRpOrder[10,1] = 'Qty7'
laRpOrder[10,2] = 'N'
laRpOrder[10,3] = 11
laRpOrder[10,4] = 3

laRpOrder[11,1] = 'Qty8'
laRpOrder[11,2] = 'N'
laRpOrder[11,3] = 11
laRpOrder[11,4] = 3

laRpOrder[12,1] = 'TotQty'
laRpOrder[12,2] = 'N'
laRpOrder[12,3] = 11
laRpOrder[12,4] = 3

laRpOrder[13,1] = 'cOrdLine'
laRpOrder[13,2] = 'C'
laRpOrder[13,3] = 6
laRpOrder[13,4] = 0

laRpOrder[14,1] = 'cDyeLot'
laRpOrder[14,2] = 'C'
laRpOrder[14,3] = 10
laRpOrder[14,4] = 0

*B608899,1 AHS Adding Piktkt,PikDate,Complete and Account fields from Ordline in Excel sheet [Start]
laRpOrder[15,1] = 'PikTkt'
laRpOrder[15,2] = 'C'
laRpOrder[15,3] = 6
laRpOrder[15,4] = 0

laRpOrder[16,1] = 'PikDate'
laRpOrder[16,2] = 'D'
laRpOrder[16,3] = 0
laRpOrder[16,4] = 0

laRpOrder[17,1] = 'Complete'
laRpOrder[17,2] = 'D'
laRpOrder[17,3] = 0
laRpOrder[17,4] = 0

laRpOrder[18,1] = 'Account'
laRpOrder[18,2] = 'C'
laRpOrder[18,3] = 5
laRpOrder[18,4] = 0
*B608899,1 AHS [End]

IF lcRpPrintBy = 'C'
  gfCrtTmp(lcRpOrder,@laRpOrder,"cTktno+Order",lcRpOrder,.T.)
ELSE
  gfCrtTmp(lcRpOrder,@laRpOrder,"PO+Order",lcRpOrder,.T.)
ENDIF


SELECT(lcRpOrder)
SET RELATION TO
IF lcRpPrintBy = 'C'
  SET RELATION TO cTktno INTO &lcRpTrans ADDIT
ELSE
  SET RELATION TO PO INTO &lcRpTrans ADDIT
ENDIF


SET RELATION TO "O" + Order INTO ORDHDR ADDITIVE
SET RELATION TO "O" + Order + cOrdLine INTO ORDLINE ADDITIVE
SET RELATION TO "M" + OrdLine.Account INTO Customer ADDITIVE
*!
*!**********************************************************************
*! Name      : lfvPrint
*! Developer : Heba Fathi (HFK)
*! Date      : 02/22/2005
*! Purpose   : Validate FRX name
*!**********************************************************************
*
FUNCTION lfvPrint

IF lcRpBaseOn = 'P'
  lcRpForm = IIF(llDyeLot .AND. llPrint,'SOALOCPD','SOALOCP')
  lcRpStatus = 'BOHCX'
ELSE
  lcRpForm = IIF(llDyeLot .AND. llPrint,'SOALOCSD','SOALOCS')
  lcRpStatus = 'OH'
ENDIF
*!
*!**********************************************************************
*! Name      : lfGetDyelot
*! Developer : Heba Fathi (HFK)
*! Date      : 02/27/2005
*! Purpose   : get dyelot or configuration
*!**********************************************************************
*
FUNCTION lfGetDyelot

*- Get Dyelot field from POSLN file.
lcTmpPo = loOGScroll.gfTempName()
SELECT DISTINCT PO FROM &lcSqlFile INTO CURSOR &lcTmpPo
lcSqlPos = loOgScroll.gfSQLTempName('','PO C(6)',lcTmpPo,'PO') && SQL Temp File
lcPosLnCond = " POSLN.CBUSDOCU = 'P' AND "
lcPosLnCond = lcPosLnCond + IIF(lcRpPrintBy = 'C'," POSLN.CSTYTYPE = 'U' "," POSLN.CSTYTYPE = 'P' ")
lcPosLnStat = " SELECT DISTINCT POSLN.PO,POSLN.STYLE,POSLN.DYELOT From PosLn (Index = PosLn) INNER JOIN " + lcSqlPos + " TmpPos On PosLn.PO = TmpPos.Po "
lcPosLnStat = lcPosLnStat + " WHERE " + lcPosLnCond
lcDyeFile = loOGScroll.gfTempName()
lnPosLnResult = loOGScroll.oRDA.SqlRun(lcPosLnStat,lcDyeFile,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
IF lnPosLnResult > 0
  SELECT &lcDyeFile
  IF lfNoRecord()  && no records found
    RETURN .F.
  ELSE
    lnBuffering = CURSORGETPROP("Buffering","&lcDyeFile")
    =CURSORSETPROP("Buffering",3,"&lcDyeFile")
    INDEX ON PO+Style TAG &lcDyeFile
    SET ORDER TO TAG &lcDyeFile
  ENDIF
ELSE
  =loOGScroll.oRDA.CheckRetResult("SqlRun",lnPosLnResult,.T.)
  RETURN .F.
ENDIF
*!
*!**********************************************************************
*! Name      : lfvStatus
*! Developer : Heba Fathi (HFK)
*! Date      : 02/27/2005
*! Purpose   : Order Status mover
*!**********************************************************************
*
FUNCTION lfvStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
*N000682,1 MMT 02/06/2013 Globalization changes[Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ORDERSTATUS,oAriaApplication.GetHeaderText("LANG_ORDERSTATUS",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 MMT 02/06/2013 Globalization changes[END]
lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    *N000682,1 MMT 02/06/2013 Globalization changes[Start]
*!*	    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
*!*	                              IIF(laRpTarget[lnI] = 'Open','O',;
*!*	                              IIF(laRpTarget[lnI] = 'Hold','H',;
*!*	                              IIF(laRpTarget[lnI] = 'Complete','C',;
*!*	                              IIF(laRpTarget[lnI] = 'Cancelled','X','')))))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Bid,oAriaApplication.GetHeaderText("LANG_SoAlocp_Bid",AHEADERFILE)),'B',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Open,oAriaApplication.GetHeaderText("LANG_SoAlocp_Open",AHEADERFILE)) ,'O',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Hold,oAriaApplication.GetHeaderText("LANG_SoAlocp_Hold",AHEADERFILE)),'H',;
                              IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Complete,oAriaApplication.GetHeaderText("LANG_SoAlocp_Complete",AHEADERFILE)),'C',;
							  IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SoAlocp_Cancelled,oAriaApplication.GetHeaderText("LANG_SoAlocp_Cancelled",AHEADERFILE)),'X','')))))
    *N000682,1 MMT 02/06/2013 Globalization changes[END]
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),IIF(lcRpBaseOn='P','BOHCX','OH'),ALLTRIM(lcRpStatus))
*!
*!**********************************************************************
*! Name      : RefreshStatus
*! Developer : Heba Fathi (HFK)
*! Date      : 02/27/2005
*! Purpose   : Refresh Status
*!**********************************************************************
*
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
*!-------------------------------------------------------------------------------------!*

