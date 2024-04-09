*:************************************************************************
*: Program file  : MFCTDT.Prg
*: Program desc. : CUTTING TICKET DETAIL REPORT
*: System        : Aria 4 XP
*: Module        : MF
*: Developer     : Saeed Mohammed Mostafa (SMM)
*: Date          : 07/28/2005
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFCTDT
*:************************************************************************
*: Modifications    :  #N037715
*:*B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears
*:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[T20091106.0012]
*:************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include R:\aria4xp\reports\ma\mfctdt.h

*N000682,1 MMT 02/11/2013 Globalization changes[ENd]
LOCAL lcCurName  && Holds Temp Cursor Name
LOCAL llFound    && Whether there is a filter or not
LOCAL lcSQLCur   && Holds Temp SQL File Name
LOCAL lcTemp     && Holds Temp File Name
LOCAL lcSQLTmp
LOCAL lcMFGTmp
*
*LOCAL lcFields   && Holds required Fields
private lcFields   && Holds required Fields
*
LOCAL lcWhereCon && Holds Where Con
LOCAL lcJoin     && Holds Join Stmt

IF loOgScroll.llOGFltCh && OG Filters changed
  lcWhereCon = "POSHDR.CBUSDOCU = 'P' and POSHDR.CSTYTYPE='U' " && Cut Ticket
  lcJoin = " POSHDR POSHDR (index=POSHDR) Inner"+;
           " join POSLN POSLN (index=POSLN) on POSHDR.CBUSDOCU = POSLN.CBUSDOCU and"+;
           " POSHDR.CSTYTYPE = POSLN.CSTYTYPE and POSHDR.PO = POSLN.PO "

  * Check if there is a filter on Cut Ticket Number
  lcCurName = lfCheckFilter(1, 'POSHDR.PO')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName  	
    llFound = ( RECCOUNT() > 0)
    IF llFound
      lcSQLCur = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO') && SQL Temp File
      IF EMPTY(lcSQLCur)
	      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpPO on TmpPO.PO = POSHDR.PO "
    ENDIF
  ENDIF
  * Check if there is a filter on Style
  lcCurName = lfCheckFilter(1, 'POSLN.STYLE')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName  	
    llFound = ( RECCOUNT() > 0)
    IF llFound
      lcTemp = loOgScroll.gfTempName()
      SELECT a.Style FROM Style a INNER join &lcCurName b ON a.cStyMajor=b.cStyMajor Into cursor &lcTemp
      lcSQLCur = loOgScroll.gfSQLTempName('','STYLE C(19)', lcTemp,'Style') && SQL Temp File
      IF EMPTY(lcSQLCur)
	    *-- SQL connection error. can't open the report
        =gfModalG;en('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty1 on TmpSty1.Style = POSLN.STYLE "
    ENDIF
  ENDIF
  * Check if there is a filter on Fabric
  lcCurName = lfCheckFilter(1, 'STYLE.FABRIC')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName
    llFound  = ( RECCOUNT() > 0)
    IF llFound
      lcTemp = loOgScroll.gfTempName()
      SELECT a.Style FROM Style a INNER join &lcCurName b ON a.Fabric = b.cStyMajor Into cursor &lcTemp
      lcSQLCur = loOgScroll.gfSQLTempName('','STYLE C(19)',lcTemp,'STYLE') && SQL Temp File
      IF EMPTY(lcSQLCur)
      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty2 on TmpSty2.Style = POSLN.STYLE "
    ENDIF
  ENDIF

*B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---Begin
*!*    * Check if there is a filter on Vendor
*!*    lcCurName = lfCheckFilter(1, 'APVENDOR.CVENDCODE')  	
*!*    IF !EMPTY(lcCurName)
*!*      SELECT &lcCurName  	
*!*      llFound = ( RECCOUNT() > 0)
*!*      IF llFound
*!*        lcSQLCur = loOgScroll.gfSQLTempName('','CVENDCODE C(8)',lcCurName,'CVENDCODE') && SQL Temp File
*!*        IF EMPTY(lcSQLCur)
*!*  	      *-- SQL connection error. can't open the report
*!*          =gfModalGen('TRM00416B40011','ALERT')
*!*          RETURN .F.
*!*        ENDIF
*!*        lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpVend on TmpVend.CVENDCODE = POSHDR.VENDOR "
*!*      ENDIF
*!*    ENDIF
* Check if there is a filter on Vendor

  lcCurName = lfCheckFilter(1, 'APVENDOR.CVENDCODE')
  IF !EMPTY(lcCurName)
    SELECT &lcCurName
    llFound = ( RECCOUNT() > 0)
    IF llFound
      lcVenPoTMP= loOGScroll.gfTempName()
      lcSQLCur1 = loOgScroll.gfSQLTempName('','CVENDCODE C(8)',lcCurName,'CVENDCODE') && SQL Temp File
      lcSqlStat="Select Distinct CTKTNO FROM MFGOPRHD inner join "+lcSQLCur1+" TmpVend1 on TmpVend1.CVENDCODE =MFGOPRHD.Ccontcode  and LInhouse='0'"
      lnResult = loOgScroll.oRDA.SqlRun(lcSqlStat, lcVenPoTMP,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
      IF lnResult <>1
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcSQLCur = loOgScroll.gfSQLTempName('','CTKTNO C(6)',lcVenPoTMP,'CTKTNO') && SQL Temp File
      IF EMPTY(lcSQLCur)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpVend on TmpVend.CTKTNO= POSHDR.po "
    ENDIF
  ENDIF
*B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---End


  * Check if there is a filter on Style Group
  lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')  	
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcCon = "'" + lcCon + "'"
    lcTemp = loOgScroll.gfTempName()
    SELECT Style FROM Style WHERE cstygroup in (lcCon) Into cursor &lcTemp
    WAIT WINDOW lccon
    lcSQLCur = loOgScroll.gfSQLTempName('','STYLE C(19)',lcTemp,'STYLE') && SQL Temp File
    IF EMPTY(lcSQLCur)
    *-- SQL connection error. can't open the report
      =gfModalGen('TRM00416B40011','ALERT')
      RETURN .F.
    ENDIF
    lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty3 on TmpSty3.STYLE = POSLN.STYLE "
  ENDIF
  * Check if there is a filter on Division
  lcCurName = lfCheckFilter(1, 'POSHDR.CDIVISION')
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CDIVISION IN ('" + lcCon + "')"
  ENDIF
  * Check if there is a filter on Season
  lcCurName = lfCheckFilter(1, 'POSHDR.SEASON')
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.SEASON IN ('" + lcCon + "')"
  ENDIF
  * Check if there is a filter on Entered Date
  lcCurName = lfCheckFilter(1, 'POSHDR.ENTERED')
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.ENTERED Between '" + lcCon + "'"
  ENDIF
  * Check if there is a filter on Completed Date
  lcCurName = lfCheckFilter(1, 'POSHDR.COMPLETE')
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.COMPLETE Between '" + lcCon + "'"
  ENDIF
  * Check if there is a filter on Season
  lcCurName = lfCheckFilter(1, 'POSHDR.STATUS')
  IF !EMPTY(lcCurName)
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.STATUS IN ('" + lcCon + "')"
  ENDIF
  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  lcSQLTMP   = loOGScroll.gfTempName()
  lcFields   = " POSHDR.PO,POSHDR.STATUS,POSHDR.ENTERED,POSHDR.COMPLETE,POSLN.STYLE,POSHDR.SEASON,POSHDR.CDIVISION,"
  *:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[Start]
*!*	  lcFields   = lcFields + "POSHDR.VENDOR,POSLN.Trancd, "
  lcFields   = lcFields + "POSHDR.VENDOR,POSLN.Trancd,POSLN.SCALE,'     ' As SIZ1,'     ' As SIZ2,'     ' As SIZ3,"+;
						  "'     ' As SIZ4,'     ' As SIZ5,'     ' As SIZ6,'     ' As SIZ7,'     ' As SIZ8,"
  *:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[End]
  lcFields   = lcFields + "POSLN.Qty1,POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,POSLN.QTY6,POSLN.QTY7,POSLN.QTY8 "
  lcFields   = lcFields + ",POSHDR.VENDOR as Contr1,POSHDR.VENDOR as Contr2,POSHDR.VENDOR as Contr3 "
    
   *E303790,MHM 04/09/2017 adding udfs to cut tickit detail report [begin]
  lcpathvar =   UPPER(oAriaApplication.CLIENTAPPLICATIONHOME)
  lcpathvar =   strtran(lcpathvar ,"PRGS\","")+"CARMAIN.FXP"
  IF FILE(lcpathvar)
    SET PROC TO (lcpathvar) addit
     = gfDoTriger('MFCTDT',UPPER(PADR('UPDCTD',10)))
   ENDIF 
   *E303790,MHM 04/09/2017 adding udfs to cut tickit detail report [End]
   
   
   
  
  lcSQLStmt  = "SELECT " + lcFields + " FROM " + lcJoin + " WHERE " + lcWhereCon
  lnResult   = loOgScroll.oRDA.SqlRun(lcSQLStmt, lcSQLTMP,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)

  IF lnResult = 1
    IF loOgScroll.FileExist(oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" )
      ERASE (oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" )
    ENDIF
    *B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---Begin
    *=gfOpenTable(oAriaApplication.Datadir+'MFGOPRHD','TKTOPER','SH')
    =gfOpenTable('MFGOPRHD','TKTOPER','SH')
    *B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---End

    SELECT (lcSQLTMP)
    SCAN

      *:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[Start]
      IF gfSEEK(&lcSQLTMP..STYLE,'STYLE')
        REPLACE &lcSQLTMP..SCALE WITH STYLE.SCALE
        =gfSeek('S'+STYLE.SCALE,'SCALE')
        FOR lnx = 1 TO 8
          lcX = ALLTRIM(STR(lnX))
          REPLACE (lcSQLTMP+'.SIZ'+lcX) WITH SCALE.SZ&lcX
        ENDFOR
      ENDIF
	  *:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[End]

      IF gfSEEK('M'+PO,'MFGOPRHD')
        SELECT MFGOPRHD
       *B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---Begin
       *SCAN REST WHILE 'M'+&lcSQLTMP..PO = 'M'+MFGOPRHD.ctktno AND !MFGOPRHD.lInHouse
        SCAN REST WHILE 'M'+&lcSQLTMP..PO = 'M'+MFGOPRHD.ctktno FOR !MFGOPRHD.lInHouse
       *B132051,1 AYM whene specify a contractor no records appears and Contractors did noy appears ---End

          IF EMPTY(&lcSQLTMP..Contr1)
            REPLACE &lcSQLTMP..Contr1 WITH MFGOPRHD.CCONTCODE
          ELSE
            IF EMPTY(&lcSQLTMP..Contr2) AND MFGOPRHD.CCONTCODE <> &lcSQLTMP..Contr1
              REPLACE &lcSQLTMP..Contr2 WITH MFGOPRHD.CCONTCODE
            ELSE
              IF EMPTY(&lcSQLTMP..Contr3) AND MFGOPRHD.CCONTCODE <> &lcSQLTMP..Contr1 ;
                 AND MFGOPRHD.CCONTCODE <> &lcSQLTMP..Contr2
                REPLACE &lcSQLTMP..Contr3 WITH MFGOPRHD.CCONTCODE
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
      SELECT (lcSQLTMP)
    ENDSCAN
    COPY TO oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF"
    =lfAdjustCRSettings()
  ELSE
	  *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF
ENDIF

IF !USED(lcRPHdrTmp)
  USE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" IN 0
ENDIF

SELECT (lcRPHDRTmp)
IF RECCOUNT() = 0
  *-- There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
   USE IN &lcRPHdrTmp
   RETURN .F.
ENDIF

USE IN &lcRPHdrTmp

*-- RETURN .T.

=gfDispRe()

*********************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected and returns the
*! 			   Cursor name if In Range and the expression if mover
*********************************************************************
FUNCTION lfCheckFilter()
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS 	
  DO CASE
	CASE lnArrayType = 1
	  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
	  ELSE
  	    lcReturn = ""	
      ENDIF
	CASE lnArrayType = 2
	  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
	  ELSE
  	    lcReturn = ""	
      ENDIF
	CASE lnArrayType = 3
	  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
	  ELSE
  	    lcReturn = ""	
      ENDIF
	OTHERWISE :
		lcReturn = ""
  ENDCASE	
  RETURN lcReturn
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
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
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
*:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[Start]
*!*	DIMENSION loOgScroll.laCRParams[7,2]
DIMENSION loOgScroll.laCRParams[8,2]
*:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[End]

loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF"
*loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRPMFGTmp  + ".DBF"
*loOgScroll.laCRTables[3] = oAriaApplication.DataDir +  "codes.DBF"
*loOgScroll.laCRTables[4] = oAriaApplication.DataDir +  "codes.DBF"

loOGScroll.cCROrientation='L'

loOgScroll.laCRParams[1,1] = 'Layout'
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	IF lcRPForm = 'S'
*!*	  loOgScroll.laCRParams[1,2] = 'Summary'
*!*	ELSE
*!*	  loOgScroll.laCRParams[1,2] = 'Detail'
*!*	ENDIF
IF lcRPForm = 'S'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_SUMMARY
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMARY,oAriaApplication.GetHeaderText("LANG_SUMMARY",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_DETAIL
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAIL,oAriaApplication.GetHeaderText("LANG_DETAIL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF
*N000682,1 MMT 02/11/2013 Globalization changes[End]
loOgScroll.laCRParams[2,1] = 'OpTitle'
loOgScroll.laCRParams[2,2] = lcRpTitle

loOgScroll.laCRParams[3,1] = 'SortBy'
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	DO CASE
*!*	  CASE lcRPSortBy = 'C'
*!*	    loOgScroll.laCRParams[3,2] = 'Cut Ticket #'
*!*	  CASE lcRPSortBy = 'S'
*!*	    loOgScroll.laCRParams[3,2] = 'Style'
*!*	  CASE lcRPSortBy = 'O'
*!*	    loOgScroll.laCRParams[3,2] = 'Color'
*!*	  CASE lcRPSortBy = 'D'
*!*	    loOgScroll.laCRParams[3,2] = 'Date'
*!*	ENDCASE	
DO CASE
  CASE lcRPSortBy = 'C'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[3,2] = LANG_CUTTKT
loOgScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUTTKT,oAriaApplication.GetHeaderText("LANG_CUTTKT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE lcRPSortBy = 'S'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[3,2] = LANG_STYLE
loOgScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLE,oAriaApplication.GetHeaderText("LANG_STYLE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE lcRPSortBy = 'O'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[3,2] = LANG_COLOR
loOgScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLOR,oAriaApplication.GetHeaderText("LANG_COLOR",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE lcRPSortBy = 'D'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[3,2] = LANG_DATE
loOgScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DATE,oAriaApplication.GetHeaderText("LANG_DATE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE	
*N000682,1 MMT 02/11/2013 Globalization changes[End]
loOgScroll.laCRParams[4,1] = 'ReportName'
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*loOgScroll.laCRParams[4,2] = 'Cut Ticket Detail Report'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[4,2] = LANG_REPORTTTL
loOgScroll.laCRParams[4,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPORTTTL,oAriaApplication.GetHeaderText("LANG_REPORTTTL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[End]
LOCAL lcTrxType
lcTrxType = ""
* Check if there is a filter on Season
lcCurName = lfCheckFilter(1, 'POSLN.TRANCD')
IF !EMPTY(lcCurName)
  lcCon = lcCurName
  lcCon = STRTRAN(lcCon,"|","")
  lcTrxType = lcCon
ENDIF

loOgScroll.laCRParams[5,1] = 'Types'
loOgScroll.laCRParams[5,2] = IIF(EMPTY(lcTrxType),"BRDCO",lcTrxType)

loOgScroll.laCRParams[6,1] = 'StyMask'
loOgScroll.laCRParams[6,2] = LEN(gfItemMask('PM',"",001))
*:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[Start]
*!*	loOgScroll.laCRParams[7,1] = 'lcstytitle'
*!*	loOgScroll.laCRParams[7,2] = gfItemMask("HI")
loOgScroll.laCRParams[7,1] = 'llRpPrtScl'
loOgScroll.laCRParams[7,2] = IIF(llRpPrtScl,1,0)
loOgScroll.laCRParams[8,1] = 'lcstytitle'
loOgScroll.laCRParams[8,2] = gfItemMask("HI")
*:*B609116,1 HES 12/24/2009 Add Option to let user print Scale Sizes[End]
