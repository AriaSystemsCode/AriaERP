*:***************************************************************************
*: Program file  : MFCTHDR
*: Program desc. : Cutting Ticket Header Report 
*: Issue #       : 037717
*: System        : Aria 4 XP
*: Module        : MANUFACTURING (MF)
*: Developer     : Saeed Mohammed Mostafa (SMM)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO MFCTHDR
*:***************************************************************************
*: Modifications  :
*: B607914,1 MMT 12/28/2006 bug or wrong contractor value T20061215.0015     
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019]     
*:***************************************************************************
#INCLUDE R:\Aria4XP\Reports\mfcthdr.h
LOCAL lcCurName  && Holds Temp Cursor Name 
LOCAL llFound    && Whether there is a filter or not
LOCAL lcSQLCur   && Holds Temp SQL File Name
LOCAL lcTemp     && Holds Temp File Name
LOCAL lcSQLTMP
LOCAL lcMFGTmp
LOCAL lcFields   && Holds required Fields
LOCAL lcWhereCon && Holds Where Con
LOCAL lcJoin     && Holds Join Stmt
LOCAL lcPOEXP

*N037717,1 MMT 01/04/2006 Convert Report to SQL [Start]
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]     
*lcMaj      = gfItemMask('PM')             && Get the major of the style
lcMaj      = gfItemMask('PM',.F.,IIF(oAriaApplication.ActiveModuleID = 'MA','0002','0001'))             && Get the major of the style
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]  
lnMajSize  = LEN(lcMaj)                   && Length of the major
*N037717,1 MMT 01/04/2006 Convert Report to SQL [End] 

loOgScroll.cCRorientation = 'P'
IF loOgScroll.llOGFltCh && OG Filters changed
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]     
  *lcWhereCon = "POSHDR.CBUSDOCU = 'P' and POSHDR.CSTYTYPE='U' " 
  lcWhereCon = "POSHDR.CBUSDOCU = 'P' and POSHDR.CSTYTYPE='"+IIF(oAriaApplication.ActiveModuleID = 'MA',"F","U")+"' "   
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]     
  * and MFGOPRHD.CIMTYP = 'M' " && Cut Ticket 
  lcJoin = " POSHDR POSHDR (index=POSHDR) Left"+;
           " outer join POSLN POSLN (index=POSLN) on POSHDR.CBUSDOCU = POSLN.CBUSDOCU and"+;
           " POSHDR.CSTYTYPE = POSLN.CSTYTYPE and POSHDR.PO = POSLN.PO "+;
           " Left outer Join MFGOPRHD MFGOPRHD on POSHDR.PO = MFGOPRHD.CTKTNO left outer Join " && Inner Join MFGOPRDT MFGOPRDT " 
  *B607914,1 MMT 12/28/2006 bug or wrong contractor value [Start]         
  *lcJoin = lcJoin + " (Select ctktno,sum(nLotTotQty) as nLotTotQty,Trancd as trancd1 from mfgoprdt where cImtyp='M'"+;
  		     " group by cTktNo,trancd) mfgoprdt on mfgoprdt.ctktno=POSHDR.PO "
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]       		     
*   lcJoin = lcJoin + " (Select ctktno,sum(nLotTotQty) as nLotTotQty,Trancd as trancd1 from mfgoprdt where cImtyp='M'"+;
  		     " and coprCode in (Select coprCode from  mfgoprhd where "+;
  		     " cImtyp='M' and Ctktno =  mfgoprdt.ctktno and "+;
  		     " Coperseq = (Select MIN(Coperseq) from mfgoprhd where cImtyp='M'"+;
  		     " AND Ctktno =  mfgoprdt.ctktno  group by ctktno)) group by cTktNo,trancd) mfgoprdt on mfgoprdt.ctktno=POSHDR.PO "
   lcJoin = lcJoin + " (Select ctktno,sum(nLotTotQty) as nLotTotQty,Trancd as trancd1 from mfgoprdt where cImtyp='"+IIF(oAriaApplication.ActiveModuleID = 'MA',"T","M")+"'"+;
  		     " and coprCode in (Select coprCode from  mfgoprhd where "+;
  		     " cImtyp='"+IIF(oAriaApplication.ActiveModuleID = 'MA',"T","M")+"' and Ctktno =  mfgoprdt.ctktno and "+;
  		     " Coperseq = (Select MIN(Coperseq) from mfgoprhd where cImtyp='"+IIF(oAriaApplication.ActiveModuleID = 'MA',"T","M")+"'"+;
  		     " AND Ctktno =  mfgoprdt.ctktno  group by ctktno)) group by cTktNo,trancd) mfgoprdt on mfgoprdt.ctktno=POSHDR.PO "
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]     
  *B607914,1 MMT 12/28/2006 bug or wrong contractor value [End]         
  * Check if there is a filter on STATUS
  IF !EMPTY(lcRPStatus)
    lcWhereCon = lcWhereCon + " and POSHDR.STATUS IN ('" + lcRPStatus + "') "
  ENDIF
  
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
*     lcPOEXP = " POSHDR.PO IN ("
*     SCAN  
*      lcPOEXP = lcPOEXP + "'" + &lcCurName..PO + "',"
*     ENDSCAN
*     lcPOEXP = LEFT(lcPOEXP ,LEN(lcPOEXP)-1) + ")"
*     lcWhereCon = lcWhereCon + ' and ' + lcPOEXP 
    ENDIF
  ENDIF
  * Check if there is a filter on Style
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]     
  IF oAriaApplication.ActiveModuleID = 'MF'
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End] 
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
          =gfModalGen('TRM00416B40011','ALERT')
          RETURN .F.
        ENDIF
        lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty1 on TmpSty1.Style = POSLN.STYLE "
      ENDIF
    ENDIF
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  ENDIF
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
  * Check if there is a filter on Fabric
  lcCurName = lfCheckFilter(1, 'STYLE.FABRIC')  	
  IF !EMPTY(lcCurName) 
    SELECT &lcCurName    
    llFound  = ( RECCOUNT() > 0) 
    IF llFound
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
      IF oAriaApplication.ActiveModuleID = 'MF'
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
        lcTemp = loOgScroll.gfTempName()
        SELECT a.Style FROM Style a INNER join &lcCurName b ON a.Fabric = b.cStyMajor Into cursor &lcTemp
        lcSQLCur = loOgScroll.gfSQLTempName('','STYLE C(19)',lcTemp,'STYLE') && SQL Temp File
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
      ELSE
        lcTemp = lcCurName
        lcSQLCur = loOgScroll.gfSQLTempName('','STYLE C('+STR(lnMajSize)+')',lcTemp,'cStyMajor') && SQL Temp File
      ENDIF
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
      IF EMPTY(lcSQLCur)
      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
      IF oAriaApplication.ActiveModuleID = 'MF'
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
        lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty2 on TmpSty2.Style = POSLN.STYLE "
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
      ELSE
        lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpSty2 on  substring(POSLN.STYLE ,1,"+STR(lnMajSize)+")= TmpSty2.Style "      
      ENDIF
      *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
    ENDIF
  ENDIF
  * Check if there is a filter on Vendor
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  IF oAriaApplication.ActiveModuleID = 'MF'
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
    lcCurName = lfCheckFilter(1, 'APVENDOR.CVENDCODE')  	
    IF !EMPTY(lcCurName)
      SELECT &lcCurName  	
      llFound = ( RECCOUNT() > 0) 
      IF llFound
        lcSQLCur = loOgScroll.gfSQLTempName('','CVENDCODE C(8)',lcCurName,'CVENDCODE') && SQL Temp File
        IF EMPTY(lcSQLCur)  
 	     *-- SQL connection error. can't open the report
          =gfModalGen('TRM00416B40011','ALERT')
          RETURN .F.
        ENDIF
        lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpVend on TmpVend.CVENDCODE = POSHDR.VENDOR "
      ENDIF
    ENDIF
    
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
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  ELSE
    lcCurName = lfCheckFilter(1, 'POSHDR.CWARECODE')  	
    IF !EMPTY(lcCurName) 
      lcTemp = lcCurName 
      lcSQLCur = loOgScroll.gfSQLTempName('','CWARECODE C(6)',lcTemp,'CWARECODE') && SQL Temp File
      IF EMPTY(lcSQLCur)
        *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcJoin = lcJoin + " inner join " + lcSQLCur + " TmpStyW on TmpStyW.CWARECODE = POSLN.CWARECODE "
    ENDIF
  ENDIF
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
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
  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  
  lcSQLTMP = loOGScroll.gfTempName()
  lcFields = " POSHDR.PO,POSHDR.STATUS,POSHDR.ENTERED,POSHDR.COMPLETE,POSLN.STYLE,POSHDR.SEASON,POSHDR.CDIVISION,"
  lcFields = lcFields + "POSHDR.NSTYORDER,POSHDR.RECEIVE,POSHDR.DAMAGE,POSHDR.CANCEL,POSHDR.[OPEN], "
  lcFields = lcFields + "POSHDR.nfactcost1,POSHDR.nfactcost2,POSHDR.nfactcost3,POSHDR.nfactcost4,POSHDR.nfactcost5,"
  lcFields = lcFields + "POSHDR.nfactcost6,POSHDR.nfactcost7,POSHDR.nfcost1,POSHDR.nfcost2,POSHDR.nfcost3,"
  lcFields = lcFields + "POSHDR.nfcost4,POSHDR.nfcost5,POSHDR.nfcost6,POSHDR.nfcost7,POSHDR.nflancost1,"
  lcFields = lcFields + "POSHDR.nflancost2,POSHDR.nflancost3,POSHDR.nflancost4,POSHDR.nflancost5,POSHDR.nflancost6,"
  lcFields = lcFields + "POSHDR.nflancost7,POSHDR.VENDOR,POSLN.Trancd,MFGOPRHD.CCONTCODE,MFGOPRHD.COPRCODE,"
  lcFields = lcFields + "MFGOPRHD.CCONTNAME,MFGOPRDT.nlottotqty,MFGOPRDT.trancd1,MFGOPRHD.lInHouse "
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  lcFields = lcFields + ",POSLN.CWARECODE" 
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]

  lcSQLStmt = "SELECT " + lcFields + " FROM " + lcJoin + " WHERE " + lcWhereCon
  lnResult  = loOgScroll.oRDA.SqlRun(lcSQLStmt, lcSQLTMP,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF" )
      ERASE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF" )
    ENDIF
    SELECT (lcSQLTMP)
    COPY TO (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF")
    USE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF") IN 0 EXCLUSIVE 
    ALTER Table (lcRPHDRTmp) ADD COLUMN COPDESC C(30)
    SELECT (lcRPHDRTmp)
    SCAN 
      IF SEEK('N'+&lcRPHDRTmp..CoprCode+'N'+'MFGCODE   ','Codes','CODES')
        REPLACE COPDESC WITH  Codes.CDISCREP
      ENDIF 
    ENDSCAN 
    IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRpTmpNote + ".DBF" )
      ERASE (oAriaApplication.WorkDir + lcRpTmpNote + ".DBF" )
    ENDIF
    CREATE TABLE (oAriaApplication.WorkDir + lcRpTmpNote + ".DBF") (PO C(6), mNote MEMO,lhasnote N(1,0))
    SELECT(lcRpTmpNote)
    INDEX on PO TAG Po
    IF lcRPFormat = 'MFCTHDRL'
	  IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRpTmpStyle + ".DBF" )
	    ERASE (oAriaApplication.WorkDir + lcRpTmpStyle + ".DBF" )
	  ENDIF
	  CREATE TABLE (oAriaApplication.WorkDir + lcRpTmpStyle + ".DBF") (Style C(19), Desc C(20), SeasDesc C(30),DivDesc C(30),PO C(6))
	  SELECT(lcRpTmpStyle)
	  INDEX ON Style TAG Style 
    ENDIF 
    *-----------
    SELECT (lcSQLTMP)
  	SCAN  
*!*	  	     REPLACE entered WITH TTOD(entered)	
*!*	  	     replace complete WITH TTOD(complete)
	    IF SEEK(PO,lcRpTmpNote)
	       LOOP 
	    ENDIF 
        *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
        *IF SEEK('I'+Po,'Notepad') 
        IF SEEK(IIF(oAriaApplication.ActiveModuleID = 'MA','O','I')+Po,'Notepad') 
        *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
	      SELECT (lcRpTmpNote)
	      APPEND BLANK 
	      REPLACE mNote    WITH Notepad.Mnotes ,;
	              PO       WITH &lcSQLTMP..Po ,;
	              lhasnote WITH 1
	     ELSE
	       SELECT (lcRpTmpNote)
	       APPEND BLANK  
	       REPLACE PO       WITH &lcSQLTMP..Po ,;
	               lhasnote WITH 0
	     ENDIF 
         IF lcRPFormat = 'MFCTHDRL'
	       IF SEEK(&lcSQLTMP..Style, lcRpTmpStyle)
	         LOOP
	       ENDIF
	       m.cDivDes = ''
	       m.cSesDes = ''
	       m.copCode = ''
	       =SEEK('N'+&lcSQLTMP..CDIVISION+'N'+'CDIVISION','Codes','CODES')
	       m.cDivDes = Codes.CDISCREP
	       =SEEK('N'+&lcSQLTMP..SEASON+'N'+'SEASON','Codes','CODES')
	       m.cSesDes = Codes.CDISCREP

	       SELECT(lcRpTmpStyle)
	       APPEND  BLANK 
	       REPLACE Style    WITH &lcSQLTMP..Style,;
	       		   desc		WITH IIF(SEEK(&lcSQLTMP..Style,'Style','Style'),Style.Desc,''),;
	               SeasDesc WITH m.cSesDes,;
	               DivDesc  WITH m.cDivDes,;
	               Po		WITH &lcSQLTMP..PO
 	     ENDIF   
	   ENDSCAN 
    *------------
*  	INSERT INTO (oAriaApplication.WorkDir + lcRPTmpNote + ".DBF") SELECT Key, mNotes FROM Notepad WHERE Type = 'I'
    IF lcRPFormat = 'MFCTHDRL'   
      USE IN (lcRpTmpStyle)
    ENDIF 
    USE IN (lcRPTmpNote)
    =lfAdjustCRSettings()
  ELSE
	  *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF	
ENDIF

IF !USED(lcRPHdrTmp)
  USE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF") IN 0 EXCLUSIVE 
ENDIF

*!*	IF !FIELD('COPDESC',lcRPHdrTmp)
*!*	  ALTER Table (lcRPHDRTmp) ADD COLUMN COPDESC C(30)
*!*	  SELECT (lcRPHDRTmp)
*!*	  SCAN 
*!*	    IF SEEK('N'+&lcRPHDRTmp..CoprCode+'N'+'MFGCODE   ','Codes','CODES')
*!*	      REPLACE COPDESC WITH  Codes.CDISCREP
*!*	    ENDIF 
*!*	  ENDSCAN 
*!*	ENDIF 

SELECT (lcRPHDRTmp)
IF RECCOUNT() = 0
  *-- There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
  USE IN (lcRPHdrTmp)
  RETURN .F.
ENDIF

IF USED(lcRPTmpNote)
  USE IN (lcRPTmpNote)
ENDIF
IF lcRPFormat = 'MFCTHDRL' AND USED(lcRpTmpStyle)
  USE IN (lcRpTmpStyle)
ENDIF 

USE IN (lcRPHdrTmp)
*--MMT
*!*	USE (oAriaApplication.WorkDir + lcRPHdrTmp + ".DBF") IN 0 EXCLUSIVE 
*!*	ALTER table (lcRPHdrTmp) alter COLUMN entered DATE(8)  
*!*	ALTER table (lcRPHdrTmp) alter COLUMN COMPLETE DATE(8)  
*!*	USE IN (lcRPHdrTmp)   
*--MMT
=gfDispRe()
RETURN

*!*	*--------------------------- OLD
*!*	XTRNCD = '2'
*!*	SCHOICE  = 'C' &&lcSortBy

*!*	QTOPIC='CUTTING TICKET HEADER REPORT'

*!*	llApLink  = IIF(ALLTRIM(gfGetMemVar('M_ApLink',gcAct_Comp)) = 'Y', .T., .F.)
*!*	lcApDir   = gcDataDir
*!*	RELEASE ALL LIKE M_*

*!*	R_TITLE = 'CUTTING TICKET HEADER REPORT'
*!*	XREPORT = 'MFCTHDR'
*!*	R_WIDTH = 'W'  && Report form width
*!*	XTITLE  = lcOpTtl
*!*	*N2,1 TMI 03/26/95 (BEGIN) Restore costing items name.
*!*	DIMENSION laEst [5]
*!*	DIMENSION laLan [5]
*!*	DIMENSION laAct [5]

*!*	DIMENSION laSEst [5]
*!*	DIMENSION laSLan [5]
*!*	DIMENSION laSAct [5]
*!*	*--HDM
*!*	STORE 0 TO laSEst , laSAct , laSlan ,laEst , laAct , laLan ,lnLan ,lnSubEst,lnSubLan,lnSubAct
*!*	*--HDM
*!*	RELEASE ALL LIKE M_*
*!*	STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
*!*	STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct
*!*	*N6,1 TMI 03/26/95 (END) Restore costing items name.

*!*	*B601001,1 MFM 03/14/96 Added the initialization of 'lnCon_Bud'.
*!*	STORE 0 TO lnPC_BUD , lnCON_ISS , lnPC_REC , lnCON_REC ,lnPC_DAM , lnCON_DAM ,lnPC_CAN ,;
*!*	lnPC_OPN , lnCON_OPN,lnCon_Bud

*!*	STORE 0 TO lnPCS_BUD , lnCONT_ISS , lnPCS_REC , lnCONT_REC ,lnPCS_DAM , lnCONT_DAM ,lnPCS_CAN ,;
*!*	lnPCS_OPN , lnCONT_OPN

*!*	STORE 0 TO laSEst , laSAct , laSlan

*!*	STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
*!*	STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct, lnFinA_L , lnFinL_E ,lnFinA_E ,lnFinAbL ,lnFinLbE , lnFinAbE
*!*	*E700037,1 TMI 03/22/95 (END) Restore costing items name.
*!*	*-----------------------------------------------------------------------------------------
*!*	**** TWO FILTERS ARE USED BECAUSE THE LENGTH OF ONE GETS TOO LONG ****
*!*	  XFILTER = ' '
*!*	  XFILTER1= ' '         && SECOND FILTER USED TO SET FILTER ON TEMP FILE
*!*	  ERRFLAG = .F.
*!*	  IF !EMPTY(XVEN)     &&  HDM<>' '
*!*	    XFILTER1 = XFILTER1 + '.AND. (XVEN=CNTR1 .OR. XVEN=CNTR2 .OR. XVEN=CNTR3)'
*!*	  ENDIF

*!*	  IF LCT<>' '.OR.HCT<>' '
*!*	      IF HCT=' '
*!*	         HCT=LCT
*!*	      ENDIF
*!*	      IF HCT<LCT
*!*	         WAIT WINDOW 'CUTTING TICKET FROM/THRU ERROR!'
*!*	         ERRFLAG = .T.
*!*	      ENDIF
*!*	      XFILTER = XFILTER +'.AND.CUTTKT>=LCT.AND.CUTTKT<=HCT'
*!*	  ENDIF

*!*	  IF DTOC(LE)<>' '.OR.DTOC(HE)<>' '
*!*	      IF DTOC(HE)=' '
*!*	         HE=LE
*!*	      ENDIF
*!*	      IF HE<LE
*!*	         WAIT WINDOW 'FROM/THRU ERROR - DATE ENTERED!'
*!*	         ERRFLAG = .T.
*!*	      ENDIF
*!*	      XFILTER = XFILTER+'.AND.ENTERED>=LE.AND.ENTERED<=HE'
*!*	  ENDIF

*!*	  IF DTOC(LC)<>' '.OR.DTOC(HC)<>' '
*!*	      IF DTOC(HC)=' '
*!*	         HC=LC
*!*	      ENDIF
*!*	      IF HC<LC
*!*	         WAIT WINDOW 'FROM/THRU ERROR - COMPLETION DATE!'
*!*	         ERRFALG = .T.
*!*	      ENDIF
*!*	      XFILTER = XFILTER+'.AND.COMPLETE>=LC.AND.COMPLETE<=HC'
*!*	  ENDIF

*!*	  XSTATUS1 = TRIM(XSTATUS)
*!*	  IF LEN(TRIM(XSTATUS1))<>0
*!*	     Y=1
*!*	     DO WHILE Y<=LEN(XSTATUS1)
*!*	       IF !(SUBSTR(XSTATUS1,Y,1) $ 'OCAHXS')
*!*	        WAIT WINDOW 'INVALID STATUS CODE !!!'
*!*	        ERRFLAG=.T.
*!*	        EXIT
*!*	       ENDIF
*!*	       Y=Y+1
*!*	     ENDDO
*!*	     IF .NOT. ERRFLAG
*!*	      XFILTER = XFILTER+'.AND.STATUS$XSTATUS1'
*!*	     ENDIF
*!*	  ENDIF

*!*	  IF !EMPTY(LSTYLE) .OR. !EMPTY(HSTYLE)
*!*	      IF EMPTY(HSTYLE)
*!*	         HSTYLE = LSTYLE
*!*	      ENDIF
*!*	      IF LSTYLE > HSTYLE
*!*	         WAIT WINDOW 'FROM/THRU STYLE ERROR!'
*!*	         ERRFLAG = .T.
*!*	      ENDIF
*!*	      XSTYLE = LSTYLE
*!*	      SELECT STYLE
*!*	      *-- HDM B602397,1 In style Range last CT not Printed [start]
*!*	      SET ORDER TO CSTYLE
*!*	      *-- HDM B602397,1 In style Range last CT not Printed [end]
*!*	      SEEK XSTYLE
*!*	      IF EOF()
*!*	         XCOLOR = '*'
*!*	         SEEK TRIM(XSTYLE)
*!*	         *DO STYBROW WITH XSTYLE,XCOLOR,'*'
*!*	         SET COLOR TO &QCLRSAY
*!*	         LSTYLE = XSTYLE
*!*	         HSTYLE = XSTYLE
*!*	        *-- HDM B602397,1 [start]
*!*	         *@ 19,46 SAY LSTYLE
*!*	         *@ 19,63 SAY HSTYLE
*!*	        *-- HDM B602397,1 [end]
*!*	      ENDIF
*!*	      XFILTER = XFILTER + ".AND. BETWEEN(STYLE,LSTYLE,HSTYLE) "
*!*	  ENDIF

*!*	  IF LEN(TRIM(XFABRIC)) <>0
*!*	      SELECT FABRIC
*!*	      SEEK XFABRIC
*!*	      IF EOF()
*!*	         XCOLOR = SPACE(6)
*!*	         SEEK TRIM(XFABRIC)
*!*	         *-- HDM B602397,1 [start]
*!*	         *SET COLOR TO &QCLRSAY
*!*	         *@ 21,46 SAY XFABRIC
*!*	         *-- HDM B602397,1 [end]
*!*	      ENDIF
*!*	      XFILTER = XFILTER + ".AND.STYLE->FABRIC=XFABRIC"
*!*	  ENDIF

*!*	  IF LEN(TRIM(XGRP)) <>0
*!*	*      DO CODECHK WITH 'G',XGRP,'GROUP'
*!*	      IF LEN(TRIM(XGRP)) <>0
*!*	         *-- HDM B602397,1 [start]
*!*	         *SET COLOR TO &QCLRSAY
*!*	         *@ 17,46 SAY XGRP
*!*	         *-- HDM B602397,1 [end]
*!*	         XFILTER = XFILTER + ".AND.STYLE->CSTYGROUP=XGRP"
*!*	      ENDIF
*!*	  ENDIF

*!*	  IF LEN(TRIM(XDIV)) <>0
*!*	*     DO CODECHK WITH 'D',XDIV,'DIVISION'
*!*	     IF LEN(TRIM(XDIV)) <> 0
*!*	       *-- HDM B602397,1 [start]
*!*	       *SET COLOR TO &QCLRSAY
*!*	       *@ 13,46 SAY XDIV
*!*	       *-- HDM B602397,1 [end]
*!*	       XFILTER1 = XFILTER1 + ".AND.CDIVISION=XDIV"
*!*	     ENDIF
*!*	  ENDIF

*!*	  IF LEN(TRIM(XSEASON)) <>0
*!*	*     DO CODECHK WITH 'Z',XSEASON,'SEASON'
*!*	     IF LEN(TRIM(XSEASON)) <> 0
*!*	       *-- HDM B602397,1 [start]
*!*	       *SET COLOR TO &QCLRSAY
*!*	       *@ 15,46 SAY XSEASON
*!*	       *-- HDM B602397,1 [end]
*!*	       XFILTER1 = XFILTER1 + ".AND.SEASON=XSEASON"
*!*	     ENDIF
*!*	  ENDIF

*!*	  SET ORDER TO TAG cStyle IN STYLE
*!*	  
*!*	  *--- B606757,1 MAN Comment out (Start)
*!*	  *SELECT CUTTKTH 
*!*	  *SET RELATION TO Style INTO STYLE
*!*	  *SET FILTER   TO
*!*	  *GOTO TOP
*!*	  *--- B606757,1 MAN Comment out (End)

*!*	   IF ERRFLAG
*!*	      SG = 'GET'
*!*	      LOOP
*!*	   ENDIF

*!*	   *--- SELECT RECORDS FOR REPORT
*!*	   IF LEN(XFILTER)>1
*!*	      XFILTER=SUBSTR(XFILTER,7)
*!*	   ELSE
*!*	      XFILTER="VAL(CUTTKT)<>0"
*!*	   ENDIF

*!*	   IF LEN(XFILTER1)>1
*!*	      XFILTER1=SUBSTR(XFILTER1,7)
*!*	   ENDIF

*!*	   *-------------------------------------------
*!*	   * SELECT SORT SEQUENCE
*!*	   *-------------------------------------------

*!*	   SORTFIELD = ' '
*!*	   BREAK     = ' '
*!*	   DO CASE
*!*	     * SUMMARY BY CUTTING TICKET
*!*	     CASE SCHOICE='C'
*!*	       SORTFIELD = 'CUTTKT'

*!*	     * SUMMARY BY COMPLETION DATE
*!*	     CASE SCHOICE='D'
*!*	       SORTFIELD = 'SYS(11,COMPLETE)+CUTTKT'
*!*	       BREAK = 'DTOC(COMPLETE)'
*!*	       TOTALD= 'COMPLETE DATE'
*!*	   ENDCASE

*!*	   XLONG = IIF( lcRepForm = 'L' , 'Y' , 'N' )
*!*	   XPRINTNOTE = lcPrntNote
*!*	*---------------------------------------------------------------------------------------------
*!*	*---------------------------------------------------------
*!*	* [3] SELECT REPORT FILE & INITIALIZE MEMORY
*!*	*---------------------------------------------------------
*!*	DO WHILE .T.
*!*	   WAIT WINDOW 'Selecting records for report ...' NOWAIT
*!*	*--- B606757,1 MAN Comment out (Start)
*!*	   *SELECT CUTTKTH
*!*	   *SET RELATION TO STYLE INTO STYLE
*!*	   *SET FILTER TO
*!*	   *LOCATE ALL FOR &XFILTER
*!*	   *IF EOF()
*!*	   *  WAIT WINDO 'NO RECORDS SELECTED FOR REPORT!'
*!*	   *   *E300285,1 MAN Change CLOSE DATABASES to gfClsData
*!*	   *   *CLOSE DATA
*!*	   *  SET DEVICE TO SCREEN && HDM
*!*	   *  RETURN
*!*	   *ENDIF
*!*	*--- B606757,1 MAN Comment out (End)   
*!*	   WORKFILE = gfTempName()
*!*	   SET TALK ON
*!*	   
*!*	   SET TALK OFF

*!*	   SELECT CUTTKTH
*!*	   =AFIELDS(laFileStru)
*!*	   lnFileStru = ALEN(laFileStru,1)
*!*	   DIMENSION laFileStru[lnFileStru+11,4]

*!*	   laFileStru[lnFileStru+1,1] = 'CNTR1'
*!*	   laFileStru[lnFileStru+1,2] = 'C'
*!*	   laFileStru[lnFileStru+1,3] = 8
*!*	   laFileStru[lnFileStru+1,4] = 0

*!*	   laFileStru[lnFileStru+2,1] = 'CNTR2'
*!*	   laFileStru[lnFileStru+2,2] = 'C'
*!*	   laFileStru[lnFileStru+2,3] = 8
*!*	   laFileStru[lnFileStru+2,4] = 0
*!*	   
*!*	   laFileStru[lnFileStru+3,1] = 'CNTR3'
*!*	   laFileStru[lnFileStru+3,2] = 'C'
*!*	   laFileStru[lnFileStru+3,3] = 8
*!*	   laFileStru[lnFileStru+3,4] = 0

*!*	   laFileStru[lnFileStru+4,1] = 'MFGCD1'
*!*	   laFileStru[lnFileStru+4,2] = 'C'
*!*	   laFileStru[lnFileStru+4,3] = 6
*!*	   laFileStru[lnFileStru+4,4] = 0

*!*	   laFileStru[lnFileStru+5,1] = 'MFGCD2'
*!*	   laFileStru[lnFileStru+5,2] = 'C'
*!*	   laFileStru[lnFileStru+5,3] = 6
*!*	   laFileStru[lnFileStru+5,4] = 0

*!*	   laFileStru[lnFileStru+6,1] = 'MFGCD3'
*!*	   laFileStru[lnFileStru+6,2] = 'C'
*!*	   laFileStru[lnFileStru+6,3] = 6
*!*	   laFileStru[lnFileStru+6,4] = 0
*!*	   
*!*	   laFileStru[lnFileStru+7,1] = 'CNTISS'
*!*	   laFileStru[lnFileStru+7,2] = 'N'
*!*	   laFileStru[lnFileStru+7,3] = 7
*!*	   laFileStru[lnFileStru+7,4] = 0

*!*	   laFileStru[lnFileStru+8,1] = 'CNTREC'
*!*	   laFileStru[lnFileStru+8,2] = 'N'
*!*	   laFileStru[lnFileStru+8,3] = 7
*!*	   laFileStru[lnFileStru+8,4] = 0

*!*	   laFileStru[lnFileStru+9,1] = 'CNTDAM'
*!*	   laFileStru[lnFileStru+9,2] = 'N'
*!*	   laFileStru[lnFileStru+9,3] = 7
*!*	   laFileStru[lnFileStru+9,4] = 0

*!*	   laFileStru[lnFileStru+10,1] = 'CNTCAN'
*!*	   laFileStru[lnFileStru+10,2] = 'N'
*!*	   laFileStru[lnFileStru+10,3] = 7
*!*	   laFileStru[lnFileStru+10,4] = 0
*!*	   
*!*	   laFileStru[lnFileStru+11,1] = 'CNTOPN'
*!*	   laFileStru[lnFileStru+11,2] = 'N'
*!*	   laFileStru[lnFileStru+11,3] = 7
*!*	   laFileStru[lnFileStru+11,4] = 0

*!*	   CREATE TABLE &gcWorkDir.&WORKFILE FROM ARRAY laFileStru
*!*	*--- B606757,1 MAN Comment out (Start)   
*!*	*   APPEND FROM (gcDataDir + '\CUTTKTH') FOR &XFILTER
*!*	*--- B606757,1 MAN Comment out End)

*!*	*--- B606757,1 MAN Added ... (Start)
*!*	   IF EMPTY(XFILTER)
*!*	     XFILTER = ".T."
*!*	   ENDIF
*!*	   IF EMPTY(XFILTER1)
*!*	     XFILTER1 = ".T."
*!*	   ENDIF

*!*	   APPEND FROM (gcDataDir + '\CUTTKTH') FOR &XFILTER AND &XFILTER1
*!*	   IF RECCOUNT(WORKFILE) = 0
*!*	     WAIT WINDO 'NO RECORDS SELECTED FOR REPORT!'
*!*	     SET DEVICE TO SCREEN && HDM
*!*	     RETURN
*!*	   ENDIF
*!*	   SELECT CUTTKTH
*!*	   SET RELATION TO STYLE INTO STYLE
*!*	   SET FILTER TO
*!*	   DECLARE LAESTCOST[5]
*!*	   DECLARE LALANCOST[5]
*!*	   DECLARE LAACTCOST[5]
*!*	   DECLARE laBudget[2]
*!*	   DECLARE laRec[2]
*!*	   DECLARE laDam[2]
*!*	   DECLARE laCAN[2]
*!*	   DECLARE laOpn[2]
*!*	   STORE 0 TO LAESTCOST,LALANCOST,LAACTCOST,laBudget,laRec,laDam,laCAN,laOpn
*!*	   
*!*	*--- B606757,1 MAN Added ... (End)

*!*	   SELECT (WORKFILE)
*!*	   SET RELATION TO


*!*	   
*!*	   *COPY REST TO (gcWorkDir + WORKFILE) FOR &XFILTER
*!*	   *SELECT 0
*!*	   *USE (gcWorkDir + WORKFILE)
*!*	   *****************************************************
*!*	   *--HDM B602289,1 02/23/1999 Contractor Information Not Printed[start]
*!*	   SCAN
*!*	     SELECT MFGOPRHD
*!*	     = SEEK('M' + &WORKFILE..CUTTKT)
*!*	     lnIndex = 0
*!*	   *abdou Add New For condition
*!*	   *B#802346,1  ADD NEW FOR CONDITION TO SELECT 3 CNTR ONLY [ BEGIN ]
*!*	     *SCAN REST WHILE cimtyp+ctktno+coprcode =;
*!*	                  'M' + &WORKFILE..CutTkt ;
*!*	               FOR !lInHouse AND cContCode = ALLTRIM(Xven) 
*!*	     SCAN REST WHILE cimtyp+ctktno+coprcode =;
*!*	                     'M' + &WORKFILE..CutTkt ;
*!*	                     FOR !lInHouse AND cContCode = ALLTRIM(Xven); 
*!*	                         AND lnIndex <=2
*!*	   *B#802346,1  ADD NEW FOR CONDITION TO SELECT 3 CNTR ONLY [ END ]               
*!*	       lnIndex = lnIndex + 1
*!*	       SELECT (WORKFILE)
*!*	       lcIndex = ALLTRIM(STR(lnIndex))
*!*	       REPLACE CNTR&lcIndex  WITH MFGOPRHD.cContCode ;
*!*	               MFGCD&lcIndex WITH MFGOPRHD.COPRCODE
*!*	     ENDSCAN
*!*	     SELECT MFGOPRDT
*!*	     *--- B606757,1 MAN Commented ... (Start)
*!*	     *GO TOP
*!*	     *SUM NLOTTOTQTY TO lnCntIss FOR cimtyp+ctktno = 'M' + &WORKFILE..CutTkt  .AND. TRANCD = '1'
*!*	     *SUM NLOTTOTQTY TO lnCntRec FOR cimtyp+ctktno = 'M' + &WORKFILE..CutTkt  .AND. TRANCD = '2'
*!*	     *SUM NLOTTOTQTY TO lnCntDam FOR cimtyp+ctktno = 'M' + &WORKFILE..CutTkt  .AND. TRANCD = '3'
*!*	     *SUM NLOTTOTQTY TO lnCntCan FOR cimtyp+ctktno = 'M' + &WORKFILE..CutTkt  .AND. TRANCD = '4'
*!*	     *--- B606757,1 MAN Commented ... (End)     
*!*	     
*!*	     *--- B606757,1 MAN Added ... (Start)
*!*	     lnCntIss = 0
*!*	     lnCntRec = 0
*!*	     lnCntDam = 0
*!*	     lnCntCan = 0
*!*	     
*!*	     SCAN FOR cimtyp+ctktno+coprcode+trancd = 'M' + &WORKFILE..CutTkt  
*!*	       DO CASE
*!*	         CASE TRANCD = '1'
*!*	           lnCntIss = lnCntIss + NLOTTOTQTY
*!*	         CASE TRANCD = '2'
*!*	           lnCntRec = lnCntRec + NLOTTOTQTY
*!*	         CASE TRANCD = '3'
*!*	           lnCntDam = lnCntDam + NLOTTOTQTY
*!*	         CASE TRANCD = '4'
*!*	           lnCntCan = lnCntCan + NLOTTOTQTY
*!*	       ENDCASE  
*!*	     ENDSCAN
*!*	     
*!*	     *--- B606757,1 MAN Added ... (End)
*!*	     
*!*	     lnOpen = lnCntIss - (lnCntRec + lnCntDam + lnCntCan)
*!*	     SELECT &WORKFILE
*!*	     REPLACE CNTISS WITH lnCntIss,;
*!*	             CNTRec WITH lnCntRec,;
*!*	             CNTDam WITH lnCntDam,;
*!*	             CNTCan WITH lnCntCan,;
*!*	             CNTOPN WITH lnOpen
*!*	      FOR i = 1 TO 5       
*!*	        lcNo = STR(i,1)
*!*	        LAESTCOST[i] = LAESTCOST[i] + NEST_COST&lcNo.
*!*	        LALANCOST[i] = LALANCOST[i] + NLAN_COST&lcNo.
*!*	        LAACTCOST[i] = LAACTCOST[i] + NACT_COST&lcNo.
*!*	      ENDFOR   

*!*	      laBudget[1] = laBudget[1] + PCS_BUD
*!*	      laBudget[2] = laBudget[2] + CNTISS
*!*	            
*!*	      laRec[1] = laRec[1] + 
*!*	      laRec[2] = laRec[2] + CNTREC
*!*	      
*!*	      laDam[1] = laDam[1] + PCS_DAM
*!*	      laDam[2] = laDam[2] + CNTDAM
*!*	      
*!*	      laCAN[1] = laCAN[1] + PCS_CAN
*!*	      
*!*	      laOpn[1] = laOpn[1] + PCS_OPN
*!*	      laOpn[2] = laOpn[2] + CNTOPN
*!*	   ENDSCAN
*!*	   *--HDM B602289,1 02/23/1999 Contractor Information Not Printed[end]
*!*	   *****************************************************
*!*	   *--- B606757,1 MAN Comment out (Start)
*!*	   
*!*	   *IF .NOT. EMPTY(XFILTER1)
*!*	   *  SET FILTER TO &XFILTER1
*!*	   *  GOTO TOP  
*!*	   *  IF EOF()
*!*	   *    WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT !!!'
*!*	   *     *E300285,1 MAN Change CLOSE DATABASES to gfClsData
*!*	   *     *CLOSE DATA
*!*	   *     *=gfclsdata()
*!*	   *    SET DEVICE TO SCREEN
*!*	   *    RETURN
*!*	   *  ENDIF
*!*	   *ENDIF
*!*	   
*!*	   *--- B606757,1 MAN Comment out (End)

*!*	   IF SORTFIELD<>' '
*!*	      Z = LTRIM(STR(RECCOUNT(),7))
*!*	      WAIT WINDOW 'SORTING RECORDS FOR CUTTING TICKET HEADER REPORT ...' NOWAIT
*!*	      INDEX ON &SORTFIELD TAG &WORKFILE
*!*	      SET ORDER TO TAG &WORKFILE
*!*	      *@ 23,00
*!*	   ENDIF
*!*	  &&TAK 03/08/94 Change the heder of report due to style incresing.
*!*	  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!*	  *
*!*	  *TICKET#  ENTERED    STYLE           ....... DESC ....... VENDOR   ....... NAME ....... ..MFG.  OPERATRION.. ......... NOTE ..........
*!*	  *STATUS   COMPLETE   SEASON          .................... ........ .................... .................... .........................
*!*	  *TYPE     COMPLETE%  DIVISION        .................... ........ .................... .................... .........................
*!*	  *
*!*	  *          <-----------------PIECES------------------>     <---------------------------------COST--------------------------->
*!*	  *                           CUTTING TICKET  CONTRACTOR                      MATERIAL          LABOR    TOTAL COST   UNIT COST
*!*	  *          BUDGET/ISSUE :          9999999     9999999     ESTIMATED :  999999999.99   999999999.99  999999999.99   999999.99
*!*	  *          COMPLETE/REC.:          9999999     9999999     ACTUAL    :  999999999.99   999999999.99  999999999.99   999999.99
*!*	  *          DAMAGE       :          9999999     9999999     VARIANCE  :  999999999.99   999999999.99  999999999.99
*!*	  *          CANCELLED    :          9999999     9999999     PERCENTAGE:  999999999.99   999999999.99  999999999.99
*!*	  *          OPEN         :          9999999     9999999
*!*	  *
*!*	  **
*!*	  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!*	  *TICKET #  STATUS  ENTERED   COMPLETE  COMPLETE %  STYLE            SEASON  DIVISION   BUDGET  COMPLETE   DAMAGE   CANCEL     OPEN
*!*	  *XXXXXX      X     XX/XX/XX  XX/XX/XX   9999.99 %  XXXXXXXXXXXXXXX    XX       XX     9999999   9999999  9999999  9999999  9999999
*!*	  *
*!*	   DIMENSION XTOT1(5),XTOT2(4),XTOT3(2),XTOT4(2),XGTOT1(5),XGTOT2(4),XGTOT3(2),XGTOT4(2)
*!*	   STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XGTOT1,XGTOT2,XGTOT3,XGTOT4
*!*	   PAGENO = 0
*!*	   ROW    = 99
*!*	   XTIME  = TIME()

*!*	   SELECT &WORKFILE
*!*	   GOTO TOP

*!*	   HBREAK=SPACE(1)
*!*	   IF LEN(TRIM(BREAK)) <>0
*!*	      HBREAK = &BREAK
*!*	   ENDIF
*!*	   **

*!*	*--- B606757,1 MAN Comment out (Start)
*!*	   *E700037,1 TMI 03/22/95
*!*	*   SELECT SUM(&WORKFILE..NEST_COST1), SUM(&WORKFILE..NEST_COST2),;
*!*	   SUM(&WORKFILE..NEST_COST3), SUM(&WORKFILE..NEST_COST4),;
*!*	   SUM(&WORKFILE..NEST_COST5);
*!*	   FROM &WORKFILE INTO ARRAY LAESTCOST

*!*	*   SELECT SUM(&WORKFILE..NLAN_COST1), SUM(&WORKFILE..NLAN_COST2),;
*!*	   SUM(&WORKFILE..NLAN_COST3), SUM(&WORKFILE..NLAN_COST4),;
*!*	   SUM(&WORKFILE..NLAN_COST5);
*!*	   FROM &WORKFILE INTO ARRAY LALANCOST

*!*	*   SELECT SUM(&WORKFILE..NACT_COST1), SUM(&WORKFILE..NACT_COST2),;
*!*	   SUM(&WORKFILE..NACT_COST3), SUM(&WORKFILE..NACT_COST4),;
*!*	   SUM(&WORKFILE..NACT_COST5);
*!*	   FROM &WORKFILE INTO ARRAY LAACTCOST

*!*	*   SELECT SUM(&WORKFILE..PCS_BUD) ,SUM(&WORKFILE..CNTISS);
*!*	   FROM &WORKFILE INTO ARRAY laBudget

*!*	*   SELECT SUM(&WORKFILE..PCS_REC) ,SUM(&WORKFILE..CNTREC);
*!*	   FROM &WORKFILE INTO ARRAY laRec

*!*	*   SELECT SUM(&WORKFILE..PCS_DAM) ,SUM(&WORKFILE..CNTDAM);
*!*	   FROM &WORKFILE INTO ARRAY laDam

*!*	*  SELECT SUM(&WORKFILE..PCS_CAN) ;
*!*	   FROM &WORKFILE INTO ARRAY laCAN

*!*	*   SELECT SUM(&WORKFILE..PCS_OPN) ,SUM(&WORKFILE..CNTOPN);
*!*	   FROM &WORKFILE INTO ARRAY laOpn
*!*	*--- B606757,1 MAN Comment out (End)
*!*	   CLEAR TYPEAHEAD
*!*	   SET DEVICE TO PRINT
*!*	   SELE &WORKFILE

*!*	   *---------------------------------------------------------
*!*	   * [REPORT] LOOP
*!*	   *---------------------------------------------------------
*!*	   *SET DEVICE TO PRINTER
*!*	   *******************

*!*	   DO WHILE .T.  && HDM  INKEY() <>32
*!*	   
*!*	      IF ROW >=51
*!*	         PAGENO = PAGENO+1
*!*	         IF BREAK <> ' '
*!*	           XREPORT='MFCTHDR'+'  '+'TOTAL :'+TOTALD
*!*	         ENDIF
*!*	         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

*!*	         IF XLONG = 'Y'
*!*	           @ 06,00 SAY 'TICKET#  ENTERED    STYLE           ....... DESC ....... VENDOR   ....... NAME ....... ..MFG.  OPERATIONS.. ......... NOTE ..........'
*!*	           @ 07,00 SAY 'STATUS   COMPLETE   SEASON          .................... ........ .................... .................... .........................'
*!*	           @ 08,00 SAY 'TYPE     COMPLETE%  DIVISION        .................... ........ .................... .................... .........................'
*!*	           ROW = 09
*!*	         ELSE
*!*	           @ 06,00 SAY 'TICKET #  STATUS  ENTERED   COMPLETE  COMPLETE %  STYLE            SEASON  DIVISION   BUDGET  COMPLETE   DAMAGE   CANCEL     OPEN'
*!*	           ROW = 7
*!*	         ENDIF
*!*	      ENDIF
*!*	      DO WHILE .NOT. EMPTY(BREAK)
*!*	         IF &BREAK = HBREAK
*!*	            EXIT
*!*	         ENDIF
*!*	         ROW=ROW+1
*!*	         @ ROW,00 SAY REPLICATE('=',132)
*!*	         ROW = ROW+1
*!*	        **************************
*!*	        @ ROW,001 SAY '**** SUB TOTAL ****  '+HBREAK
*!*	        @ ROW,000 SAY '        CUT TKT  CONTR.               ESTIMATED        LANDED       ACTUAL    ACT-LAN   LAND-EST   ACT-EST   ACT/LAN LAN/EST ACT/EST'
*!*	        ROW = ROW+1
*!*	        @ ROW,000 SAY 'BUDGET  '
*!*	        @ ROW,08 SAY lnPC_BUD  PICTURE '9999999'
*!*	        @ ROW,16 SAY lnCON_BUD PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[1]
*!*	        @ ROW,36 SAY laSEst[1]             PICTURE '9999999.999'
*!*	        @ ROW,50 SAY laSlan[1]             PICTURE '9999999.999'
*!*	        @ ROW,63 SAY laSAct[1]             PICTURE '9999999.999'
*!*	        @ ROW,75 SAY laSAct[1] - laSlan[1] PICTURE '999999.999'
*!*	        @ ROW,86 SAY laSlan[1] - laSEst[1] PICTURE '999999.999'
*!*	        @ ROW,96 SAY laSAct[1] - laSEst[1] PICTURE '999999.999'
*!*	        @ ROW,108 SAY IIF(laSlan[1]=0,'N/A',(laSAct[1] / laSlan[1])*100)    PICTURE '9999.99'
*!*	        @ ROW,116 SAY IIF(laSEst[1]=0,'N/A',(laSlan[1] / laSEst[1])*100)    PICTURE '9999.99'
*!*	        @ ROW,124 SAY IIF(laSEst[1]=0,'N/A',(laSAct[1] / laSEst[1])*100)    PICTURE '9999.99'

*!*	        ROW = ROW+1
*!*	        @ ROW,000 SAY 'RECEIVE '
*!*	        @ ROW,08 SAY lnPCS_REC    PICTURE '9999999'
*!*	        @ ROW,16 SAY CntIss   PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[2]
*!*	        @ ROW,36 SAY laSEst[2]         PICTURE '9999999.999'
*!*	        @ ROW,50 SAY laSlan[2]         PICTURE '9999999.999'
*!*	        @ ROW,63 SAY laSAct[2]         PICTURE '9999999.999'
*!*	        @ ROW,75 SAY laSAct[2] - laSlan[2]         PICTURE '999999.999'
*!*	        @ ROW,86 SAY laSlan[2] - laSEst[2]         PICTURE '999999.999'
*!*	        @ ROW,96 SAY laSAct[2] - laSEst[2]         PICTURE '999999.999'

*!*	        @ ROW,108 SAY IIF(laSlan[2]=0,'N/A',(laSAct[2] / laSlan[2])*100)            PICTURE '9999.99'
*!*	        @ ROW,116 SAY IIF(laSEst[2]=0,'N/A',(laSlan[2] / laSEst[2])*100)            PICTURE '9999.99'
*!*	        @ ROW,124 SAY IIF(laSEst[2]=0,'N/A',(laSAct[2] / laSEst[2])*100)            PICTURE '9999.99'


*!*	        ROW = ROW+1
*!*	        @ ROW,000 SAY 'DAMAGE  '
*!*	        @ ROW,08 SAY lnPCS_DAM    PICTURE '9999999'
*!*	        @ ROW,16 SAY CntDam   PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[3]
*!*	        @ ROW,36 SAY laSEst[3]               PICTURE '9999999.999'
*!*	        @ ROW,50 SAY laSlan[3]               PICTURE '9999999.999'
*!*	        @ ROW,63 SAY laSAct[3]               PICTURE '9999999.999'
*!*	        @ ROW,75 SAY laSAct[3] - laSlan[3]               PICTURE '999999.999'
*!*	        @ ROW,86 SAY laSlan[3] - laSEst[3]               PICTURE '999999.999'
*!*	        @ ROW,96 SAY laSAct[3] - laSEst[3]               PICTURE '999999.999'
*!*	        @ ROW,108 SAY IIF(laSlan[3]=0,'N/A',(laSAct[3] / laSlan[3])*100)               PICTURE '9999.99'
*!*	        @ ROW,116 SAY IIF(laSEst[3]=0,'N/A',(laSlan[3] / laSEst[3])*100)               PICTURE '9999.99'
*!*	        @ ROW,124 SAY IIF(laSEst[3]=0,'N/A',(laSAct[3] / laSEst[3])*100)               PICTURE '9999.99'

*!*	        ROW = ROW+1
*!*	        @ ROW,000 SAY 'CANCEL  '
*!*	        @ ROW,08 SAY lnPCS_CAN   PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[4]   PICTURE '9999999'
*!*	        @ ROW,36 SAY laSEst[4]                  PICTURE '9999999.999'
*!*	        @ ROW,50 SAY laSlan[4]                  PICTURE '9999999.999'
*!*	        @ ROW,63 SAY laSAct[4]                  PICTURE '9999999.999'
*!*	        @ ROW,75 SAY laSAct[4] - laSlan[4]      PICTURE '999999.999'
*!*	        @ ROW,86 SAY laSlan[4] - laSEst[4]      PICTURE '999999.999'
*!*	        @ ROW,96 SAY laSAct[4] - laSEst[4]      PICTURE '999999.999'
*!*	        @ ROW,108 SAY IIF(laSlan[4]=0,'N/A',(laSAct[4] / laSlan[4])*100)      PICTURE '9999.99'
*!*	        @ ROW,116 SAY IIF(laSEst[4]=0,'N/A',(laSlan[4] / laSEst[4])*100)      PICTURE '9999.99'
*!*	        @ ROW,124 SAY IIF(laSEst[4]=0,'N/A',(laSAct[4] / laSEst[4])*100)      PICTURE '9999.99'

*!*	        ROW = ROW+1
*!*	        @ ROW,000 SAY 'OPEN    '
*!*	        @ ROW,08 SAY lnPCS_OPN   PICTURE '9999999'
*!*	        
*!*	        @ ROW,16 SAY CNTOPN   PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[5]
*!*	        @ ROW,36 SAY laSEst[5]         PICTURE '9999999.999'
*!*	        @ ROW,50 SAY laSlan[5]         PICTURE '9999999.999'
*!*	        @ ROW,63 SAY laSAct[5]         PICTURE '9999999.999'
*!*	        @ ROW,75 SAY laSAct[5] - laSlan[5]         PICTURE '999999.999'
*!*	        @ ROW,86 SAY laSlan[5] - laSEst[5]         PICTURE '999999.999'
*!*	        @ ROW,96 SAY laSAct[5] - laSEst[5]         PICTURE '999999.999'
*!*	        @ ROW,108 SAY IIF(laSlan[5]=0,'N/A',(laSAct[5] / laSlan[5])*100)         PICTURE '9999.99'
*!*	        @ ROW,116 SAY IIF(laSEst[5]=0,'N/A',(laSlan[5] / laSEst[5])*100)         PICTURE '9999.99'
*!*	        @ ROW,124 SAY IIF(laSEst[5]=0,'N/A',(laSAct[5] / laSEst[5])*100)         PICTURE '9999.99'
*!*	        STORE 0 TO laSEst , laSEst , laSlan
*!*	        STORE 0 TO lnPCS_BUD , lnCONT_ISS , lnPCS_REC , lnCONT_REC ,lnPCS_DAM , lnCONT_DAM ,lnPCS_CAN ,;
*!*	        lnPCS_OPN , lnCONT_OPN
*!*	        *---- CALCULATE THE TOTAL FIELDS -----*
*!*	        *XMAT_VAR=XTOT3(2) - XTOT3(1)
*!*	        *XLAB_VAR=XTOT4(2) - XTOT4(1)
*!*	        *XMAT_PCT=IIF(XTOT3(1) <> 0,ROUND(XTOT3(2)/XTOT3(1)*100,2),0.00)
*!*	        *XLAB_PCT=IIF(XTOT4(1) <> 0,ROUND(XTOT4(2)/XTOT4(1)*100,2),0.00)
*!*	        *XTOT_EST=XTOT3(1)  + XTOT4(1)
*!*	        *XTOT_ACT=XTOT3(2)  + XTOT4(2)
*!*	        *XTOT_VAR=XTOT_ACT   - XTOT_EST
*!*	        *XTOT_PCT=IIF(XTOT_EST <> 0,ROUND(XTOT_ACT/XTOT_EST*100,2),0.00)
*!*	        *XES_UNTCST=IIF(XTOT1(1)<>0,ROUND(XTOT_EST/XTOT1(1),2),0.00)
*!*	        *XAC_UNTCST=IIF((XTOT1(2)+XTOT1(3))<>0,ROUND(XTOT_ACT/(XTOT1(2)+XTOT1(3)),2),0.00)
*!*	        *--------------------------------------------------------------------------------*

*!*	         IF XLONG = 'Y'
*!*	           *- ARH 03/27/94 Changed QPW(14) with qCostPrv
*!*	           IF qCostPrv                       && COSTING ACCESS.
*!*	             *@ ROW,71 SAY XMAT_VAR PICTURE '999999999.99'
*!*	             *@ ROW,86 SAY XLAB_VAR PICTURE '999999999.99'
*!*	             *@ ROW,100 SAY XTOT_VAR PICTURE '999999999.99'
*!*	           ENDIF
*!*	           *- ARH 03/27/94 Changed QPW(14) with qCostPrv
*!*	         ELSE    && IF IT IS A SHORT FORMAT
*!*	           @ ROW,084 SAY XTOT1(1) PICTURE '99999999'
*!*	           @ ROW,094 SAY XTOT1(2) PICTURE '99999999'
*!*	           @ ROW,103 SAY XTOT1(3) PICTURE '99999999'
*!*	           @ ROW,112 SAY XTOT1(4) PICTURE '99999999'
*!*	           @ ROW,121 SAY XTOT1(5) PICTURE '99999999'
*!*	         ENDIF

*!*	         ROW = ROW+1
*!*	         @ ROW,00 SAY REPLICATE('=',132)
*!*	         ROW = ROW+1

*!*	         ******************** INITIALIZE SUBTOTALS ************
*!*	         X = 1
*!*	         DO WHILE X<=5
*!*	            XGTOT1(X) = XGTOT1(X) + XTOT1(X)
*!*	            XTOT1(X) = 0.00
*!*	            IF X<=4
*!*	              XGTOT2(X) = XGTOT2(X) + XTOT2(X)
*!*	              XTOT2(X) = 0.00
*!*	            ENDIF
*!*	            X = X+1
*!*	         ENDDO
*!*	         X = 1
*!*	         DO WHILE X <=2
*!*	           XGTOT3(X) = XGTOT3(X) + XTOT3(X)
*!*	           XGTOT4(X) = XGTOT4(X) + XTOT4(X)
*!*	           XTOT3(X) = 0.00
*!*	           XTOT4(X) = 0.00
*!*	           X = X + 1
*!*	         ENDDO
*!*	         HBREAK = &BREAK
*!*	         EXIT
*!*	      ENDDO
*!*	      *--------------------- END SUBTOTALS ----------------------------
*!*	      IF EOF()
*!*	         EXIT
*!*	      ENDIF

*!*	      IF ROW >=50
*!*	         ROW = 99
*!*	         LOOP
*!*	      ENDIF

*!*	      SELECT &WORKFILE
*!*	      ********************** LONG FORMAT ******************************
*!*	      XCUT = CUTTKT
*!*	      XCOMPCT   = IIF(PCS_BUD<>0, ROUND((PCS_REC+PCS_DAM)/PCS_BUD*100,2),0)
*!*	      IF XLONG = 'Y'
*!*	        XSEASON   = SEASON
*!*	        XDIVISION = CDIVISION
*!*	        XVEN1 = SPACE(0)
*!*	        XVEN2 = SPACE(0)
*!*	        XVEN3 = SPACE(0)
*!*	        XOPER1= SPACE(0)
*!*	        XOPER2= SPACE(0)
*!*	        XOPER3= SPACE(0)
*!*	        IF NOT EMPTY(CNTR1)
*!*	          *-- MFM 07/12/94.
*!*	          SELE &lcVenFile
*!*	          SEEK &WORKFILE->CNTR1
*!*	          IF FOUND()
*!*	            XVEN1 = &lcVenFile..CVENCOMP
*!*	            *-- END MFM 07/12/94.
*!*	          ENDIF
*!*	          XOPER1  = ALLTRIM(gfCodDes(&WORKFILE..MFGCD1 ,'MFGCODE'))
*!*	          SELE &WORKFILE
*!*	        ENDIF

*!*	        IF .NOT. EMPTY(CNTR2)
*!*	          *-- MFM 07/12/94.
*!*	          SELE &lcVenFile
*!*	          SEEK &WORKFILE->CNTR2
*!*	          IF FOUND()
*!*	            XVEN2 = &lcVenFile..CVENCOMP
*!*	            *-- END MFM 07/12/94.
*!*	          ENDIF
*!*	          XOPER2  = ALLTRIM(gfCodDes(&WORKFILE..MFGCD2 ,'MFGCODE'))
*!*	          SELE &WORKFILE
*!*	        ENDIF
*!*	        IF NOT EMPTY(CNTR3)
*!*	          *-- MFM 07/12/94.
*!*	          SELE &lcVenFile
*!*	          SEEK &WORKFILE->CNTR3
*!*	          IF FOUND()
*!*	            XVEN3 = &lcVenFile..CVENCOMP
*!*	            *-- END MFM 07/12/94.
*!*	          ENDIF
*!*	          XOPER3  = ALLTRIM(gfCodDes(&WORKFILE..MFGCD3 ,'MFGCODE'))
*!*	          SELE &WORKFILE
*!*	        ENDIF

*!*	        Z = ' '
*!*	        *-- B602289,1 HDM 11/30/1998 [start]
*!*	        PSEASON   = gfCodDes(SEASON,'SEASON')
*!*	        PDIVISION = gfCodDes(CDIVISION,'CDIVISION')

*!*	        *-- B602289,1 HDM 11/30/1998 [END]
*!*	        SELECT &WORKFILE
*!*	        @ ROW,000 SAY REPLICATE('-',132)
*!*	        ROW = ROW+1

*!*	        * LINE #1
*!*	        @ ROW,000 SAY CUTTKT
*!*	        @ ROW,009 SAY ENTERED
*!*	        *B603171,1 AMH Get the descreption from style file [Start]
*!*	        *@ ROW,020 SAY STYLE
*!*	        *@ ROW,036 SAY ALLTRIM(DESC)
*!*	        @ ROW,020 SAY ALLTRIM(STYLE)
*!*	        =SEEK(STYLE,'STYLE')
*!*	        @ ROW,036 SAY ALLTRIM(STYLE.DESC)
*!*	        *B603171,1 AMH [End]
*!*	        @ ROW,057 SAY ALLTRIM(CNTR1)
*!*	        @ ROW,066 SAY SUBSTR(XVEN1,1,20)
*!*	        @ ROW,087 SAY MFGCD1+' '+XOPER1
*!*	        *@ ROW,108 SAY SUBSTR(NOTE1,1,25)
*!*	        ROW = ROW+1
*!*	        * LINE #2
*!*	        DO CASE
*!*	          CASE STATUS = 'O'
*!*	            @ ROW,00 SAY 'OPEN'
*!*	          CASE STATUS = 'C'
*!*	            @ ROW,00 SAY 'CMPLTE'
*!*	          CASE STATUS = 'X'
*!*	            @ ROW,00 SAY 'CANCEL'
*!*	          CASE STATUS = 'H'
*!*	            @ ROW,00 SAY 'HOLD'
*!*	          CASE STATUS = 'A'        &&TAK 03/08/94 Added new status.
*!*	            @ ROW,00 SAY 'ACTUAL'
*!*	          *E700037,1 TMI 03/22/95 (BEGINING) Added new status called CLOSE.
*!*	          CASE STATUS = 'S'
*!*	            @ ROW,00 SAY 'CLOSE'
*!*	          *E700037,1 TMI 03/22/95 (END).
*!*	        ENDCASE
*!*	        @ ROW,009 SAY COMPLETE
*!*	        @ ROW,020 SAY SEASON
*!*	        *B603171,1 AMH Print 15 ch. only [Start]
*!*	        *@ ROW,035 SAY ALLTRIM(PSEASON)
*!*	        @ ROW,036 SAY SUBSTR(PSEASON,1,20)
*!*	        *B603171,1 AMH [End]
*!*	        @ ROW,057 SAY ALLTRIM(CNTR2)
*!*	        @ ROW,066 SAY SUBSTR(XVEN2,1,20)
*!*	        @ ROW,087 SAY MFGCD2+' '+XOPER2
*!*	        *@ ROW,108 SAY SUBSTR(NOTE2,1,25)
*!*	        ROW = ROW+1
*!*	        *LINE #3
*!*	        @ ROW,000 SAY CTKTTYPE
*!*	        @ ROW,009 SAY XCOMPCT PICTURE '9999.99'
*!*	        @ ROW,020 SAY ALLTRIM(CDIVISION)
*!*	        *B603171,1 AMH Print 15 ch. only [Start]
*!*	        *@ ROW,035 SAY ALLTRIM(PDIVISION)
*!*	        @ ROW,036 SAY SUBSTR(PDIVISION,1,20)
*!*	        *B603171,1 AMH [End]
*!*	        @ ROW,057 SAY ALLTRIM(CNTR3)
*!*	        @ ROW,066 SAY SUBSTR(XVEN3,1,20)
*!*	        @ ROW,087 SAY MFGCD3+' '+XOPER3
*!*	        *@ ROW,108 SAY SUBSTR(NOTE3,1,24)
*!*	        ROW = ROW+1
*!*	        *E700037,1 TMI 03/22/95  (BEGINING) Printing with new form by details costing.
*!*	        * LINE #4
*!*	        @ ROW,00 SAY ' -------PIECES---------  ------------------------------------COSTING--------------------------------------------------------------- '
*!*	        ROW = ROW+1
*!*	        @ ROW,00 SAY '        CUT TKT  CONTR.               ESTIMATED        LANDED       ACTUAL    ACT-LAN   LAND-EST    ACT-EST  ACT/LAN LAN/EST ACT/EST'
*!*	        ROW=ROW+1
*!*	        *---- CALCULATE THE FIELDS -----*
*!*	        *XMAT_VAR = AC_MAT_CST - ES_MAT_CST
*!*	        *XLAB_VAR = AC_LAB_CST - ES_LAB_CST
*!*	        *XMAT_PCT = IIF(ES_MAT_CST <> 0,ROUND(AC_MAT_CST/ES_MAT_CST*100,2),0.00)
*!*	        *XLAB_PCT = IIF(ES_LAB_CST <> 0,ROUND(AC_LAB_CST/ES_LAB_CST*100,2),0.00)
*!*	        *XTOT_EST = ES_MAT_CST + ES_LAB_CST
*!*	        *XTOT_ACT = AC_MAT_CST + AC_LAB_CST
*!*	        *XTOT_VAR = XTOT_ACT   - XTOT_EST
*!*	        *XTOT_PCT = IIF(XTOT_EST <> 0,ROUND(XTOT_ACT/XTOT_EST*100,2),0.00)
*!*	        *XES_UNTCST = IIF(PCS_BUD<>0,ROUND(XTOT_EST/PCS_BUD,2),0.00)
*!*	        *XAC_UNTCST = IIF((PCS_REC+PCS_DAM)<>0,ROUND(XTOT_ACT/(PCS_REC+PCS_DAM),2),0.00)

*!*	        ******  (1)  **************

*!*	        @ ROW,00 SAY 'BUDGET '
*!*	        @ ROW,08 SAY PCS_BUD PICTURE '9999999'
*!*	        
*!*	        lnPCS_BUD =lnPCS_BUD+PCS_BUD
*!*	        *@ ROW,16 SAY CntIss PICTURE '9999999'
*!*	        
*!*	        @ ROW,16 SAY CntIss PICTURE '9999999'
*!*	        
*!*	        *lnCONT_ISS=lnCONT_ISS+CONT_ISS

*!*	        @ ROW,25 SAY laCost[1]
*!*	        @ ROW , 35 SAY NEST_COST1                       PICTURE '999999999.99'

*!*	        @ ROW , 48 SAY NLAN_COST1                       PICTURE '9999999999.99'

*!*	        @ ROW , 62 SAY NACT_COST1                       PICTURE '999999999.99'

*!*	        @ ROW , 75 SAY NACT_COST1 -  NLAN_COST1         PICTURE '9999999.99'

*!*	        lnAct_Lan= lnAct_Lan+(NACT_COST1 -  NLAN_COST1)

*!*	        @ ROW , 85 SAY NLAN_COST1 - NEST_COST1             PICTURE '9999999.999'
*!*	        lnLan_Est=lnLan_Est+(NLAN_COST1 - NEST_COST1)

*!*	        @ ROW , 96 SAY NACT_COST1 - NEST_COST1             PICTURE '9999999.999'
*!*	        lnAct_Est=lnAct_Est+(NACT_COST1 - NEST_COST1)

*!*	        @ ROW , 109 SAY IIF(NLAN_COST1=0,'N/A',(NACT_COST1 / NLAN_COST1)*100)  PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(NEST_COST1=0,'N/A',(NLAN_COST1 / NEST_COST1)*100)  PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(NEST_COST1=0,'N/A',(NACT_COST1 / NEST_COST1)*100)  PICTURE '9999.99'

*!*	        ROW=ROW+1
*!*	        ******  (2)  **************

*!*	        @ ROW,00 SAY 'RECEIVE'
*!*	        @ ROW,08 SAY PCS_REC PICTURE '9999999'
*!*	        @ ROW,16 SAY CntRec PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[2]
*!*	        *@ ROW,58 SAY 'ACTUAL    :'
*!*	        @ ROW , 35 SAY NEST_COST2                       PICTURE '999999999.99'
*!*	        @ ROW , 48 SAY NLAN_COST2                       PICTURE '9999999999.99'
*!*	        @ ROW , 62 SAY NACT_COST2                       PICTURE '999999999.99'
*!*	        @ ROW , 75 SAY NACT_COST2 -  NLAN_COST2         PICTURE '9999999.99'
*!*	        lnAct_Lan= lnAct_Lan+(NACT_COST2 -  NLAN_COST2)
*!*	        @ ROW , 85 SAY NLAN_COST2 - NEST_COST2             PICTURE '9999999.999'
*!*	        lnLan_Est=lnLan_Est+(NLAN_COST2 - NEST_COST2)
*!*	        @ ROW , 96 SAY NACT_COST2 - NEST_COST2             PICTURE '9999999.999'
*!*	        lnAct_Est=lnAct_Est+(NACT_COST2 - NEST_COST2)
*!*	        @ ROW , 109 SAY IIF(NLAN_COST2=0,'N/A',(NACT_COST2 / NLAN_COST2)*100)  PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(NEST_COST2=0,'N/A',(NLAN_COST2 / NEST_COST2)*100)  PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(NEST_COST2=0,'N/A',(NACT_COST2 / NEST_COST2)*100)  PICTURE '9999.99'
*!*	        *- ARH 03/27/94 Changed QPW(14) with qCostPrv

*!*	        ROW=ROW+1
*!*	        ******  (3)  **************
*!*	        @ ROW,00 SAY 'DAMAGE  '
*!*	        @ ROW,08 SAY PCS_DAM PICTURE '9999999'
*!*	        @ ROW,16 SAY CntDam PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[3]
*!*	        ***********
*!*	        @ ROW , 35 SAY NEST_COST3                       PICTURE '999999999.99'
*!*	        @ ROW , 48 SAY NLAN_COST3                       PICTURE '9999999999.99'
*!*	        @ ROW , 62 SAY NACT_COST3                       PICTURE '999999999.99'
*!*	        @ ROW , 75 SAY NACT_COST3 -  NLAN_COST3         PICTURE '9999999.99'

*!*	        lnAct_Lan= lnAct_Lan+(NACT_COST3 -  NLAN_COST3)

*!*	        @ ROW , 85 SAY NLAN_COST3 - NEST_COST3             PICTURE '9999999.999'
*!*	        lnLan_Est=lnLan_Est+(NLAN_COST3 - NEST_COST3)

*!*	        @ ROW , 96 SAY NACT_COST3 - NEST_COST3             PICTURE '9999999.999'
*!*	        lnAct_Est=lnAct_Est+(NACT_COST3 - NEST_COST3)

*!*	        @ ROW , 109 SAY IIF(NLAN_COST3=0,'N/A',(NACT_COST3 / NLAN_COST3)*100)  PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(NEST_COST3=0,'N/A',(NLAN_COST3 / NEST_COST3)*100)  PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(NEST_COST3=0,'N/A',(NACT_COST3 / NEST_COST3)*100)  PICTURE '9999.99'

*!*	        *- ARH 03/27/94 Changed QPW(14) with qCostPrv

*!*	        ******  (4)  **************
*!*	        ROW=ROW+1
*!*	        @ ROW,00 SAY 'CANCEL  '
*!*	        @ ROW,08 SAY PCS_CAN PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[4]
*!*	        *@ ROW,58 SAY 'PERCENTAGE:'

*!*	        @ ROW , 35 SAY NEST_COST4                       PICTURE '999999999.99'
*!*	        @ ROW , 48 SAY NLAN_COST4                       PICTURE '9999999999.99'
*!*	        @ ROW , 62 SAY NACT_COST4                       PICTURE '999999999.99'
*!*	        @ ROW , 75 SAY NACT_COST4 -  NLAN_COST4         PICTURE '9999999.99'

*!*	        lnAct_Lan= lnAct_Lan+(NACT_COST4 -  NLAN_COST4)

*!*	        @ ROW , 85 SAY NLAN_COST4 - NEST_COST4             PICTURE '9999999.999'
*!*	        lnLan_Est=lnLan_Est+(NLAN_COST4 - NEST_COST4)

*!*	        @ ROW , 96 SAY NACT_COST4 - NEST_COST4            PICTURE '9999999.999'
*!*	        lnAct_Est=lnAct_Est+(NACT_COST4 - NEST_COST4)

*!*	        @ ROW , 109 SAY IIF(NLAN_COST4=0,'N/A',(NACT_COST4 / NLAN_COST4)*100)  PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(NEST_COST4=0,'N/A',(NLAN_COST4 / NEST_COST4)*100)  PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(NEST_COST4=0,'N/A',(NACT_COST4 / NEST_COST4)*100)  PICTURE '9999.99'

*!*	        *- ARH 03/27/94 Changed QPW(14) with qCostPrv
*!*	        ******  (5)  **************
*!*	        ROW=ROW+1
*!*	        @ ROW,00 SAY 'OPEN    '
*!*	        @ ROW,08 SAY PCS_OPN PICTURE '9999999'
*!*	        *@ ROW,16 SAY CONT_OPN PICTURE '9999999'
*!*	        @ ROW,16 SAY lnOpen PICTURE '9999999'
*!*	        @ ROW,25 SAY laCost[5]
*!*	        * @ ROW,58 SAY 'ACTUAL    :'

*!*	        @ ROW , 35 SAY NEST_COST5                       PICTURE '999999999.99'
*!*	        @ ROW , 48 SAY NLAN_COST5                       PICTURE '9999999999.99'
*!*	        @ ROW , 62 SAY NACT_COST5                       PICTURE '999999999.99'
*!*	        @ ROW , 75 SAY NACT_COST5 -  NLAN_COST5         PICTURE '9999999.99'

*!*	        lnAct_Lan= lnAct_Lan+(NACT_COST5 -  NLAN_COST5)

*!*	        @ ROW , 85 SAY NLAN_COST5 - NEST_COST5             PICTURE '9999999.999'
*!*	        lnLan_Est=lnLan_Est+(NLAN_COST5 - NEST_COST5)

*!*	        @ ROW , 96 SAY NACT_COST5 - NEST_COST5             PICTURE '9999999.999'
*!*	        lnAct_Est=lnAct_Est+(NACT_COST5 - NEST_COST5)

*!*	        @ ROW , 109 SAY IIF(NLAN_COST5=0,'N/A',(NACT_COST5 / NLAN_COST5)*100)  PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(NEST_COST5=0,'N/A',(NLAN_COST5 / NEST_COST5)*100)  PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(NEST_COST5=0,'N/A',(NACT_COST5 / NEST_COST5)*100)  PICTURE '9999.99'


*!*	        lnPC_BUD =lnPC_BUD+PCS_BUD
*!*	        *lnCON_BUD=lnCON_ISS+CONT_ISS

*!*	        lnPC_REC =lnPC_REC+PCS_REC
*!*	        *lnCON_REC=lnCON_REC+CONT_REC

*!*	        lnPC_DAM =lnPC_DAM+PCS_DAM
*!*	        *lnCON_DAM=lnCON_DAM+CONT_DAM

*!*	        lnPC_CAN =lnPC_CAN+PCS_CAN

*!*	        lnPC_OPN =lnPC_OPN+PCS_OPN
*!*	        *lnCON_OPN=lnCON_OPN+CONT_OPN

*!*	        laSEst[1] = laSEst[1]+NEST_COST1
*!*	        laSEst[2] = laSEst[2]+NEST_COST2
*!*	        laSEst[3] = laSEst[3]+NEST_COST3
*!*	        laSEst[4] = laSEst[4]+NEST_COST4
*!*	        laSEst[5] = laSEst[5]+NEST_COST5

*!*	        laSAct[1] = laSAct[1]+NACT_COST1
*!*	        laSAct[2] = laSAct[2]+NACT_COST2
*!*	        laSAct[3] = laSAct[3]+NACT_COST3
*!*	        laSAct[4] = laSAct[4]+NACT_COST4
*!*	        laSAct[5] = laSAct[5]+NACT_COST5

*!*	        laSLan[1] = laSLan[1]+NLAN_COST1
*!*	        laSLan[2] = laSLan[2]+NLAN_COST2
*!*	        laSLan[3] = laSLan[3]+NLAN_COST3
*!*	        laSLan[4] = laSLan[4]+NLAN_COST4
*!*	        laSLan[5] = laSLan[5]+NLAN_COST5

*!*	        ROW = ROW+1
*!*	        @ ROW , 35  SAY '  ----------    ----------   ----------  ---------    -------  ---------  ------- ------- ------- '
*!*	        ROW = ROW+1

*!*	        @ ROW,23 SAY 'Total Cost'
*!*	        @ ROW , 36 SAY NEST_COST1+NEST_COST2+NEST_COST3+NEST_COST4+NEST_COST5                  PICTURE '9999999.999'

*!*	        lnEst = NEST_COST1+NEST_COST2+NEST_COST3+NEST_COST4+NEST_COST5

*!*	        @ ROW , 50 SAY NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5   PICTURE '9999999.999'

*!*	        lnLand = NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5

*!*	        @ ROW , 63 SAY NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5   PICTURE '9999999.999'

*!*	        lnAcT  = NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5

*!*	        @ ROW , 74 SAY lnAct_Lan                                                PICTURE '9999999.999'
*!*	        @ ROW , 85 SAY lnLan_Est                                                PICTURE '9999999.999'
*!*	        @ ROW , 96 SAY lnAct_Est                                                PICTURE '9999999.999'
*!*	        @ ROW , 109 SAY IIF(lnLand=0,'N/A',(lnAcT / lnLand)*100)                PICTURE '9999.99'
*!*	        @ ROW , 117 SAY IIF(lnEst=0,'N/A',(lnLand / lnEst)*100)                 PICTURE '9999.99'
*!*	        @ ROW , 125 SAY IIF(lnEst=0,'N/A',(lnAcT / lnEst)*100)                  PICTURE '9999.99'
*!*	        ROW = ROW + 1
*!*	        @ ROW ,35 SAY '  ==========    ==========   ==========  =========    =======  =========  ======= ======= ======= '
*!*	        lnFinEst = lnFinEst+lnEst
*!*	        lnFinLan = lnFinLan+lnLand
*!*	        lnFinAct = lnFinAct+lnAcT
*!*	        lnFinA_L = lnFinA_L+lnAct_Lan
*!*	        lnFinL_E = lnFinL_E+lnLan_Est
*!*	        lnFinA_E = lnFinA_E+lnAct_Est
*!*	        lnFinAbL = lnFinAbL+IIF(lnLand=0,0,(lnAcT / lnLand)*100)
*!*	        lnFinLbE = lnFinLbE+IIF(lnEst=0,0,(lnLand / lnEst)*100)
*!*	        lnFinAbE = lnFinAbE+IIF(lnEst=0,0,(lnAcT / lnEst)*100)

*!*	        STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
*!*	        *E700037,1 TMI 03/22/95  (ENDING)
*!*	      ELSE            *** IF SHORT FORMAT
*!*	        *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!*	        *TICKET #  STATUS  ENTERED   COMPLETE  COMPLETE %  STYLE            SEASON  DIVISION   BUDGET  COMPLETE   DAMAGE   CANCEL     OPEN
*!*	        *XXXXXX      X     XX/XX/XX  XX/XX/XX   9999.99 %  XXXXXXXXXXXXXXX    XX       XX     9999999   9999999  9999999  9999999  9999999
*!*	        @ ROW,00 SAY CUTTKT
*!*	        @ ROW,12 SAY STATUS
*!*	        @ ROW,18 SAY ENTERED
*!*	        @ ROW,28 SAY COMPLETE
*!*	        @ ROW,39 SAY XCOMPCT PICTURE '9999.99'
*!*	        @ ROW,50 SAY STYLE
*!*	        @ ROW,69 SAY SEASON
*!*	        @ ROW,78 SAY CDIVISION
*!*	        @ ROW,85 SAY PCS_BUD  PICTURE '9999999'
*!*	        @ ROW,95 SAY PCS_REC  PICTURE '9999999'
*!*	        @ ROW,104 SAY PCS_DAM  PICTURE '9999999'
*!*	        @ ROW,113 SAY PCS_CAN PICTURE '9999999'
*!*	        @ ROW,122 SAY PCS_OPN PICTURE '9999999'
*!*	      ENDIF
*!*	      XTOT1(1) = XTOT1(1) + PCS_BUD
*!*	      XTOT1(2) = XTOT1(2) + PCS_REC
*!*	      XTOT1(3) = XTOT1(3) + PCS_DAM
*!*	      XTOT1(4) = XTOT1(4) + PCS_CAN
*!*	      XTOT1(5) = XTOT1(5) + PCS_OPN

*!*	      *----- PUT CODE HERE FOR FIGURES FROM CONTRACTOR ----*
*!*	      *XTOT2(1) = XTOT2(1) + CONT_ISS
*!*	      *XTOT2(2) = XTOT2(2) + CONT_REC
*!*	      *XTOT2(3) = XTOT2(3) + CONT_DAM
*!*	      *XTOT2(4) = XTOT2(4) + CONT_OPN

*!*	      XTOT2(1) = XTOT2(1) + CNTISS
*!*	      XTOT2(2) = XTOT2(2) + CNTREC
*!*	      XTOT2(3) = XTOT2(3) + CNTDAM
*!*	      XTOT2(4) = XTOT2(4) + CNTOPN




*!*	      ******************************************************

*!*	      *XTOT3(1) = XTOT3(1) + ES_MAT_CST
*!*	      *XTOT3(2) = XTOT3(2) + AC_MAT_CST

*!*	      *XTOT4(1) = XTOT4(1) + ES_LAB_CST
*!*	      *XTOT4(2) = XTOT4(2) + AC_LAB_CST

*!*	      ROW=ROW+1

*!*	      *B803314,1 ARH 06/01/2000 (Begin) Uncomented the following lines to fix the bug of 
*!*	      *B803314,1 ARH                    not printiing the notepad in any case
*!*	      *IF XPRINTNOTE = 'Y'
*!*	      *  DO PRT_NOTE WITH 'I',XCUT
*!*	      *ENDIF

*!*	      IF XPRINTNOTE = 'Y'
*!*	        DO PRT_NOTE WITH 'I',XCUT
*!*	      ENDIF
*!*	      *B803314,1 ARH 06/01/2000 (End)

*!*	      SELECT &WORKFILE
*!*	      SKIP
*!*	   ENDDO

*!*	   ****************
*!*	   *------------------ END MAIN REPORT LOOP --------------------

*!*	   IF EMPTY(BREAK)              && IN CASE OF SORT BY CUTTKT WHEN NO TOTALS
*!*	      X = 1
*!*	      DO WHILE X<=5
*!*	         XGTOT1(X) = XGTOT1(X) + XTOT1(X)
*!*	         XTOT1(X) = 0.00
*!*	         IF X<=4   && CONTRACTOR TOTALS ARE ONLY 4
*!*	           XGTOT2(X) = XGTOT2(X) + XTOT2(X)
*!*	           XTOT2(X) = 0.00
*!*	         ENDIF
*!*	         X = X+1
*!*	      ENDDO
*!*	      X = 1
*!*	      DO WHILE X <=2
*!*	        XGTOT3(X) = XGTOT3(X) + XTOT3(X)
*!*	        XGTOT4(X) = XGTOT4(X) + XTOT4(X)
*!*	        XTOT3(X) = 0.00
*!*	        XTOT4(X) = 0.00
*!*	        X = X + 1
*!*	      ENDDO
*!*	   ENDIF

*!*	   ROW=ROW+2
*!*	   @ ROW,00 SAY REPLICATE('*',132)
*!*	   ROW = ROW+1

*!*	   IF XLONG = 'Y'
*!*	     *E700037,1 TMI 03/22/95 (BEGIN) Printing grand total.
*!*	     @ ROW,000 SAY '**** GRAND TOTAL *****'
*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY '        CUT TKT  CONTR.              ESTIMATED       LANDED        ACTUAL     ACT-LAN   LAND-EST    ACT-EST   ACT/LAN LAN/EST ACT/EST'
*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY 'BUDGET  '
*!*	     @ ROW,08 SAY laBudget[1]    PICTURE '9999999'
*!*	     @ ROW,16 SAY laBudget[2]    PICTURE '9999999'
*!*	     @ ROW,25 SAY laCost[1]
*!*	     @ ROW,35 SAY laEstCost[1]                                               PICTURE '9999999.999'
*!*	     @ ROW,48 SAY laLanCost[1]                                               PICTURE '9999999.999'
*!*	     @ ROW,62 SAY laActCost[1]                                               PICTURE '9999999.999'
*!*	     @ ROW,75 SAY laActCost[1] -  laLanCost[1]                               PICTURE '9999999.99'
*!*	     @ ROW,86 SAY laLanCost[1] -  laEstCost[1]                               PICTURE '9999999.99'
*!*	     @ ROW,97 SAY laActCost[1] -  laEstCost[1]                               PICTURE '9999999.99'
*!*	     @ ROW,110 SAY IIF(laLanCost[1]=0,'N/A',(laActCost[1] / laLanCost[1])*100)         PICTURE '9999.99'
*!*	     @ ROW,118 SAY IIF(laESTCost[1]=0,'N/A',(laLanCost[1] / laEstCost[1])*100)         PICTURE '9999.99'
*!*	     @ ROW,126 SAY IIF(laEstCost[1]=0,'N/A',(laActCost[1] / laEstCost[1])*100)         PICTURE '9999.99'

*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY 'RECEIVE '
*!*	     @ ROW,08 SAY laREC[1]    PICTURE '9999999'
*!*	     @ ROW,16 SAY laREC[2]    PICTURE '9999999'
*!*	     @ ROW,25 SAY laCost[2]
*!*	     @ ROW,35 SAY laEstCost[2]                                               PICTURE '9999999.999'
*!*	     @ ROW,48 SAY laLanCost[2]                                               PICTURE '9999999.999'
*!*	     @ ROW,62 SAY laActCost[2]                                               PICTURE '9999999.999'
*!*	     @ ROW,75 SAY laActCost[2] -  laLanCost[2]                               PICTURE '9999999.99'
*!*	     @ ROW,86 SAY laLanCost[2] -  laEstCost[2]                               PICTURE '9999999.99'
*!*	     @ ROW,97 SAY laActCost[2] -  laEstCost[2]                               PICTURE '9999999.99'
*!*	     @ ROW,110 SAY IIF(laLanCost[2]=0,'N/A',(laActCost[2] / laLanCost[2])*100)         PICTURE '9999.99'
*!*	     @ ROW,118 SAY IIF(laESTCost[2]=0,'N/A',(laLanCost[2] / laEstCost[2])*100)         PICTURE '9999.99'
*!*	     @ ROW,126 SAY IIF(laEstCost[2]=0,'N/A',(laActCost[2] / laEstCost[2])*100)         PICTURE '9999.99'

*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY 'DAM     '
*!*	     @ ROW,08 SAY laDAM[1]    PICTURE '9999999'
*!*	     @ ROW,16 SAY laDAM[2]    PICTURE '9999999'
*!*	     @ ROW,25 SAY laCost[3]
*!*	     @ ROW,35 SAY laEstCost[3]                                               PICTURE '9999999.999'
*!*	     @ ROW,48 SAY laLanCost[3]                                               PICTURE '9999999.999'
*!*	     @ ROW,62 SAY laActCost[3]                                               PICTURE '9999999.999'
*!*	     @ ROW,75 SAY laActCost[3] -  laLanCost[3]                               PICTURE '9999999.99'
*!*	     @ ROW,86 SAY laLanCost[3] -  laEstCost[3]                               PICTURE '9999999.99'
*!*	     @ ROW,97 SAY laActCost[3] -  laEstCost[3]                               PICTURE '9999999.99'
*!*	     @ ROW,110 SAY IIF(laLanCost[3]=0,'N/A',(laActCost[3] / laLanCost[3])*100)         PICTURE '9999.99'
*!*	     @ ROW,118 SAY IIF(laESTCost[3]=0,'N/A',(laLanCost[3] / laEstCost[3])*100)         PICTURE '9999.99'
*!*	     @ ROW,126 SAY IIF(laEstCost[3]=0,'N/A',(laActCost[3] / laEstCost[3])*100)         PICTURE '9999.99'


*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY 'CANCEL  '
*!*	     @ ROW,08 SAY laCAN[1]    PICTURE '9999999'
*!*	     @ ROW,25 SAY laCost[4]
*!*	     @ ROW,35 SAY laEstCost[4]                                              PICTURE '9999999.999'
*!*	     @ ROW,48 SAY laLanCost[4]                                               PICTURE '9999999.999'
*!*	     @ ROW,62 SAY laActCost[4]                                               PICTURE '9999999.999'
*!*	     @ ROW,75 SAY laActCost[4] -  laLanCost[4]                               PICTURE '9999999.99'
*!*	     @ ROW,86 SAY laLanCost[4] -  laEstCost[4]                               PICTURE '9999999.99'
*!*	     @ ROW,97 SAY laActCost[4] -  laEstCost[4]                               PICTURE '9999999.99'
*!*	     @ ROW,110 SAY IIF(laLanCost[4]=0,'N/A',(laActCost[4] / laLanCost[4])*100)         PICTURE '9999.99'
*!*	     @ ROW,118 SAY IIF(laESTCost[4]=0,'N/A',(laLanCost[4] / laEstCost[4])*100)         PICTURE '9999.99'
*!*	     @ ROW,126 SAY IIF(laEstCost[4]=0,'N/A',(laActCost[4] / laEstCost[4])*100)         PICTURE '9999.99'


*!*	     ROW=ROW+1
*!*	     @ ROW,00 SAY 'OPEN    '
*!*	     @ ROW,08 SAY laOPN[1]    PICTURE '9999999'
*!*	     @ ROW,16 SAY laOPN    PICTURE '9999999'
*!*	     @ ROW,25 SAY laCost[5]
*!*	     @ ROW,35 SAY laEstCost[5]                                               PICTURE '9999999.999'
*!*	     @ ROW,48 SAY laLanCost[5]                                               PICTURE '9999999.999'
*!*	     @ ROW,62 SAY laActCost[5]                                               PICTURE '9999999.999'
*!*	     @ ROW,75 SAY laActCost[5] -  laLanCost[5]                               PICTURE '9999999.99'
*!*	     @ ROW,86 SAY laLanCost[5] -  laEstCost[5]                               PICTURE '9999999.99'
*!*	     @ ROW,97 SAY laActCost[5] -  laEstCost[5]                               PICTURE '9999999.99'
*!*	     @ ROW,110 SAY IIF(laLanCost[5]=0,'N/A',(laActCost[5] / laLanCost[5])*100)         PICTURE '9999.99'
*!*	     @ ROW,118 SAY IIF(laESTCost[5]=0,'N/A',(laLanCost[5] / laEstCost[5])*100)         PICTURE '9999.99'
*!*	     @ ROW,126 SAY IIF(laEstCost[5]=0,'N/A',(laActCost[5] / laEstCost[5])*100)         PICTURE '9999.99'
*!*	     ROW=ROW+1
*!*	     FOR I = 1 TO 5
*!*	       lnFinEst=lnFinEst+ laEST[I]
*!*	       lnFinLan=lnFinLan+ laLan[I]
*!*	       lnFinAct=lnFinAct+ laAct[I]
*!*	     ENDFOR
*!*	     @ ROW ,00 SAY '         ------  ------             ----------   ----------     ---------  ----------    ------- ----------   ------- ------- -------'
*!*	     ROW = ROW + 1
*!*	     @ ROW , 02 SAY 'Total'
*!*	     @ ROW , 08 SAY laBudget[1]+laRec[1]+laDam[1]+laCan[1]+laOpn[1]   PICTURE '9999999'
*!*	     @ ROW , 16 SAY laBudget[2]+laRec[2]+laDam[2]+laOpn               PICTURE '9999999'

*!*	     @ ROW , 35 SAY lnFinEst                     PICTURE '9999999.999'
*!*	     lnEst = NEST_COST1+NEST_COST2+NEST_COST3+NEST_COST4+NEST_COST5
*!*	     @ ROW , 48 SAY lnFinLan   PICTURE '9999999.999'
*!*	     lnLand = NLAN_COST1+NLAN_COST2+NLAN_COST3+NLAN_COST4+NLAN_COST5

*!*	     @ ROW , 62 SAY lnFinAct   PICTURE '9999999.999'
*!*	     lnAcT  = NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5
*!*	     @ ROW , 74 SAY lnFinAct-lnFinLan                                                PICTURE '9999999.999'
*!*	     @ ROW , 85 SAY lnFinLan-lnFinEst                                                PICTURE '9999999.999'
*!*	     @ ROW , 96 SAY lnFinAct-lnFinEst                                                PICTURE '9999999.999'
*!*	     @ ROW , 110 SAY IIF(lnFinLan=0,'N/A',(lnFinAct / lnFinLan)*100)                 PICTURE '9999.99'
*!*	     @ ROW , 118 SAY IIF(lnFinEst=0,'N/A',(lnFinLan / lnFinEst)*100)                  PICTURE '9999.99'
*!*	     @ ROW , 126 SAY IIF(lnFinEst=0,'N/A',(lnFinAct / lnFinEst)*100)                  PICTURE '9999.99'
*!*	     ROW = ROW + 1
*!*	     @ ROW ,00 SAY '         ====== =======              =========     ========     =========  ==========   ========   ========   ======= ======= ======= '
*!*	     *N700037,1 TMI 03/22/95 (END)

*!*	     *---- CALCULATE THE TOTAL FIELDS -----*
*!*	     *XMAT_VAR=XGTOT3(2) - XGTOT3(1)
*!*	     *XLAB_VAR=XGTOT4(2) - XGTOT4(1)
*!*	     *XMAT_PCT=IIF(XGTOT3(1) <> 0,ROUND(XGTOT3(2)/XGTOT3(1)*100,2),0.00)
*!*	     *XLAB_PCT=IIF(XGTOT4(1) <> 0,ROUND(XGTOT4(2)/XGTOT4(1)*100,2),0.00)
*!*	     *XGTOT_EST=XGTOT3(1)  + XGTOT4(1)
*!*	     *XGTOT_ACT=XGTOT3(2)  + XGTOT4(2)
*!*	     *XGTOT_VAR=XGTOT_ACT   - XGTOT_EST
*!*	     *XGTOT_PCT=IIF(XGTOT_EST <> 0,ROUND(XGTOT_ACT/XGTOT_EST*100,2),0.00)
*!*	     *XES_UNTCST=IIF(XGTOT1(1)<>0,ROUND(XGTOT_EST/XGTOT1(1),2),0.00)
*!*	     *XAC_UNTCST=IIF((XGTOT1(2)+XGTOT1(3))<>0,ROUND(XGTOT_ACT/(XGTOT1(2)+XGTOT1(3)),2),0.00)
*!*	*--------------------------------------------------------------------------------*
*!*	   ELSE
*!*	     @ ROW,00 SAY '* GRAND TOTAL *'
*!*	     @ ROW,84 SAY XGTOT1(1) PICTURE '99999999'
*!*	     @ ROW,94 SAY XGTOT1(2) PICTURE '99999999'
*!*	     @ ROW,103 SAY XGTOT1(3) PICTURE '99999999'
*!*	     @ ROW,112 SAY XGTOT1(4) PICTURE '99999999'
*!*	     @ ROW,121 SAY XGTOT1(5) PICTURE '99999999'
*!*	   ENDIF
*!*	   ROW = ROW+1
*!*	   @ ROW,00 SAY REPLICATE('*',132)
*!*	   EXIT
*!*	ENDDO

*!*	*-- HDM
*!*	*DO ENDPRT         && END THE REPORT OR DISPLAY ON SCREEN

*!*	DO ENDREPORT

*!*	*-- HDM
*!*	SET DEVICE TO SCREEN
*!*	RETURN
*---------------------------------------------------------------------------------------------

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen
DECLARE laRpSource[7],laRpTarget[1]

*--HDM B802368,1 [Start] assign '' to laRpTarget
STORE '' TO laRpTarget
*--HDM B802368,1 [End]



lnStatus = lcRpStatus
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
*!*	STORE 'Open'     TO laRpSource[1] &&,laRpTarget[1]
*!*	STORE 'Hold'     TO laRpSource[2] &&,laRpTarget[2]
*!*	STORE 'Canceled' TO laRpSource[3]
*!*	STORE 'Complete' TO laRpSource[4]
*!*	STORE 'Closed'   TO laRpSource[5]
*!*	STORE 'Actual'   TO laRpSource[6]
*!*	STORE 'Bid'      TO laRpSource[7]
STORE LANG_OPEN      TO laRpSource[1] &&,laRpTarget[1]
STORE LANG_HOLD      TO laRpSource[2] &&,laRpTarget[2]
STORE LANG_CANCELLED TO laRpSource[3]
STORE LANG_COMPLETE  TO laRpSource[4]
STORE LANG_CLOSED    TO laRpSource[5]
STORE LANG_ACTUAL    TO laRpSource[6]
STORE LANG_BID       TO laRpSource[7]
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
lcRpStatus = ''
=gfOpenTable(oAriaApplication.Datadir+'NOTEPAD','NOTEPAD','SH')
=gfOpenTable(oAriaApplication.Datadir+'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.Datadir+'CODES','CODES','SH')


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
ENDCASE

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

*= gfMover(@laRpSource,@laRpTarget,'Cutting Ticket Status',.T.,'')  && call mover function.
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Cutting Ticket Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.ActiveModuleID = 'MA',LANG_Order_STATUS, LANG_CUTTKT_STATUS),.T.,'')  && call mover function.
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
*!*	  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O|',;
*!*	                            IIF(laRpTarget[lnI] = 'Hold','H|',;
*!*	                            IIF(laRpTarget[lnI] = 'Canceled','X|',;
*!*	                            IIF(laRpTarget[lnI] = 'Complete','C|',;
*!*	                            IIF(laRpTarget[lnI] = 'CLOSED'  ,'S|',;
*!*	                            IIF(laRpTarget[lnI] = 'Bid'  ,'B|',;                         
*!*	                            IIF(laRpTarget[lnI] = 'Actual'  ,'A|','')))))))
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = LANG_OPEN,'O|',;
                            IIF(laRpTarget[lnI] = LANG_HOLD,'H|',;
                            IIF(laRpTarget[lnI] = LANG_CANCELLED,'X|',;
                            IIF(laRpTarget[lnI] = LANG_COMPLETE,'C|',;
                            IIF(laRpTarget[lnI] = LANG_CLOSED  ,'S|',;
                            IIF(laRpTarget[lnI] = LANG_BID  ,'B|',;                         
                            IIF(laRpTarget[lnI] = LANG_ACTUAL   ,'A|','')))))))
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
ENDFOR  && end Loop to make Status expression.
IF RIGHT(lcRPStatus,1) = '|'
  lcRPStatus = SUBSTR(lcRPStatus,1,LEN(lcRPStatus)-1)  
ENDIF
lcRPStatus = STRTRAN(lcRPStatus,'|',"','") 
lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.
ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.
*------------------------------------------------------------------

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
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

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
*DIMENSION loOgScroll.laCRParams[6,2]
DIMENSION loOgScroll.laCRParams[7,2]
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
*- Cost Labels
DIMENSION laCost [7]
STORE SPACE(9) TO laCost [7]
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
IF oAriaApplication.ActiveModuleID = 'MA'
	laCost[1] = (ALLTRIM(gfGetMemVar('M_CTSLBL1',gcAct_Comp)))
	laCost[2] = (ALLTRIM(gfGetMemVar('M_CTSLBL2',gcAct_Comp)))
	laCost[3] = (ALLTRIM(gfGetMemVar('M_CTSLBL3',gcAct_Comp)))
	laCost[4] = (ALLTRIM(gfGetMemVar('M_CTSLBL4',gcAct_Comp)))
	laCost[5] = (ALLTRIM(gfGetMemVar('M_CTSLBL5',gcAct_Comp)))
	laCost[6] = (ALLTRIM(gfGetMemVar('M_CTSLBL6',gcAct_Comp)))
	laCost[7] = (ALLTRIM(gfGetMemVar('M_CTSLBL7',gcAct_Comp)))

ELSE
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
  laCost[1] = (ALLTRIM(gfGetMemVar('M_cMSLbl1',gcAct_Comp)))
  laCost[2] = (ALLTRIM(gfGetMemVar('M_cMSLbl2',gcAct_Comp)))
  laCost[3] = (ALLTRIM(gfGetMemVar('M_cMSLbl3',gcAct_Comp)))
  laCost[4] = (ALLTRIM(gfGetMemVar('M_cMSLbl4',gcAct_Comp)))
  laCost[5] = (ALLTRIM(gfGetMemVar('M_cMSLbl5',gcAct_Comp)))
  laCost[6] = (ALLTRIM(gfGetMemVar('M_cMSLbl6',gcAct_Comp)))
  laCost[7] = (ALLTRIM(gfGetMemVar('M_cMSLbl7',gcAct_Comp)))
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
ENDIF
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
*- Cost Labels
loOgScroll.laCRParams[1,1] = 'Layout'


IF lcRPFormat = 'MFCTHDRS'
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  *loOgScroll.laCRParams[1,2] = 'Short Format'  
  loOgScroll.laCRParams[1,2] = LANG_SFORMAT
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
ELSE
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  *loOgScroll.laCRParams[1,2] = 'Long Format'
  loOgScroll.laCRParams[1,2] = LANG_LFORMAT 
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
ENDIF

loOgScroll.laCRParams[2,1] = 'OpTitle'
loOgScroll.laCRParams[2,2] = lcRpTitle


loOgScroll.laCRParams[3,1] = 'SortBy'
IF lcRPSortBy = 'C'   
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  *loOgScroll.laCRParams[3,2] = 'Cut Ticket #'
  IF oAriaApplication.ActiveModuleID = 'MA'
    loOgScroll.laCRParams[3,2] = LANG_ORDERNUMBER
  ELSE
    loOgScroll.laCRParams[3,2] = LANG_CUTTKTNUMBER
  ENDIF
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
ELSE
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  *loOgScroll.laCRParams[3,2] = 'Date'
  IF oAriaApplication.ActiveModuleID = 'MA'
    IF lcRPSortBy = 'D'
      loOgScroll.laCRParams[3,2] = LANG_DATE 
    ELSE
      loOgScroll.laCRParams[3,2] = LANG_ITEM 
    ENDIF    
  ELSE
    loOgScroll.laCRParams[3,2] = LANG_DATE 
  ENDIF
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]  
ENDIF


loOgScroll.laCRParams[4,1] = 'ReportName'
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
*  loOgScroll.laCRParams[4,2] = 'Cut Ticket Header Report'
IF oAriaApplication.ActiveModuleID = 'MA'
  loOgScroll.laCRParams[4,2] = LANG_MMOHEADER 
ELSE
  loOgScroll.laCRParams[4,2] = LANG_CUTHEADER
ENDIF  
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
loOgScroll.laCRParams[5,1] = 'ShowNote'
loOgScroll.laCRParams[5,2] =  IIF(lcPrntNote='Y',1,0)

loOgScroll.laCRParams[6,1] = 'lnMajSize'
loOgScroll.laCRParams[6,2] =  lnMajSize

*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
loOgScroll.laCRParams[7,1] = 'ActiveModule'
loOgScroll.laCRParams[7,2] =  oAriaApplication.ActiveModuleID
*: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
IF lcRpFormat='MFCTHDRL' then
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  *DIMENSION loOgScroll.laCRParams[13,2]
  DIMENSION loOgScroll.laCRParams[14,2]  
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
  loOgScroll.laCRParams[7,1] = "rp_cost1"
  loOgScroll.laCRParams[7,2] = laCost[1]
  loOgScroll.laCRParams[8,1] = "rp_cost2"
  loOgScroll.laCRParams[8,2] = laCost[2]
  loOgScroll.laCRParams[9,1] = "rp_cost3"
  loOgScroll.laCRParams[9,2] = laCost[3]
  loOgScroll.laCRParams[10,1] = "rp_cost4"
  loOgScroll.laCRParams[10,2] = laCost[4]
  loOgScroll.laCRParams[11,1] = "rp_cost5"
  loOgScroll.laCRParams[11,2] = laCost[5]
  loOgScroll.laCRParams[12,1] = "rp_cost6"
  loOgScroll.laCRParams[12,2] = laCost[6]
  loOgScroll.laCRParams[13,1] = "rp_cost7"
  loOgScroll.laCRParams[13,2] = laCost[7]
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][Start]
  loOgScroll.laCRParams[14,1] = 'ActiveModule'
  loOgScroll.laCRParams[14,2] =  oAriaApplication.ActiveModuleID
  *: E303288,1 MMT 10/30/2012 Convert MMO Header report to Aria4XP[T20110914.0019][End]
  DIMENSION LOogsCROLL.laCRTables[3]
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRpTmpNote + ".DBF"
  loOgScroll.laCRTables[3] = oAriaApplication.WorkDir +  lcRpTmpStyle+ ".DBF"
ELSE
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF"
  loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRpTmpNote + ".DBF"
ENDIF

*************************************************************
*! Name      : lfvFormat
*! Developer : Saeed Mohammed (SMM)
*! Date      : 07/18/2005
*! Purpose   : To set the report layout
*!*************************************************************
FUNCTION lfvFormat

loOgScroll.lcOGLastForm = lcRPFormat
*!*****************************************************************************************
*! Name      : RefreshStatus
*! Developer : MMT
*! Date      : 09/14/2006
*! Purpose   : To dislay the staus in the text box
*! Entry no. : 037717
*!*****************************************************************************************
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
*-- end of RefreshStatus.

