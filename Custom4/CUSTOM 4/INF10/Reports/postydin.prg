*:***************************************************************************
*: Program File  : POSTYDIN(N037716)
*: Program Desc. : P/O Detail report (Style, Material.  
*: System        : Aria 4 XP.
*: Module        :  STYLE PURCHASE ORDER DETAIL (PO)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************

*!*	_SCREEN.Visible= .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSPEND

LOCAL lnPOS 	     && Holds the position of a specified sub string
LOCAL lcNotes      && Holds the PO notes 
LOCAL lcSQLStmt    && Holds the SQL statement
LOCAL lcFields     && Holds the fields list to get from SQL
LOCAL lcWhereCon   && Holds the SQL where condition
LOCAL lcJoin       && Holds the SQL join expression 
LOCAL lcSQLTemp    && Holds the SQL tmp file name
LOCAL lnResult	   && Holds the result of the remote data access
LOCAL llStyle      
LOCAL llFabric
LOCAL llSqlStyGP
LOCAL llPO
LOCAL llVendor
LOCAL llLocation
LOCAL lcSQLStyle
LOCAL lcSQLFabric
LOCAL lcSQLPO
LOCAL lcSQLVendor
LOCAL lcSQLLocation
LOCAL lcSQLStyGP
LOCAL lcCon
LOCAL lcTemp
LOCAL lcTrxType
LOCAL llSqlDEPT 
LOCAL lcSqlDEPT 
lcTrxType = ""

IF loOgScroll.llOGFltCh 
  * Check if there is a filter on PO Number
  lcCurName = lfCheckFilter(1, 'POSHDR.PO')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName  	
    llPO  = ( RECCOUNT() > 0) 
    IF llPO 
      LOCAL lnConn
      lcSQLPO = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO') && SQL Temp File
      IF EMPTY(lcSQLPO)
	      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  
  *-- Check if there is a filter on Vendor
  lcCurName = lfCheckFilter(1, 'POSHDR.VENDOR')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName  	
    llVendor = ( RECCOUNT() > 0) 
    IF llVendor 
      lcSQLVendor = loOgScroll.gfSQLTempName('','Vendor C(8)',lcCurName,'cVendCode') && SQL Temp File
      IF EMPTY(lcSQLVendor)
	      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF

  *-- Check if there is a filter on Location
  lcCurName = lfCheckFilter(1, 'POSLN.CWARECODE')  	
  IF !EMPTY(lcCurName)
    SELECT &lcCurName  	
    llLocation = ( RECCOUNT() > 0) 
    IF llLocation
      lcSQLLocation = loOgScroll.gfSQLTempName('','Location C(6)',lcCurName,'cWareCode') && SQL Temp File
      IF EMPTY(lcSQLLocation)
	      *-- SQL connection error. can't open the report
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF

    *-- Check if there is a filter on Style
    lcCurName = lfCheckFilter(1, 'STYLE.CSTYMAJOR')  	
  	IF !EMPTY(lcCurName) 
      SELECT &lcCurName  	
      llStyle = ( RECCOUNT() > 0) 
      IF llStyle
        lcTemp = loOgScroll.gfTempName()
        SELECT a.Style FROM Style a INNER join &lcCurName b ON a.cStyMajor=b.cStyMajor ;
        Into cursor &lcTemp
        lcSQLStyle = loOgScroll.gfSQLTempName('','STYLE C(19)', lcTemp,'Style') && SQL Temp File
        IF EMPTY(lcSQLStyle)
	        *-- SQL connection error. can't open the report
          =gfModalGen('TRM00416B40011','ALERT')
          RETURN .F.
        ENDIF
      ENDIF
    ENDIF
      * Get Style Group Filter
    LOCAL lcSearch
    lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')  	
    IF !EMPTY(lcCurName) 
      llSqlStyGP = .T.
      lcSQLStyGP = loOgScroll.gfSQLTempName('','STYLE C(19)','','') && SQL Temp File
      IF EMPTY(lcSQLStyGP)
				*-- SQL connection error. can't open the report      
        =gfModalGen('TRM00416B40011','ALERT')
        RETURN .F.
      ENDIF
      lcCon = lcCurName
      lcCon = "|" + lcCon + "|"
      DO WHILE LEN(lcCon) > 1 
        lnPos = AT("|",lcCon,2)
        lcSearch = SUBSTR(lcCon ,2,lnPos-2)
        lcCon = STRTRAN(lcCon,"|"+lcSearch,"")
        SELECT STYLE      
        SET FILTER TO ALLTRIM(cStyGroup) = ALLTRIM(lcSearch)
        SCAN
          lcSQLStmt = " INSERT INTO " + lcSQLStyGP + " Values ('" + STYLE.Style + "')"
          lnResult  = loOgScroll.oRDA.SqlRun(lcSQLStmt,,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
        ENDSCAN
      ENDDO
      SET FILTER TO 
    ENDIF




  *-- Selection Formula 
  lcWhereCon = "POSLN.CBUSDOCU= '"+ lcBusDocu +"' AND POSLN.CSTYTYPE = '" + lcStyType +"' AND (SHPMTHDR.CSHPTYPE='" + lcStyType + "' OR POSLN.SHIPNO='')" 
  lcWhereCon = lcWhereCon + " AND POSLN.CINVTYPE= '"  + lcInvType + "'"
  * Get Currency Filter
  lcCurName = lfCheckFilter(1, 'POSHDR.CPRICECUR')
  IF !EMPTY(lcCurName) 
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CPRICECUR IN ('" + lcCon + "')"
  ENDIF
  * Get Status Filter
  lcCurName = lfCheckFilter(1, 'POSHDR.STATUS')
  IF !EMPTY(lcCurName) 
	lcCon = lcCurName 
	lcCon = STRTRAN(lcCon,"|","','")
	lcWhereCon = lcWhereCon + " AND POSHDR.STATUS IN ('" + lcCon + "')"
  ENDIF
  * Get cDivision Filter
  lcCurName = lfCheckFilter(1, 'POSHDR.CDIVISION')
  IF !EMPTY(lcCurName) 
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","','")
    lcWhereCon = lcWhereCon + " AND POSHDR.CDIVISION IN ('" + lcCon + "')"
  ENDIF
  * Get Transaction type Filter
  lcCurName = lfCheckFilter(1, 'POSLN.TRANCD')
  IF !EMPTY(lcCurName) 
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","")
    *!* lcWhereCon = lcWhereCon + " AND POSLN.TRANCD IN ('" + lcCon + "')"
    lcCon = STRTRAN(lcCon,"1","O")
    lcCon = STRTRAN(lcCon,"2","R")
    lcCon = STRTRAN(lcCon,"3","S")
    lcCon = STRTRAN(lcCon,"4","T")
    lcCon = STRTRAN(lcCon,"5","C")
    lcCon = STRTRAN(lcCon,"6","P")

    lcTrxType = lcCon
  ENDIF
  * Get Entered Filter
  lcCurName = lfCheckFilter(1, 'POSHDR.ENTERED')
  IF !EMPTY(lcCurName) 
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.ENTERED Between '" + lcCon + "'"
  ENDIF

  * Get Completed Filter
  lcCurName = lfCheckFilter(1, 'POSHDR.COMPLETE')
  IF !EMPTY(lcCurName) 
    lcCon = lcCurName
    lcCon = STRTRAN(lcCon,"|","' and '")
    lcWhereCon = lcWhereCon + " AND POSHDR.COMPLETE Between '" + lcCon + "'"
  ENDIF

  lcFields = "POSLN.PO,POSLN.cSTYTYPE,POSLN.SHIPNO,Case(Trancd) when 1 "+;
             "then posln.Complete When 3 then SHPMTHDR.ETA Else posln.Date End as Date,POSLN.trancd,POSLN.Scale,POSLN.Style,"+;
             "POSHDR.Vendor,POSLN.niCost1,POSLN.nfCost1,POSHDR.cPriceCur"+;
             ",POSLN.Qty1,POSLN.Qty2,POSLN.Qty3,POSLN.Qty4,POSLN.Qty5,"+;
             "POSLN.Qty6,POSLN.Qty7,POSLN.Qty8,POSHDR.STATUS,UOM.cUOM_B as cUOMCode,UOM.cUOM_V as cUOMUse,UOM.nConf,POSLN.dyelot,POSLN.Pattern,"+;
      			 "POSLN.reference,POSLN.width,POSLN.ccstsht_id"

   lcFields = lcFields + ",'                 ' as [desc]"
 
  
  lcJoin = " POSHDR POSHDR (index=POSHDR) Inner "+;
           "join POSLN POSLN (index=POSLN) on POSHDR.CBUSDOCU = POSLN.CBUSDOCU and "+;
           "POSHDR.CSTYTYPE = POSLN.CSTYTYPE and "+;
           "POSHDR.PO = POSLN.PO Left Outer join SHPMTHDR SHPMTHDR on POSLN.SHIPNO = SHPMTHDR.SHIPNO "+;
           " Left Outer Join UOM UOM on POSLN.cUOMCODE = UOM.cUOMCode "

  IF llPO
    lcJoin = lcJoin + " inner join " + lcSQLPO + " TmpPO on TmpPO.PO = POSLN.PO "
  ENDIF
    
  IF llVendor
    lcJoin = lcJoin +" inner join "+ lcSQLVendor +" TmpVend on TmpVend.Vendor = POSHDR.Vendor "
  ENDIF

  IF llLocation
    lcJoin = lcJoin+" inner join "+lcSQLLocation+" TmpLoc on TmpLoc.Location=POSHDR.cWareCode "
  ENDIF

  IF llStyle
    lcJoin = lcJoin +" inner join "+ lcSQLStyle +" TmpSty on TmpSty.Style = POSLN.Style "
  ENDIF

  IF llFabric
    lcJoin = lcJoin +" inner join "+ lcSQLFabric +" TmpFab on TmpFab.Fabric = Item.cStyMajor"
  ENDIF

  IF llSqlStyGP
    lcJoin = lcJoin +" inner join "+ lcSQLStyGP +" TmpStyGP on TmpStyGP.Style = POSLN.Style "
  ENDIF
 

  

   
  lcSQLPOS = loOgScroll.gfTempName()
  lcSQLStmt = "SELECT " + lcFields + " FROM " + lcJoin + " WHERE " + lcWhereCon
  WAIT WINDOW " Collecting data for Report... " NOWAIT
  lnResult  = loOgScroll.oRDA.SqlRun(lcSQLStmt,lcSQLPOS,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",)
  IF lnResult = 1
    IF loOgScroll.FileExist(oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" )
      ERASE (oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" )
    ENDIF
    SELECT(lcSQLPOS)
    COPY TO oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" WITH CDX
    ALTER TABLE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" ADD column Notes memo
    ALTER TABLE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" ADD column PrnNotes N
    ALTER TABLE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" ADD column Dept C(5)

    SELECT (lcRPHDRTmp)
    Replace ALL PrnNotes WITH 0
    IF lcRpNote='Y'
  		SET RELATION TO 'P'+PO INTO NotePad
  		Replace ALL &lcRPHDRTmp..Notes WITH ALLTRIM(NotePad.Mnotes) FOR !EOF('NotePad')
      Replace ALL PrnNotes WITH 1 FOR !EMPTY(ALLTRIM(Notes)) 
    ENDIF
    * Handling Multi Currency
    IF llMultCurr 
  	  SELECT SYCCURR
  	  SET ORDER TO CCURRCODE 
  	  SELECT &lcRPHDRTmp
  	  SET RELATION TO cPriceCur INTO SYCCURR 
    ENDIF
    SET RELATION TO Style INTO style ADDITIVE 
   	SCAN	
      REPLACE &lcRPHDRTmp..niCost1 with lfCalAmnts(lcRPHDRTmp)
      REPLACE &lcRPHDRTmp..Dept    with Style.Dept
      REPLACE &lcRPHDRTmp..PATTERN with STYLE.Pattern
      REPLACE &lcRPHDRTmp..Desc with STYLE.Desc
  	ENDSCAN
  ELSE
	  *-- SQL connection error. can't open the report
    =gfModalGen('TRM00416B40011','ALERT')
    RETURN .F.
  ENDIF
  SELECT &lcRPHDRTmp
  SET RELATION TO 


*** PAT OF ADDING OPEN LINE AYM BEGIN
IF lcRPSumDet = 'D'
	SELECT &lcRPHDRTmp
	INDEX ON PO+STYLE TAG &lcRPHDRTmp OF (oAriaApplication.WorkDir+lcRPHDRTmp +'.CDX')
	lcKey=''
	llFrst=.t.
	STORE 0 TO m.nqty1,m.nqty2,m.nqty3,m.nqty4,m.nqty5,m.nqty6,m.nqty7,m.nqty8,m.lnPrnPrice 
	GO top
	SCATTER MEMVAR memo

	SCAN
	IF lcKey<>PO+STYLE 
	  lcKey=PO+STYLE
	  IF  !llFrst  
	    IF  m.nqty1+m.nqty2+m.nqty3+m.nqty4+m.nqty5+m.nqty6+m.nqty7+m.nqty8<>0
			  lnRec=RECNO()
			  APPEND BLANK 
			  GATHER MEMVAR MEMO 
			  FOR lnI=1 TO 8
			    z=ALLTRIM(str(lnI,1))
			    REPLACE Qty&z WITH m.nqty&z 
			  ENDFOR 
			  REPLACE    TRANCD WITH '6'
			   GO lnRec
		  ENDIF 
		  SCATTER MEMVAR memo
		  STORE 0 TO m.nqty1,m.nqty2,m.nqty3,m.nqty4,m.nqty5,m.nqty6,m.nqty7,m.nqty8,m.lnPrnPrice
	  ENDIF 
	ENDIF 
	llFrst=.f.
	FOR lnI=1 TO 8
	  z=ALLTRIM(str(lnI,1))
	  IF TRANCD = '1'
	      m.nqty&z = m.nqty&z +Qty&z
	  ELSE
	    IF TRANCD <> '9'
	      m.nqty&z = MAX(m.nqty&z  - Qty&z,0) 
	    ENDIF
	  ENDIF
	ENDFOR 

	ENDSCAN
	SET ORDER TO 
ENDIF 
ENDIF
=lfAdjustCRSettings(lcTrxType)

IF !USED(lcRPHdrTmp)
  USE oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF" IN 0  
ENDIF

SELECT (lcRPHDRTmp)
IF RECCOUNT()=0  
	*-- There is no record to display
  =gfModalGen('TRM00052B40011','ALERT')
   USE IN &lcRPHdrTmp
   RETURN .F.
ENDIF

USE IN &lcRPHdrTmp
*-- RETURN .T.	
gfDispRe()


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
LPARAMETERS lcTrxType
DIMENSION loOgScroll.laCRTables[2]

lcActMod= oAriaApplication.ActiveModuleID
DIMENSION loOgScroll.laCRParams[17,2]
IF lcActMod='PO' 
  lcInv_Typ='0001'
ELSE 
  lcInv_Typ='0002'
ENDIF 
lcMjrHdr  = gfItemMask("HM","",lcInv_Typ)
loOgScroll.laCRTables[1] = oAriaApplication.Datadir              + "SCALE.DBF"
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcRPHdrTmp  + ".DBF"
loOgScroll.laCRParams[1,1] = 'Format'
IF lcRPFormat = 'POSTYDIN'
  IF lcRPSumDet = 'D'
	  loOgScroll.laCRParams[1,2] = 'Format A - Detailed'
  ELSE
	  loOgScroll.laCRParams[1,2] = 'Format A - Summary'
  ENDIF
ELSE
    IF lcRPSumDet = 'D'
      loOgScroll.laCRParams[1,2] = 'Format B - Detailed'
    ELSE
      loOgScroll.laCRParams[1,2] = 'Format B - Summary'
    ENDIF
ENDIF

loOgScroll.laCRParams[2,1] = 'OpTitle'
loOgScroll.laCRParams[2,2] = lcRpTitle
loOgScroll.laCRParams[3,1] = 'GroupName'
loOgScroll.laCRParams[3,2] = lcRPSortBy
loOgScroll.laCRParams[4,1] = 'Form'
loOgScroll.laCRParams[4,2] = lcRPSumDet
loOgScroll.laCRParams[5,1] = 'REPEATREPORTHEADER'
loOgScroll.laCRParams[5,2] = 1
loOgScroll.laCRParams[6,1] = 'ReportName'
IF oAriaApplication.ActiveModuleID='PO'
  loOgScroll.laCRParams[6,2] = 'Style Purchase Order Detail'
ELSE
  loOgScroll.laCRParams[6,2] = 'Material Purchase Order Detail'
ENDIF
loOgScroll.laCRParams[7,1] = 'SortBy'
DO CASE
 CASE lcRPSortBy = 'P'
   loOgScroll.laCRParams[7,2] = 'PO Number'
 CASE lcRPSortBy = 'S'   
   loOgScroll.laCRParams[7,2] = 'Style'
 CASE lcRPSortBy = 'V'   
   loOgScroll.laCRParams[7,2] = 'Vendor'
ENDCASE
loOgScroll.laCRParams[8,1] = 'PrintNotes'
loOgScroll.laCRParams[8,2] = lcRpNote
loOgScroll.laCRParams[9,1] = 'Decimals'
loOgScroll.laCRParams[9,2] = lnRpPrtdec
loOgScroll.laCRParams[10,1] = 'StyMask'
lcStyMask = gfItemMask('PM',"",001)

lnStyMask = LEN(lcStyMask)
loOgScroll.laCRParams[10,2] = lnStyMask 

loOgScroll.laCRParams[11,1] = 'Types'
loOgScroll.laCRParams[11,2] = IIF(EMPTY(lcTrxType),"ORTCSP",lcTrxType)

*! B038980,1 SMM 01/27/2005 Print qty decimals if material [START] 
loOgScroll.laCRParams[12,1] = 'MA'
loOgScroll.laCRParams[12,2] = .F.
*loOgScroll.laCRParams[12,2] = .F.
loOgScroll.laCRParams[13,1] = 'AllowCost'
loOgScroll.laCRParams[13,2] =  1 &&IIF(gfUserPriv('IC', 'ICSTYLE', 'COSTING'),1,0)
loOgScroll.laCRParams[14,1] = 'llConfig'
IF oAriaApplication.ActiveModuleID='PO' 
  loOgScroll.laCRParams[14,2] = llConfig
ELSE
  loOgScroll.laCRParams[14,2] = .F.
ENDIF
loOgScroll.laCRParams[15,1] = 'lcActMod'
loOgScroll.laCRParams[15,2] = lcActMod
loOgScroll.laCRParams[16,1] = 'llDyelot'
loOgScroll.laCRParams[16,2] = llDyelot
loOgScroll.laCRParams[17,1] = 'lcMjrHdr'
loOgScroll.laCRParams[17,2] = lcMjrHdr
*************************************************************
*! Name      : lfvFormat
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report format
*!*************************************************************
FUNCTION lfvFormat()
loOgScroll.lcOGLastForm = lcRPFormat
=lfSortDumy()


*************************************************************
*! Name      : lfMajInfGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the title and picture of style major segement 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajInfGet()
*!*************************************************************

FUNCTION lfMajGet
lcMajPic = gfItemMask("PM")
lnMajPic = LEN(lcMajPic)
lcMajPic = "@! " + lcMajPic
lcMajTtl = gfItemMask("HM")
*!*************************************************************
*! Name      : lfPOType
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/06/2004
*! Purpose   : Validate POType
*!*************************************************************

FUNCTION lfPOTYPE

LOCAL lcCurName, lnAlias

lnAlias = SELECT() 

lcBusDocu  = lcRpPOType 
IF oAriaApplication.ActiveModuleID = 'PO'
  lcStyType  = lcRpPOType 
ENDIF
IF oAriaApplication.ActiveModuleID = 'MA'
  lcStyType  = IIF (lcRpPOType = 'P','M','L')
ENDIF

lcCurName = lfCheckFilter(1, 'POSHDR.PO')    
IF !EMPTY(lcCurName)
  SELECT (lcCurName)
  ZAP 
ENDIF

SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen



* B037716,1 03/10/2005 HMA ,Check configuration if the active module is style PO [Begin].
IF oAriaApplication.ActiveModuleID='PO'&& This Filter for Fabric PO 
 llConfig=(ALLTRIM(UPPER(gfGetMemVar('M_STYCNFG'))) = 'Y')
ENDIF 
IF oAriaApplication.ActiveModuleID='PO'
  llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')
ELSE 
  llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_MATDYE'))) = 'Y')
ENDIF 
* B037716,1 03/10/2005  HMA ,Check configuration if the active module is style PO [END].
*E301272,6 Evaluate currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('POSHDR.CPRICECUR')
  =lfvPrice()
ELSE
  lcRpCurr = "O"
ENDIF  
*E301272,6 Evaluate currency filter postion. [End  ]
lnRPPrice = ASCAN(laOGObjType,'LCRPPRICE')
IF lnRPPrice > 0
  lnRPPrice = ASUBSCRIPT(laOGObjType,lnRPPrice,1)
  IF !gfUserPriv('IC', 'ICSTYLE', 'COSTING')
    lcRpPrice = 'R'	
    laOGObjCnt[lnRPPrice ] = .F.
  ENDIF
ENDIF

= lfOGShowGet('LCRPPRICE')

= lfvSumDet()
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfMultCurr()
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/24/2004
*! Purpose   : Get multi currency setting
*!*************************************************************
FUNCTION lfMultCurr()
  LOCAL aSetup(1,2)
  aSetup(1,1)= 'llMulCurr'

  =gfGetMemVar(@aSetup)
RETURN aSetup(1,2)



*!*************************************************************
*! Name      : lfvSumDet
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Summary/Detail Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfOGShowGet
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSumDet()
*!*************************************************************
FUNCTION lfvSumDet
lnPrnNotePo = ASCAN(laOGObjType,'LCRPNOTE')
IF lnPrnNotePo > 0
  lnPrnNotePo = ASUBSCRIPT(laOGObjType,lnPrnNotePo,1)
  laOGObjCnt[lnPrnNotePo] = (lcRpSumDet = "D") AND (lcRpSortBy = "P")
  IF (lcRpSumDet = 'S') OR (lcRpSortBy <> "P")
    lcRPNote = 'N'
  ENDIF  
ENDIF

= lfOGShowGet('LCRPNOTE')
*-- end of lfvSumDet.

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

DO CASE
  CASE lcRpPoType = 'P'
    lcVenFld = VARREAD()
    lcVendor = EVAL(lcVenFld)
    SELECT APVENDOR
    SET ORDER TO TAG VenCode 
    IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
      =gfApVnBrow(@lcVendor)
    ENDIF
    &lcVenFld = lcVendor 

  CASE lcRpPoType = 'N'
    = lfvWareHo()
ENDCASE    

*!*************************************************************
*! Name      : lfvWareHo
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Validate warehouse
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWareHo()
*!*************************************************************

FUNCTION lfvWareHo

lcWareHo = VARREAD()

lcTag = ORDER('WAREHOUS')

SET ORDER TO WAREHOUS IN WAREHOUS

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBrowWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF

SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************
*! Name      : lfvPO
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate purchase order
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************

FUNCTION lfvPO
lcPONo = _screen.ActiveForm.ActiveControl.value 
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK(lcRpPoType+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"",lcRpPoType
ENDIF
_screen.ActiveForm.ActiveControl.value = lcPONo

*:***************************************************************************

FUNCTION lfType
PARAMETERS lcType
DO CASE 
  CASE lcType = '1'
    lcReturn = 'O'
  CASE lcType = '2'
    lcReturn = 'R'
  *--E301108 (Start)
  *CASE lcType = '3'
  CASE lcType = lcShpTrnCd
  *--E301108 (End)
    lcReturn = 'S'
  CASE lcType = '4'
    lcReturn = 'D'
  CASE lcType = '5'
    lcReturn = 'C'
ENDCASE  
RETURN lcReturn

*!*************************************************************
*! Name      : lfPrnPrice
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : print price
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : lfPrnPrice()
*!*************************************************************

FUNCTION lfPrnPrice
PRIVATE lnPrice,ldStart,ldEnd,lnDscPrcnt
IF lcRpPoType = 'N'
  IF EMPTY(Style.cDiscCode)
    lnPrice = Style.nSugRetPri
  ELSE
    STORE {} TO ldStart,ldEnd
    STORE 0 TO lnDscPrcnt
    DIMENSION laDiscRng[3,2]
    laDiscRng[1,1] = "START"
    laDiscRng[1,2] = "ldStart"
    laDiscRng[2,1] = "DENDATE"
    laDiscRng[2,2] = "ldEnd"
    laDiscRng[3,1] = "DISCPCNT"
    laDiscRng[3,2] = "lnDscPrcnt"
    *-- Fill the related GL information from the codes file.
    =gfRltFld(Style.cDiscCode, @laDiscRng, "CDISCCODE")
    IF BETWEEN(PoSHdr.Entered,ldStart,ldEnd)
      lnPrice = Style.nSugRetPri - (Style.nSugRetPri*lnDscPrcnt/100)
    ELSE
      lnPrice = Style.nSugRetPri
    ENDIF
  ENDIF
ELSE
  IF oAriaApplication.ActiveModuleID = 'PO'
    lnPrice = Style.nSugRetPri
  ELSE
    SELECT ITEM
    LOCATE FOR STYLE = &lcRPHDRTmp..STYLE
    lnPrice = ITEM.nSugRetPri
    SELECT &lcRPHDRTmp
  ENDIF
ENDIF
RETURN lnPrice

*E301272,6 Enhancement Functions Begin.
*E301272,6 *****************************************************
*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1999
*! Purpose   : Create Temporary cursor has open quantities
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatCur()
*!*************************************************************
*Fix Progarm Bug
FUNCTION lfCreatCur
CREATE CURSOR (lcOpenLine) (PO C(6) , STYLE C(19), cLineNo C(6)         ,;
   OPN1 N (7) , OPN2 N (7) , OPN3 N (7), OPN4 N (7), OPN5 N (7)     ,;
   OPN6 N (7), OPN7 N (7), OPN8 N (7), TOTQTY N(9),PRICE N(9,4) , AMOUNT N(11,4))

SELECT (lcOpenLine)
ZAP
INDEX ON PO+STYLE+cLineNo TAG &lcOpenLine OF (gcWorkDir+lcOpenLine+'.CDX')
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfChkAddLn
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1999
*! Purpose   : Add new line to temporary open quantites cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChkAddLn()
*!*************************************************************
*Fix Progarm Bug
FUNCTION lfChkAddLn

*B802637,1 RAMY Add these lines to sotre the current alias and select the 
*B802637,1 RAMY work file in case of empty PO#  [start]
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (WorkFile)
*B802637,1 RAMY Add these lines to sotre the current alias and select the [end]

IF !SEEK(PO+STYLE+STR(lineNo,6),lcOpenLine)
  m.cLineNo = STR(lineNo,6)
  m.Po      = PO
  m.Style   = STYLE
  INSERT INTO (lcOpenLine) FROM MEMVAR
ENDIF

*B802637,1 RAMY Add these lines to retrive the old alias if changed [start]

SELECT (lnAlias)

*B802637,1 RAMY Add these lines to retrive the old alias if changed [end]
*-- end of lfChkAddLn.


*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!E301272,6
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Example   : =lfCurrDesc()
*!*************************************************************
*!E301272,6
FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc
lcCurDesc = ''
lcCurrVal  = ALLTRIM(cPriceCur)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF  
RETURN PADR(ALLTRIM(lcCurDesc),18)
*-- end of lfCurrDesc.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*E301272,6
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E301272,6
FUNCTION lfFillVars
*B802637,1 RAMY Change this line to add new element that holds the  DYELOTS usage to the array
*B802637,1 RAMY of the setups [START]

*DIMENSION laCost [5,1] , laSetUps[6,2]
DIMENSION laCost [5,1] , laSetUps[7,2]
*B802637,1 RAMY [END]
laCost = SPACE(9)

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
laSetUps[1,1]   = 'M_CITYPE1'
laSetUps[2,1]   = 'M_CITYPE2'
laSetUps[3,1]   = 'M_CITYPE3'
laSetUps[4,1]   = 'M_CITYPE4'
laSetUps[5,1]   = 'M_CITYPE5'

laSetUps[6,1]  = 'llMulCurr'

*B802637,1 RAMY Add this line to get the dyelot usage yes\no [start]
laSetUps[7,1]  = 'M_DYELOT'
*B802637,1 RAMY [end]

= gfGetMemVar(@laSetups)

laCost[1,1]  = ALLTRIM(laSetUps[1,2])
laCost[2,1]  = ALLTRIM(laSetUps[2,2])
laCost[3,1]  = ALLTRIM(laSetUps[3,2])
laCost[4,1]  = ALLTRIM(laSetUps[4,2])
laCost[5,1]  = ALLTRIM(laSetUps[5,2])

llMultCurr = laSetUps[6,2]

*B802637,1 RAMY Variable that says if we use dyelot or not [start]
lcUseDye = laSetUps[7,2]
*B802637,1 RAMY [end]
IF llMultCurr

  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

*B602590,1 Adjust currency symbol [Begin]
*-- if multi currency evaluate currency arrays [Begin]
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]

  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*!*    SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
*!*    DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

*!*    FOR lnI = 1 TO ALEN(laCurrVal,1)
*!*      = SEEK(ALLTRIM(laCurrVal[lnI,1]))
*!*      laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
*!*      laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
*!*    ENDFOR
  *-- Fill Currency arrays [End  ]

*-- end of lfFillVars.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*E301272,6
FUNCTION lfClearRep

IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfCalAmnts
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/30/1999
*! Purpose   : Calculte amounts in base currency to be printed.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCalAmnts()
*!*************************************************************
*E301272,6
FUNCTION lfCalAmnts
PARAMETERS lcName
PRIVATE lnRetrnVal

lnRetrnVal = 0
*-- If Print in forign currency.
IF lcRpCurr = "F"
  lnRetrnVal = EVALUATE(lcName+".nFCost1")
ELSE
  *-- If Print in original base currency.
  IF lcRpCurr = "O"
    lnRetrnVal = IIF(lcRpPrice='R',lfPrnPrice(),EVALUATE(lcName+".niCost1"))
  ELSE  && print By Date or calculated now.
    SELECT SYCCURR
	  IF (cCurrCode = gcBaseCurr) OR (EVALUATE(lcName+".niCost1") = 0)
		  lnRetrnVal = EVALUATE(lcName+".niCost1")
	  ELSE
	  	lnRetrnVal = gfAmntDisp(EVALUATE(lcName+".nfCost1"),lcRpCurr,ldRpExDate,lcRpTmpNam)
	  ENDIF  
  ENDIF
ENDIF
*-- End If Print in forign currency.
RETURN lnRetrnVal
*--end of lfCalAmnts.

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/15/98
*! Purpose   : Enable / Disable Currency display if Retail price.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfChCurSm()
*!*************************************************************
*!E301272,6
FUNCTION lfvPrice
lnPricePos = ASCAN(laOGObjType,'LNREPCURR')
IF lnPricePos # 0
  lnPricePos = ASUBSCRIPT(laOGObjType,lnPricePos,1)
  laOGObjCnt[lnPricePos] = (lcRpPrice = "C")
  *-- if Costing price.
  IF lcRpPrice = "C"
    lcRpCurr = "F"
  ELSE  && else if Retail price.
    lcRpCurr = "O"
  ENDIF
  = lfOGShowGet('LNREPCURR')
ENDIF  
*-- end of lfvPrice.

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
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfSRVPo
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POSHDR
    SET ORDER TO TAG POSHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POSHDR

  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO 
ENDCASE
*-- end of lfSRVPo


FUNCTION lfvCurr


*!*************************************************************
*! Name      : lfSortDumy
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Validate sortby
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************

FUNCTION lfSortDumy
PARAMETERS lcPar
DIMENSION laSortItem[3,1], laSortVal[3,1]
laSortItem[1]= 'Po Number'  
laSortItem[2]= 'Style' 
laSortItem[3]= 'Vendor' 
laSortVal[1] = 'P'
laSortVal[2] = 'S'
laSortVal[3] = 'V'
 =lfOGShowGet('lcRPSortBy')



