*:***************************************************************************
*: Program file  : POSHPST
*: Program desc. : P/O Shipment Status (PO)
*: System        : Aria Advantage Series Aria4XP.
*: Module        : Style Purchase Orders and Materials
*: Developer     : Mariam Mazhar (MMT)
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO POSHPST
*:***************************************************************************
*:Modifictions: issue no. 125130 fixing the problem of the format of the vessel field in layout
*!B125582,[MMT],12/12/04 ADD new fields to the table print from
*!B125624,mariam,12/20/04 fixing the bug of not printing the total qty true
*!B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037]
*!B610446,1 MMT 07/24/2013 Shipment Status report does not print cancelled lines[T20130708.0017]
*!B611610,1 MMT 07/03/2018 Shipment Status report displays incorrect Style size in the detail format[T20180626.0005]
*:***************************************************************************
*--fill the parameter array for crystal
* N000682 ,1 Thabet Handle globalization issues [Start]
#INCLUDE  r:\aria4xp\reports\poshpst.h
* N000682 ,1 Thabet Handle globalization issues [END]
=lfvChoose()
IF lcRpChos <> 'A'
 =lfvChooseDetFrom()
ENDIF
DIMENSION loOGScroll.laCRParams[6,2]
loOGScroll.laCRParams[1,1] = 'Layout'
IF lcRpChos = 'B' && if details report
  loogScroll.cCROrientation = 'L'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *loOGScroll.laCRParams[1,2] = 'Detail'
  loOGScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAIL,oAriaApplication.GetHeaderText("LANG_DETAIL",AHEADERFILE))
  * N000682 ,1 Thabet Handle globalization issues [END]
ELSE &&summary report
  loogScroll.cCROrientation = 'L'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *loOGScroll.laCRParams[1,2] = 'Summary'
  loOGScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMARY,oAriaApplication.GetHeaderText("LANG_SUMMARY",AHEADERFILE))
  * N000682 ,1 Thabet Handle globalization issues [END]
ENDIF
*--Optional Title
loOGScroll.laCRParams[2,1] = 'OpTitle'
loOGScroll.laCRParams[2,2] = lcRpOpTtl
*--Report Name
loOGScroll.laCRParams[4,1] = 'ReportName'

* N000682 ,1 Thabet Handle globalization issues [Start]
*loOGScroll.laCRParams[4,2] = 'Shipment Status Report'
loOGScroll.laCRParams[4,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SHIPMENT_STATUS_REPORT,oAriaApplication.GetHeaderText("LANG_SHIPMENT_STATUS_REPORT",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]

*--Group By
loOGScroll.laCRParams[5,1] = 'Group'
loOGScroll.laCRParams[5,2] = lcRpSortBy
*--use configuration or not
loOGScroll.laCRParams[6,1] = 'useconf'
IF (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y') AND (ALLTRIM(gfGetMemVar("M_STYCNFG")) = "Y")
  loOGScroll.laCRParams[6,2]= 1
ELSE
  loOGScroll.laCRParams[6,2]= 0
ENDIF
*--Sort By
loOGScroll.laCRParams[3,1] = 'sortby'

DO CASE
  *-- Sort by Shipment
  CASE lcRpSortBy = 'S'
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *loOGScroll.laCRParams[3,2] = 'Ship No.'
    loOGScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SHIP_NO,oAriaApplication.GetHeaderText("LANG_SHIP_NO",AHEADERFILE))
    * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Sort by Status
  CASE lcRpSortBy = 'T'
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *loOGScroll.laCRParams[3,2] = 'Status'
	loOGScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATUS,oAriaApplication.GetHeaderText("LANG_STATUS",AHEADERFILE))
    * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Sort by ETA
  CASE lcRpSortBy = 'E'
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *loOGScroll.laCRParams[3,2] = 'E.T.A'
	loOGScroll.laCRParams[3,2] =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ETA,oAriaApplication.GetHeaderText("LANG_ETA",AHEADERFILE))
    * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Sort by Entered date
  CASE lcRpSortBy=  'N'

    * N000682 ,1 Thabet Handle globalization issues [Start]
    *loOGScroll.laCRParams[3,2] = 'Entered'
	loOGScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENTERED,oAriaApplication.GetHeaderText("LANG_ENTERED",AHEADERFILE))
    * N000682 ,1 Thabet Handle globalization issues [END]
  *-- Sort by location
  CASE lcRpSortBy=  'L'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
	*loOGScroll.laCRParams[3,2] = LANG_LOCATION
	loOGScroll.laCRParams[3,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LOCATION,oAriaApplication.GetHeaderText("LANG_LOCATION",AHEADERFILE))
	*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDCASE

IF llOGFltCh
*--collecting data
  lcWorkFile  =  loOgScroll.gfTempName()&& to hold the selected data
  lcRepFile   =  loOgScroll.gfTempName()&& to hold the report data(will print from)
  lcShpHdr    =  loOgScroll.gfTempName()&& temp shpmthdr
  lcStyleFile =  loOgScroll.gfTempName()&& temp File to hold the user selected styles

  llReturnValue  = .T. && Variable To Hold the return value from collecting data
  =lfCollectData()
ENDIF
*-- if the caller module is the style purchase order
IF oAriaApplication.ActiveModuleID ='PO'
  IF !llReturnValue &&if the result of collecting data is false
    *--no records to display
    =gfModalGen('TRM00052B34000','ALERT')
    RETURN .F.
  ENDIF
ENDIF

IF !USED(lcRepfile)
  USE oAriaApplication.WorkDir+lcRepFile+'.DBF'  IN 0
ENDIF
SELECT(lcRepfile)&&the file will print from it

IF RECCOUNT()=0   &&if the file is empty
*--no records to display
  =gfModalGen('TRM00052B34000','ALERT')
  RETURN .F.
ENDIF  &&endif the file is not empty

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRepFile+'.DBF'
USE IN (lcRepFile)&&close the file will display from to be opened exclusive
= gfDispRe()
*!fixing the bug of not removing DBFS after closing report
*--ERASE oAriaApplication.WorkDir+lcRepFile+'.DBF'
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/09/2004
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/09/2004
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
DECLARE laRpSource[3],laRpTarget[1]

lnStatus = lcRpStatus
* N000682 ,1 Thabet Handle globalization issues [Start]
*!*	STORE 'Open'      TO laRpSource[1]
*!*	STORE 'Completed' TO laRpSource[2]
*!*	STORE 'Canceled'  TO laRpSource[3]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE))      TO laRpSource[1]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE)) TO laRpSource[2]
STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE))  TO laRpSource[3]
* N000682 ,1 Thabet Handle globalization issues [End]
lcRpStatus = ''
IF !llFirst
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP,ItemLoc.TOTSTK,ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcCursorLoc = loOgScroll.gfTempName()
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO
	  LOCATE
	ENDIF
	llFirst = .T.
ENDIF
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/09/2004
*! Purpose   : Valid function for STSTUS Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

*= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
* N000682 ,1 Thabet Handle globalization issues [Start]
*= lfOGMover(@laRpSource,@laRpTarget,'Select Report Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTSTATUS,oAriaApplication.GetHeaderText("LANG_SELECTSTATUS",AHEADERFILE)),.T.,'')  && call mover function.
* N000682 ,1 Thabet Handle globalization issues [End]
lcRpStatus = ' '
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    * N000682 ,1 Thabet Handle globalization issues [Start]
*!*	    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
*!*	      IIF(laRpTarget[lnI] = 'Canceled','X',;
*!*	      IIF(laRpTarget[lnI] = 'Complete','C','')))
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE)),'O',;
      IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE)),'X',;
	  IIF(laRpTarget[lnI] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE)),'C','')))
    * N000682 ,1 Thabet Handle globalization issues [END]
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

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
*!*************************************************************
*! Name      : lfClrRead
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/09/2004
*! Refer to  : E301404,1
*! Purpose   : Function called to rebuld the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
FUNCTION lfClrRead
*--B128030,1 05/23/2005 , fix bug of not saving status while switching bet. forms[Start]
lcOldStatusVlaue = lcRpStatus
lloldFirst = llFirst
DIMENSION laCopyArr[1]
=ACOPY(laRpTarget,laCopyArr)
CLEARREAD()
lcRpStatus = lcOldStatusVlaue
llFirst  = lloldFirst
=ACOPY(laCopyArr,laRpTarget)
*--B128030,1 05/23/2005 , fix bug of not saving status while switching bet. forms[End]
*!*************************************************************
*! Name      : lfvChoose
*! Developer : Mariam Mazhar [MMT]
*! Date      : 09/08/2004
*! Purpose   : Function used to change the layout in case the
*!             user change filter
*!*************************************************************
FUNCTION lfvChoose
IF lcRpChos = 'A'
  loOgScroll.lcOGLastForm = "POSHPSTA"
*!*    lcRpForm = 'POSHPSTA'
*!*      =lfRepPltFr(lcRpForm)
*!*    llFrxForm   = .F.
*!*    llOGRefForm = .F.
*!*    IF !EMPTY(LCOGTMPFORM)
*!*      ERASE (gcWorkDir+LCOGTMPFORM+'.FRX')
*!*      ERASE (gcWorkDir+LCOGTMPFORM+'.FRT')
*!*    ENDIF
*!*    LCOGTMPFORM = ""
*!*    IF EMPTY(lcOGFormV)
*!*      lcOGFormV = "lcRpForm"
*!*    ENDIF
ELSE
  loOgScroll.lcOGLastForm = "POSHPSTB"

*!*    lcRpForm = 'POSHPSTB'
*!*      =lfRepPltFr(lcRpForm)
*!*  *!*    llFrxForm   = .T.
*!*    llOGRefForm = .T.
*!*    IF EMPTY(LCOGTMPFORM)
*!*      LCOGTMPFORM = gfTempName()
*!*    ENDIF
*!*    IF EMPTY(lcOGFormV)
*!*      lcOGFormV = "lcRpForm"
*!*    ENDIF
ENDIF


*!*************************************************************
*! Name      : lfShpVia
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/09/2004
*! Purpose   : print The Corresponding ShipVia
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShpVia()
*!*************************************************************
FUNCTION lfShpVia
PARAMETERS lcShipVia

IF SEEK('N' + 'SHIPVIA   ' + lcShipVia , 'CODES')
  RETURN (SUBSTR(CODES.cDiscRep,1,3))
ELSE
  RETURN (SPACE(03))
ENDIF
*!***************************************************************************
*! Name      : lfCriteria
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Prepare selected criteria to be printed on the first page of FRX.
*!*****************************************************************************
FUNCTION lfCriteria

lcSeaVal   = lfRtFltVal('STYLE.SEASON')
lcSeason   = 'Season         [' +lcSeaVal+' ]'
lcDivVal   = lfRtFltVal('STYLE.CDIVISION')
lcDivision = 'Division        [' +lcDivVal+' ]'
lcShipVal  = lfRtFltVal('SHPMTHDR.SHIPNO')
llShipMent = !EMPTY(lcShipMent)
lcShipMent = 'Shipment#    [ From : ' +lcShipVal+' ]'
lcStatVal  = lfRtFltVal('SHPMTHDR.STATUS')
lcStatus   = 'Status          [ ' +lcStatVal+' ]'
lcStatus   = STRTRAN(lcstatus,'O','Open')
lcStatus   = STRTRAN(lcstatus,'C','Complete')
lcStatus   = STRTRAN(lcstatus,'X','Canceled')
lcEntered  = 'Ship Date     [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ENTERED'),1),6]),'|',' TO:   ')+' ]'
lcEtA      = 'E.T.A Date   [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ETA'),1),6]),'|',' TO:   ')+' ]'
lcStyle    = 'Style            [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'STYLE.CSTYMAJOR'),1),6]),'|',' TO:   ')+' ]'
lcFabric   = 'Fabric          [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'STYLE.FABRIC'),1),6]),'|',' TO:   ')+' ]'
llSeason   = !EMPTY(lcSeaVal)
llDivision = !EMPTY(lcDivVal)
llStatus   = !EMPTY(lcStatVal)
llEntered  = ATC('ENTER',lcRpExp) >0
llEta      = ATC('ETA',lcRpExp) >0
llStyle    = ATC('CSTYMAJOR',lcRpExp) >0
llFabric   = ATC('STYLE.FABRIC',lcRpExp) >0
RETURN ''


*!***************************************************************************
*! Name      : lfRtFltVal
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Return filter options value.
*!*****************************************************************************
FUNCTION lfRtFltVal
PARAMETERS lcStrVal
PRIVATE lnPos

lnPos = ASCAN(laOgFXFlt,lcStrVal)
IF lnPos >0
  RETURN STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , lnPos,1),6]),'|',', ')
ELSE
  RETURN ''
ENDIF


FUNCTION lfGroupCh

SKIP
llGroupCh = llGrpChng OR EOF()
SKIP-1
RETURN ''

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mariam Mazhar (MMT)
*! Date      : 17/08/2004
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
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)


lnTotcomp = 0
SELECT Style_X
SET ORDER TO Style
IF SEEK(ALLTRIM(lcSty))
  SUM &lcCOMP TO lnTotcomp WHILE cStyMajor = lcSty
ENDIF

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
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mariam Mazhar (MMT)
*! Date      : 17/08/2004
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
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    LOCATE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/08/2004
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
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
*--
LOCAL lnAlias
lnAlias = SELECT()
*--

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF

*!*  IF RECCOUNT() != 0
*!*    lnFabRec = RECNO('ITEM')
*!*    SELECT ITEM
*!*    SUM &lcCOMP TO lnTotcomp WHILE STYLE=lcFab
*!*    SELECT ITEM
*!*    IF BETWEEN(lnFabRec,1,RECCOUNT())
*!*      GO lnFabRec
*!*    ENDIF
*!*  ENDIF
SELECT(lnAlias)

RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
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

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE

  CASE UPPER(lcTable) = lcShpHdr
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CBUSDOCU+CSHPTYPE+SHIPNO'
    laIndex[1,2] = 'SHPMTHDR'

ENDCASE

*!*************************************************************
*! Name      : lfCreateStatusExp
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/08/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCreateStatusExp

IF !EMPTY(lcRpStatus)
  IF LEN(lcRpStatus) = 1
    lcStatusVal ="('"+lcRpStatus+"')"
  ELSE
    IF LEN(lcRpStatus) = 2
      lcStatusVal ="('"+SUBSTR(lcRpStatus,1,1)+"','"+SUBSTR(lcRpStatus,2,1)+"')"
    ELSE
      lcStatusVal ="('"+SUBSTR(lcRpStatus,1,1)+"','"+SUBSTR(lcRpStatus,2,1)+"','"+SUBSTR(lcRpStatus,3,1)+"')"
    ENDIF
  ENDIF
ELSE
  lcStatusVal ="('O','C','X')"
  lcRpStatus = 'OCX'
ENDIF
RETURN lcStatusVal
*!*************************************************************
*! Name        : lfCollWh
*! Developer   : Mariam Mazhar(MMT)
*! Date        : 09/08/2004
*! Purpose     : Collect data for warehouse
*!*************************************************************
*! Called from :
*!*************************************************************
FUNCTION lfCollWh

SELECT (lcShpHdr)
SET ORDER TO Shpmthdr

SELECT(lcShpHdr)
COPY TO oAriaApplication.WorkDir +  lcWorkFile  + ".DBF"
COPY TO oAriaApplication.WorkDir +  lcRepFile  + ".DBF"
=gfOpenFile(oAriaApplication.WorkDir + (lcWorkFile),'','EX')
=gfOpenFile(oAriaApplication.WorkDir + (lcRepFile),'','EX')
SELECT(lcWorkFile)
INDEX ON  CBUSDOCU+CSHPTYPE+SHIPNO TAG lcWorkFile
SELECT(lcRepFile)
INDEX ON  CBUSDOCU+CSHPTYPE+SHIPNO TAG lcRepfile
ZAP
SELECT(lcShpHdr)
SCAN
  SCATTER MEMO MEMVAR
  lcRecStk =0
  lcCancellQty=0
  lcDam =0
  lcTotQty = 0
  lcSeekExp  =m.cBusDocu+m.cShpType+m.ShipNo
  lcWareCode =m.cWareCode

  SELECT(lcRepFile)
  IF !SEEK(lcSeekExp) OR ;
    (SEEK(lcSeekExp) AND cWareCode <> lcWareCode)
    LOCATE REST WHILE cBusDocu+cShpType+ShipNo = lcSeekExp FOR  cWarecode = lcWareCode
    IF !FOUND()
      SELECT(lcWorkFile)
      IF SEEK(lcSeekExp)
       SCAN WHILE cBusDocu+cShpType+ShipNo = lcSeekExp FOR  cWarecode = lcWareCode
          DO CASE
            *-- Received Qty
            CASE Trancd = '2'
              lcRecStk = lcRecStk + TotQty
              *-- Shipped Qty Qty
            CASE Trancd = '3'
              lcTotQty = lcTotQty + TotQty
            *--  Damaged Qty
            CASE Trancd = '4'
              lcDam = lcDam + TotQty
              *-- Cancelled Qty
            CASE Trancd = '5'
               lcCancellQty = lcCancellQty + TotQty
            ENDCASE
        ENDSCAN
*!*            lnCount =  lnCount +1
*!*            IF   lnCount > 1
*!*            m.totqtyhdr = 0
*!*            ENDIF
      m.RECV_STK = lcRecStk       && Received Qty
      m.RECV_DAM = lcDam          && Damaged Qty
      m.RECV_CAN = lcCancellQty   && Cancelled Qty
      IF m.Status <> 'X'
         m.TOTQTYOP = lcTotQty
    *--     m.TOTQTYhdr = lcTotQty +lcRecStk  +lcCancellQty +  lcDam
         *--+ lcRecStk      && Shipped Qty Qty
      ENDIF
*--     totqty
    *--B125624,mariam 12/20/04 fixing the bug of not printing the total qty true
    SELECT(lcRepFile)
    APPEND BLANK
    GATHER MEMO MEMVAR

      ENDIF
   ENDIF
*!*      m.RECV_STK = lcRecStk       && Received Qty
*!*      m.RECV_DAM = lcDam          && Damaged Qty
*!*      m.RECV_CAN = lcCancellQty   && Cancelled Qty
*!*      *--B125624,mariam 12/20/04 fixing the bug of not printing the total qty true
*!*      *m.TotQtyHdr = lcTotQty      && Shipped Qty Qty
*!*      IF m.Status <> 'X'
*!*         m.TOTQTYOP = lcTotQty
*!*         *--+ lcRecStk      && Shipped Qty Qty
*!*      ENDIF
*!*  *--     totqty
*!*      *--B125624,mariam 12/20/04 fixing the bug of not printing the total qty true
*!*      SELECT(lcRepFile)
*!*      APPEND BLANK
*!*      GATHER MEMO MEMVAR
  ENDIF
  SELECT(lcShpHdr)
ENDSCAN
*--B125582,[MMT],12/12/04 ADD new fields to the table print from
SELECT(lcRepFile)
ALTER TABLE &lcRepFile ADD COLUMN Etastr char(8)
ALTER TABLE &lcRepFile ADD COLUMN Enterstr char(8)
UPDATE &lcRepFile SET Etastr = DTOS(eta)
UPDATE &lcRepFile SET enterstr =DTOS(entered)
*--B125582,[MMT],12/12/04 ADD new fields to the table print from

*!fixing the bug of not removing DBFS after closing report
SELECT(lcWorkFile)
USE
ERASE oAriaApplication.WorkDir +  lcWorkFile  + ".DBF"
*!*************************************************************
*! Name        : lfsrvShp
*! Developer   : Mariam Mazhar(MMT)
*! Date        : 05/09/2004
*! Purpose     : Collect data for warehouse
*!*************************************************************
*! Called from :
*!*************************************************************
FUNCTION lfsrvShp
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    SELECT shpmthdr
    IF lcRpShpType = 'S'
      SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
    ELSE
      SET FILTER TO ShpmtHdr.cBusDocu = 'N' AND ShpmtHdr.cShpType = 'N'
    ENDIF
  CASE lcParm = 'R'  && Reset code
    SELECT shpmthdr
    SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'P'
ENDCASE
*!*************************************************************
*! Name        : lfsrvShpMA
*! Developer   : Mariam Mazhar(MMT)
*! Date        : 05/09/2004
*! Purpose     : Prepare the shpmthdr to be browsed from it
*!*************************************************************
*! Called from :
*!*************************************************************
FUNCTION lfsrvShpMA
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT shpmthdr
    SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'M'
  CASE lcParm = 'R'  && Reset code
    SELECT shpmthdr
    SET FILTER TO ShpmtHdr.cBusDocu = 'P' AND ShpmtHdr.cShpType = 'M'
ENDCASE
*!*************************************************************
*! Name        : lfCollectData
*! Developer   : Mariam Mazhar(MMT)
*! Date        : 05/09/2004
*! Purpose     : Collect data for warehouse
*!*************************************************************
*! Called from :
*!*************************************************************
FUNCTION lfCollectData
lcWhereCond =""
lcSelFlds = ""
lcTable =""
lcTmpStyle  = loOgScroll.gfTempName()

*B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
If !Used('Scale')
  =gfOpenTable('Scale','Scale')
Endif
*B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
*-- To Get the style major length
lcMajPic  = gfItemMask("PM")
lnMajPic  = LEN(lcMajPic)
IF oAriaApplication.ActiveModuleID ='PO'&&caller module is the style PO Module
 *-- 'S' Style PO Shipmnet , "L" Style Inter Location PO Shipmnet
  IF lcRpShpType = 'S'
    lcWhereCond = "ShpmtHdr.cBusDocu ='P' AND SHPMTHDR.CSHPTYPE = 'P' AND "
  ELSE
    lcWhereCond = "SHPMTHDR.cBusDocu ='N' AND SHPMTHDR.CSHPTYPE = 'N' AND "
  ENDIF
ELSE   &&caller module is the Materials Module
  lcWhereCond = "SHPMTHDR.cbusdocu ='P' AND SHPMTHDR.CSHPTYPE = 'M' AND "
ENDIF
IF lcRpChos= 'A' && summary report
  *--B125624,02/03/05,MMT,fixing Bug of not printing cancelled shipments
  *lcTable = 'SHPMTHDR(INDEX = SHPMTHDR) , POSLN(INDEX =POSLNSH)'&&files will select from them
  lcTable = 'SHPMTHDR(INDEX = SHPMTHDR) Left OUTER JOIN  POSLN(INDEX =POSLNSH) ON SHPMTHDR.cbusdocu = POSLN.cbusdocu AND SHPMTHDR.CSHPTYPE =POSLN.CSTYTYPE AND  SHPMTHDR.SHIPNO=POSLN.SHIPNO'&&files will select from them
  lcWhereCond = lcWhereCond +"SHPMTHDR.STATUS IN"+lfCreateStatusExp()&&the select condition
  * lcWhereCond =lcWhereCond +" AND  POSLN.TRANCD in ('2','3','4','5')"
  *--B125624,02/03/05,MMT,fixing Bug of not printing cancelled shipments
  lcSelFlds ="SHPMTHDR.SHIPNO,SHPMTHDR.cbusdocu,SHPMTHDR.cshptype,SHPMTHDR.Status,SHPMTHDR.entered," &&fields will be selected
  lcSelFlds =lcSelFlds + "SHPMTHDR.ETA,SHPMTHDR.CVESSEL,SHPMTHDR.airwayb,SHPMTHDR.cartons,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.TOTQTYHDR,SHPMTHDR.totqtyhdr as totqtyop,SHPMTHDR.RECV_STK,SHPMTHDR.RECV_DAM,SHPMTHDR.RECV_CAN,"
  lcSelFlds =lcSelFlds + "PosLn.cinvtype,PosLn.po,PosLn.style,PosLn.trancd,PosLn.[lineno],PosLn.cwarecode,PosLn.totqty"
  lcCursor =lcShpHdr&&the returned cursor name
  IF !EMPTY(loOGscroll.lcRpSqlExp)
    lcWhereCond =lcWhereCond +" AND "+ loOGscroll.lcrpSQLexp
  ENDIF
  IF lfOpenSql(lcSelFlds,lcTable,lcCursor,lcWhereCond )
    SELECT &lcCursor
    SET RELATION TO
    LOCATE
    =lfCollWh()&& function to calculate the received stock,amaged and cancelled quantities
    RETURN
  ENDIF
ELSE && if datail report
  IF oAriaApplication.ActiveModuleID ='PO'
    lcTable = 'SHPMTHDR(INDEX = SHPMTHDR) , POSLN(INDEX =POSLNSH),POSHDR(INDEX = POSHDR)'&&files will select from them
  ELSE
    lcTable = 'SHPMTHDR(INDEX = SHPMTHDR) , POSLN(INDEX =POSLNSH),POSHDR(INDEX = POSHDR),ITEM'&&files will select from them
  ENDIF

  lcWhereCond = lcWhereCond +"SHPMTHDR.STATUS IN"+lfCreateStatusExp()&&the select condition
  lcWhereCond =lcWhereCond +" AND  SHPMTHDR.cbusdocu = POSLN.cbusdocu AND"
  lcWhereCond =lcWhereCond +"   SHPMTHDR.CSHPTYPE =POSLN.CSTYTYPE AND  "
  lcWhereCond =lcWhereCond +" SHPMTHDR.SHIPNO=POSLN.SHIPNO AND "
  *B610446,1 MMT 07/24/2013 Shipment Status report does not print cancelled lines[T20130708.0017][Start]
  *lcWhereCond =lcWhereCond +" Posln.trancd in ('2','3') AND "
  lcWhereCond =lcWhereCond +" Posln.trancd in ('2','3','5') AND "
  *B610446,1 MMT 07/24/2013 Shipment Status report does not print cancelled lines[T20130708.0017][End]
  lcWhereCond =lcWhereCond +" POSHDR.CSTYTYPE =POSLN.CSTYTYPE AND "
  lcWhereCond =lcWhereCond +" POSHDR.CBUSDOCU =POSLN.CBUSDOCU AND  "
  lcWhereCond =lcWhereCond +" POSHDR.PO =POSLN.PO"

  lcSelFlds ="SHPMTHDR.SHIPNO,SHPMTHDR.cbusdocu,SHPMTHDR.cshptype,"
  lcSelFlds =lcSelFlds + "  SHPMTHDR.Status,SHPMTHDR.entered," &&fields will be selected
  lcSelFlds =lcSelFlds + "SHPMTHDR.ETA,SHPMTHDR.CVESSEL,SHPMTHDR.airwayb,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.cartons,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.TOTQTYHDR,SHPMTHDR.RECV_STK,SHPMTHDR.RECV_DAM,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.RECV_CAN,SHPMTHDR.dCustom,SHPMTHDR.dtrdate,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.dwarehous,SHPMTHDR.shipvia,SHPMTHDR.reference AS cship_adv,"
  lcSelFlds =lcSelFlds + "SHPMTHDR.reference AS CdivDisc,SHPMTHDR.reference AS SeasDisc,"
  lcSelFlds =lcSelFlds + "PosLn.cinvtype,PosLn.po,"
  lcSelFlds =lcSelFlds + "PosLn.style,posln.dyelot,PosLn.trancd,PosLn.[lineno],PosLn.cwarecode,"
  lcSelFlds =lcSelFlds + "PosLn.totqty,"
  lcSelFlds =lcSelFlds + "PosLn.qty1,PosLn.qty2,PosLn.qty3,PosLn.qty4,PosLn.qty5,"
  lcSelFlds =lcSelFlds + "PosLn.qty6,PosLn.qty7,PosLn.qty8,POSHDR.CDIVIsION"

  *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
  lcSelFlds =lcSelFlds + ",'     ' as size1,'     ' as size2,'     ' as size3,'     ' as size4,'     ' as size5,'     ' as size6,'     ' as size7,'     ' as size8"
  *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
  *!B611610,1 MMT 07/03/2018 Shipment Status report displays incorrect Style size in the detail format[T20180626.0005][Start]
  lcSelFlds =lcSelFlds + ",POSLN.SCALE"
  *!B611610,1 MMT 07/03/2018 Shipment Status report displays incorrect Style size in the detail format[T20180626.0005][End]
  lcCursor =lcShpHdr  &&the returned cursor name

  IF oAriaApplication.ActiveModuleID ='MA' &&IF called from MA module
    *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
    *lcSelFlds ="DISTINCT  "+lcSelFlds + ",item.season "
    lcSelFlds ="DISTINCT  "+lcSelFlds + ",item.season,item.Scale "
    *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
    lcWhereCond =lcWhereCond +" AND  item.cinvtype ='0002' AND substring(posln.style,1,"+ALLTRIM(STR(lnMajPic))+") = item.cstymajor"
  ELSE
    lcSelFlds =lcSelFlds + ",PosLn.VENDOR AS SEASON "
  ENDIF

  IF !EMPTY(loOGscroll.lcRpSqlExp)
    lcWhereCond =lcWhereCond +" AND "+ loOGscroll.lcrpSQLexp
  ENDIF

*!*    IF !EMPTY(loOGscroll.lcRpFoxExp)
*!*      lcStyleCond = ""
*!*      llFabric =.F.
*!*      llStyle = .F.
*!*      lcStyleTables = "Style"
*!*      lcSelection = loogscroll.lcRpFoxExp
*!*      ************************
*!*
*!*      lnPosStyle = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
*!*      IF lnPosStyle > 0
*!*        lnPosStyle = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyle,1)
*!*        lcCursorStyle= loOgScroll.laOgFxFlt[lnPosStyle,6]
*!*      ENDIF
*!*      IF !EMPTY(lcCursorStyle)
*!*        SELECT(lcCursorStyle)
*!*        LOCATE
*!*        IF !EOF()
*!*           llStyle = .T.
*!*           lcStyleTables =lcStyleTables+" INNER JOIN " + lcCursorStyle + " ON STYLE.CSTYMAJOR = "+lcCursorStyle+".CSTYMAJOR"
*!*        ENDIF
*!*      ENDIF
*!*
*!*      lnPosFab = ASCAN(loOgScroll.laOgFXFlt,"STYLE.FABRIC")
*!*      IF lnPosFab >0
*!*        lnPosFab = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosFab,1)
*!*        lcCursorFabric = loOgScroll.laOgFxFlt[lnPosFab,6]
*!*      ENDIF
*!*      IF !EMPTY(lcCursorFabric)
*!*        SELECT(lcCursorFabric)
*!*        LOCATE
*!*        IF !EOF()
*!*          llFabric =.T.
*!*          lcStyleTables =lcStyleTables+ " INNER JOIN "+lcCursorFabric+" ON STYLE.FABRIC = "+lcCursorFabric+".STYLE"
*!*        ENDIF
*!*      ENDIF

*!*  *!*      lnPosDivision = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
*!*  *!*      lcDivCursor = loOgScroll.gfTempName()
*!*  *!*      DIMENSION laTempacstru[1,4]
*!*  *!*      laTempacstru[1,1]='CDIVISION'
*!*  *!*      laTempacstru[1,2]='C'
*!*  *!*      laTempacstru[1,3]= 6
*!*  *!*      laTempacstru[1,4]= 0
*!*  *!*      gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
*!*  *!*      IF  lnPosDivision > 0
*!*  *!*        lnPosDivision = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDivision,1)
*!*  *!*        lcDivisions= loOgScroll.laOgFxFlt[lnPosDivision,6]
*!*  *!*        IF !EMPTY(lcDivisions)
*!*  *!*          lnStart=1
*!*  *!*          lnEnd=AT('|',lcDivisions)
*!*  *!*          DO WHILE lnEnd <> 0
*!*  *!*            SELECT(lcDivCursor)
*!*  *!*            APPEND BLANK
*!*  *!*            REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
*!*  *!*            lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
*!*  *!*            lnEnd=AT('|',lcDivisions)
*!*  *!*            *lnStart= lnEnd + 1
*!*  *!*          ENDDO
*!*  *!*          IF lnEnd = 0
*!*  *!*            APPEND BLANK
*!*  *!*            REPLACE CDIVISION WITH lcDivisions
*!*  *!*          ENDIF
*!*  *!*          lcStyleTables =lcStyleTables+" INNER JOIN " + lcDivCursor + " ON STYLE.CDIVISION = "+lcDivCursor+".CDIVISION"
*!*  *!*  *!*          lcDivisions = "('"+STRTRAN(lcDivisions,'|',"','")+"')"
*!*  *!*  *!*          lcDivisions =STRTRAN(lcDivisions ,' ','')
*!*  *!*  *!*          lcStyleCond = " STYLE.CDIVISION  IN "+lcDivisions
*!*  *!*        ENDIF
*!*  *!*      ENDIF
*!*  *!*
*!*  *!*
*!*  *!*      *-- 1) Check if the user selects season from season mover
*!*  *!*      *-- 2) Create SQL table to hold the selected seasons
*!*  *!*      lnPosSeason = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
*!*  *!*      IF lnPosSeason > 0
*!*  *!*        lcSeasonCursor = loOgScroll.gfTempName()
*!*  *!*        DIMENSION laTempacstru[1,4]
*!*  *!*        laTempacstru[1,1]='Season'
*!*  *!*        laTempacstru[1,2]='C'
*!*  *!*        laTempacstru[1,3]= 6
*!*  *!*        laTempacstru[1,4]= 0
*!*  *!*        gfCrtTmp(lcSeasonCursor,@laTempacstru,"SEASON",lcSeasonCursor,.T.)
*!*  *!*        lnPosSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSeason,1)
*!*  *!*        lcSeasons= loOgScroll.laOgFxFlt[lnPosSeason,6]
*!*  *!*        IF !EMPTY(lcSeasons)
*!*  *!*          lnStart=1
*!*  *!*          lnEnd=AT('|',lcSeasons)
*!*  *!*          DO WHILE lnEnd <> 0
*!*  *!*            SELECT(lcSeasonCursor)
*!*  *!*            APPEND BLANK
*!*  *!*            REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
*!*  *!*            lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
*!*  *!*            lnEnd=AT('|',lcSeasons)
*!*  *!*            *lnStart= lnEnd + 1
*!*  *!*          ENDDO
*!*  *!*          IF lnEnd = 0
*!*  *!*            APPEND BLANK
*!*  *!*            REPLACE SEASON WITH lcSeasons
*!*  *!*          ENDIF
*!*  *!*          lcStyleTables = lcStyleTables +" INNER JOIN "+ lcSeasonCursor +" ON STYLE.SEASON = "+ lcSeasonCursor+".SEASON"
*!*  *!*  *!*          lcSeasons = "('"+STRTRAN(lcSeasons,'|',"','")+"')"
*!*  *!*  *!*          lcStyleCond =lcStyleCond+IIF(EMPTY(lcStyleCond),'',' AND ')+" STYLE.SEASON  IN "+ lcSeasons
*!*  *!*        ENDIF
*!*  *!*      ENDIF
*!*      IF llStyle  OR llFabric
*!*        SELECT Style.Style AS Style FROM &lcStyleTables INTO CURSOR &lcStyleFile
*!*      ************************
*!*  *    SELECT Style.Style AS Style FROM Style WHERE &lcSelection INTO CURSOR &lcStyleFile
*!*        SELECT &lcStyleFile
*!*        LOCATE
*!*        IF !EOF()&&IF user select styles from the style file
*!*         lcCurName = lcStyleFile
*!*         IF !EMPTY(lcCurName)
*!*           SELECT &lcCurName
*!*           IF (RECCOUNT() > 0)
*!*             lcSQLStyle = loOgScroll.gfSQLTempName('','Style C(19)',lcCurName,'Style')
*!*             lcTable = 'SHPMTHDR(INDEX = SHPMTHDR) , POSHDR(INDEX = POSHDR),ITEM,POSLN(INDEX =POSLNSH) INNER JOIN '+lcSQLStyle +' ON posln.Style ='+lcSQLStyle+'.STYLE ' &&files will select from them
*!*             lcWhereCond =lcWhereCond  + " AND POSLN.STYLE ="+lcSQLStyle+".Style"
*!*             lcSelFlds ="DISTINCT  "+lcSelFlds
*!*             IF EMPTY(lcSQLStyle)
*!*               *-- SQL connection error. can't open the report
*!*               =gfModalGen('TRM00416B40011','ALERT')
*!*               RETURN .F.
*!*             ENDIF
*!*           ENDIF
*!*         ENDIF
*!*

*!*  *      lcWhereCond =lcWhereCond  + " AND POSLN.STYLE = "+lcSQLStyle +".Style"
*!*  *!*        ****************

*!*  *!*        lcSelStyle =""
*!*  *!*        llFirst = .T.
*!*  *!*        SCAN
*!*  *!*          lcSelStyle = lcSelStyle + IIF(llFirst," ",",") + "'"+ALLTRIM(style)+"'"
*!*  *!*          llFirst = .F.
*!*  *!*        ENDSCAN
*!*  *!*        lcWhereCond =lcWhereCond  + " AND POSLN.STYLE IN ( "+lcSelStyle+")"
*!*        llReturnValue  = .T.
*!*      ELSE
*!*        llReturnValue  = .F.
*!*        RETURN .F.
*!*      ENDIF
*!*     ENDIF
*!*    ENDIF

  IF lfOpenSql(lcSelFlds,lcTable,lcCursor,lcWhereCond )&&in case of the correct excusion of the select statment
    llDivison = .F.
    llSeason = .F.
    llFabric =.F.
    llStyle = .F.
    lcStyleTables = lcCursor
    SELECT &lcCursor
    SET RELATION TO
    LOCATE
    IF oAriaApplication.ActiveModuleID ='PO' &&IF called from Po module
    **********************************
      IF !EMPTY(loOGscroll.lcRpFoxExp)
     *********************************************
       lnPosStyle = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CSTYMAJOR")
     IF lnPosStyle > 0
       lnPosStyle = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStyle,1)
       lcCursorStyle= loOgScroll.laOgFxFlt[lnPosStyle,6]
     ENDIF
     IF !EMPTY(lcCursorStyle)
       SELECT(lcCursorStyle)
       LOCATE
       IF !EOF()
         llStyle = .T.
         lcStyleTables =lcStyleTables+" INNER JOIN " + lcCursorStyle + " ON SUBSTR("+lcCursor+".STYLE,1,"+ALLTRIM(STR(lnMajPic))+") = "+lcCursorStyle+".CSTYMAJOR"
       ENDIF
     ENDIF
     lnPosFab = ASCAN(loOgScroll.laOgFXFlt,"STYLE.FABRIC")
     IF lnPosFab >0
       lnPosFab = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosFab,1)
       lcCursorFabric = loOgScroll.laOgFxFlt[lnPosFab,6]
     ENDIF
     IF !EMPTY(lcCursorFabric)
       SELECT(lcCursorFabric)
       LOCATE
       IF !EOF()
         llFabric =.T.
         lcStyleTables =lcStyleTables+ " INNER JOIN Style"+;
         " ON "+lcCursor+".STYLE = STYLE.STYLE INNER JOIN "+;
         lcCursorFabric+" ON STYLE.FABRIC = "+lcCursorFabric+".CSTYMAJOR"
*         lcCursorFabric+" ON STYLE.FABRIC = "+lcCursorFabric+".STYLE"
       ENDIF
     ENDIF

        *********************************************
        lnPosDivision = ASCAN(loOgScroll.laOgFXFlt,"STYLE.CDIVISION")
        IF  lnPosDivision > 0
          lcDivCursor = loOgScroll.gfTempName()
          DIMENSION laTempacstru[1,4]
          laTempacstru[1,1]='CDIVISION'
          laTempacstru[1,2]='C'
          laTempacstru[1,3]= 6
          laTempacstru[1,4]= 0
          gfCrtTmp(lcDivCursor,@laTempacstru,"CDIVISION",lcDivCursor,.T.)
          lnPosDivision = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosDivision,1)
          lcDivisions= loOgScroll.laOgFxFlt[lnPosDivision,6]
          IF !EMPTY(lcDivisions)
            lnStart=1
            lnEnd=AT('|',lcDivisions)
            DO WHILE lnEnd <> 0
              SELECT(lcDivCursor)
              APPEND BLANK
              REPLACE CDIVISION WITH SUBSTR(lcDivisions,lnStart,lnEnd-1)
              lcDivisions = STUFF(lcDivisions ,lnStart,lnEnd,"")
              lnEnd=AT('|',lcDivisions)
            ENDDO
            IF lnEnd = 0
              SELECT(lcDivCursor)
              APPEND BLANK
              REPLACE CDIVISION WITH lcDivisions
            ENDIF
            *!*          lcStyleTables =lcStyleTables+" INNER JOIN " + lcDivCursor + " ON STYLE.CDIVISION = "+lcDivCursor+".CDIVISION"
*!*          lcDivisions = "('"+STRTRAN(lcDivisions,'|',"','")+"')"
*!*          lcDivisions =STRTRAN(lcDivisions ,' ','')
*!*          lcStyleCond = " STYLE.CDIVISION  IN "+lcDivisions
          ENDIF
          SELECT(lcDivCursor)
          LOCATE
          IF !EOF()
            lcStyleTables =lcStyleTables+" INNER JOIN " + lcDivCursor + " ON "+lcCursor+".CDIVISION = "+lcDivCursor+".CDIVISION"
            llDivison = .T.
          ENDIF
        ENDIF
    *-- 1) Check if the user selects season from season mover
    *-- 2) Create SQL table to hold the selected seasons
    lnPosSeason = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
    IF lnPosSeason > 0
      lcSeasonCursor = loOgScroll.gfTempName()
      DIMENSION laTempacstru[1,4]
      laTempacstru[1,1]='Season'
      laTempacstru[1,2]='C'
      laTempacstru[1,3]= 6
      laTempacstru[1,4]= 0
      gfCrtTmp(lcSeasonCursor,@laTempacstru,"SEASON",lcSeasonCursor,.T.)
      lnPosSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSeason,1)
      lcSeasons= loOgScroll.laOgFxFlt[lnPosSeason,6]
      IF !EMPTY(lcSeasons)
        lnStart=1
        lnEnd=AT('|',lcSeasons)
        DO WHILE lnEnd <> 0
          SELECT(lcSeasonCursor)
          APPEND BLANK
          REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
          lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
          lnEnd=AT('|',lcSeasons)
          *lnStart= lnEnd + 1
        ENDDO
        IF lnEnd = 0
          SELECT(lcSeasonCursor)
          APPEND BLANK
          REPLACE SEASON WITH lcSeasons
        ENDIF
*        lcStyleTables = lcStyleTables +" INNER JOIN "+ lcSeasonCursor +" ON "+ lcCursor+".SEASON = "+ lcSeasonCursor+".SEASON"
*!*          lcSeasons = "('"+STRTRAN(lcSeasons,'|',"','")+"')"
*!*          lcStyleCond =lcStyleCond+IIF(EMPTY(lcStyleCond),'',' AND ')+" STYLE.SEASON  IN "+ lcSeasons
      ENDIF
      SELECT(lcSeasonCursor)
      LOCATE
      IF !EOF()
        lcStyleTables = lcStyleTables +" INNER JOIN "+ lcSeasonCursor +" ON "+ lcCursor+".SEASON = "+ lcSeasonCursor+".SEASON"
        llSeason = .T.
      ENDIF
    ENDIF
  ENDIF

    ***********************************
      *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
      *SELECT Style.Style, Style.Season FROM Style,&lcCursor WHERE Style.Style = &lcCursor..Style INTO CURSOR &lcTmpStyle
      Select Style.Style, Style.SEASON,Style.Scale From Style,&lcCursor Where Style.Style = &lcCursor..Style Into Cursor &lcTmpStyle
      *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
      SELECT(lcTmpStyle)
      INDEX ON Style TAG  style
      SET ORDER TO TAG Style
      SELECT &lcCursor
      SCAN &&get some fields values
        SELECT(lcTmpStyle)
        =SEEK(&lcCursor..Style)
        SELECT &lcCursor
        REPLACE SeasDisc  WITH gfCodDes(&lcTmpStyle..season,'SEASON    '),;
                Season    WITH &lcTmpStyle..season,;
                CdivDisc  WITH gfCodDes(&lcCursor..cDivision,'CDIVISION '),;
                cShip_Adv WITH ALLTRIM(gfCodDes(&lcCursor..ShipVia,'SHIPVIA')),;
                ShipVia   WITH lfShpVia(&lcCursor..ShipVia)
       *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
        =gfSeek('S'+&lcTmpStyle..Scale,'Scale')
        Replace Size1 With Scale.SZ1,;
          Size2 With Scale.SZ2,;
          Size3 With Scale.SZ3,;
          Size4 With Scale.SZ4,;
          Size5 With Scale.SZ5,;
          Size6 With Scale.SZ6,;
          Size7 With Scale.SZ7,;
          Size8 With Scale.SZ8
       *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
      ENDSCAN
      IF llSeason OR llDivison  OR llStyle  OR llFabric
        lcFnlFile = loOgScroll.gfTempName()
        SELECT &lcCursor..* FROM &lcStyleTables INTO CURSOR &lcFnlFile
        lcCursor = lcFnlFile
      ENDIF
    ELSE &&IF called from MA module
      SELECT &lcCursor
      SCAN
        REPLACE SeasDisc  WITH gfCodDes(season,'SEASON    '),;
                CdivDisc  WITH gfCodDes(&lcCursor..cDivision,'CDIVISION '),;
                cShip_Adv WITH ALLTRIM(gfCodDes(&lcCursor..ShipVia,'SHIPVIA')),;
                ShipVia   WITH lfShpVia(&lcCursor..ShipVia)
        *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][Start]
        =gfSeek('S'+&lcCursor..Scale,'Scale')
        Replace Size1 With Scale.SZ1,;
          Size2 With Scale.SZ2,;
          Size3 With Scale.SZ3,;
          Size4 With Scale.SZ4,;
          Size5 With Scale.SZ5,;
          Size6 With Scale.SZ6,;
          Size7 With Scale.SZ7,;
          Size8 With Scale.SZ8
        *B610079,1 MMT 09/11/2012 Shipment Status report does not print sizes description[T20120910.0037][END]
      ENDSCAN
    ENDIF
    SELECT(lcCursor)
    *--copying the data file to the file will print from
    COPY TO oAriaApplication.WorkDir +  lcRepFile  + ".DBF"
    =gfOpenFile(oAriaApplication.WorkDir + (lcRepFile),'','EX')
    *--B125582,MMT 12/12/04 Add new fields to the file print from
    SELECT(lcRepFile)
  ALTER TABLE &lcRepFile ADD COLUMN Etastr char(8)
  ALTER TABLE &lcRepFile ADD COLUMN Enterstr char(8)
  UPDATE &lcRepFile SET Etastr = DTOS(eta)
  UPDATE &lcRepFile SET enterstr =DTOS(entered)
    *--B125582,MMT 12/12/04 Add new fields to the file print from
    RETURN
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfStatus
*! Developer : Mariam Mazhar (MAB)
*! Date      : 08/31/2004
*! Purpose   : Translate the Status field from symbol to be a complete word
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStatus()
*!*************************************************************
FUNCTION lfStatus
PARAMETERS lcStatSym
lcStatus = IIF(lcStatSym ='O','Open',IIF(lcStatSym ='X','Cancelled','Completed'))
RETURN lcStatus
*!*************************************************************
*! Name      : lfStatus
*! Developer : Mariam Mazhar (MAB)
*! Date      : 08/31/2004
*! Purpose   : Translate the Status field from symbol to be a complete word
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStatus()
*!*************************************************************
FUNCTION lfvChooseDetFrom
IF lcRpFormDt= 'A'
  loOgScroll.lcOGLastForm = "POSHPSTB"
ELSE
  loOgScroll.lcOGLastForm = "POSHPSTC"
ENDIF
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!***************************************************************************
*! Modification:
*!***************************************************************************
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











