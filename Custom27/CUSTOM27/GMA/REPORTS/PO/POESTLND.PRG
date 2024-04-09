****************************************************************************
*: Program file      : POESTLND.PRG   (C#102413)
*: Program desc.     : ESTIMATED VS LANDED COSTS REPORT FOR GMA
*: System            : Aria Apparel System (A27)
*: Module            : Style Purchase Order (PO)
*: Developer         : (AMH) AHMED MAHER
*: Date              : 09/17/2001
*:**************************************************************************
*:Modifications :
*:****************************************************************************************
*:B605020,1 AMH 10/14/2001 Print the estimated unit cost from the BOM file.
*:B605073,1 AMH 10/30/2001 Fix the bug of wronge est. cost in the shipment summery report.
*:C200593,1 SSE 08/20/2003 Custom changing 'Var at 30%' for Freight to be 'Var at 15%'
*:C200593,1                All modifications is in FRX, report program for documentation only.
*:****************************************************************************************

lnMajorLen = LEN(gfItemMask("PM"))
lcTmpFile  = gfTempName()
lcTmpFile1 = gfTempName()
lcTmpFile2 = gfTempName()
lcTmpFile3 = gfTempName()
lcTmpFile4 = gfTempName()
CREATE CURSOR (lcTmpFile);
   (CSHIPNO C(6), CWARECODE C(6), DRECEIVED D, CFOB C(18), CVENDOR C(8), CDESC C(81), CDEPT C(36),;
    CSTYGROUP C(37), CPO C(6), CSTYLE C(19), CCOLOR C(6), NTOTQTY N(7,0), NSTYCOST N(13,3), NLNDCOST N(13,3),;
    CSHIPVIA C(6), DLASTRECV D, CDATEFROM C(10), CDATETO C(10), LFIRST L, LFIRSTLND L)
INDEX ON CSHIPNO+CSTYLE+CCOLOR TAG (lcTmpFile)  OF (lcTmpFile)
INDEX ON CSHIPVIA+CSHIPNO      TAG (lcTmpFile1) OF (lcTmpFile)
INDEX ON CSTYLE+CVENDOR        TAG (lcTmpFile2) OF (lcTmpFile)
INDEX ON CSTYLE+CPO            TAG (lcTmpFile3) OF (lcTmpFile)
INDEX ON CSTYLE+CFOB           TAG (lcTmpFile4) OF (lcTmpFile)
SELECT (lcTmpFile)
SET ORDER TO (lcTmpFile)

*-- To get the selected shipment if any.
lcShipFile = ''
llUseShipn = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.SHIPNO'),1)
IF lnPosition > 0
  lcShipFile = laOGFxFlt[lnPosition,6]
  llUseShipN = IIF(!EMPTY(lcShipFile) .AND. USED(lcShipFile) .AND. RECCOUNT(lcShipFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Po if any.
lcPoHdFile = ''
llUsePosHd = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
IF lnPosition > 0
  lcPoHdFile = laOGFxFlt[lnPosition,6]
  llUsePosHd = IIF(!EMPTY(lcPoHdFile) .AND. USED(lcPoHdFile) .AND. RECCOUNT(lcPoHdFile)>0,.T.,.F.)
ENDIF

*-- To get the selected style if any.
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSLN.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

*-- To get the selected vendor if any.
lcVendFile = ''
llUseVendr = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.VENDOR'),1)
IF lnPosition > 0
  lcVendFile = laOGFxFlt[lnPosition,6]
  llUseVendr = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
ENDIF

*-- To get the selected warehouse if any.
lcWareFile = ''
llUseWareH = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSLN.CWARECODE'),1)
IF lnPosition > 0
  lcWareFile = laOGFxFlt[lnPosition,6]
  llUseWareH = IIF(!EMPTY(lcWareFile) .AND. USED(lcWareFile) .AND. RECCOUNT(lcWareFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Ship Via if any.
lcShipVia = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.SHIPVIA'),1)
IF lnPosition > 0
  lcShipVia = laOGFxFlt[lnPosition,6]
ENDIF

*-- To get the selected Receiving Date if any.
lcFromDate = '  /  /    '

*B605020,1 AMH Add variable to save the date fillter [Start]
lcDateFltr = ".T."
lcToDate   = '  /  /    '
*B605020,1 AMH [End]

lcRecvDate = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSLN.DATE'),1)
IF lnPosition > 0
  lcRecvDate = laOGFxFlt[lnPosition,6]
  lnRecvDate = LEN(lcRecvDate)
  IF lnRecvDate > 0
    lnLen = (lnRecvDate - 1) / 2
    lcFromDate = LEFT(lcRecvDate,lnLen)
    lcToDate   = RIGHT(lcRecvDate,lnLen)
    
    *B605020,1 AMH Comment the next line [Start]
    *lcToDate   = IIF(EMPTY(LEFT(lcToDate,2)),'99/99/9999',lcToDate)
    *B605020,1 AMH [End]
    
  ELSE

    *B605020,1 AMH Comment the next lines [Start]
    *lcFromDate = '  /  /    '
    *lcToDate   = '99/99/9999'
    *B605020,1 AMH [End]
    
  ENDIF
ENDIF

*B605020,1 AMH Save the date filter [Start]
IF !EMPTY(CTOD(lcToDate))
  lcDateFltr = "BETWEEN(DATE,CTOD(lcFromDate),CTOD(lcToDate))"
ENDIF
*B605020,1 AMH [End]

*-- To get the selected Style Group if any.
lcStyGroup = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcStyGroup = laOGFxFlt[lnPosition,6]
ENDIF

*C102413,4 AMH Change the century setting [Start]
lcSetting = SET('CENTURY')
SET CENTURY ON


*C102413,4 AMH [End]
DO CASE
  CASE lcRpType = 'D'  && Shipment Detail report.
    DO CASE
      CASE llUseShipn
        =lfCases(1)
      CASE !llUseShipn .AND. llUsePosHd
        =lfCases(2)
      CASE !llUseShipn .AND. !llUsePosHd .AND. llUseStyle
        =lfCases(3)
      CASE !llUseShipn .AND. !llUsePosHd .AND. !llUseStyle .AND. llUseVendr
        =lfCases(4)
      OTHERWISE
        =lfCases(5)
    ENDCASE
  CASE lcRpType = 'H'  && Shipment Summary report.
    DO CASE
      CASE llUsePosHd
        =lfCases(6)
      OTHERWISE
        =lfCases(7)
    ENDCASE
    SELECT (lcTmpFile)
    SET ORDER TO (lcTmpFile1)
  CASE lcRpType = 'S'  && Style Detail report.
    DO CASE
      CASE llUsePosHd
        =lfCases(2)
      CASE !llUsePosHd .AND. llUseStyle
        =lfCases(3)
      CASE !llUsePosHd .AND. !llUseStyle .AND. llUseVendr
        =lfCases(4)
      OTHERWISE
        =lfCases(5)
    ENDCASE
    SELECT (lcTmpFile)
    DO CASE
      CASE lcRpSort = 'V'
        SET ORDER TO (lcTmpFile2)
      CASE lcRpSort = 'P'
        SET ORDER TO (lcTmpFile3)
      CASE lcRpSort = 'F'
        SET ORDER TO (lcTmpFile4)
    ENDCASE
ENDCASE


*C102413,4 AMH Restore the century setting [Start]
SET CENTURY &lcSetting.
*C102413,4 AMH [End]

SELECT (lcTmpFile)
GO BOTTOM
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
LOCATE

*--- Start printing report
DO gfDispRe WITH EVAL('lcRpForm')
SET DEVICE TO SCREEN

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF
*--end of Main Programe.

*:----------------------------------------------------------------------------
*: Program file        : lfsrvSty.
*: Program description : Rise change style flag, in range browse screen..
*: Developer Name      : (AMH) Ahmed Maher
*: Tracking Job Number : C#102413
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfsrvSty()
*:----------------------------------------------------------------------------
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
ENDCASE
*-- end of lfsrvSty.

*:----------------------------------------------------------------------------
*: Program file        : lfStySum.
*: Program description : sum a specific field for the current style in style file.
*: Developer Name      : (AMH) Ahmed Maher
*: Tracking Job Number : C#102413
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfStySum()
*:----------------------------------------------------------------------------
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

*!*************************************************************
*! Name      : lfFillCost
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Fill the two arrays of cost items.
*!*************************************************************
*! Example   : = lffillCost()
*!*************************************************************
FUNCTION lfFillCost

DECLARE laCostDesc[6,1], laCostVal[6,1]
DECLARE laMainSetp[15,2]

FOR lnCount = 1 TO 5
  lcCount = STR(lnCount,1)
  laMainSetp[lnCount,1] = 'M_CISLBL'+lcCount
ENDFOR

=gfGetMemVar(@laMainSetp,gcAct_Comp)

FOR lnCount = 1 TO 5
  lcCount = STR(lnCount,1)
  laCostDesc[lnCount,1] = laMainSetp[lnCount,2]
  laCostVal[lnCount,1] = lcCount
ENDFOR
laCostDesc[6,1] = 'Total'
laCostVal[6,1]  = '6'
*-- end lfFillCost.

*!*************************************************************
*! Name      : lfFillMfgo
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Fill the two arrays of Mfgo.
*!*************************************************************
*! Example   : = lffillMfgo()
*!*************************************************************
FUNCTION lfFillMfgo

lnNo = 1
DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
laMfgoDesc[lnNo,1] = 'Total Misc.'
laMfgoCode[lnNo,1] = 'TOTALZ'
SELECT CODES
IF SEEK('DMFGCODE')
  lnNo = lnNo + 1
  DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
  laMfgoDesc[lnNo,1] = cCode_No + ' - ' + cDiscrep
  laMfgoCode[lnNo,1] = cCode_No
  IF SEEK('NMFGCODE')
    SELECT CODES
    SCAN REST WHILE cDefCode+cFld_Name+cCode_No+cDiscrep+cRltd_Nam = 'NMFGCODE';
              FOR CRLTFIELD = 'N' .AND. CCODE_NO <> laMfgoCode[2,1]
      lnNo = lnNo + 1
      DECLARE laMfgoDesc[lnNo,1], laMfgoCode[lnNo,1]
      laMfgoDesc[lnNo,1] = cCode_No + ' - ' + cDiscrep
      laMfgoCode[lnNo,1] = cCode_No
    ENDSCAN
  ENDIF
ENDIF
*-- end lfFillMfgo.

*!*************************************************************
*! Name      : lfvCost
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/17/2001
*! Purpose   : Validate lcRpCost option
*!*************************************************************
*! Example   : = lfvCost()
*!*************************************************************
FUNCTION lfvCost

CLEAR READ
*-- end lfvCost.

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MAHER
*! Date      : 09/18/2001
*! Purpose   : When function of the option grid.
*!*************************************************************
*! Called from : POESTLND.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

DECLARE laRpSource[3] , laRpTarget[1]
STORE 'Open'     TO laRpSource[1]
STORE 'Complete' TO laRpSource[2]
STORE 'Closed'   TO laRpSource[3]
*--end lfwOGWhen.

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : AHMED MAHER
*! Date      : 09/18/2001
*! Purpose   : Evaluate Status expression.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*
FUNCTION lfvOStatus

= gfMover(@laRpSource,@laRpTarget,'Select Po Status',.T.,'')  && call mover function.
lcRpStatus = ' '

*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open'        ,'O',;
                              IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
                              IIF(laRpTarget[lnI] = 'Complete','C','')))
  ENDFOR  && end Loop to make Status expression.
ENDIF

*-- if empty of status is like select all available values.
lcRpStatus = IIF(EMPTY(lcRpStatus),'OSC',ALLTRIM(lcRpStatus))
*-- End of lfvOStatus.

*!*************************************************************
*! Name      : lfCases
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/23/2001
*! Purpose   : All cases to collecting data of all reports
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : lnCase
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfCases(x)
*!*************************************************************
*
FUNCTION lfCases
PARAMETERS lnCase

DO CASE
  CASE lnCase = 1
    SELECT (lcShipFile)
    SCAN
      IF SEEK(SHIPNO,'SHPMTHDR')
        SELECT SHPMTHDR
        SCAN REST WHILE SHIPNO = EVALUATE(lcShipFile+'.SHIPNO');
                  FOR IIF(EMPTY(lcShipVia),.T.,SHIPVIA $ lcShipVia)
          IF SEEK(SHIPNO,'POSLN')
            lcWhileExp = SHPMTHDR.SHIPNO
            
            *B605020,1 AMH don't check if est. cost <> 0 [Start]
            *lcForExp   = "IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                         "IIF(llUsePosHd,SEEK(PO,lcPoHdFile),.T.) .AND. "+;
                         "IIF(llUseStyle,SEEK(SUBSTR(STYLE,1,lnMajorLen),lcStylFile),.T.) .AND. "+;
                         "BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                         "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                         "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
            lcForExp   = "IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                         "IIF(llUsePosHd,SEEK(PO,lcPoHdFile),.T.) .AND. "+;
                         "IIF(llUseStyle,SEEK(SUBSTR(STYLE,1,lnMajorLen),lcStylFile),.T.) .AND. "+;
                         lcDateFltr + " .AND. "+;
                         "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
            *B605020,1 AMH [End]
            
            =lfFound(lcWhileExp,lcForExp,.F.,.T.,.T.)
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  CASE lnCase = 2
    SELECT POSLN
    SET ORDER TO TAG POSLN
    SELECT (lcPoHdFile)
    SCAN
      IF SEEK('P'+PO,'POSLN')
        lcWhileExp = 'P'+PO
        
        *B605020,1 AMH don't check if est. cost <> 0 [Start]
        *lcForExp   = "!EMPTY(SHIPNO) .AND. IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                     "IIF(llUseStyle,SEEK(SUBSTR(STYLE,1,lnMajorLen),lcStylFile),.T.) .AND. "+;
                     "BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                     "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
        lcForExp   = "!EMPTY(SHIPNO) .AND. IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                     "IIF(llUseStyle,SEEK(SUBSTR(STYLE,1,lnMajorLen),lcStylFile),.T.) .AND. "+;
                     lcDateFltr + " .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
        *B605020,1 AMH [End]
        
        =lfFound(lcWhileExp,lcForExp,.T.,.T.,.T.)
      ENDIF
    ENDSCAN
    SELECT POSLN
    SET ORDER TO TAG POSLNSH
  CASE lnCase = 3
    SELECT POSLN
    SET ORDER TO TAG POSLNS
    SELECT (lcStylFile)
    SCAN
      IF SEEK(SUBSTR(EVALUATE(lcStylFile+'.CSTYMAJOR'),1,lnMajorLen),'POSLN')
        lcWhileExp = SUBSTR(EVALUATE(lcStylFile+'.CSTYMAJOR'),1,lnMajorLen)
        
        *B605020,1 AMH don't check if est. cost <> 0 [Start]
        *lcForExp   = "!EMPTY(SHIPNO) .AND. IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                     "BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                     "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
        lcForExp   = "!EMPTY(SHIPNO) .AND. IIF(llUseVendr,SEEK(VENDOR,lcVendFile),.T.) .AND. "+;
                     lcDateFltr + " .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
        *B605020,1 AMH [End]
        
        =lfFound(lcWhileExp,lcForExp,.T.,.T.,.T.)
      ENDIF
    ENDSCAN
    SELECT POSLN
    SET ORDER TO TAG POSLNSH
  CASE lnCase = 4
    SELECT POSHDR
    SET ORDER TO TAG POSHDRV
    SELECT POSLN
    SET ORDER TO TAG POSLN
    SELECT (lcVendFile)
    SCAN
      *C102413,4 AMH Fix bug of variable vendor not found [Start]
      *IF SEEK(VENDOR+'P','POSHDR')
      IF SEEK(Cvendcode+'P','POSHDR')
      *C102413,4 AMH [End]
        SELECT POSHDR
        *C102413,4 AMH Fix bug of variable vendor not found [Start]
        *SCAN REST WHILE VENDOR+CSTYTYPE+PO = EVALUATE(lcVendFile+'.VENDOR')+'P' FOR STATUS $ lcRpStatus
        SCAN REST WHILE VENDOR+CSTYTYPE+PO = EVALUATE(lcVendFile+'.Cvendcode')+'P' FOR STATUS $ lcRpStatus
        *C102413,4 AMH Fix bug of variable vendor not found [Start]
          IF SEEK(CSTYTYPE+PO,'POSLN')
            lcWhileExp = POSHDR.CSTYTYPE+POSHDR.PO
            
            *B605020,1 AMH don't check if est. cost <> 0 [Start]
            lcForExp   = "!EMPTY(SHIPNO) .AND. BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                         "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                         "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
            lcForExp   = "!EMPTY(SHIPNO) .AND. " + lcDateFltr + " .AND. "+;
                         "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
            *B605020,1 AMH [End]
            
            =lfFound(lcWhileExp,lcForExp,.T.,.F.,.T.)
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
    SELECT POSLN
    SET ORDER TO TAG POSLNSH
    SELECT POSHDR
    SET ORDER TO TAG POSHDR
  CASE lnCase = 5
    SELECT SHPMTHDR
    SCAN FOR IIF(EMPTY(lcShipVia),.T.,SHIPVIA $ lcShipVia)
      IF SEEK(SHIPNO,'POSLN')
        lcWhileExp = SHPMTHDR.SHIPNO
        
        *B605020,1 AMH don't check if est. cost <> 0 [Start]
        *lcForExp   = "BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                     "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
        lcForExp   = lcDateFltr + " .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
        *B605020,1 AMH [End]
        
        =lfFound(lcWhileExp,lcForExp,.F.,.T.,.T.)
      ENDIF
    ENDSCAN
  CASE lnCase = 6
    SELECT POSLN
    SET ORDER TO TAG POSLN
    SELECT (lcPoHdFile)
    SCAN
      IF SEEK('P'+PO,'POSLN')
        lcWhileExp = 'P'+PO
        
        *B605020,1 AMH don't check if est. cost <> 0 [Start]
        *lcForExp   = "!EMPTY(SHIPNO) .AND. BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                     "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
        lcForExp   = "!EMPTY(SHIPNO) .AND. " + lcDateFltr + " .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
        *B605020,1 AMH [End]
        
        =lfFound(lcWhileExp,lcForExp,.T.,.F.,.F.)
      ENDIF
    ENDSCAN
    SELECT POSLN
    SET ORDER TO TAG POSLNSH
  CASE lnCase = 7
    SELECT SHPMTHDR
    SCAN FOR IIF(EMPTY(lcShipVia),.T.,SHIPVIA $ lcShipVia)
      IF SEEK(SHIPNO,'POSLN')
        lcWhileExp = SHPMTHDR.SHIPNO
        
        *B605020,1 AMH don't check if est. cost <> 0 [Start]
        *lcForExp   = "BETWEEN(DTOC(DATE),lcFromDate,lcToDate) .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2' .AND. "+;
                     "IIF(lcRpCost#'6',NCOST&lcRpCost. <> 0,.T.)"
        lcForExp   = lcDateFltr + " .AND. "+;
                     "IIF(llUseWareH,SEEK(POSLN.CWARECODE,lcWareFile),.T.) .AND. TRANCD = '2'"
        *B605020,1 AMH [End]
        
        =lfFound(lcWhileExp,lcForExp,.F.,.F.,.F.)
      ENDIF
    ENDSCAN
ENDCASE
*--end of lfCases.

*!*************************************************************
*! Name      : lpAddNew
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/19/2001
*! Purpose   : Add new record to the timp table.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpAddNew
*!*************************************************************

PROCEDURE lpAddNew

=SEEK(STYLE.STYLE,'ICDEPTDT')
=SEEK(ICDEPTDT.DEPT,'ICDEPTHD')
SELECT (lcTmpFile)
IF lcRpType = 'H' .AND. SEEK(SHPMTHDR.SHIPNO)
  *--Do nothing
ELSE
  APPEND BLANK
  REPLACE LFIRST    WITH .T.,;
          LFIRSTLND WITH .T.
ENDIF
REPLACE CSHIPNO   WITH SHPMTHDR.SHIPNO,;
        CWARECODE WITH POSLN.CWARECODE,;
        CFOB      WITH POSHDR.CFOB,;
        CVENDOR   WITH POSLN.VENDOR,;
        CDESC     WITH IIF(lcRpType = 'S',STYLE.DESC+' ','') + STYLE.DESC1,;
        CDEPT     WITH ICDEPTDT.DEPT + IIF(lcRpType = 'S',' '+ICDEPTHD.CDEPTDESC,''),;
        CSTYGROUP WITH STYLE.CSTYGROUP + IIF(lcRpType = 'S',' '+gfCodDes(STYLE.CSTYGROUP,'CSTYGROUP'),''),;
        CPO       WITH POSLN.PO,;
        CSTYLE    WITH STYLE.CSTYMAJOR

*B605020,1 AMH get the est. cost from BOM file and change the last date [Start]
*REPLACE CCOLOR    WITH ALLTRIM(SUBSTR(POSLN.STYLE,lnMajorLen+2)),;
        DRECEIVED WITH POSLN.DATE,;
        CSHIPVIA  WITH SHPMTHDR.SHIPVIA,;
        DLASTRECV WITH POSLN.DATE,;
        CDATEFROM WITH lcFromDate,;
        CDATETO   WITH IIF(lcToDate='99/99/9999','  /  /    ',lcToDate)
REPLACE CCOLOR    WITH ALLTRIM(SUBSTR(POSLN.STYLE,lnMajorLen+2)),;
        DRECEIVED WITH POSLN.DATE,;
        CSHIPVIA  WITH SHPMTHDR.SHIPVIA,;
        DLASTRECV WITH POSLN.DATE,;
        CDATEFROM WITH lcFromDate,;
        CDATETO   WITH lcToDate
*REPLACE NTOTQTY   WITH IIF(lcRpType='H',NTOTQTY,0)+POSLN.TOTQTY,;
        NSTYCOST  WITH IIF(lcRpType='H',NSTYCOST,0)+lfGetCost('NCOST',lcRpCost),;
        NLNDCOST  WITH IIF(lcRpType='H',NLNDCOST,0)+lfGetCost('NLAN_CST',lcRpCost)
REPLACE NTOTQTY   WITH IIF(lcRpType='H',NTOTQTY,0)+POSLN.TOTQTY,;
        NSTYCOST  WITH IIF(lcRpType='H',NSTYCOST,0)+lfGtStyCst(lcRpCost),;
        NLNDCOST  WITH IIF(lcRpType='H',NLNDCOST,0)+lfGetCost('NLAN_CST',lcRpCost)
*B605020,1 AMH [End]
*--end of lpAddNew.

*!*************************************************************
*! Name      : lfGetCost
*! Developer : AHMED MAHER
*! Date      : 09/19/2001
*! Purpose   : Get the estimated/landed cost
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : lpAddNew
*!*************************************************************
*! Passed Parameters  : lcField = estimated/landed
*!                      lcType  = cost element type
*!*************************************************************
*! Returns            : cost value
*!*************************************************************
*! Example   : = lfGetCost()
*!*************************************************************
*
FUNCTION lfGetCost
PARAMETERS lcField, lcType

PRIVATE lnCostVal
lnCostVal = 0
IF lcType = '6'
  FOR lnI = 1 TO 5
    lcI = STR(lnI,1)
    lnCostVal = lnCostVal + EVALUATE('POSLN.'+lcField+lcI)
  ENDFOR
ELSE
  lnCostVal = EVALUATE('POSLN.'+lcField+lcType)
ENDIF
lnCostVal = lnCostVal*IIF(lcRpType='H',POSLN.TOTQTY,1)
RETURN lnCostVal
*-- End of lfGetCost.

*!*************************************************************
*! Name      : lfFound
*! Developer : AHMED MAHER
*! Date      : 09/23/2001
*! Purpose   : Scan the Posln file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : lpShipDetl
*!*************************************************************
*! Passed Parameters  : lcWhileExp = the while expration part in the scan loop
*!                      lcForExp   = the for expration part in the scan loop
*!                      llShipLoop = loop for not selected shipvia of shipment
*!                      llPosHLoop = loop for not selected status of po
*!                      llStylLoop = loop for not selected style group of style
*!*************************************************************
*! Returns            : none
*!*************************************************************
*! Example   : = lfFound(lcWhileExp,lcForExp,llShipLoop,llPosHLoop,llStylLoop)
*!*************************************************************
*
FUNCTION lfFound
PARAMETERS lcWhileExp, lcForExp, llShipLoop, llPosHLoop, llStylLoop

SELECT POSLN
lcPosLnKey = KEY()
SCAN REST WHILE &lcPosLnKey. = lcWhileExp FOR &lcForExp.
  IF llShipLoop .AND. SEEK(SHIPNO,'SHPMTHDR') .AND. !EMPTY(lcShipVia) .AND. !(SHPMTHDR.SHIPVIA $ lcShipVia)
    LOOP
  ENDIF
  IF llPosHLoop .AND. SEEK(CSTYTYPE+PO,'POSHDR') .AND. !(POSHDR.STATUS $ lcRpStatus)
    LOOP
  ENDIF
  
  *B605073,1 AMH seek in the style file in all cases [Start]
  *IF llStylLoop .AND. SEEK(STYLE,'STYLE') .AND. IIF(EMPTY(lcStyGroup),.F.,!(STYLE.CSTYGROUP $ lcStyGroup))
  IF SEEK(STYLE,'STYLE') .AND. llStylLoop .AND. IIF(EMPTY(lcStyGroup),.F.,!(STYLE.CSTYGROUP $ lcStyGroup))
  *B605073,1 AMH [End]
  
    LOOP
  ENDIF
  DO lpAddNew
  SELECT POSLN
  IF lcRpCost = '4' .AND. lcRpMfgo # 'TOTALZ'
    IF SEEK('I1'+PO+STR(LINENO,6)+'4'+STYLE,'BOMLINE')
      SELECT BOMLINE
      SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+Sclr+Item+Iclr+MfgCode =;
                      'I1'+POSLN.PO+STR(POSLN.LINENO,6)+'4'+POSLN.STYLE;
                FOR MFGCODE = lcRpMfgo
        SELECT (lcTmpFile)
        IF lFirst

          *B605020,1 AMH don't get the est. cost from BOMLINE file [Start]
          *REPLACE NSTYCOST WITH 0,;
                  NLNDCOST WITH 0,;
                  LFIRST   WITH .F.
          REPLACE NLNDCOST WITH 0,;
                  LFIRST   WITH .F.
          *B605020,1 AMH [End]
          
        ENDIF
        
        *B605020,1 AMH don't get the est. cost from BOMLINE file [Start]
        *REPLACE NSTYCOST WITH NSTYCOST + (BOMLINE.UNITCOST*IIF(lcRpType='H',POSLN.TOTQTY,1)),;
                NLNDCOST WITH NLNDCOST + (BOMLINE.UNITCOST*IIF(lcRpType='H',POSLN.TOTQTY,1))
        REPLACE NLNDCOST WITH NLNDCOST + (BOMLINE.UNITCOST*IIF(lcRpType='H',POSLN.TOTQTY,1))
        *B605020,1 AMH [End]
        
      ENDSCAN
      SELECT (lcTmpFile)
      IF lFirst .AND. lcRpType # 'H'
        DELETE
        LOOP
      ENDIF
      SELECT BOMLINE
      IF SEEK('I2'+POSLN.PO+STR(POSLN.LINENO,6)+'4'+POSLN.STYLE)
        *C102413,4 AMH Scan while ctype = 2 not ctype = 1 [Start]
        *SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+Sclr+Item+Iclr+MfgCode =;
                        'I1'+POSLN.PO+STR(POSLN.LINENO,6)+'4'+POSLN.STYLE;
                  FOR MFGCODE = lcRpMfgo .AND. !EMPTY(CRSESSION)
        SCAN REST WHILE cImTyp+cType+cTktNo+STR(LineNo,6)+cBomTyp+Style+Sclr+Item+Iclr+MfgCode =;
                        'I2'+POSLN.PO+STR(POSLN.LINENO,6)+'4'+POSLN.STYLE;
                  FOR MFGCODE = lcRpMfgo .AND. CRSESSION = POSLN.CRSESSION
        *C102413,4 AMH [End]
          SELECT (lcTmpFile)
          IF lFirstLnd
            REPLACE NLNDCOST  WITH 0,;
                    LFIRSTLND WITH .F.
          ENDIF
          REPLACE NLNDCOST WITH NLNDCOST + (BOMLINE.UNITCOST*IIF(lcRpType='H',POSLN.TOTQTY,1))
        ENDSCAN
      ENDIF
      IF lcRpType = 'H'
        SELECT (lcTmpFile)
        REPLACE LFIRSTLND WITH .F.
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
*--end of lfFound.

*!*************************************************************
*! Name      : lfvRepType
*! Developer : (AMH) Ahmed Maher
*! Date      : 09/24/2001
*! Purpose   : Validate lcRpCost option
*!*************************************************************
*! Example   : = lfvRepType()
*!*************************************************************
FUNCTION lfvRepType

DO CASE
  CASE lcRpType = 'D'  && Shipment Detail report.
    lcRpForm = 'POESTLND'
  CASE lcRpType = 'H'  && Shipment Summary report.
    lcRpForm = 'POESTLN1'
  CASE lcRpType = 'S'  && Style Detail report.
    lcRpForm = 'POESTLN2'
ENDCASE
CLEAR READ
*-- end lfvRepType.

*B605020,1 AMH [Start]
*!*************************************************************
*! Name      : lfGtStyCst
*! Developer : AHMED MAHER
*! Date      : 09/19/2001
*! Purpose   : Get the estimated cost
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : lpAddNew
*!*************************************************************
*! Passed Parameters  : lcType  = cost element type
*!*************************************************************
*! Returns            : cost value
*!*************************************************************
*! Example   : = lfGtStyCst()
*!*************************************************************
*
FUNCTION lfGtStyCst
PARAMETERS lcType
PRIVATE lnCostVal, lnAlias
lnCostVal = 0
lnAlias = SELECT(0)
lcType = IIF(lcType='6','',lcType)
IF SEEK(STYLE.CSTYMAJOR+lcType,'BOM')
  SELECT BOM
  SUM TOTCOST REST WHILE cItmMajor+Typ+cItmMask+Mfgcode+Item+iClr = STYLE.CSTYMAJOR+lcType;
              FOR (CITMMASK = POSLN.STYLE .OR. ALLTRIM(SUBSTR(CITMMASK,lnMajorLen+2)) = '******');
                  .AND. IIF(lcType = '4' .AND. lcRpMfgo # 'TOTALZ',MFGCODE=lcRpMfgo,.T.);
                  TO lnCostVal
ENDIF
SELECT (lnAlias)
lnCostVal = lnCostVal*IIF(lcRpType='H',POSLN.TOTQTY,1)
RETURN lnCostVal
*-- End of lfGtStyCst.
*B605020,1 AMH [End]